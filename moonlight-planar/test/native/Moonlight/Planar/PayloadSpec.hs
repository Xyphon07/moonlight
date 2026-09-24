{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Payload maps, traversals, geometric projection, and rewrite survival laws.
module Moonlight.Planar.PayloadSpec
  ( tests
  ) where

import Control.Monad ( forM_, unless, when )
import Control.Exception (TypeError, displayException, evaluate, try)
import Control.Monad.ST (runST)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Word ( Word8, Word32 )
import Moonlight.Planar.BulkLoad ( delaunay, insert )
import Moonlight.Planar.Dcel ( mapDirectedEdges, mapFaces, mapUndirectedEdges, mapVertices,
  destination, directedEdgeData, faceData, numFaces, numUndirectedEdges, origin,
  setDirectedEdgeData, setFaceData, setUndirectedEdgeData, setVertexData, undirectedEdgeData,
  undirectedEndpoints, vertexData, vertexPoint )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( allFaces, directedEdges, undirectedEdges,
  vertices, innerFaces )
import Moonlight.Planar.Internal.BoxedPaged
  ( BoxedPaged, BoxedFill (..), FillRequirement (..), boxedDefaulted, boxedFill
  , boxedMaterializedPageCount, boxedToVector, boxedUpdate
  , emptyBoxedPaged, freezeBoxedPaged, requiredBoxedFill, resetBoxedRange, thawBoxedPaged )
import qualified Moonlight.Planar.PayloadTypeErrors as Rejected
import Moonlight.Planar.Internal.HandleDefs ( DirectedEdgeId, UndirectedEdgeId, VertexId(VertexId),
  asUndirected )
import Moonlight.Planar.Internal.Paged ( Paged, TransactionShape (LocalTransaction) )
import Moonlight.Planar.MeshFixtures ( faceKeyOf )
import Moonlight.Planar.Payload ( directedPayloads, facePayloads, undirectedPayloads,
  vertexPayloads, overPayloads, payloadList )
import Moonlight.Planar.PayloadFixtures ( SampleVertex(..) )
import Moonlight.Planar.Session ( withSession, insertVertex, refuse, removeAt )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (unitElementDefaults, BuildError(RemovalVertexOutOfRange), ConstraintMode(Unconstrained), ElementDefaults(ElementDefaults, defaultFaceData), InsertionDisposition(Inserted), BuildResult(buildTriangulation), InsertionResult(insertionDisposition, insertionTriangulation), Triangulation)
import Support ( assertEqual, assertValid, requireRight, randomPoints )
import qualified Data.IntSet as IntSet
import Moonlight.Planar.Internal.Representation qualified as Internal
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testGenericPayloads
    , testPayloadMaps
    , testGeometryOnlyPublication
    , testPayloadTraversals
    , testRewritePayloadIdentity
    , testFillOwnership
    ]

testGenericPayloads :: IO ()
testGenericPayloads = do
  let defaults = ElementDefaults (7 :: Int) False ("new-face" :: String)
      payloads = V.fromList
        [ SampleVertex (Point 0 0) 1
        , SampleVertex (Point 2 0) 2
        , SampleVertex (Point 0 2) 3
        , SampleVertex (Point 0.5 0.5) 4
        ]
  built <- requireRight "generic payload build" (delaunay defaults payloads)
  let triangulation = buildTriangulation built
  assertValid "generic payload build" triangulation
  assertEqual "vertex payload" 4 (sampleLabel (vertexData triangulation (VertexId 3)))
  forM_ (directedEdges triangulation) $ \edge -> assertEqual "directed default" 7 (directedEdgeData triangulation edge)
  forM_ (undirectedEdges triangulation) $ \edge -> assertEqual "undirected default" False (undirectedEdgeData triangulation edge)
  forM_ (allFaces triangulation) $ \face -> assertEqual "face default" "new-face" (faceData triangulation face)
  (firstEdge, firstFace) <- case (directedEdges triangulation, innerFaces triangulation) of
    (edge : _, face : _) -> pure (edge, face)
    _ -> fail "generic payload build produced no inner topology"
  let firstUndirected = asUndirected firstEdge
      changed = setFaceData (setUndirectedEdgeData (setDirectedEdgeData triangulation firstEdge 42) firstUndirected True) firstFace "changed"
  assertEqual "directed payload update" 42 (directedEdgeData changed firstEdge)
  assertEqual "undirected payload update" True (undirectedEdgeData changed firstUndirected)
  assertEqual "face payload update" "changed" (faceData changed firstFace)
  -- Geometry owns the points, so a payload carrying a different position is not
  -- a contradiction to be refused — it is a payload whose position nobody reads.
  let moved = setVertexData triangulation (VertexId 0) (SampleVertex (Point 9 9) 0)
  assertEqual "a payload position does not site a vertex"
    (vertexPoint triangulation (VertexId 0)) (vertexPoint moved (VertexId 0))
  assertEqual "the payload is stored as given"
    (Point 9 9) (samplePosition (vertexData moved (VertexId 0)))

-- The payload layer over a fixed geometry is a product of four free components.
-- Each is a functor and each is checked as such; nothing in the product can
-- disturb the geometry underneath it.
testPayloadMaps :: IO ()
testPayloadMaps = do
  let defaults = ElementDefaults (7 :: Int) ("new-undirected" :: String) ("new-face" :: String)
      payloads = V.fromList
        [ SampleVertex (Point 0 0) 1
        , SampleVertex (Point 4 0) 2
        , SampleVertex (Point 4 4) 3
        , SampleVertex (Point 0 4) 4
        , SampleVertex (Point 1 2) 5
        ]
  built <- requireRight "payload map build" (delaunay defaults payloads)
  let plain = buildTriangulation built
      -- Distinct payloads everywhere: a map that permuted its component would
      -- be invisible against uniform defaults.
      withDirected = List.foldl' (\t (label, edge) -> setDirectedEdgeData t edge label) plain (zip [100 ..] (directedEdges plain))
      withUndirected = List.foldl' (\t (label, edge) -> setUndirectedEdgeData t edge ("u-" <> show label)) withDirected (zip [(0 :: Int) ..] (undirectedEdges withDirected))
      sample = List.foldl' (\t (label, face) -> setFaceData t face ("f-" <> show label)) withUndirected (zip [(0 :: Int) ..] (allFaces withUndirected))
      endpoints ::
        Triangulation mode vertex directed undirected face ->
        [(VertexId, VertexId)]
      endpoints t = [(origin t edge, destination t edge) | edge <- directedEdges t]

  -- Identity. Equality on a triangulation compares geometry, topology,
  -- constraint flags and element defaults as well as payloads, so this single
  -- equation states that each map disturbs nothing but the component it names.
  assertEqual "mapDirectedEdges identity" sample (mapDirectedEdges id sample)
  assertEqual "mapUndirectedEdges identity" sample (mapUndirectedEdges id sample)
  assertEqual "mapFaces identity" sample (mapFaces id sample)
  assertEqual "mapVertices identity" sample (mapVertices id sample)

  assertEqual "mapDirectedEdges composition"
    (mapDirectedEdges ((* 2) . (+ 1)) sample)
    (mapDirectedEdges (* 2) (mapDirectedEdges (+ 1) sample))
  assertEqual "mapFaces composition"
    (mapFaces (("<" <>) . (<> ">")) sample)
    (mapFaces ("<" <>) (mapFaces (<> ">") sample))

  -- Commutation with the accessors. Identity and composition are blind to the
  -- indexing; this is the law that pins each payload to its own handle.
  let directedMapped = mapDirectedEdges (+ 1) sample
      undirectedMapped = mapUndirectedEdges ("<" <>) sample
      facesMapped = mapFaces ("<" <>) sample
  forM_ (directedEdges sample) $ \edge ->
    assertEqual "mapDirectedEdges commutes with directedEdgeData"
      (directedEdgeData sample edge + 1) (directedEdgeData directedMapped edge)
  forM_ (undirectedEdges sample) $ \edge ->
    assertEqual "mapUndirectedEdges commutes with undirectedEdgeData"
      ("<" <> undirectedEdgeData sample edge) (undirectedEdgeData undirectedMapped edge)
  forM_ (allFaces sample) $ \face ->
    assertEqual "mapFaces commutes with faceData"
      ("<" <> faceData sample face) (faceData facesMapped face)

  -- The components are independent, and none of them is geometry.
  assertEqual "face and directed maps commute"
    (mapFaces ("<" <>) (mapDirectedEdges (+ 1) sample))
    (mapDirectedEdges (+ 1) (mapFaces ("<" <>) sample))
  assertEqual "mapFaces preserves topology" (endpoints sample) (endpoints facesMapped)
  assertEqual "mapFaces preserves the face count" (numFaces sample) (numFaces facesMapped)
  forM_ (vertices sample) $ \vertex ->
    assertEqual "mapFaces preserves geometry" (vertexPoint sample vertex) (vertexPoint facesMapped vertex)

  -- The element default is a payload and must travel with them: every element a
  -- later insertion creates is handed the default, so a map that reindexed the
  -- stored payloads and left the default behind would produce a triangulation
  -- whose future elements disagree with its present ones. Every stored payload
  -- here differs from the default, so carrying the wrong one is visible.
  grownFaces <- insertionTriangulation <$> requireRight "insertion into mapped faces" (insert facesMapped (SampleVertex (Point 2 1) 6))
  grownUndirected <- insertionTriangulation <$> requireRight "insertion into mapped undirected edges" (insert undirectedMapped (SampleVertex (Point 2 1) 6))
  grownDirected <- insertionTriangulation <$> requireRight "insertion into mapped directed edges" (insert directedMapped (SampleVertex (Point 2 1) 6))
  unless (numFaces grownFaces > numFaces facesMapped) $ fail "the insertion created no face"
  unless ("<new-face" `elem` map (faceData grownFaces) (allFaces grownFaces)) $
    fail ("new faces did not receive the mapped default: " <> show (map (faceData grownFaces) (allFaces grownFaces)))
  unless ("<new-undirected" `elem` map (undirectedEdgeData grownUndirected) (undirectedEdges grownUndirected)) $
    fail "new undirected edges did not receive the mapped default"
  unless (8 `elem` map (directedEdgeData grownDirected) (directedEdges grownDirected)) $
    fail "new directed edges did not receive the mapped default"

  -- Vertices, the component that used to be special. The map is total, and the
  -- one thing worth insisting on is that a function which does its worst to the
  -- stored position still cannot move a vertex.
  let relabelled = mapVertices (\v -> v{sampleLabel = sampleLabel v * 10}) sample
      collapsed = mapVertices (\v -> v{samplePosition = Point 9 9}) sample
  forM_ (vertices sample) $ \vertex -> do
    assertEqual "mapVertices commutes with vertexData"
      (sampleLabel (vertexData sample vertex) * 10) (sampleLabel (vertexData relabelled vertex))
    assertEqual "mapVertices preserves geometry" (vertexPoint sample vertex) (vertexPoint relabelled vertex)
    assertEqual "a payload map cannot move a vertex"
      (vertexPoint sample vertex) (vertexPoint collapsed vertex)
  assertEqual "mapVertices composes"
    (mapVertices (\v -> v{sampleLabel = sampleLabel v + 1}) relabelled)
    (mapVertices (\v -> v{sampleLabel = sampleLabel v * 10 + 1}) sample)

  -- The sharpest statement of freedom available: the image type has no
  -- 'HasPosition' instance at all. This does not typecheck under a vertex
  -- component that geometry reads through.
  let projected = mapVertices sampleLabel sample
  forM_ (vertices sample) $ \vertex -> do
    assertEqual "a vertex payload need not have a position"
      (sampleLabel (vertexData sample vertex)) (vertexData projected vertex)
    assertEqual "projecting payloads away preserves geometry"
      (vertexPoint sample vertex) (vertexPoint projected vertex)

-- Geometry publication is payload forgetting, not a dense payload map.  The
-- polymorphic entrance accepts all four non-unit payload planes and publishes
-- zero materialized pages while retaining the exact structural planes.
testGeometryOnlyPublication :: IO ()
testGeometryOnlyPublication = do
  let defaults = ElementDefaults (17 :: Int) False ("source-face" :: String)
      payloads =
        V.fromList
          [ SampleVertex (Point 0 0) 11
          , SampleVertex (Point 4 0) 22
          , SampleVertex (Point 4 4) 33
          , SampleVertex (Point 0 4) 44
          , SampleVertex (Point 1 2) 55
          ]
  built <- requireRight "geometry-only payload source" (delaunay defaults payloads)
  let source = buildTriangulation built
      published = Internal.geometryOnlyPublication source
      republished = Internal.geometryOnlyPublication published
      structuralPlanes
        :: forall vertex directed undirected face.
           Triangulation 'Unconstrained vertex directed undirected face
        -> ( Paged Double
           , Paged Double
           , Paged Word32
           , Paged Word32
           , Paged Word32
           , Paged Word8
           , Int
           , IntSet.IntSet
           )
      structuralPlanes triangulation =
        ( Internal.triPointX triangulation
        , Internal.triPointY triangulation
        , Internal.triVertexOut triangulation
        , Internal.triHalfTopology triangulation
        , Internal.triFaceEdge triangulation
        , Internal.triConstraint triangulation
        , Internal.triConstraintCount triangulation
        , Internal.triConstraintEdges triangulation
        )
      payloadPageCounts
        :: forall vertex directed undirected face.
           Triangulation 'Unconstrained vertex directed undirected face
        -> (Int, Int, Int, Int)
      payloadPageCounts triangulation =
        ( boxedMaterializedPageCount (Internal.triVertexData triangulation)
        , boxedMaterializedPageCount (Internal.triDirectedData triangulation)
        , boxedMaterializedPageCount (Internal.triUndirectedData triangulation)
        , boxedMaterializedPageCount (Internal.triFaceData triangulation)
        )
  unless (sumPayloadPages (payloadPageCounts source) > 0) $
    fail "geometry-only payload fixture did not materialize a source payload page"
  assertEqual
    "geometry-only publication preserves structural planes"
    (structuralPlanes source)
    (structuralPlanes published)
  assertEqual
    "geometry-only publication replaces every payload plane with a zero-page store"
    (0, 0, 0, 0)
    (payloadPageCounts published)
  assertEqual
    "geometry-only publication installs unit element defaults"
    unitElementDefaults
    (Internal.authoringElementDefaults published)
  assertEqual
    "geometry-only publication prepares the exact seam frontier"
    (Internal.prepareSeamFrontierIndex source)
    (Internal.triSeamFrontier published)
  assertEqual
    "geometry-only publication preserves an existing seam frontier"
    (Internal.triSeamFrontier published)
    (Internal.triSeamFrontier republished)
  assertValid "geometry-only publication" published
  assertEqual "geometry-only vertex traversal preserves its optional fill"
    published (overPayloads vertexPayloads id published)
  assertEqual "geometry-only vertex traversal agrees with mapping"
    (mapVertices (const (7 :: Int)) published)
    (overPayloads vertexPayloads (const (7 :: Int)) published)
  assertEqual "geometry-only vertex traversal visits residents then one fill"
    (replicate (V.length payloads + 1) ())
    (payloadList vertexPayloads published)
  let (numbered, visitCount) = runState (vertexPayloads numberVisit published) 0
  assertEqual "stateful traversal visits each vertex exactly once"
    (V.fromList [0 .. V.length payloads - 1])
    (fmap snd (boxedToVector (Internal.triVertexData numbered)))
  assertEqual "stateful traversal puts the optional fill last"
    (Fill ((), V.length payloads)) (boxedFill (Internal.triVertexData numbered))
  assertEqual "stateful traversal visits exactly one fill"
    (V.length payloads + 1) visitCount
 where
  sumPayloadPages
    :: (Int, Int, Int, Int)
    -> Int
  sumPayloadPages (vertexPages, directedPages, undirectedPages, facePages) =
    vertexPages + directedPages + undirectedPages + facePages

numberVisit :: value -> State Int (value, Int)
numberVisit value = state (\index -> ((value, index), index + 1))

newtype PayloadLabel = PayloadLabel Int
  deriving stock (Eq, Show)

testFillOwnership :: IO ()
testFillOwnership = do
  emptyBuilt <- requireRight "empty geometry publication"
    (delaunay unitElementDefaults (V.empty :: V.Vector Point))
  let sparse :: BoxedPaged 'RequiredFill Int
      sparse = boxedDefaulted 7 600
      written = boxedUpdate 590 103 . boxedUpdate 257 101 . boxedUpdate 3 99 $ sparse
      recycled = runST $ do
        mutable <- thawBoxedPaged LocalTransaction written
        resetBoxedRange mutable 254 8
        freezeBoxedPaged 730 mutable
      untouched = runST $ do
        mutable <- thawBoxedPaged LocalTransaction sparse
        resetBoxedRange mutable 0 600
        freezeBoxedPaged 730 mutable
      optionalEmpty :: BoxedPaged 'OptionalFill Int
      optionalEmpty = emptyBoxedPaged NoFill
      filledEmpty :: BoxedPaged 'RequiredFill Int
      filledEmpty = emptyBoxedPaged (Fill 7)
      labelled :: BoxedPaged 'RequiredFill PayloadLabel
      labelled = coerce sparse
      (emptyVisited, emptyVisits) = runState
        (vertexPayloads numberVisit (Internal.geometryOnlyPublication (buildTriangulation emptyBuilt))) 0
  reused <- requireRight "required-fill range reuse" recycled
  stayedSparse <- requireRight "untouched required-fill range" untouched
  assertEqual "reuse reads its owning fill across a page boundary and grown tail"
    (V.generate 730 (\index -> if index == 3 then 99 else if index == 590 then 103 else 7))
    (boxedToVector reused)
  assertEqual "resetting absent pages leaves them absent" 0
    (boxedMaterializedPageCount stayedSparse)
  assertEqual "empty required store maps its future fill" 8
    (requiredBoxedFill (fmap (+ 1) filledEmpty))
  assertEqual "empty optional store preserves absence" NoFill
    (boxedFill (fmap (+ 1) optionalEmpty))
  assertEqual "payload newtypes remain representational" (PayloadLabel 7)
    (requiredBoxedFill labelled)
  assertEqual "an empty geometry store still visits its optional fill" 1 emptyVisits
  assertEqual "the empty store retains the visited fill" (Fill ((), 0))
    (boxedFill (Internal.triVertexData emptyVisited))
  let checkRejected (label, attempted) = do
        result <- try (evaluate attempted) :: IO (Either TypeError ())
        case result of
          Left failure -> unless
            ("RequiredFill" `List.isInfixOf` displayException failure
              && "OptionalFill" `List.isInfixOf` displayException failure)
            (fail (label <> ": wrong type error: " <> displayException failure))
          Right () -> fail (label <> ": required fill was forged")
  traverse_ checkRejected
    [ ("construction", Rejected.forgeRequiredFill `seq` ())
    , ("nominal index", Rejected.coerceRequiredFill optionalEmpty `seq` ())
    ]
testPayloadTraversals :: IO ()
testPayloadTraversals = do
  let defaults = ElementDefaults (7 :: Int) ("new-undirected" :: String) ("new-face" :: String)
      payloads = V.fromList
        [ SampleVertex (Point 0 0) 1
        , SampleVertex (Point 4 0) 2
        , SampleVertex (Point 4 4) 3
        , SampleVertex (Point 0 4) 4
        , SampleVertex (Point 1 2) 5
        ]
  built <- requireRight "payload traversal build" (delaunay defaults payloads)
  let plain = buildTriangulation built
      withDirected = List.foldl' (\t (label, edge) -> setDirectedEdgeData t edge label) plain (zip [100 ..] (directedEdges plain))
      withUndirected = List.foldl' (\t (label, edge) -> setUndirectedEdgeData t edge ("u-" <> show label)) withDirected (zip [(0 :: Int) ..] (undirectedEdges withDirected))
      sample = List.foldl' (\t (label, face) -> setFaceData t face ("f-" <> show label)) withUndirected (zip [(0 :: Int) ..] (allFaces withUndirected))

  -- 'overPayloads' is the traversal under 'Identity', so this is the traversal
  -- identity law. It also says that rebuilding a payload store from its own
  -- contents is not observable, which is the part a paged store could get
  -- wrong: the traversal materializes pages the map would have left absent.
  assertEqual "vertexPayloads identity" sample (overPayloads vertexPayloads id sample)
  assertEqual "directedPayloads identity" sample (overPayloads directedPayloads id sample)
  assertEqual "undirectedPayloads identity" sample (overPayloads undirectedPayloads id sample)
  assertEqual "facePayloads identity" sample (overPayloads facePayloads id sample)

  -- Each traversal and its named map are one function. The effectful
  -- generalization is not allowed a second opinion about what relabeling means.
  assertEqual "vertexPayloads agrees with mapVertices"
    (mapVertices sampleLabel sample) (overPayloads vertexPayloads sampleLabel sample)
  assertEqual "directedPayloads agrees with mapDirectedEdges"
    (mapDirectedEdges (* 2) sample) (overPayloads directedPayloads (* 2) sample)
  assertEqual "undirectedPayloads agrees with mapUndirectedEdges"
    (mapUndirectedEdges ("<" <>) sample) (overPayloads undirectedPayloads ("<" <>) sample)
  assertEqual "facePayloads agrees with mapFaces"
    (mapFaces ("<" <>) sample) (overPayloads facePayloads ("<" <>) sample)

  assertEqual "facePayloads composes"
    (overPayloads facePayloads (("<" <>) . (<> ">")) sample)
    (overPayloads facePayloads ("<" <>) (overPayloads facePayloads (<> ">") sample))

  -- The class instances range over the face payload, being the last parameter.
  assertEqual "fmap is the face payload map" (mapFaces ("<" <>) sample) (fmap ("<" <>) sample)
  assertEqual "traverse is facePayloads"
    (Just (overPayloads facePayloads ("<" <>) sample))
    (traverse (Just . ("<" <>)) sample)
  assertEqual "the Foldable instance is the face traversal"
    (payloadList facePayloads sample) (foldr (:) [] sample)

  -- Visit order, and the element default's place in it. A fold that skipped
  -- the default would report the triangulation as holding one fewer face
  -- payload than it holds.
  assertEqual "vertexPayloads visits the vertices in order"
    (map (vertexData sample) (vertices sample))
    (payloadList vertexPayloads sample)
  assertEqual "facePayloads visits the faces and then the default"
    (map (faceData sample) (allFaces sample) <> [defaultFaceData (Internal.authoringElementDefaults sample)])
    (payloadList facePayloads sample)

  -- The point of the exercise: relabeling under an effect, with a refusal
  -- reaching the caller instead of a half-relabelled triangulation.
  let refuseAtThree :: SampleVertex -> Either String Int
      refuseAtThree v = if sampleLabel v == 3 then Left "vertex three refuses" else Right (sampleLabel v * 10)
      keepLabel :: SampleVertex -> Either String Int
      keepLabel = Right . sampleLabel
      decorate :: String -> Either String String
      decorate = Right . ("<" <>)
  assertEqual "an effectful relabel short-circuits"
    (Left "vertex three refuses") (vertexPayloads refuseAtThree sample)
  relabelled <- requireRight "effectful relabel" (vertexPayloads keepLabel sample)
  assertEqual "a successful effectful relabel is the pure one"
    (mapVertices sampleLabel sample) relabelled

  -- The default travels through the traversal, and travels exactly once: the
  -- store's fill and the element defaults are written from a single visit, so
  -- an element created afterwards inherits precisely what the traversal made.
  traversedFaces <- requireRight "effectful face relabel" (facePayloads decorate sample)
  grown <- insertionTriangulation <$> requireRight "insertion after traversal" (insert traversedFaces (SampleVertex (Point 2 1) 6))
  unless (numFaces grown > numFaces traversedFaces) $ fail "the insertion created no face"
  unless ("<new-face" `elem` map (faceData grown) (allFaces grown)) $
    fail ("new faces did not receive the traversed default: " <> show (map (faceData grown) (allFaces grown)))

-- | The in-circle predicate is the orientation of the four points lifted to
-- the paraboloid @z = x² + y²@. The referent is that 4×4 determinant evaluated
-- exactly over 'Rational' — deliberately not the translated 3×3 the instances
-- expand, which would only be the implementation checking its own algebra.
testRewritePayloadIdentity :: IO ()
testRewritePayloadIdentity = do
  let defaults = ElementDefaults (0 :: Int) (0 :: Int) (0 :: Int)
      target = Point 0.001_37 (-0.002_11)
      corpus = randomPoints 0x1a2b3c4d 512
  built <- requireRight "rewrite identity base" (delaunay defaults (V.fromList corpus))
  let base = buildTriangulation built
      directedTable = labelTable (map (directedKeyOf base) (directedEdges base))
      undirectedTable = labelTable (map (undirectedKeyOf base) (undirectedEdges base))
      faceTable = labelTable (map (faceKeyOf base) (innerFaces base))

  -- Keys identify elements only if they are unique, so the premise is checked
  -- rather than assumed: a collapsed table would silently weaken everything
  -- below it into a test of nothing.
  assertEqual "directed keys are unique" (length (directedEdges base)) (Map.size directedTable)
  assertEqual "undirected keys are unique" (length (undirectedEdges base)) (Map.size undirectedTable)
  assertEqual "face keys are unique" (length (innerFaces base)) (Map.size faceTable)

  let withDirected =
        List.foldl' (\t e -> setDirectedEdgeData t e (directedTable Map.! directedKeyOf base e)) base (directedEdges base)
      withUndirected =
        List.foldl' (\t e -> setUndirectedEdgeData t e (undirectedTable Map.! undirectedKeyOf base e)) withDirected (undirectedEdges base)
      labelled =
        List.foldl' (\t f -> setFaceData t f (faceTable Map.! faceKeyOf base f)) withUndirected (innerFaces base)
  inserted <- requireRight "rewrite identity insert" (insert labelled target)
  let result = insertionTriangulation inserted
  assertValid "rewrite identity result" result
  assertEqual "the point was genuinely inserted" Inserted (insertionDisposition inserted)

  -- Both directions, on every plane. A surviving element keeps exactly its
  -- label; everything else holds the default and nothing else.
  assertPayloadSurvival "directed" directedTable
    [(directedKeyOf result e, directedEdgeData result e) | e <- directedEdges result]
  assertPayloadSurvival "undirected" undirectedTable
    [(undirectedKeyOf result e, undirectedEdgeData result e) | e <- undirectedEdges result]
  assertPayloadSurvival "face" faceTable
    [(faceKeyOf result f, faceData result f) | f <- innerFaces result]

  -- The flip signature. Every flip after an insertion joins the new vertex to
  -- the far corner of a cavity quad, so a new edge is not evidence of one — a
  -- split produces those too. A DESTROYED edge is: splitting a face destroys
  -- nothing and adds exactly three edges and two faces, which is asserted here
  -- so that the arithmetic holds, and under it every base key that is gone from
  -- the result was flipped away. Without this the paragraphs above are a claim
  -- about splits alone.
  assertEqual "the point landed strictly inside a face"
    (numUndirectedEdges base + 3, numFaces base + 2)
    (numUndirectedEdges result, numFaces result)
  let surviving = Set.fromList (map (undirectedKeyOf result) (undirectedEdges result))
      flippedAway = filter (`Set.notMember` surviving) (Map.keys undirectedTable)
  when (null flippedAway) (fail "the insertion caused no flip, so the flip case went unchecked")

  -- The same claim across one transaction that both retires and creates.
  -- Removal swap-compacts, which leaves a retired element's payload sitting in
  -- the slot it vacated; the inserts that follow are handed those slots back.
  -- Nothing else in the suite makes an allocation reissue a used slot.
  let doomed = take 60 corpus
      arrivals = randomPoints 0x5f3a19c2 60
  (_, edited, _) <-
    requireRight "rewrite identity session" $
      withSession labelled (length arrivals) $ do
        mapM_
          (\point -> removeAt point >>= maybe (refuse (RemovalVertexOutOfRange (VertexId 0) 0)) (const (pure ())))
          doomed
        mapM_ insertVertex arrivals
  assertValid "rewrite identity session" edited
  assertPayloadSurvival "session directed" directedTable
    [(directedKeyOf edited e, directedEdgeData edited e) | e <- directedEdges edited]
  assertPayloadSurvival "session undirected" undirectedTable
    [(undirectedKeyOf edited e, undirectedEdgeData edited e) | e <- undirectedEdges edited]
  assertPayloadSurvival "session face" faceTable
    [(faceKeyOf edited f, faceData edited f) | f <- innerFaces edited]

directedKeyOf
  :: Triangulation mode vertex directed undirected face
  -> DirectedEdgeId
  -> (Point, Point)
directedKeyOf triangulation edge =
  ( vertexPoint triangulation (origin triangulation edge)
  , vertexPoint triangulation (destination triangulation edge)
  )

undirectedKeyOf
  :: Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> (Point, Point)
undirectedKeyOf triangulation edge =
  case undirectedEndpoints triangulation edge of
    (from, to) ->
      let (left, right) = (vertexPoint triangulation from, vertexPoint triangulation to)
       in if left <= right then (left, right) else (right, left)

labelTable :: Ord key => [key] -> Map.Map key Int
labelTable keys = Map.fromList (zip keys [1 ..])

-- | Every element carries the label its key was given, or the default if its
-- key is new. Both counts are asserted too: a store that kept everything and a
-- store that kept nothing each satisfy one half of this on its own.
assertPayloadSurvival :: (Ord key, Show key) => String -> Map.Map key Int -> [(key, Int)] -> IO ()
assertPayloadSurvival plane labels elements = do
  let kept = length (filter ((`Map.member` labels) . fst) elements)
  when (kept == 0) (fail (plane <> ": the insertion perturbed every element"))
  when (kept == length elements) (fail (plane <> ": the insertion perturbed no element"))
  forM_ elements $ \(key, payload) ->
    assertEqual (plane <> " label at " <> show key) (Map.findWithDefault 0 key labels) payload

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The native core slice: everything that needs no serialization surface.
module Moonlight.Triangulation.NativeSpec
  ( tests
  , regionMesh
  , regionMeshFromPoints
  , regionFaceSatisfies
  ) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.ST (runST, stToIO)
import GHC.Float (castDoubleToWord64)
import Data.Foldable (toList, traverse_)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)
import qualified Data.Vector as V
import Data.Primitive.PrimArray (indexPrimArray, primArrayFromList, sizeofPrimArray)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32, Word64)
import Moonlight.Triangulation.Alpha
import Moonlight.Triangulation.BulkLoad
import Moonlight.Triangulation.Cdt
import Moonlight.Triangulation.Dcel
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.FloodFillIterator
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
import Moonlight.Triangulation.HintGenerator
import Moonlight.Triangulation.Interpolation
import Moonlight.Triangulation.IntersectionIterator
import Moonlight.Triangulation.Internal.Session (excise)
import Moonlight.Triangulation.Internal.Validation (validateTopologyClosure)
import Moonlight.Triangulation.Math
import Moonlight.Triangulation.Payload
import Moonlight.Triangulation.FilteredPredicateOptimizationSpec
  ( assertFilteredPredicatesSkipExactOracle
  )
import Moonlight.Triangulation.PointLocation
import Moonlight.Triangulation.Refinement
import Moonlight.Triangulation.Removal
import Moonlight.Triangulation.Scalar
import Moonlight.Triangulation.Session
import Moonlight.Triangulation.Types
import Moonlight.Triangulation.Validation
import Moonlight.Triangulation.Voronoi
import Moonlight.Triangulation.Handles.Dynamic qualified as Dynamic
import Moonlight.Triangulation.Handles.Iterators.DynamicIterators qualified as DynamicIterators
import Moonlight.Triangulation.Internal.BoxedPaged (boxedMaterializedPageCount)
import Moonlight.Triangulation.Internal.Canonical (canonicalize)
import Moonlight.Triangulation.Internal.Paged
  ( Paged
  , PublicationStats (..)
  , fromVector
  , toVector
  )
import Moonlight.Triangulation.Internal.Predicates qualified as Admitted
import Moonlight.Triangulation.Internal.PointIndex
  ( MutablePointIndexUpdate (..)
  , lookupMutablePoint
  , newMutablePointIndex
  , relocateMutablePoint
  , removeMutablePoint
  , seedMutablePointIndex
  )
import Moonlight.Triangulation.Internal.Representation qualified as Internal
import Moonlight.Triangulation.Voronoi.Handles qualified as VoronoiDynamic
import GHC.Generics (Generic)
import Support (assertEqual, assertValid, requireQueryPoint, requireRight)

tests :: IO ()
tests = do
  testConstructors
  testPredicates
  testParaboloidLift
  assertFilteredPredicatesSkipExactOracle
  testValidationRejectsCorruptedMeshes
  testRefinementCompletionIsAFixpoint
  testScalarFormat
  testWideDistanceNearestNeighbor
  testIncrementalLocationDescent
  testPersistentInsertionReusesFrozenLocation
  testVertexSeededSessionInsertion
  testCircleSweepBulkLoad
  testMixedEditSession
  testBulkRemovalAgreement
  testMutablePointIndexWraparoundBackshift
  testBatchIdentityIndexBatchToEmpty
  testBatchIdentityIndexToSingletonActive
  testBulkIdentityIndexDoesNotEscapeRemovalBatch
  testHierarchyRemovalAgreement
  testHandlesAndFiniteDcel
  testDegenerateConstruction
  testPersistentLocalUpdates
  testGenericPayloads
  testPayloadMaps
  testGeometryOnlyPublication
  testPayloadTraversals
  testRewritePayloadIdentity
  testPointLocationAndHints
  testHierarchyNestingLaw
  testSibsonInterpolation
  testVoronoiDual
  testRemoval
  testConstrainedDelaunay
  testAnnotatedConstrainedUnion
  testAsymmetricConstrainedExtension
  testLargeAsymmetricConstrainedExtension
  testSeparatedConstrainedSeam
  testAxisSeparatedConstrainedSeams
  testSeparatedConstrainedSeamRefusesPinnedFrontierRewrite
  testLocalRefinementRejectsEncroachedHullSplit
  testConstrainedRefinement
  testCheckedLocalRefinement
  testMaximumEdgeLengthQuality
  testLocalRefinementRejectsTrueInterfaceCrossing
  testRepeatedBoundaryAdjacentRefinement
  testFaceComponentsAndBoundaries
  testAlphaFaceFiltration
  testTraversal
  testRandomizedConstruction
  testErrors
  putStrLn "all native core tests passed"

data SampleVertex = SampleVertex
  { samplePosition :: !(Point)
  , sampleLabel :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance HasPosition SampleVertex where
  position = samplePosition


testConstructors :: IO ()
testConstructors = do
  let vacant = empty unitElementDefaults :: DelaunayTriangulation (Point)
  originQuery <- requireQueryPoint "empty location query" (Point 0 0)
  assertValid "empty" vacant
  assertEqual "empty vertex count" 0 (numVertices vacant)
  assertEqual "empty directed edge count" 0 (numDirectedEdges vacant)
  assertEqual "empty face count" 1 (numFaces vacant)
  assertEqual "empty locates nowhere" EmptyTriangulation (locatePoint vacant originQuery)
  built <- requirePointBuild "clear source" [Point 0 0, Point 1 0, Point 0 1]
  assertEqual "clear returns the origin" vacant (clear (buildTriangulation built))

type NativeMesh = Triangulation 'Unconstrained (Point) () () ()

-- | A named corruption of a mesh that was valid one line earlier, paired with
-- the violation it must provoke. Validation that has never been made to fail is
-- evidence only that it ran.
data Corruption = Corruption
  { corruptionName :: String
  , corruptMesh :: NativeMesh -> NativeMesh
  , provokes :: InvariantViolation -> Bool
  }

-- Sheared so that no four sites are cocircular: on a square grid the diagonal
-- of every cell is a free choice, and a fixture that admits two answers cannot
-- witness a wrong one.
corruptionFixture :: [Point]
corruptionFixture =
  [ Point (fromIntegral column + 0.25 * fromIntegral row) (1.3 * fromIntegral row)
  | column <- [0 .. 3 :: Int]
  , row <- [0 .. 3 :: Int]
  ]

rewritePlane :: (U.Unbox a, Num a) => (U.Vector a -> U.Vector a) -> Paged a -> Paged a
rewritePlane edit = fromVector 0 . edit . toVector

slot :: U.Unbox a => Int -> a -> U.Vector a -> U.Vector a
slot at value = (U.// [(at, value)])

-- Past the end of every plane in the fixture, and far from the sentinels the
-- packed representation reserves for absence.
beyond :: NativeMesh -> Word32
beyond mesh = fromIntegral (numDirectedEdges mesh + numVertices mesh + 64)

onTopology :: (NativeMesh -> U.Vector Word32 -> U.Vector Word32) -> NativeMesh -> NativeMesh
onTopology edit mesh =
  mesh {Internal.triHalfTopology = rewritePlane (edit mesh) (Internal.triHalfTopology mesh)}

-- The half-edge plane has stride four — origin, next, previous, face — so the
-- first four entries below are one surgery distinguished only by which field
-- the out-of-range index lands on.
structuralCorruptions :: [Corruption]
structuralCorruptions =
  [ Corruption
      "edge origin names an absent vertex"
      (onTopology (slot 0 . beyond))
      (\case EdgeOriginOutOfRange {} -> True; _ -> False)
  , Corruption
      "edge next names an absent edge"
      (onTopology (slot 1 . beyond))
      (\case EdgeNextOutOfRange {} -> True; _ -> False)
  , Corruption
      "edge previous names an absent edge"
      (onTopology (slot 2 . beyond))
      (\case EdgePreviousOutOfRange {} -> True; _ -> False)
  , Corruption
      "edge face names an absent face"
      (onTopology (slot 3 . beyond))
      (\case EdgeFaceOutOfRange {} -> True; _ -> False)
  , Corruption
      "vertex outgoing names an absent edge"
      ( \mesh ->
          mesh
            { Internal.triVertexOut =
                rewritePlane (slot 0 (beyond mesh)) (Internal.triVertexOut mesh)
            }
      )
      (\case VertexOutgoingOutOfRange {} -> True; _ -> False)
  , Corruption
      "a coordinate plane is one short"
      (\mesh -> mesh {Internal.triPointX = rewritePlane U.init (Internal.triPointX mesh)})
      (\case CoordinatePlaneLengthMismatch {} -> True; _ -> False)
  , Corruption
      "every inner face wound clockwise"
      (\mesh -> mesh {Internal.triPointY = rewritePlane (U.map negate) (Internal.triPointY mesh)})
      (\case InnerFaceNotCounterClockwise {} -> True; _ -> False)
  ]

-- Structural corruption is caught before geometry is read, so the empty-circle
-- law needs a mesh that stays well formed and merely stops being Delaunay.
delaunayCorruptions :: [Corruption]
delaunayCorruptions =
  [ Corruption
      "one site dragged through its neighbours' circumcircles"
      (\mesh -> mesh {Internal.triPointX = rewritePlane (slot 5 40) (Internal.triPointX mesh)})
      (\case LocallyIllegalDelaunayEdge {} -> True; _ -> False)
  ]

-- A surgery that changed nothing would report the oracle as unarmed when in
-- truth it was never asked anything, so the mutant must differ before its
-- rejection means a thing.
assertRejects
  :: String -> (NativeMesh -> [InvariantViolation]) -> NativeMesh -> Corruption -> IO ()
assertRejects oracle check pristine corruption = do
  let mutant = corruptMesh corruption pristine
      label = oracle <> " / " <> corruptionName corruption
      reported = check mutant
  when (mutant == pristine) $ fail (label <> ": the surgery changed nothing")
  unless (any (provokes corruption) reported) $
    fail (label <> ": admitted, reporting " <> show reported)

testValidationRejectsCorruptedMeshes :: IO ()
testValidationRejectsCorruptedMeshes = do
  built <- requirePointBuild "corruption fixture" corruptionFixture
  let pristine = buildTriangulation built
  assertValid "corruption fixture" pristine
  mapM_ (assertRejects "topology" validateTopology pristine) structuralCorruptions
  mapM_ (assertRejects "delaunay" validateDelaunay pristine) delaunayCorruptions
  case innerFaces pristine of
    [] -> fail "local validation fixture has no inner face"
    witnessFace : _ ->
      case faceDirectedEdges pristine witnessFace of
        [] -> fail "local validation fixture has no face edge"
        DirectedEdgeId rawEdge : _ -> do
          let admitted = IntSet.singleton (fromIntegral (unFaceId witnessFace))
              corrupted =
                pristine
                  { Internal.triHalfTopology =
                      rewritePlane
                        (slot (4 * fromIntegral rawEdge + 1) rawEdge)
                        (Internal.triHalfTopology pristine)
                  }
          assertEqual
            "selected validation accepts valid closure"
            []
            (validateTopologyClosure admitted IntSet.empty pristine)
          unless (not (null (validateTopology corrupted))) $
            fail "global validation missed the locally corrupt face closure"
          unless (not (null (validateTopologyClosure admitted IntSet.empty corrupted))) $
            fail "selected validation missed a locally corrupt face closure"

reverseTraversalEvent :: Intersection -> Intersection
reverseTraversalEvent = \case
  EdgeIntersection edge -> EdgeIntersection (reverseEdge edge)
  EdgeOverlap edge -> EdgeOverlap (reverseEdge edge)
  VertexIntersection vertex -> VertexIntersection vertex

-- | What @refinementComplete@ claims is that the quality worklist drained. The
-- assertable content of that claim is a fixpoint: a second pass under the same
-- parameters can admit nothing. The complementary run is budget-starved, where
-- the run must stop short and spend exactly what it was given.
testRefinementCompletionIsAFixpoint :: IO ()
testRefinementCompletionIsAFixpoint = do
  -- Barrier parity needs constraints to bound a domain: on an unconstrained
  -- mesh every face sits at depth zero, so excluding outer faces excludes all
  -- of them and the worklist drains having refined nothing.
  bounded <-
    requireRight "refinement fixpoint domain" $
      constrainedDelaunay
        unitElementDefaults
        (V.fromList [Point 0 0, Point 8 0, Point 8 8, Point 0 8])
        (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
  let source = buildTriangulation bounded
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 500
          , refineMaxArea = Just 3
          , refineExcludeOuterFaces = True
          }
  drained <- requireRight "drained refinement" (refine id parameters source)
  unless (refinementComplete drained) $
    fail "the refinement budget was too small to drain the worklist"
  -- Without this the fixpoint below is vacuous: a run that refined nothing
  -- trivially admits nothing on a second pass.
  unless (refinementAddedVertices drained > 0) $
    fail "the drained run inserted no Steiner points, so the fixpoint proves nothing"
  assertValid "drained refinement" (refinedTriangulation drained)
  again <- requireRight "second refinement pass" (refine id parameters (refinedTriangulation drained))
  assertEqual
    "a drained worklist admits nothing on a second pass"
    0
    (refinementAddedVertices again)
  starved <-
    requireRight
      "starved refinement"
      (refine id parameters {refineMaxAdditionalVertices = Just 3} source)
  when (refinementComplete starved) $
    fail "a three-vertex budget reported a drained worklist"
  assertEqual "a starved run spends exactly its budget" 3 (refinementAddedVertices starved)

testScalarFormat :: IO ()
testScalarFormat = do
  let binary64 = scalarBinaryFormat
  assertEqual "binary64 radix" 2 (formatRadix binary64)
  assertEqual "binary64 mantissa" 53 (formatMantissaDigits binary64)
  assertEqual "binary64 unit roundoff" (encodeFloat 1 (-53)) (scalarUnitRoundoff :: Double)

testWideDistanceNearestNeighbor :: IO ()
testWideDistanceNearestNeighbor = do
  let largePoints = V.fromList
        [ Point 0 0
        , Point 1.0e20 0
        , Point 2.0e20 0
        , Point 1.0e20 1.0e19
        ] :: V.Vector (Point)
  largeBuild <- requireRight "wide-distance build" (delaunay unitElementDefaults largePoints)
  let largeTriangulation = buildTriangulation largeBuild
  assertValid "wide-distance triangulation" largeTriangulation
  query <- requireQueryPoint "wide-distance query" (Point 1.05e20 0)
  case nearestNeighbor largeTriangulation Nothing query of
    Nothing -> fail "wide-distance nearest-neighbor returned Nothing"
    Just (nearest, _) -> assertEqual "wide-distance nearest-neighbor" (VertexId 1) nearest

data IncrementalLocationEvidence = IncrementalLocationEvidence
  { incrementalTriangulation :: !(DelaunayTriangulation (Point))
  , incrementalWalkSteps :: {-# UNPACK #-} !Int
  , incrementalFallbacks :: {-# UNPACK #-} !Int
  }

testIncrementalLocationDescent :: IO ()
testIncrementalLocationDescent = do
  small <- collectIncrementalLocationEvidence 500
  large <- collectIncrementalLocationEvidence 1000
  assertEqual "incremental location fallbacks/500" 0 (incrementalFallbacks small)
  assertEqual "incremental location fallbacks/1000" 0 (incrementalFallbacks large)
  unless (2 * incrementalWalkSteps large < 7 * incrementalWalkSteps small) $
    fail
      ( "incremental location approached quadratic growth: "
          <> show (incrementalWalkSteps small, incrementalWalkSteps large)
      )
  assertValid "incremental location descent/1000" (incrementalTriangulation large)

collectIncrementalLocationEvidence :: Int -> IO IncrementalLocationEvidence
collectIncrementalLocationEvidence count =
  V.foldM' insertAndAccumulate initialEvidence (V.fromList (randomPoints 0xc1ac_10ca count))
 where
  initialEvidence =
    IncrementalLocationEvidence
      { incrementalTriangulation = empty unitElementDefaults
      , incrementalWalkSteps = 0
      , incrementalFallbacks = 0
      }

  insertAndAccumulate evidence point = do
    result <- requireRight "incremental location descent" (insert (incrementalTriangulation evidence) point)
    let stats = insertionStats result
    pure
      IncrementalLocationEvidence
        { incrementalTriangulation = insertionTriangulation result
        , incrementalWalkSteps = incrementalWalkSteps evidence + statLocationWalkSteps stats
        , incrementalFallbacks = incrementalFallbacks evidence + statLocationFallbacks stats
        }

-- A persistent insertion locates on the frozen mesh before opening its dense
-- transaction. The thaw preserves every extant handle, so a lawful frozen
-- location can be interpreted directly without a second mutable walk. A
-- degenerate-line outside witness lacks the terminal-edge evidence its mutable
-- interpreter requires, so that one stratum deliberately retains the mutable
-- fallback. Exercise every frozen stratum, including the singleton's edge-less
-- outside witness, rather than testing only the ordinary face case.
testPersistentInsertionReusesFrozenLocation :: IO ()
testPersistentInsertionReusesFrozenLocation = do
  let vacant = empty unitElementDefaults :: DelaunayTriangulation (Point)
  emptyLocation <- assertPersistentInsertionFromFrozenLocation "empty" Inserted vacant (Point 0 0)
  assertEqual "empty insertion frozen location" EmptyTriangulation emptyLocation

  singletonBuild <- requirePointBuild "singleton frozen location" [Point 0 0]
  singletonLocation <-
    assertPersistentInsertionFromFrozenLocation
      "singleton outside insertion"
      Inserted
      (buildTriangulation singletonBuild)
      (Point 2 0)
  assertEqual "singleton insertion frozen location" (OutsideConvexHull Nothing) singletonLocation

  lineBuild <- requirePointBuild "line frozen locations" [Point 0 0, Point 2 0, Point 4 0]
  let line = buildTriangulation lineBuild
  lineEdgeLocation <- assertPersistentInsertionFromFrozenLocation "line edge insertion" Inserted line (Point 1 0)
  case lineEdgeLocation of
    OnEdge _ -> pure ()
    other -> fail ("line edge insertion located " <> show other)
  lineOutsideLocation <- assertPersistentInsertionFromFrozenLocation "line extension" Inserted line (Point 6 0)
  case lineOutsideLocation of
    OutsideConvexHull (Just _) -> pure ()
    other -> fail ("line extension located " <> show other)

  triangleBuild <- requirePointBuild "area frozen locations" [Point 0 0, Point 4 0, Point 0 4]
  let triangle = buildTriangulation triangleBuild
  faceLocation <- assertPersistentInsertionFromFrozenLocation "face insertion" Inserted triangle (Point 1 1)
  case faceLocation of
    InFace _ -> pure ()
    other -> fail ("face insertion located " <> show other)
  edgeLocation <- assertPersistentInsertionFromFrozenLocation "area edge insertion" Inserted triangle (Point 2 0)
  case edgeLocation of
    OnEdge _ -> pure ()
    other -> fail ("area edge insertion located " <> show other)
  hullLocation <- assertPersistentInsertionFromFrozenLocation "hull insertion" Inserted triangle (Point 5 1)
  case hullLocation of
    OutsideConvexHull (Just _) -> pure ()
    other -> fail ("hull insertion located " <> show other)
  duplicateLocation <- assertPersistentInsertionFromFrozenLocation "duplicate insertion" AlreadyPresent triangle (Point 0 0)
  assertEqual "duplicate insertion frozen location" (OnVertex (VertexId 0)) duplicateLocation

assertPersistentInsertionFromFrozenLocation
  :: String
  -> InsertionDisposition
  -> DelaunayTriangulation (Point)
  -> Point
  -> IO Location
assertPersistentInsertionFromFrozenLocation label expectedDisposition source point = do
  query <- requireQueryPoint (label <> " frozen query") point
  let (located, walked) = locatePointWithHint source Nothing query
      sourceVertices = numVertices source
      usesMutableFallback =
        case located of
          OutsideConvexHull (Just _) -> numInnerFaces source == 0
          _ -> False
  result <- requireRight (label <> " insert") (insert source point)
  ((referenceVertex, referenceDisposition), reference, _) <-
    requireRight (label <> " session reference") $
      withSession source 1 (insertVertexAt point point)
  let stats = insertionStats result
      expectedVertices =
        case expectedDisposition of
          Inserted -> sourceVertices + 1
          AlreadyPresent -> sourceVertices
      expectedUnique =
        case expectedDisposition of
          Inserted -> 1
          AlreadyPresent -> 0
      expectedExisting =
        case expectedDisposition of
          Inserted -> 0
          AlreadyPresent -> 1
  assertEqual (label <> " disposition") expectedDisposition (insertionDisposition result)
  assertEqual (label <> " session disposition") referenceDisposition (insertionDisposition result)
  assertEqual (label <> " session vertex") referenceVertex (insertionVertex result)
  assertEqual
    (label <> " exact-location topology matches session")
    (canonicalEdges reference)
    (canonicalEdges (insertionTriangulation result))
  assertEqual (label <> " source remains unchanged") sourceVertices (numVertices source)
  assertEqual (label <> " result vertex count") expectedVertices (numVertices (insertionTriangulation result))
  assertEqual (label <> " input count") 1 (statInputPoints stats)
  assertEqual (label <> " unique count") expectedUnique (statUniquePoints stats)
  assertEqual (label <> " existing count") expectedExisting (statExistingPoints stats)
  assertEqual (label <> " duplicate count") expectedExisting (statDuplicatePoints stats)
  if usesMutableFallback
    then do
      let assertAtLeast counter expected actual =
            unless
              (actual >= expected)
              ( fail
                  ( label
                      <> " "
                      <> counter
                      <> " includes frozen evidence: expected at least "
                      <> show expected
                      <> ", got "
                      <> show actual
                  )
              )
      assertAtLeast "walk steps" (locationWalkSteps walked) (statLocationWalkSteps stats)
      assertAtLeast "walk maximum" (locationWalkSteps walked) (statLocationMaxWalk stats)
      assertAtLeast
        "fallback count"
        (if locationUsedFallback walked then 1 else 0)
        (statLocationFallbacks stats)
    else do
      assertEqual (label <> " frozen walk steps") (locationWalkSteps walked) (statLocationWalkSteps stats)
      assertEqual (label <> " frozen walk maximum") (locationWalkSteps walked) (statLocationMaxWalk stats)
      assertEqual
        (label <> " frozen fallback count")
        (if locationUsedFallback walked then 1 else 0)
        (statLocationFallbacks stats)
  assertValid (label <> " result") (insertionTriangulation result)
  pure located

-- A vertex hint restricts to one incident face and then relinquishes
-- authority to the exact mutable walk. Valid, stale, occupied, and face-less
-- local sections must therefore glue to the same published result as the
-- unhinted entrance.
testVertexSeededSessionInsertion :: IO ()
testVertexSeededSessionInsertion = do
  area <- sampleMesh "vertex-seeded area" [Point 0 0, Point 4 0, Point 0 4]
  seed <- vertexAt "vertex-seeded area seed" area (Point 0 0)
  assertVertexSeededInsertionAgreement
    "vertex-seeded interior insertion"
    area
    seed
    (SampleVertex (Point 1 1) 40)
  assertVertexSeededInsertionAgreement
    "vertex-seeded invalid hint"
    area
    (VertexId maxBound)
    (SampleVertex (Point 3 3) 41)
  assertVertexSeededInsertionAgreement
    "vertex-seeded duplicate"
    area
    seed
    (SampleVertex (Point 4 0) 42)

  line <- sampleMesh "vertex-seeded line" [Point 0 0, Point 2 0, Point 4 0]
  lineSeed <- vertexAt "vertex-seeded line seed" line (Point 2 0)
  assertVertexSeededInsertionAgreement
    "vertex-seeded face-less fallback"
    line
    lineSeed
    (SampleVertex (Point 3 0) 43)
 where
  sampleMesh
    :: String
    -> [Point]
    -> IO (Triangulation 'Unconstrained SampleVertex () () ())
  sampleMesh label points = do
    built <-
      requireRight label $
        delaunay
          unitElementDefaults
          (V.imap (\index point -> SampleVertex point index) (V.fromList points))
    pure (buildTriangulation built)

  vertexAt
    :: String
    -> Triangulation 'Unconstrained SampleVertex () () ()
    -> Point
    -> IO VertexId
  vertexAt label triangulation point = do
    query <- requireQueryPoint label point
    case locatePoint triangulation query of
      OnVertex vertex -> pure vertex
      location -> fail (label <> " did not locate a vertex: " <> show location)

assertVertexSeededInsertionAgreement
  :: String
  -> Triangulation 'Unconstrained SampleVertex () () ()
  -> VertexId
  -> SampleVertex
  -> IO ()
assertVertexSeededInsertionAgreement label source seed payload = do
  ((referenceVertex, referenceDisposition), reference, _) <-
    requireRight (label <> " unhinted") $
      withSession source 1 (insertVertexAt (samplePosition payload) payload)
  ((hintedVertex, hintedDisposition), hinted, _) <-
    requireRight (label <> " hinted") $
      withSession source 1 (insertVertexAtNearVertex seed (samplePosition payload) payload)
  assertEqual (label <> " vertex") referenceVertex hintedVertex
  assertEqual (label <> " disposition") referenceDisposition hintedDisposition
  assertEqual (label <> " topology") (canonicalEdges reference) (canonicalEdges hinted)
  assertEqual (label <> " payload") payload (vertexData hinted hintedVertex)
  assertValid label hinted

-- Circle sweep must be a construction schedule, not a second topology. It is
-- compared against the arrival-order session kernel on the same exact inputs.
-- | One session, both verbs. The reason the two published sessions became one:
-- a caller who removes and inserts had to thaw twice and pay the O(n)
-- publication a session exists to delete.
--
-- Graded against the oracle that needs no second implementation — the Delaunay
-- triangulation of a point set in general position is unique, so a mixed edit
-- must land exactly where a fresh bulk load of the surviving set lands.
testMixedEditSession :: IO ()
testMixedEditSession = do
  let original = V.fromList (randomPoints 0x5eed1e 400)
      doomed = V.take 120 original
      survivors = V.drop 120 original
      arrivals = V.fromList (randomPoints 0xa7717a1 90)
  (_, edited, stats) <-
    requireRight "mixed session" $
      withSession
        (buildTriangulation (either (error . show) id (delaunay unitElementDefaults original)))
        (V.length arrivals)
        ( do
            V.mapM_ (\point -> removeAt point >>= maybe (refuse (RemovalVertexOutOfRange (VertexId 0) 0)) (const (pure ()))) doomed
            V.mapM_ insertVertex arrivals
        )
  fresh <- requireRight "fresh rebuild" (delaunay unitElementDefaults (survivors <> arrivals))
  assertEqual
    "mixed edit equals a fresh build of the surviving set"
    (canonicalEdges (buildTriangulation fresh))
    (canonicalEdges edited)
  assertValid "mixed edit session" edited
  assertEqual
    "the whole transaction charged one counter set"
    (V.length arrivals)
    (statInputPoints stats)

-- The bulk removal verb must land exactly where the singleton fold lands, on
-- both sides of its locate-strategy crossover: a small batch keeps the
-- per-question walk, a large one buys the identity index once. Same removals,
-- same order, same survivor either way.
testBulkRemovalAgreement :: IO ()
testBulkRemovalAgreement = do
  let original = V.fromList (randomPoints 0xb01dca7 400)
      base = buildTriangulation (either (error . show) id (delaunay unitElementDefaults original))
      run label doomed = do
        (outcomes, survived, _) <-
          requireRight (label <> " bulk removal") (withSession base 0 (removeManyAt doomed))
        V.imapM_
          ( \index outcome ->
              maybe (fail (label <> " bulk removal missed index " <> show index)) (const (pure ())) outcome
          )
          outcomes
        (_, folded, _) <-
          requireRight (label <> " singleton removal fold") $
            withSession
              base
              0
              ( V.mapM_
                  (\point -> removeAt point >>= maybe (refuse (RemovalVertexOutOfRange (VertexId 0) 0)) (const (pure ())))
                  doomed
              )
        assertEqual
          (label <> " bulk removal equals singleton descent")
          (canonicalEdges folded)
          (canonicalEdges survived)
        assertValid (label <> " bulk removal") survived
  run "walking" (V.take 40 original)
  run "indexed" (V.take 120 original)

-- The three fixture positions hash to home slot 15 in the 16-slot table made
-- for three keys. They therefore occupy 15, 0, and 1 in insertion order. The
-- middle deletion exercises both wraparound and backward shift, while the
-- coordinate overwrite models the tail move performed before the table handle
-- is renamed by 'swapRemoveVertex'.
testMutablePointIndexWraparoundBackshift :: IO ()
testMutablePointIndexWraparoundBackshift = do
  (removed, first, movedBeforeRelocation, relocated, movedAfterRelocation) <-
    requireRight "mutable point-index wraparound/backshift law" wraparoundLaw
  case removed of
    MutablePointIndexUpdated -> pure ()
    MutablePointIndexInvalidated -> fail "mutable point-index middle delete invalidated a valid table"
  assertEqual "mutable point-index first wraparound occupant" (Just 0) first
  assertEqual "mutable point-index shifted tail before relocation" (Just 2) movedBeforeRelocation
  case relocated of
    MutablePointIndexUpdated -> pure ()
    MutablePointIndexInvalidated -> fail "mutable point-index tail relocation invalidated a valid table"
  assertEqual "mutable point-index relocated tail" (Just 1) movedAfterRelocation
 where
  wraparoundLaw = runST $ do
    pointXs <- MUV.replicate 3 (0 :: Double)
    pointYs <- MUV.replicate 3 (0 :: Double)
    MUV.unsafeWrite pointXs 0 (-20)
    MUV.unsafeWrite pointYs 0 (-15)
    MUV.unsafeWrite pointXs 1 (-20)
    MUV.unsafeWrite pointYs 1 0
    MUV.unsafeWrite pointXs 2 (-20)
    MUV.unsafeWrite pointYs 2 7
    table <- newMutablePointIndex 3
    seeded <- seedMutablePointIndex table 3 (MUV.unsafeRead pointXs) (MUV.unsafeRead pointYs)
    case seeded of
      Left failure -> pure (Left failure)
      Right () -> do
        -- The tail has moved into slot one before identity transport begins.
        MUV.unsafeWrite pointXs 1 (-20)
        MUV.unsafeWrite pointYs 1 7
        removed <-
          removeMutablePoint table (MUV.unsafeRead pointXs) (MUV.unsafeRead pointYs) (-20) 0 1
        first <-
          lookupMutablePoint table (MUV.unsafeRead pointXs) (MUV.unsafeRead pointYs) (-20) (-15)
        movedBeforeRelocation <-
          lookupMutablePoint table (MUV.unsafeRead pointXs) (MUV.unsafeRead pointYs) (-20) 7
        relocated <- relocateMutablePoint table (-20) 7 2 1
        movedAfterRelocation <-
          lookupMutablePoint table (MUV.unsafeRead pointXs) (MUV.unsafeRead pointYs) (-20) 7
        pure
          ( Right
              ( removed
              , first
              , movedBeforeRelocation
              , relocated
              , movedAfterRelocation
              )
          )

-- A dense table must also close lawfully when it removes every vertex. The
-- point-keyed query after freeze forces the empty published derivation rather
-- than retaining an impossible ST table.
testBatchIdentityIndexBatchToEmpty :: IO ()
testBatchIdentityIndexBatchToEmpty = do
  let points = V.fromList (randomPoints 0x7a110bad 32)
  built <- requireRight "batch identity empty base" (delaunay unitElementDefaults points)
  (outcomes, emptied, _) <-
    requireRight "batch identity removes every point" $
      withSession (buildTriangulation built) 0 (removeManyAt points)
  unless (V.all isJust outcomes) $
    fail "batch identity table missed a point while removing to empty"
  assertEqual "batch identity empty vertex count" 0 (numVertices emptied)
  assertValid "batch identity empty result" emptied
  absent <- requireRight "empty published identity lookup" (locateAndRemove emptied (Point 0 0))
  case absent of
    Nothing -> pure ()
    Just _ -> fail "empty published identity lookup manufactured a removal"

-- A dense batch discards its ST table before the next singleton handle rewrite.
-- 'excise' must therefore activate and transport the ordinary persistent index
-- without consulting the expired batch representation.
testBatchIdentityIndexToSingletonActive :: IO ()
testBatchIdentityIndexToSingletonActive = do
  let original = V.fromList (randomPoints 0x51a91e 64)
      doomed = V.take 32 original
      survivors = V.drop 32 original
  built <- requireRight "batch-to-singleton identity base" (delaunay unitElementDefaults original)
  (singletonOutcome, transitioned, _) <-
    requireRight "batch-to-singleton identity session" $
      withSession (buildTriangulation built) 0 $ do
        _ <- removeManyAt doomed
        excise (VertexId 0)
  let singletonPoint = removalOutcomePoint singletonOutcome
      expected = V.filter (/= singletonPoint) survivors
  fresh <- requireRight "batch-to-singleton fresh rebuild" (delaunay unitElementDefaults expected)
  assertEqual
    "batch-to-singleton identity topology"
    (canonicalEdges (buildTriangulation fresh))
    (canonicalEdges transitioned)
  assertValid "batch-to-singleton identity result" transitioned
  case V.find (/= singletonPoint) survivors of
    Nothing -> fail "batch-to-singleton fixture exhausted every survivor"
    Just remaining -> do
      located <- requireRight "batch-to-singleton published lookup" (locateAndRemove transitioned remaining)
      case located of
        Nothing -> fail "batch-to-singleton published lookup missed a survivor"
        Just removal -> assertValid "batch-to-singleton published removal" (removalTriangulation removal)

-- The mutable identity table is an internal section of @removeManyAt@, never a
-- session-wide owner. An insertion after the dense batch invalidates it, the
-- subsequent removal walks correctly, and the frozen mesh must rederive the
-- published identity cache from its final coordinate authority.
testBulkIdentityIndexDoesNotEscapeRemovalBatch :: IO ()
testBulkIdentityIndexDoesNotEscapeRemovalBatch = do
  let original = V.fromList (randomPoints 0x5a11ce 400)
      doomed = V.take 120 original
      survivors = V.drop 120 original
      arrival = Point (-0.25) 0.75
  baseBuild <- requireRight "bulk identity section base" (delaunay unitElementDefaults original)
  (_, edited, _) <-
    requireRight "bulk identity section mixed session" $
      withSession (buildTriangulation baseBuild) 1 $ do
        _ <- removeManyAt doomed
        _ <- insertVertexAt arrival arrival
        removeAt arrival >>= maybe (refuse (RemovalVertexOutOfRange (VertexId 0) 0)) (const (pure ()))
  fresh <- requireRight "bulk identity section fresh survivor rebuild" (delaunay unitElementDefaults survivors)
  assertEqual
    "bulk identity section mixed program equals fresh survivors"
    (canonicalEdges (buildTriangulation fresh))
    (canonicalEdges edited)
  assertValid "bulk identity section mixed session" edited
  case V.uncons survivors of
    Nothing -> fail "bulk identity section test has no survivor"
    Just (survivor, _) -> do
      located <- requireRight "published lazy identity lookup" (locateAndRemove edited survivor)
      case located of
        Nothing -> fail "published lazy identity lookup missed survivor"
        Just removal -> assertValid "published lazy identity removal" (removalTriangulation removal)

-- The hierarchy-hinted removal program must land exactly where the unhinted
-- session lands. The guesses are all computed against the original base, so
-- later removals in the batch answer for guesses whose slots swap-compaction
-- has renamed — the walk must correct every one of them. The repaired
-- hierarchy must equal the reference rebuild over the same survivor.
testHierarchyRemovalAgreement :: IO ()
testHierarchyRemovalAgreement = do
  let original = V.fromList (randomPoints 0x5eed1e55 400)
      base = buildTriangulation (either (error . show) id (delaunay unitElementDefaults original))
  hierarchy <- requireRight "hierarchy build" (buildHierarchyHint defaultHierarchyBranchFactor base)
  let run label doomed = do
        (outcomes, survived, repaired) <-
          requireRight (label <> " hinted removal") (removeManyWithHierarchy hierarchy base doomed)
        V.imapM_
          ( \index outcome ->
              maybe (fail (label <> " hinted removal missed index " <> show index)) (const (pure ())) outcome
          )
          outcomes
        (_, folded, _) <-
          requireRight (label <> " unhinted session") (withSession base 0 (removeManyAt doomed))
        reference <- requireRight (label <> " reference rebuild") (rebuildHierarchyHint hierarchy folded)
        assertEqual
          (label <> " hinted removal equals unhinted session")
          (canonicalEdges folded)
          (canonicalEdges survived)
        assertEqual (label <> " repaired hierarchy equals reference rebuild") reference repaired
        assertValid (label <> " hinted removal") survived
  run "sparse" (V.take 40 original)
  run "dense" (V.take 120 original)

testCircleSweepBulkLoad :: IO ()
testCircleSweepBulkLoad = do
  let points = V.fromList (randomPoints 0xc1ac1e 1500)
  swept <- requireRight "circle-sweep build" (delaunay unitElementDefaults points)
  (_, sessioned, _) <-
    requireRight "session build" $
      withSession (empty unitElementDefaults) (V.length points) $
        V.mapM_ insertVertex points
  assertEqual
    "circle-sweep/session topology"
    (canonicalEdges sessioned)
    (canonicalEdges (buildTriangulation swept))
  assertValid "circle-sweep build" (buildTriangulation swept)
  assertValid "session build" sessioned

testPredicates :: IO ()
testPredicates = do
  let rawA, rawB, rawC :: Point
      rawA = Point 0 0
      rawB = Point 1 0
      rawC = Point 0 1
  a <- requireRight "predicate point a" (mkQueryPoint rawA)
  b <- requireRight "predicate point b" (mkQueryPoint rawB)
  c <- requireRight "predicate point c" (mkQueryPoint rawC)
  midpointQuery <- requireRight "predicate midpoint" (mkQueryPoint (Point 0.5 0))
  inside <- requireRight "incircle inside point" (mkQueryPoint (Point 0.25 0.25))
  boundary <- requireRight "incircle boundary point" (mkQueryPoint (Point 1 1))
  outside <- requireRight "incircle outside point" (mkQueryPoint (Point 2 2))
  assertEqual "orientation left" GT (orient2d a b c)
  assertEqual "orientation right" LT (orient2d b a c)
  assertEqual "orientation collinear" EQ (orient2d a b midpointQuery)
  assertEqual "incircle inside" GT (inCircle a b c inside)
  assertEqual "incircle boundary" EQ (inCircle a b c boundary)
  assertEqual "incircle outside" LT (inCircle a b c outside)
  assertEqual "underflow mitigation" (Point 0 1) (mitigateUnderflow (Point 1.0e-44 1 :: Point))
  _ <- requireRight "point validation" (validatePoint Nothing (Point 0 1 :: Point))
  let large = encodeFloat 1 180 :: Double
      ulp = encodeFloat 1 128 :: Double
  largeA <- requireRight "large predicate point a" (mkQueryPoint (Point large large))
  largeB <- requireRight "large predicate point b" (mkQueryPoint (Point (large + ulp) large))
  largeC <- requireRight "large predicate point c" (mkQueryPoint (Point large (large + ulp)))
  largeD <- requireRight "large predicate point d" (mkQueryPoint (Point (large + ulp) (large + ulp)))
  assertEqual
    "large exact orientation"
    GT
    (orient2d largeA largeB largeC)
  assertEqual
    "large exact cocircularity"
    EQ
    (inCircle largeA largeB largeC largeD)

testHandlesAndFiniteDcel :: IO ()
testHandlesAndFiniteDcel = do
  built <- requirePointBuild "handle algebra" [Point 0 0, Point 3 0, Point 0 2, Point 0.4 0.7]
  let triangulation = buildTriangulation built
  assertValid "handle algebra" triangulation
  assertEqual "one outer face" (numInnerFaces triangulation + 1) (numFaces triangulation)
  forM_ (directedEdges triangulation) $ \edge -> do
    assertEqual "double reversal" edge (reverseEdge (reverseEdge edge))
    assertEqual "next/previous" edge (previous triangulation (next triangulation edge))
    assertEqual "previous/next" edge (next triangulation (previous triangulation edge))
  forM_ (undirectedEdges triangulation) $ \edge -> do
    let (forward, backward) = directedPair edge
    assertEqual "pair reversal" backward (reverseEdge forward)
    assertEqual "undirected projection" edge (asUndirected forward)
  assertEqual "vertex iterator" (numVertices triangulation) (length (vertices triangulation))
  assertEqual
    "dense vertex projection"
    (V.fromList (fmap (vertexPoint triangulation) (vertices triangulation)))
    (vertexPoints triangulation)
  assertEqual "edge iterator" (numUndirectedEdges triangulation) (length (undirectedEdges triangulation))
  assertEqual "face iterator" (numInnerFaces triangulation) (length (innerFaces triangulation))
  assertEqual
    "dense inner-face directed-edge projection"
    (Just (V.toList (innerFaceDirectedEdgeTriples triangulation)))
    (traverse (Dcel.innerFaceDirectedEdges triangulation) (innerFaces triangulation))
  assertEqual
    "dense inner-face projection"
    (Just (V.toList (innerFaceVertexTriples triangulation)))
    (traverse (Dcel.innerFaceVertices triangulation) (innerFaces triangulation))
  assertEqual "dynamic vertex iterator" (numVertices triangulation) (length (DynamicIterators.vertexHandles triangulation))
  assertEqual "dynamic edge iterator" (numDirectedEdges triangulation) (length (DynamicIterators.directedEdgeHandles triangulation))
  assertEqual "dynamic face iterator" (numInnerFaces triangulation) (length (DynamicIterators.innerFaceHandles triangulation))
  unless (geometryTopologyBytes triangulation > topologyIndexBytes triangulation) $
    fail "geometry byte accounting omitted coordinates"
  vertex0 <- requireJust "dynamic vertex handle" (Dynamic.vertexHandle triangulation (VertexId 0))
  assertEqual "dynamic vertex fix" (VertexId 0) (Dynamic.fixVertex vertex0)
  assertEqual "dynamic vertex position" (vertexPoint triangulation (VertexId 0)) (Dynamic.vertexHandlePosition vertex0)
  case Dynamic.vertexHandleOutEdge vertex0 of
    Nothing -> fail "connected dynamic vertex has no outgoing edge"
    Just edge -> do
      assertEqual "dynamic edge reversal" (Dynamic.fixDirectedEdge edge) (Dynamic.fixDirectedEdge (Dynamic.directedEdgeReverse (Dynamic.directedEdgeReverse edge)))
      assertEqual "dynamic edge next/previous" (Dynamic.fixDirectedEdge edge) (Dynamic.fixDirectedEdge (Dynamic.directedEdgePrevious (Dynamic.directedEdgeNext edge)))
      let face = Dynamic.directedEdgeFace edge
      if Dynamic.faceIsOuter face
        then pure ()
        else case Dynamic.faceAsInner face of
          Nothing -> fail "non-outer dynamic face did not refine to InnerTag"
          Just inner -> do
            _ <- requireJust "inner dynamic face vertices" (Dynamic.innerFaceVertices inner)
            case Dynamic.innerFaceCircumcenter inner of
              Nothing -> fail "inner dynamic face has no circumcenter"
              Just _ -> pure ()

testDegenerateConstruction :: IO ()
testDegenerateConstruction = do
  emptyBuild <- requirePointBuild "empty" []
  emptyQuery <- requireQueryPoint "empty location" (Point 0 0)
  assertEqual "empty vertices" 0 (numVertices (buildTriangulation emptyBuild))
  assertEqual "empty location" EmptyTriangulation (locatePoint (buildTriangulation emptyBuild) emptyQuery)
  assertValid "empty" (buildTriangulation emptyBuild)

  singleton <- requirePointBuild "singleton" [Point 2 3]
  singletonQuery <- requireQueryPoint "singleton lookup" (Point 2 3)
  assertEqual "singleton lookup" (OnVertex (VertexId 0)) (locatePoint (buildTriangulation singleton) singletonQuery)
  assertValid "singleton" (buildTriangulation singleton)

  duplicates <- requirePointBuild "duplicates" [Point 0 0, Point 1 0, Point (-0.0) 0, Point 1 0, Point 2 0]
  assertEqual "deduplicated count" 3 (numVertices (buildTriangulation duplicates))
  assertEqual
    "stable duplicate mapping"
    (primArrayFromList [0, 1, 0, 1, 2])
    (buildInputVertices duplicates)
  assertValid "duplicates" (buildTriangulation duplicates)

  -- Deduplicating a signed zero is not the same claim as storing it canonically:
  -- the first only needs '==', which already identifies the two, while anything
  -- reading the bit pattern — a radix ordering, a byte-for-byte cross-check —
  -- reads the sign bit and gets a coordinate below every other. Both ingest
  -- paths canonicalize, and both are held to it here rather than to the weaker
  -- statement that comparison happens to survive.
  let bits point = (castDoubleToWord64 (pointX point), castDoubleToWord64 (pointY point))
  signedZero <- requirePointBuild "signed zero" [Point (-0.0) (-0.0), Point 1 0, Point 0 1]
  assertEqual "bulk load stores a canonical zero"
    (0, 0) (bits (vertexPoint (buildTriangulation signedZero) (VertexId 0)))
  incremental <-
    requireRight "signed zero insert"
      (insert (empty unitElementDefaults :: DelaunayTriangulation (Point)) (Point (-0.0) (-0.0)))
  assertEqual "insertion stores a canonical zero"
    (0, 0) (bits (vertexPoint (insertionTriangulation incremental) (VertexId 0)))

  traverse_
    assertBulkLine
    [ ("vertical", [Point 0 4, Point 0 0, Point 0 3, Point 0 2, Point 0 1])
    , ("horizontal reversed", reverse [Point x 7 | x <- [0 .. 8]])
    , ("oblique scrambled", [Point 3 8, Point (-2) (-2), Point 1 4, Point (-1) 0, Point 2 6, Point 0 2])
    , ("duplicate-bearing", [Point 2 5, Point 0 1, Point 1 3, Point 2 5, Point (-0.0) 1, Point 3 7])
    ]
  areaBase <- requirePointBuild "resident area before collinear batch" [Point 0 0, Point 4 0, Point 0 4]
  areaExtended <-
    requireRight
      "collinear batch into resident area"
      (insertMany (buildTriangulation areaBase) (V.fromList [Point 1 1, Point 2 2, Point 3 3]))
  assertEqual "resident area collinear batch count" 6 (numVertices (buildTriangulation areaExtended))
  assertValid "resident area collinear batch" (buildTriangulation areaExtended)
 where
  assertBulkLine (label, points) = do
    annotated <- requirePointBuild (label <> " annotated line") points
    geometry <- requireRight (label <> " geometry line") (delaunayGeometry (V.fromList points))
    batched <-
      requireRight
        (label <> " empty-base batch line")
        (insertMany (empty unitElementDefaults) (V.fromList points))
    let line = buildTriangulation annotated
        batchLine = buildTriangulation batched
        ascending = Set.toAscList (Set.fromList points)
        expectedEdges = Set.fromList (zip ascending (drop 1 ascending))
        uniqueCount = length ascending
        stats = buildStats annotated
    assertEqual (label <> " annotated edge set") expectedEdges (canonicalEdges line)
    assertEqual (label <> " geometry edge set") expectedEdges (canonicalEdges geometry)
    assertEqual (label <> " empty-base batch edge set") expectedEdges (canonicalEdges batchLine)
    assertEqual (label <> " edge count") (max 0 (uniqueCount - 1)) (numUndirectedEdges line)
    assertEqual (label <> " face count") 0 (numInnerFaces line)
    assertEqual (label <> " spatial seed count") uniqueCount (statSpatialSeedPoints stats)
    assertEqual (label <> " aggregate extension count") (max 0 (uniqueCount - 2)) (statLineExtensions stats)
    assertValid (label <> " annotated line") line
    assertValid (label <> " geometry line") geometry
    assertValid (label <> " empty-base batch line") batchLine

testPersistentLocalUpdates :: IO ()
testPersistentLocalUpdates = do
  base <- requirePointBuild "persistent base" (randomPoints 0x5eed 4095)
  let triangulation = buildTriangulation base
      query = Point 0.000_123 (-0.000_271)
  inserted <- requireRight "persistent insert" (insert triangulation query)
  assertEqual "persistent source remains unchanged" 4095 (numVertices triangulation)
  assertEqual "persistent result appends one vertex" 4096 (numVertices (insertionTriangulation inserted))
  assertValid "persistent insertion result" (insertionTriangulation inserted)

  let payloads = V.fromList
        [ SampleVertex (Point 0 0) 10
        , SampleVertex (Point 1 0) 20
        , SampleVertex (Point 0 1) 30
        ]
      defaults = ElementDefaults (0 :: Int) False ("face" :: String)
  payloadBuild <- requireRight "payload base" (delaunay defaults payloads)
  duplicate <- requireRight "payload-only replacement" (insert (buildTriangulation payloadBuild) (SampleVertex (Point 1 0) 99))
  assertEqual "payload update disposition" AlreadyPresent (insertionDisposition duplicate)
  assertEqual "payload replacement" 99 (sampleLabel (vertexData (insertionTriangulation duplicate) (VertexId 1)))

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
    (Internal.triElementDefaults published)
  assertEqual
    "geometry-only publication prepares the exact seam frontier"
    (Internal.prepareSeamFrontierIndex source)
    (Internal.triSeamFrontier published)
  assertEqual
    "geometry-only publication preserves an existing seam frontier"
    (Internal.triSeamFrontier published)
    (Internal.triSeamFrontier republished)
  assertValid "geometry-only publication" published
 where
  sumPayloadPages (vertexPages, directedPages, undirectedPages, facePages) =
    vertexPages + directedPages + undirectedPages + facePages
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
    (map (faceData sample) (allFaces sample) <> [defaultFaceData (Internal.triElementDefaults sample)])
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
testParaboloidLift :: IO ()
testParaboloidLift = do
  let lifted :: Point -> (Rational, Rational, Rational)
      lifted (Point x y) =
        let (rx, ry) = (toRational x, toRational y) in (rx, ry, rx * rx + ry * ry)
      minor3
        :: (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> Rational
      minor3 (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) =
        a1 * (b2 * c3 - b3 * c2) - a2 * (b1 * c3 - b3 * c1) + a3 * (b1 * c2 - b2 * c1)
      -- Laplace expansion of the lifted determinant along its column of ones.
      liftedOrientation
        :: (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> Rational
      liftedOrientation a b c d =
        negate (minor3 b c d) + minor3 a c d - minor3 a b d + minor3 a b c
      predicted :: Point -> Point -> Point -> Point -> Ordering
      predicted a b c d = compare (liftedOrientation (lifted a) (lifted b) (lifted c) (lifted d)) 0
      measured :: Point -> Point -> Point -> Point -> Ordering
      measured (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy) =
        Admitted.inCircleCoordinates ax ay bx by cx cy dx dy
      quadruples :: [Point] -> [(Point, Point, Point, Point)]
      quadruples (a : b : c : d : rest) = (a, b, c, d) : quadruples rest
      quadruples _ = []

  forM_ (quadruples (randomPoints 0x51ca_0b17 1600)) $ \(a, b, c, d) ->
    assertEqual ("in-circle is the lifted orientation at " <> show (a, b, c, d))
      (predicted a b c d) (measured a b c d)

  -- Exactly cocircular quadruples, which are exactly the ones the floating
  -- filter must decline to answer. Every Pythagorean point of the radius-5
  -- circle is representable without rounding, so 'EQ' here is a fact about the
  -- geometry rather than about the arithmetic.
  let ring =
        [ Point x y
        | (x, y) <-
            [ (5, 0), (0, 5), (-5, 0), (0, -5)
            , (3, 4), (4, 3), (-3, 4), (-4, 3)
            , (3, -4), (4, -3), (-3, -4), (-4, -3)
            ]
        ] :: [Point]
      indexed = zip [(0 :: Int) ..] ring
      cocircular =
        [ (a, b, c, d)
        | (i, a) <- indexed, (j, b) <- indexed, j > i
        , (k, c) <- indexed, k > j, (l, d) <- indexed, l > k
        ]
  unless (length cocircular == 495) $ fail ("expected 495 cocircular quadruples, got " <> show (length cocircular))
  forM_ cocircular $ \(a, b, c, d) -> do
    assertEqual ("cocircular points are cocircular at " <> show (a, b, c, d)) EQ (measured a b c d)
    assertEqual ("the lift agrees on cocircularity at " <> show (a, b, c, d)) EQ (predicted a b c d)

  -- The geometry the sign means, stated once against a circle anyone can read.
  let (a, b, c) = (Point 1 0, Point 0 1, Point (-1) 0) :: (Point, Point, Point)
  assertEqual "the centre is inside the circle" GT (measured a b c (Point 0 0))
  assertEqual "the antipode is on the circle" EQ (measured a b c (Point 0 (-1)))
  assertEqual "a distant point is outside" LT (measured a b c (Point 2 2))

-- | A payload labels an element, and an element is its geometry. Every rewrite
-- an insertion performs — an edge split, a face split, a Lawson flip — hands
-- some slot a different element to hold, and the label the displaced one
-- carried does not describe what took its place.
--
-- So: label every element of a triangulation by its own key, insert a point,
-- and demand that an element whose key survives still carries exactly the label
-- it was given while every element whose key is new carries the default and
-- nothing else. Both directions are checked; either alone is satisfiable by a
-- store that throws everything away.
--
-- The flip is the load-bearing case. Legalization is confluent, so which flips
-- fire and in what order is not observable in the topology that comes out. A
-- payload that rode through a flip would make it observable in the payload
-- plane, and a triangulation that is a normal form in one component and a
-- history in another is not a normal form.
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

faceKeyOf
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> [Point]
faceKeyOf triangulation face = sort (map (vertexPoint triangulation) (faceVertices triangulation face))

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

testPointLocationAndHints :: IO ()
testPointLocationAndHints = do
  built <- requirePointBuild "location" (randomPoints 0x1234_5678 1200)
  let triangulation = buildTriangulation built
  queries <- traverse (requireQueryPoint "location query") (take 250 (randomPoints 0xdead_beef 250))
  let
      baseline = sum [locationWalkSteps stats | query <- queries, let (_, stats) = locatePointWithHint triangulation Nothing query]
  hierarchy <- requireRight "hierarchy build" (buildHierarchyHint 16 triangulation)
  let hinted = sum
        [ locationWalkSteps stats
        | query <- queries
        , let hint = hierarchyHint hierarchy query
              (_, stats) = locatePointWithHint triangulation hint query
        ]
  unless (hinted <= baseline) $
    fail ("hierarchy hint increased aggregate walking: " <> show (baseline, hinted))
  staleHintQuery <- requireQueryPoint "stale hint query" (Point 0.125 (-0.375))
  let staleVertexHint = VertexHint (VertexId (fromIntegral (numVertices triangulation)))
  assertEqual
    "stale vertex hint falls back to the canonical start face"
    (locatePointWithHint triangulation Nothing staleHintQuery)
    (locatePointWithHint triangulation (Just staleVertexHint) staleHintQuery)
  forM_ (vertices triangulation) $ \vertex -> do
    vertexQuery <- requireQueryPoint "vertex lookup" (vertexPoint triangulation vertex)
    assertEqual "vertex lookup" (OnVertex vertex) (locatePoint triangulation vertexQuery)
  let insertedPoint = Point 0.1234567 (-0.2345678)
  inserted <- requireRight "hierarchy incremental source" (insert triangulation insertedPoint)
  let updatedTriangulation = insertionTriangulation inserted
  updatedHierarchy <-
    requireRight
      "hierarchy incremental update"
      ( updateHierarchyAfterInsertion
          hierarchy
          insertedPoint
          (insertionVertex inserted)
          (insertionDisposition inserted)
      )
  rebuiltHierarchy <- requireRight "hierarchy reference rebuild" (buildHierarchyHint 16 updatedTriangulation)
  assertEqual "incremental hierarchy equals canonical rebuild" rebuiltHierarchy updatedHierarchy

-- The hierarchy replaces its level-to-base correspondence with the arithmetic
-- claim that level-local handle @j@ names base vertex @j * branch@. Query each
-- sampled vertex with its own position: the descent must return that very
-- vertex, at every branch factor and along the whole of level 0. A stride the
-- construction does not actually obey shows up here as a named mismatch.
testHierarchyNestingLaw :: IO ()
testHierarchyNestingLaw = do
  built <- requirePointBuild "hierarchy nesting" (randomPoints 0x0f1e_2d3c 900)
  let triangulation = buildTriangulation built
  forM_ [2, 3, 16] $ \branch -> do
    hierarchy <- requireRight ("hierarchy build at branch " <> show branch) (buildHierarchyHint branch triangulation)
    let sampled = [0, branch .. numVertices triangulation - 1]
    forM_ sampled $ \index -> do
      let vertex = VertexId (fromIntegral index)
      vertexQuery <- requireQueryPoint "sampled hierarchy vertex" (vertexPoint triangulation vertex)
      assertEqual
        ("branch " <> show branch <> " descent onto sampled vertex " <> show index)
        (Just (VertexHint vertex))
        (hierarchyHint hierarchy vertexQuery)

testSibsonInterpolation :: IO ()
testSibsonInterpolation = do
  built <- requirePointBuild "sibson" (gridPoints 9 9)
  let triangulation = buildTriangulation built
      queryPoint = Point 3.25 4.4
      linear vertex = let Point x y = vertexPoint triangulation vertex in 2 * x - 3 * y + 5
      expected = let Point x y = queryPoint in 2 * x - 3 * y + 5
  query <- requireQueryPoint "Sibson query" queryPoint
  workspace <- stToIO (newNaturalNeighborWorkspace triangulation)
  result <- stToIO (naturalNeighborWeights workspace Nothing query)
  let weights = naturalNeighborValues result
  unless (V.length weights >= 3) $ fail "Sibson query did not discover a natural-neighbor cavity"
  assertNear "Sibson partition" 1.0e-11 1 (V.sum (V.map snd weights))
  unless (V.all ((>= (-1.0e-12)) . snd) weights) $ fail "Sibson produced a negative weight"
  (folded, _, foldedStats) <- stToIO (foldNaturalNeighborWeights
    (\total vertex weight -> total + weight * linear vertex)
    0
    workspace
    Nothing
    query)
  assertNear "allocation-free Sibson fold" 2.0e-9 expected folded
  assertEqual "Sibson fold neighbor count" (V.length weights) (interpolationNaturalNeighbors foldedStats)
  (interpolated, _) <- stToIO (interpolateNaturalNeighbor linear workspace Nothing query)
  case interpolated of
    Nothing -> fail "Sibson interpolation rejected an interior query"
    Just value -> assertNear "Sibson affine precision" 2.0e-9 expected value
  let gradients = estimateGradients linear triangulation
  V.forM_ gradients $ \(gx, gy) -> do
    assertNear "planar gradient x" 1.0e-9 2 gx
    assertNear "planar gradient y" 1.0e-9 (-3) gy
  let gradient (VertexId raw) = gradients V.! fromIntegral raw
  (gradientValue, _) <- stToIO (interpolateNaturalNeighborGradient linear gradient 0.5 workspace Nothing query)
  case gradientValue of
    Nothing -> fail "gradient natural-neighbor interpolation rejected an interior query"
    Just value -> assertNear "gradient affine precision" 2.0e-9 expected value
  unless (workspaceBytes workspace > 0) $ fail "Sibson workspace byte accounting is empty"

testVoronoiDual :: IO ()
testVoronoiDual = do
  built <- requirePointBuild "voronoi" [Point 0 0, Point 2 0, Point 0 2, Point 2 2, Point 1 1]
  let triangulation = buildTriangulation built
  assertEqual "Voronoi face count" (numVertices triangulation) (length (voronoiFaces triangulation))
  assertEqual "directed dual edge count" (numDirectedEdges triangulation) (length (directedVoronoiEdges triangulation))
  assertEqual "undirected dual edge count" (numUndirectedEdges triangulation) (length (undirectedVoronoiEdges triangulation))
  forM_ (directedVoronoiEdges triangulation) $ \edge -> do
    assertEqual "dual double reversal" edge (reverseVoronoiEdge (reverseVoronoiEdge edge))
    assertEqual "dual next/previous" edge (voronoiPrevious triangulation (voronoiNext triangulation edge))
    assertEqual
      "dual face/site"
      (origin triangulation (asDelaunayDirectedEdge edge))
      (voronoiFaceSite (voronoiIncidentFace triangulation edge))
    case voronoiEdgeGeometry triangulation edge of
      Nothing -> fail "valid dual edge has no geometry"
      fixedGeometry@(Just _) -> do
        owning <- requireJust "dynamic Voronoi edge" (VoronoiDynamic.directedVoronoiEdgeHandle triangulation edge)
        assertEqual "fixed/owning dual geometry" fixedGeometry (VoronoiDynamic.voronoiEdgeGeometryH owning)
  case directedVoronoiEdges triangulation of
    [] -> fail "Voronoi test produced no directed dual edge"
    first : _ -> do
      handle <- requireJust "dynamic Voronoi edge" (VoronoiDynamic.directedVoronoiEdgeHandle triangulation first)
      assertEqual "dynamic dual fix" first (VoronoiDynamic.fixDirectedVoronoiEdge handle)
      assertEqual "dynamic dual reversal" first (VoronoiDynamic.fixDirectedVoronoiEdge (VoronoiDynamic.voronoiEdgeReverseH (VoronoiDynamic.voronoiEdgeReverseH handle)))
      assertEqual "dynamic dual/primal conversion" (asDelaunayDirectedEdge first) (Dynamic.fixDirectedEdge (VoronoiDynamic.voronoiEdgeAsDelaunayH handle))
      let dualFace = VoronoiDynamic.voronoiEdgeFaceH handle
      assertEqual "dynamic dual face site" (origin triangulation (asDelaunayDirectedEdge first)) (Dynamic.fixVertex (VoronoiDynamic.voronoiFaceSiteH dualFace))
      let source = VoronoiDynamic.voronoiEdgeFromH handle
      case VoronoiDynamic.voronoiVertexAsDelaunayFaceH source of
        Just inner -> case VoronoiDynamic.voronoiVertexPositionH source of
          Nothing -> fail "inner dynamic Voronoi vertex has no position"
          Just voronoiPosition -> assertEqual "inner dynamic Voronoi position" (Dynamic.innerFaceCircumcenter inner) (Just voronoiPosition)
        Nothing -> unless (isJust (VoronoiDynamic.voronoiVertexAsOuterEdgeH source)) $
          fail "outer dynamic Voronoi vertex has no defining edge"

testRemoval :: IO ()
testRemoval = do
  built <- requirePointBuild "removal" [Point 0 0, Point 3 0, Point 3 3, Point 0 3, Point 1.5 1.5]
  let triangulation = buildTriangulation built
  assertEqual
    "out-of-range removal obstruction"
    (Left (RemovalVertexOutOfRange (VertexId 5) 5))
    (void (removeVertex triangulation (VertexId 5)))
  removedInterior <- requireRight "interior removal" (removeVertex triangulation (VertexId 4))
  assertEqual "interior removed point" (Point 1.5 1.5) (removalOutcomePoint (removalOutcome removedInterior))
  assertEqual "interior removal count" 4 (numVertices (removalTriangulation removedInterior))
  assertValid "interior removal" (removalTriangulation removedInterior)
  let beforeHullRemoval = removalTriangulation removedInterior
      previousLastVertex = VertexId (fromIntegral (numVertices beforeHullRemoval - 1))
      swappedPoint = vertexPoint beforeHullRemoval previousLastVertex
      swappedData = vertexData beforeHullRemoval previousLastVertex
  removedHull <- requireRight "hull removal" (removeVertex beforeHullRemoval (VertexId 0))
  assertEqual "hull removal count" 3 (numVertices (removalTriangulation removedHull))
  case removalOutcomeSwap (removalOutcome removedHull) of
    Nothing -> fail "hull removal omitted the swap-compacted vertex handle"
    Just (swappedIn, swappedInPoint) -> do
      assertEqual "hull swapped-in handle" (VertexId 0) swappedIn
      assertEqual "hull swapped-in point" swappedPoint (vertexPoint (removalTriangulation removedHull) swappedIn)
      assertEqual "hull swapped-in payload" swappedData (vertexData (removalTriangulation removedHull) swappedIn)
      -- The reported position must be the one the arena now holds, bit for
      -- bit: a caller seeding a search from it is seeding from the mesh.
      assertEqual "hull swapped-in reported position" swappedPoint swappedInPoint
  assertValid "hull removal" (removalTriangulation removedHull)

  let afterHullRemoval = removalTriangulation removedHull
      lastVertex = VertexId (fromIntegral (numVertices afterHullRemoval - 1))
  removedLast <- requireRight "last-vertex removal" (removeVertex afterHullRemoval lastVertex)
  assertEqual
    "removing the last vertex relocates nothing"
    Nothing
    (removalOutcomeSwap (removalOutcome removedLast))
  assertEqual "last-vertex removal count" 2 (numVertices (removalTriangulation removedLast))
  assertValid "last-vertex removal" (removalTriangulation removedLast)

  lineBuild <- requirePointBuild "line removal" [Point 0 0, Point 1 0, Point 2 0, Point 3 0]
  lineMiddle <- requireRight "line middle removal" (removeVertex (buildTriangulation lineBuild) (VertexId 1))
  assertEqual "line middle count" 3 (numVertices (removalTriangulation lineMiddle))
  assertValid "line middle removal" (removalTriangulation lineMiddle)

  degreeThreeBuild <-
    requirePointBuild
      "degree-three removal"
      [Point 0 0, Point 4 0, Point 0 4, Point 1 1]
  let degreeThreeMapping = buildInputVertices degreeThreeBuild
  case if 3 < sizeofPrimArray degreeThreeMapping
        then Just (VertexId (indexPrimArray degreeThreeMapping 3))
        else Nothing of
    Nothing -> fail "degree-three removal input mapping omitted the interior vertex"
    Just centerVertex -> do
      degreeThree <-
        requireRight
          "degree-three interior removal"
          (removeVertex (buildTriangulation degreeThreeBuild) centerVertex)
      assertEqual "degree-three removal count" 3 (numVertices (removalTriangulation degreeThree))
      assertValid "degree-three interior removal" (removalTriangulation degreeThree)

  -- A removal hands its vertex's whole ring to edge/face cleanup, so a
  -- high-degree star is the only shape that exercises the ordered-set path
  -- there; an ordinary mesh keeps degrees near six and never leaves the
  -- insertion sort. Radii are jittered so no four rim points are cocircular.
  let rimCount = 48
      rimPoint index =
        let angle = 2 * pi * fromIntegral index / fromIntegral rimCount
            radius = 1 + 0.001 * fromIntegral (index `mod` 7)
         in Point (radius * cos angle) (radius * sin angle)
  starBuild <-
    requirePointBuild
      "high-degree removal"
      (Point 0 0 : map rimPoint [0 .. rimCount - 1])
  let starTriangulation = buildTriangulation starBuild
      starCentre = VertexId (indexPrimArray (buildInputVertices starBuild) 0)
      centreDegree =
        length
          [ ()
          | edge <- undirectedEdges starTriangulation
          , let (from, to) = undirectedEndpoints starTriangulation edge
          , from == starCentre || to == starCentre
          ]
  assertEqual "high-degree centre ring" rimCount centreDegree
  starRemoved <-
    requireRight "high-degree interior removal" (removeVertex starTriangulation starCentre)
  assertEqual "high-degree removal count" rimCount (numVertices (removalTriangulation starRemoved))
  assertEqual
    "high-degree removed point"
    (Point 0 0)
    (removalOutcomePoint (removalOutcome starRemoved))
  assertValid "high-degree interior removal" (removalTriangulation starRemoved)

  -- A transaction that refuses publishes nothing: the refusal is the whole
  -- answer, so no half-remeshed arena can reach a caller as a triangulation.
  let sessionRefusal = RemovalVertexOutOfRange (VertexId 99) 5
  assertEqual
    "a refused session publishes nothing"
    (Left sessionRefusal)
    (void (withSession triangulation 1 (refuse sessionRefusal :: Session s (Point) () () () ())))

  -- Refusal short-circuits: an edit after it never runs, so the mesh the
  -- transaction abandoned is the mesh it was handed.
  assertEqual
    "a refusal abandons the edits behind it"
    (Left sessionRefusal)
    ( void
        ( withSession
            triangulation
            1
            ( ( do
                  _ <- removeAt (Point 0 0)
                  _ <- refuse sessionRefusal
                  removeAt (Point 1 1)
              ) ::
                Session s (Point) () () () (Maybe (RemovalOutcome (Point)))
            )
        )
    )

  -- The two point-keyed entries must publish one story. A handle-keyed removal
  -- locates nothing, while the coordinate-keyed route resolves the same site
  -- through the exact derived identity section rather than a topological walk.
  -- Both therefore charge no location steps and retire the same vertex.
  handleRemoval <- requireRight "handle removal stats" (removeVertex triangulation (VertexId 4))
  assertEqual
    "a handle-keyed removal locates nothing"
    0
    (statLocationWalkSteps (removalStats handleRemoval))
  locatedRemoval <-
    requireRight "point removal stats" (locateAndRemove triangulation (Point 1.5 1.5))
  pointRemoval <- requireJust "point removal located a vertex" locatedRemoval
  assertEqual
    "a point-keyed removal performs no topological location walk"
    0
    (statLocationWalkSteps (removalStats pointRemoval))
  assertEqual
    "point-keyed and handle-keyed removal publish the same mesh"
    (canonicalEdges (removalTriangulation handleRemoval))
    (canonicalEdges (removalTriangulation pointRemoval))

testConstrainedDelaunay :: IO ()
testConstrainedDelaunay = do
  base <- requirePointBuild "CDT base" [Point 0 0, Point 4 0, Point 4 4, Point 0 4, Point 1 1, Point 3 3, Point 1 3, Point 3 1]
  let cdt0 = fromDelaunay (buildTriangulation base)
      invalidPoint = Point (0 / 0) 0
  case addConstraintEdge cdt0 invalidPoint (Point 4 0) of
    Left (CdtBuildError (InvalidCoordinate Nothing _ CoordinateNaN)) -> pure ()
    outcome -> fail ("invalid first constraint endpoint was admitted: " <> either show (const "success") outcome)
  case addConstraintEdge cdt0 (Point 0 0) invalidPoint of
    Left (CdtBuildError (InvalidCoordinate Nothing _ CoordinateNaN)) -> pure ()
    outcome -> fail ("invalid second constraint endpoint was admitted: " <> either show (const "success") outcome)
  assertEqual
    "invalid endpoint refusal leaves the immutable source unpublished"
    (8, 0)
    (numVertices cdt0, numConstraints cdt0)
  partlyResident <-
    requireRight
      "one resident one new constraint endpoint"
      (addConstraintEdge cdt0 (Point 0 0) (Point 2 2))
  let partlyResidentCdt = constraintRecoveryTriangulation partlyResident
  assertEqual
    "one resident endpoint reserves only the missing site"
    (numVertices cdt0 + 1)
    (numVertices partlyResidentCdt)
  assertCdtValid "one resident one new constraint endpoint" partlyResidentCdt
  diagonalBatch <-
    requireRight
      "constraint recovery"
      (recoverConstraints cdt0 (V.singleton (VertexId 0, VertexId 2)))
  (diagonalPath, diagonalAdded) <-
    requireAcceptedConstraint "constraint recovery" diagonalBatch
  let cdt1 = constraintBatchTriangulation diagonalBatch
  when (V.null diagonalPath) $ fail "constraint recovery returned an empty path"
  assertEqual "constraint count" diagonalAdded (numConstraints cdt1)
  assertCdtValid "constraint recovery" cdt1
  conflictBatch <-
    requireRight
      "atomic conflict"
      (recoverConstraints cdt1 (V.singleton (VertexId 1, VertexId 3)))
  case V.toList (constraintBatchOutcomes conflictBatch) of
    [ConstraintRejected _] -> pure ()
    outcomes ->
      fail
        ( "crossing constraint was not rejected atomically: "
            <> show outcomes
        )
  assertEqual
    "crossing rejection preserves topology"
    cdt1
    (constraintBatchTriangulation conflictBatch)
  let constraintProgram =
        V.fromList
          [ (VertexId 0, VertexId 2)
          , (VertexId 1, VertexId 3)
          , (VertexId 4, VertexId 6)
          ]
  wholeProgram <-
    requireRight
      "whole constraint program"
      (recoverConstraints cdt0 constraintProgram)
  (singletonTriangulation, singletonOutcomes) <-
    requireRight
      "singleton constraint program"
      (V.foldM' replayConstraintRequest (cdt0, []) constraintProgram)
  assertEqual
    "batch outcomes equal singleton descent"
    (V.toList (constraintBatchOutcomes wholeProgram))
    (reverse singletonOutcomes)
  assertEqual
    "batch topology equals singleton descent"
    singletonTriangulation
    (constraintBatchTriangulation wholeProgram)
  assertBatchStats "whole constraint program" wholeProgram
  forM_ [0 .. 7 :: Int] $ \batchIndex -> do
    randomizedBuild <-
      requirePointBuild
        ("random constraint batch " <> show batchIndex)
        (randomPoints (0x6a09_e667_f3bc_c909 + fromIntegral batchIndex) 40)
    let randomizedBase = fromDelaunay (buildTriangulation randomizedBuild)
        mapping = buildInputVertices randomizedBuild
        handles = V.generate (sizeofPrimArray mapping) (VertexId . indexPrimArray mapping)
        requests = V.take 18 (V.zip handles (V.reverse handles))
    randomizedBatch <-
      requireRight
        ("random whole constraint batch " <> show batchIndex)
        (recoverConstraints randomizedBase requests)
    (randomizedSingleton, randomizedOutcomes) <-
      requireRight
        ("random singleton constraint batch " <> show batchIndex)
        (V.foldM' replayConstraintRequest (randomizedBase, []) requests)
    assertEqual
      ("random batch outcomes equal singleton descent " <> show batchIndex)
      (V.toList (constraintBatchOutcomes randomizedBatch))
      (reverse randomizedOutcomes)
    assertEqual
      ("random batch topology equals singleton descent " <> show batchIndex)
      randomizedSingleton
      (constraintBatchTriangulation randomizedBatch)
    assertBatchStats
      ("random constraint batch " <> show batchIndex)
      randomizedBatch
    assertCdtValid
      ("random constraint batch " <> show batchIndex)
      (constraintBatchTriangulation randomizedBatch)
  -- Batch admission returns the first typed obstruction. Callers that need a
  -- complete diagnosis ask the explicit corridor query and alone pay for it.
  diagnosisBatch <-
    requireRight
      "conflict diagnosis"
      (recoverConstraints cdt1 (V.singleton (VertexId 6, VertexId 7)))
  case V.toList (constraintBatchOutcomes diagnosisBatch) of
    [ConstraintRejected blocking] -> do
      let diagnosed =
            fmap
              asUndirected
              (getConflictingEdgesBetweenVertices cdt1 (VertexId 6) (VertexId 7))
      case diagnosed of
        [] -> fail "explicit conflict diagnosis named no edge"
        firstBlocking : _ ->
          assertEqual "batch rejection is the first corridor obstruction" firstBlocking blocking
      assertEqual
        "explicit conflict diagnosis is deduplicated"
        (length diagnosed)
        (length (Set.fromList diagnosed))
      forM_ diagnosed $ \edge ->
        unless (isConstraintEdge cdt1 edge) $
          fail ("explicit conflict diagnosis named a non-constraint edge: " <> show edge)
    outcomes ->
      fail
        ( "a constraint crossing the diagonal was not rejected: "
            <> show outcomes
        )

  split <- requireRight "constraint split" (addConstraintAndSplit id cdt1 (VertexId 1) (VertexId 3))
  let splitCdt = constraintRecoveryTriangulation split
  unless (numVertices splitCdt > numVertices cdt1) $ fail "constraint split did not insert an intersection vertex"
  assertCdtValid "constraint split" splitCdt

  -- The batch splitter must agree with singleton descent, including across a
  -- suspension: every vertical below is constrained only inside the batch
  -- itself, so no census against the base can reserve for its crossings and
  -- the chunk's vertex reservation exhausts mid-batch. The driver publishes,
  -- re-reserves against the published mesh, and resumes; the final mesh must
  -- not know any of that happened.
  let bandColumns = [0 .. 7 :: Int]
      bandVertices =
        V.fromList
          ( Point 0 5
              : Point 90 5
              : concat
                  [ [Point x 10, Point x 0]
                  | column <- bandColumns
                  , let x = 10 * fromIntegral column + 5
                  ]
          )
  bandBuild <-
    requireRight
      "split band base"
      (constrainedDelaunayMaximal unitElementDefaults bandVertices V.empty)
  let bandAcceptedBuild = cdtAcceptedBuild bandBuild
      bandMapping = buildInputVertices bandAcceptedBuild
      bandHandle input = VertexId (indexPrimArray bandMapping input)
      bandBase = buildTriangulation bandAcceptedBuild
      bandRequests =
        V.fromList
          ( (bandHandle 0, bandHandle 1)
              : [ (bandHandle (2 * column + 2), bandHandle (2 * column + 3))
                | column <- bandColumns
                ]
          )
      replaySplit
        :: ConstrainedDelaunayTriangulation (Point)
        -> (VertexId, VertexId)
        -> Either (CdtError) (ConstrainedDelaunayTriangulation (Point))
      replaySplit triangulation request =
        constraintRecoveryTriangulation
          <$> uncurry (addConstraintAndSplit id triangulation) request
  bandBatch <-
    requireRight
      "split batch"
      (addConstraintsAndSplit id bandBase bandRequests)
  bandDescent <-
    requireRight
      "split singleton descent"
      (V.foldM' replaySplit bandBase bandRequests)
  assertEqual
    "split batch topology equals singleton descent"
    bandDescent
    (constraintRecoveryTriangulation bandBatch)
  assertEqual
    "split batch added one vertex per crossing"
    (numVertices bandBase + length bandColumns)
    (numVertices (constraintRecoveryTriangulation bandBatch))
  assertCdtValid "split batch" (constraintRecoveryTriangulation bandBatch)

  -- The same law with the reservation exhausting inside one corridor rather
  -- than between corridors: the closing horizontal crosses two constraints
  -- the census can see and fifteen it cannot, so the corridor suspends with
  -- its cursor mid-walk and the resumed transaction continues from the last
  -- split vertex, not from the corridor's start.
  let laceColumns = [0 .. 16 :: Int]
      laceVertices =
        V.fromList
          ( Point 0 5
              : Point 90 5
              : concat
                  [ [Point x 10, Point x 0]
                  | column <- laceColumns
                  , let x = 5 * fromIntegral column + 5
                  ]
          )
      laceBuiltIn = V.fromList [(2, 3), (34, 35)]
  laceBuild <-
    requireRight
      "split lace base"
      (constrainedDelaunayMaximal unitElementDefaults laceVertices laceBuiltIn)
  let laceAcceptedBuild = cdtAcceptedBuild laceBuild
      laceMapping = buildInputVertices laceAcceptedBuild
      laceHandle input = VertexId (indexPrimArray laceMapping input)
      laceBase = buildTriangulation laceAcceptedBuild
      laceRequests =
        V.fromList
          ( [ (laceHandle (2 * column + 2), laceHandle (2 * column + 3))
            | column <- [1 .. 15]
            ]
              <> [(laceHandle 0, laceHandle 1)]
          )
  laceBatch <-
    requireRight
      "split lace batch"
      (addConstraintsAndSplit id laceBase laceRequests)
  laceDescent <-
    requireRight
      "split lace singleton descent"
      (V.foldM' replaySplit laceBase laceRequests)
  assertEqual
    "split lace batch topology equals singleton descent"
    laceDescent
    (constraintRecoveryTriangulation laceBatch)
  assertEqual
    "split lace batch added one vertex per crossing"
    (numVertices laceBase + length laceColumns)
    (numVertices (constraintRecoveryTriangulation laceBatch))
  assertCdtValid "split lace batch" (constraintRecoveryTriangulation laceBatch)

  let verticesInput :: V.Vector (Point)
      verticesInput = V.fromList [Point 0 0, Point 4 0, Point 4 4, Point 0 4, Point 0 0]
      constraintsInput = V.fromList [(0, 2), (1, 3), (4, 1)]
  bulk <- requireRight "stable CDT bulk load" (constrainedDelaunayMaximal unitElementDefaults verticesInput constraintsInput)
  let bulkAcceptedBuild = cdtAcceptedBuild bulk
      bulkMapping = buildInputVertices bulkAcceptedBuild
  unless (sizeofPrimArray bulkMapping > 4) $ fail "buildInputVertices out of bounds"
  assertEqual "stable duplicate reroute" (VertexId (indexPrimArray bulkMapping 0)) (VertexId (indexPrimArray bulkMapping 4))
  assertEqual "conflict reporting" 1 (V.length (cdtRejectedConstraints bulk))
  assertCdtValid "stable CDT bulk load" (buildTriangulation bulkAcceptedBuild)

testAnnotatedConstrainedUnion :: IO ()
testAnnotatedConstrainedUnion = do
  let leftPoints :: V.Vector (Point)
      leftPoints = V.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2]
      rightPoints :: V.Vector (Point)
      rightPoints = V.fromList [Point 2 0, Point 4 0, Point 4 2, Point 2 2]
      thirdPoints :: V.Vector (Point)
      thirdPoints = V.fromList [Point 4 0, Point 6 0, Point 6 2, Point 4 2]
  leftBuild <-
    requireRight "annotated constrained union left" $
      constrainedDelaunay
        unitElementDefaults
        leftPoints
        (V.singleton (0, 2))
  rightBuild <-
    requireRight "annotated constrained union right" $
      constrainedDelaunay
        unitElementDefaults
        rightPoints
        (V.singleton (0, 2))
  thirdBuild <-
    requireRight "annotated constrained union third" $
      constrainedDelaunay
        unitElementDefaults
        thirdPoints
        (V.singleton (0, 2))
  let left =
        mapVertices
          (const (Set.singleton "left"))
          (buildTriangulation leftBuild)
      right =
        mapVertices
          (const (Set.singleton "right"))
          (buildTriangulation rightBuild)
      third =
        mapVertices
          (const (Set.singleton "third"))
          (buildTriangulation thirdBuild)
  joined <-
    requireRight
      "annotated constrained union"
      (unionConstrainedWith Set.union left right)
  let expectedAnnotations =
        Map.fromList
          [ (Point 0 0, Set.singleton "left")
          , (Point 0 2, Set.singleton "left")
          , (Point 2 0, Set.fromList ["left", "right"])
          , (Point 2 2, Set.fromList ["left", "right"])
          , (Point 4 0, Set.singleton "right")
          , (Point 4 2, Set.singleton "right")
          ]
      actualAnnotations =
        Map.fromList
          [ (vertexPoint joined vertex, vertexData joined vertex)
          | vertex <- vertices joined
          ]
      expectedConstraints =
        Set.union
          (Set.fromList (V.toList (constraintSegments left)))
          (Set.fromList (V.toList (constraintSegments right)))
  assertEqual
    "annotated constrained union preserves and combines site payloads"
    expectedAnnotations
    actualAnnotations
  assertEqual
    "annotated constrained union preserves both constraint sections"
    expectedConstraints
    (Set.fromList (V.toList (constraintSegments joined)))
  assertCdtValid "annotated constrained union" joined
  commuted <-
    requireRight
      "annotated constrained union commuted"
      (unionConstrainedWith Set.union right left)
  assertEqual
    "annotated constrained union is commutative under a commutative payload combiner"
    joined
    commuted
  leftAssociated <-
    requireRight
      "annotated constrained union left-associated"
      (unionConstrainedWith Set.union joined third)
  rightPair <-
    requireRight
      "annotated constrained union right pair"
      (unionConstrainedWith Set.union right third)
  rightAssociated <-
    requireRight
      "annotated constrained union right-associated"
      (unionConstrainedWith Set.union left rightPair)
  assertEqual
    "annotated constrained union is associative wherever both descents are admitted"
    leftAssociated
    rightAssociated
  unitJoined <-
    requireRight
      "unit constrained union specialization"
      ( unionConstrained
          (mapVertices (const ()) left)
          (mapVertices (const ()) right)
      )
  assertEqual
    "unit constrained union specializes annotated constrained union"
    unitJoined
    (mapVertices (const ()) joined)

-- | Sequential extension retains the base authority as the transaction root.
-- The semantic result agrees with canonical union on sites and constraint
-- sections, but its receipt proves that only the incoming constraint section
-- was interpreted.
testAsymmetricConstrainedExtension :: IO ()
testAsymmetricConstrainedExtension = do
  let basePoints :: V.Vector (Point)
      basePoints = V.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2]
      extensionPoints :: V.Vector (Point)
      extensionPoints = V.fromList [Point 2 0, Point 4 0, Point 4 2, Point 2 2]
  baseBuild <-
    requireRight
      "asymmetric extension base"
      (constrainedDelaunay unitElementDefaults basePoints (V.singleton (0, 2)))
  extensionBuild <-
    requireRight
      "asymmetric extension incoming"
      (constrainedDelaunay unitElementDefaults extensionPoints (V.singleton (0, 2)))
  let base = mapVertices (const (Set.singleton "base")) (buildTriangulation baseBuild)
      extension = mapVertices (const (Set.singleton "extension")) (buildTriangulation extensionBuild)
      expectedAnnotations =
        Map.fromList
          [ (Point 0 0, Set.singleton "base")
          , (Point 0 2, Set.singleton "base")
          , (Point 2 0, Set.fromList ["base", "extension"])
          , (Point 2 2, Set.fromList ["base", "extension"])
          , (Point 4 0, Set.singleton "extension")
          , (Point 4 2, Set.singleton "extension")
          ]
      expectedConstraints =
        Set.union
          (Set.fromList (V.toList (constraintSegments base)))
          (Set.fromList (V.toList (constraintSegments extension)))
      baseAnnotations =
        Map.fromList
          [ (vertexPoint base vertex, vertexData base vertex)
          | vertex <- vertices base
          ]
      extensionAnnotations =
        Map.fromList
          [ (vertexPoint extension vertex, vertexData extension vertex)
          | vertex <- vertices extension
          ]
  baseAnnotationsBefore <- evaluate (force baseAnnotations)
  baseConstraintsBefore <- evaluate (force (constraintSegments base))
  extensionAnnotationsBefore <- evaluate (force extensionAnnotations)
  extensionConstraintsBefore <- evaluate (force (constraintSegments extension))
  extendedResult <-
    requireRight
      "asymmetric constrained extension"
      (extendConstrainedWith Set.union base extension)
  let constraintBatch = constrainedExtensionConstraintBatch extendedResult
      extended = constraintBatchTriangulation constraintBatch
      receipt = constraintBatchStats constraintBatch
      buildReceipt = constrainedExtensionBuildStats extendedResult
      actualAnnotations =
        Map.fromList
          [ (vertexPoint extended vertex, vertexData extended vertex)
          | vertex <- vertices extended
          ]
  assertEqual
    "asymmetric extension preserves base and combines coincident site payloads"
    expectedAnnotations
    actualAnnotations
  assertEqual
    "asymmetric extension preserves resident and incoming constraint sections"
    expectedConstraints
    (Set.fromList (V.toList (constraintSegments extended)))
  assertEqual
    "asymmetric extension does not mutate the frozen base predecessor sites"
    baseAnnotationsBefore
    ( Map.fromList
        [ (vertexPoint base vertex, vertexData base vertex)
        | vertex <- vertices base
        ]
    )
  assertEqual
    "asymmetric extension does not mutate the frozen base predecessor constraints"
    baseConstraintsBefore
    (constraintSegments base)
  assertEqual
    "asymmetric extension does not mutate the frozen incoming predecessor sites"
    extensionAnnotationsBefore
    ( Map.fromList
        [ (vertexPoint extension vertex, vertexData extension vertex)
        | vertex <- vertices extension
        ]
    )
  assertEqual
    "asymmetric extension does not mutate the frozen incoming predecessor constraints"
    extensionConstraintsBefore
    (constraintSegments extension)
  assertEqual
    "asymmetric extension recovers only incoming constraints"
    (V.length (constraintSegments extension))
    (constraintBatchRequests receipt)
  assertEqual
    "asymmetric extension returns one outcome for each incoming constraint"
    (V.length (constraintSegments extension))
    (V.length (constraintBatchOutcomes constraintBatch))
  assertEqual
    "asymmetric extension admits every incoming constraint"
    (V.length (constraintSegments extension))
    (constraintBatchAccepted receipt)
  assertEqual
    "asymmetric extension reports no incoming constraint rejection"
    0
    (constraintBatchRejected receipt)
  assertEqual
    "asymmetric extension charges every incoming site once"
    (V.length extensionPoints)
    (statInputPoints buildReceipt)
  assertEqual
    "asymmetric extension distinguishes occupied incoming sites"
    2
    (statExistingPoints buildReceipt)
  assertEqual
    "asymmetric extension distinguishes newly materialized incoming sites"
    2
    (statUniquePoints buildReceipt)
  assertCdtValid "asymmetric constrained extension" extended

  crossingBuild <-
    requireRight
      "asymmetric extension crossing incoming section"
      (constrainedDelaunay unitElementDefaults basePoints (V.singleton (1, 3)))
  case
      extendConstrainedWith
        Set.union
        base
        (mapVertices (const (Set.singleton "crossing")) (buildTriangulation crossingBuild)) of
    Left (ConstraintUnionConstructionFailed (ConstraintIntersection _)) -> pure ()
    other -> fail ("asymmetric extension did not return the corridor intersection witness: " <> show other)

testLargeAsymmetricConstrainedExtension :: IO ()
testLargeAsymmetricConstrainedExtension = do
  let width = 40 :: Int
      height = 28 :: Int
      basePoints :: V.Vector (Point)
      basePoints =
        V.fromList
          ( [ Point 0 0
            , Point (fromIntegral (width + 1)) 0
            , Point (fromIntegral (width + 1)) (fromIntegral (height + 1))
            , Point 0 (fromIntegral (height + 1))
            ]
              <> [ Point
                     (fromIntegral (column + 1) + fromIntegral ((column * 17 + row * 31) `mod` 13) * 1.0e-3)
                     (fromIntegral (row + 1) + fromIntegral ((column * 23 + row * 19) `mod` 17) * 1.0e-3)
                 | row <- [0 .. height - 1]
                 , column <- [0 .. width - 1]
                 ]
          )
      baseConstraints = V.fromList [(0, 1), (1, 2), (2, 3), (3, 0), (0, 2)]
      extensionPoints :: V.Vector (Point)
      extensionPoints = V.fromList [Point 48 8, Point 53.2 8.4, Point 50.4 15.8]
      extensionConstraints = V.fromList [(0, 1), (1, 2), (2, 0)]
      conflictPoints :: V.Vector (Point)
      conflictPoints = V.fromList [Point 8 22, Point 30 5, Point 26 7]
  baseBuild <-
    requireRight
      "large asymmetric extension base"
      (constrainedDelaunay unitElementDefaults basePoints baseConstraints)
  extensionBuild <-
    requireRight
      "large asymmetric extension incoming"
      (constrainedDelaunay unitElementDefaults extensionPoints extensionConstraints)
  conflictBuild <-
    requireRight
      "large asymmetric extension conflicting incoming"
      (constrainedDelaunay unitElementDefaults conflictPoints (V.singleton (0, 1)))
  let base = mapVertices (const (Set.singleton "base")) (buildTriangulation baseBuild)
      extension = mapVertices (const (Set.singleton "extension")) (buildTriangulation extensionBuild)
      conflicting = mapVertices (const (Set.singleton "conflict")) (buildTriangulation conflictBuild)
      predecessor
        :: Triangulation 'Constrained (Set.Set String) () () ()
        -> ( Triangulation 'Constrained (Set.Set String) () () ()
           , Map.Map VertexId (Point, Set.Set String)
           , Set.Set (CanonicalSegment)
           )
      predecessor triangulation =
        ( triangulation
        , Map.fromList
            [ (vertex, (vertexPoint triangulation vertex, vertexData triangulation vertex))
            | vertex <- vertices triangulation
            ]
        , Set.fromList (V.toList (constraintSegments triangulation))
        )
      assertPredecessor
        :: String
        -> ( Triangulation 'Constrained (Set.Set String) () () ()
           , Map.Map VertexId (Point, Set.Set String)
           , Set.Set (CanonicalSegment)
           )
        -> Triangulation 'Constrained (Set.Set String) () () ()
        -> IO ()
      assertPredecessor label snapshot triangulation =
        assertEqual label snapshot (predecessor triangulation)
  baseBefore <- evaluate (force (predecessor base))
  extensionBefore <- evaluate (force (predecessor extension))
  conflictingBefore <- evaluate (force (predecessor conflicting))
  let (_, baseVertexSnapshotBefore, baseConstraintSnapshotBefore) = baseBefore
      (_, _, extensionConstraintSnapshotBefore) = extensionBefore
  extensionResult <-
    requireRight
      "large asymmetric constrained extension"
      (extendConstrainedWith Set.union base extension)
  let constraintBatch = constrainedExtensionConstraintBatch extensionResult
      extended = constraintBatchTriangulation constraintBatch
      extendedConstraints = Set.fromList (V.toList (constraintSegments extended))
      extendedVertexSnapshot =
        Map.fromList
          [ (vertex, (vertexPoint extended vertex, vertexData extended vertex))
          | vertex <- vertices extended
          ]
  assertEqual
    "large asymmetric extension retains every base vertex handle, coordinate, and payload"
    baseVertexSnapshotBefore
    (Map.restrictKeys extendedVertexSnapshot (Map.keysSet baseVertexSnapshotBefore))
  assertEqual
    "large asymmetric extension retains its complete base constraint source section"
    baseConstraintSnapshotBefore
    (Set.intersection baseConstraintSnapshotBefore extendedConstraints)
  assertEqual
    "large asymmetric extension retains its complete incoming constraint source section"
    extensionConstraintSnapshotBefore
    (Set.intersection extensionConstraintSnapshotBefore extendedConstraints)
  assertEqual
    "large asymmetric extension preserves exactly both source constraint sections"
    (Set.union baseConstraintSnapshotBefore extensionConstraintSnapshotBefore)
    extendedConstraints
  assertEqual
    "large asymmetric extension leaves the frozen base predecessor physically unchanged"
    baseBefore
    (predecessor base)
  assertPredecessor
    "large asymmetric extension leaves the frozen incoming predecessor physically unchanged"
    extensionBefore
    extension
  assertEqual
    "large asymmetric extension charges only the incoming sites"
    (V.length extensionPoints)
    (statInputPoints (constrainedExtensionBuildStats extensionResult))
  assertEqual
    "large asymmetric extension replays only the incoming constraints"
    (V.length extensionConstraints)
    (constraintBatchRequests (constraintBatchStats constraintBatch))
  assertCdtValid "large asymmetric constrained extension" extended
  case extendConstrainedWith Set.union base conflicting of
    Left (ConstraintUnionConstructionFailed (ConstraintIntersection _)) -> pure ()
    outcome ->
      fail
        ( "large asymmetric extension did not refuse the crossing incoming corridor: "
            <> show outcome
        )
  assertEqual
    "large asymmetric extension refusal leaves the frozen base predecessor physically unchanged"
    baseBefore
    (predecessor base)
  assertPredecessor
    "large asymmetric extension refusal leaves the conflicting predecessor physically unchanged"
    conflictingBefore
    conflicting

-- | A separated constrained seam is a restriction-preserving operation, not
-- the canonical site-set union wearing a cheaper costume. Both closed source
-- sections survive face-for-face; only the corridor contributes new faces.
testSeparatedConstrainedSeam :: IO ()
testSeparatedConstrainedSeam = do
  let leftPoints :: V.Vector (Point)
      leftPoints =
        V.fromList
          [ Point (-4) (-1)
          , Point (-2) (-1)
          , Point (-2) 1
          , Point (-4) 1
          ]
      rightPoints :: V.Vector (Point)
      rightPoints =
        V.fromList
          [ Point 2 (-1)
          , Point 4 (-1)
          , Point 4 1
          , Point 2 1
          ]
      closedContour = V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)]
  leftBuild <-
    requireRight
      "separated constrained seam left"
      (constrainedDelaunay unitElementDefaults leftPoints closedContour)
  rightBuild <-
    requireRight
      "separated constrained seam right"
      (constrainedDelaunay unitElementDefaults rightPoints closedContour)
  let left =
        Internal.geometryOnlyPublication
          (buildTriangulation leftBuild)
      right =
        Internal.geometryOnlyPublication
          (buildTriangulation rightBuild)
      leftFaceKeys = Set.fromList (fmap (faceKeyOf left) (innerFaces left))
      rightFaceKeys = Set.fromList (fmap (faceKeyOf right) (innerFaces right))
      expectedConstraints =
        Set.union
          (Set.fromList (V.toList (constraintSegments left)))
          (Set.fromList (V.toList (constraintSegments right)))
  seam <-
    requireRight
      "source-preserving separated constrained seam"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters left right)
  let joined = constrainedSeamResultTriangulation seam
      joinedFaceKeys = Set.fromList (fmap (faceKeyOf joined) (innerFaces joined))
      actualAnnotations =
        Map.fromList
          [ (vertexPoint joined vertex, vertexData joined vertex)
          | vertex <- vertices joined
          ]
      expectedAnnotations =
        Map.union
          ( Map.fromList
              [ (vertexPoint left vertex, vertexData left vertex)
              | vertex <- vertices left
              ]
          )
          ( Map.fromList
              [ (vertexPoint right vertex, vertexData right vertex)
              | vertex <- vertices right
              ]
          )
      actualConstraints = Set.fromList (V.toList (constraintSegments joined))
      certifiedPerimeter =
        fmap
          (\segmentValue -> (segmentStart segmentValue, segmentEnd segmentValue))
          (Set.toAscList (Set.difference actualConstraints expectedConstraints))
  assertCdtValid "source-preserving separated constrained seam" joined
  assertEqual
    "separated seam preserves all source annotations"
    expectedAnnotations
    actualAnnotations
  assertEqual
    "separated seam preserves both source constraint sections"
    True
    (expectedConstraints `Set.isSubsetOf` actualConstraints)
  assertEqual
    "separated seam certifies exactly its two synthetic perimeter bridges"
    [ (Point (-2) (-1), Point 2 (-1))
    , (Point (-2) 1, Point 2 1)
    ]
    certifiedPerimeter
  unless (leftFaceKeys `Set.isSubsetOf` joinedFaceKeys) $
    fail "separated seam removed or retriangulated a left source face"
  unless (rightFaceKeys `Set.isSubsetOf` joinedFaceKeys) $
    fail "separated seam removed or retriangulated a right source face"
  assertEqual
    "separated seam records the stable left face witness"
    (numInnerFaces left)
    (constrainedSeamLeftFaceCount seam)
  assertEqual
    "separated seam records the stable left constraint witness"
    (numConstraints left)
    (constrainedSeamLeftConstraintCount seam)
  assertEqual
    "separated seam has one right face witness per source face"
    (numInnerFaces right)
    (V.length (constrainedSeamRightFaceEvidence seam))
  forM_ (V.toList (constrainedSeamRightFaceEvidence seam)) $ \evidence ->
    let sourceFaceKey = faceKeyOf right (constrainedSeamSourceFace evidence)
        targetFaceKey = faceKeyOf joined (constrainedSeamTargetFace evidence)
        evidenceFaceKey =
          [ constrainedSeamFaceFirstPoint evidence
          , constrainedSeamFaceSecondPoint evidence
          , constrainedSeamFaceThirdPoint evidence
          ]
     in do
          assertEqual
            "right face witness names the exact target triangle"
            sourceFaceKey
            targetFaceKey
          assertEqual
            "right face witness carries the exact target triangle"
            targetFaceKey
            evidenceFaceKey
  when (V.null (constrainedSeamJoinFaces seam)) $
    fail "separated seam did not identify any new corridor face"
  assertEqual
    "copied source constraints require no corridor recovery"
    0
    (constraintBatchRequests (constrainedSeamConstraintStats seam))
  let seamPublication = constrainedSeamPublicationStats seam
  assertEqual
    "separated seam does not enumerate resident unboxed base pages"
    0
    (publicationUnboxedBasePageEnumerations seamPublication)
  assertEqual
    "separated seam does not freeze resident unboxed base pages"
    0
    (publicationUnboxedBasePageFreezes seamPublication)
  assertEqual
    "separated seam does not enumerate resident boxed base pages"
    0
    (publicationBoxedBasePageEnumerations seamPublication)
  unless
    ( publicationUnboxedDirtyBasePages seamPublication
        + publicationUnboxedDirtyAppendedPages seamPublication
        > 0
    ) $
    fail "separated seam publication did not record appended unboxed writes"
  unless (constrainedSeamCachedFrontierPointReads seam > 0) $
    fail "separated seam did not record cached frontier point reads"

  let boundedBridgeParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 6
          , refineMaxEdgeLength = Just 1
          }
  boundedSeam <-
    requireRight
      "edge-bounded separated constrained seam"
      ( joinSeparatedConstrained
          (\_ _ -> True)
          boundedBridgeParameters
          left
          right
      )
  let boundedJoined = constrainedSeamResultTriangulation boundedSeam
      boundedSyntheticSegments =
        Set.difference
          (Set.fromList (V.toList (constraintSegments boundedJoined)))
          expectedConstraints
  assertCdtValid "edge-bounded separated constrained seam" boundedJoined
  assertEqual
    "edge-bounded seam charges its exact bridge Steiner budget"
    6
    (numVertices boundedJoined - numVertices left - numVertices right)
  assertEqual
    "edge-bounded seam publishes both four-edge bridge paths"
    8
    (Set.size boundedSyntheticSegments)
  unless (all ((<= 1) . canonicalSegmentSquaredLength) boundedSyntheticSegments) $
    fail "edge-bounded seam published an oversized synthetic bridge child"
  assertEqual
    "edge-bounded seam cache agrees with its explicit frontier observation"
    (Internal.prepareSeamFrontierIndex boundedJoined)
    (Internal.triSeamFrontier boundedJoined)
  case
      joinSeparatedConstrained
        (\_ _ -> True)
        boundedBridgeParameters{refineMaxAdditionalVertices = Just 5}
        left
        right of
    Left
      ( ConstraintUnionConstructionFailed
          (CdtBuildError (RefinementSeamBridgeBudgetExceeded 6 5))
        ) -> pure ()
    outcome ->
      fail
        ( "edge-bounded seam did not return its exact budget obstruction: "
            <> either show (const "success") outcome
        )

  reversed <- requireRight "reversed source-preserving separated seam" (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters right left)
  assertCdtValid "reversed source-preserving separated seam" (constrainedSeamResultTriangulation reversed)

  let localParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 2
          , refineMaxArea = Just 0.5
          , refineKeepConstraintEdges = True
          }
      allJoinedFaces = Set.fromList (innerFaces joined)
  locallyRefined <-
    requireRight
      "seam refinement preserves cached hull frontier"
      (refineWithinDomain (const ()) localParameters allJoinedFaces joined)
  let refinedJoined = refinedTriangulation (refinementDomainResult locallyRefined)
  unless (refinementComplete (refinementDomainResult locallyRefined)) $
    fail "seam refinement fixture did not drain its local worklist"
  assertEqual
    "local refinement retains the exact cached hull frontier"
    (Internal.triSeamFrontier joined)
    (Internal.triSeamFrontier refinedJoined)

  thirdBuild <-
    requireRight
      "repeated separated constrained seam source"
      (constrainedDelaunay unitElementDefaults (V.fromList
        [ Point 8 (-1)
        , Point 10 (-1)
        , Point 10 1
        , Point 8 1
        ]) closedContour)
  let third =
        Internal.geometryOnlyPublication
          (buildTriangulation thirdBuild)
  repeated <-
    requireRight
      "repeated separated seam after local refinement"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters refinedJoined third)
  assertCdtValid
    "repeated separated seam after local refinement"
    (constrainedSeamResultTriangulation repeated)
  case joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters left left of
    Left ConstraintUnionNotSeparated -> pure ()
    other -> fail ("non-separated constrained seam was not refused: " <> show other)

-- North/south additions use the orientation-preserving Y chart rather than
-- rotating or copying the resident mesh.  The sequence then alternates axes,
-- so a stale one-axis frontier cache cannot accidentally make the third join
-- succeed.
testAxisSeparatedConstrainedSeams :: IO ()
testAxisSeparatedConstrainedSeams = do
  let closedContour :: Int -> V.Vector (Int, Int)
      closedContour pointCount =
        V.fromList
          [ (index, (index + 1) `mod` pointCount)
          | index <- [0 .. pointCount - 1]
          ]
      buildPublished
        :: String
        -> V.Vector Point
        -> IO (Triangulation 'Constrained () () () ())
      buildPublished label points = do
        built <-
          requireRight
            label
            (constrainedDelaunay
               unitElementDefaults
               points
               (closedContour (V.length points)))
        pure (Internal.geometryOnlyPublication (buildTriangulation built))
      centralPoints =
        V.fromList
          [ Point (-1) (-1)
          , Point 1 (-1)
          , Point 1 1
          , Point (-1) 1
          ]
      northPoints =
        V.fromList
          [ Point (-1) 3
          , Point 1 3
          , Point 1 5
          , Point (-1) 5
          ]
      southPoints =
        V.fromList
          [ Point (-1) (-5)
          , Point 1 (-5)
          , Point 1 (-3)
          , Point (-1) (-3)
          ]
      eastPoints =
        V.fromList
          [ Point 3 (-1)
          , Point 5 (-1)
          , Point 5 1
          , Point 3 1
          ]
      overlapPoints =
        V.fromList
          [ Point 0 0
          , Point 2 0
          , Point 2 2
          , Point 0 2
          ]
      irregularNorthPoints =
        V.fromList
          [ Point (-2) 7
          , Point 0 7
          , Point 2 7
          , Point 2.5 9
          , Point 0 11
          , Point (-2.5) 9
          ]
  central <- buildPublished "axis seam central source" centralPoints
  north <- buildPublished "axis seam north source" northPoints
  south <- buildPublished "axis seam south source" southPoints
  east <- buildPublished "axis seam east source" eastPoints
  overlap <- buildPublished "axis seam overlapping source" overlapPoints

  let northParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 2
          , refineMaxEdgeLength = Just 1
          }
  northJoined <-
    requireRight
      "edge-bounded north source-preserving separated seam"
      (joinSeparatedConstrained (\_ _ -> True) northParameters central north)
  let northTopology = constrainedSeamResultTriangulation northJoined
      northPublication = constrainedSeamPublicationStats northJoined
      northSyntheticSegments =
        Set.difference
          (Set.fromList (V.toList (constraintSegments northTopology)))
          ( Set.union
              (Set.fromList (V.toList (constraintSegments central)))
              (Set.fromList (V.toList (constraintSegments north)))
          )
  assertCdtValid
    "north source-preserving separated seam"
    northTopology
  assertEqual
    "north seam charges two bridge Steiner vertices"
    2
    (numVertices northTopology - numVertices central - numVertices north)
  assertEqual
    "north seam certifies two bounded two-edge bridge paths"
    4
    (Set.size northSyntheticSegments)
  unless (all ((<= 1) . canonicalSegmentSquaredLength) northSyntheticSegments) $
    fail "north seam published an oversized synthetic bridge child"
  assertEqual
    "north seam cache agrees with an explicit frontier observation"
    (Internal.prepareSeamFrontierIndex northTopology)
    (Internal.triSeamFrontier northTopology)
  assertEqual
    "north seam does not enumerate resident unboxed pages"
    0
    (publicationUnboxedBasePageEnumerations northPublication)
  assertEqual
    "north seam does not freeze resident unboxed pages"
    0
    (publicationUnboxedBasePageFreezes northPublication)
  assertEqual
    "north seam does not enumerate resident boxed pages"
    0
    (publicationBoxedBasePageEnumerations northPublication)
  unless
    ( publicationUnboxedDirtyBasePages northPublication
        + publicationUnboxedDirtyAppendedPages northPublication
        > 0
    ) $
    fail "north seam did not record appended unboxed writes"

  reversed <-
    requireRight
      "reversed south/north source-preserving separated seam"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters south central)
  assertCdtValid
    "reversed south/north source-preserving separated seam"
    (constrainedSeamResultTriangulation reversed)

  mixed <-
    requireRight
      "mixed-axis east extension after north seam"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters
         (constrainedSeamResultTriangulation northJoined)
         east)
  assertCdtValid
    "mixed-axis east extension after north seam"
    (constrainedSeamResultTriangulation mixed)
  assertEqual
    "mixed-axis seam retains prior perimeter and certifies two new bridges"
    ( numConstraints (constrainedSeamResultTriangulation northJoined)
        + numConstraints east
        + 2
    )
    (numConstraints (constrainedSeamResultTriangulation mixed))

  repeated <-
    requireRight
      "repeated mixed-axis south extension"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters
         (constrainedSeamResultTriangulation mixed)
         south)
  assertCdtValid
    "repeated mixed-axis south extension"
    (constrainedSeamResultTriangulation repeated)
  assertEqual
    "repeated mixed-axis seam cache agrees with an explicit frontier observation"
    (Internal.prepareSeamFrontierIndex (constrainedSeamResultTriangulation repeated))
    (Internal.triSeamFrontier (constrainedSeamResultTriangulation repeated))

  irregularNorth <-
    buildPublished
      "axis seam irregular north source"
      irregularNorthPoints
  irregularJoined <-
    requireRight
      "irregular collinear north extension"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters
         (constrainedSeamResultTriangulation repeated)
         irregularNorth)
  assertCdtValid
    "irregular collinear north extension"
    (constrainedSeamResultTriangulation irregularJoined)
  assertEqual
    "irregular collinear north cache agrees with an explicit frontier observation"
    (Internal.prepareSeamFrontierIndex (constrainedSeamResultTriangulation irregularJoined))
    (Internal.triSeamFrontier (constrainedSeamResultTriangulation irregularJoined))

  case joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters central overlap of
    Left ConstraintUnionNotSeparated -> pure ()
    outcome ->
      fail
        ( "overlapping sources were not refused by either-axis admission: "
            <> either show (const "success") outcome
        )

-- A separated source frontier may become an interior diagonal when the seam
-- closes. If that diagonal is unconstrained and locally illegal, preserving
-- the source and publishing a Delaunay join are incompatible obligations. The
-- constrained twin is lawful: its contour explicitly authorizes that fixed
-- diagonal to remain.
testSeparatedConstrainedSeamRefusesPinnedFrontierRewrite :: IO ()
testSeparatedConstrainedSeamRefusesPinnedFrontierRewrite = do
  let leftPoints =
        V.fromList
          [ Point (-2) 0
          , Point 0 0
          , Point (-1) 10
          , Point (-3) 5
          ]
      rightPoints =
        V.fromList
          [ Point 1 5
          , Point 2 4
          , Point 3 5
          , Point 2 6
          ]
      closedQuadrilateral = V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)]
      buildSource
        :: String
        -> V.Vector (Int, Int)
        -> V.Vector Point
        -> IO (BuildResult 'Constrained Point () () ())
      buildSource label constraints points =
        requireRight label (constrainedDelaunay unitElementDefaults points constraints)
      publish
        :: BuildResult 'Constrained Point () () ()
        -> Triangulation 'Constrained () () () ()
      publish = Internal.geometryOnlyPublication . buildTriangulation
  unconstrainedLeft <- publish <$> buildSource "incompatible seam left" V.empty leftPoints
  unconstrainedRight <- publish <$> buildSource "incompatible seam right" V.empty rightPoints
  case joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters unconstrainedLeft unconstrainedRight of
    Left
      ( ConstraintUnionConstructionFailed
          (CdtBuildError (SeamSourceEdgeRequiresFlip _))
        ) -> pure ()
    outcome ->
      fail
        ( "separated seam did not refuse an unconstrained pinned source rewrite: "
            <> either show (const "success") outcome
        )
  selectiveJoin <-
    requireRight
      "selective seam rewrites only unselected exterior faces"
      (joinSeparatedConstrained (\_ _ -> False) defaultRefinementParameters unconstrainedLeft unconstrainedRight)
  let selectivelyJoined = constrainedSeamResultTriangulation selectiveJoin
      sourceFaceKeys =
        Set.union
          (Set.fromList (fmap (faceKeyOf unconstrainedLeft) (innerFaces unconstrainedLeft)))
          (Set.fromList (fmap (faceKeyOf unconstrainedRight) (innerFaces unconstrainedRight)))
      selectiveFaceKeys =
        Set.fromList (fmap (faceKeyOf selectivelyJoined) (innerFaces selectivelyJoined))
  assertCdtValid "selective unconstrained seam" selectivelyJoined
  when (sourceFaceKeys `Set.isSubsetOf` selectiveFaceKeys) $
    fail "selective seam did not rewrite the incompatible exterior source face"
  when (V.null (constrainedSeamJoinFaces selectiveJoin)) $
    fail "selective seam did not publish the exact rewritten J component"
  unless (V.null (constrainedSeamRightFaceEvidence selectiveJoin)) $
    fail "selective seam fabricated transport evidence for unselected incoming faces"
  constrainedLeft <-
    publish
      <$> buildSource
        "compatible constrained seam left"
        closedQuadrilateral
        leftPoints
  constrainedRight <-
    publish
      <$> buildSource
        "compatible constrained seam right"
        closedQuadrilateral
        rightPoints
  let protectedLeftFaces = Set.fromList (boundedRegionFaces constrainedLeft)
      protectedRightFaces = Set.fromList (boundedRegionFaces constrainedRight)
      preserveConstrainedSource side faceHandle =
        case side of
          SeamResident -> Set.member faceHandle protectedLeftFaces
          SeamIncoming -> Set.member faceHandle protectedRightFaces
      protectedFaceKeys =
        Set.union
          (Set.fromList (fmap (faceKeyOf constrainedLeft) (Set.toList protectedLeftFaces)))
          (Set.fromList (fmap (faceKeyOf constrainedRight) (Set.toList protectedRightFaces)))
  constrainedJoin <-
    requireRight
      "compatible selected constrained seam"
      (joinSeparatedConstrained preserveConstrainedSource defaultRefinementParameters constrainedLeft constrainedRight)
  let constrainedJoined = constrainedSeamResultTriangulation constrainedJoin
      joinedFaceKeys =
        Set.fromList (fmap (faceKeyOf constrainedJoined) (innerFaces constrainedJoined))
      evidencedIncomingFaces =
        Set.fromList
          ( fmap
              constrainedSeamSourceFace
              (V.toList (constrainedSeamRightFaceEvidence constrainedJoin))
          )
  assertCdtValid
    "compatible selected constrained seam"
    constrainedJoined
  unless (protectedFaceKeys `Set.isSubsetOf` joinedFaceKeys) $
    fail "selected constrained source face did not survive exactly"
  assertEqual
    "selected seam evidences exactly the protected incoming faces"
    protectedRightFaces
    evidencedIncomingFaces

-- An obtuse unconstrained source has a hull edge inside the opposite vertex's
-- diametral circle.  That edge is queued by the refinement seed even though
-- it is not a source constraint.  The checked local interpreter must refuse
-- the encroachment split at its authoritative resolver, retain the cached
-- frontier, and remain joinable afterwards.
testLocalRefinementRejectsEncroachedHullSplit :: IO ()
testLocalRefinementRejectsEncroachedHullSplit = do
  built <-
    requireRight
      "encroached hull source"
      ( constrainedDelaunay
          unitElementDefaults
          (V.fromList [Point 0 0, Point 2 0, Point 0.1 0.1])
          V.empty
      )
  let source =
        Internal.geometryOnlyPublication
          (buildTriangulation built)
      permitted = Set.fromList (innerFaces source)
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 1
          , refineMaxArea = Just 0.001
          , refineKeepConstraintEdges = True
          }
      hullEdges =
        [ edge
        | edge <- undirectedEdges source
        , let (forward, backward) = directedPair edge
        , incidentFace source forward == outerFace || incidentFace source backward == outerFace
        ]
      encroachedHull edge =
        let (fromVertex, toVertex) = Dcel.undirectedEndpoints source edge
            fromPoint = vertexPoint source fromVertex
            toPoint = vertexPoint source toVertex
            oppositeFaceEdge =
              case directedPair edge of
                (forward, backward)
                  | incidentFace source forward == outerFace -> backward
                  | otherwise -> forward
            oppositePoint =
              let innerFace = incidentFace source oppositeFaceEdge
               in if innerFace == outerFace
                    then Nothing
                    else
                      case Dcel.faceVertices source innerFace of
                        verticesInFace ->
                          case filter (/= fromVertex) (filter (/= toVertex) verticesInFace) of
                            [vertex] -> Just (vertexPoint source vertex)
                            _ -> Nothing
         in maybe False (Admitted.inDiametralCircle fromPoint toPoint) oppositePoint
  unless (any encroachedHull hullEdges) $
    fail "encroached hull fixture did not produce an encroached hull pair"
  refined <-
    requireRight
      "encroached hull local refinement"
      (refineWithinDomain (const ()) parameters permitted source)
  let refinementResult = refinementDomainResult refined
      result = refinedTriangulation refinementResult
  unless (refinementComplete refinementResult) $
    fail "encroached hull local refinement exhausted without a fixpoint"
  assertEqual "encroached hull split count" (numVertices source) (numVertices result)
  assertEqual
    "encroached hull refinement retains cached frontier"
    (Internal.triSeamFrontier source)
    (Internal.triSeamFrontier result)

  secondBuild <-
    requireRight
      "encroached hull join source"
      ( constrainedDelaunay
          unitElementDefaults
          (V.fromList [Point 4 0, Point 6 0, Point 6 1, Point 4 1])
          V.empty
      )
  let second =
        Internal.geometryOnlyPublication
          (buildTriangulation secondBuild)
  joined <-
    requireRight
      "encroached hull repeated join"
      (joinSeparatedConstrained (\_ _ -> True) defaultRefinementParameters result second)
  assertCdtValid
    "encroached hull repeated join"
    (constrainedSeamResultTriangulation joined)

testConstrainedRefinement :: IO ()
testConstrainedRefinement = do
  cdtBuild <- requireRight "bounded domain" $ constrainedDelaunay
    unitElementDefaults
    (V.fromList [Point 0 0, Point 8 0, Point 8 8, Point 0 8, Point 4 2, Point 4 6])
    (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
  let cdt = buildTriangulation cdtBuild
      parameters = defaultRefinementParameters
        { refineMaxAdditionalVertices = Just 80
        , refineMaxArea = Just 3
        , refineMaxRadiusEdgeRatio = Just 1.4
        , refineExcludeOuterFaces = True
        , refineKeepConstraintEdges = False
        }
  refined <- requireRight "constrained refinement" (refine id parameters cdt)
  let result = refinedTriangulation refined
  canonicalResult <- requireRight "canonical constrained refinement" (canonicalize result)
  unless (refinementAddedVertices refined > 0) $ fail "constrained refinement inserted no Steiner points"
  assertCdtValid "constrained refinement" result
  unless (numConstraints result >= numConstraints cdt) $
    fail "constraint splitting lost the constrained boundary"
  assertEqual
    "canonical publication preserves constraint segments"
    (constraintSegments result)
    (constraintSegments canonicalResult)

  -- Refinement maintains the outer-region classification incrementally, from
  -- the touched patch alone. That is a claim about what an insertion cannot
  -- reach, so it is gated against an independent flood over the finished mesh
  -- rather than trusted. The domain here is deliberately narrower than its
  -- convex hull, so the excluded set is non-empty and the two can disagree.
  notchBuild <- requireRight "notched domain" $ constrainedDelaunay
    unitElementDefaults
    (V.fromList [Point 0 0, Point 8 0, Point 8 8, Point 0 8, Point 13 4, Point 4 4])
    (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
  let notch = buildTriangulation notchBuild
      notchMaximumEdgeLength = 6.2
      notchParameters = defaultRefinementParameters
        { refineMaxAdditionalVertices = Just 120
        , refineMaxArea = Just 1.5
        , refineMaxEdgeLength = Just notchMaximumEdgeLength
        , refineExcludeOuterFaces = True
        , refineKeepConstraintEdges = False
        }
  notchRefined <- requireRight "notched refinement" (refine id notchParameters notch)
  let notchResult = refinedTriangulation notchRefined
      maintained = sort (V.toList (refinementExcludedFaces notchRefined))
      independent = sort (outerRegionFaces notchResult)
      excludedEdgeLengthsSquared =
        [ squaredDistanceWide
            (vertexPoint notchResult fromVertex)
            (vertexPoint notchResult toVertex)
        | face <- maintained
        , directed <- faceDirectedEdges notchResult face
        , let (fromVertex, toVertex) =
                undirectedEndpoints notchResult (asUndirected directed)
        ]
  unless (refinementAddedVertices notchRefined > 0) $ fail "notched refinement inserted no Steiner points"
  unless (refinementComplete notchRefined) $ fail "notched maximum-edge refinement did not complete"
  when (null independent) $ fail "notched domain produced no outer region: the gate is vacuous"
  unless
    (any (> notchMaximumEdgeLength * notchMaximumEdgeLength) excludedEdgeLengthsSquared)
    (fail "notched fixture has no excluded maximum-edge violation")
  assertEqual "incremental exclusion agrees with an independent flood" independent maintained
  assertCdtValid "notched refinement" notchResult

  annulusBuild <- requireRight "annular domain" $ constrainedDelaunay
    unitElementDefaults
    ( V.fromList
        [ Point 0 0
        , Point 12 0
        , Point 12 12
        , Point 0 12
        , Point 4 4
        , Point 8 4
        , Point 8 8
        , Point 4 8
        ]
    )
    ( V.fromList
        [ (0, 1)
        , (1, 2)
        , (2, 3)
        , (3, 0)
        , (4, 5)
        , (5, 6)
        , (6, 7)
        , (7, 4)
        ]
    )
  let annulus = buildTriangulation annulusBuild
      annulusParameters :: Int -> Maybe Double -> RefinementParameters
      annulusParameters budget maximumArea =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just budget
          , refineMaxArea = maximumArea
          , refineExcludeOuterFaces = True
          , refineKeepConstraintEdges = False
          }
  annulusUnchanged <-
    requireRight
      "budget-zero annular refinement"
      (refine id (annulusParameters 0 Nothing) annulus)
  let initialAnnulusOutside = sort (outerRegionFaces annulus)
      budgetZeroOutside =
        sort (V.toList (refinementExcludedFaces annulusUnchanged))
  assertEqual "annulus has one two-crossing hole" 2 (length initialAnnulusOutside)
  assertEqual
    "budget-zero refinement uses the authoritative annulus classification"
    initialAnnulusOutside
    budgetZeroOutside

  annulusRefined <-
    requireRight
      "positive-budget annular refinement"
      (refine id (annulusParameters 40 (Just 4)) annulus)
  let refinedAnnulus = refinedTriangulation annulusRefined
      maintainedAnnulus =
        sort (V.toList (refinementExcludedFaces annulusRefined))
      independentAnnulus = sort (outerRegionFaces refinedAnnulus)
  unless (refinementAddedVertices annulusRefined > 0) $
    fail "annular refinement inserted no Steiner points"
  assertEqual
    "incremental annulus exclusion agrees with authoritative barrier depth"
    independentAnnulus
    maintainedAnnulus
  assertCdtValid "annular constrained refinement" refinedAnnulus

  -- A tiny constraint can share a mesh with coordinates of vastly different
  -- magnitude. Encroachment is discovered by a local cavity walk, so there is
  -- no broad phase for the range to overflow; the pin is that the scaled
  -- circumcenter and diametral predicates still produce exactly one vertex.
  wideGridBuild <- requireRight "wide-grid constrained domain" $ constrainedDelaunay
    unitElementDefaults
    ( V.fromList
        [ Point 0 0
        , Point 2.0e-43 0
        , Point 1.0e60 0
        , Point 0 1.0e60
        ] :: V.Vector (Point)
    )
    (V.singleton (0, 1))
  wideGridRefined <-
    requireRight
      "wide-grid constrained refinement"
      ( refine
          id
          defaultRefinementParameters
            { refineMaxAdditionalVertices = Just 1
            , refineMaxArea = Just 1.0e119
            , refineKeepConstraintEdges = False
            }
          (buildTriangulation wideGridBuild)
      )
  assertEqual "wide-grid refinement count" 1 (refinementAddedVertices wideGridRefined)
  assertCdtValid "wide-grid constrained refinement" (refinedTriangulation wideGridRefined)

-- | A local domain is a closed section, not a hopeful initial queue. Its
-- interface is exact, its protected faces survive point-for-point, and the
-- receipt contains no visit to the protected side.
testCheckedLocalRefinement :: IO ()
testCheckedLocalRefinement = do
  built <-
    requireRight
      "checked local refinement source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 4.1 0
              , Point 8 0.2
              , Point 0.1 4
              , Point 4 4.2
              , Point 8.1 4
              ]
          )
          (V.singleton (2, 5))
      )
  let source = buildTriangulation built
      permitted =
        Set.fromList
          [ face
          | face <- innerFaces source
          , faceCentroidX source face < 4.05
          ]
      interface =
        Set.fromList
          [ edge
          | edge <- undirectedEdges source
          , let (forward, backward) = directedPair edge
                forwardFace = incidentFace source forward
                backwardFace = incidentFace source backward
          , forwardFace /= outerFace
          , backwardFace /= outerFace
          , Set.member forwardFace permitted /= Set.member backwardFace permitted
          ]
      protected = filter (`Set.notMember` permitted) (innerFaces source)
      protectedSignatures = fmap (\face -> (face, sort (fmap (vertexPoint source) (faceVertices source face)))) protected
      calmParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 3
          , refineMaxRadiusEdgeRatio = Nothing
          , refineKeepConstraintEdges = True
          }
      crossingParameters = calmParameters{refineMaxArea = Just 1}
  unless (not (Set.null permitted) && not (null protected) && not (Set.null interface)) $
    fail "checked local refinement fixture did not form a nontrivial cover"
  case refineWithinDomain id calmParameters{refineMaxAdditionalVertices = Nothing} permitted source of
    Left RefinementDomainRequiresFiniteVertexBudget -> pure ()
    outcome -> fail ("checked local refinement accepted an unbounded local budget: " <> either show (const "success") outcome)
  refined <-
    requireRight
      "checked local refinement"
      (refineWithinDomain id calmParameters permitted source)
  let localResult = refinementDomainResult refined
      target = refinedTriangulation localResult
      targetProtectedSignatures = fmap (\face -> (face, sort (fmap (vertexPoint target) (faceVertices target face)))) protected
      receipt = refinementDomainReceipt refined
      finalPermitted = Set.fromList (V.toList (refinementFinalPermittedFaces receipt))
      expectedFinalInterfaceIncidence =
        V.fromList
          [ if Set.member sourceForwardFace permitted
              then (edge, sourceBackwardFace, targetForwardFace)
              else (edge, sourceForwardFace, targetBackwardFace)
          | edge <- Set.toAscList interface
          , let (forward, backward) = directedPair edge
                sourceForwardFace = incidentFace source forward
                sourceBackwardFace = incidentFace source backward
                targetForwardFace = incidentFace target forward
                targetBackwardFace = incidentFace target backward
          ]
      actualFinalInterfaceIncidence = refinementFinalInterfaceIncidence receipt
  assertEqual "checked local protected face restriction" protectedSignatures targetProtectedSignatures
  assertEqual
    "checked local final permitted lineage"
    permitted
    finalPermitted
  assertEqual
    "checked local final interface incidence"
    expectedFinalInterfaceIncidence
    actualFinalInterfaceIncidence
  assertEqual
    "checked local final interface edge set"
    interface
    (Set.fromList [edge | (edge, _, _) <- V.toList actualFinalInterfaceIncidence])
  traverse_
    (\(edge, protectedFace, finalPermittedFace) ->
       unless
         ( Set.notMember protectedFace permitted
             && Set.member finalPermittedFace finalPermitted
         )
         ( fail
             ( "checked local final interface incidence misclassified "
                 <> show (edge, protectedFace, finalPermittedFace)
             )
         )
    )
    actualFinalInterfaceIncidence
  assertEqual "checked local protected visit receipt" V.empty (refinementVisitedProtectedFaces receipt)
  assertEqual "checked local boundary crossing receipt" 0 (refinementAttemptedBoundaryCrossings receipt)
  let closureStats = refinementValidationClosureStats receipt
  unless
    ( validationClosureFaces closureStats > 0
        && validationClosureDirectedEdges closureStats > 0
        && validationClosureVertices closureStats > 0
    ) $
    fail "checked local refinement did not record its selected validation closure"
  assertEqual
    "checked local refinement does not enumerate resident unboxed base pages"
    0
    (publicationUnboxedBasePageEnumerations (refinementPublicationStats receipt))
  assertEqual
    "checked local protected constraint restriction"
    (constraintSegments source)
    (constraintSegments target)
  assertValid "checked local refinement" target
  case refineWithinDomain id crossingParameters permitted source of
    Left (RefinementDomainWouldCrossInterface _ _) -> pure ()
    Left obstruction -> fail ("checked local refinement returned the wrong crossing obstruction: " <> show obstruction)
    Right _ -> fail "checked local refinement silently crossed its immutable interface"
  case refineWithinDomain id calmParameters{refinePreserveConvexHull = False} permitted source of
    Left RefinementDomainRequiresConvexHullPreservation -> pure ()
    outcome -> fail ("checked local refinement accepted hull mutation: " <> either show (const "success") outcome)
  case refineWithinDomain id calmParameters{refineKeepConstraintEdges = False} permitted source of
    Left RefinementDomainRequiresConstraintPreservation -> pure ()
    outcome -> fail ("checked local refinement accepted constraint mutation: " <> either show (const "success") outcome)
  case refineWithinDomain id calmParameters{refineExcludeOuterFaces = True} permitted source of
    Left RefinementDomainForbidsOuterFaceExclusion -> pure ()
    outcome -> fail ("checked local refinement accepted outer-face exclusion: " <> either show (const "success") outcome)
  wholeRefined <-
    requireRight
      "checked whole-section refinement"
      ( refineWithinDomain
          id
          crossingParameters
          (Set.fromList (innerFaces source))
          source
      )
  let wholeResult = refinementDomainResult wholeRefined
      wholeReceipt = refinementDomainReceipt wholeRefined
  unless (refinementAddedVertices wholeResult > 0) $
    fail "checked whole-section refinement did not improve its admitted section"
  unless (not (V.null (refinementCreatedFaces wholeReceipt))) $
    fail "checked whole-section refinement omitted semantically rewritten face slots"
  unless
    ( V.any
        (\(FaceId raw) -> toInteger raw < toInteger (numFaces source))
        (refinementCreatedFaces wholeReceipt)
    ) $
    fail "checked whole-section refinement omitted recycled face slots"
  assertEqual
    "checked whole-section final permitted lineage"
    (Set.fromList (innerFaces (refinedTriangulation wholeResult)))
    (Set.fromList (V.toList (refinementFinalPermittedFaces wholeReceipt)))
  assertValid "checked whole-section refinement" (refinedTriangulation wholeResult)
 where
  faceCentroidX
    :: Triangulation mode vertex directed undirected face
    -> FaceId
    -> Double
  faceCentroidX triangulation face =
    case fmap (vertexPoint triangulation) (faceVertices triangulation face) of
      [] -> 0
      points ->
        sum [x | Point x _ <- points] / fromIntegral (length points)

-- | A maximum edge bound is a local geometric law, not a rendering preference:
-- a long, thin face is split along its longest edge even when its area is below
-- the minimum-area short-circuit. A budget-limited pass remains resumable;
-- once the longest edge is frozen at the local interface, a completed pass
-- returns the exact typed quality obstruction instead of crossing it.
testMaximumEdgeLengthQuality :: IO ()
testMaximumEdgeLengthQuality = do
  built <-
    requireRight
      "maximum-edge long-thin source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 10 0
              , Point 10 1
              , Point 0 1
              ]
          )
          (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0)])
      )
  let source = buildTriangulation built
      allPermitted = Set.fromList (innerFaces source)
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 1
          , refineMinArea = Just 6
          , refineMaxEdgeLength = Just 10.01
          , refineMaxRadiusEdgeRatio = Nothing
          , refineKeepConstraintEdges = True
          }
  partial <-
    requireRight
      "maximum-edge budget-limited refinement"
      (refineWithinDomain id parameters allPermitted source)
  let partialResult = refinementDomainResult partial
      partialTarget = refinedTriangulation partialResult
      partialReceipt = refinementDomainReceipt partial
      resumedPermitted = Set.fromList (V.toList (refinementFinalPermittedFaces partialReceipt))
  unless (refinementAddedVertices partialResult > 0) $
    fail "maximum-edge refinement did not split the long edge before the area floor"
  when (refinementComplete partialResult) $
    fail "maximum-edge budget-limited refinement reported completion"
  assertValid "maximum-edge budget-limited refinement" partialTarget
  resumed <-
    requireRight
      "maximum-edge resumed refinement"
      ( refineWithinDomain
          id
          parameters{refineMaxAdditionalVertices = Just 8}
          resumedPermitted
          partialTarget
      )
  let resumedResult = refinementDomainResult resumed
      resumedReceipt = refinementDomainReceipt resumed
  unless (refinementComplete resumedResult) $
    fail "maximum-edge resumed refinement did not drain its local worklist"
  assertMaximumEdgeLength
    "maximum-edge resumed refinement"
    10.01
    (refinedTriangulation resumedResult)
    (V.toList (refinementFinalPermittedFaces resumedReceipt))

  case
      refine
        id
        parameters
          { refineMaxAdditionalVertices = Just 8
          , refineMaxEdgeLength = Just 9
          }
        source of
    Left (RefinementOversizedEdge _ edge actual bound)
      | isConstraintEdge source edge && actual > bound -> pure ()
    Left obstruction ->
      fail ("maximum-edge global audit returned the wrong obstruction: " <> show obstruction)
    Right _ -> fail "maximum-edge global refinement accepted an oversized kept constraint"

  case innerFaces source of
    firstFace : _ -> do
      let permitted = Set.singleton firstFace
          interface =
            Set.fromList
              [ edge
              | edge <- undirectedEdges source
              , let (forward, backward) = directedPair edge
                    forwardFace = incidentFace source forward
                    backwardFace = incidentFace source backward
              , forwardFace /= outerFace
              , backwardFace /= outerFace
              , Set.member forwardFace permitted /= Set.member backwardFace permitted
              ]
      let blocked =
            refineWithinDomain
              id
              parameters{refineMaxAdditionalVertices = Just 8}
              permitted
              source
      case blocked of
        Left (RefinementOversizedEdge face edge actual bound)
          | Set.member face permitted
              && Set.member edge interface
              && actual > bound -> pure ()
        Left obstruction ->
          fail ("maximum-edge interface returned the wrong obstruction: " <> show obstruction)
        Right _ -> fail "maximum-edge interface was silently crossed or accepted"
    [] -> fail "maximum-edge source has no inner face"

-- | A circumcenter can be inside the admitted face while its legalization
-- cavity also meets the immutable interface. That is a true crossing demand,
-- not the boundary-limited locator read discharged by the local interpreter.
testLocalRefinementRejectsTrueInterfaceCrossing :: IO ()
testLocalRefinementRejectsTrueInterfaceCrossing = do
  built <-
    requireRight
      "deterministic true interface crossing source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 2 0
              , Point 1 (sqrt 3)
              , Point (-1) (sqrt 3)
              ]
          )
          V.empty
      )
  let source = buildTriangulation built
      equilateralFacePoints = sort [Point 0 0, Point 2 0, Point 1 (sqrt 3)]
      permittedFace =
        List.find
          ( \face ->
              sort (fmap (vertexPoint source) (faceVertices source face))
                == equilateralFacePoints
          )
          (innerFaces source)
  assertEqual "deterministic crossing source has two inner faces" 2 (numInnerFaces source)
  case permittedFace of
    Nothing -> fail "deterministic crossing source omitted its equilateral face"
    Just face -> do
      let permitted = Set.singleton face
          interface =
            Set.fromList
              [ edge
              | edge <- undirectedEdges source
              , let (forward, backward) = directedPair edge
                    forwardFace = incidentFace source forward
                    backwardFace = incidentFace source backward
              , forwardFace /= outerFace
              , backwardFace /= outerFace
              , Set.member forwardFace permitted /= Set.member backwardFace permitted
              ]
          parameters =
            defaultRefinementParameters
              { refineMaxAdditionalVertices = Just 1
              , refineMaxArea = Just 0.5
              , refineMaxRadiusEdgeRatio = Nothing
              , refineKeepConstraintEdges = True
              }
      assertEqual
        "deterministic crossing source has one interface edge"
        1
        (Set.size interface)
      case refineWithinDomain id parameters permitted source of
        Left (RefinementDomainWouldCrossInterface _ _) -> pure ()
        Left obstruction ->
          fail
            ( "deterministic true interface crossing returned the wrong obstruction: "
                <> show obstruction
            )
        Right _ ->
          fail "deterministic true interface crossing was silently accepted"

testRepeatedBoundaryAdjacentRefinement :: IO ()
testRepeatedBoundaryAdjacentRefinement = do
  built <-
    requireRight
      "repeated boundary-adjacent constrained source"
      ( constrainedDelaunay
          unitElementDefaults
          ( V.fromList
              [ Point 0 0
              , Point 4 0
              , Point 8 0
              , Point 12 0
              , Point 0 4
              , Point 4 4
              , Point 8 4
              , Point 12 4
              ]
          )
          (V.singleton (2, 6))
      )
  let source = buildTriangulation built
      sourcePermitted = leftSection source
      fullParameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just 20
          , refineMaxArea = Just 1
          , refineMaxRadiusEdgeRatio = Nothing
          , refineKeepConstraintEdges = True
          }
  fullRefinement <-
    requireRight
      "full boundary-adjacent refinement"
      (refineWithinDomain id fullParameters sourcePermitted source)
  let fullResult = refinementDomainResult fullRefinement
      fullTarget = refinedTriangulation fullResult
      fullReceipt = refinementDomainReceipt fullRefinement
  unless (refinementAddedVertices fullResult > 0) $
    fail "full boundary-adjacent refinement did not refine its admitted section"
  unless (refinementInterfaceBoundaryReads fullReceipt > 0) $
    fail "boundary-adjacent refinement did not report its immutable interface read"
  assertEqual
    "boundary-adjacent crossing attempts"
    0
    (refinementAttemptedBoundaryCrossings fullReceipt)
  assertEqual
    "full boundary-adjacent constraint restriction"
    (constraintSegments source)
    (constraintSegments fullTarget)
  assertEqual
    "completed boundary-adjacent refinement is idempotent"
    True
    (refinementComplete fullResult)

  let progressParameters = fullParameters{refineMaxAdditionalVertices = Just 1}
  progressFirstRefinement <-
    requireRight
      "first bounded boundary-adjacent refinement"
      (refineWithinDomain id progressParameters sourcePermitted source)
  let progressFirstResult = refinementDomainResult progressFirstRefinement
      progressFirstTarget = refinedTriangulation progressFirstResult
      progressFirstReceipt = refinementDomainReceipt progressFirstRefinement
      progressPermitted =
        Set.fromList (V.toList (refinementFinalPermittedFaces progressFirstReceipt))
  unless (refinementAddedVertices progressFirstResult > 0) $
    fail "first bounded boundary-adjacent refinement did not refine its admitted section"
  when (refinementComplete progressFirstResult) $
    fail "first bounded boundary-adjacent refinement unexpectedly reached a fixpoint"
  progressSecondRefinement <-
    requireRight
      "repeated boundary-adjacent refinement"
      ( refineWithinDomain
          id
          progressParameters
          progressPermitted
          progressFirstTarget
      )
  let progressSecondResult = refinementDomainResult progressSecondRefinement
      progressSecondTarget = refinedTriangulation progressSecondResult
      progressSecondReceipt = refinementDomainReceipt progressSecondRefinement
  unless (refinementAddedVertices progressSecondResult > 0) $
    fail "repeated boundary-adjacent refinement did not retain its dynamic join-face support"
  assertEqual
    "repeated boundary-adjacent protected visits"
    V.empty
    (refinementVisitedProtectedFaces progressSecondReceipt)
  assertEqual
    "repeated boundary-adjacent constraint restriction"
    (constraintSegments progressFirstTarget)
    (constraintSegments progressSecondTarget)
  assertValid "repeated boundary-adjacent refinement" progressSecondTarget
 where
  leftSection
    :: Triangulation mode vertex directed undirected face
    -> Set.Set FaceId
  leftSection triangulation =
    Set.fromList
      [ face
      | face <- innerFaces triangulation
      , faceCentroidX triangulation face < 8
      ]
  faceCentroidX
    :: Triangulation mode vertex directed undirected face
    -> FaceId
    -> Double
  faceCentroidX triangulation face =
    case fmap (vertexPoint triangulation) (faceVertices triangulation face) of
      [] -> 0
      points ->
        sum [x | Point x _ <- points] / fromIntegral (length points)

testFaceComponentsAndBoundaries :: IO ()
testFaceComponentsAndBoundaries = do
  emptyBuild <- requirePointBuild "empty region components" []
  collinearBuild <-
    requirePointBuild
      "collinear region components"
      [Point 0 0, Point 1 0, Point 2 0]
  assertEqual
    "empty mesh has no face components"
    ([] :: [(Bool, FaceComponent)])
    (faceComponents (buildTriangulation emptyBuild) (const True))
  assertEqual
    "collinear mesh has no face components"
    ([] :: [(Bool, FaceComponent)])
    (faceComponents (buildTriangulation collinearBuild) (const True))

  triangle <-
    regionMeshFromPoints
      "single triangle region"
      [Point 0 0, Point 2 0, Point 0 2]
  triangleBoundary <-
    requireComponentBoundary "single triangle" True triangle (const True)
  assertBoundaryShape "single triangle" triangle 3 [] triangleBoundary

  square <-
    regionMeshFromPoints
      "uniform square region"
      [Point 0 0, Point 2 0, Point 2 2, Point 0 2]
  squareComponent <-
    requireLabelledComponent
      "uniform square component"
      True
      (faceComponents square (const True))
  assertEqual
    "component provenance mismatch is typed before DCEL lookup"
    (Left (BoundaryComponentFaceOutOfRange (FaceId 2) 2))
    (componentBoundary triangle squareComponent)
  assertEqual
    "component loop provenance mismatch is typed before DCEL lookup"
    (Left (BoundaryComponentFaceOutOfRange (FaceId 2) 2))
    (componentBoundaryLoops triangle squareComponent)
  squareBoundary <- requireRight "uniform square boundary" (componentBoundary square squareComponent)
  squareLoops <-
    requireRight
      "uniform square oriented boundary loops"
      (componentBoundaryLoops square squareComponent)
  repeatedSquareBoundary <-
    requireRight "repeated uniform square boundary" (componentBoundary square squareComponent)
  assertEqual "boundary extraction is deterministic" squareBoundary repeatedSquareBoundary
  assertEqual
    "ordinary oriented loops agree with strict boundary"
    (regionBoundaryOuterLoop squareBoundary :| regionBoundaryHoleLoops squareBoundary)
    squareLoops
  assertEqual
    "uniform square drops its Delaunay diagonal"
    (Set.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2])
    (loopPointSet square (regionBoundaryOuterLoop squareBoundary))
  assertBoundaryShape "uniform square" square 4 [] squareBoundary

  let splitComponents =
        faceComponents square (regionFaceSatisfies square (\point -> pointX point < 1))
  assertEqual "split square component count" 2 (length splitComponents)
  traverse_
    (\(_, component) -> do
       boundary <- requireRight "split square boundary" (componentBoundary square component)
       assertBoundaryShape "split square component" square 3 [] boundary)
    splitComponents

  collinearHull <-
    regionMeshFromPoints
      "collinear hull simplification"
      [ Point 0 0
      , Point 1 0
      , Point 2 0
      , Point 2 2
      , Point 0 2
      , Point 1 1
      ]
  collinearBoundary <-
    requireComponentBoundary "collinear hull" True collinearHull (const True)
  assertEqual
    "exact simplification drops a redundant hull site"
    (Set.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2])
    (loopPointSet collinearHull (regionBoundaryOuterLoop collinearBoundary))
  assertEqual
    "resident boundary retains every hull edge endpoint"
    (Set.fromList [Point 0 0, Point 1 0, Point 2 0, Point 2 2, Point 0 2])
    (residentLoopPointSet collinearHull (regionBoundaryOuterLoop collinearBoundary))
  assertEqual
    "resident boundary follows exact DCEL edges"
    True
    (residentBoundaryIsEdgeCycle collinearHull (regionBoundaryOuterLoop collinearBoundary))

  lShape <- regionMesh "concave L region" 2 2
  let inL = regionFaceSatisfies lShape (\(Point x y) -> not (x > 1 && y > 1))
  lBoundary <- requireComponentBoundary "concave L" True lShape inL
  assertEqual
    "concave L boundary"
    ( Set.fromList
        [ Point 0 0
        , Point 2 0
        , Point 2 1
        , Point 1 1
        , Point 1 2
        , Point 0 2
        ]
    )
    (loopPointSet lShape (regionBoundaryOuterLoop lBoundary))
  assertBoundaryShape "concave L" lShape 6 [] lBoundary

  annulus <- regionMesh "annulus region" 3 3
  let outsideCenter =
        regionFaceSatisfies annulus (\(Point x y) -> not (x > 1 && x < 2 && y > 1 && y < 2))
  annulusBoundary <-
    requireComponentBoundary "annulus outer" True annulus outsideCenter
  assertBoundaryShape "annulus" annulus 4 [4] annulusBoundary

  twoHoles <- regionMesh "ordered hole regions" 5 3
  let outsideTwoCells =
        regionFaceSatisfies twoHoles $ \(Point x y) ->
          let cell = (floor x :: Int, floor y :: Int)
           in cell /= (1, 1) && cell /= (3, 1)
  twoHoleBoundary <-
    requireComponentBoundary "two-hole outer" True twoHoles outsideTwoCells
  assertBoundaryShape "two-hole" twoHoles 4 [4, 4] twoHoleBoundary

  disconnected <- regionMesh "disconnected equal labels" 3 1
  let outsideMiddle =
        regionFaceSatisfies disconnected (\(Point x _) -> x < 1 || x > 2)
      disconnectedComponents = faceComponents disconnected outsideMiddle
      equalLabelComponents =
        [component | (True, component) <- disconnectedComponents]
  assertEqual "equal labels remain two disconnected components" 2 (length equalLabelComponents)
  assertEqual
    "face component order is deterministic"
    disconnectedComponents
    (faceComponents disconnected outsideMiddle)

  pinched <- regionMesh "pinched region" 3 3
  let pinchedSelection =
        regionFaceSatisfies pinched $ \(Point x y) ->
          let cell = (floor x :: Int, floor y :: Int)
           in cell /= (0, 0) && cell /= (1, 1)
  pinchedComponent <-
    requireLabelledComponent
      "pinched selected component"
      True
      (faceComponents pinched pinchedSelection)
  pinchedLoops <-
    requireRight
      "pinched oriented boundary loops"
      (componentBoundaryLoops pinched pinchedComponent)
  let pinchedLoopList = toList pinchedLoops
  assertEqual "pinched simple loop count" 2 (length pinchedLoopList)
  assertEqual
    "pinched simple loop vertex counts"
    [4, 6]
    (sort (fmap (length . boundaryLoopVertices) pinchedLoopList))
  assertEqual
    "pinched loop orientations"
    [BoundaryCounterClockwise, BoundaryClockwise]
    (sort (fmap boundaryLoopOrientation pinchedLoopList))
  traverse_
    (\loop -> do
       let loopVertexIds = toList (boundaryLoopVertices loop)
       assertEqual
         "pinched loop has no repeated vertex"
         (length loopVertexIds)
         (Set.size (Set.fromList loopVertexIds))
       assertLoopWinding
         "pinched loop winding agrees with orientation"
         (case boundaryLoopOrientation loop of
            BoundaryCounterClockwise -> GT
            BoundaryClockwise -> LT)
         pinched
         loop)
    pinchedLoopList
  case componentBoundary pinched pinchedComponent of
    Left (BoundaryPinch vertex firstEdge secondEdge) -> do
      assertEqual "pinch vertex" (Point 1 1) (vertexPoint pinched vertex)
      when (firstEdge == secondEdge) $
        fail ("pinch repeated one outgoing edge: " <> show firstEdge)
    other -> fail ("pinched boundary produced " <> show other)

testAlphaFaceFiltration :: IO ()
testAlphaFaceFiltration = do
  triangle <-
    regionMeshFromPoints
      "alpha right triangle"
      [Point 0 0, Point 2 0, Point 0 2]
  face <- case innerFaces triangle of
    [singleFace] -> pure singleFace
    faces -> fail ("alpha fixture faces: " <> show faces)
  exactThreshold <- requireRight "exact alpha threshold" (mkRadiusSquared 2)
  lowerThreshold <- requireRight "lower alpha threshold" (mkRadiusSquared 1.999)
  zeroThreshold <- requireRight "zero alpha threshold" (mkRadiusSquared 0)
  assertEqual "zero alpha threshold excludes face" False
    (alphaShapeContainsFace zeroThreshold triangle face)
  assertEqual "closed alpha threshold includes equality" True (alphaShapeContainsFace exactThreshold triangle face)
  assertEqual "lower alpha threshold excludes face" False (alphaShapeContainsFace lowerThreshold triangle face)
  assertEqual "outer face is absent from alpha filtration" False (alphaShapeContainsFace exactThreshold triangle outerFace)

  let roundedPoints =
        ( Point (-20) (-20)
        , Point (-19) (-13)
        , Point (-2) (-19)
        )
      (roundedFirst, roundedSecond, roundedThird) = roundedPoints
      roundedRadiusSquared = 84.5
  roundedTriangle <-
    regionMeshFromPoints
      "rounded alpha equality"
      [roundedFirst, roundedSecond, roundedThird]
  roundedFace <- case innerFaces roundedTriangle of
    [singleFace] -> pure singleFace
    faces -> fail ("rounded alpha fixture faces: " <> show faces)
  roundedThreshold <-
    requireRight
      "rounded exact alpha threshold"
      (mkRadiusSquared roundedRadiusSquared)
  roundedLowerThreshold <-
    requireRight
      "rounded lower alpha threshold"
      (mkRadiusSquared (roundedRadiusSquared - encodeFloat 1 (-46)))
  assertEqual
    "closed alpha equality is exact"
    True
    (alphaShapeContainsFace roundedThreshold roundedTriangle roundedFace)
  assertEqual
    "one binary64 step below exact alpha equality is excluded"
    False
    (alphaShapeContainsFace roundedLowerThreshold roundedTriangle roundedFace)

  alphaBoundary <-
    requireComponentBoundary
      "alpha"
      True
      triangle
      (alphaShapeContainsFace exactThreshold triangle)
  assertBoundaryShape "alpha component" triangle 3 [] alphaBoundary

  annulus <-
    regionMeshFromPoints
      "alpha annulus"
      (ringPoints 4 32 <> ringPoints 2 16 <> ringPoints 3 24)
  annulusThreshold <-
    requireRight "alpha annulus threshold" (mkRadiusSquared 0.4)
  annulusBoundary <-
    requireComponentBoundary
      "alpha annulus"
      True
      annulus
      (alphaShapeContainsFace annulusThreshold annulus)
  assertEqual
    "alpha annulus hole count"
    1
    (length (regionBoundaryHoleLoops annulusBoundary))
  assertLoopWinding
    "alpha annulus outer winding"
    GT
    annulus
    (regionBoundaryOuterLoop annulusBoundary)
  traverse_
    (assertLoopWinding "alpha annulus hole winding" LT annulus)
    (regionBoundaryHoleLoops annulusBoundary)
  traverse_
    (\(label, value, failure) ->
       assertEqual label (Left failure) (mkRadiusSquared value))
    [ ("negative alpha threshold refusal", -1, NegativeRadiusSquared (-1))
    , ("NaN alpha threshold refusal", 0 / 0, NonFiniteRadiusSquared ValueNaN)
    , ("positive-infinite alpha threshold refusal", 1 / 0, NonFiniteRadiusSquared ValuePositiveInfinity)
    , ("negative-infinite alpha threshold refusal", (-1) / 0, NonFiniteRadiusSquared ValueNegativeInfinity)
    ]

ringPoints :: Double -> Int -> [Point]
ringPoints radius count =
  [ Point (radius * cos angle) (radius * sin angle)
  | index <- [0 .. count - 1]
  , let angle = 2 * pi * fromIntegral index / fromIntegral count
  ]

regionMesh :: String -> Int -> Int -> IO NativeMesh
regionMesh label widthInCells heightInCells =
  regionMeshFromPoints
    label
    [ Point (fromIntegral x) (fromIntegral y)
    | y <- [0 .. heightInCells]
    , x <- [0 .. widthInCells]
    ]

regionMeshFromPoints :: String -> [Point] -> IO NativeMesh
regionMeshFromPoints label points =
  buildTriangulation <$> requirePointBuild label points

regionFaceCentroid
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> Maybe Point
regionFaceCentroid triangulation face =
  (\(first, second, third) ->
     centroid
       (vertexPoint triangulation first)
       (vertexPoint triangulation second)
       (vertexPoint triangulation third))
    <$> Dcel.innerFaceVertices triangulation face

regionFaceSatisfies
  :: Triangulation mode vertex directed undirected face
  -> (Point -> Bool)
  -> FaceId
  -> Bool
regionFaceSatisfies triangulation predicate =
  maybe False predicate . regionFaceCentroid triangulation

requireLabelledComponent
  :: (Eq label, Show label)
  => String
  -> label
  -> [(label, FaceComponent)]
  -> IO FaceComponent
requireLabelledComponent label expected components =
  case [component | (actual, component) <- components, actual == expected] of
    [component] -> pure component
    matches ->
      fail
        ( label
            <> ": expected one component for label "
            <> show expected
            <> ", got "
            <> show (length matches)
        )

requireComponentBoundary
  :: (Eq label, Show label)
  => String
  -> label
  -> Triangulation mode vertex directed undirected face
  -> (FaceId -> label)
  -> IO RegionBoundary
requireComponentBoundary label expected triangulation labelFace = do
  component <-
    requireLabelledComponent
      (label <> " component")
      expected
      (faceComponents triangulation labelFace)
  requireRight (label <> " boundary") (componentBoundary triangulation component)

loopPointSet
  :: Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> Set.Set Point
loopPointSet triangulation =
  Set.fromList
    . fmap (vertexPoint triangulation)
    . toList
    . boundaryLoopVertices

residentLoopPointSet
  :: Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> Set.Set Point
residentLoopPointSet triangulation =
  Set.fromList
    . fmap (vertexPoint triangulation)
    . toList
    . boundaryLoopResidentVertices

residentBoundaryIsEdgeCycle
  :: Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> Bool
residentBoundaryIsEdgeCycle triangulation loop =
  let first :| remaining = boundaryLoopResidentVertices loop
      adjacentPairs = zip (first : remaining) (remaining <> [first])
   in all
        (\(fromVertex, toVertex) ->
           any
             ((== toVertex) . destination triangulation)
             (vertexOutgoingEdges triangulation fromVertex))
        adjacentPairs

assertLoopWinding
  :: String
  -> Ordering
  -> Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> IO ()
assertLoopWinding label expected triangulation loop =
  let first :| remaining = fmap (vertexPoint triangulation) (boundaryLoopVertices loop)
      points = first : remaining
      twiceArea =
        sum
          ( zipWith
              (\(Point ax ay) (Point bx by) -> ax * by - ay * bx)
              points
              (remaining <> [first])
          )
   in assertEqual label expected (compare twiceArea 0)

assertBoundaryShape
  :: String
  -> Triangulation mode vertex directed undirected face
  -> Int
  -> [Int]
  -> RegionBoundary
  -> IO ()
assertBoundaryShape label triangulation outerVertexCount holeVertexCounts boundary = do
  assertEqual
    (label <> " outer orientation")
    BoundaryCounterClockwise
    (boundaryLoopOrientation (regionBoundaryOuterLoop boundary))
  assertEqual (label <> " outer vertex count") outerVertexCount
    (length (boundaryLoopVertices (regionBoundaryOuterLoop boundary)))
  assertLoopWinding (label <> " outer winding") GT triangulation
    (regionBoundaryOuterLoop boundary)
  let holes = regionBoundaryHoleLoops boundary
  assertEqual (label <> " hole count") (length holeVertexCounts) (length holes)
  traverse_
    (\(vertexCount, hole) -> do
       assertEqual
         (label <> " hole orientation")
         BoundaryClockwise
         (boundaryLoopOrientation hole)
       assertEqual (label <> " hole vertex count") vertexCount
         (length (boundaryLoopVertices hole))
       assertLoopWinding (label <> " hole winding") LT triangulation hole)
    (zip holeVertexCounts holes)

testTraversal :: IO ()
testTraversal = do
  built <- requirePointBuild "traversal" [Point (-3) 0, Point (-1) (-2), Point (-1) 2, Point 1 (-2), Point 1 2, Point 3 0]
  forwardStart <- requireQueryPoint "forward traversal start" (Point (-4) 0)
  forwardEnd <- requireQueryPoint "forward traversal end" (Point 4 0)
  let triangulation = buildTriangulation built
      forward = lineIntersections triangulation forwardStart forwardEnd
      backward = lineIntersections triangulation forwardEnd forwardStart
  when (null forward) $ fail "ordered line traversal crossed nothing"
  assertEqual
    "reversing the segment reverses crossing order and orientation"
    (map reverseTraversalEvent forward)
    (reverse backward)
  interiorEnd <- requireQueryPoint "outside traversal interior end" (Point 0.25 0.5)
  let outsideToInterior = lineIntersections triangulation forwardStart interiorEnd
      interiorToOutside = lineIntersections triangulation interiorEnd forwardStart
  when (null outsideToInterior) $ fail "outside-to-interior traversal crossed nothing"
  assertEqual
    "endpoint-directed outside traversal preserves order and orientation"
    (map reverseTraversalEvent outsideToInterior)
    (reverse interiorToOutside)
  missFrom <- requireQueryPoint "outside traversal miss from" (Point (-4) 4)
  missTo <- requireQueryPoint "outside traversal miss to" (Point 4 4)
  assertEqual
    "outside segment missing the hull reports no intersections"
    []
    (lineIntersections triangulation missFrom missTo)
  circleEdges <- Set.fromList <$> requireRight "circle edge query" (edgesInCircle triangulation (Point 0 0) 4)
  let bruteCircle = Set.fromList
        [ edge
        | edge <- undirectedEdges triangulation
        , let (a, b) = undirectedEndpoints triangulation edge
        , segmentDistanceSquared (vertexPoint triangulation a) (vertexPoint triangulation b) (Point 0 0) <= 4
        ]
  assertEqual "circle edge flood" bruteCircle circleEdges
  assertEqual
    "negative circle metric refusal"
    (Left (InvalidCircleRadius (NegativeRadiusSquared (-1))))
    (circleMetric (Point 0 0 :: Point) (-1))
  assertEqual
    "negative circle edge-query refusal"
    (Left (InvalidCircleRadius (NegativeRadiusSquared (-1))))
    (edgesInCircle triangulation (Point 0 0) (-1))
  assertEqual
    "negative circle vertex-query refusal"
    (Left (InvalidCircleRadius (NegativeRadiusSquared (-1))))
    (verticesInCircle triangulation (Point 0 0) (-1))
  assertEqual
    "NaN circle metric refusal"
    (Left (InvalidCircleRadius (NonFiniteRadiusSquared ValueNaN)))
    (circleMetric (Point 0 0 :: Point) (0 / 0))
  assertEqual
    "NaN circle center-x refusal"
    (Left (InvalidCircleCenter (InvalidPointX CoordinateNaN)))
    (circleMetric (Point (0 / 0) 0 :: Point) 1)
  assertEqual
    "positive-infinite circle center-y refusal"
    (Left (InvalidCircleCenter (InvalidPointY CoordinateInfinite)))
    (edgesInCircle triangulation (Point 0 (1 / 0)) 1)
  assertEqual
    "negative-infinite circle center-x refusal"
    (Left (InvalidCircleCenter (InvalidPointX CoordinateInfinite)))
    (verticesInCircle triangulation (Point ((-1) / 0) 0) 1)
  assertEqual
    "infinite circle edge-query refusal"
    (Left (InvalidCircleRadius (NonFiniteRadiusSquared ValuePositiveInfinity)))
    (edgesInCircle triangulation (Point 0 0) (1 / 0))
  assertEqual
    "negative-infinite circle vertex-query refusal"
    (Left (InvalidCircleRadius (NonFiniteRadiusSquared ValueNegativeInfinity)))
    (verticesInCircle triangulation (Point 0 0) ((-1) / 0))
  rectangleVertices <-
    Set.fromList
      <$> requireRight
        "rectangle query"
        (verticesInRectangle triangulation (Point (-1.1) (-2.1)) (Point 1.1 2.1))
  let bruteVertices = Set.fromList
        [ vertex
        | vertex <- vertices triangulation
        , let Point x y = vertexPoint triangulation vertex
        , x >= (-1.1), x <= 1.1, y >= (-2.1), y <= 2.1
        ]
  assertEqual "rectangle vertex flood" bruteVertices rectangleVertices

testRandomizedConstruction :: IO ()
testRandomizedConstruction = do
  forM_ [0 .. 31] $ \index -> do
    let points = randomPoints (0x9e37_79b9 + fromIntegral index) (40 + index * 7)
    built <- requirePointBuild ("random build " <> show index) points
    let triangulation = buildTriangulation built
    assertValid ("random build " <> show index) triangulation
    assertEqual "random input mapping" (length points) (sizeofPrimArray (buildInputVertices built))

testErrors :: IO ()
testErrors = do
  assertEqual
    "NaN query x refusal"
    (Left (InvalidPointX CoordinateNaN))
    (mkQueryPoint (Point (0 / 0) 0))
  assertEqual
    "infinite query y refusal"
    (Left (InvalidPointY CoordinateInfinite))
    (mkQueryPoint (Point 0 (1 / 0)))
  assertEqual
    "negative-infinite query x refusal"
    (Left (InvalidPointX CoordinateInfinite))
    (mkQueryPoint (Point ((-1) / 0) 0))
  case delaunay unitElementDefaults (V.singleton (Point (0 / 0) 0 :: Point)) of
    Left (InvalidCoordinate (Just 0) _ CoordinateNaN) -> pure ()
    Left other -> fail ("NaN insertion result: " <> show other)
    Right built ->
      fail
        ( "NaN insertion result: built a triangulation of "
            <> show (numVertices (buildTriangulation built))
            <> " vertices"
        )
  expectBuildFailure
    "NaN minimum angle"
    (RefinementMinimumAngleNotFinite ValueNaN)
    (withMinimumAngle (0 / 0 :: Double) defaultRefinementParameters)
  expectBuildFailure
    "infinite minimum angle"
    (RefinementMinimumAngleNotFinite ValuePositiveInfinity)
    (withMinimumAngle (1 / 0 :: Double) defaultRefinementParameters)
  expectBuildFailure
    "out-of-range minimum angle"
    (RefinementMinimumAngleOutOfRange 61)
    (withMinimumAngle (61 :: Double) defaultRefinementParameters)
  expectBuildFailure
    "minimum angle whose derived ratio overflows"
    (RefinementMinimumAngleDerivedRatioNotFinite ValuePositiveInfinity)
    (withMinimumAngle (encodeFloat 1 (-1074) :: Double) defaultRefinementParameters)
  square <- requirePointBuild "area validation square" [Point 0 0, Point 1 0, Point 1 1, Point 0 1]
  let refineWith parameters =
        refine id parameters (buildTriangulation square)
      refineWithArea area =
        refineWith defaultRefinementParameters{refineMaxArea = Just area}
  expectBuildFailure
    "negative refinement vertex budget"
    (RefinementMaximumAdditionalVerticesNegative (-1))
    (refineWith defaultRefinementParameters{refineMaxAdditionalVertices = Just (-1)})
  expectBuildFailure
    "infinite minimum area"
    (RefinementMinimumAreaNotFinite ValuePositiveInfinity)
    (refineWith defaultRefinementParameters{refineMinArea = Just (1 / 0)})
  expectBuildFailure
    "negative minimum area"
    (RefinementMinimumAreaNegative (-1))
    (refineWith defaultRefinementParameters{refineMinArea = Just (-1)})
  expectBuildFailure
    "NaN maximum area"
    (RefinementMaximumAreaNotFinite ValueNaN)
    (refineWithArea (0 / 0))
  expectBuildFailure
    "infinite maximum area"
    (RefinementMaximumAreaNotFinite ValuePositiveInfinity)
    (refineWithArea (1 / 0))
  expectBuildFailure
    "zero maximum area"
    (RefinementMaximumAreaNotPositive 0)
    (refineWithArea 0)
  expectBuildFailure
    "negative maximum area"
    (RefinementMaximumAreaNotPositive (-1))
    (refineWithArea (-1))
  expectBuildFailure
    "infinite maximum radius/edge ratio"
    (RefinementMaximumRadiusEdgeRatioNotFinite ValuePositiveInfinity)
    (refineWith defaultRefinementParameters{refineMaxRadiusEdgeRatio = Just (1 / 0)})
  expectBuildFailure
    "non-positive maximum radius/edge ratio"
    (RefinementMaximumRadiusEdgeRatioNotPositive 0)
    (refineWith defaultRefinementParameters{refineMaxRadiusEdgeRatio = Just 0})
  expectBuildFailure
    "infinite maximum edge length"
    (RefinementMaximumEdgeLengthNotFinite ValuePositiveInfinity)
    (refineWith defaultRefinementParameters{refineMaxEdgeLength = Just (1 / 0)})
  expectBuildFailure
    "non-positive maximum edge length"
    (RefinementMaximumEdgeLengthNotPositive 0)
    (refineWith defaultRefinementParameters{refineMaxEdgeLength = Just 0})
  expectBuildFailure
    "minimum area above maximum area"
    (RefinementMinimumAreaExceedsMaximum 2 1)
    ( refineWith
        defaultRefinementParameters
          { refineMinArea = Just 2
          , refineMaxArea = Just 1
          }
    )
  case refineWithArea 0.25 of
    Right _ -> pure ()
    Left failure -> fail ("positive maximum area rejected: " <> show failure)

expectBuildFailure :: String -> BuildError -> Either BuildError value -> IO ()
expectBuildFailure label expected outcome =
  case outcome of
    Left actual -> assertEqual label expected actual
    Right _ -> fail (label <> ": expected " <> show expected <> ", got success")


canonicalEdges
  :: Triangulation mode vertex directed undirected face
  -> Set.Set (Point, Point)
canonicalEdges triangulation =
  Set.fromList
    [ ordered (vertexPoint triangulation (origin triangulation edge)) (vertexPoint triangulation (destination triangulation edge))
    | undirected <- undirectedEdges triangulation
    , let edge = normalizedDirected undirected
    ]
 where
  ordered :: Ord value => value -> value -> (value, value)
  ordered left right = if left <= right then (left, right) else (right, left)

requireJust :: String -> Maybe value -> IO value
requireJust _ (Just value) = pure value
requireJust label Nothing = fail (label <> ": expected Just")

requireAcceptedConstraint
  :: String
  -> ConstraintBatchResult vertex directed undirected face
  -> IO (V.Vector DirectedEdgeId, Int)
requireAcceptedConstraint label batch =
  case V.toList (constraintBatchOutcomes batch) of
    [ConstraintAccepted path added] -> pure (path, added)
    outcomes ->
      fail
        ( label
            <> ": expected one accepted constraint, got "
            <> show outcomes
        )

assertBatchStats
  :: String
  -> ConstraintBatchResult vertex directed undirected face
  -> IO ()
assertBatchStats label batch = do
  let stats = constraintBatchStats batch
      (accepted, rejected) =
        V.foldl'
          (\(!acceptedCount, !rejectedCount) outcome ->
            case outcome of
              ConstraintAccepted _ _ -> (acceptedCount + 1, rejectedCount)
              ConstraintRejected _ -> (acceptedCount, rejectedCount + 1)
          )
          (0, 0)
          (constraintBatchOutcomes batch)
  assertEqual
    (label <> " request count")
    (V.length (constraintBatchOutcomes batch))
    (constraintBatchRequests stats)
  assertEqual
    (label <> " accepted count")
    accepted
    (constraintBatchAccepted stats)
  assertEqual
    (label <> " rejected count")
    rejected
    (constraintBatchRejected stats)

replayConstraintRequest
  :: (ConstrainedDelaunayTriangulation (Point), [ConstraintOutcome])
  -> (VertexId, VertexId)
  -> Either
      (CdtError)
      (ConstrainedDelaunayTriangulation (Point), [ConstraintOutcome])
replayConstraintRequest (current, outcomes) request = do
  singleton <- recoverConstraints current (V.singleton request)
  case V.toList (constraintBatchOutcomes singleton) of
    [outcome] ->
      Right
        ( constraintBatchTriangulation singleton
        , outcome : outcomes
        )
    cardinality ->
      Left
        ( ConstraintBatchCardinalityMismatch
            1
            (length cardinality)
        )

requirePointBuild :: String -> [Point] -> IO (BuildResult 'Unconstrained (Point) () () ())
requirePointBuild label points = requireRight label (delaunay unitElementDefaults (V.fromList points))

assertCdtValid :: String -> Triangulation 'Constrained vertex () () () -> IO ()
assertCdtValid label triangulation =
  case validateTriangulation triangulation of
    [] -> pure ()
    violations -> fail (label <> " CDT violations: " <> show violations)

assertMaximumEdgeLength
  :: String
  -> Double
  -> Triangulation mode vertex directed undirected face
  -> [FaceId]
  -> IO ()
assertMaximumEdgeLength label maximumLength triangulation faces =
  unless (null violations) $
    fail (label <> " oversized edges: " <> show (take 1 violations))
 where
  !maximumSquaredLength = maximumLength * maximumLength
  violations =
    [ (face, edge, squaredLength)
    | face <- faces
    , directed <- faceDirectedEdges triangulation face
    , let edge = asUndirected directed
          (fromVertex, toVertex) = undirectedEndpoints triangulation edge
          squaredLength =
            squaredDistanceWide
              (vertexPoint triangulation fromVertex)
              (vertexPoint triangulation toVertex)
    , squaredLength > maximumSquaredLength
    ]

canonicalSegmentSquaredLength :: CanonicalSegment -> Double
canonicalSegmentSquaredLength segmentValue =
  squaredDistanceWide
    (segmentStart segmentValue)
    (segmentEnd segmentValue)

assertNear :: String -> Double -> Double -> Double -> IO ()
assertNear label tolerance expected actual =
  unless (abs (expected - actual) <= tolerance * max 1 (max (abs expected) (abs actual))) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

gridPoints :: Int -> Int -> [Point]
gridPoints width height =
  [ Point (fromIntegral x + jitter x y) (fromIntegral y + jitter y x)
  | y <- [0 .. height - 1]
  , x <- [0 .. width - 1]
  ]
 where
  jitter :: Int -> Int -> Double
  jitter a b = fromIntegral ((a * 17 + b * 31) `mod` 11) * 1.0e-5

randomPoints :: Word64 -> Int -> [Point]
randomPoints seed count = take count (go seed Set.empty)
 where
  go :: Word64 -> Set.Set (Point) -> [Point]
  go state seen =
    let state1 = lcg state
        state2 = lcg state1
        x = unit state1 * 2 - 1
        y = unit state2 * 2 - 1
        point = Point x y
     in if Set.member point seen
          then go state2 seen
          else point : go state2 (Set.insert point seen)

  unit :: Word64 -> Double
  unit value = fromIntegral (value `mod` 9_007_199_254_740_881) / 9_007_199_254_740_881

  lcg :: Word64 -> Word64
  lcg value = value * 6_364_136_223_846_793_005 + 1_442_695_040_888_963_407

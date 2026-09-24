{-# LANGUAGE NumericUnderscores #-}

-- | Edit sessions, point-index transitions, and persistent publication laws.
module Moonlight.Planar.EditSpec
  ( tests
  ) where

import Control.DeepSeq ( force )
import Control.Exception ( evaluate )
import Control.Monad ( unless, void )
import Control.Monad.ST ( runST )
import Data.Maybe ( isJust )
import Data.Primitive.PrimArray ( indexPrimArray, sizeofPrimArray )
import GHC.Stats ( allocated_bytes, getRTSStats, getRTSStatsEnabled )
import Moonlight.Planar.BulkLoad ( delaunay, insert )
import Moonlight.Planar.Dcel ( numVertices, undirectedEndpoints, vertexData, vertexPoint )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( undirectedEdges )
import Moonlight.Planar.HintGenerator ( buildHierarchyHint, defaultHierarchyBranchFactor,
  rebuildHierarchyHint, removeManyWithHierarchy )
import Moonlight.Planar.Internal.Canonical ( canonicalize )
import Moonlight.Planar.Internal.HandleDefs ( VertexId(VertexId) )
import Moonlight.Planar.Internal.PointIndex ( MutablePointIndexUpdate(..), lookupMutablePoint,
  newMutablePointIndex, relocateMutablePoint, removeMutablePoint, seedMutablePointIndex )
import Moonlight.Planar.Internal.Session ( excise )
import Moonlight.Planar.MeshFixtures ( requirePointBuild, edgeKeys )
import Moonlight.Planar.PayloadFixtures ( SampleVertex(..) )
import Moonlight.Planar.Removal ( removeVertex, RemovalOutcome(removalOutcomeSwap,
  removalOutcomePoint), RemovalResult(removalTriangulation, removalOutcome, removalStats),
  locateAndRemove )
import Moonlight.Planar.Session ( withSession, insertVertex, insertVertexAt, refuse, removeAt,
  removeManyAt, Session )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (unitElementDefaults, BuildError(RemovalVertexOutOfRange), ElementDefaults(ElementDefaults), InsertionDisposition(AlreadyPresent), BuildResult(buildInputVertices, buildTriangulation), InsertionResult(insertionTriangulation,
  insertionDisposition))
import Moonlight.Planar.BuildStats (BuildMetric (InputPoints, LocationWalkSteps), buildStat)
import Support ( assertEqual, assertValid, requireRight, randomPoints, requireJust )
import System.Mem ( performGC )
import Moonlight.Planar.Internal.Representation qualified as Internal
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testMixedEditSession
    , testBulkRemovalAgreement
    , testMutablePointIndexWraparoundBackshift
    , testBatchIdentityIndexBatchToEmpty
    , testBatchIdentityIndexToSingletonActive
    , testBulkIdentityIndexDoesNotEscapeRemovalBatch
    , testHierarchyRemovalAgreement
    , testPersistentLocalUpdates
    , testCanonicalWitness
    , testRemoval
    ]

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
    (edgeKeys (buildTriangulation fresh))
    (edgeKeys edited)
  assertValid "mixed edit session" edited
  assertEqual
    "the whole transaction charged one counter set"
    (V.length arrivals)
    ((buildStat InputPoints) stats)

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
          (edgeKeys folded)
          (edgeKeys survived)
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
    (edgeKeys (buildTriangulation fresh))
    (edgeKeys transitioned)
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
    (edgeKeys (buildTriangulation fresh))
    (edgeKeys edited)
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
          (edgeKeys folded)
          (edgeKeys survived)
        assertEqual (label <> " repaired hierarchy equals reference rebuild") reference repaired
        assertValid (label <> " hinted removal") survived
  run "sparse" (V.take 40 original)
  run "dense" (V.take 120 original)

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

-- The canonical witness: absent from a built mesh, minted by canonical
-- publication, honoured by a second publication as an identity that
-- allocates nothing worth the name, and forgotten by the first edit.
testCanonicalWitness :: IO ()
testCanonicalWitness = do
  enabled <- getRTSStatsEnabled
  unless enabled $
    fail "canonical witness allocation receipt requires +RTS -T"
  base <- requirePointBuild "canonical witness base" (randomPoints 0xca70 4095)
  let triangulation = buildTriangulation base
  assertEqual
    "a built mesh carries no canonical witness"
    Internal.CanonicalUnknown
    (Internal.triCanonical triangulation)
  performGC
  beforeFirst <- allocated_bytes <$> getRTSStats
  canonical <- requireRight "canonical witness publication" (canonicalize triangulation)
  _ <- evaluate (force canonical)
  performGC
  afterFirst <- allocated_bytes <$> getRTSStats
  assertEqual
    "canonical publication mints the witness"
    Internal.CanonicalKnown
    (Internal.triCanonical canonical)
  performGC
  beforeSecond <- allocated_bytes <$> getRTSStats
  again <- requireRight "canonical witness fixed point" (canonicalize canonical)
  _ <- evaluate again
  performGC
  afterSecond <- allocated_bytes <$> getRTSStats
  let firstAllocated = afterFirst - beforeFirst
      knownAllocated = afterSecond - beforeSecond
  unless (again == canonical) $
    fail "canonical publication of a canonical value changed it"
  unless (firstAllocated >= 1_048_576) $
    fail ("canonical publication of a built mesh allocated only " <> show firstAllocated <> " bytes")
  unless (knownAllocated < 16_384) $
    fail ("canonical publication of a canonical value allocated " <> show knownAllocated <> " bytes")
  putStrLn
    ( "canonical witness receipt: unknown-allocated-bytes="
        <> show firstAllocated
        <> " known-allocated-bytes="
        <> show knownAllocated
    )
  inserted <- requireRight "canonical witness edit" (insert canonical (Point 0.000_123 (-0.000_271)))
  assertEqual
    "an edit forgets the witness"
    Internal.CanonicalUnknown
    (Internal.triCanonical (insertionTriangulation inserted))

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
      rimPoint
        :: Int
        -> Point
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
    ((buildStat LocationWalkSteps) (removalStats handleRemoval))
  locatedRemoval <-
    requireRight "point removal stats" (locateAndRemove triangulation (Point 1.5 1.5))
  pointRemoval <- requireJust "point removal located a vertex" locatedRemoval
  assertEqual
    "a point-keyed removal performs no topological location walk"
    0
    ((buildStat LocationWalkSteps) (removalStats pointRemoval))
  assertEqual
    "point-keyed and handle-keyed removal publish the same mesh"
    (edgeKeys (removalTriangulation handleRemoval))
    (edgeKeys (removalTriangulation pointRemoval))

-- | A refused singleton add witnesses the first resident constraint the
-- corridor meets walking from the source vertex, not the lowest-numbered
-- constraint the request crosses. Two vertical constraints cross the request
-- y = 0 between (0,0) and (6,0): one at x = 2 from (2,-2) to (2,2) and one at
-- x = 4 from (4,-3) to (4,3). Along the request from (0,0) the crossings sit
-- at parameters 1/3 and 2/3, so the walk from (0,0) blocks on the x = 2
-- constraint and the walk from (6,0) on the x = 4 constraint. A witness
-- chosen by edge number would name the same edge in both directions, so one
-- of the two directions refutes it whatever numbering the mesh assigned.

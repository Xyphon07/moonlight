{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Incremental location, frozen-location reuse, and hierarchy laws.
module Moonlight.Planar.LocationSpec
  ( tests
  ) where

import Control.Monad ( unless, forM_ )
import Data.Foldable ( traverse_ )
import Moonlight.Planar.BulkLoad ( delaunay, empty, insert )
import Moonlight.Planar.Dcel ( innerFaceDirectedEdges, numInnerFaces, numVertices, origin,
  vertexData, vertexPoint )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( vertices, innerFaces )
import Moonlight.Planar.HintGenerator ( buildHierarchyHint, hierarchyHint,
  updateHierarchyAfterInsertion )
import Moonlight.Planar.Internal.HandleDefs ( VertexId(VertexId), reverseEdge )
import Moonlight.Planar.Math ( midpoint )
import Moonlight.Planar.MeshFixtures ( requirePointBuild, edgeKeys )
import Moonlight.Planar.PayloadFixtures ( SampleVertex(..) )
import Moonlight.Planar.PointLocation ( locatePointWithHint, locatePoint )
import Moonlight.Planar.Session ( withSession, insertVertexAt, insertVertexAtNearVertex )
import Moonlight.Planar.Point (Point(Point))
import Moonlight.Planar.Types (unitElementDefaults, ConstraintMode(Unconstrained), InsertionDisposition(..), Location(..), LocationHint(VertexHint, FaceHint), LocationStats(LocationStats, locationUsedFallback,
  locationWalkSteps), BuildResult(buildTriangulation), DelaunayTriangulation, InsertionResult(insertionDisposition, insertionStats, insertionTriangulation, insertionVertex), Triangulation)
import Moonlight.Planar.BuildStats (BuildMetric (InputPoints, UniquePoints, ExistingPoints, DuplicatePoints, LocationWalkSteps, LocationFallbacks, LocationMaxWalk), buildStat)
import Support ( assertEqual, assertValid, requireQueryPoint, requireRight, randomPoints )
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testIncrementalLocationDescent
    , testPersistentInsertionReusesFrozenLocation
    , testVertexSeededSessionInsertion
    , testPointLocationAndHints
    , testHierarchyNestingLaw
    ]

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
        , incrementalWalkSteps = incrementalWalkSteps evidence + (buildStat LocationWalkSteps) stats
        , incrementalFallbacks = incrementalFallbacks evidence + (buildStat LocationFallbacks) stats
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
    (edgeKeys reference)
    (edgeKeys (insertionTriangulation result))
  assertEqual (label <> " source remains unchanged") sourceVertices (numVertices source)
  assertEqual (label <> " result vertex count") expectedVertices (numVertices (insertionTriangulation result))
  assertEqual (label <> " input count") 1 ((buildStat InputPoints) stats)
  assertEqual (label <> " unique count") expectedUnique ((buildStat UniquePoints) stats)
  assertEqual (label <> " existing count") expectedExisting ((buildStat ExistingPoints) stats)
  assertEqual (label <> " duplicate count") expectedExisting ((buildStat DuplicatePoints) stats)
  if usesMutableFallback
    then do
      let assertAtLeast
            :: String
            -> Int
            -> Int
            -> IO ()
          assertAtLeast counter expected actual =
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
      assertAtLeast "walk steps" (locationWalkSteps walked) ((buildStat LocationWalkSteps) stats)
      assertAtLeast "walk maximum" (locationWalkSteps walked) ((buildStat LocationMaxWalk) stats)
      assertAtLeast
        "fallback count"
        (if locationUsedFallback walked then 1 else 0)
        ((buildStat LocationFallbacks) stats)
    else do
      assertEqual (label <> " frozen walk steps") (locationWalkSteps walked) ((buildStat LocationWalkSteps) stats)
      assertEqual (label <> " frozen walk maximum") (locationWalkSteps walked) ((buildStat LocationMaxWalk) stats)
      assertEqual
        (label <> " frozen fallback count")
        (if locationUsedFallback walked then 1 else 0)
        ((buildStat LocationFallbacks) stats)
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
  assertEqual (label <> " topology") (edgeKeys reference) (edgeKeys hinted)
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
testPointLocationAndHints :: IO ()
testPointLocationAndHints = do
  testTriangleLocationPriority
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

-- The immutable triangle already owns edge cyclicity. Pin early boundary hits
-- and the LAST crossing when a corner query lies beyond two incident edges.
testTriangleLocationPriority :: IO ()
testTriangleLocationPriority = do
  built <- requirePointBuild "location priority triangle" [Point 0 0, Point 4 0, Point 0 4]
  let mesh = buildTriangulation built
  traverse_
    (\face -> case innerFaceDirectedEdges mesh face of
      Nothing -> fail "location priority fixture lost its triangular face"
      Just (e0, e1, e2) -> do
        let v0 = origin mesh e0
            v1 = origin mesh e1
            v2 = origin mesh e2
            p0 = vertexPoint mesh v0
            p1 = vertexPoint mesh v1
            p2 = vertexPoint mesh v2
            hint = Just (FaceHint face)
            check label expected point = do
              query <- requireQueryPoint label point
              assertEqual label (expected, LocationStats 1 False) (locatePointWithHint mesh hint query)
            beyondCorner (Point ax ay) (Point bx by) (Point cx cy) = Point (3 * ax - bx - cx) (3 * ay - by - cy)
        traverse_ (\(vertex, point) -> check "triangle vertex priority" (OnVertex vertex) point) [(v0, p0), (v1, p1), (v2, p2)]
        traverse_ (\(edge, edgeFrom, edgeTo) -> check "triangle edge priority" (OnEdge edge) (midpoint edgeFrom edgeTo)) [(e0, p0, p1), (e1, p1, p2), (e2, p2, p0)]
        traverse_
          (\(lastCrossing, cornerPoint, nextPoint, previousPoint) -> check "triangle last crossing" (OutsideConvexHull (Just (reverseEdge lastCrossing))) (beyondCorner cornerPoint nextPoint previousPoint))
          [(e2, p0, p1, p2), (e1, p1, p2, p0), (e2, p2, p0, p1)])
    (innerFaces mesh)

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

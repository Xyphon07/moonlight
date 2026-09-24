{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Read-side traversal over a finished mesh: ordered line intersection and the
-- circle shape query. Construction here is fixture cost, not the subject.
module Moonlight.Planar.DcelBench (benchmarks) where

import BenchSupport
  ( latticeFaceBand
  , latticePoints
  , randomPoints
  )
import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import Data.Word (Word64)
import qualified Data.List as List
import qualified Data.Vector as V
import Moonlight.Planar.Alpha (alphaShapeContainsFace)
import Moonlight.Planar.BulkLoad (delaunay)
import Moonlight.Planar.Dcel (incidentFace, numFaces, numVertices, origin, destination, vertexPoint)
import Moonlight.Planar.HintGenerator (buildHierarchyHint, hierarchyHint)
import Moonlight.Planar.Internal.HandleDefs (FaceId (..), VertexId (..), normalizedDirected, unDirectedEdgeId, unFaceId, unVertexId)
import Moonlight.Planar.PointLocation (locatePointWithHint)
import Moonlight.Planar.FloodFillIterator (BoundaryObstruction (..), FaceComponent, RegionBoundary, boundaryLoopVertices, componentBoundary, edgesInCircle, faceComponentFaces, faceComponents, regionBoundaryHoleLoops, regionBoundaryOuterLoop)
import Moonlight.Planar.Handles.Iterators.FixedIterators (innerFaces, undirectedEdges, vertices)
import Moonlight.Planar.IntersectionIterator (lineIntersections)
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.Types (DelaunayTriangulation, Location (..), LocationHint (..), LocationStats (..), buildTriangulation, unitElementDefaults)
import Moonlight.Planar.Point (Point (..), QueryPoint)
import Moonlight.Planar.Scalar (mkRadiusSquared)

benchmarks :: IO ()
benchmarks = do
  benchmarkPointLocation 20_000 10_000
  benchmarkQueries 20_000 10_000
  benchmarkRegionWorkload 440 272 22

benchmarkQueries :: Int -> Int -> IO ()
benchmarkQueries pointCount queryCount = do
  built <- requireRight (delaunay unitElementDefaults (V.fromList (randomPoints 0xbf58476d1ce4e5b9 pointCount)))
  circleEdges <- requireRight (edgesInCircle (buildTriangulation built) (Point 0 0) 0.25)
  queries <-
    requireRight
      (traverse mkQueryPoint (V.fromList (take (2 * queryCount) (randomPoints 0x632be59bd9b4e019 (2 * queryCount)))))
  let triangulation = buildTriangulation built
      total = V.ifoldl' (lineCount triangulation queries queryCount) 0 (V.take queryCount queries)
      shapeTotal = length circleEdges
  _ <- timedValue "line-and-shape-queries" (pure (total, shapeTotal))
  pure ()
 where
  lineCount
    :: DelaunayTriangulation (Point)
    -> V.Vector (QueryPoint)
    -> Int
    -> Int
    -> Int
    -> QueryPoint
    -> Int
  lineCount triangulation queries stride !accumulator index from =
    accumulator + length (lineIntersections triangulation from (queries V.! (index + stride)))

-- | The workload that motivated region extraction: 239,360 bounded faces.
-- Construction is shared fixture cost and is forced before either timed lane.
benchmarkRegionWorkload :: Int -> Int -> Int -> IO ()
benchmarkRegionWorkload widthInCells heightInCells expectedBandCount = do
  built <-
    requireRight
      (delaunay unitElementDefaults (latticePoints widthInCells heightInCells))
  triangulation <- evaluate (force (buildTriangulation built))
  benchmarkRegionBoundaries triangulation expectedBandCount
  benchmarkAlphaFaceMembership triangulation

benchmarkRegionBoundaries :: DelaunayTriangulation Point -> Int -> IO ()
benchmarkRegionBoundaries triangulation expectedBandCount = do
  analysed <-
    timedValue
      "face-components-and-boundaries"
      (evaluate (regionAnalysis triangulation))
  (components, boundaries) <- requireRight analysed
  let faceCount = sum (fmap (length . faceComponentFaces . snd) components)
      outerLoopCount = length boundaries
      holeLoopCount =
        sum (fmap (length . regionBoundaryHoleLoops) boundaries)
      boundaryVertexCount =
        sum
          ( fmap
              (length . boundaryLoopVertices . regionBoundaryOuterLoop)
              boundaries
          )
  if
    ( faceCount
    , length components
    , outerLoopCount
    , holeLoopCount
    , boundaryVertexCount
    )
      == (239_360, expectedBandCount, expectedBandCount, 0, 88)
    then
      putStrLn
        "face-components-and-boundaries-receipt: faces=239360 components=22 outer-loops=22 hole-loops=0 boundary-vertices=88"
    else
      fail
        ( "region benchmark receipt mismatch: "
            <> show
              ( faceCount
              , length components
              , outerLoopCount
              , holeLoopCount
              , boundaryVertexCount
              )
        )
 where
  regionAnalysis
    :: DelaunayTriangulation Point
    -> Either
        BoundaryObstruction
        ([(Int, FaceComponent)], [RegionBoundary])
  regionAnalysis mesh = do
    let components = faceComponents mesh (latticeFaceBand mesh)
    boundaries <-
      traverse
        (componentBoundary mesh . snd)
        components
    pure (components, boundaries)

benchmarkAlphaFaceMembership :: DelaunayTriangulation Point -> IO ()
benchmarkAlphaFaceMembership triangulation = do
  threshold <- requireRight (mkRadiusSquared 0.5)
  let containsFace = alphaShapeContainsFace threshold triangulation
  admittedCount <-
    timedValue
      "alpha-face-membership"
      ( evaluate
          ( List.foldl'
              (\count face -> if containsFace face then count + 1 else count)
              (0 :: Int)
              (innerFaces triangulation)
          )
      )
  if admittedCount == 239_360
    then putStrLn "alpha-face-membership-receipt: admitted=239360"
    else fail ("alpha face membership receipt mismatch: " <> show admittedCount)

-- | Force meshes and admitted queries before timing only the existing location
-- owner. The digest retains constructor and resident identifier; step and
-- fallback totals pin the path as well as the geometric answer.
benchmarkPointLocation :: Int -> Int -> IO ()
benchmarkPointLocation pointCount queryCount = do
  built <- requireRight (delaunay unitElementDefaults (V.fromList (randomPoints 0x1234_5678 pointCount)))
  mesh <- evaluate (force (buildTriangulation built))
  queries <- requireRight (traverse mkQueryPoint (V.fromList (randomPoints 0xdead_beef queryCount))) >>= evaluate . force
  hierarchy <- requireRight (buildHierarchyHint 16 mesh) >>= evaluate . force
  let noHints :: V.Vector (Maybe LocationHint, QueryPoint)
      noHints = V.map ((,) Nothing) queries
      hierarchyHints = V.map (\query -> (hierarchyHint hierarchy query, query)) queries
      staleVertex = Just (VertexHint (VertexId (fromIntegral (numVertices mesh))))
      staleFace = Just (FaceHint (FaceId (fromIntegral (numFaces mesh))))
  traverse_
    (\(label, section) -> evaluate (force section) >>= reportPointLocation mesh label)
    [ ("point-location/random/no-hint", noHints)
    , ("point-location/random/hierarchy-hint", hierarchyHints)
    , ("point-location/random/stale-vertex", V.map ((,) staleVertex) queries)
    , ("point-location/random/stale-face", V.map ((,) staleFace) queries)
    ]
  lattice <- requireRight (delaunay unitElementDefaults (latticePoints 64 64))
  boundaryMesh <- evaluate (force (buildTriangulation lattice))
  vertexQueries <- requireRight
    (traverse (\vertex -> (,) (Just (VertexHint vertex)) <$> mkQueryPoint (vertexPoint boundaryMesh vertex)) (V.fromList (vertices boundaryMesh)))
  edgeQueries <- requireRight
    (traverse
      (\edge ->
        let forward = normalizedDirected edge
            Point ax ay = vertexPoint boundaryMesh (origin boundaryMesh forward)
            Point bx by = vertexPoint boundaryMesh (destination boundaryMesh forward)
         in (,) (Just (FaceHint (incidentFace boundaryMesh forward)))
              <$> mkQueryPoint (Point ((ax + bx) / 2) ((ay + by) / 2)))
      (V.fromList (undirectedEdges boundaryMesh)))
  traverse_
    (\(label, section) -> evaluate (force section) >>= reportPointLocation boundaryMesh label)
    [ ("point-location/boundary/vertices", vertexQueries)
    , ("point-location/boundary/edges", edgeQueries)
    ]

reportPointLocation
  :: DelaunayTriangulation Point
  -> String
  -> V.Vector (Maybe LocationHint, QueryPoint)
  -> IO ()
reportPointLocation mesh label queries = do
  (digest, steps, fallbacks) <- timedValue label (evaluate (V.foldl' observe (0, 0, 0) queries))
  putStrLn
    (label <> "-receipt: queries=" <> show (V.length queries)
      <> " semantic-digest=" <> show digest
      <> " walk-steps=" <> show steps
      <> " fallbacks=" <> show fallbacks)
 where
  observe :: (Word64, Int, Int) -> (Maybe LocationHint, QueryPoint) -> (Word64, Int, Int)
  observe (!digest, !steps, !fallbacks) (hint, query) =
    let (location, stats) = locatePointWithHint mesh hint query
        (!tag, !resident) = case location of
          EmptyTriangulation -> (0, 0)
          OnVertex vertex -> (1, fromIntegral (unVertexId vertex))
          OnEdge edge -> (2, fromIntegral (unDirectedEdgeId edge))
          InFace face -> (3, fromIntegral (unFaceId face))
          OutsideConvexHull Nothing -> (4, 0)
          OutsideConvexHull (Just edge) -> (5, fromIntegral (unDirectedEdgeId edge))
     in ( digest * 1099511628211 + tag * 4294967296 + resident
        , steps + locationWalkSteps stats
        , fallbacks + if locationUsedFallback stats then 1 else 0
        )

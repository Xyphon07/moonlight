{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Read-side traversal over a finished mesh: ordered line intersection and the
-- circle shape query. Construction here is fixture cost, not the subject.
module Moonlight.Triangulation.DcelBench (benchmarks) where

import BenchSupport
  ( latticeFaceBand
  , latticePoints
  , randomPoints
  )
import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Data.List as List
import qualified Data.Vector as V
import Moonlight.Triangulation
import Moonlight.Triangulation.FloodFillIterator (edgesInCircle)
import Moonlight.Triangulation.IntersectionIterator (lineIntersections)

benchmarks :: IO ()
benchmarks = do
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

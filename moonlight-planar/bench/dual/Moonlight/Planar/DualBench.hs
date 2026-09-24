{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

-- | The dual side: Delaunay-hierarchy hints against an unhinted walk, and
-- natural-neighbour interpolation over a reused workspace. Both report
-- allocation, because the claim in each case is about work avoided rather than
-- time taken.
module Moonlight.Planar.DualBench (benchmarks) where

import BenchMeasure (requireRight, timedValue)
import BenchSupport (randomPoints)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.ST (stToIO)
import qualified Data.Vector as V
import GHC.Exts (RealWorld)
import GHC.Stats (RTSStats (allocated_bytes), getRTSStats, getRTSStatsEnabled)
import Moonlight.Planar.BulkLoad (delaunay)
import Moonlight.Planar.Dcel (vertexPoint)
import Moonlight.Planar.Handles.HandleDefs (VertexId)
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.Types (ConstraintMode (..), DelaunayTriangulation, buildTriangulation, unitElementDefaults)
import Moonlight.Planar.Point (Point (..), QueryPoint)
import Moonlight.Planar.Telemetry
  ( LocationHint (VertexHint)
  , NearestStats (nearestWalkSteps)
  , nearestNeighbor
  )
import Moonlight.Planar.HintGenerator
  ( HierarchyHint
  , buildHierarchyHint
  , defaultHierarchyBranchFactor
  , hierarchyHint
  , hierarchyLevelCount
  , hierarchyVertexCount
  )
import Moonlight.Planar.Interpolation
  ( NaturalNeighborWorkspace
  , interpolateNaturalNeighbor
  , newNaturalNeighborWorkspace
  , workspaceBytes
  )
import System.Mem (performGC)

benchmarks :: IO ()
benchmarks = do
  benchmarkHierarchy 20_000 5_000
  benchmarkSibson 10_000 5_000

benchmarkHierarchy :: Int -> Int -> IO ()
benchmarkHierarchy pointCount queryCount = do
  built <- requireRight (delaunay unitElementDefaults (V.fromList (randomPoints 0x123456789abcdef pointCount)))
  queries <-
    requireRight
      (traverse mkQueryPoint (V.fromList (take queryCount (randomPoints 0x3141592653589793 queryCount))))
  let triangulation = buildTriangulation built
  hierarchy <- requireRight (buildHierarchyHint defaultHierarchyBranchFactor triangulation)
  (_, baselineSteps) <- timedValue "nearest/no-hierarchy" (evaluate (force (walkTotal triangulation Nothing queries)))
  (_, hierarchySteps) <- timedValue "nearest/delaunay-hierarchy" (evaluate (force (walkHierarchyTotal triangulation hierarchy queries)))
  putStrLn ("hierarchy-levels: " <> show (hierarchyLevelCount hierarchy))
  putStrLn ("hierarchy-vertices: " <> show (hierarchyVertexCount hierarchy))
  putStrLn ("nearest-walk-steps/no-hierarchy: " <> show baselineSteps)
  putStrLn ("nearest-walk-steps/hierarchy: " <> show hierarchySteps)
 where
  walkTotal
    :: DelaunayTriangulation (Point)
    -> Maybe VertexId
    -> V.Vector (QueryPoint)
    -> (Int, Int)
  walkTotal triangulation hint queries = V.foldl' step (0 :: Int, 0 :: Int) queries
   where
    step
      :: (Int, Int)
      -> QueryPoint
      -> (Int, Int)
    step (!count, !steps) query =
      case nearestNeighbor triangulation hint query of
        Nothing -> (count, steps)
        Just (_, stats) -> (count + 1, steps + nearestWalkSteps stats)

  walkHierarchyTotal
    :: DelaunayTriangulation (Point)
    -> HierarchyHint
    -> V.Vector (QueryPoint)
    -> (Int, Int)
  walkHierarchyTotal triangulation hierarchy queries = V.foldl' step (0 :: Int, 0 :: Int) queries
   where
    step
      :: (Int, Int)
      -> QueryPoint
      -> (Int, Int)
    step (!count, !steps) query =
      let hint = case hierarchyHint hierarchy query of
            Just (VertexHint vertex) -> Just vertex
            _ -> Nothing
       in case nearestNeighbor triangulation hint query of
            Nothing -> (count, steps)
            Just (_, stats) -> (count + 1, steps + nearestWalkSteps stats)

benchmarkSibson :: Int -> Int -> IO ()
benchmarkSibson pointCount queryCount = do
  built <- requireRight (delaunay unitElementDefaults (V.fromList (randomPoints 0x8cb92baa3f3d8dd7 pointCount)))
  queries <-
    requireRight
      (traverse mkQueryPoint (V.fromList (take queryCount (randomPoints 0xdb4f0b9175ae2165 queryCount))))
  let triangulation = buildTriangulation built
      height vertex =
        let Point x y = vertexPoint triangulation vertex
         in x * x + 0.5 * y
  workspace <- stToIO (newNaturalNeighborWorkspace triangulation)
  putStrLn ("sibson-workspace-bytes: " <> show (workspaceBytes workspace))
  statsEnabled <- getRTSStatsEnabled
  if statsEnabled then performGC else pure ()
  before <- if statsEnabled then Just <$> getRTSStats else pure Nothing
  _ <- timedValue "sibson/reused-workspace" (queryLoop workspace height queries)
  after <- if statsEnabled then Just <$> getRTSStats else pure Nothing
  case (before, after) of
    (Just left, Just right) -> do
      let allocated = allocated_bytes right - allocated_bytes left
          perQuery = fromIntegral allocated / fromIntegral queryCount :: Double
      putStrLn ("sibson-allocated-bytes-total: " <> show allocated)
      putStrLn ("sibson-allocated-bytes/query: " <> show perQuery)
    _ -> putStrLn "sibson allocation counters unavailable; run with +RTS -T"
 where
  queryLoop
    :: NaturalNeighborWorkspace RealWorld 'Unconstrained (Point) () () ()
    -> (VertexId -> Double)
    -> V.Vector (QueryPoint)
    -> IO Double
  queryLoop workspace height queries = go 0 0
   where
    !count = V.length queries
    go !index !total
      | index >= count = evaluate total
      | otherwise = do
          (value, _) <- stToIO (interpolateNaturalNeighbor height workspace Nothing (queries V.! index))
          go (index + 1) (total + maybe 0 id value)

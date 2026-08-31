{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

-- | The construction side: circle-sweep bulk load against the arrival-order
-- session kernel, persistent single insertion, constraint recovery and Ruppert
-- refinement. Each reports the library's own work counters alongside the time,
-- because the claim being measured is about work done rather than seconds.
module Moonlight.Triangulation.BuildBench (benchmarks) where

import BenchMeasure (requireRight, timedValue)
import BenchSupport (randomPoints)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.List (sort)
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import qualified Data.Vector as V
import Moonlight.Triangulation
import Moonlight.Triangulation.BulkLoad (empty, insert)
import Moonlight.Triangulation.Cdt (recoverConstraints)
import Moonlight.Triangulation.Foreign.Mesh (insertGeometryBatch)
-- The facade withholds this constructor. The benchmark indexes the builder's
-- own input mapping, so every handle it forges is one the builder issued, and
-- it owns that obligation explicitly by naming the module that grants it.
import Moonlight.Triangulation.Internal.HandleDefs (VertexId (VertexId))
import Moonlight.Triangulation.Internal.Session (insertVertex, insertVertexAt, withLocalSession, withSession)
import Moonlight.Triangulation.Types
  ( InsertionResult (insertionTriangulation)
  , refinementStats
  , statEdgeFlips
  , statLocationWalkSteps
  , statRefinementFaceChecks
  , statRefinementQueuePops
  )

benchmarks :: IO ()
benchmarks = do
  putStrLn "moonlight-triangulation native construction benchmark"
  forM_ [1_000, 10_000, 50_000] benchmarkConstruction
  forM_ [1_000, 10_000, 50_000, 100_000, 1_000_000] benchmarkSingletonInsertionCrossover
  forM_ [(50_000, 1_000), (50_000, 10_000), (50_000, 50_000)] benchmarkGeometryBatchInsertion
  benchmarkConstraints
  benchmarkRefinement 2_500

-- | What the geometry-only foreign batch entrance costs against the session
-- insertion it is built out of. The two arms differ by exactly one thing: the
-- ABI route validates every point into an intermediate boxed vector before it
-- opens the session, and the session arm receives the same points with that
-- pass already paid. Their difference is therefore the price of admission,
-- and it is the number that decides whether a fused geometry entrance is worth
-- designing at all. Inserting into an existing mesh is not the law of fresh
-- circle sweep, so nothing here transfers from the construction lanes above.
benchmarkGeometryBatchInsertion :: (Int, Int) -> IO ()
benchmarkGeometryBatchInsertion (baseCount, addedCount) = do
  let label suffix = "geometry-batch-" <> suffix <> "/" <> show baseCount <> "+" <> show addedCount
      basePoints = V.fromList (randomPoints 0x9e3779b97f4a7c15 baseCount)
      addedPoints = V.fromList (randomPoints 0xbf58476d1ce4e5b9 addedCount)
  base <- evaluate . force =<< requireRight (delaunayGeometry basePoints)
  _ <- evaluate (force addedPoints)
  admitted <- timedValue (label "abi") $ requireRight (insertGeometryBatch base addedPoints)
  (_, sessioned, _) <- timedValue (label "session") $
    requireRight
      ( withSession base (V.length addedPoints) $
          V.mapM_ (\point -> () <$ insertVertexAt point ()) addedPoints
      )
  admittedCanonical <- requireRight (canonicalize admitted)
  sessionedCanonical <- requireRight (canonicalize sessioned)
  equal <- evaluate (force (admittedCanonical == sessionedCanonical))
  unless equal $
    fail (label "witness" <> ": the admitted and session arms disagree")
  putStrLn (label "witness" <> ": ok")

benchmarkConstruction :: Int -> IO ()
benchmarkConstruction count = do
  let points = V.fromList (randomPoints 0x9e3779b97f4a7c15 count)
  swept <- timedValue ("circle-sweep/" <> show count) $ requireRight (delaunay unitElementDefaults points)
  (_, sessioned, _) <- timedValue ("session/" <> show count) $
    requireRight
      ( withSession (empty unitElementDefaults) (V.length points) $
          V.mapM_ insertVertex points
      )
  evaluate (force (canonicalEdges (buildTriangulation swept) == canonicalEdges sessioned)) >>= \equal ->
    if equal then pure () else fail "circle-sweep and session construction disagree"

benchmarkSingletonInsertionCrossover :: Int -> IO ()
benchmarkSingletonInsertionCrossover count = do
  let points = V.fromList (randomPoints 0xd1b54a32d192ed03 count)
  built <- requireRight (delaunay unitElementDefaults points)
  let query = Point 0.000_123_456_7 (-0.000_765_432_1)
      base = buildTriangulation built
  scheduled <- timedValue ("singleton-scheduled-public-insert/" <> show count) $ requireRight (insert base query)
  (_, local, _) <- timedValue ("singleton-local-session-insert/" <> show count) $
    requireRight (withLocalSession base 1 (insertVertex query))
  let scheduledMesh = insertionTriangulation scheduled
  unless (null (validateTriangulation scheduledMesh)) $
    fail ("scheduled singleton insertion produced an invalid triangulation at " <> show count <> " sites")
  unless (null (validateTriangulation local)) $
    fail ("local singleton insertion produced an invalid triangulation at " <> show count <> " sites")
  scheduledCanonical <- requireRight (canonicalize scheduledMesh)
  localCanonical <- requireRight (canonicalize local)
  equal <- evaluate (force (scheduledCanonical == localCanonical))
  unless equal $
    fail ("scheduled and local singleton insertion disagree semantically at " <> show count <> " sites")
  putStrLn ("singleton-insertion-crossover/" <> show count <> "-semantic-witness: ok")

benchmarkConstraints :: IO ()
benchmarkConstraints = do
  let pointCount = 8_000
      constraintCount = 800
  built <- requireRight (delaunay unitElementDefaults (V.fromList (randomPoints 0x94d049bb133111eb pointCount)))
  let cdt = fromDelaunay (buildTriangulation built)
      inputMapping = buildInputVertices built
      requestIndices =
        V.fromList
          ( take constraintCount
              [ (a, b)
              | index <- [0 .. pointCount * constraintCount - 1]
              , let a = index `mod` pointCount
                    b = (index * 6151 + pointCount `quot` 2) `mod` pointCount
              , a /= b
              ]
          )
  pairs <-
    V.mapM
      (\(fromIndex, toIndex) ->
        let len = sizeofPrimArray inputMapping
            mFrom = if fromIndex >= 0 && fromIndex < len then Just (VertexId (indexPrimArray inputMapping fromIndex)) else Nothing
            mTo = if toIndex >= 0 && toIndex < len then Just (VertexId (indexPrimArray inputMapping toIndex)) else Nothing
        in case (mFrom, mTo) of
          (Just from, Just to) -> pure (from, to)
          _ -> fail "constraint benchmark endpoint is out of range"
      )
      requestIndices
  batch <- timedValue "cdt/recovery" (requireRight (recoverConstraints cdt pairs))
  putStrLn ("cdt/recovery-stats: " <> show (constraintBatchStats batch))

-- Ruppert refinement on a constrained square. The Steiner budget is the
-- variable of interest: both the encroachment search and the outer-region
-- classification are per-insertion costs, so their growth shows as a widening
-- gap between the two budgets rather than in either figure alone.
benchmarkRefinement :: Int -> IO ()
benchmarkRefinement steinerBudget = do
  cdtBuild <- requireRight $ constrainedDelaunay
    unitElementDefaults
    (V.fromList [Point 0 0, Point 64 0, Point 64 64, Point 0 64, Point 20 20, Point 44 44] :: V.Vector (Point))
    (V.fromList [(0, 1), (1, 2), (2, 3), (3, 0), (4, 5)])
  let cdt :: ConstrainedDelaunayTriangulation (Point)
      cdt = buildTriangulation cdtBuild
      parameters :: Int -> RefinementParameters
      parameters budget = defaultRefinementParameters
        { refineMaxAdditionalVertices = Just budget
        , refineMaxArea = Just 0.5
        , refineMaxRadiusEdgeRatio = Just 1.0
        , refineExcludeOuterFaces = True
        , refineKeepConstraintEdges = False
        }
  forM_ [steinerBudget `quot` 4, steinerBudget] $ \budget -> do
    refined <- timedValue ("refine/steiner-" <> show budget) (requireRight (refine id (parameters budget) cdt))
    let stats = refinementStats refined
    putStrLn ("refine-added/" <> show budget <> ": " <> show (refinementAddedVertices refined))
    putStrLn
      ( "refine-work/"
          <> show budget
          <> ": location-steps="
          <> show (statLocationWalkSteps stats)
          <> ", face-checks="
          <> show (statRefinementFaceChecks stats)
          <> ", queue-pops="
          <> show (statRefinementQueuePops stats)
          <> ", flips="
          <> show (statEdgeFlips stats)
      )

canonicalEdges :: Triangulation mode vertex directed undirected face -> [(Point, Point)]
canonicalEdges triangulation =
  sort
    [ ordered (vertexPoint triangulation (origin triangulation edge)) (vertexPoint triangulation (destination triangulation edge))
    | undirected <- undirectedEdges triangulation
    , let edge = normalizedDirected undirected
    ]
 where
  ordered :: Ord value => value -> value -> (value, value)
  ordered left right = if left <= right then (left, right) else (right, left)

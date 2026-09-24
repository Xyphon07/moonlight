{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

-- | The construction side: circle-sweep bulk load against the arrival-order
-- session kernel, persistent single insertion, constraint recovery and Ruppert
-- refinement. Each reports the library's own work counters alongside the time,
-- because the claim being measured is about work done rather than seconds.
module Moonlight.Planar.BuildBench (benchmarks) where

import BenchMeasure (requireRight, timedValue)
import BenchSupport (randomPoints)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, unless)
import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.List as List
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad (delaunay, delaunayGeometry, empty, insert)
import Moonlight.Planar.Canonical (canonicalize)
import Moonlight.Planar.Cdt (CdtError (..), ConstraintRecoveryResult (..), ConstraintResult, addConstraintEdge, canAddConstraint, constrainedDelaunay, constraintBatchStats, constraintBatchTriangulation, fromDelaunay, recoverConstraints)
import Moonlight.Planar.Dcel (destination, numUndirectedEdges, numVertices, origin, undirectedEndpoints, vertexPoint)
import Moonlight.Planar.Handles.HandleDefs (VertexId, normalizedDirected)
import Moonlight.Planar.Handles.Iterators.FixedIterators (undirectedEdges, vertices)
import Moonlight.Planar.Refinement (refine, withAdditionalVertexBudget, withConstraintPreservation, withMaximumArea, withMaximumRadiusEdgeRatio, withOuterFaceExclusion)
import Moonlight.Planar.Session (insertVertex, insertVertexAt, withSession)
import Moonlight.Planar.Types (BuildError (..), BuildResult, ConstrainedDelaunayTriangulation, ConstraintMode (..), DelaunayTriangulation, RefinementParameters, Triangulation, buildInputVertices, buildTriangulation, defaultRefinementParameters, insertionTriangulation, refinementAddedVertices, refinementStats, unitElementDefaults)
import Moonlight.Planar.BuildStats (BuildMetric (EdgeFlips, LocationWalkSteps, RefinementFaceChecks, RefinementQueuePops), buildStat)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Validation (validateTriangulation)
import Moonlight.Planar.Foreign.Mesh (insertGeometryBatch)
-- The public handle module hides this constructor. The benchmark indexes the builder's
-- own input mapping, so every handle it forges is one the builder issued, and
-- it owns that obligation explicitly by naming the module that grants it.
import Moonlight.Planar.Internal.HandleDefs (VertexId (VertexId))
import Moonlight.Planar.Internal.Session (withLocalSession)

benchmarks :: IO ()
benchmarks = do
  putStrLn "moonlight-planar native construction benchmark"
  forM_ [1_000, 10_000, 50_000] benchmarkConstruction
  forM_ [1_000, 10_000, 50_000, 100_000, 1_000_000] benchmarkSingletonInsertionCrossover
  traverse_
    benchmarkGeometryBatchInsertion
    [(0, 0), (0, 1_000), (50_000, 0), (50_000, 1), (50_000, 8), (50_000, 64), (50_000, 1_000), (50_000, 10_000), (50_000, 50_000)]
  benchmarkDuplicateGeometryBatches
  benchmarkConstraints
  benchmarkRefinement 2_500

-- | Compare the geometry batch entrance with its arrival-order session
-- control. Both include coordinate admission and publication. Small, empty,
-- and duplicate-heavy sections must not disappear behind a large fresh batch:
-- a resident hash or radial sort may cost more than the local work it saves.
benchmarkGeometryBatchInsertion :: (Int, Int) -> IO ()
benchmarkGeometryBatchInsertion (baseCount, addedCount) =
  benchmarkGeometryBatchFixture
    (show baseCount <> "+" <> show addedCount)
    (V.fromList (randomPoints 0x9e3779b97f4a7c15 baseCount))
    (V.fromList (randomPoints 0xbf58476d1ce4e5b9 addedCount))

benchmarkDuplicateGeometryBatches :: IO ()
benchmarkDuplicateGeometryBatches =
  traverse_
    (\(label, points) -> benchmarkGeometryBatchFixture label basePoints points)
    [ ("resident-duplicates/50000+1000", V.take 1_000 basePoints)
    , ("mixed-duplicates/50000+1000", V.take 900 basePoints <> V.replicate 100 freshPoint)
    , ("fresh-duplicates/50000+10000", V.replicate 10_000 freshPoint)
    ]
 where
  basePoints :: V.Vector Point
  basePoints = V.fromList (randomPoints 0x9e3779b97f4a7c15 50_000)
  freshPoint :: Point
  freshPoint = Point 0.3141592653589793 0.2718281828459045

benchmarkGeometryBatchFixture :: String -> V.Vector Point -> V.Vector Point -> IO ()
benchmarkGeometryBatchFixture fixtureLabel basePoints addedPoints = do
  let label suffix = "geometry-batch-" <> suffix <> "/" <> fixtureLabel
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
  unless
    ( numVertices admitted >= numVertices base
        && numVertices sessioned >= numVertices base
        && all
          (\vertex -> vertexPoint admitted vertex == vertexPoint base vertex && vertexPoint sessioned vertex == vertexPoint base vertex)
          (vertices base)
    )
    (fail (label "witness" <> ": a resident vertex handle changed identity"))
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
  benchmarkConstraintFixture "cdt/recovery" 8_000 (randomChords 800)
  benchmarkConstraintFixture "cdt/recovery/20000+4000" 20_000 (randomChords 4_000)
  benchmarkConstraintFixture "cdt/recovery/edges-8000+2000" 8_000 (delaunayEdges 2_000)
  benchmarkConstraintFixture "cdt/recovery/edges-20000+6000" 20_000 (delaunayEdges 6_000)

-- | Chords between pseudo-random input positions: long, so most cross an
-- earlier constraint and the resident constraint set stays sparse.
randomChords :: Int -> DelaunayTriangulation Point -> BuildResult 'Unconstrained Point () () () -> IO (V.Vector (VertexId, VertexId))
randomChords constraintCount _ built =
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
 where
  inputMapping = buildInputVertices built
  pointCount = sizeofPrimArray inputMapping
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

-- | Every k-th Delaunay edge: each is accepted without a corridor, so the
-- resident constraint set is as dense as the request count.
delaunayEdges :: Int -> DelaunayTriangulation Point -> BuildResult 'Unconstrained Point () () () -> IO (V.Vector (VertexId, VertexId))
delaunayEdges constraintCount mesh _ =
  pure
    ( V.fromList
        ( take constraintCount
            [ undirectedEndpoints mesh edge
            | (rank, edge) <- zip [0 :: Int ..] (undirectedEdges mesh)
            , rank `mod` stride == 0
            ]
        )
    )
 where
  stride = max 1 (numUndirectedEdges mesh `quot` constraintCount)

-- | Batch recovery on a fixture, then singleton admission against the
-- recovered mesh: the same number of accepted and rejected requests, so the
-- admission prescan and the corridor walk are measured on both outcomes.
benchmarkConstraintFixture
  :: String
  -> Int
  -> (DelaunayTriangulation Point -> BuildResult 'Unconstrained Point () () () -> IO (V.Vector (VertexId, VertexId)))
  -> IO ()
benchmarkConstraintFixture label pointCount requestsOf = do
  built <- requireRight (delaunay unitElementDefaults (V.fromList (randomPoints 0x94d049bb133111eb pointCount)))
  let mesh = buildTriangulation built
      cdt = fromDelaunay mesh
  pairs <- requestsOf mesh built
  batch <- timedValue label (requireRight (recoverConstraints cdt pairs))
  putStrLn (label <> "-stats: " <> show (constraintBatchStats batch))
  let constrained = constraintBatchTriangulation batch
      resident = numVertices constrained
      singletonCount = 200
      candidates =
        [ (VertexId (fromIntegral a), VertexId (fromIntegral b))
        | index <- [0 .. 40 * singletonCount * 50 - 1 :: Int]
        , let a = (index * 7919 + 17) `mod` resident
              b = (index * 104_729 + resident `quot` 3) `mod` resident
        , a /= b
        ]
      (admitted, blocked) = List.partition (uncurry (canAddConstraint constrained)) candidates
      acceptedPairs = take singletonCount admitted
      rejectedPairs = take singletonCount blocked
      pointOf = vertexPoint constrained
      publishedEdges :: Either CdtError (ConstraintResult Point () () ()) -> Int
      publishedEdges = either (const 0) (numUndirectedEdges . constraintRecoveryTriangulation)
      refusals :: Either CdtError (ConstraintResult Point () () ()) -> Int
      refusals = either (const 1) (const 0)
  unless (length acceptedPairs == singletonCount && length rejectedPairs == singletonCount) $
    fail (label <> " could not draw enough admitted and blocked singleton requests")
  _ <- evaluate (force (fmap (\(a, b) -> (pointOf a, pointOf b)) (acceptedPairs <> rejectedPairs)))
  acceptedEdges <-
    timedValue (label <> "/singleton-add-accepted") $
      traverse
        (\(a, b) -> evaluate (publishedEdges (addConstraintEdge constrained (pointOf a) (pointOf b))))
        acceptedPairs
  rejectedRefusals <-
    timedValue (label <> "/singleton-add-rejected") $
      traverse
        (\(a, b) -> evaluate (refusals (addConstraintEdge constrained (pointOf a) (pointOf b))))
        rejectedPairs
  putStrLn
    ( label
        <> "/singleton-outcomes: accepted-published="
        <> show (length (filter (> 0) acceptedEdges))
        <> "/"
        <> show singletonCount
        <> " rejected-refused="
        <> show (sum rejectedRefusals)
        <> "/"
        <> show singletonCount
    )

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
      parameters :: Int -> Either BuildError RefinementParameters
      parameters budget =
        fmap
          (withOuterFaceExclusion True . withConstraintPreservation False)
          ( withAdditionalVertexBudget budget defaultRefinementParameters
              >>= withMaximumArea 0.5
              >>= withMaximumRadiusEdgeRatio 1.0
          )
  forM_ [steinerBudget `quot` 4, steinerBudget] $ \budget -> do
    budgeted <- requireRight (parameters budget)
    refined <- timedValue ("refine/steiner-" <> show budget) (requireRight (refine id budgeted cdt))
    let stats = refinementStats refined
    putStrLn ("refine-added/" <> show budget <> ": " <> show (refinementAddedVertices refined))
    putStrLn
      ( "refine-work/"
          <> show budget
          <> ": location-steps="
          <> show ((buildStat LocationWalkSteps) stats)
          <> ", face-checks="
          <> show ((buildStat RefinementFaceChecks) stats)
          <> ", queue-pops="
          <> show ((buildStat RefinementQueuePops) stats)
          <> ", flips="
          <> show ((buildStat EdgeFlips) stats)
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

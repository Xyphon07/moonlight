-- | Haskell half of the spade external referent.
--
-- @moonlight-planar@ is a port of the Rust @spade@ crate, so spade is the
-- referent that grades it. Both halves generate their inputs from the same LCG
-- with the same seeds and emit their gate output in the same encoding, so
-- neither side can reformat a disagreement into agreement.
--
-- Vertex handles are NOT comparable across the two implementations — both bulk
-- loaders reorder. Anything naming a vertex names it by input index or by
-- coordinate, never by handle.
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.ST (runST)
import Data.Word (Word64)
import Moonlight.Planar.Internal.Mutable (freezeTriangulation, thawTriangulation)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector as V
import Moonlight.Planar.BulkLoad
import Moonlight.Planar.Cdt
import Moonlight.Planar.Dcel (numVertices)
import Moonlight.Planar.Handles.HandleDefs (VertexId)
import Moonlight.Planar.PointLocation
import Moonlight.Planar.Refinement (refine)
import Moonlight.Planar.Types
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Telemetry (LocationHint (..), insertMany, nearestNeighbor)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.HintGenerator
import Moonlight.Planar.Interpolation
import Moonlight.Planar.Bench.SpadeCompare.Constraint
import Moonlight.Planar.Bench.SpadeCompare.Dcel
import Moonlight.Planar.Bench.SpadeCompare.Delaunay
import Moonlight.Planar.Bench.SpadeCompare.Gates
import Moonlight.Planar.Bench.SpadeCompare.Hierarchy
import Moonlight.Planar.Bench.SpadeCompare.Interpolation
import Moonlight.Planar.Bench.SpadeCompare.Intersection
import Moonlight.Planar.Bench.SpadeCompare.Input (randomPoints)
import Moonlight.Planar.Bench.SpadeCompare.Lane
import Moonlight.Planar.Bench.SpadeCompare.Removal
import Moonlight.Planar.Bench.SpadeCompare.Support
import Moonlight.Planar.Bench.SpadeCompare.Timing (measureLane)
import Moonlight.Planar.Bench.SpadeCompare.Voronoi

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["gate", directory] -> writeGate directory
    ["divergence", path] -> writeDivergence path
    ["inventory-csv"] -> putStr renderInventoryCsv
    ["inventory-human"] -> putStr renderInventoryHuman
    ["lane-specs", laneClass] -> either refuseLane putStr (renderLaneSpecs laneClass)
    ["snapshot-specs"] -> putStr renderSnapshotSpecs
    ["bench-one", lane, first, second] ->
      either refuseLane runLane (parseLaneRequest lane first second)
    ["counters", budget] -> reportRefinementCounters (read budget)
    ["sweep-stats", count] -> reportSweepStats (read count)
    ["removal-audit", count, removals] -> reportRemovalAudit (read count) (read removals)
    ["removal-context", count, index] -> reportRemovalContext (read count) (read index)
    ["fresh-suffix", count, dropped, path] -> writeFreshSuffix (read count) (read dropped) path
    ["interp-audit", count, queries] -> reportInterpolationAudit (read count) (read queries)
    ["interp-context", count] -> reportInterpolationContext (read count) Nothing
    ["interp-context", count, index] -> reportInterpolationContext (read count) (Just (read index))
    ["interp-locate-audit", count, queries] -> reportInterpolationLocateAudit (read count) (read queries)
    ["interp-cavity-audit", count, queries] -> reportInterpolationCavityAudit (read count) (read queries)
    _ -> do
      hPutStrLn stderr "usage: moonlight-planar-spade-referent (gate DIR | divergence FILE | inventory-csv | inventory-human | lane-specs CLASS | snapshot-specs | bench-one LANE A B | counters BUDGET | sweep-stats COUNT | removal-audit COUNT REMOVALS | removal-context COUNT INDEX | fresh-suffix COUNT DROPPED PATH | interp-audit COUNT QUERIES | interp-context COUNT [INDEX])"
      exitWith (ExitFailure 2)

refuseLane :: LaneObstruction -> IO value
refuseLane obstruction = do
  hPutStrLn stderr (renderLaneObstruction obstruction)
  exitWith (ExitFailure 2)

-- Everything each lane needs before the clock starts. Handle resolution and
-- input generation are setup, not work.
runLane :: LaneRequest -> IO ()
runLane request@(LaneRequest lane first second) = case lane of
  -- The input vector is forced here rather than inside 'measure'. A lazy
  -- 'V.fromList' handed to the clock puts the generator, the list, every boxed
  -- coordinate, and the vector's own construction inside the lane — none of
  -- which the Rust side pays, because it builds its @Vec@ before its @Instant@.
  BulkLoadLane -> do
    points <- evaluate (force (V.fromList (randomPoints 0x9e3779b97f4a7c15 first)))
    measure (require (delaunayGeometry points))
  IncrementalLane -> do
    points <- evaluate (force (V.fromList (randomPoints 0x9e3779b97f4a7c15 first)))
    measure (insertAllIncrementally points)
  -- One immutable snapshot per point. This is deliberately not a cross-
  -- language lane: spade's referent is timed only on the mutable session
  -- operation, while this stress lane is paired with the Haskell session
  -- reference by the driver.
  SnapshotInsertLane -> do
    points <- evaluate (force (V.fromList (randomPoints 0x9e3779b97f4a7c15 first)))
    measure (insertAllPersistently points)
  -- The angular index both bulk loaders open with, given one angle to work
  -- with. The input stays in general position, so the load runs the ordinary
  -- face-building path from the third point on and every insertion locates by
  -- walking; what degrades is the hull lookup that walk starts from, whose
  -- buckets all collapse into one. This lane is that degradation and nothing
  -- else — the face-less path it does NOT reach is "degenerate-line" below.
  SweepAngleCollapseLane -> do
    points <- evaluate (force (V.fromList (nearCollinearPoints 0x9e3779b97f4a7c15 first)))
    measure (buildTriangulation <$> require (delaunay unitElementDefaults points))
  -- Exactly collinear input never builds a face. Moonlight recognizes that
  -- section after radial admission and glues its complete DCEL chain once;
  -- Spade still locates against the growing face-less mesh per insertion.
  DegenerateLineLane -> do
    points <- evaluate (force (V.fromList (exactlyCollinearPoints first)))
    measure (buildTriangulation <$> require (delaunay unitElementDefaults points))
  BatchSweepLane -> do
    points <- evaluate (force (V.fromList (randomPoints 0x9e3779b97f4a7c15 first)))
    measure (buildTriangulation <$> require (insertMany (empty unitElementDefaults) points))
  NearestLane -> do
    triangulation <- buildTriangulation <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x0123456789abcdef first)))
    hierarchy <- require (buildHierarchyHint defaultHierarchyBranchFactor triangulation)
    queries <- requireQueryPoints (V.fromList (randomPoints 0x3141592653589793 second)) >>= evaluate . force
    measure (evaluate (V.sum (V.map (resolve triangulation hierarchy) queries)))
  CdtRecoveryLane -> do
    let points = V.fromList (randomPoints 0x94d049bb133111eb first)
    base <- fromDelaunay . buildTriangulation <$> require (delaunay unitElementDefaults points)
    queryPoints <- requireQueryPoints points
    handles <- evaluate (force (V.map (locateVertex base) queryPoints))
    pairs <-
      evaluate
        ( force
            (V.fromList [(handles V.! a, handles V.! b) | (a, b) <- constraintPairs first second])
        )
    measure (evaluate (recover base pairs))
  -- The same constraint program, requested one edge at a time through the
  -- singleton entry point instead of as a batch. The endpoints are handed over
  -- as points rather than handles because that is what this entry point takes;
  -- the strict side's counterpart takes points too, so both pay the same two
  -- locates per request.
  ConstraintIncrementalLane -> do
    let points = V.fromList (randomPoints 0x94d049bb133111eb first)
    base <- fromDelaunay . buildTriangulation <$> require (delaunay unitElementDefaults points)
    requests <-
      evaluate
        ( force
            [ (points V.! fromIndex, points V.! toIndex)
            | (fromIndex, toIndex) <- constraintPairs first second
            ]
        )
    measure (evaluate (length (snd (addConstraintsIncrementally base requests))))
  ConstraintSplitLane -> do
    (base, handles) <- splitBandCdt first
    resolved <- evaluate (force handles)
    measure (evaluate (numVertices (splitConstraints base resolved)))
  PublicationFloorLane -> do
    triangulation <- delaunayOf first
    measure $ do
      reopened <- require
        ( V.foldl'
            reopenTriangulation
            (Right triangulation)
            (V.enumFromN (0 :: Int) second)
        )
      evaluate (numVertices reopened)
  -- Removal by coordinate, against the same input prefix on both sides. The
  -- build is setup on both sides; the measured region is the removals alone.
  RemovalLane -> do
    let input = randomPoints 0x9e3779b97f4a7c15 first
    points <- evaluate (force (V.fromList input))
    base <- buildTriangulation <$> require (delaunay unitElementDefaults points)
    removals <- evaluate (force (V.fromList (take second input)))
    measure (removeAllByCoordinate base removals)
  -- One immutable snapshot per removal. Like snapshot insertion, this is a
  -- Haskell-only publication stress lane paired with the session reference;
  -- it is not a spade timing ratio.
  SnapshotRemovalLane -> do
    let input = randomPoints 0x9e3779b97f4a7c15 first
    points <- evaluate (force (V.fromList input))
    base <- buildTriangulation <$> require (delaunay unitElementDefaults points)
    removals <- evaluate (force (V.fromList (take second input)))
    measure (removeAllPersistently base removals)
  -- The hierarchy lanes. spade's hint generator maintains itself from inside
  -- the triangulation, so an insert or a removal there is the whole maintenance
  -- cost; here the hierarchy is a separate value and the maintenance is a
  -- separate call, and both are inside the clock.
  HierarchyIncrementalLane -> do
    points <- evaluate (force (V.fromList (randomPoints 0x9e3779b97f4a7c15 first)))
    measure (insertAllWithHierarchy points)
  HierarchyDuplicateLane -> do
    base <- hierarchyOf first
    duplicates <- evaluate (force (V.fromList (randomPoints 0x9e3779b97f4a7c15 second)))
    measure (reinsertAllWithHierarchy base duplicates)
  HierarchyRemovalLane -> do
    let input = randomPoints 0x9e3779b97f4a7c15 first
    base <- hierarchyOf first
    removals <- evaluate (force (V.fromList (take second input)))
    measure (removeAllWithHierarchy base removals)
  HierarchyRemovalOnlyLane -> do
    let input = randomPoints 0x9e3779b97f4a7c15 first
    (triangulation, _) <- hierarchyOf first
    removals <- evaluate (force (V.fromList (take second input)))
    measure (removeAllByCoordinate triangulation removals)
  HierarchyRebuildOnlyLane -> do
    let input = randomPoints 0x9e3779b97f4a7c15 first
    (triangulation, hierarchy) <- hierarchyOf first
    removals <- evaluate (force (V.fromList (take second input)))
    surviving <- removeAllByCoordinate triangulation removals
    measure (require (rebuildHierarchyHint hierarchy surviving))
  -- Sibson weights through the reusable workspace; the strict side holds the
  -- same buffers behind its NaturalNeighbor handle. The sum forces every
  -- weight, the way the nearest lane's index sum forces every search.
  InterpolationLane -> do
    triangulation <- buildTriangulation <$> require (delaunay unitElementDefaults (V.fromList (randomPoints 0x0123456789abcdef first)))
    workspace <- newNaturalNeighborWorkspace triangulation
    queries <- requireQueryPoints (V.fromList (randomPoints 0x2718281828459045 second)) >>= evaluate . force
    measure (interpolationWeightSum workspace queries)
  -- The dual. Both sides hold it as a view of the primal mesh rather than as a
  -- second structure, so the mesh is setup on both and the sweep alone is timed.
  VoronoiSweepLane -> do
    triangulation <- delaunayOf first
    measure (evaluate (voronoiSweep triangulation))
  DcelWalkLane -> do
    triangulation <- delaunayOf first
    measure (evaluate (dcelWalk triangulation))
  AlphaPersistenceLane ->
    refuseLane AlphaPersistenceRequiresSpecializedRunner
  -- Corridor walks between interior endpoints. Both sides locate the start on a
  -- face and step edge to edge from there.
  IntersectionLane -> do
    triangulation <- delaunayOf first
    chords <- requireQueryChords (intersectionChords second) >>= evaluate . force
    measure (evaluate (intersectionWalk triangulation chords))
  -- The same walk from outside the hull. spade steps around the hull from the
  -- edge its locate returned; this side has no such entry and scans every
  -- vertex and every edge to find where the line goes in, once per query.
  IntersectionOutsideLane -> do
    triangulation <- delaunayOf first
    chords <- requireQueryChords (outsideChords second) >>= evaluate . force
    measure (evaluate (intersectionWalk triangulation chords))
  RefineLane -> do
    let (points, constraints) = refinementInput
    cdt <- buildTriangulation <$> require (constrainedDelaunay unitElementDefaults points constraints)
    parameters <- require (refinementParameters first)
    measure (refinementAddedVertices <$> require (refine id parameters (cdt :: ConstrainedDelaunayTriangulation Point)))
 where
  reopenTriangulation state _ = do
    triangulation <- state
    runST $ do
      mutable <- thawTriangulation (numVertices triangulation) triangulation
      freezeTriangulation mutable

  measure :: NFData value => IO value -> IO ()
  measure = measureLane request

  -- The answer is summed rather than merely tested for presence. A 'Maybe'
  -- scrutinised only to its outer constructor can be a 'Just' wrapping a thunk,
  -- and the lane would then time the allocation of a search rather than the
  -- search; the strict side has no such option and must be matched.
  resolve triangulation hierarchy query =
    let hint = case hierarchyHint hierarchy query of
          Just (VertexHint vertex) -> Just vertex
          _ -> Nothing
     in case nearestNeighbor triangulation hint query of
          Nothing -> 0 :: Word64
          Just (VertexId vertex, _) -> fromIntegral vertex

  locateVertex cdt point = case locatePoint cdt point of
    OnVertex vertex -> vertex
    other -> error ("input point is not a vertex: " <> show other)

  recover triangulation pairs =
    case recoverConstraints triangulation pairs of
      Left failure -> error (show failure)
      Right batch ->
        V.length
          ( V.filter
              (\outcome ->
                case outcome of
                  ConstraintAccepted _ _ -> True
                  ConstraintRejected _ -> False
              )
              (constraintBatchOutcomes batch)
          )

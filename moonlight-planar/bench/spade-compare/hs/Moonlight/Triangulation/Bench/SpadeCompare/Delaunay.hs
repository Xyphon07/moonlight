{-# LANGUAGE DataKinds #-}

-- | Bulk-load, incremental, nearest, and refinement lanes and their gates.
module Moonlight.Triangulation.Bench.SpadeCompare.Delaunay where

import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified Data.Vector as V
import Moonlight.Triangulation
import Moonlight.Triangulation.BulkLoad
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.HintGenerator
import Moonlight.Triangulation.Session
import Moonlight.Triangulation.Types
import Moonlight.Triangulation.Bench.SpadeCompare.Support
import Moonlight.Triangulation.Bench.SpadeCompare.Input (randomPoints)

writeBulkLoadGate :: FilePath -> Int -> IO ()
writeBulkLoadGate directory count = do
  triangulation <-
    require
      ( delaunayGeometry
          (V.fromList (randomPoints 0x9e3779b97f4a7c15 count))
      )
  writeFile
    (directory </> ("bulk-load-" <> show count <> "-edges.txt"))
    (unlines (canonicalEdges triangulation))

writeIncrementalGate :: FilePath -> Int -> IO ()
writeIncrementalGate directory count = do
  triangulation <-
    insertAllIncrementally
      (V.fromList (randomPoints 0x9e3779b97f4a7c15 count))
  writeFile
    (directory </> ("incremental-" <> show count <> "-edges.txt"))
    (unlines (canonicalEdges triangulation))

writeNearestGate :: FilePath -> IO ()
writeNearestGate directory = do
  let pointCount = 20000
      queryCount = 5000
  triangulation <-
    buildTriangulation
      <$> require
        ( delaunay
            unitElementDefaults
            (V.fromList (randomPoints 0x0123456789abcdef pointCount))
        )
  hierarchy <- require (buildHierarchyHint defaultHierarchyBranchFactor triangulation)
  queries <- requireQueryPoints (randomPoints 0x3141592653589793 queryCount)
  writeFile
    (directory </> "nearest-20000-5000.txt")
    (unlines (map (nearestAnswer triangulation hierarchy) queries))

writeRefinementGate :: FilePath -> Int -> IO ()
writeRefinementGate directory steinerBudget = do
  let (points, constraints) = refinementInput
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just steinerBudget
          , refineMaxArea = Just 0.5
          , refineMaxRadiusEdgeRatio = Just 1.0
          , refineExcludeOuterFaces = True
          , refineKeepConstraintEdges = False
          }
  cdtBuild <- require (constrainedDelaunay unitElementDefaults points constraints)
  refined <-
    require
      ( refine
          id
          parameters
          (buildTriangulation cdtBuild :: ConstrainedDelaunayTriangulation Point)
      )
  let triangulation = refinedTriangulation refined
      prefix = directory </> ("refine-" <> show steinerBudget)
      completion = if refinementComplete refined then "1" else "0"
      summary =
        [ "added " <> show (refinementAddedVertices refined)
        , "vertices " <> show (numVertices triangulation)
        , "edges " <> show (numUndirectedEdges triangulation)
        , "inner-faces " <> show (numInnerFaces triangulation)
        , "excluded " <> show (V.length (refinementExcludedFaces refined))
        , "complete " <> completion
        ]
  writeFile (prefix <> "-summary.txt") (unlines summary)
  writeFile (prefix <> "-edges.txt") (unlines (canonicalEdges triangulation))
  writeFile
    (prefix <> "-constraints.txt")
    (unlines (canonicalConstraintEdges triangulation))

-- | Arrival-order insertion through immutable snapshots. The artifact proves
-- final-state agreement with Spade; it is not a cross-language timing row.
writePersistentGate :: FilePath -> Int -> IO ()
writePersistentGate directory count = do
  triangulation <-
    insertAllPersistently (V.fromList (randomPoints 0x9e3779b97f4a7c15 count))
  writeFile
    (directory </> ("persistent-" <> show count <> "-edges.txt"))
    (unlines (canonicalEdges triangulation))

writeSweepAngleGate :: FilePath -> Int -> IO ()
writeSweepAngleGate directory count = do
  triangulation <-
    buildTriangulation
      <$> require
        ( delaunay
            unitElementDefaults
            (V.fromList (nearCollinearPoints 0x9e3779b97f4a7c15 count))
        )
  writeFile
    (directory </> ("sweep-angle-collapse-" <> show count <> "-edges.txt"))
    (unlines (canonicalEdges triangulation))

-- | The degenerate chain, pinned by the only thing it has: its edges.
--
-- There are no faces to compare and no circumcentres to argue about, so unlike
-- every other gate here this one has no per-side half and no exclusion. The
-- answer is @n - 1@ consecutive segments and the two sides either both produce
-- it or one of them is wrong.
writeDegenerateLineGate :: FilePath -> Int -> IO ()
writeDegenerateLineGate directory count = do
  triangulation <-
    buildTriangulation
      <$> require
        (delaunay unitElementDefaults (V.fromList (exactlyCollinearPoints count)))
  writeFile
    (directory </> ("degenerate-line-" <> show count <> "-edges.txt"))
    (unlines (canonicalEdges triangulation))

-- Arrival-order insertion with one publication: the transactional comparison
-- against Spade's @&mut self@ insertion. The per-operation snapshot fold is
-- measured separately as Moonlight publication stress.
insertAllIncrementally
  :: V.Vector Point
  -> IO (DelaunayTriangulation Point)
insertAllIncrementally points =
  evaluate
    ( case either (error . show) id
        ( withSession
            (empty unitElementDefaults)
            (V.length points)
            (V.mapM_ insertVertex points)
        ) of
        (_, frozen, _) -> frozen
    )

insertAllPersistently
  :: V.Vector Point
  -> IO (DelaunayTriangulation Point)
insertAllPersistently =
  V.foldM'
    (\triangulation point ->
      insertionTriangulation <$> require (insert triangulation point)
    )
    (empty unitElementDefaults)

-- | The refinement lane's own counters, so the worklist question is answered by
-- measurement rather than by assertion.
--
-- @faceChecks@ is not a worklist counter: the terminal audit runs the complete
-- face evaluation over every final inner face without popping anything. So the
-- amplification ratio is @pops / steiner@, and @faceChecks - pops@ should equal
-- the final inner-face count less any pop discarded as excluded.
-- | The circle sweep's placement split, on the same generator the bulk lanes
-- use. A point takes the fast path when the hull edge its own angle selected
-- has the point strictly to its left; otherwise it is deferred to a generic
-- insertion after the sweep finishes. spade takes the identical branch on
-- @is_on_right_side_or_on_line@ and its own source calls that fallback "very
-- slow", so a materially non-zero skip count is wasted work in the bulk lane
-- rather than a curiosity. @seed + fast + skipped@ must exhaust the unique
-- points; this reports the identity rather than asserting it silently.
reportSweepStats :: Int -> IO ()
reportSweepStats count = do
  let points = V.fromList (randomPoints 0x9e3779b97f4a7c15 count)
  built <- require (delaunay unitElementDefaults points)
  let stats = buildStats (built :: BuildResult 'Unconstrained Point () () ())
      seeded = statSpatialSeedPoints stats
      fast = statSweepFastPoints stats
      skippedPoints = statSweepSkippedPoints stats
      unique = statUniquePoints stats
      accounted = seeded + fast + skippedPoints
      share :: Int -> String
      share value
        | unique == 0 = "n/a"
        | otherwise = show (100 * fromIntegral value / fromIntegral unique :: Double) <> "%"
  traverse_
    putStrLn
    [ "input-points " <> show (statInputPoints stats)
    , "unique-points " <> show unique
    , "duplicate-points " <> show (statDuplicatePoints stats)
    , "seed-points " <> show seeded
    , "sweep-fast-points " <> show fast
    , "sweep-skipped-points " <> show skippedPoints
    , "skipped-share " <> share skippedPoints
    , "accounted " <> show accounted
    , "identity-holds " <> show (accounted == unique)
    , "edge-flips " <> show (statEdgeFlips stats)
    , "hull-insertions " <> show (statHullInsertions stats)
    , "legalization-max-stack " <> show (statLegalizationMaxStack stats)
    ]
  if accounted == unique
    then pure ()
    else do
      hPutStrLn
        stderr
        ( "sweep-stats: seed + fast + skipped = "
            <> show accounted
            <> " but unique = "
            <> show unique
        )
      exitWith (ExitFailure 1)

reportRefinementCounters :: Int -> IO ()
reportRefinementCounters budget = do
  let (points, constraints) = refinementInput
      parameters =
        defaultRefinementParameters
          { refineMaxAdditionalVertices = Just budget
          , refineMaxArea = Just 0.5
          , refineMaxRadiusEdgeRatio = Just 1.0
          , refineExcludeOuterFaces = True
          , refineKeepConstraintEdges = False
          }
  cdt <- buildTriangulation <$> require (constrainedDelaunay unitElementDefaults points constraints)
  result <-
    require (refine id parameters (cdt :: ConstrainedDelaunayTriangulation Point))
  let stats = refinementStats result
      steiner = statSteinerPoints stats
      pops = statRefinementQueuePops stats
      checks = statRefinementFaceChecks stats
      inner = numInnerFaces (refinedTriangulation result)
      excluded = V.length (refinementExcludedFaces result)
      ratio :: Int -> Int -> String
      ratio numerator denominator
        | denominator == 0 = "n/a"
        | otherwise =
            show (fromIntegral numerator / fromIntegral denominator :: Double)
  traverse_
    putStrLn
    [ "budget " <> show budget
    , "steiner " <> show steiner
    , "queue-pops " <> show pops
    , "face-checks " <> show checks
    , "inner-faces " <> show inner
    , "excluded " <> show excluded
    , "edge-flips " <> show (statEdgeFlips stats)
    , "location-walk-steps " <> show (statLocationWalkSteps stats)
    , "complete " <> show (refinementComplete result)
    , "pops/steiner " <> ratio pops steiner
    , "checks/steiner " <> ratio checks steiner
    , "flips/steiner " <> ratio (statEdgeFlips stats) steiner
    , "walk-steps/steiner " <> ratio (statLocationWalkSteps stats) steiner
    , "checks-minus-pops " <> show (checks - pops)
    -- Every face check is now a queue pop. The terminal audit used to make the
    -- difference equal the final inner-face count; if this ever reads False
    -- again, a second whole-mesh quality pass has come back.
    , "no-second-pass " <> show (checks == pops)
    ]

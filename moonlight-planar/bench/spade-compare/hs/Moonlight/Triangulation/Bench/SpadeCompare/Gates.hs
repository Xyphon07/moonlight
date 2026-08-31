{-# LANGUAGE DataKinds #-}

-- | The artifact rosters: every gate the binary writes, and the divergence set.
module Moonlight.Triangulation.Bench.SpadeCompare.Gates where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Vector as V
import Moonlight.Triangulation
import Moonlight.Triangulation.BulkLoad
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Removal
import Moonlight.Triangulation.Types
import Moonlight.Triangulation.Bench.SpadeCompare.Constraint
import Moonlight.Triangulation.Bench.SpadeCompare.Dcel
import Moonlight.Triangulation.Bench.SpadeCompare.Delaunay
import Moonlight.Triangulation.Bench.SpadeCompare.Hierarchy
import Moonlight.Triangulation.Bench.SpadeCompare.Interpolation
import Moonlight.Triangulation.Bench.SpadeCompare.Intersection
import Moonlight.Triangulation.Bench.SpadeCompare.Removal
import Moonlight.Triangulation.Bench.SpadeCompare.Support
import Moonlight.Triangulation.Bench.SpadeCompare.Input (randomPoints)
import Moonlight.Triangulation.Bench.SpadeCompare.Voronoi

-- | The exact observable mesh egress retained by the persistent witnesses.
--
-- These artifacts validate immutable predecessor and final-state agreement;
-- they are not Spade timing ratios. Snapshot publication is measured only on
-- the Haskell side against its session reference.
--
-- The cross-language gate already compares the canonical-edge projection.
-- This witness additionally retains the geometry-labelled circular DCEL
-- section, so an isolated or face-less predecessor cannot change unseen.  It
-- deliberately observes geometric topology rather than internal storage, and
-- remains a gate-only sink outside every timed lane.
data PersistentEndpoint = PersistentEndpoint
  { persistentEndpointVertices :: !Int
  , persistentEndpointUndirectedEdges :: !Int
  , persistentEndpointInnerFaces :: !Int
  , persistentEndpointCanonicalEdges :: ![String]
  , persistentEndpointCircularDcel :: ![String]
  }
  deriving (Eq, Show)

-- | Every retained section is named by its operation and completed edit count,
-- never by a formatting-only string.
data PersistentWitnessLabel
  = PersistentInsertionWitness !Int
  | PersistentRemovalWitness !Int
  deriving (Eq, Show)

-- | A preserved immutable section together with its authoritative egress at
-- capture time.  Insert and removal each supply one concrete use, so this is
-- the shared gate algebra rather than a second triangulation representation.
data PersistentSnapshotWitness = PersistentSnapshotWitness
  { persistentSnapshotLabel :: !PersistentWitnessLabel
  , persistentSnapshotEndpoint :: !PersistentEndpoint
  , persistentSnapshotTriangulation :: !(DelaunayTriangulation Point)
  }

-- | Complete typed refusal surface for the persistence gate.  The public
-- operations already return 'BuildError'; the witness transports it instead
-- of manufacturing a stringly failure.  A changed snapshot carries its exact
-- observed endpoint so the gate failure is inspectable without a unilateral
-- artifact.
data PersistentSnapshotObstruction
  = PersistentInitialBuildRefused !BuildError
  | PersistentInsertionRefused !Int !BuildError
  | PersistentRemovalRefused !Int !BuildError
  | PersistentRemovalTargetMissing !Int
  | PersistentSnapshotChanged
      !PersistentWitnessLabel
      !PersistentEndpoint
      !PersistentEndpoint
  deriving Show

persistentEndpoint
  :: DelaunayTriangulation Point
  -> IO PersistentEndpoint
persistentEndpoint triangulation = do
  edges <- evaluate (force (canonicalEdges triangulation))
  circular <- evaluate (force (circularGateLines triangulation))
  pure $!
    PersistentEndpoint
      { persistentEndpointVertices = numVertices triangulation
      , persistentEndpointUndirectedEdges = numUndirectedEdges triangulation
      , persistentEndpointInnerFaces = numInnerFaces triangulation
      , persistentEndpointCanonicalEdges = edges
      , persistentEndpointCircularDcel = circular
      }

capturePersistentSnapshot
  :: PersistentWitnessLabel
  -> DelaunayTriangulation Point
  -> IO PersistentSnapshotWitness
capturePersistentSnapshot label triangulation = do
  endpoint <- persistentEndpoint triangulation
  pure
    PersistentSnapshotWitness
      { persistentSnapshotLabel = label
      , persistentSnapshotEndpoint = endpoint
      , persistentSnapshotTriangulation = triangulation
      }

verifyPersistentSnapshot
  :: PersistentSnapshotWitness
  -> IO (Either PersistentSnapshotObstruction ())
verifyPersistentSnapshot witness = do
  observed <- persistentEndpoint (persistentSnapshotTriangulation witness)
  pure
    ( if observed == persistentSnapshotEndpoint witness
        then Right ()
        else
          Left
            ( PersistentSnapshotChanged
                (persistentSnapshotLabel witness)
                (persistentSnapshotEndpoint witness)
                observed
            )
    )

verifyPersistentSnapshots :: [PersistentSnapshotWitness] -> IO (Either PersistentSnapshotObstruction ())
verifyPersistentSnapshots = fmap sequence_ . traverse verifyPersistentSnapshot

assertPersistentSnapshot
  :: Either PersistentSnapshotObstruction value
  -> IO value
assertPersistentSnapshot = require

-- | Fixed sparse checkpoints of a longer immutable chain.  Each one has later
-- edits behind it, so the test distinguishes persistence from merely producing
-- the correct final mesh.
persistentCheckpoint :: Int -> Bool
persistentCheckpoint index = index `elem` [0, 1, 2, 7, 31, 63]

verifyPersistentInsertionSnapshots :: IO ()
verifyPersistentInsertionSnapshots = do
  let points = V.fromList (randomPoints 0x9e3779b97f4a7c15 96)
      initial = empty unitElementDefaults
  initialWitness <- capturePersistentSnapshot (PersistentInsertionWitness 0) initial
  (final, witnesses) <-
    V.ifoldM'
      (\(current, preserved) index point -> do
        nextTriangulation <-
          assertPersistentSnapshot
            ( insertionTriangulation
                <$> first (PersistentInsertionRefused index) (insert current point)
            )
        if persistentCheckpoint index
          then do
            witness <- capturePersistentSnapshot (PersistentInsertionWitness (index + 1)) nextTriangulation
            pure (nextTriangulation, preserved <> [witness])
          else pure (nextTriangulation, preserved)
      )
      (initial, [initialWitness])
      points
  -- Force the terminal endpoint as an out-of-clock comparable sink before
  -- checking predecessors; its edge projection is the same projection the
  -- Rust gate emits for the timed lanes.
  _ <- persistentEndpoint final
  assertPersistentSnapshot =<< verifyPersistentSnapshots witnesses

verifyPersistentRemovalSnapshots :: IO ()
verifyPersistentRemovalSnapshots = do
  let points = V.fromList (randomPoints 0x9e3779b97f4a7c15 128)
      removals = V.take 96 points
  initial <-
    buildTriangulation
      <$> assertPersistentSnapshot
        (first PersistentInitialBuildRefused (delaunay unitElementDefaults points))
  initialWitness <- capturePersistentSnapshot (PersistentRemovalWitness 0) initial
  (final, witnesses) <-
    V.ifoldM'
      (\(current, preserved) index point -> do
        outcome <-
          assertPersistentSnapshot
            (first (PersistentRemovalRefused index) (locateAndRemove current point))
        nextTriangulation <-
          case outcome of
            Nothing -> assertPersistentSnapshot (Left (PersistentRemovalTargetMissing index))
            Just removal -> pure (removalTriangulation removal)
        if persistentCheckpoint index
          then do
            witness <- capturePersistentSnapshot (PersistentRemovalWitness (index + 1)) nextTriangulation
            pure (nextTriangulation, preserved <> [witness])
          else pure (nextTriangulation, preserved)
      )
      (initial, [initialWitness])
      removals
  _ <- persistentEndpoint final
  assertPersistentSnapshot =<< verifyPersistentSnapshots witnesses

-- | The Haskell side's immutable-contract gate.  It emits no extra artifact:
-- adding a unilateral file would make the cross-language artifact diff fail.
-- Its selected predecessor sections are instead verified before the existing
-- shared endpoint artifacts are written.
verifyPersistentSnapshotWitnesses :: IO ()
verifyPersistentSnapshotWitnesses = do
  verifyPersistentInsertionSnapshots
  verifyPersistentRemovalSnapshots

-- | Pin the exact output of every timed workload before comparing its cost.
-- Persistent witness artifacts above are final-state agreement gates, not
-- cross-language timing rows.
writeGate :: FilePath -> IO ()
writeGate directory = do
  createDirectoryIfMissing True directory
  verifyPersistentSnapshotWitnesses
  traverse_ (writeBulkLoadGate directory) [1000, 10000]
  traverse_ (writeIncrementalGate directory) [1000, 10000]
  writeNearestGate directory
  writeConstraintGate directory
  traverse_ (writeRefinementGate directory) [625, 2500]
  traverse_ (uncurry (writeRemovalGate directory)) [(1000, 250), (10000, 2500)]
  traverse_ (uncurry (writeInterpolationGate directory)) [(10000, 1000), (100000, 2000)]
  writeVoronoiGate directory 1000
  writeDcelWalkGate directory 2000
  writeIntersectionGate directory 10000 500
  writeIntersectionVertexGate directory 2000
  -- Persistent artifacts validate final-state agreement only; they are not
  -- Spade timing rows. Their canonical egress is retained for the hard gate.
  writePersistentGate directory 1000
  writePersistentRemovalGate directory 10000 2500
  -- The remaining diagnostic gates. Timing is meaningful only after both
  -- sides have been shown to compute the same canonical result.
  writeConstraintIncrementalGate directory 8000 800
  writeConstraintSplitGate directory 1000
  writeSweepAngleGate directory 2000
  writeDegenerateLineGate directory 2000
  writeHierarchyIncrementalGate directory 1000
  writeHierarchyDuplicateGate directory 10000 500
  writeHierarchyRemovalGate directory 10000 250
  writeIntersectionOutsideGate directory 2000 100

-- | The shapes on which the two implementations classify the constrained
-- domain. They do not agree, so this is a characterization rather than a gate:
-- the committed baselines lock in the known divergence and fail if either side
-- moves. See @README.md@ for the compatibility rule.
divergenceShapes :: [(String, [Point], [(Int, Int)])]
divergenceShapes =
  [ ("flush", square 8 <> [Point 4 4], loop4)
  , ("notched", square 8 <> [Point 13 4, Point 4 4], loop4)
  , ("flush-plus-dangling-segment", square 8 <> [Point 2 2, Point 6 6], loop4 <> [(4, 5)])
  , ("notched-plus-dangling-segment", square 8 <> [Point 13 4, Point 2 2, Point 6 6], loop4 <> [(5, 6)])
  , ( "annulus"
    , square 12 <> [Point 4 4, Point 8 4, Point 8 8, Point 4 8]
    , loop4 <> [(4, 5), (5, 6), (6, 7), (7, 4)]
    )
  ]
 where
  square scale = [Point 0 0, Point scale 0, Point scale scale, Point 0 scale]
  loop4 = [(0, 1), (1, 2), (2, 3), (3, 0)]

writeDivergence :: FilePath -> IO ()
writeDivergence path = do
  rows <- mapM row divergenceShapes
  writeFile path (unlines rows)
 where
  row (label, points, constraints) = do
    built <- require (constrainedDelaunay unitElementDefaults (V.fromList points) (V.fromList constraints))
    let cdt = buildTriangulation built
        parameters =
          defaultRefinementParameters
            { refineMaxAdditionalVertices = Just 0
            , refineExcludeOuterFaces = True
            }
    classified <- require (refine id parameters cdt)
    pure
      ( label
          <> ": inner " <> show (numInnerFaces cdt)
          <> ", excluded " <> show (V.length (refinementExcludedFaces classified))
      )

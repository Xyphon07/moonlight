-- | The one module in the package that spawns threads. Its whole contract is
-- that scheduling is invisible: the same tournament, the same value, at every
-- worker count. That is what is asserted here, because a concurrent join whose
-- result depended on the worker count would be wrong in a way no amount of
-- validity checking on a single run could see.
module Moonlight.Triangulation.ParallelSpec
  ( tests
  ) where

import qualified Data.List.NonEmpty as NE
import Moonlight.Triangulation (unions)
import Moonlight.Triangulation.AlgebraFixtures (Mesh, assertMesh, operands)
import Moonlight.Triangulation.Parallel (unionsConcurrently)
import Support (requireRight)

tests :: IO ()
tests = do
  testConcurrentJoinAgreesWithSequential
  testConcurrentJoinIsWorkerCountInvariant
  putStrLn "parallel: ok"

-- Counts on both sides of the operand count, so the tournament is starved at
-- one end and saturated at the other, and 1 exercises the sequential-collapse
-- branch that a purely concurrent test would never reach.
workerCounts :: [Int]
workerCounts = [1, 2, 3, 5, 16]

concurrentJoin :: Int -> NE.NonEmpty Mesh -> IO Mesh
concurrentJoin workers shards = do
  outcome <- unionsConcurrently workers shards
  requireRight ("concurrent unions at " <> show workers <> " workers") outcome

-- The sequential tournament is the oracle. It is a different interpreter over
-- the same plan, not a second copy of this one, so agreement is evidence.
testConcurrentJoinAgreesWithSequential :: IO ()
testConcurrentJoinAgreesWithSequential = do
  shards <- fmap (map snd) operands
  nonEmptyShards <- case NE.nonEmpty shards of
    Nothing -> fail "concurrent join agreement: no operands"
    Just present -> pure present
  sequential <- requireRight "sequential unions" (unions shards)
  mapM_
    ( \workers -> do
        concurrent <- concurrentJoin workers nonEmptyShards
        assertMesh
          ("concurrent tournament at " <> show workers <> " workers")
          sequential
          concurrent
    )
    workerCounts

-- Stated separately from agreement: even if both interpreters were wrong in
-- the same way, a result that moved with the worker count would still be a
-- defect, and this is the assertion that would catch it.
testConcurrentJoinIsWorkerCountInvariant :: IO ()
testConcurrentJoinIsWorkerCountInvariant = do
  shards <- fmap (map snd) operands
  nonEmptyShards <- case NE.nonEmpty shards of
    Nothing -> fail "worker-count invariance: no operands"
    Just present -> pure present
  results <- traverse (`concurrentJoin` nonEmptyShards) workerCounts
  case results of
    [] -> fail "worker-count invariance: no worker counts"
    (reference : rest) ->
      mapM_
        ( \(workers, result) ->
            assertMesh
              ("worker count " <> show workers <> " agrees with the first")
              reference
              result
        )
        (zip (drop 1 workerCounts) rest)

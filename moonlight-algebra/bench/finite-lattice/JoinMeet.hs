module JoinMeet
  ( joinMeetComparisonBenchmarks,
  )
where

import Control.DeepSeq
  ( NFData (..),
  )
import Data.Bifunctor
  ( first,
  )
import Data.Map.Strict qualified as Map
import Fixtures
  ( Shape,
    assertFiniteFixture,
    caseLabel,
    compileLatticeEnv,
    keys,
    querySizes,
    shapeJoinMeetTable,
    shapeLabel,
    shapes,
  )
import Kernels
  ( joinMeetSweepWeight,
  )
import Moonlight.FiniteLattice.Core
  ( ContextLattice,
    joinContext,
    meetContext,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

data PreparedJoinMeetComparison = PreparedJoinMeetComparison !Int !(ContextLattice Int) !(Map.Map (Int, Int) (Int, Int))

instance NFData PreparedJoinMeetComparison where
  rnf (PreparedJoinMeetComparison size lattice table) =
    rnf (size, lattice, Map.toAscList table)

joinMeetComparisonBenchmarks :: Benchmark
joinMeetComparisonBenchmarks =
  bgroup
    "join-meet-world-baseline"
    [ bgroup
        (shapeLabel shape)
        (fmap (joinMeetComparisonBenchmark shape) querySizes)
    | shape <- shapes
    ]

joinMeetComparisonBenchmark :: Shape -> Int -> Benchmark
joinMeetComparisonBenchmark shape size =
  env (prepareJoinMeetComparison shape size) $ \prepared ->
    bgroup
      (caseLabel "query sweep" size)
      [ bench "moonlight: compiled ContextLattice join/meet" (nf moonlightJoinMeetComparisonWeight prepared),
        bench "baseline: precomputed join/meet Data.Map lookup" (nf worldJoinMeetComparisonWeight prepared)
      ]

prepareJoinMeetComparison :: Shape -> Int -> IO PreparedJoinMeetComparison
prepareJoinMeetComparison shape size = do
  lattice <- compileLatticeEnv shape size
  let !table = shapeJoinMeetTable shape size
  assertFiniteFixture "join/meet table" (assertJoinMeetTableAgrees size lattice table)
  pure (PreparedJoinMeetComparison size lattice table)

assertJoinMeetTableAgrees :: Int -> ContextLattice Int -> Map.Map (Int, Int) (Int, Int) -> Either String ()
assertJoinMeetTableAgrees size lattice table =
  fmap (const ()) (traverse checkPair pairs)
  where
    pairs =
      [ (leftValue, rightValue)
      | leftValue <- keys size,
        rightValue <- keys size
      ]

    checkPair (leftValue, rightValue) = do
      joined <- first show (joinContext lattice leftValue rightValue)
      met <- first show (meetContext lattice leftValue rightValue)
      case Map.lookup (leftValue, rightValue) table of
        Just expected
          | expected == (joined, met) ->
              Right ()
          | otherwise ->
              Left
                ( "join/meet table mismatch for "
                    <> show (leftValue, rightValue)
                    <> ": baseline "
                    <> show expected
                    <> ", lattice "
                    <> show (joined, met)
                )
        Nothing ->
          Left ("missing join/meet table entry " <> show (leftValue, rightValue))

moonlightJoinMeetComparisonWeight :: PreparedJoinMeetComparison -> Either String Int
moonlightJoinMeetComparisonWeight (PreparedJoinMeetComparison size lattice _) =
  joinMeetSweepWeight size lattice

worldJoinMeetComparisonWeight :: PreparedJoinMeetComparison -> Either String Int
worldJoinMeetComparisonWeight (PreparedJoinMeetComparison size _ table) =
  fmap sum (traverse tablePairWeight [(leftValue, rightValue) | leftValue <- keys size, rightValue <- keys size])
  where
    tablePairWeight pairValue =
      case Map.lookup pairValue table of
        Just (joined, met) -> Right (joined + met)
        Nothing -> Left ("missing join/meet table entry " <> show pairValue)

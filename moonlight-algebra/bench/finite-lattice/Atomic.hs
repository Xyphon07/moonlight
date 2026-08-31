module Atomic
  ( atomicOperationComparisonBenchmarks,
  )
where

import Algebra.Lattice
  ( joinLeq,
    (/\),
    (\/),
  )
import Control.DeepSeq
  ( NFData (..),
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntSet qualified as IntSet
import Fixtures
  ( Shape (BooleanCube),
    assertFiniteFixture,
    booleanCubeBits,
    caseLabel,
    compileLatticeEnv,
    hackageBooleanCubeElement,
    hackageBooleanCubeElements,
    keys,
    querySizes,
  )
import Kernels
  ( leqSweepWeight,
    residentJoinMeetKeySweepWeight,
  )
import Moonlight.FiniteLattice.Core
  ( ContextLattice,
    joinContext,
    leqContext,
    meetContext,
  )
import Moonlight.FiniteLattice.Resident
  ( residentContextKeys,
    withResidentContext,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

data PreparedHackageAtomicComparison = PreparedHackageAtomicComparison !Int !(ContextLattice Int) ![IntSet.IntSet]

instance NFData PreparedHackageAtomicComparison where
  rnf (PreparedHackageAtomicComparison size lattice elements) =
    rnf (size, lattice, elements)

atomicOperationComparisonBenchmarks :: Benchmark
atomicOperationComparisonBenchmarks =
  bgroup
    "atomic-operation-baseline"
    [ bgroup
        "boolean-cube"
        (fmap hackageAtomicComparisonBenchmark querySizes)
    ]

hackageAtomicComparisonBenchmark :: Int -> Benchmark
hackageAtomicComparisonBenchmark size =
  env (prepareHackageAtomicComparison size) $ \prepared ->
    bgroup
      (caseLabel "hackage-compatible query sweep" size)
      [ bench "moonlight: resident key <= sweep" (nf moonlightAtomicLeqWeight prepared),
        bench "hackage lattices: IntSet joinLeq sweep" (nf hackageAtomicLeqWeight prepared),
        bench "moonlight: public join sweep" (nf moonlightAtomicJoinWeight prepared),
        bench "hackage lattices: IntSet \\/ sweep" (nf hackageAtomicJoinWeight prepared),
        bench "moonlight: public meet sweep" (nf moonlightAtomicMeetWeight prepared),
        bench "hackage lattices: IntSet /\\ sweep" (nf hackageAtomicMeetWeight prepared),
        bench "moonlight: resident key join/meet sweep" (nf moonlightAtomicJoinMeetWeight prepared),
        bench "hackage lattices: IntSet join/meet sweep" (nf hackageAtomicJoinMeetWeight prepared)
      ]

prepareHackageAtomicComparison :: Int -> IO PreparedHackageAtomicComparison
prepareHackageAtomicComparison size = do
  lattice <- compileLatticeEnv BooleanCube size
  let !elements = hackageBooleanCubeElements size
  assertFiniteFixture "hackage BooleanCube atomic baseline" (assertHackageAtomicAgrees size lattice elements)
  pure (PreparedHackageAtomicComparison size lattice elements)

assertHackageAtomicAgrees :: Int -> ContextLattice Int -> [IntSet.IntSet] -> Either String ()
assertHackageAtomicAgrees size lattice elements
  | length elements /= size =
      Left ("hackage element count " <> show (length elements) <> " /= " <> show size)
  | otherwise =
      fmap (const ()) (traverse checkPair pairs)
  where
    bits =
      booleanCubeBits size

    keyedElements =
      zip (keys size) elements

    pairs =
      [ (leftValue, leftElement, rightValue, rightElement)
      | (leftValue, leftElement) <- keyedElements,
        (rightValue, rightElement) <- keyedElements
      ]

    checkPair (leftValue, leftElement, rightValue, rightElement) = do
      actualLeq <- first show (leqContext lattice leftValue rightValue)
      joined <- first show (joinContext lattice leftValue rightValue)
      met <- first show (meetContext lattice leftValue rightValue)
      let !hackageLeq = joinLeq leftElement rightElement
          !hackageJoin = leftElement \/ rightElement
          !hackageMeet = leftElement /\ rightElement
          !moonlightJoin = hackageBooleanCubeElement bits joined
          !moonlightMeet = hackageBooleanCubeElement bits met
      if actualLeq == hackageLeq && moonlightJoin == hackageJoin && moonlightMeet == hackageMeet
        then Right ()
        else
          Left ("hackage BooleanCube mismatch for " <> show (leftValue, rightValue))

moonlightAtomicLeqWeight :: PreparedHackageAtomicComparison -> Int
moonlightAtomicLeqWeight (PreparedHackageAtomicComparison size lattice _) =
  leqSweepWeight size lattice

moonlightAtomicJoinWeight :: PreparedHackageAtomicComparison -> Either String Int
moonlightAtomicJoinWeight (PreparedHackageAtomicComparison size lattice _) =
  fmap sum
    ( traverse
        joinPairWeight
        [ (leftValue, rightValue)
        | leftValue <- keys size,
          rightValue <- keys size
        ]
    )
  where
    joinPairWeight (leftValue, rightValue) =
      first show (joinContext lattice leftValue rightValue)

moonlightAtomicMeetWeight :: PreparedHackageAtomicComparison -> Either String Int
moonlightAtomicMeetWeight (PreparedHackageAtomicComparison size lattice _) =
  fmap sum
    ( traverse
        meetPairWeight
        [ (leftValue, rightValue)
        | leftValue <- keys size,
          rightValue <- keys size
        ]
    )
  where
    meetPairWeight (leftValue, rightValue) =
      first show (meetContext lattice leftValue rightValue)

moonlightAtomicJoinMeetWeight :: PreparedHackageAtomicComparison -> Either String Int
moonlightAtomicJoinMeetWeight (PreparedHackageAtomicComparison _size lattice _) =
  withResidentContext lattice $ \contextValue ->
    residentJoinMeetKeySweepWeight contextValue (residentContextKeys contextValue)

hackageAtomicLeqWeight :: PreparedHackageAtomicComparison -> Int
hackageAtomicLeqWeight (PreparedHackageAtomicComparison _size _lattice elements) =
  length
    [ ()
    | leftElement <- elements,
      rightElement <- elements,
      joinLeq leftElement rightElement
    ]

hackageAtomicJoinWeight :: PreparedHackageAtomicComparison -> Int
hackageAtomicJoinWeight (PreparedHackageAtomicComparison _size _lattice elements) =
  sum
    [ IntSet.size (leftElement \/ rightElement)
    | leftElement <- elements,
      rightElement <- elements
    ]

hackageAtomicMeetWeight :: PreparedHackageAtomicComparison -> Int
hackageAtomicMeetWeight (PreparedHackageAtomicComparison _size _lattice elements) =
  sum
    [ IntSet.size (leftElement /\ rightElement)
    | leftElement <- elements,
      rightElement <- elements
    ]

hackageAtomicJoinMeetWeight :: PreparedHackageAtomicComparison -> Int
hackageAtomicJoinMeetWeight (PreparedHackageAtomicComparison _size _lattice elements) =
  sum
    [ IntSet.size (leftElement \/ rightElement)
        + IntSet.size (leftElement /\ rightElement)
    | leftElement <- elements,
      rightElement <- elements
    ]

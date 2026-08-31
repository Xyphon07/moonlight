module Adhesive.Symbolic
  ( symbolicFixtureBenchmarks,
  )
where

import AbstractFixtures
  ( BenchCategory (..),
    BenchMorphism (..),
    benchLeftCospanLeg,
    benchMorphism,
    benchMorphismWeight,
    benchMonicMatch,
    benchObjectWeight,
    benchRuleLeg,
  )
import BenchSupport (batchWeight, boolWeight, sampleBatch512)
import Moonlight.Category.Pure.Adhesive
  ( PBPOComplementWitness,
    PushoutComplementWitness,
    monicMatchArrow,
    pbpoComplement,
    pbpoComplementBorrowedLeg,
    pbpoComplementPullbackObject,
    pbpoComplementPullbackToBorrowed,
    pbpoComplementPullbackToMatch,
    pbpoComplementPushoutFromComplement,
    pbpoComplementPushoutFromMatch,
    pbpoComplementPushoutObject,
    pbpoComplementResidualLeg,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pushoutComplement,
    pushoutComplementBorrowedLeg,
    pushoutComplementObject,
    pushoutComplementResidualLeg,
    pushoutComplementSquareCommutes,
    witnessMonic,
  )
import Moonlight.Category.Pure.Limits (pullback, pullbackMediator, pushout)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

symbolicFixtureBenchmarks :: Benchmark
symbolicFixtureBenchmarks =
  bgroup
    "symbolic constant fixture"
    [ symbolicLimitBenchmarks,
      symbolicAdhesiveBenchmarks,
      symbolicPBPObenchmarks
    ]

symbolicLimitBenchmarks :: Benchmark
symbolicLimitBenchmarks =
  bgroup
    "limits"
    [ bench "pullback witness batch x512" (nf (batchWeight pullbackWeight) sampleBatch512),
      bench "pullback mediator batch x512" (nf (batchWeight pullbackMediatorWeight) sampleBatch512),
      bench "pushout witness batch x512" (nf (batchWeight pushoutWeight) sampleBatch512)
    ]

symbolicAdhesiveBenchmarks :: Benchmark
symbolicAdhesiveBenchmarks =
  bgroup
    "adhesive"
    [ bench "witnessMonic batch x512" (nf (batchWeight monicWitnessWeight) sampleBatch512),
      bench "pushoutComplement witness batch x512" (nf (batchWeight pushoutComplementWeight) sampleBatch512),
      bench "pushoutComplement square commute check batch x512" (nf (batchWeight pushoutComplementCommuteWeight) sampleBatch512)
    ]

symbolicPBPObenchmarks :: Benchmark
symbolicPBPObenchmarks =
  bgroup
    "PBPO"
    [ bench "pbpoComplement witness batch x512" (nf (batchWeight pbpoComplementWeight) sampleBatch512),
      bench "pbpo pullback square commute check batch x512" (nf (batchWeight pbpoPullbackCommuteWeight) sampleBatch512),
      bench "pbpo pushout square commute check batch x512" (nf (batchWeight pbpoPushoutCommuteWeight) sampleBatch512)
    ]

pullbackWeight :: Int -> Int
pullbackWeight seed =
  seed + maybe 0 pullbackTripleWeight (pullback BenchCategory benchMonicMatch borrowedLeg)

pullbackMediatorWeight :: Int -> Int
pullbackMediatorWeight seed =
  seed + maybe 0 benchMorphismWeight (pullbackMediator BenchCategory benchMonicMatch borrowedLeg benchRuleLeg benchLeftCospanLeg)

pushoutWeight :: Int -> Int
pushoutWeight seed =
  seed + maybe 0 pushoutTripleWeight (pushout BenchCategory benchRuleLeg benchLeftCospanLeg)

monicWitnessWeight :: Int -> Int
monicWitnessWeight seed =
  seed + maybe 0 (benchMorphismWeight . monicMatchArrow) (witnessMonic BenchCategory benchMonicMatch)

pushoutComplementWeight :: Int -> Int
pushoutComplementWeight seed =
  seed + maybe 0 pushoutComplementWitnessWeight pushoutComplementWitnessValue

pushoutComplementCommuteWeight :: Int -> Int
pushoutComplementCommuteWeight seed =
  seed + maybe 0 (boolWeight . pushoutComplementSquareCommutes BenchCategory) pushoutComplementWitnessValue

pbpoComplementWeight :: Int -> Int
pbpoComplementWeight seed =
  seed + maybe 0 pbpoComplementWitnessWeight pbpoComplementWitnessValue

pbpoPullbackCommuteWeight :: Int -> Int
pbpoPullbackCommuteWeight seed =
  seed + maybe 0 (boolWeight . pbpoPullbackSquareCommutes BenchCategory) pbpoComplementWitnessValue

pbpoPushoutCommuteWeight :: Int -> Int
pbpoPushoutCommuteWeight seed =
  seed + maybe 0 (boolWeight . pbpoPushoutSquareCommutes BenchCategory) pbpoComplementWitnessValue

borrowedLeg :: BenchMorphism
borrowedLeg = benchMorphism (benchMorphismTarget benchLeftCospanLeg) (benchMorphismTarget benchMonicMatch)

pushoutComplementWitnessValue :: Maybe (PushoutComplementWitness BenchCategory)
pushoutComplementWitnessValue = do
  monicWitness <- witnessMonic BenchCategory benchMonicMatch
  pushoutComplement BenchCategory benchRuleLeg monicWitness

pbpoComplementWitnessValue :: Maybe (PBPOComplementWitness BenchCategory)
pbpoComplementWitnessValue = do
  monicWitness <- witnessMonic BenchCategory benchMonicMatch
  pbpoComplement BenchCategory benchRuleLeg monicWitness

pullbackTripleWeight :: (a, BenchMorphism, BenchMorphism) -> Int
pullbackTripleWeight (_, leftLeg, rightLeg) =
  benchMorphismWeight leftLeg + benchMorphismWeight rightLeg

pushoutTripleWeight :: (a, BenchMorphism, BenchMorphism) -> Int
pushoutTripleWeight (_, leftLeg, rightLeg) =
  benchMorphismWeight leftLeg + benchMorphismWeight rightLeg

pushoutComplementWitnessWeight :: PushoutComplementWitness BenchCategory -> Int
pushoutComplementWitnessWeight witness =
  benchObjectWeight (pushoutComplementObject witness)
    + benchMorphismWeight (pushoutComplementBorrowedLeg witness)
    + benchMorphismWeight (pushoutComplementResidualLeg witness)

pbpoComplementWitnessWeight :: PBPOComplementWitness BenchCategory -> Int
pbpoComplementWitnessWeight witness =
  benchObjectWeight (pbpoComplementPullbackObject witness)
    + benchMorphismWeight (pbpoComplementPullbackToBorrowed witness)
    + benchMorphismWeight (pbpoComplementPullbackToMatch witness)
    + benchObjectWeight (pbpoComplementPushoutObject witness)
    + benchMorphismWeight (pbpoComplementPushoutFromComplement witness)
    + benchMorphismWeight (pbpoComplementPushoutFromMatch witness)
    + benchMorphismWeight (pbpoComplementBorrowedLeg witness)
    + benchMorphismWeight (pbpoComplementResidualLeg witness)

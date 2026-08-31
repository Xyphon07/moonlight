module Algebraic.StructuredCospan
  ( structuredCospanBenchmarks,
    structuredCospanWeight,
  )
where

import BenchSupport (batchWeight, sampleBatch512)
import Data.Function ((&))
import AbstractFixtures
  ( BenchCategory (..),
    benchLeftCospanLeg,
    benchLeftCospanRightLeg,
    benchMorphismWeight,
    benchObjectWeight,
    benchRightCospanLeftLeg,
    benchRightCospanRightLeg,
  )
import Moonlight.Category.Pure.StructuredCospan
  ( StructuredCospan,
    composeStructuredCospan,
    mkStructuredCospan,
    structuredApex,
    structuredDecoration,
    structuredLeftLeg,
    structuredRightLeg,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

structuredCospanBenchmarks :: Benchmark
structuredCospanBenchmarks =
  bgroup
    "StructuredCospan"
    [ bench "mkStructuredCospan left batch x512" (nf (batchWeight structuredCospanBuildWeight) sampleBatch512),
      bench "composeStructuredCospan batch x512" (nf (batchWeight structuredCospanComposeWeight) sampleBatch512)
    ]
structuredCospanBuildWeight :: Int -> Int
structuredCospanBuildWeight seed =
  mkStructuredCospan BenchCategory benchLeftCospanLeg benchLeftCospanRightLeg seed
    & either (const 0) structuredCospanWeight

structuredCospanComposeWeight :: Int -> Int
structuredCospanComposeWeight seed =
  case (demoLeftStructuredCospan seed, demoRightStructuredCospan (seed + 1)) of
    (Right leftValue, Right rightValue) ->
      composeStructuredCospan BenchCategory (+) leftValue rightValue
        & either (const 0) structuredCospanWeight
    _ -> 0

demoLeftStructuredCospan :: Int -> Either () (StructuredCospan BenchCategory Int)
demoLeftStructuredCospan seed =
  mkStructuredCospan BenchCategory benchLeftCospanLeg benchLeftCospanRightLeg seed
    & either (const (Left ())) Right

demoRightStructuredCospan :: Int -> Either () (StructuredCospan BenchCategory Int)
demoRightStructuredCospan seed =
  mkStructuredCospan BenchCategory benchRightCospanLeftLeg benchRightCospanRightLeg seed
    & either (const (Left ())) Right

structuredCospanWeight :: StructuredCospan BenchCategory Int -> Int
structuredCospanWeight cospanValue =
  benchMorphismWeight (structuredLeftLeg cospanValue)
    + benchMorphismWeight (structuredRightLeg cospanValue)
    + benchObjectWeight (structuredApex cospanValue)
    + structuredDecoration cospanValue

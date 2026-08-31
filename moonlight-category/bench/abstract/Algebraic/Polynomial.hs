{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Algebraic.Polynomial
  ( polynomialBenchmarks,
  )
where

import BenchSupport (batchWeight, boolWeight, sampleBatch512)
import Data.Function ((&))
import Moonlight.Category.Pure.CoveringFamily (Exists (..))
import Moonlight.Category.Pure.PolynomialFunctor
  ( Direction,
    ParameterizedDirection,
    ParameterizedPolynomialFunctor (..),
    PolynomialFunctor (..),
  )
import Moonlight.Category.Test.PolynomialFixture
  ( BranchPosition,
    DemoParameterizedPolynomial,
    DemoPolynomial,
    FullSliceBranchPosition,
    FullSliceRootPosition,
    ParameterizedPosition
      ( FullSliceBranchWitness,
        FullSliceRootWitness,
        TrimmedSliceRootWitness
      ),
    Position (BranchWitness, RootWitness),
    RootPosition,
    TrimmedSliceRootPosition,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

polynomialBenchmarks :: Benchmark
polynomialBenchmarks =
  bgroup
    "PolynomialFunctor"
    [ bench "allPositions batch x512" (nf (batchWeight polynomialPositionsWeight) sampleBatch512),
      bench "positionsAt full/trimmed alternation batch x512" (nf parameterizedPolynomialPositionsBatchWeight sampleBatch512),
      bench "direction witnesses batch x512" (nf (batchWeight polynomialDirectionWeight) sampleBatch512)
    ]
polynomialPositionsWeight :: Int -> Int
polynomialPositionsWeight seed =
  seed + sum (fmap polynomialPositionWeight (allPositions @DemoPolynomial))

parameterizedPolynomialPositionsBatchWeight :: [Int] -> Int
parameterizedPolynomialPositionsBatchWeight =
  sum . fmap (\seed -> seed + parameterizedPolynomialPositionsWeight (even seed))

parameterizedPolynomialPositionsWeight :: Bool -> Int
parameterizedPolynomialPositionsWeight includeBranch =
  positionsAt @DemoParameterizedPolynomial includeBranch
    & fmap parameterizedPolynomialPositionWeight
    & sum

polynomialDirectionWeight :: Int -> Int
polynomialDirectionWeight seed =
  seed + boolWeight rootDirectionWitness + maybe 0 boolWeight (branchDirectionWitness seed) + boolWeight fullSliceRootDirectionWitness + maybe 0 boolWeight (fullSliceBranchDirectionWitness seed) + trimmedSliceRootDirectionWeight trimmedSliceRootDirectionWitness

rootDirectionWitness :: Direction DemoPolynomial RootPosition
rootDirectionWitness = True

branchDirectionWitness :: Int -> Direction DemoPolynomial BranchPosition
branchDirectionWitness seed = Just (even seed)

fullSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial FullSliceRootPosition
fullSliceRootDirectionWitness = True

fullSliceBranchDirectionWitness :: Int -> ParameterizedDirection DemoParameterizedPolynomial FullSliceBranchPosition
fullSliceBranchDirectionWitness seed = Just (odd seed)

trimmedSliceRootDirectionWitness :: ParameterizedDirection DemoParameterizedPolynomial TrimmedSliceRootPosition
trimmedSliceRootDirectionWitness = ()

polynomialPositionWeight :: Exists (Position DemoPolynomial) -> Int
polynomialPositionWeight (Exists witness) =
  case witness of
    RootWitness -> 1
    BranchWitness -> 2

parameterizedPolynomialPositionWeight :: Exists (ParameterizedPosition DemoParameterizedPolynomial) -> Int
parameterizedPolynomialPositionWeight (Exists witness) =
  case witness of
    FullSliceRootWitness -> 3
    FullSliceBranchWitness -> 5
    TrimmedSliceRootWitness -> 7

trimmedSliceRootDirectionWeight :: () -> Int
trimmedSliceRootDirectionWeight () = 1

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Algebraic.Double
  ( doubleCategoryBenchmarks,
  )
where

import BenchSupport (batchWeight, boolWeight, sampleBatch512)
import Data.Proxy (Proxy (..))
import Moonlight.Category.Pure.DoubleCategory (DoubleCategory (..), interchangeLaw)
import Moonlight.Category.Test.DoubleFixture
  ( SymbolicDouble,
    SymbolicHorizontal (..),
    SymbolicObject (..),
    SymbolicSquare (..),
    SymbolicVertical (..),
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

doubleCategoryBenchmarks :: Benchmark
doubleCategoryBenchmarks =
  bgroup
    "DoubleCategory"
    [ bench "interchangeLaw symbolic 2x2 grid batch x512" (nf (batchWeight doubleInterchangeWeight) sampleBatch512),
      bench "typed horizontal identity compose batch x512" (nf (batchWeight doubleHorizontalIdentityWeight) sampleBatch512),
      bench "typed vertical identity compose batch x512" (nf (batchWeight doubleVerticalIdentityWeight) sampleBatch512)
    ]

doubleInterchangeWeight :: Int -> Int
doubleInterchangeWeight seed =
  seed + maybe 0 boolWeight (interchangeLaw @SymbolicObject @(SymbolicDouble Int) (northWestSquare seed) (northEastSquare seed) (southWestSquare seed) (southEastSquare seed))

doubleHorizontalIdentityWeight :: Int -> Int
doubleHorizontalIdentityWeight seed =
  seed + maybe 0 horizontalWeight (composeHorizontal @SymbolicObject @(SymbolicDouble Int) (horizontalIdentity @SymbolicObject @(SymbolicDouble Int) (Proxy @'ObjectB)) (horizontalArrow seed :: SymbolicHorizontal Int 'ObjectA 'ObjectB))

doubleVerticalIdentityWeight :: Int -> Int
doubleVerticalIdentityWeight seed =
  seed + maybe 0 verticalWeight (composeVertical @SymbolicObject @(SymbolicDouble Int) (verticalIdentity @SymbolicObject @(SymbolicDouble Int) (Proxy @'ObjectB)) (verticalArrow seed :: SymbolicVertical Int 'ObjectA 'ObjectB))

horizontalArrow :: Int -> SymbolicHorizontal Int source target
horizontalArrow labelValue = SymbolicHorizontal [labelValue]

verticalArrow :: Int -> SymbolicVertical Int source target
verticalArrow labelValue = SymbolicVertical [labelValue]

northWestSquare :: Int -> SymbolicSquare Int 'ObjectA 'ObjectB 'ObjectD 'ObjectE
northWestSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 1),
      symbolicSquareBottom = horizontalArrow (seed + 4),
      symbolicSquareLeft = verticalArrow (seed + 7),
      symbolicSquareRight = verticalArrow (seed + 8)
    }

northEastSquare :: Int -> SymbolicSquare Int 'ObjectB 'ObjectC 'ObjectE 'ObjectF
northEastSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 2),
      symbolicSquareBottom = horizontalArrow (seed + 5),
      symbolicSquareLeft = verticalArrow (seed + 8),
      symbolicSquareRight = verticalArrow (seed + 9)
    }

southWestSquare :: Int -> SymbolicSquare Int 'ObjectD 'ObjectE 'ObjectG 'ObjectH
southWestSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 4),
      symbolicSquareBottom = horizontalArrow (seed + 6),
      symbolicSquareLeft = verticalArrow (seed + 10),
      symbolicSquareRight = verticalArrow (seed + 11)
    }

southEastSquare :: Int -> SymbolicSquare Int 'ObjectE 'ObjectF 'ObjectH 'ObjectI
southEastSquare seed =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow (seed + 5),
      symbolicSquareBottom = horizontalArrow (seed + 7),
      symbolicSquareLeft = verticalArrow (seed + 11),
      symbolicSquareRight = verticalArrow (seed + 12)
    }

horizontalWeight :: SymbolicHorizontal Int source target -> Int
horizontalWeight (SymbolicHorizontal traceValue) = sum traceValue

verticalWeight :: SymbolicVertical Int source target -> Int
verticalWeight (SymbolicVertical traceValue) = sum traceValue

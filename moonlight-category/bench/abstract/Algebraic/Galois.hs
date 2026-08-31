{-# LANGUAGE TypeApplications #-}

module Algebraic.Galois
  ( galoisBenchmarks,
  )
where

import BenchSupport (batchWeight, sampleBatch512)
import Data.Function ((&))
import Moonlight.Category.Pure.Galois (GaloisConnection (..), OrdinalGalois (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

galoisBenchmarks :: Benchmark
galoisBenchmarks =
  bgroup
    "Galois"
    [ bench "alpha/gamma round trip batch x512" (nf galoisRoundTripBatchWeight sampleBatch512),
      bench "threshold enumeration batch x512" (nf (batchWeight galoisThresholdWeight) sampleBatch512)
    ]

newtype FineLevel = FineLevel {unFineLevel :: Int}
  deriving stock (Eq, Ord, Show)

newtype CoarseLevel = CoarseLevel {unCoarseLevel :: Int}
  deriving stock (Eq, Ord, Show)

instance GaloisConnection FineLevel CoarseLevel where
  alpha (FineLevel value) = CoarseLevel (value `div` 4)
  gamma (CoarseLevel value) = FineLevel (value * 4)

instance OrdinalGalois FineLevel CoarseLevel where
  thresholds =
    [ (FineLevel 0, CoarseLevel 0),
      (FineLevel 4, CoarseLevel 1),
      (FineLevel 8, CoarseLevel 2),
      (FineLevel 16, CoarseLevel 4)
    ]

galoisRoundTripBatchWeight :: [Int] -> Int
galoisRoundTripBatchWeight =
  sum . fmap (galoisRoundTripWeight . FineLevel)

galoisRoundTripWeight :: FineLevel -> Int
galoisRoundTripWeight fineValue =
  let coarseValue = alpha fineValue
      returnedFine = gamma coarseValue
   in unFineLevel fineValue + unCoarseLevel coarseValue + unFineLevel returnedFine

galoisThresholdWeight :: Int -> Int
galoisThresholdWeight seed =
  seed
    + ( thresholds @FineLevel @CoarseLevel
          & fmap (\(FineLevel fineValue, CoarseLevel coarseValue) -> fineValue + coarseValue + seed `mod` 3)
          & sum
      )

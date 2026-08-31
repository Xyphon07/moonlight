module BenchSupport
  ( BenchSetup (..),
    batchWeight,
    boolWeight,
    prepareBenchValue,
    sampleBatch512,
  )
where

import Data.Kind (Type)

type BenchSetup :: Type -> Type
newtype BenchSetup value = BenchSetup
  { runBenchSetup :: Either String value
  }

prepareBenchValue :: BenchSetup value -> IO value
prepareBenchValue =
  either (ioError . userError) pure . runBenchSetup

sampleBatch512 :: [Int]
sampleBatch512 = [0 .. 511]

batchWeight :: (Int -> Int) -> [Int] -> Int
batchWeight weight =
  sum . fmap weight
{-# INLINE batchWeight #-}

boolWeight :: Bool -> Int
boolWeight value =
  if value then 1 else 0
{-# INLINE boolWeight #-}

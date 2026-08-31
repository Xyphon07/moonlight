module Simplex
  ( indexedSimplexBenchmarks,
  )
where

import BenchSupport (sampleBatch512)
import Numeric.Natural (Natural)
import Moonlight.Category.Pure.Indexed.Category qualified as Indexed
import Moonlight.Category.Pure.Indexed.Simplex
  ( S,
    Simplex,
    Z,
    codegeneracyFirst,
    codegeneracyLast,
    codegeneracySucc,
    cofaceFirst,
    cofaceLast,
    cofaceSucc,
    simplexSucc,
    simplexValues,
    simplexZero,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5

indexedSimplexBenchmarks :: Benchmark
indexedSimplexBenchmarks =
  bgroup
    "simplex Δ"
    [ bench "identity decode Δ6 batch x512" (nf simplexIdentityBatchWeight sampleBatch512),
      bench "coface/codegeneracy decode batch x512" (nf simplexGeneratorBatchWeight sampleBatch512),
      bench "compose and decode generators batch x512" (nf simplexComposeDecodeBatchWeight sampleBatch512)
    ]

simplexIdentityBatchWeight :: [Int] -> Int
simplexIdentityBatchWeight =
  sum . fmap (\seed -> seed + simplexValuesWeight (simplexValues simplex6))

simplexGeneratorBatchWeight :: [Int] -> Int
simplexGeneratorBatchWeight =
  sum . fmap simplexGeneratorWeight

simplexGeneratorWeight :: Int -> Int
simplexGeneratorWeight seed =
  seed
    + case seed `mod` 6 of
      0 -> simplexValuesWeight (simplexValues (cofaceFirst simplex5))
      1 -> simplexValuesWeight (simplexValues (cofaceLast simplex5))
      2 -> simplexValuesWeight (simplexValues (cofaceSucc (cofaceFirst simplex4)))
      3 -> simplexValuesWeight (simplexValues (codegeneracyFirst simplex5))
      4 -> simplexValuesWeight (simplexValues (codegeneracyLast simplex5))
      _ -> simplexValuesWeight (simplexValues (codegeneracySucc (codegeneracyFirst simplex4)))

simplexComposeDecodeBatchWeight :: [Int] -> Int
simplexComposeDecodeBatchWeight =
  sum . fmap simplexComposeDecodeWeight

simplexComposeDecodeWeight :: Int -> Int
simplexComposeDecodeWeight seed =
  let left = codegeneracyFirst simplex5 :: Simplex N6 N5
      right = cofaceFirst simplex5 :: Simplex N5 N6
      identityLike = left Indexed.. right
      shifted = cofaceSucc (cofaceSucc (cofaceFirst simplex3))
      collapsed = codegeneracySucc (codegeneracySucc (codegeneracyFirst simplex3))
   in seed
        + simplexValuesWeight (simplexValues identityLike)
        + simplexValuesWeight (simplexValues (collapsed Indexed.. shifted))

simplexValuesWeight :: [Natural] -> Int
simplexValuesWeight =
  sum . fmap fromIntegral

simplex0 :: Simplex N0 N0
simplex0 = simplexZero

simplex1 :: Simplex N1 N1
simplex1 = simplexSucc simplex0

simplex2 :: Simplex N2 N2
simplex2 = simplexSucc simplex1

simplex3 :: Simplex N3 N3
simplex3 = simplexSucc simplex2

simplex4 :: Simplex N4 N4
simplex4 = simplexSucc simplex3

simplex5 :: Simplex N5 N5
simplex5 = simplexSucc simplex4

simplex6 :: Simplex N6 N6
simplex6 = simplexSucc simplex5

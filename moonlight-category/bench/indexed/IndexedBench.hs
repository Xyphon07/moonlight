module IndexedBench
  ( indexedBenchmarks,
  )
where

import Simplex (indexedSimplexBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup)

indexedBenchmarks :: Benchmark
indexedBenchmarks =
  bgroup
    "indexed"
    [indexedSimplexBenchmarks]

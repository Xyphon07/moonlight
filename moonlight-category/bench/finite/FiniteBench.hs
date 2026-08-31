module FiniteBench
  ( finiteBenchmarks,
  )
where

import FinCat (finCatBenchmarks)
import Invertibility (invertibilityBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup)

finiteBenchmarks :: Benchmark
finiteBenchmarks =
  bgroup
    "finite"
    [ finCatBenchmarks,
      invertibilityBenchmarks
    ]

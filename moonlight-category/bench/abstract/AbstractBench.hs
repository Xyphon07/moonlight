module AbstractBench
  ( abstractBenchmarks,
  )
where

import Adhesive.Suite (adhesiveBenchmarks)
import Algebraic.Suite (algebraicSurfaceBenchmarks)
import Covering (coveringBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup)

abstractBenchmarks :: Benchmark
abstractBenchmarks =
  bgroup
    "abstract"
    [ coveringBenchmarks,
      adhesiveBenchmarks,
      algebraicSurfaceBenchmarks
    ]

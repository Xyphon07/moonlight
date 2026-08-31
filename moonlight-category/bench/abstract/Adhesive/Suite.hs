module Adhesive.Suite
  ( adhesiveBenchmarks,
  )
where

import Adhesive.Graph (finiteGraphDPOBenchmarks)
import Adhesive.Subset (finiteSubsetDPOBenchmarks)
import Adhesive.Symbolic (symbolicFixtureBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup)

adhesiveBenchmarks :: Benchmark
adhesiveBenchmarks =
  bgroup
    "limits / adhesive / PBPO witnesses"
    [ symbolicFixtureBenchmarks,
      finiteGraphDPOBenchmarks,
      finiteSubsetDPOBenchmarks
    ]

module SimplicialBench
  ( simplicialBenchmarks,
  )
where

import SimplicialDelta (deltaBenchmarks)
import SimplicialNerve (nerveBenchmarks)
import SimplicialSpaces (generatedSpaceBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup)

simplicialBenchmarks :: Benchmark
simplicialBenchmarks =
  bgroup
    "simplicial"
    [ deltaBenchmarks,
      generatedSpaceBenchmarks,
      nerveBenchmarks
    ]

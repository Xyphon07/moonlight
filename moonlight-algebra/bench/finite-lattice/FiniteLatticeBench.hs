module FiniteLatticeBench
  ( finiteLatticeBenchmarkSuite,
  )
where

import Atomic
  ( atomicOperationComparisonBenchmarks,
  )
import Finite
  ( finiteLatticeBenchmarks,
  )
import JoinMeet
  ( joinMeetComparisonBenchmarks,
  )
import Rows
  ( contextRowComparisonBenchmarks,
  )
import Support
  ( supportBenchmarks,
  )
import Tableless
  ( tablelessFallbackBenchmarks,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bgroup,
  )

finiteLatticeBenchmarkSuite :: Benchmark
finiteLatticeBenchmarkSuite =
  bgroup
    "finite-lattice"
    [ finiteLatticeBenchmarks,
      tablelessFallbackBenchmarks,
      supportBenchmarks,
      atomicOperationComparisonBenchmarks,
      joinMeetComparisonBenchmarks,
      contextRowComparisonBenchmarks
    ]

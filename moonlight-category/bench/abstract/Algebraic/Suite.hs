module Algebraic.Suite
  ( algebraicSurfaceBenchmarks,
  )
where

import Algebraic.Decorated (decoratedBenchmarks)
import Algebraic.Double (doubleCategoryBenchmarks)
import Algebraic.Galois (galoisBenchmarks)
import Algebraic.Polynomial (polynomialBenchmarks)
import Algebraic.StructuredCospan (structuredCospanBenchmarks)
import Test.Tasty.Bench (Benchmark, bgroup)

algebraicSurfaceBenchmarks :: Benchmark
algebraicSurfaceBenchmarks =
  bgroup
    "algebraic category surfaces"
    [ galoisBenchmarks,
      polynomialBenchmarks,
      structuredCospanBenchmarks,
      decoratedBenchmarks,
      doubleCategoryBenchmarks
    ]

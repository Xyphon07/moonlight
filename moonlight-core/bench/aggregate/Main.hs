module Main
  ( main,
  )
where

import NumericBench (numericBenchmarks)
import SolverBench (solverBenchmarks)
import BasisBench (basisBenchmarks)
import AutomataBench
  ( automataBenchmarkPreflight,
    automataBenchmarks,
  )
import SyntaxBench
  ( syntaxBenchmarkPreflight,
    syntaxBenchmarks,
  )
import TermBench (termBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  (syntaxBenchmarkPreflight *> automataBenchmarkPreflight)
    *> defaultMain
      [ basisBenchmarks,
        numericBenchmarks,
        syntaxBenchmarks,
        automataBenchmarks,
        solverBenchmarks,
        termBenchmarks
      ]

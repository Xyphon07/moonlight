module Main
  ( main,
  )
where

import AbstractBench
  ( abstractBenchmarks,
  )
import FiniteLatticeBench
  ( finiteLatticeBenchmarkSuite,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ abstractBenchmarks,
      finiteLatticeBenchmarkSuite
    ]

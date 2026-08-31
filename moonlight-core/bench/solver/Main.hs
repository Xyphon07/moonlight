module Main
  ( main,
  )
where

import SolverBench (solverBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [solverBenchmarks]

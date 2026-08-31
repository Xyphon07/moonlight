module Main
  ( main,
  )
where

import FiniteLatticeBench
  ( finiteLatticeBenchmarkSuite,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [finiteLatticeBenchmarkSuite]

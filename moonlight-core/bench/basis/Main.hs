module Main
  ( main,
  )
where

import BasisBench (basisBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [basisBenchmarks]

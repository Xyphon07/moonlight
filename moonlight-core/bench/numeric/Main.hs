module Main
  ( main,
  )
where

import NumericBench (numericBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [numericBenchmarks]

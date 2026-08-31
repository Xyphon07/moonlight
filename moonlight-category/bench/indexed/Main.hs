module Main
  ( main,
  )
where

import IndexedBench (indexedBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [indexedBenchmarks]

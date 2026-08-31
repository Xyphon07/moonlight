module Main
  ( main,
  )
where

import AbstractBench (abstractBenchmarks)
import FiniteBench (finiteBenchmarks)
import IndexedBench (indexedBenchmarks)
import SimplicialBench (simplicialBenchmarks)
import SiteBench (siteBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain
    [ abstractBenchmarks,
      finiteBenchmarks,
      siteBenchmarks,
      indexedBenchmarks,
      simplicialBenchmarks
    ]

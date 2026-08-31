module Main
  ( main,
  )
where

import RepairBench
  ( repairBenchmarks,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [repairBenchmarks]

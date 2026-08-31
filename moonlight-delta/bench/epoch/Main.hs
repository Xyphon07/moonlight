module Main
  ( main,
  )
where

import EpochBench
  ( epochBenchmarks,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [epochBenchmarks]

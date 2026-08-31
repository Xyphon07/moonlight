module Main
  ( main,
  )
where

import AbstractBench
  ( abstractBenchmarks,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [abstractBenchmarks]

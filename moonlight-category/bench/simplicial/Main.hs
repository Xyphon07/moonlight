module Main
  ( main,
  )
where

import SimplicialBench (simplicialBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [simplicialBenchmarks]

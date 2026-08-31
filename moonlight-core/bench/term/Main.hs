module Main
  ( main,
  )
where

import TermBench (termBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [termBenchmarks]

module Main
  ( main,
  )
where

import FiniteBench (finiteBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [finiteBenchmarks]

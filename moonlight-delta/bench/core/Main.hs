module Main
  ( main,
  )
where

import CoreBench
  ( coreBenchmarks,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [coreBenchmarks]

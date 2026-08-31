module Main
  ( main,
  )
where

import SiteBench (siteBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  defaultMain [siteBenchmarks]

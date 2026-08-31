module Main
  ( main,
  )
where

import AutomataBench
  ( automataBenchmarkPreflight,
    automataBenchmarks,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  automataBenchmarkPreflight
    *> defaultMain [automataBenchmarks]

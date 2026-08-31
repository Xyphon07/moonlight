module Main
  ( main,
  )
where

import SyntaxBench
  ( syntaxBenchmarkPreflight,
    syntaxBenchmarks,
  )
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main =
  syntaxBenchmarkPreflight
    *> defaultMain [syntaxBenchmarks]

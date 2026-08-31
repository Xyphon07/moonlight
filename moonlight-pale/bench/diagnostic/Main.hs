module Main
  ( main,
  )
where

import DiagnosticBench (diagnosticBenchmarks)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = defaultMain [diagnosticBenchmarks]

module Main (main) where

import qualified Moonlight.Triangulation.PowerBench as PowerBench

main :: IO ()
main = do
  cases <- PowerBench.prepareCases
  PowerBench.reportCases cases
  PowerBench.runBenchmarks cases

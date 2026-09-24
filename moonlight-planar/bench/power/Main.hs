module Main (main) where

import qualified Moonlight.Planar.PowerBench as PowerBench
import qualified Moonlight.Planar.PowerMassBench as PowerMassBench
import System.Environment (getArgs, withArgs)
import Test.Tasty.Bench (defaultMain)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    "--mass-only" : remaining ->
      withArgs remaining (PowerMassBench.prepareBenchmarks >>= defaultMain)
    _ -> do
      cases <- PowerBench.prepareCases
      PowerBench.reportCases cases
      PowerBench.runBenchmarks cases

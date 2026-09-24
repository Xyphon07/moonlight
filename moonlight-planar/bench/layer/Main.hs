module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Moonlight.Planar.LayerBench as LayerBench

main :: IO ()
main = do
  fixtures <- LayerBench.prepareFixtures >>= evaluate . force
  LayerBench.runBenchmarks fixtures

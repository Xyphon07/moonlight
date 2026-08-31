module Main (main) where

import qualified Moonlight.Triangulation.LayerBench as LayerBench

main :: IO ()
main = do
  fixtures <- LayerBench.prepareFixtures
  LayerBench.runBenchmarks fixtures

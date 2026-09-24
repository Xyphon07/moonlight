module Main (main) where

import qualified Moonlight.Planar.CurveBench as Curve
import qualified Moonlight.Planar.AuthoringBench as Authoring

main :: IO ()
main = Curve.benchmarks >> Authoring.benchmarks

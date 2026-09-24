module Main (main) where

import qualified Moonlight.Planar.CurveBench as Curve
import qualified Moonlight.Planar.AuthoringBench as Authoring
import qualified Moonlight.Planar.MeasureBench as Measure
import qualified Moonlight.Planar.RegionBench as Region

main :: IO ()
main = Curve.benchmarks >> Authoring.benchmarks >> Measure.benchmarks >> Region.benchmarks

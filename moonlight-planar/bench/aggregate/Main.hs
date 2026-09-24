-- | Every benchmark slice, in one process. This imports the slice modules
-- rather than restating their contents, so a benchmark added to a slice appears
-- here without anyone remembering to add it. An aggregate that duplicates its
-- slices instead of importing them drifts the moment a slice grows.
module Main (main) where

import qualified Moonlight.Planar.BuildBench as BuildBench
import qualified Moonlight.Planar.DcelBench as DcelBench
import qualified Moonlight.Planar.DualBench as DualBench
import qualified Moonlight.Planar.JoinBench as JoinBench
import qualified Moonlight.Planar.RegionBench as RegionBench

main :: IO ()
main = do
  BuildBench.benchmarks
  DcelBench.benchmarks
  DualBench.benchmarks
  JoinBench.benchmarks
  RegionBench.benchmarks

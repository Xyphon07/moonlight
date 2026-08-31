module Main
  ( main,
  )
where

import BenchSupport (readStateScale)
import PatchBench
  ( patchBenchmarks,
  )
import Patch.Allocation
  ( runPatchAllocationOrBenchmarks,
  )

main :: IO ()
main = do
  stateScale <- readStateScale
  runPatchAllocationOrBenchmarks [patchBenchmarks stateScale]

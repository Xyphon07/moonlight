module Main
  ( main,
  )
where

import BenchSupport
  ( readStateScale,
  )
import CoreBench
  ( coreBenchmarks,
  )
import EpochBench
  ( epochBenchmarks,
  )
import PatchBench
  ( patchBenchmarks,
  )
import Patch.Allocation
  ( runPatchAllocationOrBenchmarks,
  )
import RepairBench
  ( repairBenchmarks,
  )

main :: IO ()
main = do
  stateScale <- readStateScale
  runPatchAllocationOrBenchmarks
    [ coreBenchmarks,
      patchBenchmarks stateScale,
      epochBenchmarks,
      repairBenchmarks
    ]

module RepairBench
  ( repairBenchmarks,
  )
where

import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import BenchSupport
  ( caseLabel,
    naturalWeight,
    repairSizes,
  )
import Moonlight.Delta.Repair
  ( Config (..),
    Kernel (..),
    Result (..),
    Round,
    Step (..),
    Trace (..),
    boundedRepair,
    boundedRepairTraced,
    applied,
    irreducible,
    obstructions,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nf,
  )

data RepairObstruction = BelowTarget !Int
  deriving stock (Eq, Show)

data Correction = AddChunk !Int
  deriving stock (Eq, Show)

repairBenchmarks :: Benchmark
repairBenchmarks =
  bgroup
    "repair"
    (repairSizes >>= repairBenchmarksForSize)

repairBenchmarksForSize :: Int -> [Benchmark]
repairBenchmarksForSize target =
  [ bench (caseLabel "boundedRepair" target) (nf repairResultWeight target),
    bench (caseLabel "boundedRepairTraced" target) (nf repairTraceWeight target)
  ]

repairResultWeight :: Int -> Int
repairResultWeight target =
  repairResultScore (boundedRepair (repairKernel target) (repairConfig target) 0)

repairTraceWeight :: Int -> Int
repairTraceWeight target =
  let (result, traceValue) = boundedRepairTraced (repairKernel target) (repairConfig target) 0
   in repairResultScore result + repairTraceScore traceValue

repairResultScore :: Result Int RepairObstruction -> Int
repairResultScore result =
  case result of
    ResultConverged stateValue rounds -> stateValue + naturalWeight rounds
    ResultStuck stateValue obstructionValues rounds -> stateValue + obstructionWeight obstructionValues + naturalWeight rounds
    ResultBudgetExhausted stateValue obstructionValues irreducibleObstructions rounds ->
      stateValue
        + obstructionWeight obstructionValues
        + length irreducibleObstructions
        + naturalWeight rounds

repairTraceScore :: Trace RepairObstruction Correction -> Int
repairTraceScore (Trace rounds) =
  sum (fmap repairRoundScore rounds)

repairRoundScore :: Round RepairObstruction Correction -> Int
repairRoundScore roundValue =
  obstructionWeight (obstructions roundValue)
    + naturalWeight (applied roundValue)
    + length (irreducible roundValue)

repairKernel :: Int -> Kernel Int RepairObstruction Correction
repairKernel target =
  Kernel
    { check = inspectRepairState target,
      residuate = proposeRepairCorrection target,
      applyKernelCorrection = applyRepairCorrection target
    }

inspectRepairState :: Int -> Int -> Step Int RepairObstruction
inspectRepairState target stateValue
  | stateValue >= target = StepConverged stateValue
  | otherwise = StepObstructed stateValue (BelowTarget target :| [])

proposeRepairCorrection :: Int -> RepairObstruction -> Maybe Correction
proposeRepairCorrection target obstruction =
  case obstruction of
    BelowTarget _ -> Just (AddChunk (repairChunk target))

applyRepairCorrection :: Int -> Int -> Correction -> Int
applyRepairCorrection target stateValue correction =
  case correction of
    AddChunk chunk -> min target (stateValue + chunk)

repairChunk :: Int -> Int
repairChunk target =
  max 1 (target `div` 16)

repairConfig :: Int -> Config
repairConfig target =
  Config
    { maxRounds = fromIntegral target
    }

obstructionWeight :: NonEmpty RepairObstruction -> Int
obstructionWeight =
  sum . fmap (\(BelowTarget target) -> target)

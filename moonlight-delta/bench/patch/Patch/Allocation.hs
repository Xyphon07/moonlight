{-# LANGUAGE BangPatterns #-}

module Patch.Allocation
  ( runPatchAllocationOrBenchmarks,
    writePatchAllocationCsv,
  )
where

import Control.Exception
  ( evaluate,
  )
import Control.Monad
  ( foldM,
  )
import Data.Word
  ( Word64,
  )
import GHC.Stats
  ( RTSStats (allocated_bytes),
    getRTSStats,
    getRTSStatsEnabled,
  )
import BenchSupport
  ( caseLabel,
    defaultAllocationRepetitions,
    forceBenchmarkFixture,
    halfSize,
    lastKey,
    mapIntWeight,
    middleKey,
    patchDeltaSizes,
    quarterSize,
  )
import PatchBench
  ( hackagePatchApplyConstructionAllocationWeight,
    hackagePatchApplyForcedConstructionWeight,
    hackagePatchComposeConstructionAllocationWeight,
    hackagePatchComposeForcedConstructionWeight,
    hackagePatchReplayForcedWeight,
    hackagePatchReplayWeight,
    patchApplyConstructionAllocationWeight,
    patchApplyForcedConstructionWeight,
    patchApplyOutcomeFixtureWeight,
    patchApplyReferenceOutcomeFixtureWeight,
    patchComposeConstructionAllocationWeight,
    patchComposeForcedConstructionWeight,
    patchFusedReplayForcedWeight,
    patchFusedReplayOutcomeWeight,
    patchFusedReplayWeight,
    patchSequentialReferenceReplayForcedWeight,
    patchSequentialReferenceReplayOutcomeWeight,
    patchSequentialReferenceReplayWeight,
    patchSequentialReplayForcedWeight,
    patchSequentialReplayOutcomeWeight,
    patchSequentialReplayWeight,
    prepareHackagePatchApplyOutput,
    prepareHackagePatchComposeOutput,
    preparePatchApplyOutput,
    preparePatchComposeOutput,
  )
import Patch.Fixtures
import Patch.Hackage
  ( hackagePatchMapWeight,
    prepareHackagePatchApplyFixture,
    prepareHackagePatchComposeFixture,
    prepareHackagePatchReplayFixture,
  )
import Patch.Types
import Moonlight.Delta.Patch qualified as Patch
import PatchReference qualified
import System.Environment
  ( getArgs,
  )
import System.Exit
  ( die,
  )
import System.Mem
  ( performGC,
  )
import Test.Tasty.Bench
  ( Benchmark,
    defaultMain,
  )
import Text.Read
  ( readMaybe,
  )

runPatchAllocationOrBenchmarks :: [Benchmark] -> IO ()
runPatchAllocationOrBenchmarks benchmarks = do
  args <- getArgs
  case parseAllocationRequest args of
    Left message ->
      die message
    Right Nothing ->
      defaultMain benchmarks
    Right (Just (path, repetitions)) ->
      writePatchAllocationCsv path repetitions

parseAllocationRequest :: [String] -> Either String (Maybe (FilePath, Int))
parseAllocationRequest args =
  case args of
    ["--patch-allocation-csv", path] ->
      Right (Just (path, defaultAllocationRepetitions))
    ["--patch-allocation-csv", path, "--allocation-repetitions", repetitionsText] ->
      fmap (\repetitions -> Just (path, repetitions)) (parsePositiveRepetitions repetitionsText)
    [] ->
      Right Nothing
    _ ->
      if "--patch-allocation-csv" `elem` args || "--allocation-repetitions" `elem` args
        then
          Left
            "usage: moonlight-delta-bench --patch-allocation-csv PATH [--allocation-repetitions POSITIVE_INT] +RTS -T"
        else Right Nothing

parsePositiveRepetitions :: String -> Either String Int
parsePositiveRepetitions repetitionsText =
  case readMaybe repetitionsText of
    Just repetitions | repetitions > 0 ->
      Right repetitions
    _ ->
      Left ("invalid --allocation-repetitions value: " <> repetitionsText)

patchAllocationCases :: [AllocationCase]
patchAllocationCases =
  patchDeltaSizes >>= patchAllocationCasesForSize

patchAllocationCasesForSize :: Int -> [AllocationCase]
patchAllocationCasesForSize size =
  patchComposeAllocationCasesForSize size
    <> patchApplyAllocationCasesForSize size
    <> patchReplayAllocationCasesForSize size
    <> patchDerivedAllocationCasesForSize size

patchComposeAllocationCasesForSize :: Int -> [AllocationCase]
patchComposeAllocationCasesForSize size =
  composeAllocationCases "compose overlap none" size (overlapPatchComposeFixture size 0)
    <> composeAllocationCases "compose overlap quarter" size (overlapPatchComposeFixture size (quarterSize size))
    <> composeAllocationCases "compose overlap half" size (overlapPatchComposeFixture size (halfSize size))
    <> composeAllocationCases "compose overlap full" size (PatchComposeFixture (newerPatch size) (olderPatch size))
    <> composeAllocationCases "compose asymmetric newer sparse" size (PatchComposeFixture (sparseNewerPatch size) (olderPatch size))
    <> composeAllocationCases "compose asymmetric older sparse" size (PatchComposeFixture (newerPatch size) (sparseOlderPatch size))

patchApplyAllocationCasesForSize :: Int -> [AllocationCase]
patchApplyAllocationCasesForSize size =
  applyAllocationCases "aligned whole checked apply" size (PatchApplyFixture (initialPatchState size) (olderPatch size))
    <> applyAllocationCases "sparse checked apply to large map" size (PatchApplyFixture (largeInitialPatchState size) (sparsePatch size))
    <> applyAllocationCases "shape-changing assert-absent checked apply" size (PatchApplyFixture (shapeChangingInitialState size) (shapeChangingPatch size))
    <> applyOutcomeAllocationCases "apply failure first key" size (patchApplyFailureFixture size 0)
    <> applyOutcomeAllocationCases "apply failure middle key" size (patchApplyFailureFixture size (middleKey size))
    <> applyOutcomeAllocationCases "apply failure last key" size (patchApplyFailureFixture size (lastKey size))

patchReplayAllocationCasesForSize :: Int -> [AllocationCase]
patchReplayAllocationCasesForSize size =
  replayAllocationCases
    "stable sparse replay over large map"
    size
    (PatchReplayFixture (repeatedPatchInitialState size) (repeatedPatchStream size))
    <> replayAllocationCases
      "rotating sparse replay over large map"
      size
      (PatchReplayFixture (repeatedPatchInitialState size) (rotatingPatchStream size))
    <> replayAllocationCases
      "expanding sparse replay over large map"
      size
      (PatchReplayFixture (repeatedPatchInitialState size) (expandingPatchStream size))
    <> replayAllocationCases
      "disjoint sparse replay over large map"
      size
      (PatchReplayFixture (largeInitialPatchState size) (disjointPatchStream size))
    <> replayAllocationCases
      "insertion-deletion replay over large map"
      size
      (PatchReplayFixture (largeInitialPatchState size) (insertionDeletionPatchStream size))
    <> replayAllocationCases
      "cancellation replay over large map"
      size
      (PatchReplayFixture (repeatedPatchInitialState size) (cancellationPatchStream size))
    <> replayOutcomeAllocationCases
      "stale-at-first replay over large map"
      size
      (PatchReplayFixture (repeatedPatchInitialState size) (stalePatchStreamAt size 0))
    <> replayOutcomeAllocationCases
      "stale-at-middle replay over large map"
      size
      (PatchReplayFixture (repeatedPatchInitialState size) (stalePatchStreamAt size (middleKey size)))
    <> replayOutcomeAllocationCases
      "stale-at-last replay over large map"
      size
      (PatchReplayFixture (repeatedPatchInitialState size) (stalePatchStreamAt size (lastKey size)))

patchDerivedAllocationCasesForSize :: Int -> [AllocationCase]
patchDerivedAllocationCasesForSize size =
  [ fixtureAllocationCase
      (allocationScalarName "snapshot diff" size)
      (preparePatchDiffFixture (PatchDiffFixture (snapshotDiffBeforeState size) (snapshotDiffAfterState size)))
      patchDiffWeight,
    fixtureAllocationCase
      (allocationScalarName "invert insertion deletion patch" size)
      (preparePatchInvertFixture (PatchInvertFixture (shapeChangingPatch size)))
      patchInvertWeight,
    fixtureAllocationCase
      (allocationScalarName "support materialization" size)
      (forceBenchmarkFixture (PatchSupportFixture (olderPatch size)))
      patchSupportWeight,
    fixtureAllocationCase
      (allocationScalarName "repeated-key temporal recording one-by-one" size)
      (forceBenchmarkFixture (PatchProducerFixture (producerCells size)))
      patchProducerWeight,
    fixtureAllocationCase
      (allocationScalarName "recordApplied single-cell producer" size)
      (forceBenchmarkFixture (PatchProducerFixture (singleCellProducerCells size)))
      patchProducerWeight,
    fixtureAllocationCase
      (allocationScalarName "repeated-key temporal recording batch" size)
      (forceBenchmarkFixture (PatchProducerFixture (producerCells size)))
      patchProducerBatchWeight
  ]

composeAllocationCases :: String -> Int -> PatchComposeFixture -> [AllocationCase]
composeAllocationCases label size fixture =
  let prepareFixture =
        preparePatchComposeFixture fixture
      prepareHackageFixture =
        prepareHackagePatchComposeFixture fixture
   in [ fixtureAllocationCase
          (allocationVariantPhaseName label size checkedMapMergeLabel "construct")
          prepareFixture
          (patchComposeConstructionAllocationWeight PatchReference.compose),
        fixtureAllocationCase
          (allocationVariantPhaseName label size checkedMapMergeLabel "construct forced")
          prepareFixture
          (patchComposeForcedConstructionWeight PatchReference.compose),
        fixtureAllocationCase
          (allocationVariantPhaseName label size checkedMapMergeLabel "consume")
          (prepareFixture >>= preparePatchComposeOutput checkedMapMergeLabel PatchReference.compose)
          (patchDeltaWeight . preparedPatch),
        fixtureAllocationCase
          (allocationVariantPhaseName label size moonlightPagedPatchLabel "construct")
          prepareFixture
          (patchComposeConstructionAllocationWeight Patch.compose),
        fixtureAllocationCase
          (allocationVariantPhaseName label size moonlightPagedPatchLabel "construct forced")
          prepareFixture
          (patchComposeForcedConstructionWeight Patch.compose),
        fixtureAllocationCase
          (allocationVariantPhaseName label size moonlightPagedPatchLabel "consume")
          (prepareFixture >>= preparePatchComposeOutput moonlightPagedPatchLabel Patch.compose)
          (patchDeltaWeight . preparedPatch),
        fixtureAllocationCase
          (allocationVariantPhaseName label size hackagePatchMapLabel "construct")
          prepareHackageFixture
          hackagePatchComposeConstructionAllocationWeight,
        fixtureAllocationCase
          (allocationVariantPhaseName label size hackagePatchMapLabel "construct forced")
          prepareHackageFixture
          hackagePatchComposeForcedConstructionWeight,
        fixtureAllocationCase
          (allocationVariantPhaseName label size hackagePatchMapLabel "consume")
          (prepareHackageFixture >>= prepareHackagePatchComposeOutput hackagePatchMapLabel)
          (hackagePatchMapWeight . preparedHackagePatchMap)
      ]

applyAllocationCases :: String -> Int -> PatchApplyFixture -> [AllocationCase]
applyAllocationCases label size fixture =
  let prepareFixture =
        preparePatchApplyFixture fixture
      prepareHackageFixture =
        prepareHackagePatchApplyFixture fixture
   in [ fixtureAllocationCase
          (allocationVariantPhaseName label size checkedMapMergeLabel "construct")
          prepareFixture
          (patchApplyConstructionAllocationWeight PatchReference.apply),
        fixtureAllocationCase
          (allocationVariantPhaseName label size checkedMapMergeLabel "construct forced")
          prepareFixture
          (patchApplyForcedConstructionWeight PatchReference.apply),
        fixtureAllocationCase
          (allocationVariantPhaseName label size checkedMapMergeLabel "consume")
          (prepareFixture >>= preparePatchApplyOutput checkedMapMergeLabel PatchReference.apply)
          mapIntWeight,
        fixtureAllocationCase
          (allocationVariantPhaseName label size moonlightPagedPatchLabel "construct")
          prepareFixture
          (patchApplyConstructionAllocationWeight Patch.apply),
        fixtureAllocationCase
          (allocationVariantPhaseName label size moonlightPagedPatchLabel "construct forced")
          prepareFixture
          (patchApplyForcedConstructionWeight Patch.apply),
        fixtureAllocationCase
          (allocationVariantPhaseName label size moonlightPagedPatchLabel "consume")
          (prepareFixture >>= preparePatchApplyOutput moonlightPagedPatchLabel Patch.apply)
          mapIntWeight,
        fixtureAllocationCase
          (allocationVariantPhaseName label size hackagePatchMapLabel "construct")
          prepareHackageFixture
          hackagePatchApplyConstructionAllocationWeight,
        fixtureAllocationCase
          (allocationVariantPhaseName label size hackagePatchMapLabel "construct forced")
          prepareHackageFixture
          hackagePatchApplyForcedConstructionWeight,
        fixtureAllocationCase
          (allocationVariantPhaseName label size hackagePatchMapLabel "consume")
          (prepareHackageFixture >>= prepareHackagePatchApplyOutput hackagePatchMapLabel)
          mapIntWeight
      ]

applyOutcomeAllocationCases :: String -> Int -> PatchApplyFixture -> [AllocationCase]
applyOutcomeAllocationCases label size fixture =
  [ fixtureAllocationCase
      (allocationVariantName label size checkedMapMergeLabel)
      (preparePatchApplyFixture fixture)
      patchApplyReferenceOutcomeFixtureWeight,
    fixtureAllocationCase
      (allocationVariantName label size moonlightSplitApplyLabel)
      (preparePatchApplyFixture fixture)
      patchApplyOutcomeFixtureWeight
  ]

replayAllocationCases :: String -> Int -> PatchReplayFixture -> [AllocationCase]
replayAllocationCases label size fixture =
  [ fixtureAllocationCase
      (allocationVariantName label size checkedSequentialMapMergeLabel)
      (preparePatchReplayFixture fixture)
      patchSequentialReferenceReplayWeight,
    fixtureAllocationCase
      (allocationVariantPhaseName label size checkedSequentialMapMergeLabel "construct forced")
      (preparePatchReplayFixture fixture)
      patchSequentialReferenceReplayForcedWeight,
    fixtureAllocationCase
      (allocationVariantName label size moonlightSequentialApplyLabel)
      (preparePatchReplayFixture fixture)
      patchSequentialReplayWeight,
    fixtureAllocationCase
      (allocationVariantPhaseName label size moonlightSequentialApplyLabel "construct forced")
      (preparePatchReplayFixture fixture)
      patchSequentialReplayForcedWeight,
    fixtureAllocationCase
      (allocationVariantName label size moonlightUncheckedSequentialPatchLabel)
      (preparePatchReplayFixture fixture)
      patchUncheckedSequentialReplayWeight,
    fixtureAllocationCase
      (allocationVariantName label size hackageSequentialPatchMapLabel)
      (prepareHackagePatchReplayFixture fixture)
      hackagePatchReplayWeight,
    fixtureAllocationCase
      (allocationVariantPhaseName label size hackageSequentialPatchMapLabel "construct forced")
      (prepareHackagePatchReplayFixture fixture)
      hackagePatchReplayForcedWeight,
    fixtureAllocationCase
      (allocationVariantName label size moonlightFusedReplayLabel)
      (preparePatchReplayFixture fixture)
      patchFusedReplayWeight,
    fixtureAllocationCase
      (allocationVariantPhaseName label size moonlightFusedReplayLabel "construct forced")
      (preparePatchReplayFixture fixture)
      patchFusedReplayForcedWeight
  ]

replayOutcomeAllocationCases :: String -> Int -> PatchReplayFixture -> [AllocationCase]
replayOutcomeAllocationCases label size fixture =
  [ fixtureAllocationCase
      (allocationVariantName label size checkedSequentialMapMergeLabel)
      (preparePatchReplayFixture fixture)
      patchSequentialReferenceReplayOutcomeWeight,
    fixtureAllocationCase
      (allocationVariantName label size moonlightSequentialApplyLabel)
      (preparePatchReplayFixture fixture)
      patchSequentialReplayOutcomeWeight,
    fixtureAllocationCase
      (allocationVariantName label size moonlightFusedReplayLabel)
      (preparePatchReplayFixture fixture)
      patchFusedReplayOutcomeWeight
  ]

fixtureAllocationCase ::
  String ->
  IO fixture ->
  (fixture -> Int) ->
  AllocationCase
fixtureAllocationCase name prepareFixture weighFixture =
  AllocationCase
    { allocationCaseName = name,
      allocationCaseAction = do
        !fixture <- prepareFixture
        pure
          (\iteration ->
             evaluate (freshAllocationWeight iteration weighFixture fixture)
          )
    }

allocationVariantName :: String -> Int -> String -> String
allocationVariantName label size variant =
  allocationScalarName label size <> "." <> variant

allocationScalarName :: String -> Int -> String
allocationScalarName label size =
  "All.patch." <> caseLabel label size

allocationVariantPhaseName :: String -> Int -> String -> String -> String
allocationVariantPhaseName label size variant phase =
  allocationVariantName label size (variant <> "." <> phase)

writePatchAllocationCsv :: FilePath -> Int -> IO ()
writePatchAllocationCsv path repetitions = do
  ensureRtsStatsEnabled
  results <- traverse (measureAllocationCase repetitions) patchAllocationCases
  writeFile path (allocationCsv results)

ensureRtsStatsEnabled :: IO ()
ensureRtsStatsEnabled = do
  enabled <- getRTSStatsEnabled
  if enabled
    then pure ()
    else die "RTS stats are disabled; rerun with +RTS -T"

measureAllocationCase :: Int -> AllocationCase -> IO AllocationResult
measureAllocationCase repetitions allocationCase = do
  action <- allocationCaseAction allocationCase
  (baselineBytes, _baselineChecksum) <-
    measureAllocatedBytes
      repetitions
      (\iteration -> evaluate (allocationBaseline iteration))
  (grossBytes, checksum) <- measureAllocatedBytes repetitions action
  pure
    AllocationResult
      { allocationResultName = allocationCaseName allocationCase,
        allocationResultGrossBytes = grossBytes,
        allocationResultBaselineBytes = baselineBytes,
        allocationResultNetBytes = fromIntegral grossBytes - fromIntegral baselineBytes,
        allocationResultRepetitions = repetitions,
        allocationResultChecksum = checksum
      }

measureAllocatedBytes :: Int -> (Int -> IO Int) -> IO (Word64, Int)
measureAllocatedBytes repetitions action = do
  performGC
  !before <- allocated_bytes <$> getRTSStats
  !checksum <- repeatMeasuredAction repetitions action
  !after <- allocated_bytes <$> getRTSStats
  performGC
  pure (after - before, checksum)

repeatMeasuredAction :: Int -> (Int -> IO Int) -> IO Int
repeatMeasuredAction repetitions action =
  foldM measureStep 0 [1 .. repetitions]
  where
    measureStep :: Int -> Int -> IO Int
    measureStep !checksum iteration = do
      !value <- action iteration
      pure (checksum + value)

{-# NOINLINE freshAllocationWeight #-}
freshAllocationWeight :: Int -> (fixture -> Int) -> fixture -> Int
freshAllocationWeight !iteration weighFixture fixture =
  iteration `seq` weighFixture fixture

{-# NOINLINE allocationBaseline #-}
allocationBaseline :: Int -> Int
allocationBaseline !iteration =
  iteration `seq` 1

allocationCsv :: [AllocationResult] -> String
allocationCsv results =
  unlines
    ( "name,gross_allocated_bytes_per_run,baseline_allocated_bytes_per_run,net_allocated_bytes_per_run,repetitions,checksum"
        : fmap allocationCsvLine results
    )

allocationCsvLine :: AllocationResult -> String
allocationCsvLine result =
  allocationResultName result
    <> ","
    <> show (bytesPerRun (allocationResultGrossBytes result) (allocationResultRepetitions result))
    <> ","
    <> show (bytesPerRun (allocationResultBaselineBytes result) (allocationResultRepetitions result))
    <> ","
    <> show (integerBytesPerRun (allocationResultNetBytes result) (allocationResultRepetitions result))
    <> ","
    <> show (allocationResultRepetitions result)
    <> ","
    <> show (allocationResultChecksum result)

bytesPerRun :: Word64 -> Int -> Word64
bytesPerRun bytes repetitions =
  bytes `quot` fromIntegral repetitions

integerBytesPerRun :: Integer -> Int -> Integer
integerBytesPerRun bytes repetitions =
  bytes `quot` fromIntegral repetitions

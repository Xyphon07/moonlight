{-# LANGUAGE BangPatterns #-}

module Patch.Fixtures
  ( preparePatchComposeFixture,
    preparePatchApplyFixture,
    preparePatchReplayFixture,
    preparePatchDiffFixture,
    preparePatchInvertFixture,
    patchCanonicalBefore,
    patchCanonicalAfter,
    patchFromList,
    singletonPatch,
    recordProducerCells,
    replaySequentially,
    replayReferenceSequentially,
    replayUncheckedSequentially,
    applyUncheckedPatch,
    patchUncheckedSequentialReplayWeight,
    patchDeltaWeight,
    patchSupportWeight,
    patchProducerWeight,
    patchProducerBatchWeight,
    patchDiffWeight,
    patchInvertWeight,
    repeatedPatchInitialState,
    scaledRepeatedPatchInitialState,
    repeatedPatchStream,
    rotatingPatchStream,
    expandingPatchStream,
    disjointPatchStream,
    insertionDeletionPatchStream,
    cancellationPatchStream,
    stalePatchStreamAt,
    olderPatch,
    newerPatch,
    sparseOlderPatch,
    sparseNewerPatch,
    overlapPatchComposeFixture,
    newerPatchAtRange,
    initialPatchState,
    largeInitialPatchState,
    scaledLargeInitialPatchState,
    sparsePatch,
    shapeChangingInitialState,
    shapeChangingPatch,
    patchApplyFailureFixture,
    snapshotDiffBeforeState,
    snapshotDiffAfterState,
    producerCells,
    singleCellProducerCells,
    absentReplayKeys,
  )
where

import Control.Monad
  ( foldM,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import BenchSupport
  ( assertBenchmarkAgreement,
    benchFailure,
    boundedOverlap,
    forceBenchmarkFixture,
    halfSize,
    keys,
    mapIntWeight,
    repeatedDeltaKeys,
    repeatedDeltaSupportSize,
  )
import Patch.Types
import Moonlight.Delta.Patch
  ( ApplyError,
    CellPatch,
    ComposeError,
    Patch,
    PatchKey,
    PatchValue,
  )
import Moonlight.Delta.Patch qualified as Patch
import PatchReference qualified

preparePatchComposeFixture :: PatchComposeFixture -> IO PatchComposeFixture
preparePatchComposeFixture fixture =
  assertBenchmarkAgreement
    "patch compose reference/optimized"
    (PatchReference.compose (pcfNewer fixture) (pcfOlder fixture))
    (Patch.compose (pcfNewer fixture) (pcfOlder fixture))
    *> forceBenchmarkFixture fixture

preparePatchApplyFixture :: PatchApplyFixture -> IO PatchApplyFixture
preparePatchApplyFixture fixture =
  assertBenchmarkAgreement
    "patch apply reference/optimized"
    (PatchReference.apply (pafPatch fixture) (pafState fixture))
    (Patch.apply (pafPatch fixture) (pafState fixture))
    *> forceBenchmarkFixture fixture

preparePatchReplayFixture :: PatchReplayFixture -> IO PatchReplayFixture
preparePatchReplayFixture fixture =
  assertBenchmarkAgreement
    "patch replay reference/sequential"
    (replayReferenceSequentially (prfInitialState fixture) (prfPatches fixture))
    (replaySequentially (prfInitialState fixture) (prfPatches fixture))
    *> assertBenchmarkAgreement
      "patch replay reference/fused"
      (PatchReference.replay (prfPatches fixture) (prfInitialState fixture))
      (Patch.replay (prfPatches fixture) (prfInitialState fixture))
    *> forceBenchmarkFixture fixture

preparePatchDiffFixture :: PatchDiffFixture -> IO PatchDiffFixture
preparePatchDiffFixture fixture =
  assertBenchmarkAgreement
    "patch diff applies"
    (Right (pdfAfterState fixture))
    (Patch.apply (Patch.diff (pdfBeforeState fixture) (pdfAfterState fixture)) (pdfBeforeState fixture))
    *> forceBenchmarkFixture fixture

preparePatchInvertFixture :: PatchInvertFixture -> IO PatchInvertFixture
preparePatchInvertFixture fixture =
  assertBenchmarkAgreement
    "patch invert applies"
    (Right (patchCanonicalBefore (pifPatch fixture)))
    (Patch.apply (Patch.invert (pifPatch fixture)) (patchCanonicalAfter (pifPatch fixture)))
    *> forceBenchmarkFixture fixture

recordProducerCells ::
  Patch Int Int ->
  [(Int, CellPatch Int)] ->
  Either (ComposeError Int Int) (Patch Int Int)
recordProducerCells =
  foldM recordCell
  where
    recordCell :: Patch Int Int -> (Int, CellPatch Int) -> Either (ComposeError Int Int) (Patch Int Int)
    recordCell !patchValue (key, cell) =
      Patch.recordApplied key cell patchValue

replaySequentially ::
  Map Int Int ->
  [Patch Int Int] ->
  Either (ApplyError Int Int) (Map Int Int)
replaySequentially =
  foldM (\ !state patchValue -> Patch.apply patchValue state)

replayReferenceSequentially ::
  Map Int Int ->
  [Patch Int Int] ->
  Either (ApplyError Int Int) (Map Int Int)
replayReferenceSequentially =
  foldM (\ !state patchValue -> PatchReference.apply patchValue state)

replayUncheckedSequentially ::
  Map Int Int ->
  [Patch Int Int] ->
  Map Int Int
replayUncheckedSequentially =
  foldl' (\ !state patchValue -> applyUncheckedPatch patchValue state)

applyUncheckedPatch :: Patch Int Int -> Map Int Int -> Map Int Int
applyUncheckedPatch patch state =
  Patch.foldWithKey'
    (\ !updated _key -> updated)
    (\ !updated key after -> Map.insert key after updated)
    (\ !updated key _before -> Map.delete key updated)
    (\ !updated key _before after -> Map.insert key after updated)
    state
    patch

patchUncheckedSequentialReplayWeight :: PatchReplayFixture -> Int
patchUncheckedSequentialReplayWeight fixture =
  mapIntWeight (replayUncheckedSequentially (prfInitialState fixture) (prfPatches fixture))

patchDeltaWeight :: Patch Int Int -> Int
patchDeltaWeight =
  Patch.foldWithKey'
    (\ !total key -> total + key)
    (\ !total key after -> total + key + after)
    (\ !total key before -> total + key + before)
    (\ !total key before after -> total + key + before + after)
    0

patchSupportWeight :: PatchSupportFixture -> Int
patchSupportWeight fixture =
  Set.size (Patch.support (psfPatch fixture))

patchProducerWeight :: PatchProducerFixture -> Int
patchProducerWeight fixture =
  case recordProducerCells Patch.empty (ppfCells fixture) of
    Left err -> benchFailure "patch producer accumulation" err
    Right patchValue -> patchDeltaWeight patchValue

patchProducerBatchWeight :: PatchProducerFixture -> Int
patchProducerBatchWeight fixture =
  case Patch.recordMany (ppfCells fixture) of
    Left err -> benchFailure "patch producer batch accumulation" err
    Right patchValue -> patchDeltaWeight patchValue

patchDiffWeight :: PatchDiffFixture -> Int
patchDiffWeight fixture =
  patchDeltaWeight (Patch.diff (pdfBeforeState fixture) (pdfAfterState fixture))

patchInvertWeight :: PatchInvertFixture -> Int
patchInvertWeight fixture =
  patchDeltaWeight (Patch.invert (pifPatch fixture))

singletonPatch :: key -> Maybe value -> Maybe value -> Patch key value
singletonPatch key before after =
  Patch.singleton key (Patch.cellFromEndpoints before after)

patchFromList :: (PatchKey key, PatchValue value) => [(key, CellPatch value)] -> Patch key value
patchFromList =
  Patch.fromList

patchCanonicalBefore :: Patch Int Int -> Map Int Int
patchCanonicalBefore =
  Patch.mapMaybeWithKey (\_key cell -> Patch.cellBefore cell)

patchCanonicalAfter :: Patch Int Int -> Map Int Int
patchCanonicalAfter =
  Patch.mapMaybeWithKey (\_key cell -> Patch.cellAfter cell)

repeatedPatchInitialState :: Int -> Map Int Int
repeatedPatchInitialState =
  scaledRepeatedPatchInitialState 1

scaledRepeatedPatchInitialState :: Int -> Int -> Map Int Int
scaledRepeatedPatchInitialState stateScale size =
  Map.fromAscList
    [ (key, repeatedPatchInitialValue key)
    | key <- keys (size * 64 * stateScale)
    ]

repeatedPatchInitialValue :: Int -> Int
repeatedPatchInitialValue key
  | key < repeatedDeltaSupportSize = 0
  | otherwise = key

repeatedPatchStream :: Int -> [Patch Int Int]
repeatedPatchStream size =
  fmap patchForStep (keys size)
  where
    patchForStep :: Int -> Patch Int Int
    patchForStep step =
      patchFromList
        [ (key, Patch.replace step (step + 1))
        | key <- repeatedDeltaKeys
        ]

rotatingPatchStream :: Int -> [Patch Int Int]
rotatingPatchStream size =
  fmap patchForStep (keys size)
  where
    patchForStep :: Int -> Patch Int Int
    patchForStep step =
      singletonPatch
        (step `mod` repeatedDeltaSupportSize)
        (Just (step `div` repeatedDeltaSupportSize))
        (Just (step `div` repeatedDeltaSupportSize + 1))

expandingPatchStream :: Int -> [Patch Int Int]
expandingPatchStream size =
  fmap patchForStep (keys size)
  where
    patchForStep :: Int -> Patch Int Int
    patchForStep step =
      patchFromList
        [ (key, Patch.replace (step - key) (step - key + 1))
        | key <- keys (min repeatedDeltaSupportSize (step + 1))
        ]

disjointPatchStream :: Int -> [Patch Int Int]
disjointPatchStream size =
  fmap patchForStep (keys size)
  where
    patchForStep :: Int -> Patch Int Int
    patchForStep step =
      singletonPatch step (Just step) (Just (step + 1))

insertionDeletionPatchStream :: Int -> [Patch Int Int]
insertionDeletionPatchStream size =
  fmap patchForStep (keys size)
  where
    patchForStep :: Int -> Patch Int Int
    patchForStep step =
      patchFromList
        [ (key, insertionDeletionCell step)
        | key <- absentReplayKeys
        ]

    insertionDeletionCell :: Int -> CellPatch Int
    insertionDeletionCell step =
      if even step
        then Patch.insert step
        else Patch.delete (step - 1)

cancellationPatchStream :: Int -> [Patch Int Int]
cancellationPatchStream size =
  fmap patchForStep (keys size)
  where
    patchForStep :: Int -> Patch Int Int
    patchForStep step =
      patchFromList
        [ (key, cancellationCell step)
        | key <- repeatedDeltaKeys
        ]

    cancellationCell :: Int -> CellPatch Int
    cancellationCell step =
      if even step
        then Patch.replace 0 1
        else Patch.replace 1 0

stalePatchStreamAt :: Int -> Int -> [Patch Int Int]
stalePatchStreamAt size failureStep =
  fmap patchForStep (keys size)
  where
    patchForStep :: Int -> Patch Int Int
    patchForStep step =
      patchFromList
        (fmap (patchEntryForStep step) repeatedDeltaKeys)

    patchEntryForStep :: Int -> Int -> (Int, CellPatch Int)
    patchEntryForStep step key =
      (key, patchCellForStep step)

    patchCellForStep :: Int -> CellPatch Int
    patchCellForStep step =
      if step == failureStep
        then Patch.replace (-1) 0
        else Patch.replace step (step + 1)

olderPatch :: Int -> Patch Int Int
olderPatch size =
  patchFromList
    [ (key, Patch.replace key (key + 1))
    | key <- keys size
    ]

newerPatch :: Int -> Patch Int Int
newerPatch size =
  patchFromList
    [ (key, Patch.replace (key + 1) (key + 2))
    | key <- keys size
    ]

sparseOlderPatch :: Int -> Patch Int Int
sparseOlderPatch _size =
  patchFromList
    (fmap sparseOlderEntry repeatedDeltaKeys)
  where
    sparseOlderEntry :: Int -> (Int, CellPatch Int)
    sparseOlderEntry key =
      (key, Patch.replace key (key + 1))

sparseNewerPatch :: Int -> Patch Int Int
sparseNewerPatch _size =
  patchFromList
    (fmap sparseNewerEntry repeatedDeltaKeys)
  where
    sparseNewerEntry :: Int -> (Int, CellPatch Int)
    sparseNewerEntry key =
      (key, Patch.replace (key + 1) (key + 2))

overlapPatchComposeFixture :: Int -> Int -> PatchComposeFixture
overlapPatchComposeFixture size requestedOverlap =
  PatchComposeFixture
    (newerPatchAtRange size (size - boundedOverlap size requestedOverlap))
    (olderPatch size)

newerPatchAtRange :: Int -> Int -> Patch Int Int
newerPatchAtRange size start =
  patchFromList
    (fmap newerEntryAt (keys size))
  where
    newerEntryAt :: Int -> (Int, CellPatch Int)
    newerEntryAt offset =
      let key = start + offset
       in (key, Patch.replace (key + 1) (key + 2))

initialPatchState :: Int -> Map Int Int
initialPatchState size =
  Map.fromAscList
    [ (key, key)
    | key <- keys size
    ]

largeInitialPatchState :: Int -> Map Int Int
largeInitialPatchState =
  scaledLargeInitialPatchState 1

scaledLargeInitialPatchState :: Int -> Int -> Map Int Int
scaledLargeInitialPatchState stateScale size =
  initialPatchState (size * 64 * stateScale)

sparsePatch :: Int -> Patch Int Int
sparsePatch _size =
  patchFromList
    [ (key, Patch.replace key (key + 1))
    | key <- repeatedDeltaKeys
    ]

shapeChangingInitialState :: Int -> Map Int Int
shapeChangingInitialState =
  initialPatchState

shapeChangingPatch :: Int -> Patch Int Int
shapeChangingPatch size =
  patchFromList (presentEdits <> absentEdits)
  where
    presentEdits :: [(Int, CellPatch Int)]
    presentEdits =
      fmap presentShapeEdit (keys size)

    absentEdits :: [(Int, CellPatch Int)]
    absentEdits =
      fmap absentShapeEdit (keys size)

    presentShapeEdit :: Int -> (Int, CellPatch Int)
    presentShapeEdit key =
      if even key
        then (key, Patch.delete key)
        else (key, Patch.replace key (key + 1))

    absentShapeEdit :: Int -> (Int, CellPatch Int)
    absentShapeEdit key =
      let absentKey = size + key
       in if even key
            then (absentKey, Patch.insert key)
            else (absentKey, Patch.assertAbsent)

patchApplyFailureFixture :: Int -> Int -> PatchApplyFixture
patchApplyFailureFixture size failureKey =
  PatchApplyFixture
    (initialPatchState size)
    (patchFromList (fmap patchEntry (keys size)))
  where
    patchEntry :: Int -> (Int, CellPatch Int)
    patchEntry key =
      if key == failureKey
        then (key, Patch.replace (-1) key)
        else (key, Patch.replace key (key + 1))

snapshotDiffBeforeState :: Int -> Map Int Int
snapshotDiffBeforeState =
  initialPatchState

snapshotDiffAfterState :: Int -> Map Int Int
snapshotDiffAfterState size =
  Map.union
    (Map.mapMaybeWithKey changedExistingValue (snapshotDiffBeforeState size))
    (Map.fromAscList (fmap insertedEntry (keys (halfSize size))))
  where
    changedExistingValue :: Int -> Int -> Maybe Int
    changedExistingValue key value
      | key `mod` 4 == 0 = Nothing
      | key `mod` 4 == 1 = Just (value + size)
      | otherwise = Just value

    insertedEntry :: Int -> (Int, Int)
    insertedEntry key =
      (size + key, key)

producerCells :: Int -> [(Int, CellPatch Int)]
producerCells size =
  [ (key, Patch.replace step (step + 1))
  | step <- keys size,
    key <- repeatedDeltaKeys
  ]

singleCellProducerCells :: Int -> [(Int, CellPatch Int)]
singleCellProducerCells size =
  [ (step, Patch.replace step (step + 1))
  | step <- keys size
  ]

absentReplayKeys :: [Int]
absentReplayKeys =
  fmap (subtract repeatedDeltaSupportSize) repeatedDeltaKeys

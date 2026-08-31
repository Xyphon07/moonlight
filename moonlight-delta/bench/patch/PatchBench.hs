{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module PatchBench
  ( patchBenchmarks,
    patchComposeConstructionAllocationWeight,
    patchComposeForcedConstructionWeight,
    patchApplyConstructionAllocationWeight,
    patchApplyForcedConstructionWeight,
    hackagePatchComposeConstructionAllocationWeight,
    hackagePatchComposeForcedConstructionWeight,
    hackagePatchApplyConstructionAllocationWeight,
    hackagePatchApplyForcedConstructionWeight,
    preparePatchComposeOutput,
    preparePatchApplyOutput,
    prepareHackagePatchComposeOutput,
    prepareHackagePatchApplyOutput,
    patchApplyOutcomeFixtureWeight,
    patchApplyReferenceOutcomeFixtureWeight,
    patchSequentialReplayWeight,
    patchSequentialReplayForcedWeight,
    patchSequentialReferenceReplayWeight,
    patchSequentialReferenceReplayForcedWeight,
    patchSequentialReplayOutcomeWeight,
    patchSequentialReferenceReplayOutcomeWeight,
    hackagePatchReplayWeight,
    hackagePatchReplayForcedWeight,
    patchFusedReplayWeight,
    patchFusedReplayForcedWeight,
    patchFusedReplayOutcomeWeight,
  )
where

import Control.DeepSeq
  ( NFData (rnf),
  )
import Control.Exception
  ( evaluate,
    throwIO,
  )
import Data.ByteString qualified as ByteString
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( StableHashDigest (..),
    StableHashEncoding,
    stableHashEncodingChunks,
    stableHashEncodingWord64LE,
  )
import BenchSupport
  ( BenchmarkFixtureFailure (..),
    benchFailure,
    caseLabel,
    forceBenchmarkFixture,
    halfSize,
    lastKey,
    mapIntWeight,
    maybeIntWeight,
    middleKey,
    patchDeltaSizes,
    quarterSize,
  )
import Patch.Fixtures
import Patch.Hackage
import Patch.Types
import Moonlight.Delta.Patch
  ( ApplyError (..),
    Patch,
    ReplayError (..),
  )
import Moonlight.Delta.Patch qualified as Patch
import PatchReference qualified
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
    whnf,
  )

patchBenchmarks :: Int -> Benchmark
patchBenchmarks stateScale =
  bgroup
    "patch"
    ( (patchDeltaSizes >>= patchBenchmarksForSize stateScale)
        <> (deltaHashStateSizes >>= deltaHash128BitRebaselineBenchmarksForSize)
        <> [deltaHashContextualCompositionBenchmarks]
    )

deltaHashStateSizes :: [Int]
deltaHashStateSizes =
  [128, 256, 512, 1024, 2048, 32768, 131072]

deltaHash128BitRebaselineBenchmarksForSize :: Int -> [Benchmark]
deltaHash128BitRebaselineBenchmarksForSize stateSize =
  [ env
      (forceBenchmarkFixture (PatchApplyFixture (initialPatchState stateSize) (sparsePatch stateSize)))
      ( \fixture ->
          bench
            (caseLabel "delta hash flat rebuild after sparse checked apply" stateSize)
            (nf flatDeltaHashPatchWeight fixture)
      ),
    env
      (prepareMerkleDeltaHashFixture stateSize)
      ( \fixture ->
          bench
            (caseLabel "delta hash adaptive sparse checked apply" stateSize)
            (nf adaptiveDeltaHashWeight fixture)
      ),
    env
      (prepareMultisetDeltaHashFixture stateSize)
      ( \fixture ->
          bench
            (caseLabel "delta hash multiset sparse checked apply" stateSize)
            (nf multisetDeltaHashWeight fixture)
      )
  ]

data MerkleDeltaHashFixture = MerkleDeltaHashFixture
  { merkleDeltaHashFixturePatch :: !(Patch Int Int),
    merkleDeltaHashFixtureValue :: !(Patch.MerkleDeltaHash Int Int)
  }

instance NFData MerkleDeltaHashFixture where
  rnf fixture =
    rnfPatch (merkleDeltaHashFixturePatch fixture)
      `seq` rnf (Patch.merkleDeltaHashState (merkleDeltaHashFixtureValue fixture))
      `seq` rnfDeltaHashDigest (Patch.merkleDeltaHashDigest (merkleDeltaHashFixtureValue fixture))

prepareMerkleDeltaHashFixture :: Int -> IO MerkleDeltaHashFixture
prepareMerkleDeltaHashFixture stateSize = do
  fixture <-
    forceBenchmarkFixture
      (PatchApplyFixture (initialPatchState stateSize) (sparsePatch stateSize))
  case
      Patch.buildMerkleDeltaHash
        benchmarkEncoderVersion
        stableHashIntEncoding
        stableHashIntEncoding
        (pafState fixture)
    of
      Left err ->
        throwIO (BenchmarkFixtureFailure "merkle delta hash fixture" (show err))
      Right merkleDeltaHash -> do
        _ <- evaluate (rnfDeltaHashDigest (Patch.merkleDeltaHashDigest merkleDeltaHash))
        pure
          MerkleDeltaHashFixture
            { merkleDeltaHashFixturePatch = pafPatch fixture,
              merkleDeltaHashFixtureValue = merkleDeltaHash
            }

data MultisetDeltaHashFixture = MultisetDeltaHashFixture
  { multisetDeltaHashFixturePatch :: !(Patch Int Int),
    multisetDeltaHashFixtureValue :: !(Patch.MultisetDeltaHash Int Int)
  }

instance NFData MultisetDeltaHashFixture where
  rnf fixture =
    rnfPatch (multisetDeltaHashFixturePatch fixture)
      `seq` rnf (Patch.multisetDeltaHashState (multisetDeltaHashFixtureValue fixture))
      `seq` rnfDeltaHashDigest (Patch.multisetDeltaHashDigest (multisetDeltaHashFixtureValue fixture))

prepareMultisetDeltaHashFixture :: Int -> IO MultisetDeltaHashFixture
prepareMultisetDeltaHashFixture stateSize = do
  fixture <-
    forceBenchmarkFixture
      (PatchApplyFixture (initialPatchState stateSize) (sparsePatch stateSize))
  let multisetDeltaHash =
        Patch.buildMultisetDeltaHash
          benchmarkEncoderVersion
          stableHashIntEncoding
          stableHashIntEncoding
          (pafState fixture) :: Patch.MultisetDeltaHash Int Int
  _ <- evaluate (rnfDeltaHashDigest (Patch.multisetDeltaHashDigest multisetDeltaHash))
  pure
    MultisetDeltaHashFixture
      { multisetDeltaHashFixturePatch = pafPatch fixture,
        multisetDeltaHashFixtureValue = multisetDeltaHash
      }

flatDeltaHashPatchWeight :: PatchApplyFixture -> Int
flatDeltaHashPatchWeight fixture =
  case Patch.apply (pafPatch fixture) (pafState fixture) of
    Left err ->
      benchFailure "delta hash flat rebuild" err
    Right updatedState ->
      case stableHashEncodingChunks stableHashMapEntryEncoding (Map.toAscList updatedState) of
        StableHashDigest digest ->
          fromIntegral digest

stableHashMapEntryEncoding :: (Int, Int) -> StableHashEncoding
stableHashMapEntryEncoding (key, value) =
  stableHashIntEncoding key
    <> stableHashIntEncoding value

stableHashIntEncoding :: Int -> StableHashEncoding
stableHashIntEncoding =
  stableHashEncodingWord64LE . fromIntegral

adaptiveDeltaHashWeight :: MerkleDeltaHashFixture -> (Int, Int)
adaptiveDeltaHashWeight fixture =
  case
      Patch.applyMerkleDeltaHash
        (merkleDeltaHashFixturePatch fixture)
        (merkleDeltaHashFixtureValue fixture)
    of
      Left err ->
        benchFailure "merkle delta hash incremental apply" err
      Right updatedMerkleDeltaHash ->
        deltaHashDigestEncodingWeight (Patch.merkleDeltaHashDigest updatedMerkleDeltaHash)

multisetDeltaHashWeight :: MultisetDeltaHashFixture -> (Int, Int)
multisetDeltaHashWeight fixture =
  case
      Patch.applyMultisetDeltaHash
        (multisetDeltaHashFixturePatch fixture)
        (multisetDeltaHashFixtureValue fixture)
    of
      Left err ->
        benchFailure "multiset delta hash apply" err
      Right updatedMultisetDeltaHash ->
        deltaHashDigestEncodingWeight (Patch.multisetDeltaHashDigest updatedMultisetDeltaHash)

rnfDeltaHashDigest :: Patch.DeltaHashDigest strategy -> ()
rnfDeltaHashDigest =
  rnf . Patch.encodeDeltaHashDigest

deltaHashDigestEncodingWeight :: Patch.DeltaHashDigest strategy -> (Int, Int)
deltaHashDigestEncodingWeight digestValue =
  let encodedDigest = Patch.encodeDeltaHashDigest digestValue
   in ( ByteString.length encodedDigest,
        ByteString.foldl' (\total byte -> total + fromIntegral byte) 0 encodedDigest
      )

data MerkleContextualCompositionFixture = MerkleContextualCompositionFixture
  { merkleContextualNewer :: !(Patch Int Int),
    merkleContextualOlder :: !(Patch Int Int),
    merkleContextualInitial :: !(Patch.MerkleDeltaHash Int Int)
  }

instance NFData MerkleContextualCompositionFixture where
  rnf fixture =
    rnfPatch (merkleContextualNewer fixture)
      `seq` rnfPatch (merkleContextualOlder fixture)
      `seq` rnf (Patch.merkleDeltaHashState (merkleContextualInitial fixture))
      `seq` rnfDeltaHashDigest (Patch.merkleDeltaHashDigest (merkleContextualInitial fixture))

data MultisetContextualCompositionFixture = MultisetContextualCompositionFixture
  { multisetContextualNewer :: !(Patch Int Int),
    multisetContextualOlder :: !(Patch Int Int),
    multisetContextualInitial :: !(Patch.MultisetDeltaHash Int Int)
  }

instance NFData MultisetContextualCompositionFixture where
  rnf fixture =
    rnfPatch (multisetContextualNewer fixture)
      `seq` rnfPatch (multisetContextualOlder fixture)
      `seq` rnf (Patch.multisetDeltaHashState (multisetContextualInitial fixture))
      `seq` rnfDeltaHashDigest (Patch.multisetDeltaHashDigest (multisetContextualInitial fixture))

deltaHashContextualCompositionBenchmarks :: Benchmark
deltaHashContextualCompositionBenchmarks =
  bgroup
    (caseLabel "delta hash contextual composition of two 2-row patches" contextualCompositionStateSize)
    [ env
        prepareMerkleContextualCompositionFixture
        (\fixture -> bench "Merkle" (nf merkleContextualCompositionWeight fixture)),
      env
        prepareMultisetContextualCompositionFixture
        (\fixture -> bench "multiset" (nf multisetContextualCompositionWeight fixture))
    ]

contextualCompositionStateSize :: Int
contextualCompositionStateSize =
  4096

contextualCompositionInitialState :: Map Int Int
contextualCompositionInitialState =
  Map.fromDistinctAscList
    (fmap (\key -> (key, key)) [0 .. contextualCompositionStateSize - 1])

contextualCompositionOlderPatch :: Patch Int Int
contextualCompositionOlderPatch =
  Patch.fromAscList
    [ (0, Patch.replace 0 4096),
      (1, Patch.replace 1 4097)
    ]

contextualCompositionNewerPatch :: Patch Int Int
contextualCompositionNewerPatch =
  Patch.fromAscList
    [ (2, Patch.replace 2 4098),
      (3, Patch.replace 3 4099)
    ]

prepareMerkleContextualCompositionFixture :: IO MerkleContextualCompositionFixture
prepareMerkleContextualCompositionFixture =
  case
      Patch.buildMerkleDeltaHash
        benchmarkEncoderVersion
        stableHashIntEncoding
        stableHashIntEncoding
        contextualCompositionInitialState
    of
      Left err ->
        throwIO (BenchmarkFixtureFailure "Merkle contextual composition fixture" (show err))
      Right initialDeltaHash ->
        forceBenchmarkFixture
          MerkleContextualCompositionFixture
            { merkleContextualNewer = contextualCompositionNewerPatch,
              merkleContextualOlder = contextualCompositionOlderPatch,
              merkleContextualInitial = initialDeltaHash
            }

prepareMultisetContextualCompositionFixture :: IO MultisetContextualCompositionFixture
prepareMultisetContextualCompositionFixture =
  forceBenchmarkFixture
    MultisetContextualCompositionFixture
      { multisetContextualNewer = contextualCompositionNewerPatch,
        multisetContextualOlder = contextualCompositionOlderPatch,
        multisetContextualInitial =
          Patch.buildMultisetDeltaHash
            benchmarkEncoderVersion
            stableHashIntEncoding
            stableHashIntEncoding
            contextualCompositionInitialState
      }

merkleContextualCompositionWeight :: MerkleContextualCompositionFixture -> Int
merkleContextualCompositionWeight fixture =
  case
      Patch.composeMerkleDeltaHash
        (merkleContextualNewer fixture)
        (merkleContextualOlder fixture)
        (merkleContextualInitial fixture)
    of
      Left err ->
        benchFailure "Merkle contextual composition" err
      Right composed ->
        rnfPatch composed `seq` Patch.size composed

multisetContextualCompositionWeight :: MultisetContextualCompositionFixture -> Int
multisetContextualCompositionWeight fixture =
  case
      Patch.composeMultisetDeltaHash
        (multisetContextualNewer fixture)
        (multisetContextualOlder fixture)
        (multisetContextualInitial fixture)
    of
      Left err ->
        benchFailure "multiset contextual composition" err
      Right composed ->
        rnfPatch composed `seq` Patch.size composed

benchmarkEncoderVersion :: Patch.DeltaHashEncoderVersion
benchmarkEncoderVersion =
  Patch.DeltaHashEncoderVersion 1

patchBenchmarksForSize :: Int -> Int -> [Benchmark]
patchBenchmarksForSize stateScale size =
  patchComposeBenchmarksForSize size
    <> patchApplyBenchmarksForSize stateScale size
    <> patchReplayBenchmarksForSize stateScale size
    <> patchDerivedBenchmarksForSize size

patchComposeBenchmarksForSize :: Int -> [Benchmark]
patchComposeBenchmarksForSize size =
  [ env
      (preparePatchComposeFixture (overlapPatchComposeFixture size 0))
      (patchComposeBenchGroup "compose overlap none" size),
    env
      (preparePatchComposeFixture (overlapPatchComposeFixture size (quarterSize size)))
      (patchComposeBenchGroup "compose overlap quarter" size),
    env
      (preparePatchComposeFixture (overlapPatchComposeFixture size (halfSize size)))
      (patchComposeBenchGroup "compose overlap half" size),
    env
      (preparePatchComposeFixture (PatchComposeFixture (newerPatch size) (olderPatch size)))
      (patchComposeBenchGroup "compose overlap full" size),
    env
      (preparePatchComposeFixture (PatchComposeFixture (sparseNewerPatch size) (olderPatch size)))
      (patchComposeBenchGroup "compose asymmetric newer sparse" size),
    env
      (preparePatchComposeFixture (PatchComposeFixture (newerPatch size) (sparseOlderPatch size)))
      (patchComposeBenchGroup "compose asymmetric older sparse" size)
  ]

patchApplyBenchmarksForSize :: Int -> Int -> [Benchmark]
patchApplyBenchmarksForSize stateScale size =
  [ env
      (preparePatchApplyFixture (PatchApplyFixture (initialPatchState size) (olderPatch size)))
      (patchApplyBenchGroup "aligned whole checked apply" size),
    env
      (preparePatchApplyFixture (PatchApplyFixture (scaledLargeInitialPatchState stateScale size) (sparsePatch size)))
      (patchApplyBenchGroup "sparse checked apply to large map" size),
    env
      (preparePatchApplyFixture (PatchApplyFixture (shapeChangingInitialState size) (shapeChangingPatch size)))
      (patchApplyBenchGroup "shape-changing assert-absent checked apply" size),
    env
      (preparePatchApplyFixture (patchApplyFailureFixture size 0))
      (patchApplyOutcomeBenchGroup "apply failure first key" size),
    env
      (preparePatchApplyFixture (patchApplyFailureFixture size (middleKey size)))
      (patchApplyOutcomeBenchGroup "apply failure middle key" size),
    env
      (preparePatchApplyFixture (patchApplyFailureFixture size (lastKey size)))
      (patchApplyOutcomeBenchGroup "apply failure last key" size)
  ]

patchReplayBenchmarksForSize :: Int -> Int -> [Benchmark]
patchReplayBenchmarksForSize stateScale size =
  [ patchReplayBenchmark
      "stable sparse replay over large map"
      size
      (PatchReplayFixture (scaledRepeatedPatchInitialState stateScale size) (repeatedPatchStream size)),
    patchReplayBenchmark
      "rotating sparse replay over large map"
      size
      (PatchReplayFixture (scaledRepeatedPatchInitialState stateScale size) (rotatingPatchStream size)),
    patchReplayBenchmark
      "expanding sparse replay over large map"
      size
      (PatchReplayFixture (scaledRepeatedPatchInitialState stateScale size) (expandingPatchStream size)),
    patchReplayBenchmark
      "disjoint sparse replay over large map"
      size
      (PatchReplayFixture (scaledLargeInitialPatchState stateScale size) (disjointPatchStream size)),
    patchReplayBenchmark
      "insertion-deletion replay over large map"
      size
      (PatchReplayFixture (scaledLargeInitialPatchState stateScale size) (insertionDeletionPatchStream size)),
    patchReplayBenchmark
      "cancellation replay over large map"
      size
      (PatchReplayFixture (scaledRepeatedPatchInitialState stateScale size) (cancellationPatchStream size)),
    patchStaleReplayBenchmark
      "stale-at-first replay over large map"
      size
      (PatchReplayFixture (scaledRepeatedPatchInitialState stateScale size) (stalePatchStreamAt size 0)),
    patchStaleReplayBenchmark
      "stale-at-middle replay over large map"
      size
      (PatchReplayFixture (scaledRepeatedPatchInitialState stateScale size) (stalePatchStreamAt size (middleKey size))),
    patchStaleReplayBenchmark
      "stale-at-last replay over large map"
      size
      (PatchReplayFixture (scaledRepeatedPatchInitialState stateScale size) (stalePatchStreamAt size (lastKey size)))
  ]

patchDerivedBenchmarksForSize :: Int -> [Benchmark]
patchDerivedBenchmarksForSize size =
  [ env
      (preparePatchDiffFixture (PatchDiffFixture (snapshotDiffBeforeState size) (snapshotDiffAfterState size)))
      (bench (caseLabel "snapshot diff" size) . nf patchDiffWeight),
    env
      (preparePatchInvertFixture (PatchInvertFixture (shapeChangingPatch size)))
      (bench (caseLabel "invert insertion deletion patch" size) . nf patchInvertWeight),
    env
      (forceBenchmarkFixture (PatchSupportFixture (olderPatch size)))
      (bench (caseLabel "support materialization" size) . nf patchSupportWeight),
    env
      (forceBenchmarkFixture (PatchProducerFixture (producerCells size)))
      (bench (caseLabel "repeated-key temporal recording one-by-one" size) . nf patchProducerWeight),
    env
      (forceBenchmarkFixture (PatchProducerFixture (singleCellProducerCells size)))
      (bench (caseLabel "recordApplied single-cell producer" size) . nf patchProducerWeight),
    env
      (forceBenchmarkFixture (PatchProducerFixture (producerCells size)))
      (bench (caseLabel "repeated-key temporal recording batch" size) . nf patchProducerBatchWeight)
  ]

patchComposeBenchGroup :: String -> Int -> PatchComposeFixture -> Benchmark
patchComposeBenchGroup label size fixture =
  bgroup
    (caseLabel label size)
    [ patchComposeVariantBenchGroup checkedMapMergeLabel PatchReference.compose fixture,
      patchComposeVariantBenchGroup moonlightPagedPatchLabel Patch.compose fixture,
      env
        (prepareHackagePatchComposeFixture fixture)
        (hackagePatchComposeVariantBenchGroup hackagePatchMapLabel)
    ]

patchComposeVariantBenchGroup ::
  String ->
  PatchComposeImplementation ->
  PatchComposeFixture ->
  Benchmark
patchComposeVariantBenchGroup variant implementation fixture =
  bgroup
    variant
    [ bench "construct" (whnf (patchComposeConstructWith implementation) fixture),
      bench "construct forced" (nf (patchComposeForcedConstructionWeight implementation) fixture),
      env
        (preparePatchComposeOutput variant implementation fixture)
        (\composed -> bench "consume" (nf (patchDeltaWeight . preparedPatch) composed))
    ]

{-# NOINLINE patchComposeConstructWith #-}
patchComposeConstructWith ::
  PatchComposeImplementation ->
  PatchComposeFixture ->
  Patch Int Int
patchComposeConstructWith implementation fixture =
  case implementation (pcfNewer fixture) (pcfOlder fixture) of
    Left err ->
      benchFailure "patch compose construction" err
    Right !composed ->
      composed

preparePatchComposeOutput ::
  String ->
  PatchComposeImplementation ->
  PatchComposeFixture ->
  IO PreparedPatch
preparePatchComposeOutput variant implementation fixture =
  case implementation (pcfNewer fixture) (pcfOlder fixture) of
    Left err ->
      throwIO (BenchmarkFixtureFailure ("patch compose output: " <> variant) (show err))
    Right !composed -> do
      evaluate (rnfPatch composed)
      pure (PreparedPatch composed)

hackagePatchComposeVariantBenchGroup ::
  String ->
  HackagePatchComposeFixture ->
  Benchmark
hackagePatchComposeVariantBenchGroup variant fixture =
  bgroup
    variant
    [ bench "construct" (whnf hackagePatchComposeConstruct fixture),
      bench "construct forced" (nf hackagePatchComposeForcedConstructionWeight fixture),
      env
        (prepareHackagePatchComposeOutput variant fixture)
        (\composed -> bench "consume" (nf (hackagePatchMapWeight . preparedHackagePatchMap) composed))
    ]

prepareHackagePatchComposeOutput ::
  String ->
  HackagePatchComposeFixture ->
  IO PreparedHackagePatchMap
prepareHackagePatchComposeOutput variant fixture = do
  let !composed = hackagePatchComposeConstruct fixture
  evaluate (rnfHackagePatchMap composed)
  variant `seq` pure (PreparedHackagePatchMap composed)

patchApplyBenchGroup :: String -> Int -> PatchApplyFixture -> Benchmark
patchApplyBenchGroup label size fixture =
  bgroup
    (caseLabel label size)
    [ patchApplyVariantBenchGroup checkedMapMergeLabel PatchReference.apply fixture,
      patchApplyVariantBenchGroup moonlightPagedPatchLabel Patch.apply fixture,
      env
        (prepareHackagePatchApplyFixture fixture)
        (hackagePatchApplyVariantBenchGroup hackagePatchMapLabel)
    ]

patchApplyVariantBenchGroup ::
  String ->
  PatchApplyImplementation ->
  PatchApplyFixture ->
  Benchmark
patchApplyVariantBenchGroup variant implementation fixture =
  bgroup
    variant
    [ bench "construct" (whnf (patchApplyConstructWith implementation) fixture),
      bench "construct forced" (nf (patchApplyForcedConstructionWeight implementation) fixture),
      env
        (preparePatchApplyOutput variant implementation fixture)
        (\updatedState -> bench "consume" (nf mapIntWeight updatedState))
    ]

{-# NOINLINE patchApplyConstructWith #-}
patchApplyConstructWith ::
  PatchApplyImplementation ->
  PatchApplyFixture ->
  Map Int Int
patchApplyConstructWith implementation fixture =
  case implementation (pafPatch fixture) (pafState fixture) of
    Left err ->
      benchFailure "patch apply construction" err
    Right !updatedState ->
      updatedState

preparePatchApplyOutput ::
  String ->
  PatchApplyImplementation ->
  PatchApplyFixture ->
  IO (Map Int Int)
preparePatchApplyOutput variant implementation fixture =
  case implementation (pafPatch fixture) (pafState fixture) of
    Left err ->
      throwIO (BenchmarkFixtureFailure ("patch apply output: " <> variant) (show err))
    Right !updatedState -> do
      evaluate (rnf updatedState)
      pure updatedState

hackagePatchApplyVariantBenchGroup ::
  String ->
  HackagePatchApplyFixture ->
  Benchmark
hackagePatchApplyVariantBenchGroup variant fixture =
  bgroup
    variant
    [ bench "construct" (whnf hackagePatchApplyConstruct fixture),
      bench "construct forced" (nf hackagePatchApplyForcedConstructionWeight fixture),
      env
        (prepareHackagePatchApplyOutput variant fixture)
        (\updatedState -> bench "consume" (nf mapIntWeight updatedState))
    ]

prepareHackagePatchApplyOutput ::
  String ->
  HackagePatchApplyFixture ->
  IO (Map Int Int)
prepareHackagePatchApplyOutput variant fixture = do
  let !updatedState = hackagePatchApplyConstruct fixture
  evaluate (rnf updatedState)
  variant `seq` pure updatedState

patchApplyOutcomeBenchGroup :: String -> Int -> PatchApplyFixture -> Benchmark
patchApplyOutcomeBenchGroup label size fixture =
  bgroup
    (caseLabel label size)
    [ bench checkedMapMergeLabel (nf patchApplyReferenceOutcomeFixtureWeight fixture),
      bench moonlightSplitApplyLabel (nf patchApplyOutcomeFixtureWeight fixture)
    ]

patchReplayBenchmark ::
  String ->
  Int ->
  PatchReplayFixture ->
  Benchmark
patchReplayBenchmark label size replayFixture =
  env
    (preparePatchReplayFixture replayFixture)
    ( \fixture ->
        bgroup
          (caseLabel label size)
          [ bench checkedSequentialMapMergeLabel (nf patchSequentialReferenceReplayWeight fixture),
            bench (forcedVariantName checkedSequentialMapMergeLabel) (nf patchSequentialReferenceReplayForcedWeight fixture),
            bench moonlightSequentialApplyLabel (nf patchSequentialReplayWeight fixture),
            bench (forcedVariantName moonlightSequentialApplyLabel) (nf patchSequentialReplayForcedWeight fixture),
            bench moonlightUncheckedSequentialPatchLabel (nf patchUncheckedSequentialReplayWeight fixture),
            env
              (prepareHackagePatchReplayFixture fixture)
              (\hackageFixture -> bench hackageSequentialPatchMapLabel (nf hackagePatchReplayWeight hackageFixture)),
            env
              (prepareHackagePatchReplayFixture fixture)
              (\hackageFixture -> bench (forcedVariantName hackageSequentialPatchMapLabel) (nf hackagePatchReplayForcedWeight hackageFixture)),
            bench moonlightFusedReplayLabel (nf patchFusedReplayWeight fixture),
            bench (forcedVariantName moonlightFusedReplayLabel) (nf patchFusedReplayForcedWeight fixture)
          ]
    )

patchStaleReplayBenchmark ::
  String ->
  Int ->
  PatchReplayFixture ->
  Benchmark
patchStaleReplayBenchmark label size replayFixture =
  env
    (preparePatchReplayFixture replayFixture)
    ( \fixture ->
        bgroup
          (caseLabel label size)
          [ bench checkedSequentialMapMergeLabel (nf patchSequentialReferenceReplayOutcomeWeight fixture),
            bench moonlightSequentialApplyLabel (nf patchSequentialReplayOutcomeWeight fixture),
            bench moonlightFusedReplayLabel (nf patchFusedReplayOutcomeWeight fixture)
          ]
    )

patchApplyOutcomeFixtureWeight :: PatchApplyFixture -> Int
patchApplyOutcomeFixtureWeight fixture =
  patchApplyOutcomeWeight (Patch.apply (pafPatch fixture) (pafState fixture))

patchApplyReferenceOutcomeFixtureWeight :: PatchApplyFixture -> Int
patchApplyReferenceOutcomeFixtureWeight fixture =
  patchApplyOutcomeWeight (PatchReference.apply (pafPatch fixture) (pafState fixture))

patchSequentialReplayWeight :: PatchReplayFixture -> Int
patchSequentialReplayWeight fixture =
  case replaySequentially (prfInitialState fixture) (prfPatches fixture) of
    Left err -> benchFailure "patch sequential replay" err
    Right finalState -> mapIntWeight finalState

patchSequentialReplayForcedWeight :: PatchReplayFixture -> Int
patchSequentialReplayForcedWeight fixture =
  case replaySequentially (prfInitialState fixture) (prfPatches fixture) of
    Left err -> benchFailure "patch sequential replay forced" err
    Right finalState -> rnf finalState `seq` 1

patchSequentialReferenceReplayWeight :: PatchReplayFixture -> Int
patchSequentialReferenceReplayWeight fixture =
  case replayReferenceSequentially (prfInitialState fixture) (prfPatches fixture) of
    Left err -> benchFailure "patch sequential reference replay" err
    Right finalState -> mapIntWeight finalState

patchSequentialReferenceReplayForcedWeight :: PatchReplayFixture -> Int
patchSequentialReferenceReplayForcedWeight fixture =
  case replayReferenceSequentially (prfInitialState fixture) (prfPatches fixture) of
    Left err -> benchFailure "patch sequential reference replay forced" err
    Right finalState -> rnf finalState `seq` 1

patchSequentialReplayOutcomeWeight :: PatchReplayFixture -> Int
patchSequentialReplayOutcomeWeight fixture =
  patchReplayOutcomeWeight
    (replaySequentially (prfInitialState fixture) (prfPatches fixture))

patchSequentialReferenceReplayOutcomeWeight :: PatchReplayFixture -> Int
patchSequentialReferenceReplayOutcomeWeight fixture =
  patchReplayOutcomeWeight
    (replayReferenceSequentially (prfInitialState fixture) (prfPatches fixture))

hackagePatchReplayWeight :: HackagePatchReplayFixture -> Int
hackagePatchReplayWeight fixture =
  mapIntWeight
    (hackageReplaySequentially (hprfInitialState fixture) (hprfPatches fixture))

hackagePatchReplayForcedWeight :: HackagePatchReplayFixture -> Int
hackagePatchReplayForcedWeight fixture =
  rnf (hackageReplaySequentially (hprfInitialState fixture) (hprfPatches fixture)) `seq` 1

patchFusedReplayWeight :: PatchReplayFixture -> Int
patchFusedReplayWeight fixture =
  case Patch.replay (prfPatches fixture) (prfInitialState fixture) of
    Left err -> benchFailure "patch fused replay" err
    Right finalState -> mapIntWeight finalState

patchFusedReplayForcedWeight :: PatchReplayFixture -> Int
patchFusedReplayForcedWeight fixture =
  case Patch.replay (prfPatches fixture) (prfInitialState fixture) of
    Left err -> benchFailure "patch fused replay forced" err
    Right finalState -> rnf finalState `seq` 1

patchFusedReplayOutcomeWeight :: PatchReplayFixture -> Int
patchFusedReplayOutcomeWeight fixture =
  patchFusedReplayOutcomeWeightFromResult
    (Patch.replay (prfPatches fixture) (prfInitialState fixture))

patchReplayOutcomeWeight ::
  Either (ApplyError Int Int) (Map Int Int) ->
  Int
patchReplayOutcomeWeight outcome =
  case outcome of
    Left err ->
      patchApplyErrorWeight err
    Right finalState ->
      mapIntWeight finalState

patchApplyOutcomeWeight ::
  Either (ApplyError Int Int) (Map Int Int) ->
  Int
patchApplyOutcomeWeight outcome =
  case outcome of
    Left err ->
      patchApplyErrorWeight err
    Right finalState ->
      mapIntWeight finalState

patchFusedReplayOutcomeWeightFromResult ::
  Either (ReplayError Int Int) (Map Int Int) ->
  Int
patchFusedReplayOutcomeWeightFromResult outcome =
  case outcome of
    Left err ->
      fromIntegral (replayIndex err) + patchApplyErrorWeight (replayApply err)
    Right finalState ->
      mapIntWeight finalState

patchApplyErrorWeight ::
  ApplyError Int Int ->
  Int
patchApplyErrorWeight err =
  mismatchKey err
    + maybeIntWeight (expectedBefore err)
    + maybeIntWeight (actualBefore err)

patchComposeConstructionAllocationWeight ::
  PatchComposeImplementation ->
  PatchComposeFixture ->
  Int
patchComposeConstructionAllocationWeight implementation fixture =
  let !composed = patchComposeConstructWith implementation fixture
   in composed `seq` 1

patchComposeForcedConstructionWeight ::
  PatchComposeImplementation ->
  PatchComposeFixture ->
  Int
patchComposeForcedConstructionWeight implementation fixture =
  let !composed = patchComposeConstructWith implementation fixture
   in rnfPatch composed `seq` 1

patchApplyForcedConstructionWeight ::
  PatchApplyImplementation ->
  PatchApplyFixture ->
  Int
patchApplyForcedConstructionWeight implementation fixture =
  let !updatedState = patchApplyConstructWith implementation fixture
   in rnf updatedState `seq` 1

patchApplyConstructionAllocationWeight ::
  PatchApplyImplementation ->
  PatchApplyFixture ->
  Int
patchApplyConstructionAllocationWeight implementation fixture =
  let !updatedState = patchApplyConstructWith implementation fixture
   in updatedState `seq` 1

hackagePatchComposeConstructionAllocationWeight ::
  HackagePatchComposeFixture ->
  Int
hackagePatchComposeConstructionAllocationWeight fixture =
  let !composed = hackagePatchComposeConstruct fixture
   in composed `seq` 1

hackagePatchComposeForcedConstructionWeight ::
  HackagePatchComposeFixture ->
  Int
hackagePatchComposeForcedConstructionWeight fixture =
  let !composed = hackagePatchComposeConstruct fixture
   in rnfHackagePatchMap composed `seq` 1

hackagePatchApplyForcedConstructionWeight ::
  HackagePatchApplyFixture ->
  Int
hackagePatchApplyForcedConstructionWeight fixture =
  let !updatedState = hackagePatchApplyConstruct fixture
   in rnf updatedState `seq` 1

hackagePatchApplyConstructionAllocationWeight ::
  HackagePatchApplyFixture ->
  Int
hackagePatchApplyConstructionAllocationWeight fixture =
  let !updatedState = hackagePatchApplyConstruct fixture
   in updatedState `seq` 1

forcedVariantName :: String -> String
forcedVariantName variant =
  variant <> ".construct forced"

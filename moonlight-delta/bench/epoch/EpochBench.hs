module EpochBench
  ( epochBenchmarks,
  )
where

import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import BenchSupport
  ( benchFailure,
    caseLabel,
    deltaSizes,
    keys,
    repeatedDeltaKeys,
  )
import Moonlight.Delta.Epoch
  ( ContextProjectionDelta (..),
    EpochDelta,
    Endpoint (..),
    changedKeysAcrossEpoch,
    dirtyBaseDelta,
    dirtyResultDelta,
    epochDelta,
    versionFromKey,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nf,
  )

epochBenchmarks :: Benchmark
epochBenchmarks =
  bgroup
    "epoch"
    (deltaSizes >>= epochBenchmarksForSize)

epochBenchmarksForSize :: Int -> [Benchmark]
epochBenchmarksForSize size =
  [ bench (caseLabel "ContextProjectionDelta union" size) (nf contextProjectionUnionWeight size),
    bench (caseLabel "EpochDelta changedKeys" size) (nf epochChangedKeysWeight size),
    bench (caseLabel "EpochDelta changedKeys production identity transport large universe" size) (nf epochProductionChangedKeysWeight size),
    bench (caseLabel "EpochDelta changedKeys sparse transport large universe" size) (nf epochSparseChangedKeysWeight size)
  ]

contextProjectionUnionWeight :: Int -> Int
contextProjectionUnionWeight size =
  contextProjectionWeight
    ( foldMap
        (\key -> dirtyBaseDelta key <> dirtyResultDelta (key + 1))
        (sampleKeys size)
    )

epochChangedKeysWeight :: Int -> Int
epochChangedKeysWeight size =
  IntSet.size (changedKeysAcrossEpoch (epochDeltaForSize size))

epochProductionChangedKeysWeight :: Int -> Int
epochProductionChangedKeysWeight size =
  IntSet.size (changedKeysAcrossEpoch (epochProductionDeltaForSize size))

epochSparseChangedKeysWeight :: Int -> Int
epochSparseChangedKeysWeight size =
  IntSet.size (changedKeysAcrossEpoch (epochSparseDeltaForSize size))

epochDeltaForSize :: Int -> EpochDelta (IntMap Int) IntSet
epochDeltaForSize size =
  mintEpochBenchmarkDelta
    "epoch changedKeys"
    (IntSet.fromAscList (sampleKeys size))
    ( IntSet.fromAscList
        (fmap (+ size) (sampleKeys size) <> fmap (+ (2 * size)) (sampleKeys size))
    )
    (IntMap.fromAscList [(key, key + size) | key <- sampleKeys size])
    IntSet.empty
    (IntSet.fromAscList (sampleKeys size))

epochProductionDeltaForSize :: Int -> EpochDelta (IntMap Int) IntSet
epochProductionDeltaForSize size =
  mintEpochBenchmarkDelta
    "epoch changedKeys production"
    largeUniverse
    largeUniverse
    IntMap.empty
    IntSet.empty
    (IntSet.fromAscList (sampleKeys size))
  where
    largeUniverse =
      epochLargeUniverse size

epochSparseDeltaForSize :: Int -> EpochDelta (IntMap Int) IntSet
epochSparseDeltaForSize size =
  mintEpochBenchmarkDelta
    "epoch changedKeys sparse"
    sourceUniverse
    targetUniverse
    transport
    IntSet.empty
    (IntSet.fromAscList sparseDomain)
  where
    sourceUniverse =
      epochLargeUniverse size
    sparseDomain =
      repeatedDeltaKeys
    largeSize =
      size * 64
    transport =
      IntMap.fromAscList [(key, largeSize + key) | key <- sparseDomain]
    targetUniverse =
      IntSet.union
        (IntSet.difference sourceUniverse (IntSet.fromAscList sparseDomain))
        (IntSet.fromAscList [largeSize + key | key <- sparseDomain])

mintEpochBenchmarkDelta ::
  String ->
  IntSet ->
  IntSet ->
  IntMap Int ->
  IntSet ->
  IntSet ->
  EpochDelta (IntMap Int) IntSet
mintEpochBenchmarkDelta label sourceKeys targetKeys transport retiredKeys changedKeys =
  case epochDelta source target transport retiredKeys changedKeys of
    Left err ->
      benchFailure label err
    Right deltaValue ->
      deltaValue
  where
    source =
      Endpoint (versionFromKey 1) sourceKeys
    target =
      Endpoint (versionFromKey 2) targetKeys

epochLargeUniverse :: Int -> IntSet
epochLargeUniverse size =
  IntSet.fromAscList (keys (size * 64))

contextProjectionWeight :: ContextProjectionDelta IntSet -> Int
contextProjectionWeight deltaValue =
  IntSet.size (dirtyBaseKeys deltaValue) + IntSet.size (dirtyResultKeys deltaValue)

sampleKeys :: Int -> [Int]
sampleKeys size =
  keys size

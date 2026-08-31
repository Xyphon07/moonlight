{-# LANGUAGE BangPatterns #-}

module CoreBench
  ( coreBenchmarks,
  )
where

import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import BenchSupport
  ( benchFailure,
    caseLabel,
    deltaSizes,
    frontierDominatedSizes,
    frontierSizes,
    keys,
    naturalWeight,
    repeatedDeltaKeys,
  )
import Moonlight.Delta.Frontier
  ( frontierContains,
    frontierPoints,
    mkFrontier,
    mkProductFrontier2,
    productFrontier2Contains,
    productFrontier2Points,
  )
import Moonlight.Delta.Normalize
  ( normalizeDelta,
  )
import Moonlight.Delta.Scope
  ( Scope,
    dirtyScope,
    restrictScope,
    scopeKeys,
    unionScope,
  )
import Moonlight.Delta.Signed
  ( Multiplicity (..),
    SignedApplyError,
    Signed,
    applySignedToMap,
    combineSigned,
    signedFromList,
    signedNull,
    support,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    nf,
    whnf,
  )

coreBenchmarks :: Benchmark
coreBenchmarks =
  bgroup
    "core"
    [ signedDeltaBenchmarks,
      frontierBenchmarks,
      scopeBenchmarks
    ]

signedDeltaBenchmarks :: Benchmark
signedDeltaBenchmarks =
  bgroup
    "signed-delta"
    (deltaSizes >>= signedDeltaBenchmarksForSize)

signedDeltaBenchmarksForSize :: Int -> [Benchmark]
signedDeltaBenchmarksForSize size =
  [ bench (caseLabel "prepared normalize" size) (whnf normalizeDelta preparedDelta),
    bench (caseLabel "prepared null" size) (whnf signedNull preparedDelta),
    bench (caseLabel "prepared support" size) (nf (Set.size . support) preparedDelta),
    bench (caseLabel "fromList/normalize" size) (nf signedBuildWeight size),
    bench (caseLabel "build/combine" size) (nf signedComposeSizeWeight size),
    bench (caseLabel "build/apply to map" size) (nf signedApplySizeWeight size),
    bench (caseLabel "apply first underflow" size) (nf signedFirstUnderflowWeight (signedFirstUnderflowDelta size)),
    bench (caseLabel "repeated sparse apply stream" size) (nf signedRepeatedSparseApplyWeight size)
  ]
  where
    preparedDelta =
      signedDelta size

signedFirstUnderflowDelta :: Int -> Signed Int
signedFirstUnderflowDelta size =
  signedFromList ((0, -1) : fmap (\key -> (key, 1)) [1 .. max 0 (size - 1)])

signedFirstUnderflowWeight :: Signed Int -> Int
signedFirstUnderflowWeight deltaValue =
  case applySignedToMap deltaValue Map.empty of
    Left _underflow -> 1
    Right unexpectedState -> Map.size unexpectedState

signedBuildWeight :: Int -> Int
signedBuildWeight =
  Set.size . support . signedDelta

signedComposeWeight :: (Signed Int, Signed Int) -> Int
signedComposeWeight (newer, older) =
  Set.size (support (combineSigned newer older))

signedComposeSizeWeight :: Int -> Int
signedComposeSizeWeight size =
  signedComposeWeight (signedDelta size, shiftedSigned size)

signedApplySizeWeight :: Int -> Int
signedApplySizeWeight size =
  signedApplyWeight (initialSignedState size, signedDelta size)

signedApplyWeight :: (Map Int Multiplicity, Signed Int) -> Int
signedApplyWeight (stateValue, deltaValue) =
  case applySignedToMap deltaValue stateValue of
    Left err -> benchFailure "signed apply" err
    Right updatedState -> Map.size updatedState

signedRepeatedSparseApplyWeight :: Int -> Int
signedRepeatedSparseApplyWeight size =
  signedRepeatedApplyWeight (repeatedSignedInitialState, repeatedSignedStream size)

signedRepeatedApplyWeight :: (Map Int Multiplicity, [Signed Int]) -> Int
signedRepeatedApplyWeight (initialState, deltas) =
  case foldl' applySignedStreamStep (Right initialState) deltas of
    Left err -> benchFailure "signed repeated apply" err
    Right finalState -> Map.foldl' (\total (Multiplicity value) -> total + naturalWeight value) 0 finalState

applySignedStreamStep :: Either (SignedApplyError Int) (Map Int Multiplicity) -> Signed Int -> Either (SignedApplyError Int) (Map Int Multiplicity)
applySignedStreamStep state deltaValue =
  state >>= applySignedToMap deltaValue

repeatedSignedInitialState :: Map Int Multiplicity
repeatedSignedInitialState =
  Map.fromAscList (fmap (\key -> (key, Multiplicity 1)) repeatedDeltaKeys)

repeatedSignedStream :: Int -> [Signed Int]
repeatedSignedStream size =
  fmap
    (\step ->
       signedFromList
        ( fmap
            (\key -> (key, if even (key + step) then 1 else -1))
            repeatedDeltaKeys
        )
    )
    (keys size)

signedDelta :: Int -> Signed Int
signedDelta =
  signedFromList . signedEntries

shiftedSigned :: Int -> Signed Int
shiftedSigned size =
  signedFromList
    [ (key + 1, signedAmount key)
    | key <- keys size
    ]

signedEntries :: Int -> [(Int, Int)]
signedEntries size =
  [ (key, signedAmount key)
  | key <- keys size
  ]

signedAmount :: Int -> Int
signedAmount key
  | even key = 1
  | otherwise = -1

initialSignedState :: Int -> Map Int Multiplicity
initialSignedState size =
  Map.fromAscList
    [ (key, Multiplicity 1)
    | key <- keys (size + 1)
    ]

frontierBenchmarks :: Benchmark
frontierBenchmarks =
  bgroup
    "frontier-antichain"
    ( (frontierSizes >>= frontierDiagonalBenchmarksForSize)
        <> (frontierDominatedSizes >>= frontierDominatedBenchmarksForSize)
    )

frontierDiagonalBenchmarksForSize :: Int -> [Benchmark]
frontierDiagonalBenchmarksForSize size =
  [ bench (caseLabel "generic build diagonal" size) (nf genericFrontierBuildWeight size),
    bench (caseLabel "product2 build diagonal" size) (nf productFrontierBuildWeight size),
    bench (caseLabel "generic build/contains sweep" size) (nf genericFrontierContainsSizeWeight size),
    bench (caseLabel "product2 build/contains sweep" size) (nf productFrontierContainsSizeWeight size)
  ]

frontierDominatedBenchmarksForSize :: Int -> [Benchmark]
frontierDominatedBenchmarksForSize size =
  [ bench (caseLabel "generic dominated-chain build/contains" size) (nf genericFrontierDominatedContainsWeight size),
    bench (caseLabel "product2 dominated-chain build/contains" size) (nf productFrontierDominatedContainsWeight size)
  ]

genericFrontierBuildWeight :: Int -> Int
genericFrontierBuildWeight =
  length . frontierPoints . mkFrontier . frontierDiagonal

productFrontierBuildWeight :: Int -> Int
productFrontierBuildWeight =
  length . productFrontier2Points . mkProductFrontier2 . frontierDiagonal

genericFrontierContainsSizeWeight :: Int -> Int
genericFrontierContainsSizeWeight size =
  let frontier =
        mkFrontier (frontierDiagonal size)
   in length
        ( filter
            (\point -> frontierContains point frontier)
            (frontierProbePoints size)
        )

productFrontierContainsSizeWeight :: Int -> Int
productFrontierContainsSizeWeight size =
  let frontier =
        mkProductFrontier2 (frontierDiagonal size)
   in length
        ( filter
            (\point -> productFrontier2Contains point frontier)
            (frontierProbePoints size)
        )

genericFrontierDominatedContainsWeight :: Int -> Int
genericFrontierDominatedContainsWeight size =
  let frontier =
        mkFrontier (frontierDominatedChain size)
   in length
        ( filter
            (\point -> frontierContains point frontier)
            (frontierProbePoints size)
        )

productFrontierDominatedContainsWeight :: Int -> Int
productFrontierDominatedContainsWeight size =
  let frontier =
        mkProductFrontier2 (frontierDominatedChain size)
   in length
        ( filter
            (\point -> productFrontier2Contains point frontier)
            (frontierProbePoints size)
        )

frontierDiagonal :: Int -> [(Int, Int)]
frontierDiagonal size =
  [ (key, size - key)
  | key <- keys size
  ]

frontierProbePoints :: Int -> [(Int, Int)]
frontierProbePoints size =
  fmap (\key -> (key, key)) (keys size)

frontierDominatedChain :: Int -> [(Int, Int)]
frontierDominatedChain size =
  fmap (\key -> (key, key)) (keys size)

scopeBenchmarks :: Benchmark
scopeBenchmarks =
  bgroup
    "scope"
    (deltaSizes >>= scopeBenchmarksForSize)

scopeBenchmarksForSize :: Int -> [Benchmark]
scopeBenchmarksForSize size =
  [ bench (caseLabel "Scope union/restrict" size) (nf scopeUnionRestrictWeight size)
  ]

scopeUnionRestrictWeight :: Int -> Int
scopeUnionRestrictWeight size =
  scopeWeight
    ( restrictScope
        (IntSet.fromAscList (sampleKeys size))
        (unionScope (dirtyScope (evenKeySet size)) (dirtyScope (thirdKeySet size)))
    )

scopeWeight :: Scope IntSet -> Int
scopeWeight =
  maybe 0 IntSet.size . scopeKeys

evenKeySet :: Int -> IntSet
evenKeySet size =
  IntSet.fromAscList
    [ key
    | key <- keys size,
      even key
    ]

thirdKeySet :: Int -> IntSet
thirdKeySet size =
  IntSet.fromAscList
    [ key
    | key <- keys size,
      key `mod` 3 == 0
    ]

sampleKeys :: Int -> [Int]
sampleKeys size =
  keys size

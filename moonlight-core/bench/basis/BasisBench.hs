{-# LANGUAGE DerivingStrategies #-}

module BasisBench
  ( basisBenchmarks,
  )
where

import Control.DeepSeq
  ( NFData (..),
  )
import Data.ByteArray.Hash
  ( SipHash (..),
    sipHash,
  )
import Data.ByteString
  ( ByteString,
  )
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Functor.Identity
  ( Identity (..),
    runIdentity,
  )
import Data.List qualified as List
import Data.List.NonEmpty
  ( NonEmpty,
  )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text
  ( Text,
  )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word
  ( Word64,
    Word8,
  )
import Moonlight.Core
  ( adjacentPairs,
    averageOf,
    safeIndex,
  )
import BenchSupport
  ( caseLabel,
    foundationSizes,
    keys,
  )
import Moonlight.Core
  ( FiniteUniverse (..),
    boundedEnumUniverse,
  )
import Moonlight.Core
  ( accumByKey,
    groupByKey,
  )
import Moonlight.Core
  ( StableHashDigest (..),
    SipHashState,
    defaultStableHashKey,
    sipHashFinalize,
    sipHashInit,
    sipHashUpdateWord64Dec,
    sipHashUpdateWord64LE,
    stableHashByteStrings,
    stableHashEncodingChunks,
    stableHashEncodingTextUtf8,
    stableHashEncodingVersion,
    stableHashEncodingWord64Dec,
    sipHashUpdateWord8,
  )
import Moonlight.Core
  ( TotalRegistry,
    lookupTotal,
    mkTotalRegistry,
  )
import Moonlight.Core
  ( collectEither,
  )
import Moonlight.Core
  ( ProofManifestError (..),
    Queue,
    dedupStableOn,
    dequeue,
    duplicateValuesOn,
    duplicatesOrd,
    emptyQueue,
    enqueueAll,
    firstDuplicate,
    invertMapOfSets,
    canonicalTheoremManifestNames,
    parseTheoremManifestNames,
    queueFromList,
    renderTheoremManifestJson,
    scanFoldM,
    scanMap,
    unfoldM,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )
import Prelude

basisBenchmarks :: Benchmark
basisBenchmarks =
  bgroup
    "basis"
    ( (foundationSizes >>= basisBenchmarksForSize)
        <> (proofManifestSizes >>= proofManifestBenchmarksForSize)
        <> [bench (caseLabel "proof manifest canonical names" 8000) (nf proofManifestCanonicalWeight 8000)]
    )

basisBenchmarksForSize :: Int -> [Benchmark]
basisBenchmarksForSize size =
  [ bench (caseLabel "stable hash byte chunks" size) (nf stableHashTokenWeight size),
    env (pure (stableHashChunkCorpus size)) $ \chunks ->
      bench (caseLabel "stable hash prepared byte chunks" size) (nf stableHashPreparedTokenWeight chunks),
    env (pure (stableHashWideChunkCorpus size)) $ \chunks ->
      bench (caseLabel "stable hash prepared wide byte chunks" size) (nf stableHashPreparedTokenWeight chunks),
    bench (caseLabel "stable hash direct decimal word chunks" size) (nf stableHashDecimalWordTokenWeight size),
    bench (caseLabel "stable hash direct raw decimal token bytes" size) (nf stableHashRawDecimalTokenWeight size),
    bench (caseLabel "stable hash direct utf8 text chunks" size) (nf stableHashTextEncodingTokenWeight size),
    bench (caseLabel "stable hash utf8 text byte chunks" size) (nf stableHashTextByteTokenWeight size),
    bench (caseLabel "hackage: memory SipHash framed byte chunks" size) (nf hackageMemorySipHashFramedChunksWeight size),
    env (pure (framedChunkPayload (stableHashChunkCorpus size))) $ \payload ->
      bench (caseLabel "hackage: memory SipHash prepared framed bytes" size) (nf hackageMemorySipHashPreparedPayloadWeight payload),
    env (pure (framedChunkPayload (stableHashWideChunkCorpus size))) $ \payload ->
      bench (caseLabel "hackage: memory SipHash prepared wide framed bytes" size) (nf hackageMemorySipHashPreparedPayloadWeight payload),
    bench (caseLabel "hackage: memory SipHash raw token bytes" size) (nf hackageMemorySipHashWeight size),
    env (pure (rawTokenPayload size)) $ \payload ->
      bench (caseLabel "hackage: memory SipHash prepared raw token bytes" size) (nf hackageMemorySipHashPreparedPayloadWeight payload),
    bench (caseLabel "map accumulation" size) (nf mapAccumWeight size),
    bench (caseLabel "hackage: containers Map.fromListWith group+accum" size) (nf hackageContainersAccumWeight size),
    bench (caseLabel "total registry lookup sweep" size) (nf registryLookupSweepWeight size),
    bench (caseLabel "hackage: containers Map.lookup sweep" size) (nf hackageRegistryLookupSweepWeight size),
    bench (caseLabel "validation collect" size) (nf validationCollectWeight size),
    bench (caseLabel "hackage/base: Either short-circuit traverse" size) (nf hackageEitherTraverseWeight size),
    bench (caseLabel "aggregate adjacent/safe-index" size) (nf aggregateWeight size),
    bench (caseLabel "queue enqueue/drain" size) (nf queueEnqueueDrainWeight size),
    bench (caseLabel "queue from-list/drain" size) (nf queueFromListDrainWeight size),
    bench (caseLabel "dedup stable/duplicate detection" size) (nf dedupWeight size),
    bench (caseLabel "map invert set adjacency" size) (nf mapInvertWeight size),
    bench (caseLabel "scan map/fold/unfold" size) (nf scanWeight size)
  ]

proofManifestBenchmarksForSize :: Int -> [Benchmark]
proofManifestBenchmarksForSize size =
  [ bench (caseLabel "proof manifest canonical names" size) (nf proofManifestCanonicalWeight size),
    bench (caseLabel "proof manifest render/parse" size) (nf proofManifestRoundtripWeight size),
    bench (caseLabel "proof manifest reject invalid payloads" size) (nf proofManifestRejectWeight size)
  ]

proofManifestSizes :: [Int]
proofManifestSizes =
  [32, 128, 512]

proofManifestCanonicalWeight :: Int -> Int
proofManifestCanonicalWeight =
  sum . fmap length . canonicalTheoremManifestNames . theoremNamesForSize

stableHashTokenWeight :: Int -> Int
stableHashTokenWeight size =
  case stableHashByteStrings (stableHashChunkCorpus size) of
    StableHashDigest digest -> fromIntegral digest

stableHashPreparedTokenWeight :: [ByteString] -> Int
stableHashPreparedTokenWeight chunks =
  case stableHashByteStrings chunks of
    StableHashDigest digest -> fromIntegral digest

stableHashTextEncodingTokenWeight :: Int -> Int
stableHashTextEncodingTokenWeight size =
  case stableHashEncodingChunks stableHashEncodingTextUtf8 (stableHashTextCorpus size) of
    StableHashDigest digest -> fromIntegral digest

stableHashDecimalWordTokenWeight :: Int -> Int
stableHashDecimalWordTokenWeight size =
  case stableHashEncodingChunks (stableHashEncodingWord64Dec . fromIntegral) (keys size) of
    StableHashDigest digest -> fromIntegral digest

stableHashRawDecimalTokenWeight :: Int -> Int
stableHashRawDecimalTokenWeight size =
  fromIntegral (sipHashFinalize (foldl' absorbToken seededState (keys size)))
  where
    seededState =
      sipHashUpdateWord64LE (sipHashInit defaultStableHashKey) stableHashEncodingVersion
    absorbToken :: SipHashState -> Int -> SipHashState
    absorbToken state key =
      sipHashUpdateWord8
        (sipHashUpdateWord64Dec state (fromIntegral key))
        0

stableHashTextByteTokenWeight :: Int -> Int
stableHashTextByteTokenWeight size =
  case stableHashByteStrings (fmap TextEncoding.encodeUtf8 (stableHashTextCorpus size)) of
    StableHashDigest digest -> fromIntegral digest

stableHashTextCorpus :: Int -> [Text]
stableHashTextCorpus =
  fmap (Text.pack . show) . keys

stableHashChunkCorpus :: Int -> [ByteString]
stableHashChunkCorpus =
  fmap stableHashChunkToken . keys

stableHashWideChunkCorpus :: Int -> [ByteString]
stableHashWideChunkCorpus =
  fmap stableHashWideChunkToken . keys

stableHashChunkToken :: Int -> ByteString
stableHashChunkToken =
  ByteString.Char8.pack . show

stableHashWideChunkToken :: Int -> ByteString
stableHashWideChunkToken key =
  ByteString.replicate 128 (fromIntegral (key `mod` 251))

hackageMemorySipHashWeight :: Int -> Int
hackageMemorySipHashWeight size =
  case sipHash defaultStableHashKey (rawTokenPayload size) of
    SipHash digest -> fromIntegral digest

hackageMemorySipHashFramedChunksWeight :: Int -> Int
hackageMemorySipHashFramedChunksWeight =
  hackageMemorySipHashPreparedPayloadWeight . framedChunkPayload . stableHashChunkCorpus

hackageMemorySipHashPreparedPayloadWeight :: ByteString -> Int
hackageMemorySipHashPreparedPayloadWeight payload =
  case sipHash defaultStableHashKey payload of
    SipHash digest -> fromIntegral digest

framedChunkPayload :: [ByteString] -> ByteString
framedChunkPayload chunks =
  LazyByteString.toStrict
    ( Builder.toLazyByteString
        ( Builder.word64LE stableHashEncodingVersion
            <> compactWord64Builder (fromIntegral (length chunks))
            <> foldMap framedChunkBuilder chunks
        )
    )

framedChunkBuilder :: ByteString -> Builder.Builder
framedChunkBuilder chunk =
  compactWord64Builder (fromIntegral (ByteString.length chunk))
    <> Builder.byteString chunk

compactWord64Builder :: Word64 -> Builder.Builder
compactWord64Builder wordValue =
  let byteValue =
        fromIntegral (wordValue `rem` 128)
      remainingValue =
        wordValue `quot` 128
   in if remainingValue == 0
        then Builder.word8 byteValue
        else Builder.word8 (byteValue + 128) <> compactWord64Builder remainingValue

rawTokenPayload :: Int -> ByteString
rawTokenPayload =
  LazyByteString.toStrict
    . Builder.toLazyByteString
    . foldMap rawTokenBuilder
    . keys

rawTokenBuilder :: Int -> Builder.Builder
rawTokenBuilder key =
  Builder.intDec key <> Builder.char7 '\0'

mapAccumWeight :: Int -> Int
mapAccumWeight size =
  let grouped = groupByKey (`mod` 17) (keys size)
      accumulated = accumByKey (`mod` 31) SumValue (keys size)
   in Map.size grouped + sumValueMapDigest accumulated

hackageContainersAccumWeight :: Int -> Int
hackageContainersAccumWeight size =
  Map.size grouped + sumValueMapDigest accumulated
  where
    grouped =
      Map.fromListWith
        (++)
        [ (key `mod` 17, [key])
        | key <- keys size
        ]
    accumulated =
      Map.fromListWith
        (<>)
        [ (key `mod` 31, SumValue key)
        | key <- keys size
        ]

newtype SumValue = SumValue {unSumValue :: Int}
  deriving stock (Eq, Show)

instance Semigroup SumValue where
  SumValue left <> SumValue right =
    SumValue (left + right)

newtype BenchRegistryKey = BenchRegistryKey
  { unBenchRegistryKey :: Word8
  }
  deriving stock (Eq, Ord, Show)

instance FiniteUniverse BenchRegistryKey where
  finiteUniverse =
    fmap BenchRegistryKey (boundedEnumUniverse :: NonEmpty Word8)

registryLookupSweepWeight :: Int -> Int
registryLookupSweepWeight size =
  case benchTotalRegistry of
    Left missingKeys -> length missingKeys
    Right registry ->
      sum
        [ lookupTotal registry (registryKeyAt index)
        | index <- keys size
        ]

hackageRegistryLookupSweepWeight :: Int -> Int
hackageRegistryLookupSweepWeight size =
  sum
    [ maybe 0 id (Map.lookup (registryKeyAt index) benchRegistryMap)
    | index <- keys size
    ]

benchTotalRegistry :: Either [BenchRegistryKey] (TotalRegistry BenchRegistryKey Int)
benchTotalRegistry =
  mkTotalRegistry benchRegistryMap

registryKeyAt :: Int -> BenchRegistryKey
registryKeyAt index =
  BenchRegistryKey (fromIntegral (index `mod` 256))

benchRegistryMap :: Map.Map BenchRegistryKey Int
benchRegistryMap =
  Map.fromList (fmap registryEntry [minBound .. maxBound])
  where
    registryEntry :: Word8 -> (BenchRegistryKey, Int)
    registryEntry key =
      (BenchRegistryKey key, fromIntegral key * 31 + 7)

validationCollectWeight :: Int -> Int
validationCollectWeight size =
  case collectEither (fmap validateEven (keys size)) of
    Left rejected -> length rejected
    Right accepted -> length accepted

hackageEitherTraverseWeight :: Int -> Int
hackageEitherTraverseWeight size =
  case traverse validateEven (keys size) of
    Left rejected -> length rejected
    Right accepted -> length accepted

validateEven :: Int -> Either [Int] Int
validateEven value
  | even value = Right value
  | otherwise = Left [value]

aggregateWeight :: Int -> Int
aggregateWeight size =
  let values = keys size
      maybeAverage = averageOf (fmap fromIntegral values)
      maybeIndexed = safeIndex (size `div` 2) values
   in length (adjacentPairs values)
        + maybe 0 round maybeAverage
        + maybe 0 id maybeIndexed

queueEnqueueDrainWeight :: Int -> Int
queueEnqueueDrainWeight size =
  drainQueue (enqueueAll (keys size) emptyQueue)

queueFromListDrainWeight :: Int -> Int
queueFromListDrainWeight =
  drainQueue . queueFromList . keys

drainQueue :: Queue Int -> Int
drainQueue =
  sum . List.unfoldr dequeue

dedupWeight :: Int -> Int
dedupWeight size =
  length (dedupStableOn id payload)
    + length (duplicatesOrd payload)
    + length (duplicateValuesOn id payload)
    + maybe 0 id (firstDuplicate payload)
  where
    payload =
      duplicatePayload size

duplicatePayload :: Int -> [Int]
duplicatePayload size =
  fmap (`mod` bucketCount) (keys size <> keys size)
  where
    bucketCount =
      max 1 (size `div` 4)

mapInvertWeight :: Int -> Int
mapInvertWeight size =
  Map.size inverted + sum (fmap Set.size (Map.elems inverted))
  where
    inverted =
      invertMapOfSets (setAdjacencyMap size)

setAdjacencyMap :: Int -> Map.Map Int (Set.Set Int)
setAdjacencyMap size =
  Map.fromList (fmap adjacencyEntry (keys size))
  where
    bucketCount =
      max 1 (size `div` 8)
    adjacencyEntry key =
      ( key,
        Set.fromList
          [ key `mod` bucketCount,
            (key + 3) `mod` bucketCount,
            (key * 7 + 1) `mod` bucketCount
          ]
      )

scanWeight :: Int -> Int
scanWeight size =
  finalState + sum mappedValues + foldedValue + sum unfoldedValues
  where
    (finalState, mappedValues) =
      scanMap scanStep 0 (keys size)
    foldedValue =
      runIdentity (scanFoldM scanFoldStep 0 (keys size))
    unfoldedValues =
      runIdentity (unfoldM unfoldIdentityStep size)

scanStep :: Int -> Int -> (Int, Int)
scanStep state value =
  let nextState = state + value
   in (nextState, nextState `mod` 97)

scanFoldStep :: Int -> Int -> Identity Int
scanFoldStep state value =
  Identity (state + value `mod` 31)

unfoldIdentityStep :: Int -> Identity (Maybe (Int, Int))
unfoldIdentityStep remaining
  | remaining <= 0 = Identity Nothing
  | otherwise = Identity (Just (remaining, remaining - 1))

data ProofManifestBenchMeasure
  = ProofManifestBenchMeasured !Int
  | ProofManifestBenchParseFailed !ProofManifestError
  | ProofManifestBenchUnexpectedAccept ![String]
  deriving stock (Eq, Show)

instance NFData ProofManifestBenchMeasure where
  rnf measure =
    case measure of
      ProofManifestBenchMeasured weightValue ->
        rnf weightValue
      ProofManifestBenchParseFailed failure ->
        rnfProofManifestError failure
      ProofManifestBenchUnexpectedAccept accepted ->
        rnf accepted

proofManifestRoundtripWeight :: Int -> ProofManifestBenchMeasure
proofManifestRoundtripWeight size =
  case parseTheoremManifestNames rendered of
    Left failure ->
      ProofManifestBenchParseFailed failure
    Right parsedNames ->
      ProofManifestBenchMeasured
        ( length rendered
            + length parsedNames
            + codecAgreementWeight theoremNames parsedNames
        )
  where
    theoremNames =
      theoremNamesForSize size
    rendered =
      renderTheoremManifestJson theoremNames

proofManifestRejectWeight :: Int -> ProofManifestBenchMeasure
proofManifestRejectWeight size =
  either
    ProofManifestBenchUnexpectedAccept
    (ProofManifestBenchMeasured . sum)
    (traverse invalidManifestPayloadWeight (invalidManifestPayloads size))

invalidManifestPayloadWeight :: String -> Either [String] Int
invalidManifestPayloadWeight payload =
  case parseTheoremManifestNames payload of
    Left failure ->
      Right (proofManifestErrorWeight failure)
    Right accepted ->
      Left accepted

codecAgreementWeight :: [String] -> [String] -> Int
codecAgreementWeight required actual =
  if canonicalTheoremManifestNames required == actual
    then 1
    else 0

theoremNamesForSize :: Int -> [String]
theoremNamesForSize size =
  fmap theoremNameForKey (keys size)

theoremNameForKey :: Int -> String
theoremNameForKey key =
  "Moonlight.Core.Generated.Theorem."
    <> show key
    <> ".escaped\\quote\""

invalidManifestPayloads :: Int -> [String]
invalidManifestPayloads size =
  [ "{\"theorems\":[alpha]}",
    "{\"laws\":[\"alpha\"]}",
    "{\"theorems\":[\"\"]}",
    "{\"theorems\":[\" alpha\"]}",
    "{\"theorems\":[\"alpha\",\"alpha\"]}",
    "{\"theorems\":[\""
      <> theoremNameForKey size
      <> "\n\"]}",
    "{\"theorems\":[\"\\uD800\"]}"
  ]

proofManifestErrorWeight :: ProofManifestError -> Int
proofManifestErrorWeight failure =
  case failure of
    ProofManifestParseFailure ->
      1
    EmptyTheoremManifestName ->
      2
    WhitespacePaddedTheoremManifestName theoremName ->
      3 + length theoremName
    DuplicateTheoremManifestName theoremName ->
      5 + length theoremName

rnfProofManifestError :: ProofManifestError -> ()
rnfProofManifestError failure =
  case failure of
    ProofManifestParseFailure ->
      ()
    EmptyTheoremManifestName ->
      ()
    WhitespacePaddedTheoremManifestName theoremName ->
      rnf theoremName
    DuplicateTheoremManifestName theoremName ->
      rnf theoremName

sumValueMapDigest :: Map.Map Int SumValue -> Int
sumValueMapDigest =
  Map.foldlWithKey'
    (\digest key (SumValue value) -> digest * 16777619 + key * 31 + value)
    146959810

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Moonlight.Delta.Patch.Internal.MultisetDeltaHash
  ( MultisetDeltaHash,
    buildMultisetDeltaHash,
    multisetDeltaHashState,
    multisetDeltaHashDigest,
    applyMultisetDeltaHash,
    composeMultisetDeltaHash,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.Algebra.Pure.LaneVector
  ( LaneVector,
    laneCount,
    laneVectorFromLanes,
    laneVectorLanes,
    laneVectorZero,
  )
import Moonlight.Core
  ( AdditiveGroup (sub),
    AdditiveMonoid (add),
    SipKey (..),
    StableHashDigest (..),
    StableHashEncoding,
    stableHashEncodingDigest,
    stableHashEncodingLength,
    stableHashEncodingWord8,
    stableHashEncodingWord64LE,
  )
import Moonlight.Delta.Patch.Internal.Apply qualified as Patch
import Moonlight.Delta.Patch.Internal.Construction qualified as Patch
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashApplyError (..),
    DeltaHashComposeError,
    DeltaHashEncoderVersion,
    DigestStrategy (..),
    MultisetDeltaHashDigest,
    IncrementalDigest (..),
    composeDeltaHashPatches,
    deltaHashEncodingVersion,
    rawDigestEncoding,
    sealMultisetDigest,
  )
import Moonlight.Delta.Patch.Internal.Types (Patch, PatchKey, PatchValue)
import Prelude

data MultisetDeltaHash key value = MultisetDeltaHash
  { multisetDeltaHashEncoderVersion :: !DeltaHashEncoderVersion,
    multisetDeltaHashKeyEncoding :: !(key -> StableHashEncoding),
    multisetDeltaHashValueEncoding :: !(value -> StableHashEncoding),
    multisetDeltaHashAuthoritativeState :: !(Map key value),
    multisetDeltaHashAccumulator :: !LaneVector
  }

buildMultisetDeltaHash ::
  DeltaHashEncoderVersion ->
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  MultisetDeltaHash key value
buildMultisetDeltaHash encoderVersion encodeKey encodeValue authoritativeState =
  MultisetDeltaHash
    { multisetDeltaHashEncoderVersion = encoderVersion,
      multisetDeltaHashKeyEncoding = encodeKey,
      multisetDeltaHashValueEncoding = encodeValue,
      multisetDeltaHashAuthoritativeState = authoritativeState,
      multisetDeltaHashAccumulator =
        Map.foldlWithKey'
          (addEncodedEntry encodeKey encodeValue)
          laneVectorZero
          authoritativeState
    }

multisetDeltaHashState :: MultisetDeltaHash key value -> Map key value
multisetDeltaHashState =
  multisetDeltaHashAuthoritativeState

-- | The homomorphic multiset commitment to the caller-encoded __current map
-- state__, compressed from a 16-lane accumulator in @(Z / 2^64 Z)^16@. Like the
-- Merkle digest it is a state commitment, not a history commitment, and not a
-- provenance record or MAC (the SipHash keys are fixed and public). Because the
-- accumulator is exactly additive, a non-injective value encoder collapses
-- distinct logical states to one digest: a replacement that subtracts and
-- re-adds an identical lane vector leaves the digest unchanged. Multiplicity is
-- modulo @2^64@ in every lane, so @2^64@ distinct keys with identical encodings
-- also contribute zero. The 1024-bit accumulator is compressed to 128 bits by
-- two fixed-key SipHash evaluations. Under the unproved ideal-independent-lane
-- assumption, generic collision work is about @2^64@ and preimage or second-
-- preimage work about @2^128@. The caller-supplied encoder version identifies
-- the encoding protocol; it does not prove encoder injectivity. The result is
-- strategy-indexed, so it cannot be compared with a 'MerkleDeltaHashDigest'.
multisetDeltaHashDigest :: MultisetDeltaHash key value -> MultisetDeltaHashDigest
multisetDeltaHashDigest deltaHashValue =
  digestLaneVector
    (multisetDeltaHashEncoderVersion deltaHashValue)
    (multisetDeltaHashAccumulator deltaHashValue)

applyMultisetDeltaHash ::
  (Ord key, Eq value) =>
  Patch key value ->
  MultisetDeltaHash key value ->
  Either (DeltaHashApplyError key value) (MultisetDeltaHash key value)
applyMultisetDeltaHash patchValue currentMultisetDeltaHash = do
  updatedState <-
    either
      (Left . DeltaHashPatchRejected)
      Right
      (Patch.apply patchValue (multisetDeltaHashAuthoritativeState currentMultisetDeltaHash))
  let !encodeKey = multisetDeltaHashKeyEncoding currentMultisetDeltaHash
      !encodeValue = multisetDeltaHashValueEncoding currentMultisetDeltaHash
      !authoritativeState = multisetDeltaHashAuthoritativeState currentMultisetDeltaHash
      !updatedAccumulator =
        Patch.foldWithKey'
          const
          (addEncodedEntry encodeKey encodeValue)
          (\accumulator key before ->
             let (storedKey, storedValue) =
                   storedEntryRepresentative authoritativeState key before
              in subtractEncodedEntry encodeKey encodeValue accumulator storedKey storedValue
          )
          (\accumulator key before after ->
             let (storedKey, storedValue) =
                   storedEntryRepresentative authoritativeState key before
              in addEncodedEntry
                   encodeKey
                   encodeValue
                   (subtractEncodedEntry encodeKey encodeValue accumulator storedKey storedValue)
                   key
                   after
          )
          (multisetDeltaHashAccumulator currentMultisetDeltaHash)
          patchValue
  pure
    MultisetDeltaHash
      { multisetDeltaHashEncoderVersion = multisetDeltaHashEncoderVersion currentMultisetDeltaHash,
        multisetDeltaHashKeyEncoding = encodeKey,
        multisetDeltaHashValueEncoding = encodeValue,
        multisetDeltaHashAuthoritativeState = updatedState,
        multisetDeltaHashAccumulator = updatedAccumulator
      }

-- | Contextual composition costs @O(|state|)@ regardless of patch size; fold
-- many small patches first and apply once rather than composing pairwise.
composeMultisetDeltaHash ::
  (PatchKey key, PatchValue value) =>
  Patch key value ->
  Patch key value ->
  MultisetDeltaHash key value ->
  Either (DeltaHashComposeError key value) (Patch key value)
composeMultisetDeltaHash =
  composeDeltaHashPatches

addEncodedEntry ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  LaneVector ->
  key ->
  value ->
  LaneVector
addEncodedEntry encodeKey encodeValue accumulator key value =
  add accumulator (expand (encodeKey key) (encodeValue value))

subtractEncodedEntry ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  LaneVector ->
  key ->
  value ->
  LaneVector
subtractEncodedEntry encodeKey encodeValue accumulator key value =
  sub accumulator (expand (encodeKey key) (encodeValue value))

expand :: StableHashEncoding -> StableHashEncoding -> LaneVector
expand keyEncoding valueEncoding =
  laneVectorFromLanes
    ( UVector.generate
        laneCount
        (\laneIndex ->
           stableHashDigestWord
             multisetExpansionSipKey
             (multisetElementEncoding laneIndex keyEncoding valueEncoding)
        )
    )

multisetElementEncoding ::
  Int ->
  StableHashEncoding ->
  StableHashEncoding ->
  StableHashEncoding
multisetElementEncoding laneIndex keyEncoding valueEncoding =
  stableHashEncodingWord8 3
    <> stableHashEncodingWord64LE deltaHashEncodingVersion
    <> stableHashEncodingWord64LE (fromIntegral (stableHashEncodingLength keyEncoding))
    <> keyEncoding
    <> stableHashEncodingWord64LE (fromIntegral (stableHashEncodingLength valueEncoding))
    <> valueEncoding
    <> stableHashEncodingWord64LE (fromIntegral laneIndex)

digestLaneVector :: DeltaHashEncoderVersion -> LaneVector -> MultisetDeltaHashDigest
digestLaneVector encoderVersion laneVector =
  sealMultisetDigest encoderVersion
    ( rawDigestEncoding
        multisetDigestSipKeyLane0
        multisetDigestSipKeyLane1
        ( stableHashEncodingWord8 4
            <> stableHashEncodingWord64LE deltaHashEncodingVersion
            <> stableHashEncodingWord64LE (fromIntegral laneCount)
            <> UVector.foldl'
              (\encoding lane -> encoding <> stableHashEncodingWord64LE lane)
              mempty
              (laneVectorLanes laneVector)
        )
    )

storedEntryRepresentative :: Ord key => Map key value -> key -> value -> (key, value)
storedEntryRepresentative authoritativeState patchKey patchValue =
  case Map.lookupGE patchKey authoritativeState of
    Just storedEntry@(storedKey, _storedValue)
      | compare storedKey patchKey == EQ -> storedEntry
    _ -> (patchKey, patchValue)

stableHashDigestWord :: SipKey -> StableHashEncoding -> Word64
stableHashDigestWord sipKey encoding =
  case stableHashEncodingDigest sipKey encoding of
    StableHashDigest digestWord -> digestWord

multisetExpansionSipKey :: SipKey
multisetExpansionSipKey =
  SipKey 0x6d6c2d64656c7461 0x6d756c74692d7632

multisetDigestSipKeyLane0 :: SipKey
multisetDigestSipKeyLane0 =
  SipKey 0x6d6c2d64656c7461 0x6d7365742d763031

multisetDigestSipKeyLane1 :: SipKey
multisetDigestSipKeyLane1 =
  SipKey 0x6d6c2d64656c7461 0x6d7365742d763032

instance IncrementalDigest MultisetDeltaHash where
  type IncrementalDigestError MultisetDeltaHash key value = DeltaHashApplyError key value
  type IncrementalDigestStrategy MultisetDeltaHash = 'MultisetDigestStrategy
  empty encoderVersion encodeKey encodeValue = buildMultisetDeltaHash encoderVersion encodeKey encodeValue Map.empty
  applyPatch = applyMultisetDeltaHash
  incrementalDigestState = multisetDeltaHashState
  digest = multisetDeltaHashDigest

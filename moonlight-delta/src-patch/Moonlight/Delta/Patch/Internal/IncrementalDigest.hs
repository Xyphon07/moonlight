{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashStrategy (..),
    DigestStrategy (..),
    KnownDigestStrategy (..),
    DeltaHashEncoderVersion (..),
    DeltaHashDigest (..),
    DeltaHashDigestIdentity (..),
    DeltaHashDigestLanes (..),
    MerkleDeltaHashDigest,
    MultisetDeltaHashDigest,
    deltaHashDigestIdentity,
    deltaHashDigestLanes,
    DeltaHashDigestDecodeError (..),
    encodeDeltaHashDigest,
    decodeMerkleDeltaHashDigest,
    decodeMultisetDeltaHashDigest,
    RawDigest128 (..),
    DeltaHashBuildError (..),
    DeltaHashApplyError (..),
    DeltaHashComposeError (..),
    IncrementalDigest (..),
    composeDeltaHashPatches,
    deltaHashEncodingVersion,
    sealMerkleDigest,
    sealMultisetDigest,
    rawDigestEncoding,
    rawDigestEncodingChunks,
  )
where

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Data.Bits (Bits (shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Core
  ( SipHashState,
    SipKey,
    StableHashDigest (..),
    StableHashEncoding,
    stableHashEncodingDigest,
    stableHashEncodingLength,
    stableHashUpdateEncoding,
    sipHashFinalize,
    sipHashInit,
    sipHashUpdateWord8,
    sipHashUpdateWord64LE,
  )
import Moonlight.Delta.Patch.Internal.Cell (cellFromEndpoints)
import Moonlight.Delta.Patch.Internal.Construction qualified as PatchConstruction
import Moonlight.Delta.Patch.Internal.Types (ApplyError, Patch, PatchKey, PatchValue)
import Prelude

-- | The closed runtime vocabulary of digest strategies. A projected digest
-- stores exactly one of these so that a persisted value identifies the scheme
-- that produced it; verification under the wrong scheme is then a visible tag
-- mismatch rather than a silent false negative.
type DeltaHashStrategy :: Type
data DeltaHashStrategy
  = MerkleDeltaHashStrategy
  | MultisetDeltaHashStrategy
  deriving stock (Eq, Ord, Show, Read, Generic)

-- | The type-level counterpart of 'DeltaHashStrategy'. It indexes
-- 'DeltaHashDigest' so that comparing a Merkle digest with a multiset digest is
-- a type error, not a runtime coincidence.
type DigestStrategy :: Type
data DigestStrategy
  = MerkleDigestStrategy
  | MultisetDigestStrategy

-- | Recover the runtime strategy tag from its type-level index. This is the one
-- bridge from phantom to value, so the stored tag can never drift from the
-- phantom that a projection carries.
type KnownDigestStrategy :: DigestStrategy -> Constraint
class KnownDigestStrategy (strategy :: DigestStrategy) where
  digestStrategyValue :: proxy strategy -> DeltaHashStrategy

instance KnownDigestStrategy 'MerkleDigestStrategy where
  digestStrategyValue _proxy = MerkleDeltaHashStrategy

instance KnownDigestStrategy 'MultisetDigestStrategy where
  digestStrategyValue _proxy = MultisetDeltaHashStrategy

type DeltaHashEncoderVersion :: Type
newtype DeltaHashEncoderVersion = DeltaHashEncoderVersion Word64
  deriving stock (Eq, Ord, Show, Read, Generic)

-- | Runtime interpretation metadata for a complete digest. Its constructor is
-- internal; public values come only from sealed digests or typed decode errors.
type DeltaHashDigestIdentity :: Type
data DeltaHashDigestIdentity
  = DeltaHashDigestIdentity
      !DeltaHashStrategy
      {-# UNPACK #-} !Word64
      !DeltaHashEncoderVersion
  deriving stock (Eq, Ord, Show)

-- | The raw commitment words under a nominal strategy index. The public facade
-- exports only this type name: construction and projection remain internal, so
-- persisted consumers use 'encodeDeltaHashDigest' instead of raw words.
type DeltaHashDigestLanes :: DigestStrategy -> Type
type role DeltaHashDigestLanes nominal
data DeltaHashDigestLanes (strategy :: DigestStrategy)
  = DeltaHashDigestLanes
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
  deriving stock (Eq, Ord, Show)

-- | A self-identifying 128-bit commitment. The phantom index makes cross-scheme
-- comparison ill-typed; the stored strategy tag, protocol version, and caller-
-- supplied encoder version make a persisted digest self-describing. The public
-- facade hides the constructor: a projection may only be minted through
-- 'sealMerkleDigest' or 'sealMultisetDigest', which stamp the matching identity
-- by construction.
type DeltaHashDigest :: DigestStrategy -> Type
type role DeltaHashDigest nominal
data DeltaHashDigest (strategy :: DigestStrategy) = DeltaHashDigest
  !DeltaHashDigestIdentity
  !(DeltaHashDigestLanes strategy)
  deriving stock (Eq, Ord, Show)

deltaHashDigestIdentity :: DeltaHashDigest strategy -> DeltaHashDigestIdentity
deltaHashDigestIdentity (DeltaHashDigest identity _lanes) =
  identity

deltaHashDigestLanes :: DeltaHashDigest strategy -> DeltaHashDigestLanes strategy
deltaHashDigestLanes (DeltaHashDigest _identity lanes) =
  lanes

type MerkleDeltaHashDigest :: Type
type MerkleDeltaHashDigest = DeltaHashDigest 'MerkleDigestStrategy

type MultisetDeltaHashDigest :: Type
type MultisetDeltaHashDigest = DeltaHashDigest 'MultisetDigestStrategy

-- | A typed refusal to reinterpret persisted digest bytes under the wrong
-- strategy, protocol version, or caller-declared encoder version.
data DeltaHashDigestDecodeError
  = DeltaHashDigestMalformed !String
  | DeltaHashDigestTrailingBytes {-# UNPACK #-} !Word64
  | DeltaHashDigestUnknownStrategyTag {-# UNPACK #-} !Word64
  | DeltaHashDigestIdentityMismatch
      !DeltaHashDigestIdentity
      !DeltaHashDigestIdentity
  deriving stock (Eq, Ord, Show)

-- | Canonical CBOR serialization of the complete digest identity and both
-- commitment lanes. The five fields are, in order, strategy, DeltaHash
-- protocol version, caller encoder version, lane zero, and lane one.
encodeDeltaHashDigest :: DeltaHashDigest strategy -> ByteString
encodeDeltaHashDigest
  ( DeltaHashDigest
      (DeltaHashDigestIdentity strategy protocolVersion (DeltaHashEncoderVersion encoderVersion))
      (DeltaHashDigestLanes lane0 lane1)
    ) =
    CBOR.toStrictByteString
      ( CBOR.encodeListLen 5
          <> CBOR.encodeWord64 (deltaHashStrategyTag strategy)
          <> CBOR.encodeWord64 protocolVersion
          <> CBOR.encodeWord64 encoderVersion
          <> CBOR.encodeWord64 lane0
          <> CBOR.encodeWord64 lane1
      )

decodeMerkleDeltaHashDigest ::
  DeltaHashEncoderVersion ->
  ByteString ->
  Either DeltaHashDigestDecodeError MerkleDeltaHashDigest
decodeMerkleDeltaHashDigest =
  decodeDeltaHashDigestFor (Proxy :: Proxy 'MerkleDigestStrategy)

decodeMultisetDeltaHashDigest ::
  DeltaHashEncoderVersion ->
  ByteString ->
  Either DeltaHashDigestDecodeError MultisetDeltaHashDigest
decodeMultisetDeltaHashDigest =
  decodeDeltaHashDigestFor (Proxy :: Proxy 'MultisetDigestStrategy)

decodeDeltaHashDigestFor ::
  KnownDigestStrategy strategy =>
  proxy strategy ->
  DeltaHashEncoderVersion ->
  ByteString ->
  Either DeltaHashDigestDecodeError (DeltaHashDigest strategy)
decodeDeltaHashDigestFor strategyProxy expectedEncoderVersion bytes = do
  (strategyTag, protocolVersion, encodedEncoderVersion, lane0, lane1) <-
    decodeDeltaHashDigestFields bytes
  encodedStrategy <-
    deltaHashStrategyFromTag strategyTag
  let expectedIdentity =
        deltaHashIdentityFor strategyProxy expectedEncoderVersion
      encodedIdentity =
        DeltaHashDigestIdentity
          encodedStrategy
          protocolVersion
          (DeltaHashEncoderVersion encodedEncoderVersion)
  if encodedIdentity == expectedIdentity
    then
      Right
        ( DeltaHashDigest
            encodedIdentity
            (DeltaHashDigestLanes lane0 lane1)
        )
    else
      Left
        ( DeltaHashDigestIdentityMismatch
            expectedIdentity
            encodedIdentity
        )

decodeDeltaHashDigestFields ::
  ByteString ->
  Either DeltaHashDigestDecodeError (Word64, Word64, Word64, Word64, Word64)
decodeDeltaHashDigestFields bytes =
  case CBOR.deserialiseFromBytes decodeFields (LazyByteString.fromStrict bytes) of
    Left failure ->
      Left (DeltaHashDigestMalformed (show failure))
    Right (trailingBytes, fields)
      | LazyByteString.null trailingBytes -> Right fields
      | otherwise ->
          Left
            ( DeltaHashDigestTrailingBytes
                (fromIntegral (LazyByteString.length trailingBytes))
            )
  where
    decodeFields :: CBOR.Decoder s (Word64, Word64, Word64, Word64, Word64)
    decodeFields = do
      fieldCount <- CBOR.decodeListLenCanonical
      if fieldCount == 5
        then
          (,,,,)
            <$> CBOR.decodeWord64Canonical
            <*> CBOR.decodeWord64Canonical
            <*> CBOR.decodeWord64Canonical
            <*> CBOR.decodeWord64Canonical
            <*> CBOR.decodeWord64Canonical
        else
          fail
            ( "expected DeltaHash digest field count 5, received "
                <> show fieldCount
            )

deltaHashStrategyTag :: DeltaHashStrategy -> Word64
deltaHashStrategyTag strategy =
  case strategy of
    MerkleDeltaHashStrategy -> 0
    MultisetDeltaHashStrategy -> 1

deltaHashStrategyFromTag ::
  Word64 ->
  Either DeltaHashDigestDecodeError DeltaHashStrategy
deltaHashStrategyFromTag strategyTag =
  case strategyTag of
    0 -> Right MerkleDeltaHashStrategy
    1 -> Right MultisetDeltaHashStrategy
    _ -> Left (DeltaHashDigestUnknownStrategyTag strategyTag)

-- | The raw two-lane commitment used to compose Merkle nodes and to compress the
-- multiset accumulator. It carries no identity because it is never compared or
-- persisted directly; identity is stamped only when a raw value is sealed into a
-- projection.
type RawDigest128 :: Type
data RawDigest128 = RawDigest128
  { rawDigestLane0 :: {-# UNPACK #-} !Word64,
    rawDigestLane1 :: {-# UNPACK #-} !Word64
  }
  deriving stock (Eq, Ord, Show)

data DeltaHashBuildError key = DeltaHashKeyCollision
  { deltaHashCollisionDigest :: !StableHashDigest,
    deltaHashExistingKey :: !key,
    deltaHashIncomingKey :: !key
  }
  deriving stock (Eq, Ord, Show)

data DeltaHashApplyError key value
  = DeltaHashPatchRejected !(ApplyError key value)
  | DeltaHashUpdateRejected !(DeltaHashBuildError key)
  deriving stock (Eq, Ord, Show)

-- | Failure from one binary contextual composition. 'DeltaHashOlderPatchRejected'
-- and 'DeltaHashNewerPatchRejected' identify the failing immediate operand of
-- that binary call only. Successful glued transitions associate, and failure
-- definedness plus the enclosed 'DeltaHashApplyError' associate, but this
-- binary-local stage tag is deliberately excluded because reassociation changes
-- which immediate operand surfaces the same sequential obstruction.
data DeltaHashComposeError key value
  = DeltaHashOlderPatchRejected !(DeltaHashApplyError key value)
  | DeltaHashNewerPatchRejected !(DeltaHashApplyError key value)
  deriving stock (Eq, Ord, Show)

class IncrementalDigest (derived :: Type -> Type -> Type) where
  type IncrementalDigestError derived key value :: Type
  type IncrementalDigestStrategy derived :: DigestStrategy
  empty :: DeltaHashEncoderVersion -> (key -> StableHashEncoding) -> (value -> StableHashEncoding) -> derived key value
  applyPatch ::
    (Ord key, Eq value) =>
    Patch key value ->
    derived key value ->
    Either (IncrementalDigestError derived key value) (derived key value)
  incrementalDigestState :: derived key value -> Map key value
  digest :: derived key value -> DeltaHashDigest (IncrementalDigestStrategy derived)

-- | Contextually compose two patches by descending through the authoritative
-- intermediate state and gluing the exact initial-to-final transition.
--
-- Successful results associate as exact glued transitions. Failure definedness
-- and the underlying 'DeltaHashApplyError' also associate. The 'Older'/'Newer'
-- refinement in 'DeltaHashComposeError' is not associative: it names the
-- failing immediate operand of the binary call that surfaced the obstruction.
-- Cost is linear in the final state because the glued transition is constructed
-- from the authoritative initial and final maps.
composeDeltaHashPatches ::
  ( PatchKey key,
    PatchValue value,
    IncrementalDigest derived,
    IncrementalDigestError derived key value ~ DeltaHashApplyError key value
  ) =>
  Patch key value ->
  Patch key value ->
  derived key value ->
  Either (DeltaHashComposeError key value) (Patch key value)
composeDeltaHashPatches newer older initial = do
  intermediate <-
    either
      (Left . DeltaHashOlderPatchRejected)
      Right
      (applyPatch older initial)
  final <-
    either
      (Left . DeltaHashNewerPatchRejected)
      Right
      (applyPatch newer intermediate)
  pure
    ( exactStateTransition
        (incrementalDigestState initial)
        (incrementalDigestState final)
    )

exactStateTransition ::
  (PatchKey key, PatchValue value) =>
  Map key value ->
  Map key value ->
  Patch key value
exactStateTransition before after =
  PatchConstruction.fromAscList
    ( fmap
        (\(key, (beforeValue, afterValue)) -> (key, cellFromEndpoints beforeValue afterValue))
        ( Map.toAscList
            ( Map.unionWith
                combineEndpoints
                (Map.map (\afterValue -> (Nothing, Just afterValue)) after)
                (Map.map (\beforeValue -> (Just beforeValue, Nothing)) before)
            )
        )
    )

combineEndpoints ::
  (Maybe value, Maybe value) ->
  (Maybe value, Maybe value) ->
  (Maybe value, Maybe value)
combineEndpoints (leftBefore, leftAfter) (rightBefore, rightAfter) =
  (firstPresent leftBefore rightBefore, firstPresent leftAfter rightAfter)

firstPresent :: Maybe value -> Maybe value -> Maybe value
firstPresent left right =
  case left of
    Just value -> Just value
    Nothing -> right

-- | The DeltaHash protocol encoding version. Every DeltaHash node — empty, leaf,
-- branch, flat framing, and multiset framing — is bound to this single constant,
-- so the whole seam shares one protocol authority. Bumping it is a deliberate
-- wire-format break; 0.1.0.1 raises it to 3 to admit the true 128-bit leaf.
deltaHashEncodingVersion :: Word64
deltaHashEncodingVersion = 3

sealDigest ::
  forall strategy.
  KnownDigestStrategy strategy =>
  DeltaHashEncoderVersion ->
  RawDigest128 ->
  DeltaHashDigest strategy
sealDigest encoderVersion (RawDigest128 lane0 lane1) =
  DeltaHashDigest
    (deltaHashIdentityFor (Proxy :: Proxy strategy) encoderVersion)
    (DeltaHashDigestLanes lane0 lane1)

deltaHashIdentityFor ::
  KnownDigestStrategy strategy =>
  proxy strategy ->
  DeltaHashEncoderVersion ->
  DeltaHashDigestIdentity
deltaHashIdentityFor strategyProxy =
  DeltaHashDigestIdentity
    (digestStrategyValue strategyProxy)
    deltaHashEncodingVersion

sealMerkleDigest :: DeltaHashEncoderVersion -> RawDigest128 -> MerkleDeltaHashDigest
sealMerkleDigest = sealDigest

sealMultisetDigest :: DeltaHashEncoderVersion -> RawDigest128 -> MultisetDeltaHashDigest
sealMultisetDigest = sealDigest

rawDigestEncoding :: SipKey -> SipKey -> StableHashEncoding -> RawDigest128
rawDigestEncoding lane0Key lane1Key encoding =
  RawDigest128 (stableHashDigestWord lane0Key encoding) (stableHashDigestWord lane1Key encoding)

rawDigestEncodingChunks ::
  Foldable values =>
  SipKey ->
  SipKey ->
  (value -> StableHashEncoding) ->
  values value ->
  RawDigest128
rawDigestEncodingChunks lane0Key lane1Key project values =
  finalizeRawDigestState (Foldable.foldl' absorbEncoding initialState values)
  where
    !chunkCount = fromIntegral (Foldable.length values)
    !initialState =
      RawDigestHashState
        (framedChunksSeed lane0Key chunkCount)
        (framedChunksSeed lane1Key chunkCount)
    absorbEncoding (RawDigestHashState lane0 lane1) value =
      let !encoding = project value
          !encodedLength = fromIntegral (stableHashEncodingLength encoding)
       in RawDigestHashState
            (stableHashUpdateEncoding (sipHashUpdateCompactWord64 lane0 encodedLength) encoding)
            (stableHashUpdateEncoding (sipHashUpdateCompactWord64 lane1 encodedLength) encoding)

data RawDigestHashState = RawDigestHashState !SipHashState !SipHashState

finalizeRawDigestState :: RawDigestHashState -> RawDigest128
finalizeRawDigestState (RawDigestHashState lane0 lane1) =
  RawDigest128 (sipHashFinalize lane0) (sipHashFinalize lane1)

stableHashDigestWord :: SipKey -> StableHashEncoding -> Word64
stableHashDigestWord sipKey encoding =
  case stableHashEncodingDigest sipKey encoding of
    StableHashDigest digestWord -> digestWord

framedChunksSeed :: SipKey -> Word64 -> SipHashState
framedChunksSeed sipKey =
  sipHashUpdateCompactWord64
    (sipHashUpdateWord64LE (sipHashInit sipKey) deltaHashEncodingVersion)

sipHashUpdateCompactWord64 :: SipHashState -> Word64 -> SipHashState
sipHashUpdateCompactWord64 state wordValue =
  let !byteValue = fromIntegral (wordValue .&. 0x7f)
      !remainingValue = wordValue `shiftR` 7
      !updatedState =
        sipHashUpdateWord8 state (if remainingValue == 0 then byteValue else byteValue .|. 0x80)
   in if remainingValue == 0
        then updatedState
        else sipHashUpdateCompactWord64 updatedState remainingValue

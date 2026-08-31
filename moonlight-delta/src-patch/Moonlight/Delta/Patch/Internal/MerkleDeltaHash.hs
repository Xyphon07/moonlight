{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Moonlight.Delta.Patch.Internal.MerkleDeltaHash
  ( MerkleDeltaHash,
    buildMerkleDeltaHash,
    merkleDeltaHashState,
    merkleDeltaHashDigest,
    applyMerkleDeltaHash,
    composeMerkleDeltaHash,
    deltaHashFlatMaximumSize,
  )
where

import Data.Bits
  ( Bits (complement, shiftL, xor, (.&.), (.|.)),
    FiniteBits (countLeadingZeros, finiteBitSize),
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import Moonlight.Core
  ( SipKey (..),
    StableHashDigest (..),
    StableHashEncoding,
    defaultStableHashKey,
  )
import Moonlight.Delta.Patch.Internal.Apply qualified as Patch
import Moonlight.Delta.Patch.Internal.Construction qualified as Patch
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashApplyError (..),
    DeltaHashBuildError (..),
    DeltaHashComposeError,
    DeltaHashEncoderVersion,
    DigestStrategy (..),
    MerkleDeltaHashDigest,
    RawDigest128,
    IncrementalDigest (..),
    composeDeltaHashPatches,
    rawDigestEncodingChunks,
    sealMerkleDigest,
  )
import Moonlight.Delta.Patch.Internal.NodeCommitment qualified as Node
import Moonlight.Delta.Patch.Internal.Types (Patch, PatchKey, PatchValue)
import Prelude

data MerkleDeltaHash key value = MerkleDeltaHash
  { merkleDeltaHashEncoderVersion :: !DeltaHashEncoderVersion,
    merkleDeltaHashKeyEncoding :: !(key -> StableHashEncoding),
    merkleDeltaHashValueEncoding :: !(value -> StableHashEncoding),
    merkleDeltaHashAuthoritativeState :: !(Map key value),
    merkleDeltaHashDerivedView :: !(MerkleDeltaHashView key)
  }

data MerkleDeltaHashView key
  = FlatStableHash !RawDigest128
  | IncrementalMerkleHash !(MerkleTrie key)

data MerkleTrie key
  = MerkleEmpty
  | MerkleLeaf {-# UNPACK #-} !Word64 !key !RawDigest128
  | MerkleBranch
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      !(MerkleTrie key)
      !(MerkleTrie key)
      !RawDigest128

buildMerkleDeltaHash ::
  Eq key =>
  DeltaHashEncoderVersion ->
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Either (DeltaHashBuildError key) (MerkleDeltaHash key value)
buildMerkleDeltaHash encoderVersion encodeKey encodeValue authoritativeState =
  fmap
    (MerkleDeltaHash encoderVersion encodeKey encodeValue authoritativeState)
    (buildMerkleDeltaHashView encodeKey encodeValue authoritativeState)

merkleDeltaHashState :: MerkleDeltaHash key value -> Map key value
merkleDeltaHashState =
  merkleDeltaHashAuthoritativeState

-- | The Merkle commitment to the caller-encoded __current map state__. It is a
-- state commitment, not a history commitment: two different edit histories that
-- reach the same encoded map deliberately produce the same digest. It is not a
-- provenance record or a MAC — the SipHash keys are fixed and public — and its
-- collision resistance is only as strong as the caller's key and value
-- encoders. The result is strategy-indexed, so it cannot be compared with a
-- 'MultisetDeltaHashDigest'.
merkleDeltaHashDigest :: MerkleDeltaHash key value -> MerkleDeltaHashDigest
merkleDeltaHashDigest deltaHashValue =
  sealMerkleDigest
    (merkleDeltaHashEncoderVersion deltaHashValue)
    (merkleDeltaHashViewDigest (merkleDeltaHashDerivedView deltaHashValue))

applyMerkleDeltaHash ::
  (Ord key, Eq value) =>
  Patch key value ->
  MerkleDeltaHash key value ->
  Either (DeltaHashApplyError key value) (MerkleDeltaHash key value)
applyMerkleDeltaHash patchValue currentMerkleDeltaHash = do
  updatedState <-
    either
      (Left . DeltaHashPatchRejected)
      Right
      (Patch.apply patchValue (merkleDeltaHashAuthoritativeState currentMerkleDeltaHash))
  updatedView <-
    either
      (Left . DeltaHashUpdateRejected)
      Right
      ( updateMerkleDeltaHashView
          (merkleDeltaHashKeyEncoding currentMerkleDeltaHash)
          (merkleDeltaHashValueEncoding currentMerkleDeltaHash)
          (merkleDeltaHashAuthoritativeState currentMerkleDeltaHash)
          patchValue
          updatedState
          (merkleDeltaHashDerivedView currentMerkleDeltaHash)
      )
  pure
    MerkleDeltaHash
      { merkleDeltaHashEncoderVersion = merkleDeltaHashEncoderVersion currentMerkleDeltaHash,
        merkleDeltaHashKeyEncoding = merkleDeltaHashKeyEncoding currentMerkleDeltaHash,
        merkleDeltaHashValueEncoding = merkleDeltaHashValueEncoding currentMerkleDeltaHash,
        merkleDeltaHashAuthoritativeState = updatedState,
        merkleDeltaHashDerivedView = updatedView
      }

-- | Contextual composition costs @O(|state|)@ regardless of patch size; fold
-- many small patches first and apply once rather than composing pairwise.
composeMerkleDeltaHash ::
  (PatchKey key, PatchValue value) =>
  Patch key value ->
  Patch key value ->
  MerkleDeltaHash key value ->
  Either (DeltaHashComposeError key value) (Patch key value)
composeMerkleDeltaHash =
  composeDeltaHashPatches

buildMerkleDeltaHashView ::
  Eq key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Either (DeltaHashBuildError key) (MerkleDeltaHashView key)
buildMerkleDeltaHashView encodeKey encodeValue authoritativeState
  | usesFlatStableHash authoritativeState =
      Right (FlatStableHash (flatStateDigest encodeKey encodeValue authoritativeState))
  | otherwise =
      fmap IncrementalMerkleHash (buildMerkleTrie encodeKey encodeValue authoritativeState)

updateMerkleDeltaHashView ::
  Ord key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Patch key value ->
  Map key value ->
  MerkleDeltaHashView key ->
  Either (DeltaHashBuildError key) (MerkleDeltaHashView key)
updateMerkleDeltaHashView encodeKey encodeValue authoritativeState patchValue updatedState currentView
  | usesFlatStableHash updatedState =
      Right (FlatStableHash (flatStateDigest encodeKey encodeValue updatedState))
  | otherwise =
      case currentView of
        FlatStableHash _digest ->
          fmap IncrementalMerkleHash (buildMerkleTrie encodeKey encodeValue updatedState)
        IncrementalMerkleHash trie ->
          fmap IncrementalMerkleHash (applyPatchToTrie encodeKey encodeValue authoritativeState patchValue trie)

merkleDeltaHashViewDigest :: MerkleDeltaHashView key -> RawDigest128
merkleDeltaHashViewDigest view =
  case view of
    FlatStableHash digestValue -> digestValue
    IncrementalMerkleHash trie -> merkleTrieDigest trie

usesFlatStableHash :: Map key value -> Bool
usesFlatStableHash authoritativeState =
  Map.size authoritativeState <= deltaHashFlatMaximumSize

flatStateDigest ::
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  RawDigest128
flatStateDigest encodeKey encodeValue authoritativeState =
  rawDigestEncodingChunks
    defaultStableHashKey
    flatSipKeyLane1
    (either encodeKey encodeValue)
    (flatStateEncodings authoritativeState)

flatStateEncodings :: Map key value -> [Either key value]
flatStateEncodings =
  Map.foldrWithKey (\key value encodings -> Left key : Right value : encodings) []

buildMerkleTrie ::
  Eq key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
buildMerkleTrie encodeKey encodeValue =
  Map.foldlWithKey' insertEntry (Right MerkleEmpty)
  where
    insertEntry accumulated key value =
      accumulated >>= insertEncodedValue encodeKey encodeValue key value

applyPatchToTrie ::
  Ord key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  Map key value ->
  Patch key value ->
  MerkleTrie key ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
applyPatchToTrie encodeKey encodeValue authoritativeState patchValue initialTrie =
  Patch.foldWithKey'
    const
    (\accumulated key after -> accumulated >>= insertEncodedValue encodeKey encodeValue key after)
    (\accumulated _key _before -> accumulated)
    (\accumulated key _before after -> accumulated >>= insertEncodedValue encodeKey encodeValue key after)
    (Right (retirePatchPaths encodeKey authoritativeState patchValue initialTrie))
    patchValue

insertEncodedValue ::
  Eq key =>
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  key ->
  value ->
  MerkleTrie key ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
insertEncodedValue encodeKey encodeValue key value =
  let !keyEncoding = encodeKey key
      !path = Node.localizePath Node.sipHashNodeCommitment keyEncoding
      !leafCommitment =
        Node.leafDigest Node.sipHashNodeCommitment path keyEncoding (encodeValue value)
   in insertMerkleLeaf path key leafCommitment

deleteEncodedKey ::
  Eq key =>
  (key -> StableHashEncoding) ->
  key ->
  MerkleTrie key ->
  MerkleTrie key
deleteEncodedKey encodeKey key =
  deleteMerkleLeaf (stableHashPathWord (encodeKey key)) key

retirePatchPaths ::
  Ord key =>
  (key -> StableHashEncoding) ->
  Map key value ->
  Patch key value ->
  MerkleTrie key ->
  MerkleTrie key
retirePatchPaths encodeKey authoritativeState patchValue initialTrie =
  Patch.foldWithKey'
    const
    (\trie _key _after -> trie)
    (\trie key _before -> deleteAuthoritativeKey encodeKey authoritativeState key trie)
    (\trie key _before _after -> deleteMovedAuthoritativeKey encodeKey authoritativeState key trie)
    initialTrie
    patchValue

deleteAuthoritativeKey ::
  Ord key =>
  (key -> StableHashEncoding) ->
  Map key value ->
  key ->
  MerkleTrie key ->
  MerkleTrie key
deleteAuthoritativeKey encodeKey authoritativeState patchKey trie =
  case Map.lookupGE patchKey authoritativeState of
    Just (storedKey, _value)
      | compare storedKey patchKey == EQ -> deleteEncodedKey encodeKey storedKey trie
    _ -> trie

deleteMovedAuthoritativeKey ::
  Ord key =>
  (key -> StableHashEncoding) ->
  Map key value ->
  key ->
  MerkleTrie key ->
  MerkleTrie key
deleteMovedAuthoritativeKey encodeKey authoritativeState patchKey trie =
  case Map.lookupGE patchKey authoritativeState of
    Just (storedKey, _value)
      | compare storedKey patchKey == EQ ->
          let !storedPath = stableHashPathWord (encodeKey storedKey)
              !patchPath = stableHashPathWord (encodeKey patchKey)
           in if storedPath == patchPath
                then trie
                else deleteMerkleLeaf storedPath storedKey trie
    _ -> trie

insertMerkleLeaf ::
  Eq key =>
  Word64 ->
  key ->
  RawDigest128 ->
  MerkleTrie key ->
  Either (DeltaHashBuildError key) (MerkleTrie key)
insertMerkleLeaf path key leafCommitment trie =
  case trie of
    MerkleEmpty ->
      Right (merkleLeaf path key leafCommitment)
    MerkleLeaf existingPath existingKey _existingDigest
      | path == existingPath ->
          if key == existingKey
            then Right (merkleLeaf path key leafCommitment)
            else
              Left
                DeltaHashKeyCollision
                  { deltaHashCollisionDigest = StableHashDigest path,
                    deltaHashExistingKey = existingKey,
                    deltaHashIncomingKey = key
                  }
      | otherwise ->
          Right (linkMerkleTries path (merkleLeaf path key leafCommitment) existingPath trie)
    branch@(MerkleBranch prefix branchingBit left right _branchDigest)
      | not (matchesPrefix path prefix branchingBit) ->
          Right (linkMerkleTries path (merkleLeaf path key leafCommitment) prefix branch)
      | zeroAtBit path branchingBit ->
          fmap
            (\updatedLeft -> merkleBranch prefix branchingBit updatedLeft right)
            (insertMerkleLeaf path key leafCommitment left)
      | otherwise ->
          fmap
            (\updatedRight -> merkleBranch prefix branchingBit left updatedRight)
            (insertMerkleLeaf path key leafCommitment right)

deleteMerkleLeaf ::
  Eq key =>
  Word64 ->
  key ->
  MerkleTrie key ->
  MerkleTrie key
deleteMerkleLeaf path key trie =
  case trie of
    MerkleEmpty -> MerkleEmpty
    MerkleLeaf existingPath existingKey _digest ->
      if path == existingPath && key == existingKey then MerkleEmpty else trie
    MerkleBranch prefix branchingBit left right _branchDigest
      | not (matchesPrefix path prefix branchingBit) -> trie
      | zeroAtBit path branchingBit ->
          compactMerkleBranch prefix branchingBit (deleteMerkleLeaf path key left) right
      | otherwise ->
          compactMerkleBranch prefix branchingBit left (deleteMerkleLeaf path key right)

linkMerkleTries ::
  Word64 ->
  MerkleTrie key ->
  Word64 ->
  MerkleTrie key ->
  MerkleTrie key
linkMerkleTries leftPath leftTrie rightPath rightTrie =
  let !branchingBit = highestDifferingBit leftPath rightPath
      !prefix = maskPrefix leftPath branchingBit
   in if zeroAtBit leftPath branchingBit
        then merkleBranch prefix branchingBit leftTrie rightTrie
        else merkleBranch prefix branchingBit rightTrie leftTrie

compactMerkleBranch ::
  Word64 ->
  Word64 ->
  MerkleTrie key ->
  MerkleTrie key ->
  MerkleTrie key
compactMerkleBranch prefix branchingBit left right =
  case (left, right) of
    (MerkleEmpty, remaining) -> remaining
    (remaining, MerkleEmpty) -> remaining
    _ -> merkleBranch prefix branchingBit left right

merkleLeaf :: Word64 -> key -> RawDigest128 -> MerkleTrie key
merkleLeaf path key leafCommitment =
  MerkleLeaf path key leafCommitment

merkleBranch ::
  Word64 ->
  Word64 ->
  MerkleTrie key ->
  MerkleTrie key ->
  MerkleTrie key
merkleBranch prefix branchingBit left right =
  MerkleBranch
    prefix
    branchingBit
    left
    right
    (Node.branchDigest Node.sipHashNodeCommitment prefix branchingBit (merkleTrieDigest left) (merkleTrieDigest right))

merkleTrieDigest :: MerkleTrie key -> RawDigest128
merkleTrieDigest trie =
  case trie of
    MerkleEmpty -> Node.emptyDigest Node.sipHashNodeCommitment
    MerkleLeaf _path _key digestValue -> digestValue
    MerkleBranch _prefix _branchingBit _left _right digestValue -> digestValue

stableHashPathWord :: StableHashEncoding -> Word64
stableHashPathWord =
  Node.localizePath Node.sipHashNodeCommitment


deltaHashFlatMaximumSize :: Int
deltaHashFlatMaximumSize =
  256

flatSipKeyLane1 :: SipKey
flatSipKeyLane1 =
  SipKey 0x6d6f6f6e6c696768 0x742d636f72652d32

matchesPrefix :: Word64 -> Word64 -> Word64 -> Bool
matchesPrefix path prefix branchingBit =
  maskPrefix path branchingBit == prefix

maskPrefix :: Word64 -> Word64 -> Word64
maskPrefix path branchingBit =
  path .&. complement (branchingBit .|. (branchingBit - 1))

zeroAtBit :: Word64 -> Word64 -> Bool
zeroAtBit path branchingBit =
  path .&. branchingBit == 0

highestDifferingBit :: Word64 -> Word64 -> Word64
highestDifferingBit leftPath rightPath =
  let !differentBits = leftPath `xor` rightPath
      !highestIndex = finiteBitSize differentBits - countLeadingZeros differentBits - 1
   in 1 `shiftL` highestIndex

emptyMerkleDeltaHash ::
  DeltaHashEncoderVersion ->
  (key -> StableHashEncoding) ->
  (value -> StableHashEncoding) ->
  MerkleDeltaHash key value
emptyMerkleDeltaHash encoderVersion encodeKey encodeValue =
  MerkleDeltaHash encoderVersion encodeKey encodeValue Map.empty (FlatStableHash (flatStateDigest encodeKey encodeValue Map.empty))

instance IncrementalDigest MerkleDeltaHash where
  type IncrementalDigestError MerkleDeltaHash key value = DeltaHashApplyError key value
  type IncrementalDigestStrategy MerkleDeltaHash = 'MerkleDigestStrategy
  empty = emptyMerkleDeltaHash
  applyPatch = applyMerkleDeltaHash
  incrementalDigestState = merkleDeltaHashState
  digest = merkleDeltaHashDigest

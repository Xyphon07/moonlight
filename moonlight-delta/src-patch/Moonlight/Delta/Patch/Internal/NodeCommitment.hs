module Moonlight.Delta.Patch.Internal.NodeCommitment
  ( DigestSipKeys (..),
    NodeCommitment (..),
    SipHashNodeCommitment,
    sipHashNodeCommitment,
  )
where

import Data.Word (Word64)
import Moonlight.Core
  ( SipKey (..),
    StableHashDigest (..),
    StableHashEncoding,
    stableHashEncodingDigest,
    stableHashEncodingLength,
    stableHashEncodingWord8,
    stableHashEncodingWord64LE,
  )
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( RawDigest128 (..),
    deltaHashEncodingVersion,
    rawDigestEncoding,
  )
import Prelude

data DigestSipKeys = DigestSipKeys !SipKey !SipKey

class NodeCommitment commitment where
  pathSipKey :: commitment -> SipKey
  emptySipKey :: commitment -> DigestSipKeys
  leafSipKey :: commitment -> DigestSipKeys
  branchSipKey :: commitment -> DigestSipKeys

  -- | Deterministically localize a key encoding to a 64-bit Patricia route.
  -- This word only chooses trie branches and detects co-resident collisions; it
  -- is no longer the key commitment. The leaf binds the complete key encoding.
  localizePath :: commitment -> StableHashEncoding -> Word64
  localizePath commitment =
    stableHashDigestWord (pathSipKey commitment)

  emptyDigest :: commitment -> RawDigest128
  emptyDigest commitment =
    digestWithKeys
      (emptySipKey commitment)
      ( stableHashEncodingWord8 0
          <> stableHashEncodingWord64LE deltaHashEncodingVersion
      )

  -- | Commit a leaf. Both digest lanes absorb the node tag, protocol version,
  -- routing path, and the length-framed complete key and value encodings, so a
  -- 64-bit path or value collision no longer yields equal leaves.
  leafDigest :: commitment -> Word64 -> StableHashEncoding -> StableHashEncoding -> RawDigest128
  leafDigest commitment path keyEncoding valueEncoding =
    digestWithKeys
      (leafSipKey commitment)
      ( stableHashEncodingWord8 1
          <> stableHashEncodingWord64LE deltaHashEncodingVersion
          <> stableHashEncodingWord64LE path
          <> stableHashEncodingWord64LE (fromIntegral (stableHashEncodingLength keyEncoding))
          <> keyEncoding
          <> stableHashEncodingWord64LE (fromIntegral (stableHashEncodingLength valueEncoding))
          <> valueEncoding
      )

  branchDigest :: commitment -> Word64 -> Word64 -> RawDigest128 -> RawDigest128 -> RawDigest128
  branchDigest commitment prefix branchingBit leftDigest rightDigest =
    digestWithKeys
      (branchSipKey commitment)
      ( stableHashEncodingWord8 2
          <> stableHashEncodingWord64LE deltaHashEncodingVersion
          <> stableHashEncodingWord64LE prefix
          <> stableHashEncodingWord64LE branchingBit
          <> stableHashEncodingWord64LE (rawDigestLane0 leftDigest)
          <> stableHashEncodingWord64LE (rawDigestLane1 leftDigest)
          <> stableHashEncodingWord64LE (rawDigestLane0 rightDigest)
          <> stableHashEncodingWord64LE (rawDigestLane1 rightDigest)
      )

data SipHashNodeCommitment = SipHashNodeCommitment

sipHashNodeCommitment :: SipHashNodeCommitment
sipHashNodeCommitment = SipHashNodeCommitment

instance NodeCommitment SipHashNodeCommitment where
  pathSipKey _commitment =
    SipKey 0x6d6c2d64656c7461 0x706174682d763031

  emptySipKey _commitment =
    DigestSipKeys
      (SipKey 0x6d6c2d64656c7461 0x656d70742d763031)
      (SipKey 0x6d6c2d64656c7461 0x656d70742d763032)

  leafSipKey _commitment =
    DigestSipKeys
      (SipKey 0x6d6c2d64656c7461 0x6c6561662d763031)
      (SipKey 0x6d6c2d64656c7461 0x6c6561662d763032)

  branchSipKey _commitment =
    DigestSipKeys
      (SipKey 0x6d6c2d64656c7461 0x6272616e2d763031)
      (SipKey 0x6d6c2d64656c7461 0x6272616e2d763032)

digestWithKeys :: DigestSipKeys -> StableHashEncoding -> RawDigest128
digestWithKeys (DigestSipKeys lane0Key lane1Key) =
  rawDigestEncoding lane0Key lane1Key

stableHashDigestWord :: SipKey -> StableHashEncoding -> Word64
stableHashDigestWord sipKey encoding =
  case stableHashEncodingDigest sipKey encoding of
    StableHashDigest digestWord -> digestWord

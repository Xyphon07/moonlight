{-# LANGUAGE DerivingStrategies #-}

module DeltaHashGoldenOracle
  ( OracleDigest (..),
    oracleEmptyNodeDigest,
    oracleFlatDigest,
    oracleMultisetDigest,
    oracleSipHashReferenceVectors,
    oracleTrieDigest,
  )
where

import Control.Monad (foldM)
import Data.Bits
  ( Bits (complement, rotateL, shiftL, shiftR, xor, (.&.), (.|.)),
    FiniteBits (countLeadingZeros, finiteBitSize),
  )
import Data.List (unfoldr)
import Data.Word (Word8, Word64)
import Prelude

data OracleDigest = OracleDigest !Word64 !Word64
  deriving stock (Eq, Ord, Show)

data SipState = SipState !Word64 !Word64 !Word64 !Word64

data OracleTrie
  = OracleEmpty
  | OracleLeaf !Word64 !OracleDigest
  | OracleBranch !Word64 !Word64 !OracleTrie !OracleTrie !OracleDigest

oracleFlatDigest :: OracleDigest
oracleFlatDigest =
  digestWithKeys
    defaultLane0Key
    defaultLane1Key
    (word64LE deltaHashProtocolVersion <> compactWord64 4 <> foldMap frameChunk [word64LE 1, word64LE 10, word64LE 2, word64LE 20])

oracleEmptyNodeDigest :: OracleDigest
oracleEmptyNodeDigest =
  digestWithKeys
    emptyLane0Key
    emptyLane1Key
    (0 : word64LE deltaHashProtocolVersion)

oracleTrieDigest :: Maybe OracleDigest
oracleTrieDigest =
  fmap trieDigest
    (foldM insertEntry OracleEmpty [(key, key) | key <- [0 .. 256]])
  where
    insertEntry trie (key, value) =
      insertLeaf
        (sipHash pathKey (word64LE key))
        (leafDigest key value)
        trie

oracleMultisetDigest :: OracleDigest
oracleMultisetDigest =
  digestWithKeys
    multisetLane0Key
    multisetLane1Key
    ( 4
        : word64LE deltaHashProtocolVersion
          <> word64LE 16
          <> foldMap word64LE accumulatedLanes
    )
  where
    accumulatedLanes =
      foldl'
        (zipWith (+))
        (replicate 16 0)
        [expandedEntry 1 10, expandedEntry 2 20]

oracleSipHashReferenceVectors :: [Word64]
oracleSipHashReferenceVectors =
  fmap
    ( \messageLength ->
        sipHash
          (0x0706050403020100, 0x0f0e0d0c0b0a0908)
          (take messageLength [0 ..])
    )
    [0 .. 63]

expandedEntry :: Word64 -> Word64 -> [Word64]
expandedEntry key value =
  fmap
    ( \laneIndex ->
        sipHash
          multisetExpansionKey
          ( 3
              : word64LE deltaHashProtocolVersion
                <> word64LE 8
                <> word64LE key
                <> word64LE 8
                <> word64LE value
                <> word64LE laneIndex
          )
    )
    [0 .. 15]

leafDigest :: Word64 -> Word64 -> OracleDigest
leafDigest key value =
  digestWithKeys
    leafLane0Key
    leafLane1Key
    ( 1
        : word64LE deltaHashProtocolVersion
          <> word64LE path
          <> word64LE 8
          <> word64LE key
          <> word64LE 8
          <> word64LE value
    )
  where
    path = sipHash pathKey (word64LE key)

branchDigest :: Word64 -> Word64 -> OracleTrie -> OracleTrie -> OracleDigest
branchDigest prefix branchingBit left right =
  digestWithKeys
    branchLane0Key
    branchLane1Key
    ( 2
        : word64LE deltaHashProtocolVersion
          <> word64LE prefix
          <> word64LE branchingBit
          <> digestBytes (trieDigest left)
          <> digestBytes (trieDigest right)
    )

insertLeaf :: Word64 -> OracleDigest -> OracleTrie -> Maybe OracleTrie
insertLeaf path digestValue trie =
  case trie of
    OracleEmpty ->
      Just (OracleLeaf path digestValue)
    OracleLeaf existingPath _existingDigest
      | path == existingPath ->
          Nothing
      | otherwise ->
          linkTries path (OracleLeaf path digestValue) existingPath trie
    OracleBranch prefix branchingBit left right _branchDigest
      | maskPrefix path branchingBit /= prefix ->
          linkTries path (OracleLeaf path digestValue) prefix trie
      | zeroAtBit path branchingBit ->
          fmap (\updatedLeft -> oracleBranch prefix branchingBit updatedLeft right) (insertLeaf path digestValue left)
      | otherwise ->
          fmap (oracleBranch prefix branchingBit left) (insertLeaf path digestValue right)

linkTries :: Word64 -> OracleTrie -> Word64 -> OracleTrie -> Maybe OracleTrie
linkTries leftPath leftTrie rightPath rightTrie = do
  branchingBit <- highestDifferingBit leftPath rightPath
  let prefix = maskPrefix leftPath branchingBit
  pure
    ( if zeroAtBit leftPath branchingBit
        then oracleBranch prefix branchingBit leftTrie rightTrie
        else oracleBranch prefix branchingBit rightTrie leftTrie
    )

oracleBranch :: Word64 -> Word64 -> OracleTrie -> OracleTrie -> OracleTrie
oracleBranch prefix branchingBit left right =
  OracleBranch prefix branchingBit left right (branchDigest prefix branchingBit left right)

trieDigest :: OracleTrie -> OracleDigest
trieDigest trie =
  case trie of
    OracleEmpty -> oracleEmptyNodeDigest
    OracleLeaf _path digestValue -> digestValue
    OracleBranch _prefix _branchingBit _left _right digestValue -> digestValue

highestDifferingBit :: Word64 -> Word64 -> Maybe Word64
highestDifferingBit leftPath rightPath =
  let differentBits = leftPath `xor` rightPath
   in if differentBits == 0
        then Nothing
        else
          Just
            ( 1
                `shiftL` (finiteBitSize differentBits - countLeadingZeros differentBits - 1)
            )

maskPrefix :: Word64 -> Word64 -> Word64
maskPrefix path branchingBit =
  path .&. complement (branchingBit .|. (branchingBit - 1))

zeroAtBit :: Word64 -> Word64 -> Bool
zeroAtBit path branchingBit =
  path .&. branchingBit == 0

digestWithKeys :: (Word64, Word64) -> (Word64, Word64) -> [Word8] -> OracleDigest
digestWithKeys lane0Key lane1Key bytes =
  OracleDigest (sipHash lane0Key bytes) (sipHash lane1Key bytes)

digestBytes :: OracleDigest -> [Word8]
digestBytes (OracleDigest lane0 lane1) =
  word64LE lane0 <> word64LE lane1

frameChunk :: [Word8] -> [Word8]
frameChunk bytes =
  compactWord64 (fromIntegral (length bytes)) <> bytes

word64LE :: Word64 -> [Word8]
word64LE word =
  fmap (fromIntegral . (word `shiftR`) . (* 8)) [0 .. 7]

packWord64 :: [Word8] -> Word64
packWord64 bytes =
  foldl'
    (.|.)
    0
    (zipWith (\shift byte -> fromIntegral byte `shiftL` shift) [0, 8 ..] bytes)

compactWord64 :: Word64 -> [Word8]
compactWord64 =
  unfoldr
    ( \maybeWord ->
        case maybeWord of
          Nothing -> Nothing
          Just word ->
            let byte = fromIntegral (word .&. 0x7f)
                remaining = word `shiftR` 7
             in Just
                  ( if remaining == 0 then byte else byte .|. 0x80,
                    if remaining == 0 then Nothing else Just remaining
                  )
    )
    . Just

sipHash :: (Word64, Word64) -> [Word8] -> Word64
sipHash (key0, key1) bytes =
  finalizeSipState
    (applySipRounds 4 (xorSecondStateWord 0xff absorbedFinalBlock))
  where
    initialState =
      SipState
        (key0 `xor` 0x736f6d6570736575)
        (key1 `xor` 0x646f72616e646f6d)
        (key0 `xor` 0x6c7967656e657261)
        (key1 `xor` 0x7465646279746573)
    chunks =
      unfoldr
        ( \remaining ->
            case remaining of
              [] -> Nothing
              _ -> Just (splitAt 8 remaining)
        )
        bytes
    fullBlocks = filter ((== 8) . length) chunks
    trailingBytes = foldMap (\chunk -> if length chunk < 8 then chunk else []) chunks
    absorbedBlocks = foldl' absorbBlock initialState (fmap packWord64 fullBlocks)
    finalBlock =
      (fromIntegral (length bytes) `shiftL` 56)
        .|. packWord64 trailingBytes
    absorbedFinalBlock = absorbBlock absorbedBlocks finalBlock

absorbBlock :: SipState -> Word64 -> SipState
absorbBlock (SipState v0 v1 v2 v3) block =
  case applySipRounds 2 (SipState v0 v1 v2 (v3 `xor` block)) of
    SipState r0 r1 r2 r3 -> SipState (r0 `xor` block) r1 r2 r3

applySipRounds :: Int -> SipState -> SipState
applySipRounds count initialState =
  foldl' (\state _round -> sipRound state) initialState [1 .. count]

sipRound :: SipState -> SipState
sipRound (SipState v0 v1 v2 v3) =
  SipState i0 i1 i2 i3
  where
    a0 = v0 + v1
    a1 = rotateL v1 13 `xor` a0
    a2 = rotateL a0 32
    b2 = v2 + v3
    b3 = rotateL v3 16 `xor` b2
    c0 = a2 + b3
    c3 = rotateL b3 21 `xor` c0
    d2 = b2 + a1
    d1 = rotateL a1 17 `xor` d2
    i0 = c0
    i1 = d1
    i2 = rotateL d2 32
    i3 = c3

xorSecondStateWord :: Word64 -> SipState -> SipState
xorSecondStateWord word (SipState v0 v1 v2 v3) =
  SipState v0 v1 (v2 `xor` word) v3

finalizeSipState :: SipState -> Word64
finalizeSipState (SipState v0 v1 v2 v3) =
  v0 `xor` v1 `xor` v2 `xor` v3

deltaHashProtocolVersion :: Word64
deltaHashProtocolVersion = 3

defaultLane0Key :: (Word64, Word64)
defaultLane0Key = (0x6d6f6f6e6c696768, 0x742d636f72652d31)

defaultLane1Key :: (Word64, Word64)
defaultLane1Key = (0x6d6f6f6e6c696768, 0x742d636f72652d32)

pathKey :: (Word64, Word64)
pathKey = (0x6d6c2d64656c7461, 0x706174682d763031)

emptyLane0Key :: (Word64, Word64)
emptyLane0Key = (0x6d6c2d64656c7461, 0x656d70742d763031)

emptyLane1Key :: (Word64, Word64)
emptyLane1Key = (0x6d6c2d64656c7461, 0x656d70742d763032)

leafLane0Key :: (Word64, Word64)
leafLane0Key = (0x6d6c2d64656c7461, 0x6c6561662d763031)

leafLane1Key :: (Word64, Word64)
leafLane1Key = (0x6d6c2d64656c7461, 0x6c6561662d763032)

branchLane0Key :: (Word64, Word64)
branchLane0Key = (0x6d6c2d64656c7461, 0x6272616e2d763031)

branchLane1Key :: (Word64, Word64)
branchLane1Key = (0x6d6c2d64656c7461, 0x6272616e2d763032)

multisetExpansionKey :: (Word64, Word64)
multisetExpansionKey = (0x6d6c2d64656c7461, 0x6d756c74692d7632)

multisetLane0Key :: (Word64, Word64)
multisetLane0Key = (0x6d6c2d64656c7461, 0x6d7365742d763031)

multisetLane1Key :: (Word64, Word64)
multisetLane1Key = (0x6d6c2d64656c7461, 0x6d7365742d763032)

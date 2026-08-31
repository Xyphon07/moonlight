{-# LANGUAGE BangPatterns #-}

-- | Stable, deterministic structural hashing via SipHash: digests of byte
-- strings, builders, and framed byte chunks, with a default key.
--
-- The framed-chunk path ('stableHashByteStrings') folds its version-and-length
-- framing directly into an incremental SipHash-2-4 state, so no intermediate
-- lazy or strict buffer is ever materialized. The incremental core is
-- byte-for-byte identical to @memory@'s one-shot 'sipHash' over the same framed
-- byte sequence; the differential property in the test-suite is the proof, and
-- it guarantees every existing digest is preserved unchanged.
module Moonlight.Core.StableHash
  ( SipKey (..),
    SipHashState,
    StableHashEncoding,
    StableHashDigest (..),
    stableHashEncodingVersion,
    stableHashEncodingLength,
    defaultStableHashKey,
    stableHashByteString,
    stableHashBuilder,
    stableHashByteStrings,
    stableHashEncodingByteString,
    stableHashEncodingChunks,
    stableHashEncodingDigest,
    stableHashEncodingTextUtf8,
    stableHashEncodingWord8,
    stableHashEncodingWord64Dec,
    stableHashEncodingWord32LE,
    stableHashEncodingWord64LE,
    stableHashUpdateEncoding,
    sipHashDigest,
    sipHashInit,
    sipHashUpdateByteString,
    sipHashUpdateWord8,
    sipHashUpdateWord64Dec,
    sipHashUpdateWord32LE,
    sipHashUpdateWord64LE,
    sipHashFinalize,
    sipHash24,
  )
where

import Data.Bits (countLeadingZeros, finiteBitSize, rotateL, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteArray.Hash (SipHash (..), SipKey (..), sipHash)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Internal as ByteStringInternal
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Unsafe as ByteStringUnsafe
import Data.Char (ord)
import Data.Foldable (foldl')
import Data.Kind (Type)
import qualified Data.Text as Text
import qualified Data.Text.Internal as TextInternal
import Data.Word (Word32, Word64, Word8, byteSwap64)
import qualified Foreign.ForeignPtr as ForeignPtr
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Generics (Generic)
import Prelude
  ( Char,
    Eq,
    Foldable,
    IO,
    Int,
    Monoid (..),
    Ord,
    Read,
    Semigroup (..),
    Show,
    fromIntegral,
    id,
    length,
    min,
    otherwise,
    ($),
    (*),
    (+),
    (-),
    (.),
    (<),
    (>=),
    (==),
    pure,
    quot,
    rem,
  )

type StableHashDigest :: Type
newtype StableHashDigest = StableHashDigest
  { unStableHashDigest :: Word64
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

defaultStableHashKey :: SipKey
defaultStableHashKey =
  SipKey 0x6d6f6f6e6c696768 0x742d636f72652d31

stableHashEncodingVersion :: Word64
stableHashEncodingVersion =
  2

sipHashDigest :: SipKey -> ByteString -> Word64
sipHashDigest key payload =
  case sipHash key payload of
    SipHash digest -> digest

stableHashByteString :: SipKey -> ByteString -> StableHashDigest
stableHashByteString key =
  StableHashDigest . sipHashDigest key

stableHashBuilder :: Builder.Builder -> StableHashDigest
stableHashBuilder payload =
  StableHashDigest
    . sipHashFinalize
    . LazyByteString.foldlChunks sipHashUpdateByteString seededState
    . Builder.toLazyByteString
    $ payload
  where
    seededState =
      sipHashUpdateWord64LE (sipHashInit defaultStableHashKey) stableHashEncodingVersion

-- | Digest a framed sequence of byte-string chunks. The version word, the
-- compact chunk count, and each compact chunk length are folded — together with
-- the chunk bytes — straight into an incremental SipHash-2-4 state. No lazy or
-- strict buffer is materialized; the result is identical to hashing the
-- concatenated framing @version ++ count ++ (length ++ bytes)*@ in one shot.
stableHashByteStrings :: Foldable chunks => chunks ByteString -> StableHashDigest
stableHashByteStrings chunks =
  StableHashDigest (sipHashFinalize (foldl' absorbChunk seededState chunks))
  where
    seededState =
      stableHashFramedChunksSeed (length chunks)
    absorbChunk state chunk =
      sipHashUpdateByteString
        (sipHashUpdateCompactWord64 state (fromIntegral (ByteString.length chunk)))
        chunk

sipHash24 :: Word64 -> Word64 -> ByteString -> Word64
sipHash24 leftKey rightKey =
  sipHashDigest (SipKey leftKey rightKey)

-- Incremental SipHash-2-4. The abstract state carries the four @v@ lanes plus
-- a 0..7-byte little-endian partial block and the running length. Absorbing
-- bytes one at a time keeps the public state immutable while deleting framed
-- buffer materialization in callers that can write directly into the digest.

type SipHashState :: Type
data SipHashState
  = SipHashState
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Word64
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int

type StableHashEncoding :: Type
data StableHashEncoding = StableHashEncoding !Int (SipHashState -> SipHashState)

type DecimalWord64Shape :: Type
data DecimalWord64Shape = DecimalWord64Shape !Int !Word64

stableHashEncodingLength :: StableHashEncoding -> Int
stableHashEncodingLength (StableHashEncoding byteLength _writer) =
  byteLength

instance Semigroup StableHashEncoding where
  StableHashEncoding leftLength leftWriter <> StableHashEncoding rightLength rightWriter =
    StableHashEncoding (leftLength + rightLength) (rightWriter . leftWriter)

instance Monoid StableHashEncoding where
  mempty =
    StableHashEncoding 0 id

sipHashInit :: SipKey -> SipHashState
sipHashInit (SipKey keyLow keyHigh) =
  SipHashState
    (keyLow `xor` 0x736f6d6570736575)
    (keyHigh `xor` 0x646f72616e646f6d)
    (keyLow `xor` 0x6c7967656e657261)
    (keyHigh `xor` 0x7465646279746573)
    0
    0
    0

sipRound ::
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  (Word64, Word64, Word64, Word64)
sipRound v0 v1 v2 v3 =
  let !a0 = v0 + v1
      !a1 = rotateL v1 13 `xor` a0
      !a0' = rotateL a0 32
      !a2 = v2 + v3
      !a3 = rotateL v3 16 `xor` a2
      !b0 = a0' + a3
      !b3 = rotateL a3 21 `xor` b0
      !b2 = a2 + a1
      !b1 = rotateL a1 17 `xor` b2
      !b2' = rotateL b2 32
   in (b0, b1, b2', b3)
{-# INLINE sipRound #-}

sipCompress ::
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  Word64 ->
  (Word64, Word64, Word64, Word64)
sipCompress v0 v1 v2 v3 messageWord =
  let !w3 = v3 `xor` messageWord
      (r0, r1, r2, r3) = sipRound v0 v1 v2 w3
      (s0, s1, s2, s3) = sipRound r0 r1 r2 r3
      !s0' = s0 `xor` messageWord
   in (s0', s1, s2, s3)
{-# INLINE sipCompress #-}

sipHashUpdateWord8 :: SipHashState -> Word8 -> SipHashState
sipHashUpdateWord8 (SipHashState v0 v1 v2 v3 partial partialLen total) byteValue =
  let !partial' = partial .|. (fromIntegral byteValue `shiftL` (8 * partialLen))
      !partialLen' = partialLen + 1
      !total' = total + 1
   in if partialLen' == 8
        then
          let (u0, u1, u2, u3) = sipCompress v0 v1 v2 v3 partial'
           in SipHashState u0 u1 u2 u3 0 0 total'
        else SipHashState v0 v1 v2 v3 partial' partialLen' total'
{-# INLINE sipHashUpdateWord8 #-}

sipHashUpdateByteString :: SipHashState -> ByteString -> SipHashState
sipHashUpdateByteString state chunk =
  let byteLength =
        ByteString.length chunk
   in if byteLength < 8
        then trustedSmallByteStringFold state chunk byteLength
        else
          if byteLength >= 64
            then trustedByteStringBlockFold state chunk
            else ByteString.foldl' sipHashUpdateWord8 state chunk
{-# INLINE sipHashUpdateByteString #-}

trustedSmallByteStringFold :: SipHashState -> ByteString -> Int -> SipHashState
-- Guarded small-chunk reader. The caller passes 'ByteString.length chunk', and
-- every 'unsafeIndex' below is under an exact length case. The fallback keeps
-- the helper total even if a future internal caller forgets that contract.
trustedSmallByteStringFold state chunk byteLength =
  if byteLength < 8
    then sipHashUpdateLittleEndianWord byteLength (trustedSmallByteStringWord chunk byteLength) state
    else ByteString.foldl' sipHashUpdateWord8 state chunk
{-# INLINE trustedSmallByteStringFold #-}

trustedSmallByteStringWord :: ByteString -> Int -> Word64
trustedSmallByteStringWord chunk byteLength =
  case byteLength of
    0 ->
      0
    1 ->
      fromIntegral (ByteStringUnsafe.unsafeIndex chunk 0)
    2 ->
      fromIntegral (ByteStringUnsafe.unsafeIndex chunk 0)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 1) `shiftL` 8)
    3 ->
      fromIntegral (ByteStringUnsafe.unsafeIndex chunk 0)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 1) `shiftL` 8)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 2) `shiftL` 16)
    4 ->
      fromIntegral (ByteStringUnsafe.unsafeIndex chunk 0)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 1) `shiftL` 8)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 2) `shiftL` 16)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 3) `shiftL` 24)
    5 ->
      fromIntegral (ByteStringUnsafe.unsafeIndex chunk 0)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 1) `shiftL` 8)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 2) `shiftL` 16)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 3) `shiftL` 24)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 4) `shiftL` 32)
    6 ->
      fromIntegral (ByteStringUnsafe.unsafeIndex chunk 0)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 1) `shiftL` 8)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 2) `shiftL` 16)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 3) `shiftL` 24)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 4) `shiftL` 32)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 5) `shiftL` 40)
    7 ->
      fromIntegral (ByteStringUnsafe.unsafeIndex chunk 0)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 1) `shiftL` 8)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 2) `shiftL` 16)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 3) `shiftL` 24)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 4) `shiftL` 32)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 5) `shiftL` 40)
        .|. (fromIntegral (ByteStringUnsafe.unsafeIndex chunk 6) `shiftL` 48)
    _ ->
      0
{-# INLINE trustedSmallByteStringWord #-}

-- | The sole trusted ingestion boundary in this module. 'ByteString' is
-- immutable by contract; this folds its stable foreign-ptr region directly into
-- the SipHash state, first filling any existing partial block, then consuming
-- complete 8-byte little-endian blocks, then folding the tail bytes. The public
-- API remains pure and immutable; the internal pointer read is sealed here so
-- callers cannot observe or compose the effect.
trustedByteStringBlockFold :: SipHashState -> ByteString -> SipHashState
trustedByteStringBlockFold state chunk =
  let (foreignPtr, offset, byteLength) =
        ByteStringInternal.toForeignPtr chunk
   in ByteStringInternal.accursedUnutterablePerformIO
        ( ForeignPtr.withForeignPtr foreignPtr $ \basePtr ->
            sipHashUpdatePtrRange state (basePtr `plusPtr` offset) byteLength
        )
{-# INLINE trustedByteStringBlockFold #-}

sipHashUpdatePtrRange :: SipHashState -> Ptr Word8 -> Int -> IO SipHashState
sipHashUpdatePtrRange state ptr byteLength =
  let prefixLength =
        sipHashPrefixLength state byteLength
   in do
        prefixedState <-
          sipHashUpdatePtrBytes prefixLength state ptr
        let remainingLength =
              byteLength - prefixLength
            remainingPtr =
              ptr `plusPtr` prefixLength
        sipHashUpdateAlignedPtrRange prefixedState remainingPtr remainingLength
{-# INLINE sipHashUpdatePtrRange #-}

sipHashPrefixLength :: SipHashState -> Int -> Int
sipHashPrefixLength (SipHashState _v0 _v1 _v2 _v3 _partial partialLen _total) byteLength =
  if partialLen == 0
    then 0
    else min byteLength (8 - partialLen)
{-# INLINE sipHashPrefixLength #-}

sipHashUpdateAlignedPtrRange :: SipHashState -> Ptr Word8 -> Int -> IO SipHashState
sipHashUpdateAlignedPtrRange state ptr byteLength =
  let tailLength =
        byteLength `rem` 8
      blockLength =
        byteLength - tailLength
   in do
        blockedState <-
          sipHashUpdatePtrBlocks blockLength state ptr
        sipHashUpdatePtrBytes tailLength blockedState (ptr `plusPtr` blockLength)
{-# INLINE sipHashUpdateAlignedPtrRange #-}

sipHashUpdatePtrBlocks :: Int -> SipHashState -> Ptr Word8 -> IO SipHashState
sipHashUpdatePtrBlocks byteLength state ptr =
  if byteLength == 0
    then pure state
    else do
      wordValue <-
        peekWord64LE ptr
      sipHashUpdatePtrBlocks
        (byteLength - 8)
        (sipHashUpdateAlignedWord64Block state wordValue)
        (ptr `plusPtr` 8)
{-# INLINE sipHashUpdatePtrBlocks #-}

sipHashUpdatePtrBytes :: Int -> SipHashState -> Ptr Word8 -> IO SipHashState
sipHashUpdatePtrBytes byteLength state ptr =
  if byteLength == 0
    then pure state
    else do
      byteValue <-
        peekWord8Off ptr 0
      sipHashUpdatePtrBytes
        (byteLength - 1)
        (sipHashUpdateWord8 state byteValue)
        (ptr `plusPtr` 1)
{-# INLINE sipHashUpdatePtrBytes #-}

sipHashUpdateAlignedWord64Block :: SipHashState -> Word64 -> SipHashState
sipHashUpdateAlignedWord64Block (SipHashState v0 v1 v2 v3 _partial 0 total) wordValue =
  let (u0, u1, u2, u3) =
        sipCompress v0 v1 v2 v3 wordValue
   in SipHashState u0 u1 u2 u3 0 0 (total + 8)
sipHashUpdateAlignedWord64Block state wordValue =
  sipHashUpdateWord64LE state wordValue
{-# INLINE sipHashUpdateAlignedWord64Block #-}

peekWord64LE :: Ptr Word8 -> IO Word64
peekWord64LE ptr =
  do
    nativeWord <-
      peekWord64Off ptr 0
    pure (word64FromNativeLittleEndian nativeWord)
{-# INLINE peekWord64LE #-}

word64FromNativeLittleEndian :: Word64 -> Word64
word64FromNativeLittleEndian =
  case targetByteOrder of
    LittleEndian ->
      id
    BigEndian ->
      byteSwap64
{-# INLINE word64FromNativeLittleEndian #-}

peekWord64Off :: Ptr Word8 -> Int -> IO Word64
peekWord64Off =
  peekByteOff
{-# INLINE peekWord64Off #-}

peekWord8Off :: Ptr Word8 -> Int -> IO Word8
peekWord8Off =
  peekByteOff
{-# INLINE peekWord8Off #-}

stableHashUpdateEncoding :: SipHashState -> StableHashEncoding -> SipHashState
stableHashUpdateEncoding state (StableHashEncoding _byteLength writer) =
  writer state

stableHashEncodingByteString :: ByteString -> StableHashEncoding
stableHashEncodingByteString bytes =
  StableHashEncoding (ByteString.length bytes) (`sipHashUpdateByteString` bytes)

stableHashEncodingTextUtf8 :: Text.Text -> StableHashEncoding
stableHashEncodingTextUtf8 text@(TextInternal.Text _bytes _offset byteLength) =
  StableHashEncoding byteLength (`sipHashUpdateTextUtf8` text)

sipHashUpdateTextUtf8 :: SipHashState -> Text.Text -> SipHashState
sipHashUpdateTextUtf8 =
  Text.foldl' sipHashUpdateUtf8Char

sipHashUpdateUtf8Char :: SipHashState -> Char -> SipHashState
sipHashUpdateUtf8Char state char =
  let codePoint = ord char
   in if codePoint < 0x80
        then sipHashUpdateWord8 state (fromIntegral codePoint)
        else
          if codePoint < 0x800
            then
              sipHashUpdateWord8
                (sipHashUpdateWord8 state (fromIntegral (0xc0 + (codePoint `shiftR` 6))))
                (fromIntegral (0x80 + (codePoint .&. 0x3f)))
            else
              if codePoint < 0x10000
                then
                  sipHashUpdateWord8
                    ( sipHashUpdateWord8
                        (sipHashUpdateWord8 state (fromIntegral (0xe0 + (codePoint `shiftR` 12))))
                        (fromIntegral (0x80 + ((codePoint `shiftR` 6) .&. 0x3f)))
                    )
                    (fromIntegral (0x80 + (codePoint .&. 0x3f)))
                else
                  sipHashUpdateWord8
                    ( sipHashUpdateWord8
                        ( sipHashUpdateWord8
                            (sipHashUpdateWord8 state (fromIntegral (0xf0 + (codePoint `shiftR` 18))))
                            (fromIntegral (0x80 + ((codePoint `shiftR` 12) .&. 0x3f)))
                        )
                        (fromIntegral (0x80 + ((codePoint `shiftR` 6) .&. 0x3f)))
                    )
                    (fromIntegral (0x80 + (codePoint .&. 0x3f)))

stableHashEncodingWord8 :: Word8 -> StableHashEncoding
stableHashEncodingWord8 byteValue =
  StableHashEncoding 1 (`sipHashUpdateWord8` byteValue)

stableHashEncodingWord64Dec :: Word64 -> StableHashEncoding
stableHashEncodingWord64Dec wordValue =
  case decimalWord64Shape wordValue of
    DecimalWord64Shape byteLength divisor ->
      StableHashEncoding byteLength (\state -> sipHashUpdateWord64DecWithDivisor state wordValue divisor)

sipHashUpdateWord64Dec :: SipHashState -> Word64 -> SipHashState
sipHashUpdateWord64Dec state wordValue =
  case decimalWord64Shape wordValue of
    DecimalWord64Shape _byteLength divisor ->
      sipHashUpdateWord64DecWithDivisor state wordValue divisor
{-# INLINE sipHashUpdateWord64Dec #-}

sipHashUpdateCompactWord64 :: SipHashState -> Word64 -> SipHashState
sipHashUpdateCompactWord64 state wordValue =
  let !byteValue =
        fromIntegral (wordValue .&. 0x7f)
      !remainingValue =
        wordValue `shiftR` 7
   in if remainingValue == 0
        then sipHashUpdateWord8 state byteValue
        else sipHashUpdateCompactWord64 (sipHashUpdateWord8 state (byteValue .|. 0x80)) remainingValue
{-# INLINE sipHashUpdateCompactWord64 #-}

sipHashUpdateWord64DecWithDivisor :: SipHashState -> Word64 -> Word64 -> SipHashState
sipHashUpdateWord64DecWithDivisor state wordValue divisor =
  let !digit =
        wordValue `quot` divisor
      !remainder =
        wordValue - (digit * divisor)
      !state' =
        sipHashUpdateWord8 state (fromIntegral (0x30 + digit))
   in if divisor == 1
        then state'
        else sipHashUpdateWord64DecWithDivisor state' remainder (divisor `quot` 10)
{-# INLINE sipHashUpdateWord64DecWithDivisor #-}

decimalWord64Shape :: Word64 -> DecimalWord64Shape
decimalWord64Shape wordValue =
  DecimalWord64Shape digitCount (tenToThe (digitCount - 1))
  where
    digitCount =
      decimalDigitCount wordValue
{-# INLINE decimalWord64Shape #-}

-- | The number of decimal digits of a 'Word64', in @[1, 20]@ (zero has width
-- one). Constant time: 'countLeadingZeros' gives the bit length, and
-- @bitLength * 1233 \`shiftR\` 12@ approximates @floor (logBase 10 (2 ^
-- bitLength))@ — @1233 / 4096@ is a tight rational above @logBase 10 2@ — so the
-- true width is that estimate or exactly one more, settled by a single decade
-- comparison. Proven digit-for-digit against the exact decade ladder at every
-- power-of-ten boundary.
decimalDigitCount :: Word64 -> Int
decimalDigitCount wordValue
  | wordValue == 0 = 1
  | otherwise =
      let bitLength = finiteBitSize wordValue - countLeadingZeros wordValue
          estimate = (bitLength * 1233) `shiftR` 12
       in if wordValue < tenToThe estimate then estimate else estimate + 1

-- | @10 ^ exponent@ for @exponent@ in @[0, 19]@ — every power of ten a 'Word64'
-- can hold. The search invariant only ever evaluates it inside that range; the
-- final clause supplies the @10^19@ divisor for a full-width value and keeps the
-- function total.
tenToThe :: Int -> Word64
tenToThe exponent =
  case exponent of
    0 -> 1
    1 -> 10
    2 -> 100
    3 -> 1000
    4 -> 10000
    5 -> 100000
    6 -> 1000000
    7 -> 10000000
    8 -> 100000000
    9 -> 1000000000
    10 -> 10000000000
    11 -> 100000000000
    12 -> 1000000000000
    13 -> 10000000000000
    14 -> 100000000000000
    15 -> 1000000000000000
    16 -> 10000000000000000
    17 -> 100000000000000000
    18 -> 1000000000000000000
    _ -> 10000000000000000000
{-# INLINE tenToThe #-}

sipHashUpdateWord32LE :: SipHashState -> Word32 -> SipHashState
sipHashUpdateWord32LE state word =
  sipHashUpdateLittleEndianWord 4 (fromIntegral word) state
{-# INLINE sipHashUpdateWord32LE #-}

stableHashEncodingWord32LE :: Word32 -> StableHashEncoding
stableHashEncodingWord32LE wordValue =
  StableHashEncoding 4 (`sipHashUpdateWord32LE` wordValue)

sipHashUpdateWord64LE :: SipHashState -> Word64 -> SipHashState
sipHashUpdateWord64LE state word =
  sipHashUpdateLittleEndianWord 8 word state
{-# INLINE sipHashUpdateWord64LE #-}

stableHashEncodingWord64LE :: Word64 -> StableHashEncoding
stableHashEncodingWord64LE wordValue =
  StableHashEncoding 8 (`sipHashUpdateWord64LE` wordValue)

sipHashUpdateLittleEndianWord :: Int -> Word64 -> SipHashState -> SipHashState
sipHashUpdateLittleEndianWord byteCount word (SipHashState v0 v1 v2 v3 partial partialLen total) =
  let !total' = total + byteCount
      !combinedByteCount = partialLen + byteCount
      !wordPayload = word .&. wordByteMask byteCount
   in if combinedByteCount < 8
        then
          SipHashState
            v0
            v1
            v2
            v3
            (partial .|. (wordPayload `shiftL` (8 * partialLen)))
            combinedByteCount
            total'
        else
          let !bytesToFill = 8 - partialLen
              !remainingByteCount = combinedByteCount - 8
              !messageWord =
                partial
                  .|. ((word .&. wordByteMask bytesToFill) `shiftL` (8 * partialLen))
              (u0, u1, u2, u3) = sipCompress v0 v1 v2 v3 messageWord
              !remainingPartial =
                (word `shiftR` (8 * bytesToFill)) .&. wordByteMask remainingByteCount
           in SipHashState u0 u1 u2 u3 remainingPartial remainingByteCount total'
{-# INLINE sipHashUpdateLittleEndianWord #-}

wordByteMask :: Int -> Word64
wordByteMask byteCount =
  if byteCount == 8
    then 0xffffffffffffffff
    else (1 `shiftL` (8 * byteCount)) - 1
{-# INLINE wordByteMask #-}

sipHashFinalize :: SipHashState -> Word64
sipHashFinalize (SipHashState v0 v1 v2 v3 partial _partialLen total) =
  let !finalBlock = partial .|. (fromIntegral (total .&. 0xff) `shiftL` 56)
      (w0, w1, w2, w3) = sipCompress v0 v1 v2 v3 finalBlock
      !w2' = w2 `xor` 0xff
      (f0, f1, f2, f3) = sipRound w0 w1 w2' w3
      (g0, g1, g2, g3) = sipRound f0 f1 f2 f3
      (h0, h1, h2, h3) = sipRound g0 g1 g2 g3
      (i0, i1, i2, i3) = sipRound h0 h1 h2 h3
   in i0 `xor` i1 `xor` i2 `xor` i3

stableHashEncodingDigest :: SipKey -> StableHashEncoding -> StableHashDigest
stableHashEncodingDigest key encoding =
  StableHashDigest (sipHashFinalize (stableHashUpdateEncoding (sipHashInit key) encoding))

stableHashEncodingChunks :: Foldable values => (value -> StableHashEncoding) -> values value -> StableHashDigest
stableHashEncodingChunks project values =
  StableHashDigest (sipHashFinalize (foldl' absorbChunk seededState values))
  where
    seededState =
      stableHashFramedChunksSeed (length values)
    absorbChunk state value =
      let encoding =
            project value
       in stableHashUpdateEncoding
            (sipHashUpdateCompactWord64 state (fromIntegral (stableHashEncodingLength encoding)))
            encoding

stableHashFramedChunksSeed :: Int -> SipHashState
stableHashFramedChunksSeed chunkCount =
  sipHashUpdateCompactWord64
    (sipHashUpdateWord64LE (sipHashInit defaultStableHashKey) stableHashEncodingVersion)
    (fromIntegral chunkCount)

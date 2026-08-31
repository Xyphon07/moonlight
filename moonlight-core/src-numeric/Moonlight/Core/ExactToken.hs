{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Self-delimiting byte encodings of exact values: an 'ExactToken' is the canonical serialized witness used for structural identity.
module Moonlight.Core.ExactToken
  ( ExactToken,
    ExactEncoding,
    ExactEncodingAtom (..),
    ExactEncodingError (..),
    exactTokenFromEncoding,
    exactTokenBytes,
    exactTokenLength,
    exactEncodingLength,
    exactAtomEncoding,
    exactSequenceEncoding,
    exactSequenceMapEncoding,
  )
where

import Data.Bits (shiftR)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.ByteString.Short.Internal qualified as ShortByteString
import Data.Foldable (foldr)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Word (Word64)
import GHC.Exts
  ( Int (I#),
    Int#,
    MutableByteArray#,
    State#,
    newByteArray#,
    unsafeFreezeByteArray#,
    writeWord8Array#,
    (<#),
    (+#),
  )
import GHC.ST (ST (..), runST)
import GHC.Word (Word8 (W8#))
import Prelude
  ( Bool (False),
    Either (..),
    Eq (..),
    Foldable,
    Ord (..),
    Ordering (GT, LT),
    Show (..),
    fromIntegral,
    id,
    (<>),
  )

type ExactToken :: Type
data ExactToken
  = CompactExactToken !ShortByteString
  | OversizedExactToken ExactStructure

instance Eq ExactToken where
  left == right =
    case (left, right) of
      (CompactExactToken leftBytes, CompactExactToken rightBytes) -> leftBytes == rightBytes
      (OversizedExactToken leftStructure, OversizedExactToken rightStructure) -> leftStructure == rightStructure
      _ -> False

instance Ord ExactToken where
  compare left right =
    case (left, right) of
      (CompactExactToken leftBytes, CompactExactToken rightBytes) -> compare leftBytes rightBytes
      (OversizedExactToken leftStructure, OversizedExactToken rightStructure) -> compare leftStructure rightStructure
      (CompactExactToken _, OversizedExactToken _) -> LT
      (OversizedExactToken _, CompactExactToken _) -> GT

instance Show ExactToken where
  show token =
    case token of
      CompactExactToken bytes -> "ExactToken " <> show bytes
      OversizedExactToken structure -> "OversizedExactToken " <> show structure

type ExactEncoding :: Type
data ExactEncoding = ExactEncoding ExactStructure {-# UNPACK #-} !ExactLength ExactEncodingWriter

type ExactStructure :: Type
data ExactStructure
  = ExactAtomStructure !ExactEncodingAtom
  | ExactSequenceStructure [ExactStructure]
  deriving stock (Eq, Ord, Show)

type ExactLength :: Type
data ExactLength
  = ValidExactLength Int#
  | ExactLengthOverflow

type ExactEncodingAtom :: Type
data ExactEncodingAtom
  = ExactWord8 Word8
  | ExactInt Int
  deriving stock (Eq, Ord, Show)

type ExactEncodingError :: Type
data ExactEncodingError
  = ExactEncodingLengthExceedsPlatformLimit
  deriving stock (Eq, Show)

exactTokenFromEncoding :: ExactEncoding -> ExactToken
exactTokenFromEncoding (ExactEncoding structure byteLength writer) =
  case byteLength of
    ValidExactLength validByteLength ->
      CompactExactToken (sealExactEncoding# validByteLength writer)
    ExactLengthOverflow ->
      OversizedExactToken structure

exactTokenBytes :: ExactToken -> Either ExactEncodingError ShortByteString
exactTokenBytes token =
  case token of
    CompactExactToken bytes -> Right bytes
    OversizedExactToken _structure -> Left ExactEncodingLengthExceedsPlatformLimit

exactTokenLength :: ExactToken -> Either ExactEncodingError Int
exactTokenLength token =
  case token of
    CompactExactToken bytes -> Right (ShortByteString.length bytes)
    OversizedExactToken _structure -> Left ExactEncodingLengthExceedsPlatformLimit

exactEncodingLength :: ExactEncoding -> Either ExactEncodingError Int
exactEncodingLength (ExactEncoding _structure byteLength _writer) =
  case byteLength of
    ValidExactLength validByteLength -> Right (I# validByteLength)
    ExactLengthOverflow -> Left ExactEncodingLengthExceedsPlatformLimit

exactAtomEncoding :: ExactEncodingAtom -> ExactEncoding
exactAtomEncoding atom =
  case atom of
    ExactWord8 byteValue ->
      ExactEncoding
        (ExactAtomStructure atom)
        (ValidExactLength 2#)
        (writeExactWord8 0x01 `appendExactEncodingWriter` writeExactWord8 byteValue)
    ExactInt intValue ->
      ExactEncoding
        (ExactAtomStructure atom)
        (ValidExactLength 9#)
        (writeExactWord8 0x02 `appendExactEncodingWriter` exactInt64BEWriter intValue)
{-# INLINE exactAtomEncoding #-}

-- The grammar is prefix-decodable: atoms have fixed payload widths, while
-- 0x03 and 0x04 delimit a sequence. Recursive decoding therefore determines
-- every child boundary without redundant per-child lengths.
exactSequenceEncoding :: Foldable values => values ExactEncoding -> ExactEncoding
exactSequenceEncoding =
  exactSequenceMapEncoding id
{-# INLINE exactSequenceEncoding #-}

exactSequenceMapEncoding :: Foldable values => (value -> ExactEncoding) -> values value -> ExactEncoding
exactSequenceMapEncoding project values =
  let SequenceEncoding childByteLength childWriter =
        foldr
          (\value suffix -> prependSequenceEncoding (project value) suffix)
          emptySequenceEncoding
          values
   in ExactEncoding
        ( ExactSequenceStructure
            (foldr (\value suffix -> exactEncodingStructure (project value) : suffix) [] values)
        )
        (checkedLengthAdd (ValidExactLength 2#) childByteLength)
        ( writeExactWord8 0x03
            `appendExactEncodingWriter` childWriter
            `appendExactEncodingWriter` writeExactWord8 0x04
        )
{-# INLINE exactSequenceMapEncoding #-}

type SequenceEncoding :: Type
data SequenceEncoding = SequenceEncoding {-# UNPACK #-} !ExactLength ExactEncodingWriter

emptySequenceEncoding :: SequenceEncoding
emptySequenceEncoding =
  SequenceEncoding (ValidExactLength 0#) emptyExactEncodingWriter

prependSequenceEncoding :: ExactEncoding -> SequenceEncoding -> SequenceEncoding
prependSequenceEncoding (ExactEncoding _structure childByteLength childWriter) (SequenceEncoding suffixByteLength suffixWriter) =
  SequenceEncoding
    (checkedLengthAdd childByteLength suffixByteLength)
    (appendExactEncodingWriter childWriter suffixWriter)
{-# INLINE prependSequenceEncoding #-}

exactEncodingStructure :: ExactEncoding -> ExactStructure
exactEncodingStructure (ExactEncoding structure _byteLength _writer) =
  structure

checkedLengthAdd :: ExactLength -> ExactLength -> ExactLength
checkedLengthAdd left right =
  case (left, right) of
    (ValidExactLength leftValue#, ValidExactLength rightValue#) ->
      case leftValue# +# rightValue# of
        sumValue# ->
          case sumValue# <# 0# of
            0# -> ValidExactLength sumValue#
            _ -> ExactLengthOverflow
    _ -> ExactLengthOverflow
{-# INLINE checkedLengthAdd #-}

exactInt64BEWriter :: Int -> ExactEncodingWriter
exactInt64BEWriter intValue =
  writeExactWord64BE (fromIntegral (fromIntegral intValue :: Int64))

sealExactEncoding# :: Int# -> ExactEncodingWriter -> ShortByteString
sealExactEncoding# byteLength# writer =
  runST (ST createExactTokenBytes)
  where
    createExactTokenBytes :: State# s -> (# State# s, ShortByteString #)
    createExactTokenBytes state0 =
      case newByteArray# byteLength# state0 of
        (# state1, mutableBytes #) ->
          case writer mutableBytes 0# state1 of
            (# state2, _endOffset #) ->
              case unsafeFreezeByteArray# mutableBytes state2 of
                (# state3, frozenBytes #) -> (# state3, SBS frozenBytes #)

type ExactEncodingWriter :: Type
type ExactEncodingWriter =
  forall s.
  MutableByteArray# s ->
  Int# ->
  State# s ->
  (# State# s, Int# #)

appendExactEncodingWriter :: ExactEncodingWriter -> ExactEncodingWriter -> ExactEncodingWriter
appendExactEncodingWriter leftWriter rightWriter mutableBytes offset state0 =
  case leftWriter mutableBytes offset state0 of
    (# state1, offset1 #) -> rightWriter mutableBytes offset1 state1

emptyExactEncodingWriter :: ExactEncodingWriter
emptyExactEncodingWriter _mutableBytes offset state =
  (# state, offset #)

writeExactWord64BE :: Word64 -> ExactEncodingWriter
writeExactWord64BE wordValue =
  writeExactWord8 (fromIntegral (wordValue `shiftR` 56))
    `appendExactEncodingWriter` writeExactWord8 (fromIntegral (wordValue `shiftR` 48))
    `appendExactEncodingWriter` writeExactWord8 (fromIntegral (wordValue `shiftR` 40))
    `appendExactEncodingWriter` writeExactWord8 (fromIntegral (wordValue `shiftR` 32))
    `appendExactEncodingWriter` writeExactWord8 (fromIntegral (wordValue `shiftR` 24))
    `appendExactEncodingWriter` writeExactWord8 (fromIntegral (wordValue `shiftR` 16))
    `appendExactEncodingWriter` writeExactWord8 (fromIntegral (wordValue `shiftR` 8))
    `appendExactEncodingWriter` writeExactWord8 (fromIntegral wordValue)

writeExactWord8 :: Word8 -> ExactEncodingWriter
writeExactWord8 (W8# byteValue) mutableBytes offset state0 =
  case writeWord8Array# mutableBytes offset byteValue state0 of
    state1 -> (# state1, offset +# 1# #)

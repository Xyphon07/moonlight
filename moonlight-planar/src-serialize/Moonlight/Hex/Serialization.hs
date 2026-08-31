{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Versioned canonical bytes for native packed hexagonal regions.
module Moonlight.Hex.Serialization
  ( HexDecodingBudget (..)
  , HexSerializationError (..)
  , hexSerializationVersion
  , encodeHexRegion
  , decodeHexRegion
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Binary.Get
  ( Get
  , getInt64be
  , getWord16be
  , getWord64be
  , runGetOrFail
  )
import Data.Binary.Put
  ( putInt64be
  , putWord16be
  , putWord64be
  , runPut
  )
import Data.Bits ((.&.), complement, shiftL)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate (HexCoord (..))
import Moonlight.Hex.Region
  ( HexLayout
  , HexLayoutObstruction
  , HexRegion
  , hexLayout
  , hexLayoutCellCount
  , hexLayoutHeight
  , hexLayoutOrigin
  , hexLayoutWidth
  , hexLayoutWordCount
  , foldHexRegionPackedWords
  , hexRegionGenerate
  , hexRegionLayout
  )

-- | Bounds checked before any payload-sized allocation.
data HexDecodingBudget = HexDecodingBudget
  { hexDecodingMaximumInputBytes :: !Word64
  , hexDecodingMaximumCells :: !Word64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data HexSerializationError
  = HexBinaryDecodeFailure !Int64 !String
  | HexTrailingBytes !Word64
  | HexInvalidFormatMagic !Word64
  | HexUnsupportedFormatVersion !Word16
  | HexInputByteBudgetExceeded !Word64 !Word64
  | HexCellBudgetExceeded !Word64 !Word64
  | HexCoordinateOutsideInt !Int64 !Int64
  | HexExtentOutsideInt !Word64 !Word64
  | HexSerializedLayoutInvalid !HexLayoutObstruction
  | HexNonCanonicalPadding !Word64
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

hexSerializationVersion :: Word16
hexSerializationVersion = 1

formatMagic :: Word64
formatMagic = 0x4d4f4f4e48455801 -- "MOONHEX" + format family 1

encodeHexRegion :: HexRegion -> BL.ByteString
encodeHexRegion region = runPut $ do
  let layout = hexRegionLayout region
      HexCoord originQ originR = hexLayoutOrigin layout
  putWord64be formatMagic
  putWord16be hexSerializationVersion
  putInt64be (fromIntegral originQ)
  putInt64be (fromIntegral originR)
  putWord64be (fromIntegral (hexLayoutWidth layout))
  putWord64be (fromIntegral (hexLayoutHeight layout))
  foldHexRegionPackedWords
    (\writeWords word -> writeWords *> putWord64be word)
    (pure ())
    region

decodeHexRegion
  :: HexDecodingBudget
  -> BL.ByteString
  -> Either HexSerializationError HexRegion
decodeHexRegion budget bytes
  | inputBytes > hexDecodingMaximumInputBytes budget =
      Left (HexInputByteBudgetExceeded inputBytes (hexDecodingMaximumInputBytes budget))
  | otherwise =
      case runGetOrFail (runExceptT (getHexRegion budget)) bytes of
        Left (_, offset, message) -> Left (HexBinaryDecodeFailure offset message)
        Right (_, _, Left obstruction) -> Left obstruction
        Right (trailing, _, Right region)
          | BL.null trailing -> Right region
          | otherwise -> Left (HexTrailingBytes (fromIntegral (BL.length trailing)))
 where
  inputBytes = fromIntegral (BL.length bytes)

type HexDecoder = ExceptT HexSerializationError Get

getHexRegion :: HexDecodingBudget -> HexDecoder HexRegion
getHexRegion budget = do
  magic <- lift getWord64be
  if magic == formatMagic
    then pure ()
    else throwE (HexInvalidFormatMagic magic)
  version <- lift getWord16be
  if version == hexSerializationVersion
    then pure ()
    else throwE (HexUnsupportedFormatVersion version)
  q64 <- lift getInt64be
  r64 <- lift getInt64be
  width64 <- lift getWord64be
  height64 <- lift getWord64be
  (origin, width, height) <-
    case (int64PairToCoord q64 r64, word64ToInt width64, word64ToInt height64) of
      (Just coordinate, Just widthValue, Just heightValue) ->
        pure (coordinate, widthValue, heightValue)
      (Nothing, _, _) -> throwE (HexCoordinateOutsideInt q64 r64)
      (_, Nothing, _) -> throwE (HexExtentOutsideInt width64 height64)
      (_, _, Nothing) -> throwE (HexExtentOutsideInt width64 height64)
  layout <- either (throwE . HexSerializedLayoutInvalid) pure (hexLayout origin width height)
  let cellCount = fromIntegral (hexLayoutCellCount layout)
  if cellCount > hexDecodingMaximumCells budget
    then throwE (HexCellBudgetExceeded cellCount (hexDecodingMaximumCells budget))
    else pure ()
  serializedWords <- U.fromList <$> lift (replicateM (hexLayoutWordCount layout) getWord64be)
  validatePadding layout serializedWords
  pure (hexRegionGenerate layout (serializedMember layout serializedWords))

validatePadding :: HexLayout -> U.Vector Word64 -> HexDecoder ()
validatePadding layout wordsValue =
  let mask = finalWordMask layout
      canonical =
        U.ifoldl'
          (\valid index word ->
              valid
                && ( index + 1 < hexLayoutWordCount layout
                       || word .&. complement mask == 0
                   )
          )
          True
          wordsValue
   in if canonical
        then pure ()
        else
          let padding = U.foldl' (\_ word -> word .&. complement mask) 0 wordsValue
           in throwE (HexNonCanonicalPadding padding)

serializedMember :: HexLayout -> U.Vector Word64 -> HexCoord -> Bool
serializedMember layout wordsValue (HexCoord q r) =
  let HexCoord originQ originR = hexLayoutOrigin layout
      index = (r - originR) * hexLayoutWidth layout + (q - originQ)
      (wordIndex, bitIndex) = index `quotRem` 64
   in wordsValue `U.unsafeIndex` wordIndex .&. (1 `shiftL` bitIndex) /= 0
{-# INLINE serializedMember #-}

finalWordMask :: HexLayout -> Word64
finalWordMask layout = lowBitMask (((hexLayoutCellCount layout - 1) `rem` 64) + 1)

lowBitMask :: Int -> Word64
lowBitMask count
  | count >= 64 = maxBound
  | count <= 0 = 0
  | otherwise = (1 `shiftL` count) - 1

int64PairToCoord :: Int64 -> Int64 -> Maybe HexCoord
int64PairToCoord q r = HexCoord <$> int64ToInt q <*> int64ToInt r

int64ToInt :: Int64 -> Maybe Int
int64ToInt value
  | toInteger value < toInteger (minBound :: Int) = Nothing
  | toInteger value > toInteger (maxBound :: Int) = Nothing
  | otherwise = Just (fromIntegral value)

word64ToInt :: Word64 -> Maybe Int
word64ToInt value
  | toInteger value > toInteger (maxBound :: Int) = Nothing
  | otherwise = Just (fromIntegral value)

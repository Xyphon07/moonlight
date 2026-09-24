{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Trusted packed representation shared by the public region algebra and its
-- topology interpretation. This module is private to the @hex@ component.
module Moonlight.Hex.Region.Internal
  ( HexLayout (..)
  , HexRegion (..)
  , extractBits
  , finalWordMask
  , firstSameLayoutDifferenceIndex
  , generateHexRegionIndexed
  , generateHexRegionIndexedM
  , hexCoordAtIndex
  , hexCoordIndex
  , hexRegionMemberAtIndex
  , lowBitMask
  , setBitAt
  ) where

import Control.DeepSeq (NFData)
import Data.Bits ((.&.), (.|.), complement, countTrailingZeros, shiftL, shiftR)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64)
import GHC.Generics (Generic)
import Moonlight.Hex.Coordinate (HexCoord (..))

-- | A nonempty axial parallelogram with an overflow-safe dense row-major index.
data HexLayout = HexLayout
  { layoutOrigin :: !HexCoord
  , layoutWidth :: !Int
  , layoutHeight :: !Int
  , layoutCellCount :: !Int
  , layoutWordCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Canonical packed membership: one bit per layout cell and zero padding.
data HexRegion = HexRegion !HexLayout !(U.Vector Word64)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Construct canonical packed membership from a dense row-major predicate.
-- The index is private representation vocabulary; public callers author by
-- coordinate through 'Moonlight.Hex.Region.hexRegionGenerateM'.
generateHexRegionIndexedM
  :: Monad effect
  => HexLayout
  -> (Int -> effect Bool)
  -> effect HexRegion
generateHexRegionIndexedM layout predicate =
  HexRegion layout <$> U.generateM (layoutWordCount layout) generateWord
 where
  generateWord wordIndex =
    let baseIndex = wordIndex * 64
        bitCount = min 64 (layoutCellCount layout - baseIndex)
     in gatherBits baseIndex bitCount 0 0

  gatherBits !baseIndex !bitCount !bitIndex !word
    | bitIndex >= bitCount = pure word
    | otherwise = do
        selected <- predicate (baseIndex + bitIndex)
        gatherBits
          baseIndex
          bitCount
          (bitIndex + 1)
          (if selected then setBitAt word bitIndex else word)
{-# INLINE generateHexRegionIndexedM #-}

generateHexRegionIndexed :: HexLayout -> (Int -> Bool) -> HexRegion
generateHexRegionIndexed layout predicate =
  HexRegion layout (U.generate (layoutWordCount layout) generateWord)
 where
  generateWord wordIndex =
    let baseIndex = wordIndex * 64
        bitCount = min 64 (layoutCellCount layout - baseIndex)
     in gatherBits baseIndex bitCount 0 0

  gatherBits !baseIndex !bitCount !bitIndex !word
    | bitIndex >= bitCount = word
    | predicate (baseIndex + bitIndex) =
        gatherBits baseIndex bitCount (bitIndex + 1) (setBitAt word bitIndex)
    | otherwise = gatherBits baseIndex bitCount (bitIndex + 1) word
{-# INLINE generateHexRegionIndexed #-}

-- | Membership at an index already proved to belong to the region's layout.
hexRegionMemberAtIndex :: HexRegion -> Int -> Bool
hexRegionMemberAtIndex (HexRegion _ wordsValue) index =
  let (wordIndex, bitIndex) = index `quotRem` 64
   in wordsValue `U.unsafeIndex` wordIndex .&. (1 `shiftL` bitIndex) /= 0
{-# INLINE hexRegionMemberAtIndex #-}

-- | First selected cell in the left region absent from the right region.
-- Callers establish equal layouts, which entails equal canonical word counts.
firstSameLayoutDifferenceIndex :: HexRegion -> HexRegion -> Maybe Int
firstSameLayoutDifferenceIndex (HexRegion _ left) (HexRegion _ right) = descend 0
 where
  descend wordIndex
    | wordIndex >= U.length left = Nothing
    | otherwise =
        let difference =
              left `U.unsafeIndex` wordIndex
                .&. complement (right `U.unsafeIndex` wordIndex)
         in if difference == 0
              then descend (wordIndex + 1)
              else Just (wordIndex * 64 + countTrailingZeros difference)
{-# INLINE firstSameLayoutDifferenceIndex #-}

extractBits :: U.Vector Word64 -> Int -> Int -> Word64
extractBits wordsValue bitIndex count
  | count <= 0 = 0
  | otherwise =
      let (wordIndex, bitOffset) = bitIndex `quotRem` 64
          low = wordsValue `U.unsafeIndex` wordIndex `shiftR` bitOffset
          high =
            if bitOffset == 0 || bitOffset + count <= 64
              then 0
              else wordsValue `U.unsafeIndex` (wordIndex + 1) `shiftL` (64 - bitOffset)
       in (low .|. high) .&. lowBitMask count
{-# INLINE extractBits #-}

hexCoordIndex :: HexLayout -> HexCoord -> Maybe Int
hexCoordIndex layout (HexCoord q r)
  | q < originQ || r < originR = Nothing
  | q > maximumQ || r > maximumR = Nothing
  | otherwise = Just ((r - originR) * layoutWidth layout + (q - originQ))
 where
  HexCoord originQ originR = layoutOrigin layout
  maximumQ = originQ + layoutWidth layout - 1
  maximumR = originR + layoutHeight layout - 1
{-# INLINE hexCoordIndex #-}

hexCoordAtIndex :: HexLayout -> Int -> HexCoord
hexCoordAtIndex layout index =
  let (row, column) = index `quotRem` layoutWidth layout
      HexCoord originQ originR = layoutOrigin layout
   in HexCoord (originQ + column) (originR + row)
{-# INLINE hexCoordAtIndex #-}

finalWordMask :: HexLayout -> Word64
finalWordMask layout =
  lowBitMask (((layoutCellCount layout - 1) `rem` 64) + 1)
{-# INLINE finalWordMask #-}

lowBitMask :: Int -> Word64
lowBitMask count
  | count >= 64 = maxBound
  | count <= 0 = 0
  | otherwise = (1 `shiftL` count) - 1
{-# INLINE lowBitMask #-}

setBitAt :: Word64 -> Int -> Word64
setBitAt word bitIndex = word .|. (1 `shiftL` bitIndex)
{-# INLINE setBitAt #-}

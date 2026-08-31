-- | Floating-point normalization helpers.
--
-- 'ulpDistance' and 'ulpDistanceFloat' map IEEE bit patterns into monotone
-- unsigned order keys before subtracting. The chosen key law collapses the two
-- IEEE zeros to the same key, so their distance is zero, while adjacent
-- distinct representable values have distance one. If either operand is NaN,
-- the distance is the sentinel @0xFFFFFFFFFFFFFFFF@.
module Moonlight.Internal.FloatMath
  ( ulpDistance,
    ulpDistanceFloat,
    isNegativeZero,
    normalizeNegativeZero,
  )
where

import Data.Bits ((.&.), complement)
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import Prelude (Bool, Double, Eq (..), Float, Num (..), Ord (..), fromIntegral, otherwise, (||))

doubleSignBit :: Word64
doubleSignBit = 0x8000000000000000

doubleZeroKey :: Word64
doubleZeroKey = doubleSignBit - 1

floatSignBit :: Word32
floatSignBit = 0x80000000

floatZeroKey :: Word32
floatZeroKey = floatSignBit - 1

-- | Detect IEEE negative zero by inspecting the sign bit pattern directly.
isNegativeZero :: Double -> Bool
isNegativeZero x = castDoubleToWord64 x == doubleSignBit

-- | Rewrite negative zero to positive zero and leave all other values intact.
normalizeNegativeZero :: Double -> Double
normalizeNegativeZero x
  | isNegativeZero x = 0.0
  | otherwise = x

doubleOrderKey :: Double -> Word64
doubleOrderKey x =
  let bits = castDoubleToWord64 x
   in if bits .&. doubleSignBit == doubleSignBit
        then complement bits
        else bits + doubleZeroKey

floatOrderKey :: Float -> Word32
floatOrderKey x =
  let bits = castFloatToWord32 x
   in if bits .&. floatSignBit == floatSignBit
        then complement bits
        else bits + floatZeroKey

-- | ULP distance between two 'Double' values after monotonic-bias reinterpretation.
--
-- Returns @0xFFFFFFFFFFFFFFFF@ when either input is NaN.
ulpDistance :: Double -> Double -> Word64
ulpDistance a b
  | a /= a || b /= b = 0xFFFFFFFFFFFFFFFF
  | otherwise =
      let leftKey = doubleOrderKey a
          rightKey = doubleOrderKey b
       in if leftKey >= rightKey
            then leftKey - rightKey
            else rightKey - leftKey

-- | ULP distance between two 'Float' values in the native 32-bit key lattice.
--
-- Returns @0xFFFFFFFFFFFFFFFF@ when either input is NaN.
ulpDistanceFloat :: Float -> Float -> Word64
ulpDistanceFloat a b
  | a /= a || b /= b = 0xFFFFFFFFFFFFFFFF
  | otherwise =
      let leftKey = floatOrderKey a
          rightKey = floatOrderKey b
       in fromIntegral
            ( if leftKey >= rightKey
                then leftKey - rightKey
                else rightKey - leftKey
            )

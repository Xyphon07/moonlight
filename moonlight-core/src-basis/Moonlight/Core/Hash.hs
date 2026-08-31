-- | Word64 hash-mixing primitives and the golden-ratio constant.
module Moonlight.Core.Hash
  ( mixWord64,
    deriveSeedWord,
    goldenRatioConstant,
  )
where

import Data.Bits (shiftL, shiftR, xor)
import Data.Word (Word64)
import Prelude

goldenRatioConstant :: Word64
goldenRatioConstant = 0x9e3779b97f4a7c15

mixWord64 :: Word64 -> Word64 -> Word64
mixWord64 leftValue rightValue =
  let mixed = leftValue `xor` (rightValue + goldenRatioConstant + shiftL leftValue 6 + shiftR leftValue 2)
   in mixed * 0xbf58476d1ce4e5b9 + 0x94d049bb133111eb

deriveSeedWord :: Word64 -> Word64 -> Word64
deriveSeedWord leftValue rightValue =
  (leftValue * 0xff51afd7ed558ccd) `xor` (rightValue + goldenRatioConstant)

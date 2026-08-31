module Moonlight.LinAlg.Internal.Discrete
  ( GF2 (..),
    gf2Zero,
    gf2One,
    gf2ToBool,
    gf2FromBool,
    rankPackedRows,
    PackedBitMatrix (..),
    matrixRowWords,
    packedBitMatrixFromRowMajor,
    packedBitMatrixFromXorEntries,
  )
where

import Data.Bits (Bits (bit, xor, (.|.)), zeroBits)
import qualified Data.IntMap.Strict as IntMap
import Data.Kind (Type)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Moonlight.Algebra.Pure.Ring
  ( EuclideanDomain (..),
    GCDDomain (..),
    IntegralDomain (..),
  )
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    CommutativeRing,
    Field (..),
    MultiplicativeMonoid (..),
    Ring,
    Semiring,
  )
import qualified Moonlight.LinAlg.Internal.GF2.Xor as GF2Xor
import Prelude

type GF2 :: Type
data GF2 = GF2Zero | GF2One
  deriving stock (Eq, Ord, Show)

instance Num GF2 where
  (+) = add
  (*) = mul
  negate = id
  abs = id
  signum value =
    case value of
      GF2Zero -> GF2Zero
      GF2One -> GF2One
  fromInteger integerValue =
    if odd integerValue
      then GF2One
      else GF2Zero

gf2Zero :: GF2
gf2Zero = GF2Zero

gf2One :: GF2
gf2One = GF2One

gf2FromBool :: Bool -> GF2
gf2FromBool flag = if flag then GF2One else GF2Zero

gf2ToBool :: GF2 -> Bool
gf2ToBool value = case value of
  GF2Zero -> False
  GF2One -> True

type PackedBitMatrix :: Type
data PackedBitMatrix = PackedBitMatrix
  { packedRows :: Int,
    packedCols :: Int,
    packedWordsPerRow :: Int,
    packedWords :: U.Vector Word64
  }
  deriving stock (Eq, Show)

wordWidth :: Int -> Int
wordWidth count
  | count <= 0 = 0
  | otherwise =
      count `Prelude.div` 64
        + if count `mod` 64 == 0 then 0 else 1

packRowMajorBits :: Int -> Int -> [GF2] -> U.Vector Word64
packRowMajorBits rowCount columnCount values =
  let wordsPerRowValue = wordWidth columnCount
      packedWordCount = rowCount * wordsPerRowValue
      updateWord mapValue (linearIndex, bitValue) =
        if bitValue == GF2Zero || columnCount <= 0
          then mapValue
          else
            let rowIndex = linearIndex `Prelude.div` columnCount
                columnIndex = linearIndex `mod` columnCount
                wordIndex = (rowIndex * wordsPerRowValue) + (columnIndex `Prelude.div` 64)
                bitIndex = columnIndex `mod` 64
                bitMask = bit bitIndex :: Word64
             in IntMap.insertWith (.|.) wordIndex bitMask mapValue
      packedMap = foldl' updateWord IntMap.empty (zip [0 :: Int ..] values)
   in U.generate packedWordCount (\wordIndex -> IntMap.findWithDefault zeroBits wordIndex packedMap)

packedBitMatrixFromRowMajor :: Int -> Int -> [GF2] -> PackedBitMatrix
packedBitMatrixFromRowMajor rowCount columnCount values =
  PackedBitMatrix
    { packedRows = rowCount,
      packedCols = columnCount,
      packedWordsPerRow = wordWidth columnCount,
      packedWords = packRowMajorBits rowCount columnCount values
    }

packedBitMatrixFromXorEntries :: Int -> Int -> [(Int, Int)] -> PackedBitMatrix
packedBitMatrixFromXorEntries rowCount columnCount entries =
  let wordsPerRowValue = wordWidth columnCount
      packedWordCount = rowCount * wordsPerRowValue
      updateWord mapValue (rowIndex, columnIndex) =
        let wordIndex = (rowIndex * wordsPerRowValue) + (columnIndex `Prelude.div` 64)
            bitIndex = columnIndex `mod` 64
            bitMask = bit bitIndex :: Word64
         in IntMap.insertWith xor wordIndex bitMask mapValue
      packedMap = foldl' updateWord IntMap.empty entries
   in PackedBitMatrix
        { packedRows = rowCount,
          packedCols = columnCount,
          packedWordsPerRow = wordsPerRowValue,
          packedWords = U.generate packedWordCount (\wordIndex -> IntMap.findWithDefault zeroBits wordIndex packedMap)
        }

matrixRowWords :: PackedBitMatrix -> Int -> U.Vector Word64
matrixRowWords matrixValue rowIndex =
  let wordsPerRowValue = packedWordsPerRow matrixValue
      startIndex = rowIndex * wordsPerRowValue
   in U.take wordsPerRowValue (U.drop startIndex (packedWords matrixValue))

rankPackedRows :: Int -> [U.Vector Word64] -> Int
rankPackedRows =
  GF2Xor.rankPackedRowsByReduction

instance AdditiveMonoid GF2 where
  zero = GF2Zero
  add left right =
    case (left, right) of
      (GF2Zero, value) -> value
      (value, GF2Zero) -> value
      (GF2One, GF2One) -> GF2Zero

instance AdditiveGroup GF2 where
  neg = id

instance MultiplicativeMonoid GF2 where
  one = GF2One
  mul left right =
    case (left, right) of
      (GF2One, GF2One) -> GF2One
      _ -> GF2Zero

instance Ring GF2

instance Field GF2 where
  tryInv value = case value of
    GF2Zero -> Nothing
    GF2One -> Just GF2One

instance Semiring GF2

instance CommutativeRing GF2

instance IntegralDomain GF2 where
  isZero value = value == GF2Zero
  unitInverse value = case value of
    GF2Zero -> Nothing
    GF2One -> Just GF2One

instance GCDDomain GF2 where
  gcdDomain left right
    | left == GF2Zero && right == GF2Zero = GF2Zero
    | otherwise = GF2One
  extendedGcdDomain left right
    | left == GF2Zero && right == GF2Zero = (GF2Zero, GF2Zero, GF2Zero)
    | left == GF2One = (GF2One, GF2One, GF2Zero)
    | otherwise = (GF2One, GF2Zero, GF2One)

instance EuclideanDomain GF2 where
  type Degree GF2 = Int

  divideWithRemainder numerator _ = (numerator, GF2Zero)

  degree value =
    case value of
      GF2Zero -> 0
      GF2One -> 0

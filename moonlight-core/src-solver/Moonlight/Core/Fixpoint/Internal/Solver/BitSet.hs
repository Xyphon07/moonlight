-- | A mutable @Word64@-packed bit set over a bounded non-negative key range.
module Moonlight.Core.Fixpoint.Internal.Solver.BitSet
  ( MutableBitSet (..),
    newBitSet,
    bitSetMember,
    bitSetInsert,
    bitSetDelete,
  )
where

import Control.Monad.ST (ST)
import Data.Bits (clearBit, setBit, testBit)
import Data.Vector.Unboxed.Mutable qualified as UMVector
import Data.Word (Word64)
import Prelude

data MutableBitSet state = MutableBitSet
  { size :: !Int,
    bitSetWords :: !(UMVector.MVector state Word64)
  }

newBitSet :: Int -> ST state (MutableBitSet state)
newBitSet size = do
  wordsVector <- UMVector.replicate wordCount 0
  pure
    MutableBitSet
      { size = max 0 size,
        bitSetWords = wordsVector
      }
  where
    wordCount =
      (max 0 size + bitSetWordBits - 1) `quot` bitSetWordBits

bitSetMember :: Int -> MutableBitSet state -> ST state Bool
bitSetMember key bitSet
  | not (bitSetInBounds key bitSet) = pure False
  | otherwise = do
      word <- UMVector.read (bitSetWords bitSet) (bitSetWordIndex key)
      pure (testBit word (bitSetBitIndex key))

bitSetInsert :: Int -> MutableBitSet state -> ST state ()
bitSetInsert key bitSet
  | not (bitSetInBounds key bitSet) = pure ()
  | otherwise =
      modifyBitSetWord key (`setBit` bitSetBitIndex key) bitSet

bitSetDelete :: Int -> MutableBitSet state -> ST state ()
bitSetDelete key bitSet
  | not (bitSetInBounds key bitSet) = pure ()
  | otherwise =
      modifyBitSetWord key (`clearBit` bitSetBitIndex key) bitSet

modifyBitSetWord :: Int -> (Word64 -> Word64) -> MutableBitSet state -> ST state ()
modifyBitSetWord key update bitSet = do
  word <- UMVector.read (bitSetWords bitSet) wordIndex
  UMVector.write (bitSetWords bitSet) wordIndex (update word)
  where
    wordIndex =
      bitSetWordIndex key

bitSetInBounds :: Int -> MutableBitSet state -> Bool
bitSetInBounds key bitSet =
  key >= 0 && key < size bitSet

bitSetWordIndex :: Int -> Int
bitSetWordIndex key =
  key `quot` bitSetWordBits

bitSetBitIndex :: Int -> Int
bitSetBitIndex key =
  key `rem` bitSetWordBits

bitSetWordBits :: Int
bitSetWordBits =
  64

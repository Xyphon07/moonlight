{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | The set of faces still owed a refinement decision.
module Moonlight.Triangulation.Internal.FaceQueue
  ( FaceQueue
  , newFaceQueue
  , pushFace
  , popFace
  ) where

import Control.Monad.ST (ST)
import qualified Data.IntSet as IntSet
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Data.Word (Word32)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Moonlight.Triangulation.Internal.Growable
  ( GrowableWord32
  , newGrowableWord32
  , popGrowableOr
  , pushGrowable
  )
import Moonlight.Triangulation.Internal.PackedIndex (noIndex, packIndex)

-- A worst-first heap has to be told a score for every face it is offered, so
-- the caller derives one — circumradius, area, the encroachment verdict — for
-- faces that are then found acceptable and dropped. Ruppert's termination does
-- not rest on that order; it rests on every bad face being reached before the
-- run ends. So this only has to be a set with a discipline for draining it.
--
-- A stack is that, and it makes membership O(1) with nothing to compute. The
-- pending flags are what keep it a set: a face touched by several insertions
-- before it is drained is decided once, not once per touch.
--
-- The size lives in an unboxed cell for the reason the growable stack's does:
-- it is written on every push and every pop, and a boxed counter allocates a
-- box per write once the size leaves the shared small-'Int' range.
data FaceQueue s
  = DenseFaceQueue
      !(MUV.MVector s Word32)
      !(MUV.MVector s Bool)
      !(MUV.MVector s Int)
  | SparseFaceQueue
      !(GrowableWord32 s)
      !(STRef s IntSet.IntSet)

newFaceQueue :: Maybe Int -> ST s (FaceQueue s)
newFaceQueue capacity =
  case capacity of
    Just size -> do
      faces <- MUV.new (max 1 size)
      pending <- MUV.replicate (max 1 size) False
      stackSize <- MUV.replicate 1 0
      pure (DenseFaceQueue faces pending stackSize)
    Nothing -> do
      faces <- newGrowableWord32 16
      pending <- newSTRef IntSet.empty
      pure (SparseFaceQueue faces pending)

-- The checked read of the pending flag is the one bound this module does not
-- establish itself: the face arrives from mesh topology. Once it succeeds,
-- uniqueness proves the stack write: at most one slot exists for each pending
-- flag, and the two vectors have the same length.
pushFace :: FaceQueue s -> Int -> ST s ()
pushFace queue face =
  case queue of
    DenseFaceQueue faces pending stackSize -> do
      isPending <- MUV.unsafeRead pending face
      if isPending
        then pure ()
        else do
          size <- MUV.unsafeRead stackSize 0
          MUV.unsafeWrite faces size (packIndex face)
          MUV.unsafeWrite pending face True
          MUV.unsafeWrite stackSize 0 (size + 1)
    SparseFaceQueue faces pending -> do
      isPending <- IntSet.member face <$> readSTRef pending
      if isPending
        then pure ()
        else do
          pushGrowable faces (packIndex face)
          modifySTRef' pending (IntSet.insert face)

popFace :: FaceQueue s -> ST s (Maybe Int)
popFace queue =
  case queue of
    DenseFaceQueue faces pending stackSize -> do
      size <- MUV.unsafeRead stackSize 0
      if size == 0
        then pure Nothing
        else do
          let !index = size - 1
          face <- fromIntegral <$> MUV.unsafeRead faces index
          MUV.unsafeWrite pending face False
          MUV.unsafeWrite stackSize 0 index
          pure (Just face)
    SparseFaceQueue faces pending -> do
      packed <- popGrowableOr noIndex faces
      if packed == noIndex
        then pure Nothing
        else do
          let !face = fromIntegral packed
          modifySTRef' pending (IntSet.delete face)
          pure (Just face)

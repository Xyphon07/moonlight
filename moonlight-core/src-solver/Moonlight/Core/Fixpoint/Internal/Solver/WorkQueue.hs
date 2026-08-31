-- | A mutable ring-buffer work queue with membership dedup via a bit set.
module Moonlight.Core.Fixpoint.Internal.Solver.WorkQueue
  ( WorkQueue (..),
    new,
    enqueue,
    drain,
  )
where

import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed.Mutable qualified as UMVector
import Moonlight.Core.Fixpoint.Internal.Solver.BitSet
  ( MutableBitSet,
    bitSetDelete,
    bitSetInsert,
    bitSetMember,
    newBitSet,
  )
import Prelude

data WorkQueue state = WorkQueue
  { items :: !(UMVector.MVector state Int),
    queued :: !(MutableBitSet state),
    headRef :: !(STRef state Int),
    sizeRef :: !(STRef state Int)
  }

new :: Int -> ST state (WorkQueue state)
new capacity = do
  items <- UMVector.replicate capacity 0
  queued <- newBitSet capacity
  headRef <- newSTRef 0
  sizeRef <- newSTRef 0
  pure
    WorkQueue
      { items = items,
        queued = queued,
        headRef = headRef,
        sizeRef = sizeRef
      }

enqueue :: WorkQueue state -> Int -> ST state ()
enqueue queue key = do
  alreadyQueued <- bitSetMember key (queued queue)
  if alreadyQueued
    then pure ()
    else do
      size <- readSTRef (sizeRef queue)
      headIndex <- readSTRef (headRef queue)
      let insertionIndex = (headIndex + size) `rem` UMVector.length (items queue)
      UMVector.write (items queue) insertionIndex key
      bitSetInsert key (queued queue)
      writeSTRef (sizeRef queue) (size + 1)

dequeue :: WorkQueue state -> ST state (Maybe Int)
dequeue queue = do
  size <- readSTRef (sizeRef queue)
  if size <= 0
    then pure Nothing
    else do
      headIndex <- readSTRef (headRef queue)
      key <- UMVector.read (items queue) headIndex
      bitSetDelete key (queued queue)
      writeSTRef (headRef queue) ((headIndex + 1) `rem` UMVector.length (items queue))
      writeSTRef (sizeRef queue) (size - 1)
      pure (Just key)

drain :: WorkQueue state -> (Int -> ST state ()) -> ST state ()
drain queue step = do
  item <- dequeue queue
  case item of
    Nothing -> pure ()
    Just key -> step key *> drain queue step

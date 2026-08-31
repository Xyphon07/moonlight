-- | Raw dense-arena primitives: presence-gated parent/rank reads and writes,
-- slot initialization, dirty-key tracking, and capacity growth. All mutation
-- is confined here behind the presence flag.
module Moonlight.Core.UnionFind.Transaction.Internal.DenseStore
  ( readDenseParentKey,
    readDenseRank,
    denseSlotPresent,
    writeDenseParentIfPresent,
    writeDenseRankIfPresent,
    initializeDenseSlot,
    ensureDenseCapacity,
    growDenseStore,
    growZeroed,
  )
where

import Control.Monad.ST (ST)
import Data.STRef (modifySTRef', readSTRef, writeSTRef)
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as Mutable
import Moonlight.Core.UnionFind.Transaction.Internal.Policy
  ( denseKeyInBounds,
    denseTargetLength,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( DenseStore (..),
    UnionFindEditor (..),
    denseFlagSet,
  )
import Prelude

readDenseParentKey ::
  UnionFindEditor state ->
  Int ->
  ST state (Maybe Int)
readDenseParentKey editor key = do
  store <- readSTRef (dense editor)
  if denseKeyInBounds store key
    then do
      present <- Mutable.read (present store) key
      if present == denseFlagSet
        then fmap Just (Mutable.read (parent store) key)
        else pure Nothing
    else pure Nothing
{-# INLINE readDenseParentKey #-}

readDenseRank ::
  UnionFindEditor state ->
  Int ->
  ST state (Maybe Int)
readDenseRank editor key = do
  store <- readSTRef (dense editor)
  if denseKeyInBounds store key
    then do
      present <- Mutable.read (present store) key
      if present == denseFlagSet
        then fmap Just (Mutable.read (rank store) key)
        else pure Nothing
    else pure Nothing
{-# INLINE readDenseRank #-}

denseSlotPresent ::
  UnionFindEditor state ->
  Int ->
  ST state Bool
denseSlotPresent editor key = do
  store <- readSTRef (dense editor)
  if denseKeyInBounds store key
    then fmap (== denseFlagSet) (Mutable.read (present store) key)
    else pure False
{-# INLINE denseSlotPresent #-}

writeDenseParentIfPresent ::
  UnionFindEditor state ->
  Int ->
  Int ->
  ST state Bool
writeDenseParentIfPresent editor key parentKey = do
  store <- readSTRef (dense editor)
  if denseKeyInBounds store key
    then do
      present <- Mutable.read (present store) key
      if present == denseFlagSet
        then do
          Mutable.write (parent store) key parentKey
          markDenseParentDirty editor store key
          pure True
        else pure False
    else pure False
{-# INLINE writeDenseParentIfPresent #-}

writeDenseRankIfPresent ::
  UnionFindEditor state ->
  Int ->
  Int ->
  ST state Bool
writeDenseRankIfPresent editor key rankValue = do
  store <- readSTRef (dense editor)
  if denseKeyInBounds store key
    then do
      present <- Mutable.read (present store) key
      if present == denseFlagSet
        then do
          Mutable.write (rank store) key rankValue
          markDenseRankDirty editor store key
          pure True
        else pure False
    else pure False
{-# INLINE writeDenseRankIfPresent #-}

initializeDenseSlot ::
  UnionFindEditor state ->
  Int ->
  ST state Bool
initializeDenseSlot editor key = do
  store <- readSTRef (dense editor)
  if denseKeyInBounds store key
    then do
      Mutable.write (parent store) key key
      Mutable.write (rank store) key 0
      Mutable.write (present store) key denseFlagSet
      markDenseParentDirty editor store key
      markDenseRankDirty editor store key
      modifySTRef' (denseMemberCount editor) (+ 1)
      pure True
    else pure False

markDenseParentDirty ::
  UnionFindEditor state ->
  DenseStore state ->
  Int ->
  ST state ()
markDenseParentDirty editor store key = do
  dirty <- Mutable.read (parentDirty store) key
  if dirty == denseFlagSet
    then pure ()
    else do
      Mutable.write (parentDirty store) key denseFlagSet
      modifySTRef' (dirtyDenseParents editor) (key :)
      modifySTRef' (dirtyDenseParentCount editor) (+ 1)
{-# INLINE markDenseParentDirty #-}

markDenseRankDirty ::
  UnionFindEditor state ->
  DenseStore state ->
  Int ->
  ST state ()
markDenseRankDirty editor store key = do
  dirty <- Mutable.read (rankDirty store) key
  if dirty == denseFlagSet
    then pure ()
    else do
      Mutable.write (rankDirty store) key denseFlagSet
      modifySTRef' (dirtyDenseRanks editor) (key :)
      modifySTRef' (dirtyDenseRankCount editor) (+ 1)
{-# INLINE markDenseRankDirty #-}

ensureDenseCapacity ::
  UnionFindEditor state ->
  Int ->
  ST state Bool
ensureDenseCapacity editor key = do
  store <- readSTRef (dense editor)
  let currentLength = Mutable.length (parent store)
  case denseTargetLength currentLength key of
    Nothing ->
      pure False
    Just targetLength
      | targetLength <= currentLength ->
          pure True
      | currentLength == 0 ->
          growDenseStore editor store targetLength
      | otherwise -> do
          denseMemberCount <- readSTRef (denseMemberCount editor)
          if denseMemberCount * 4 >= currentLength * 3
            then growDenseStore editor store targetLength
            else pure False

growDenseStore ::
  UnionFindEditor state ->
  DenseStore state ->
  Int ->
  ST state Bool
growDenseStore editor store targetLength = do
  parents <- growZeroed (parent store) targetLength
  ranks <- growZeroed (rank store) targetLength
  presence <- growZeroed (present store) targetLength
  dirtyParents <- growZeroed (parentDirty store) targetLength
  dirtyRanks <- growZeroed (rankDirty store) targetLength
  writeSTRef
    (dense editor)
    DenseStore
      { parent = parents,
        rank = ranks,
        present = presence,
        parentDirty = dirtyParents,
        rankDirty = dirtyRanks
      }
  pure True

growZeroed ::
  (Unbox value, Num value) =>
  MVector state value ->
  Int ->
  ST state (MVector state value)
growZeroed values targetLength =
  let currentLength = Mutable.length values
      additionalLength = targetLength - currentLength
   in if additionalLength <= 0
        then pure values
        else do
          grownValues <- Mutable.grow values additionalLength
          Mutable.set (Mutable.slice currentLength additionalLength grownValues) 0
          pure grownValues

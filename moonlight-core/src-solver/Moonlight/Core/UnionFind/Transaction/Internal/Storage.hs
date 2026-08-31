-- | The dense↔sparse↔base dispatch seam: unified parent/rank reads and writes
-- that consult the dense arena first, then the sparse overlay, then the frozen
-- base, plus the class-identity writers that place a fresh key on the right tier.
module Moonlight.Core.UnionFind.Transaction.Internal.Storage
  ( readParentKey,
    readRankValue,
    writeParentKey,
    writeRankValue,
    writeFreshClassIdentity,
    writeSparseIdentity,
  )
where

import Control.Monad.ST (ST)
import Data.IntMap.Strict qualified as IntMap
import Data.STRef (modifySTRef', readSTRef)
import Moonlight.Core.Identifier.EGraph (ClassId (..), classIdKey)
import Moonlight.Core.UnionFind.Internal.Types (UnionFind (..))
import Moonlight.Core.UnionFind.Transaction.Internal.DenseStore
  ( ensureDenseCapacity,
    initializeDenseSlot,
    readDenseParentKey,
    readDenseRank,
    writeDenseParentIfPresent,
    writeDenseRankIfPresent,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( UnionFindEditor (..),
  )
import Prelude

readParentKey ::
  UnionFindEditor state ->
  Int ->
  ST state (Maybe Int)
readParentKey editor key = do
  maybeDenseParent <- readDenseParentKey editor key
  case maybeDenseParent of
    Just parentKey ->
      pure (Just parentKey)
    Nothing -> do
      sparseWrites <- readSTRef (sparseParentWrites editor)
      case IntMap.lookup key sparseWrites of
        Just parentClassId ->
          pure (Just (classIdKey parentClassId))
        Nothing ->
          pure (fmap classIdKey (IntMap.lookup key (ufParent (base editor))))
{-# INLINE readParentKey #-}

readRankValue :: UnionFindEditor state -> Int -> ST state Int
readRankValue editor key = do
  maybeDenseRank <- readDenseRank editor key
  case maybeDenseRank of
    Just rankValue ->
      pure rankValue
    Nothing -> do
      sparseWrites <- readSTRef (sparseRankWrites editor)
      let baseRank = IntMap.findWithDefault 0 key (ufRank (base editor))
      pure (IntMap.findWithDefault baseRank key sparseWrites)
{-# INLINE readRankValue #-}

writeParentKey ::
  UnionFindEditor state ->
  Int ->
  Int ->
  ST state ()
writeParentKey editor childKey parentKey = do
  currentParent <- readParentKey editor childKey
  if currentParent == Just parentKey
    then pure ()
    else do
      wroteDense <- writeDenseParentIfPresent editor childKey parentKey
      if wroteDense
        then pure ()
        else
          modifySTRef'
            (sparseParentWrites editor)
            (IntMap.insert childKey (ClassId parentKey))
{-# INLINE writeParentKey #-}

writeRankValue ::
  UnionFindEditor state ->
  Int ->
  Int ->
  ST state ()
writeRankValue editor key rankValue = do
  currentRank <- readRankValue editor key
  if currentRank == rankValue
    then pure ()
    else do
      wroteDense <- writeDenseRankIfPresent editor key rankValue
      if wroteDense
        then pure ()
        else
          modifySTRef'
            (sparseRankWrites editor)
            (IntMap.insert key rankValue)
{-# INLINE writeRankValue #-}

writeFreshClassIdentity :: UnionFindEditor state -> Int -> ST state ()
writeFreshClassIdentity editor key = do
  denseReady <- ensureDenseCapacity editor key
  if denseReady
    then do
      initialized <- initializeDenseSlot editor key
      if initialized
        then pure ()
        else writeSparseIdentity editor key
    else writeSparseIdentity editor key

writeSparseIdentity :: UnionFindEditor state -> Int -> ST state ()
writeSparseIdentity editor key = do
  modifySTRef'
    (sparseParentWrites editor)
    (IntMap.insert key (ClassId key))
  modifySTRef'
    (sparseRankWrites editor)
    (IntMap.insert key 0)

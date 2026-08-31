-- | Freezing the editor back to immutable maps: parent/rank snapshots that
-- choose between a full dense sweep and a dirty-key overlay depending on which
-- is cheaper, with the dense sweeps producing ascending entry lists.
module Moonlight.Core.UnionFind.Transaction.Internal.Snapshot
  ( parentMap,
    rankMap,
  )
where

import Control.Monad.ST (ST)
import Data.Foldable (foldlM)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.STRef (STRef, readSTRef)
import Data.Vector.Unboxed.Mutable qualified as Mutable
import Moonlight.Core.Identifier.EGraph (ClassId (..))
import Moonlight.Core.UnionFind.Internal.Types (UnionFind (..))
import Moonlight.Core.UnionFind.Transaction.Internal.DenseStore
  ( readDenseParentKey,
    readDenseRank,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( DenseStore (..),
    UnionFindEditor (..),
    denseFlagSet,
  )
import Prelude

parentMap ::
  UnionFindEditor state ->
  ST state (IntMap ClassId)
parentMap editor = do
  sparseWrites <- readSTRef (sparseParentWrites editor)
  useDenseSnapshot <- denseOutweighsDirtyOverlay editor (dirtyDenseParentCount editor)
  if IntMap.null (ufParent (base editor)) || useDenseSnapshot
    then do
      denseParents <- denseParentMap editor
      pure (IntMap.union denseParents (IntMap.union sparseWrites (ufParent (base editor))))
    else do
      dirtyDenseKeys <- readSTRef (dirtyDenseParents editor)
      foldlM
        insertDenseParent
        (IntMap.union sparseWrites (ufParent (base editor)))
        dirtyDenseKeys
  where
    insertDenseParent parents key = do
      maybeParentKey <- readDenseParentKey editor key
      pure $
        case maybeParentKey of
          Nothing ->
            parents
          Just parentKey ->
            IntMap.insert key (ClassId parentKey) parents

denseOutweighsDirtyOverlay ::
  UnionFindEditor state ->
  STRef state Int ->
  ST state Bool
denseOutweighsDirtyOverlay editor dirtyCountReference = do
  dirtyCount <- readSTRef dirtyCountReference
  denseMemberCount <- readSTRef (denseMemberCount editor)
  pure (dirtyCount * 4 >= denseMemberCount)

denseParentMap ::
  UnionFindEditor state ->
  ST state (IntMap ClassId)
denseParentMap editor = do
  store <- readSTRef (dense editor)
  entries <-
    denseParentEntries
      store
      0
      (Mutable.length (parent store))
      []
  pure (IntMap.fromDistinctAscList entries)

denseParentEntries ::
  DenseStore state ->
  Int ->
  Int ->
  [(Int, ClassId)] ->
  ST state [(Int, ClassId)]
denseParentEntries store key limit entries
  | key >= limit =
      pure (reverse entries)
  | otherwise = do
      present <- Mutable.read (present store) key
      if present == denseFlagSet
        then do
          parentKey <- Mutable.read (parent store) key
          denseParentEntries
            store
            (key + 1)
            limit
            ((key, ClassId parentKey) : entries)
        else denseParentEntries store (key + 1) limit entries

rankMap ::
  UnionFindEditor state ->
  ST state (IntMap Int)
rankMap editor = do
  sparseWrites <- readSTRef (sparseRankWrites editor)
  useDenseSnapshot <- denseOutweighsDirtyOverlay editor (dirtyDenseRankCount editor)
  if IntMap.null (ufRank (base editor)) || useDenseSnapshot
    then do
      denseRanks <- denseRankMap editor
      pure
        (IntMap.union denseRanks (IntMap.union sparseWrites (ufRank (base editor))))
    else do
      dirtyDenseKeys <- readSTRef (dirtyDenseRanks editor)
      foldlM
        insertDenseRank
        (IntMap.union sparseWrites (ufRank (base editor)))
        dirtyDenseKeys
  where
    insertDenseRank ranks key = do
      maybeRank <- readDenseRank editor key
      pure $
        case maybeRank of
          Nothing ->
            ranks
          Just rankValue ->
            IntMap.insert key rankValue ranks

denseRankMap ::
  UnionFindEditor state ->
  ST state (IntMap Int)
denseRankMap editor = do
  store <- readSTRef (dense editor)
  entries <-
    denseRankEntries
      store
      0
      (Mutable.length (rank store))
      []
  pure (IntMap.fromDistinctAscList entries)

denseRankEntries ::
  DenseStore state ->
  Int ->
  Int ->
  [(Int, Int)] ->
  ST state [(Int, Int)]
denseRankEntries store key limit entries
  | key >= limit =
      pure (reverse entries)
  | otherwise = do
      present <- Mutable.read (present store) key
      if present == denseFlagSet
        then do
          rankValue <- Mutable.read (rank store) key
          denseRankEntries
            store
            (key + 1)
            limit
            ((key, rankValue) : entries)
        else denseRankEntries store (key + 1) limit entries

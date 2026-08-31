-- | The two union-find algorithms in mutable form: iterated path compression to
-- a root, and application of a rank-directed link decision.
module Moonlight.Core.UnionFind.Transaction.Internal.Algorithm
  ( compressRootKey,
    applyLinkDecision,
  )
where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Moonlight.Core.Identifier.EGraph (classIdKey)
import Moonlight.Core.UnionFind.Internal.Semantics
  ( LinkDecision (..),
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Storage
  ( readParentKey,
    writeParentKey,
    writeRankValue,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( UnionFindEditor,
    UnionOutcome (..),
  )
import Prelude

compressRootKey :: UnionFindEditor state -> Int -> ST state Int
compressRootKey editor key = do
  maybeParentKey <- readParentKey editor key
  case maybeParentKey of
    Nothing ->
      pure key
    Just parentKey
      | parentKey == key ->
          pure key
      | otherwise -> do
          rootKey <- compressRootKey editor parentKey
          writeParentKey editor key rootKey
          pure rootKey
{-# INLINE compressRootKey #-}

applyLinkDecision ::
  UnionFindEditor state ->
  LinkDecision ->
  ST state UnionOutcome
applyLinkDecision editor decision = do
  let (childRoot, parentRoot, maybeRaisedRank) =
        case decision of
          AttachRoot child parent ->
            (child, parent, Nothing)
          AttachRootAndRaise child parent raisedRank ->
            (child, parent, Just raisedRank)
  writeParentKey editor (classIdKey childRoot) (classIdKey parentRoot)
  traverse_
    (\raisedRank -> writeRankValue editor (classIdKey parentRoot) raisedRank)
    maybeRaisedRank
  pure (MergedClasses parentRoot childRoot)

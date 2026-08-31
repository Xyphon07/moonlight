-- | A sealed mutable execution mode for the canonical persistent union-find.
-- Dense non-negative prefixes use mutable parent/rank arrays with in-place path
-- compression; sparse, negative, and giant keys stay in an 'IntMap' overlay.
-- Successful transactions freeze back to the immutable 'UnionFind' owner.
--
-- This module is the public face: the two transaction runners (the only
-- @runST@ seals) and the query/mutation verbs. The mutable arena lives in
-- sealed @Transaction.Internal.*@ owners; 'UnionFindEditor' is re-exported
-- abstractly so no external caller can forge or dissect editor state.
module Moonlight.Core.UnionFind.Transaction
  ( UnionFindEditor,
    UnionOutcome (..),
    UnionFindAllocationError (..),
    runUnionFindTransaction,
    runUnionFindTransactionEither,
    transactionMember,
    transactionFind,
    transactionFindExisting,
    transactionCanonicalClass,
    transactionInsertClassId,
    transactionMakeSet,
    transactionUnion,
    transactionEquivalent,
    transactionCanonicalMapAndCompress,
    transactionCompress,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (isJust)
import Data.STRef (modifySTRef', readSTRef, writeSTRef)
import Moonlight.Core.Identifier.EGraph (ClassId (..), classIdKey)
import Moonlight.Core.UnionFind.Internal.Semantics (chooseLink, rootKeyBy)
import Moonlight.Core.UnionFind.Internal.Types
  ( UnionFind,
    UnionFindAllocationError (..),
    advanceNextFreshForClassIdKey,
    allocateNextClassId,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Algorithm
  ( applyLinkDecision,
    compressRootKey,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Lifecycle
  ( freezeUnionFind,
    thawUnionFind,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Snapshot
  ( parentMap,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Storage
  ( readParentKey,
    readRankValue,
    writeFreshClassIdentity,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( UnionFindEditor (nextFresh),
    UnionOutcome (..),
  )
import Prelude

runUnionFindTransaction ::
  UnionFind ->
  (forall state. UnionFindEditor state -> ST state value) ->
  (value, UnionFind)
runUnionFindTransaction base action =
  runST $ do
    editor <- thawUnionFind base
    value <- action editor
    committed <- freezeUnionFind editor
    pure (value, committed)

runUnionFindTransactionEither ::
  UnionFind ->
  (forall state. UnionFindEditor state -> ST state (Either err value)) ->
  Either err (value, UnionFind)
runUnionFindTransactionEither base action =
  runST $ do
    editor <- thawUnionFind base
    outcome <- action editor
    case outcome of
      Left err ->
        pure (Left err)
      Right value -> do
        committed <- freezeUnionFind editor
        pure (Right (value, committed))

transactionMember :: UnionFindEditor state -> ClassId -> ST state Bool
transactionMember editor classId =
  fmap isJust (readParentKey editor (classIdKey classId))
{-# INLINE transactionMember #-}

transactionFind :: UnionFindEditor state -> ClassId -> ST state ClassId
transactionFind editor classId =
  fmap ClassId (compressRootKey editor (classIdKey classId))
{-# INLINE transactionFind #-}

transactionFindExisting ::
  UnionFindEditor state ->
  ClassId ->
  ST state (Maybe ClassId)
transactionFindExisting editor classId = do
  exists <- transactionMember editor classId
  if exists
    then fmap Just (transactionFind editor classId)
    else pure Nothing
{-# INLINE transactionFindExisting #-}

transactionCanonicalClass ::
  UnionFindEditor state ->
  ClassId ->
  ST state (Maybe ClassId)
transactionCanonicalClass editor classId = do
  exists <- transactionMember editor classId
  if exists
    then fmap (Just . ClassId) (rootKeyBy (readParentKey editor) (classIdKey classId))
    else pure Nothing
{-# INLINE transactionCanonicalClass #-}

transactionInsertClassId :: UnionFindEditor state -> ClassId -> ST state ()
transactionInsertClassId editor classId = do
  let key = classIdKey classId
  modifySTRef' (nextFresh editor) (advanceNextFreshForClassIdKey key)
  exists <- fmap isJust (readParentKey editor key)
  if exists
    then pure ()
    else writeFreshClassIdentity editor key
{-# INLINE transactionInsertClassId #-}

transactionMakeSet :: UnionFindEditor state -> ST state (Either UnionFindAllocationError ClassId)
transactionMakeSet editor = do
  currentNextFresh <- readSTRef (nextFresh editor)
  case allocateNextClassId currentNextFresh of
    Left allocationError ->
      pure (Left allocationError)
    Right (classId, updatedNextFresh) -> do
      writeFreshClassIdentity editor (classIdKey classId)
      writeSTRef (nextFresh editor) updatedNextFresh
      pure (Right classId)
{-# INLINE transactionMakeSet #-}

transactionUnion ::
  UnionFindEditor state ->
  ClassId ->
  ClassId ->
  ST state UnionOutcome
transactionUnion editor leftClassId rightClassId = do
  transactionInsertClassId editor leftClassId
  transactionInsertClassId editor rightClassId
  leftRoot <- transactionFind editor leftClassId
  rightRoot <- transactionFind editor rightClassId
  if leftRoot == rightRoot
    then pure (AlreadyEquivalent leftRoot)
    else do
      leftRank <- readRankValue editor (classIdKey leftRoot)
      rightRank <- readRankValue editor (classIdKey rightRoot)
      applyLinkDecision editor (chooseLink leftRoot leftRank rightRoot rightRank)
{-# INLINE transactionUnion #-}

transactionEquivalent ::
  UnionFindEditor state ->
  ClassId ->
  ClassId ->
  ST state Bool
transactionEquivalent editor leftClassId rightClassId = do
  leftRoot <- rootKeyBy (readParentKey editor) (classIdKey leftClassId)
  rightRoot <- rootKeyBy (readParentKey editor) (classIdKey rightClassId)
  pure (leftRoot == rightRoot)
{-# INLINE transactionEquivalent #-}

transactionCanonicalMapAndCompress ::
  UnionFindEditor state ->
  ST state (IntMap ClassId)
transactionCanonicalMapAndCompress editor = do
  currentParents <- parentMap editor
  IntMap.traverseWithKey
    (\key _ -> transactionFind editor (ClassId key))
    currentParents

-- | Compress every parent chain in place; the freeze commits the flat
-- structure.
transactionCompress :: UnionFindEditor state -> ST state ()
transactionCompress editor = do
  currentParents <- parentMap editor
  traverse_ (compressRootKey editor) (IntMap.keys currentParents)
{-# INLINE transactionCompress #-}

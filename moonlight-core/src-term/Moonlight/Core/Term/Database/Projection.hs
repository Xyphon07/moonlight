{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Projection where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray qualified as PrimArray
import Moonlight.Core.Term.Database.Index
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Types
import Prelude

dirtyRowsForKeys :: Database f key -> IntSet -> Map (Operator f) RowIdSet
dirtyRowsForKeys database keys =
  operatorMapFromIds database (dirtyRowsForKeysByOperatorId database keys)

dirtyRowsForKeysByOperatorId :: Database f key -> IntSet -> IntMap RowIdSet
dirtyRowsForKeysByOperatorId database keys =
  IntMap.filter (not . rowIdSetNull) (fmap dirtyRows tables)
  where
    tables =
      operatorTables database
    dirtyRows table =
      rowIdSetUnion
        indexedDirtyRows
        (unindexedRowIdsWhere (rowReferencesAnyKey keys) table)
      where
        indexedDirtyRows =
          rowIdSetUnion
            (RowIdSet (intMapDependentsOfMany keyList (derivedResultIndex table)))
            (RowIdSet (intMapDependentsOfMany keyList (derivedChildUserIndex table)))
    keyList =
      IntSet.toAscList keys

operatorRows :: Database f key -> Map (Operator f) [(RowId, DatabaseRow)]
operatorRows database =
  operatorMapFromIds database (operatorRowsByOperatorId database)

operatorRowsByOperatorId :: Database f key -> IntMap [(RowId, DatabaseRow)]
operatorRowsByOperatorId =
  fmap operatorTableRows . operatorTables

operatorMapFromIds :: Database f key -> IntMap value -> Map (Operator f) value
operatorMapFromIds database valuesByOperatorId =
  Map.mapMaybe (`IntMap.lookup` valuesByOperatorId) (operatorIds database)
{-# INLINE operatorMapFromIds #-}

operatorMapFromShapes ::
  (forall a. Ord a => Ord (f a)) =>
  Database f key ->
  IntMap value ->
  Map (Operator f) value
operatorMapFromShapes database =
  IntMap.foldlWithKey' projectOperatorValue Map.empty
  where
    projectOperatorValue valuesByOperator operatorId value =
      maybe
        valuesByOperator
        (\operator -> Map.insert operator value valuesByOperator)
        (IntMap.lookup operatorId (operatorShapes database))
{-# INLINE operatorMapFromShapes #-}

resultKeys :: Database f key -> IntSet
resultKeys database =
  IntMap.foldl' collectTable IntSet.empty tables
  where
    tables =
      operatorTables database
    collectTable :: IntSet -> OperatorTable f -> IntSet
    collectTable accumulatedResultKeys table =
      rowIdSetFoldl'
        (insertLiveTableRowResult table)
        accumulatedResultKeys
        (tableLiveRows table)
{-# INLINE resultKeys #-}

resultKeysUsingAnyChildKey :: IntSet -> Database f key -> IntSet
resultKeysUsingAnyChildKey keys database =
  IntMap.foldl' collectTable IntSet.empty tables
  where
    tables =
      operatorTables database
    keyList =
      IntSet.toAscList keys
    collectTable :: IntSet -> OperatorTable f -> IntSet
    collectTable accumulatedResultKeys table =
      rowIdSetFoldl'
        (insertLiveTableRowResult table)
        accumulatedResultKeys
        (rowIdSetUnion
            (RowIdSet (intMapDependentsOfMany keyList (derivedChildUserIndex table)))
            (unindexedRowIdsWhere (rowUsesAnyChildKey keys) table)
        )
{-# INLINE resultKeysUsingAnyChildKey #-}

rowReferencesAnyKey :: IntSet -> DatabaseRow -> Bool
rowReferencesAnyKey keys row =
  IntSet.member (rowResult row) keys
    || rowUsesAnyChildKey keys row
{-# INLINE rowReferencesAnyKey #-}

rowUsesAnyChildKey :: IntSet -> DatabaseRow -> Bool
rowUsesAnyChildKey keys row =
  PrimArray.foldrPrimArray
    (\childKey childMatched -> IntSet.member childKey keys || childMatched)
    False
    (rowChildrenArray row)
{-# INLINE rowUsesAnyChildKey #-}

insertLiveTableRowResult :: OperatorTable f -> IntSet -> Int -> IntSet
insertLiveTableRowResult table accumulatedResultKeys rowKey =
  maybe
    accumulatedResultKeys
    (`IntSet.insert` accumulatedResultKeys)
    (operatorTableResultAt rowKey table)
{-# INLINE insertLiveTableRowResult #-}

rowsForOperator :: (forall a. Ord a => Ord (f a)) => Operator f -> Database f key -> [(RowId, DatabaseRow)]
rowsForOperator operator database =
  maybe [] operatorTableRows (operatorTableFor operator database)

operatorRowsForIds ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  RowIdSet ->
  Database f key ->
  [(RowId, DatabaseRow)]
operatorRowsForIds operator rowIds database =
  maybe [] (`rowsForIds` rowIds) (operatorTableFor operator database)

operatorRowsForIdsByOperatorId :: Int -> RowIdSet -> Database f key -> [(RowId, DatabaseRow)]
operatorRowsForIdsByOperatorId operatorId rowIds database =
  maybe [] (`rowsForIds` rowIds) (IntMap.lookup operatorId (operatorTables database))

rowsForResultKey :: (forall a. Ord a => Ord (f a)) => Int -> Operator f -> Database f key -> [(RowId, DatabaseRow)]
rowsForResultKey resultKey operator database =
  maybe [] rowsForTable (operatorTableFor operator database)
  where
    rowsForTable table =
      rowsForIds table (operatorTableRowIdsForResultKeys (IntSet.singleton resultKey) table)

rowsForResultKeys :: (forall a. Ord a => Ord (f a)) => IntSet -> Operator f -> Database f key -> [(RowId, DatabaseRow)]
rowsForResultKeys encodedResultKeys operator database =
  maybe [] rowsForTable (operatorTableFor operator database)
  where
    rowsForTable table =
      rowsForIds table (operatorTableRowIdsForResultKeys encodedResultKeys table)

operatorTableFor :: (forall a. Ord a => Ord (f a)) => Operator f -> Database f key -> Maybe (OperatorTable f)
operatorTableFor operator database =
  operatorIdFor operator database
    >>= \operatorId -> IntMap.lookup operatorId (operatorTables database)
{-# INLINE operatorTableFor #-}

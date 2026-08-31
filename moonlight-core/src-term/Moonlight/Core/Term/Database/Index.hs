{-# LANGUAGE BangPatterns #-}

module Moonlight.Core.Term.Database.Index where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Traversable (mapAccumL)
import Moonlight.Core.Term.Database.Types
import Prelude

derivedResultIndex :: OperatorTable f -> ResultIndex
derivedResultIndex =
  resultIx
{-# INLINE derivedResultIndex #-}

derivedChildColumnValueIndex :: OperatorTable f -> ChildColumnValueIndex
derivedChildColumnValueIndex =
  childColumnIx
{-# INLINE derivedChildColumnValueIndex #-}

derivedExactIndex :: OperatorTable f -> ExactIndex
derivedExactIndex =
  exactIx
{-# INLINE derivedExactIndex #-}

derivedChildUserIndex :: OperatorTable f -> ChildUserIndex
derivedChildUserIndex =
  childUserIx
{-# INLINE derivedChildUserIndex #-}

emptyExactIndex :: Int -> ExactIndex
emptyExactIndex =
  emptyChildTupleIndex

emptyExactResultIndex :: Int -> ExactResultIndex
emptyExactResultIndex =
  emptyChildTupleIndex

emptyChildTupleIndex :: Int -> ChildTupleIndex
emptyChildTupleIndex arity =
  case arity of
    0 -> NullaryChildTupleIndex IntSet.empty
    1 -> UnaryChildTupleIndex IntMap.empty
    2 -> BinaryChildTupleIndex IntMap.empty
    _ -> NaryChildTupleIndex Map.empty

emptyResultIndex :: ResultIndex
emptyResultIndex =
  IntMap.empty

emptyChildColumnValueIndex :: Int -> ChildColumnValueIndex
emptyChildColumnValueIndex arity =
  case arity of
    0 -> NullaryChildColumnValueIndex
    1 -> UnaryChildColumnValueIndex IntMap.empty
    2 -> BinaryChildColumnValueIndex IntMap.empty IntMap.empty
    _ -> NaryChildColumnValueIndex (SmallArray.smallArrayFromList (replicate arity IntMap.empty))
{-# INLINE emptyChildColumnValueIndex #-}

emptyChildUserIndex :: ChildUserIndex
emptyChildUserIndex =
  IntMap.empty

lookupExactIndex :: [Int] -> ExactIndex -> RowIdSet
lookupExactIndex children =
  RowIdSet . lookupChildTupleProbe children
{-# INLINE lookupExactIndex #-}

lookupExactIndexArray :: PrimArray Int -> ExactIndex -> RowIdSet
lookupExactIndexArray children =
  RowIdSet . lookupChildTupleIndex children
{-# INLINE lookupExactIndexArray #-}

lookupExactResultIndex :: PrimArray Int -> ExactResultIndex -> IntSet
lookupExactResultIndex =
  lookupChildTupleIndex
{-# INLINE lookupExactResultIndex #-}

lookupChildTupleProbe :: [Int] -> ChildTupleIndex -> IntSet
lookupChildTupleProbe children childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      case children of
        [] -> keys
        _nonNullaryChildren -> IntSet.empty
    UnaryChildTupleIndex byChild ->
      case children of
        [child] -> IntMap.findWithDefault IntSet.empty child byChild
        _nonUnaryChildren -> IntSet.empty
    BinaryChildTupleIndex byLeftChild ->
      case children of
        [leftChild, rightChild] ->
          maybe IntSet.empty (IntMap.findWithDefault IntSet.empty rightChild) (IntMap.lookup leftChild byLeftChild)
        _nonBinaryChildren -> IntSet.empty
    NaryChildTupleIndex byChildren ->
      Map.findWithDefault IntSet.empty (ProbeChildTupleKey children) byChildren
{-# INLINE lookupChildTupleProbe #-}

lookupChildTupleIndex :: PrimArray Int -> ChildTupleIndex -> IntSet
lookupChildTupleIndex children childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      if PrimArray.sizeofPrimArray children == 0
        then keys
        else IntSet.empty
    UnaryChildTupleIndex byChild ->
      if PrimArray.sizeofPrimArray children == 1
        then IntMap.findWithDefault IntSet.empty (PrimArray.indexPrimArray children 0) byChild
        else IntSet.empty
    BinaryChildTupleIndex byLeftChild ->
      if PrimArray.sizeofPrimArray children == 2
        then
          maybe
            IntSet.empty
            (IntMap.findWithDefault IntSet.empty (PrimArray.indexPrimArray children 1))
            (IntMap.lookup (PrimArray.indexPrimArray children 0) byLeftChild)
        else IntSet.empty
    NaryChildTupleIndex byChildren ->
      Map.findWithDefault IntSet.empty (StoredChildTupleKey children) byChildren
{-# INLINE lookupChildTupleIndex #-}

lookupResultIndex :: Int -> ResultIndex -> RowIdSet
lookupResultIndex resultKey =
  RowIdSet . IntMap.findWithDefault IntSet.empty resultKey
{-# INLINE lookupResultIndex #-}

intMapDependentsOfMany :: [Int] -> IntMap IntSet -> IntSet
intMapDependentsOfMany keys indexValue =
  foldMap (\key -> IntMap.findWithDefault IntSet.empty key indexValue) keys
{-# INLINE intMapDependentsOfMany #-}

insertExactIndex :: Int -> PrimArray Int -> ExactIndex -> ExactIndex
insertExactIndex =
  insertChildTupleIndex
{-# INLINE insertExactIndex #-}

insertExactResultIndex :: Int -> PrimArray Int -> ExactResultIndex -> ExactResultIndex
insertExactResultIndex =
  insertChildTupleIndex
{-# INLINE insertExactResultIndex #-}

insertChildTupleIndex :: Int -> PrimArray Int -> ChildTupleIndex -> ChildTupleIndex
insertChildTupleIndex dependent children childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      if PrimArray.sizeofPrimArray children == 0
        then NullaryChildTupleIndex (IntSet.insert dependent keys)
        else childTupleIndex
    UnaryChildTupleIndex byChild ->
      if PrimArray.sizeofPrimArray children == 1
        then UnaryChildTupleIndex (insertIntSetAtKey (PrimArray.indexPrimArray children 0) dependent byChild)
        else childTupleIndex
    BinaryChildTupleIndex byLeftChild ->
      if PrimArray.sizeofPrimArray children == 2
        then
          BinaryChildTupleIndex $
            IntMap.insertWith
              (IntMap.unionWith IntSet.union)
              (PrimArray.indexPrimArray children 0)
              (IntMap.singleton (PrimArray.indexPrimArray children 1) (IntSet.singleton dependent))
              byLeftChild
        else childTupleIndex
    NaryChildTupleIndex byChildren ->
      NaryChildTupleIndex $
        Map.insertWith
          IntSet.union
          (StoredChildTupleKey children)
          (IntSet.singleton dependent)
          byChildren
{-# INLINE insertChildTupleIndex #-}

insertResultIndex :: Int -> Int -> ResultIndex -> ResultIndex
insertResultIndex rowKey resultKey =
  insertIntSetAtKey resultKey rowKey
{-# INLINE insertResultIndex #-}

insertChildUserIndex :: Int -> Int -> ChildUserIndex -> ChildUserIndex
insertChildUserIndex rowKey childKey =
  insertIntSetAtKey childKey rowKey
{-# INLINE insertChildUserIndex #-}

deleteExactIndex :: Int -> PrimArray Int -> ExactIndex -> ExactIndex
deleteExactIndex =
  deleteChildTupleIndex
{-# INLINE deleteExactIndex #-}

deleteExactResultIndex :: Int -> PrimArray Int -> ExactResultIndex -> ExactResultIndex
deleteExactResultIndex =
  deleteChildTupleIndex
{-# INLINE deleteExactResultIndex #-}

deleteResultIndex :: Int -> Int -> ResultIndex -> ResultIndex
deleteResultIndex rowKey resultKey =
  removeIntSetAtIntKey resultKey rowKey
{-# INLINE deleteResultIndex #-}

deleteChildUserIndex :: Int -> Int -> ChildUserIndex -> ChildUserIndex
deleteChildUserIndex rowKey childKey =
  removeIntSetAtIntKey childKey rowKey
{-# INLINE deleteChildUserIndex #-}

deleteChildTupleIndex :: Int -> PrimArray Int -> ChildTupleIndex -> ChildTupleIndex
deleteChildTupleIndex dependent children childTupleIndex =
  case childTupleIndex of
    NullaryChildTupleIndex keys ->
      if PrimArray.sizeofPrimArray children == 0
        then NullaryChildTupleIndex (IntSet.delete dependent keys)
        else childTupleIndex
    UnaryChildTupleIndex byChild ->
      if PrimArray.sizeofPrimArray children == 1
        then UnaryChildTupleIndex (removeIntSetAtIntKey (PrimArray.indexPrimArray children 0) dependent byChild)
        else childTupleIndex
    BinaryChildTupleIndex byLeftChild ->
      if PrimArray.sizeofPrimArray children == 2
        then
          BinaryChildTupleIndex $
            IntMap.update
              (nonEmptyIntMap . removeIntSetAtIntKey (PrimArray.indexPrimArray children 1) dependent)
              (PrimArray.indexPrimArray children 0)
              byLeftChild
        else childTupleIndex
    NaryChildTupleIndex byChildren ->
      NaryChildTupleIndex (removeIntSetAtMapKey (StoredChildTupleKey children) dependent byChildren)
{-# INLINE deleteChildTupleIndex #-}

insertIntSetAtKey :: Int -> Int -> IntMap IntSet -> IntMap IntSet
insertIntSetAtKey key dependent =
  IntMap.insertWith IntSet.union key (IntSet.singleton dependent)
{-# INLINE insertIntSetAtKey #-}

removeIntSetAtIntKey :: Int -> Int -> IntMap IntSet -> IntMap IntSet
removeIntSetAtIntKey key dependent =
  IntMap.update (nonEmptyIntSet . IntSet.delete dependent) key
{-# INLINE removeIntSetAtIntKey #-}

removeIntSetAtMapKey :: Ord key => key -> Int -> Map key IntSet -> Map key IntSet
removeIntSetAtMapKey key dependent =
  Map.update (nonEmptyIntSet . IntSet.delete dependent) key
{-# INLINE removeIntSetAtMapKey #-}

nonEmptyIntSet :: IntSet -> Maybe IntSet
nonEmptyIntSet values
  | IntSet.null values = Nothing
  | otherwise = Just values
{-# INLINE nonEmptyIntSet #-}

nonEmptyIntMap :: IntMap value -> Maybe (IntMap value)
nonEmptyIntMap values
  | IntMap.null values = Nothing
  | otherwise = Just values
{-# INLINE nonEmptyIntMap #-}

insertChildColumnValueIndex :: Int -> PrimArray Int -> ChildColumnValueIndex -> ChildColumnValueIndex
insertChildColumnValueIndex =
  alterChildColumnValueIndex insertIntSetAtKey
{-# INLINE insertChildColumnValueIndex #-}

deleteChildColumnValueIndex :: Int -> PrimArray Int -> ChildColumnValueIndex -> ChildColumnValueIndex
deleteChildColumnValueIndex =
  alterChildColumnValueIndex removeIntSetAtIntKey
{-# INLINE deleteChildColumnValueIndex #-}

alterChildColumnValueIndex ::
  (Int -> Int -> IntMap IntSet -> IntMap IntSet) ->
  Int ->
  PrimArray Int ->
  ChildColumnValueIndex ->
  ChildColumnValueIndex
alterChildColumnValueIndex alterValueRows rowKey children childColumnIndexes =
  case childColumnIndexes of
    NullaryChildColumnValueIndex ->
      childColumnIndexes
    UnaryChildColumnValueIndex values ->
      if PrimArray.sizeofPrimArray children == 1
        then UnaryChildColumnValueIndex (alterValueRows (PrimArray.indexPrimArray children 0) rowKey values)
        else childColumnIndexes
    BinaryChildColumnValueIndex leftValues rightValues ->
      if PrimArray.sizeofPrimArray children == 2
        then
          BinaryChildColumnValueIndex
            (alterValueRows (PrimArray.indexPrimArray children 0) rowKey leftValues)
            (alterValueRows (PrimArray.indexPrimArray children 1) rowKey rightValues)
        else childColumnIndexes
    NaryChildColumnValueIndex values ->
      if PrimArray.sizeofPrimArray children == SmallArray.sizeofSmallArray values
        then NaryChildColumnValueIndex (snd (mapAccumL alterColumnAt 0 values))
        else childColumnIndexes
  where
    alterColumnAt !childIndex childValueRows =
      ( childIndex + 1,
        alterValueRows
          (PrimArray.indexPrimArray children childIndex)
          rowKey
          childValueRows
      )
{-# INLINE alterChildColumnValueIndex #-}

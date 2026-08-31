module Moonlight.Core.Term.Database.OperatorTable where

import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Sequence qualified as Seq
import Moonlight.Core.Term.Database.Index
import Moonlight.Core.Term.Database.Types
import Prelude

operatorRowChunkCapacity :: Int
-- One sealed column occupies 4 KiB on the measured 64-bit target. The
-- unsealed suffix therefore remains strictly bounded without forcing small
-- batches through column transposition.
operatorRowChunkCapacity =
  512

emptyOperatorRowStore :: OperatorRowStore
emptyOperatorRowStore =
  OperatorRowStore
    { sealedRowChunks = Seq.empty,
      pendingStoredRows = Seq.empty,
      tombstonedRowIds = IntSet.empty
    }

emptyOperatorTable :: Foldable f => f () -> OperatorTable f
emptyOperatorTable shape =
  OperatorTable
    { opShape = shape,
      opArity = arity,
      rowStore = emptyOperatorRowStore,
      nextRowId = 0,
      derivedIndexWatermark = 0,
      resultIx = emptyResultIndex,
      childColumnIx = emptyChildColumnValueIndex arity,
      exactIx = emptyExactIndex arity,
      exactResultIx = emptyExactResultIndex arity,
      childUserIx = emptyChildUserIndex
    }
  where
    arity = length (toList shape)
{-# INLINE emptyOperatorTable #-}

operatorTableFromRows :: Foldable f => f () -> [DatabaseRow] -> OperatorTable f
operatorTableFromRows shape rows =
  OperatorTable
    { opShape = shape,
      opArity = arity,
      rowStore = operatorRowStoreFromRows arity rows,
      nextRowId = rowCount,
      derivedIndexWatermark = rowCount,
      resultIx =
        foldl'
          (\resultIndex (rowKey, row) ->
             insertResultIndex rowKey (rowResult row) resultIndex)
          emptyResultIndex
          indexedRows,
      childColumnIx =
        foldl'
          (\childColumnIndexes (rowKey, row) ->
             insertChildColumnValueIndex rowKey (rowChildrenArray row) childColumnIndexes)
          (emptyChildColumnValueIndex arity)
          indexedRows,
      exactIx =
        foldl'
          (\exactIndex (rowKey, row) ->
             insertExactIndex rowKey (rowChildrenArray row) exactIndex)
          (emptyExactIndex arity)
          indexedRows,
      exactResultIx =
        foldl'
          (\exactResultIndex (_rowKey, row) ->
             insertExactResultIndex (rowResult row) (rowChildrenArray row) exactResultIndex)
          (emptyExactResultIndex arity)
          indexedRows,
      childUserIx =
        foldl'
          (\childUsers (rowKey, row) ->
             PrimArray.foldrPrimArray
               (\childKey -> insertChildUserIndex rowKey childKey)
               childUsers
               (rowChildrenArray row))
          emptyChildUserIndex
          indexedRows
    }
  where
    arity = length (toList shape)
    rowCount = length rows
    indexedRows = zip (ascendingRowKeys rowCount) rows
{-# INLINE operatorTableFromRows #-}

operatorRowStoreFromRows :: Int -> [DatabaseRow] -> OperatorRowStore
operatorRowStoreFromRows arity =
  foldl' (\store row -> appendStoredRow arity row store) emptyOperatorRowStore

appendStoredRow :: Int -> DatabaseRow -> OperatorRowStore -> OperatorRowStore
appendStoredRow arity row store
  | Seq.length appendedPendingRows == operatorRowChunkCapacity =
      store
        { sealedRowChunks =
            sealedRowChunks store Seq.|> operatorRowChunkFromRows arity appendedPendingRows,
          pendingStoredRows = Seq.empty
        }
  | otherwise =
      store
        { pendingStoredRows = appendedPendingRows
        }
  where
    appendedPendingRows =
      pendingStoredRows store Seq.|> row
{-# INLINE appendStoredRow #-}

operatorRowChunkFromRows :: Int -> Seq.Seq DatabaseRow -> OperatorRowChunk
operatorRowChunkFromRows arity rows =
  OperatorRowChunk
    { chunkResults =
        PrimArray.primArrayFromListN rowCount (fmap rowResult rowList),
      chunkChildren =
        SmallArray.smallArrayFromList
          (fmap childColumn (ascendingRowKeys arity))
    }
  where
    rowList =
      toList rows
    rowCount =
      length rowList
    childColumn childIndex =
      PrimArray.primArrayFromListN (length childValues) childValues
      where
        childValues =
          mapMaybe (childValueAt childIndex . rowChildrenArray) rowList
{-# INLINE operatorRowChunkFromRows #-}

tableLiveRows :: OperatorTable f -> RowIdSet
tableLiveRows table =
  RowIdSet
    ( IntSet.difference
        (IntSet.fromDistinctAscList (ascendingRowKeys (nextRowId table)))
        (tombstonedRowIds (rowStore table))
    )
{-# INLINE tableLiveRows #-}

tableLiveRowCount :: OperatorTable f -> Int
tableLiveRowCount table =
  nextRowId table - IntSet.size (tombstonedRowIds (rowStore table))
{-# INLINE tableLiveRowCount #-}

applyOperatorTableRowEdit :: OperatorTableRowEdit -> OperatorTable f -> OperatorTable f
applyOperatorTableRowEdit edit table =
  case edit of
    InsertOperatorTableRow row ->
      table
        { exactResultIx =
            insertExactResultIndex (rowResult row) (rowChildrenArray row) (exactResultIx table)
        }
    DeleteOperatorTableRow rowKey row ->
      let tableWithoutEagerRow =
            table
              { exactResultIx =
                  deleteExactResultIndex (rowResult row) (rowChildrenArray row) (exactResultIx table)
              }
       in if rowKey < derivedIndexWatermark table
            then deleteRowFromDerivedIndexes rowKey row tableWithoutEagerRow
            -- Rows at or above the watermark were never derived-indexed, and
            -- refresh reads the post-delete live row store, so it cannot
            -- restore them.
            else tableWithoutEagerRow
{-# INLINE applyOperatorTableRowEdit #-}

insertRowIntoDerivedIndexes :: Int -> DatabaseRow -> OperatorTable f -> OperatorTable f
insertRowIntoDerivedIndexes rowKey row table =
  table
    { resultIx =
        insertResultIndex rowKey (rowResult row) (resultIx table),
      childColumnIx =
        insertChildColumnValueIndex rowKey (rowChildrenArray row) (childColumnIx table),
      exactIx =
        insertExactIndex rowKey (rowChildrenArray row) (exactIx table),
      childUserIx =
        PrimArray.foldrPrimArray
          (insertChildUserIndex rowKey)
          (childUserIx table)
          (rowChildrenArray row)
    }
{-# INLINE insertRowIntoDerivedIndexes #-}

deleteRowFromDerivedIndexes :: Int -> DatabaseRow -> OperatorTable f -> OperatorTable f
deleteRowFromDerivedIndexes rowKey row table =
  table
    { resultIx =
        deleteResultIndex rowKey (rowResult row) (resultIx table),
      childColumnIx =
        deleteChildColumnValueIndex rowKey (rowChildrenArray row) (childColumnIx table),
      exactIx =
        deleteExactIndex rowKey (rowChildrenArray row) (exactIx table),
      childUserIx =
        PrimArray.foldrPrimArray
          (deleteChildUserIndex rowKey)
          (childUserIx table)
          (rowChildrenArray row)
    }
{-# INLINE deleteRowFromDerivedIndexes #-}

ensureDerivedIndexes :: OperatorTable f -> OperatorTable f
ensureDerivedIndexes table
  | derivedIndexWatermark table >= nextRowId table =
      table
  | otherwise =
      ( foldUnindexedOperatorTableRowsWithId
          (\indexedTable rowKey row -> insertRowIntoDerivedIndexes rowKey row indexedTable)
          table
          table
      )
        { derivedIndexWatermark = nextRowId table
        }
{-# INLINE ensureDerivedIndexes #-}

foldOperatorTableRowsWithId ::
  (result -> Int -> DatabaseRow -> result) ->
  result ->
  OperatorTable f ->
  result
foldOperatorTableRowsWithId step initialResult table =
  foldOperatorTableRowsFromId 0 step initialResult table
{-# INLINE foldOperatorTableRowsWithId #-}

foldUnindexedOperatorTableRowsWithId ::
  (result -> Int -> DatabaseRow -> result) ->
  result ->
  OperatorTable f ->
  result
foldUnindexedOperatorTableRowsWithId step initialResult table =
  foldOperatorTableRowsFromId (derivedIndexWatermark table) step initialResult table
{-# INLINE foldUnindexedOperatorTableRowsWithId #-}

foldOperatorTableRowsFromId ::
  Int ->
  (result -> Int -> DatabaseRow -> result) ->
  result ->
  OperatorTable f ->
  result
foldOperatorTableRowsFromId firstRowKey step initialResult table =
  foldl'
    (\result (RowId rowKey, row) -> step result rowKey row)
    initialResult
    (dropWhile ((< firstRowKey) . unRowId . fst) (operatorTableRows table))
{-# INLINE foldOperatorTableRowsFromId #-}

unindexedRowIdsWhere :: (DatabaseRow -> Bool) -> OperatorTable f -> RowIdSet
unindexedRowIdsWhere predicate table =
  RowIdSet $
    foldUnindexedOperatorTableRowsWithId
      (\matchingRowIds rowKey row ->
         if predicate row
           then IntSet.insert rowKey matchingRowIds
           else matchingRowIds)
      IntSet.empty
      table
{-# INLINE unindexedRowIdsWhere #-}

insertEncodedRow :: DatabaseRow -> OperatorTable f -> OperatorTable f
insertEncodedRow row table
  | encodedRowPresent row table = table
  | otherwise = appendEncodedRow row table
{-# INLINE insertEncodedRow #-}

insertEncodedRowsWithInsertedRows :: [DatabaseRow] -> OperatorTable f -> ([(RowId, DatabaseRow)], OperatorTable f)
insertEncodedRowsWithInsertedRows rows table =
  case indexedAcceptedRows of
    [] ->
      ([], table)
    _ ->
      ( fmap (\(rowKey, row) -> (RowId rowKey, row)) indexedAcceptedRows,
        table
          { rowStore =
              foldl'
                (\store (_rowKey, row) -> appendStoredRow (opArity table) row store)
                (rowStore table)
                indexedAcceptedRows,
            nextRowId =
              acceptedNextRowId acceptance,
            exactResultIx =
              acceptedExactResultIndex acceptance
          }
      )
  where
    acceptance =
      acceptEncodedRows rows table
    indexedAcceptedRows =
      reverse (acceptedReversedRows acceptance)
{-# INLINE insertEncodedRowsWithInsertedRows #-}

type EncodedRowAcceptance :: Type
data EncodedRowAcceptance = EncodedRowAcceptance
  { acceptedReversedRows :: ![(Int, DatabaseRow)],
    acceptedNextRowId :: !Int,
    acceptedExactResultIndex :: !ExactResultIndex
  }

acceptEncodedRows :: [DatabaseRow] -> OperatorTable f -> EncodedRowAcceptance
acceptEncodedRows rows table =
  foldl'
    acceptEncodedRow
    ( EncodedRowAcceptance
        { acceptedReversedRows = [],
          acceptedNextRowId = nextRowId table,
          acceptedExactResultIndex = exactResultIx table
        }
    )
    rows
  where
    acceptEncodedRow ::
      EncodedRowAcceptance ->
      DatabaseRow ->
      EncodedRowAcceptance
    acceptEncodedRow acceptance row
      | IntSet.member
          (rowResult row)
          (lookupExactResultIndex (rowChildrenArray row) (acceptedExactResultIndex acceptance)) =
          acceptance
      | otherwise =
          acceptance
            { acceptedReversedRows =
                (rowKey, row) : acceptedReversedRows acceptance,
              acceptedNextRowId = rowKey + 1,
              acceptedExactResultIndex =
                insertExactResultIndex
                  (rowResult row)
                  (rowChildrenArray row)
                  (acceptedExactResultIndex acceptance)
            }
      where
        rowKey =
          acceptedNextRowId acceptance
{-# INLINE acceptEncodedRows #-}

appendEncodedRow :: DatabaseRow -> OperatorTable f -> OperatorTable f
appendEncodedRow row table =
  applyOperatorTableRowEdit (InsertOperatorTableRow row) $
    table
      { rowStore = appendStoredRow (opArity table) row (rowStore table),
        nextRowId = rowKey + 1
      }
  where
    rowKey = nextRowId table
{-# INLINE appendEncodedRow #-}

encodedRowPresent :: DatabaseRow -> OperatorTable f -> Bool
encodedRowPresent row table =
  IntSet.member
    (rowResult row)
    (lookupExactResultIndex (rowChildrenArray row) (exactResultIx table))
{-# INLINE encodedRowPresent #-}

operatorTableRows :: OperatorTable f -> [(RowId, DatabaseRow)]
operatorTableRows table =
  concat (toList sealedRowsByChunk) <> pendingRows
  where
    store =
      rowStore table
    tombstones =
      tombstonedRowIds store
    sealedRowsByChunk =
      Seq.mapWithIndex
        (\chunkIndex ->
           operatorRowChunkRowsAt
             (chunkIndex * operatorRowChunkCapacity)
             (opArity table)
             tombstones)
        (sealedRowChunks store)
    pendingRows =
      mapMaybe pendingRow (zip pendingRowKeys (toList (pendingStoredRows store)))
    pendingRowKeys =
      ascendingRowKeysBetween (sealedRowCount table) (nextRowId table)
    pendingRow (rowKey, row)
      | IntSet.member rowKey tombstones =
          Nothing
      | otherwise =
          Just (RowId rowKey, row)

operatorRowChunkRowsAt :: Int -> Int -> IntSet -> OperatorRowChunk -> [(RowId, DatabaseRow)]
operatorRowChunkRowsAt firstRowKey arity tombstones chunk
  | SmallArray.sizeofSmallArray childColumns /= arity =
      []
  | not (all ((== rowCount) . PrimArray.sizeofPrimArray) (smallArrayToList childColumns)) =
      []
  | otherwise =
      fmap rowAt liveRowOffsets
  where
    resultColumn =
      chunkResults chunk
    childColumns =
      chunkChildren chunk
    rowCount =
      PrimArray.sizeofPrimArray resultColumn
    liveRowOffsets =
      filter
        (\rowOffset -> not (IntSet.member (firstRowKey + rowOffset) tombstones))
        (ascendingRowKeys rowCount)
    rowAt rowOffset =
      ( RowId (firstRowKey + rowOffset),
        DatabaseRow
          { rowResult = PrimArray.indexPrimArray resultColumn rowOffset,
            rowChildrenArray =
              PrimArray.generatePrimArray arity $ \childIndex ->
                PrimArray.indexPrimArray
                  (SmallArray.indexSmallArray childColumns childIndex)
                  rowOffset
          }
      )
{-# INLINE operatorRowChunkRowsAt #-}

operatorTableRowsForResultKey :: Int -> OperatorTable f -> [(RowId, DatabaseRow)]
operatorTableRowsForResultKey resultKey table =
  rowsForIds table (operatorTableRowIdsForResultKeys (IntSet.singleton resultKey) table)

operatorTableRowIdsForResultKeys :: IntSet -> OperatorTable f -> RowIdSet
operatorTableRowIdsForResultKeys resultKeys table =
  rowIdSetUnion indexedRowIds unindexedRowIds
  where
    indexedRowIds =
      RowIdSet
        (intMapDependentsOfMany (IntSet.toAscList resultKeys) (derivedResultIndex table))
    unindexedRowIds =
      unindexedRowIdsWhere
        (\row -> IntSet.member (rowResult row) resultKeys)
        table
{-# INLINE operatorTableRowIdsForResultKeys #-}

lookupChildResultKeys :: PrimArray Int -> OperatorTable f -> IntSet
lookupChildResultKeys childKeys table =
  lookupExactResultIndex childKeys (exactResultIx table)

rowsForIds :: OperatorTable f -> RowIdSet -> [(RowId, DatabaseRow)]
rowsForIds table rowIds =
  mapMaybe rowForId (rowIdSetToAscList rowIds)
  where
    rowForId rowKey =
      fmap (\row -> (RowId rowKey, row)) (operatorTableRowAt rowKey table)

deleteEncodedRows :: DatabaseRow -> OperatorTable f -> OperatorTable f
deleteEncodedRows row table
  | encodedRowPresent row table =
      rowIdSetFoldl'
        (flip deleteEncodedRow)
        indexedTable
        ( rowIdSetIntersection
            (lookupResultIndex (rowResult row) (derivedResultIndex indexedTable))
            (lookupExactIndexArray (rowChildrenArray row) (derivedExactIndex indexedTable))
        )
  | otherwise =
      table
  where
    indexedTable =
      ensureDerivedIndexes table

deleteEncodedRow :: Int -> OperatorTable f -> OperatorTable f
deleteEncodedRow rowKey table =
  maybe table (\row -> deleteStoredRow rowKey row table) (operatorTableRowAt rowKey table)

deleteStoredRow :: Int -> DatabaseRow -> OperatorTable f -> OperatorTable f
deleteStoredRow rowKey row table =
  applyOperatorTableRowEdit (DeleteOperatorTableRow rowKey row) $
    table
      { rowStore =
          (rowStore table)
            { tombstonedRowIds =
                IntSet.insert rowKey (tombstonedRowIds (rowStore table))
            }
      }

operatorTableRowAt :: Int -> OperatorTable f -> Maybe DatabaseRow
operatorTableRowAt rowKey table
  | rowKey < 0 || rowKey >= nextRowId table =
      Nothing
  | IntSet.member rowKey (tombstonedRowIds (rowStore table)) =
      Nothing
  | rowKey < sealedRowCount table =
      sealedRowChunkAt rowKey table >>= operatorRowChunkRowAt (opArity table) (rowKey `mod` operatorRowChunkCapacity)
  | otherwise =
      Seq.lookup (rowKey - sealedRowCount table) (pendingStoredRows (rowStore table))
{-# INLINE operatorTableRowAt #-}

operatorTableResultAt :: Int -> OperatorTable f -> Maybe Int
operatorTableResultAt rowKey table
  | rowKey < 0 || rowKey >= nextRowId table =
      Nothing
  | IntSet.member rowKey (tombstonedRowIds (rowStore table)) =
      Nothing
  | rowKey < sealedRowCount table =
      sealedRowChunkAt rowKey table >>= operatorRowChunkResultAt (rowKey `mod` operatorRowChunkCapacity)
  | otherwise =
      rowResult <$> Seq.lookup (rowKey - sealedRowCount table) (pendingStoredRows (rowStore table))
{-# INLINE operatorTableResultAt #-}

sealedRowCount :: OperatorTable f -> Int
sealedRowCount table =
  Seq.length (sealedRowChunks (rowStore table)) * operatorRowChunkCapacity
{-# INLINE sealedRowCount #-}

sealedRowChunkAt :: Int -> OperatorTable f -> Maybe OperatorRowChunk
sealedRowChunkAt rowKey table =
  Seq.lookup
    (rowKey `div` operatorRowChunkCapacity)
    (sealedRowChunks (rowStore table))
{-# INLINE sealedRowChunkAt #-}

operatorRowChunkRowAt :: Int -> Int -> OperatorRowChunk -> Maybe DatabaseRow
operatorRowChunkRowAt arity rowOffset chunk
  | rowOffset < 0 || rowOffset >= rowCount =
      Nothing
  | SmallArray.sizeofSmallArray childColumns /= arity =
      Nothing
  | not (all ((== rowCount) . PrimArray.sizeofPrimArray) (smallArrayToList childColumns)) =
      Nothing
  | otherwise =
      Just
        DatabaseRow
          { rowResult = PrimArray.indexPrimArray resultColumn rowOffset,
            rowChildrenArray =
              PrimArray.generatePrimArray arity $ \childIndex ->
                PrimArray.indexPrimArray
                  (SmallArray.indexSmallArray childColumns childIndex)
                  rowOffset
          }
  where
    resultColumn =
      chunkResults chunk
    childColumns =
      chunkChildren chunk
    rowCount =
      PrimArray.sizeofPrimArray resultColumn
{-# INLINE operatorRowChunkRowAt #-}

operatorRowChunkResultAt :: Int -> OperatorRowChunk -> Maybe Int
operatorRowChunkResultAt rowOffset chunk =
  if rowOffset < 0 || rowOffset >= PrimArray.sizeofPrimArray results
    then Nothing
    else Just (PrimArray.indexPrimArray results rowOffset)
  where
    results =
      chunkResults chunk
{-# INLINE operatorRowChunkResultAt #-}

ascendingRowKeys :: Int -> [Int]
ascendingRowKeys count =
  ascendingRowKeysBetween 0 count
{-# INLINE ascendingRowKeys #-}

ascendingRowKeysBetween :: Int -> Int -> [Int]
ascendingRowKeysBetween firstRowKey endRowKey
  | firstRowKey >= endRowKey = []
  | otherwise = [max 0 firstRowKey .. endRowKey - 1]
{-# INLINE ascendingRowKeysBetween #-}

childValueAt :: Int -> PrimArray Int -> Maybe Int
childValueAt childIndex children =
  if childIndex < 0 || childIndex >= PrimArray.sizeofPrimArray children
    then Nothing
    else Just (PrimArray.indexPrimArray children childIndex)
{-# INLINE childValueAt #-}

keepNonEmptyTable :: OperatorTable f -> Maybe (OperatorTable f)
keepNonEmptyTable table
  | tableLiveRowCount table == 0 = Nothing
  | otherwise = Just table

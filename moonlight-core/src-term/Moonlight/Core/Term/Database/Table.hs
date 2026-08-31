{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Core.Term.Database.Table where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database.Encode
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Types
import Prelude

insertTuple ::
  (DenseKey key, Language f) =>
  key ->
  f key ->
  Database f key ->
  Database f key
insertTuple resultValue tupleValue =
  insertEncodedDatabaseRow
    (extractOperator tupleValue)
    (encodedRow resultValue tupleValue)
{-# INLINE insertTuple #-}

insertTuples ::
  forall key f.
  (DenseKey key, Language f) =>
  [(key, f key)] ->
  Database f key ->
  Database f key
insertTuples entries database =
  snd (insertTuplesWithInsertedRows entries database)

insertTuplesWithInsertedRows ::
  forall key f.
  (DenseKey key, Language f) =>
  [(key, f key)] ->
  Database f key ->
  (IntMap [(RowId, DatabaseRow)], Database f key)
insertTuplesWithInsertedRows entries database =
  IntMap.foldlWithKey'
    insertOperatorRows
    (IntMap.empty, internedDatabase)
    rowsByOperatorId
  where
    (internedDatabase, rowsByOperatorId) =
      foldl'
        collectRowsByOperatorId
        (database, IntMap.empty)
        entries

    insertOperatorRows ::
      (IntMap [(RowId, DatabaseRow)], Database f key) ->
      Int ->
      (Operator f, [DatabaseRow]) ->
      (IntMap [(RowId, DatabaseRow)], Database f key)
    insertOperatorRows (insertedRows, currentDatabase) operatorId (operator, reversedRows) =
      ( insertOperatorInsertedRows operatorId operatorInsertedRows insertedRows,
        updatedDatabase
      )
      where
        (operatorInsertedRows, updatedDatabase) =
          insertEncodedDatabaseRowsWithInsertedRows
            operatorId
            operator
            (reverse reversedRows)
            currentDatabase

    collectRowsByOperatorId ::
      (Database f key, IntMap (Operator f, [DatabaseRow])) ->
      (key, f key) ->
      (Database f key, IntMap (Operator f, [DatabaseRow]))
    collectRowsByOperatorId (currentDatabase, operatorRows) (resultValue, tupleValue) =
      ( internedOperatorDatabase,
        IntMap.insertWith
          prependOperatorRows
          operatorId
          (operator, [encodedRow resultValue tupleValue])
          operatorRows
      )
      where
        operator =
          extractOperator tupleValue
        (operatorId, internedOperatorDatabase) =
          internOperator operator currentDatabase

    prependOperatorRows ::
      (Operator f, [DatabaseRow]) ->
      (Operator f, [DatabaseRow]) ->
      (Operator f, [DatabaseRow])
    prependOperatorRows (_newOperator, newRows) (operator, existingRows) =
      (operator, newRows <> existingRows)

    insertOperatorInsertedRows ::
      Int ->
      [(RowId, DatabaseRow)] ->
      IntMap [(RowId, DatabaseRow)] ->
      IntMap [(RowId, DatabaseRow)]
    insertOperatorInsertedRows operatorId rows insertedRows =
      case rows of
        [] ->
          insertedRows
        _ ->
          IntMap.insert operatorId rows insertedRows

insertEncodedDatabaseRow ::
  (Foldable f, forall a. Ord a => Ord (f a)) =>
  Operator f ->
  DatabaseRow ->
  Database f key ->
  Database f key
insertEncodedDatabaseRow operator@(Operator shape) row database =
  case nextRowId updatedTable == nextRowId table of
    True ->
      internedDatabase
    False ->
      invalidateOperatorArrangements operatorId $
        internedDatabase
          { operatorTables =
              IntMap.insert operatorId updatedTable (operatorTables internedDatabase)
          }
  where
    (operatorId, internedDatabase) =
      internOperator operator database
    table =
      IntMap.findWithDefault
        (emptyOperatorTable shape)
        operatorId
        (operatorTables internedDatabase)
    updatedTable =
      insertEncodedRow row table
{-# INLINE insertEncodedDatabaseRow #-}

insertEncodedDatabaseRowsWithInsertedRows ::
  Foldable f =>
  Int ->
  Operator f ->
  [DatabaseRow] ->
  Database f key ->
  ([(RowId, DatabaseRow)], Database f key)
insertEncodedDatabaseRowsWithInsertedRows _operatorId _operator [] database =
  ([], database)
insertEncodedDatabaseRowsWithInsertedRows operatorId (Operator shape) rows database =
  case insertedRows of
    [] ->
      ([], database)
    _ ->
      ( insertedRows,
        invalidateOperatorArrangements operatorId $
          database
            { operatorTables =
                IntMap.insert operatorId updatedTable (operatorTables database)
            }
      )
  where
    table =
      IntMap.findWithDefault
        (emptyOperatorTable shape)
        operatorId
        (operatorTables database)
    (insertedRows, updatedTable) =
      insertEncodedRowsWithInsertedRows rows table

internOperator ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  Database f key ->
  (Int, Database f key)
internOperator operator database =
  case Map.lookup operator (operatorIds database) of
    Just operatorId ->
      (operatorId, database)
    Nothing ->
      ( operatorId,
        database
          { operatorIds = Map.insert operator operatorId (operatorIds database),
            operatorShapes = IntMap.insert operatorId operator (operatorShapes database),
            nextOperatorId = operatorId + 1
          }
      )
      where
        operatorId =
          nextOperatorId database
{-# INLINE internOperator #-}

deleteRow ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  RowId ->
  Database f key ->
  Database f key
deleteRow operator (RowId rowKey) database =
  deleteOperatorRows operator (deleteEncodedRow rowKey) database

deleteTuple ::
  (DenseKey key, Language f) =>
  key ->
  f key ->
  Database f key ->
  Database f key
deleteTuple resultValue tupleValue database =
  deleteOperatorRows operator (deleteEncodedRows row) database
  where
    operator =
      extractOperator tupleValue
    row =
      encodedRow resultValue tupleValue

deleteOperatorRows ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  (OperatorTable f -> OperatorTable f) ->
  Database f key ->
  Database f key
deleteOperatorRows operator deleteRows database =
  case operatorIdFor operator database of
    Nothing ->
      database
    Just operatorId ->
      deleteOperatorRowsByOperatorId operatorId deleteRows database

deleteOperatorRowsByOperatorId ::
  Int ->
  (OperatorTable f -> OperatorTable f) ->
  Database f key ->
  Database f key
deleteOperatorRowsByOperatorId operatorId deleteRows database =
  case IntMap.lookup operatorId (operatorTables database) of
    Nothing ->
      database
    Just table ->
      let updatedTable =
            deleteRows table
       in if tableLiveRowCount updatedTable == tableLiveRowCount table
            then database
            else
              invalidateOperatorArrangements operatorId $
                database
                  { operatorTables =
                      IntMap.update
                        (const (keepNonEmptyTable updatedTable))
                        operatorId
                        (operatorTables database)
                  }

ensureOperatorDerivedIndexes ::
  Int ->
  Database f key ->
  Database f key
ensureOperatorDerivedIndexes operatorId database =
  case IntMap.lookup operatorId (operatorTables database) of
    Just table
      | derivedIndexWatermark table /= nextRowId table ->
          database
            { operatorTables =
                IntMap.insert
                  operatorId
                  (ensureDerivedIndexes table)
                  (operatorTables database)
            }
    _ ->
      database
{-# INLINE ensureOperatorDerivedIndexes #-}

invalidateOperatorArrangements :: Int -> Database f key -> Database f key
invalidateOperatorArrangements operatorId database =
  case IntMap.null (arrangements database) of
    True ->
      database
    False ->
      database
        { arrangements =
            IntMap.delete operatorId (arrangements database)
        }
{-# INLINE invalidateOperatorArrangements #-}

compact :: Foldable f => Database f key -> Database f key
compact database =
  database
    { operatorTables =
        IntMap.mapMaybe compactOperatorTable (operatorTables database),
      arrangements = IntMap.empty
    }
{-# INLINE compact #-}

compactOperatorTable :: Foldable f => OperatorTable f -> Maybe (OperatorTable f)
compactOperatorTable table =
  keepNonEmptyTable (operatorTableFromRows (opShape table) (snd <$> operatorTableRows table))
{-# INLINE compactOperatorTable #-}

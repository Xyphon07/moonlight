{-# LANGUAGE ScopedTypeVariables #-}

-- | Canonicalisation of a term 'Database' under a key-renaming, plus entry
-- enumeration and tuple rehydration.
module Moonlight.Core.Term.Database.Canonicalize
  ( canonicalizeDatabase,
    canonicalizeDirtyRows,
    canonicalizeDirtyRowsInEditor,
  )
where

import Control.Monad.ST (ST)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database
  ( Database,
    DatabaseEditor,
    DatabaseRow,
    DatabaseRowDelta (..),
    RowId,
    RowIdSet,
    TermCommitResult (..),
    TermCommand (..),
    rowChildren,
    rowResult,
    rowEntry,
    editorQueueCommands,
    editorSnapshot,
    commitTermCommands,
    mapRowKeys,
    normalizeTermCommands,
    runCommandTransaction,
  )
import Moonlight.Core.Term.Database.Projection
  ( dirtyRowsForKeysByOperatorId,
    operatorMapFromShapes,
    operatorRowsByOperatorId,
    operatorRowsForIdsByOperatorId,
  )
import Moonlight.Core.Term.Database.Types (operatorShapes)
import Prelude

-- | Rewrite every key in the database through the supplied renaming in a
-- single pass.
--
-- A congruence-closed renaming returns the rewritten database as 'Right'. Rows
-- that collide after rewriting return the residual congruence unions as
-- 'Left'; the partially rewritten database is deliberately not published.
-- Callers holding an open union-find should discharge those commands and
-- re-invoke with the updated renaming.
canonicalizeDatabase ::
  (DenseKey key, Language f) =>
  (key -> key) ->
  Database f key ->
  Either [TermCommand f key] (Database f key)
canonicalizeDatabase canonicalize db =
  case runCommandTransaction db (canonicalizeDirtyRowsInEditor (allKeys db) canonicalize) of
    (_result, [], canonicalDatabase) ->
      Right canonicalDatabase
    (_result, residualCommands, _partiallyCanonicalDatabase) ->
      Left residualCommands

canonicalizeDirtyRows ::
  (DenseKey key, Language f) =>
  IntSet ->
  (key -> key) ->
  Database f key ->
  (DatabaseRowDelta f, [TermCommand f key], Database f key)
canonicalizeDirtyRows dirtyKeys canonicalize db =
  canonicalizationResult plan (commitTermCommands (planCommands plan) db)
  where
    plan =
      canonicalizationPlan dirtyKeys canonicalize db

canonicalizeDirtyRowsInEditor ::
  (DenseKey key, Language f) =>
  IntSet ->
  (key -> key) ->
  DatabaseEditor s f key ->
  ST s (DatabaseRowDelta f, [TermCommand f key])
canonicalizeDirtyRowsInEditor dirtyKeys canonicalize editor = do
  snapshot <- editorSnapshot editor
  let plan =
        canonicalizationPlan dirtyKeys canonicalize snapshot
  commitResult <- editorQueueCommands (planCommands plan) editor
  let (delta, commands, _canonicalDb) =
        canonicalizationResult plan commitResult
  pure (delta, commands)

type CanonicalizationPlan :: (Type -> Type) -> Type -> Type
data CanonicalizationPlan f key = CanonicalizationPlan
  { deletedRowsByOperatorId :: !(IntMap [(RowId, DatabaseRow)]),
    planCommands :: ![TermCommand f key]
  }

canonicalizationPlan ::
  (DenseKey key, Language f) =>
  IntSet ->
  (key -> key) ->
  Database f key ->
  CanonicalizationPlan f key
canonicalizationPlan dirtyKeys canonicalize db =
  CanonicalizationPlan
    { deletedRowsByOperatorId = dirtyRows,
      planCommands =
        deleteRowCommands db dirtyRows
          <> insertRowCommands db candidateInsertedRows
    }
  where
    dirtyRows =
      rowsSelectedBy (dirtyRowsForKeysByOperatorId db dirtyKeys) db

    candidateInsertedRows =
      fmap (fmap (canonicalizeRow canonicalize . snd)) dirtyRows

canonicalizationResult ::
  (Ord key, Language f) =>
  CanonicalizationPlan f key ->
  TermCommitResult f key ->
  (DatabaseRowDelta f, [TermCommand f key], Database f key)
canonicalizationResult plan commitResult =
  (delta, commands, committedDatabase commitResult)
  where
    delta =
      DatabaseRowDelta
        { rowsDeleted =
            operatorMapFromShapes
              (committedDatabase commitResult)
              (deletedRowsByOperatorId plan),
          rowsInserted = insertedRows commitResult
        }

    commands =
      normalizeTermCommands (planCommands plan <> residualCommands commitResult)

deleteRowCommands ::
  forall f key.
  Database f key ->
  IntMap [(RowId, DatabaseRow)] ->
  [TermCommand f key]
deleteRowCommands database =
  IntMap.foldMapWithKey operatorDeleteCommands
  where
    operatorDeleteCommands ::
      Int ->
      [(RowId, DatabaseRow)] ->
      [TermCommand f key]
    operatorDeleteCommands operatorId rows =
      maybe
        []
        (\operator -> fmap (DeleteRow operator . fst) rows)
        (IntMap.lookup operatorId (operatorShapes database))

insertRowCommands ::
  forall key f.
  (DenseKey key, Language f) =>
  Database f key ->
  IntMap [DatabaseRow] ->
  [TermCommand f key]
insertRowCommands database =
  IntMap.foldMapWithKey operatorInsertCommands
  where
    operatorInsertCommands ::
      Int ->
      [DatabaseRow] ->
      [TermCommand f key]
    operatorInsertCommands operatorId rows =
      maybe
        []
        (\operator -> mapMaybe (fmap (uncurry InsertTerm) . rowEntry operator) rows)
        (IntMap.lookup operatorId (operatorShapes database))

rowsSelectedBy ::
  IntMap RowIdSet ->
  Database f key ->
  IntMap [(RowId, DatabaseRow)]
rowsSelectedBy selectedRows db =
  IntMap.mapWithKey
    (\operatorId rowIds -> operatorRowsForIdsByOperatorId operatorId rowIds db)
    selectedRows

canonicalizeRow :: DenseKey key => (key -> key) -> DatabaseRow -> DatabaseRow
canonicalizeRow canonicalize row =
  mapRowKeys canonicalizeKey row
  where
    canonicalizeKey =
      encodeDenseKey . canonicalize . decodeDenseKey

allKeys :: Database f key -> IntSet
allKeys db =
  IntSet.fromList
    [ key
      | rows <- IntMap.elems (operatorRowsByOperatorId db),
        (_rowId, row) <- rows,
        key <- rowResult row : rowChildren row
    ]

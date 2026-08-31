{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Transaction where

import Control.Monad.ST (ST, runST)
import Data.Functor (void)
import Data.STRef
  ( modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,
  )
import Moonlight.Core.DenseKey (DenseKey)
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database.Command
import Moonlight.Core.Term.Database.Table
import Moonlight.Core.Term.Database.Types
import Prelude

runCommandTransaction ::
  (Ord key, Language f) =>
  Database f key ->
  (forall s. DatabaseEditor s f key -> ST s result) ->
  (result, [TermCommand f key], Database f key)
runCommandTransaction =
  runTransactionWithCommands normalizeTermCommands
{-# INLINE runCommandTransaction #-}

runTransactionWithCommands ::
  ([TermCommand f key] -> [TermCommand f key]) ->
  Database f key ->
  (forall s. DatabaseEditor s f key -> ST s result) ->
  (result, [TermCommand f key], Database f key)
runTransactionWithCommands normalizeResidualCommands database action =
  runST $ do
    workingRef <- newSTRef database
    residualRef <- newSTRef []
    controlRef <- newSTRef CommitDatabaseTransaction
    let editor =
          DatabaseEditor
            { workingRef = workingRef,
              residualCommandsRef = residualRef,
              controlRef = controlRef
            }
    result <- action editor
    control <- readSTRef controlRef
    workingDatabase <- readSTRef workingRef
    residualCommands <- readSTRef residualRef
    let (committedCommands, committedDatabase) =
          case control of
            CommitDatabaseTransaction ->
              (normalizeResidualCommands residualCommands, workingDatabase)
            AbortDatabaseTransaction ->
              ([], database)
    pure (result, committedCommands, committedDatabase)
{-# INLINE runTransactionWithCommands #-}

editorSnapshot :: DatabaseEditor s f key -> ST s (Database f key)
editorSnapshot =
  readSTRef . workingRef
{-# INLINE editorSnapshot #-}

editorQueueCommands ::
  (DenseKey key, Language f) =>
  [TermCommand f key] ->
  DatabaseEditor s f key ->
  ST s (TermCommitResult f key)
editorQueueCommands commands editor =
  modifyEditorWithResiduals editor (commitTermCommands commands)
{-# INLINE editorQueueCommands #-}

editorInsertTuple ::
  (DenseKey key, Language f) =>
  key ->
  f key ->
  DatabaseEditor s f key ->
  ST s ()
editorInsertTuple resultValue tupleValue editor =
  void (editorQueueCommands [InsertTerm resultValue tupleValue] editor)
{-# INLINE editorInsertTuple #-}

editorDeleteRow ::
  (forall a. Ord a => Ord (f a)) =>
  Operator f ->
  RowId ->
  DatabaseEditor s f key ->
  ST s ()
editorDeleteRow operator rowId editor =
  modifyEditor editor (deleteRow operator rowId)
{-# INLINE editorDeleteRow #-}

editorDeleteTuple ::
  (DenseKey key, Language f) =>
  key ->
  f key ->
  DatabaseEditor s f key ->
  ST s ()
editorDeleteTuple resultValue tupleValue editor =
  modifyEditor editor (deleteTuple resultValue tupleValue)
{-# INLINE editorDeleteTuple #-}

editorReplaceTuple ::
  (DenseKey key, Language f) =>
  key ->
  f key ->
  key ->
  f key ->
  DatabaseEditor s f key ->
  ST s ()
editorReplaceTuple oldResult oldTuple newResult newTuple editor =
  void $
    modifyEditorWithResiduals
      editor
      (commitTermCommands [InsertTerm newResult newTuple] . deleteTuple oldResult oldTuple)
{-# INLINE editorReplaceTuple #-}

editorCompact :: Foldable f => DatabaseEditor s f key -> ST s ()
editorCompact editor =
  modifyEditor editor compact
{-# INLINE editorCompact #-}

abortTransaction :: DatabaseEditor s f key -> ST s ()
abortTransaction editor =
  writeSTRef (controlRef editor) AbortDatabaseTransaction
{-# INLINE abortTransaction #-}

modifyEditor :: DatabaseEditor s f key -> (Database f key -> Database f key) -> ST s ()
modifyEditor editor update =
  modifySTRef' (workingRef editor) update
{-# INLINE modifyEditor #-}

modifyEditorWithResiduals ::
  DatabaseEditor s f key ->
  (Database f key -> TermCommitResult f key) ->
  ST s (TermCommitResult f key)
modifyEditorWithResiduals editor update = do
  database <- readSTRef (workingRef editor)
  let commitResult = update database
  writeSTRef (workingRef editor) (committedDatabase commitResult)
  appendEditorResiduals editor (residualCommands commitResult)
  pure commitResult
{-# INLINE modifyEditorWithResiduals #-}

appendEditorResiduals :: DatabaseEditor s f key -> [TermCommand f key] -> ST s ()
appendEditorResiduals editor residualCommands =
  modifySTRef'
    (residualCommandsRef editor)
    (residualCommands <>)
{-# INLINE appendEditorResiduals #-}

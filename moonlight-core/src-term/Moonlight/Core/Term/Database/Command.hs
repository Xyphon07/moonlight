{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Command where

import Data.List qualified as List
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database.Lookup
import Moonlight.Core.Term.Database.Projection
import Moonlight.Core.Term.Database.Table
import Moonlight.Core.Term.Database.Types
import Prelude

normalizeTermCommands ::
  (Ord key, Language f) =>
  [TermCommand f key] ->
  [TermCommand f key]
normalizeTermCommands =
  deduplicateSortedTermCommands
    . List.sortBy compareTermCommand
    . mapMaybe normalizeTermCommand

commitTermCommands ::
  forall key f.
  (DenseKey key, Language f) =>
  [TermCommand f key] ->
  Database f key ->
  TermCommitResult f key
commitTermCommands commands database =
  TermCommitResult
    { residualCommands =
        normalizeTermCommands (residualCommands insertCommit <> unionCommands),
      insertedRows =
        insertedRows insertCommit,
      committedDatabase =
        committedDatabase insertCommit
    }
  where
    (deletedDatabase, insertCommands, unionCommands) =
      foldl'
        collectTermCommand
        (database, [], [])
        (normalizeTermCommands commands)

    insertCommit =
      commitInsertCommands (reverse insertCommands) deletedDatabase

    collectTermCommand ::
      (Database f key, [(key, f key)], [TermCommand f key]) ->
      TermCommand f key ->
      (Database f key, [(key, f key)], [TermCommand f key])
    collectTermCommand (currentDatabase, inserts, unions) command =
      case command of
        DeleteRow operator rowId ->
          (deleteRow operator rowId currentDatabase, inserts, unions)
        InsertTerm resultValue tupleValue ->
          (currentDatabase, (resultValue, tupleValue) : inserts, unions)
        UnionResults {} ->
          (currentDatabase, inserts, command : unions)
{-# INLINE commitTermCommands #-}

commitInsertCommands ::
  forall key f.
  (DenseKey key, Language f) =>
  [(key, f key)] ->
  Database f key ->
  TermCommitResult f key
commitInsertCommands [] database =
  TermCommitResult
    { residualCommands = [],
      insertedRows = Map.empty,
      committedDatabase = database
    }
commitInsertCommands inserts database =
  TermCommitResult
    { residualCommands = residualCommands,
      insertedRows = operatorMapFromShapes insertedDatabase insertedRowsByOperatorId,
      committedDatabase = insertedDatabase
    }
  where
    (residualCommands, _tupleOwners) =
      foldl'
        collectInsertCommand
        ([], Map.empty)
        inserts

    (insertedRowsByOperatorId, insertedDatabase) =
      insertTuplesWithInsertedRows inserts database

    collectInsertCommand ::
      ([TermCommand f key], Map (f key) (Set.Set key)) ->
      (key, f key) ->
      ([TermCommand f key], Map (f key) (Set.Set key))
    collectInsertCommand (residualSoFar, tupleOwners) (resultValue, tupleValue) =
      ( Set.foldr (consUnionResult resultValue) residualSoFar owners,
        Map.insert tupleValue (Set.insert resultValue owners) tupleOwners
      )
      where
        owners =
          Map.findWithDefault
            (lookupTupleOwnerSet tupleValue database)
            tupleValue
            tupleOwners

    consUnionResult :: key -> key -> [TermCommand f key] -> [TermCommand f key]
    consUnionResult resultValue owner =
      (UnionResults resultValue owner :)

lookupTupleOwnerSet ::
  (DenseKey key, Language f) =>
  f key ->
  Database f key ->
  Set.Set key
lookupTupleOwnerSet tupleValue database
  | IntMap.null (operatorTables database) = Set.empty
  | otherwise =
      case lookupTupleAll tupleValue database of
        TupleMissing ->
          Set.empty
        TupleUnique owner ->
          Set.singleton owner
        TupleAmbiguous owners ->
          Set.fromList (toList owners)

normalizeTermCommand :: Ord key => TermCommand f key -> Maybe (TermCommand f key)
normalizeTermCommand command =
  case command of
    UnionResults left right
      | left == right ->
          Nothing
      | right < left ->
          Just (UnionResults right left)
      | otherwise ->
          Just command
    _ ->
      Just command
{-# INLINE normalizeTermCommand #-}

deduplicateSortedTermCommands ::
  forall key f.
  (Ord key, Language f) =>
  [TermCommand f key] ->
  [TermCommand f key]
deduplicateSortedTermCommands =
  foldr insertIfDifferent []
  where
    insertIfDifferent ::
      TermCommand f key ->
      [TermCommand f key] ->
      [TermCommand f key]
    insertIfDifferent command deduplicated@(nextCommand : _)
      | compareTermCommand command nextCommand == EQ =
          deduplicated
      | otherwise =
          command : deduplicated
    insertIfDifferent command [] =
      [command]
{-# INLINE deduplicateSortedTermCommands #-}

compareTermCommand ::
  (Ord key, Language f) =>
  TermCommand f key ->
  TermCommand f key ->
  Ordering
compareTermCommand left right =
  case left of
    DeleteRow leftOperator leftRow ->
      case right of
        DeleteRow rightOperator rightRow ->
          compare leftOperator rightOperator <> compare leftRow rightRow
        InsertTerm {} ->
          LT
        UnionResults {} ->
          LT
    InsertTerm leftResult leftTuple ->
      case right of
        DeleteRow {} ->
          GT
        InsertTerm rightResult rightTuple ->
          compare leftResult rightResult <> compare leftTuple rightTuple
        UnionResults {} ->
          LT
    UnionResults leftA leftB ->
      case right of
        DeleteRow {} ->
          GT
        InsertTerm {} ->
          GT
        UnionResults rightA rightB ->
          compare leftA rightA <> compare leftB rightB
{-# INLINE compareTermCommand #-}

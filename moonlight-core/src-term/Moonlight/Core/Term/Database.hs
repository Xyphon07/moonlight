{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A relational term database: one encoded row table per operator. Rows own
-- facts; indexes are row-id/result access paths. Per operator, one always-current
-- exact map (children to results) carries insert dedup; the other indices defer
-- behind a private row-id watermark and fold in on first read. The public
-- monotone commit frontier is 'TermCommitResult.insertedRows'; the watermark is
-- merely index-refresh bookkeeping. Query arrangements are prefix tries, forced
-- per level and cached on the threaded 'Database'. Shape: egg's rebuilding
-- (POPL 2021), egglog's functional tables (PLDI 2023), Free Join's lazy column
-- tries (SIGMOD 2023).
module Moonlight.Core.Term.Database
  ( Operator (..),
    extractOperator,
    RowId,
    RowIdSet,
    DatabaseRow,
    rowResult,
    rowChildren,
    mapRowKeys,
    DatabaseRowDelta (..),
    TermCommitResult (..),
    TermCommand (..),
    Column (..),
    ArrangementKey,
    ArrangementValidationError (..),
    arrangementKeyForOperator,
    arrangementKeyColumns,
    ArrangementPrefix,
    arrangementPrefixForKey,
    ArrangementNode (..),
    Arrangement (..),
    RelationStats (..),
    QueryVar (..),
    QueryTerm (..),
    QueryAtom (..),
    FreeJoinPlan (..),
    FreeJoinStrategy (..),
    QueryBinding (..),
    PatternFreeJoinPlan (..),
    Database,
    DatabaseEditor,
    TupleLookup (..),
    emptyDatabase,
    runCommandTransaction,
    editorSnapshot,
    editorQueueCommands,
    editorInsertTuple,
    editorDeleteRow,
    editorDeleteTuple,
    editorReplaceTuple,
    editorCompact,
    abortTransaction,
    insertTuple,
    insertTuples,
    deleteRow,
    deleteTuple,
    normalizeTermCommands,
    commitTermCommands,
    compact,
    dirtyRowsForKeys,
    operatorRows,
    resultKeys,
    resultKeysUsingAnyChildKey,
    operatorRowsForIds,
    rowsForOperator,
    rowsForResultKeys,
    arrangementRowsForPrefix,
    relationStats,
    compilePatternFreeJoinPlan,
    compilePatternsFreeJoinPlan,
    freeJoinStrategy,
    freeJoin,
    rowEntry,
    databaseEntries,
    entriesForResultKey,
    rehydrateTuple,
    lookupTupleAll,
    lookupTupleUnique,
    lookupLeastTuple,
  )
where

import Moonlight.Core.Term.Database.Arrangement
import Moonlight.Core.Term.Database.Command
import Moonlight.Core.Term.Database.FreeJoin
import Moonlight.Core.Term.Database.Lookup
import Moonlight.Core.Term.Database.Pattern
import Moonlight.Core.Term.Database.Projection
import Moonlight.Core.Term.Database.Table
import Moonlight.Core.Term.Database.Transaction
import Moonlight.Core.Term.Database.Types

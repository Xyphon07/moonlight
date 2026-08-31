{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.FreeJoin where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing, mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database.Arrangement qualified as Arrangement
import Moonlight.Core.Term.Database.Atom
import Moonlight.Core.Term.Database.Index
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Table
import Moonlight.Core.Term.Database.Types
import Prelude

data ResolvedQueryAtom f key = ResolvedQueryAtom
  { resolvedQueryAtom :: !(QueryAtom f key),
    resolvedQueryAtomOperatorId :: !(Maybe Int)
  }

freeJoin ::
  (DenseKey key, Language f) =>
  FreeJoinPlan f key ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
freeJoin plan database =
  let resolvedAtoms =
        resolveFreeJoinAtoms database plan
      refreshedDatabase =
        ensurePlanDerivedIndexes resolvedAtoms database
   in case freeJoinStrategy plan of
        FreeJoinEmptyConjunction ->
          Right ([QueryBinding Map.empty], refreshedDatabase)
        FreeJoinExactAtomProbe ->
          exactAtomFreeJoin resolvedAtoms refreshedDatabase
        FreeJoinGenericIntersection ->
          genericFreeJoin resolvedAtoms refreshedDatabase
{-# INLINE freeJoin #-}

resolveFreeJoinAtoms ::
  Language f =>
  Database f key ->
  FreeJoinPlan f key ->
  [ResolvedQueryAtom f key]
resolveFreeJoinAtoms database =
  fmap (resolveQueryAtom database) . freeJoinAtoms
{-# INLINE resolveFreeJoinAtoms #-}

resolveQueryAtom ::
  Language f =>
  Database f key ->
  QueryAtom f key ->
  ResolvedQueryAtom f key
resolveQueryAtom database atom =
  ResolvedQueryAtom
    { resolvedQueryAtom = atom,
      resolvedQueryAtomOperatorId = operatorIdFor (atomOperator atom) database
    }
{-# INLINE resolveQueryAtom #-}

ensurePlanDerivedIndexes ::
  [ResolvedQueryAtom f key] ->
  Database f key ->
  Database f key
ensurePlanDerivedIndexes resolvedAtoms database =
  IntSet.foldl'
    (flip ensureOperatorDerivedIndexes)
    database
    (IntSet.fromList (mapMaybe resolvedQueryAtomOperatorId resolvedAtoms))
{-# INLINE ensurePlanDerivedIndexes #-}

freeJoinStrategy :: FreeJoinPlan f key -> FreeJoinStrategy
freeJoinStrategy plan =
  case freeJoinAtoms plan of
    [] ->
      FreeJoinEmptyConjunction
    [atom]
      | all termLiteralBound (atomChildren atom) ->
          FreeJoinExactAtomProbe
    _ ->
      FreeJoinGenericIntersection
{-# INLINE freeJoinStrategy #-}

termLiteralBound :: QueryTerm key -> Bool
termLiteralBound term =
  case term of
    QueryBound _key ->
      True
    QueryVariable _variable ->
      False
{-# INLINE termLiteralBound #-}

exactAtomFreeJoin ::
  (DenseKey key, Language f) =>
  [ResolvedQueryAtom f key] ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
exactAtomFreeJoin resolvedAtoms database =
  case resolvedAtoms of
    [resolvedAtom] -> do
      let emptyBinding :: QueryBinding key
          emptyBinding =
            QueryBinding Map.empty
          atom =
            resolvedQueryAtom resolvedAtom
      (candidateRows, arrangedDatabase) <-
        atomCandidateRows resolvedAtom emptyBinding database
      pure (mapMaybe (atomBindRow atom emptyBinding . snd) candidateRows, arrangedDatabase)
    _ ->
      genericFreeJoin resolvedAtoms database
{-# INLINE exactAtomFreeJoin #-}

genericFreeJoin ::
  (DenseKey key, Language f) =>
  [ResolvedQueryAtom f key] ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
genericFreeJoin resolvedAtoms database = do
  (joinedBindings, joinedDatabase) <-
    bindVariables
      resolvedAtoms
      (orderedVariables database resolvedAtoms)
      [QueryBinding Map.empty]
      database
  filterBindings resolvedAtoms joinedBindings joinedDatabase
{-# INLINE genericFreeJoin #-}

strictJoinTraverse ::
  (Database f key -> input -> Either err (Database f key, output)) ->
  Database f key ->
  [input] ->
  Either err (Database f key, [output])
strictJoinTraverse step database inputs =
  fmap
    (\(finalDatabase, reversedOutputs) -> (finalDatabase, reverse reversedOutputs))
    (foldM collect (database, []) inputs)
  where
    collect (!currentDatabase, !reversedOutputs) input = do
      (!nextDatabase, output) <- step currentDatabase input
      pure (nextDatabase, output : reversedOutputs)
{-# INLINE strictJoinTraverse #-}

bindVariables ::
  (DenseKey key, Language f) =>
  [ResolvedQueryAtom f key] ->
  [QueryVar] ->
  [QueryBinding key] ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
bindVariables resolvedAtoms variables bindings database =
  foldM bindVariable (bindings, database) variables
  where
    bindVariable (!currentBindings, !currentDatabase) variable = do
      (nextDatabase, extendedBindings) <-
        strictJoinTraverse
          (extendVariable resolvedAtoms variable)
          currentDatabase
          currentBindings
      let !nextBindings = concat extendedBindings
      pure (nextBindings, nextDatabase)
{-# INLINE bindVariables #-}

extendVariable ::
  (DenseKey key, Language f) =>
  [ResolvedQueryAtom f key] ->
  QueryVar ->
  Database f key ->
  QueryBinding key ->
  Either ArrangementValidationError (Database f key, [QueryBinding key])
extendVariable resolvedAtoms variable database binding = do
  (candidateKeys, candidateDatabase) <-
    variableCandidates resolvedAtoms variable binding database
  pure $
    case Map.lookup variable (queryBindingAssignments binding) of
      Just boundValue ->
        (candidateDatabase, [binding | IntSet.member (encodeDenseKey boundValue) candidateKeys])
      Nothing ->
        (candidateDatabase, mapMaybe (\key -> bindVariableValue variable key binding) (IntSet.toAscList candidateKeys))
{-# INLINE extendVariable #-}

variableCandidates ::
  (DenseKey key, Language f) =>
  [ResolvedQueryAtom f key] ->
  QueryVar ->
  QueryBinding key ->
  Database f key ->
  Either ArrangementValidationError (IntSet, Database f key)
variableCandidates resolvedAtoms variable binding database = do
  (arrangedDatabase, candidateSets) <-
    strictJoinTraverse
      (atomVariableCandidates variable binding)
      database
      (atomsForVariable variable resolvedAtoms)
  pure (intersectCandidateSets candidateSets, arrangedDatabase)
{-# INLINE variableCandidates #-}

atomVariableCandidates ::
  (DenseKey key, Language f) =>
  QueryVar ->
  QueryBinding key ->
  Database f key ->
  ResolvedQueryAtom f key ->
  Either ArrangementValidationError (Database f key, IntSet)
atomVariableCandidates variable binding database resolvedAtom = do
  (candidateRows, arrangedDatabase) <-
    atomCandidateRows resolvedAtom binding database
  pure
    ( arrangedDatabase,
      IntSet.fromList
        [ encodeDenseKey variableValue
          | (_rowId, row) <- candidateRows,
            Just nextBinding <- [atomBindRow (resolvedQueryAtom resolvedAtom) binding row],
            Just variableValue <- [Map.lookup variable (queryBindingAssignments nextBinding)]
        ]
    )
{-# INLINE atomVariableCandidates #-}

intersectCandidateSets :: [IntSet] -> IntSet
intersectCandidateSets candidateSets =
  case candidateSets of
    [] ->
      IntSet.empty
    firstSet : restSets ->
      foldl' IntSet.intersection firstSet restSets
{-# INLINE intersectCandidateSets #-}

filterBindings ::
  (DenseKey key, Language f) =>
  [ResolvedQueryAtom f key] ->
  [QueryBinding key] ->
  Database f key ->
  Either ArrangementValidationError ([QueryBinding key], Database f key)
filterBindings resolvedAtoms bindings database = do
  (checkedDatabase, checkedBindings) <-
    strictJoinTraverse (filterBinding resolvedAtoms) database bindings
  pure (catMaybes checkedBindings, checkedDatabase)
{-# INLINE filterBindings #-}

filterBinding ::
  (DenseKey key, Language f) =>
  [ResolvedQueryAtom f key] ->
  Database f key ->
  QueryBinding key ->
  Either ArrangementValidationError (Database f key, Maybe (QueryBinding key))
filterBinding resolvedAtoms database binding = do
  (checkedDatabase, satisfied) <-
    strictJoinTraverse
      (atomSatisfied binding)
      database
      resolvedAtoms
  pure (checkedDatabase, if and satisfied then Just binding else Nothing)
{-# INLINE filterBinding #-}

atomSatisfied ::
  (DenseKey key, Language f) =>
  QueryBinding key ->
  Database f key ->
  ResolvedQueryAtom f key ->
  Either ArrangementValidationError (Database f key, Bool)
atomSatisfied binding database resolvedAtom = do
  (candidateRows, arrangedDatabase) <-
    atomCandidateRows resolvedAtom binding database
  pure
    ( arrangedDatabase,
      any (isJust . atomBindRow (resolvedQueryAtom resolvedAtom) binding . snd) candidateRows
    )
{-# INLINE atomSatisfied #-}

orderedVariables ::
  Database f key ->
  [ResolvedQueryAtom f key] ->
  [QueryVar]
orderedVariables database resolvedAtoms =
  List.sortOn
    (variableCost database resolvedAtoms)
    (Set.toList (planVariables resolvedAtoms))
{-# INLINE orderedVariables #-}

planVariables :: [ResolvedQueryAtom f key] -> Set.Set QueryVar
planVariables =
  foldMap (atomVariables . resolvedQueryAtom)
{-# INLINE planVariables #-}

atomVariables :: QueryAtom f key -> Set.Set QueryVar
atomVariables =
  foldMap termVariables . atomTerms
{-# INLINE atomVariables #-}

termVariables :: QueryTerm key -> Set.Set QueryVar
termVariables term =
  case term of
    QueryBound _ ->
      Set.empty
    QueryVariable variable ->
      Set.singleton variable
{-# INLINE termVariables #-}

atomsForVariable :: QueryVar -> [ResolvedQueryAtom f key] -> [ResolvedQueryAtom f key]
atomsForVariable variable =
  filter (atomMentionsVariable variable . resolvedQueryAtom)
{-# INLINE atomsForVariable #-}

atomMentionsVariable :: QueryVar -> QueryAtom f key -> Bool
atomMentionsVariable variable =
  Set.member variable . atomVariables
{-# INLINE atomMentionsVariable #-}

variableCost ::
  Database f key ->
  [ResolvedQueryAtom f key] ->
  QueryVar ->
  (Int, Int, QueryVar)
variableCost database resolvedAtoms variable =
  ( minimumCandidateDistinct variableDistincts,
    negate (length variableDistincts),
    variable
  )
  where
    variableDistincts =
      [ distinct
        | resolvedAtom <- atomsForVariable variable resolvedAtoms,
          column <- atomVariableColumns variable (resolvedQueryAtom resolvedAtom),
          Just distinct <- [atomColumnDistinct database resolvedAtom column]
      ]
{-# INLINE variableCost #-}

minimumCandidateDistinct :: [Int] -> Int
minimumCandidateDistinct values =
  case values of
    [] ->
      maxBound
    firstValue : restValues ->
      foldl' min firstValue restValues
{-# INLINE minimumCandidateDistinct #-}

atomVariableColumns :: QueryVar -> QueryAtom f key -> [Column]
atomVariableColumns variable atom =
  [ column
    | (column, QueryVariable columnVariable) <- zip (atomColumns atom) (atomTerms atom),
      columnVariable == variable
  ]
{-# INLINE atomVariableColumns #-}

atomColumnDistinct :: Database f key -> ResolvedQueryAtom f key -> Column -> Maybe Int
atomColumnDistinct database resolvedAtom column =
  resolvedQueryAtomOperatorId resolvedAtom
    >>= \operatorId -> IntMap.lookup operatorId (operatorTables database)
    >>= \table -> Just (Arrangement.distinctColumnValueCount table column)
{-# INLINE atomColumnDistinct #-}

atomCandidateRows ::
  (DenseKey key, Language f) =>
  ResolvedQueryAtom f key ->
  QueryBinding key ->
  Database f key ->
  Either ArrangementValidationError ([(RowId, DatabaseRow)], Database f key)
atomCandidateRows resolvedAtom binding database =
  case resolvedQueryAtomOperatorId resolvedAtom >>= lookupOperatorTable of
    Just table
      | Just rowIds <- atomIndexedCandidateRows atom binding table ->
          Right (rowsForIds table rowIds, database)
    _ -> do
      (arrangementKey, arrangementPrefix) <-
        atomArrangementPrefix atom binding
      Arrangement.arrangementRowsForResolvedOperator
        (resolvedQueryAtomOperatorId resolvedAtom)
        (atomOperator atom)
        arrangementKey
        arrangementPrefix
        database
  where
    atom =
      resolvedQueryAtom resolvedAtom
    lookupOperatorTable operatorId =
      IntMap.lookup operatorId (operatorTables database)
{-# INLINE atomCandidateRows #-}

atomIndexedCandidateRows ::
  DenseKey key =>
  QueryAtom f key ->
  QueryBinding key ->
  OperatorTable f ->
  Maybe RowIdSet
atomIndexedCandidateRows atom binding table =
  chooseIndexedCandidateRows exactRows resultRows
  where
    exactRows =
      fmap
        (\childKeys -> lookupExactIndex childKeys (derivedExactIndex table))
        (traverse (termBoundValue binding) (atomChildren atom))

    resultRows =
      fmap
        (\resultKey -> lookupResultIndex resultKey (derivedResultIndex table))
        (termBoundValue binding (atomResult atom))
{-# INLINE atomIndexedCandidateRows #-}

chooseIndexedCandidateRows :: Maybe RowIdSet -> Maybe RowIdSet -> Maybe RowIdSet
chooseIndexedCandidateRows exactRows resultRows =
  case (exactRows, resultRows) of
    (Just leftRows, Just rightRows)
      | rowIdSetSize leftRows <= rowIdSetSize rightRows ->
          Just leftRows
      | otherwise ->
          Just rightRows
    (Just rows, Nothing) ->
      Just rows
    (Nothing, Just rows) ->
      Just rows
    (Nothing, Nothing) ->
      Nothing
{-# INLINE chooseIndexedCandidateRows #-}

atomArrangementPrefix ::
  (DenseKey key, Foldable f) =>
  QueryAtom f key ->
  QueryBinding key ->
  Either ArrangementValidationError (ArrangementKey, ArrangementPrefix)
atomArrangementPrefix atom binding = do
  arrangementKey <-
    arrangementKeyForOperator
      (atomOperator atom)
      (fmap fst boundColumns <> unboundColumns)
  arrangementPrefix <-
    arrangementPrefixForKey arrangementKey (fmap snd boundColumns)
  pure (arrangementKey, arrangementPrefix)
  where
    columnTerms =
      zip (atomColumns atom) (atomTerms atom)

    boundColumns =
      mapMaybe (boundColumn binding) columnTerms

    unboundColumns =
      fmap fst (filter (isNothing . termBoundValue binding . snd) columnTerms)
{-# INLINE atomArrangementPrefix #-}

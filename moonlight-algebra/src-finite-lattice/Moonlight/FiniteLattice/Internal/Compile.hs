{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Compile
  ( compileContextLattice,
    compileContextLatticeWith,
    contextLatticeFromClosedOrder,
    contextLatticeFromClosedOrderWith,
    singletonContextLattice,
  )
where

import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as UVector
import Moonlight.FiniteLattice.Internal.Dense
  ( compileDensePlan,
  )
import Moonlight.FiniteLattice.Internal.Index
  ( ContextIndex (..),
    contextIndexFromUniverse,
    contextIndexValueForKey,
    contextLatticeFromPlan,
    encodeRelationKeyPairs,
    firstDuplicate,
    lookupCompileKey,
    reflexiveKeyPairs,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
  )
import Moonlight.FiniteLattice.Internal.Layout
  ( checkedRelationWordCount,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( ContextPlan (..),
    ContextTotalOrderPlan (..),
  )
import Moonlight.FiniteLattice.Internal.Recognize
  ( specializedContextPlanFromDeclaredPairs,
    specializedContextPlanFromRows,
  )
import Moonlight.FiniteLattice.Internal.Relation
  ( lowerRowsFromUpperRows,
    relationRowsFromKeyPairs,
    relationRowsGenerate,
    transitiveClosureRows,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLattice (..),
    ContextCompileLimits,
    ContextLatticeCompileError (..),
    ContextOrderDecl (..),
    defaultContextCompileLimits,
  )
import Moonlight.FiniteLattice.Internal.Validate
  ( validateAntisymmetryRows,
    validateBottomLeastRows,
    validateClosedOrderRows,
    validateDeclaredUniverse,
    validateSuppliedOperations,
    validateTopGreatestRows,
  )

compileContextLattice ::
  Ord c =>
  Set c ->
  ContextOrderDecl c ->
  Either (ContextLatticeCompileError c) (ContextLattice c)
compileContextLattice =
  compileContextLatticeWith defaultContextCompileLimits

compileContextLatticeWith ::
  Ord c =>
  ContextCompileLimits ->
  Set c ->
  ContextOrderDecl c ->
  Either (ContextLatticeCompileError c) (ContextLattice c)
compileContextLatticeWith limits universe declaration = do
  validateDeclaredUniverse universe declaration
  index <- contextIndexFromUniverse universe
  declaredPairs <-
    encodeRelationKeyPairs index (codGeneratingPairs declaration)
  topKey <-
    lookupCompileKey
      index
      (codTop declaration)
      (ContextLatticeUnknownTop (codTop declaration))
  bottomKey <-
    lookupCompileKey
      index
      (codBottom declaration)
      (ContextLatticeUnknownBottom (codBottom declaration))
  plan <-
    case
      specializedContextPlanFromDeclaredPairs
        (ciSize index)
        topKey
        bottomKey
        declaredPairs
      of
      Just specializedPlan -> Right specializedPlan
      Nothing -> do
        _ <- checkedRelationWordCount limits (ciSize index)
        let initialUpperRows =
              relationRowsFromKeyPairs
                (ciSize index)
                (reflexiveKeyPairs index <> declaredPairs)
            upperRows = transitiveClosureRows initialUpperRows
        validateAntisymmetryRows index upperRows
        validateTopGreatestRows index upperRows topKey
        validateBottomLeastRows index upperRows bottomKey
        let lowerRows = lowerRowsFromUpperRows upperRows
        case
          specializedContextPlanFromRows
            (ciSize index)
            topKey
            bottomKey
            upperRows
            lowerRows
          of
          Just specializedPlan -> Right specializedPlan
          Nothing -> compileDensePlan limits index topKey bottomKey upperRows lowerRows
  pure
    ( contextLatticeFromPlan
        (codTop declaration)
        (codBottom declaration)
        topKey
        bottomKey
        index
        plan
    )

contextLatticeFromClosedOrder ::
  Ord c =>
  c ->
  c ->
  [c] ->
  (c -> c -> Bool) ->
  (c -> c -> c) ->
  (c -> c -> c) ->
  Either (ContextLatticeCompileError c) (ContextLattice c)
contextLatticeFromClosedOrder =
  contextLatticeFromClosedOrderWith defaultContextCompileLimits

contextLatticeFromClosedOrderWith ::
  Ord c =>
  ContextCompileLimits ->
  c ->
  c ->
  [c] ->
  (c -> c -> Bool) ->
  (c -> c -> c) ->
  (c -> c -> c) ->
  Either (ContextLatticeCompileError c) (ContextLattice c)
contextLatticeFromClosedOrderWith limits topValue bottomValue objects leqFn joinFn meetFn = do
  case firstDuplicate objects of
    Just duplicate -> Left (ContextLatticeDuplicateElement duplicate)
    Nothing -> Right ()
  when (null objects) (Left ContextLatticeEmptyUniverse)
  let universe = Set.fromList objects
  index <- contextIndexFromUniverse universe
  let size = ciSize index
  unless (Set.member topValue universe) (Left (ContextLatticeUnknownTop topValue))
  unless (Set.member bottomValue universe) (Left (ContextLatticeUnknownBottom bottomValue))
  _ <- checkedRelationWordCount limits size
  topKey <-
    lookupCompileKey
      index
      topValue
      (ContextLatticeUnknownTop topValue)
  bottomKey <-
    lookupCompileKey
      index
      bottomValue
      (ContextLatticeUnknownBottom bottomValue)
  let upperRows =
        relationRowsGenerate size $ \leftOrdinal rightOrdinal ->
          leqFn
            (contextIndexValueForKey index (ContextKey leftOrdinal))
            (contextIndexValueForKey index (ContextKey rightOrdinal))
  validateClosedOrderRows index upperRows topKey bottomKey
  let lowerRows = lowerRowsFromUpperRows upperRows
  plan <-
    case
      specializedContextPlanFromRows
        size
        topKey
        bottomKey
        upperRows
        lowerRows
      of
      Just specializedPlan -> Right specializedPlan
      Nothing -> compileDensePlan limits index topKey bottomKey upperRows lowerRows
  validateSuppliedOperations index plan joinFn meetFn
  pure
    ( contextLatticeFromPlan
        topValue
        bottomValue
        topKey
        bottomKey
        index
        plan
    )

singletonContextLattice :: c -> ContextLattice c
singletonContextLattice contextValue =
  ContextLattice
    { clTop = contextValue,
      clBottom = contextValue,
      clTopKey = singletonKey,
      clBottomKey = singletonKey,
      clContextsByKey = Vector.singleton contextValue,
      clKeyByContext = Map.singleton contextValue singletonKey,
      clPlan =
        TotalOrderPlan
          ContextTotalOrderPlan
            { ctoTopKey = singletonKey,
              ctoRankByKey = UVector.singleton 0,
              ctoKeyByRank = UVector.singleton 0
            },
      clSize = 1
    }
  where
    singletonKey = ContextKey 0

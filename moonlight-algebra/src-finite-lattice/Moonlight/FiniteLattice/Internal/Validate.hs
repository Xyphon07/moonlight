{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Validate
  ( validateDeclaredUniverse,
    validateClosedOrderRows,
    validateAntisymmetryRows,
    validateTopGreatestRows,
    validateBottomLeastRows,
    validateSuppliedOperations,
  )
where

import Control.Monad (unless, when)
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.FiniteLattice.Internal.Index
  ( ContextIndex (..),
    contextIndexValueForKey,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    contextKeySetFind,
    contextKeySetFindDifference,
  )
import Moonlight.FiniteLattice.Internal.Plan
  ( ContextPlan,
    contextPlanJoinKey,
    contextPlanMeetKey,
  )
import Moonlight.FiniteLattice.Internal.Relation
  ( ContextRows,
    contextKeyRelated,
    rowForRawKey,
  )
import Moonlight.FiniteLattice.Internal.Types
  ( ContextLatticeCompileError (..),
    ContextOrderDecl (..),
  )

validateDeclaredUniverse ::
  Ord c =>
  Set c ->
  ContextOrderDecl c ->
  Either (ContextLatticeCompileError c) ()
validateDeclaredUniverse universe declaration = do
  when (Set.null universe) (Left ContextLatticeEmptyUniverse)
  unless
    (Set.member (codTop declaration) universe)
    (Left (ContextLatticeUnknownTop (codTop declaration)))
  unless
    (Set.member (codBottom declaration) universe)
    (Left (ContextLatticeUnknownBottom (codBottom declaration)))
  case
    Foldable.find
      (\(lower, upper) ->
         not (Set.member lower universe && Set.member upper universe)
      )
      (codGeneratingPairs declaration)
    of
    Nothing -> Right ()
    Just pair -> Left (ContextLatticeUnknownRelationEndpoint pair)

validateClosedOrderRows ::
  ContextIndex c ->
  ContextRows ->
  ContextKey ->
  ContextKey ->
  Either (ContextLatticeCompileError c) ()
validateClosedOrderRows index upperRows topKey bottomKey = do
  validateReflexivityRows index upperRows
  validateAntisymmetryRows index upperRows
  validateTransitivityRows index upperRows
  validateTopGreatestRows index upperRows topKey
  validateBottomLeastRows index upperRows bottomKey

validateReflexivityRows ::
  ContextIndex c ->
  ContextRows ->
  Either (ContextLatticeCompileError c) ()
validateReflexivityRows index upperRows =
  case
    firstOrdinal
      (ciSize index)
      (\keyOrdinal ->
         not
           ( contextKeyRelated
               upperRows
               (ContextKey keyOrdinal)
               (ContextKey keyOrdinal)
           )
      )
    of
    Nothing -> Right ()
    Just keyOrdinal ->
      Left
        (ContextLatticeNotReflexive (contextIndexValueForKey index (ContextKey keyOrdinal)))

validateAntisymmetryRows ::
  ContextIndex c ->
  ContextRows ->
  Either (ContextLatticeCompileError c) ()
validateAntisymmetryRows index upperRows =
  checkLeft 0
  where
    size = ciSize index

    checkLeft !leftOrdinal
      | leftOrdinal >= size = Right ()
      | otherwise = checkRight leftOrdinal (leftOrdinal + 1)

    checkRight !leftOrdinal !rightOrdinal
      | rightOrdinal >= size = checkLeft (leftOrdinal + 1)
      | otherwise =
          let leftKey = ContextKey leftOrdinal
              rightKey = ContextKey rightOrdinal
           in if
                contextKeyRelated upperRows leftKey rightKey
                  && contextKeyRelated upperRows rightKey leftKey
                then
                  Left
                    ( ContextLatticeAntisymmetryViolation
                        (contextIndexValueForKey index leftKey)
                        (contextIndexValueForKey index rightKey)
                    )
                else checkRight leftOrdinal (rightOrdinal + 1)

validateTransitivityRows ::
  ContextIndex c ->
  ContextRows ->
  Either (ContextLatticeCompileError c) ()
validateTransitivityRows index upperRows =
  checkLeft 0
  where
    size = ciSize index

    checkLeft !leftOrdinal
      | leftOrdinal >= size = Right ()
      | otherwise =
          case
            contextKeySetFind
              (hasMissingSuccessor leftOrdinal)
              (rowForRawKey upperRows leftOrdinal)
            of
            Nothing -> checkLeft (leftOrdinal + 1)
            Just middleOrdinal ->
              case
                contextKeySetFindDifference
                  (rowForRawKey upperRows middleOrdinal)
                  (rowForRawKey upperRows leftOrdinal)
                of
                Nothing -> checkLeft (leftOrdinal + 1)
                Just rightOrdinal ->
                  Left
                    ( ContextLatticeNotTransitive
                        (contextIndexValueForKey index (ContextKey leftOrdinal))
                        (contextIndexValueForKey index (ContextKey middleOrdinal))
                        (contextIndexValueForKey index (ContextKey rightOrdinal))
                    )

    hasMissingSuccessor leftOrdinal middleOrdinal =
      case
        contextKeySetFindDifference
          (rowForRawKey upperRows middleOrdinal)
          (rowForRawKey upperRows leftOrdinal)
        of
        Nothing -> False
        Just _ -> True

validateTopGreatestRows ::
  ContextIndex c ->
  ContextRows ->
  ContextKey ->
  Either (ContextLatticeCompileError c) ()
validateTopGreatestRows index upperRows topKey =
  case
    firstOrdinal
      (ciSize index)
      (\keyOrdinal ->
         not (contextKeyRelated upperRows (ContextKey keyOrdinal) topKey)
      )
    of
    Nothing -> Right ()
    Just keyOrdinal ->
      Left
        ( ContextLatticeTopNotGreatest
            (contextIndexValueForKey index (ContextKey keyOrdinal))
        )

validateBottomLeastRows ::
  ContextIndex c ->
  ContextRows ->
  ContextKey ->
  Either (ContextLatticeCompileError c) ()
validateBottomLeastRows index upperRows bottomKey =
  case
    firstOrdinal
      (ciSize index)
      (\keyOrdinal ->
         not (contextKeyRelated upperRows bottomKey (ContextKey keyOrdinal))
      )
    of
    Nothing -> Right ()
    Just keyOrdinal ->
      Left
        ( ContextLatticeBottomNotLeast
            (contextIndexValueForKey index (ContextKey keyOrdinal))
        )

validateSuppliedOperations ::
  Ord c =>
  ContextIndex c ->
  ContextPlan ->
  (c -> c -> c) ->
  (c -> c -> c) ->
  Either (ContextLatticeCompileError c) ()
validateSuppliedOperations index plan joinFn meetFn =
  checkLeft 0
  where
    size = ciSize index

    checkLeft !leftOrdinal
      | leftOrdinal >= size = Right ()
      | otherwise = checkRight leftOrdinal 0

    checkRight !leftOrdinal !rightOrdinal
      | rightOrdinal >= size = checkLeft (leftOrdinal + 1)
      | otherwise = do
          let leftKey = ContextKey leftOrdinal
              rightKey = ContextKey rightOrdinal
              leftContext = contextIndexValueForKey index leftKey
              rightContext = contextIndexValueForKey index rightKey
              joined = joinFn leftContext rightContext
              met = meetFn leftContext rightContext
          joinedKey <-
            maybe
              (Left (ContextLatticeJoinOutsideUniverse leftContext rightContext joined))
              Right
              (Map.lookup joined (ciKeyByContext index))
          metKey <-
            maybe
              (Left (ContextLatticeMeetOutsideUniverse leftContext rightContext met))
              Right
              (Map.lookup met (ciKeyByContext index))
          let planJoinedKey = contextPlanJoinKey plan leftKey rightKey
              planMetKey = contextPlanMeetKey plan leftKey rightKey
          unless
            (joinedKey == planJoinedKey)
            (Left (ContextLatticeInvalidJoin leftContext rightContext joined))
          unless
            (metKey == planMetKey)
            (Left (ContextLatticeInvalidMeet leftContext rightContext met))
          checkRight leftOrdinal (rightOrdinal + 1)

firstOrdinal :: Int -> (Int -> Bool) -> Maybe Int
firstOrdinal size predicate =
  go 0
  where
    go !ordinal
      | ordinal >= size = Nothing
      | predicate ordinal = Just ordinal
      | otherwise = go (ordinal + 1)

-- | Object maps between finite thin categories, validated once against the
-- source and target reachability relations.
module Moonlight.Category.Pure.FinCat.Functor
  ( FinThinFunctor,
    FinThinFunctorValidationError (..),
    FinThinFunctorApplicationError (..),
    mkFinThinFunctor,
    finThinFunctorSource,
    finThinFunctorTarget,
    finThinFunctorObjectMap,
    applyFinThinFunctor,
  )
where

import Data.Kind (Type)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinObjectId,
    finCatExplicitMorphismMapView,
    finCatIsThin,
    finCatObjects,
  )

-- | A validated functor between finite thin categories. Thinness makes the
-- morphism action proof-irrelevant: a total object action is functorial exactly
-- when it preserves every source reachability relation.
type FinThinFunctor :: Type
data FinThinFunctor = FinThinFunctor
  { finThinFunctorSource :: !FinCat,
    finThinFunctorTarget :: !FinCat,
    finThinFunctorObjectMap :: !(Map FinObjectId FinObjectId)
  }

type FinThinFunctorValidationError :: Type
data FinThinFunctorValidationError
  = FinThinFunctorSourceNotThin
  | FinThinFunctorTargetNotThin
  | FinThinFunctorMissingSourceObject !FinObjectId
  | FinThinFunctorUnexpectedSourceObject !FinObjectId
  | FinThinFunctorTargetObjectAbsent !FinObjectId !FinObjectId
  | FinThinFunctorOrderNotPreserved !FinObjectId !FinObjectId !FinObjectId !FinObjectId
  deriving stock (Eq, Ord, Show)

type FinThinFunctorApplicationError :: Type
data FinThinFunctorApplicationError
  = FinThinFunctorUnknownSourceObject !FinObjectId
  deriving stock (Eq, Ord, Show)

-- | Check a total finite object table once and retain its thin-functor proof.
mkFinThinFunctor ::
  FinCat ->
  FinCat ->
  Map FinObjectId FinObjectId ->
  Either FinThinFunctorValidationError FinThinFunctor
mkFinThinFunctor sourceCategory targetCategory objectMap = do
  requireThin FinThinFunctorSourceNotThin sourceCategory
  requireThin FinThinFunctorTargetNotThin targetCategory
  validateObjectMapDomain
  validateObjectMapCodomain
  validateOrderPreservation
  Right
    FinThinFunctor
      { finThinFunctorSource = sourceCategory,
        finThinFunctorTarget = targetCategory,
        finThinFunctorObjectMap = objectMap
      }
  where
    sourceObjects = finCatObjects sourceCategory
    targetObjects = finCatObjects targetCategory
    suppliedSourceObjects = Map.keysSet objectMap

    validateObjectMapDomain =
      case Set.lookupMin (Set.difference sourceObjects suppliedSourceObjects) of
        Just missingObject -> Left (FinThinFunctorMissingSourceObject missingObject)
        Nothing ->
          case Set.lookupMin (Set.difference suppliedSourceObjects sourceObjects) of
            Just unexpectedObject -> Left (FinThinFunctorUnexpectedSourceObject unexpectedObject)
            Nothing -> Right ()

    validateObjectMapCodomain =
      case find (not . (`Set.member` targetObjects) . snd) (Map.toAscList objectMap) of
        Just (sourceObject, targetObject) ->
          Left (FinThinFunctorTargetObjectAbsent sourceObject targetObject)
        Nothing -> Right ()

    validateOrderPreservation =
      case findOrderViolation of
        Just (sourceObject, sourceTarget, mappedSource, mappedTarget) ->
          Left
            ( FinThinFunctorOrderNotPreserved
                sourceObject
                sourceTarget
                mappedSource
                mappedTarget
            )
        Nothing -> Right ()

    findOrderViolation =
      listToMaybe
        [ (sourceObject, targetObject, mappedSource, mappedTarget)
        | ((sourceObject, targetObject), morphisms) <- Map.toAscList sourceMorphismMap,
          not (null morphisms),
          Just mappedSource <- [Map.lookup sourceObject objectMap],
          Just mappedTarget <- [Map.lookup targetObject objectMap],
          not (targetRelates mappedSource mappedTarget)
        ]

    sourceMorphismMap = finCatExplicitMorphismMapView sourceCategory
    targetMorphismMap = finCatExplicitMorphismMapView targetCategory

    targetRelates sourceObject targetObject =
      sourceObject == targetObject
        || not
          ( null
              ( Map.findWithDefault
                  []
                  (sourceObject, targetObject)
                  targetMorphismMap
              )
          )

requireThin :: FinThinFunctorValidationError -> FinCat -> Either FinThinFunctorValidationError ()
requireThin notThinError categoryValue
  | finCatIsThin categoryValue = Right ()
  | otherwise = Left notThinError

applyFinThinFunctor ::
  FinThinFunctor ->
  FinObjectId ->
  Either FinThinFunctorApplicationError FinObjectId
applyFinThinFunctor functorValue sourceObject =
  case Map.lookup sourceObject (finThinFunctorObjectMap functorValue) of
    Just targetObject -> Right targetObject
    Nothing -> Left (FinThinFunctorUnknownSourceObject sourceObject)

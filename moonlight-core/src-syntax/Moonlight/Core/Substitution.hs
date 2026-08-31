-- | Pattern-variable substitutions ('Substitution', a map from variables to
-- class identifiers).
module Moonlight.Core.Substitution
  ( Substitution (..),
    emptySubstitution,
    insertSubst,
    lookupSubst,
    mapSubstitutionClasses,
    extendSubst,
    mergeSubstitutions,
    intersectRootedMatches,
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Core.Identifier.EGraph (ClassId, PatternVar, classIdKey, mkPatternVar, patternVarKey)
import Prelude

type Substitution :: Type
newtype Substitution = Substitution (IntMap ClassId)
  deriving stock (Eq, Ord, Show)

emptySubstitution :: Substitution
emptySubstitution =
  Substitution IntMap.empty

lookupSubst :: PatternVar -> Substitution -> Maybe ClassId
lookupSubst patternVar (Substitution entries) =
  IntMap.lookup (patternVarKey patternVar) entries

insertSubst :: PatternVar -> ClassId -> Substitution -> Substitution
insertSubst patternVar classId (Substitution entries) =
  Substitution (IntMap.insert (patternVarKey patternVar) classId entries)

mapSubstitutionClasses :: (ClassId -> ClassId) -> Substitution -> Substitution
mapSubstitutionClasses mapClassId (Substitution entries) =
  Substitution (IntMap.map mapClassId entries)

extendSubst :: PatternVar -> ClassId -> Substitution -> Maybe Substitution
extendSubst patternVar classId substitution =
  case lookupSubst patternVar substitution of
    Nothing ->
      Just (insertSubst patternVar classId substitution)
    Just existingClassId
      | existingClassId == classId ->
          Just substitution
      | otherwise ->
          Nothing

mergeSubstitutions :: Substitution -> Substitution -> Maybe Substitution
mergeSubstitutions substitution (Substitution newEntries) =
  IntMap.foldrWithKey
    ( \patternKey classId mergeRemaining currentSubstitution ->
        extendSubst (mkPatternVar patternKey) classId currentSubstitution >>= mergeRemaining
    )
    Just
    newEntries
    substitution

intersectRootedMatches :: NonEmpty.NonEmpty [(ClassId, Substitution)] -> [(ClassId, Substitution)]
intersectRootedMatches rootedMatches =
  case rootedMatches of
    initialMatches NonEmpty.:| remainingMatches ->
      foldl' intersectRootedMatchSet initialMatches remainingMatches

intersectRootedMatchSet :: [(ClassId, Substitution)] -> [(ClassId, Substitution)] -> [(ClassId, Substitution)]
intersectRootedMatchSet leftMatches rightMatches =
  leftMatches
    >>= \(leftClassId, leftSubstitution) ->
      IntMap.findWithDefault [] (classIdKey leftClassId) rightMatchesByClass
        >>= \rightSubstitution ->
          maybe
            []
            (\mergedSubstitution -> [(leftClassId, mergedSubstitution)])
            (mergeSubstitutions leftSubstitution rightSubstitution)
  where
    rightMatchesByClass =
      foldr insertRightSubstitution IntMap.empty rightMatches

    insertRightSubstitution :: (ClassId, substitution) -> IntMap.IntMap [substitution] -> IntMap.IntMap [substitution]
    insertRightSubstitution (rightClassId, rightSubstitution) =
      IntMap.insertWith (<>) (classIdKey rightClassId) [rightSubstitution]

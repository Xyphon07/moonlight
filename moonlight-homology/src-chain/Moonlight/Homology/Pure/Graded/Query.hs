module Moonlight.Homology.Pure.Graded.Query
  ( DegreeSelection (..),
    GradedAggregation (..),
    GradedQuery (..),
    selectAllDegrees,
    selectDegree,
    degreeSelectionFromMaybe,
    combineSelectedQuery,
    preserveDegreewiseQuery,
    matchesDegreeSelection,
    enumerateDegreeIndexed,
    lookupDegreeIndexed,
    selectDegreeIndexed,
    selectGradedMembers,
    countGradedMembers,
  )
where

import Data.Kind (Type)
import qualified Data.List as List
import Moonlight.Homology.Pure.Chain (HomologicalDegree (..))

type DegreeSelection :: Type
data DegreeSelection
  = SelectAllDegrees
  | SelectDegree HomologicalDegree
  deriving stock (Eq, Ord, Show, Read)

type GradedAggregation :: Type
data GradedAggregation
  = CombineSelected
  | PreserveDegreewise
  deriving stock (Eq, Ord, Show, Read)

type GradedQuery :: Type
data GradedQuery = GradedQuery
  { gradedQuerySelection :: DegreeSelection,
    gradedQueryAggregation :: GradedAggregation
  }
  deriving stock (Eq, Ord, Show, Read)

selectAllDegrees :: DegreeSelection
selectAllDegrees = SelectAllDegrees

selectDegree :: HomologicalDegree -> DegreeSelection
selectDegree = SelectDegree

degreeSelectionFromMaybe :: Maybe HomologicalDegree -> DegreeSelection
degreeSelectionFromMaybe =
  maybe selectAllDegrees selectDegree

combineSelectedQuery :: DegreeSelection -> GradedQuery
combineSelectedQuery selectionValue =
  GradedQuery
    { gradedQuerySelection = selectionValue,
      gradedQueryAggregation = CombineSelected
    }

preserveDegreewiseQuery :: DegreeSelection -> GradedQuery
preserveDegreewiseQuery selectionValue =
  GradedQuery
    { gradedQuerySelection = selectionValue,
      gradedQueryAggregation = PreserveDegreewise
    }

matchesDegreeSelection :: DegreeSelection -> HomologicalDegree -> Bool
matchesDegreeSelection selectionValue degreeValue =
  case selectionValue of
    SelectAllDegrees -> True
    SelectDegree requiredDegree -> degreeValue == requiredDegree

enumerateDegreeIndexed :: [a] -> [(HomologicalDegree, a)]
enumerateDegreeIndexed =
  zipWith (\indexValue memberValue -> (HomologicalDegree indexValue, memberValue)) [0 :: Int ..]

lookupDegreeIndexed :: HomologicalDegree -> [(HomologicalDegree, a)] -> Maybe a
lookupDegreeIndexed degreeValue =
  fmap snd . List.find ((== degreeValue) . fst)

selectDegreeIndexed :: DegreeSelection -> [(HomologicalDegree, a)] -> [a]
selectDegreeIndexed selectionValue =
  fmap snd . filter (matchesDegreeSelection selectionValue . fst)

selectGradedMembers :: (a -> HomologicalDegree) -> DegreeSelection -> [a] -> [a]
selectGradedMembers degreeOf selectionValue =
  filter (matchesDegreeSelection selectionValue . degreeOf)

countGradedMembers :: (a -> HomologicalDegree) -> DegreeSelection -> [a] -> Int
countGradedMembers degreeOf selectionValue =
  length . selectGradedMembers degreeOf selectionValue

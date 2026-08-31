module Moonlight.Homology.Pure.GradedTorsion
  ( DegreeSelection (..),
    GradedAggregation (..),
    GradedQuery (..),
    selectAllDegrees,
    selectDegree,
    degreeSelectionFromMaybe,
    combineSelectedQuery,
    preserveDegreewiseQuery,
    enumerateDegreeIndexed,
    lookupDegreeIndexed,
    selectDegreeIndexed,
    selectGradedMembers,
    countGradedMembers,
    directProductQuery,
    degreewiseUnionQuery,
    GradedTorsionFamily,
    mkGradedTorsionFamily,
    gradedTorsionPresent,
    gradedTorsionAtDegree,
    gradedTorsionCombined,
    gradedTorsionOrderSupport,
    gradedTorsionPrimaryOrderSupport,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import Moonlight.Homology.Pure.FiniteAbelian
  ( FiniteAbelianTorsion,
    finiteAbelianInvariants,
    finiteAbelianOrderSupport,
    finiteAbelianPrimaryOrderSupport,
    mkFiniteAbelianTorsion,
    normalizeTorsionOrders,
    torsionFromHomologyGroup,
  )
import Moonlight.Homology.Pure.Group (HomologyGroup (..))
import Moonlight.Homology.Pure.Chain (HomologicalDegree (..))
import Moonlight.Homology.Pure.Graded.Query
  ( DegreeSelection (..),
    GradedAggregation (..),
    GradedQuery (..),
    combineSelectedQuery,
    countGradedMembers,
    degreeSelectionFromMaybe,
    enumerateDegreeIndexed,
    lookupDegreeIndexed,
    preserveDegreewiseQuery,
    selectAllDegrees,
    selectDegree,
    selectDegreeIndexed,
    selectGradedMembers,
  )

-- | Alias of 'combineSelectedQuery' under its mathematical name; the two
-- exported names denote one function.
directProductQuery :: DegreeSelection -> GradedQuery
directProductQuery = combineSelectedQuery

-- | Alias of 'preserveDegreewiseQuery' under its mathematical name; the two
-- exported names denote one function.
degreewiseUnionQuery :: DegreeSelection -> GradedQuery
degreewiseUnionQuery = preserveDegreewiseQuery

type GradedTorsionFamily :: Type
newtype GradedTorsionFamily = GradedTorsionFamily
  { gradedTorsionEntries :: [(HomologicalDegree, FiniteAbelianTorsion)]
  }
  deriving stock (Eq, Show)

mkGradedTorsionFamily :: [HomologyGroup Integer] -> GradedTorsionFamily
mkGradedTorsionFamily homologyGroups =
  homologyGroups
    & enumerateDegreeIndexed
    & fmap (\(degreeValue, homologyGroupValue) -> (degreeValue, torsionFromHomologyGroup homologyGroupValue))
    & GradedTorsionFamily

gradedTorsionPresent :: GradedTorsionFamily -> Bool
gradedTorsionPresent =
  not . null . gradedTorsionEntries

gradedTorsionAtDegree ::
  HomologicalDegree ->
  GradedTorsionFamily ->
  Maybe FiniteAbelianTorsion
gradedTorsionAtDegree degreeValue family =
  gradedTorsionEntries family
    & lookupDegreeIndexed degreeValue

gradedTorsionCombined ::
  DegreeSelection ->
  GradedTorsionFamily ->
  FiniteAbelianTorsion
gradedTorsionCombined selectionValue =
  mkFiniteAbelianTorsion . foldMap finiteAbelianInvariants . selectTorsion selectionValue

gradedTorsionOrderSupport ::
  GradedQuery ->
  GradedTorsionFamily ->
  [Integer]
gradedTorsionOrderSupport queryValue family =
  case gradedQueryAggregation queryValue of
    CombineSelected ->
      finiteAbelianOrderSupport (gradedTorsionCombined (gradedQuerySelection queryValue) family)
    PreserveDegreewise ->
      selectTorsion (gradedQuerySelection queryValue) family
        & fmap finiteAbelianOrderSupport
        & concat
        & normalizeTorsionOrders

gradedTorsionPrimaryOrderSupport ::
  Integer ->
  GradedQuery ->
  GradedTorsionFamily ->
  Maybe [Integer]
gradedTorsionPrimaryOrderSupport primeValue queryValue family =
  case gradedQueryAggregation queryValue of
    CombineSelected ->
      finiteAbelianPrimaryOrderSupport
        primeValue
        (gradedTorsionCombined (gradedQuerySelection queryValue) family)
    PreserveDegreewise ->
      selectTorsion (gradedQuerySelection queryValue) family
        & traverse (finiteAbelianPrimaryOrderSupport primeValue)
        & fmap (normalizeTorsionOrders . concat)

selectTorsion ::
  DegreeSelection ->
  GradedTorsionFamily ->
  [FiniteAbelianTorsion]
selectTorsion selectionValue family =
  gradedTorsionEntries family
    & selectDegreeIndexed selectionValue

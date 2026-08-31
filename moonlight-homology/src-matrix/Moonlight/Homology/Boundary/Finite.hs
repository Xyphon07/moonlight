module Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    mkFiniteChainComplex,
    mkFiniteChainComplexChecked,
    maxHomologicalDegree,
    incidenceMatrixAt,
    degreeCardinality,
    basisCellNodeId,
    finiteChainBasisRefsAtDegree,
    basisIndexCellMapAtDegree,
    inverseBasisRefMap,
    validateFiniteChainComplexShape,
    restrictComplex,
  )
where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector qualified as Vector
import Moonlight.Core (Semiring)
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryIncidence,
    boundaryCoefficient,
    boundaryEntriesBySource,
    boundaryIncidenceCompositionIsZero,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    materializeIncidenceBoundary,
    sourceCardinality,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..), decrementDegree)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))

type FiniteChainComplex :: Type -> Type
data FiniteChainComplex r = FiniteChainComplex
  { maxDimension :: Int,
    incidenceMatrix :: Int -> BoundaryIncidence r
  }

mkFiniteChainComplex :: HomologicalDegree -> (HomologicalDegree -> BoundaryIncidence r) -> FiniteChainComplex r
mkFiniteChainComplex (HomologicalDegree maxDimensionValue) incidenceLookup =
  FiniteChainComplex
    { maxDimension = maxDimensionValue,
      incidenceMatrix = incidenceLookup . HomologicalDegree
    }

mkFiniteChainComplexChecked ::
  (Eq r, Num r, Semiring r) =>
  HomologicalDegree ->
  (HomologicalDegree -> BoundaryIncidence r) ->
  Either HomologyFailure (FiniteChainComplex r)
mkFiniteChainComplexChecked maxDegree incidenceLookup = do
  let finite = mkFiniteChainComplex maxDegree incidenceLookup
  validateFiniteChainComplexShape finite
  traverse_ (adjacentNilpotenceAt finite) [0 .. maxDimension finite - 1]
  pure finite

adjacentNilpotenceAt ::
  (Eq r, Num r, Semiring r) =>
  FiniteChainComplex r ->
  Int ->
  Either HomologyFailure ()
adjacentNilpotenceAt finite degreeIndex =
  boundaryIncidenceCompositionIsZero
    (incidenceMatrixAt finite (HomologicalDegree degreeIndex))
    (incidenceMatrixAt finite (HomologicalDegree (degreeIndex + 1)))
    & either
      (Left . InvalidBoundaryIncidence . show)
      ( \compositionIsZero ->
          if compositionIsZero
            then Right ()
            else Left (ChainComplexNilpotenceViolation degreeIndex)
      )

maxHomologicalDegree :: FiniteChainComplex r -> HomologicalDegree
maxHomologicalDegree =
  HomologicalDegree . maxDimension

incidenceMatrixAt :: FiniteChainComplex r -> HomologicalDegree -> BoundaryIncidence r
incidenceMatrixAt finite =
  incidenceMatrix finite . unHomologicalDegree

degreeCardinality :: FiniteChainComplex r -> HomologicalDegree -> Int
degreeCardinality finite degreeValue@(HomologicalDegree degreeIndex) =
  case maxHomologicalDegree finite of
    HomologicalDegree maxDegreeValue
      | degreeIndex < 0 -> 0
      | degreeIndex > maxDegreeValue -> 0
      | otherwise -> sourceCardinality (incidenceMatrixAt finite degreeValue)

basisCellNodeId :: FiniteChainComplex r -> BasisCellRef -> Int
basisCellNodeId finite basisCellRef =
  case cellDegree basisCellRef of
    HomologicalDegree degreeValue ->
      sum
        (fmap (degreeCardinality finite . HomologicalDegree) [0 .. degreeValue - 1])
        + cellIndex basisCellRef

finiteChainBasisRefsAtDegree :: FiniteChainComplex r -> HomologicalDegree -> [BasisCellRef]
finiteChainBasisRefsAtDegree finite degreeValue =
  fmap
    (\cellIndexValue -> BasisCellRef {cellDegree = degreeValue, cellIndex = cellIndexValue})
    [0 .. degreeCardinality finite degreeValue - 1]

basisIndexCellMapAtDegree ::
  HomologicalDegree ->
  Map.Map cell BasisCellRef ->
  Map.Map Int cell
basisIndexCellMapAtDegree degreeValue =
  Map.foldrWithKey
    ( \cellValue basisCellRef ->
        if cellDegree basisCellRef == degreeValue
          then Map.insert (cellIndex basisCellRef) cellValue
          else id
    )
    Map.empty

inverseBasisRefMap :: Map.Map cell BasisCellRef -> Map.Map BasisCellRef cell
inverseBasisRefMap =
  Map.fromList . fmap (\(cellValue, basisRef) -> (basisRef, cellValue)) . Map.toList

validateFiniteChainComplexShape :: FiniteChainComplex r -> Either HomologyFailure ()
validateFiniteChainComplexShape finite =
  dimensionsOf finite
    & mapMaybe (shapeViolation finite)
    & safeHead
    & maybe (Right ()) Left

dimensionsOf :: FiniteChainComplex r -> [HomologicalDegree]
dimensionsOf finite =
  fmap HomologicalDegree [0 .. maxDimension finite]

shapeViolation :: FiniteChainComplex r -> HomologicalDegree -> Maybe HomologyFailure
shapeViolation finite degreeValue@(HomologicalDegree degreeIndex) =
  let incidence = incidenceMatrixAt finite degreeValue
      expectedTarget =
        if degreeIndex <= 0
          then 0
          else sourceCardinality (incidenceMatrixAt finite (decrementDegree degreeValue))
   in if targetCardinality incidence == expectedTarget
        then Nothing
        else Just (ChainComplexShapeMismatch degreeIndex expectedTarget (targetCardinality incidence))

-- | Restrict a complex to the given active cells. Every retained degree is
-- materialized exactly once up front (the previous incarnation re-ran a full
-- entry-list scan per source cell on every incidence access), and any
-- materialization failure propagates as a 'Left' instead of being collapsed
-- into a silently empty boundary matrix.
restrictComplex :: Set BasisCellRef -> FiniteChainComplex Int -> Either HomologyFailure (FiniteChainComplex Int)
restrictComplex activeCells finiteComplex = do
  let retainedByDegree = groupByDegree (Set.toAscList activeCells)
      maxDimensionValue = maximum (0 : Map.keys retainedByDegree)
  restrictedBoundaries <-
    traverse
      (restrictedBoundaryAt activeCells retainedByDegree finiteComplex)
      [0 .. maxDimensionValue]
  let boundaryByDegree = Map.fromList (zip [0 .. maxDimensionValue] restrictedBoundaries)
  mkFiniteChainComplexChecked
    (HomologicalDegree maxDimensionValue)
    ( \(HomologicalDegree degreeValue) ->
        Map.findWithDefault emptyBoundaryIncidence degreeValue boundaryByDegree
    )

restrictedBoundaryAt ::
  Set BasisCellRef ->
  Map.Map Int [BasisCellRef] ->
  FiniteChainComplex Int ->
  Int ->
  Either HomologyFailure (BoundaryIncidence Int)
restrictedBoundaryAt activeCells retainedByDegree finiteComplex dimensionValue
  | dimensionValue <= 0 =
      Right
        ( emptyBoundaryIncidenceOf
            (fromIntegral (length (Map.findWithDefault [] 0 retainedByDegree)))
            0
        )
  | otherwise =
      let incidence = incidenceMatrixAt finiteComplex (HomologicalDegree dimensionValue)
          entriesBySource = boundaryEntriesBySource incidence
          restrictedEntriesOf sourceCell =
            maybe [] id (entriesBySource Vector.!? cellIndex sourceCell)
              & mapMaybe
                ( \entryValue ->
                    let targetCell =
                          BasisCellRef
                            { cellDegree = HomologicalDegree (dimensionValue - 1),
                              cellIndex = targetIndex entryValue
                            }
                     in if Set.member targetCell activeCells
                          then Just (boundaryCoefficient entryValue, targetCell)
                          else Nothing
                )
       in materializeIncidenceBoundary
            restrictedEntriesOf
            (Map.findWithDefault [] dimensionValue retainedByDegree)
            (Map.findWithDefault [] (dimensionValue - 1) retainedByDegree)

groupByDegree :: [BasisCellRef] -> Map.Map Int [BasisCellRef]
groupByDegree cellRefs =
  -- Strict left fold with O(1) prepends, then one reversal per bucket:
  -- preserves the ascending order 'Set.toAscList' delivers without either a
  -- foldr thunk spine or quadratic list appends.
  Map.map
    reverse
    ( foldl'
        (\accumulator cellRef -> Map.insertWith (<>) (unHomologicalDegree (cellDegree cellRef)) [cellRef] accumulator)
        Map.empty
        cellRefs
    )

safeHead :: [a] -> Maybe a
safeHead values =
  case values of
    firstValue : _ -> Just firstValue
    [] -> Nothing

-- | Homology interpretations of canonical labelled planar complexes.
--
-- 'PlanarComplex' owns simplex identity and downward closure. This module
-- materializes that admitted section once; Homology remains the sole owner of
-- chain-law and filtration-law validation.
module Moonlight.Planar.LabelledComplex
  ( PlanarComplexInterpretationError (..)
  , finitePlanarComplex
  , filteredPlanarComplex
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Homology.Boundary
  ( BoundaryIncidence
  , FiniteChainComplex
  , emptyBoundaryIncidence
  , emptyBoundaryIncidenceOf
  , materializeIncidenceBoundary
  , mkFiniteChainComplexChecked
  )
import Moonlight.Homology.Chain
  ( HomologicalDegree (..)
  , HomologyFailure
  )
import Moonlight.Homology.Persistence
  ( FilteredFiniteChainComplex
  , mkFilteredFiniteChainComplex
  )
import Moonlight.Homology.Topology (BasisCellRef (..))
import Moonlight.Planar.Simplex
  ( PlanarComplex
  , PlanarSimplex
  , SimplexDimension (..)
  , planarComplexBasisAt
  , planarSimplexBoundary
  )

-- | Typed obstructions at the labelled-complex/Homology boundary.
data PlanarComplexInterpretationError label
  = PlanarComplexChainInvalid !HomologyFailure
  | PlanarComplexBirthUnexpected !(PlanarSimplex label)
  | PlanarComplexFiltrationInvalid !HomologyFailure
  deriving stock (Eq, Show)

-- | Canonical integral chains in ascending labelled-simplex order.
finitePlanarComplex
  :: Ord label
  => PlanarComplex label
  -> Either (PlanarComplexInterpretationError label) (FiniteChainComplex Int)
finitePlanarComplex complexValue = do
  degreeOneBoundary <-
    first PlanarComplexChainInvalid
      ( materializeIncidenceBoundary
          planarSimplexBoundary
          (basisList SimplexDimension1 complexValue)
          (basisList SimplexDimension0 complexValue)
      )
  degreeTwoBoundary <-
    first PlanarComplexChainInvalid
      ( materializeIncidenceBoundary
          planarSimplexBoundary
          (basisList SimplexDimension2 complexValue)
          (basisList SimplexDimension1 complexValue)
      )
  let degreeZeroBoundary :: BoundaryIncidence Int
      degreeZeroBoundary =
        emptyBoundaryIncidenceOf
          (fromIntegral (Vector.length (planarComplexBasisAt SimplexDimension0 complexValue)))
          0
      boundaryAt (HomologicalDegree degreeIndex) =
        case degreeIndex of
          0 -> degreeZeroBoundary
          1 -> degreeOneBoundary
          2 -> degreeTwoBoundary
          _ -> emptyBoundaryIncidence
  first PlanarComplexChainInvalid
    (mkFiniteChainComplexChecked (HomologicalDegree 2) boundaryAt)

-- | Add exact births to the admitted complex. Homology validates uniqueness,
-- total coverage, and every boundary inequality; this interpreter deliberately
-- does not duplicate those laws.
filteredPlanarComplex
  :: (Ord label, Ord filtration)
  => PlanarComplex label
  -> Map (PlanarSimplex label) filtration
  -> Either
      (PlanarComplexInterpretationError label)
      (FilteredFiniteChainComplex filtration Int)
filteredPlanarComplex complexValue births = do
  finite <- finitePlanarComplex complexValue
  assignments <- traverse birthAssignment (Map.toAscList births)
  first PlanarComplexFiltrationInvalid
    (mkFilteredFiniteChainComplex finite assignments)
 where
  basisReferences = planarBasisReferences complexValue
  birthAssignment (simplex, birthValue) =
    maybe
      (Left (PlanarComplexBirthUnexpected simplex))
      (Right . (,birthValue))
      (Map.lookup simplex basisReferences)

planarBasisReferences
  :: Ord label
  => PlanarComplex label
  -> Map (PlanarSimplex label) BasisCellRef
planarBasisReferences complexValue =
  Map.unions
    [ referencesAt 0 SimplexDimension0
    , referencesAt 1 SimplexDimension1
    , referencesAt 2 SimplexDimension2
    ]
 where
  referencesAt degreeValue dimension =
    Map.fromDistinctAscList
      ( zipWith
          (\basisIndex simplex ->
              (simplex, BasisCellRef (HomologicalDegree degreeValue) basisIndex)
          )
          [0 ..]
          (basisList dimension complexValue)
      )

basisList
  :: SimplexDimension
  -> PlanarComplex label
  -> [PlanarSimplex label]
basisList dimension = Vector.toList . planarComplexBasisAt dimension

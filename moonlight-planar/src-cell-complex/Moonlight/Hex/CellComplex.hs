-- | Integral cellular chains derived from one native hexagonal region.
module Moonlight.Hex.CellComplex
  ( HexCellComplexError (..)
  , finiteHexCellComplex
  ) where

import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Moonlight.Hex.Coordinate (HexCoord)
import Moonlight.Hex.Element
  ( HexSide
  , HexBoundarySide
  , HexVertex
  , hexBoundaryFrom
  , hexBoundaryIdentity
  , hexCellBoundary
  , hexCellSides
  , hexCellVertices
  , hexSideEndpoints
  )
import Moonlight.Hex.Region (HexRegion, foldHexRegionCoords)
import Moonlight.Homology.Boundary
  ( BoundaryIncidence
  , FiniteChainComplex
  , emptyBoundaryIncidence
  , emptyBoundaryIncidenceOf
  , materializeIncidenceBoundary
  , mkFiniteChainComplexChecked
  )
import Moonlight.Homology.Chain (HomologicalDegree (..), HomologyFailure)

newtype HexCellComplexError = HexCellComplexInvalid HomologyFailure
  deriving stock (Eq, Show)

-- | Interpret the selected cells and their downward closure in Homology's
-- existing finite-chain carrier. No second topology representation survives.
finiteHexCellComplex :: HexRegion -> Either HexCellComplexError (FiniteChainComplex Int)
finiteHexCellComplex region = do
  degreeOne <-
    first HexCellComplexInvalid
      (materializeIncidenceBoundary edgeBoundary edgeBasis vertexBasis)
  degreeTwo <-
    first HexCellComplexInvalid
      (materializeIncidenceBoundary faceBoundary faceBasis edgeBasis)
  let degreeZero :: BoundaryIncidence Int
      degreeZero = emptyBoundaryIncidenceOf (fromIntegral (length vertexBasis)) 0
      boundaryAt (HomologicalDegree degree) = case degree of
        0 -> degreeZero
        1 -> degreeOne
        2 -> degreeTwo
        _ -> emptyBoundaryIncidence
  first HexCellComplexInvalid
    (mkFiniteChainComplexChecked (HomologicalDegree 2) boundaryAt)
 where
  faceBasis = reverse (foldHexRegionCoords (flip (:)) [] region)
  edgeBasis =
    Set.toAscList
      (Set.fromList (faceBasis >>= NonEmpty.toList . hexCellSides))
  vertexBasis =
    Set.toAscList
      (Set.fromList (faceBasis >>= NonEmpty.toList . hexCellVertices))

edgeBoundary :: HexSide -> [(Int, HexVertex)]
edgeBoundary side =
  let (from, to) = hexSideEndpoints side
   in [(-1, from), (1, to)]

faceBoundary :: HexCoord -> [(Int, HexSide)]
faceBoundary = fmap orientedCoefficient . NonEmpty.toList . hexCellBoundary
 where
  orientedCoefficient :: HexBoundarySide -> (Int, HexSide)
  orientedCoefficient boundary =
    let side = hexBoundaryIdentity boundary
        (canonicalFrom, _) = hexSideEndpoints side
        coefficient = if hexBoundaryFrom boundary == canonicalFrom then 1 else -1
     in (coefficient, side)
{-# LANGUAGE DerivingStrategies #-}

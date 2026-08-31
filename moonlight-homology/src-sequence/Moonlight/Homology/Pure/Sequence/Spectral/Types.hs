module Moonlight.Homology.Pure.Sequence.Spectral.Types
  ( FiltrationFunction,
    AmbientVector,
    FormalMap (..),
    RationalFormalMap,
    SpectralEntry (..),
    RationalSpectralEntry,
    SpectralSource,
    mkSpectralSource,
    spectralBaseComplex,
    spectralLevelsByDegree,
    spectralSupportRegistry,
    spectralMinLevel,
    spectralMaxLevel,
    SpectralSlice (..),
    SpectralChain (..),
    SpectralPage (..),
    RationalSpectralPage,
    SpectralFamily (..),
    RationalSpectralFamily,
    SpectralAdvance (..),
    SpectralCapability,
  )
where

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Kind (Type)
import qualified Data.List as List
import Moonlight.Core (Capability)
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex, incidenceMatrixAt)
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryEntry,
    boundaryCoefficient,
    boundaryEntries,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Homology.Pure.Chain (RepresentativeCocycle)
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Group (HomologyGroup)
import Moonlight.Homology.Pure.Phase (HomologyPhase, RequirePhase4)
import Moonlight.Homology.Pure.Sequence.Spectral.Bidegree (Bidegree)
import Moonlight.Homology.Pure.Sequence.Spectral.Support (SpectralSupportRegistry, SpectralWindow, mkSpectralSupportRegistry)
import Moonlight.Homology.Pure.Topology.Algebra (QuotientPresentation)
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))
import Moonlight.Homology.Pure.Filtration (enumerateFromZero)
import Moonlight.Homology.Pure.Matrix.Shape
  ( cellCountAtDegree,
    dimensionsOf,
  )
import Moonlight.Homology.Pure.Matrix.SparseLinAlg (SparseRow)

type FiltrationFunction :: Type
type FiltrationFunction = BasisCellRef -> Int

type AmbientVector :: Type
type AmbientVector = SparseRow

type FormalMap :: Type -> Type
data FormalMap r = FormalMap
  { formalMatrix :: [[r]],
    formalDomainBasis :: [RepresentativeCocycle r Int],
    formalCodomainBasis :: [RepresentativeCocycle r Int]
  }
  deriving stock (Eq, Show)

type RationalFormalMap :: Type
type RationalFormalMap = FormalMap Rational

type SpectralEntry :: Type -> Type
data SpectralEntry r = SpectralEntry
  { entryPresentation :: QuotientPresentation r,
    entryGroupValue :: HomologyGroup r
  }
  deriving stock (Eq, Show)

type RationalSpectralEntry :: Type
type RationalSpectralEntry = SpectralEntry Rational

type SpectralSource :: Type
data SpectralSource = UnsafeSpectralSource
  { spectralBaseComplex :: FiniteChainComplex Rational,
    spectralLevelsByDegree :: Map.Map HomologicalDegree [Int],
    spectralSupportRegistry :: SpectralSupportRegistry,
    spectralMinLevel :: Int,
    spectralMaxLevel :: Int
  }

mkSpectralSource :: FiniteChainComplex Rational -> FiltrationFunction -> Either HomologyFailure SpectralSource
mkSpectralSource rationalFinite filtration = do
  let levelMap = spectralLevelMap rationalFinite filtration
  validateSpectralFiltration rationalFinite levelMap
  pure
    UnsafeSpectralSource
      { spectralBaseComplex = rationalFinite,
        spectralLevelsByDegree = levelMap,
        spectralSupportRegistry = mkSpectralSupportRegistry levelMap,
        spectralMinLevel = minimumSpectralLevel levelMap,
        spectralMaxLevel = maximumSpectralLevel levelMap
      }

spectralLevelMap ::
  FiniteChainComplex Rational ->
  FiltrationFunction ->
  Map.Map HomologicalDegree [Int]
spectralLevelMap rationalFinite filtration =
  dimensionsOf rationalFinite
    & fmap
      ( \degreeValue ->
          ( degreeValue,
            enumerateFromZero (cellCountAtDegree rationalFinite degreeValue)
              & fmap
                ( \cellIndexValue ->
                    filtration
                      BasisCellRef
                        { cellDegree = degreeValue,
                          cellIndex = cellIndexValue
                        }
                )
          )
      )
    & Map.fromList

minimumSpectralLevel :: Map.Map HomologicalDegree [Int] -> Int
minimumSpectralLevel =
  extremalSpectralLevel (List.foldl' min)

maximumSpectralLevel :: Map.Map HomologicalDegree [Int] -> Int
maximumSpectralLevel =
  extremalSpectralLevel (List.foldl' max)

extremalSpectralLevel :: (Int -> [Int] -> Int) -> Map.Map HomologicalDegree [Int] -> Int
extremalSpectralLevel foldLevels levelMap =
  case Map.elems levelMap & concat of
    [] -> 0
    levelValue : remainingLevels -> foldLevels levelValue remainingLevels

validateSpectralFiltration ::
  FiniteChainComplex Rational ->
  Map.Map HomologicalDegree [Int] ->
  Either HomologyFailure ()
validateSpectralFiltration rationalFinite levelMap =
  spectralFiltrationViolations rationalFinite levelMap
    & List.find (const True)
    & maybe (Right ()) Left

spectralFiltrationViolations ::
  FiniteChainComplex Rational ->
  Map.Map HomologicalDegree [Int] ->
  [HomologyFailure]
spectralFiltrationViolations rationalFinite levelMap =
  dimensionsOf rationalFinite
    >>= spectralFiltrationViolationsAtDegree rationalFinite levelMap

spectralFiltrationViolationsAtDegree ::
  FiniteChainComplex Rational ->
  Map.Map HomologicalDegree [Int] ->
  HomologicalDegree ->
  [HomologyFailure]
spectralFiltrationViolationsAtDegree rationalFinite levelMap degreeValue =
  if unHomologicalDegree degreeValue <= 0
    then []
    else
      boundaryEntries (incidenceMatrixAt rationalFinite degreeValue)
        >>= spectralFiltrationViolationAtBoundaryEntry levelMap degreeValue

spectralFiltrationViolationAtBoundaryEntry ::
  Map.Map HomologicalDegree [Int] ->
  HomologicalDegree ->
  BoundaryEntry Rational ->
  [HomologyFailure]
spectralFiltrationViolationAtBoundaryEntry levelMap degreeValue entryValue =
  let upperCell =
        BasisCellRef
          { cellDegree = degreeValue,
            cellIndex = sourceIndex entryValue
          }
      lowerCell =
        BasisCellRef
          { cellDegree = HomologicalDegree (unHomologicalDegree degreeValue - 1),
            cellIndex = targetIndex entryValue
          }
   in if boundaryCoefficient entryValue == 0
        then []
        else
          case (levelAt levelMap lowerCell, levelAt levelMap upperCell) of
            (Just lowerLevel, Just upperLevel)
              | lowerLevel <= upperLevel -> []
              | otherwise ->
                  [FiltrationNotPreserved lowerCell upperCell lowerLevel upperLevel]
            _ ->
              [ InvalidBoundaryIncidence
                  ( "boundary incidence entry references a cell outside the spectral filtration: "
                      <> show (lowerCell, upperCell)
                  )
              ]

levelAt :: Map.Map HomologicalDegree [Int] -> BasisCellRef -> Maybe Int
levelAt levelMap basisCellRef =
  Map.lookup (cellDegree basisCellRef) levelMap
    >>= elementAt (cellIndex basisCellRef)

elementAt :: Int -> [a] -> Maybe a
elementAt indexValue values =
  if indexValue < 0
    then Nothing
    else
      case drop indexValue values of
        value : _ -> Just value
        [] -> Nothing

type SpectralSlice :: Type
data SpectralSlice = SpectralSlice
  { spectralSliceCyclesBasis :: [AmbientVector],
    spectralSliceImageBasis :: [AmbientVector],
    spectralSliceBoundariesBasis :: [AmbientVector]
  }

type SpectralChain :: Type
data SpectralChain = SpectralChain
  { spectralChainSource :: SpectralSource,
    spectralChainPageIndex :: Int,
    spectralChainPreviousSlices :: Maybe (Map.Map SpectralWindow SpectralSlice),
    spectralChainSlices :: Map.Map SpectralWindow SpectralSlice
  }

type SpectralPage :: Type -> Type
data SpectralPage r = SpectralPage
  { pageIndex :: Int,
    groupAt :: Int -> Int -> HomologyGroup r,
    diffMap :: Int -> Int -> FormalMap r,
    pageEntryMap :: Map.Map Bidegree (SpectralEntry r),
    pageDifferentialMap :: Map.Map Bidegree (FormalMap r),
    pageAdvanceSource :: Maybe SpectralSource,
    pageAdvanceState :: Maybe SpectralChain
  }

type RationalSpectralPage :: Type
type RationalSpectralPage = SpectralPage Rational

type SpectralFamily :: Type -> Type
data SpectralFamily r = SpectralFamily
  { spectralFamilyPages :: [SpectralPage r],
    spectralFamilyStableFrom :: Int,
    spectralFamilyLimitPage :: SpectralPage r
  }

type RationalSpectralFamily :: Type
type RationalSpectralFamily = SpectralFamily Rational

type SpectralAdvance :: Type -> Type
newtype SpectralAdvance r = SpectralAdvance
  { runSpectralAdvance :: SpectralPage r -> Either HomologyFailure (SpectralPage r)
  }

type SpectralCapability :: HomologyPhase -> Type -> Type
type SpectralCapability phase r =
  Capability RequirePhase4 phase (SpectralAdvance r)

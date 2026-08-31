module Moonlight.Homology.Pure.Sequence.Spectral.Build
  ( advanceSpectralPage,
    buildEntryFromBases,
    buildInitialSpectralPage,
    buildConstantFiltrationSpectralFamily,
    buildSpectralFamily,
    buildSpectralPage,
    buildSpectralPages,
    filtrationWidth,
    homologicalPageBound,
    mkRationalSpectralSource,
  )
where

import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    maxHomologicalDegree,
  )
import Moonlight.Homology.Pure.Chain (HomologicalDegree (..), RepresentativeCocycle)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Group (HomologyGroup (..))
import Moonlight.Homology.Pure.Sequence.Spectral.Bidegree
  ( Bidegree,
    bidegreeCoordinates,
    bidegreeFromTotalDegree,
    bidegreeTotalDegree,
    mkBidegree,
    targetBidegreeAfterDifferential,
  )
import Moonlight.Homology.Pure.Sequence.Spectral.Linear
  ( FiltrationPivot (..),
    FiltrationReducedColumn (..),
    FiltrationReduction (..),
    boundaryMatrixAtRational,
    filtrationOrderedReduction,
    firstVectorOutsideSpan,
    independentModuloBasis,
    reduceBasisChecked,
    reshapeSparseMatrix,
    zeroSparseMatrix,
  )
import Moonlight.Homology.Pure.Sequence.Spectral.Support
  ( supportWindowBidegree,
    supportWindows,
  )
import Moonlight.Homology.Pure.Sequence.Spectral.Types
  ( AmbientVector,
    FiltrationFunction,
    FormalMap (..),
    SpectralChain (..),
    SpectralEntry (..),
    SpectralFamily (..),
    SpectralPage (..),
    SpectralSource,
    mkSpectralSource,
    spectralBaseComplex,
    spectralLevelsByDegree,
    spectralMaxLevel,
    spectralMinLevel,
    spectralSupportRegistry,
  )
import Moonlight.Homology.Pure.Topology.Algebra
  ( QuotientPresentation,
    mkQuotientPresentation,
    vectorToRepresentative,
  )
import Moonlight.Homology.Pure.Matrix.Shape (cellCountAtDegree)
import Moonlight.Homology.Pure.Matrix.SparseLinAlg
  ( SparseMatrix,
    sparseRowToDense,
    sparseTransposeMatrix,
  )

data SpectralResolution = SpectralResolution
  { spectralResolutionSource :: !SpectralSource,
    spectralResolutionPairs :: ![SpectralPair],
    spectralResolutionUnpaired :: ![SpectralUnpairedGenerator]
  }

data SpectralDegreeReduction = SpectralDegreeReduction
  { spectralDegreePairs :: ![SpectralPair],
    spectralDegreeZeroColumns :: ![SpectralUnpairedGenerator]
  }

data SpectralPairId = SpectralPairId
  { spectralPairIdDegree :: !HomologicalDegree,
    spectralPairIdSourceIndex :: !Int
  }
  deriving stock (Eq, Ord, Show)

data SpectralPair = SpectralPair
  { spectralPairIdentifier :: !SpectralPairId,
    spectralPairSourceLevel :: !Int,
    spectralPairSourceVector :: !AmbientVector,
    spectralPairTargetDegree :: !HomologicalDegree,
    spectralPairTargetIndex :: !Int,
    spectralPairTargetLevel :: !Int,
    spectralPairTargetVector :: !AmbientVector,
    spectralPairPage :: !Int
  }

data SpectralUnpairedGenerator = SpectralUnpairedGenerator
  { spectralUnpairedDegree :: !HomologicalDegree,
    spectralUnpairedIndex :: !Int,
    spectralUnpairedLevel :: !Int,
    spectralUnpairedVector :: !AmbientVector
  }

newtype SpectralPairedTargetCell = SpectralPairedTargetCell (HomologicalDegree, Int)
  deriving stock (Eq, Ord, Show)

data SpectralEndpointRole
  = SpectralSourceEndpoint !SpectralPairId
  | SpectralTargetEndpoint !SpectralPairId
  | SpectralUnpairedEndpoint !HomologicalDegree !Int
  deriving stock (Eq, Ord, Show)

data SpectralPageBasisVector = SpectralPageBasisVector
  { spectralPageBasisRole :: !SpectralEndpointRole,
    spectralPageBasisDegree :: !HomologicalDegree,
    spectralPageBasisLevel :: !Int,
    spectralPageBasisAnchor :: !Int,
    spectralPageBasisVector :: !AmbientVector
  }
  deriving stock (Eq, Show)

buildInitialSpectralPage :: SpectralSource -> Either HomologyFailure (SpectralPage Rational)
buildInitialSpectralPage source =
  buildSpectralPage source 0

buildConstantFiltrationSpectralFamily :: SpectralSource -> Either HomologyFailure (SpectralFamily Rational)
buildConstantFiltrationSpectralFamily =
  buildSpectralFamily

buildSpectralFamily :: SpectralSource -> Either HomologyFailure (SpectralFamily Rational)
buildSpectralFamily source = do
  resolution <- buildSpectralResolution source
  let pages = spectralPagesFromResolution resolution
  case List.reverse pages of
    limitPage : _ ->
      Right
        SpectralFamily
          { spectralFamilyPages = pages,
            spectralFamilyStableFrom = stableFromResolution resolution,
            spectralFamilyLimitPage = limitPage
          }
    [] ->
      Left (BackendFailure "spectral family unexpectedly empty")

buildSpectralPages :: SpectralSource -> Either HomologyFailure [SpectralPage Rational]
buildSpectralPages source =
  spectralPagesFromResolution <$> buildSpectralResolution source

spectralPagesFromResolution :: SpectralResolution -> [SpectralPage Rational]
spectralPagesFromResolution resolution =
  fmap
    (spectralPageFromResolution resolution)
    [0 .. homologicalPageBound (spectralResolutionSource resolution)]

buildSpectralPage :: SpectralSource -> Int -> Either HomologyFailure (SpectralPage Rational)
buildSpectralPage source pageNumber =
  spectralPageFromResolution <$> buildSpectralResolution source <*> pure pageNumber

advanceSpectralPage :: SpectralPage Rational -> Either HomologyFailure (SpectralPage Rational)
advanceSpectralPage page =
  case pageAdvanceSource page of
    Just source -> buildSpectralPage source (pageIndex page + 1)
    Nothing ->
      case pageAdvanceState page of
        Just chain -> buildSpectralPage (spectralChainSource chain) (pageIndex page + 1)
        Nothing -> Left (BackendFailure "spectral page cannot advance without source data")

spectralPageFromResolution :: SpectralResolution -> Int -> SpectralPage Rational
spectralPageFromResolution resolution pageNumber =
  let source = spectralResolutionSource resolution
      basisByBidegree = pageBasisByBidegree resolution pageNumber
      entries = buildEntriesFromPageBasis source basisByBidegree
      differentials = buildDifferentialsFromPageBasis resolution pageNumber basisByBidegree
   in spectralPageFromEntries source pageNumber entries differentials

spectralPageFromEntries ::
  SpectralSource ->
  Int ->
  Map.Map Bidegree (SpectralEntry Rational) ->
  Map.Map Bidegree (FormalMap Rational) ->
  SpectralPage Rational
spectralPageFromEntries source pageNumber entries differentials =
  let zeroGroup :: HomologyGroup Rational
      zeroGroup = HomologyGroup {freeRank = 0, torsionInvariants = []}
      zeroFormalMap :: FormalMap Rational
      zeroFormalMap = FormalMap [] [] []
   in SpectralPage
        { pageIndex = pageNumber,
          groupAt =
            \filtrationDegreeValue complementaryDegreeValue ->
              Map.lookup (mkBidegree filtrationDegreeValue complementaryDegreeValue) entries
                & maybe zeroGroup entryGroupValue,
          diffMap =
            \filtrationDegreeValue complementaryDegreeValue ->
              Map.findWithDefault zeroFormalMap (mkBidegree filtrationDegreeValue complementaryDegreeValue) differentials,
          pageEntryMap = entries,
          pageDifferentialMap = differentials,
          pageAdvanceSource = Just source,
          pageAdvanceState = Nothing
        }

buildSpectralResolution :: SpectralSource -> Either HomologyFailure SpectralResolution
buildSpectralResolution source = do
  degreeReductions <- traverse (buildSpectralDegreeReduction source) (Map.keys (spectralLevelsByDegree source))
  let pairs = degreeReductions >>= spectralDegreePairs
      pairedTargets = Set.fromList (fmap pairedTargetCell pairs)
      unpairedGenerators =
        degreeReductions
          >>= spectralDegreeZeroColumns
          & filter (not . (`Set.member` pairedTargets) . unpairedTargetCell)
  pure
    SpectralResolution
      { spectralResolutionSource = source,
        spectralResolutionPairs = pairs,
        spectralResolutionUnpaired = unpairedGenerators
      }

buildSpectralDegreeReduction :: SpectralSource -> HomologicalDegree -> Either HomologyFailure SpectralDegreeReduction
buildSpectralDegreeReduction source sourceDegree = do
  matrixValue <- safeCoboundaryMatrixAt source sourceDegree
  reductionValue <-
    filtrationOrderedReduction
      (levelsAtDegree source sourceDegree)
      (levelsAtDegree source (successorDegree sourceDegree))
      matrixValue
  pure
    SpectralDegreeReduction
      { spectralDegreePairs = filtrationReductionColumns reductionValue >>= spectralPairFromReducedColumn sourceDegree,
        spectralDegreeZeroColumns = filtrationReductionColumns reductionValue >>= spectralZeroColumnFromReducedColumn sourceDegree
      }

spectralPairFromReducedColumn :: HomologicalDegree -> FiltrationReducedColumn -> [SpectralPair]
spectralPairFromReducedColumn sourceDegree columnValue =
  case filtrationColumnPivot columnValue of
    Nothing -> []
    Just pivotValue ->
      [ SpectralPair
          { spectralPairIdentifier =
              SpectralPairId
                { spectralPairIdDegree = sourceDegree,
                  spectralPairIdSourceIndex = filtrationColumnSourceIndex columnValue
                },
            spectralPairSourceLevel = filtrationColumnSourceLevel columnValue,
            spectralPairSourceVector = filtrationColumnSourceVector columnValue,
            spectralPairTargetDegree = successorDegree sourceDegree,
            spectralPairTargetIndex = filtrationPivotTargetIndex pivotValue,
            spectralPairTargetLevel = filtrationPivotTargetLevel pivotValue,
            spectralPairTargetVector = filtrationColumnTargetVector columnValue,
            spectralPairPage = filtrationPivotDistance pivotValue
          }
      ]

spectralZeroColumnFromReducedColumn :: HomologicalDegree -> FiltrationReducedColumn -> [SpectralUnpairedGenerator]
spectralZeroColumnFromReducedColumn degreeValue columnValue =
  case filtrationColumnPivot columnValue of
    Just _ -> []
    Nothing ->
      [ SpectralUnpairedGenerator
          { spectralUnpairedDegree = degreeValue,
            spectralUnpairedIndex = filtrationColumnSourceIndex columnValue,
            spectralUnpairedLevel = filtrationColumnSourceLevel columnValue,
            spectralUnpairedVector = filtrationColumnSourceVector columnValue
          }
      ]

pairedTargetCell :: SpectralPair -> SpectralPairedTargetCell
pairedTargetCell pairValue =
  SpectralPairedTargetCell
    (spectralPairTargetDegree pairValue, spectralPairTargetIndex pairValue)

unpairedTargetCell :: SpectralUnpairedGenerator -> SpectralPairedTargetCell
unpairedTargetCell generatorValue =
  SpectralPairedTargetCell
    (spectralUnpairedDegree generatorValue, spectralUnpairedIndex generatorValue)

stableFromResolution :: SpectralResolution -> Int
stableFromResolution resolution
  | filtrationWidth (spectralResolutionSource resolution) == 0 = 1
  | otherwise =
      case fmap spectralPairPage (spectralResolutionPairs resolution) of
        [] -> 0
        firstPage : remainingPages -> List.foldl' max firstPage remainingPages + 1

pageBasisByBidegree :: SpectralResolution -> Int -> Map.Map Bidegree [SpectralPageBasisVector]
pageBasisByBidegree resolution pageNumber =
  let source = spectralResolutionSource resolution
      groupedBasis =
        activePageBasis resolution pageNumber
          & fmap (\basisValue -> (pageBasisBidegree basisValue, [basisValue]))
          & Map.fromListWith (<>)
          & fmap sortPageBasis
   in supportWindows (spectralSupportRegistry source)
        & fmap
          ( \windowValue ->
              let bidegreeValue = supportWindowBidegree windowValue
               in (bidegreeValue, Map.findWithDefault [] bidegreeValue groupedBasis)
          )
        & Map.fromList

activePageBasis :: SpectralResolution -> Int -> [SpectralPageBasisVector]
activePageBasis resolution pageNumber =
  sortPageBasis
    ( (spectralResolutionPairs resolution >>= activePairEndpoints pageNumber)
        <> fmap unpairedPageBasis (spectralResolutionUnpaired resolution)
    )

activePairEndpoints :: Int -> SpectralPair -> [SpectralPageBasisVector]
activePairEndpoints pageNumber pairValue =
  if pageNumber <= spectralPairPage pairValue
    then
      [ SpectralPageBasisVector
          { spectralPageBasisRole = SpectralSourceEndpoint (spectralPairIdentifier pairValue),
            spectralPageBasisDegree = spectralPairIdDegree (spectralPairIdentifier pairValue),
            spectralPageBasisLevel = spectralPairSourceLevel pairValue,
            spectralPageBasisAnchor = spectralPairIdSourceIndex (spectralPairIdentifier pairValue),
            spectralPageBasisVector = spectralPairSourceVector pairValue
          },
        SpectralPageBasisVector
          { spectralPageBasisRole = SpectralTargetEndpoint (spectralPairIdentifier pairValue),
            spectralPageBasisDegree = spectralPairTargetDegree pairValue,
            spectralPageBasisLevel = spectralPairTargetLevel pairValue,
            spectralPageBasisAnchor = spectralPairTargetIndex pairValue,
            spectralPageBasisVector = spectralPairTargetVector pairValue
          }
      ]
    else []

unpairedPageBasis :: SpectralUnpairedGenerator -> SpectralPageBasisVector
unpairedPageBasis generatorValue =
  SpectralPageBasisVector
    { spectralPageBasisRole = SpectralUnpairedEndpoint (spectralUnpairedDegree generatorValue) (spectralUnpairedIndex generatorValue),
      spectralPageBasisDegree = spectralUnpairedDegree generatorValue,
      spectralPageBasisLevel = spectralUnpairedLevel generatorValue,
      spectralPageBasisAnchor = spectralUnpairedIndex generatorValue,
      spectralPageBasisVector = spectralUnpairedVector generatorValue
    }

pageBasisBidegree :: SpectralPageBasisVector -> Bidegree
pageBasisBidegree basisValue =
  bidegreeFromTotalDegree
    (spectralPageBasisLevel basisValue)
    (spectralPageBasisDegree basisValue)

sortPageBasis :: [SpectralPageBasisVector] -> [SpectralPageBasisVector]
sortPageBasis =
  List.sortOn
    ( \basisValue ->
        ( spectralPageBasisDegree basisValue,
          spectralPageBasisLevel basisValue,
          spectralPageBasisAnchor basisValue,
          spectralPageBasisRole basisValue
        )
    )

buildEntriesFromPageBasis :: SpectralSource -> Map.Map Bidegree [SpectralPageBasisVector] -> Map.Map Bidegree (SpectralEntry Rational)
buildEntriesFromPageBasis source basisByBidegree =
  supportWindows (spectralSupportRegistry source)
    & fmap
      ( \windowValue ->
          let bidegreeValue = supportWindowBidegree windowValue
           in ( bidegreeValue,
                buildEntryFromPageBasis
                  source
                  bidegreeValue
                  (Map.findWithDefault [] bidegreeValue basisByBidegree)
              )
      )
    & Map.fromList

buildEntryFromPageBasis :: SpectralSource -> Bidegree -> [SpectralPageBasisVector] -> SpectralEntry Rational
buildEntryFromPageBasis source bidegreeValue basisVectors =
  let ambientDimension = degreeDimension source (bidegreeTotalDegree bidegreeValue)
      denseBasis = fmap (sparseRowToDense ambientDimension . spectralPageBasisVector) basisVectors
      representatives = fmap (basisRepresentativeValue source) basisVectors
      presentation = mkQuotientPresentation ambientDimension denseBasis representatives []
   in SpectralEntry
        { entryPresentation = presentation,
          entryGroupValue =
            HomologyGroup
              { freeRank = length basisVectors,
                torsionInvariants = []
              }
        }

buildDifferentialsFromPageBasis :: SpectralResolution -> Int -> Map.Map Bidegree [SpectralPageBasisVector] -> Map.Map Bidegree (FormalMap Rational)
buildDifferentialsFromPageBasis resolution pageNumber basisByBidegree =
  supportWindows (spectralSupportRegistry (spectralResolutionSource resolution))
    & fmap
      ( \windowValue ->
          let bidegreeValue = supportWindowBidegree windowValue
           in (bidegreeValue, buildDifferentialFromPageBasis resolution pageNumber basisByBidegree bidegreeValue)
      )
    & Map.fromList

buildDifferentialFromPageBasis ::
  SpectralResolution ->
  Int ->
  Map.Map Bidegree [SpectralPageBasisVector] ->
  Bidegree ->
  FormalMap Rational
buildDifferentialFromPageBasis resolution pageNumber basisByBidegree sourceBidegree =
  let source = spectralResolutionSource resolution
      targetBidegree = targetBidegreeAfterDifferential pageNumber sourceBidegree
      domainBasis = Map.findWithDefault [] sourceBidegree basisByBidegree
      codomainBasis = Map.findWithDefault [] targetBidegree basisByBidegree
      livePairIds = differentialPairIdsAt resolution pageNumber sourceBidegree
   in FormalMap
        { formalMatrix =
            fmap
              ( \targetBasisValue ->
                  fmap
                    (\domainBasisValue -> differentialCoefficient livePairIds domainBasisValue targetBasisValue)
                    domainBasis
              )
              codomainBasis,
          formalDomainBasis = fmap (basisRepresentativeValue source) domainBasis,
          formalCodomainBasis = fmap (basisRepresentativeValue source) codomainBasis
        }

differentialPairIdsAt :: SpectralResolution -> Int -> Bidegree -> Set.Set SpectralPairId
differentialPairIdsAt resolution pageNumber sourceBidegree =
  spectralResolutionPairs resolution
    & filter
      ( \pairValue ->
          spectralPairPage pairValue == pageNumber
            && pairSourceBidegree pairValue == sourceBidegree
      )
    & fmap spectralPairIdentifier
    & Set.fromList

differentialCoefficient :: Set.Set SpectralPairId -> SpectralPageBasisVector -> SpectralPageBasisVector -> Rational
differentialCoefficient livePairIds domainBasisValue targetBasisValue =
  case (spectralPageBasisRole domainBasisValue, spectralPageBasisRole targetBasisValue) of
    (SpectralSourceEndpoint sourcePairId, SpectralTargetEndpoint targetPairId)
      | sourcePairId == targetPairId && Set.member sourcePairId livePairIds -> 1
    _ -> 0

pairSourceBidegree :: SpectralPair -> Bidegree
pairSourceBidegree pairValue =
  bidegreeFromTotalDegree
    (spectralPairSourceLevel pairValue)
    (spectralPairIdDegree (spectralPairIdentifier pairValue))

basisRepresentativeValue :: SpectralSource -> SpectralPageBasisVector -> RepresentativeCocycle Rational Int
basisRepresentativeValue source basisValue =
  let degreeValue = spectralPageBasisDegree basisValue
      ambientDimension = degreeDimension source degreeValue
   in vectorToRepresentative
        degreeValue
        (sparseRowToDense ambientDimension (spectralPageBasisVector basisValue))

buildEntryFromBases ::
  Bidegree ->
  Int ->
  [AmbientVector] ->
  [AmbientVector] ->
  Either HomologyFailure (SpectralEntry Rational)
buildEntryFromBases bidegreeValue ambientDimension numeratorBasis denominatorBasis = do
  reducedNumerator <- reduceBasisChecked ambientDimension numeratorBasis
  reducedDenominator <- reduceBasisChecked ambientDimension denominatorBasis
  assertDenominatorSubset bidegreeValue ambientDimension reducedNumerator reducedDenominator
  let quotientBasis = independentModuloBasis reducedDenominator reducedNumerator
      presentation =
        presentationFromSparseBases
          (bidegreeTotalDegree bidegreeValue)
          ambientDimension
          quotientBasis
          reducedDenominator
      groupValue =
        HomologyGroup
          { freeRank = length quotientBasis,
            torsionInvariants = []
          }
  pure
    SpectralEntry
      { entryPresentation = presentation,
        entryGroupValue = groupValue
      }

presentationFromSparseBases ::
  HomologicalDegree ->
  Int ->
  [AmbientVector] ->
  [AmbientVector] ->
  QuotientPresentation Rational
presentationFromSparseBases degreeValue ambientDimension quotientBasis denominatorBasis =
  let basisVectors = fmap (sparseRowToDense ambientDimension) quotientBasis
      denominatorVectors = fmap (sparseRowToDense ambientDimension) denominatorBasis
      basisRepresentatives = fmap (vectorToRepresentative degreeValue) basisVectors
   in mkQuotientPresentation
        ambientDimension
        basisVectors
        basisRepresentatives
        denominatorVectors

assertDenominatorSubset ::
  Bidegree ->
  Int ->
  [AmbientVector] ->
  [AmbientVector] ->
  Either HomologyFailure ()
assertDenominatorSubset bidegreeValue ambientDimension numeratorBasis denominatorBasis =
  case firstVectorOutsideSpan numeratorBasis denominatorBasis of
    Nothing -> Right ()
    Just denominatorVector ->
      Left
        ( SpectralQuotientDenominatorNotSubspace
            (bidegreeCoordinates bidegreeValue)
            ambientDimension
            (sparseRowToDense ambientDimension denominatorVector)
        )

filtrationWidth :: SpectralSource -> Int
filtrationWidth source =
  max 0 (spectralMaxLevel source - spectralMinLevel source)

homologicalPageBound :: SpectralSource -> Int
homologicalPageBound source =
  filtrationWidth source + 1

mkRationalSpectralSource :: FiniteChainComplex Rational -> FiltrationFunction -> Either HomologyFailure SpectralSource
mkRationalSpectralSource =
  mkSpectralSource

degreeDimension :: SpectralSource -> HomologicalDegree -> Int
degreeDimension source degreeValue =
  cellCountAtDegree (spectralBaseComplex source) degreeValue

levelsAtDegree :: SpectralSource -> HomologicalDegree -> [Int]
levelsAtDegree source degreeValue =
  Map.findWithDefault [] degreeValue (spectralLevelsByDegree source)

successorDegree :: HomologicalDegree -> HomologicalDegree
successorDegree (HomologicalDegree degreeValue) =
  HomologicalDegree (degreeValue + 1)

safeCoboundaryMatrixAt ::
  SpectralSource ->
  HomologicalDegree ->
  Either HomologyFailure SparseMatrix
safeCoboundaryMatrixAt source degreeValue
  | unHomologicalDegree degreeValue < 0 =
      zeroSparseMatrix
        (degreeDimension source (successorDegree degreeValue))
        0
  | unHomologicalDegree degreeValue >= unHomologicalDegree (maxHomologicalDegree (spectralBaseComplex source)) =
      zeroSparseMatrix
        0
        (degreeDimension source degreeValue)
  | otherwise = do
      let targetDegreeValue = successorDegree degreeValue
          boundaryRowCount = degreeDimension source degreeValue
          boundaryColumnCount = degreeDimension source targetDegreeValue
      boundaryMatrix <-
        boundaryMatrixAtRational (spectralBaseComplex source) targetDegreeValue
          >>= reshapeSparseMatrix boundaryRowCount boundaryColumnCount
      pure (sparseTransposeMatrix boundaryMatrix)

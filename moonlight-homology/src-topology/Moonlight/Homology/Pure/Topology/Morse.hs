{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Homology.Pure.Topology.Morse
  ( MorsePivotOps (..),
    intUnitMorsePivotOps,
    integerUnitMorsePivotOps,
    rationalMorsePivotOps,
    gf2MorsePivotOps,
    AlgebraicMorsePair,
    AlgebraicMorseMatching,
    AlgebraicMorseComplex,
    AcyclicPair (..),
    IntegralAcyclicPair,
    LocalizedAcyclicPair (..),
    RationalAcyclicPair,
    CollapseObstruction (..),
    LocalizedCollapseObstruction (..),
    AcyclicMatching (..),
    LocalizedAcyclicMatching (..),
    MorseComplex (..),
    LocalizedMorseComplex (..),
    RefinedMatchingStage,
    RefinedAcyclicMatching,
    RefinedMorseComplex (..),
    FilteredMorsePairWitness (..),
    FilteredMorseCompatibility (..),
    FilteredRefinedMorseComplex (..),
    RefinedMatchingSummary (..),
    acyclicMatching,
    acyclicMatchingLocalized,
    refinedAcyclicMatchingTranscript,
    refinedMorseComplex,
    filteredRefinedMorseComplex,
    reducedFiltrationByCriticalBasis,
    filteredReducedFiltration,
    rationalizeFiniteChainComplex,
    foldRefinedAcyclicMatching,
    traverseRefinedStages,
    mapRefinedStages,
    summarizeRefinedMatching,
    refinedMatchingSummary,
    refinedStageCount,
    hasRefinedStages,
    isTerminalRefinedMatching,
    finalRefinedCriticalDegrees,
    finalRefinedCriticalCellCount,
    finalRefinedCriticalDegreeHistogram,
    finalRefinedHomologicalSupport,
    finalRefinedMaxCriticalDegree,
    refinedMatchingCriticalCells,
    refinedStageMatching,
    refinedStageReducedComplex,
    refinedStageCriticalBasis,
    flattenRefinedAcyclicMatching,
    refinedAcyclicMatching,
    acyclicMatchingWith,
    morseComplexWith,
    isAcyclicMatchingWith,
    extractCandidatePairsWith,
    reverseCandidateEdgeWith,
    morseComplex,
    morseComplexLocalized,
    isAcyclicMatching,
    isAcyclicMatchingLocalized,
    extractCandidatePairsLocalized,
    reverseCandidateEdgeLocalized,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Core (Semiring)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
    maxHomologicalDegree,
    mkFiniteChainComplex,
    validateFiniteChainComplexShape,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( boundaryCoefficient,
    boundaryEntries,
    emptyBoundaryIncidence,
    mapBoundaryCoefficients,
    materializeIncidenceBoundary,
    sourceIndex,
    targetIndex,
  )
import Moonlight.Homology.Pure.Chain (HomologicalDegree (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure (..), HomologyLaw (..))
import Moonlight.Homology.Pure.LinearCombination qualified as LC
import Moonlight.Homology.Pure.Reductions
  ( ChainHomotopy (..),
    ChainMap (..),
  )
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))
import Moonlight.Homology.Pure.Topology.Core (allBasisCellRefs)
import Moonlight.Homology.Pure.Topology.Morse.Digraph
  ( DirectedAdjacencyMap,
    DirectedEdgeMap,
    PathWeightOracle,
    adjacencyMap,
    basisCellDimension,
    basisCellKey,
    deleteAdjacencyEdge,
    graphIsAcyclic,
    insertAdjacencyEdge,
    pathWeightOracle,
    pathWeightsFromOracle,
    topologicalOrderWithVertices,
  )
import Moonlight.LinAlg.Pure.Dense.GF2 (GF2 (..))

type MorsePivotOps :: Type -> Type
newtype MorsePivotOps r = MorsePivotOps
  { mpoUnitInverse :: r -> Maybe r
  }

intUnitMorsePivotOps :: MorsePivotOps Int
intUnitMorsePivotOps = unitMorsePivotOps

integerUnitMorsePivotOps :: MorsePivotOps Integer
integerUnitMorsePivotOps = unitMorsePivotOps

rationalMorsePivotOps :: MorsePivotOps Rational
rationalMorsePivotOps =
  MorsePivotOps
    { mpoUnitInverse =
        \coefficientValue ->
          if coefficientValue == 0
            then Nothing
            else Just (recip coefficientValue)
    }

gf2MorsePivotOps :: MorsePivotOps GF2
gf2MorsePivotOps =
  MorsePivotOps
    { mpoUnitInverse =
        \coefficientValue ->
          case coefficientValue of
            GF2Zero -> Nothing
            GF2One -> Just GF2One
    }

unitMorsePivotOps :: (Eq r, Num r) => MorsePivotOps r
unitMorsePivotOps =
  MorsePivotOps
    { mpoUnitInverse =
        \coefficientValue ->
          case coefficientValue of
            1 -> Just 1
            -1 -> Just (-1)
            _ -> Nothing
    }

type AcyclicPair :: Type
data AcyclicPair = AcyclicPair
  { apLowerCell :: BasisCellRef,
    apUpperCell :: BasisCellRef,
    apIncidenceCoefficient :: Int
  }
  deriving stock (Eq, Show)

type IntegralAcyclicPair :: Type
type IntegralAcyclicPair = AcyclicPair

type LocalizedAcyclicPair :: Type -> Type
data LocalizedAcyclicPair r = LocalizedAcyclicPair
  { lapLowerCell :: BasisCellRef,
    lapUpperCell :: BasisCellRef,
    lapIncidenceCoefficient :: r
  }
  deriving stock (Eq, Show)

type RationalAcyclicPair :: Type
type RationalAcyclicPair = LocalizedAcyclicPair Rational

type AlgebraicMorsePair :: Type -> Type
type AlgebraicMorsePair = LocalizedAcyclicPair

type CollapseObstruction :: Type
data CollapseObstruction = CollapseObstruction
  { coCandidate :: AcyclicPair,
    coCycleWitness :: [BasisCellRef]
  }
  deriving stock (Eq, Show)

type LocalizedCollapseObstruction :: Type -> Type
data LocalizedCollapseObstruction pair = LocalizedCollapseObstruction
  { lcoCandidate :: pair,
    lcoCycleWitness :: [BasisCellRef]
  }
  deriving stock (Eq, Show)

type AcyclicMatching :: Type
data AcyclicMatching = AcyclicMatching
  { amPairs :: [AcyclicPair],
    amCriticalCells :: [BasisCellRef],
    amObstructions :: [CollapseObstruction]
  }
  deriving stock (Eq, Show)

type LocalizedAcyclicMatching :: Type -> Type
data LocalizedAcyclicMatching pair = LocalizedAcyclicMatching
  { lamPairs :: [pair],
    lamCriticalCells :: [BasisCellRef],
    lamObstructions :: [LocalizedCollapseObstruction pair]
  }
  deriving stock (Eq, Show)

type AlgebraicMorseMatching :: Type -> Type
type AlgebraicMorseMatching r = LocalizedAcyclicMatching (AlgebraicMorsePair r)

type MorseComplex :: Type -> Type
data MorseComplex r = MorseComplex
  { mcMatching :: AcyclicMatching,
    mcReducedComplex :: FiniteChainComplex r,
    mcCriticalBasis :: Map BasisCellRef BasisCellRef,
    mcProjection :: ChainMap BasisCellRef BasisCellRef r,
    mcInclusion :: ChainMap BasisCellRef BasisCellRef r,
    mcHomotopy :: ChainHomotopy BasisCellRef r
  }

type LocalizedMorseComplex :: Type -> Type
data LocalizedMorseComplex r = LocalizedMorseComplex
  { lmcMatching :: LocalizedAcyclicMatching (LocalizedAcyclicPair r),
    lmcReducedComplex :: FiniteChainComplex r,
    lmcCriticalBasis :: Map BasisCellRef BasisCellRef,
    lmcProjection :: ChainMap BasisCellRef BasisCellRef r,
    lmcInclusion :: ChainMap BasisCellRef BasisCellRef r,
    lmcHomotopy :: ChainHomotopy BasisCellRef r
  }

type AlgebraicMorseComplex :: Type -> Type
type AlgebraicMorseComplex = LocalizedMorseComplex

type RefinedMatchingStage :: Type -> Type
data RefinedMatchingStage r = RefinedMatchingStage
  { rmsMatching :: LocalizedAcyclicMatching (LocalizedAcyclicPair r),
    rmsReducedComplex :: Maybe (FiniteChainComplex r),
    rmsCriticalBasis :: Maybe (Map BasisCellRef BasisCellRef)
  }

type RefinedAcyclicMatching :: Type -> Type
data RefinedAcyclicMatching r = RefinedAcyclicMatching
  { ramStages :: [RefinedMatchingStage r],
    ramCriticalCells :: [BasisCellRef]
  }

type RefinedMorseComplex :: Type -> Type
data RefinedMorseComplex r = RefinedMorseComplex
  { rmcTranscript :: RefinedAcyclicMatching r,
    rmcReducedComplex :: FiniteChainComplex r,
    rmcCriticalBasis :: Map BasisCellRef BasisCellRef
  }

type FilteredMorsePairWitness :: Type
data FilteredMorsePairWitness = FilteredMorsePairWitness
  { fmpwLowerCell :: BasisCellRef,
    fmpwUpperCell :: BasisCellRef,
    fmpwFiltrationLevel :: Int
  }
  deriving stock (Eq, Show)

type FilteredMorseCompatibility :: Type
newtype FilteredMorseCompatibility = FilteredMorseCompatibility
  { fmcPairWitnesses :: [FilteredMorsePairWitness]
  }
  deriving stock (Eq, Show)

type FilteredRefinedMorseComplex :: Type -> Type
data FilteredRefinedMorseComplex r = FilteredRefinedMorseComplex
  { frmcRefinedMorseComplex :: RefinedMorseComplex r,
    frmcReducedFiltrationByBasis :: Map BasisCellRef Int,
    frmcCompatibility :: FilteredMorseCompatibility
  }

type RefinedMorseDescent :: Type -> Type
data RefinedMorseDescent r
  = RefinedMorseDescentComplete (RefinedMorseComplex r)
  | RefinedMorseDescentBlocked (RefinedAcyclicMatching r) HomologyFailure

type RefinedMatchingSummary :: Type
data RefinedMatchingSummary = RefinedMatchingSummary
  { rmsStageCount :: Int,
    rmsHasStages :: Bool,
    rmsIsTerminal :: Bool,
    rmsFinalCriticalCellCount :: Int,
    rmsFinalCriticalDegreeHistogram :: Map HomologicalDegree Int,
    rmsFinalHomologicalSupport :: Set.Set HomologicalDegree,
    rmsFinalMaxCriticalDegree :: Maybe HomologicalDegree
  }
  deriving stock (Eq, Show)

type GradientAcyclicState :: Type -> Type
data GradientAcyclicState edgeWeight = GradientAcyclicState
  { gasEdgeMap :: !(DirectedEdgeMap edgeWeight),
    gasAdjacency :: !DirectedAdjacencyMap,
    gasMatchedUpperByLower :: !(Map BasisCellRef BasisCellRef),
    gasGradientAdjacency :: !DirectedAdjacencyMap
  }

type AcyclicRewrite :: Type -> Type
data AcyclicRewrite edgeWeight
  = AcyclicRewriteAccepted (GradientAcyclicState edgeWeight)
  | AcyclicRewriteRejected [BasisCellRef]

acyclicMatching ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  AcyclicMatching
acyclicMatching chainComplex cellScore =
  integralMatchingFromAlgebraic
    ( acyclicMatchingFromEdgeMapWith
        integerUnitMorsePivotOps
        (const True)
        (allBasisCellRefs chainComplex)
        (integralBoundaryEdgeMap chainComplex)
        cellScore
    )

acyclicMatchingWith ::
  (Eq r, Num r) =>
  MorsePivotOps r ->
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  AlgebraicMorseMatching r
acyclicMatchingWith pivotOps chainComplex =
  acyclicMatchingFromEdgeMapWith
    pivotOps
    (const True)
    (allBasisCellRefs chainComplex)
    (boundaryEdgeMap chainComplex)

acyclicMatchingLocalized ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  LocalizedAcyclicMatching RationalAcyclicPair
acyclicMatchingLocalized chainComplex cellScore =
  acyclicMatchingWith rationalMorsePivotOps (rationalizeFiniteChainComplex chainComplex) cellScore

refinedAcyclicMatching ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  Either HomologyFailure (LocalizedAcyclicMatching RationalAcyclicPair)
{-# DEPRECATED refinedAcyclicMatching "Use refinedAcyclicMatchingTranscript for canonical stage-aware reductions; use fmap flattenRefinedAcyclicMatching only as a compatibility projection." #-}
refinedAcyclicMatching chainComplex cellScore =
  flattenRefinedAcyclicMatching <$> refinedAcyclicMatchingTranscript chainComplex cellScore

refinedAcyclicMatchingTranscript ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  Either HomologyFailure (RefinedAcyclicMatching Rational)
refinedAcyclicMatchingTranscript chainComplex cellScore =
  case runRefinedMorseDescent chainComplex cellScore of
    RefinedMorseDescentComplete refinedComplex -> Right (rmcTranscript refinedComplex)
    RefinedMorseDescentBlocked _ failureValue -> Left failureValue

refinedMorseComplex ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  Either HomologyFailure (RefinedMorseComplex Rational)
refinedMorseComplex chainComplex cellScore =
  case runRefinedMorseDescent chainComplex cellScore of
    RefinedMorseDescentComplete refinedComplex -> Right refinedComplex
    RefinedMorseDescentBlocked _ failureValue -> Left failureValue

filteredRefinedMorseComplex ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Int) ->
  (BasisCellRef -> Double) ->
  Either HomologyFailure (FilteredRefinedMorseComplex Rational)
filteredRefinedMorseComplex chainComplex originalFiltration cellScore =
  case runFilteredRefinedMorseDescent chainComplex originalFiltration cellScore of
    RefinedMorseDescentBlocked _ failureValue -> Left failureValue
    RefinedMorseDescentComplete refinedComplex -> do
      reducedFiltrationValue <-
        reducedFiltrationByCriticalBasis
          (rmcReducedComplex refinedComplex)
          (rmcCriticalBasis refinedComplex)
          originalFiltration
      compatibilityValue <- filteredMorseCompatibility originalFiltration (rmcTranscript refinedComplex)
      Right
        FilteredRefinedMorseComplex
          { frmcRefinedMorseComplex = refinedComplex,
            frmcReducedFiltrationByBasis = reducedFiltrationValue,
            frmcCompatibility = compatibilityValue
          }

runRefinedMorseDescent ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  RefinedMorseDescent Rational
runRefinedMorseDescent chainComplex cellScore =
  runRefinedMorseDescentWith (\_ currentComplex currentScore -> acyclicMatchingRational currentComplex currentScore) chainComplex cellScore

runFilteredRefinedMorseDescent ::
  Integral r =>
  FiniteChainComplex r ->
  (BasisCellRef -> Int) ->
  (BasisCellRef -> Double) ->
  RefinedMorseDescent Rational
runFilteredRefinedMorseDescent chainComplex originalFiltration cellScore =
  runRefinedMorseDescentWith
    ( \currentBasis currentComplex currentScore ->
        acyclicMatchingRationalWith
          (filteredPairCompatible originalFiltration currentBasis)
          currentComplex
          currentScore
    )
    chainComplex
    cellScore

runRefinedMorseDescentWith ::
  Integral r =>
  ( Map BasisCellRef BasisCellRef ->
    FiniteChainComplex Rational ->
    (BasisCellRef -> Double) ->
    LocalizedAcyclicMatching RationalAcyclicPair
  ) ->
  FiniteChainComplex r ->
  (BasisCellRef -> Double) ->
  RefinedMorseDescent Rational
runRefinedMorseDescentWith matchingAt chainComplex cellScore =
  -- The first stage runs the fully gated 'morseComplexRational' (the user's
  -- complex and the initial matching are unproven input); every later stage
  -- operates on a theorem-guaranteed reduction with an engine-built matching
  -- and descends through the trusted entry — the invariant is established
  -- once at the boundary, not re-proved per stage.
  refineMatching morseComplexRational [] initialBasis initialComplex
  where
    initialComplex = rationalizeFiniteChainComplex chainComplex
    initialBasis = Map.fromList [(cell, cell) | cell <- allBasisCellRefs initialComplex]
    buildTranscript :: [RefinedMatchingStage Rational] -> [BasisCellRef] -> RefinedAcyclicMatching Rational
    buildTranscript accumulatedStages criticalCells =
      RefinedAcyclicMatching
        { ramStages = reverse accumulatedStages,
          ramCriticalCells = criticalCells
        }
    refineMatching ::
      ( FiniteChainComplex Rational ->
        LocalizedAcyclicMatching RationalAcyclicPair ->
        Either HomologyFailure (LocalizedMorseComplex Rational)
      ) ->
      [RefinedMatchingStage Rational] ->
      Map BasisCellRef BasisCellRef ->
      FiniteChainComplex Rational ->
      RefinedMorseDescent Rational
    refineMatching reduceStage accumulatedStages currentBasis currentComplex =
      let currentMatching = matchingAt currentBasis currentComplex (cellScore . rebaseCell currentBasis)
          currentPairs = lamPairs currentMatching
          translatedMatching = rebaseLocalizedMatching currentBasis currentMatching
          translatedCriticalCells = lamCriticalCells translatedMatching
       in case currentPairs of
            [] ->
              let transcriptValue = buildTranscript accumulatedStages translatedCriticalCells
               in RefinedMorseDescentComplete
                    RefinedMorseComplex
                      { rmcTranscript = transcriptValue,
                        rmcReducedComplex = currentComplex,
                        rmcCriticalBasis = currentBasis
                      }
            _ ->
              case reduceStage currentComplex currentMatching of
                Left failureValue ->
                  RefinedMorseDescentBlocked
                    ( buildTranscript
                        ( RefinedMatchingStage
                            { rmsMatching = translatedMatching,
                              rmsReducedComplex = Nothing,
                              rmsCriticalBasis = Nothing
                            } :
                          accumulatedStages
                        )
                        translatedCriticalCells
                    )
                    failureValue
                Right reducedComplexData ->
                  let nextReducedComplex = lmcReducedComplex reducedComplexData
                      nextCriticalBasis = composeCriticalBasis currentBasis (lmcCriticalBasis reducedComplexData)
                      stageValue =
                        RefinedMatchingStage
                          { rmsMatching = translatedMatching,
                            rmsReducedComplex = Just nextReducedComplex,
                            rmsCriticalBasis = Just nextCriticalBasis
                          }
                   in refineMatching
                        morseComplexRationalTrusted
                        (stageValue : accumulatedStages)
                        nextCriticalBasis
                        nextReducedComplex

filteredPairCompatible ::
  (BasisCellRef -> Int) ->
  Map BasisCellRef BasisCellRef ->
  RationalAcyclicPair ->
  Bool
filteredPairCompatible originalFiltration currentBasis candidatePair =
  let lowerOriginal = rebaseCell currentBasis (localizedLowerCell candidatePair)
      upperOriginal = rebaseCell currentBasis (localizedUpperCell candidatePair)
   in originalFiltration lowerOriginal == originalFiltration upperOriginal

reducedFiltrationByCriticalBasis ::
  FiniteChainComplex r ->
  Map BasisCellRef BasisCellRef ->
  (BasisCellRef -> Int) ->
  Either HomologyFailure (Map BasisCellRef Int)
reducedFiltrationByCriticalBasis reducedComplex criticalBasis originalFiltration =
  Map.fromList
    <$> traverse
      ( \basisRef ->
          fmap
            (\originalBasisRef -> (basisRef, originalFiltration originalBasisRef))
            (criticalOriginalBasis criticalBasis basisRef)
      )
      (allBasisCellRefs reducedComplex)

-- | Filtration value of a reduced basis cell. Cells outside the reduced
-- critical basis answer 0 by convention — callers that need to distinguish
-- "filtration 0" from "not a critical cell" should consult
-- 'frmcReducedFiltrationByBasis' directly.
filteredReducedFiltration ::
  FilteredRefinedMorseComplex r ->
  BasisCellRef ->
  Int
filteredReducedFiltration filteredComplex basisRef =
  Map.findWithDefault 0 basisRef (frmcReducedFiltrationByBasis filteredComplex)

criticalOriginalBasis ::
  Map BasisCellRef BasisCellRef ->
  BasisCellRef ->
  Either HomologyFailure BasisCellRef
criticalOriginalBasis criticalBasis basisRef =
  case Map.lookup basisRef criticalBasis of
    Just originalBasisRef -> Right originalBasisRef
    Nothing -> Left (MissingCriticalBasisProvenance basisRef)

filteredMorseCompatibility ::
  (BasisCellRef -> Int) ->
  RefinedAcyclicMatching Rational ->
  Either HomologyFailure FilteredMorseCompatibility
filteredMorseCompatibility originalFiltration transcriptValue =
  fmap
    (FilteredMorseCompatibility . foldMap id)
    (traverse (stageFilteredPairWitnesses originalFiltration) (ramStages transcriptValue))

stageFilteredPairWitnesses ::
  (BasisCellRef -> Int) ->
  RefinedMatchingStage Rational ->
  Either HomologyFailure [FilteredMorsePairWitness]
stageFilteredPairWitnesses originalFiltration stageValue =
  traverse
    (filteredPairWitness originalFiltration)
    (lamPairs (rmsMatching stageValue))

filteredPairWitness ::
  (BasisCellRef -> Int) ->
  RationalAcyclicPair ->
  Either HomologyFailure FilteredMorsePairWitness
filteredPairWitness originalFiltration candidatePair =
  let lowerCell = localizedLowerCell candidatePair
      upperCell = localizedUpperCell candidatePair
      lowerLevel = originalFiltration lowerCell
      upperLevel = originalFiltration upperCell
   in if lowerLevel == upperLevel
        then
          Right
            FilteredMorsePairWitness
              { fmpwLowerCell = lowerCell,
                fmpwUpperCell = upperCell,
                fmpwFiltrationLevel = lowerLevel
              }
        else
          Left (FiltrationIncompatibleMorsePair lowerCell upperCell lowerLevel upperLevel)

foldRefinedAcyclicMatching ::
  (RefinedMatchingStage r -> a -> a) ->
  ([BasisCellRef] -> a) ->
  RefinedAcyclicMatching r ->
  a
foldRefinedAcyclicMatching step finish refinedMatching =
  foldr step (finish (ramCriticalCells refinedMatching)) (ramStages refinedMatching)

traverseRefinedStages ::
  Applicative f =>
  (RefinedMatchingStage r -> f a) ->
  RefinedAcyclicMatching r ->
  f [a]
traverseRefinedStages summarizeStage =
  foldRefinedAcyclicMatching
    (\stageValue accumulatedSummaries -> (:) <$> summarizeStage stageValue <*> accumulatedSummaries)
    (const (pure []))

mapRefinedStages ::
  (RefinedMatchingStage r -> a) ->
  RefinedAcyclicMatching r ->
  [a]
mapRefinedStages summarizeStage =
  foldRefinedAcyclicMatching
    (\stageValue accumulatedSummaries -> summarizeStage stageValue : accumulatedSummaries)
    (const [])

summarizeRefinedMatching ::
  Monoid a =>
  (RefinedMatchingStage r -> a) ->
  RefinedAcyclicMatching r ->
  (a, [BasisCellRef])
summarizeRefinedMatching summarizeStage =
  foldRefinedAcyclicMatching
    (\stageValue (stageSummary, criticalCells) -> (summarizeStage stageValue <> stageSummary, criticalCells))
    (\criticalCells -> (mempty, criticalCells))

refinedMatchingSummary :: RefinedAcyclicMatching r -> RefinedMatchingSummary
refinedMatchingSummary refinedMatching =
  RefinedMatchingSummary
    { rmsStageCount = refinedStageCount refinedMatching,
      rmsHasStages = hasRefinedStages refinedMatching,
      rmsIsTerminal = isTerminalRefinedMatching refinedMatching,
      rmsFinalCriticalCellCount = finalRefinedCriticalCellCount refinedMatching,
      rmsFinalCriticalDegreeHistogram = finalRefinedCriticalDegreeHistogram refinedMatching,
      rmsFinalHomologicalSupport = finalRefinedHomologicalSupport refinedMatching,
      rmsFinalMaxCriticalDegree = finalRefinedMaxCriticalDegree refinedMatching
    }

refinedStageCount :: RefinedAcyclicMatching r -> Int
refinedStageCount =
  foldRefinedAcyclicMatching (\_ countValue -> countValue + 1) (const 0)

hasRefinedStages :: RefinedAcyclicMatching r -> Bool
hasRefinedStages =
  foldRefinedAcyclicMatching (\_ _ -> True) (const False)

isTerminalRefinedMatching :: RefinedAcyclicMatching r -> Bool
isTerminalRefinedMatching =
  all (== HomologicalDegree 0) . finalRefinedCriticalDegrees

finalRefinedCriticalDegrees :: RefinedAcyclicMatching r -> [HomologicalDegree]
finalRefinedCriticalDegrees =
  fmap cellDegree . refinedMatchingCriticalCells

finalRefinedCriticalCellCount :: RefinedAcyclicMatching r -> Int
finalRefinedCriticalCellCount =
  length . refinedMatchingCriticalCells

finalRefinedCriticalDegreeHistogram :: RefinedAcyclicMatching r -> Map HomologicalDegree Int
finalRefinedCriticalDegreeHistogram =
  Map.fromListWith (+)
    . fmap (\degreeValue -> (degreeValue, 1))
    . finalRefinedCriticalDegrees

finalRefinedHomologicalSupport :: RefinedAcyclicMatching r -> Set.Set HomologicalDegree
finalRefinedHomologicalSupport =
  Set.fromList . finalRefinedCriticalDegrees

finalRefinedMaxCriticalDegree :: RefinedAcyclicMatching r -> Maybe HomologicalDegree
finalRefinedMaxCriticalDegree =
  Set.lookupMax . finalRefinedHomologicalSupport

refinedMatchingCriticalCells :: RefinedAcyclicMatching r -> [BasisCellRef]
refinedMatchingCriticalCells =
  foldRefinedAcyclicMatching (\_ criticalCells -> criticalCells) id

refinedStageMatching ::
  RefinedMatchingStage r ->
  LocalizedAcyclicMatching (LocalizedAcyclicPair r)
refinedStageMatching RefinedMatchingStage {rmsMatching = matching} = matching

refinedStageReducedComplex ::
  RefinedMatchingStage r ->
  Maybe (FiniteChainComplex r)
refinedStageReducedComplex RefinedMatchingStage {rmsReducedComplex = reducedComplex} = reducedComplex

refinedStageCriticalBasis ::
  RefinedMatchingStage r ->
  Maybe (Map BasisCellRef BasisCellRef)
refinedStageCriticalBasis RefinedMatchingStage {rmsCriticalBasis = criticalBasis} = criticalBasis

flattenRefinedAcyclicMatching ::
  RefinedAcyclicMatching r ->
  LocalizedAcyclicMatching (LocalizedAcyclicPair r)
flattenRefinedAcyclicMatching refinedMatching =
  foldRefinedAcyclicMatching
    (\stage accumulatedMatching ->
       LocalizedAcyclicMatching
         { lamPairs = lamPairs (rmsMatching stage) <> lamPairs accumulatedMatching,
           lamCriticalCells = lamCriticalCells accumulatedMatching,
           lamObstructions = lamObstructions (rmsMatching stage) <> lamObstructions accumulatedMatching
         }
    )
    (\criticalCells ->
       LocalizedAcyclicMatching
         { lamPairs = [],
           lamCriticalCells = criticalCells,
           lamObstructions = []
         }
    )
    refinedMatching

morseComplex ::
  (Integral r, Semiring r) =>
  FiniteChainComplex r ->
  AcyclicMatching ->
  Either HomologyFailure (MorseComplex r)
morseComplex chainComplex matching =
  case morseComplexWith unitMorsePivotOps chainComplex (integralMatchingToAlgebraic matching) of
    Left failureValue -> Left failureValue
    Right morseValue ->
      Right
        MorseComplex
          { mcMatching = matching,
            mcReducedComplex = lmcReducedComplex morseValue,
            mcCriticalBasis = lmcCriticalBasis morseValue,
            mcProjection = lmcProjection morseValue,
            mcInclusion = lmcInclusion morseValue,
            mcHomotopy = lmcHomotopy morseValue
          }

morseComplexWith ::
  (Eq r, Num r, Semiring r) =>
  MorsePivotOps r ->
  FiniteChainComplex r ->
  AlgebraicMorseMatching r ->
  Either HomologyFailure (AlgebraicMorseComplex r)
morseComplexWith pivotOps chainComplex matching
  | not (isAcyclicMatchingAgainstEdgeMapWith pivotOps allCells initialEdgeMap matching) =
      Left (InvalidTopologyInput "Invalid algebraic acyclic matching")
  | otherwise =
      case reductionData of
        Left failureValue -> Left failureValue
        Right reducedData ->
          Right
            LocalizedMorseComplex
              { lmcMatching = matching,
                lmcReducedComplex = mrdReducedComplex reducedData,
                lmcCriticalBasis = mrdCriticalBasis reducedData,
                lmcProjection = mrdProjection reducedData,
                lmcInclusion = mrdInclusion reducedData,
                lmcHomotopy = mrdHomotopy reducedData
              }
  where
    allCells = allBasisCellRefs chainComplex
    initialEdgeMap = boundaryEdgeMap chainComplex
    reducedMatchingPairs = lamPairs matching
    criticalCells = expectedCriticalCells localizedLowerCell localizedUpperCell allCells reducedMatchingPairs
    finalEdgeMap = foldl' (flip (reverseCandidateEdgeWith pivotOps)) initialEdgeMap reducedMatchingPairs
    reductionData =
      buildReducedComplexData chainComplex criticalCells reducedMatchingPairs initialEdgeMap finalEdgeMap

morseComplexLocalized ::
  Integral r =>
  FiniteChainComplex r ->
  LocalizedAcyclicMatching RationalAcyclicPair ->
  Either HomologyFailure (LocalizedMorseComplex Rational)
morseComplexLocalized chainComplex matching =
  morseComplexWith rationalMorsePivotOps (rationalizeFiniteChainComplex chainComplex) matching

isAcyclicMatching ::
  Integral r =>
  FiniteChainComplex r ->
  AcyclicMatching ->
  Bool
isAcyclicMatching chainComplex =
  isAcyclicMatchingWith unitMorsePivotOps chainComplex . integralMatchingToAlgebraic

isAcyclicMatchingWith ::
  (Eq r, Num r) =>
  MorsePivotOps r ->
  FiniteChainComplex r ->
  AlgebraicMorseMatching r ->
  Bool
isAcyclicMatchingWith pivotOps chainComplex matching =
  isAcyclicMatchingAgainstEdgeMapWith pivotOps (allBasisCellRefs chainComplex) (boundaryEdgeMap chainComplex) matching

isAcyclicMatchingAgainstEdgeMapWith ::
  (Eq r, Num r) =>
  MorsePivotOps r ->
  [BasisCellRef] ->
  DirectedEdgeMap r ->
  AlgebraicMorseMatching r ->
  Bool
isAcyclicMatchingAgainstEdgeMapWith pivotOps allCells initialEdgeMap matching =
  uniquePairEndpoints localizedLowerCell localizedUpperCell matchingPairs
    && all pairIsValid matchingPairs
    && Set.fromList (lamCriticalCells matching) == Set.fromList expectedCritical
    && graphIsAcyclic finalEdgeMap
  where
    matchingPairs = lamPairs matching
    expectedCritical = expectedCriticalCells localizedLowerCell localizedUpperCell allCells matchingPairs
    finalEdgeMap = foldl' (flip (reverseCandidateEdgeWith pivotOps)) initialEdgeMap matchingPairs
    pairIsValid candidatePair =
      let lowerCell = localizedLowerCell candidatePair
          upperCell = localizedUpperCell candidatePair
          coefficientValue = localizedIncidenceCoefficient candidatePair
       in isCodimensionOne lowerCell upperCell
            && maybe False (const True) (mpoUnitInverse pivotOps coefficientValue)
            && Map.lookup (upperCell, lowerCell) initialEdgeMap == Just coefficientValue

isAcyclicMatchingLocalized ::
  Integral r =>
  FiniteChainComplex r ->
  LocalizedAcyclicMatching RationalAcyclicPair ->
  Bool
isAcyclicMatchingLocalized chainComplex matching =
  isAcyclicMatchingWith rationalMorsePivotOps (rationalizeFiniteChainComplex chainComplex) matching

extractCandidatePairsWith :: MorsePivotOps r -> DirectedEdgeMap r -> [AlgebraicMorsePair r]
extractCandidatePairsWith pivotOps edgeMap =
  edgeMap
    & Map.toAscList
    & mapMaybe
      ( \((upperCell, lowerCell), coefficientValue) ->
          if maybe False (const True) (mpoUnitInverse pivotOps coefficientValue)
            then
              Just
                LocalizedAcyclicPair
                  { lapLowerCell = lowerCell,
                    lapUpperCell = upperCell,
                    lapIncidenceCoefficient = coefficientValue
                  }
            else Nothing
      )

extractCandidatePairsLocalized :: DirectedEdgeMap Rational -> [RationalAcyclicPair]
extractCandidatePairsLocalized =
  extractCandidatePairsWith rationalMorsePivotOps

reverseCandidateEdgeWith ::
  Num r =>
  MorsePivotOps r ->
  AlgebraicMorsePair r ->
  DirectedEdgeMap r ->
  DirectedEdgeMap r
reverseCandidateEdgeWith pivotOps candidatePair =
  case mpoUnitInverse pivotOps (localizedIncidenceCoefficient candidatePair) of
    Nothing -> id
    Just inverseCoefficient ->
      Map.delete (localizedUpperCell candidatePair, localizedLowerCell candidatePair)
        . Map.insert
          (localizedLowerCell candidatePair, localizedUpperCell candidatePair)
          (negate inverseCoefficient)

reverseCandidateEdgeLocalized ::
  RationalAcyclicPair ->
  DirectedEdgeMap Rational ->
  DirectedEdgeMap Rational
reverseCandidateEdgeLocalized =
  reverseCandidateEdgeWith rationalMorsePivotOps

acyclicMatchingRational ::
  FiniteChainComplex Rational ->
  (BasisCellRef -> Double) ->
  LocalizedAcyclicMatching RationalAcyclicPair
acyclicMatchingRational chainComplex cellScore =
  acyclicMatchingWith rationalMorsePivotOps chainComplex cellScore

acyclicMatchingRationalWith ::
  (RationalAcyclicPair -> Bool) ->
  FiniteChainComplex Rational ->
  (BasisCellRef -> Double) ->
  LocalizedAcyclicMatching RationalAcyclicPair
acyclicMatchingRationalWith pairAllowed chainComplex cellScore =
  acyclicMatchingFromEdgeMapWith
    rationalMorsePivotOps
    pairAllowed
    (allBasisCellRefs chainComplex)
    (boundaryEdgeMap chainComplex)
    cellScore

acyclicMatchingFromEdgeMapWith ::
  Num r =>
  MorsePivotOps r ->
  (AlgebraicMorsePair r -> Bool) ->
  [BasisCellRef] ->
  DirectedEdgeMap r ->
  (BasisCellRef -> Double) ->
  AlgebraicMorseMatching r
acyclicMatchingFromEdgeMapWith pivotOps pairAllowed allCells edgeMap cellScore =
  buildMatchingBy
    allCells
    cellScore
    edgeMap
    (filter pairAllowed (extractCandidatePairsWith pivotOps edgeMap))
    localizedLowerCell
    localizedUpperCell
    (reverseCandidateEdgeWith pivotOps)
    (\candidate witness -> LocalizedCollapseObstruction {lcoCandidate = candidate, lcoCycleWitness = witness})
    (\pairs criticalCells obstructions ->
       LocalizedAcyclicMatching
         { lamPairs = pairs,
           lamCriticalCells = criticalCells,
           lamObstructions = obstructions
         }
    )

morseComplexRational ::
  FiniteChainComplex Rational ->
  LocalizedAcyclicMatching RationalAcyclicPair ->
  Either HomologyFailure (LocalizedMorseComplex Rational)
morseComplexRational =
  morseComplexWith rationalMorsePivotOps

-- | Gate-free variant of 'morseComplexRational' for the refined descent's
-- interior stages: the stage complex is a Morse reduction of a complex the
-- first (gated) stage already validated, and the stage matching comes from
-- the internal greedy engine, which certifies acyclicity incrementally as it
-- accepts each pair. Re-running 'isAcyclicMatchingAgainstEdgeMapWith' and the
-- shape/nilpotence sweep per stage would re-prove established invariants.
morseComplexRationalTrusted ::
  FiniteChainComplex Rational ->
  LocalizedAcyclicMatching RationalAcyclicPair ->
  Either HomologyFailure (LocalizedMorseComplex Rational)
morseComplexRationalTrusted chainComplex matching =
  case buildReducedComplexDataTrusted chainComplex criticalCells reducedMatchingPairs finalEdgeMap of
    Left failureValue -> Left failureValue
    Right reducedData ->
      Right
        LocalizedMorseComplex
          { lmcMatching = matching,
            lmcReducedComplex = mrdReducedComplex reducedData,
            lmcCriticalBasis = mrdCriticalBasis reducedData,
            lmcProjection = mrdProjection reducedData,
            lmcInclusion = mrdInclusion reducedData,
            lmcHomotopy = mrdHomotopy reducedData
          }
  where
    allCells = allBasisCellRefs chainComplex
    initialEdgeMap = boundaryEdgeMap chainComplex
    reducedMatchingPairs = lamPairs matching
    criticalCells = expectedCriticalCells localizedLowerCell localizedUpperCell allCells reducedMatchingPairs
    finalEdgeMap = foldl' (flip (reverseCandidateEdgeWith rationalMorsePivotOps)) initialEdgeMap reducedMatchingPairs

buildMatchingBy ::
  [BasisCellRef] ->
  (BasisCellRef -> Double) ->
  DirectedEdgeMap edgeWeight ->
  [pair] ->
  (pair -> BasisCellRef) ->
  (pair -> BasisCellRef) ->
  (pair -> DirectedEdgeMap edgeWeight -> DirectedEdgeMap edgeWeight) ->
  (pair -> [BasisCellRef] -> obstruction) ->
  ([pair] -> [BasisCellRef] -> [obstruction] -> matching) ->
  matching
buildMatchingBy allCells cellScore initialEdgeMap initialCandidates lowerCell upperCell reverseEdge makeObstruction makeMatching =
  go initialGradientState Set.empty [] [] orderedCandidates
  where
    initialGradientState = gradientAcyclicState initialEdgeMap
    orderedCandidates = sortCandidates lowerCell upperCell cellScore initialCandidates
    go currentGradientState matchedCells acceptedPairs acceptedObstructions remainingCandidates =
      case remainingCandidates of
        [] ->
          let finalizedPairs = reverse acceptedPairs
           in makeMatching
                finalizedPairs
                (expectedCriticalCells lowerCell upperCell allCells finalizedPairs)
                (reverse acceptedObstructions)
        candidatePair : restCandidates
          | Set.member lower matchedCells || Set.member upper matchedCells ->
              go currentGradientState matchedCells acceptedPairs acceptedObstructions restCandidates
          | otherwise ->
              case reverseAcyclicState (reverseEdge candidatePair) upper lower currentGradientState of
                AcyclicRewriteRejected witnessPath ->
                  go
                    currentGradientState
                    matchedCells
                    acceptedPairs
                    (makeObstruction candidatePair (lower : witnessPath) : acceptedObstructions)
                    restCandidates
                AcyclicRewriteAccepted nextGradientState ->
                  go
                    nextGradientState
                    (Set.insert lower (Set.insert upper matchedCells))
                    (candidatePair : acceptedPairs)
                    acceptedObstructions
                    restCandidates
          where
            lower = lowerCell candidatePair
            upper = upperCell candidatePair

type MorseReductionData :: Type -> Type
data MorseReductionData r = MorseReductionData
  { mrdReducedComplex :: !(FiniteChainComplex r),
    mrdCriticalBasis :: !(Map BasisCellRef BasisCellRef),
    mrdProjection :: !(ChainMap BasisCellRef BasisCellRef r),
    mrdInclusion :: !(ChainMap BasisCellRef BasisCellRef r),
    mrdHomotopy :: !(ChainHomotopy BasisCellRef r)
  }

type BoundaryLookup :: Type -> Type
newtype BoundaryLookup r = BoundaryLookup
  (Map BasisCellRef [(r, BasisCellRef)])

buildReducedComplexData ::
  (Eq r, Num r, Semiring r) =>
  FiniteChainComplex r ->
  [BasisCellRef] ->
  [AlgebraicMorsePair r] ->
  DirectedEdgeMap r ->
  DirectedEdgeMap r ->
  Either HomologyFailure (MorseReductionData r)
buildReducedComplexData chainComplex criticalCells matchingPairs originalEdgeMap edgeMap = do
  validateMorseReductionInput chainComplex originalEdgeMap
  buildReducedComplexDataTrusted chainComplex criticalCells matchingPairs edgeMap

-- | The reduction body without the input gate. Interior callers (the refined
-- descent, stages ≥ 2) operate on complexes that are Morse reductions of an
-- already-validated complex — chain complexes by the reduction theorem — so
-- re-running shape and nilpotence validation there would re-prove a theorem
-- per stage. Exterior entry points go through 'buildReducedComplexData'.
buildReducedComplexDataTrusted ::
  (Eq r, Num r, Semiring r) =>
  FiniteChainComplex r ->
  [BasisCellRef] ->
  [AlgebraicMorsePair r] ->
  DirectedEdgeMap r ->
  Either HomologyFailure (MorseReductionData r)
buildReducedComplexDataTrusted chainComplex criticalCells matchingPairs edgeMap = do
  let originalCells = allBasisCellRefs chainComplex
      complexMaxDegree = maxHomologicalDegree chainComplex
      HomologicalDegree complexMaxDimension = complexMaxDegree
      adjacency = adjacencyMap edgeMap
      topologicalVertexOrder = topologicalOrderWithVertices originalCells adjacency
      sortedCriticalCells = sortOn basisCellKey criticalCells
      criticalCellsByDegree =
        Map.fromListWith (<>)
          [(basisCellDimension cell, [cell]) | cell <- sortedCriticalCells]
      criticalCellsAt degreeValue = Map.findWithDefault [] degreeValue criticalCellsByDegree
      pathWeightsBySource =
        pathWeightOracle edgeMap topologicalVertexOrder
      criticalBasis =
        Map.fromList
          [ ( BasisCellRef
                { cellDegree = HomologicalDegree degreeValue,
                  cellIndex = reducedIndex
                },
              originalCell
            )
          | degreeValue <- [0 .. complexMaxDimension],
            (reducedIndex, originalCell) <- zip [0 ..] (criticalCellsAt degreeValue)
          ]
      criticalOriginalToReduced =
        Map.fromList [(originalCell, reducedCell) | (reducedCell, originalCell) <- Map.toAscList criticalBasis]
      (projectionMap, inclusionMap, homotopyMap) =
        morseReductionMaps
          pathWeightsBySource
          criticalBasis
          criticalOriginalToReduced
          matchingPairs
  incidencesByDegree <-
    traverse
      ( \degreeValue ->
          (\incidence -> (degreeValue, incidence))
            <$> materializeIncidenceBoundary
              (reducedBoundaryOf pathWeightsBySource (criticalCellsAt (degreeValue - 1)))
              (criticalCellsAt degreeValue)
              (criticalCellsAt (degreeValue - 1))
      )
      [0 .. complexMaxDimension]
  let incidenceByDegree = Map.fromList incidencesByDegree
  let reducedComplex =
        mkFiniteChainComplex
          complexMaxDegree
          (\(HomologicalDegree degreeValue) -> Map.findWithDefault emptyBoundaryIncidence degreeValue incidenceByDegree)
  pure
    MorseReductionData
      { mrdReducedComplex = reducedComplex,
        mrdCriticalBasis = criticalBasis,
        mrdProjection = projectionMap,
        mrdInclusion = inclusionMap,
        mrdHomotopy = homotopyMap
      }

validateMorseReductionInput ::
  (Eq r, Num r) =>
  FiniteChainComplex r ->
  DirectedEdgeMap r ->
  Either HomologyFailure ()
validateMorseReductionInput chainComplex originalEdgeMap = do
  validateFiniteChainComplexShape chainComplex
  validateBoundaryNilpotenceAsMorseObstruction (boundaryLookupFromEdgeMap originalEdgeMap) (allBasisCellRefs chainComplex)

validateBoundaryNilpotenceAsMorseObstruction ::
  (Eq r, Num r) =>
  BoundaryLookup r ->
  [BasisCellRef] ->
  Either HomologyFailure ()
validateBoundaryNilpotenceAsMorseObstruction boundaryLookupValue cells =
  -- The condition checked here is d ∘ d = 0 on the input complex; report it
  -- as the nilpotence law (as every other detection path does), not as a
  -- reduction chain-map violation.
  LC.checkLawWith
    LC.numArithmetic
    ChainNilpotenceLaw
    cells
    (const [])
    (LC.composeWith LC.numArithmetic (morseBoundaryOfLookup boundaryLookupValue) . morseBoundaryOfLookup boundaryLookupValue)

reducedBoundaryOf :: (Eq r, Num r) => PathWeightOracle r -> [BasisCellRef] -> BasisCellRef -> [(r, BasisCellRef)]
reducedBoundaryOf pathWeightSums targetCells upperCell =
  -- One O(V+E) path-weight propagation per upper cell, shared by every
  -- lower cell — floated out of the lambda so the sharing does not depend
  -- on the optimizer's full-laziness pass.
  let weightsFromUpper = pathWeightsFromOracle pathWeightSums upperCell
   in targetCells
        & mapMaybe
          ( \lowerCell ->
              case Map.lookup lowerCell weightsFromUpper of
                Just coefficientValue
                  | coefficientValue /= 0 -> Just (coefficientValue, lowerCell)
                _ -> Nothing
          )

morseReductionMaps ::
  (Eq r, Num r) =>
  PathWeightOracle r ->
  Map BasisCellRef BasisCellRef ->
  Map BasisCellRef BasisCellRef ->
  [AlgebraicMorsePair r] ->
  (ChainMap BasisCellRef BasisCellRef r, ChainMap BasisCellRef BasisCellRef r, ChainHomotopy BasisCellRef r)
morseReductionMaps pathWeightSums criticalBasis criticalOriginalToReduced matchingPairs =
  (projectionMap, inclusionMap, homotopyMap)
  where
    pathWeightsFrom =
      pathWeightsFromOracle pathWeightSums

    matchedUpperCells =
      Set.fromList (fmap localizedUpperCell matchingPairs)

    projectionMap =
      ChainMap $
        \originalCell ->
          LC.normalizeWith LC.numArithmetic
            [ (coefficientValue, reducedCell)
            | (targetCell, coefficientValue) <- Map.toAscList (pathWeightsFrom originalCell),
              coefficientValue /= 0,
              cellDegree targetCell == cellDegree originalCell,
              Just reducedCell <- [Map.lookup targetCell criticalOriginalToReduced]
            ]

    inclusionMap =
      ChainMap $
        \reducedCell ->
          case Map.lookup reducedCell criticalBasis of
            Nothing -> []
            Just criticalOriginal ->
              LC.normalizeWith LC.numArithmetic
                [ (coefficientValue, targetCell)
                | (targetCell, coefficientValue) <- Map.toAscList (pathWeightsFrom criticalOriginal),
                  coefficientValue /= 0,
                  cellDegree targetCell == cellDegree criticalOriginal
                ]

    homotopyMap =
      ChainHomotopy $
        \originalCell ->
          LC.normalizeWith LC.numArithmetic
            [ (negate coefficientValue, targetCell)
            | (targetCell, coefficientValue) <- Map.toAscList (pathWeightsFrom originalCell),
              coefficientValue /= 0,
              Set.member targetCell matchedUpperCells,
              basisCellDimension targetCell == basisCellDimension originalCell + 1
            ]

morseBoundaryOfLookup ::
  BoundaryLookup r ->
  BasisCellRef ->
  [(r, BasisCellRef)]
morseBoundaryOfLookup (BoundaryLookup boundaryBySource) cell =
  Map.findWithDefault [] cell boundaryBySource

boundaryLookupFromEdgeMap :: (Eq r, Num r) => DirectedEdgeMap r -> BoundaryLookup r
boundaryLookupFromEdgeMap edgeMap =
  BoundaryLookup
    (Map.map (LC.normalizeWith LC.numArithmetic) boundaryBySource)
  where
    indexedBoundaryEntries =
      [ (sourceCell, targetCell, coefficientValue)
      | ((sourceCell, targetCell), coefficientValue) <- Map.toAscList edgeMap,
        coefficientValue /= 0
      ]
    boundaryBySource =
      Map.fromListWith
        (<>)
        [ (sourceCell, [(coefficientValue, targetCell)])
        | (sourceCell, targetCell, coefficientValue) <- indexedBoundaryEntries
        ]

integralMatchingFromAlgebraic :: AlgebraicMorseMatching Integer -> AcyclicMatching
integralMatchingFromAlgebraic matching =
  AcyclicMatching
    { amPairs = fmap integralPairFromAlgebraic (lamPairs matching),
      amCriticalCells = lamCriticalCells matching,
      amObstructions = fmap integralObstructionFromAlgebraic (lamObstructions matching)
    }

integralPairFromAlgebraic :: AlgebraicMorsePair Integer -> AcyclicPair
integralPairFromAlgebraic candidatePair =
  AcyclicPair
    { apLowerCell = localizedLowerCell candidatePair,
      apUpperCell = localizedUpperCell candidatePair,
      apIncidenceCoefficient = fromIntegral (localizedIncidenceCoefficient candidatePair)
    }

integralObstructionFromAlgebraic ::
  LocalizedCollapseObstruction (AlgebraicMorsePair Integer) ->
  CollapseObstruction
integralObstructionFromAlgebraic obstruction =
  CollapseObstruction
    { coCandidate = integralPairFromAlgebraic (lcoCandidate obstruction),
      coCycleWitness = lcoCycleWitness obstruction
    }

integralMatchingToAlgebraic :: Num r => AcyclicMatching -> AlgebraicMorseMatching r
integralMatchingToAlgebraic matching =
  LocalizedAcyclicMatching
    { lamPairs = fmap integralPairToAlgebraic (amPairs matching),
      lamCriticalCells = amCriticalCells matching,
      lamObstructions = fmap integralObstructionToAlgebraic (amObstructions matching)
    }

integralPairToAlgebraic :: Num r => AcyclicPair -> AlgebraicMorsePair r
integralPairToAlgebraic candidatePair =
  LocalizedAcyclicPair
    { lapLowerCell = apLowerCell candidatePair,
      lapUpperCell = apUpperCell candidatePair,
      lapIncidenceCoefficient = fromIntegral (apIncidenceCoefficient candidatePair)
    }

integralObstructionToAlgebraic ::
  Num r =>
  CollapseObstruction ->
  LocalizedCollapseObstruction (AlgebraicMorsePair r)
integralObstructionToAlgebraic obstruction =
  LocalizedCollapseObstruction
    { lcoCandidate = integralPairToAlgebraic (coCandidate obstruction),
      lcoCycleWitness = coCycleWitness obstruction
    }

boundaryEdgeMap :: (Eq r, Num r) => FiniteChainComplex r -> DirectedEdgeMap r
boundaryEdgeMap chainComplex =
  Map.filter (/= 0) $
    Map.fromListWith (+)
      [ (edgeKey, coefficientValue)
      | degreeValue <- [1 .. maxDegreeValue],
        let boundaryIncidence = incidenceMatrixAt chainComplex (HomologicalDegree degreeValue),
        boundaryEntry <- boundaryEntries boundaryIncidence,
        let coefficientValue = boundaryCoefficient boundaryEntry,
        let edgeKey =
              ( BasisCellRef
                  { cellDegree = HomologicalDegree degreeValue,
                    cellIndex = sourceIndex boundaryEntry
                  },
                BasisCellRef
                  { cellDegree = HomologicalDegree (degreeValue - 1),
                    cellIndex = targetIndex boundaryEntry
                  }
              )
      ]
  where
    HomologicalDegree maxDegreeValue = maxHomologicalDegree chainComplex

integralBoundaryEdgeMap :: Integral r => FiniteChainComplex r -> DirectedEdgeMap Integer
integralBoundaryEdgeMap = Map.map toInteger . boundaryEdgeMap

rationalizeFiniteChainComplex :: Integral r => FiniteChainComplex r -> FiniteChainComplex Rational
rationalizeFiniteChainComplex chainComplex =
  mkFiniteChainComplex
    (maxHomologicalDegree chainComplex)
    (mapBoundaryCoefficients fromIntegral . incidenceMatrixAt chainComplex)

expectedCriticalCells ::
  (pair -> BasisCellRef) ->
  (pair -> BasisCellRef) ->
  [BasisCellRef] ->
  [pair] ->
  [BasisCellRef]
expectedCriticalCells lowerCell upperCell allCells matchingPairs =
  filter (`Set.notMember` matchedCells) allCells
  where
    matchedCells =
      Set.fromList
        [ cell
        | candidatePair <- matchingPairs,
          cell <- [lowerCell candidatePair, upperCell candidatePair]
        ]

uniquePairEndpoints ::
  (pair -> BasisCellRef) ->
  (pair -> BasisCellRef) ->
  [pair] ->
  Bool
uniquePairEndpoints lowerCell upperCell matchingPairs =
  length endpointCells == Set.size (Set.fromList endpointCells)
  where
    endpointCells =
      [ cell
      | candidatePair <- matchingPairs,
        cell <- [lowerCell candidatePair, upperCell candidatePair]
      ]

sortCandidates ::
  (pair -> BasisCellRef) ->
  (pair -> BasisCellRef) ->
  (BasisCellRef -> Double) ->
  [pair] ->
  [pair]
sortCandidates lowerCell upperCell cellScore =
  sortOn
    (\candidatePair ->
       ( cellScore (upperCell candidatePair),
         cellScore (lowerCell candidatePair),
         basisCellKey (upperCell candidatePair),
         basisCellKey (lowerCell candidatePair)
       )
    )

gradientAcyclicState ::
  DirectedEdgeMap edgeWeight ->
  GradientAcyclicState edgeWeight
gradientAcyclicState edgeMap =
  GradientAcyclicState
    { gasEdgeMap = edgeMap,
      gasAdjacency = adjacencyMap edgeMap,
      gasMatchedUpperByLower = Map.empty,
      gasGradientAdjacency = Map.empty
    }

reverseAcyclicState ::
  (DirectedEdgeMap edgeWeight -> DirectedEdgeMap edgeWeight) ->
  BasisCellRef ->
  BasisCellRef ->
  GradientAcyclicState edgeWeight ->
  AcyclicRewrite edgeWeight
reverseAcyclicState reverseEdge upperCell lowerCell state =
  let lowerTargets =
        gasAdjacency state
          & Map.findWithDefault [] upperCell
          & filter (/= lowerCell)
   in case gradientCycleWitness upperCell lowerCell lowerTargets state of
        Just witnessPath ->
          AcyclicRewriteRejected witnessPath
        Nothing ->
          AcyclicRewriteAccepted
            GradientAcyclicState
              { gasEdgeMap = reverseEdge (gasEdgeMap state),
                gasAdjacency = insertAdjacencyEdge lowerCell upperCell (deleteAdjacencyEdge upperCell lowerCell (gasAdjacency state)),
                gasMatchedUpperByLower = Map.insert lowerCell upperCell (gasMatchedUpperByLower state),
                gasGradientAdjacency =
                  if null lowerTargets
                    then gasGradientAdjacency state
                    else Map.insert lowerCell lowerTargets (gasGradientAdjacency state)
              }

gradientCycleWitness ::
  BasisCellRef ->
  BasisCellRef ->
  [BasisCellRef] ->
  GradientAcyclicState edgeWeight ->
  Maybe [BasisCellRef]
gradientCycleWitness upperCell lowerCell lowerTargets state =
  firstJust
    ( \targetCell ->
        gradientPathInAdjacency (gasGradientAdjacency state) targetCell lowerCell
          >>= directedPathFromGradientPath upperCell (gasMatchedUpperByLower state)
    )
    lowerTargets

gradientPathInAdjacency ::
  DirectedAdjacencyMap ->
  BasisCellRef ->
  BasisCellRef ->
  Maybe [BasisCellRef]
gradientPathInAdjacency adjacency startCell goalCell =
  snd (findFrom Set.empty startCell)
  where
    findFrom visited currentCell
      | currentCell == goalCell = (visited, Just [goalCell])
      | Set.member currentCell visited = (visited, Nothing)
      | otherwise =
          foldl'
            (findThroughSuccessor currentCell)
            (Set.insert currentCell visited, Nothing)
            (Map.findWithDefault [] currentCell adjacency)
    findThroughSuccessor currentCell (visited, discoveredPath) successorCell =
      case discoveredPath of
        Just pathValue -> (visited, Just pathValue)
        Nothing ->
          case findFrom visited successorCell of
            (visitedAfterSuccessor, Just suffixPath) ->
              (visitedAfterSuccessor, Just (currentCell : suffixPath))
            (visitedAfterSuccessor, Nothing) ->
              (visitedAfterSuccessor, Nothing)

directedPathFromGradientPath ::
  BasisCellRef ->
  Map BasisCellRef BasisCellRef ->
  [BasisCellRef] ->
  Maybe [BasisCellRef]
directedPathFromGradientPath upperCell matchedUpperByLower gradientPath =
  fmap (upperCell :) (expandGradientPath gradientPath)
  where
    expandGradientPath pathValue =
      case pathValue of
        [] -> Just []
        [terminalLower] -> Just [terminalLower]
        lowerValue : remainingPath@(_ : _) ->
          case Map.lookup lowerValue matchedUpperByLower of
            Nothing -> Nothing
            Just matchedUpper ->
              fmap ((lowerValue :) . (matchedUpper :)) (expandGradientPath remainingPath)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust selectValue =
  foldr
    ( \value restValue ->
        case selectValue value of
          Just selectedValue -> Just selectedValue
          Nothing -> restValue
    )
    Nothing

rebaseCell :: Map BasisCellRef BasisCellRef -> BasisCellRef -> BasisCellRef
rebaseCell basis cell = Map.findWithDefault cell cell basis

rebaseLocalizedPair ::
  Map BasisCellRef BasisCellRef ->
  LocalizedAcyclicPair r ->
  LocalizedAcyclicPair r
rebaseLocalizedPair basis candidatePair =
  LocalizedAcyclicPair
    { lapLowerCell = rebaseCell basis (localizedLowerCell candidatePair),
      lapUpperCell = rebaseCell basis (localizedUpperCell candidatePair),
      lapIncidenceCoefficient = localizedIncidenceCoefficient candidatePair
    }

rebaseLocalizedMatching ::
  Map BasisCellRef BasisCellRef ->
  LocalizedAcyclicMatching RationalAcyclicPair ->
  LocalizedAcyclicMatching RationalAcyclicPair
rebaseLocalizedMatching basis matching =
  LocalizedAcyclicMatching
    { lamPairs = fmap (rebaseLocalizedPair basis) (lamPairs matching),
      lamCriticalCells = fmap (rebaseCell basis) (lamCriticalCells matching),
      lamObstructions = fmap (rebaseLocalizedObstruction basis) (lamObstructions matching)
    }

rebaseLocalizedObstruction ::
  Map BasisCellRef BasisCellRef ->
  LocalizedCollapseObstruction RationalAcyclicPair ->
  LocalizedCollapseObstruction RationalAcyclicPair
rebaseLocalizedObstruction basis obstruction =
  LocalizedCollapseObstruction
    { lcoCandidate = rebaseLocalizedPair basis (lcoCandidate obstruction),
      lcoCycleWitness = fmap (rebaseCell basis) (lcoCycleWitness obstruction)
    }

composeCriticalBasis ::
  Map BasisCellRef BasisCellRef ->
  Map BasisCellRef BasisCellRef ->
  Map BasisCellRef BasisCellRef
composeCriticalBasis priorBasis = fmap (rebaseCell priorBasis)

localizedLowerCell :: LocalizedAcyclicPair r -> BasisCellRef
localizedLowerCell LocalizedAcyclicPair {lapLowerCell = lowerCell} = lowerCell

localizedUpperCell :: LocalizedAcyclicPair r -> BasisCellRef
localizedUpperCell LocalizedAcyclicPair {lapUpperCell = upperCell} = upperCell

localizedIncidenceCoefficient :: LocalizedAcyclicPair r -> r
localizedIncidenceCoefficient LocalizedAcyclicPair {lapIncidenceCoefficient = coefficientValue} = coefficientValue

isCodimensionOne :: BasisCellRef -> BasisCellRef -> Bool
isCodimensionOne lowerCell upperCell =
  basisCellDimension upperCell == basisCellDimension lowerCell + 1

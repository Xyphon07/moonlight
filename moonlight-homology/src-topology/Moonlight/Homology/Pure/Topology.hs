module Moonlight.Homology.Pure.Topology
  ( CellTypes (..),
    Dimension (..),
    CellRef (..),
    cellDimension,
    OrientedEdge (..),
    CellComplex2D (..),
    ValidateComplex2D (..),
    isBoundaryEdge,
    isInteriorEdge,
    eulerCharacteristic,
    BasisCellRef (..),
    MorsePivotOps (..),
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
    RefinedMatchingSummary (..),
    FiltrationValue (..),
    FilteredFiniteChainComplex (..),
    TopologyObservationConfig (..),
    defaultTopologyObservationConfig,
    TopologyWitnessSeed (..),
    CriticalKind (..),
    PotentialValue,
    PotentialValueError (..),
    unPotentialValue,
    mkPotentialValue,
    PotentialNormalization (..),
    CellCarrier,
    CellCarrierError (..),
    carrierDegree,
    carrierCells,
    mkCellCarrier,
    ScalarPotentialField,
    ScalarPotentialFieldError (..),
    scalarPotentialCarrier,
    scalarPotentialNormalization,
    scalarPotentialSamples,
    mkScalarPotentialField,
    mkScalarPotentialFieldFromSamples,
    DirectionSymmetryOrder,
    DirectionSymmetryOrderError (..),
    unDirectionSymmetryOrder,
    mkDirectionSymmetryOrder,
    DirectionPhase,
    DirectionPhaseError (..),
    unDirectionPhase,
    mkDirectionPhase,
    DirectionCoefficient,
    DirectionCoefficientError (..),
    unDirectionCoefficient,
    mkDirectionCoefficient,
    DirectionFieldEncoding (..),
    DirectionField,
    DirectionFieldError (..),
    directionFieldCarrier,
    directionFieldSymmetryOrder,
    directionFieldEncoding,
    mkDirectionField,
    mkDirectionAngleField,
    mkDirectionCochainField,
    ReebNodeId (..),
    ReebArcId (..),
    MorseReebNode (..),
    Monotonicity (..),
    MorseReebArc (..),
    MorseReebScaffold (..),
    SingularityIndex (..),
    SingularityId (..),
    Singularity (..),
    HarmonicLoopId (..),
    HarmonicLoopWeight (..),
    HarmonicLoopPeriod (..),
    HarmonicLoop (..),
    MacroScaffoldIR (..),
    MacroScaffoldCompositionError (..),
    StitchRoute (..),
    StitchRouteKey (..),
    StitchBoundarySide (..),
    StitchSupportSelection (..),
    StitchSupportRefinement (..),
    StitchSemantics (..),
    MacroScaffoldStitchError (..),
    composeMacroScaffoldsWithScopes,
    composeMacroScaffolds,
    stitchMacroScaffoldRoutes,
    GraphEdge (..),
    Graph1Skeleton (..),
    graphAdjacency,
    GraphSkeletonExtractionFailure (..),
    GraphSpectralMode (..),
    mkFilteredFiniteChainComplex,
    eulerCharacteristicOf,
    integralHomologyGroupsOf,
    exactRepresentativeClassesOf,
    freeBettiVector,
    representativeCyclesOverQ,
    representativeCocyclesOverQ,
    homologyBasisAt,
    cohomologyBasisAt,
    sparseHomologyBasisAt,
    sparseCohomologyBasisAt,
    sparseFreeBettiVector,
    sparseQuotientRepresentatives,
    QuotientPresentation (..),
    mkQuotientPresentation,
    presentationCoordinates,
    quotientRepresentatives,
    vectorToRepresentative,
    representativeToVector,
    exactTopologyWitness,
    mod2PersistentPairs,
    CriticalBettiTable,
    criticalBettiDegreeCount,
    criticalBettiRankCount,
    criticalBettiTableValues,
    criticalBettiVectors,
    mod2PersistentPairsWithCriticalBettiTable,
    persistentBettiAt,
    persistentBettiAtMany,
    persistentBettiAtCriticalValues,
    mod2PersistenceTopologyWitness,
    TopologyTarget (..),
    TargetViolation (..),
    topologyTargetConstraints,
    validateTarget,
    validateTargets,
    validateTopologyTarget,
    validateTopologyTargets,
    graphFromEdgeSupports,
    graph1SkeletonFromComplex,
    graphFiniteChainComplex,
    graphMacroScaffold,
    acyclicMatching,
    acyclicMatchingLocalized,
    refinedAcyclicMatchingTranscript,
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
    graphSpectralModes,
    graphTopologyWitness,
    criticalKindAt,
    lowerNeighborEdges,
    lowerNeighbors,
    higherNeighborEdges,
    higherNeighbors,
    isAcyclicMatching,
    isAcyclicMatchingLocalized,
    morseComplex,
    morseComplexLocalized,
    extractCandidatePairsLocalized,
    reverseCandidateEdgeLocalized,
    addUndirectedAdjacency,
    connectedComponentsFromAdjacency,
    observeGraphTopologyWitness,
    observeTopologyWitnessSeed,
    observeTopologyWitness,
    Orientation (..),
    RawCellData (..),
    RawCellScopes (..),
    RealizationBudget (..),
    realizeScaffoldRawWithScopes,
    realizeScaffoldRaw,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex)
import Moonlight.Homology.Pure.Chain
  ( TopologyWitness (..),
    emptyTopologyWitness,
    mergeTopologyWitness,
  )
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Topology.Algebra
import Moonlight.Homology.Pure.Topology.SparseAlgebra
import Moonlight.Homology.Pure.Topology.CellComplex
import Moonlight.Homology.Pure.Topology.Core
import Moonlight.Homology.Pure.Topology.Graph
import Moonlight.Homology.Pure.Topology.Harmonic (attachDiscoveredHarmonicLoops)
import Moonlight.Homology.Pure.Topology.Integral (exactRepresentativeClassesOf, integralHomologyGroupsOf)
import Moonlight.Homology.Pure.Topology.MacroScaffold
import Moonlight.Homology.Pure.Topology.Morse
import Moonlight.Homology.Pure.Topology.MacroScaffold.Compose
import Moonlight.Homology.Pure.Topology.Observation
import Moonlight.Homology.Pure.Topology.Persistence
import Moonlight.Homology.Pure.Topology.Realize
import Moonlight.Homology.Pure.Topology.Target

type TopologyWitnessSeed :: Type -> Type
data TopologyWitnessSeed r
  = GraphTopologySeed Graph1Skeleton (Maybe (FilteredFiniteChainComplex FiltrationValue r)) (Maybe ScalarPotentialField) Int
  | FiniteTopologySeed (FiniteChainComplex r) (TopologyObservationConfig r)

observeTopologyWitnessSeed ::
  Integral r =>
  TopologyWitnessSeed r ->
  Either HomologyFailure (TopologyWitness MacroScaffoldIR GraphSpectralMode FiltrationValue Rational Int)
observeTopologyWitnessSeed topologySeed =
  case topologySeed of
    GraphTopologySeed skeleton maybeFiltered maybePotential requestedModeCount ->
      observeGraphTopologyWitness maybeFiltered maybePotential requestedModeCount skeleton
    FiniteTopologySeed finite config ->
      observeTopologyWitness config finite

observeGraphTopologyWitness ::
  Integral r =>
  Maybe (FilteredFiniteChainComplex FiltrationValue r) ->
  Maybe ScalarPotentialField ->
  Int ->
  Graph1Skeleton ->
  Either HomologyFailure (TopologyWitness MacroScaffoldIR GraphSpectralMode FiltrationValue Rational Int)
observeGraphTopologyWitness maybeFiltered maybePotential requestedModeCount skeleton = do
  graphComplex <- (graphFiniteChainComplex skeleton :: Either HomologyFailure (FiniteChainComplex Integer))
  exactWitness <- exactTopologyWitness graphComplex
  persistenceWitness <-
    maybe
      (Right emptyTopologyWitness)
      mod2PersistenceTopologyWitness
      maybeFiltered
  graphWitness <- graphTopologyWitness requestedModeCount maybePotential skeleton
  pure
    ( exactWitness
        `mergeTopologyWitness` persistenceWitness
        `mergeTopologyWitness` graphWitness
        & attachDiscoveredHarmonicLoops graphComplex
    )

observeTopologyWitness ::
  Integral r =>
  TopologyObservationConfig r ->
  FiniteChainComplex r ->
  Either HomologyFailure (TopologyWitness MacroScaffoldIR GraphSpectralMode FiltrationValue Rational Int)
observeTopologyWitness config finite = do
  exactWitness <- exactTopologyWitness finite
  persistenceWitness <-
    maybe
      (Right emptyTopologyWitness)
      mod2PersistenceTopologyWitness
      (observationFiltration config)
  graphWitness <-
    case graph1SkeletonFromComplex finite of
      Right skeleton ->
        graphTopologyWitness
          (observationLowModeCount config)
          (observationPotential config)
          skeleton
      Left extractionFailure ->
        if observationLowModeCount config > 0 || maybe False (const True) (observationPotential config)
          then Left (InvalidTopologyInput ("graph extraction requires explicit oriented unit two-endpoint 1-cell boundaries: " <> show extractionFailure))
          else Right emptyTopologyWitness
  pure
    ( exactWitness
        `mergeTopologyWitness` persistenceWitness
        `mergeTopologyWitness` graphWitness
        & attachDiscoveredHarmonicLoops finite
    )

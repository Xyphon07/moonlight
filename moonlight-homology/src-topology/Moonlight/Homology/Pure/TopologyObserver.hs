module Moonlight.Homology.Pure.TopologyObserver
  ( TopologyObserver,
    runTopologyObserver,
    WitnessInterpreter,
    mkWitnessInterpreter,
    runWitnessInterpreter,
    observeEulerCharacteristic,
    observeBettiVector,
    observeIntegralHomology,
    observeIntegralHomologyAt,
    observeTorsionFamily,
    observeExactRepresentativeClasses,
    observeExactRepresentativeClassCount,
    observePersistencePairs,
    observePersistenceCount,
    observeCoefficientRepresentativeCycles,
    observeCoefficientRepresentativeCycleCount,
    observeCoefficientRepresentativeCocycles,
    observeCoefficientRepresentativeCocycleCount,
    observeHarmonicBasis,
    observeHarmonicCount,
    observeMacroScaffold,
    observeScaffoldSummary,
    observeLowSpectralModes,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Chain
  ( EulerCharacteristic,
    ExactRepresentativeClass,
    HarmonicBasisElement,
    HomologicalDegree,
    PersistencePair,
    RepresentativeChain,
    TopologyWitness,
  )
import Moonlight.Homology.Pure.Graded.Query (DegreeSelection)
import Moonlight.Homology.Pure.GradedTorsion (GradedTorsionFamily)
import Moonlight.Homology.Pure.Group (HomologyGroup)
import Moonlight.Homology.Pure.TopologyView
  ( ScaffoldSummaryAlgebra,
    TopologyView,
    WitnessSlice,
    mkTopologyView,
    runWitnessSlice,
    topologyViewTorsionFamily,
    sliceBettiVector,
    sliceCoefficientRepresentativeCocycles,
    sliceCoefficientRepresentativeCycles,
    sliceEulerCharacteristic,
    sliceExactRepresentativeClasses,
    sliceHarmonicBasis,
    sliceIntegralHomology,
    sliceIntegralHomologyAt,
    sliceLowSpectralModes,
    sliceMacroScaffold,
    slicePersistencePairs,
    sliceScaffoldSummary,
  )

type TopologyObserver :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype TopologyObserver summary scaffold spectral persistence coefficient basis observed = TopologyObserver
  { runTopologyObserver :: TopologyView summary scaffold spectral persistence coefficient basis -> observed
  }

instance Functor (TopologyObserver summary scaffold spectral persistence coefficient basis) where
  fmap mapper (TopologyObserver observeValue) =
    TopologyObserver (mapper . observeValue)

instance Applicative (TopologyObserver summary scaffold spectral persistence coefficient basis) where
  pure value =
    TopologyObserver (const value)
  TopologyObserver observeFunction <*> TopologyObserver observeValue =
    TopologyObserver (\topologyViewValue -> observeFunction topologyViewValue (observeValue topologyViewValue))

type WitnessInterpreter :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype WitnessInterpreter summary scaffold spectral persistence coefficient basis observed = WitnessInterpreter
  { runWitnessInterpreter :: TopologyWitness scaffold spectral persistence coefficient basis -> observed
  }

mkWitnessInterpreter ::
  ScaffoldSummaryAlgebra scaffold summary ->
  TopologyObserver summary scaffold spectral persistence coefficient basis observed ->
  WitnessInterpreter summary scaffold spectral persistence coefficient basis observed
mkWitnessInterpreter summaryAlgebra (TopologyObserver observeValue) =
  WitnessInterpreter (observeValue . mkTopologyView summaryAlgebra)

observeWithSlice ::
  WitnessSlice summary scaffold spectral persistence coefficient basis observed ->
  TopologyObserver summary scaffold spectral persistence coefficient basis observed
observeWithSlice sliceValue =
  TopologyObserver (runWitnessSlice sliceValue)

observeEulerCharacteristic ::
  TopologyObserver summary scaffold spectral persistence coefficient basis (Maybe EulerCharacteristic)
observeEulerCharacteristic =
  observeWithSlice sliceEulerCharacteristic

observeBettiVector ::
  TopologyObserver summary scaffold spectral persistence coefficient basis [Int]
observeBettiVector =
  observeWithSlice sliceBettiVector

observeIntegralHomology ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis [HomologyGroup Integer]
observeIntegralHomology selectionValue =
  observeWithSlice (sliceIntegralHomology selectionValue)

observeIntegralHomologyAt ::
  HomologicalDegree ->
  TopologyObserver summary scaffold spectral persistence coefficient basis (Maybe (HomologyGroup Integer))
observeIntegralHomologyAt degreeValue =
  observeWithSlice (sliceIntegralHomologyAt degreeValue)

observeTorsionFamily ::
  TopologyObserver summary scaffold spectral persistence coefficient basis GradedTorsionFamily
observeTorsionFamily =
  TopologyObserver topologyViewTorsionFamily

observeExactRepresentativeClasses ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis [ExactRepresentativeClass basis]
observeExactRepresentativeClasses selectionValue =
  observeWithSlice (sliceExactRepresentativeClasses selectionValue)

observeExactRepresentativeClassCount ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis Int
observeExactRepresentativeClassCount selectionValue =
  length <$> observeExactRepresentativeClasses selectionValue

observePersistencePairs ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis [PersistencePair persistence]
observePersistencePairs selectionValue =
  observeWithSlice (slicePersistencePairs selectionValue)

observePersistenceCount ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis Int
observePersistenceCount selectionValue =
  length <$> observePersistencePairs selectionValue

observeCoefficientRepresentativeCycles ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis [RepresentativeChain coefficient basis]
observeCoefficientRepresentativeCycles selectionValue =
  observeWithSlice (sliceCoefficientRepresentativeCycles selectionValue)

observeCoefficientRepresentativeCycleCount ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis Int
observeCoefficientRepresentativeCycleCount selectionValue =
  length <$> observeCoefficientRepresentativeCycles selectionValue

observeCoefficientRepresentativeCocycles ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis [RepresentativeChain coefficient basis]
observeCoefficientRepresentativeCocycles selectionValue =
  observeWithSlice (sliceCoefficientRepresentativeCocycles selectionValue)

observeCoefficientRepresentativeCocycleCount ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis Int
observeCoefficientRepresentativeCocycleCount selectionValue =
  length <$> observeCoefficientRepresentativeCocycles selectionValue

observeHarmonicBasis ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis [HarmonicBasisElement coefficient basis]
observeHarmonicBasis selectionValue =
  observeWithSlice (sliceHarmonicBasis selectionValue)

observeHarmonicCount ::
  DegreeSelection ->
  TopologyObserver summary scaffold spectral persistence coefficient basis Int
observeHarmonicCount selectionValue =
  length <$> observeHarmonicBasis selectionValue

observeMacroScaffold ::
  TopologyObserver summary scaffold spectral persistence coefficient basis (Maybe scaffold)
observeMacroScaffold =
  observeWithSlice sliceMacroScaffold

observeScaffoldSummary ::
  TopologyObserver summary scaffold spectral persistence coefficient basis (Maybe summary)
observeScaffoldSummary =
  observeWithSlice sliceScaffoldSummary

observeLowSpectralModes ::
  TopologyObserver summary scaffold spectral persistence coefficient basis [spectral]
observeLowSpectralModes =
  observeWithSlice sliceLowSpectralModes

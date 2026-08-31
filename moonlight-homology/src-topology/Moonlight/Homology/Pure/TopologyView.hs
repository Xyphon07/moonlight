module Moonlight.Homology.Pure.TopologyView
  ( ScaffoldSummaryAlgebra,
    mkScaffoldSummaryAlgebra,
    runScaffoldSummaryAlgebra,
    TopologyView,
    WitnessSlice,
    mkTopologyView,
    runWitnessSlice,
    topologyViewTorsionFamily,
    sliceEulerCharacteristic,
    sliceBettiVector,
    sliceIntegralHomology,
    sliceIntegralHomologyAt,
    sliceExactRepresentativeClasses,
    slicePersistencePairs,
    sliceCoefficientRepresentativeCycles,
    sliceCoefficientRepresentativeCocycles,
    sliceHarmonicBasis,
    sliceMacroScaffold,
    sliceScaffoldSummary,
    sliceLowSpectralModes,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Chain
  ( EulerCharacteristic,
    ExactRepresentativeClass (..),
    HarmonicBasisElement (..),
    HomologicalDegree,
    PersistencePair (..),
    RepresentativeChain (..),
    TopologyWitness (..),
  )
import Moonlight.Homology.Pure.Graded.Query
  ( DegreeSelection,
    enumerateDegreeIndexed,
    lookupDegreeIndexed,
    selectDegreeIndexed,
    selectGradedMembers,
  )
import Moonlight.Homology.Pure.GradedTorsion
  ( GradedTorsionFamily,
    mkGradedTorsionFamily,
  )
import Moonlight.Homology.Pure.Group (HomologyGroup)

type ScaffoldSummaryAlgebra :: Type -> Type -> Type
newtype ScaffoldSummaryAlgebra scaffold summary = ScaffoldSummaryAlgebra
  { runScaffoldSummaryAlgebra :: scaffold -> summary
  }

mkScaffoldSummaryAlgebra :: (scaffold -> summary) -> ScaffoldSummaryAlgebra scaffold summary
mkScaffoldSummaryAlgebra = ScaffoldSummaryAlgebra

type TopologyView :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data TopologyView summary scaffold spectral persistence coefficient basis = TopologyView
  { topologyViewEulerCharacteristic :: Maybe EulerCharacteristic,
    topologyViewBettiVector :: [Int],
    topologyViewIntegralHomology :: [(HomologicalDegree, HomologyGroup Integer)],
    topologyViewTorsionFamily :: GradedTorsionFamily,
    topologyViewExactRepresentativeClasses :: [ExactRepresentativeClass basis],
    topologyViewPersistencePairs :: [PersistencePair persistence],
    topologyViewCoefficientRepresentativeCycles :: [RepresentativeChain coefficient basis],
    topologyViewCoefficientRepresentativeCocycles :: [RepresentativeChain coefficient basis],
    topologyViewHarmonicBasis :: [HarmonicBasisElement coefficient basis],
    topologyViewMacroScaffold :: Maybe scaffold,
    topologyViewScaffoldSummary :: Maybe summary,
    topologyViewLowSpectralModes :: [spectral]
  }
  deriving stock (Eq, Show)

type WitnessSlice :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype WitnessSlice summary scaffold spectral persistence coefficient basis observed = WitnessSlice
  { runWitnessSlice :: TopologyView summary scaffold spectral persistence coefficient basis -> observed
  }

mkTopologyView ::
  ScaffoldSummaryAlgebra scaffold summary ->
  TopologyWitness scaffold spectral persistence coefficient basis ->
  TopologyView summary scaffold spectral persistence coefficient basis
mkTopologyView summaryAlgebra witnessValue =
  let integralHomology = enumerateDegreeIndexed (topologyIntegralHomologyGroups witnessValue)
   in TopologyView
        { topologyViewEulerCharacteristic = topologyEulerCharacteristic witnessValue,
          topologyViewBettiVector = topologyBettiVector witnessValue,
          topologyViewIntegralHomology = integralHomology,
          topologyViewTorsionFamily = mkGradedTorsionFamily (topologyIntegralHomologyGroups witnessValue),
          topologyViewExactRepresentativeClasses = topologyExactRepresentativeClasses witnessValue,
          topologyViewPersistencePairs = topologyPersistencePairs witnessValue,
          topologyViewCoefficientRepresentativeCycles = topologyCoefficientRepresentativeCycles witnessValue,
          topologyViewCoefficientRepresentativeCocycles = topologyCoefficientRepresentativeCocycles witnessValue,
          topologyViewHarmonicBasis = topologyHarmonicBasis witnessValue,
          topologyViewMacroScaffold = topologyMacroScaffold witnessValue,
          topologyViewScaffoldSummary = runScaffoldSummaryAlgebra summaryAlgebra <$> topologyMacroScaffold witnessValue,
          topologyViewLowSpectralModes = topologyLowSpectralModes witnessValue
        }

sliceEulerCharacteristic ::
  WitnessSlice summary scaffold spectral persistence coefficient basis (Maybe EulerCharacteristic)
sliceEulerCharacteristic =
  WitnessSlice topologyViewEulerCharacteristic

sliceBettiVector ::
  WitnessSlice summary scaffold spectral persistence coefficient basis [Int]
sliceBettiVector =
  WitnessSlice topologyViewBettiVector

sliceIntegralHomology ::
  DegreeSelection ->
  WitnessSlice summary scaffold spectral persistence coefficient basis [HomologyGroup Integer]
sliceIntegralHomology selectionValue =
  WitnessSlice (\viewValue -> selectDegreeIndexed selectionValue (topologyViewIntegralHomology viewValue))

sliceIntegralHomologyAt ::
  HomologicalDegree ->
  WitnessSlice summary scaffold spectral persistence coefficient basis (Maybe (HomologyGroup Integer))
sliceIntegralHomologyAt degreeValue =
  WitnessSlice (\viewValue -> lookupDegreeIndexed degreeValue (topologyViewIntegralHomology viewValue))

sliceExactRepresentativeClasses ::
  DegreeSelection ->
  WitnessSlice summary scaffold spectral persistence coefficient basis [ExactRepresentativeClass basis]
sliceExactRepresentativeClasses selectionValue =
  WitnessSlice (\viewValue -> selectGradedMembers exactClassDegree selectionValue (topologyViewExactRepresentativeClasses viewValue))

slicePersistencePairs ::
  DegreeSelection ->
  WitnessSlice summary scaffold spectral persistence coefficient basis [PersistencePair persistence]
slicePersistencePairs selectionValue =
  WitnessSlice (\viewValue -> selectGradedMembers persistenceDegree selectionValue (topologyViewPersistencePairs viewValue))

sliceCoefficientRepresentativeCycles ::
  DegreeSelection ->
  WitnessSlice summary scaffold spectral persistence coefficient basis [RepresentativeChain coefficient basis]
sliceCoefficientRepresentativeCycles selectionValue =
  WitnessSlice (\viewValue -> selectGradedMembers representativeDegree selectionValue (topologyViewCoefficientRepresentativeCycles viewValue))

sliceCoefficientRepresentativeCocycles ::
  DegreeSelection ->
  WitnessSlice summary scaffold spectral persistence coefficient basis [RepresentativeChain coefficient basis]
sliceCoefficientRepresentativeCocycles selectionValue =
  WitnessSlice (\viewValue -> selectGradedMembers representativeDegree selectionValue (topologyViewCoefficientRepresentativeCocycles viewValue))

sliceHarmonicBasis ::
  DegreeSelection ->
  WitnessSlice summary scaffold spectral persistence coefficient basis [HarmonicBasisElement coefficient basis]
sliceHarmonicBasis selectionValue =
  WitnessSlice (\viewValue -> selectGradedMembers harmonicDegree selectionValue (topologyViewHarmonicBasis viewValue))

sliceMacroScaffold ::
  WitnessSlice summary scaffold spectral persistence coefficient basis (Maybe scaffold)
sliceMacroScaffold =
  WitnessSlice topologyViewMacroScaffold

sliceScaffoldSummary ::
  WitnessSlice summary scaffold spectral persistence coefficient basis (Maybe summary)
sliceScaffoldSummary =
  WitnessSlice topologyViewScaffoldSummary

sliceLowSpectralModes ::
  WitnessSlice summary scaffold spectral persistence coefficient basis [spectral]
sliceLowSpectralModes =
  WitnessSlice topologyViewLowSpectralModes

module Moonlight.Homology.Pure.Chain
  ( HomologicalDegree (..),
    incrementDegree,
    decrementDegree,
    EulerCharacteristic (..),
    PersistencePair (..),
    RepresentativeChain (..),
    RepresentativeCycle,
    RepresentativeCocycle,
    HarmonicBasisElement (..),
    ExactRepresentativeClass (..),
    TopologyWitness (..),
    emptyTopologyWitness,
    mergeTopologyWitness,
    mergeTopologyWitnessChecked,
    topologyRepresentativeCycles,
    topologyRepresentativeCocycles,
    topologyWitnessFromBetti,
  )
where

import Control.Applicative ((<|>))
import Data.Kind (Type)
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..), decrementDegree, incrementDegree)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Group (HomologyGroup (..))

type EulerCharacteristic :: Type
newtype EulerCharacteristic = EulerCharacteristic
  { unEulerCharacteristic :: Int
  }
  deriving stock (Eq, Ord, Show, Read)

type PersistencePair :: Type -> Type
data PersistencePair scalar = PersistencePair
  { persistenceDegree :: HomologicalDegree,
    persistenceBirth :: scalar,
    persistenceDeath :: Maybe scalar
  }
  deriving stock (Eq, Show)

type RepresentativeChain :: Type -> Type -> Type
data RepresentativeChain coefficient basis = RepresentativeChain
  { representativeDegree :: HomologicalDegree,
    representativeTerms :: [(coefficient, basis)]
  }
  deriving stock (Eq, Show)

type RepresentativeCycle :: Type -> Type -> Type
type RepresentativeCycle coefficient basis = RepresentativeChain coefficient basis

type RepresentativeCocycle :: Type -> Type -> Type
type RepresentativeCocycle coefficient basis = RepresentativeChain coefficient basis

type HarmonicBasisElement :: Type -> Type -> Type
data HarmonicBasisElement coefficient basis = HarmonicBasisElement
  { harmonicDegree :: HomologicalDegree,
    harmonicRepresentative :: RepresentativeCocycle coefficient basis
  }
  deriving stock (Eq, Show)

type ExactRepresentativeClass :: Type -> Type
data ExactRepresentativeClass basis = ExactRepresentativeClass
  { exactClassDegree :: HomologicalDegree,
    exactClassOrder :: Maybe Integer,
    exactClassRepresentative :: RepresentativeCycle Integer basis
  }
  deriving stock (Eq, Show)

type TopologyWitness :: Type -> Type -> Type -> Type -> Type -> Type
data TopologyWitness scaffold spectral persistence coefficient basis = TopologyWitness
  { topologyEulerCharacteristic :: Maybe EulerCharacteristic,
    topologyBettiVector :: [Int],
    topologyIntegralHomologyGroups :: [HomologyGroup Integer],
    topologyExactRepresentativeClasses :: [ExactRepresentativeClass basis],
    topologyPersistencePairs :: [PersistencePair persistence],
    topologyCoefficientRepresentativeCycles :: [RepresentativeCycle coefficient basis],
    topologyCoefficientRepresentativeCocycles :: [RepresentativeCocycle coefficient basis],
    topologyHarmonicBasis :: [HarmonicBasisElement coefficient basis],
    topologyMacroScaffold :: Maybe scaffold,
    topologyLowSpectralModes :: [spectral]
  }
  deriving stock (Eq, Show)

emptyTopologyWitness :: TopologyWitness scaffold spectral persistence coefficient basis
emptyTopologyWitness =
  TopologyWitness
    { topologyEulerCharacteristic = Nothing,
      topologyBettiVector = [],
      topologyIntegralHomologyGroups = [],
      topologyExactRepresentativeClasses = [],
      topologyPersistencePairs = [],
      topologyCoefficientRepresentativeCycles = [],
      topologyCoefficientRepresentativeCocycles = [],
      topologyHarmonicBasis = [],
      topologyMacroScaffold = Nothing,
      topologyLowSpectralModes = []
    }

mergeTopologyWitness ::
  TopologyWitness scaffold spectral persistence coefficient basis ->
  TopologyWitness scaffold spectral persistence coefficient basis ->
  TopologyWitness scaffold spectral persistence coefficient basis
mergeTopologyWitness left right =
  TopologyWitness
    { topologyEulerCharacteristic =
        topologyEulerCharacteristic left <|> topologyEulerCharacteristic right,
      topologyBettiVector =
        preferNonEmpty (topologyBettiVector left) (topologyBettiVector right),
      topologyIntegralHomologyGroups =
        preferNonEmpty (topologyIntegralHomologyGroups left) (topologyIntegralHomologyGroups right),
      topologyExactRepresentativeClasses =
        preferNonEmpty (topologyExactRepresentativeClasses left) (topologyExactRepresentativeClasses right),
      topologyPersistencePairs =
        topologyPersistencePairs left <> topologyPersistencePairs right,
      topologyCoefficientRepresentativeCycles =
        topologyCoefficientRepresentativeCycles left <> topologyCoefficientRepresentativeCycles right,
      topologyCoefficientRepresentativeCocycles =
        topologyCoefficientRepresentativeCocycles left <> topologyCoefficientRepresentativeCocycles right,
      topologyHarmonicBasis =
        topologyHarmonicBasis left <> topologyHarmonicBasis right,
      topologyMacroScaffold =
        topologyMacroScaffold left <|> topologyMacroScaffold right,
      topologyLowSpectralModes =
        topologyLowSpectralModes left <> topologyLowSpectralModes right
    }
  where
    preferNonEmpty :: [a] -> [a] -> [a]
    preferNonEmpty preferred fallback =
      case preferred of
        [] -> fallback
        _ -> preferred

mergeTopologyWitnessChecked ::
  (Eq scaffold, Eq basis) =>
  TopologyWitness scaffold spectral persistence coefficient basis ->
  TopologyWitness scaffold spectral persistence coefficient basis ->
  Either HomologyFailure (TopologyWitness scaffold spectral persistence coefficient basis)
mergeTopologyWitnessChecked left right = do
  mergedEuler <- mergeOptional "Euler characteristic" (topologyEulerCharacteristic left) (topologyEulerCharacteristic right)
  mergedBetti <- mergePreferNonEmpty "Betti vector" (topologyBettiVector left) (topologyBettiVector right)
  mergedGroups <- mergePreferNonEmpty "integral homology groups" (topologyIntegralHomologyGroups left) (topologyIntegralHomologyGroups right)
  mergedExact <- mergePreferNonEmpty "exact representative classes" (topologyExactRepresentativeClasses left) (topologyExactRepresentativeClasses right)
  mergedScaffold <- mergeOptional "macro scaffold" (topologyMacroScaffold left) (topologyMacroScaffold right)
  pure
    TopologyWitness
      { topologyEulerCharacteristic = mergedEuler,
        topologyBettiVector = mergedBetti,
        topologyIntegralHomologyGroups = mergedGroups,
        topologyExactRepresentativeClasses = mergedExact,
        topologyPersistencePairs = topologyPersistencePairs left <> topologyPersistencePairs right,
        topologyCoefficientRepresentativeCycles = topologyCoefficientRepresentativeCycles left <> topologyCoefficientRepresentativeCycles right,
        topologyCoefficientRepresentativeCocycles = topologyCoefficientRepresentativeCocycles left <> topologyCoefficientRepresentativeCocycles right,
        topologyHarmonicBasis = topologyHarmonicBasis left <> topologyHarmonicBasis right,
        topologyMacroScaffold = mergedScaffold,
        topologyLowSpectralModes = topologyLowSpectralModes left <> topologyLowSpectralModes right
      }

mergeOptional :: Eq a => String -> Maybe a -> Maybe a -> Either HomologyFailure (Maybe a)
mergeOptional fieldName leftValue rightValue =
  case (leftValue, rightValue) of
    (Nothing, _) -> Right rightValue
    (_, Nothing) -> Right leftValue
    (Just leftInner, Just rightInner)
      | leftInner == rightInner -> Right leftValue
      | otherwise -> Left (InvalidTopologyInput ("conflicting topology witness data for " <> fieldName))

mergePreferNonEmpty :: Eq a => String -> [a] -> [a] -> Either HomologyFailure [a]
mergePreferNonEmpty fieldName leftValue rightValue =
  case (leftValue, rightValue) of
    ([], _) -> Right rightValue
    (_, []) -> Right leftValue
    _
      | leftValue == rightValue -> Right leftValue
      | otherwise -> Left (InvalidTopologyInput ("conflicting topology witness data for " <> fieldName))

topologyWitnessFromBetti ::
  [HomologyGroup r] ->
  TopologyWitness scaffold spectral persistence coefficient basis
topologyWitnessFromBetti groups =
  emptyTopologyWitness
    { topologyBettiVector = fmap freeRank groups
    }

topologyRepresentativeCycles ::
  TopologyWitness scaffold spectral persistence coefficient basis ->
  [RepresentativeCycle coefficient basis]
topologyRepresentativeCycles =
  topologyCoefficientRepresentativeCycles

topologyRepresentativeCocycles ::
  TopologyWitness scaffold spectral persistence coefficient basis ->
  [RepresentativeCocycle coefficient basis]
topologyRepresentativeCocycles =
  topologyCoefficientRepresentativeCocycles


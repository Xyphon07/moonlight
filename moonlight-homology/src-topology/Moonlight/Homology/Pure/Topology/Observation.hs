module Moonlight.Homology.Pure.Topology.Observation
  ( TopologyObservationConfig (..),
    defaultTopologyObservationConfig,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Filtration (FiltrationValue)
import Moonlight.Homology.Pure.Topology.Core (FilteredFiniteChainComplex)
import Moonlight.Homology.Pure.Topology.MacroScaffold (ScalarPotentialField)

type TopologyObservationConfig :: Type -> Type
data TopologyObservationConfig r = TopologyObservationConfig
  { observationFiltration :: Maybe (FilteredFiniteChainComplex FiltrationValue r),
    observationPotential :: Maybe ScalarPotentialField,
    observationLowModeCount :: Int
  }

defaultTopologyObservationConfig :: TopologyObservationConfig r
defaultTopologyObservationConfig =
  TopologyObservationConfig
    { observationFiltration = Nothing,
      observationPotential = Nothing,
      observationLowModeCount = 0
    }

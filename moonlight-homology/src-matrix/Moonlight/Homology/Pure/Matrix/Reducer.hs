module Moonlight.Homology.Pure.Matrix.Reducer
  ( BettiReducer (..),
    BettiCapability,
    computeBettiNumbers,
    TopologyWitnessReducer (..),
    TopologyWitnessCapability,
    computeTopologyWitness,
  )
where

import Data.Kind (Type)
import Moonlight.Core (Capability, withCapability)
import Moonlight.Homology.Boundary.Finite (FiniteChainComplex)
import Moonlight.Homology.Pure.Chain (TopologyWitness)
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Group (HomologyGroup)
import Moonlight.Homology.Pure.Phase (HomologyPhase, RequirePhase2)

type BettiReducer :: Type -> Type
newtype BettiReducer r = BettiReducer
  { runBettiReducer :: FiniteChainComplex r -> Either HomologyFailure [HomologyGroup r]
  }

type BettiCapability :: HomologyPhase -> Type -> Type
type BettiCapability phase r =
  Capability RequirePhase2 phase (BettiReducer r)

computeBettiNumbers :: BettiCapability phase r -> FiniteChainComplex r -> Either HomologyFailure [HomologyGroup r]
computeBettiNumbers capability finite =
  withCapability capability
    (\reducer -> runBettiReducer reducer finite)

type TopologyWitnessReducer ::
  Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype TopologyWitnessReducer scaffold spectral persistence coefficient r basis = TopologyWitnessReducer
  { runTopologyWitnessReducer ::
      FiniteChainComplex r ->
      Either HomologyFailure (TopologyWitness scaffold spectral persistence coefficient basis)
  }

type TopologyWitnessCapability ::
  HomologyPhase -> Type -> Type -> Type -> Type -> Type -> Type -> Type
type TopologyWitnessCapability phase scaffold spectral persistence coefficient r basis =
  Capability RequirePhase2 phase (TopologyWitnessReducer scaffold spectral persistence coefficient r basis)

computeTopologyWitness ::
  TopologyWitnessCapability phase scaffold spectral persistence coefficient r basis ->
  FiniteChainComplex r ->
  Either HomologyFailure (TopologyWitness scaffold spectral persistence coefficient basis)
computeTopologyWitness capability finite =
  withCapability capability
    (\reducer -> runTopologyWitnessReducer reducer finite)

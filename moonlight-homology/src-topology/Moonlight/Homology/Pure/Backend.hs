{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Homology.Pure.Backend
  ( HomologyBackendTag (..),
    HomologyBackend (..),
    homologyBackendTag,
    runHomologyBackend,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
  )
import Moonlight.Homology.Pure.Matrix.Reducer
  ( BettiCapability,
    computeBettiNumbers,
  )
import Moonlight.Homology.Pure.Failure
  ( HomologyFailure,
  )
import Moonlight.Homology.Pure.Group
  ( HomologyGroup,
  )
import Moonlight.Homology.Pure.Phase
  ( HomologyPhase (Phase2),
  )
import Moonlight.Homology.Pure.Topology.Integral
  ( integralHomologyGroupsOf,
  )
import Moonlight.Homology.Pure.Rank.Field
  ( FieldRankBackend (..),
    fieldBettiCapability,
  )
import Moonlight.LinAlg.Pure.Dense.GF2
  ( GF2,
  )

type HomologyBackendTag :: Type
data HomologyBackendTag
  = IntegralSmithBackendTag
  | RationalRankBackendTag
  | GF2RankBackendTag
  deriving stock (Eq, Ord, Show)

type HomologyBackend :: Type -> Type -> Type
data HomologyBackend chainCoeff groupCoeff where
  IntegralSmithBackend :: Integral chainCoeff => HomologyBackend chainCoeff Integer
  RationalRankBackend :: HomologyBackend Rational Rational
  GF2RankBackend :: HomologyBackend GF2 GF2

deriving stock instance Show (HomologyBackend chainCoeff groupCoeff)

homologyBackendTag :: HomologyBackend chainCoeff groupCoeff -> HomologyBackendTag
homologyBackendTag backend =
  case backend of
    IntegralSmithBackend -> IntegralSmithBackendTag
    RationalRankBackend -> RationalRankBackendTag
    GF2RankBackend -> GF2RankBackendTag
{-# INLINE homologyBackendTag #-}

runHomologyBackend ::
  HomologyBackend chainCoeff groupCoeff ->
  FiniteChainComplex chainCoeff ->
  Either HomologyFailure [HomologyGroup groupCoeff]
runHomologyBackend backend finite =
  case backend of
    IntegralSmithBackend ->
      integralHomologyGroupsOf finite
    RationalRankBackend ->
      computeBettiNumbers
        (fieldBettiCapability RationalFieldRankBackend :: BettiCapability 'Phase2 Rational)
        finite
    GF2RankBackend ->
      computeBettiNumbers
        (fieldBettiCapability GF2FieldRankBackend :: BettiCapability 'Phase2 GF2)
        finite
{-# INLINEABLE runHomologyBackend #-}

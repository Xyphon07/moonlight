{-| Per-iteration saturation traces and their accumulation. -}
module Moonlight.Pale.Diagnostic.Local.Saturation
  ( SaturationIterationTrace (..),
    SaturationTrace (..),
    emptySaturationTrace,
  )
where

import Data.Kind (Type)
import Moonlight.Pale.Diagnostic.Local.Rewrite (RuleTrace)
import Prelude (Bool, Eq, Int, Monoid (mempty), Semigroup ((<>)), Show)

type SaturationIterationTrace :: Type -> Type
data SaturationIterationTrace ruleId = SaturationIterationTrace
  { sitIteration :: Int,
    sitNodeCountBefore :: Int,
    sitNodeCountAfter :: Int,
    sitBaseEligibleCount :: Int,
    sitContextEligibleCount :: Int,
    sitAggregatedEligibleCount :: Int,
    sitGuidedCount :: Int,
    sitScheduledCount :: Int,
    sitFactsChanged :: Bool,
    sitFactRoundCount :: Int,
    sitContextRevision :: Int,
    sitRuleTraces :: [RuleTrace ruleId]
  }
  deriving stock (Eq, Show)

type SaturationTrace :: Type -> Type
newtype SaturationTrace ruleId = SaturationTrace
  { stIterations :: [SaturationIterationTrace ruleId]
  }
  deriving stock (Eq, Show)

instance Semigroup (SaturationTrace ruleId) where
  leftTrace <> rightTrace =
    SaturationTrace (stIterations leftTrace <> stIterations rightTrace)

instance Monoid (SaturationTrace ruleId) where
  mempty = SaturationTrace []

emptySaturationTrace :: SaturationTrace ruleId
emptySaturationTrace =
  mempty

{-| Derived summaries of saturation and rewrite traces. -}
module Moonlight.Pale.Diagnostic.Views.Rewrite
  ( RewriteOutcomeSummary (..),
    summarizeSaturationTrace,
  )
where

import Data.Kind (Type)
import Data.List (foldl', sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Moonlight.Pale.Diagnostic.Local.Rewrite
  ( RewriteOutcomeStat (..),
    RuleTrace (..),
  )
import Moonlight.Pale.Diagnostic.Local.Saturation
  ( SaturationIterationTrace (..),
    SaturationTrace (..),
  )
import Prelude
  ( Eq,
    Int,
    Ord,
    Show,
    length,
    (+),
    (.),
    (>>=),
  )

type RewriteOutcomeSummary :: Type -> Type
data RewriteOutcomeSummary ruleId = RewriteOutcomeSummary
  { rosIterations :: Int,
    rosTotalMatched :: Int,
    rosTotalFiltered :: Int,
    rosTotalScheduled :: Int,
    rosRuleStats :: [RewriteOutcomeStat ruleId]
  }
  deriving stock (Eq, Show)

data RewriteSummaryAccumulator ruleId = RewriteSummaryAccumulator
  { rsaRuleStats :: !(Map.Map ruleId (RewriteOutcomeStat ruleId)),
    rsaTotalMatched :: !Int,
    rsaTotalFiltered :: !Int,
    rsaTotalScheduled :: !Int
  }

summarizeSaturationTrace :: Ord ruleId => SaturationTrace ruleId -> RewriteOutcomeSummary ruleId
summarizeSaturationTrace saturationTrace =
  let accumulated =
        foldl'
          accumulateSummary
          (RewriteSummaryAccumulator Map.empty 0 0 0)
          (stIterations saturationTrace >>= sitRuleTraces)
      ruleStats =
        sortOn
          (Down . rosScheduledCount)
          (Map.elems (rsaRuleStats accumulated))
   in RewriteOutcomeSummary
        { rosIterations = length (stIterations saturationTrace),
          rosTotalMatched = rsaTotalMatched accumulated,
          rosTotalFiltered = rsaTotalFiltered accumulated,
          rosTotalScheduled = rsaTotalScheduled accumulated,
          rosRuleStats = ruleStats
        }

accumulateSummary ::
  Ord ruleId =>
  RewriteSummaryAccumulator ruleId ->
  RuleTrace ruleId ->
  RewriteSummaryAccumulator ruleId
accumulateSummary accumulator ruleTrace =
  RewriteSummaryAccumulator
    { rsaRuleStats =
        Map.insertWith
          combineStats
          (rtRuleId ruleTrace)
          (statFromTrace ruleTrace)
          (rsaRuleStats accumulator),
      rsaTotalMatched = rsaTotalMatched accumulator + rtMatchedCount ruleTrace,
      rsaTotalFiltered = rsaTotalFiltered accumulator + rtFilteredCount ruleTrace,
      rsaTotalScheduled = rsaTotalScheduled accumulator + rtScheduledCount ruleTrace
    }

combineStats :: RewriteOutcomeStat ruleId -> RewriteOutcomeStat ruleId -> RewriteOutcomeStat ruleId
combineStats leftStat rightStat =
  RewriteOutcomeStat
    { rosRuleId = rosRuleId leftStat,
      rosMatchedCount = rosMatchedCount leftStat + rosMatchedCount rightStat,
      rosFilteredCount = rosFilteredCount leftStat + rosFilteredCount rightStat,
      rosScheduledCount = rosScheduledCount leftStat + rosScheduledCount rightStat,
      rosBannedCount = rosBannedCount leftStat + rosBannedCount rightStat
    }

statFromTrace :: RuleTrace ruleId -> RewriteOutcomeStat ruleId
statFromTrace ruleTrace =
  RewriteOutcomeStat
    { rosRuleId = rtRuleId ruleTrace,
      rosMatchedCount = rtMatchedCount ruleTrace,
      rosFilteredCount = rtFilteredCount ruleTrace,
      rosScheduledCount = rtScheduledCount ruleTrace,
      rosBannedCount = if rtSkippedByScheduler ruleTrace then 1 else 0
    }

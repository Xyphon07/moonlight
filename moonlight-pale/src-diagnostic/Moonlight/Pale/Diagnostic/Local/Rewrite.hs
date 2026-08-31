{-| Per-rule rewrite traces and outcome counts. -}
module Moonlight.Pale.Diagnostic.Local.Rewrite
  ( RuleTrace (..),
    RewriteOutcomeStat (..),
  )
where

import Data.Kind (Type)
import Prelude (Bool, Eq, Int, Maybe, Show)

type RuleTrace :: Type -> Type
data RuleTrace ruleId = RuleTrace
  { rtRuleId :: ruleId,
    rtMatchedCount :: Int,
    rtFilteredCount :: Int,
    rtScheduledCount :: Int,
    rtSkippedByScheduler :: Bool,
    rtBannedUntil :: Maybe Int
  }
  deriving stock (Eq, Show)

type RewriteOutcomeStat :: Type -> Type
data RewriteOutcomeStat ruleId = RewriteOutcomeStat
  { rosRuleId :: ruleId,
    rosMatchedCount :: Int,
    rosFilteredCount :: Int,
    rosScheduledCount :: Int,
    rosBannedCount :: Int
  }
  deriving stock (Eq, Show)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | The closed roles and positions of one ABBA comparison quartet.
--
-- The order is evidence: alternating the baseline and candidate is what lets
-- a quartet expose monotonic drift instead of quietly attributing it to an
-- arm.  Keeping that order here prevents every benchmark from inventing its
-- own four constructors and role mapping.
module Moonlight.Pale.Bench.ABBA
  ( RunRole (..),
    runRoleText,
    QuartetPosition (..),
    abbaQuartet,
    quartetPositionRole,
    quartetPositionText,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

type RunRole :: Type
data RunRole
  = BaselineRole
  | CandidateRole
  deriving stock (Eq, Ord, Show, Enum, Bounded)

runRoleText :: RunRole -> Text
runRoleText = \case
  BaselineRole -> "baseline"
  CandidateRole -> "candidate"

type QuartetPosition :: Type
data QuartetPosition
  = QuartetA1
  | QuartetB1
  | QuartetB2
  | QuartetA2
  deriving stock (Eq, Ord, Show, Enum, Bounded)

abbaQuartet :: NonEmpty QuartetPosition
abbaQuartet = QuartetA1 :| [QuartetB1, QuartetB2, QuartetA2]

quartetPositionRole :: QuartetPosition -> RunRole
quartetPositionRole = \case
  QuartetA1 -> BaselineRole
  QuartetB1 -> CandidateRole
  QuartetB2 -> CandidateRole
  QuartetA2 -> BaselineRole

quartetPositionText :: QuartetPosition -> Text
quartetPositionText = \case
  QuartetA1 -> "a1"
  QuartetB1 -> "b1"
  QuartetB2 -> "b2"
  QuartetA2 -> "a2"

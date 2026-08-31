{-# LANGUAGE DerivingStrategies #-}

-- | Monotone deltas are relative increments plus an explicit rebase form.
--
-- The application and composition laws hold exactly when the supplied join
-- function is associative. Join-over-Reset composes to
-- @ResetDelta (join target increment)@, which agrees with sequential
-- application on the nose; Join-over-Join is where associativity is required.
--
-- 'ResetDelta' is the deliberate non-monotone rebase primitive. Composition
-- treats a newer 'ResetDelta' as left-absorbing, so rebasing discards all older
-- increments by construction.
module Moonlight.Delta.Monotone
  ( Monotone (..),
    applyDelta,
    composeDelta,
    mapDelta,
  )
where

import Data.Kind (Type)
import Prelude (Eq, Ord, Show)

type Monotone :: Type -> Type
data Monotone value
  = JoinDelta !value
  | ResetDelta !value
  deriving stock (Eq, Ord, Show)

applyDelta ::
  (value -> value -> value) ->
  Monotone value ->
  value ->
  value
applyDelta joinValue delta current =
  case delta of
    JoinDelta increment ->
      joinValue current increment
    ResetDelta target ->
      target

-- | Compose a newer monotone delta over an older monotone delta.
--
-- Precondition: the supplied join function must be associative for
-- 'applyDelta' over 'composeDelta' to agree with sequential
-- application for every pair of deltas. A newer 'ResetDelta' is left-absorbing:
-- @composeDelta joinValue (ResetDelta target) older = ResetDelta target@.
-- An older 'ResetDelta' followed by @JoinDelta increment@ composes to
-- @ResetDelta (joinValue target increment)@.
composeDelta ::
  (value -> value -> value) ->
  Monotone value ->
  Monotone value ->
  Monotone value
composeDelta joinValue newer older =
  case newer of
    ResetDelta target ->
      ResetDelta target
    JoinDelta increment ->
      case older of
        ResetDelta target ->
          ResetDelta (joinValue target increment)
        JoinDelta priorIncrement ->
          JoinDelta (joinValue priorIncrement increment)

mapDelta ::
  (left -> right) ->
  Monotone left ->
  Monotone right
mapDelta f delta =
  case delta of
    JoinDelta value ->
      JoinDelta (f value)
    ResetDelta value ->
      ResetDelta (f value)

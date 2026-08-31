{-# LANGUAGE DerivingStrategies #-}

-- | Identifier and epoch newtypes for the relational query engine.
module Moonlight.Core.Relational
  ( QueryId,
    mkQueryId,
    queryIdKey,
    AtomId,
    mkAtomId,
    atomIdKey,
    SlotId,
    mkSlotId,
    slotIdKey,
    QuotientEpoch,
    mkQuotientEpoch,
    initialQuotientEpoch,
    quotientEpochKey,
    nextQuotientEpoch,
    LiveEpoch,
    mkLiveEpoch,
    initialLiveEpoch,
    liveEpochKey,
    nextLiveEpoch,
  )
where

import Data.Kind (Type)
import Moonlight.Core.Order
  ( PartialOrder (..),
    totalOrderLeq,
  )
import Prelude (Bounded (maxBound), Eq ((==)), Int, Num ((+)), Ord, Read, Show)

type QueryId :: Type
newtype QueryId = QueryId {unQueryId :: Int}
  deriving stock (Eq, Ord, Show, Read)

mkQueryId :: Int -> QueryId
mkQueryId =
  QueryId

queryIdKey :: QueryId -> Int
queryIdKey = unQueryId

type AtomId :: Type
newtype AtomId = AtomId {unAtomId :: Int}
  deriving stock (Eq, Ord, Show, Read)

mkAtomId :: Int -> AtomId
mkAtomId =
  AtomId

atomIdKey :: AtomId -> Int
atomIdKey = unAtomId

type SlotId :: Type
newtype SlotId = SlotId {unSlotId :: Int}
  deriving stock (Eq, Ord, Show, Read)

mkSlotId :: Int -> SlotId
mkSlotId =
  SlotId

slotIdKey :: SlotId -> Int
slotIdKey =
  unSlotId

type QuotientEpoch :: Type
newtype QuotientEpoch = QuotientEpoch {unQuotientEpoch :: Int}
  deriving stock (Eq, Ord, Show, Read)

mkQuotientEpoch :: Int -> QuotientEpoch
mkQuotientEpoch =
  QuotientEpoch

initialQuotientEpoch :: QuotientEpoch
initialQuotientEpoch =
  QuotientEpoch 0

quotientEpochKey :: QuotientEpoch -> Int
quotientEpochKey =
  unQuotientEpoch

-- | Successor epoch.  Law (monotone below ceiling): for every epoch @e@ with
-- @quotientEpochKey e < maxBound@, @nextQuotientEpoch e@ strictly increases.
-- Epochs are counters reachable by successor from 'initialQuotientEpoch'; at
-- the public constructor ceiling, successor saturates instead of overflowing.
nextQuotientEpoch :: QuotientEpoch -> QuotientEpoch
nextQuotientEpoch (QuotientEpoch epochValue) =
  QuotientEpoch (nextEpochValue epochValue)

instance PartialOrder QuotientEpoch where
  leq =
    totalOrderLeq

type LiveEpoch :: Type
newtype LiveEpoch = LiveEpoch {unLiveEpoch :: Int}
  deriving stock (Eq, Ord, Show, Read)

mkLiveEpoch :: Int -> LiveEpoch
mkLiveEpoch =
  LiveEpoch

initialLiveEpoch :: LiveEpoch
initialLiveEpoch =
  LiveEpoch 0

liveEpochKey :: LiveEpoch -> Int
liveEpochKey =
  unLiveEpoch

-- | Successor epoch.  Law (monotone below ceiling): for every epoch @e@ with
-- @liveEpochKey e < maxBound@, @nextLiveEpoch e@ strictly increases.  At the
-- public constructor ceiling, successor saturates instead of overflowing.
nextLiveEpoch :: LiveEpoch -> LiveEpoch
nextLiveEpoch (LiveEpoch epochValue) =
  LiveEpoch (nextEpochValue epochValue)

nextEpochValue :: Int -> Int
nextEpochValue epochValue =
  if epochValue == maxBound
    then maxBound
    else epochValue + 1

instance PartialOrder LiveEpoch where
  leq =
    totalOrderLeq

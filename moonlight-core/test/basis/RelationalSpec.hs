{-# LANGUAGE DerivingStrategies #-}

module RelationalSpec (tests) where

import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Core (PartialOrder (..))
import Moonlight.Core
  ( atomIdKey,
    initialLiveEpoch,
    initialQuotientEpoch,
    liveEpochKey,
    mkAtomId,
    mkLiveEpoch,
    mkQueryId,
    mkQuotientEpoch,
    mkSlotId,
    nextLiveEpoch,
    nextQuotientEpoch,
    queryIdKey,
    quotientEpochKey,
    slotIdKey,
  )
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    counterexample,
    elements,
    frequency,
    property,
    (===),
    (==>),
    (.&&.),
  )

newtype EpochKey = EpochKey Int
  deriving stock (Eq, Show)

instance Arbitrary EpochKey where
  arbitrary =
    EpochKey <$> epochKey

  shrink (EpochKey value) =
    EpochKey <$> shrink value

data RelationalLaw
  = RelationalQueryIdKeyRoundTrip
  | RelationalAtomIdKeyRoundTrip
  | RelationalSlotIdKeyRoundTrip
  | RelationalQuotientEpochKeyRoundTrip
  | RelationalLiveEpochKeyRoundTrip
  | RelationalQuotientEpochInitialKey
  | RelationalLiveEpochInitialKey
  | RelationalQuotientEpochNextKeySuccessor
  | RelationalLiveEpochNextKeySuccessor
  | RelationalQuotientEpochNextMaxBoundCeiling
  | RelationalLiveEpochNextMaxBoundCeiling
  | RelationalQuotientEpochNextMonotoneBelowCeiling
  | RelationalLiveEpochNextMonotoneBelowCeiling
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName RelationalLaw where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  testGroup
    "Relational"
    [ lawProperty RelationalQueryIdKeyRoundTrip propQueryIdKeyRoundTrip,
      lawProperty RelationalAtomIdKeyRoundTrip propAtomIdKeyRoundTrip,
      lawProperty RelationalSlotIdKeyRoundTrip propSlotIdKeyRoundTrip,
      lawProperty RelationalQuotientEpochKeyRoundTrip propQuotientEpochKeyRoundTrip,
      lawProperty RelationalLiveEpochKeyRoundTrip propLiveEpochKeyRoundTrip,
      lawProperty RelationalQuotientEpochInitialKey propQuotientEpochInitialKey,
      lawProperty RelationalLiveEpochInitialKey propLiveEpochInitialKey,
      lawProperty RelationalQuotientEpochNextKeySuccessor propQuotientEpochNextKeySuccessor,
      lawProperty RelationalLiveEpochNextKeySuccessor propLiveEpochNextKeySuccessor,
      lawProperty RelationalQuotientEpochNextMaxBoundCeiling propQuotientEpochNextMaxBoundCeiling,
      lawProperty RelationalLiveEpochNextMaxBoundCeiling propLiveEpochNextMaxBoundCeiling,
      lawProperty RelationalQuotientEpochNextMonotoneBelowCeiling propQuotientEpochNextMonotoneBelowCeiling,
      lawProperty RelationalLiveEpochNextMonotoneBelowCeiling propLiveEpochNextMonotoneBelowCeiling
    ]

epochKey :: Gen Int
epochKey =
  frequency
    [ (8, elements [maxBound]),
      (3, elements [minBound, -1, 0, 1, maxBound - 1]),
      (1, arbitrary)
    ]

propQueryIdKeyRoundTrip :: Int -> Property
propQueryIdKeyRoundTrip key =
  keyRoundTrip mkQueryId queryIdKey key

propAtomIdKeyRoundTrip :: Int -> Property
propAtomIdKeyRoundTrip key =
  keyRoundTrip mkAtomId atomIdKey key

propSlotIdKeyRoundTrip :: Int -> Property
propSlotIdKeyRoundTrip key =
  keyRoundTrip mkSlotId slotIdKey key

propQuotientEpochKeyRoundTrip :: EpochKey -> Property
propQuotientEpochKeyRoundTrip (EpochKey key) =
  keyRoundTrip mkQuotientEpoch quotientEpochKey key

propLiveEpochKeyRoundTrip :: EpochKey -> Property
propLiveEpochKeyRoundTrip (EpochKey key) =
  keyRoundTrip mkLiveEpoch liveEpochKey key

propQuotientEpochInitialKey :: Property
propQuotientEpochInitialKey =
  quotientEpochKey initialQuotientEpoch === 0

propLiveEpochInitialKey :: Property
propLiveEpochInitialKey =
  liveEpochKey initialLiveEpoch === 0

propQuotientEpochNextKeySuccessor :: EpochKey -> Property
propQuotientEpochNextKeySuccessor (EpochKey key) =
  quotientEpochKey (nextQuotientEpoch (mkQuotientEpoch key)) === successorEpochKey key

propLiveEpochNextKeySuccessor :: EpochKey -> Property
propLiveEpochNextKeySuccessor (EpochKey key) =
  liveEpochKey (nextLiveEpoch (mkLiveEpoch key)) === successorEpochKey key

propQuotientEpochNextMaxBoundCeiling :: Property
propQuotientEpochNextMaxBoundCeiling =
  quotientEpochKey (nextQuotientEpoch (mkQuotientEpoch maxBound)) === maxBound

propLiveEpochNextMaxBoundCeiling :: Property
propLiveEpochNextMaxBoundCeiling =
  liveEpochKey (nextLiveEpoch (mkLiveEpoch maxBound)) === maxBound

propQuotientEpochNextMonotoneBelowCeiling :: EpochKey -> Property
propQuotientEpochNextMonotoneBelowCeiling (EpochKey key) =
  key < maxBound ==> epochMonotone mkQuotientEpoch quotientEpochKey nextQuotientEpoch key

propLiveEpochNextMonotoneBelowCeiling :: EpochKey -> Property
propLiveEpochNextMonotoneBelowCeiling (EpochKey key) =
  key < maxBound ==> epochMonotone mkLiveEpoch liveEpochKey nextLiveEpoch key

keyRoundTrip :: (Eq identifier, Show identifier) => (Int -> identifier) -> (identifier -> Int) -> Int -> Property
keyRoundTrip construct project key =
  project identifier === key
    .&&. construct (project identifier) === identifier
  where
    identifier =
      construct key

successorEpochKey :: Int -> Int
successorEpochKey key
  | key < maxBound = key + 1
  | otherwise = maxBound

epochMonotone ::
  (PartialOrder epoch, Show epoch) =>
  (Int -> epoch) ->
  (epoch -> Int) ->
  (epoch -> epoch) ->
  Int ->
  Property
epochMonotone construct project advance key =
  counterexample monotonicityFailure $
    property (leq epochValue nextEpoch && project epochValue < project nextEpoch)
  where
    epochValue =
      construct key
    nextEpoch =
      advance epochValue
    monotonicityFailure =
      "epoch did not strictly increase from "
        <> show epochValue
        <> " to "
        <> show nextEpoch

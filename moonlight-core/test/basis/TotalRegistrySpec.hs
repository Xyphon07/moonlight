{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module TotalRegistrySpec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( FiniteUniverse (..),
    boundedEnumUniverse,
    finiteUniverseList,
  )
import SourceShape (assertSourceShape)
import Moonlight.Core
  ( lookupTotal,
    mkTotalRegistry,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

data RegistryKey
  = RegistryA
  | RegistryB
  | RegistryC
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance FiniteUniverse RegistryKey where
  finiteUniverse =
    boundedEnumUniverse

data OutsideUniverseKey
  = InsideUniverse
  | OutsideUniverse
  deriving stock (Eq, Ord, Show)

instance FiniteUniverse OutsideUniverseKey where
  finiteUniverse =
    InsideUniverse :| []

tests :: TestTree
tests =
  testGroup
    "TotalRegistry"
    [ testCase "full registry resolves every finite key in universe order" testFullRegistryResolvesUniverse,
      testCase "incomplete registry reports missing keys in finite-universe order" testMissingKeysReportedInUniverseOrder,
      testCase "lookupTotal can be used as a first-class resolver" testLookupTotalFirstClassResolver,
      testCase "lookupTotal falls back to the first finite-universe value outside the retained map" testLookupTotalOutsideUniverseFallback,
      testCase "TotalRegistry source stays checked and partial-free" testTotalRegistrySourceShape
    ]

testFullRegistryResolvesUniverse :: IO ()
testFullRegistryResolvesUniverse =
  case mkTotalRegistry fullRegistryEntries of
    Left missingKeys ->
      assertFailure ("expected full registry, missing keys: " <> show missingKeys)
    Right registry ->
      fmap (lookupTotal registry) (finiteUniverseList @RegistryKey) @?= [10, 20, 30]

testMissingKeysReportedInUniverseOrder :: IO ()
testMissingKeysReportedInUniverseOrder =
  case mkTotalRegistry missingRegistryEntries of
    Left missingKeys ->
      missingKeys @?= [RegistryB, RegistryC]
    Right _registry ->
      assertFailure "expected RegistryB and RegistryC to be reported missing"

testLookupTotalFirstClassResolver :: IO ()
testLookupTotalFirstClassResolver =
  case mkTotalRegistry fullRegistryEntries of
    Left missingKeys ->
      assertFailure ("expected full registry, missing keys: " <> show missingKeys)
    Right registry -> do
      let resolve = lookupTotal registry
      fmap resolve [RegistryC, RegistryA] @?= [30, 10]

testLookupTotalOutsideUniverseFallback :: IO ()
testLookupTotalOutsideUniverseFallback =
  case mkTotalRegistry (Map.singleton InsideUniverse (99 :: Int)) of
    Left missingKeys ->
      assertFailure ("expected registry with the declared finite universe, missing keys: " <> show missingKeys)
    Right registry ->
      lookupTotal registry OutsideUniverse @?= 99

testTotalRegistrySourceShape :: IO ()
testTotalRegistrySourceShape =
  assertSourceShape
    __FILE__
    "src-basis/Moonlight/Core/TotalRegistry.hs"
    [ "mkTotalRegistry",
      "lookupTotal"
    ]
    [ "Map.!",
      "fromJust",
      "error",
      "undefined"
    ]

fullRegistryEntries :: Map.Map RegistryKey Int
fullRegistryEntries =
  Map.fromList
    [ (RegistryA, 10),
      (RegistryB, 20),
      (RegistryC, 30)
    ]

missingRegistryEntries :: Map.Map RegistryKey Int
missingRegistryEntries =
  Map.singleton RegistryA 10

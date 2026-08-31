{-# LANGUAGE DerivingStrategies #-}

module VersionSpec
  ( versionTests,
  )
where

import LawManifest (lawManifestCase, lawProperty)
import Moonlight.Core (IsLawName (..), PartialOrder (..), constructorLawName)
import Moonlight.Delta.Epoch
import Test.QuickCheck (Arbitrary (..), Property, (===))
import Test.Tasty (TestTree, testGroup)

newtype EpochKey = EpochKey Integer
  deriving stock (Eq, Show)

instance Arbitrary EpochKey where
  arbitrary = EpochKey <$> arbitrary
  shrink (EpochKey value) = EpochKey <$> shrink value

data VersionLaw
  = VersionInitialKey
  | VersionKeyRoundTrip
  | VersionNextKeySuccessor
  | VersionNextStrictlyAdvances
  | VersionLeqAgreesWithKeyOrder
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName VersionLaw where
  lawNameText = constructorLawName . show

versionTests :: TestTree
versionTests =
  testGroup
    "version"
    [ lawManifestCase "epoch version" ([minBound .. maxBound] :: [VersionLaw]),
      lawProperty VersionInitialKey $ versionKey initialVersion === 0,
      lawProperty VersionKeyRoundTrip $ \(EpochKey key) -> versionKey (versionFromKey key) === key,
      lawProperty VersionNextKeySuccessor $ \(EpochKey key) -> versionKey (nextVersion (versionFromKey key)) === key + 1,
      lawProperty VersionNextStrictlyAdvances $ nextStrictlyAdvances,
      lawProperty VersionLeqAgreesWithKeyOrder $ \(EpochKey leftKey) (EpochKey rightKey) ->
        leq (versionFromKey leftKey) (versionFromKey rightKey) === (leftKey <= rightKey)
    ]

nextStrictlyAdvances :: EpochKey -> Property
nextStrictlyAdvances (EpochKey key) =
  (leq versionValue nextValue && versionValue /= nextValue) === True
  where
    versionValue = versionFromKey key
    nextValue = nextVersion versionValue

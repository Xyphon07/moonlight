{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IsoNormSpec (tests) where

import Moonlight.Core (IsoNorm (..), isoNormalize)
import Moonlight.Core (IsLawName (..), constructorLawName)
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Property, (===))

newtype IsoProbe = IsoProbe (Int, Bool)
  deriving stock (Eq, Show)

type IsoProbeRep = (Bool, Int)

instance IsoNorm IsoProbe IsoProbeRep where
  isoFrom (flag, value) =
    IsoProbe (value, flag)

  isoTo (IsoProbe (value, flag)) =
    (flag, value)

instance Arbitrary IsoProbe where
  arbitrary =
    IsoProbe <$> arbitrary

data IsoNormLaw
  = IsoNormFromToRoundTrip
  | IsoNormToFromRoundTrip
  | IsoNormNormalizeIdempotent
  | IsoNormNormalizeIdentity
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName IsoNormLaw where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  testGroup
    "IsoNorm"
    [ lawProperty IsoNormFromToRoundTrip propFromToRoundTrip,
      lawProperty IsoNormToFromRoundTrip propToFromRoundTrip,
      lawProperty IsoNormNormalizeIdempotent propNormalizeIdempotent,
      lawProperty IsoNormNormalizeIdentity propNormalizeIdentity
    ]

propFromToRoundTrip :: IsoProbe -> Property
propFromToRoundTrip wrapped =
  isoFrom (isoTo wrapped) === wrapped

propToFromRoundTrip :: IsoProbeRep -> Property
propToFromRoundTrip representation =
  isoTo (isoFrom representation :: IsoProbe) === representation

propNormalizeIdempotent :: IsoProbe -> Property
propNormalizeIdempotent wrapped =
  isoNormalize (isoNormalize wrapped) === isoNormalize wrapped

propNormalizeIdentity :: IsoProbe -> Property
propNormalizeIdentity wrapped =
  isoNormalize wrapped === wrapped

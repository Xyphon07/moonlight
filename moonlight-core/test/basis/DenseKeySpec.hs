{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module DenseKeySpec (tests) where

import Moonlight.Core (DenseKey (..))
import Moonlight.Core (IsLawName (..), constructorLawName)
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, (===))

data DenseKeyLaw
  = DenseKeyIntDecodeEncodeRoundTrip
  | DenseKeyIntEncodeDecodeRoundTrip
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName DenseKeyLaw where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  testGroup
    "DenseKey"
    [ lawProperty DenseKeyIntDecodeEncodeRoundTrip propIntDecodeEncodeRoundTrip,
      lawProperty DenseKeyIntEncodeDecodeRoundTrip propIntEncodeDecodeRoundTrip
    ]

propIntDecodeEncodeRoundTrip :: Int -> Property
propIntDecodeEncodeRoundTrip value =
  decodeDenseKey @Int (encodeDenseKey value) === value

propIntEncodeDecodeRoundTrip :: Int -> Property
propIntEncodeDecodeRoundTrip denseKey =
  encodeDenseKey (decodeDenseKey @Int denseKey) === denseKey

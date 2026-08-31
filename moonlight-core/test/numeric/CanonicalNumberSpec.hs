{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}

module CanonicalNumberSpec (tests) where

import Moonlight.Core
import Moonlight.Core.Unsound (unsafeCanonicalFiniteAssumeCanonical, unsafeCanonicalFiniteLiteral)
import SourceShape (assertSourceShape)
import Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, Testable, testProperty, (==>))

data CanonicalNumberLawName
  = CanonicalFiniteConstructorRejectsNonFinite
  | CanonicalFiniteConstructorReturnsCanonical
  | CanonicalFiniteConstructorRoundTrip
  | CanonicalFiniteUnsafeAssumptionNormalizesNegativeZero
  | CanonicalFiniteUnsafeLiteralNormalizesNegativeZero
  | CanonicalNumberFiniteFromDoubleAgreesWithConstructor
  | CanonicalNumberFiniteMaybeRoundTrip
  | CanonicalNumberNonFiniteClassification
  | CanonicalNumberReadBoundaryExcludesHiddenConstructors
  deriving stock (Eq, Ord, Show)

instance IsLawName CanonicalNumberLawName where
  lawNameText = constructorLawName . show
lawProperty :: Testable property => CanonicalNumberLawName -> property -> TestTree
lawProperty lawName =
  testProperty (lawNameText lawName)

lawCase :: CanonicalNumberLawName -> Assertion -> TestTree
lawCase lawName =
  testCase (lawNameText lawName)

assertCanonicalNumberReadBoundaryShape :: Assertion
assertCanonicalNumberReadBoundaryShape =
  assertSourceShape
    __FILE__
    "src-numeric/Moonlight/Core/CanonicalNumber/Internal.hs"
    [ "newtype CanonicalFiniteValue = CanonicalFiniteValue",
      "data CanonicalNumber"
    ]
    [ "Read"
    ]


finiteDouble :: Double -> Bool
finiteDouble value =
  not (isNaN value) && not (isInfinite value)

canonicalFiniteConstructorRoundTrip :: Double -> Property
canonicalFiniteConstructorRoundTrip value =
  finiteDouble value ==>
    case (canonicalize value, mkCanonicalFiniteValue value) of
      (Right canonicalValue, Right finiteValue) ->
        canonicalFiniteValue finiteValue == canonicalValue
      _ ->
        False

canonicalNumberFiniteRoundTrip :: Double -> Property
canonicalNumberFiniteRoundTrip value =
  finiteDouble value ==>
    case (canonicalize value, mkCanonicalFiniteNumber value) of
      (Right canonicalValue, Right finiteNumber) ->
        canonicalNumberToMaybeDouble finiteNumber == Just canonicalValue
      _ ->
        False

tests :: TestTree
tests =
  testGroup
    "CanonicalNumber"
    [ lawCase CanonicalNumberReadBoundaryExcludesHiddenConstructors assertCanonicalNumberReadBoundaryShape,
      testGroup
        "CanonicalFiniteValue"
        [ lawCase CanonicalFiniteConstructorRejectsNonFinite $ do
            mkCanonicalFiniteValue (0 / 0) @?= Left (NonFiniteValue CanonicalizeContext NaNInput)
            case mkCanonicalFiniteValue (1 / 0) of
              Left _ -> pure ()
              Right _ -> assertFailure "canonical finite constructor accepted infinity",
          lawProperty CanonicalFiniteConstructorReturnsCanonical $ \(x :: Double) ->
            case mkCanonicalFiniteValue x of
              Right finiteValue -> isCanonical (canonicalFiniteValue finiteValue)
              Left _ -> True,
          lawProperty CanonicalFiniteConstructorRoundTrip canonicalFiniteConstructorRoundTrip,
          lawCase CanonicalFiniteUnsafeLiteralNormalizesNegativeZero $
            canonicalFiniteValue (unsafeCanonicalFiniteLiteral (-0.0)) @?= 0.0,
          lawCase CanonicalFiniteUnsafeAssumptionNormalizesNegativeZero $
            canonicalFiniteValue (unsafeCanonicalFiniteAssumeCanonical (-0.0)) @?= 0.0
        ],
      testGroup
        "CanonicalNumber"
        [ lawProperty CanonicalNumberFiniteFromDoubleAgreesWithConstructor $ \(x :: Double) ->
            finiteDouble x ==> canonicalNumberFromDouble x == either (const NaN) id (mkCanonicalFiniteNumber x),
          lawProperty CanonicalNumberFiniteMaybeRoundTrip canonicalNumberFiniteRoundTrip,
          lawCase CanonicalNumberNonFiniteClassification $ do
            canonicalNumberFromDouble (1 / 0) @?= PosInf
            canonicalNumberFromDouble ((-1) / 0) @?= NegInf
            assertBool "NaN classifies as NaN" (canonicalNumberFromDouble (0 / 0) == NaN)
        ]
    ]

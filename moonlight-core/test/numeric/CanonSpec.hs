{-# LANGUAGE DerivingStrategies #-}

module CanonSpec (tests) where

import Data.Int (Int64)
import Data.Word (Word32)
import Moonlight.Core
import Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (NonNegative (..), Property, Testable, testProperty, (==>))

data CanonLawName
  = CanonCanonicalizeIdempotent
  | CanonCanonicalizeNormalizesNegativeZero
  | CanonCanonicalizeRejectsInfinite
  | CanonCanonicalizeRejectsNaN
  | CanonCanonicalizeReturnsCanonical
  | CanonIsCanonicalIdentity
  | CanonMkFiniteDoubleAgreesWithCanonicalize
  | CanonMkNonNegativeFiniteCanonicalizes
  | CanonMkPositiveFiniteRejectsNonPositive
  | CanonQuantizeFinitePrecisionTotal
  | CanonQuantizeRejectsInvalidPrecision
  | CanonQuantizeRejectsNonFinite
  | CanonQuantizeSaturatesMaximum
  | CanonQuantizeSaturatesMinimum
  deriving stock (Eq, Ord, Show)

instance IsLawName CanonLawName where
  lawNameText = constructorLawName . show
lawProperty :: Testable property => CanonLawName -> property -> TestTree
lawProperty lawName =
  testProperty (lawNameText lawName)

lawCase :: CanonLawName -> Assertion -> TestTree
lawCase lawName =
  testCase (lawNameText lawName)
finiteDouble :: Double -> Bool
finiteDouble value =
  not (isNaN value) && not (isInfinite value)

validHashPrecision :: Word32 -> Word32
validHashPrecision precision =
  precision `mod` 10

canonicalizeReturnsCanonical :: Double -> Bool
canonicalizeReturnsCanonical value =
  case canonicalize value of
    Right canonicalValue ->
      isCanonical canonicalValue
    Left _ ->
      True

mkFiniteDoubleAgreesWithCanonicalize :: Double -> Bool
mkFiniteDoubleAgreesWithCanonicalize value =
  case (mkFiniteDouble "test" value, canonicalize value) of
    (Right finiteValue, Right canonicalValue) ->
      finiteValue == canonicalValue
    (Left _, Left _) ->
      True
    _ ->
      False

quantizeFinitePrecisionTotal :: Word32 -> Double -> Property
quantizeFinitePrecisionTotal precision value =
  finiteDouble value ==>
    case quantizeForHash (validHashPrecision precision) value of
      Right _ -> True
      Left _ -> False

tests :: TestTree
tests =
  testGroup
    "Canon"
    [ testGroup
        "canonicalize"
        [ lawCase CanonCanonicalizeRejectsNaN $
            canonicalize (0 / 0) @?= Left (NonFiniteValue CanonicalizeContext NaNInput),
          lawCase CanonCanonicalizeRejectsInfinite $ do
            canonicalize (1 / 0) @?= Left (NonFiniteValue CanonicalizeContext InfiniteInput)
            canonicalize ((-1) / 0) @?= Left (NonFiniteValue CanonicalizeContext InfiniteInput),
          lawCase CanonCanonicalizeNormalizesNegativeZero $
            canonicalize (-0.0) @?= Right 0.0,
          lawProperty CanonCanonicalizeIdempotent $ \(x :: Double) ->
            case canonicalize x of
              Right y -> canonicalize y == Right y
              Left _ -> True,
          lawProperty CanonCanonicalizeReturnsCanonical canonicalizeReturnsCanonical
        ],
      testGroup
        "isCanonical"
        [ lawProperty CanonIsCanonicalIdentity $ \(x :: Double) ->
            isCanonical x ==> canonicalize x == Right x
        ],
      testGroup
        "finite constructors"
        [ lawProperty CanonMkFiniteDoubleAgreesWithCanonicalize mkFiniteDoubleAgreesWithCanonicalize,
          lawProperty CanonMkNonNegativeFiniteCanonicalizes $ \(NonNegative x :: NonNegative Double) ->
            finiteDouble x ==>
              case mkNonNegativeFiniteDouble "test" x of
                Right y -> isCanonical y && y >= 0.0
                Left _ -> False,
          lawProperty CanonMkPositiveFiniteRejectsNonPositive $ \(NonNegative x :: NonNegative Double) ->
            finiteDouble x ==>
              case mkPositiveFiniteDouble "test" (negate x) of
                Left _ -> True
                Right _ -> False
        ],
      testGroup
        "quantizeForHash"
        [ lawProperty CanonQuantizeFinitePrecisionTotal quantizeFinitePrecisionTotal,
          lawProperty CanonQuantizeRejectsInvalidPrecision $ \(precision :: Word32) (x :: Double) ->
            precision > 9 ==> case quantizeForHash precision x of
              Left _ -> True
              Right _ -> False,
          testCase "quantize rejects invalid precision with typed error" $
            quantizeForHash 10 1.0 @?= Left (QuantizePrecisionTooLarge 10),
          lawCase CanonQuantizeRejectsNonFinite $ do
            quantizeForHash 0 (0 / 0) @?= Left (NonFiniteValue QuantizeContext NaNInput)
            quantizeForHash 0 (1 / 0) @?= Left (NonFiniteValue QuantizeContext InfiniteInput),
          lawCase CanonQuantizeSaturatesMaximum $
            quantizeForHash 0 (fromIntegral (maxBound :: Int64) * 2.0) @?= Right maxBound,
          lawCase CanonQuantizeSaturatesMinimum $
            quantizeForHash 0 (fromIntegral (minBound :: Int64) * 2.0) @?= Right minBound
        ]
    ]

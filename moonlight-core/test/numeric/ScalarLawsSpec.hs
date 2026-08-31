{-# LANGUAGE DerivingStrategies #-}

module ScalarLawsSpec (tests) where

import Data.Maybe (isJust)
import Moonlight.Core
import Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, Testable, property, testProperty, (==>))

data ScalarLawName
  = ScalarAdditiveAssociativity
  | ScalarAdditiveCommutativity
  | ScalarAdditiveLeftIdentity
  | ScalarAdditiveLeftInverse
  | ScalarAdditiveNegationInvolution
  | ScalarAdditiveRightIdentity
  | ScalarAdditiveRightInverse
  | ScalarFieldCanInvertAgreement
  | ScalarFieldDivisionDefinition
  | ScalarFieldDivisionByOne
  | ScalarFieldDivisionByZero
  | ScalarFieldInverseMultiplicativeIdentity
  | ScalarFieldInverseResultFinite
  | ScalarFieldRejectsInverseOutOfRange
  | ScalarFieldRequireInvertibleAgreement
  | ScalarFieldZeroNotInvertible
  | ScalarLeftDistributivity
  | ScalarMetricNegationInvariant
  | ScalarMetricNonNegative
  | ScalarMetricZeroMagnitude
  | ScalarMultiplicativeAssociativity
  | ScalarMultiplicativeCommutativity
  | ScalarMultiplicativeLeftIdentity
  | ScalarMultiplicativeRightIdentity
  | ScalarRightDistributivity
  | ScalarSubtractionDefinition
  deriving stock (Eq, Ord, Show)

instance IsLawName ScalarLawName where
  lawNameText = constructorLawName . show
lawProperty :: Testable property => ScalarLawName -> property -> TestTree
lawProperty lawName =
  testProperty (lawNameText lawName)

lawCase :: ScalarLawName -> Assertion -> TestTree
lawCase lawName =
  testCase (lawNameText lawName)
tests :: TestTree
tests =
  testGroup
    "Scalar Laws"
    [ testGroup
        "AdditiveGroup Int"
        [ lawProperty ScalarAdditiveLeftIdentity $ \(x :: Int) ->
            add zero x == x,
          lawProperty ScalarAdditiveRightIdentity $ \(x :: Int) ->
            add x zero == x,
          lawProperty ScalarAdditiveLeftInverse $ \(x :: Int) ->
            add (neg x) x == zero,
          lawProperty ScalarAdditiveRightInverse $ \(x :: Int) ->
            add x (neg x) == zero,
          lawProperty ScalarAdditiveAssociativity $ \(x :: Int) (y :: Int) (z :: Int) ->
            add (add x y) z == add x (add y z),
          lawProperty ScalarAdditiveCommutativity $ \(x :: Int) (y :: Int) ->
            add x y == add y x,
          lawProperty ScalarAdditiveNegationInvolution $ \(x :: Int) ->
            neg (neg x) == x,
          lawProperty ScalarSubtractionDefinition $ \(x :: Int) (y :: Int) ->
            sub x y == add x (neg y)
        ],
      testGroup
        "AdditiveGroup Integer"
        [ lawProperty ScalarAdditiveLeftIdentity $ \(x :: Integer) ->
            add zero x == x,
          lawProperty ScalarAdditiveRightIdentity $ \(x :: Integer) ->
            add x zero == x,
          lawProperty ScalarAdditiveLeftInverse $ \(x :: Integer) ->
            add (neg x) x == zero,
          lawProperty ScalarAdditiveRightInverse $ \(x :: Integer) ->
            add x (neg x) == zero,
          lawProperty ScalarAdditiveAssociativity $ \(x :: Integer) (y :: Integer) (z :: Integer) ->
            add (add x y) z == add x (add y z),
          lawProperty ScalarAdditiveCommutativity $ \(x :: Integer) (y :: Integer) ->
            add x y == add y x,
          lawProperty ScalarAdditiveNegationInvolution $ \(x :: Integer) ->
            neg (neg x) == x
        ],
      testGroup
        "MultiplicativeMonoid Int"
        [ lawProperty ScalarMultiplicativeLeftIdentity $ \(x :: Int) ->
            mul one x == x,
          lawProperty ScalarMultiplicativeRightIdentity $ \(x :: Int) ->
            mul x one == x,
          lawProperty ScalarMultiplicativeAssociativity $ \(x :: Int) (y :: Int) (z :: Int) ->
            mul (mul x y) z == mul x (mul y z),
          lawProperty ScalarMultiplicativeCommutativity $ \(x :: Int) (y :: Int) ->
            mul x y == mul y x
        ],
      testGroup
        "Ring Int"
        [ lawProperty ScalarLeftDistributivity $ \(x :: Int) (y :: Int) (z :: Int) ->
            mul x (add y z) == add (mul x y) (mul x z),
          lawProperty ScalarRightDistributivity $ \(x :: Int) (y :: Int) (z :: Int) ->
            mul (add y z) x == add (mul y x) (mul z x)
        ],
      testGroup
        "AdditiveGroup Double"
        [ lawProperty ScalarAdditiveLeftIdentity $ \(x :: Double) ->
            fieldValueValid x ==> add zero x == x,
          lawProperty ScalarAdditiveRightIdentity $ \(x :: Double) ->
            fieldValueValid x ==> add x zero == x,
          lawProperty ScalarAdditiveNegationInvolution $ \(x :: Double) ->
            fieldValueValid x ==> neg (neg x) == x
        ],
      testGroup
        "MultiplicativeMonoid Double"
        [ lawProperty ScalarMultiplicativeLeftIdentity $ \(x :: Double) ->
            fieldValueValid x ==> mul one x == x,
          lawProperty ScalarMultiplicativeRightIdentity $ \(x :: Double) ->
            fieldValueValid x ==> mul x one == x
        ],
      testGroup
        "Field Double"
        [ lawProperty ScalarFieldInverseMultiplicativeIdentity fieldInverse,
          lawProperty ScalarFieldInverseResultFinite doubleInverseResultFinite,
          lawCase ScalarFieldRejectsInverseOutOfRange $
            tryInv leastPositiveSubnormalDouble @?= Nothing,
          lawCase ScalarFieldZeroNotInvertible $
            tryInv (zero :: Double) @?= Nothing,
          lawProperty ScalarFieldCanInvertAgreement $ \(x :: Double) ->
            canInvert x == isJust (tryInv x),
          lawProperty ScalarFieldRequireInvertibleAgreement $ \(x :: Double) ->
            requireInvertible () x == maybe (Left ()) Right (tryInv x),
          lawProperty ScalarFieldDivisionDefinition $ \(x :: Double) (y :: Double) ->
            fieldValueValid x ==> tryDiv x y == fmap (mul x) (tryInv y),
          lawProperty ScalarFieldDivisionByOne $ \(x :: Double) ->
            fieldValueValid x ==> tryDiv x one == Just x,
          lawCase ScalarFieldDivisionByZero $
            tryDiv (one :: Double) zero @?= Nothing
        ],
      testGroup
        "Field Float"
        [ lawProperty ScalarFieldInverseResultFinite floatInverseResultFinite,
          lawCase ScalarFieldRejectsInverseOutOfRange $
            tryInv leastPositiveSubnormalFloat @?= Nothing,
          lawCase ScalarFieldZeroNotInvertible $
            tryInv (zero :: Float) @?= Nothing
        ],
      testGroup
        "Field Rational"
        [ lawProperty ScalarFieldInverseMultiplicativeIdentity $ \(x :: Rational) ->
            x /= zero ==> fmap (mul x) (tryInv x) == Just one,
          lawCase ScalarFieldZeroNotInvertible $
            tryInv (zero :: Rational) @?= Nothing,
          lawProperty ScalarFieldDivisionDefinition $ \(x :: Rational) (y :: Rational) ->
            tryDiv x y == fmap (mul x) (tryInv y),
          lawProperty ScalarFieldDivisionByOne $ \(x :: Rational) ->
            tryDiv x one == Just x,
          lawCase ScalarFieldDivisionByZero $
            tryDiv (one :: Rational) zero @?= Nothing
        ],
      testGroup
        "Metric Double"
        [ lawCase ScalarMetricZeroMagnitude $
            magnitude (zero :: Double) @?= (zero :: Double),
          lawProperty ScalarMetricNonNegative $ \(x :: Double) ->
            fieldValueValid x ==> magnitude x >= zero,
          lawProperty ScalarMetricNegationInvariant $ \(x :: Double) ->
            fieldValueValid x ==> magnitude (neg x) == magnitude x
        ],
      testGroup
        "Metric Int"
        [ lawCase ScalarMetricZeroMagnitude $
            magnitude (zero :: Int) @?= (zero :: Int),
          lawProperty ScalarMetricNegationInvariant $ \(x :: Int) ->
            magnitude (neg x) == magnitude x
        ]
    ]

inverseResultFinite :: Field a => a -> Bool
inverseResultFinite value =
  case tryInv value of
    Just inverseValue -> fieldValueValid inverseValue
    Nothing -> True

doubleInverseResultFinite :: Double -> Bool
doubleInverseResultFinite =
  inverseResultFinite

floatInverseResultFinite :: Float -> Bool
floatInverseResultFinite =
  inverseResultFinite

leastPositiveSubnormalDouble :: Double
leastPositiveSubnormalDouble =
  encodeFloat 1 (fst (floatRange (0 :: Double)) - floatDigits (0 :: Double))

leastPositiveSubnormalFloat :: Float
leastPositiveSubnormalFloat =
  encodeFloat 1 (fst (floatRange (0 :: Float)) - floatDigits (0 :: Float))

fieldInverse :: Double -> Property
fieldInverse x = case tryInv x of
  Just xInv -> case absTol 1e-10 of
    Right tolerance -> property (approxEq tolerance (mul x xInv) one)
    Left _ -> property False
  Nothing -> property True

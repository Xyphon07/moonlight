{-# LANGUAGE DerivingStrategies #-}

module ApproxEqSpec (tests) where

import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import Moonlight.Core
import Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Gen, NonNegative (..), Positive (..), Property, Testable, arbitrary, chooseBoundedIntegral, forAll, property, testProperty, (==>))

data ApproxEqLawName
  = ApproxEqAbsTolConstructorRejectsInvalid
  | ApproxEqAbsTolMonotonicity
  | ApproxEqAbsTolReflexivity
  | ApproxEqAbsTolSymmetry
  | ApproxEqFloatAbsTolReflexivity
  | ApproxEqFloatUlpTolMonotonicity
  | ApproxEqFloatUlpTolNativeAdjacentValues
  | ApproxEqFloatUlpTolReflexivity
  | ApproxEqFloatUlpTolRejectsNaNOperands
  | ApproxEqFloatUlpTolSymmetry
  | ApproxEqRelTolConstructorRejectsInvalid
  | ApproxEqRelTolMonotonicity
  | ApproxEqRelTolReflexivity
  | ApproxEqRelTolSymmetry
  | ApproxEqUlpTolMonotonicity
  | ApproxEqUlpTolNativeAdjacentValues
  | ApproxEqUlpTolReflexivity
  | ApproxEqUlpTolRejectsNaNOperands
  | ApproxEqUlpTolSymmetry
  | ApproxEqToleranceCompositeNormalForm
  | ApproxEqToleranceExactConjunctionBottom
  | ApproxEqToleranceExactDisjunctionIdentity
  | ApproxEqWithinTolAlias
  deriving stock (Eq, Ord, Show)

instance IsLawName ApproxEqLawName where
  lawNameText = constructorLawName . show
lawProperty :: Testable property => ApproxEqLawName -> property -> TestTree
lawProperty lawName =
  testProperty (lawNameText lawName)

lawCase :: ApproxEqLawName -> Assertion -> TestTree
lawCase lawName =
  testCase (lawNameText lawName)
withAbsTol :: Double -> (AbsTol -> Property) -> Property
withAbsTol rawTolerance prove =
  case absTol rawTolerance of
    Right tolerance ->
      prove tolerance
    Left _ ->
      property False

withRelTol :: Double -> (RelTol -> Property) -> Property
withRelTol rawTolerance prove =
  case relTol rawTolerance of
    Right tolerance ->
      prove tolerance
    Left _ ->
      property False

absTolMonotonic :: NonNegative Double -> NonNegative Double -> Double -> Double -> Property
absTolMonotonic (NonNegative narrowRaw) (NonNegative wideningRaw) x y =
  case (absTol narrowRaw, absTol (narrowRaw + wideningRaw)) of
    (Right narrowTolerance, Right wideTolerance) ->
      approxEq narrowTolerance x y ==> approxEq wideTolerance x y
    _ ->
      property True

relTolMonotonic :: NonNegative Double -> NonNegative Double -> Double -> Double -> Property
relTolMonotonic (NonNegative narrowRaw) (NonNegative wideningRaw) x y =
  case (relTol narrowRaw, relTol (narrowRaw + wideningRaw)) of
    (Right narrowTolerance, Right wideTolerance) ->
      approxEq narrowTolerance x y ==> approxEq wideTolerance x y
    _ ->
      property True

ulpTolMonotonic :: Property
ulpTolMonotonic =
  forAll ulpNeighborhood $ \(narrowTolerance, wideTolerance, x, y) ->
    approxEq (mkUlpTol narrowTolerance) x y ==> approxEq (mkUlpTol wideTolerance) x y

floatUlpTolMonotonic :: Property
floatUlpTolMonotonic =
  forAll floatUlpNeighborhood $ \(narrowTolerance, wideTolerance, x, y) ->
    approxEq (mkUlpTol narrowTolerance) x y ==> approxEq (mkUlpTol wideTolerance) x y

ulpNeighborhood :: Gen (Word64, Word64, Double, Double)
ulpNeighborhood = do
  x <- arbitrary
  offset <- chooseBoundedIntegral (0, 512)
  narrowTolerance <- chooseBoundedIntegral (0, 1024)
  wideMargin <- chooseBoundedIntegral (0, 1024)
  let y = castWord64ToDouble (castDoubleToWord64 x + offset)
  pure (narrowTolerance, narrowTolerance + wideMargin, x, y)

floatUlpNeighborhood :: Gen (Word64, Word64, Float, Float)
floatUlpNeighborhood = do
  x <- arbitrary
  offset <- chooseBoundedIntegral (0 :: Word32, 512)
  narrowTolerance <- chooseBoundedIntegral (0, 1024)
  wideMargin <- chooseBoundedIntegral (0, 1024)
  let y = castWord32ToFloat (castFloatToWord32 x + offset)
  pure (narrowTolerance, narrowTolerance + wideMargin, x, y)

assertUlpTolRejectsNaNOperands :: Assertion
assertUlpTolRejectsNaNOperands = do
  let tolerance = mkUlpTol maxBound
      nan = 0 / 0 :: Double
  assertBool "max ULP tolerance must reject a left NaN operand" (not (approxEq tolerance nan 0.0))
  assertBool "max ULP tolerance must reject a right NaN operand" (not (approxEq tolerance 0.0 nan))
  assertBool "max ULP tolerance must reject two NaN operands" (not (approxEq tolerance nan nan))

assertUlpTolNativeAdjacentValues :: Assertion
assertUlpTolNativeAdjacentValues = do
  let exactTolerance = mkUlpTol 0
      adjacentTolerance = mkUlpTol 1
      positiveZero = 0.0 :: Double
      negativeZero = -0.0 :: Double
      positiveMinSubnormal = castWord64ToDouble 0x0000000000000001
      negativeMinSubnormal = castWord64ToDouble 0x8000000000000001
  assertBool "Double ULP key must collapse IEEE zeros" (approxEq exactTolerance positiveZero negativeZero)
  assertBool "Double ULP key must accept adjacent positive subnormal" (approxEq adjacentTolerance positiveZero positiveMinSubnormal)
  assertBool "zero ULP tolerance must reject adjacent positive subnormal" (not (approxEq exactTolerance positiveZero positiveMinSubnormal))
  assertBool "Double ULP key must accept adjacent negative subnormal" (approxEq adjacentTolerance negativeZero negativeMinSubnormal)
  assertBool "zero ULP tolerance must reject adjacent negative subnormal" (not (approxEq exactTolerance negativeZero negativeMinSubnormal))

assertFloatUlpTolRejectsNaNOperands :: Assertion
assertFloatUlpTolRejectsNaNOperands = do
  let tolerance = mkUlpTol maxBound
      nan = 0 / 0 :: Float
  assertBool "max Float ULP tolerance must reject a left NaN operand" (not (approxEq tolerance nan 0.0))
  assertBool "max Float ULP tolerance must reject a right NaN operand" (not (approxEq tolerance 0.0 nan))
  assertBool "max Float ULP tolerance must reject two NaN operands" (not (approxEq tolerance nan nan))

assertFloatUlpTolNativeAdjacentValues :: Assertion
assertFloatUlpTolNativeAdjacentValues = do
  let exactTolerance = mkUlpTol 0
      adjacentTolerance = mkUlpTol 1
      positiveZero = 0.0 :: Float
      negativeZero = -0.0 :: Float
      positiveMinSubnormal = castWord32ToFloat 0x00000001
      negativeMinSubnormal = castWord32ToFloat 0x80000001
  assertBool "Float ULP key must collapse IEEE zeros" (approxEq exactTolerance positiveZero negativeZero)
  assertBool "Float ULP key must accept adjacent positive subnormal" (approxEq adjacentTolerance positiveZero positiveMinSubnormal)
  assertBool "zero ULP tolerance must reject adjacent positive subnormal" (not (approxEq exactTolerance positiveZero positiveMinSubnormal))
  assertBool "Float ULP key must accept adjacent negative subnormal" (approxEq adjacentTolerance negativeZero negativeMinSubnormal)
  assertBool "zero ULP tolerance must reject adjacent negative subnormal" (not (approxEq exactTolerance negativeZero negativeMinSubnormal))

assertToleranceCompositeNormalForm :: Assertion
assertToleranceCompositeNormalForm =
  expectRight (mkAbsTol 0.25) $ \absoluteTolerance ->
    expectRight (mkRelTol 0.5) $ \relativeTolerance -> do
      let absoluteBranch = AbsTolBound absoluteTolerance
          relativeBranch = RelTolBound relativeTolerance
          ulpBranch = UlpTolBound (mkUlpTol 4)
      normalizeTolerance
        ( CompositeTol
            (CompositeTol relativeBranch absoluteBranch)
            (CompositeTol relativeBranch ulpBranch)
        )
        @?= CompositeTol (CompositeTol absoluteBranch relativeBranch) ulpBranch

assertToleranceExactConjunctionBottom :: Assertion
assertToleranceExactConjunctionBottom =
  expectRight (mkAbsTol 0.25) $ \absoluteTolerance ->
    normalizeTolerance (CompositeTol (AbsTolBound absoluteTolerance) Exact) @?= Exact

assertToleranceExactDisjunctionIdentity :: Assertion
assertToleranceExactDisjunctionIdentity =
  expectRight (mkRelTol 0.5) $ \relativeTolerance -> do
    let relativeBranch = RelTolBound relativeTolerance
    normalizeTolerance (DisjunctiveTol Exact (DisjunctiveTol relativeBranch relativeBranch)) @?= relativeBranch

expectRight :: Show err => Either err value -> (value -> Assertion) -> Assertion
expectRight result prove =
  case result of
    Left err ->
      assertFailure ("expected Right, received Left: " <> show err)
    Right value ->
      prove value

tests :: TestTree
tests =
  testGroup
    "ApproxEq"
    [ testGroup
        "AbsTol Double"
        [ lawProperty ApproxEqAbsTolReflexivity $ \(x :: Double) ->
            fieldValueValid x ==> withAbsTol 1e-15 (\tolerance -> property (approxEq tolerance x x)),
          lawProperty ApproxEqAbsTolSymmetry $ \(x :: Double) (y :: Double) ->
            withAbsTol 0.1 (\tolerance -> property (approxEq tolerance x y == approxEq tolerance y x)),
          lawProperty ApproxEqAbsTolMonotonicity absTolMonotonic
        ],
      testGroup
        "RelTol Double"
        [ lawProperty ApproxEqRelTolReflexivity $ \(x :: Double) ->
            fieldValueValid x ==> withRelTol 1e-15 (\tolerance -> property (approxEq tolerance x x)),
          lawProperty ApproxEqRelTolSymmetry $ \(x :: Double) (y :: Double) ->
            withRelTol 0.01 (\tolerance -> property (approxEq tolerance x y == approxEq tolerance y x)),
          lawProperty ApproxEqRelTolMonotonicity relTolMonotonic
        ],
      testGroup
        "UlpTol Double"
        [ lawProperty ApproxEqUlpTolReflexivity $ \(x :: Double) ->
            fieldValueValid x ==> approxEq (mkUlpTol 0) x x,
          lawCase ApproxEqUlpTolRejectsNaNOperands assertUlpTolRejectsNaNOperands,
          lawProperty ApproxEqUlpTolSymmetry $ \(tolerance :: Word64) (x :: Double) (y :: Double) ->
            approxEq (mkUlpTol tolerance) x y == approxEq (mkUlpTol tolerance) y x,
          lawProperty ApproxEqUlpTolMonotonicity ulpTolMonotonic,
          lawCase ApproxEqUlpTolNativeAdjacentValues assertUlpTolNativeAdjacentValues
        ],
      testGroup
        "AbsTol Float"
        [ lawProperty ApproxEqFloatAbsTolReflexivity $ \(x :: Float) ->
            fieldValueValid x ==> withAbsTol 1e-6 (\tolerance -> property (approxEq tolerance x x))
        ],
      testGroup
        "UlpTol Float"
        [ lawProperty ApproxEqFloatUlpTolReflexivity $ \(x :: Float) ->
            fieldValueValid x ==> approxEq (mkUlpTol 0) x x,
          lawCase ApproxEqFloatUlpTolRejectsNaNOperands assertFloatUlpTolRejectsNaNOperands,
          lawProperty ApproxEqFloatUlpTolSymmetry $ \(tolerance :: Word64) (x :: Float) (y :: Float) ->
            approxEq (mkUlpTol tolerance) x y == approxEq (mkUlpTol tolerance) y x,
          lawProperty ApproxEqFloatUlpTolMonotonicity floatUlpTolMonotonic,
          lawCase ApproxEqFloatUlpTolNativeAdjacentValues assertFloatUlpTolNativeAdjacentValues
        ],
      testGroup
        "withinTol"
        [ lawProperty ApproxEqWithinTolAlias $ \(x :: Double) (y :: Double) ->
            withAbsTol 0.01 (\tolerance -> property (withinTol tolerance x y == approxEq tolerance x y))
        ],
      testGroup
        "Tolerance constructors"
        [ lawProperty ApproxEqAbsTolConstructorRejectsInvalid $ \(Positive invalidMagnitude :: Positive Double) ->
            case (mkAbsTol (negate invalidMagnitude), absTol (negate invalidMagnitude)) of
              (Left _, Left _) -> True
              _ -> False,
          lawCase ApproxEqRelTolConstructorRejectsInvalid $ do
            case mkRelTol (0 / 0) of
              Left _ -> pure ()
              Right _ -> assertFailure "relative tolerance accepted NaN"
            case relTol (1 / 0) of
              Left _ -> pure ()
              Right _ -> assertFailure "relative tolerance accepted infinity"
        ],
      testGroup
        "Tolerance normal form"
        [ lawCase ApproxEqToleranceCompositeNormalForm assertToleranceCompositeNormalForm,
          lawCase ApproxEqToleranceExactConjunctionBottom assertToleranceExactConjunctionBottom,
          lawCase ApproxEqToleranceExactDisjunctionIdentity assertToleranceExactDisjunctionIdentity
        ]
    ]

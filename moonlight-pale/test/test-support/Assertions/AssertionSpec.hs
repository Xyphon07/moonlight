module Assertions.AssertionSpec
  ( tests,
  )
where

import Data.Set qualified as Set
import Moonlight.Pale.Test.Assertions
  ( assertApproxEqual,
    assertNonEmpty,
    assertSubsetOf,
    expectRight,
    expectRightWithLabel,
    expectSome,
    withResult,
  )
import Moonlight.Pale.Test.Core (ToleranceObstruction (..), mkTolerance)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Assertions"
    [ testCase "unwraps an unlabeled Right" $
        expectRight (Right "value" :: Either String String) >>= (@?= "value"),
      testCase "unwraps a labeled Right" $
        expectRightWithLabel "fixture" (Right "value" :: Either String String) >>= (@?= "value"),
      testCase "unwraps a labeled Just" $
        expectSome "fixture" (Just "value") >>= (@?= "value"),
      testCase "continues an assertion with a Right value" $
        withResult (Right "value" :: Either String String) (@?= "value"),
      testCase "accepts a non-empty list" $
        assertNonEmpty ["value"],
      testCase "accepts a subset" $
        assertSubsetOf (Set.fromList [1, 2 :: Int]) (Set.fromList [1, 2, 3]),
      testCase "zero tolerance accepts exact equality" $
        assertApproxEqual "exact" (mkTolerance 0 0) 1 1,
      testCase "relative tolerance scales with magnitude" $
        assertApproxEqual "relative" (mkTolerance 0 0.1) 100 105,
      testCase "equal infinities compare exactly" $
        assertApproxEqual "infinity" (mkTolerance 0 0) (1 / 0) (1 / 0),
      testCase "non-finite tolerance is a typed obstruction" $
        case mkTolerance (0 / 0) 0 of
          Left (ToleranceNotFinite absoluteLimit _) ->
            assertBool "expected retained NaN evidence" (isNaN absoluteLimit)
          other ->
            assertFailure ("expected ToleranceNotFinite, got " <> show other),
      testCase "negative tolerance is a typed obstruction" $
        mkTolerance (-1) 0 @?= Left (ToleranceNegative (-1) 0),
      testCase "non-finite relative tolerance is a typed obstruction" $
        case mkTolerance 0 (1 / 0) of
          Left (ToleranceNotFinite _ relativeLimit) ->
            assertBool "expected retained infinity evidence" (isInfinite relativeLimit)
          other ->
            assertFailure ("expected ToleranceNotFinite, got " <> show other)
    ]

{-| Typed-result, numeric-tolerance, and collection assertions. -}
module Moonlight.Pale.Test.Assertions
  ( expectRight,
    expectRightWithLabel,
    expectSome,
    assertApproxEqual,
    assertNonEmpty,
    assertSubsetOf,
    withResult,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Moonlight.Pale.Test.Core
  ( Tolerance,
    ToleranceObstruction,
    absoluteTolerance,
    relativeTolerance,
  )
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure)

expectedRightMessage :: Show e => e -> String
expectedRightMessage err = "expected Right, got Left: " <> show err

expectRight :: (HasCallStack, Show e) => Either e a -> IO a
expectRight = either (assertFailure . expectedRightMessage) pure

expectRightWithLabel :: (HasCallStack, Show e) => String -> Either e a -> IO a
expectRightWithLabel label =
  either (\err -> assertFailure (label <> ": " <> expectedRightMessage err)) pure

expectSome :: HasCallStack => String -> Maybe a -> IO a
expectSome label result =
  case result of
    Nothing -> assertFailure ("expected Just for " <> label <> ", got Nothing")
    Just val -> pure val

assertApproxEqual ::
  HasCallStack =>
  String ->
  Either ToleranceObstruction Tolerance ->
  Double ->
  Double ->
  Assertion
assertApproxEqual label toleranceResult expected actual =
  case toleranceResult of
    Left toleranceObstruction ->
      assertFailure
        (label <> ": invalid tolerance: " <> show toleranceObstruction)
    Right tolerance
      | isNaN expected || isNaN actual ->
          assertFailure
            (label <> ": NaN is never approximately equal; expected " <> show expected <> ", got " <> show actual)
      | isInfinite expected || isInfinite actual ->
          assertBool
            (label <> ": infinities must be exactly equal; expected " <> show expected <> ", got " <> show actual)
            (expected == actual)
      | otherwise ->
          let absoluteError = abs (actual - expected)
              relativeScale = max (abs expected) (abs actual)
              relativeLimit = relativeTolerance tolerance * relativeScale
              acceptedLimit = max (absoluteTolerance tolerance) relativeLimit
              relativeError =
                if relativeScale == 0
                  then 0
                  else absoluteError / relativeScale
           in assertBool
                ( label
                    <> ": expected "
                    <> show expected
                    <> ", got "
                    <> show actual
                    <> "; absolute error "
                    <> show absoluteError
                    <> " (limit "
                    <> show (absoluteTolerance tolerance)
                    <> "), relative error "
                    <> show relativeError
                    <> " (limit "
                    <> show (relativeTolerance tolerance)
                    <> ")"
                )
                (absoluteError <= acceptedLimit)

assertNonEmpty :: HasCallStack => [a] -> Assertion
assertNonEmpty xs =
  assertBool "expected non-empty list" (not (null xs))

assertSubsetOf :: (HasCallStack, Ord a, Show a) => Set a -> Set a -> Assertion
assertSubsetOf subset superset =
  let missing = Set.difference subset superset
   in assertBool
        ("expected subset, missing: " <> show (Set.toList missing))
        (Set.null missing)

withResult :: (HasCallStack, Show e) => Either e a -> (a -> Assertion) -> Assertion
withResult result check =
  either (assertFailure . expectedRightMessage) check result

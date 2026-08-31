module Helpers
  ( extractRight,
  )
where

import Test.Tasty.HUnit (Assertion, assertFailure)

extractRight :: Show e => Either e a -> (a -> Assertion) -> Assertion
extractRight value onRight =
  case value of
    Left err -> assertFailure ("expected Right, got Left: " <> show err)
    Right rightValue -> onRight rightValue

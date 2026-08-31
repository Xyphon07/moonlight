module CardinalitySpec (tests) where

import Moonlight.Core
  ( CardinalityFailure (..),
    checkedNaturalToInt,
    checkedNonNegativeProduct,
    checkedNonNegativeSum,
  )
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "checked cardinality"
    [ testCase "preserves zero-factor products" $
        checkedNonNegativeProduct 0 maxBound @?= Right 0,
      testCase "accepts the largest representable product" $
        checkedNonNegativeProduct 1 maxBound @?= Right maxBound,
      testCase "rejects a product beyond maxBound" $
        checkedNonNegativeProduct 2 maxBound
          @?= Left (CardinalityProductExceedsIntRange 2 maxBound),
      testCase "rejects the 2^32 by 2^32 wraparound shape" $
        let dimension = 2 ^ (32 :: Int)
         in checkedNonNegativeProduct dimension dimension
              @?= Left (CardinalityProductExceedsIntRange dimension dimension),
      testCase "rejects negative factors before multiplication" $
        checkedNonNegativeProduct (-1) 0
          @?= Left (NegativeCardinalityFactor (-1)),
      testCase "checks workspace sums" $
        checkedNonNegativeSum maxBound 1
          @?= Left (CardinalitySumExceedsIntRange maxBound 1),
      testCase "rejects Natural cardinality beyond Int" $
        let oversized = fromIntegral (maxBound :: Int) + 1 :: Natural
         in checkedNaturalToInt oversized
              @?= Left (NaturalCardinalityExceedsIntRange oversized)
    ]

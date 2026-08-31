{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Core.Cardinality
  ( CardinalityFailure (..),
    checkedNaturalToInt,
    checkedNonNegativeProduct,
    checkedNonNegativeSum,
  )
where

import Numeric.Natural (Natural)
import Prelude

data CardinalityFailure
  = NegativeCardinalityFactor Int
  | NaturalCardinalityExceedsIntRange Natural
  | CardinalityProductExceedsIntRange Int Int
  | CardinalitySumExceedsIntRange Int Int
  deriving stock (Eq, Show)

checkedNaturalToInt :: Natural -> Either CardinalityFailure Int
checkedNaturalToInt cardinality
  | cardinality > fromIntegral (maxBound :: Int) =
      Left (NaturalCardinalityExceedsIntRange cardinality)
  | otherwise =
      Right (fromIntegral cardinality)

checkedNonNegativeProduct :: Int -> Int -> Either CardinalityFailure Int
checkedNonNegativeProduct =
  checkedNonNegativeBinary CardinalityProductExceedsIntRange (*)

checkedNonNegativeSum :: Int -> Int -> Either CardinalityFailure Int
checkedNonNegativeSum =
  checkedNonNegativeBinary CardinalitySumExceedsIntRange (+)

checkedNonNegativeBinary ::
  (Int -> Int -> CardinalityFailure) ->
  (Integer -> Integer -> Integer) ->
  Int ->
  Int ->
  Either CardinalityFailure Int
checkedNonNegativeBinary rangeFailure combine left right
  | left < 0 = Left (NegativeCardinalityFactor left)
  | right < 0 = Left (NegativeCardinalityFactor right)
  | combinedValue > toInteger (maxBound :: Int) =
      Left (rangeFailure left right)
  | otherwise = Right (fromInteger combinedValue)
  where
    combinedValue = combine (toInteger left) (toInteger right)
{-# INLINE checkedNonNegativeBinary #-}

-- | Small total aggregation utilities over lists and foldables: safe indexing,
-- pairwise/adjacent folds, extrema, averages, spreads and unit-interval clamping.
module Moonlight.Core.Aggregate
  ( adjacentPairs,
    averageOf,
    maximumOf,
    minimumOf,
    minimumPositive,
    note,
    pairwise,
    safeIndex,
    safeIndexNatural,
    spectralGap,
    spreadOf,
    clampUnitInterval,
  )
where

import Data.List (foldl', genericDrop, sort, tails)
import Numeric.Natural (Natural)
import Prelude
  ( Double,
    Either (..),
    Int,
    Maybe (..),
    Num,
    Ord,
    drop,
    filter,
    fromIntegral,
    isNaN,
    length,
    max,
    maybe,
    min,
    otherwise,
    (+),
    (/),
    (-),
    (<),
    (<$>),
    (<*>),
    (>),
  )

averageOf :: [Double] -> Maybe Double
averageOf values =
  case values of
    [] -> Nothing
    _ -> Just (foldl' (+) 0.0 values / fromIntegral (length values))

minimumOf :: Ord value => [value] -> Maybe value
minimumOf values =
  case values of
    [] -> Nothing
    firstValue : restValues -> Just (foldl' min firstValue restValues)

maximumOf :: Ord value => [value] -> Maybe value
maximumOf values =
  case values of
    [] -> Nothing
    firstValue : restValues -> Just (foldl' max firstValue restValues)

minimumPositive :: (Ord value, Num value) => [value] -> Maybe value
minimumPositive values =
  case filter (> 0) values of
    [] -> Nothing
    firstValue : restValues -> Just (foldl' min firstValue restValues)

pairwise :: [value] -> [(value, value)]
pairwise values = [(x, y) | (x : ys) <- tails values, y <- ys]

spectralGap :: (Ord value, Num value) => [value] -> Maybe value
spectralGap values =
  case sort values of
    firstValue : secondValue : _ -> Just (secondValue - firstValue)
    _ -> Nothing

spreadOf :: (Ord value, Num value) => [value] -> Maybe value
spreadOf values =
  (-) <$> maximumOf values <*> minimumOf values

clampUnitInterval :: Double -> Double
clampUnitInterval value
  | isNaN value = value
  | otherwise = max 0.0 (min 1.0 value)

note :: e -> Maybe a -> Either e a
note e = maybe (Left e) Right

safeIndex :: Int -> [a] -> Maybe a
safeIndex idx xs
  | idx < 0 = Nothing
  | otherwise = case drop idx xs of
      [] -> Nothing
      (x : _) -> Just x

safeIndexNatural :: Natural -> [a] -> Maybe a
safeIndexNatural idx xs =
  case genericDrop idx xs of
    [] -> Nothing
    (x : _) -> Just x

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs values =
  case values of
    leftValue : rightValue : remainingValues ->
      (leftValue, rightValue) : adjacentPairs (rightValue : remainingValues)
    _ ->
      []

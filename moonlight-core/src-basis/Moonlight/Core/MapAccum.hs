{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

-- | Strict map building and accumulation helpers: indexing, grouping,
-- accumulating by key, and triple indexing.
module Moonlight.Core.MapAccum
  ( indexMap
  , groupByKey
  , accumByKey
  , buildTripleIndex
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Prelude
  ( Int
  , Ord
  , Semigroup ((<>))
  , foldl'
  , foldr
  , reverse
  , zip
  , (++)
  )

-- | Index values by zero-based input position.
--
-- Duplicate keys follow a last-occurrence-wins law: the stored index is the
-- final position at which the key appears in the input list.
indexMap :: Ord a => [a] -> Map a Int
indexMap values = Map.fromList (zip values [0 ..])

groupByKey :: Ord k => (a -> k) -> [a] -> Map k [a]
groupByKey keyOf = foldr (insertGroupedValue keyOf) Map.empty

-- | Accumulate values by key, combining them in input order.
--
-- Folding rightwards keeps every combination right-associated, so a
-- list-shaped value costs time linear in the group rather than quadratic.
-- Accumulating leftwards would append each new value to the end of an
-- ever-longer left operand.
accumByKey :: (Ord k, Semigroup v) => (a -> k) -> (a -> v) -> [a] -> Map k v
accumByKey keyOf valueOf =
  foldr
    (\value acc -> Map.insertWith (<>) (keyOf value) (valueOf value) acc)
    Map.empty

buildTripleIndex ::
  (Ord k1, Ord k2, Ord k3) =>
  (a -> k1) -> (a -> k2) -> (a -> k3) ->
  [a] ->
  (Map k1 [a], Map k2 [a], Map k3 [a])
buildTripleIndex keyOf1 keyOf2 keyOf3 values =
  tripleIndexAccumulatorMaps
    (foldl' (accumulateTripleIndex keyOf1 keyOf2 keyOf3) emptyTripleIndexAccumulator values)

data TripleIndexAccumulator k1 k2 k3 a
  = TripleIndexAccumulator !(Map k1 [a]) !(Map k2 [a]) !(Map k3 [a])

emptyTripleIndexAccumulator :: TripleIndexAccumulator k1 k2 k3 a
emptyTripleIndexAccumulator =
  TripleIndexAccumulator Map.empty Map.empty Map.empty

accumulateTripleIndex ::
  (Ord k1, Ord k2, Ord k3) =>
  (a -> k1) -> (a -> k2) -> (a -> k3) ->
  TripleIndexAccumulator k1 k2 k3 a ->
  a ->
  TripleIndexAccumulator k1 k2 k3 a
accumulateTripleIndex keyOf1 keyOf2 keyOf3 (TripleIndexAccumulator index1 index2 index3) value =
  TripleIndexAccumulator
    (insertGroupedValue keyOf1 value index1)
    (insertGroupedValue keyOf2 value index2)
    (insertGroupedValue keyOf3 value index3)

tripleIndexAccumulatorMaps :: TripleIndexAccumulator k1 k2 k3 a -> (Map k1 [a], Map k2 [a], Map k3 [a])
tripleIndexAccumulatorMaps (TripleIndexAccumulator index1 index2 index3) =
  (Map.map reverse index1, Map.map reverse index2, Map.map reverse index3)

insertGroupedValue :: Ord k => (a -> k) -> a -> Map k [a] -> Map k [a]
insertGroupedValue keyOf value =
  Map.insertWith (++) (keyOf value) [value]

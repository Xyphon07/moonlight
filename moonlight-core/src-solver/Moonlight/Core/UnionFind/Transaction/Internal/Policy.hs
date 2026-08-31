-- | Pure dense-prefix sizing policy: how many dense slots to allocate for a
-- given key distribution, growth targets, and the in-bounds predicate. No 'ST'.
module Moonlight.Core.UnionFind.Transaction.Internal.Policy
  ( chooseDenseLength,
    selectDenseLength,
    countKeysBelow,
    densePrefixMap,
    denseTargetLength,
    denseKeyInBounds,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Unboxed.Mutable qualified as Mutable
import Moonlight.Core.Identifier.EGraph (ClassId)
import Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( DenseStore (..),
    maximumDenseSlots,
    minimumDenseSlots,
  )
import Prelude

chooseDenseLength :: IntMap ClassId -> Int
chooseDenseLength parents
  | IntMap.null parents =
      minimumDenseSlots
  | otherwise =
      selectDenseLength
        (IntMap.keys (densePrefixMap maximumDenseSlots parents))
        denseCandidates
        0
        0
  where
    denseCandidates =
      takeWhile
        (<= maximumDenseSlots)
        (iterate (* 2) minimumDenseSlots)

selectDenseLength ::
  [Int] ->
  [Int] ->
  Int ->
  Int ->
  Int
selectDenseLength remainingKeys remainingLimits observedCount bestLength =
  case remainingLimits of
    [] ->
      bestLength
    limit : trailingLimits ->
      let (countAtLimit, keysAtOrAboveLimit) =
            countKeysBelow limit observedCount remainingKeys
          accepted =
            countAtLimit > 0
              && ( limit == minimumDenseSlots
                     || countAtLimit * 4 >= limit * 3
                 )
          nextBest =
            if accepted
              then limit
              else bestLength
       in selectDenseLength keysAtOrAboveLimit trailingLimits countAtLimit nextBest

countKeysBelow :: Int -> Int -> [Int] -> (Int, [Int])
countKeysBelow limit =
  go
  where
    go count keys =
      case keys of
        key : trailingKeys
          | key < limit ->
              go (count + 1) trailingKeys
        _ ->
          (count, keys)

densePrefixMap :: Int -> IntMap value -> IntMap value
densePrefixMap limit entries
  | limit <= 0 =
      IntMap.empty
  | otherwise =
      let (_, nonNegativeEntries) = IntMap.split (-1) entries
          (prefixEntries, _) = IntMap.split limit nonNegativeEntries
       in prefixEntries

denseTargetLength :: Int -> Int -> Maybe Int
denseTargetLength currentLength key
  | key < 0 =
      Nothing
  | key >= maximumDenseSlots =
      Nothing
  | key < currentLength =
      Just currentLength
  | currentLength == 0 =
      if key < minimumDenseSlots
        then Just minimumDenseSlots
        else Nothing
  | key < min maximumDenseSlots (currentLength * 2) =
      Just (min maximumDenseSlots (currentLength * 2))
  | otherwise =
      Nothing

denseKeyInBounds :: DenseStore state -> Int -> Bool
denseKeyInBounds store key =
  key >= 0
    && key < Mutable.length (parent store)
{-# INLINE denseKeyInBounds #-}

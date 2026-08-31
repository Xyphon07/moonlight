-- | The 'PartialOrder' class and helpers: comparability, and pointwise and
-- total-order orderings.
module Moonlight.Core.Order
  ( PartialOrder (..),
    comparable,
    incomparable,
    pointwiseLeqOver,
    finitePointwiseLeq,
    totalOrderLeq,
  )
where

import Data.Bool
  ( Bool (True),
    not,
    (&&),
    (||),
  )
import Data.Eq
  ( Eq,
    (/=),
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Ord
  ( Ord,
    (<=),
  )
import Data.Set qualified as Set
import Moonlight.Core.Finite
  ( FiniteUniverse,
    finiteUniverseList,
  )
import Numeric.Natural
  ( Natural,
  )
import Prelude
  ( Foldable,
    Int,
    Integer,
    foldr,
  )

class Eq order => PartialOrder order where
  leq :: order -> order -> Bool

  lt :: order -> order -> Bool
  lt left right =
    leq left right && left /= right

comparable :: PartialOrder order => order -> order -> Bool
comparable left right =
  leq left right || leq right left

incomparable :: PartialOrder order => order -> order -> Bool
incomparable left right =
  not (comparable left right)

pointwiseLeqOver ::
  (Foldable foldable, PartialOrder order) =>
  foldable domain ->
  (domain -> order) ->
  (domain -> order) ->
  Bool
pointwiseLeqOver domainValues left right =
  foldr
    ( \domainValue accepted ->
        leq (left domainValue) (right domainValue) && accepted
    )
    True
    domainValues

finitePointwiseLeq ::
  (FiniteUniverse domain, PartialOrder order) =>
  (domain -> order) ->
  (domain -> order) ->
  Bool
finitePointwiseLeq =
  pointwiseLeqOver finiteUniverseList

totalOrderLeq :: Ord order => order -> order -> Bool
totalOrderLeq =
  (<=)

instance PartialOrder () where
  leq _ _ =
    True

instance PartialOrder Bool where
  leq left right =
    not left || right

instance PartialOrder Int where
  leq =
    totalOrderLeq

instance PartialOrder Integer where
  leq =
    totalOrderLeq

instance PartialOrder Natural where
  leq =
    totalOrderLeq

instance Ord value => PartialOrder (Set.Set value) where
  leq =
    Set.isSubsetOf

instance PartialOrder IntSet.IntSet where
  leq =
    IntSet.isSubsetOf

instance (Ord key, PartialOrder value) => PartialOrder (Map.Map key value) where
  leq =
    Map.isSubmapOfBy leq

instance PartialOrder value => PartialOrder (IntMap.IntMap value) where
  leq =
    IntMap.isSubmapOfBy leq

instance (PartialOrder left, PartialOrder right) => PartialOrder (left, right) where
  leq (leftA, rightA) (leftB, rightB) =
    leq leftA leftB && leq rightA rightB

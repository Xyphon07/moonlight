{-# LANGUAGE DerivingStrategies #-}

{-|
Antichain frontiers over partial orders: lower, upper, and two-dimensional
product variants, answering containment queries. @ProductFrontier2@
canonicalizes a two-dimensional progress skyline by removing dominated points.

> import Moonlight.Delta.Frontier qualified as Frontier
>
> progress :: Frontier.ProductFrontier2 Int Int
> progress = Frontier.mkProductFrontier2 [(0, 3), (1, 2), (2, 1), (3, 0)]
>
> covered :: Bool
> covered = Frontier.productFrontier2Contains (2, 2) progress

Use @mkFrontier@ or @mkUpperFrontier@ for a general @PartialOrder@.
-}
module Moonlight.Delta.Frontier
  ( Frontier,
    UpperFrontier,
    ProductFrontier2,
    emptyFrontier,
    emptyUpperFrontier,
    singletonFrontier,
    singletonUpperFrontier,
    mkFrontier,
    mkUpperFrontier,
    mkProductFrontier2,
    frontierPoints,
    upperFrontierPoints,
    productFrontier2Points,
    frontierNull,
    upperFrontierNull,
    frontierContains,
    productFrontier2Contains,
    insertPoint,
    insertUpperFrontierPoint,
  )
where

import Data.Bool (Bool (False, True))
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Moonlight.Core
  ( PartialOrder (..),
  )
import Prelude (Eq (..), Maybe (..), Ord (..), Show, flip, maybe, min, not, otherwise, reverse, (.))

type LowerFrontierPolarity :: Type
data LowerFrontierPolarity

type UpperFrontierPolarity :: Type
data UpperFrontierPolarity

type FrontierWith :: Type -> Type -> Type
newtype FrontierWith polarity time = Frontier
  { antichain :: [time]
  }
  deriving stock (Eq, Ord, Show)

type Frontier :: Type -> Type
type Frontier =
  FrontierWith LowerFrontierPolarity

type UpperFrontier :: Type -> Type
type UpperFrontier =
  FrontierWith UpperFrontierPolarity

type ProductFrontier2 :: Type -> Type -> Type
newtype ProductFrontier2 left right = ProductFrontier2
  { staircase :: Map left right
  }
  deriving stock (Eq, Ord, Show)

emptyFrontier :: Frontier time
emptyFrontier =
  Frontier []

emptyUpperFrontier :: UpperFrontier time
emptyUpperFrontier =
  Frontier []

singletonFrontier :: time -> Frontier time
singletonFrontier time =
  Frontier [time]

singletonUpperFrontier :: time -> UpperFrontier time
singletonUpperFrontier time =
  Frontier [time]

mkFrontier ::
  (Ord time, PartialOrder time) =>
  [time] ->
  Frontier time
mkFrontier times =
  canonicalizeFrontier
    (Foldable.foldl' (flip (insertFrontierRaw leq)) emptyFrontier times)

mkUpperFrontier ::
  (Ord time, PartialOrder time) =>
  [time] ->
  UpperFrontier time
mkUpperFrontier times =
  canonicalizeFrontier
    (Foldable.foldl' (flip (insertFrontierRaw (flip leq))) emptyUpperFrontier times)

mkProductFrontier2 ::
  (Ord left, Ord right) =>
  [(left, right)] ->
  ProductFrontier2 left right
mkProductFrontier2 =
  ProductFrontier2 . productFrontierStaircase . Map.fromListWith min

frontierPoints :: Frontier time -> [time]
frontierPoints =
  antichain

upperFrontierPoints :: UpperFrontier time -> [time]
upperFrontierPoints =
  antichain

productFrontier2Points :: ProductFrontier2 left right -> [(left, right)]
productFrontier2Points =
  Map.toAscList . staircase

frontierNull :: Frontier time -> Bool
frontierNull (Frontier times) =
  case times of
    [] -> True
    _ -> False

upperFrontierNull :: UpperFrontier time -> Bool
upperFrontierNull (Frontier times) =
  case times of
    [] -> True
    _ -> False

frontierContains ::
  PartialOrder time =>
  time ->
  Frontier time ->
  Bool
frontierContains time (Frontier times) =
  Foldable.any
    (`leq` time)
    times

productFrontier2Contains ::
  (Ord left, Ord right) =>
  (left, right) ->
  ProductFrontier2 left right ->
  Bool
productFrontier2Contains (left, right) (ProductFrontier2 staircase) =
  maybe
    False
    (\(_, frontierRight) -> frontierRight <= right)
    (Map.lookupLE left staircase)

insertPoint ::
  (Ord time, PartialOrder time) =>
  time ->
  Frontier time ->
  Frontier time
insertPoint =
  insertFrontierWith leq

insertUpperFrontierPoint ::
  (Ord time, PartialOrder time) =>
  time ->
  UpperFrontier time ->
  UpperFrontier time
insertUpperFrontierPoint =
  insertFrontierWith (flip leq)

insertFrontierWith ::
  Ord time =>
  (time -> time -> Bool) ->
  time ->
  FrontierWith polarity time ->
  FrontierWith polarity time
insertFrontierWith coveredBy time frontier@(Frontier times) =
  case times of
    [] ->
      Frontier [time]
    [existingTime]
      | existingTime `coveredBy` time ->
          frontier
      | time `coveredBy` existingTime ->
          Frontier [time]
      | otherwise ->
          Frontier (List.insert time [existingTime])
    _ ->
      if Foldable.any (`coveredBy` time) times
        then frontier
        else
          Frontier (List.insert time (List.filter (not . coveredBy time) times))

insertFrontierRaw ::
  (time -> time -> Bool) ->
  time ->
  FrontierWith polarity time ->
  FrontierWith polarity time
insertFrontierRaw coveredBy time frontier@(Frontier times) =
  case times of
    [] ->
      Frontier [time]
    _ ->
      if Foldable.any (`coveredBy` time) times
        then frontier
        else Frontier (time : List.filter (not . coveredBy time) times)

canonicalizeFrontier :: Ord time => FrontierWith polarity time -> FrontierWith polarity time
canonicalizeFrontier (Frontier times) =
  Frontier (List.sort times)

productFrontierStaircase ::
  Ord right =>
  Map left right ->
  Map left right
productFrontierStaircase candidates =
  Map.fromDistinctAscList (reverse keptEntries)
  where
    (_, keptEntries) =
      Map.foldlWithKey' retainUndominated (Nothing, []) candidates

    retainUndominated ::
      Ord right =>
      (Maybe right, [(left, right)]) ->
      left ->
      right ->
      (Maybe right, [(left, right)])
    retainUndominated (bestRight, kept) left right =
      case bestRight of
        Nothing ->
          (Just right, (left, right) : kept)
        Just currentRight
          | right < currentRight ->
              (Just right, (left, right) : kept)
        _ ->
          (bestRight, kept)

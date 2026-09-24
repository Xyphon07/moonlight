{-# LANGUAGE DerivingStrategies #-}

-- | The dependency-light join-semilattice owner.
--
-- 'join' is associative, commutative, and idempotent. 'bottom' is its
-- identity. Instances are laws, not merely binary combining strategies.
module Moonlight.Algebra.Pure.Join
  ( JoinSemilattice (..)
  , BoundedJoinSemilattice (..)
  , Join (..)
  , joins
  , joins1
  ) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Foldable qualified as Foldable
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- | Select the join operation as a standard 'Semigroup' or 'Monoid'.
type Join :: Type -> Type
newtype Join a = Join {getJoin :: a}
  deriving stock (Eq, Ord, Show)

type JoinSemilattice :: Type -> Constraint
-- | Values admitting a lawful least upper bound.
class JoinSemilattice a where
  -- | Associative, commutative, and idempotent join.
  join :: a -> a -> a

instance JoinSemilattice () where
  join _ _ = ()

instance JoinSemilattice Bool where
  join = (||)

instance Ord a => JoinSemilattice (Set.Set a) where
  join = Set.union

instance JoinSemilattice IntSet.IntSet where
  join = IntSet.union

instance (Ord key, JoinSemilattice value) => JoinSemilattice (Map.Map key value) where
  join = Map.unionWith join

instance JoinSemilattice value => JoinSemilattice (IntMap.IntMap value) where
  join = IntMap.unionWith join

instance (JoinSemilattice left, JoinSemilattice right) => JoinSemilattice (left, right) where
  join (leftA, rightA) (leftB, rightB) =
    (join leftA leftB, join rightA rightB)

instance JoinSemilattice value => JoinSemilattice (key -> value) where
  join left right key =
    join (left key) (right key)

instance JoinSemilattice a => Semigroup (Join a) where
  Join left <> Join right =
    Join (join left right)

type BoundedJoinSemilattice :: Type -> Constraint
-- | A join-semilattice with an identity element.
class JoinSemilattice a => BoundedJoinSemilattice a where
  -- | The least element: @join bottom value == value@.
  bottom :: a

instance BoundedJoinSemilattice () where
  bottom = ()

instance BoundedJoinSemilattice Bool where
  bottom = False

instance Ord a => BoundedJoinSemilattice (Set.Set a) where
  bottom = Set.empty

instance BoundedJoinSemilattice IntSet.IntSet where
  bottom = IntSet.empty

instance (Ord key, JoinSemilattice value) => BoundedJoinSemilattice (Map.Map key value) where
  bottom = Map.empty

instance JoinSemilattice value => BoundedJoinSemilattice (IntMap.IntMap value) where
  bottom = IntMap.empty

instance
  (BoundedJoinSemilattice left, BoundedJoinSemilattice right) =>
  BoundedJoinSemilattice (left, right)
  where
  bottom =
    (bottom, bottom)

instance BoundedJoinSemilattice value => BoundedJoinSemilattice (key -> value) where
  bottom =
    const bottom

instance BoundedJoinSemilattice a => Monoid (Join a) where
  mempty =
    Join bottom

joins :: (BoundedJoinSemilattice a, Foldable foldable) => foldable a -> a
joins =
  Foldable.foldl' join bottom

joins1 :: JoinSemilattice a => NonEmpty a -> a
joins1 (first :| rest) =
  Foldable.foldl' join first rest

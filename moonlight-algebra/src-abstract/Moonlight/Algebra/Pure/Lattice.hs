-- | The lattice class tower: join/meet semilattices and their bounded forms,
-- lattices, distributive, Heyting and Boolean algebras, with fixpoint helpers.
--
-- Laws: join and meet are idempotent, commutative and associative; bounded
-- variants add an identity; lattices satisfy absorption; distributive adds
-- distributivity; Heyting adds residuation; Boolean adds complementation.
module Moonlight.Algebra.Pure.Lattice
  ( JoinSemilattice (..),
    BoundedJoinSemilattice (..),
    MeetSemilattice (..),
    BoundedMeetSemilattice (..),
    Lattice,
    OrderedLattice,
    BoundedLattice,
    DistributiveLattice,
    HeytingAlgebra (..),
    BooleanAlgebra (..),
    FixpointDivergence (..),
    Join (..),
    Meet (..),
    iterateFixpointFrom,
    leastFixpoint,
    greatestFixpoint,
    leastPreFixpoint,
    leastPreFixpointFrom,
    greatestPostFixpoint,
    greatestPostFixpointFrom,
    joinLeq,
    meetLeq,
    joins,
    joins1,
    meets,
    meets1,
    fromBool,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Algebra.Pure.Join
  ( BoundedJoinSemilattice (..)
  , Join (..)
  , JoinSemilattice (..)
  , joins
  , joins1
  )
import Moonlight.Core
  ( FixpointDivergence (..),
    PartialOrder,
    fixpointBounded,
  )
import Numeric.Natural
  ( Natural,
  )

type Meet :: Type -> Type
newtype Meet a = Meet {getMeet :: a}
  deriving stock (Eq, Ord, Show)

type MeetSemilattice :: Type -> Constraint
class MeetSemilattice a where
  meet :: a -> a -> a

instance MeetSemilattice () where
  meet _ _ = ()

instance MeetSemilattice Bool where
  meet = (&&)

instance Ord a => MeetSemilattice (Set.Set a) where
  meet = Set.intersection

instance MeetSemilattice IntSet.IntSet where
  meet = IntSet.intersection

instance (Ord key, MeetSemilattice value) => MeetSemilattice (Map.Map key value) where
  meet = Map.intersectionWith meet

instance MeetSemilattice value => MeetSemilattice (IntMap.IntMap value) where
  meet = IntMap.intersectionWith meet

instance (MeetSemilattice left, MeetSemilattice right) => MeetSemilattice (left, right) where
  meet (leftA, rightA) (leftB, rightB) =
    (meet leftA leftB, meet rightA rightB)

instance MeetSemilattice value => MeetSemilattice (key -> value) where
  meet left right key =
    meet (left key) (right key)

instance MeetSemilattice a => Semigroup (Meet a) where
  Meet left <> Meet right =
    Meet (meet left right)

type BoundedMeetSemilattice :: Type -> Constraint
class MeetSemilattice a => BoundedMeetSemilattice a where
  top :: a

instance BoundedMeetSemilattice () where
  top = ()

instance BoundedMeetSemilattice Bool where
  top = True

instance
  (BoundedMeetSemilattice left, BoundedMeetSemilattice right) =>
  BoundedMeetSemilattice (left, right)
  where
  top =
    (top, top)

instance BoundedMeetSemilattice value => BoundedMeetSemilattice (key -> value) where
  top =
    const top

instance BoundedMeetSemilattice a => Monoid (Meet a) where
  mempty =
    Meet top

type Lattice :: Type -> Constraint
class (JoinSemilattice a, MeetSemilattice a) => Lattice a

type OrderedLattice :: Type -> Constraint
class (PartialOrder a, Lattice a) => OrderedLattice a

type BoundedLattice :: Type -> Constraint
type BoundedLattice a = (Lattice a, BoundedJoinSemilattice a, BoundedMeetSemilattice a)

instance Lattice ()

instance OrderedLattice ()

instance Lattice Bool

instance OrderedLattice Bool

instance Ord a => Lattice (Set.Set a)

instance Ord a => OrderedLattice (Set.Set a)

instance Lattice IntSet.IntSet

instance OrderedLattice IntSet.IntSet

instance (Ord key, Lattice value) => Lattice (Map.Map key value)

instance (Ord key, OrderedLattice value) => OrderedLattice (Map.Map key value)

instance Lattice value => Lattice (IntMap.IntMap value)

instance OrderedLattice value => OrderedLattice (IntMap.IntMap value)

instance (Lattice left, Lattice right) => Lattice (left, right)

instance (OrderedLattice left, OrderedLattice right) => OrderedLattice (left, right)

instance Lattice value => Lattice (key -> value)

type DistributiveLattice :: Type -> Constraint
class Lattice a => DistributiveLattice a

instance DistributiveLattice ()

instance DistributiveLattice Bool

instance Ord a => DistributiveLattice (Set.Set a)

instance DistributiveLattice IntSet.IntSet

instance
  (Ord key, DistributiveLattice value) =>
  DistributiveLattice (Map.Map key value)

instance DistributiveLattice value => DistributiveLattice (IntMap.IntMap value)

instance
  (DistributiveLattice left, DistributiveLattice right) =>
  DistributiveLattice (left, right)

instance DistributiveLattice value => DistributiveLattice (key -> value)

infixr 5 <=>

type HeytingAlgebra :: Type -> Constraint
class (BoundedLattice a, DistributiveLattice a) => HeytingAlgebra a where
  implies :: a -> a -> a
  neg :: a -> a
  neg value =
    implies value bottom
  (<=>) :: a -> a -> a
  left <=> right =
    meet (implies left right) (implies right left)

instance HeytingAlgebra () where
  implies _ _ = ()

instance HeytingAlgebra Bool where
  implies left right =
    not left || right

instance
  (HeytingAlgebra left, HeytingAlgebra right) =>
  HeytingAlgebra (left, right)
  where
  implies (leftA, rightA) (leftB, rightB) =
    (implies leftA leftB, implies rightA rightB)

instance HeytingAlgebra value => HeytingAlgebra (key -> value) where
  implies left right key =
    implies (left key) (right key)

type BooleanAlgebra :: Type -> Constraint
class HeytingAlgebra a => BooleanAlgebra a where
  complement :: a -> a
  symmetricDifference :: a -> a -> a
  symmetricDifference left right =
    join
      (meet left (complement right))
      (meet (complement left) right)

instance BooleanAlgebra () where
  complement _ = ()

instance BooleanAlgebra Bool where
  complement =
    not

instance
  (BooleanAlgebra left, BooleanAlgebra right) =>
  BooleanAlgebra (left, right)
  where
  complement (left, right) =
    (complement left, complement right)

instance BooleanAlgebra value => BooleanAlgebra (key -> value) where
  complement value key =
    complement (value key)

iterateFixpointFrom ::
  Eq a =>
  Natural ->
  a ->
  (a -> a) ->
  Either (FixpointDivergence a) a
iterateFixpointFrom budget seed step =
  fixpointBounded budget step seed
{-# INLINE iterateFixpointFrom #-}

leastFixpoint ::
  (Eq a, BoundedJoinSemilattice a) =>
  Natural ->
  (a -> a) ->
  Either (FixpointDivergence a) a
leastFixpoint budget =
  iterateFixpointFrom budget bottom
{-# INLINE leastFixpoint #-}

greatestFixpoint ::
  (Eq a, BoundedMeetSemilattice a) =>
  Natural ->
  (a -> a) ->
  Either (FixpointDivergence a) a
greatestFixpoint budget =
  iterateFixpointFrom budget top
{-# INLINE greatestFixpoint #-}

leastPreFixpoint ::
  (Eq a, BoundedJoinSemilattice a) =>
  Natural ->
  (a -> a) ->
  Either (FixpointDivergence a) a
leastPreFixpoint budget =
  leastPreFixpointFrom budget bottom
{-# INLINE leastPreFixpoint #-}

leastPreFixpointFrom ::
  (Eq a, JoinSemilattice a) =>
  Natural ->
  a ->
  (a -> a) ->
  Either (FixpointDivergence a) a
leastPreFixpointFrom budget seed step =
  fixpointBounded budget (\current -> join current (step current)) seed
{-# INLINE leastPreFixpointFrom #-}

greatestPostFixpoint ::
  (Eq a, BoundedMeetSemilattice a) =>
  Natural ->
  (a -> a) ->
  Either (FixpointDivergence a) a
greatestPostFixpoint budget =
  greatestPostFixpointFrom budget top
{-# INLINE greatestPostFixpoint #-}

greatestPostFixpointFrom ::
  (Eq a, MeetSemilattice a) =>
  Natural ->
  a ->
  (a -> a) ->
  Either (FixpointDivergence a) a
greatestPostFixpointFrom budget seed step =
  fixpointBounded budget (\current -> meet current (step current)) seed
{-# INLINE greatestPostFixpointFrom #-}

joinLeq :: (Eq a, JoinSemilattice a) => a -> a -> Bool
joinLeq left right =
  join left right == right

meetLeq :: (Eq a, MeetSemilattice a) => a -> a -> Bool
meetLeq left right =
  meet left right == left

meets :: (BoundedMeetSemilattice a, Foldable foldable) => foldable a -> a
meets =
  foldl' meet top

meets1 :: MeetSemilattice a => NonEmpty a -> a
meets1 (first :| rest) =
  foldl' meet first rest

fromBool :: BoundedLattice a => Bool -> a
fromBool value =
  if value then top else bottom

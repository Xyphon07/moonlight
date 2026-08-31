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
import Moonlight.Core
  ( FixpointDivergence (..),
    PartialOrder,
    fixpointBounded,
  )
import Numeric.Natural
  ( Natural,
  )

type Join :: Type -> Type
newtype Join a = Join {getJoin :: a}
  deriving stock (Eq, Ord, Show)

type Meet :: Type -> Type
newtype Meet a = Meet {getMeet :: a}
  deriving stock (Eq, Ord, Show)

type JoinSemilattice :: Type -> Constraint
class JoinSemilattice a where
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
class JoinSemilattice a => BoundedJoinSemilattice a where
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

joins :: (BoundedJoinSemilattice a, Foldable foldable) => foldable a -> a
joins =
  foldl' join bottom

joins1 :: JoinSemilattice a => NonEmpty a -> a
joins1 (first :| rest) =
  foldl' join first rest

meets :: (BoundedMeetSemilattice a, Foldable foldable) => foldable a -> a
meets =
  foldl' meet top

meets1 :: MeetSemilattice a => NonEmpty a -> a
meets1 (first :| rest) =
  foldl' meet first rest

fromBool :: BoundedLattice a => Bool -> a
fromBool value =
  if value then top else bottom

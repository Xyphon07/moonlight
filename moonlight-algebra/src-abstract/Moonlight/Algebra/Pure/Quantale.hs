-- | Quantales: monoids in the join-semilattice — 'tensor' distributes over
-- 'join' and annihilates 'bottom' — with commutative and residuated
-- refinements, and the Viterbi, Łukasiewicz and tropical carriers.
--
-- Laws: tensor is associative with unit 'tensorUnit'; tensor distributes over
-- join on both sides and @tensor a bottom = bottom = tensor bottom a@;
-- commutative quantales add @tensor a b = tensor b a@; residuated quantales
-- satisfy the Galois law @joinLeq (tensor x y) z = joinLeq x (residual y z)@.
-- 'IntegralQuantale' marks carriers whose unit is top; 'ChainQuantale' marks
-- carriers whose join is selective.
module Moonlight.Algebra.Pure.Quantale
  ( Quantale (..),
    CommutativeQuantale,
    ResiduatedQuantale (..),
    IntegralQuantale,
    ChainQuantale,
    ChainOrder (..),
    Viterbi (..),
    Lukasiewicz (..),
    Tropical (..),
    tensors,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Algebra.Pure.Lattice
  ( BoundedJoinSemilattice (..),
    HeytingAlgebra (implies),
    JoinSemilattice (..),
    joinLeq,
  )

type Quantale :: Type -> Constraint
class BoundedJoinSemilattice a => Quantale a where
  tensor :: a -> a -> a
  tensorUnit :: a

instance Quantale () where
  tensor _ _ = ()
  tensorUnit = ()

instance Quantale Bool where
  tensor = (&&)
  tensorUnit = True

instance (Quantale left, Quantale right) => Quantale (left, right) where
  tensor (leftA, rightA) (leftB, rightB) =
    (tensor leftA leftB, tensor rightA rightB)
  tensorUnit =
    (tensorUnit, tensorUnit)

instance Quantale value => Quantale (key -> value) where
  tensor left right key =
    tensor (left key) (right key)
  tensorUnit =
    const tensorUnit

type CommutativeQuantale :: Type -> Constraint
class Quantale a => CommutativeQuantale a

instance CommutativeQuantale ()

instance CommutativeQuantale Bool

instance
  (CommutativeQuantale left, CommutativeQuantale right) =>
  CommutativeQuantale (left, right)

instance CommutativeQuantale value => CommutativeQuantale (key -> value)

type ResiduatedQuantale :: Type -> Constraint
class CommutativeQuantale a => ResiduatedQuantale a where
  residual :: a -> a -> a

instance ResiduatedQuantale () where
  residual _ _ = ()

instance ResiduatedQuantale Bool where
  residual = implies

instance
  (ResiduatedQuantale left, ResiduatedQuantale right) =>
  ResiduatedQuantale (left, right)
  where
  residual (leftA, rightA) (leftB, rightB) =
    (residual leftA leftB, residual rightA rightB)

instance ResiduatedQuantale value => ResiduatedQuantale (key -> value) where
  residual left right key =
    residual (left key) (right key)

-- | Marker law: 'tensorUnit' is the top element, so 'tensor' is two-sided
-- deflationary and valuation fixpoints converge within the simple-path bound.
type IntegralQuantale :: Type -> Constraint
class Quantale a => IntegralQuantale a

instance IntegralQuantale ()

instance IntegralQuantale Bool

instance
  (IntegralQuantale left, IntegralQuantale right) =>
  IntegralQuantale (left, right)

instance IntegralQuantale value => IntegralQuantale (key -> value)

-- | Marker law: 'join' is selective (returns one of its arguments), so a
-- single witness attains any finite join.
type ChainQuantale :: Type -> Constraint
class Quantale a => ChainQuantale a

instance ChainQuantale ()

instance ChainQuantale Bool

-- | The total order a selective 'join' induces on a chain quantale; the 'Ord'
-- instance is lawful exactly when the 'ChainQuantale' law holds.
type ChainOrder :: Type -> Type
newtype ChainOrder a = ChainOrder {getChainOrder :: a}
  deriving stock (Eq, Show)

instance (ChainQuantale a, Eq a) => Ord (ChainOrder a) where
  compare (ChainOrder left) (ChainOrder right)
    | left == right = EQ
    | joinLeq left right = LT
    | otherwise = GT

-- | Carrier discipline: values lie in the unit interval; the operations
-- preserve it.
type Viterbi :: Type -> Type
newtype Viterbi a = Viterbi {getViterbi :: a}
  deriving stock (Eq, Ord, Show)

instance Ord a => JoinSemilattice (Viterbi a) where
  join = max

instance (Ord a, Num a) => BoundedJoinSemilattice (Viterbi a) where
  bottom = Viterbi 0

instance (Ord a, Num a) => Quantale (Viterbi a) where
  tensor (Viterbi left) (Viterbi right) =
    Viterbi (left * right)
  tensorUnit = Viterbi 1

instance (Ord a, Num a) => CommutativeQuantale (Viterbi a)

instance (Ord a, Fractional a) => ResiduatedQuantale (Viterbi a) where
  residual (Viterbi left) (Viterbi right)
    | left <= right = Viterbi 1
    | otherwise = Viterbi (right / left)

instance (Ord a, Num a) => IntegralQuantale (Viterbi a)

instance (Ord a, Num a) => ChainQuantale (Viterbi a)

-- | Carrier discipline: values lie in the unit interval; the operations
-- preserve it.
type Lukasiewicz :: Type -> Type
newtype Lukasiewicz a = Lukasiewicz {getLukasiewicz :: a}
  deriving stock (Eq, Ord, Show)

instance Ord a => JoinSemilattice (Lukasiewicz a) where
  join = max

instance (Ord a, Num a) => BoundedJoinSemilattice (Lukasiewicz a) where
  bottom = Lukasiewicz 0

instance (Ord a, Num a) => Quantale (Lukasiewicz a) where
  tensor (Lukasiewicz left) (Lukasiewicz right) =
    Lukasiewicz (max 0 (left + right - 1))
  tensorUnit = Lukasiewicz 1

instance (Ord a, Num a) => CommutativeQuantale (Lukasiewicz a)

instance (Ord a, Num a) => ResiduatedQuantale (Lukasiewicz a) where
  residual (Lukasiewicz left) (Lukasiewicz right) =
    Lukasiewicz (min 1 (1 - left + right))

instance (Ord a, Num a) => IntegralQuantale (Lukasiewicz a)

instance (Ord a, Num a) => ChainQuantale (Lukasiewicz a)

-- | Min-plus costs. The lattice order is the dual of the derived 'Ord': join
-- is the numeric minimum and 'bottom' is 'TropicalInfinity'. The quantale
-- laws hold over all of @a@; only integrality ('tensorUnit' as top) and the
-- truncated 'residual' require the nonnegative cone.
type Tropical :: Type -> Type
data Tropical a
  = TropicalFinite !a
  | TropicalInfinity
  deriving stock (Eq, Ord, Show)

instance Ord a => JoinSemilattice (Tropical a) where
  join = min

instance Ord a => BoundedJoinSemilattice (Tropical a) where
  bottom = TropicalInfinity

instance (Ord a, Num a) => Quantale (Tropical a) where
  tensor TropicalInfinity _ = TropicalInfinity
  tensor _ TropicalInfinity = TropicalInfinity
  tensor (TropicalFinite left) (TropicalFinite right) =
    TropicalFinite (left + right)
  tensorUnit = TropicalFinite 0

instance (Ord a, Num a) => CommutativeQuantale (Tropical a)

instance (Ord a, Num a) => ResiduatedQuantale (Tropical a) where
  residual TropicalInfinity _ = TropicalFinite 0
  residual (TropicalFinite _) TropicalInfinity = TropicalInfinity
  residual (TropicalFinite left) (TropicalFinite right) =
    TropicalFinite (max 0 (right - left))

instance (Ord a, Num a) => ChainQuantale (Tropical a)

tensors :: (Quantale a, Foldable foldable) => foldable a -> a
tensors =
  foldl' tensor tensorUnit

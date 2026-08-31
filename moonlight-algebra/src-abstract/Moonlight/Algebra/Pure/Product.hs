{-# LANGUAGE RoleAnnotations #-}

-- | Finite @n@-fold product algebras ('ProductAlgebra', arity at the type level)
-- with lattice, ring and module structure defined coordinatewise.
--
-- Laws: every law holds in the product exactly when it holds in each component.
module Moonlight.Algebra.Pure.Product
  ( ProductAlgebra,
    mkProductAlgebra,
    toProductList,
  )
where

import Data.Kind (Type)
import Data.List (genericLength, genericReplicate)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Numeric.Natural (Natural)
import Moonlight.Algebra.Pure.Lattice
  ( BoundedJoinSemilattice (..),
    BoundedMeetSemilattice (..),
    BooleanAlgebra (..),
    DistributiveLattice,
    HeytingAlgebra (implies),
    JoinSemilattice (..),
    Lattice,
    MeetSemilattice (..),
    OrderedLattice,
  )
import Moonlight.Algebra.Pure.Module (BilinearSpace (..), Module (..), VectorSpace)
import Moonlight.Algebra.Pure.Ring
  ( CommutativeRing,
    Semiring,
  )
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    Field,
    MultiplicativeMonoid (..),
    PartialOrder (..),
    Ring,
  )

type ProductAlgebra :: Nat -> Type -> Type
type role ProductAlgebra nominal representational
newtype ProductAlgebra (n :: Nat) a = ProductAlgebra [a]
  deriving stock (Eq, Show, Functor)

mkProductAlgebra :: forall n a. KnownNat n => [a] -> Maybe (ProductAlgebra n a)
mkProductAlgebra values
  | genericLength values == expectedArity (Proxy @n) = Just (ProductAlgebra values)
  | otherwise = Nothing

toProductList :: ProductAlgebra n a -> [a]
toProductList (ProductAlgebra values) = values

liftProduct2 :: (a -> b -> c) -> ProductAlgebra n a -> ProductAlgebra n b -> ProductAlgebra n c
liftProduct2 combine (ProductAlgebra left) (ProductAlgebra right) =
  ProductAlgebra (zipWith combine left right)

expectedArity :: forall n. KnownNat n => Proxy n -> Natural
expectedArity _ = natVal (Proxy @n)

instance KnownNat n => Applicative (ProductAlgebra n) where
  pure value = ProductAlgebra (genericReplicate (expectedArity (Proxy @n)) value)
  ProductAlgebra transforms <*> ProductAlgebra values =
    ProductAlgebra (zipWith ($) transforms values)

instance (KnownNat n, AdditiveMonoid a) => AdditiveMonoid (ProductAlgebra n a) where
  zero = pure zero
  add = liftProduct2 add

instance (KnownNat n, AdditiveGroup a) => AdditiveGroup (ProductAlgebra n a) where
  neg = fmap neg
  sub left right = add left (neg right)

instance (KnownNat n, MultiplicativeMonoid a) => MultiplicativeMonoid (ProductAlgebra n a) where
  one = pure one
  mul = liftProduct2 mul

instance (KnownNat n, Ring a) => Ring (ProductAlgebra n a)

instance (KnownNat n, Semiring a) => Semiring (ProductAlgebra n a)

instance (KnownNat n, CommutativeRing a) => CommutativeRing (ProductAlgebra n a)

instance (KnownNat n, Ring r) => Module r (ProductAlgebra n r) where
  scale scalar = fmap (mul scalar)

instance (KnownNat n, Field k) => VectorSpace k (ProductAlgebra n k)

instance (KnownNat n, Field k) => BilinearSpace k (ProductAlgebra n k) where
  bilinearForm (ProductAlgebra left) (ProductAlgebra right) =
    foldr add zero (zipWith mul left right)

instance JoinSemilattice a => JoinSemilattice (ProductAlgebra n a) where
  join = liftProduct2 join

instance (KnownNat n, BoundedJoinSemilattice a) => BoundedJoinSemilattice (ProductAlgebra n a) where
  bottom = pure bottom

instance MeetSemilattice a => MeetSemilattice (ProductAlgebra n a) where
  meet = liftProduct2 meet

instance PartialOrder a => PartialOrder (ProductAlgebra n a) where
  leq (ProductAlgebra left) (ProductAlgebra right) =
    and (zipWith leq left right)

instance (KnownNat n, BoundedMeetSemilattice a) => BoundedMeetSemilattice (ProductAlgebra n a) where
  top = pure top

instance Lattice a => Lattice (ProductAlgebra n a)

instance OrderedLattice a => OrderedLattice (ProductAlgebra n a)

instance DistributiveLattice a => DistributiveLattice (ProductAlgebra n a)

instance (KnownNat n, HeytingAlgebra a) => HeytingAlgebra (ProductAlgebra n a) where
  implies = liftProduct2 implies

instance (KnownNat n, BooleanAlgebra a) => BooleanAlgebra (ProductAlgebra n a) where
  complement = fmap complement

{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Integers modulo a type-level natural ('Zn'), carrying the additive group
-- and 'CommutativeRing' instances.
--
-- Laws: @Zn n@ is a commutative ring under arithmetic modulo @n@ (a field exactly
-- when @n@ is prime).
module Moonlight.Algebra.Pure.Zn
  ( Zn,
    mkZn,
    unZn,
    znModulus,
  )
where

import Data.Kind (Type)
import GHC.TypeNats (KnownNat, Nat, natVal, type (<=))
import Moonlight.Algebra.Pure.Ring (CommutativeRing, Semiring)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MultiplicativeMonoid (..),
    Ring,
  )
import Data.Proxy (Proxy (..))

type Zn :: Nat -> Type
type role Zn nominal
newtype Zn (n :: Nat) = Zn Integer
  deriving stock (Eq, Ord)

unZn :: Zn n -> Integer
unZn (Zn value) = value

znModulus :: forall n proxy. KnownNat n => proxy n -> Integer
znModulus _ = toInteger (natVal (Proxy @n))

mkZn :: forall n. (KnownNat n, 1 <= n) => Integer -> Zn n
mkZn value = Zn (value `mod` znModulus (Proxy @n))

instance (KnownNat n, 1 <= n) => Show (Zn n) where
  show value = show (unZn value) <> " (mod " <> show (znModulus (Proxy @n)) <> ")"

instance (KnownNat n, 1 <= n) => AdditiveMonoid (Zn n) where
  zero = mkZn @n 0
  add (Zn left) (Zn right) = mkZn @n (left + right)

instance (KnownNat n, 1 <= n) => AdditiveGroup (Zn n) where
  neg (Zn value) = mkZn @n (negate value)
  sub (Zn left) (Zn right) = mkZn @n (left - right)

instance (KnownNat n, 1 <= n) => MultiplicativeMonoid (Zn n) where
  one = mkZn @n 1
  mul (Zn left) (Zn right) = mkZn @n (left * right)

instance (KnownNat n, 1 <= n) => Ring (Zn n)

instance (KnownNat n, 1 <= n) => Semiring (Zn n)

instance (KnownNat n, 1 <= n) => CommutativeRing (Zn n)

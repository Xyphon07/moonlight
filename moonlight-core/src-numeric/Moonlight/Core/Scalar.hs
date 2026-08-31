{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Core.Scalar
  ( AdditiveMonoid (..),
    AdditiveGroup (..),
    MultiplicativeMonoid (..),
    Semiring,
    Ring,
    CommutativeRing,
    Field (..),
    requireInvertible,
    Metric (..),
  )
where

import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Data.Type.Equality (type (~))
import Prelude
  ( Bool (..),
    Double,
    Either (..),
    Float,
    Fractional ((/)),
    Int,
    Integer,
    Maybe (..),
    Num (..),
    Rational,
    RealFloat,
    fmap,
    isInfinite,
    isNaN,
    not,
    otherwise,
    (&&),
    (||),
    (.),
    (==),
  )

type AdditiveMonoid :: Type -> Constraint
-- | Additive monoid structure for scalar carriers.
--
-- Laws hold exactly for exact carriers ('Int', 'Integer', 'Rational',
-- 'Bool'). For IEEE floating-point carriers ('Double', 'Float') the
-- identity laws hold on non-NaN values, while associativity and
-- distributivity hold only up to rounding; this qualification applies
-- to every law block in this module that is instantiated at a
-- floating-point carrier.
--
-- Laws:
--
-- [Left identity] @add zero x = x@
--
-- [Right identity] @add x zero = x@
--
-- [Associativity] @add (add x y) z = add x (add y z)@
class AdditiveMonoid a where
  -- | Additive identity.
  zero :: a
  default zero :: Num a => a
  zero = 0

  -- | Additive composition.
  add :: a -> a -> a
  default add :: Num a => a -> a -> a
  add = (+)

type AdditiveGroup :: Type -> Constraint
-- | Additive group structure extending 'AdditiveMonoid'.
--
-- Laws:
--
-- [Left inverse] @add (neg x) x = zero@
--
-- [Right inverse] @add x (neg x) = zero@
--
-- [Involution] @neg (neg x) = x@
--
-- [Subtraction] @sub x y = add x (neg y)@
class AdditiveMonoid a => AdditiveGroup a where
  -- | Additive inverse.
  neg :: a -> a
  default neg :: Num a => a -> a
  neg = negate

  -- | Subtraction as addition of the inverse.
  sub :: a -> a -> a
  sub x y = x `add` neg y

type MultiplicativeMonoid :: Type -> Constraint
-- | Multiplicative monoid structure for scalar carriers.
--
-- Laws:
--
-- [Left identity] @mul one x = x@
--
-- [Right identity] @mul x one = x@
--
-- [Associativity] @mul (mul x y) z = mul x (mul y z)@
class MultiplicativeMonoid a where
  -- | Multiplicative identity.
  one :: a
  default one :: Num a => a
  one = 1

  -- | Multiplicative composition.
  mul :: a -> a -> a
  default mul :: Num a => a -> a -> a
  mul = (*)

type Semiring :: Type -> Constraint
-- | Semiring structure over additive and multiplicative monoids.
--
-- Laws:
--
-- [Additive commutativity] @add x y = add y x@
--
-- [Left distributivity] @mul x (add y z) = add (mul x y) (mul x z)@
--
-- [Right distributivity] @mul (add y z) x = add (mul y x) (mul z x)@
class (AdditiveMonoid a, MultiplicativeMonoid a) => Semiring a

type Ring :: Type -> Constraint
-- | Ring structure: a semiring whose additive structure is a group.
class (Semiring a, AdditiveGroup a) => Ring a

type CommutativeRing :: Type -> Constraint
-- | Commutative ring structure.
--
-- Law:
--
-- [Multiplicative commutativity] @mul x y = mul y x@
class Ring a => CommutativeRing a

type Field :: Type -> Constraint
-- | Partial field interface for scalar carriers.
--
-- Laws over values satisfying 'fieldValueValid':
--
-- [Zero rejection] @tryInv zero = Nothing@
--
-- [Inverse identity] for invertible @x@, @tryInv x = Just y@ implies @mul x y = one@
--
-- [Division] @tryDiv x y = fmap (mul x) (tryInv y)@
--
-- [Division by one] @tryDiv x one = Just x@
--
-- [Division by zero] @tryDiv x zero = Nothing@
--
-- [Invertibility predicate] @canInvert x = isJust (tryInv x)@
class (Ring a) => Field a where
  -- | Attempt to produce a multiplicative inverse.
  tryInv :: a -> Maybe a

  -- | Attempt division by multiplying by the inverse of the divisor.
  tryDiv :: a -> a -> Maybe a
  tryDiv x y = fmap (mul x) (tryInv y)

  -- | Whether 'tryInv' succeeds.
  canInvert :: a -> Bool
  canInvert = isJust . tryInv

  -- | Domain predicate for values on which field laws are required.
  fieldValueValid :: a -> Bool
  fieldValueValid _ = True

requireInvertible :: Field a => errorValue -> a -> Either errorValue a
requireInvertible errorValue value =
  case tryInv value of
    Just inverseValue -> Right inverseValue
    Nothing -> Left errorValue

type Metric :: Type -> Constraint
-- | Magnitude projection for scalar carriers.
--
-- Laws:
--
-- [Zero magnitude] @magnitude zero = zero@
--
-- [Negation invariance] @magnitude (neg x) = magnitude x@
--
-- [Non-negativity] ordered magnitudes that can represent absolute values satisfy @magnitude x >= zero@.
class Metric a where
  type Magnitude a :: Type
  -- | Project a scalar into its magnitude carrier.
  magnitude :: a -> Magnitude a
  default magnitude :: (Num a, Magnitude a ~ a) => a -> Magnitude a
  magnitude = abs

instance AdditiveMonoid Bool where
  zero = False
  add = (||)

instance MultiplicativeMonoid Bool where
  one = True
  mul = (&&)

instance Semiring Bool

instance AdditiveMonoid Int

instance AdditiveGroup Int where
  sub = (-)

instance MultiplicativeMonoid Int

instance Semiring Int

instance Ring Int

instance Metric Int where
  type Magnitude Int = Int

instance AdditiveMonoid Integer

instance AdditiveGroup Integer where
  sub = (-)

instance MultiplicativeMonoid Integer

instance Semiring Integer

instance Ring Integer

instance CommutativeRing Integer

instance Metric Integer where
  type Magnitude Integer = Integer

instance AdditiveMonoid Double

instance AdditiveGroup Double where
  sub = (-)

instance MultiplicativeMonoid Double

instance Semiring Double

instance Ring Double

instance CommutativeRing Double

instance Field Double where
  tryInv = ieeeTryInv
  fieldValueValid = ieeeFieldValueValid

instance Metric Double where
  type Magnitude Double = Double

instance AdditiveMonoid Float

instance AdditiveGroup Float where
  sub = (-)

instance MultiplicativeMonoid Float

instance Semiring Float

instance Ring Float

instance CommutativeRing Float

instance Field Float where
  tryInv = ieeeTryInv
  fieldValueValid = ieeeFieldValueValid

instance Metric Float where
  type Magnitude Float = Float

ieeeTryInv :: RealFloat scalar => scalar -> Maybe scalar
ieeeTryInv value
  | not (ieeeFieldValueValid value) = Nothing
  | value == 0 = Nothing
  | not (ieeeFieldValueValid inverse) = Nothing
  | otherwise = Just inverse
  where
    inverse = 1 / value
{-# INLINE ieeeTryInv #-}

ieeeFieldValueValid :: RealFloat scalar => scalar -> Bool
ieeeFieldValueValid value =
  not (isNaN value) && not (isInfinite value)
{-# INLINE ieeeFieldValueValid #-}

instance AdditiveMonoid Rational

instance AdditiveGroup Rational where
  sub = (-)

instance MultiplicativeMonoid Rational

instance Semiring Rational

instance Ring Rational

instance CommutativeRing Rational

instance Field Rational where
  tryInv value
    | value == zero = Nothing
    | otherwise = Just (1 / value)

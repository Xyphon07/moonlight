{-# LANGUAGE ConstraintKinds #-}

-- | Domain refinements over the canonical numeric tower in "Moonlight.Core".
--
-- 'Semiring' and 'CommutativeRing' are re-exported from "Moonlight.Core"; they
-- carry no second copies of @zero@, @one@, @add@, or @mul@. This module adds the
-- progressively stronger integral, GCD, Euclidean, and canonical-remainder
-- laws.
module Moonlight.Algebra.Pure.Ring
  ( Semiring,
    CommutativeRing,
    IntegralDomain (..),
    GCDDomain (..),
    NonZeroDivisor,
    mkNonZeroDivisor,
    nonZeroDivisorValue,
    EuclideanDomain (..),
    CanonicalEuclideanDomain (..),
  )
where

import Data.Kind (Constraint, Type)
import Data.Maybe (isJust)
import Moonlight.Algebra.Unsafe.GCDWitness
  ( NonZero,
    mkNonZeroInternal,
    nonZeroValue,
  )
import Moonlight.Core
  ( CommutativeRing,
    Semiring,
    zero,
  )
import Numeric.Natural (Natural)
import Prelude
  ( Eq,
    Bool,
    Integer,
    Maybe (..),
    Ord,
    abs,
    divMod,
    fromInteger,
    gcd,
    mod,
    otherwise,
    signum,
    (*),
    (-),
    (==),
    (.),
  )

type IntegralDomain :: Type -> Constraint
class (CommutativeRing a, Eq a) => IntegralDomain a where
  isZero :: a -> Bool
  isZero value = value == zero

  unitInverse :: a -> Maybe a

  isUnit :: a -> Bool
  isUnit = isJust . unitInverse

type GCDDomain :: Type -> Constraint
class IntegralDomain a => GCDDomain a where
  gcdDomain :: a -> a -> a
  extendedGcdDomain :: a -> a -> (a, a, a)

data EuclideanDivisor

type NonZeroDivisor :: Type -> Type
type NonZeroDivisor a = NonZero EuclideanDivisor a

mkNonZeroDivisor :: IntegralDomain a => a -> Maybe (NonZeroDivisor a)
mkNonZeroDivisor =
  mkNonZeroInternal isZero

nonZeroDivisorValue :: NonZeroDivisor a -> a
nonZeroDivisorValue =
  nonZeroValue

type EuclideanDomain :: Type -> Constraint
class (GCDDomain a, Ord (Degree a)) => EuclideanDomain a where
  type Degree a :: Type
  divideWithRemainder :: a -> NonZeroDivisor a -> (a, a)
  degree :: a -> Degree a

type CanonicalEuclideanDomain :: Type -> Constraint
class EuclideanDomain a => CanonicalEuclideanDomain a where
  canonicalRemainder :: a -> NonZeroDivisor a -> a

instance IntegralDomain Integer where
  unitInverse value
    | abs value == 1 = Just value
    | otherwise = Nothing

instance GCDDomain Integer where
  gcdDomain left right = abs (gcd left right)
  extendedGcdDomain = integerExtendedGcd

instance EuclideanDomain Integer where
  type Degree Integer = Natural
  divideWithRemainder value divisor =
    divMod value (nonZeroDivisorValue divisor)
  degree value = fromInteger (abs value)

instance CanonicalEuclideanDomain Integer where
  canonicalRemainder value divisor =
    value `mod` abs (nonZeroDivisorValue divisor)

integerExtendedGcd :: Integer -> Integer -> (Integer, Integer, Integer)
integerExtendedGcd left right
  | right == 0 = (abs left, signum left, 0)
  | otherwise =
      let (quotient, remainder) = divMod left right
          (gcdValue, coeffRight, coeffRemainder) = integerExtendedGcd right remainder
       in (gcdValue, coeffRemainder, coeffRight - quotient * coeffRemainder)

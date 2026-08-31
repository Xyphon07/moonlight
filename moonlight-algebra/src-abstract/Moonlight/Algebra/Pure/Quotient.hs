{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Elements of a quotient ring under a branded runtime modulus.
--
-- The context carries the modulus witness. Quotient values carry only their
-- canonical representative and cannot be combined across different modulus
-- brands.
module Moonlight.Algebra.Pure.Quotient
  ( QuotientContext,
    Quotient,
    withQuotientContext,
    quotient,
    quotientRepresentative,
    zeroQuotient,
    oneQuotient,
    negateQuotient,
    addQuotient,
    subtractQuotient,
    multiplyQuotient,
  )
where

import Data.Kind (Type)
import Moonlight.Algebra.Pure.GCD
  ( NonZeroModulus,
    withNonZeroModulus,
  )
import Moonlight.Algebra.Pure.Ring
  ( CanonicalEuclideanDomain (..),
    IntegralDomain,
  )
import Moonlight.Algebra.Unsafe.GCDWitness (retagNonZero)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MultiplicativeMonoid (..),
  )

type QuotientContext :: Type -> Type -> Type
newtype QuotientContext modulus a =
  QuotientContext (NonZeroModulus modulus a)

type role QuotientContext nominal representational

type Quotient :: Type -> Type -> Type
newtype Quotient modulus a =
  Quotient a
  deriving stock (Eq, Ord, Show)

type role Quotient nominal representational

withQuotientContext ::
  IntegralDomain a =>
  a ->
  (forall modulus. QuotientContext modulus a -> result) ->
  Maybe result
withQuotientContext modulus continuation =
  withNonZeroModulus modulus (continuation . QuotientContext)

quotient ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  a ->
  Quotient modulus a
quotient context =
  Quotient . normalize context

quotientRepresentative ::
  Quotient modulus a ->
  a
quotientRepresentative (Quotient value) =
  value

zeroQuotient ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  Quotient modulus a
zeroQuotient context =
  quotient context zero

oneQuotient ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  Quotient modulus a
oneQuotient context =
  quotient context one

negateQuotient ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  Quotient modulus a ->
  Quotient modulus a
negateQuotient context (Quotient value) =
  quotient context (neg value)

addQuotient ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  Quotient modulus a ->
  Quotient modulus a ->
  Quotient modulus a
addQuotient context (Quotient left) (Quotient right) =
  quotient context (add left right)

subtractQuotient ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  Quotient modulus a ->
  Quotient modulus a ->
  Quotient modulus a
subtractQuotient context (Quotient left) (Quotient right) =
  quotient context (sub left right)

multiplyQuotient ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  Quotient modulus a ->
  Quotient modulus a ->
  Quotient modulus a
multiplyQuotient context (Quotient left) (Quotient right) =
  quotient context (mul left right)

normalize ::
  CanonicalEuclideanDomain a =>
  QuotientContext modulus a ->
  a ->
  a
normalize (QuotientContext modulus) value =
  canonicalRemainder value (retagNonZero modulus)

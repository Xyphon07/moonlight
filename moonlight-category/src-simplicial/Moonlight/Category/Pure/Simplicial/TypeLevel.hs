
{-# LANGUAGE RankNTypes #-}

-- | Runtime-checked dimension and finite-index types (@Dimension@, @Fin@)
-- shared by the simplicial layer.
module Moonlight.Category.Pure.Simplicial.TypeLevel
  ( Dimension (..),
    SomeDimension (..),
    dimensionValue,
    mkSomeDimension,
    allDimensionsUpTo,
    Fin,
    finValue,
    mkFin,
    mkFinOffset,
    withFin,
    withReifiedFinOffset,
    allFinite,
    weakenFin,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, SomeNat (..), natVal, someNatVal, type (+))
import Numeric.Natural (Natural)

type Dimension :: Nat -> Type
data Dimension (n :: Nat) = Dimension

type SomeDimension :: Type
data SomeDimension where
  SomeDimension :: KnownNat n => Dimension n -> SomeDimension

dimensionValue :: forall n. KnownNat n => Dimension n -> Natural
dimensionValue _ = natVal (Proxy @n)

mkSomeDimension :: Natural -> SomeDimension
mkSomeDimension naturalValue =
  case someNatVal naturalValue of
    SomeNat (_ :: Proxy n) -> SomeDimension (Dimension :: Dimension n)

allDimensionsUpTo :: Natural -> [SomeDimension]
allDimensionsUpTo upperBound =
  map mkSomeDimension [0 .. upperBound]

type Fin :: Nat -> Type
newtype Fin (n :: Nat) = Fin Natural
  deriving stock (Eq, Ord, Show)

finValue :: Fin n -> Natural
finValue (Fin naturalValue) = naturalValue

mkFin :: forall n. KnownNat n => Natural -> Maybe (Fin n)
mkFin candidate =
  if candidate < natVal (Proxy @n)
    then Just (Fin candidate)
    else Nothing

mkFinOffset :: forall n m. (KnownNat n, KnownNat m) => Dimension n -> Natural -> Maybe (Fin (n + m))
mkFinOffset _ candidate =
  let upperBound = natVal (Proxy @n) + natVal (Proxy @m)
   in if candidate < upperBound
        then Just (Fin candidate)
        else Nothing

withFin :: forall n result. KnownNat n => Natural -> (Fin n -> result) -> Maybe result
withFin candidate handler = handler <$> mkFin candidate

allFinite :: forall n. KnownNat n => [Fin n]
allFinite =
  let upperBound = natVal (Proxy @n)
   in if upperBound == 0
        then []
        else map Fin [0 .. upperBound - 1]

withReifiedFinOffset :: forall offset a. KnownNat offset
  => Natural -> Natural -> (forall n. KnownNat n => Dimension n -> Fin (n + offset) -> Maybe a) -> Maybe a
withReifiedFinOffset dimensionNat indexNat f =
  case someNatVal dimensionNat of
    SomeNat (_ :: Proxy n) -> mkFinOffset @n @offset (Dimension @n) indexNat >>= f (Dimension @n)

weakenFin :: Fin n -> Fin (n + 1)
weakenFin (Fin naturalValue) = Fin naturalValue

{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Algebra.Unsafe.GCDWitness
  ( NonZero,
    NonZeroModulus,
    mkNonZeroInternal,
    nonZeroValue,
    retagNonZero,
    CanonicalResidue (..),
    canonicalResidueModulus,
    canonicalResidueValue,
  )
where

import Data.Kind (Type)
import Prelude (Bool, Eq, Maybe (..), Show, not)

type NonZero :: Type -> Type -> Type
newtype NonZero witness a = NonZero a
  deriving stock (Eq, Show)

type role NonZero nominal representational

type NonZeroModulus :: Type -> Type -> Type
type NonZeroModulus modulus a = NonZero modulus a

mkNonZeroInternal :: (a -> Bool) -> a -> Maybe (NonZero witness a)
mkNonZeroInternal isZeroValue value =
  if not (isZeroValue value)
    then Just (NonZero value)
    else Nothing

nonZeroValue :: NonZero witness a -> a
nonZeroValue (NonZero value) = value

retagNonZero :: NonZero source a -> NonZero target a
retagNonZero (NonZero value) = NonZero value

type CanonicalResidue :: Type -> Type -> Type
data CanonicalResidue modulus a = CanonicalResidue !(NonZeroModulus modulus a) !a
  deriving stock (Eq, Show)

type role CanonicalResidue nominal representational

canonicalResidueModulus :: CanonicalResidue modulus a -> NonZeroModulus modulus a
canonicalResidueModulus (CanonicalResidue modulus _) = modulus

canonicalResidueValue :: CanonicalResidue modulus a -> a
canonicalResidueValue (CanonicalResidue _ value) = value

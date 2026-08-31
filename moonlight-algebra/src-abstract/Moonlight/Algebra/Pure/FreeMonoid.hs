{-# LANGUAGE DeriveTraversable #-}

-- | The free monoid: 'Batch', a @newtype@ over a list carrying the
-- standard 'Semigroup'/'Monoid' instances.
--
-- Laws: concatenation is associative with the empty batch as two-sided identity.
module Moonlight.Algebra.Pure.FreeMonoid
  ( Batch (..),
    singletonBatch,
  )
where

import Data.Kind (Type)

type Batch :: Type -> Type
newtype Batch a = Batch [a]
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
  deriving newtype (Semigroup, Monoid)

singletonBatch :: a -> Batch a
singletonBatch value = Batch [value]

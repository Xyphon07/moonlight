{-# LANGUAGE FunctionalDependencies #-}

-- | Galois connections between ordered types (the 'alpha'/'gamma' adjoint pair), with
-- the ordinal-threshold refinement.
module Moonlight.Category.Pure.Galois
  ( GaloisConnection (..),
    OrdinalGalois (..),
  )
where

import Data.Kind (Constraint, Type)

type GaloisConnection :: Type -> Type -> Constraint
-- | An adjoint pair between ordered carriers.
class (Ord a, Ord b) => GaloisConnection a b | a -> b, b -> a where
  alpha :: a -> b
  gamma :: b -> a

type OrdinalGalois :: Type -> Type -> Constraint
-- | A Galois connection with a finite threshold presentation.
class GaloisConnection a b => OrdinalGalois a b where
  thresholds :: [(a, b)]

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Modules, free modules, vector spaces and bilinear spaces over a ring.
--
-- Laws: scaling distributes over module and ring addition, is compatible with
-- ring multiplication and is unital; a vector space is a module over a field.
-- A 'BilinearSpace' supplies a bilinear form only. It deliberately does not
-- claim conjugation or positivity.
module Moonlight.Algebra.Pure.Module
  ( Module (..),
    FreeModule (..),
    VectorSpace,
    BilinearSpace (..),
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Core (AdditiveGroup, Field, Ring)

type Module :: Type -> Type -> Constraint
class (Ring scalar, AdditiveGroup moduleValue) => Module scalar moduleValue where
  scale :: scalar -> moduleValue -> moduleValue

type FreeModule :: Type -> Type -> Constraint
class Module scalar moduleValue => FreeModule scalar moduleValue where
  type Basis scalar moduleValue :: Type
  support :: moduleValue -> [Basis scalar moduleValue]
  coefficient :: Basis scalar moduleValue -> moduleValue -> scalar
  generator :: Basis scalar moduleValue -> moduleValue

type VectorSpace :: Type -> Type -> Constraint
class (Field scalar, Module scalar vector) => VectorSpace scalar vector

type BilinearSpace :: Type -> Type -> Constraint
class VectorSpace scalar vector => BilinearSpace scalar vector where
  bilinearForm :: vector -> vector -> scalar
  quadraticForm :: vector -> scalar
  quadraticForm vector = bilinearForm vector vector

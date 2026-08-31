{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Polynomial functors as positions with directions, and the parameterized variant.
module Moonlight.Category.Pure.PolynomialFunctor
  ( PolynomialFunctor (..),
    ParameterizedPolynomialFunctor (..),
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Category.Pure.CoveringFamily (Exists)

type PolynomialFunctor :: Type -> Constraint
-- | A polynomial described by positions and the directions at each position.
class PolynomialFunctor polynomial where
  data Position polynomial :: Type -> Type
  type Direction polynomial position :: Type
  allPositions :: [Exists (Position polynomial)]

type ParameterizedPolynomialFunctor :: Type -> Constraint
-- | A polynomial family whose positions and directions vary by parameter.
class ParameterizedPolynomialFunctor polynomial where
  type PolynomialParameter polynomial :: Type
  data ParameterizedPosition polynomial :: Type -> Type
  type ParameterizedDirection polynomial position :: Type
  positionsAt :: PolynomialParameter polynomial -> [Exists (ParameterizedPosition polynomial)]

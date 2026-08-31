module Moonlight.Category.Pure.Simplicial.Delta.Types
  ( DeltaMorphism (..),
  )
where

import Data.Kind (Type)
import Numeric.Natural (Natural)

-- | Internal representation for operational Δ morphisms.
--
-- This module is intentionally hidden from the package surface. Public callers
-- construct values through 'Moonlight.Category.Pure.Simplicial.Delta.mkDeltaMorphism',
-- or by lowering a statically indexed categorical simplex arrow.
type DeltaMorphism :: Type
data DeltaMorphism = DeltaMorphism
  { deltaDomainDimension :: Natural,
    deltaCodomainDimension :: Natural,
    deltaMapValues :: [Natural]
  }
  deriving stock (Eq, Show)

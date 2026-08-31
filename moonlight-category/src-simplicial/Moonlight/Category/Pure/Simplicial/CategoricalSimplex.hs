module Moonlight.Category.Pure.Simplicial.CategoricalSimplex
  ( categoricalSimplexToDeltaMorphism,
    categoricalSimplexValues,
  )
where

import Data.Kind (Type)
import Moonlight.Category.Pure.Indexed.Category qualified as Indexed
import Moonlight.Category.Pure.Indexed.Simplex qualified as Indexed
import Moonlight.Category.Pure.Simplicial.Delta.Types (DeltaMorphism (..))
import Numeric.Natural (Natural)

-- | Lower a statically indexed categorical Δ arrow into the operational
-- runtime-dimensional representation.
--
-- This is total because the arrow already carries a typed monotone map between
-- ordinary non-empty finite ordinals. The operational constructor stays
-- hidden from public callers; this module is the checked package-internal
-- bridge.
categoricalSimplexToDeltaMorphism :: Indexed.Simplex (n :: Type) (m :: Type) -> DeltaMorphism
categoricalSimplexToDeltaMorphism simplexArrow =
  DeltaMorphism
    { deltaDomainDimension = simplexObjectDimension (Indexed.src simplexArrow),
      deltaCodomainDimension = simplexObjectDimension (Indexed.tgt simplexArrow),
      deltaMapValues = categoricalSimplexValues simplexArrow
    }

categoricalSimplexValues :: Indexed.Simplex (n :: Type) (m :: Type) -> [Natural]
categoricalSimplexValues =
  Indexed.simplexValues

-- | Decode the public ordinary ordinal dimension. Public constructor values are
-- non-empty; the empty branch
-- keeps the derived view total instead of manufacturing a partial assertion.
simplexObjectDimension :: Indexed.Obj Indexed.Simplex (n :: Type) -> Natural
simplexObjectDimension objectArrow =
  case Indexed.simplexValues objectArrow of
    [] -> 0
    _ : lowerSimplexValues -> fromIntegral (length lowerSimplexValues)

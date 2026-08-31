{-| The indexed, typed-arrow category-theory layer.

This is the layer adapted from Sjoerd Visscher's @data-category@: indexed categories
and functors, natural transformations, adjunctions, limits and colimits, Kan
extensions, products and coproducts, the unit and empty categories, and the simplex
category. For general indexed category theory, prefer @data-category@ directly; see
@THIRD_PARTY_NOTICES.md@.
-}
module Moonlight.Category.Indexed
  ( module X,
  )
where

import Moonlight.Category.Pure.Indexed.Adjunction as X
import Moonlight.Category.Pure.Indexed.Category as X
import Moonlight.Category.Pure.Indexed.Coproduct as X
import Moonlight.Category.Pure.Indexed.Functor as X
import Moonlight.Category.Pure.Indexed.KanExtension as X
import Moonlight.Category.Pure.Indexed.Limit as X
import Moonlight.Category.Pure.Indexed.NaturalTransformation as X
import Moonlight.Category.Pure.Indexed.Product as X
import Moonlight.Category.Pure.Indexed.Simplex as X
import Moonlight.Category.Pure.Indexed.Unit as X
import Moonlight.Category.Pure.Indexed.Void as X

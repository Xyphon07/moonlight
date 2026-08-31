{-# LANGUAGE TypeFamilyDependencies #-}

-- | The totalised, explicit-error 'Category' class: objects, morphisms, 2-morphisms,
-- compositors and errors as associated types, with 'Either'-returning operations.
module Moonlight.Category.Pure.Category
  ( Category (..),
    composeMor,
  )
where

import Data.Kind (Constraint, Type)

type Category :: Type -> Constraint
-- | A category whose primitive operations totalise structural failure through
-- its associated 'CategoryError'.
class Category c where
  type Ob c = (ob :: Type) | ob -> c
  type Mor c = (mor :: Type) | mor -> c
  type TwoMor c = (twomor :: Type) | twomor -> c
  type TwoMor c = ()
  type Compositor c = (compositor :: Type) | compositor -> c
  type Compositor c = ()
  type CategoryError c :: Type
  type CategoryError c = ()

  identity :: c -> Ob c -> Either (CategoryError c) (Mor c)
  compose :: c -> Mor c -> Mor c -> Either (CategoryError c) (Mor c, Compositor c)
  source :: c -> Mor c -> Either (CategoryError c) (Ob c)
  target :: c -> Mor c -> Either (CategoryError c) (Ob c)

-- | Compose two morphisms while discarding compositor evidence.
composeMor :: forall c. Category c => c -> Mor c -> Mor c -> Either (CategoryError c) (Mor c)
composeMor categoryValue left right = fmap fst (compose @c categoryValue left right)
{-# INLINE composeMor #-}

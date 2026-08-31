{-# LANGUAGE EmptyCase, LambdaCase, TypeOperators, GADTs, TypeFamilies, NoImplicitPrelude #-}
-- | Adapted from data-category-0.11 (BSD-3-Clause), copyright Sjoerd Visscher 2011.
--   See compiler/foundation/moonlight-category/THIRD_PARTY_NOTICES.md.
module Moonlight.Category.Pure.Indexed.Void where

import Data.Kind (Type)
import Data.Type.Equality (type (~))

import Moonlight.Category.Pure.Indexed.Category
import Moonlight.Category.Pure.Indexed.Functor
import Moonlight.Category.Pure.Indexed.NaturalTransformation


data Void a b

magic :: Void a b -> x
magic = \case { }

-- | `Void` is the category with no objects.
instance Category Void where

  src = magic
  tgt = magic

  (.) = magic


voidNat :: (Functor f, Functor g, Dom f ~ Void, Dom g ~ Void, Cod f ~ d, Cod g ~ d)
  => f -> g -> Nat Void d f g
voidNat f g = Nat f g magic


data Magic (k :: Type -> Type -> Type) = Magic
-- | Since there is nothing to map in `Void`, there's a functor from it to any other category.
instance Category k => Functor (Magic k) where
  type Dom (Magic k) = Void
  type Cod (Magic k) = k
  type Magic k :% a = a

  Magic % f = magic f

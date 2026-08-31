{-# LANGUAGE TypeFamilies, TypeOperators, GADTs, FlexibleContexts, NoImplicitPrelude #-}
-- | Adapted from data-category-0.11 (BSD-3-Clause), copyright Sjoerd Visscher 2011.
--   See compiler/foundation/moonlight-category/THIRD_PARTY_NOTICES.md.
module Moonlight.Category.Pure.Indexed.Product where

import Data.Kind (Type)

import Moonlight.Category.Pure.Indexed.Category


data (:**:) :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> Type -> Type -> Type where
  (:**:) :: c1 a1 b1 -> c2 a2 b2 -> (:**:) c1 c2 (a1, a2) (b1, b2)

-- | The product category of categories @c1@ and @c2@.
instance (Category c1, Category c2) => Category (c1 :**: c2) where

  src (a1 :**: a2)            = src a1 :**: src a2
  tgt (a1 :**: a2)            = tgt a1 :**: tgt a2

  (a1 :**: a2) . (b1 :**: b2) = (a1 . b1) :**: (a2 . b2)

{-# LANGUAGE GADTs, NoImplicitPrelude #-}
-- | Adapted from data-category-0.11 (BSD-3-Clause), copyright Sjoerd Visscher 2011.
--   See compiler/foundation/moonlight-category/THIRD_PARTY_NOTICES.md.
module Moonlight.Category.Pure.Indexed.Unit where

import Moonlight.Category.Pure.Indexed.Category


data Unit a b where
  Unit :: Unit () ()

-- | t'Unit' is the category with one object.
instance Category Unit where

  src Unit = Unit
  tgt Unit = Unit

  Unit . Unit = Unit

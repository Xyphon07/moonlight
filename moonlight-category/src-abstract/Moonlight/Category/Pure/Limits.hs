{-# LANGUAGE TypeFamilies #-}

-- | Universal-property classes for (co)limits: products, coproducts, pullbacks,
-- pushouts, equalizers and coequalizers over a 'Category'.
module Moonlight.Category.Pure.Limits
  ( HasProducts (..),
    HasCoproducts (..),
    HasPullbacks (..),
    HasPushouts (..),
    HasEqualizers (..),
    HasCoequalizers (..),
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Category.Pure.Category (Category (..))

type HasProducts :: Type -> Constraint
-- | Categories with chosen binary products and their universal mediators.
class Category c => HasProducts c where
  type ProductOb c :: Type
  productProj1 :: c -> ProductOb c -> Mor c
  productProj2 :: c -> ProductOb c -> Mor c
  productUniversal :: c -> Mor c -> Mor c -> Mor c

type HasCoproducts :: Type -> Constraint
-- | Categories with chosen binary coproducts and their universal mediators.
class Category c => HasCoproducts c where
  type CoproductOb c :: Type
  coproductInj1 :: c -> CoproductOb c -> Mor c
  coproductInj2 :: c -> CoproductOb c -> Mor c
  coproductUniversal :: c -> Mor c -> Mor c -> Mor c

type HasPullbacks :: Type -> Constraint
-- | Categories with partial, explicitly witnessed pullback construction.
class Category c => HasPullbacks c where
  pullback :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c, Mor c)
  pullbackMediator :: c -> Mor c -> Mor c -> Mor c -> Mor c -> Maybe (Mor c)

type HasPushouts :: Type -> Constraint
-- | Categories with partial, explicitly witnessed pushout construction.
class Category c => HasPushouts c where
  pushout :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c, Mor c)

type HasEqualizers :: Type -> Constraint
-- | Categories with partial equalizer construction.
class Category c => HasEqualizers c where
  equalizer :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c)

type HasCoequalizers :: Type -> Constraint
-- | Categories with partial coequalizer construction.
class Category c => HasCoequalizers c where
  coequalizer :: c -> Mor c -> Mor c -> Maybe (Ob c, Mor c)

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

-- | The higher-category tower: 'HigherCategory', 'TwoCategory', 'Bicategory',
-- 'MonoidalCategory' and 'EnrichedCategory'.
module Moonlight.Category.Pure.Higher
  ( HigherCategory (..),
    TwoCategory (..),
    Bicategory (..),
    MonoidalCategory (..),
    EnrichedCategory (..),
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Category.Pure.Category (Category (..))

type HigherCategory :: Type -> Constraint
-- | A category equipped with horizontal and vertical 2-morphism composition.
class Category c => HigherCategory c where
  source2 :: TwoMor c -> Mor c
  target2 :: TwoMor c -> Mor c
  id2 :: Mor c -> TwoMor c
  hCompose :: c -> TwoMor c -> TwoMor c -> Either (CategoryError c) (TwoMor c)
  vCompose :: c -> TwoMor c -> TwoMor c -> Either (CategoryError c) (TwoMor c)
  whiskerLeft :: c -> Mor c -> TwoMor c -> Either (CategoryError c) (TwoMor c)
  whiskerLeft categoryValue morphism
    = hCompose categoryValue (id2 morphism)
  whiskerRight :: c -> TwoMor c -> Mor c -> Either (CategoryError c) (TwoMor c)
  whiskerRight categoryValue twoMorphism morphism = hCompose categoryValue twoMorphism (id2 morphism)
  compositor :: c -> Mor c -> Mor c -> Mor c -> Compositor c

type TwoCategory :: Type -> Constraint
-- | A higher category whose 2-morphisms admit inverses.
class HigherCategory c => TwoCategory c where
  inverse2 :: c -> TwoMor c -> Either (CategoryError c) (TwoMor c)

type Bicategory :: Type -> Constraint
-- | A higher category with explicit unitor and associator witnesses.
class HigherCategory c => Bicategory c where
  leftUnitor :: c -> Mor c -> Compositor c
  rightUnitor :: c -> Mor c -> Compositor c
  associator :: c -> Mor c -> Mor c -> Mor c -> Compositor c

type MonoidalCategory :: Type -> Constraint
-- | A category with tensor product, unit, and coherence witnesses.
class Category v => MonoidalCategory v where
  tensorOb :: Ob v -> Ob v -> Ob v
  tensorMor :: v -> Mor v -> Mor v -> Either (CategoryError v) (Mor v, Compositor v)
  unitOb :: Ob v
  associatorV :: Ob v -> Ob v -> Ob v -> Compositor v
  leftUnitorV :: Ob v -> Compositor v
  rightUnitorV :: Ob v -> Compositor v

type EnrichedCategory :: Type -> Type -> Constraint
-- | A category whose hom-objects and composition live in a monoidal category.
class (Category c, MonoidalCategory v) => EnrichedCategory c v | c -> v where
  enrichHom :: Ob c -> Ob c -> Ob v
  enrichIdentity :: Ob c -> Mor v
  enrichCompose :: Ob c -> Ob c -> Ob c -> Mor v

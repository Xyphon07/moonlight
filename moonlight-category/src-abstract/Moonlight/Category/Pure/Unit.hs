{-# LANGUAGE DerivingStrategies #-}

-- | The one-object, identity-only category: the smallest lawful 'Category' carrier.
module Moonlight.Category.Pure.Unit
  ( UnitCat (..),
    UnitObj (..),
    UnitMor (..),
    UnitTwoMor (..),
    UnitCompositor (..),
  )
where

import Data.Kind (Type)
import Moonlight.Category.Pure.Adhesive
  ( AdhesiveCategory (..),
    MonicMatchComponents (..),
    PBPOAdhesiveCategory,
    PushoutComplementComponents (..),
  )
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Higher (Bicategory (..), EnrichedCategory (..), HigherCategory (..), MonoidalCategory (..), TwoCategory (..))
import Moonlight.Category.Pure.Limits (HasCoequalizers (..), HasCoproducts (..), HasEqualizers (..), HasProducts (..), HasPullbacks (..), HasPushouts (..))

type UnitCat :: Type
-- | The terminal one-object category.
data UnitCat = UnitCat
  deriving stock (Eq, Show)

type UnitObj :: Type
-- | The sole object of t'UnitCat'.
data UnitObj = UnitObj
  deriving stock (Eq, Show, Enum, Bounded)

type UnitMor :: Type
-- | The sole 1-morphism of t'UnitCat'.
data UnitMor = UnitMor
  deriving stock (Eq, Show, Enum, Bounded)

type UnitTwoMor :: Type
-- | A 2-morphism between the unique 1-morphism endpoints.
data UnitTwoMor = UnitTwoMor
  { unitTwoSource :: UnitMor,
    unitTwoTarget :: UnitMor
  }
  deriving stock (Eq, Show)

type UnitCompositor :: Type
-- | Coherence evidence for the strict unit category.
data UnitCompositor
  = UnitStrictCompositor
  | UnitAssociator UnitMor UnitMor UnitMor
  | UnitLeftUnitor UnitMor
  | UnitRightUnitor UnitMor
  deriving stock (Eq, Show)

instance Category UnitCat where
  type Ob UnitCat = UnitObj
  type Mor UnitCat = UnitMor
  type TwoMor UnitCat = UnitTwoMor
  type Compositor UnitCat = UnitCompositor

  identity _ _ = Right UnitMor

  compose _ _ _ = Right (UnitMor, UnitStrictCompositor)

  source _ _ = Right UnitObj
  target _ _ = Right UnitObj

instance HigherCategory UnitCat where
  source2 = unitTwoSource
  target2 = unitTwoTarget
  id2 morphism = UnitTwoMor morphism morphism
  hCompose _ left right =
    if unitTwoTarget right == unitTwoSource left
      then Right (UnitTwoMor (unitTwoSource right) (unitTwoTarget left))
      else Left ()
  vCompose _ left right =
    if unitTwoTarget right == unitTwoSource left
      then Right (UnitTwoMor (unitTwoSource right) (unitTwoTarget left))
      else Left ()
  compositor _ = UnitAssociator

instance TwoCategory UnitCat where
  inverse2 _ twoMorphism = Right (UnitTwoMor (unitTwoTarget twoMorphism) (unitTwoSource twoMorphism))

instance Bicategory UnitCat where
  leftUnitor _ = UnitLeftUnitor
  rightUnitor _ = UnitRightUnitor
  associator _ = UnitAssociator

instance MonoidalCategory UnitCat where
  tensorOb _ _ = UnitObj
  tensorMor _ _ _ = Right (UnitMor, UnitStrictCompositor)
  unitOb = UnitObj
  associatorV _ _ _ = UnitAssociator UnitMor UnitMor UnitMor
  leftUnitorV _ = UnitLeftUnitor UnitMor
  rightUnitorV _ = UnitRightUnitor UnitMor

instance EnrichedCategory UnitCat UnitCat where
  enrichHom _ _ = UnitObj
  enrichIdentity _ = UnitMor
  enrichCompose _ _ _ = UnitMor

instance HasProducts UnitCat where
  type ProductOb UnitCat = UnitObj
  productProj1 _ _ = UnitMor
  productProj2 _ _ = UnitMor
  productUniversal _ _ _ = UnitMor

instance HasCoproducts UnitCat where
  type CoproductOb UnitCat = UnitObj
  coproductInj1 _ _ = UnitMor
  coproductInj2 _ _ = UnitMor
  coproductUniversal _ _ _ = UnitMor

instance HasPullbacks UnitCat where
  pullback _ _ _ = Just (UnitObj, UnitMor, UnitMor)
  pullbackMediator _ _ _ _ _ = Just UnitMor

instance HasPushouts UnitCat where
  pushout _ _ _ = Just (UnitObj, UnitMor, UnitMor)

instance HasEqualizers UnitCat where
  equalizer _ _ _ = Just (UnitObj, UnitMor)

instance HasCoequalizers UnitCat where
  coequalizer _ _ _ = Just (UnitObj, UnitMor)

instance AdhesiveCategory UnitCat where
  monicMatchComponents _ _ =
    Just (MonicMatchComponents UnitMor)

  pushoutComplementComponents _ _ _ =
    Just
      PushoutComplementComponents
        { pushoutComplementComponentObject = UnitObj,
          pushoutComplementComponentBorrowedLeg = UnitMor,
          pushoutComplementComponentResidualLeg = UnitMor
        }

instance PBPOAdhesiveCategory UnitCat

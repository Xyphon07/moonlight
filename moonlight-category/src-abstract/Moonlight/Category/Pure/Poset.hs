{-# LANGUAGE DerivingStrategies #-}

-- | Posets viewed as thin categories: a morphism exists exactly where the
-- order relation holds.
module Moonlight.Category.Pure.Poset
  ( PosetCat (..),
    PosetOb (..),
    PosetMor,
    PosetTwoMor (..),
    PosetCompositor (..),
    mkPosetMor,
    posetSource,
    posetTarget,
    OrdinalLower (..),
    OrdinalUpper (..),
    LowerPosetCat,
    UpperPosetCat,
    LowerMor,
    UpperMor,
    mkLowerMor,
    mkUpperMor,
  )
where

import Data.Kind (Type)
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Galois (GaloisConnection (..), OrdinalGalois (..))
import Moonlight.Category.Pure.Thin
  ( ThinMorphism,
    identityThinMorphism,
    mkThinMorphismBy,
    thinMorphismSource,
    thinMorphismTarget,
  )

type PosetCat :: Type -> Type
-- | The thin category induced by an ordered carrier.
data PosetCat a = PosetCat
  deriving stock (Eq, Show)

type PosetOb :: Type -> Type
-- | An object in a poset category.
newtype PosetOb a = PosetOb {unPosetOb :: a}
  deriving stock (Eq, Ord, Show)

type PosetMor :: Type -> Type
-- | Order evidence from a source value to a target value.
data PosetMor a = PosetMor a a
  deriving stock (Eq, Show)

type PosetTwoMor :: Type -> Type
-- | The unique 2-morphism witness in a poset category.
data PosetTwoMor (a :: Type) = PosetTwoMor
  deriving stock (Eq, Show)

type PosetCompositor :: Type -> Type
-- | Composition evidence for poset morphisms.
data PosetCompositor (a :: Type) = PosetCompositor
  deriving stock (Eq, Show)

-- | Admit a poset morphism exactly when the source is below the target.
mkPosetMor :: Ord a => a -> a -> Maybe (PosetMor a)
mkPosetMor sourceValue targetValue =
  fromThinMorphism <$> mkThinMorphismBy (<=) sourceValue targetValue

-- | Project the source value.
posetSource :: PosetMor a -> a
posetSource (PosetMor sourceValue _) = sourceValue

-- | Project the target value.
posetTarget :: PosetMor a -> a
posetTarget (PosetMor _ targetValue) = targetValue

instance Ord a => Category (PosetCat a) where
  type Ob (PosetCat a) = PosetOb a
  type Mor (PosetCat a) = PosetMor a
  type TwoMor (PosetCat a) = PosetTwoMor a
  type Compositor (PosetCat a) = PosetCompositor a

  identity _ (PosetOb objectValue) =
    Right (fromThinMorphism (identityThinMorphism objectValue))

  compose _ leftMorphism rightMorphism
    | posetTarget rightMorphism /= posetSource leftMorphism = Left ()
    | otherwise =
        case mkPosetMor (posetSource rightMorphism) (posetTarget leftMorphism) of
          Just composedMorphism -> Right (composedMorphism, PosetCompositor)
          Nothing -> Left ()

  source _ = Right . PosetOb . posetSource
  target _ = Right . PosetOb . posetTarget

type OrdinalLower :: Type
-- | The lower carrier of the example ordinal Galois connection.
newtype OrdinalLower = OrdinalLower {unOrdinalLower :: Int}
  deriving stock (Eq, Ord, Show)

type OrdinalUpper :: Type
-- | The upper carrier of the example ordinal Galois connection.
newtype OrdinalUpper = OrdinalUpper {unOrdinalUpper :: Int}
  deriving stock (Eq, Ord, Show)

instance GaloisConnection OrdinalLower OrdinalUpper where
  alpha (OrdinalLower value) = OrdinalUpper (value * 2)
  gamma (OrdinalUpper value) = OrdinalLower (value `div` 2)

instance OrdinalGalois OrdinalLower OrdinalUpper where
  thresholds = map (\value -> (OrdinalLower value, OrdinalUpper (value * 2))) [0 .. 32]

type LowerPosetCat :: Type
-- | The poset category over t'OrdinalLower'.
type LowerPosetCat = PosetCat OrdinalLower

type UpperPosetCat :: Type
-- | The poset category over t'OrdinalUpper'.
type UpperPosetCat = PosetCat OrdinalUpper

type LowerMor :: Type
-- | A morphism in 'LowerPosetCat'.
type LowerMor = PosetMor OrdinalLower

type UpperMor :: Type
-- | A morphism in 'UpperPosetCat'.
type UpperMor = PosetMor OrdinalUpper

-- | Construct a lower-carrier order witness.
mkLowerMor :: OrdinalLower -> OrdinalLower -> Maybe LowerMor
mkLowerMor = mkPosetMor

-- | Construct an upper-carrier order witness.
mkUpperMor :: OrdinalUpper -> OrdinalUpper -> Maybe UpperMor
mkUpperMor = mkPosetMor


fromThinMorphism :: ThinMorphism a -> PosetMor a
fromThinMorphism thinMorphism =
  PosetMor
    (thinMorphismSource thinMorphism)
    (thinMorphismTarget thinMorphism)

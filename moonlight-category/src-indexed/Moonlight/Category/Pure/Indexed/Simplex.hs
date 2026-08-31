{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Adapted from data-category-0.11 (BSD-3-Clause), copyright Sjoerd Visscher 2011.
--   See compiler/foundation/moonlight-category/THIRD_PARTY_NOTICES.md.
--
-- The ordinary simplex category, presented as the non-empty slice of the
-- augmented simplex category from data-category.
module Moonlight.Category.Pure.Indexed.Simplex
  ( -- * Ordinary simplex category
    Simplex,
    Z,
    S,
    simplexZero,
    simplexSucc,
    simplexFirstVertex,
    simplexWeakenCodomain,
    simplexExtendDomain,
    simplexCollapse,
    simplexValues,
    cofaceFirst,
    cofaceLast,
    cofaceSucc,
    codegeneracyFirst,
    codegeneracyLast,
    codegeneracySucc,

    -- * Finite ordinal elements
    Fin (..),
    SimplexFin,
    finToNatural,

    -- * Functor to Hask
    ForgetSimplex (..),

    -- * Simplicial-set aliases
    SSet,
    StandardSimplex,
  )
where

import Numeric.Natural (Natural)
import Prelude (Bool (..), Eq (..), Show (..), id, map, (+), (++))

import Moonlight.Category.Pure.Indexed.Category (Category (..), Obj)
import Moonlight.Category.Pure.Indexed.Functor (Functor (..), (:-*:))
import Moonlight.Category.Pure.Indexed.NaturalTransformation (Presheaves)

-- | Zero in the public ordinary simplex index. Public @Z@ denotes the standard
-- simplex object @[0]@ through t'Simplex', not the hidden augmented empty ordinal.
data Z

-- | Successor in the public ordinary simplex index. Public @S Z@ denotes @[1]@.
data S n

-- | The augmented simplex category. Its object @AugmentedZ@ is the empty finite
-- ordinal; public t'Simplex' shifts both endpoints by 'S' so that the empty
-- ordinal cannot appear at the public boundary.
data AugmentedSimplex x y where
  AugmentedZ :: AugmentedSimplex Z Z
  AugmentedY :: AugmentedSimplex x y -> AugmentedSimplex x (S y)
  AugmentedX :: AugmentedSimplex x (S y) -> AugmentedSimplex (S x) (S y)

instance Eq (AugmentedSimplex a b) where
  AugmentedZ == AugmentedZ = True
  AugmentedY left == AugmentedY right = left == right
  AugmentedX left == AugmentedX right = left == right
  _ == _ = False

instance Show (AugmentedSimplex a b) where
  show AugmentedZ = "AugmentedZ"
  show (AugmentedY arrow) = "AugmentedY (" ++ show arrow ++ ")"
  show (AugmentedX arrow) = "AugmentedX (" ++ show arrow ++ ")"

augmentedSucc :: Obj AugmentedSimplex n -> Obj AugmentedSimplex (S n)
augmentedSucc = AugmentedX . AugmentedY

-- | The augmented simplex category is the category of finite ordinals and
-- order-preserving maps, including the empty ordinal.
instance Category AugmentedSimplex where
  src AugmentedZ = AugmentedZ
  src (AugmentedY arrow) = src arrow
  src (AugmentedX arrow) = augmentedSucc (src arrow)

  tgt AugmentedZ = AugmentedZ
  tgt (AugmentedY arrow) = augmentedSucc (tgt arrow)
  tgt (AugmentedX arrow) = tgt arrow

  AugmentedZ . arrow = arrow
  arrow . AugmentedZ = arrow
  AugmentedY left . right = AugmentedY (left . right)
  AugmentedX left . AugmentedY right = left . right
  AugmentedX left . AugmentedX right = AugmentedX (AugmentedX left . right)

-- | Ordinary simplex category Δ. Object @n@ denotes the non-empty finite ordinal
-- @[n]@, represented internally by the augmented object @S n@.
newtype Simplex a b = Simplex (AugmentedSimplex (S a) (S b))

instance Eq (Simplex a b) where
  Simplex left == Simplex right = left == right

instance Show (Simplex a b) where
  show (Simplex arrow) = "Simplex (" ++ show arrow ++ ")"

-- | The ordinary simplex category is the full non-empty subcategory of the
-- augmented simplex category.
instance Category Simplex where
  src (Simplex arrow) = Simplex (src arrow)
  tgt (Simplex arrow) = Simplex (tgt arrow)

  Simplex left . Simplex right = Simplex (left . right)

-- | The identity arrow on @[0]@.
simplexZero :: Obj Simplex Z
simplexZero = Simplex (augmentedSucc AugmentedZ)

-- | Given the identity arrow on @[n]@, construct the identity arrow on @[n+1]@.
simplexSucc :: Obj Simplex n -> Obj Simplex (S n)
simplexSucc objectArrow =
  case canonicalSimplexObject objectArrow of
    Simplex canonicalObject -> Simplex (augmentedSucc canonicalObject)

-- | The first vertex inclusion @[0] -> [n]@.
simplexFirstVertex :: Obj Simplex n -> Simplex Z n
simplexFirstVertex objectArrow =
  case canonicalSimplexObject objectArrow of
    Simplex canonicalObject -> Simplex (AugmentedX (augmentedInitial canonicalObject))

-- | Shift a map into the upper face of the codomain.
simplexWeakenCodomain :: Simplex a b -> Simplex a (S b)
simplexWeakenCodomain (Simplex arrow) = Simplex (AugmentedY arrow)

-- | Extend a map by sending the new least domain element to the least codomain
-- element and shifting the previous domain through the supplied map.
simplexExtendDomain :: Simplex a (S b) -> Simplex (S a) (S b)
simplexExtendDomain (Simplex arrow) = Simplex (AugmentedX arrow)

-- | The unique monotone map @[n] -> [0]@.
simplexCollapse :: Obj Simplex n -> Simplex n Z
simplexCollapse objectArrow =
  case canonicalSimplexObject objectArrow of
    Simplex canonicalObject -> Simplex (augmentedTerminalObject canonicalObject)

-- | Decode a simplex arrow as the monotone list of target ordinal values.
simplexValues :: Simplex a b -> [Natural]
simplexValues (Simplex arrow) =
  map (finToNatural . augmentedForget arrow) (augmentedFinElements (src arrow))

-- | The first coface @δ₀ : [n] -> [n+1]@, skipping the least codomain value.
cofaceFirst :: Obj Simplex n -> Simplex n (S n)
cofaceFirst objectArrow =
  simplexWeakenCodomain (canonicalSimplexObject objectArrow)

-- | The last coface @δₙ₊₁ : [n] -> [n+1]@, skipping the greatest codomain value.
cofaceLast :: Obj Simplex n -> Simplex n (S n)
cofaceLast objectArrow =
  case canonicalSimplexObject objectArrow of
    Simplex canonicalObject -> Simplex (augmentedPreserveValuesCodomainSucc canonicalObject)

-- | Shift @δᵢ@ to @δᵢ₊₁@ by adjoining a new least endpoint.
cofaceSucc :: Simplex n (S n) -> Simplex (S n) (S (S n))
cofaceSucc cofaceArrow =
  simplexExtendDomain (simplexWeakenCodomain cofaceArrow)

-- | The first codegeneracy @σ₀ : [n+1] -> [n]@, identifying the first pair.
codegeneracyFirst :: Obj Simplex n -> Simplex (S n) n
codegeneracyFirst objectArrow =
  case canonicalSimplexObject objectArrow of
    Simplex canonicalObject -> Simplex (AugmentedX canonicalObject)

-- | The last codegeneracy @σₙ : [n+1] -> [n]@, identifying the last pair.
codegeneracyLast :: Obj Simplex n -> Simplex (S n) n
codegeneracyLast objectArrow =
  case canonicalSimplexObject objectArrow of
    Simplex canonicalObject -> Simplex (augmentedDuplicateLastDomain canonicalObject)

-- | Shift @σᵢ@ to @σᵢ₊₁@ by adjoining a new least endpoint.
codegeneracySucc :: Simplex (S n) n -> Simplex (S (S n)) (S n)
codegeneracySucc codegeneracyArrow =
  simplexExtendDomain (simplexWeakenCodomain codegeneracyArrow)

-- | Elements of a finite ordinal.
data Fin n where
  Fz :: Fin (S n)
  Fs :: Fin n -> Fin (S n)

instance Eq (Fin n) where
  Fz == Fz = True
  Fs left == Fs right = left == right
  _ == _ = False

instance Show (Fin n) where
  show Fz = "Fz"
  show (Fs value) = "Fs (" ++ show value ++ ")"

-- | Elements of the public ordinary simplex object @[n]@.
type SimplexFin n = Fin (S n)

finToNatural :: Fin n -> Natural
finToNatural Fz = 0
finToNatural (Fs value) = 1 + finToNatural value

data ForgetSimplex = ForgetSimplex

-- | Forget an ordinary simplex arrow to its monotone function between finite
-- ordinal element types.
instance Functor ForgetSimplex where
  type Dom ForgetSimplex = Simplex
  type Cod ForgetSimplex = (->)
  type ForgetSimplex :% n = SimplexFin n

  ForgetSimplex % Simplex arrow = augmentedForget arrow

-- | Simplicial sets as presheaves on the ordinary simplex category.
type SSet = Presheaves Simplex

-- | The representable standard simplex @Δ[n] = Hom(-, [n])@.
type StandardSimplex n = Simplex :-*: n

canonicalSimplexObject :: Obj Simplex n -> Obj Simplex n
canonicalSimplexObject = src

augmentedForget :: AugmentedSimplex x y -> Fin x -> Fin y
augmentedForget AugmentedZ = id
augmentedForget (AugmentedY arrow) = Fs . augmentedForget arrow
augmentedForget (AugmentedX arrow) = \case
  Fz -> Fz
  Fs value -> augmentedForget arrow value

augmentedPreserveValuesCodomainSucc :: AugmentedSimplex x y -> AugmentedSimplex x (S y)
augmentedPreserveValuesCodomainSucc AugmentedZ = AugmentedY AugmentedZ
augmentedPreserveValuesCodomainSucc (AugmentedY arrow) = AugmentedY (augmentedPreserveValuesCodomainSucc arrow)
augmentedPreserveValuesCodomainSucc (AugmentedX arrow) = AugmentedX (augmentedPreserveValuesCodomainSucc arrow)

augmentedDuplicateLastDomain :: AugmentedSimplex x (S y) -> AugmentedSimplex (S x) (S y)
augmentedDuplicateLastDomain (AugmentedX arrow) = AugmentedX (augmentedDuplicateLastDomain arrow)
augmentedDuplicateLastDomain (AugmentedY AugmentedZ) = AugmentedX (AugmentedY AugmentedZ)
augmentedDuplicateLastDomain (AugmentedY (AugmentedY arrow)) =
  AugmentedY (augmentedDuplicateLastDomain (AugmentedY arrow))
augmentedDuplicateLastDomain (AugmentedY (AugmentedX arrow)) =
  AugmentedY (augmentedDuplicateLastDomain (AugmentedX arrow))

augmentedInitial :: Obj AugmentedSimplex n -> AugmentedSimplex Z n
augmentedInitial AugmentedZ = AugmentedZ
augmentedInitial (AugmentedX (AugmentedY objectArrow)) = AugmentedY (augmentedInitial objectArrow)
augmentedInitial (AugmentedY arrow) = AugmentedY (augmentedInitial (tgt arrow))
augmentedInitial (AugmentedX arrow) = augmentedInitial (tgt arrow)

augmentedTerminalObject :: Obj AugmentedSimplex n -> AugmentedSimplex n (S Z)
augmentedTerminalObject AugmentedZ = AugmentedY AugmentedZ
augmentedTerminalObject (AugmentedY arrow) = augmentedTerminalObject (src arrow)
augmentedTerminalObject (AugmentedX arrow) = AugmentedX (augmentedTerminalObject (src arrow))

augmentedFinElements :: Obj AugmentedSimplex n -> [Fin n]
augmentedFinElements AugmentedZ = []
augmentedFinElements (AugmentedY arrow) = augmentedFinElements (src arrow)
augmentedFinElements (AugmentedX arrow) = Fz : map Fs (augmentedFinElements (src arrow))

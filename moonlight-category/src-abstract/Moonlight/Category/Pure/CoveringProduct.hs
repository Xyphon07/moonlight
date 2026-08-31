{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | Dependent products over a covering family: a total function from each family
-- member to its fibre, with tabulation, indexing, restriction and mapping.
module Moonlight.Category.Pure.CoveringProduct
  ( CoveringProduct,
    tabulateCoveringProduct,
    indexCoveringProduct,
    restrictCoveringProduct,
    adjustCoveringProduct,
    replaceCoveringProduct,
    mapCoveringProduct,
    mapCoveringProductWithWitness,
    foldMapCoveringProductWithWitness,
  )
where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Moonlight.Category.Pure.CoveringFamily
  ( CoveringFamily (..),
    Exists (..),
  )

type CoveringProduct :: forall k. (k -> Type) -> (k -> Type) -> Type
-- | A total dependent product indexed by the members of a covering family.
newtype CoveringProduct (w :: k -> Type) (f :: k -> Type) = CoveringProduct
  { -- | Project the fibre selected by a witness.
    indexCoveringProduct :: forall member. w member -> f member
  }

-- | Build a dependent product from its total projection.
tabulateCoveringProduct ::
  (forall member. w member -> f member) ->
  CoveringProduct w f
tabulateCoveringProduct = CoveringProduct

-- | Reindex a product along a witness embedding.
restrictCoveringProduct ::
  (forall member. subset member -> superset member) ->
  CoveringProduct superset f ->
  CoveringProduct subset f
restrictCoveringProduct embedWitness coveringProduct =
  tabulateCoveringProduct
    (\witness -> indexCoveringProduct coveringProduct (embedWitness witness))

-- | Modify the fibre at one witness selected by decidable witness equality.
adjustCoveringProduct ::
  (forall left right. w left -> w right -> Maybe (left :~: right)) ->
  w member ->
  (f member -> f member) ->
  CoveringProduct w f ->
  CoveringProduct w f
adjustCoveringProduct sameWitness targetWitness adjustValue coveringProduct =
  tabulateCoveringProduct
    ( \witness ->
        case sameWitness witness targetWitness of
          Just Refl -> adjustValue (indexCoveringProduct coveringProduct witness)
          Nothing -> indexCoveringProduct coveringProduct witness
    )

-- | Replace the fibre at one witness.
replaceCoveringProduct ::
  (forall left right. w left -> w right -> Maybe (left :~: right)) ->
  w member ->
  f member ->
  CoveringProduct w f ->
  CoveringProduct w f
replaceCoveringProduct sameWitness targetWitness replacement =
  adjustCoveringProduct sameWitness targetWitness (const replacement)

-- | Apply a natural transformation to every fibre.
mapCoveringProduct ::
  (forall member. f member -> g member) ->
  CoveringProduct w f ->
  CoveringProduct w g
mapCoveringProduct transform =
  mapCoveringProductWithWitness (\_ -> transform)

-- | Apply a witness-aware natural transformation to every fibre.
mapCoveringProductWithWitness ::
  (forall member. w member -> f member -> g member) ->
  CoveringProduct w f ->
  CoveringProduct w g
mapCoveringProductWithWitness transform (CoveringProduct productAt) =
  CoveringProduct (\witness -> transform witness (productAt witness))

-- | Fold all fibres in covering order with their witnesses.
foldMapCoveringProductWithWitness ::
  forall k (w :: k -> Type) (f :: k -> Type) monoidValue.
  (CoveringFamily w, Monoid monoidValue) =>
  (forall member. w member -> f member -> monoidValue) ->
  CoveringProduct w f ->
  monoidValue
foldMapCoveringProductWithWitness foldValue coveringProduct =
  foldMap
    (\(Exists witness) -> foldValue witness (indexCoveringProduct coveringProduct witness))
    (allMembers @k @w)

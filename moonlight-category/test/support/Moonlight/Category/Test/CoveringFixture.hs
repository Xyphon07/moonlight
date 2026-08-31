{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Moonlight.Category.Test.CoveringFixture
  ( DemoField (..),
    DemoFieldWitness (..),
    DemoSubsetWitness (..),
    embedDemoSubsetWitness,
    sameDemoFieldWitness,
  )
where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Moonlight.Category.Pure.CoveringFamily (CoveringFamily (..), Exists (..))

type DemoField :: Type
data DemoField
  = AlphaField
  | BetaField
  | GammaField

type DemoFieldWitness :: DemoField -> Type
data DemoFieldWitness field where
  AlphaFieldWitness :: DemoFieldWitness 'AlphaField
  BetaFieldWitness :: DemoFieldWitness 'BetaField
  GammaFieldWitness :: DemoFieldWitness 'GammaField

type DemoSubsetWitness :: DemoField -> Type
data DemoSubsetWitness field where
  AlphaSubsetWitness :: DemoSubsetWitness 'AlphaField
  GammaSubsetWitness :: DemoSubsetWitness 'GammaField

instance CoveringFamily DemoFieldWitness where
  allMembers =
    [ Exists AlphaFieldWitness,
      Exists BetaFieldWitness,
      Exists GammaFieldWitness
    ]

instance CoveringFamily DemoSubsetWitness where
  allMembers =
    [ Exists AlphaSubsetWitness,
      Exists GammaSubsetWitness
    ]

sameDemoFieldWitness ::
  DemoFieldWitness left ->
  DemoFieldWitness right ->
  Maybe (left :~: right)
sameDemoFieldWitness leftWitness rightWitness =
  case (leftWitness, rightWitness) of
    (AlphaFieldWitness, AlphaFieldWitness) -> Just Refl
    (BetaFieldWitness, BetaFieldWitness) -> Just Refl
    (GammaFieldWitness, GammaFieldWitness) -> Just Refl
    _ -> Nothing

embedDemoSubsetWitness ::
  DemoSubsetWitness field ->
  DemoFieldWitness field
embedDemoSubsetWitness subsetWitness =
  case subsetWitness of
    AlphaSubsetWitness -> AlphaFieldWitness
    GammaSubsetWitness -> GammaFieldWitness

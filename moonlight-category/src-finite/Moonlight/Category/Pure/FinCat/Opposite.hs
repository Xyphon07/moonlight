{-# LANGUAGE DerivingStrategies #-}

-- | Opposite finite categories: 'FinCat' with sources and targets reversed.
module Moonlight.Category.Pure.FinCat.Opposite
  ( OppositeFinCat (..),
    OppositeFinObj (..),
    OppositeFinMor (..),
    OppositeFinTwoMor (..),
    OppositeFinCompositor (..),
  )
where

import Data.Kind (Type)
import Moonlight.Category.Pure.Category (Category (..), Compositor, Mor, Ob, TwoMor)
import Moonlight.Category.Pure.FinCat (FinCat, FinCatError)

type OppositeFinCat :: Type
newtype OppositeFinCat = OppositeFinCat {oppositeFinCatSource :: FinCat}
  deriving stock (Eq, Show)

type OppositeFinObj :: Type
newtype OppositeFinObj = OppositeFinObj {unwrapOppositeFinObj :: Ob FinCat}
  deriving stock (Eq, Show)

type OppositeFinMor :: Type
newtype OppositeFinMor = OppositeFinMor {unwrapOppositeFinMor :: Mor FinCat}
  deriving stock (Eq, Show)

type OppositeFinTwoMor :: Type
newtype OppositeFinTwoMor = OppositeFinTwoMor {unwrapOppositeFinTwoMor :: TwoMor FinCat}
  deriving stock (Eq, Show)

type OppositeFinCompositor :: Type
newtype OppositeFinCompositor = OppositeFinCompositor {unwrapOppositeFinCompositor :: Compositor FinCat}
  deriving stock (Eq, Show)

instance Category OppositeFinCat where
  type Ob OppositeFinCat = OppositeFinObj
  type Mor OppositeFinCat = OppositeFinMor
  type TwoMor OppositeFinCat = OppositeFinTwoMor
  type Compositor OppositeFinCat = OppositeFinCompositor
  type CategoryError OppositeFinCat = FinCatError

  identity (OppositeFinCat categoryValue) (OppositeFinObj objectValue) =
    OppositeFinMor <$> identity @FinCat categoryValue objectValue

  compose (OppositeFinCat categoryValue) (OppositeFinMor left) (OppositeFinMor right) = do
    (composed, coherence) <- compose @FinCat categoryValue right left
    pure (OppositeFinMor composed, OppositeFinCompositor coherence)

  source (OppositeFinCat categoryValue) (OppositeFinMor morphism) =
    OppositeFinObj <$> target @FinCat categoryValue morphism

  target (OppositeFinCat categoryValue) (OppositeFinMor morphism) =
    OppositeFinObj <$> source @FinCat categoryValue morphism

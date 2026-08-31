{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Builds the t'CategoryLaws' record for a carrier.
module Moonlight.Category.Effect.Harness.Category
  ( mkCategoryLaws,
  )
where

import Moonlight.Category.Effect.Harness.Core
  ( CategoryLaws (..),
    composeC,
    identityC,
    sourceC,
    targetC,
  )
import Moonlight.Category.Pure.Category (Category (..))

mkCategoryLaws :: forall c. (Category c, Eq (Mor c), Eq (Ob c)) => c -> CategoryLaws c
mkCategoryLaws categoryValue =
  CategoryLaws
    { categoryLeftIdentity = categoryLeftIdentityLaw @c categoryValue,
      categoryRightIdentity = categoryRightIdentityLaw @c categoryValue,
      categoryAssociativity = categoryAssociativityLaw @c categoryValue
    }

categoryLeftIdentityLaw :: forall c. (Category c, Eq (Mor c)) => c -> Mor c -> Bool
categoryLeftIdentityLaw categoryValue morphism =
  case do
    targetObject <- targetC @c categoryValue morphism
    identityMorphism <- identityC @c categoryValue targetObject
    composeC @c categoryValue identityMorphism morphism
    of
      Right composed -> composed == morphism
      Left _ -> False

categoryRightIdentityLaw :: forall c. (Category c, Eq (Mor c)) => c -> Mor c -> Bool
categoryRightIdentityLaw categoryValue morphism =
  case do
    sourceObject <- sourceC @c categoryValue morphism
    identityMorphism <- identityC @c categoryValue sourceObject
    composeC @c categoryValue morphism identityMorphism
    of
      Right composed -> composed == morphism
      Left _ -> False

categoryAssociativityLaw :: forall c. (Category c, Eq (Mor c), Eq (Ob c)) => c -> Mor c -> Mor c -> Mor c -> Bool
categoryAssociativityLaw categoryValue first second third =
  case do
    firstTarget <- targetC @c categoryValue first
    secondSource <- sourceC @c categoryValue second
    secondTarget <- targetC @c categoryValue second
    thirdSource <- sourceC @c categoryValue third
    pure (firstTarget == secondSource && secondTarget == thirdSource)
    of
      Left _ -> False
      Right False -> True
      Right True ->
        rightValuesEqual
          (composeC @c categoryValue third second >>= (\composed -> composeC @c categoryValue composed first))
          (composeC @c categoryValue second first >>= composeC @c categoryValue third)

rightValuesEqual :: Eq value => Either left value -> Either right value -> Bool
rightValuesEqual left right =
  case (left, right) of
    (Right leftValue, Right rightValue) -> leftValue == rightValue
    _ -> False

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Executable checks for limit and colimit universal-property laws.
module Moonlight.Category.Effect.Harness.Limits
  ( productProjection1,
    productProjection2,
    coproductInjection1,
    coproductInjection2,
    pullbackCommutative,
    pushoutCommutative,
    equalizerCommutative,
    coequalizerCommutative,
  )
where

import Moonlight.Category.Effect.Harness.Core (composeC, sourceC, targetC)
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Limits
  ( HasCoequalizers (..),
    HasCoproducts (..),
    HasEqualizers (..),
    HasProducts (..),
    HasPullbacks (..),
    HasPushouts (..),
  )
import Prelude hiding (Functor)

productProjection1 :: forall c. (HasProducts c, Eq (Mor c)) => c -> ProductOb c -> Mor c -> Mor c -> Bool
productProjection1 categoryValue productObject first second =
  rightEquals (composeC @c categoryValue (productProj1 @c categoryValue productObject) (productUniversal @c categoryValue first second)) first

productProjection2 :: forall c. (HasProducts c, Eq (Mor c)) => c -> ProductOb c -> Mor c -> Mor c -> Bool
productProjection2 categoryValue productObject first second =
  rightEquals (composeC @c categoryValue (productProj2 @c categoryValue productObject) (productUniversal @c categoryValue first second)) second

coproductInjection1 :: forall c. (HasCoproducts c, Eq (Mor c)) => c -> CoproductOb c -> Mor c -> Mor c -> Bool
coproductInjection1 categoryValue coproductObject first second =
  rightEquals (composeC @c categoryValue (coproductUniversal @c categoryValue first second) (coproductInj1 @c categoryValue coproductObject)) first

coproductInjection2 :: forall c. (HasCoproducts c, Eq (Mor c)) => c -> CoproductOb c -> Mor c -> Mor c -> Bool
coproductInjection2 categoryValue coproductObject first second =
  rightEquals (composeC @c categoryValue (coproductUniversal @c categoryValue first second) (coproductInj2 @c categoryValue coproductObject)) second

pullbackCommutative :: forall c. (HasPullbacks c, Eq (Mor c), Eq (Ob c)) => c -> Mor c -> Mor c -> Bool
pullbackCommutative categoryValue first second =
  case endpointAgreement (targetC @c categoryValue first) (targetC @c categoryValue second) of
    Nothing -> False
    Just False -> True
    Just True ->
      case pullback @c categoryValue first second of
        Nothing -> False
        Just (_, leftLeg, rightLeg) ->
          rightValuesEqual
            (composeC @c categoryValue first leftLeg)
            (composeC @c categoryValue second rightLeg)

pushoutCommutative :: forall c. (HasPushouts c, Eq (Mor c), Eq (Ob c)) => c -> Mor c -> Mor c -> Bool
pushoutCommutative categoryValue first second =
  case endpointAgreement (sourceC @c categoryValue first) (sourceC @c categoryValue second) of
    Nothing -> False
    Just False -> True
    Just True ->
      case pushout @c categoryValue first second of
        Nothing -> False
        Just (_, leftLeg, rightLeg) ->
          rightValuesEqual
            (composeC @c categoryValue leftLeg first)
            (composeC @c categoryValue rightLeg second)

equalizerCommutative :: forall c. (HasEqualizers c, Eq (Mor c), Eq (Ob c)) => c -> Mor c -> Mor c -> Bool
equalizerCommutative categoryValue first second =
  case parallelMorphisms @c categoryValue first second of
    Nothing -> False
    Just False -> True
    Just True ->
      case equalizer @c categoryValue first second of
        Nothing -> False
        Just (_, equalizerMorphism) ->
          rightValuesEqual
            (composeC @c categoryValue first equalizerMorphism)
            (composeC @c categoryValue second equalizerMorphism)

coequalizerCommutative :: forall c. (HasCoequalizers c, Eq (Mor c), Eq (Ob c)) => c -> Mor c -> Mor c -> Bool
coequalizerCommutative categoryValue first second =
  case parallelMorphisms @c categoryValue first second of
    Nothing -> False
    Just False -> True
    Just True ->
      case coequalizer @c categoryValue first second of
        Nothing -> False
        Just (_, coequalizerMorphism) ->
          rightValuesEqual
            (composeC @c categoryValue coequalizerMorphism first)
            (composeC @c categoryValue coequalizerMorphism second)

rightEquals :: Eq value => Either err value -> value -> Bool
rightEquals eitherValue expected =
  case eitherValue of
    Right value -> value == expected
    Left _ -> False

rightValuesEqual :: Eq value => Either left value -> Either right value -> Bool
rightValuesEqual left right =
  case (left, right) of
    (Right leftValue, Right rightValue) -> leftValue == rightValue
    _ -> False

endpointAgreement :: Eq object => Either left object -> Either right object -> Maybe Bool
endpointAgreement left right =
  case (left, right) of
    (Right leftObject, Right rightObject) -> Just (leftObject == rightObject)
    _ -> Nothing

parallelMorphisms :: forall c. (Category c, Eq (Ob c)) => c -> Mor c -> Mor c -> Maybe Bool
parallelMorphisms categoryValue first second = do
  sourcesAgree <- endpointAgreement (sourceC @c categoryValue first) (sourceC @c categoryValue second)
  targetsAgree <- endpointAgreement (targetC @c categoryValue first) (targetC @c categoryValue second)
  pure (sourcesAgree && targetsAgree)

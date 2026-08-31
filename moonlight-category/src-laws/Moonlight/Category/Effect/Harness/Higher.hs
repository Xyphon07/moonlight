{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Executable checks for 2-category boundary and interchange laws.
module Moonlight.Category.Effect.Harness.Higher
  ( horizontalBoundary,
    verticalBoundary,
    interchange,
  )
where

import Moonlight.Category.Effect.Harness.Core (composeC, sourceC, targetC)
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Higher (HigherCategory (..))

horizontalBoundary :: forall c. (HigherCategory c, Eq (Mor c), Eq (Ob c)) => c -> TwoMor c -> TwoMor c -> Bool
horizontalBoundary categoryValue left right =
  case horizontalComposable @c categoryValue left right of
    Nothing -> False
    Just False -> True
    Just True ->
      case
        ( composeC @c categoryValue (source2 @c left) (source2 @c right),
          composeC @c categoryValue (target2 @c left) (target2 @c right)
        )
        of
          (Right expectedSource, Right expectedTarget) ->
            case hCompose @c categoryValue left right of
              Left _ -> False
              Right composed ->
                source2 @c composed == expectedSource
                  && target2 @c composed == expectedTarget
          _ -> False

verticalBoundary :: forall c. (HigherCategory c, Eq (Mor c)) => c -> TwoMor c -> TwoMor c -> Bool
verticalBoundary categoryValue left right =
  not (verticalComposable @c left right)
    || case vCompose @c categoryValue left right of
      Left _ -> False
      Right composed ->
        source2 @c composed == source2 @c right
          && target2 @c composed == target2 @c left

interchange :: forall c. (HigherCategory c, Eq (Ob c), Eq (Mor c), Eq (TwoMor c)) => c -> TwoMor c -> TwoMor c -> TwoMor c -> TwoMor c -> Bool
interchange categoryValue upperLeft upperRight lowerLeft lowerRight =
  let horizontalUpper = hCompose @c categoryValue upperLeft upperRight
      horizontalLower = hCompose @c categoryValue lowerLeft lowerRight
      lhs = horizontalUpper >>= (\upper -> horizontalLower >>= vCompose @c categoryValue upper)
      verticalLeft = vCompose @c categoryValue upperLeft lowerLeft
      verticalRight = vCompose @c categoryValue upperRight lowerRight
      rhs = verticalLeft >>= (\left -> verticalRight >>= hCompose @c categoryValue left)
      horizontalApplicability =
        liftA2
          (&&)
          (horizontalComposable @c categoryValue upperLeft upperRight)
          (horizontalComposable @c categoryValue lowerLeft lowerRight)
   in case horizontalApplicability of
        Nothing -> False
        Just horizontalApplicable ->
          let applicable =
                horizontalApplicable
                  && verticalComposable @c upperLeft lowerLeft
                  && verticalComposable @c upperRight lowerRight
           in not applicable || rightValuesEqual lhs rhs

rightValuesEqual :: Eq value => Either left value -> Either right value -> Bool
rightValuesEqual left right =
  case (left, right) of
    (Right leftValue, Right rightValue) -> leftValue == rightValue
    _ -> False

horizontalComposable :: forall c. (HigherCategory c, Eq (Ob c)) => c -> TwoMor c -> TwoMor c -> Maybe Bool
horizontalComposable categoryValue left right =
  liftA2
    (&&)
    (endpointAgreement (sourceC @c categoryValue (source2 @c left)) (targetC @c categoryValue (source2 @c right)))
    (endpointAgreement (sourceC @c categoryValue (target2 @c left)) (targetC @c categoryValue (target2 @c right)))

endpointAgreement :: Eq object => Either left object -> Either right object -> Maybe Bool
endpointAgreement left right =
  case (left, right) of
    (Right leftObject, Right rightObject) -> Just (leftObject == rightObject)
    _ -> Nothing

verticalComposable :: forall c. (HigherCategory c, Eq (Mor c)) => TwoMor c -> TwoMor c -> Bool
verticalComposable left right =
  target2 @c right == source2 @c left

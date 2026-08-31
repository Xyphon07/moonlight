{-# LANGUAGE TypeFamilies #-}

-- | Structured cospans: two legs into a shared apex carrying a decoration, with
-- boundary projections and pushout composition.
module Moonlight.Category.Pure.StructuredCospan
  ( StructuredCospan,
    structuredLeftLeg,
    structuredRightLeg,
    structuredApex,
    structuredDecoration,
    mkStructuredCospan,
    mkStructuredCospanFromSharedLeg,
    leftBoundary,
    rightBoundary,
    composeStructuredCospan,
    StructuredCospanError (..),
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Category.Pure.Category (Category (..), composeMor)
import Moonlight.Category.Pure.Limits (HasPushouts (..))

type StructuredCospan :: Type -> Type -> Type
-- | Two boundary morphisms with a common apex and an attached decoration.
data StructuredCospan category decoration = StructuredCospan
  { structuredLeftBoundary :: Ob category,
    structuredRightBoundary :: Ob category,
    -- | The morphism from the left boundary into the apex.
    structuredLeftLeg :: Mor category,
    -- | The morphism from the right boundary into the apex.
    structuredRightLeg :: Mor category,
    -- | The common codomain of both legs.
    structuredApex :: Ob category,
    -- | Data carried by the apex.
    structuredDecoration :: decoration
  }

type StructuredCospanError :: Type -> Type
-- | Failure to inspect, align, or push out structured boundaries.
data StructuredCospanError category
  = StructuredCospanCategoryError (CategoryError category)
  | StructuredCospanBoundaryMismatch (Ob category) (Ob category)
  | StructuredCospanPushoutMissing (Mor category) (Mor category)

-- | Validate that both legs share an apex.
mkStructuredCospan :: (Category category, Eq (Ob category)) => category -> Mor category -> Mor category -> decoration -> Either (StructuredCospanError category) (StructuredCospan category decoration)
mkStructuredCospan categoryValue leftLeg rightLeg decoration = do
  leftSource <- first StructuredCospanCategoryError (source categoryValue leftLeg)
  rightSource <- first StructuredCospanCategoryError (source categoryValue rightLeg)
  leftTarget <- first StructuredCospanCategoryError (target categoryValue leftLeg)
  rightTarget <- first StructuredCospanCategoryError (target categoryValue rightLeg)
  if leftTarget == rightTarget
    then Right (StructuredCospan leftSource rightSource leftLeg rightLeg leftTarget decoration)
    else Left (StructuredCospanBoundaryMismatch leftTarget rightTarget)
{-# INLINE mkStructuredCospan #-}

-- | Build a cospan whose two legs are the same admitted morphism.
--
-- The shared leg supplies both boundary objects and its codomain supplies the
-- common apex, so no object equality witness is needed or rechecked.
mkStructuredCospanFromSharedLeg :: Category category => category -> Mor category -> decoration -> Either (StructuredCospanError category) (StructuredCospan category decoration)
mkStructuredCospanFromSharedLeg categoryValue sharedLeg decoration = do
  sharedSource <- first StructuredCospanCategoryError (source categoryValue sharedLeg)
  sharedTarget <- first StructuredCospanCategoryError (target categoryValue sharedLeg)
  pure (StructuredCospan sharedSource sharedSource sharedLeg sharedLeg sharedTarget decoration)
{-# INLINE mkStructuredCospanFromSharedLeg #-}

-- | Project the left boundary through the category's failure surface.
leftBoundary :: category -> StructuredCospan category decoration -> Either (CategoryError category) (Ob category)
leftBoundary _ =
  Right . structuredLeftBoundary
{-# INLINE leftBoundary #-}

-- | Project the right boundary through the category's failure surface.
rightBoundary :: category -> StructuredCospan category decoration -> Either (CategoryError category) (Ob category)
rightBoundary _ =
  Right . structuredRightBoundary
{-# INLINE rightBoundary #-}

-- | Glue matching boundaries by pushout and combine their decorations.
composeStructuredCospan ::
  (HasPushouts category, Eq (Ob category)) =>
  category ->
  (leftDecoration -> rightDecoration -> combinedDecoration) ->
  StructuredCospan category leftDecoration ->
  StructuredCospan category rightDecoration ->
  Either (StructuredCospanError category) (StructuredCospan category combinedDecoration)
composeStructuredCospan categoryValue combineDecorations leftCospan rightCospan = do
  if structuredRightBoundary leftCospan == structuredLeftBoundary rightCospan
    then Right ()
    else Left (StructuredCospanBoundaryMismatch (structuredRightBoundary leftCospan) (structuredLeftBoundary rightCospan))
  (pushoutObject, pushoutLeft, pushoutRight) <-
    maybe
      (Left (StructuredCospanPushoutMissing (structuredRightLeg leftCospan) (structuredLeftLeg rightCospan)))
      Right
      (pushout categoryValue (structuredRightLeg leftCospan) (structuredLeftLeg rightCospan))
  composedLeft <- first StructuredCospanCategoryError (composeMor categoryValue pushoutLeft (structuredLeftLeg leftCospan))
  composedRight <- first StructuredCospanCategoryError (composeMor categoryValue pushoutRight (structuredRightLeg rightCospan))
  pure
    ( StructuredCospan
        (structuredLeftBoundary leftCospan)
        (structuredRightBoundary rightCospan)
        composedLeft
        composedRight
        pushoutObject
        (combineDecorations (structuredDecoration leftCospan) (structuredDecoration rightCospan))
    )
{-# INLINE composeStructuredCospan #-}

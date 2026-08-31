{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Double categories: horizontal and vertical morphisms, squares and their
-- compositions, and the interchange law.
module Moonlight.Category.Pure.DoubleCategory
  ( DoubleCategory (..),
    interchangeLaw,
  )
where

import Data.Kind (Constraint, Type)

type DoubleCategory :: Type -> Type -> Constraint
-- | Horizontal and vertical categories connected by composable squares.
class DoubleCategory object double | double -> object where
  type ObjectWitness object double :: object -> Type
  type HorizontalMor object double :: object -> object -> Type
  type VerticalMor object double :: object -> object -> Type
  type Square object double :: object -> object -> object -> object -> Type

  horizontalIdentity :: ObjectWitness object double objectValue -> HorizontalMor object double objectValue objectValue
  verticalIdentity :: ObjectWitness object double objectValue -> VerticalMor object double objectValue objectValue

  composeHorizontal ::
    HorizontalMor object double boundary target ->
    HorizontalMor object double source boundary ->
    Maybe (HorizontalMor object double source target)
  composeVertical ::
    VerticalMor object double boundary target ->
    VerticalMor object double source boundary ->
    Maybe (VerticalMor object double source target)

  squareTop :: Square object double northWest northEast southWest southEast -> HorizontalMor object double northWest northEast
  squareBottom :: Square object double northWest northEast southWest southEast -> HorizontalMor object double southWest southEast
  squareLeft :: Square object double northWest northEast southWest southEast -> VerticalMor object double northWest southWest
  squareRight :: Square object double northWest northEast southWest southEast -> VerticalMor object double northEast southEast

  composeSquaresHorizontal ::
    Square object double middleNorth eastNorth middleSouth eastSouth ->
    Square object double westNorth middleNorth westSouth middleSouth ->
    Maybe (Square object double westNorth eastNorth westSouth eastSouth)
  composeSquaresVertical ::
    Square object double middleWest middleEast southWest southEast ->
    Square object double northWest northEast middleWest middleEast ->
    Maybe (Square object double northWest northEast southWest southEast)

-- | Check that horizontal-then-vertical and vertical-then-horizontal square
-- composition agree whenever all four composites exist.
interchangeLaw ::
  forall object double northWest middleNorth middleWest center eastNorth middleEast westSouth middleSouth southEast.
  ( DoubleCategory object double,
    Eq (Square object double northWest eastNorth westSouth southEast)
  ) =>
  Square object double northWest middleNorth middleWest center ->
  Square object double middleNorth eastNorth center middleEast ->
  Square object double middleWest center westSouth middleSouth ->
  Square object double center middleEast middleSouth southEast ->
  Maybe Bool
interchangeLaw northWestSquare northEastSquare southWestSquare southEastSquare = do
  northRow <- composeSquaresHorizontal @object @double northEastSquare northWestSquare
  southRow <- composeSquaresHorizontal @object @double southEastSquare southWestSquare
  horizontalThenVertical <- composeSquaresVertical @object @double southRow northRow
  westColumn <- composeSquaresVertical @object @double southWestSquare northWestSquare
  eastColumn <- composeSquaresVertical @object @double southEastSquare northEastSquare
  verticalThenHorizontal <- composeSquaresHorizontal @object @double eastColumn westColumn
  pure (horizontalThenVertical == verticalThenHorizontal)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Category.Test.DoubleFixture
  ( SymbolicDouble,
    SymbolicHorizontal (..),
    SymbolicObject (..),
    SymbolicSquare (..),
    SymbolicVertical (..),
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Moonlight.Category.Pure.DoubleCategory (DoubleCategory (..))

type SymbolicObject :: Type
data SymbolicObject
  = ObjectA
  | ObjectB
  | ObjectC
  | ObjectD
  | ObjectE
  | ObjectF
  | ObjectG
  | ObjectH
  | ObjectI

type SymbolicHorizontal :: Type -> SymbolicObject -> SymbolicObject -> Type
newtype SymbolicHorizontal label source target = SymbolicHorizontal
  { symbolicHorizontalTrace :: [label]
  }
  deriving stock (Eq, Show)

type SymbolicVertical :: Type -> SymbolicObject -> SymbolicObject -> Type
newtype SymbolicVertical label source target = SymbolicVertical
  { symbolicVerticalTrace :: [label]
  }
  deriving stock (Eq, Show)

type SymbolicSquare ::
  Type ->
  SymbolicObject ->
  SymbolicObject ->
  SymbolicObject ->
  SymbolicObject ->
  Type
data SymbolicSquare label northWest northEast southWest southEast = SymbolicSquare
  { symbolicSquareTop :: SymbolicHorizontal label northWest northEast,
    symbolicSquareBottom :: SymbolicHorizontal label southWest southEast,
    symbolicSquareLeft :: SymbolicVertical label northWest southWest,
    symbolicSquareRight :: SymbolicVertical label northEast southEast
  }
  deriving stock (Eq, Show)

type SymbolicDouble :: Type -> Type
data SymbolicDouble label

instance DoubleCategory SymbolicObject (SymbolicDouble label) where
  type ObjectWitness SymbolicObject (SymbolicDouble label) = Proxy
  type HorizontalMor SymbolicObject (SymbolicDouble label) = SymbolicHorizontal label
  type VerticalMor SymbolicObject (SymbolicDouble label) = SymbolicVertical label
  type Square SymbolicObject (SymbolicDouble label) = SymbolicSquare label

  horizontalIdentity _ = SymbolicHorizontal []
  verticalIdentity _ = SymbolicVertical []
  composeHorizontal leftHorizontal rightHorizontal =
    Just
      SymbolicHorizontal
        { symbolicHorizontalTrace = symbolicHorizontalTrace rightHorizontal <> symbolicHorizontalTrace leftHorizontal
        }
  composeVertical lowerVertical upperVertical =
    Just
      SymbolicVertical
        { symbolicVerticalTrace = symbolicVerticalTrace upperVertical <> symbolicVerticalTrace lowerVertical
        }
  squareTop = symbolicSquareTop
  squareBottom = symbolicSquareBottom
  squareLeft = symbolicSquareLeft
  squareRight = symbolicSquareRight
  composeSquaresHorizontal eastSquare westSquare =
    Just
      SymbolicSquare
        { symbolicSquareTop =
            SymbolicHorizontal
              { symbolicHorizontalTrace =
                  symbolicHorizontalTrace (symbolicSquareTop westSquare)
                    <> symbolicHorizontalTrace (symbolicSquareTop eastSquare)
              },
          symbolicSquareBottom =
            SymbolicHorizontal
              { symbolicHorizontalTrace =
                  symbolicHorizontalTrace (symbolicSquareBottom westSquare)
                    <> symbolicHorizontalTrace (symbolicSquareBottom eastSquare)
              },
          symbolicSquareLeft = symbolicSquareLeft westSquare,
          symbolicSquareRight = symbolicSquareRight eastSquare
        }
  composeSquaresVertical southSquare northSquare =
    Just
      SymbolicSquare
        { symbolicSquareTop = symbolicSquareTop northSquare,
          symbolicSquareBottom = symbolicSquareBottom southSquare,
          symbolicSquareLeft =
            SymbolicVertical
              { symbolicVerticalTrace =
                  symbolicVerticalTrace (symbolicSquareLeft northSquare)
                    <> symbolicVerticalTrace (symbolicSquareLeft southSquare)
              },
          symbolicSquareRight =
            SymbolicVertical
              { symbolicVerticalTrace =
                  symbolicVerticalTrace (symbolicSquareRight northSquare)
                    <> symbolicVerticalTrace (symbolicSquareRight southSquare)
              }
        }

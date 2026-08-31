{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module DoubleCategorySpec
  ( tests,
  )
where

import Data.Proxy (Proxy (..))
import Moonlight.Category (DoubleCategory (..), interchangeLaw)
import Moonlight.Category.Test.DoubleFixture
  ( SymbolicDouble,
    SymbolicHorizontal (..),
    SymbolicObject (..),
    SymbolicSquare (..),
    SymbolicVertical (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "DoubleCategory"
    [ testCase "interchange law holds for a symbolic 2x2 grid" testInterchangeLaw,
      testCase "typed identities are neutral on symbolic morphisms" testIdentity
    ]

horizontalArrow :: String -> SymbolicHorizontal String source target
horizontalArrow labelValue = SymbolicHorizontal [labelValue]

verticalArrow :: String -> SymbolicVertical String source target
verticalArrow labelValue = SymbolicVertical [labelValue]

northWestSquare :: SymbolicSquare String 'ObjectA 'ObjectB 'ObjectD 'ObjectE
northWestSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "top-west",
      symbolicSquareBottom = horizontalArrow "middle-west",
      symbolicSquareLeft = verticalArrow "left-north",
      symbolicSquareRight = verticalArrow "middle-north"
    }

northEastSquare :: SymbolicSquare String 'ObjectB 'ObjectC 'ObjectE 'ObjectF
northEastSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "top-east",
      symbolicSquareBottom = horizontalArrow "middle-east",
      symbolicSquareLeft = verticalArrow "middle-north",
      symbolicSquareRight = verticalArrow "right-north"
    }

southWestSquare :: SymbolicSquare String 'ObjectD 'ObjectE 'ObjectG 'ObjectH
southWestSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "middle-west",
      symbolicSquareBottom = horizontalArrow "bottom-west",
      symbolicSquareLeft = verticalArrow "left-south",
      symbolicSquareRight = verticalArrow "middle-south"
    }

southEastSquare :: SymbolicSquare String 'ObjectE 'ObjectF 'ObjectH 'ObjectI
southEastSquare =
  SymbolicSquare
    { symbolicSquareTop = horizontalArrow "middle-east",
      symbolicSquareBottom = horizontalArrow "bottom-east",
      symbolicSquareLeft = verticalArrow "middle-south",
      symbolicSquareRight = verticalArrow "right-south"
    }

testInterchangeLaw :: IO ()
testInterchangeLaw =
  interchangeLaw @SymbolicObject @(SymbolicDouble String) northWestSquare northEastSquare southWestSquare southEastSquare
    @?= Just True

testIdentity :: IO ()
testIdentity = do
  composeHorizontal @SymbolicObject @(SymbolicDouble String) (horizontalIdentity @SymbolicObject @(SymbolicDouble String) (Proxy @'ObjectB)) (horizontalArrow "edge" :: SymbolicHorizontal String 'ObjectA 'ObjectB)
    @?= Just (horizontalArrow "edge")
  composeVertical @SymbolicObject @(SymbolicDouble String) (verticalIdentity @SymbolicObject @(SymbolicDouble String) (Proxy @'ObjectB)) (verticalArrow "edge" :: SymbolicVertical String 'ObjectA 'ObjectB)
    @?= Just (verticalArrow "edge")

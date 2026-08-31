{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module CoveringProductSpec
  ( tests,
  )
where

import Data.Kind (Type)
import Moonlight.Category
  ( CoveringProduct,
    adjustCoveringProduct,
    indexCoveringProduct,
    replaceCoveringProduct,
    restrictCoveringProduct,
    tabulateCoveringProduct,
  )
import Moonlight.Category.Test.CoveringFixture
  ( DemoField,
    DemoFieldWitness (..),
    DemoSubsetWitness (..),
    embedDemoSubsetWitness,
    sameDemoFieldWitness,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

type DemoValue :: DemoField -> Type
newtype DemoValue (field :: DemoField) = DemoValue
  { unDemoValue :: String
  }
  deriving stock (Eq, Show)

demoProduct :: CoveringProduct DemoFieldWitness DemoValue
demoProduct =
  tabulateCoveringProduct
    ( \witness ->
        case witness of
          AlphaFieldWitness -> DemoValue "alpha"
          BetaFieldWitness -> DemoValue "beta"
          GammaFieldWitness -> DemoValue "gamma"
    )

tests :: TestTree
tests =
  testGroup
    "CoveringProduct"
    [ testCase "restrictCoveringProduct projects a witness-indexed subset" $
        let restrictedProduct =
              restrictCoveringProduct embedDemoSubsetWitness demoProduct
         in do
              unDemoValue (indexCoveringProduct restrictedProduct AlphaSubsetWitness) @?= "alpha"
              unDemoValue (indexCoveringProduct restrictedProduct GammaSubsetWitness) @?= "gamma",
      testCase "adjustCoveringProduct updates exactly the targeted witness" $
        let adjustedProduct =
              adjustCoveringProduct
                sameDemoFieldWitness
                BetaFieldWitness
                (\(DemoValue value) -> DemoValue (value <> "-adjusted"))
                demoProduct
         in do
              unDemoValue (indexCoveringProduct adjustedProduct AlphaFieldWitness) @?= "alpha"
              unDemoValue (indexCoveringProduct adjustedProduct BetaFieldWitness) @?= "beta-adjusted"
              unDemoValue (indexCoveringProduct adjustedProduct GammaFieldWitness) @?= "gamma",
      testCase "replaceCoveringProduct delegates through typed witness equality" $
        let replacedProduct =
              replaceCoveringProduct
                sameDemoFieldWitness
                GammaFieldWitness
                (DemoValue "gamma-replaced")
                demoProduct
         in do
              unDemoValue (indexCoveringProduct replacedProduct AlphaFieldWitness) @?= "alpha"
              unDemoValue (indexCoveringProduct replacedProduct BetaFieldWitness) @?= "beta"
              unDemoValue (indexCoveringProduct replacedProduct GammaFieldWitness) @?= "gamma-replaced"
    ]

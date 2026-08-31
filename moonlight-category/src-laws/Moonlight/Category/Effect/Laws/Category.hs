module Moonlight.Category.Effect.Laws.Category
  ( lawSuites,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators
  ( SampleComposableFinTriple (..),
    SampleFinMorphism (..),
  )
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category.Pure.FinCat (FinCat)
import Moonlight.Pale.Test.Laws.Suite
  ( LawSuite,
    lawGroup,
    namedQuickCheckLaw,
  )

finCategoryLaws :: Harness.CategoryLaws FinCat
finCategoryLaws = Harness.mkCategoryLaws @FinCat sampleFinCat

categoryLeftIdProp :: SampleFinMorphism -> Bool
categoryLeftIdProp (SampleFinMorphism morphism) =
  Harness.categoryLeftIdentity finCategoryLaws morphism

categoryRightIdProp :: SampleFinMorphism -> Bool
categoryRightIdProp (SampleFinMorphism morphism) =
  Harness.categoryRightIdentity finCategoryLaws morphism

categoryAssocProp :: SampleComposableFinTriple -> Bool
categoryAssocProp (SampleComposableFinTriple firstValue secondValue thirdValue) =
  Harness.categoryAssociativity finCategoryLaws firstValue secondValue thirdValue

lawSuites :: [LawSuite]
lawSuites =
  [ lawGroup
      "category"
      [ namedQuickCheckLaw CategoryLeftId categoryLeftIdProp,
        namedQuickCheckLaw CategoryRightId categoryRightIdProp,
        namedQuickCheckLaw CategoryAssoc categoryAssocProp
      ]
  ]

module Moonlight.Category.Effect.Laws.Higher
  ( lawSuites,
  )
where

import qualified Moonlight.Category.Effect.Harness as Harness
import Moonlight.Category.Effect.LawNames (LawName (..))
import Moonlight.Category.Effect.Laws.Generators
  ( SampleUnitTwoMorphism (..),
  )
import Moonlight.Category.Pure.Unit
  ( UnitCat (..),
  )
import Moonlight.Pale.Test.Laws.Suite (LawSuite, lawGroup, namedQuickCheckLaw)

higherHorizontalProp :: SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> Bool
higherHorizontalProp (SampleUnitTwoMorphism leftValue) (SampleUnitTwoMorphism rightValue) =
  Harness.horizontalBoundary @UnitCat UnitCat leftValue rightValue

higherVerticalProp :: SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> Bool
higherVerticalProp (SampleUnitTwoMorphism leftValue) (SampleUnitTwoMorphism rightValue) =
  Harness.verticalBoundary @UnitCat UnitCat leftValue rightValue

higherInterchangeProp :: SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> SampleUnitTwoMorphism -> Bool
higherInterchangeProp
  (SampleUnitTwoMorphism upperLeftValue)
  (SampleUnitTwoMorphism upperRightValue)
  (SampleUnitTwoMorphism lowerLeftValue)
  (SampleUnitTwoMorphism lowerRightValue) =
    Harness.interchange @UnitCat UnitCat upperLeftValue upperRightValue lowerLeftValue lowerRightValue

lawSuites :: [LawSuite]
lawSuites =
  [ lawGroup
      "higher"
      [ namedQuickCheckLaw HigherHorizontalBoundary higherHorizontalProp,
        namedQuickCheckLaw HigherVerticalBoundary higherVerticalProp,
        namedQuickCheckLaw HigherInterchange higherInterchangeProp
      ]
  ]

module AbstractTests
  ( tests,
  )
where

import qualified AdhesiveSpec
import qualified CoveringProductSpec
import qualified DecoratedPresentationSpec
import qualified DoubleCategorySpec
import qualified FiniteComposableSpec
import qualified PolynomialFunctorWitnessSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "abstract"
    [ AdhesiveSpec.tests,
      CoveringProductSpec.tests,
      DecoratedPresentationSpec.tests,
      DoubleCategorySpec.tests,
      FiniteComposableSpec.tests,
      PolynomialFunctorWitnessSpec.tests
    ]

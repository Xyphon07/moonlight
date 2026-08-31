module FiniteTests
  ( tests,
  )
where

import qualified DenseReachabilitySpec
import qualified FinPresentationSpec
import qualified FinThinFunctorSpec
import qualified InvertibilitySpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "finite"
    [ FinPresentationSpec.tests,
      FinThinFunctorSpec.tests,
      InvertibilitySpec.tests,
      DenseReachabilitySpec.tests
    ]

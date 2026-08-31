module NumericTests
  ( tests,
  )
where

import qualified ApproxEqSpec as ApproxEqSpec
import qualified CanonSpec as CanonSpec
import qualified CanonicalNumberSpec as CanonicalNumberSpec
import qualified ExactTokenSpec as ExactTokenSpec
import qualified NicheSpec as NicheSpec
import qualified ScalarLawsSpec as ScalarLawsSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-core-numeric"
    [ ApproxEqSpec.tests,
      CanonSpec.tests,
      CanonicalNumberSpec.tests,
      ExactTokenSpec.tests,
      NicheSpec.tests,
      ScalarLawsSpec.tests
    ]

module FacadeTests
  ( tests,
  )
where

import qualified NotationSpec
import qualified FacadeSiteNerveSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "facade"
    [ NotationSpec.tests,
      FacadeSiteNerveSpec.tests
    ]

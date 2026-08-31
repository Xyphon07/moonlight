module IndexedTests
  ( tests,
  )
where

import qualified IndexedSpec
import qualified SimplexSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "indexed"
    [ IndexedSpec.tests,
      SimplexSpec.tests
    ]

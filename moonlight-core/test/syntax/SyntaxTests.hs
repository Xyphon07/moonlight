module SyntaxTests
  ( tests,
  )
where

import qualified PatternSpec as PatternSpec
import qualified SubstitutionSpec as SubstitutionSpec
import qualified TheorySpec as TheorySpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-core-syntax"
    [ PatternSpec.tests,
      SubstitutionSpec.tests,
      TheorySpec.tests
    ]

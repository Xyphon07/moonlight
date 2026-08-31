module TermTests
  ( tests,
  )
where

import qualified DatabaseSpec as DatabaseSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-core-term"
    [ DatabaseSpec.tests
    ]

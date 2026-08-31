module Main
  ( main,
  )
where

import Assertions.AssertionSpec qualified as AssertionSpec
import Recursion.RecursionSpec qualified as RecursionSpec
import Resources.ResourceSpec qualified as ResourceSpec
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "pale-test-support"
      [ AssertionSpec.tests,
        RecursionSpec.tests,
        ResourceSpec.tests
      ]

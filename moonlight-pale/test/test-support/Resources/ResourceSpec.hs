module Resources.ResourceSpec
  ( tests,
  )
where

import Moonlight.Pale.Test.Resources
  ( ResourcePathError (ResourcePathNotRelativeToRoot),
    resolveCompilerFile,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Resources"
    [ testCase "rejects an absolute child path" $
        assertPathNotRelativeToRoot "/tmp/moonlight-pale-escape",
      testCase "rejects a parent-relative child path" $
        assertPathNotRelativeToRoot "../moonlight-pale-escape"
    ]

assertPathNotRelativeToRoot :: FilePath -> Assertion
assertPathNotRelativeToRoot childPath =
  resolveCompilerFile packageMarker childPath
    >>= \resolution ->
      case resolution of
        Left ResourcePathNotRelativeToRoot {} ->
          pure ()
        Left otherFailure ->
          assertFailure ("expected ResourcePathNotRelativeToRoot, got " <> show otherFailure)
        Right resolvedPath ->
          assertFailure ("escaped resource unexpectedly resolved to " <> resolvedPath)

packageMarker :: FilePath
packageMarker =
  "foundation/moonlight-pale/moonlight-pale.cabal"

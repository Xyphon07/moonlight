module SolverTests
  ( tests,
  )
where

import qualified FixpointSpec as FixpointSpec
import qualified UnionFindSpec as UnionFindSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "moonlight-core-solver"
    [ FixpointSpec.tests,
      UnionFindSpec.tests
    ]

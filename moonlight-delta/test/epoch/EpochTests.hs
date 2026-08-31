module EpochTests
  ( contextProjectionCarrierIntGen,
    contextProjectionDeltaIntGen,
    epochDeltaIntGen,
    tests,
  )
where

import ComposeSpec (composeTests)
import ConstructionSpec (constructionTests)
import FiniteSpec (finiteTests)
import EpochSupport.Generators
  ( contextProjectionCarrierIntGen,
    contextProjectionDeltaIntGen,
    epochDeltaIntGen,
  )
import ProjectionSpec (projectionTests)
import TransportSpec (transportTests)
import VersionSpec (versionTests)
import ViewSpec (viewTests)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "epoch"
    [ versionTests,
      projectionTests,
      viewTests,
      constructionTests,
      transportTests,
      composeTests,
      finiteTests
    ]

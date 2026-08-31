module SimplicialTests
  ( tests,
  )
where

import qualified Laws.Registry as LawRegistry
import Laws.Suite (lawfulCarrierSuite)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "simplicial"
    ( LawRegistry.carrierTestSuites
        <> [lawfulCarrierSuite LawRegistry.lawfulCarrierSpecs]
    )

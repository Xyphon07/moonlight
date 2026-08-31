module SiteTests
  ( tests,
  )
where

import qualified PathQuotientSpec
import qualified SiteSpec
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "site"
    [ SiteSpec.tests,
      PathQuotientSpec.tests
    ]

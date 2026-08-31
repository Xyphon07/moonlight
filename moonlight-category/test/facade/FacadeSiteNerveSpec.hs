module FacadeSiteNerveSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Category
  ( SiteManifest (..),
    chainVertices,
    thinSiteImportKernel,
    thinSiteKernelCodomain,
    thinSiteObjectValue,
  )
import Moonlight.Category.Simplicial
  ( nerveSimplexChain,
    normalizedNerve,
    simplicesAtDimension,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "main and simplicial facades"
    [ testCase
        "site imports compile, normalize, and recover names without Pure imports"
        testFacadeOnlySiteNerve
    ]

testFacadeOnlySiteNerve :: Assertion
testFacadeOnlySiteNerve =
  case thinSiteImportKernel threeModuleManifest of
    Left siteError ->
      assertFailure ("import kernel rejected a valid module manifest: " <> show siteError)
    Right kernel -> do
      let normalized = normalizedNerve (thinSiteKernelCodomain kernel) 2
          fVector = length . simplicesAtDimension normalized <$> [0, 1, 2]
      fVector @?= [3, 3, 1]
      case simplicesAtDimension normalized 2 of
        [topSimplex] ->
          traverse
            (thinSiteObjectValue kernel)
            (chainVertices (nerveSimplexChain topSimplex))
            @?= Right ("app" :| ["api", "core"])
        topSimplices ->
          assertFailure
            ( "expected exactly one normalized 2-simplex, found "
                <> show (length topSimplices)
            )

threeModuleManifest :: SiteManifest String
threeModuleManifest =
  SiteManifest
    { siteObjects = Set.fromList ["app", "api", "core"],
      siteImports =
        Map.fromList
          [ ("app", Set.singleton "api"),
            ("api", Set.singleton "core"),
            ("core", Set.empty)
          ],
      siteCovers =
        Map.fromList
          [ ("app", Set.fromList ["api", "core"]),
            ("api", Set.singleton "core"),
            ("core", Set.empty)
          ]
    }

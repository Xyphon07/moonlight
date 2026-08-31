module PathQuotientSpec
  ( tests,
  )
where

import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Category
  ( SiteManifest (..),
    SitePathCategory,
    SitePathMorphism,
    SitePathObject,
    SitePathQuotient,
    SitePathQuotientError (..),
    mkSitePathMorphism,
    mkSitePathObject,
    quotientMapMorphism,
    quotientMapObject,
    siteObjects,
    sitePathCategory,
    sitePathQuotient,
    thinSiteKernel,
  )
import qualified Moonlight.Category.Effect.PathQuotientHarness as PathQuotientHarness
import Moonlight.Category.Effect.SiteGen (diamondManifest)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

diamondPathCategory :: Either () (SitePathCategory Int)
diamondPathCategory =
  case thinSiteKernel diamondManifest of
    Left _ -> Left ()
    Right kernel -> Right (sitePathCategory kernel)

tests :: TestTree
tests =
  case diamondPathCategory of
    Left _ ->
      testGroup
        "path-quotient"
        [ testCase "diamond manifest yields a path category" (assertFailure "diamond manifest should admit a site path category")
        ]
    Right category ->
      let objects = Set.toList (siteObjects diamondManifest)
          objectPairs = liftA2 (,) objects objects
       in testGroup
            "path-quotient"
            [ testCase
                "quotient uniqueness per endpoint"
                ( assertBool
                    "quotient should identify all endpoint-equal paths"
                    (all (\(sourceValue, targetValue) -> PathQuotientHarness.quotientUniquenessPerEndpoint @Int category sourceValue targetValue) objectPairs)
                ),
              testCase
                "path-thin codomain faithful"
                (assertBool "path-thin codomain map should be faithful" (PathQuotientHarness.pathThinCodomainFaithful @Int category)),
              testCase
                "interpreter coherence"
                (assertBool "quotient interpreter should agree with the codomain map" (PathQuotientHarness.quotientInterpreterCoherence @Int category)),
              testCase
                "quotientMapObject rejects objects from a different path domain"
                testQuotientMapObjectRejectsWrongDomain,
              testCase
                "quotientMapMorphism rejects morphisms from a different path domain"
                testQuotientMapMorphismRejectsWrongDomain
            ]

testQuotientMapObjectRejectsWrongDomain :: Assertion
testQuotientMapObjectRejectsWrongDomain =
  case wrongDomainFixture of
    Left message ->
      assertFailure message
    Right (leftQuotient, rightObject, _) ->
      quotientMapObject leftQuotient rightObject @?= Left QuotientObjectWrongDomain

testQuotientMapMorphismRejectsWrongDomain :: Assertion
testQuotientMapMorphismRejectsWrongDomain =
  case wrongDomainFixture of
    Left message ->
      assertFailure message
    Right (leftQuotient, _, rightMorphism) ->
      quotientMapMorphism leftQuotient rightMorphism @?= Left QuotientMorphismWrongDomain

wrongDomainFixture :: Either String (SitePathQuotient Int, SitePathObject Int, SitePathMorphism Int)
wrongDomainFixture = do
  leftKernel <- first (("left site kernel rejected: " <>) . show) (thinSiteKernel leftManifest)
  rightKernel <- first (("right site kernel rejected: " <>) . show) (thinSiteKernel rightManifest)
  let leftCategory = sitePathCategory leftKernel
      rightCategory = sitePathCategory rightKernel
      leftQuotient = sitePathQuotient leftCategory
  rightObject <-
    maybe
      (Left "right path object was not constructible")
      Right
      (mkSitePathObject rightCategory 0)
  rightMorphism <-
    maybe
      (Left "right identity path morphism was not constructible")
      Right
      (mkSitePathMorphism rightCategory (0 :| [2]))
  pure (leftQuotient, rightObject, rightMorphism)


leftManifest :: SiteManifest Int
leftManifest =
  SiteManifest
    { siteObjects = Set.fromList [0, 1, 2],
      siteImports =
        Map.fromList
          [ (0, Set.singleton 1),
            (1, Set.singleton 2),
            (2, Set.empty)
          ],
      siteCovers =
        Map.fromList
          [ (0, Set.fromList [1, 2]),
            (1, Set.singleton 2),
            (2, Set.empty)
          ]
    }

rightManifest :: SiteManifest Int
rightManifest =
  SiteManifest
    { siteObjects = Set.fromList [0, 1, 2],
      siteImports =
        Map.fromList
          [ (0, Set.fromList [1, 2]),
            (1, Set.singleton 2),
            (2, Set.empty)
          ],
      siteCovers =
        Map.fromList
          [ (0, Set.fromList [1, 2]),
            (1, Set.singleton 2),
            (2, Set.empty)
          ]
    }

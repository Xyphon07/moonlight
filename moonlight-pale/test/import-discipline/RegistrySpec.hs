module RegistrySpec
  ( tests,
  )
where

import Data.List (isInfixOf)
import Data.Set qualified as Set
import Moonlight.Pale.Test.ImportDiscipline.Registry
  ( CabalComponentSelector (..),
    cabalComponentExposedModules,
    cabalComponentOtherModules,
    cabalComponentSourceDirectories,
    cabalLibraryComponents,
    parseCabalPackageMetadata,
    renderCabalMetadataObstruction,
    selectCabalComponentMetadata,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "typed Cabal registry"
    [ testCase "projects main, named, and conditional component metadata" projectsComponentMetadata,
      testCase "reports malformed Cabal as a typed obstruction" reportsMalformedCabal,
      testCase "reports an absent component as a typed obstruction" reportsAbsentComponent
    ]

projectsComponentMetadata :: Assertion
projectsComponentMetadata =
  case parseCabalPackageMetadata fixturePackage of
    Left metadataObstruction ->
      assertFailure (renderCabalMetadataObstruction "fixture.cabal" metadataObstruction)
    Right packageMetadata -> do
      fmap fst (cabalLibraryComponents packageMetadata)
        @?= [CabalMainLibrary, CabalNamedLibrary "support"]
      case
          ( selectCabalComponentMetadata CabalMainLibrary packageMetadata,
            selectCabalComponentMetadata (CabalNamedLibrary "support") packageMetadata,
            selectCabalComponentMetadata (CabalTestSuite "unit") packageMetadata
          )
        of
          (Right mainLibrary, Right supportLibrary, Right unitTestSuite) -> do
            cabalComponentSourceDirectories mainLibrary @?= Set.singleton "src"
            cabalComponentExposedModules mainLibrary @?= Set.singleton "Surface.Main"
            cabalComponentOtherModules mainLibrary
              @?= Set.fromList ["Surface.Conditional", "Surface.Shared"]
            cabalComponentSourceDirectories supportLibrary @?= Set.singleton "support"
            cabalComponentExposedModules supportLibrary @?= Set.singleton "Surface.Support"
            cabalComponentOtherModules unitTestSuite @?= Set.singleton "Surface.UnitSpec"
          selectionResults ->
            assertFailure ("expected all fixture components, observed " <> showSelectionResults selectionResults)

reportsMalformedCabal :: Assertion
reportsMalformedCabal =
  case parseCabalPackageMetadata "this is not a Cabal package" of
    Right _ ->
      assertFailure "expected malformed Cabal text to be rejected"
    Left metadataObstruction ->
      assertBool
        "expected the parse obstruction to retain the Cabal source path"
        ("fixture.cabal:" `isInfixOf` renderCabalMetadataObstruction "fixture.cabal" metadataObstruction)

reportsAbsentComponent :: Assertion
reportsAbsentComponent =
  case parseCabalPackageMetadata fixturePackage of
    Left metadataObstruction ->
      assertFailure (renderCabalMetadataObstruction "fixture.cabal" metadataObstruction)
    Right packageMetadata ->
      case selectCabalComponentMetadata (CabalNamedLibrary "absent") packageMetadata of
        Right _ ->
          assertFailure "expected an absent component to be rejected"
        Left metadataObstruction ->
          renderCabalMetadataObstruction "fixture.cabal" metadataObstruction
            @?= "fixture.cabal: missing Cabal component library absent"

showSelectionResults :: (Either obstruction value, Either obstruction value, Either obstruction value) -> String
showSelectionResults selectionResults =
  case selectionResults of
    (Left _, _, _) -> "main library obstruction"
    (_, Left _, _) -> "support library obstruction"
    (_, _, Left _) -> "unit test-suite obstruction"
    (Right _, Right _, Right _) -> "all components present"

fixturePackage :: String
fixturePackage =
  unlines
    [ "cabal-version: 3.8",
      "name: pale-cabal-registry-fixture",
      "version: 0.1.0.0",
      "flag feature",
      "  default: True",
      "library",
      "  hs-source-dirs: src",
      "  exposed-modules: Surface.Main",
      "  other-modules: Surface.Shared",
      "  if flag(feature)",
      "    other-modules: Surface.Conditional",
      "library support",
      "  hs-source-dirs: support",
      "  exposed-modules: Surface.Support",
      "test-suite unit",
      "  type: exitcode-stdio-1.0",
      "  main-is: Main.hs",
      "  hs-source-dirs: test",
      "  other-modules: Surface.UnitSpec"
    ]

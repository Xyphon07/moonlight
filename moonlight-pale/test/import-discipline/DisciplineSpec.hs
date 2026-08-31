module DisciplineSpec
  ( tests,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Pale.Test.ImportDiscipline (SheafManifest (..), assertSheafDiscipline)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.ImportDiscipline"
    [ testCase "accepts lawful sheaf layering" lawfulLayeringIsClean,
      testCase "rejects forbidden local import edge" violatingLayeringNamesForbiddenEdge
    ]

lawfulLayeringIsClean :: Assertion
lawfulLayeringIsClean =
  assertSheafDiscipline packageMarker testSurfaceDirectory lawfulManifest

violatingLayeringNamesForbiddenEdge :: Assertion
violatingLayeringNamesForbiddenEdge =
  runDiscipline violatingManifest
    >>= \disciplineResult ->
      case disciplineResult of
        Right () ->
          assertFailure "expected sheaf import discipline to reject the forbidden local import edge"
        Left exception ->
          assertViolationNamesForbiddenEdge (displayException exception)

assertViolationNamesForbiddenEdge :: String -> Assertion
assertViolationNamesForbiddenEdge failureMessage =
  assertBool
    "expected violation to name the forbidden Discipline -> Registry import edge"
    (sourceModuleName `isInfixOf` failureMessage && targetModuleName `isInfixOf` failureMessage)

runDiscipline :: SheafManifest -> IO (Either SomeException ())
runDiscipline =
  try . assertSheafDiscipline packageMarker testSurfaceDirectory

lawfulManifest :: SheafManifest
lawfulManifest =
  SheafManifest
    { sheafModulePrefix = modulePrefix,
      sheafAllowedImports = lawfulAllowedImports
    }

violatingManifest :: SheafManifest
violatingManifest =
  SheafManifest
    { sheafModulePrefix = modulePrefix,
      sheafAllowedImports = violatingAllowedImports
    }

lawfulAllowedImports :: Map String (Set String)
lawfulAllowedImports =
  Map.fromList
    [ (sourceModuleName, Set.singleton targetModuleName),
      (targetModuleName, Set.empty)
    ]

violatingAllowedImports :: Map String (Set String)
violatingAllowedImports =
  Map.fromList
    [ (sourceModuleName, Set.empty),
      (targetModuleName, Set.empty)
    ]

packageMarker :: FilePath
packageMarker =
  "moonlight-pale.cabal"

testSurfaceDirectory :: FilePath
testSurfaceDirectory =
  "src-test-surface"

modulePrefix :: String
modulePrefix =
  "Moonlight.Pale.Test.ImportDiscipline"

sourceModuleName :: String
sourceModuleName =
  "Moonlight.Pale.Test.ImportDiscipline"

targetModuleName :: String
targetModuleName =
  "Moonlight.Pale.Test.ImportDiscipline.Registry"

{-# LANGUAGE OverloadedStrings #-}

module CompileFailSpec (tests) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Pale.TestSupport.CompileDiagnostics
  ( CompileDiagnosticsSession,
    DiagnosticSnapshot (..),
    GhcPackageSpec (..),
    SnapshotExit (..),
    compileFixtures,
    normalizeSnapshot,
    openCompileDiagnosticsSession,
    readSnapshot,
    renderFixtureFailure,
    renderResourcePathError,
    renderSnapshotFileFailure,
    resolveCompilerRoot,
    snapshotRefreshEnabled,
    writeSnapshot,
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

type FixtureCase :: Type
data FixtureCase = FixtureCase
  { fixtureCaseLabel :: !String,
    fixtureRelativePath :: !FilePath,
    fixtureSnapshotFile :: !FilePath,
    fixtureExpectedExit :: !SnapshotExit
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  withResource acquireCompileContext (const (pure ())) $ \getCompileContext ->
    testGroup "compile-fixtures" (map (buildFixtureCase getCompileContext) fixtureCases)

fixtureCases :: [FixtureCase]
fixtureCases =
  [ FixtureCase
      { fixtureCaseLabel = "phase 2 capability compiles in dedicated fixture runner",
        fixtureRelativePath = "CompilePass" </> "Phase2Betti.hs",
        fixtureSnapshotFile = "CompilePass.Phase2Betti.snapshot.json",
        fixtureExpectedExit = SnapshotSuccess
      },
    FixtureCase
      { fixtureCaseLabel = "public API cleanup exposes safe names",
        fixtureRelativePath = "CompilePass" </> "PublicApiCleanup.hs",
        fixtureSnapshotFile = "CompilePass.PublicApiCleanup.snapshot.json",
        fixtureExpectedExit = SnapshotSuccess
      },
    FixtureCase
      { fixtureCaseLabel = "phase 1 betti capability fails at compile-time",
        fixtureRelativePath = "CompileFail" </> "Phase1BettiLeak.hs",
        fixtureSnapshotFile = "CompileFail.Phase1BettiLeak.snapshot.json",
        fixtureExpectedExit = SnapshotFailure
      },
    FixtureCase
      { fixtureCaseLabel = "phase 2 spectral capability fails at compile-time",
        fixtureRelativePath = "CompileFail" </> "Phase2SpectralLeak.hs",
        fixtureSnapshotFile = "CompileFail.Phase2SpectralLeak.snapshot.json",
        fixtureExpectedExit = SnapshotFailure
      }
  ]

buildFixtureCase ::
  IO (FilePath, CompileDiagnosticsSession) ->
  FixtureCase ->
  TestTree
buildFixtureCase getCompileContext fixtureCase =
  testCase (fixtureCaseLabel fixtureCase) $
    assertFixtureSnapshot getCompileContext fixtureCase

assertFixtureSnapshot ::
  IO (FilePath, CompileDiagnosticsSession) ->
  FixtureCase ->
  IO ()
assertFixtureSnapshot getCompileContext fixtureCase = do
  (compilerRoot, session) <- getCompileContext
  let fixturesRelativeRoot =
        "foundation"
          </> "moonlight-homology"
          </> "test"
          </> "fixtures"
      fixturesRoot = compilerRoot </> fixturesRelativeRoot
      compilerRelativeFixturePath = fixturesRelativeRoot </> fixtureRelativePath fixtureCase
      fixturePath = compilerRoot </> compilerRelativeFixturePath
      snapshotPath = fixturesRoot </> "Snapshots" </> fixtureSnapshotFile fixtureCase

  refreshSnapshots <-
    either (assertFailure . renderSnapshotFileFailure) pure
      =<< snapshotRefreshEnabled
  result <-
    expectRight
      =<< compileFixtures session fixturePackageIds (fixturePath :| [])

  let actualSnapshot =
        normalizeSnapshot compilerRoot compilerRelativeFixturePath result

  assertEqual
    "fixture exit mode mismatch"
    (fixtureExpectedExit fixtureCase)
    (snapshotExit actualSnapshot)

  case snapshotExit actualSnapshot of
    SnapshotSuccess ->
      assertBool
        "compile-pass fixture must emit zero structured error diagnostics"
        (null (snapshotDiagnostics actualSnapshot))
    SnapshotFailure ->
      assertBool
        "compile-fail fixture must emit at least one structured error diagnostic"
        (not (null (snapshotDiagnostics actualSnapshot)))

  if refreshSnapshots
    then
      writeSnapshot snapshotPath actualSnapshot
        >>= either (assertFailure . renderSnapshotFileFailure) pure
    else do
      expectedSnapshotResult <- readSnapshot snapshotPath
      expectedSnapshot <-
        case expectedSnapshotResult of
          Left snapshotFailure ->
            assertFailure (renderSnapshotFileFailure snapshotFailure)
          Right snapshotValue -> pure snapshotValue

      assertEqual
        ( "snapshot mismatch for fixture: "
            <> fixtureRelativePath fixtureCase
            <> "\n"
            <> renderFixtureFailure result
        )
        expectedSnapshot
        actualSnapshot

acquireCompileContext :: IO (FilePath, CompileDiagnosticsSession)
acquireCompileContext = do
  compilerRootResult <-
    resolveCompilerRoot
      ( "foundation"
          </> "moonlight-homology"
          </> "moonlight-homology.cabal"
      )
  case compilerRootResult of
    Left errorMessage -> assertFailure (renderResourcePathError errorMessage)
    Right compilerRoot -> do
      session <- expectRight =<< openCompileDiagnosticsSession compilerRoot
      pure (compilerRoot, session)

-- Unit ids, not names: a bare name leaves every other exposed version of the
-- same package visible, which reports as an ambiguous module rather than as a
-- fixture verdict.
fixturePackageIds :: [GhcPackageSpec]
fixturePackageIds =
  [GhcPackageId "moonlight-homology-0.1.0.3-inplace"]

expectRight ::
  Show left =>
  Either left right ->
  IO right
expectRight result =
  case result of
    Left failureValue ->
      assertFailure (show failureValue)
    Right value ->
      pure value

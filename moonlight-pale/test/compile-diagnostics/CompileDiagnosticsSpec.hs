{-# LANGUAGE OverloadedStrings #-}

module CompileDiagnosticsSpec
  ( tests,
  )
where

import Data.Aeson (decode, encode)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Pale.Test.Assertions (expectRightWithLabel)
import Moonlight.Pale.TestSupport.CompileDiagnostics
  ( CompileDiagnosticsSession,
    CompileFixtureFailure (..),
    DiagnosticSnapshot (..),
    GhcPackageSpec (..),
    NormalizedDiagnostic (..),
    SnapshotExit (..),
    UnstructuredCompileFailure (..),
    compileFixtures,
    normalizeSnapshot,
    openCompileDiagnosticsSession,
  )
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  withResource acquireCompileContext (const (pure ())) $ \getCompileContext ->
    testGroup
      "Moonlight.Pale.TestSupport.CompileDiagnostics"
      [ testCase "compileFixtures captures a round-trippable clean snapshot" $
          compileTrivialFixture getCompileContext,
        testCase "compileFixtures preserves unstructured failures" $
          compileUnstructuredFailure getCompileContext,
        testCase "snapshot JSON establishes canonical diagnostic order" $
          assertCanonicalSnapshotRoundTrip
      ]

compileTrivialFixture :: IO (FilePath, CompileDiagnosticsSession) -> IO ()
compileTrivialFixture getCompileContext = do
  (packageRoot, session) <- getCompileContext
  compileResult <- compileFixtures session [] (packageRelativeFixturePath :| [])
  fixtureResult <- expectRightWithLabel "compile fixture" compileResult
  let snapshot :: DiagnosticSnapshot
      snapshot = normalizeSnapshot packageRoot packageRelativeFixturePath fixtureResult
  assertEqual "clean fixture exits successfully" SnapshotSuccess (snapshotExit snapshot)
  assertEqual "diagnostic snapshot JSON round-trips" (pure snapshot) (roundTripDiagnosticSnapshot snapshot)

compileUnstructuredFailure :: IO (FilePath, CompileDiagnosticsSession) -> IO ()
compileUnstructuredFailure getCompileContext = do
  (_, session) <- getCompileContext
  compileResult <-
    compileFixtures
      session
      [GhcPackageId "pale-definitely-missing-unit-id"]
      (packageRelativeFixturePath :| [])
  case compileResult of
    Left (CompileFixtureUnstructuredFailure failureValue) ->
      case unstructuredCompileExitCode failureValue of
        ExitFailure _ -> pure ()
        ExitSuccess -> assertFailure "unstructured compiler failure cannot report success"
    Left otherFailure ->
      assertFailure ("expected unstructured compiler failure, got " <> show otherFailure)
    Right fixtureResult ->
      assertFailure ("expected fixture compilation to fail, got " <> show fixtureResult)

acquireCompileContext :: IO (FilePath, CompileDiagnosticsSession)
acquireCompileContext = do
  packageRoot <- getCurrentDirectory
  session <-
    expectRightWithLabel "compile diagnostics session"
      =<< openCompileDiagnosticsSession packageRoot
  pure (packageRoot, session)

assertCanonicalSnapshotRoundTrip :: IO ()
assertCanonicalSnapshotRoundTrip =
  assertEqual
    "decoded snapshot diagnostics are canonical"
    (Just canonicalSnapshot)
    (roundTripDiagnosticSnapshot nonCanonicalSnapshot)
  where
    canonicalSnapshot =
      nonCanonicalSnapshot
        { snapshotDiagnostics = [alphaDiagnostic, betaDiagnostic]
        }
    nonCanonicalSnapshot =
      DiagnosticSnapshot
        { snapshotFixture = "Fixture.hs",
          snapshotDiagnosticsFlag = "-fdiagnostics-as-json",
          snapshotExit = SnapshotFailure,
          snapshotDiagnostics = [betaDiagnostic, alphaDiagnostic]
        }
    alphaDiagnostic =
      NormalizedDiagnostic
        { normalizedCode = "GHC-001",
          normalizedFile = "Fixture.hs",
          normalizedStartLine = 1,
          normalizedStartCol = 1,
          normalizedEndLine = 1,
          normalizedEndCol = 2
        }
    betaDiagnostic =
      NormalizedDiagnostic
        { normalizedCode = "GHC-002",
          normalizedFile = "Fixture.hs",
          normalizedStartLine = 2,
          normalizedStartCol = 1,
          normalizedEndLine = 2,
          normalizedEndCol = 2
        }

packageRelativeFixturePath :: FilePath
packageRelativeFixturePath =
  "test/compile-diagnostics/fixtures/Trivial.hs"

roundTripDiagnosticSnapshot :: DiagnosticSnapshot -> Maybe DiagnosticSnapshot
roundTripDiagnosticSnapshot snapshot =
  decode (encode snapshot)

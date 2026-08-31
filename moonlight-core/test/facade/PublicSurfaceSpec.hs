{-# LANGUAGE CPP #-}

module PublicSurfaceSpec (tests) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (void)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Text (pack)
import SourceShape (assertTopLevelDeclarationExcludesToken)
import Moonlight.Core (Refined, isQualifiedModuleName, refinedValue, spectralGap, splitModuleName)
import Moonlight.Core.Unsound (TrustJustification, unsafelyTrustRefined)
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "PublicSurface"
    [ testCase "validated public tokens do not derive Read" testValidatedPublicTokensDoNotDeriveRead,
      testCase "module identifier splitting uses the Text segment owner" testModuleIdentifierTextSegments,
      testCase "spectralGap uses the two smallest sorted values" testSpectralGapUsesSortedValues,
      testCase "default public library exposes only Moonlight.Core" testDefaultPublicLibraryExposesOnlyCore,
      testCase "default public library reexports only Moonlight.Core.Unsound" testDefaultPublicLibraryReexportsOnlyUnsound,
      testCase "named public sublibraries expose the declared direct modules" testPublicNamedSublibrarySurface,
      testCase "egraph program sublibrary is public and canonically exposed" testEGraphProgramSublibrarySurface,
      testCase "unsafelyTrustRefined forces trust justification before minting Refined" testUnsafelyTrustRefinedForcesTrustJustification
    ]

data UnsafelyTrustRefinedProbe

testUnsafelyTrustRefinedForcesTrustJustification :: IO ()
testUnsafelyTrustRefinedForcesTrustJustification =
  assertIOFails $
    evaluate $
      refinedValue
        ( unsafelyTrustRefined
            (undefined :: TrustJustification)
            (42 :: Int) ::
            Refined UnsafelyTrustRefinedProbe Int
        )

testValidatedPublicTokensDoNotDeriveRead :: IO ()
testValidatedPublicTokensDoNotDeriveRead = do
  assertTopLevelDeclarationExcludesToken
    __FILE__
    "src-basis/Moonlight/Internal/Unsound.hs"
    "newtype Refined"
    "Read"
  assertTopLevelDeclarationExcludesToken
    __FILE__
    "src-basis/Moonlight/Internal/Unsound.hs"
    "newtype IdentifierToken"
    "Read"
  assertTopLevelDeclarationExcludesToken
    __FILE__
    "src-basis/Moonlight/Core/DomainId/Internal.hs"
    "newtype DomainId"
    "Read"

testModuleIdentifierTextSegments :: IO ()
testModuleIdentifierTextSegments = do
  splitModuleName (pack "Moonlight.Core.Test") @?= fmap pack ["Moonlight", "Core", "Test"]
  isQualifiedModuleName (pack "Moonlight.Core.Test") @?= True
  isQualifiedModuleName (pack "Moonlight..Core") @?= False

testSpectralGapUsesSortedValues :: IO ()
testSpectralGapUsesSortedValues =
  spectralGap [5 :: Int, 1, 3] @?= Just 2

testDefaultPublicLibraryExposesOnlyCore :: IO ()
testDefaultPublicLibraryExposesOnlyCore = do
  cabalText <- readFile (packageRootFromTestModule __FILE__ </> "moonlight-core.cabal")
  case defaultPublicLibraryExposedModules cabalText of
    Left parseFailure ->
      assertFailure parseFailure
    Right exposedModules ->
      exposedModules @?= ["Moonlight.Core"]

testDefaultPublicLibraryReexportsOnlyUnsound :: IO ()
testDefaultPublicLibraryReexportsOnlyUnsound = do
  cabalText <- readFile (packageRootFromTestModule __FILE__ </> "moonlight-core.cabal")
  case defaultPublicLibraryReexports cabalText of
    Left parseFailure ->
      assertFailure parseFailure
    Right reexports ->
      reexports @?= ["Moonlight.Core.Unsound"]

testPublicNamedSublibrarySurface :: IO ()
testPublicNamedSublibrarySurface = do
  cabalText <- readFile (packageRootFromTestModule __FILE__ </> "moonlight-core.cabal")
  publicNamedLibraryExposedModules cabalText
    @?= Right
      [ "Moonlight.Core.EGraph.Program",
        "Moonlight.Core.Guidance",
        "Moonlight.Core.Language",
        "Moonlight.Core.Pattern",
        "Moonlight.Core.Pattern.AntiUnify",
        "Moonlight.Core.Fix.Order",
        "Moonlight.Core.Substitution",
        "Moonlight.Core.Theory",
        "Moonlight.Automata.Pure.Algebra",
        "Moonlight.Automata.Pure.Coalgebra",
        "Moonlight.Automata.Pure.Core",
        "Moonlight.Automata.Pure.Transducer",
        "Moonlight.Core.Pattern.Automata",
        "Moonlight.Core.Pattern.Kernel"
      ]

testEGraphProgramSublibrarySurface :: IO ()
testEGraphProgramSublibrarySurface = do
  cabalText <- readFile (packageRootFromTestModule __FILE__ </> "moonlight-core.cabal")
  namedLibraryFieldValues "egraph-program" "visibility" cabalText
    @?= Right ["public"]
  namedLibraryFieldValues "egraph-program" "hs-source-dirs" cabalText
    @?= Right ["src-egraph-program"]
  namedLibraryFieldValues "egraph-program" "exposed-modules" cabalText
    @?= Right ["Moonlight.Core.EGraph.Program"]

packageRootFromTestModule :: FilePath -> FilePath
packageRootFromTestModule testModulePath =
  takeDirectory $
    takeDirectory $
      takeDirectory $
        takeDirectory testModulePath

defaultPublicLibraryExposedModules :: String -> Either String [String]
defaultPublicLibraryExposedModules =
  defaultPublicLibraryFieldValues "exposed-modules"

defaultPublicLibraryReexports :: String -> Either String [String]
defaultPublicLibraryReexports =
  defaultPublicLibraryFieldValues "reexported-modules"

publicNamedLibraryExposedModules :: String -> Either String [String]
publicNamedLibraryExposedModules =
  fmap concat
    . traverse (fieldValues "exposed-modules")
    . filter isPublicNamedLibraryStanza
    . cabalStanzas
    . lines

isPublicNamedLibraryStanza :: [String] -> Bool
isPublicNamedLibraryStanza stanza =
  case stanza of
    stanzaHeader : _stanzaBody ->
      case words stanzaHeader of
        ["library", _libraryName] ->
          fieldValues "visibility" stanza == Right ["public"]
        _ -> False
    [] -> False

defaultPublicLibraryFieldValues :: String -> String -> Either String [String]
defaultPublicLibraryFieldValues fieldName cabalText =
  case filter isDefaultPublicLibraryStanza (cabalStanzas (lines cabalText)) of
    [] -> Left "expected to find the default library stanza with hs-source-dirs: src-public"
    [defaultLibraryStanza] -> fieldValues fieldName defaultLibraryStanza
    _multipleDefaultLibraries -> Left "expected exactly one default library stanza with hs-source-dirs: src-public"

cabalStanzas :: [String] -> [[String]]
cabalStanzas [] =
  []
cabalStanzas sourceLines =
  case dropWhile (not . isCabalStanzaHeader) sourceLines of
    [] -> []
    stanzaHeader : remainingLines ->
      let (stanzaBody, rest) = break isCabalStanzaHeader remainingLines
       in (stanzaHeader : stanzaBody) : cabalStanzas rest

isDefaultPublicLibraryStanza :: [String] -> Bool
isDefaultPublicLibraryStanza stanza =
  case stanza of
    stanzaHeader : stanzaBody ->
      trim stanzaHeader == "library" && any ((== "hs-source-dirs: src-public") . trim) stanzaBody
    [] -> False

namedLibraryFieldValues :: String -> String -> String -> Either String [String]
namedLibraryFieldValues libraryName fieldName cabalText =
  case filter (isNamedLibraryStanza libraryName) (cabalStanzas (lines cabalText)) of
    [] -> Left ("expected to find library stanza: " <> libraryName)
    [libraryStanza] -> fieldValues fieldName libraryStanza
    _multipleLibraries -> Left ("expected exactly one library stanza: " <> libraryName)

isNamedLibraryStanza :: String -> [String] -> Bool
isNamedLibraryStanza libraryName stanza =
  case stanza of
    stanzaHeader : _stanzaBody ->
      trim stanzaHeader == "library " <> libraryName
    [] -> False

fieldValues :: String -> [String] -> Either String [String]
fieldValues fieldName stanza =
  case dropWhile (not . isRequestedField) stanza of
    [] -> Left ("expected " <> fieldName <> " field in default library stanza")
    fieldLine : rest -> Right (fieldLineValues fieldLine <> continuationValues rest)
  where
    fieldPrefix = fieldName <> ":"
    isRequestedField line = fieldPrefix `isPrefixOf` trim line
    fieldLineValues line = fieldEntryValues (drop (length fieldPrefix) (trim line))
    continuationValues = fieldEntryValues . unlines . takeWhile (not . isCabalFieldStart)

fieldEntryValues :: String -> [String]
fieldEntryValues fieldText =
  filter (not . null) $
    fmap (trim . dropWhile (== ',') . trim) $
      lines fieldText

isCabalFieldStart :: String -> Bool
isCabalFieldStart sourceLine =
  case sourceLine of
    ' ' : ' ' : nextCharacter : _ -> nextCharacter /= ' ' && ':' `elem` sourceLine
    _ -> isCabalStanzaHeader sourceLine

isCabalStanzaHeader :: String -> Bool
isCabalStanzaHeader sourceLine =
  case words sourceLine of
    stanzaKind : _ | not (startsWithSpace sourceLine) -> stanzaKind `elem` cabalStanzaKinds
    _ -> False

cabalStanzaKinds :: [String]
cabalStanzaKinds =
  [ "benchmark",
    "common",
    "executable",
    "flag",
    "library",
    "test-suite"
  ]

startsWithSpace :: String -> Bool
startsWithSpace sourceLine =
  case sourceLine of
    leadingCharacter : _ -> isSpace leadingCharacter
    [] -> False

assertIOFails :: IO value -> IO ()
assertIOFails action = do
  result <- try (void action) :: IO (Either SomeException ())
  case result of
    Left _expectedFailure -> pure ()
    Right () -> assertFailure "expected IO action to fail"

trim :: String -> String
trim =
  trimRight . trimLeft

trimLeft :: String -> String
trimLeft =
  dropWhile isSpace

trimRight :: String -> String
trimRight =
  reverse . trimLeft . reverse

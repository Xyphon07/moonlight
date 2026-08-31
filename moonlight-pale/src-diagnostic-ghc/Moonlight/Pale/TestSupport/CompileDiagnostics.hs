{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Typed GHC diagnostic capture, normalization, and snapshot persistence for tests. -}
module Moonlight.Pale.TestSupport.CompileDiagnostics
  ( SnapshotExit (..),
    DiagnosticsFlag (..),
    DiagnosticsFlagSelectionFailure (..),
    DiagnosticStream (..),
    DiagnosticParseFailureReason (..),
    DiagnosticParseFailure (..),
    CompileFixtureFailure (..),
    UnstructuredCompileFailure (..),
    ProcessInvocationFailure (..),
    SnapshotFileFailure (..),
    GhcPackageSpec (..),
    NormalizedDiagnostic (..),
    DiagnosticSnapshot (..),
    FixtureCompileResult (..),
    CompileDiagnosticsSession,
    CompileSessionFailure (..),
    openCompileDiagnosticsSession,
    compileFixtures,
    normalizeSnapshot,
    readSnapshot,
    writeSnapshot,
    snapshotRefreshEnabled,
    renderSnapshotFileFailure,
    renderFixtureFailure,
    ResourcePath.ResourcePathError (..),
    ResourcePath.renderResourcePathError,
    resolveCompilerRoot,
  )
where

import Control.Applicative ((<|>))
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    displayException,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (join)
import Data.Bifunctor (first)
import Data.Aeson
  ( FromJSON (..),
    Object,
    Value (..),
    ToJSON (..),
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.KeyMap qualified as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isSpace)
import Data.Kind (Type)
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Moonlight.Pale.Test.Resources as ResourcePath
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (makeRelative, normalise, takeDirectory)
import System.Process (CreateProcess (cwd), proc, readCreateProcessWithExitCode)

type SnapshotExit :: Type
data SnapshotExit
  = SnapshotSuccess
  | SnapshotFailure
  deriving stock (Eq, Show)

type GhcPackageSpec :: Type
data GhcPackageSpec
  = GhcPackageName !String
  | GhcPackageId !String
  deriving stock (Eq, Show)

type DiagnosticsFlag :: Type
data DiagnosticsFlag
  = DiagnosticsAsJson
  | DiagnosticsJson
  | DumpJson
  deriving stock (Bounded, Enum, Eq, Show)

type DiagnosticsFlagSelectionFailure :: Type
data DiagnosticsFlagSelectionFailure = DiagnosticsFlagSelectionFailure
  { diagnosticsFlagSelectionExitCode :: !ExitCode,
    diagnosticsFlagSelectionObservedOptions :: ![String]
  }
  deriving stock (Eq, Show)

type CompileSessionFailure :: Type
data CompileSessionFailure
  = CompileSessionDiagnosticsFlagSelectionFailed !DiagnosticsFlagSelectionFailure
  | CompileSessionResourceDiscoveryFailed !ResourcePath.ResourcePathError
  | CompileSessionProcessInvocationFailed !ProcessInvocationFailure
  deriving stock (Eq, Show)

type ProcessInvocationFailure :: Type
data ProcessInvocationFailure = ProcessInvocationFailure
  { processInvocationCommand :: !FilePath,
    processInvocationArguments :: ![String],
    processInvocationException :: !String
  }
  deriving stock (Eq, Show)

type CompileDiagnosticsSession :: Type
data CompileDiagnosticsSession = CompileDiagnosticsSession
  { cdsCompilerRoot :: !FilePath,
    cdsBuildDirectory :: !(Maybe FilePath),
    cdsDiagnosticsFlag :: !DiagnosticsFlag
  }
  deriving stock (Eq, Show)

type DiagnosticStream :: Type
data DiagnosticStream
  = DiagnosticStdout
  | DiagnosticStderr
  deriving stock (Eq, Show)

type DiagnosticParseFailureReason :: Type
data DiagnosticParseFailureReason
  = DiagnosticLineMalformedJson !String
  | DiagnosticLineMalformedPayload !String
  deriving stock (Eq, Show)

type DiagnosticParseFailure :: Type
data DiagnosticParseFailure = DiagnosticParseFailure
  { diagnosticParseFailureStream :: !DiagnosticStream,
    diagnosticParseFailureLineNumber :: !Int,
    diagnosticParseFailureLine :: !String,
    diagnosticParseFailureReason :: !DiagnosticParseFailureReason
  }
  deriving stock (Eq, Show)

type CompileFixtureFailure :: Type
data CompileFixtureFailure
  = CompileFixtureDiagnosticParseFailed ![DiagnosticParseFailure]
  | CompileFixtureUnstructuredFailure !UnstructuredCompileFailure
  | CompileFixtureProcessInvocationFailed !ProcessInvocationFailure
  deriving stock (Eq, Show)

type UnstructuredCompileFailure :: Type
data UnstructuredCompileFailure = UnstructuredCompileFailure
  { unstructuredCompileExitCode :: !ExitCode,
    unstructuredCompileStdout :: !String,
    unstructuredCompileStderr :: !String
  }
  deriving stock (Eq, Show)

type DiagnosticParseResult :: Type
data DiagnosticParseResult = DiagnosticParseResult
  { diagnosticParseResultFailures :: ![DiagnosticParseFailure],
    diagnosticParseResultDiagnostics :: ![GhcDiagnostic]
  }
  deriving stock (Eq, Show)

type DiagnosticPayloadKey :: Type
data DiagnosticPayloadKey
  = DiagnosticPayloadMessageClass
  | DiagnosticPayloadSeverity
  | DiagnosticPayloadSpan
  | DiagnosticPayloadCode
  | DiagnosticPayloadReason
  | DiagnosticPayloadDoc
  deriving stock (Bounded, Enum, Eq, Show)

instance Semigroup DiagnosticParseResult where
  leftResult <> rightResult =
    DiagnosticParseResult
      { diagnosticParseResultFailures =
          diagnosticParseResultFailures leftResult
            <> diagnosticParseResultFailures rightResult,
        diagnosticParseResultDiagnostics =
          diagnosticParseResultDiagnostics leftResult
            <> diagnosticParseResultDiagnostics rightResult
      }

instance Monoid DiagnosticParseResult where
  mempty =
    DiagnosticParseResult
      { diagnosticParseResultFailures = [],
        diagnosticParseResultDiagnostics = []
      }

instance FromJSON SnapshotExit where
  parseJSON =
    withText "SnapshotExit" $ \value ->
      case value of
        "success" -> pure SnapshotSuccess
        "failure" -> pure SnapshotFailure
        _ -> fail ("unsupported snapshot exit value: " <> Text.unpack value)

instance ToJSON SnapshotExit where
  toJSON snapshotExitValue =
    case snapshotExitValue of
      SnapshotSuccess -> "success"
      SnapshotFailure -> "failure"

type DiagnosticSpan :: Type
data DiagnosticSpan = DiagnosticSpan
  { spanFile :: !FilePath,
    spanStartLine :: !Int,
    spanStartCol :: !Int,
    spanEndLine :: !Int,
    spanEndCol :: !Int
  }
  deriving stock (Eq, Show)

instance FromJSON DiagnosticSpan where
  parseJSON =
    withObject "DiagnosticSpan" $ \diagnosticObject ->
      do
        spanFilePath <- diagnosticObject .: "file"
        startLineValue <- coordinateValue diagnosticObject "startLine" "start" "line"
        startColValue <- coordinateValue diagnosticObject "startCol" "start" "column"
        endLineValue <- coordinateValue diagnosticObject "endLine" "end" "line"
        endColValue <- coordinateValue diagnosticObject "endCol" "end" "column"
        pure
          DiagnosticSpan
            { spanFile = spanFilePath,
              spanStartLine = startLineValue,
              spanStartCol = startColValue,
              spanEndLine = endLineValue,
              spanEndCol = endColValue
            }
    where
      coordinateValue ::
        FromJSON coordinate =>
        Object ->
        Key.Key ->
        Key.Key ->
        Key.Key ->
        Parser coordinate
      coordinateValue diagnosticObject flatKey positionKey coordinateKey = do
        flatValue <- diagnosticObject .:? flatKey
        case flatValue of
          Just value -> pure value
          Nothing -> diagnosticObject .: positionKey >>= (.: coordinateKey)

type GhcDiagnostic :: Type
data GhcDiagnostic = GhcDiagnostic
  { diagnosticSpan :: !(Maybe DiagnosticSpan),
    diagnosticClass :: !Text,
    diagnosticSeverity :: !(Maybe Text),
    diagnosticCodeText :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance FromJSON GhcDiagnostic where
  parseJSON =
    withObject "GhcDiagnostic" $ \diagnosticObject ->
      do
        messageClassValue <- diagnosticObject .:? "messageClass"
        severityValue <- diagnosticObject .:? "severity"
        spanValue <- diagnosticObject .:? "span"
        rawCodeValue <- diagnosticObject .:? "code" :: Parser (Maybe Value)
        let codeValue =
              rawCodeValue >>= \codeValue' ->
                case codeValue' of
                  String textValue -> Just ("GHC-" <> textValue)
                  Number numericValue ->
                    Just
                      ( "GHC-"
                          <> Text.takeWhile (/= '.') (Text.pack (show numericValue))
                      )
                  _ -> Nothing
        pure
          GhcDiagnostic
            { diagnosticSpan = spanValue,
              diagnosticClass = fromMaybe "" messageClassValue,
              diagnosticSeverity = severityValue,
              diagnosticCodeText = codeValue
            }

type NormalizedDiagnostic :: Type
data NormalizedDiagnostic = NormalizedDiagnostic
  { normalizedCode :: !Text,
    normalizedFile :: !FilePath,
    normalizedStartLine :: !Int,
    normalizedStartCol :: !Int,
    normalizedEndLine :: !Int,
    normalizedEndCol :: !Int
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON NormalizedDiagnostic where
  parseJSON =
    withObject "NormalizedDiagnostic" $ \diagnosticObject ->
      NormalizedDiagnostic
        <$> diagnosticObject .: "code"
        <*> diagnosticObject .: "file"
        <*> diagnosticObject .: "startLine"
        <*> diagnosticObject .: "startCol"
        <*> diagnosticObject .: "endLine"
        <*> diagnosticObject .: "endCol"

instance ToJSON NormalizedDiagnostic where
  toJSON normalizedDiagnostic =
    object
      [ "code" .= normalizedCode normalizedDiagnostic,
        "file" .= normalizedFile normalizedDiagnostic,
        "startLine" .= normalizedStartLine normalizedDiagnostic,
        "startCol" .= normalizedStartCol normalizedDiagnostic,
        "endLine" .= normalizedEndLine normalizedDiagnostic,
        "endCol" .= normalizedEndCol normalizedDiagnostic
      ]

type DiagnosticSnapshot :: Type
data DiagnosticSnapshot = DiagnosticSnapshot
  { snapshotFixture :: !FilePath,
    snapshotDiagnosticsFlag :: !String,
    snapshotExit :: !SnapshotExit,
    snapshotDiagnostics :: ![NormalizedDiagnostic]
  }
  deriving stock (Eq, Show)

type SnapshotFileFailure :: Type
data SnapshotFileFailure
  = SnapshotFileMissing !FilePath
  | SnapshotDecodeFailed !FilePath !String
  | SnapshotFilesystemFailed !FilePath !String
  deriving stock (Eq, Show)

instance FromJSON DiagnosticSnapshot where
  parseJSON =
    withObject "DiagnosticSnapshot" $ \diagnosticObject ->
      DiagnosticSnapshot
        <$> diagnosticObject .: "fixture"
        <*> diagnosticObject .: "diagnosticsFlag"
        <*> diagnosticObject .: "exit"
        <*> (sort <$> diagnosticObject .: "diagnostics")

instance ToJSON DiagnosticSnapshot where
  toJSON diagnosticSnapshot =
    object
      [ "fixture" .= snapshotFixture diagnosticSnapshot,
        "diagnosticsFlag" .= snapshotDiagnosticsFlag diagnosticSnapshot,
        "exit" .= snapshotExit diagnosticSnapshot,
        "diagnostics" .= sort (snapshotDiagnostics diagnosticSnapshot)
      ]

type FixtureCompileResult :: Type
data FixtureCompileResult = FixtureCompileResult
  { fixtureExitCode :: !ExitCode,
    fixtureStdout :: !String,
    fixtureStderr :: !String,
    fixtureDiagnostics :: ![GhcDiagnostic],
    diagnosticsFlag :: !DiagnosticsFlag
  }
  deriving stock (Eq, Show)

openCompileDiagnosticsSession ::
  FilePath ->
  IO (Either CompileSessionFailure CompileDiagnosticsSession)
openCompileDiagnosticsSession compilerRoot = do
  buildDirectoryResult <- ResourcePath.findActiveCabalBuildDirectory
  case buildDirectoryResult of
    Left resourceFailure ->
      pure (Left (CompileSessionResourceDiscoveryFailed resourceFailure))
    Right buildDirectory -> do
      let cabalArguments =
            diagnosticsFlagArguments buildDirectory
      diagnosticsFlagIoResult <-
        trySynchronous
          (processInvocationFailure "cabal" cabalArguments)
          (resolveDiagnosticsFlagWithBuildDirectory buildDirectory compilerRoot)
      pure
        ( case diagnosticsFlagIoResult of
            Left processFailure ->
              Left (CompileSessionProcessInvocationFailed processFailure)
            Right diagnosticsFlagResult ->
              first
                CompileSessionDiagnosticsFlagSelectionFailed
                ( fmap
                    (CompileDiagnosticsSession compilerRoot buildDirectory)
                    diagnosticsFlagResult
                )
        )

compileFixtures ::
  CompileDiagnosticsSession ->
  [GhcPackageSpec] ->
  NonEmpty FilePath ->
  IO (Either CompileFixtureFailure FixtureCompileResult)
compileFixtures session packageSpecs fixturePaths = do
  let selectedFlag = cdsDiagnosticsFlag session
      compilerRoot = cdsCompilerRoot session
      cabalArguments =
        cabalArgumentsForBuildDirectory
          (cdsBuildDirectory session)
          (ghcInvocation packageSpecs selectedFlag (NonEmpty.toList fixturePaths))
  processResult <-
    trySynchronous
      (processInvocationFailure "cabal" cabalArguments)
      ( readCreateProcessWithExitCode
          ( (proc "cabal" cabalArguments)
              {cwd = Just compilerRoot}
          )
          ""
      )
  pure
    ( case processResult of
        Left processFailure ->
          Left (CompileFixtureProcessInvocationFailed processFailure)
        Right (exitCode, stdoutText, stderrText) ->
          case diagnosticParseResultEither
            ( parseDiagnostics DiagnosticStdout stdoutText
                <> parseDiagnostics DiagnosticStderr stderrText
            ) of
            Left parseFailures ->
              Left (CompileFixtureDiagnosticParseFailed parseFailures)
            Right diagnostics
              | ExitFailure _ <- exitCode,
                null (normalizeErrorDiagnostics compilerRoot diagnostics) ->
                  Left
                    ( CompileFixtureUnstructuredFailure
                        UnstructuredCompileFailure
                          { unstructuredCompileExitCode = exitCode,
                            unstructuredCompileStdout = stdoutText,
                            unstructuredCompileStderr = stderrText
                          }
                    )
            Right diagnostics ->
              Right
                FixtureCompileResult
                  { fixtureExitCode = exitCode,
                    fixtureStdout = stdoutText,
                    fixtureStderr = stderrText,
                    fixtureDiagnostics = diagnostics,
                    diagnosticsFlag = selectedFlag
                  }
    )

-- A snapshot names one fixture, so diagnostics from the other fixtures of a
-- multi-fixture invocation are not part of it. The scoping is a no-op for a
-- singleton invocation; it is what makes a batched one attributable.
normalizeSnapshot :: FilePath -> FilePath -> FixtureCompileResult -> DiagnosticSnapshot
normalizeSnapshot compilerRoot fixtureRelativePath' result =
  DiagnosticSnapshot
    { snapshotFixture = normalizedFixture,
      snapshotDiagnosticsFlag = diagnosticsFlagArgument (diagnosticsFlag result),
      snapshotExit = toSnapshotExit (fixtureExitCode result),
      snapshotDiagnostics =
        filter
          ((== normalizedFixture) . normalizedFile)
          (normalizeErrorDiagnostics compilerRoot (fixtureDiagnostics result))
    }
  where
    normalizedFixture = normalizeRelativePath fixtureRelativePath'

readSnapshot :: FilePath -> IO (Either SnapshotFileFailure DiagnosticSnapshot)
readSnapshot snapshotPath =
  fmap join $
    trySynchronous
      (SnapshotFilesystemFailed snapshotPath . displayException)
      ( do
          exists <- doesFileExist snapshotPath
          if exists
            then do
              payload <- ByteString.readFile snapshotPath
              pure
                ( first
                    (SnapshotDecodeFailed snapshotPath)
                    (eitherDecodeStrict' payload)
                )
            else
              pure (Left (SnapshotFileMissing snapshotPath))
      )

writeSnapshot ::
  FilePath ->
  DiagnosticSnapshot ->
  IO (Either SnapshotFileFailure ())
writeSnapshot snapshotPath snapshot =
  trySynchronous
    (SnapshotFilesystemFailed snapshotPath . displayException)
    ( do
        createDirectoryIfMissing True (takeDirectory snapshotPath)
        LazyByteString.writeFile snapshotPath (encode snapshot)
    )

snapshotRefreshEnabled :: IO (Either SnapshotFileFailure Bool)
snapshotRefreshEnabled =
  trySynchronous
    (SnapshotFilesystemFailed "UPDATE_SNAPSHOTS" . displayException)
    ((== Just "1") <$> lookupEnv "UPDATE_SNAPSHOTS")

renderSnapshotFileFailure :: SnapshotFileFailure -> String
renderSnapshotFileFailure = \case
  SnapshotFileMissing snapshotPath ->
    "missing snapshot file: " <> snapshotPath
  SnapshotDecodeFailed snapshotPath decodeError ->
    "failed to decode snapshot file: " <> snapshotPath <> "\n" <> decodeError
  SnapshotFilesystemFailed snapshotPath exceptionText ->
    "snapshot filesystem failure at " <> snapshotPath <> ": " <> exceptionText

renderFixtureFailure :: FixtureCompileResult -> String
renderFixtureFailure result =
  "diagnostics flag: "
    <> diagnosticsFlagArgument (diagnosticsFlag result)
    <> "\nstdout:\n"
    <> fixtureStdout result
    <> "\nstderr:\n"
    <> fixtureStderr result

resolveCompilerRoot ::
  FilePath ->
  IO (Either ResourcePath.ResourcePathError FilePath)
resolveCompilerRoot = ResourcePath.resolveCompilerRoot

diagnosticsFlagArgument :: DiagnosticsFlag -> String
diagnosticsFlagArgument selectedFlag =
  case selectedFlag of
    DiagnosticsAsJson -> "-fdiagnostics-as-json"
    DiagnosticsJson -> "-fdiagnostics-json"
    DumpJson -> "-ddump-json"

ghcInvocation :: [GhcPackageSpec] -> DiagnosticsFlag -> [FilePath] -> [String]
ghcInvocation packageSpecs selectedFlag fixturePaths =
  [ "exec",
    "--",
    "ghc",
    "-fforce-recomp",
    "-fno-code"
  ]
    <> concatMap renderGhcPackageSpec packageSpecs
    <> [diagnosticsFlagArgument selectedFlag]
    <> fixturePaths

renderGhcPackageSpec :: GhcPackageSpec -> [String]
renderGhcPackageSpec packageSpec =
  case packageSpec of
    GhcPackageName packageName ->
      ["-package", packageName]
    GhcPackageId packageId ->
      ["-package-id", packageId]

resolveDiagnosticsFlagWithBuildDirectory ::
  Maybe FilePath ->
  FilePath ->
  IO (Either DiagnosticsFlagSelectionFailure DiagnosticsFlag)
resolveDiagnosticsFlagWithBuildDirectory buildDirectory compilerRoot = do
  let cabalArguments = diagnosticsFlagArguments buildDirectory
  (exitCode, stdoutText, stderrText) <-
    readCreateProcessWithExitCode
      ((proc "cabal" cabalArguments) {cwd = Just compilerRoot})
      ""
  let optionLines = lines stdoutText <> lines stderrText
  pure (selectedDiagnosticsFlag exitCode optionLines)

diagnosticsFlagArguments :: Maybe FilePath -> [String]
diagnosticsFlagArguments buildDirectory =
  cabalArgumentsForBuildDirectory
    buildDirectory
    ["exec", "--", "ghc", "--show-options"]

processInvocationFailure ::
  FilePath ->
  [String] ->
  SomeException ->
  ProcessInvocationFailure
processInvocationFailure commandPath commandArguments exceptionValue =
  ProcessInvocationFailure
    { processInvocationCommand = commandPath,
      processInvocationArguments = commandArguments,
      processInvocationException = displayException exceptionValue
    }

trySynchronous ::
  (SomeException -> failure) ->
  IO value ->
  IO (Either failure value)
trySynchronous toFailure action = do
  result <- try action
  case result of
    Left exceptionValue
      | Just asyncException <-
          (fromException exceptionValue :: Maybe SomeAsyncException) ->
          throwIO asyncException
      | otherwise ->
          pure (Left (toFailure exceptionValue))
    Right value ->
      pure (Right value)

selectedDiagnosticsFlag ::
  ExitCode ->
  [String] ->
  Either DiagnosticsFlagSelectionFailure DiagnosticsFlag
selectedDiagnosticsFlag exitCode optionLines =
  case find (`diagnosticsFlagIsObservedIn` optionLines) diagnosticsFlagPriority of
    Just selectedFlag -> Right selectedFlag
    Nothing ->
      Left
        DiagnosticsFlagSelectionFailure
          { diagnosticsFlagSelectionExitCode = exitCode,
            diagnosticsFlagSelectionObservedOptions = optionLines
          }

diagnosticsFlagPriority :: [DiagnosticsFlag]
diagnosticsFlagPriority = [minBound .. maxBound]

diagnosticsFlagIsObservedIn :: DiagnosticsFlag -> [String] -> Bool
diagnosticsFlagIsObservedIn selectedFlag optionLines =
  diagnosticsFlagArgument selectedFlag `elem` optionLines

cabalArgumentsForBuildDirectory :: Maybe FilePath -> [String] -> [String]
cabalArgumentsForBuildDirectory maybeBuildDirectory commandArguments =
  maybe
    commandArguments
    (\buildDirectory -> ("--builddir=" <> buildDirectory) : commandArguments)
    maybeBuildDirectory

parseDiagnostics :: DiagnosticStream -> String -> DiagnosticParseResult
parseDiagnostics streamName =
  foldMap (decodeDiagnosticLine streamName)
    . zip [1 ..]
    . lines

diagnosticParseResultEither :: DiagnosticParseResult -> Either [DiagnosticParseFailure] [GhcDiagnostic]
diagnosticParseResultEither result =
  case diagnosticParseResultFailures result of
    [] -> Right (diagnosticParseResultDiagnostics result)
    parseFailures -> Left parseFailures

decodeDiagnosticLine :: DiagnosticStream -> (Int, String) -> DiagnosticParseResult
decodeDiagnosticLine streamName (lineNumber, line) =
  case eitherDecodeStrict' (ByteStringChar8.pack line) of
    Left jsonError ->
      if looksLikeJsonObjectLine line
        then diagnosticLineFailure streamName lineNumber line (DiagnosticLineMalformedJson jsonError)
        else mempty
    Right value ->
      if looksLikeDiagnosticValue value
        then decodeDiagnosticPayload streamName lineNumber line value
        else mempty

decodeDiagnosticPayload :: DiagnosticStream -> Int -> String -> Value -> DiagnosticParseResult
decodeDiagnosticPayload streamName lineNumber line value =
  case parseEither parseJSON value of
    Left payloadError ->
      diagnosticLineFailure streamName lineNumber line (DiagnosticLineMalformedPayload payloadError)
    Right diagnostic ->
      diagnosticLineSuccess diagnostic

diagnosticLineFailure ::
  DiagnosticStream ->
  Int ->
  String ->
  DiagnosticParseFailureReason ->
  DiagnosticParseResult
diagnosticLineFailure streamName lineNumber line reason =
  DiagnosticParseResult
    { diagnosticParseResultFailures =
        [ DiagnosticParseFailure
            { diagnosticParseFailureStream = streamName,
              diagnosticParseFailureLineNumber = lineNumber,
              diagnosticParseFailureLine = line,
              diagnosticParseFailureReason = reason
            }
        ],
      diagnosticParseResultDiagnostics = []
    }

diagnosticLineSuccess :: GhcDiagnostic -> DiagnosticParseResult
diagnosticLineSuccess diagnostic =
  DiagnosticParseResult
    { diagnosticParseResultFailures = [],
      diagnosticParseResultDiagnostics = [diagnostic]
    }

looksLikeJsonObjectLine :: String -> Bool
looksLikeJsonObjectLine line =
  case dropWhile isSpace line of
    '{' : _ -> True
    _ -> False

looksLikeDiagnosticValue :: Value -> Bool
looksLikeDiagnosticValue value =
  case value of
    Object diagnosticObject ->
      any (`KeyMap.member` diagnosticObject) diagnosticPayloadKeys
    _ -> False

diagnosticPayloadKeys :: [Key.Key]
diagnosticPayloadKeys = fmap diagnosticPayloadKeyName [minBound .. maxBound]

diagnosticPayloadKeyName :: DiagnosticPayloadKey -> Key.Key
diagnosticPayloadKeyName payloadKey =
  case payloadKey of
    DiagnosticPayloadMessageClass -> "messageClass"
    DiagnosticPayloadSeverity -> "severity"
    DiagnosticPayloadSpan -> "span"
    DiagnosticPayloadCode -> "code"
    DiagnosticPayloadReason -> "reason"
    DiagnosticPayloadDoc -> "doc"

normalizeDiagnostic :: FilePath -> GhcDiagnostic -> Maybe NormalizedDiagnostic
normalizeDiagnostic compilerRoot diagnostic =
  case (diagnosticCode diagnostic, diagnosticSpan diagnostic) of
    (Just code, Just spanValue) ->
      Just
        NormalizedDiagnostic
          { normalizedCode = code,
            normalizedFile = normalizeRelativePath (makeRelative compilerRoot (spanFile spanValue)),
            normalizedStartLine = spanStartLine spanValue,
            normalizedStartCol = spanStartCol spanValue,
            normalizedEndLine = spanEndLine spanValue,
            normalizedEndCol = spanEndCol spanValue
          }
    _ -> Nothing

normalizeErrorDiagnostics :: FilePath -> [GhcDiagnostic] -> [NormalizedDiagnostic]
normalizeErrorDiagnostics compilerRoot =
  sort . mapMaybe (normalizeDiagnostic compilerRoot) . errorDiagnostics

toSnapshotExit :: ExitCode -> SnapshotExit
toSnapshotExit exitCode =
  case exitCode of
    ExitSuccess -> SnapshotSuccess
    ExitFailure _ -> SnapshotFailure

errorDiagnostics :: [GhcDiagnostic] -> [GhcDiagnostic]
errorDiagnostics = filter isSevError

isSevError :: GhcDiagnostic -> Bool
isSevError diagnostic =
  case diagnosticSeverity diagnostic of
    Just severityValue -> severityValue == "Error"
    Nothing ->
      Text.isInfixOf "MCDiagnostic" (diagnosticClass diagnostic)
        && Text.isInfixOf "SevError" (diagnosticClass diagnostic)

diagnosticCode :: GhcDiagnostic -> Maybe Text
diagnosticCode diagnostic =
  diagnosticCodeText diagnostic
    <|> find (Text.isPrefixOf "GHC-") (Text.words (diagnosticClass diagnostic))

normalizeRelativePath :: FilePath -> FilePath
normalizeRelativePath = normalise

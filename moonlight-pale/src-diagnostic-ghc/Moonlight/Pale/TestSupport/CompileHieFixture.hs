{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

{-| Isolated compilation of Haskell source into HIE artifacts and name oracles. -}
module Moonlight.Pale.TestSupport.CompileHieFixture
  ( HieFixtureModuleName,
    mkHieFixtureModuleName,
    CompileHieFixtureFailure (..),
    CompiledHieFixture (..),
    compileHieFixture,
  )
where

import Control.Exception (IOException, displayException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isAlphaNum, isUpper)
import Data.Kind (Type)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..))
import Moonlight.Pale.Ghc.Hie.Read (HieReadError, indexHieRoots)
import Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieSourceKeyKind,
    hieArtifactOracle,
    OracleLookup (..),
    OracleQuery (..),
    TriedKey,
    lookupModuleOracle,
  )
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    findExecutable,
    listDirectory,
    makeAbsolute,
  )
import System.Exit (ExitCode (..))
import System.FilePath
  ( joinPath,
    normalise,
    takeDirectory,
    takeExtension,
    (<.>),
    (</>),
  )
import System.IO (IOMode (WriteMode), withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
  ( CreateProcess (std_err, std_out),
    StdStream (UseHandle),
    proc,
    waitForProcess,
    withCreateProcess,
  )

type HieFixtureModuleName :: Type
newtype HieFixtureModuleName = HieFixtureModuleName (NonEmpty Text)
  deriving stock (Eq, Show)

type CompileHieFixtureFailure :: Type
data CompileHieFixtureFailure
  = CompileHieFixtureInvalidModuleName !String
  | CompileHieFixtureGhcNotFound
  | CompileHieFixtureProcessLaunchFailed !FilePath !(NonEmpty String) !String
  | CompileHieFixtureProcessFailed !FilePath !(NonEmpty String) !ExitCode !ByteString !ByteString
  | CompileHieFixtureHieDecoderFailed !(NonEmpty HieReadError)
  | CompileHieFixtureOracleMissing ![TriedKey]
  | CompileHieFixtureOracleAmbiguous !HieSourceKeyKind !FilePath ![FilePath]
  | CompileHieFixtureOracleIndexObstruction ![Int]
  | CompileHieFixtureHieFileMissing !FilePath
  | CompileHieFixtureMultipleHieFiles !FilePath !FilePath ![FilePath]
  | CompileHieFixtureSourcePathDisagreement !FilePath !FilePath
  | CompileHieFixtureIoFailed !String
  deriving stock (Eq, Show)

type CompiledHieFixture :: Type
data CompiledHieFixture = CompiledHieFixture
  { compiledHieFixtureSourcePath :: !FilePath,
    compiledHieFixtureSourceBytes :: !ByteString,
    compiledHieFixtureHiePath :: !FilePath,
    compiledHieFixtureHieBytes :: !ByteString,
    compiledHieFixtureOracle :: !ModuleNameOracle,
    compiledHieFixtureGhcPath :: !FilePath,
    compiledHieFixtureGhcArguments :: !(NonEmpty String),
    compiledHieFixtureStdout :: !ByteString,
    compiledHieFixtureStderr :: !ByteString
  }
  deriving stock (Eq, Show)

mkHieFixtureModuleName :: String -> Either CompileHieFixtureFailure HieFixtureModuleName
mkHieFixtureModuleName rawModuleName =
  case NonEmpty.nonEmpty (Text.splitOn (Text.singleton '.') (Text.pack rawModuleName)) of
    Just moduleComponents
      | all validModuleComponent (NonEmpty.toList moduleComponents) ->
          Right (HieFixtureModuleName moduleComponents)
    _ ->
      Left (CompileHieFixtureInvalidModuleName rawModuleName)

compileHieFixture ::
  HieFixtureModuleName ->
  ByteString ->
  IO (Either CompileHieFixtureFailure CompiledHieFixture)
compileHieFixture moduleName sourceBytes =
  captureIoFailure $ do
    maybeGhcPath <- findExecutable "ghc"
    case maybeGhcPath of
      Nothing ->
        pure (Left CompileHieFixtureGhcNotFound)
      Just discoveredGhcPath -> do
        ghcPath <- makeAbsolute discoveredGhcPath
        withSystemTempDirectory "moonlight-pale-hie-fixture" $ \temporaryRoot -> do
          canonicalRoot <- canonicalizePath temporaryRoot
          compileHieFixtureAtRoot ghcPath moduleName sourceBytes canonicalRoot

captureIoFailure ::
  IO (Either CompileHieFixtureFailure fixture) ->
  IO (Either CompileHieFixtureFailure fixture)
captureIoFailure action = do
  result <- try @IOException action
  pure
    ( case result of
        Left ioFailure -> Left (CompileHieFixtureIoFailed (displayException ioFailure))
        Right fixtureResult -> fixtureResult
    )

compileHieFixtureAtRoot ::
  FilePath ->
  HieFixtureModuleName ->
  ByteString ->
  FilePath ->
  IO (Either CompileHieFixtureFailure CompiledHieFixture)
compileHieFixtureAtRoot ghcPath moduleName sourceBytes temporaryRoot = do
  let sourceDirectory = temporaryRoot </> "src"
      hieDirectory = temporaryRoot </> "hie"
      sourcePath = sourceDirectory </> moduleSourcePath moduleName
      stdoutPath = temporaryRoot </> "ghc.stdout"
      stderrPath = temporaryRoot </> "ghc.stderr"
      ghcArguments =
        "-fno-code"
          :| [ "-fforce-recomp",
               "-fwrite-ide-info",
               "-hiedir",
               hieDirectory,
               sourcePath
             ]
  createDirectoryIfMissing True (takeDirectory sourcePath)
  createDirectoryIfMissing True hieDirectory
  ByteString.writeFile sourcePath sourceBytes
  processResult <- runGhcProcess ghcPath ghcArguments stdoutPath stderrPath
  stdoutBytes <- ByteString.readFile stdoutPath
  stderrBytes <- ByteString.readFile stderrPath
  case processResult of
    Left processLaunchFailure ->
      pure (Left processLaunchFailure)
    Right exitCode@(ExitFailure _) ->
      pure
        ( Left
            ( CompileHieFixtureProcessFailed
                ghcPath
                ghcArguments
                exitCode
                stdoutBytes
                stderrBytes
            )
        )
    Right ExitSuccess ->
      decodeCompiledFixture
        ghcPath
        ghcArguments
        sourceDirectory
        sourcePath
        hieDirectory
        stdoutBytes
        stderrBytes

runGhcProcess ::
  FilePath ->
  NonEmpty String ->
  FilePath ->
  FilePath ->
  IO (Either CompileHieFixtureFailure ExitCode)
runGhcProcess ghcPath ghcArguments stdoutPath stderrPath =
  withBinaryFile stdoutPath WriteMode $ \stdoutHandle ->
    withBinaryFile stderrPath WriteMode $ \stderrHandle -> do
      processResult <-
        try @IOException
          ( withCreateProcess
              ( (proc ghcPath (NonEmpty.toList ghcArguments))
                  { std_out = UseHandle stdoutHandle,
                    std_err = UseHandle stderrHandle
                  }
              )
              (\_ _ _ processHandle -> waitForProcess processHandle)
          )
      pure
        ( case processResult of
            Left processFailure ->
              Left
                ( CompileHieFixtureProcessLaunchFailed
                    ghcPath
                    ghcArguments
                    (displayException processFailure)
                )
            Right exitCode ->
              Right exitCode
        )

decodeCompiledFixture ::
  FilePath ->
  NonEmpty String ->
  FilePath ->
  FilePath ->
  FilePath ->
  ByteString ->
  ByteString ->
  IO (Either CompileHieFixtureFailure CompiledHieFixture)
decodeCompiledFixture ghcPath ghcArguments sourceDirectory sourcePath hieDirectory stdoutBytes stderrBytes = do
  hieFiles <- collectHieFiles hieDirectory
  case hieFiles of
    [] ->
      pure (Left (CompileHieFixtureHieFileMissing hieDirectory))
    firstHiePath : secondHiePath : remainingHiePaths ->
      pure
        ( Left
            ( CompileHieFixtureMultipleHieFiles
                firstHiePath
                secondHiePath
                remainingHiePaths
            )
        )
    [hiePath] -> do
      (hieReadErrors, oracleIndex) <- indexHieRoots [hieDirectory]
      case NonEmpty.nonEmpty hieReadErrors of
        Just decoderFailures ->
          pure (Left (CompileHieFixtureHieDecoderFailed decoderFailures))
        Nothing ->
          retainSelectedFixture
            ghcPath
            ghcArguments
            sourcePath
            hiePath
            stdoutBytes
            stderrBytes
            ( lookupModuleOracle
                oracleIndex
                OracleQuery
                  { oqGivenPath = normalise sourcePath,
                    oqAbsolutePath = Just (normalise sourcePath),
                    oqSourceRoots = [normalise sourceDirectory]
                  }
            )

retainSelectedFixture ::
  FilePath ->
  NonEmpty String ->
  FilePath ->
  FilePath ->
  ByteString ->
  ByteString ->
  OracleLookup ->
  IO (Either CompileHieFixtureFailure CompiledHieFixture)
retainSelectedFixture ghcPath ghcArguments sourcePath hiePath stdoutBytes stderrBytes oracleLookup =
  case oracleLookup of
    OracleMissing triedKeys ->
      pure (Left (CompileHieFixtureOracleMissing triedKeys))
    OracleAmbiguous keyKind keyValue candidates ->
      pure (Left (CompileHieFixtureOracleAmbiguous keyKind keyValue candidates))
    OracleIndexObstruction missingOracleIds ->
      pure (Left (CompileHieFixtureOracleIndexObstruction missingOracleIds))
    OracleFound _ artifact
      | normalise (mnoSourcePath (hieArtifactOracle artifact)) /= normalise sourcePath ->
          pure
            ( Left
                ( CompileHieFixtureSourcePathDisagreement
                    (normalise sourcePath)
                    (normalise (mnoSourcePath (hieArtifactOracle artifact)))
                )
            )
      | otherwise -> do
          retainedSourceBytes <- ByteString.readFile sourcePath
          retainedHieBytes <- ByteString.readFile hiePath
          pure
            ( Right
                CompiledHieFixture
                  { compiledHieFixtureSourcePath = normalise sourcePath,
                    compiledHieFixtureSourceBytes = retainedSourceBytes,
                    compiledHieFixtureHiePath = normalise hiePath,
                    compiledHieFixtureHieBytes = retainedHieBytes,
                    compiledHieFixtureOracle = hieArtifactOracle artifact,
                    compiledHieFixtureGhcPath = ghcPath,
                    compiledHieFixtureGhcArguments = ghcArguments,
                    compiledHieFixtureStdout = stdoutBytes,
                    compiledHieFixtureStderr = stderrBytes
                  }
            )

collectHieFiles :: FilePath -> IO [FilePath]
collectHieFiles directory = do
  entries <- sort <$> listDirectory directory
  concat <$> traverse (collectHiePath . (directory </>)) entries

collectHiePath :: FilePath -> IO [FilePath]
collectHiePath path = do
  pathIsDirectory <- doesDirectoryExist path
  if pathIsDirectory
    then collectHieFiles path
    else pure [normalise path | takeExtension path == ".hie"]

moduleSourcePath :: HieFixtureModuleName -> FilePath
moduleSourcePath (HieFixtureModuleName moduleComponents) =
  joinPath (fmap Text.unpack (NonEmpty.toList moduleComponents)) <.> "hs"

validModuleComponent :: Text -> Bool
validModuleComponent moduleComponent =
  case Text.uncons moduleComponent of
    Just (initialCharacter, remainingCharacters) ->
      isUpper initialCharacter
        && Text.all validModuleContinuationCharacter remainingCharacters
    Nothing ->
      False

validModuleContinuationCharacter :: Char -> Bool
validModuleContinuationCharacter character =
  isAlphaNum character || character == '_' || character == '\''

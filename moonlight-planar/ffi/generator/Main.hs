module Main (main) where

import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Bifunctor (first)
import Moonlight.Planar.Foreign.Contract.Render
  ( GeneratedFile (..)
  , generatedFiles
  )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (readFile')
import System.IO.Error (ioeGetErrorString, tryIOError)

data Command
  = WriteGenerated !FilePath
  | CheckGenerated !FilePath
  deriving stock (Eq, Show)

data GeneratorObstruction
  = InvalidArguments
  | GeneratedFileMissing !FilePath
  | GeneratedFileStale !FilePath
  | GeneratedFileReadFailed !FilePath !String
  | GeneratedFileWriteFailed !FilePath !String
  deriving stock (Eq, Show)

main :: IO ()
main = do
  arguments <- getArgs
  case parseCommand arguments of
    Left obstruction -> reportObstructions [obstruction]
    Right command -> runCommand command >>= either reportObstructions (const (pure ()))

parseCommand :: [String] -> Either GeneratorObstruction Command
parseCommand arguments =
  case arguments of
    ["write", packageRoot] -> Right (WriteGenerated packageRoot)
    ["check", packageRoot] -> Right (CheckGenerated packageRoot)
    _ -> Left InvalidArguments

runCommand :: Command -> IO (Either [GeneratorObstruction] ())
runCommand command =
  case command of
    WriteGenerated packageRoot -> do
      outcomes <- traverse (writeGeneratedFile packageRoot) generatedFiles
      pure (collectObstructions outcomes)
    CheckGenerated packageRoot -> do
      comparisons <- traverse (checkGeneratedFile packageRoot) generatedFiles
      pure (collectObstructions comparisons)

collectObstructions :: [Either GeneratorObstruction ()] -> Either [GeneratorObstruction] ()
collectObstructions outcomes =
  case partitionEithers outcomes of
    ([], _) -> Right ()
    (obstructions, _) -> Left obstructions

writeGeneratedFile :: FilePath -> GeneratedFile -> IO (Either GeneratorObstruction ())
writeGeneratedFile packageRoot generated@GeneratedFile {generatedFilePath, generatedFileContents} = do
  let destination = packageRoot </> generatedFilePath
  comparison <- checkGeneratedFile packageRoot generated
  case comparison of
    Right () -> putStrLn ("current " <> destination) >> pure (Right ())
    Left (GeneratedFileMissing _) -> replaceGeneratedFile destination generatedFileContents
    Left (GeneratedFileStale _) -> replaceGeneratedFile destination generatedFileContents
    Left obstruction -> pure (Left obstruction)

replaceGeneratedFile :: FilePath -> String -> IO (Either GeneratorObstruction ())
replaceGeneratedFile destination contents = do
  outcome <-
    captureFileFailure GeneratedFileWriteFailed destination $ do
      createDirectoryIfMissing True (takeDirectory destination)
      writeFile destination contents
  case outcome of
    Left obstruction -> pure (Left obstruction)
    Right () -> putStrLn ("wrote " <> destination) >> pure (Right ())

checkGeneratedFile :: FilePath -> GeneratedFile -> IO (Either GeneratorObstruction ())
checkGeneratedFile packageRoot GeneratedFile {generatedFilePath, generatedFileContents} = do
  let destination = packageRoot </> generatedFilePath
  existence <- captureFileFailure GeneratedFileReadFailed destination (doesFileExist destination)
  case existence of
    Left obstruction -> pure (Left obstruction)
    Right False -> pure (Left (GeneratedFileMissing destination))
    Right True -> do
      observed <- captureFileFailure GeneratedFileReadFailed destination (readFile' destination)
      pure $ do
        contents <- observed
        if contents == generatedFileContents
          then Right ()
          else Left (GeneratedFileStale destination)

captureFileFailure
  :: (FilePath -> String -> GeneratorObstruction)
  -> FilePath
  -> IO value
  -> IO (Either GeneratorObstruction value)
captureFileFailure obstruction path action =
  first (obstruction path . ioeGetErrorString) <$> tryIOError action

reportObstructions :: [GeneratorObstruction] -> IO ()
reportObstructions obstructions = do
  traverse_ (putStrLn . renderObstruction) obstructions
  exitFailure

renderObstruction :: GeneratorObstruction -> String
renderObstruction obstruction =
  case obstruction of
    InvalidArguments ->
      "usage: moonlight-planar-ffi-contract (write|check) PACKAGE_ROOT"
    GeneratedFileMissing path -> "missing generated file: " <> path
    GeneratedFileStale path -> "stale generated file: " <> path
    GeneratedFileReadFailed path message -> "cannot read generated file " <> path <> ": " <> message
    GeneratedFileWriteFailed path message -> "cannot write generated file " <> path <> ": " <> message

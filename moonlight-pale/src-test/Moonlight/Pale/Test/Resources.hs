{-# LANGUAGE DerivingStrategies #-}

{-| Validated discovery of compiler, package, and resource paths. -}
module Moonlight.Pale.Test.Resources
  ( ResourcePathError (..),
    renderResourcePathError,
    resolveCompilerRoot,
    findActiveCabalBuildDirectory,
    resolvePackageRoot,
    resolveCompilerFile,
    resolveCompilerDirectory,
    resolvePackageFile,
    resolvePackageDirectory,
  )
where

import Control.Exception
  ( SomeAsyncException,
    SomeException,
    displayException,
    fromException,
    throwIO,
    try,
  )
import Control.Monad (join)
import Data.Kind (Type)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, (</>))

type ResourcePathError :: Type
data ResourcePathError
  = CompilerRootNotFound FilePath
  | MissingResourceFile FilePath
  | MissingResourceDirectory FilePath
  | ResourcePathNotRelativeToRoot FilePath
  | ResourcePathEscapesRoot FilePath FilePath
  | ResourceFilesystemFailure FilePath String
  deriving stock (Eq, Show)

renderResourcePathError :: ResourcePathError -> String
renderResourcePathError resourcePathError =
  case resourcePathError of
    CompilerRootNotFound packageMarker ->
      "unable to locate compiler root with cabal.project and marker: " <> packageMarker
    MissingResourceFile filePath ->
      "missing resource file: " <> filePath
    MissingResourceDirectory directoryPath ->
      "missing resource directory: " <> directoryPath
    ResourcePathNotRelativeToRoot resourcePath ->
      "resource path is not relative to its root: " <> resourcePath
    ResourcePathEscapesRoot rootPath escapedPath ->
      "resource path escapes root " <> rootPath <> ": " <> escapedPath
    ResourceFilesystemFailure contextPath exceptionText ->
      "filesystem failure while resolving " <> contextPath <> ": " <> exceptionText

resolveCompilerRoot :: FilePath -> IO (Either ResourcePathError FilePath)
resolveCompilerRoot packageMarker =
  fmap join $
    trySynchronous packageMarker $ do
      currentDirectory <- getCurrentDirectory
      executableDirectory <- takeDirectory <$> getExecutablePath
      maybeCompilerRoot <-
        findAnyCompilerRoot
          packageMarker
          [currentDirectory, executableDirectory]
      pure
        ( case maybeCompilerRoot of
            Nothing -> Left (CompilerRootNotFound packageMarker)
            Just compilerRoot -> Right compilerRoot
        )

resolvePackageRoot :: FilePath -> IO (Either ResourcePathError FilePath)
resolvePackageRoot packageMarker =
  fmap
    (fmap (\compilerRoot -> normalise (compilerRoot </> takeDirectory packageMarker)))
    (resolveCompilerRoot packageMarker)

resolveCompilerFile :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolveCompilerFile =
  resolveExistingPath resolveCompilerRoot doesFileExist MissingResourceFile

resolveCompilerDirectory :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolveCompilerDirectory =
  resolveExistingPath resolveCompilerRoot doesDirectoryExist MissingResourceDirectory

resolvePackageFile :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolvePackageFile =
  resolveExistingPath resolvePackageRoot doesFileExist MissingResourceFile

resolvePackageDirectory :: FilePath -> FilePath -> IO (Either ResourcePathError FilePath)
resolvePackageDirectory =
  resolveExistingPath resolvePackageRoot doesDirectoryExist MissingResourceDirectory

resolveExistingPath ::
  (FilePath -> IO (Either ResourcePathError FilePath)) ->
  (FilePath -> IO Bool) ->
  (FilePath -> ResourcePathError) ->
  FilePath ->
  FilePath ->
  IO (Either ResourcePathError FilePath)
resolveExistingPath resolveRoot pathExists toMissingError packageMarker relativePath
  | not (pathRelativeToRoot relativePath) =
      pure (Left (ResourcePathNotRelativeToRoot relativePath))
  | otherwise =
      fmap join $
        trySynchronous (packageMarker </> relativePath) $ do
          rootResult <- resolveRoot packageMarker
          case rootResult of
            Left rootError -> pure (Left rootError)
            Right rootPath -> do
              canonicalRoot <- canonicalizePath rootPath
              let resolvedPath = normalise (canonicalRoot </> relativePath)
              if not (pathWithinRoot canonicalRoot resolvedPath)
                then
                  pure (Left (ResourcePathEscapesRoot canonicalRoot resolvedPath))
                else do
                  pathPresent <- pathExists resolvedPath
                  if pathPresent
                    then do
                      canonicalResolvedPath <- canonicalizePath resolvedPath
                      pure
                        ( if pathWithinRoot canonicalRoot canonicalResolvedPath
                            then Right canonicalResolvedPath
                            else Left (ResourcePathEscapesRoot canonicalRoot canonicalResolvedPath)
                        )
                    else
                      pure (Left (toMissingError resolvedPath))

findActiveCabalBuildDirectory :: IO (Either ResourcePathError (Maybe FilePath))
findActiveCabalBuildDirectory =
  trySynchronous "cache/plan.json" $ do
    maybeComponentBuildDirectory <- lookupEnv "HASKELL_DIST_DIR"
    executableDirectory <- takeDirectory <$> getExecutablePath
    findAnyAncestorDirectory
      hasCabalBuildPlan
      (catMaybes [maybeComponentBuildDirectory, Just executableDirectory])

trySynchronous ::
  FilePath ->
  IO value ->
  IO (Either ResourcePathError value)
trySynchronous contextPath action = do
  result <- try action
  case result of
    Left exceptionValue
      | Just asyncException <-
          (fromException exceptionValue :: Maybe SomeAsyncException) ->
          throwIO asyncException
      | otherwise ->
          pure
            ( Left
                (ResourceFilesystemFailure contextPath (displayException (exceptionValue :: SomeException)))
            )
    Right value ->
      pure (Right value)

findAncestorDirectory :: (FilePath -> IO Bool) -> FilePath -> IO (Maybe FilePath)
findAncestorDirectory hasMarker directoryPath =
  canonicalizePath directoryPath
    >>= firstJustM matchingDirectory . ancestorDirectories
  where
    matchingDirectory candidateDirectory =
      hasMarker candidateDirectory
        >>= \markerPresent ->
          pure
            ( if markerPresent
                then Just candidateDirectory
                else Nothing
            )

findAnyCompilerRoot :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findAnyCompilerRoot packageMarker =
  findAnyAncestorDirectory (hasCompilerRootMarkers packageMarker)

findAnyAncestorDirectory :: (FilePath -> IO Bool) -> [FilePath] -> IO (Maybe FilePath)
findAnyAncestorDirectory hasMarker seedDirectories =
  traverse canonicalizePath seedDirectories
    >>= firstJustM (findAncestorDirectory hasMarker) . Set.toAscList . Set.fromList

ancestorDirectories :: FilePath -> [FilePath]
ancestorDirectories initialDirectory =
  initialDirectory : unfoldr parentDirectory initialDirectory
  where
    parentDirectory childDirectory =
      let parent = takeDirectory childDirectory
       in if parent == childDirectory
            then Nothing
            else Just (parent, parent)

firstJustM :: Monad effect => (candidate -> effect (Maybe result)) -> [candidate] -> effect (Maybe result)
firstJustM inspectCandidate =
  foldr
    ( \candidate laterResult ->
        inspectCandidate candidate
          >>= maybe laterResult (pure . Just)
    )
    (pure Nothing)

pathWithinRoot :: FilePath -> FilePath -> Bool
pathWithinRoot rootPath childPath =
  let relativePath = makeRelative rootPath childPath
   in not (isAbsolute relativePath)
        && case splitDirectories relativePath of
          ".." : _ -> False
          _ -> True

pathRelativeToRoot :: FilePath -> Bool
pathRelativeToRoot resourcePath =
  not (isAbsolute resourcePath)
    && case splitDirectories (normalise resourcePath) of
      ".." : _ -> False
      _ -> True

hasCompilerRootMarkers :: FilePath -> FilePath -> IO Bool
hasCompilerRootMarkers packageMarker directoryPath = do
  hasProject <- doesFileExist (directoryPath </> "cabal.project")
  hasPackage <- doesFileExist (directoryPath </> packageMarker)
  pure (hasProject && hasPackage)

hasCabalBuildPlan :: FilePath -> IO Bool
hasCabalBuildPlan directoryPath =
  doesFileExist (directoryPath </> "cache" </> "plan.json")

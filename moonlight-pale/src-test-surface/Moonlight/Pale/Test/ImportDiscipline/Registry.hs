{-# LANGUAGE LambdaCase #-}

{-| Cabal metadata and parsed-source discovery for import-discipline tests. -}
module Moonlight.Pale.Test.ImportDiscipline.Registry
  ( CabalComponentMetadata,
    CabalComponentSelector (..),
    CabalMetadataObstruction,
    CabalPackageMetadata,
    SourceDiscoveryFailure (..),
    ModuleSurface,
    assertRegisteredSetMatches,
    cabalComponentExposedModules,
    cabalComponentOtherModules,
    cabalComponentSourceDirectories,
    cabalLibraryComponents,
    discoverParsedHaskellFiles,
    discoverParsedHaskellFilesWithExcludes,
    discoverModuleSurfaces,
    moduleSurfaceExportedNames,
    moduleSurfaceIdentity,
    moduleSurfaceImportedNames,
    parseCabalPackageMetadata,
    parseModuleSurfaceFile,
    renderCabalComponentSelector,
    renderCabalMetadataObstruction,
    renderSourceDiscoveryFailure,
    selectCabalComponentMetadata,
  )
where

import Data.Bifunctor (first)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    displayException,
    fromException,
    throwIO,
    try,
  )
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.List (intercalate, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Text.Encoding qualified as TextEncoding
import Distribution.ModuleName qualified as CabalModuleName
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.PackageDescription
  ( BuildInfo (hsSourceDirs, otherModules),
    GenericPackageDescription (condLibrary, condSubLibraries, condTestSuites),
    Library (exposedModules, libBuildInfo),
    TestSuite (testBuildInfo),
  )
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.Error (PError, showPError)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Moonlight.Pale.Ghc.ModuleSurface
  ( ModuleSurface (..),
    explicitExportNames,
    moduleSurfaceFromGhcPs,
    parseHsModule,
    renderGhcParseFailure,
    unParsedModuleName,
    unParsedName,
  )
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeExtension, (</>))
import Test.Tasty.HUnit (Assertion, assertFailure)

data CabalComponentSelector
  = CabalMainLibrary
  | CabalNamedLibrary !String
  | CabalTestSuite !String
  deriving stock (Eq, Ord, Show)

data CabalComponentMetadata = CabalComponentMetadata
  { cabalComponentSourceDirectories :: !(Set FilePath),
    cabalComponentExposedModules :: !(Set String),
    cabalComponentOtherModules :: !(Set String)
  }
  deriving stock (Eq, Show)

instance Semigroup CabalComponentMetadata where
  leftMetadata <> rightMetadata =
    CabalComponentMetadata
      { cabalComponentSourceDirectories =
          cabalComponentSourceDirectories leftMetadata
            <> cabalComponentSourceDirectories rightMetadata,
        cabalComponentExposedModules =
          cabalComponentExposedModules leftMetadata
            <> cabalComponentExposedModules rightMetadata,
        cabalComponentOtherModules =
          cabalComponentOtherModules leftMetadata
            <> cabalComponentOtherModules rightMetadata
      }

instance Monoid CabalComponentMetadata where
  mempty =
    CabalComponentMetadata
      { cabalComponentSourceDirectories = Set.empty,
        cabalComponentExposedModules = Set.empty,
        cabalComponentOtherModules = Set.empty
      }

newtype CabalPackageMetadata = CabalPackageMetadata
  { cabalPackageComponents :: [(CabalComponentSelector, CabalComponentMetadata)]
  }

data CabalMetadataObstruction
  = CabalParseObstruction !(NonEmpty PError)
  | CabalComponentAbsent !CabalComponentSelector

data SourceDiscoveryFailure
  = SourceDirectoryTraversalFailed !FilePath !String
  | SourceFileReadFailed !FilePath !String
  | SourceFileParseFailed !FilePath !String
  deriving stock (Eq, Show)

renderSourceDiscoveryFailure :: SourceDiscoveryFailure -> String
renderSourceDiscoveryFailure = \case
  SourceDirectoryTraversalFailed rootDirectory exceptionText ->
    rootDirectory <> ": directory traversal failed: " <> exceptionText
  SourceFileReadFailed sourcePath exceptionText ->
    sourcePath <> ": read failed: " <> exceptionText
  SourceFileParseFailed sourcePath parseError ->
    sourcePath <> ": " <> parseError

assertRegisteredSetMatches :: String -> Set String -> Set String -> Assertion
assertRegisteredSetMatches label expectedEntries registeredEntries =
  let missingEntries = Set.toAscList (Set.difference expectedEntries registeredEntries)
      unexpectedEntries = Set.toAscList (Set.difference registeredEntries expectedEntries)
   in
    if null missingEntries && null unexpectedEntries
      then pure ()
      else
        assertFailure
          ( intercalate
              "\n"
              [ label,
                "missing: " <> show missingEntries,
                "unexpected: " <> show unexpectedEntries,
                "expected: " <> show (Set.toAscList expectedEntries),
                "registered: " <> show (Set.toAscList registeredEntries)
              ]
          )

parseCabalPackageMetadata :: String -> Either CabalMetadataObstruction CabalPackageMetadata
parseCabalPackageMetadata cabalContents =
  case snd (runParseResult (parseGenericPackageDescription (TextEncoding.encodeUtf8 (Text.pack cabalContents)))) of
    Left (_, parseErrors) ->
      Left (CabalParseObstruction parseErrors)
    Right genericDescription ->
      Right (packageMetadataFromDescription genericDescription)

selectCabalComponentMetadata ::
  CabalComponentSelector ->
  CabalPackageMetadata ->
  Either CabalMetadataObstruction CabalComponentMetadata
selectCabalComponentMetadata componentSelector packageMetadata =
  maybe
    (Left (CabalComponentAbsent componentSelector))
    Right
    (lookup componentSelector (cabalPackageComponents packageMetadata))

cabalLibraryComponents ::
  CabalPackageMetadata ->
  [(CabalComponentSelector, CabalComponentMetadata)]
cabalLibraryComponents =
  filter (isLibrarySelector . fst) . cabalPackageComponents

renderCabalComponentSelector :: CabalComponentSelector -> String
renderCabalComponentSelector CabalMainLibrary =
  "library"
renderCabalComponentSelector (CabalNamedLibrary componentName) =
  "library " <> componentName
renderCabalComponentSelector (CabalTestSuite componentName) =
  "test-suite " <> componentName

renderCabalMetadataObstruction :: FilePath -> CabalMetadataObstruction -> String
renderCabalMetadataObstruction cabalPath cabalObstruction =
  case cabalObstruction of
    CabalParseObstruction parseErrors ->
      intercalate "\n" (NonEmpty.toList (fmap (showPError cabalPath) parseErrors))
    CabalComponentAbsent componentSelector ->
      cabalPath <> ": missing Cabal component " <> renderCabalComponentSelector componentSelector

packageMetadataFromDescription :: GenericPackageDescription -> CabalPackageMetadata
packageMetadataFromDescription genericDescription =
  CabalPackageMetadata
    { cabalPackageComponents =
        maybe
          []
          (\conditionalLibrary -> [(CabalMainLibrary, foldMap libraryMetadata conditionalLibrary)])
          (condLibrary genericDescription)
          <> fmap
            ( \(componentName, conditionalLibrary) ->
                ( CabalNamedLibrary (unUnqualComponentName componentName),
                  foldMap libraryMetadata conditionalLibrary
                )
            )
            (condSubLibraries genericDescription)
          <> fmap
            ( \(componentName, conditionalTestSuite) ->
                ( CabalTestSuite (unUnqualComponentName componentName),
                  foldMap testSuiteMetadata conditionalTestSuite
                )
            )
            (condTestSuites genericDescription)
    }

libraryMetadata :: Library -> CabalComponentMetadata
libraryMetadata library =
  buildInfoMetadata (libBuildInfo library)
    <> mempty
      { cabalComponentExposedModules =
          Set.fromList (fmap renderCabalModuleName (exposedModules library))
      }

testSuiteMetadata :: TestSuite -> CabalComponentMetadata
testSuiteMetadata =
  buildInfoMetadata . testBuildInfo

buildInfoMetadata :: BuildInfo -> CabalComponentMetadata
buildInfoMetadata buildInfo =
  CabalComponentMetadata
    { cabalComponentSourceDirectories =
        Set.fromList (fmap getSymbolicPath (hsSourceDirs buildInfo)),
      cabalComponentExposedModules = Set.empty,
      cabalComponentOtherModules =
        Set.fromList (fmap renderCabalModuleName (otherModules buildInfo))
    }

renderCabalModuleName :: CabalModuleName.ModuleName -> String
renderCabalModuleName =
  intercalate "." . CabalModuleName.components

isLibrarySelector :: CabalComponentSelector -> Bool
isLibrarySelector componentSelector =
  case componentSelector of
    CabalMainLibrary -> True
    CabalNamedLibrary _ -> True
    CabalTestSuite _ -> False

discoverModuleSurfaces ::
  FilePath ->
  IO (Either (NonEmpty SourceDiscoveryFailure) [ModuleSurface])
discoverModuleSurfaces rootDirectory = do
  parsedFiles <-
    discoverParsedHaskellFiles
      (\path -> first renderGhcParseFailure . parseHsModule path)
      rootDirectory
  pure
    ( parsedFiles >>= \files ->
        collectDiscoveryResults (fmap parseSurface files)
    )
  where
    parseSurface (sourcePath, moduleAst) =
      first
        (SourceFileParseFailed sourcePath . show)
        (moduleSurfaceFromGhcPs moduleAst)

parseModuleSurfaceFile ::
  FilePath ->
  IO (Either SourceDiscoveryFailure ModuleSurface)
parseModuleSurfaceFile sourcePath = do
  sourceResult <-
    trySynchronous
      (SourceFileReadFailed sourcePath . displayException)
      (Text.unpack <$> TextIO.readFile sourcePath)
  pure
    ( sourceResult
        >>= first (SourceFileParseFailed sourcePath . renderGhcParseFailure)
          . parseHsModule sourcePath
        >>= first (SourceFileParseFailed sourcePath . show) . moduleSurfaceFromGhcPs
    )

discoverParsedHaskellFiles ::
  (FilePath -> String -> Either String value) ->
  FilePath ->
  IO (Either (NonEmpty SourceDiscoveryFailure) [(FilePath, value)])
discoverParsedHaskellFiles parser rootDirectory =
  discoverParsedHaskellFilesWithExcludes [] parser rootDirectory

discoverParsedHaskellFilesWithExcludes ::
  [FilePath] ->
  (FilePath -> String -> Either String value) ->
  FilePath ->
  IO (Either (NonEmpty SourceDiscoveryFailure) [(FilePath, value)])
discoverParsedHaskellFilesWithExcludes excludedDirectoryNames parser rootDirectory = do
  sourcePathsResult <-
    trySynchronous
      (SourceDirectoryTraversalFailed rootDirectory . displayException)
      (haskellModuleFilesWithExcludes excludedDirectoryNames rootDirectory)
  case sourcePathsResult of
    Left traversalFailure ->
      pure (Left (traversalFailure NonEmpty.:| []))
    Right sourcePaths ->
      collectDiscoveryResults
        <$> traverse (readAndParseSourceFile parser) sourcePaths

readAndParseSourceFile ::
  (FilePath -> String -> Either String value) ->
  FilePath ->
  IO (Either SourceDiscoveryFailure (FilePath, value))
readAndParseSourceFile parser sourcePath = do
  sourceResult <-
    trySynchronous
      (SourceFileReadFailed sourcePath . displayException)
      (Text.unpack <$> TextIO.readFile sourcePath)
  pure
    ( sourceResult
        >>= first (SourceFileParseFailed sourcePath) . parser sourcePath
        >>= \parsedValue -> Right (sourcePath, parsedValue)
    )

moduleSurfaceIdentity :: ModuleSurface -> Maybe String
moduleSurfaceIdentity moduleSurface =
  fmap unParsedModuleName (surfaceModuleName moduleSurface)

moduleSurfaceImportedNames :: ModuleSurface -> Set String
moduleSurfaceImportedNames moduleSurface =
  surfaceImportedModules moduleSurface
    & Set.map unParsedModuleName

moduleSurfaceExportedNames :: ModuleSurface -> Maybe (Set String)
moduleSurfaceExportedNames moduleSurface =
  explicitExportNames (surfaceExports moduleSurface)
    & fmap (Set.map unParsedName)

haskellModuleFilesWithExcludes :: [FilePath] -> FilePath -> IO [FilePath]
haskellModuleFilesWithExcludes excludedDirectoryNames rootDirectory =
  canonicalizePath rootDirectory
    >>= walkHaskellModuleFiles Set.empty
  where
    walkHaskellModuleFiles visitedDirectories currentDirectory
      | Set.member currentDirectory visitedDirectories =
          pure []
      | otherwise =
          sort
            <$> listDirectory currentDirectory
            >>= traverse
              (discoverEntry (Set.insert currentDirectory visitedDirectories) currentDirectory)
            >>= pure . concat

    discoverEntry visitedDirectories currentDirectory entryName =
      let entryPath = currentDirectory </> entryName
       in pathIsSymbolicLink entryPath
            >>= \isSymbolicLink ->
              if isSymbolicLink
                then pure []
                else
                  doesDirectoryExist entryPath
                    >>= \isDirectory ->
                      if isDirectory
                        then
                          if isExcludedDirectory excludedDirectoryNames entryName
                            then pure []
                            else
                              canonicalizePath entryPath
                                >>= walkHaskellModuleFiles visitedDirectories
                        else
                          pure
                            ( if takeExtension entryPath `elem` [".hs", ".lhs"]
                                then [entryPath]
                                else []
                            )

collectDiscoveryResults ::
  [Either SourceDiscoveryFailure value] ->
  Either (NonEmpty SourceDiscoveryFailure) [value]
collectDiscoveryResults parseResults =
  case partitionEithers parseResults of
    ([], parsedValues) ->
      Right parsedValues
    (firstFailure : remainingFailures, _) ->
      Left (firstFailure NonEmpty.:| remainingFailures)

isExcludedDirectory :: [FilePath] -> FilePath -> Bool
isExcludedDirectory excludedDirectoryNames entryName =
  entryName `elem` excludedDirectoryNames

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

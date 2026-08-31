{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-| Reading HIE files into module-name oracle indexes. -}
module Moonlight.Pale.Ghc.Hie.Read
  ( HieReadError (..),
    readModuleOracle,
    hieFileOracle,
    indexHieRoots,
  )
where

import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Data.Array (Array)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (foldlM)
import Data.Kind (Type)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Iface.Ext.Binary (HieFileResult (..), readHieFile)
import GHC.Iface.Ext.Types
  ( ContextInfo (..),
    HieAST (..),
    HieASTs (..),
    HieFile (..),
    HieTypeFlat,
    Identifier,
    IdentifierDetails (..),
    NodeInfo (..),
    SourcedNodeInfo (..),
    TypeIndex,
  )
import GHC.Types.Name (Name, isExternalName, nameModule, nameOccName)
import GHC.Types.Name.Cache (NameCache, newNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Unit.Module (moduleName, moduleNameString, moduleUnit, unitString)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), ResolvedOrigin (..), mkPackageUnit)
import Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieOracleArtifact (..),
    HieOracleIndex,
    buildHieOracleIndex,
  )
import Moonlight.Pale.Ghc.Hie.TypeWords
  ( TypeGraphObstruction (..),
    TypeWords,
    hieTypeRootsTypeWords,
  )
import Moonlight.Pale.Ghc.Expr (SourceRegion, sourceRegionFromRealSrcSpan)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    pathIsSymbolicLink,
  )
import System.FilePath (normalise, takeExtension, (</>))

type HieReadError :: Type
data HieReadError
  = HieReadError !FilePath !String
  | HieRootError !FilePath !String
  | HieTraversalError !FilePath !String
  | HieTypeGraphError !FilePath !(Map SourceRegion (Set.Set TypeGraphObstruction))
  deriving stock (Eq, Show)

readModuleOracle :: NameCache -> FilePath -> IO (Either HieReadError ModuleNameOracle)
readModuleOracle nameCache hiePath = do
  readResult <- tryReadHieFile nameCache hiePath
  pure
    ( first (HieReadError hiePath . show) readResult
        >>= hieFileOracle hiePath
    )

indexHieRoots :: [FilePath] -> IO ([HieReadError], HieOracleIndex)
indexHieRoots [] =
  pure ([], buildHieOracleIndex [])
indexHieRoots roots = do
  nameCache <- newNameCache
  collection <- collectHieRoots roots
  readResults <-
    traverse
      ( \hiePath ->
          fmap (HieOracleArtifact hiePath)
            <$> readModuleOracle nameCache hiePath
      )
      (Set.toAscList (hcFiles collection))
  let (readErrors, artifacts) =
        partitionEithers readResults
  pure
    ( reverse (hcErrorsReversed collection) <> readErrors,
      buildHieOracleIndex artifacts
    )

tryReadHieFile :: NameCache -> FilePath -> IO (Either SomeException HieFileResult)
tryReadHieFile nameCache hiePath =
  trySynchronousException (readHieFile nameCache hiePath)

hieFileOracle :: FilePath -> HieFileResult -> Either HieReadError ModuleNameOracle
hieFileOracle hiePath result =
  let hieFile = hie_file_result result
      oracleBuild = foldHieAsts (hie_asts hieFile)
      typeProjection = projectTypeRoots (hie_types hieFile) (obTypeRoots oracleBuild)
   in if Map.null (tpObstructions typeProjection)
        then
          Right
            ModuleNameOracle
              { mnoSourcePath = normalise (hie_hs_file hieFile),
                mnoGlobalUsesAtSpan = obGlobalUsesAtSpan oracleBuild,
                mnoGlobalUses = obGlobals oracleBuild,
                mnoEvidenceAtSpan = obEvidence oracleBuild,
                mnoTypeAtSpan = tpWords typeProjection
              }
        else
          Left (HieTypeGraphError hiePath (tpObstructions typeProjection))

data OracleBuild = OracleBuild
  { obGlobals :: !(Map String (Set.Set ResolvedOrigin)),
    obGlobalUsesAtSpan :: !(Map SourceRegion (Map String (Set.Set ResolvedOrigin))),
    obEvidence :: !(Map SourceRegion (Set.Set ResolvedOrigin)),
    obTypeRoots :: !(Map SourceRegion (Set.Set TypeIndex))
  }

emptyOracleBuild :: OracleBuild
emptyOracleBuild =
  OracleBuild
    { obGlobals = Map.empty,
      obGlobalUsesAtSpan = Map.empty,
      obEvidence = Map.empty,
      obTypeRoots = Map.empty
    }

foldHieAsts :: HieASTs TypeIndex -> OracleBuild
foldHieAsts (HieASTs astsByPath) =
  Map.foldl' foldHieAst emptyOracleBuild astsByPath

foldHieAst :: OracleBuild -> HieAST TypeIndex -> OracleBuild
foldHieAst oracleBuild ast =
  foldl'
    foldHieAst
    ( Map.foldl'
        (foldNodeInfo (sourceRegionFromRealSrcSpan (nodeSpan ast)))
        oracleBuild
        (getSourcedNodeInfo (sourcedNodeInfo ast))
    )
    (nodeChildren ast)

foldNodeInfo :: SourceRegion -> OracleBuild -> NodeInfo TypeIndex -> OracleBuild
foldNodeInfo region oracleBuild nodeInfo =
  Map.foldlWithKey'
    (foldIdentifierDetails region)
    ( foldl'
        (\buildValue typeIndex -> buildValue {obTypeRoots = insertAt region typeIndex (obTypeRoots buildValue)})
        oracleBuild
        (nodeType nodeInfo)
    )
    (nodeIdentifiers nodeInfo)

foldIdentifierDetails :: SourceRegion -> OracleBuild -> Identifier -> IdentifierDetails TypeIndex -> OracleBuild
foldIdentifierDetails region oracleBuild identifier details =
  maybe
    oracleBuild
    ( \origin ->
        OracleBuild
          { obGlobals =
              if Set.member Use (identInfo details)
                then insertAt (roOcc origin) origin (obGlobals oracleBuild)
                else obGlobals oracleBuild,
            obGlobalUsesAtSpan =
              if Set.member Use (identInfo details)
                then insertGlobalUseAtSpan region origin (obGlobalUsesAtSpan oracleBuild)
                else obGlobalUsesAtSpan oracleBuild,
            obEvidence =
              if any evidenceContext (identInfo details)
                then insertAt region origin (obEvidence oracleBuild)
                else obEvidence oracleBuild,
            obTypeRoots = obTypeRoots oracleBuild
          }
    )
    (identifierOrigin identifier)

insertAt :: (Ord key, Ord value) => key -> value -> Map key (Set.Set value) -> Map key (Set.Set value)
insertAt key value =
  Map.insertWith Set.union key (Set.singleton value)

insertGlobalUseAtSpan :: SourceRegion -> ResolvedOrigin -> Map SourceRegion (Map String (Set.Set ResolvedOrigin)) -> Map SourceRegion (Map String (Set.Set ResolvedOrigin))
insertGlobalUseAtSpan region origin =
  Map.insertWith
    (Map.unionWith Set.union)
    region
    (Map.singleton (roOcc origin) (Set.singleton origin))

evidenceContext :: ContextInfo -> Bool
evidenceContext = \case
  EvidenceVarBind {} ->
    True
  EvidenceVarUse ->
    True
  _ ->
    False

data TypeProjection = TypeProjection
  { tpWords :: !(Map SourceRegion (Set.Set TypeWords)),
    tpObstructions :: !(Map SourceRegion (Set.Set TypeGraphObstruction))
  }

projectTypeRoots ::
  Array TypeIndex HieTypeFlat ->
  Map SourceRegion (Set.Set TypeIndex) ->
  TypeProjection
projectTypeRoots typeTable rootsByRegion =
  let regionsByRoot = regionsByTypeRoot rootsByRegion
      compiledRoots = hieTypeRootsTypeWords typeTable (Map.keysSet regionsByRoot)
   in Map.foldlWithKey'
        (projectRoot compiledRoots)
        TypeProjection {tpWords = Map.empty, tpObstructions = Map.empty}
        regionsByRoot
  where
    projectRoot compiledRoots projection typeIndex regions =
      case Map.findWithDefault (Left (MissingTypeIndex typeIndex)) typeIndex compiledRoots of
        Left obstruction ->
          projection
            { tpObstructions =
                insertAcrossRegions obstruction regions (tpObstructions projection)
            }
        Right wordsValue ->
          projection
            { tpWords =
                insertAcrossRegions wordsValue regions (tpWords projection)
            }

regionsByTypeRoot :: Map SourceRegion (Set.Set TypeIndex) -> Map TypeIndex (Set.Set SourceRegion)
regionsByTypeRoot =
  Map.foldlWithKey'
    ( \rootsByIndex region typeIndices ->
        Set.foldl'
          (\nextRoots typeIndex -> insertAt typeIndex region nextRoots)
          rootsByIndex
          typeIndices
    )
    Map.empty

insertAcrossRegions ::
  (Ord value) =>
  value ->
  Set.Set SourceRegion ->
  Map SourceRegion (Set.Set value) ->
  Map SourceRegion (Set.Set value)
insertAcrossRegions value regions valuesByRegion =
  Set.foldl'
    (\nextValues region -> insertAt region value nextValues)
    valuesByRegion
    regions

identifierOrigin :: Identifier -> Maybe ResolvedOrigin
identifierOrigin = \case
  Left _ ->
    Nothing
  Right name ->
    nameOrigin name

nameOrigin :: Name -> Maybe ResolvedOrigin
nameOrigin name =
  if isExternalName name
    then
      let nameModuleValue = nameModule name
          unitText = unitString (moduleUnit nameModuleValue)
       in case mkPackageUnit unitText of
            Left _ ->
              Nothing
            Right unitValue ->
              Just
                ResolvedOrigin
                  { roUnit = unitValue,
                    roModule = moduleNameString (moduleName nameModuleValue),
                    roOcc = occNameString (nameOccName name)
                  }
    else Nothing

data HieCollection = HieCollection
  { hcVisitedDirectories :: !(Set.Set FilePath),
    hcFiles :: !(Set.Set FilePath),
    hcErrorsReversed :: ![HieReadError]
  }

emptyHieCollection :: HieCollection
emptyHieCollection =
  HieCollection
    { hcVisitedDirectories = Set.empty,
      hcFiles = Set.empty,
      hcErrorsReversed = []
    }

data TraversalContext
  = RootContext
  | DescendantContext

data PathKind
  = DirectoryPath
  | DirectorySymlinkPath
  | FilePathKind
  | MissingPath

collectHieRoots :: [FilePath] -> IO HieCollection
collectHieRoots =
  foldlM
    (\collection root -> collectPath RootContext root collection)
    emptyHieCollection
    . sort

collectPath ::
  TraversalContext ->
  FilePath ->
  HieCollection ->
  IO HieCollection
collectPath context path collection = do
  pathKindResult <- classifyPath path
  case pathKindResult of
    Left message ->
      pure (recordTraversalFailure context path message collection)
    Right MissingPath ->
      pure (recordTraversalFailure context path "no such file or directory" collection)
    Right DirectorySymlinkPath ->
      pure
        ( case context of
            RootContext ->
              recordTraversalFailure
                RootContext
                path
                "directory symlink roots are not traversed"
                collection
            DescendantContext ->
              collection
        )
    Right DirectoryPath ->
      collectDirectory context path collection
    Right FilePathKind ->
      collectFile context path collection

classifyPath :: FilePath -> IO (Either String PathKind)
classifyPath path = do
  symbolicLinkResult <- tryFilesystem (pathIsSymbolicLink path)
  case symbolicLinkResult of
    Left message ->
      pure (Left message)
    Right symbolicLink -> do
      directoryResult <- tryFilesystem (doesDirectoryExist path)
      fileResult <- tryFilesystem (doesFileExist path)
      pure
        ( classifyObservedPath symbolicLink
            <$> directoryResult
            <*> fileResult
        )

classifyObservedPath :: Bool -> Bool -> Bool -> PathKind
classifyObservedPath symbolicLink directoryExists fileExists
  | directoryExists && symbolicLink =
      DirectorySymlinkPath
  | directoryExists =
      DirectoryPath
  | fileExists =
      FilePathKind
  | otherwise =
      MissingPath

collectDirectory ::
  TraversalContext ->
  FilePath ->
  HieCollection ->
  IO HieCollection
collectDirectory context directory collection = do
  canonicalResult <- canonicalPath directory
  case canonicalResult of
    Left message ->
      pure (recordTraversalFailure context directory message collection)
    Right canonicalDirectory
      | Set.member canonicalDirectory (hcVisitedDirectories collection) ->
          pure collection
      | otherwise -> do
          entriesResult <- tryFilesystem (listDirectory canonicalDirectory)
          case entriesResult of
            Left message ->
              pure (recordTraversalFailure context directory message collection)
            Right entries ->
              foldlM
                (\nextCollection entry -> collectPath DescendantContext (canonicalDirectory </> entry) nextCollection)
                collection
                  { hcVisitedDirectories =
                      Set.insert canonicalDirectory (hcVisitedDirectories collection)
                  }
                (sort (filter (not . isPrefixOf ".") entries))

collectFile ::
  TraversalContext ->
  FilePath ->
  HieCollection ->
  IO HieCollection
collectFile context path collection
  | not (hieFilePath path) =
      pure collection
  | otherwise = do
      canonicalResult <- canonicalPath path
      pure
        ( either
            (\message -> recordTraversalFailure context path message collection)
            (\canonicalFile -> collection {hcFiles = Set.insert canonicalFile (hcFiles collection)})
            canonicalResult
        )

canonicalPath :: FilePath -> IO (Either String FilePath)
canonicalPath path =
  fmap normalise <$> tryFilesystem (canonicalizePath path)

tryFilesystem :: IO value -> IO (Either String value)
tryFilesystem action =
  first show <$> trySynchronousException action

trySynchronousException :: IO value -> IO (Either SomeException value)
trySynchronousException action = do
  result <- try action
  case result of
    Left exceptionValue ->
      case fromException exceptionValue :: Maybe SomeAsyncException of
        Just asynchronousException ->
          throwIO asynchronousException
        Nothing ->
          pure (Left exceptionValue)
    Right value ->
      pure (Right value)

recordTraversalFailure ::
  TraversalContext ->
  FilePath ->
  String ->
  HieCollection ->
  HieCollection
recordTraversalFailure context path message collection =
  collection
    { hcErrorsReversed =
        traversalFailure context path message : hcErrorsReversed collection
    }

traversalFailure :: TraversalContext -> FilePath -> String -> HieReadError
traversalFailure RootContext =
  HieRootError
traversalFailure DescendantContext =
  HieTraversalError

hieFilePath :: FilePath -> Bool
hieFilePath =
  (== ".hie") . takeExtension

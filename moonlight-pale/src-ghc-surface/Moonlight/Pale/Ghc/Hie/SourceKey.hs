{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-| Source-path keys and indexed lookup for HIE oracle artifacts. -}
module Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieSourceKeyKind (..),
    TriedKey (..),
    HieOracleArtifact (..),
    OracleLookup (..),
    OracleAttachFailure (..),
    HieOracleIndex,
    OracleQuery (..),
    buildHieOracleIndex,
    lookupModuleOracle,
    oracleLookupOracle,
    oracleAttachFailure,
  )
where

import Data.Char (isAlpha, toUpper)
import Data.Either (partitionEithers)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List (intercalate, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..))

type HieSourceKeyKind :: Type
data HieSourceKeyKind
  = GivenPathKey
  | AbsolutePathKey
  | RootRelativeKey
  | ModuleSuffixKey
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type TriedKey :: Type
data TriedKey = TriedKey !HieSourceKeyKind !FilePath
  deriving stock (Eq, Ord, Show, Read)

type HieOracleArtifact :: Type
data HieOracleArtifact = HieOracleArtifact
  { hieArtifactPath :: !FilePath,
    hieArtifactOracle :: !ModuleNameOracle
  }
  deriving stock (Eq, Show)

type OracleLookup :: Type
data OracleLookup
  = OracleFound !HieSourceKeyKind !HieOracleArtifact
  | OracleMissing ![TriedKey]
  | OracleAmbiguous !HieSourceKeyKind !FilePath ![FilePath]
  | OracleIndexObstruction ![Int]
  deriving stock (Eq, Show)

type OracleAttachFailure :: Type
data OracleAttachFailure
  = OracleLookupMissing ![TriedKey]
  | OracleLookupAmbiguous !HieSourceKeyKind !FilePath ![FilePath]
  | OracleLookupIndexObstruction ![Int]
  deriving stock (Eq, Ord, Show, Read)

data PathAnchor
  = RelativeAnchor
  | PosixRootAnchor
  | DriveRootAnchor !Char
  | UncRootAnchor !String !String
  deriving stock (Eq, Ord, Show)

data CanonicalPath = CanonicalPath
  { cpAnchor :: !PathAnchor,
    cpComponents :: ![FilePath]
  }
  deriving stock (Eq, Ord, Show)

data PathPart
  = ComponentPart !FilePath
  | AnchorPart !PathAnchor
  deriving stock (Eq, Ord, Show)

newtype OracleId = OracleId Int
  deriving stock (Eq, Ord, Show)

data CandidateSummary
  = NoCandidate
  | OneCandidate !OracleId
  | ManyCandidates !IntSet
  deriving stock (Eq, Show)

data PathTrie = PathTrie
  { ptTerminal :: !CandidateSummary,
    ptDescendants :: !CandidateSummary,
    ptChildren :: !(Map PathPart PathTrie)
  }
  deriving stock (Eq, Show)

data HieOracleIndex = HieOracleIndex
  { hoiArtifacts :: !(Vector HieOracleArtifact),
    hoiPaths :: !PathTrie
  }
  deriving stock (Eq, Show)

type OracleQuery :: Type
data OracleQuery = OracleQuery
  { oqGivenPath :: !FilePath,
    oqAbsolutePath :: !(Maybe FilePath),
    oqSourceRoots :: ![FilePath]
  }
  deriving stock (Eq, Show)

buildHieOracleIndex :: [HieOracleArtifact] -> HieOracleIndex
buildHieOracleIndex artifacts =
  HieOracleIndex
    { hoiArtifacts = Vector.fromList artifacts,
      hoiPaths =
        foldl'
          ( \pathTrie (oracleIndex, artifact) ->
              insertPath
                (OracleId oracleIndex)
                (canonicalPath (mnoSourcePath (hieArtifactOracle artifact)))
                pathTrie
          )
          emptyPathTrie
          (zip [0 ..] artifacts)
    }

lookupModuleOracle :: HieOracleIndex -> OracleQuery -> OracleLookup
lookupModuleOracle oracleIndex query =
  case firstExactLookup oracleIndex (exactQueryKeys query) of
    Just exactResult ->
      exactResult
    Nothing ->
      maybe
        (OracleMissing (exactTriedKeys query <> suffixTriedKeys query))
        ( \(matchedPath, candidates) ->
            lookupOutcome
              oracleIndex
              ModuleSuffixKey
              (renderCanonicalPath matchedPath)
              candidates
        )
        (deepestSuffixCandidates (canonicalPath (oqGivenPath query)) (hoiPaths oracleIndex))

firstExactLookup :: HieOracleIndex -> [(HieSourceKeyKind, CanonicalPath)] -> Maybe OracleLookup
firstExactLookup oracleIndex =
  foldr
    ( \(keyKind, pathValue) next ->
        case exactCandidates pathValue (hoiPaths oracleIndex) of
          NoCandidate ->
            next
          candidates ->
            Just
              ( lookupOutcome
                  oracleIndex
                  keyKind
                  (renderCanonicalPath pathValue)
                  candidates
              )
    )
    Nothing

lookupOutcome :: HieOracleIndex -> HieSourceKeyKind -> FilePath -> CandidateSummary -> OracleLookup
lookupOutcome oracleIndex keyKind matchedKey candidates =
  case candidateArtifacts candidates (hoiArtifacts oracleIndex) of
    Left missingOracleIds ->
      OracleIndexObstruction missingOracleIds
    Right [] ->
      OracleMissing [TriedKey keyKind matchedKey]
    Right [artifact] ->
      OracleFound keyKind artifact
    Right ambiguous ->
      OracleAmbiguous keyKind matchedKey (fmap hieArtifactPath ambiguous)

candidateArtifacts :: CandidateSummary -> Vector HieOracleArtifact -> Either [Int] [HieOracleArtifact]
candidateArtifacts summary artifacts =
  case
      partitionEithers
        ( fmap
            ( \artifactIndex ->
                maybe
                  (Left artifactIndex)
                  Right
                  (artifacts Vector.!? artifactIndex)
            )
            (IntSet.toAscList (candidateIds summary))
        )
    of
    ([], foundArtifacts) ->
      Right foundArtifacts
    (missingArtifactIds, _) ->
      Left missingArtifactIds

candidateIds :: CandidateSummary -> IntSet
candidateIds = \case
  NoCandidate ->
    IntSet.empty
  OneCandidate (OracleId oracleId) ->
    IntSet.singleton oracleId
  ManyCandidates oracleIds ->
    oracleIds

oracleLookupOracle :: OracleLookup -> Maybe ModuleNameOracle
oracleLookupOracle = \case
  OracleFound _ artifact ->
    Just (hieArtifactOracle artifact)
  OracleMissing _ ->
    Nothing
  OracleAmbiguous _ _ _ ->
    Nothing
  OracleIndexObstruction _ ->
    Nothing

oracleAttachFailure :: OracleLookup -> Maybe OracleAttachFailure
oracleAttachFailure = \case
  OracleFound {} ->
    Nothing
  OracleMissing triedKeys ->
    Just (OracleLookupMissing triedKeys)
  OracleAmbiguous keyKind keyValue candidates ->
    Just (OracleLookupAmbiguous keyKind keyValue candidates)
  OracleIndexObstruction missingOracleIds ->
    Just (OracleLookupIndexObstruction missingOracleIds)

emptyPathTrie :: PathTrie
emptyPathTrie =
  PathTrie
    { ptTerminal = NoCandidate,
      ptDescendants = NoCandidate,
      ptChildren = Map.empty
    }

insertPath :: OracleId -> CanonicalPath -> PathTrie -> PathTrie
insertPath oracleId pathValue =
  insertParts
    (fmap ComponentPart (reverse (cpComponents pathValue)) <> [AnchorPart (cpAnchor pathValue)])
  where
    insertParts parts pathTrie =
      case parts of
        [] ->
          pathTrie
            { ptTerminal = insertCandidate oracleId (ptTerminal pathTrie),
              ptDescendants = insertCandidate oracleId (ptDescendants pathTrie)
            }
        pathPart : remaining ->
          pathTrie
            { ptDescendants = insertCandidate oracleId (ptDescendants pathTrie),
              ptChildren =
                Map.alter
                  ( Just
                      . insertParts remaining
                      . maybe emptyPathTrie id
                  )
                  pathPart
                  (ptChildren pathTrie)
            }

insertCandidate :: OracleId -> CandidateSummary -> CandidateSummary
insertCandidate oracleId = \case
  NoCandidate ->
    OneCandidate oracleId
  OneCandidate existing
    | existing == oracleId ->
        OneCandidate existing
    | otherwise ->
        ManyCandidates
          (IntSet.fromList [oracleIdInt existing, oracleIdInt oracleId])
  ManyCandidates existing ->
    ManyCandidates (IntSet.insert (oracleIdInt oracleId) existing)

oracleIdInt :: OracleId -> Int
oracleIdInt (OracleId oracleId) =
  oracleId

exactCandidates :: CanonicalPath -> PathTrie -> CandidateSummary
exactCandidates pathValue =
  descend
    (fmap ComponentPart (reverse (cpComponents pathValue)) <> [AnchorPart (cpAnchor pathValue)])
  where
    descend parts pathTrie =
      case parts of
        [] ->
          ptTerminal pathTrie
        pathPart : remaining ->
          maybe
            NoCandidate
            (descend remaining)
            (Map.lookup pathPart (ptChildren pathTrie))

deepestSuffixCandidates :: CanonicalPath -> PathTrie -> Maybe (CanonicalPath, CandidateSummary)
deepestSuffixCandidates queryPath =
  descend Nothing [] (reverse (cpComponents queryPath))
  where
    descend best matchedComponents remaining pathTrie =
      case remaining of
        [] ->
          best
        component : nextComponents ->
          case Map.lookup (ComponentPart component) (ptChildren pathTrie) of
            Nothing ->
              best
            Just childTrie ->
              let nextMatchedComponents = component : matchedComponents
                  nextBest =
                    case ptDescendants childTrie of
                      NoCandidate ->
                        best
                      candidates ->
                        Just
                          ( CanonicalPath RelativeAnchor nextMatchedComponents,
                            candidates
                          )
               in descend nextBest nextMatchedComponents nextComponents childTrie

exactQueryKeys :: OracleQuery -> [(HieSourceKeyKind, CanonicalPath)]
exactQueryKeys query =
  [(GivenPathKey, canonicalPath (oqGivenPath query))]
    <> maybe [] (\absolutePath -> [(AbsolutePathKey, canonicalPath absolutePath)]) (oqAbsolutePath query)
    <> fmap (\relativePath -> (RootRelativeKey, relativePath)) (rootRelativePaths query)

exactTriedKeys :: OracleQuery -> [TriedKey]
exactTriedKeys =
  mapMaybe
    ( \(keyKind, pathValue) ->
        case renderCanonicalPath pathValue of
          "" ->
            Nothing
          renderedPath ->
            Just (TriedKey keyKind renderedPath)
    )
    . exactQueryKeys

rootRelativePaths :: OracleQuery -> [CanonicalPath]
rootRelativePaths query =
  [ relativePath
  | root <- fmap canonicalPath (oqSourceRoots query),
    pathValue <-
      canonicalPath (oqGivenPath query)
        : maybe [] (pure . canonicalPath) (oqAbsolutePath query),
    Just relativePath <- [stripCanonicalRoot root pathValue]
  ]

stripCanonicalRoot :: CanonicalPath -> CanonicalPath -> Maybe CanonicalPath
stripCanonicalRoot root pathValue
  | cpAnchor root /= cpAnchor pathValue =
      Nothing
  | otherwise =
      CanonicalPath RelativeAnchor
        <$> stripPrefix (cpComponents root) (cpComponents pathValue)

suffixTriedKeys :: OracleQuery -> [TriedKey]
suffixTriedKeys query =
  fmap
    (TriedKey ModuleSuffixKey . renderCanonicalPath . CanonicalPath RelativeAnchor)
    (componentSuffixes (cpComponents (canonicalPath (oqGivenPath query))))

componentSuffixes :: [FilePath] -> [[FilePath]]
componentSuffixes components =
  case components of
    [] ->
      []
    _ : remaining ->
      components : componentSuffixes remaining

canonicalPath :: FilePath -> CanonicalPath
canonicalPath rawPath =
  case rawPath of
    firstSeparator : secondSeparator : remaining
      | pathSeparator firstSeparator,
        pathSeparator secondSeparator ->
          case splitPathComponents remaining of
            server : share : components ->
              CanonicalPath
                (UncRootAnchor server share)
                (normaliseComponents True components)
            components ->
              CanonicalPath PosixRootAnchor (normaliseComponents True components)
    driveLetter : ':' : remaining
      | isAlpha driveLetter ->
          CanonicalPath
            (DriveRootAnchor (toUpper driveLetter))
            (normaliseComponents True (splitPathComponents remaining))
    firstSeparator : remaining
      | pathSeparator firstSeparator ->
          CanonicalPath
            PosixRootAnchor
            (normaliseComponents True (splitPathComponents remaining))
    _ ->
      CanonicalPath
        RelativeAnchor
        (normaliseComponents False (splitPathComponents rawPath))

splitPathComponents :: FilePath -> [FilePath]
splitPathComponents pathValue =
  case dropWhile pathSeparator pathValue of
    [] ->
      []
    remaining ->
      let (component, next) = break pathSeparator remaining
       in component : splitPathComponents next

normaliseComponents :: Bool -> [FilePath] -> [FilePath]
normaliseComponents rooted =
  reverse . foldl' normaliseComponent []
  where
    normaliseComponent reversedComponents component
      | component == "." || null component =
          reversedComponents
      | component == ".." =
          case reversedComponents of
            previous : remaining
              | previous /= ".." ->
                  remaining
            _
              | rooted ->
                  reversedComponents
              | otherwise ->
                  ".." : reversedComponents
      | otherwise =
          component : reversedComponents

renderCanonicalPath :: CanonicalPath -> FilePath
renderCanonicalPath pathValue =
  let componentText = intercalate "/" (cpComponents pathValue)
   in case cpAnchor pathValue of
        RelativeAnchor ->
          componentText
        PosixRootAnchor ->
          "/" <> componentText
        DriveRootAnchor driveLetter ->
          driveLetter : ':' : '/' : componentText
        UncRootAnchor server share ->
          "//" <> server <> "/" <> share
            <> if null componentText
              then ""
              else "/" <> componentText

pathSeparator :: Char -> Bool
pathSeparator character =
  character == '/' || character == '\\'

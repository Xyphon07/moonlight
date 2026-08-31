module Hie.OracleSpec (tests) where

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.Char (isAlpha, toUpper)
import Data.List (find, intercalate, isSuffixOf, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Clock (getMonotonicTimeNSec)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..), ResolvedOrigin (..), mkResolvedOrigin, occResolvesUniquely)
import Moonlight.Pale.Ghc.Hie.Read (HieReadError (..), indexHieRoots)
import Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieOracleArtifact (..),
    HieSourceKeyKind (..),
    OracleLookup (..),
    OracleQuery (..),
    TriedKey (..),
    buildHieOracleIndex,
    lookupModuleOracle,
  )
import Moonlight.Pale.TestSupport.CompileHieFixture
  ( CompiledHieFixture (compiledHieFixtureOracle),
    compileHieFixture,
    mkHieFixtureModuleName,
  )
import System.Directory
  ( createDirectoryIfMissing,
    createDirectoryLink,
    getTemporaryDirectory,
    removePathForcibly,
  )
import System.Exit (ExitCode (..))
import System.FilePath (normalise, takeFileName, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "pale.hie.oracle"
    [ testCase "ghc hie resolves map and composition into accepted base origins" $ do
        artifact <- compileAndReadArtifact "OracleFixture" oracleFixtureSource
        let oracle = hieArtifactOracle artifact
        acceptedMapOrigins <- acceptedOriginsFor "map"
        acceptedComposeOrigins <- acceptedOriginsFor "."
        assertBool
          "HIE discovery retains the selected artifact path"
          (takeFileName (hieArtifactPath artifact) == "OracleFixture.hie")
        assertBool "map resolves through the base registry" (occResolvesUniquely oracle "map" acceptedMapOrigins)
        assertBool "composition resolves through the base registry" (occResolvesUniquely oracle "." acceptedComposeOrigins)
        assertBool "hie evidence variables are decoded into span-indexed evidence" (not (Map.null (mnoEvidenceAtSpan oracle)))
        assertBool "hie type table is flattened into span-indexed oracle words" (not (Map.null (mnoTypeAtSpan oracle))),
      testCase "a user-defined composition operator is not accepted as base composition" $ do
        oracle <- compileAndReadOracle "ShadowFixture" shadowFixtureSource
        acceptedComposeOrigins <- acceptedOriginsFor "."
        assertBool "shadowed composition does not satisfy the base registry" (not (occResolvesUniquely oracle "." acceptedComposeOrigins)),
      testCase "exact HIE spans preserve Prelude filter beside qualified Map.filter" $ do
        oracle <- compileAndReadOracle "QualifiedOverlapFixture" qualifiedOverlapFixtureSource
        let aggregateOrigins =
              Map.findWithDefault Set.empty "filter" (mnoGlobalUses oracle)
            exactOrigins =
              [ origins
              | namesAtSpan <- Map.elems (mnoGlobalUsesAtSpan oracle),
                Just origins <- [Map.lookup "filter" namesAtSpan]
              ]
            singletonFrom originModule =
              any
                ( \origins ->
                    case Set.toAscList origins of
                      [origin] -> roModule origin == originModule
                      _ -> False
                )
                exactOrigins
        assertBool "module aggregation retains both filter authorities" (Set.size aggregateOrigins == 2)
        assertBool "the unqualified Prelude filter has singleton exact-span evidence" (singletonFrom "GHC.Internal.List")
        assertBool "the qualified Map.filter remains a distinct singleton exact-span authority" (singletonFrom "Data.Map.Internal"),
      testCase "source-key lookup uses suffix fallback without guessing through ambiguities" $ do
        let artifact = emptyArtifact "src/Foo/Bar.hs"
            oracleIndex = buildHieOracleIndex [artifact]
            lookupResult =
              lookupModuleOracle
                oracleIndex
                OracleQuery
                  { oqGivenPath = "compiler/foundation/demo/src/Foo/Bar.hs",
                    oqAbsolutePath = Nothing,
                    oqSourceRoots = []
                  }
        case lookupResult of
          OracleFound ModuleSuffixKey foundArtifact ->
            assertBool
              "lookup returns the selected HIE artifact"
              (hieArtifactPath foundArtifact == hieArtifactPath artifact)
          other ->
            assertFailure ("expected module suffix hit, got " <> show other),
      testCase "source-key lookup prefers exact keys over suffix keys" $ do
        let exactArtifact = emptyArtifact "app/Foo.hs"
            suffixArtifact = emptyArtifact "src/Foo.hs"
            oracleIndex = buildHieOracleIndex [exactArtifact, suffixArtifact]
        case lookupModuleOracle oracleIndex (OracleQuery "app/Foo.hs" Nothing []) of
          OracleFound GivenPathKey _ ->
            pure ()
          other ->
            assertFailure ("expected exact hit before suffix fallback, got " <> show other),
      testCase "source-key lookup attaches root-relative paths before suffix fallback" $ do
        let oracleIndex = buildHieOracleIndex [emptyArtifact "src/Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "/workspace/pkg/src/Foo.hs" Nothing ["/workspace/pkg"]) of
          OracleFound RootRelativeKey _ ->
            pure ()
          other ->
            assertFailure ("expected root-relative hit, got " <> show other),
      testCase "source-key lookup stops at the longest matching suffix before shorter ambiguities" $ do
        let oracleIndex =
              buildHieOracleIndex
                [ emptyArtifact "pkg-a/src/Foo.hs",
                  emptyArtifact "pkg-b/src/Foo.hs",
                  emptyArtifact "other/Foo.hs"
                ]
        case lookupModuleOracle oracleIndex (OracleQuery "/workspace/pkg-a/src/Foo.hs" Nothing []) of
          OracleFound ModuleSuffixKey _ ->
            pure ()
          other ->
            assertFailure ("expected longest singleton suffix hit, got " <> show other),
      testCase "source-key lookup reports exact-key ambiguity" $ do
        let firstArtifact = HieOracleArtifact "first/Foo.hie" (emptyOracle "src/Foo.hs")
            secondArtifact = HieOracleArtifact "second/Foo.hie" (emptyOracle "src/Foo.hs")
            oracleIndex = buildHieOracleIndex [firstArtifact, secondArtifact]
        case lookupModuleOracle oracleIndex (OracleQuery "src/Foo.hs" Nothing []) of
          OracleAmbiguous GivenPathKey "src/Foo.hs" candidates ->
            assertBool
              "ambiguous exact lookup carries artifact paths"
              (candidates == ["first/Foo.hie", "second/Foo.hie"])
          other ->
            assertFailure ("expected exact ambiguity, got " <> show other),
      testCase "source-key lookup records tried keys for misses" $ do
        let oracleIndex = buildHieOracleIndex [emptyArtifact "src/Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "src/Bar.hs" Nothing []) of
          OracleMissing triedKeys ->
            assertBool
              "miss reports exact identity before suffix identities"
              ( take 3 triedKeys
                  == [ TriedKey GivenPathKey "src/Bar.hs",
                       TriedKey ModuleSuffixKey "src/Bar.hs",
                       TriedKey ModuleSuffixKey "Bar.hs"
                     ]
              )
          other ->
            assertFailure ("expected miss, got " <> show other),
      testCase "source-key lookup preserves relative versus POSIX-root anchors" $ do
        let oracleIndex = buildHieOracleIndex [emptyArtifact "src/Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "/src/Foo.hs" Nothing []) of
          OracleFound ModuleSuffixKey _ ->
            pure ()
          other ->
            assertFailure ("expected anchored path to require suffix lookup, got " <> show other),
      testCase "source-key lookup canonicalizes drive-root anchors" $ do
        let oracleIndex = buildHieOracleIndex [emptyArtifact "C:\\workspace\\src\\Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "c:/workspace/src/Foo.hs" Nothing []) of
          OracleFound GivenPathKey _ ->
            pure ()
          other ->
            assertFailure ("expected drive-root exact hit, got " <> show other),
      testCase "source-key lookup preserves UNC anchors" $ do
        let oracleIndex = buildHieOracleIndex [emptyArtifact "\\\\server\\share\\src\\Foo.hs"]
        case lookupModuleOracle oracleIndex (OracleQuery "//server/share/src/Foo.hs" Nothing []) of
          OracleFound GivenPathKey _ ->
            pure ()
          other ->
            assertFailure ("expected UNC exact hit, got " <> show other),
      testCase "source-key trie agrees with the exhaustive normalized-list oracle" $ do
        assertBool
          "bounded corpus generator must retain 2,380 ordered corpora"
          (length sourceKeyOracleCorpora == 2380)
        assertBool
          "query matrix must retain all 27 structural lookup contexts"
          (length sourceKeyDifferentialQueries == 27)
        case sourceKeyDifferentialFailure of
          Nothing ->
            pure ()
          Just failure ->
            assertFailure failure,
      testCase "HIE discovery canonicalizes duplicate directory and file roots" $
        withFreshTestDirectory "duplicate-roots" $ \root -> do
          let invalidHiePath = root </> "Invalid.hie"
          writeFile invalidHiePath ""
          (failures, _oracleIndex) <-
            indexHieRoots
              [ root,
                root </> ".",
                invalidHiePath
              ]
          case failures of
            [HieReadError _ _] ->
              pure ()
            other ->
              assertFailure ("expected one canonicalized artifact failure, got " <> show other),
      testCase "HIE discovery does not follow directory symlink cycles" $
        withFreshTestDirectory "directory-symlink-cycle" $ \root -> do
          let nestedDirectory = root </> "nested"
              invalidHiePath = nestedDirectory </> "Invalid.hie"
              cyclePath = nestedDirectory </> "cycle"
          createDirectoryIfMissing True nestedDirectory
          writeFile invalidHiePath ""
          createDirectoryLink root cyclePath
          (failures, _oracleIndex) <- indexHieRoots [root]
          case failures of
            [HieReadError _ _] ->
              pure ()
            other ->
              assertFailure ("expected one artifact failure without cycle traversal, got " <> show other),
      testCase "ordinary global uses retain exact source spans" $ do
        fixtureModuleName <-
          case mkHieFixtureModuleName "SpanFixture" of
            Left failure ->
              assertFailure ("fixture module name rejected: " <> show failure)
            Right value ->
              pure value
        fixtureResult <-
          compileHieFixture fixtureModuleName spanFixtureSource
        fixture <-
          case fixtureResult of
            Left failure ->
              assertFailure ("HIE fixture compilation failed: " <> show failure)
            Right value ->
              pure value
        let useRows =
              [ (region, origins)
              | (region, names) <- Map.toList (mnoGlobalUsesAtSpan (compiledHieFixtureOracle fixture)),
                Just origins <- [Map.lookup "map" names]
              ]
        assertBool "ordinary map uses are indexed at more than one exact span" (length useRows >= 2)
        assertBool "each exact map span resolves to one origin" (all ((== 1) . Set.size . snd) useRows)
    ]

spanFixtureSource :: ByteString
spanFixtureSource =
  ByteStringChar8.pack
    ( unlines
        [ "module SpanFixture where",
          "first = map id []",
          "second = map id []"
        ]
    )

withFreshTestDirectory :: FilePath -> (FilePath -> IO value) -> IO value
withFreshTestDirectory label action = do
  temporaryDirectory <- getTemporaryDirectory
  uniqueSuffix <- show <$> getMonotonicTimeNSec
  let root =
        temporaryDirectory
          </> "pale-hie-traversal-spec"
          </> (label <> "-" <> uniqueSuffix)
      prepare = do
        createDirectoryIfMissing True root
        pure root
  bracket prepare removePathForcibly action

emptyOracle :: FilePath -> ModuleNameOracle
emptyOracle sourcePath =
  oracleAt (normalise sourcePath)

emptyArtifact :: FilePath -> HieOracleArtifact
emptyArtifact sourcePath =
  HieOracleArtifact
    { hieArtifactPath = sourcePath <> ".hie",
      hieArtifactOracle = emptyOracle sourcePath
    }

oracleAt :: FilePath -> ModuleNameOracle
oracleAt sourcePath =
  ModuleNameOracle
    { mnoSourcePath = sourcePath,
      mnoGlobalUsesAtSpan = Map.empty,
      mnoGlobalUses = Map.empty,
      mnoEvidenceAtSpan = Map.empty,
      mnoTypeAtSpan = Map.empty
    }

data SlowPathAnchor
  = SlowRelativeAnchor
  | SlowPosixRootAnchor
  | SlowDriveRootAnchor !Char
  | SlowUncRootAnchor !String !String
  deriving stock (Eq, Ord, Show)

data SlowCanonicalPath = SlowCanonicalPath
  { slowPathAnchor :: !SlowPathAnchor,
    slowPathComponents :: ![FilePath]
  }
  deriving stock (Eq, Ord, Show)

sourceKeyDifferentialFailure :: Maybe String
sourceKeyDifferentialFailure =
  foldr compareCorpus Nothing sourceKeyOracleCorpora
  where
    compareCorpus sourcePaths nextFailure =
      let artifacts = fmap artifactAt sourcePaths
          oracleIndex = buildHieOracleIndex artifacts
       in case
            find
              ( \query ->
                  lookupModuleOracle oracleIndex query
                    /= slowLookupModuleOracle artifacts query
              )
              sourceKeyDifferentialQueries
            of
            Nothing ->
              nextFailure
            Just query ->
              let expected = slowLookupModuleOracle artifacts query
                  actual = lookupModuleOracle oracleIndex query
               in Just
                    ( "source-key differential failed"
                        <> "\ncorpus: "
                        <> show sourcePaths
                        <> "\nquery: "
                        <> show query
                        <> "\nexpected: "
                        <> show expected
                        <> "\nactual: "
                        <> show actual
                    )

artifactAt :: FilePath -> HieOracleArtifact
artifactAt sourcePath =
  HieOracleArtifact
    { hieArtifactPath = "hie/" <> show sourcePath <> ".hie",
      hieArtifactOracle = oracleAt sourcePath
    }

sourceKeyOracleCorpora :: [[FilePath]]
sourceKeyOracleCorpora =
  concatMap
    (`replicateM` sourceKeyPathUniverse)
    [0 .. 3]

sourceKeyPathUniverse :: [FilePath]
sourceKeyPathUniverse =
  [ "src/Foo.hs",
    "./src/./Foo.hs",
    "app/Foo.hs",
    "pkg-a/src/Foo.hs",
    "pkg-b/src/Foo.hs",
    "other/Foo.hs",
    "/workspace/pkg/src/Foo.hs",
    "/other/pkg/src/Foo.hs",
    "C:\\workspace\\src\\Foo.hs",
    "c:/workspace/src/Foo.hs",
    "\\\\server\\share\\src\\Foo.hs",
    "//server/share/src/Foo.hs",
    "src/Bar.hs"
  ]

sourceKeyDifferentialQueries :: [OracleQuery]
sourceKeyDifferentialQueries =
  fmap (\sourcePath -> OracleQuery sourcePath Nothing []) sourceKeyPathUniverse
    <> [ OracleQuery
           { oqGivenPath = "app/Foo.hs",
             oqAbsolutePath = Just "/workspace/pkg/src/Foo.hs",
             oqSourceRoots = ["/workspace/pkg"]
           },
         OracleQuery
           { oqGivenPath = "missing/Foo.hs",
             oqAbsolutePath = Just "/workspace/pkg/src/Foo.hs",
             oqSourceRoots = ["/workspace/pkg"]
           },
         OracleQuery
           { oqGivenPath = "/workspace/pkg/src/Foo.hs",
             oqAbsolutePath = Nothing,
             oqSourceRoots = ["/workspace/pkg"]
           },
         OracleQuery
           { oqGivenPath = "/workspace/pkg/src/Foo.hs",
             oqAbsolutePath = Nothing,
             oqSourceRoots = ["/workspace", "/workspace/pkg"]
           },
         OracleQuery
           { oqGivenPath = "c:/workspace/src/Foo.hs",
             oqAbsolutePath = Nothing,
             oqSourceRoots = ["C:\\workspace"]
           },
         OracleQuery
           { oqGivenPath = "//server/share/src/Foo.hs",
             oqAbsolutePath = Nothing,
             oqSourceRoots = ["\\\\server\\share"]
           },
         OracleQuery "/checkout/pkg-a/src/Foo.hs" Nothing [],
         OracleQuery "/checkout/src/Foo.hs" Nothing [],
         OracleQuery "/checkout/Foo.hs" Nothing [],
         OracleQuery
           { oqGivenPath = "/workspace/src/Missing.hs",
             oqAbsolutePath = Just "/workspace/src/Missing.hs",
             oqSourceRoots = ["/workspace"]
           },
         OracleQuery "pkg/../src/./Foo.hs" Nothing [],
         OracleQuery "/src/Foo.hs" Nothing [],
         OracleQuery "c:/workspace/src/Foo.hs" Nothing [],
         OracleQuery "//server/share/src/Foo.hs" Nothing []
       ]

slowLookupModuleOracle :: [HieOracleArtifact] -> OracleQuery -> OracleLookup
slowLookupModuleOracle artifacts query =
  case slowFirstExactLookup artifacts (slowExactQueryKeys query) of
    Just exactLookup ->
      exactLookup
    Nothing ->
      case
          find
            (not . null . (`slowSuffixCandidates` artifacts))
            (slowComponentSuffixes (slowPathComponents (slowCanonicalPath (oqGivenPath query))))
        of
        Nothing ->
          OracleMissing (slowExactTriedKeys query <> slowSuffixTriedKeys query)
        Just matchedComponents ->
          slowLookupOutcome
            ModuleSuffixKey
            (intercalate "/" matchedComponents)
            (slowSuffixCandidates matchedComponents artifacts)

slowFirstExactLookup ::
  [HieOracleArtifact] ->
  [(HieSourceKeyKind, SlowCanonicalPath)] ->
  Maybe OracleLookup
slowFirstExactLookup artifacts =
  foldr
    ( \(keyKind, pathValue) nextLookup ->
        case slowExactCandidates pathValue artifacts of
          [] ->
            nextLookup
          candidates ->
            Just
              ( slowLookupOutcome
                  keyKind
                  (slowRenderCanonicalPath pathValue)
                  candidates
              )
    )
    Nothing

slowLookupOutcome ::
  HieSourceKeyKind ->
  FilePath ->
  [HieOracleArtifact] ->
  OracleLookup
slowLookupOutcome keyKind matchedKey candidates =
  case candidates of
    [] ->
      OracleMissing [TriedKey keyKind matchedKey]
    [artifact] ->
      OracleFound keyKind artifact
    ambiguous ->
      OracleAmbiguous keyKind matchedKey (fmap hieArtifactPath ambiguous)

slowExactCandidates ::
  SlowCanonicalPath ->
  [HieOracleArtifact] ->
  [HieOracleArtifact]
slowExactCandidates pathValue =
  filter
    ( (== pathValue)
        . slowCanonicalPath
        . mnoSourcePath
        . hieArtifactOracle
    )

slowSuffixCandidates ::
  [FilePath] ->
  [HieOracleArtifact] ->
  [HieOracleArtifact]
slowSuffixCandidates suffixComponents =
  filter
    ( (suffixComponents `isSuffixOf`)
        . slowPathComponents
        . slowCanonicalPath
        . mnoSourcePath
        . hieArtifactOracle
    )

slowExactQueryKeys :: OracleQuery -> [(HieSourceKeyKind, SlowCanonicalPath)]
slowExactQueryKeys query =
  [(GivenPathKey, slowCanonicalPath (oqGivenPath query))]
    <> maybe
      []
      (\absolutePath -> [(AbsolutePathKey, slowCanonicalPath absolutePath)])
      (oqAbsolutePath query)
    <> fmap (\relativePath -> (RootRelativeKey, relativePath)) (slowRootRelativePaths query)

slowExactTriedKeys :: OracleQuery -> [TriedKey]
slowExactTriedKeys =
  mapMaybe
    ( \(keyKind, pathValue) ->
        case slowRenderCanonicalPath pathValue of
          "" ->
            Nothing
          renderedPath ->
            Just (TriedKey keyKind renderedPath)
    )
    . slowExactQueryKeys

slowRootRelativePaths :: OracleQuery -> [SlowCanonicalPath]
slowRootRelativePaths query =
  [ relativePath
  | root <- fmap slowCanonicalPath (oqSourceRoots query),
    pathValue <-
      slowCanonicalPath (oqGivenPath query)
        : maybe [] (pure . slowCanonicalPath) (oqAbsolutePath query),
    Just relativePath <- [slowStripCanonicalRoot root pathValue]
  ]

slowStripCanonicalRoot ::
  SlowCanonicalPath ->
  SlowCanonicalPath ->
  Maybe SlowCanonicalPath
slowStripCanonicalRoot root pathValue
  | slowPathAnchor root /= slowPathAnchor pathValue =
      Nothing
  | otherwise =
      SlowCanonicalPath SlowRelativeAnchor
        <$> stripPrefix
          (slowPathComponents root)
          (slowPathComponents pathValue)

slowSuffixTriedKeys :: OracleQuery -> [TriedKey]
slowSuffixTriedKeys =
  fmap (TriedKey ModuleSuffixKey . intercalate "/")
    . slowComponentSuffixes
    . slowPathComponents
    . slowCanonicalPath
    . oqGivenPath

slowComponentSuffixes :: [FilePath] -> [[FilePath]]
slowComponentSuffixes components =
  case components of
    [] ->
      []
    _ : remaining ->
      components : slowComponentSuffixes remaining

slowCanonicalPath :: FilePath -> SlowCanonicalPath
slowCanonicalPath rawPath =
  case rawPath of
    firstSeparator : secondSeparator : remaining
      | slowPathSeparator firstSeparator,
        slowPathSeparator secondSeparator ->
          case slowSplitPathComponents remaining of
            server : share : components ->
              SlowCanonicalPath
                (SlowUncRootAnchor server share)
                (slowNormaliseComponents True components)
            components ->
              SlowCanonicalPath
                SlowPosixRootAnchor
                (slowNormaliseComponents True components)
    driveLetter : ':' : remaining
      | isAlpha driveLetter ->
          SlowCanonicalPath
            (SlowDriveRootAnchor (toUpper driveLetter))
            (slowNormaliseComponents True (slowSplitPathComponents remaining))
    firstSeparator : remaining
      | slowPathSeparator firstSeparator ->
          SlowCanonicalPath
            SlowPosixRootAnchor
            (slowNormaliseComponents True (slowSplitPathComponents remaining))
    _ ->
      SlowCanonicalPath
        SlowRelativeAnchor
        (slowNormaliseComponents False (slowSplitPathComponents rawPath))

slowSplitPathComponents :: FilePath -> [FilePath]
slowSplitPathComponents pathValue =
  case dropWhile slowPathSeparator pathValue of
    [] ->
      []
    remaining ->
      let (component, next) = break slowPathSeparator remaining
       in component : slowSplitPathComponents next

slowNormaliseComponents :: Bool -> [FilePath] -> [FilePath]
slowNormaliseComponents rooted =
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

slowRenderCanonicalPath :: SlowCanonicalPath -> FilePath
slowRenderCanonicalPath pathValue =
  let componentText = intercalate "/" (slowPathComponents pathValue)
   in case slowPathAnchor pathValue of
        SlowRelativeAnchor ->
          componentText
        SlowPosixRootAnchor ->
          "/" <> componentText
        SlowDriveRootAnchor driveLetter ->
          driveLetter : ':' : '/' : componentText
        SlowUncRootAnchor server share ->
          "//" <> server <> "/" <> share
            <> if null componentText
              then ""
              else "/" <> componentText

slowPathSeparator :: Char -> Bool
slowPathSeparator character =
  character == '/' || character == '\\'

oracleFixtureSource :: String
oracleFixtureSource =
  unlines
    [ "module OracleFixture where",
      "composed = (.) id id",
      "mapped xs = map id xs",
      "mappedMaybe = fmap not (Just True)",
      "shown = show (Just True)"
    ]

shadowFixtureSource :: String
shadowFixtureSource =
  unlines
    [ "module ShadowFixture where",
      "import Prelude hiding ((.))",
      "(.) x = x",
      "token = ()",
      "shadow = (.) token"
    ]

qualifiedOverlapFixtureSource :: String
qualifiedOverlapFixtureSource =
  unlines
    [ "module QualifiedOverlapFixture where",
      "import qualified Data.Map.Strict as Map",
      "listResult = filter odd [1, 2, 3]",
      "mapResult = Map.filter odd (Map.fromList [(1, 2), (2, 3)])"
    ]

acceptedOriginsFor :: String -> IO (Set.Set ResolvedOrigin)
acceptedOriginsFor occText =
  either
    (\failure -> assertFailure ("accepted-origin fixture failed to parse: " <> show failure))
    (pure . Set.fromList)
    ( traverse
        (\(unitText, moduleText) -> mkResolvedOrigin unitText moduleText occText)
        [ ("base", "GHC.Base"),
          ("base", "GHC.Internal.Base"),
          ("ghc-internal", "GHC.Internal.Base")
        ]
    )

compileAndReadOracle :: String -> String -> IO ModuleNameOracle
compileAndReadOracle moduleName sourceText =
  hieArtifactOracle <$> compileAndReadArtifact moduleName sourceText

compileAndReadArtifact :: String -> String -> IO HieOracleArtifact
compileAndReadArtifact moduleName sourceText =
  withFreshTestDirectory ("oracle-" <> moduleName) $ \root -> do
    let sourceDirectory = root </> "src"
        hieDirectory = root </> "hie"
        sourcePath = sourceDirectory </> moduleName <> ".hs"
    createDirectoryIfMissing True sourceDirectory
    createDirectoryIfMissing True hieDirectory
    writeFile sourcePath sourceText
    (exitCode, _stdoutText, stderrText) <-
      readProcessWithExitCode
        "ghc"
        [ "-fno-code",
          "-fforce-recomp",
          "-fwrite-ide-info",
          "-hiedir",
          hieDirectory,
          sourcePath
        ]
        ""
    case exitCode of
      ExitSuccess -> do
        (errors, oracleIndex) <- indexHieRoots [hieDirectory]
        let lookupResult =
              lookupModuleOracle
                oracleIndex
                OracleQuery
                  { oqGivenPath = normalise sourcePath,
                    oqAbsolutePath = Just (normalise sourcePath),
                    oqSourceRoots = [sourceDirectory]
                  }
        case (errors, lookupResult) of
          ([], OracleFound _ artifact) ->
            pure artifact
          ([], _) ->
            assertFailure ("oracle missing for " <> sourcePath <> ": " <> show lookupResult)
          (hieErrors, _) ->
            assertFailure ("hie read errors: " <> show hieErrors)
      ExitFailure _ ->
        assertFailure stderrText

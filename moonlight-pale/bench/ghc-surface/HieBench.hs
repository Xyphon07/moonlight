-- Current HIE type-graph property workload.  The historical encoder unfolds
-- the same doubling DAG exponentially, so these rows certify the exact current
-- linear wire law rather than manufacturing a ratio between different wire
-- contracts.
module HieBench
  ( HieBenchmarkObstruction (..),
    hieBenchmarks,
  )
where

import BenchSupport (preparedBenchmarks)
import Control.DeepSeq (NFData (rnf))
import Data.Array (Array, array, assocs)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Iface.Ext.Types (HieArgs (..), HieType (..), HieTypeFlat, TypeIndex)
import GHC.Types.Name (Name, mkSystemName)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (mkUnique)
import Language.Haskell.Syntax.Specificity (data Specified)
import Moonlight.Pale.Ghc.Hie.Oracle (ModuleNameOracle (..))
import Moonlight.Pale.Ghc.Hie.SourceKey
  ( HieOracleArtifact (..),
    HieSourceKeyKind (..),
    OracleLookup (..),
    OracleQuery (..),
    buildHieOracleIndex,
    lookupModuleOracle,
  )
import Moonlight.Pale.Ghc.Hie.TypeWords
  ( TypeGraphObstruction (..),
    TypeWords,
    hieTypeIndexTypeWords,
    hieTypeRootsTypeWords,
    typeWordsList,
  )
import Test.Tasty.Bench (Benchmark, bgroup)

data HieBenchmarkObstruction
  = HieGraphCompilationRejected !Int !TypeGraphObstruction
  | HieRootSetCompilationRejected !Int !TypeGraphObstruction
  | UnexpectedHieWireLength !Int !Int !Int
  | UnexpectedHieRootCount !Int !Int !Int
  | InvalidHieFailureCorpusAccepted !String
  | UnexpectedHieFailureObstruction !String !TypeGraphObstruction
  | UnexpectedSourceKeyLookup !String !OracleLookup
  deriving stock (Eq, Show)

instance NFData HieBenchmarkObstruction where
  rnf obstruction =
    rnf (show obstruction)

data HieWireDigest = HieWireDigest
  { hieWireWordCount :: !Int,
    hieWireWordHash :: !Word64
  }
  deriving stock (Eq, Show)

instance NFData HieWireDigest where
  rnf (HieWireDigest wordCount wordHash) =
    rnf wordCount `seq` rnf wordHash

data PreparedHieCorpus = PreparedHieCorpus
  { preparedHieDepth :: !Int,
    preparedHieTypeTable :: !(Array TypeIndex HieTypeFlat)
  }

instance NFData PreparedHieCorpus where
  rnf corpus =
    rnf (preparedHieDepth corpus)
      `seq` forceTypeTable (preparedHieTypeTable corpus)

forceTypeTable :: Array TypeIndex HieTypeFlat -> ()
forceTypeTable =
  foldr forceTypeEntry () . assocs

forceTypeEntry :: (TypeIndex, HieTypeFlat) -> () -> ()
forceTypeEntry (typeIndex, flatType) forcedTail =
  rnf typeIndex
    `seq` case flatType of
      HCoercionTy ->
        forcedTail
      HAppTy functionIndex (HieArgs argumentIndices) ->
        rnf functionIndex
          `seq` foldr
            ( \(visible, argumentIndex) nestedTail ->
                rnf visible `seq` rnf argumentIndex `seq` nestedTail
            )
            forcedTail
            argumentIndices
      HCastTy childIndex ->
        rnf childIndex `seq` forcedTail
      HForAllTy ((binderName, binderKind), specificity) bodyIndex ->
        binderName
          `seq` rnf binderKind
          `seq` specificity
          `seq` rnf bodyIndex
          `seq` forcedTail
      HTyVarTy variableName ->
        variableName `seq` forcedTail
      otherFlatType ->
        otherFlatType `seq` forcedTail

data PreparedTypeGraphCorpus = PreparedTypeGraphCorpus
  { preparedTypeGraphSize :: !Int,
    preparedTypeGraphRoot :: !TypeIndex,
    preparedTypeGraphTable :: !(Array TypeIndex HieTypeFlat)
  }

instance NFData PreparedTypeGraphCorpus where
  rnf corpus =
    rnf (preparedTypeGraphSize corpus)
      `seq` rnf (preparedTypeGraphRoot corpus)
      `seq` forceTypeTable (preparedTypeGraphTable corpus)

data PreparedRootSetCorpus = PreparedRootSetCorpus
  { preparedRootSetSize :: !Int,
    preparedRootSetRoots :: !(Set TypeIndex),
    preparedRootSetTable :: !(Array TypeIndex HieTypeFlat)
  }

instance NFData PreparedRootSetCorpus where
  rnf corpus =
    rnf (preparedRootSetSize corpus)
      `seq` rnf (Set.toAscList (preparedRootSetRoots corpus))
      `seq` forceTypeTable (preparedRootSetTable corpus)

data SourceKeyExpectation
  = ExpectLongestSingleton
  | ExpectAmbiguousSuffix
  deriving stock (Eq, Show)

data PreparedSourceKeyCorpus = PreparedSourceKeyCorpus
  { preparedSourceKeyLabel :: !String,
    preparedSourceKeyPaths :: ![FilePath],
    preparedSourceKeyQuery :: !OracleQuery,
    preparedSourceKeyExpectation :: !SourceKeyExpectation
  }

instance NFData PreparedSourceKeyCorpus where
  rnf corpus =
    rnf (preparedSourceKeyLabel corpus)
      `seq` rnf (preparedSourceKeyPaths corpus)
      `seq` rnf (show (preparedSourceKeyQuery corpus))
      `seq` rnf (show (preparedSourceKeyExpectation corpus))

data SourceKeyDigest = SourceKeyDigest
  { sourceKeyDigestCandidateCount :: !Int,
    sourceKeyDigestHash :: !Int
  }
  deriving stock (Eq, Show)

instance NFData SourceKeyDigest where
  rnf digest =
    rnf (sourceKeyDigestCandidateCount digest)
      `seq` rnf (sourceKeyDigestHash digest)

hieBenchmarks :: Either HieBenchmarkObstruction Benchmark
hieBenchmarks = do
  validateInvalidHieCorpora
  preparedCorpora <- traverse prepareHieCorpus currentHieDepths
  graphBenchmarks <- traverse prepareTypeGraphBenchmark typeGraphFamilies
  repeatedRootCorpora <- traverse prepareRepeatedRootCorpus repeatedRootSizes
  sourceKeyBenchmarks <- traverse prepareSourceKeyBenchmark sourceKeyFamilies
  pure
    ( bgroup
        "hie-type-words"
        [ bgroup
            "doubling-dag-linear-wire"
            (preparedBenchmarks "depth" preparedCorpora compileHieWireDigest),
          bgroup
            "adversarial-type-graphs"
            graphBenchmarks,
          bgroup
            "repeated-structural-roots"
            (preparedBenchmarks "roots" repeatedRootCorpora compileRepeatedRootDigest),
          bgroup
            "source-path-collisions"
            sourceKeyBenchmarks
        ]
    )

currentHieDepths :: [Int]
currentHieDepths =
  [8, 16, 32, 128, 512]

typeGraphFamilies ::
  [(String, [Int], Int -> (TypeIndex, Array TypeIndex HieTypeFlat))]
typeGraphFamilies =
  [ ("diamond-fanout", [8, 64, 512], diamondFanoutCorpus),
    ("deep-forall", [8, 32, 128], deepForAllCorpus),
    ("variable-rich-doubling", [8, 32, 128, 512], variableRichDoublingCorpus)
  ]

prepareTypeGraphBenchmark ::
  (String, [Int], Int -> (TypeIndex, Array TypeIndex HieTypeFlat)) ->
  Either HieBenchmarkObstruction Benchmark
prepareTypeGraphBenchmark (familyLabel, sizes, corpusForSize) = do
  preparedCorpora <-
    traverse
      ( \size -> do
          let (rootIndex, typeTable) = corpusForSize size
              corpus =
                PreparedTypeGraphCorpus
                  { preparedTypeGraphSize = size,
                    preparedTypeGraphRoot = rootIndex,
                    preparedTypeGraphTable = typeTable
                  }
          _ <- compileTypeGraphDigest corpus
          pure (size, corpus)
      )
      sizes
  pure
    ( bgroup
        familyLabel
        (preparedBenchmarks "size" preparedCorpora compileTypeGraphDigest)
    )

compileTypeGraphDigest ::
  PreparedTypeGraphCorpus ->
  Either HieBenchmarkObstruction HieWireDigest
compileTypeGraphDigest corpus =
  case
      hieTypeIndexTypeWords
        (preparedTypeGraphTable corpus)
        (preparedTypeGraphRoot corpus)
    of
    Left obstruction ->
      Left (HieGraphCompilationRejected (preparedTypeGraphSize corpus) obstruction)
    Right typeWordsValue ->
      Right (foldl' digestHieWord emptyHieWireDigest (typeWordsList typeWordsValue))

repeatedRootSizes :: [Int]
repeatedRootSizes =
  [8, 64, 512]

prepareRepeatedRootCorpus ::
  Int ->
  Either HieBenchmarkObstruction (Int, PreparedRootSetCorpus)
prepareRepeatedRootCorpus rootCount = do
  let corpus =
        PreparedRootSetCorpus
          { preparedRootSetSize = rootCount,
            preparedRootSetRoots = Set.fromList [1 .. fromIntegral rootCount],
            preparedRootSetTable = repeatedRootTable rootCount
          }
  _ <- compileRepeatedRootDigest corpus
  pure (rootCount, corpus)

compileRepeatedRootDigest ::
  PreparedRootSetCorpus ->
  Either HieBenchmarkObstruction HieWireDigest
compileRepeatedRootDigest corpus = do
  compiledRoots <-
    firstRootFailure
      (preparedRootSetSize corpus)
      ( hieTypeRootsTypeWords
          (preparedRootSetTable corpus)
          (preparedRootSetRoots corpus)
      )
  let actualRootCount = Map.size compiledRoots
  if actualRootCount /= preparedRootSetSize corpus
    then
      Left
        ( UnexpectedHieRootCount
            (preparedRootSetSize corpus)
            (preparedRootSetSize corpus)
            actualRootCount
        )
    else
      Right
        ( Map.foldlWithKey'
            digestCompiledRoot
            emptyHieWireDigest
            compiledRoots
        )

firstRootFailure ::
  Int ->
  Map.Map TypeIndex (Either TypeGraphObstruction typeWords) ->
  Either HieBenchmarkObstruction (Map.Map TypeIndex typeWords)
firstRootFailure rootCount =
  traverse
    (either (Left . HieRootSetCompilationRejected rootCount) Right)

digestCompiledRoot :: HieWireDigest -> TypeIndex -> TypeWords -> HieWireDigest
digestCompiledRoot digest rootIndex typeWordsValue =
  foldl'
    digestHieWord
    (digestHieWord digest (fromIntegral rootIndex))
    (typeWordsList typeWordsValue)

sourceKeyFamilies :: [(String, SourceKeyExpectation)]
sourceKeyFamilies =
  [ ("longest-singleton-suffix", ExpectLongestSingleton),
    ("ambiguous-shared-suffix", ExpectAmbiguousSuffix)
  ]

prepareSourceKeyBenchmark ::
  (String, SourceKeyExpectation) ->
  Either HieBenchmarkObstruction Benchmark
prepareSourceKeyBenchmark (familyLabel, expectation) = do
  preparedCorpora <-
    traverse
      (prepareSourceKeyCorpus familyLabel expectation)
      [8, 64, 512]
  pure
    ( bgroup
        familyLabel
        (preparedBenchmarks "paths" preparedCorpora compileSourceKeyDigest)
    )

prepareSourceKeyCorpus ::
  String ->
  SourceKeyExpectation ->
  Int ->
  Either HieBenchmarkObstruction (Int, PreparedSourceKeyCorpus)
prepareSourceKeyCorpus familyLabel expectation pathCount = do
  let corpus =
        PreparedSourceKeyCorpus
          { preparedSourceKeyLabel = familyLabel <> "/" <> show pathCount,
            preparedSourceKeyPaths =
              fmap
                (\pathIndex -> "pkg" <> show pathIndex <> "/src/Foo.hs")
                [1 .. pathCount],
            preparedSourceKeyQuery =
              OracleQuery
                { oqGivenPath =
                    case expectation of
                      ExpectLongestSingleton ->
                        "/workspace/pkg1/src/Foo.hs"
                      ExpectAmbiguousSuffix ->
                        "/workspace/src/Foo.hs",
                  oqAbsolutePath = Nothing,
                  oqSourceRoots = []
                },
            preparedSourceKeyExpectation = expectation
          }
  _ <- compileSourceKeyDigest corpus
  pure (pathCount, corpus)

compileSourceKeyDigest ::
  PreparedSourceKeyCorpus ->
  Either HieBenchmarkObstruction SourceKeyDigest
compileSourceKeyDigest corpus =
  let lookupResult =
        lookupModuleOracle
          (buildHieOracleIndex (fmap emptyArtifact (preparedSourceKeyPaths corpus)))
          (preparedSourceKeyQuery corpus)
   in case (preparedSourceKeyExpectation corpus, lookupResult) of
        (ExpectLongestSingleton, OracleFound ModuleSuffixKey artifact) ->
          Right
            SourceKeyDigest
              { sourceKeyDigestCandidateCount = 1,
                sourceKeyDigestHash =
                  digestString
                    2166136261
                    (mnoSourcePath (hieArtifactOracle artifact))
              }
        (ExpectAmbiguousSuffix, OracleAmbiguous ModuleSuffixKey matchedPath candidates) ->
          Right
            SourceKeyDigest
              { sourceKeyDigestCandidateCount = length candidates,
                sourceKeyDigestHash =
                  foldl'
                    digestString
                    (digestString 2166136261 matchedPath)
                    candidates
              }
        _ ->
          Left
            ( UnexpectedSourceKeyLookup
                (preparedSourceKeyLabel corpus)
                lookupResult
            )

emptyOracle :: FilePath -> ModuleNameOracle
emptyOracle sourcePath =
  ModuleNameOracle
    { mnoSourcePath = sourcePath,
      mnoGlobalUsesAtSpan = Map.empty,
      mnoGlobalUses = Map.empty,
      mnoEvidenceAtSpan = Map.empty,
      mnoTypeAtSpan = Map.empty
    }

emptyArtifact :: FilePath -> HieOracleArtifact
emptyArtifact sourcePath =
  HieOracleArtifact
    { hieArtifactPath = sourcePath <> ".hie",
      hieArtifactOracle = emptyOracle sourcePath
    }

validateInvalidHieCorpora :: Either HieBenchmarkObstruction ()
validateInvalidHieCorpora = do
  expectHieObstruction
    "cycle"
    (CyclicTypeIndex 0)
    (hieTypeIndexTypeWords (array (0, 0) [(0, HCastTy 0)]) 0)
  expectHieObstruction
    "out-of-range"
    (MissingTypeIndex 1)
    (hieTypeIndexTypeWords (array (0, 0) [(0, HCastTy 1)]) 0)

expectHieObstruction ::
  String ->
  TypeGraphObstruction ->
  Either TypeGraphObstruction typeWords ->
  Either HieBenchmarkObstruction ()
expectHieObstruction corpusLabel expectedObstruction = \case
  Right _ ->
    Left (InvalidHieFailureCorpusAccepted corpusLabel)
  Left actualObstruction
    | actualObstruction == expectedObstruction ->
        Right ()
    | otherwise ->
        Left
          ( UnexpectedHieFailureObstruction
              corpusLabel
              actualObstruction
          )

prepareHieCorpus :: Int -> Either HieBenchmarkObstruction (Int, PreparedHieCorpus)
prepareHieCorpus depth = do
  let corpus =
        PreparedHieCorpus
          { preparedHieDepth = depth,
            preparedHieTypeTable = doublingDagTable depth
          }
  _ <- compileHieWireDigest corpus
  pure (depth, corpus)

compileHieWireDigest :: PreparedHieCorpus -> Either HieBenchmarkObstruction HieWireDigest
compileHieWireDigest corpus =
  case
      hieTypeIndexTypeWords
        (preparedHieTypeTable corpus)
        (fromIntegral (preparedHieDepth corpus))
    of
    Left obstruction ->
      Left (HieGraphCompilationRejected (preparedHieDepth corpus) obstruction)
    Right typeWordsValue ->
      let digest = foldl' digestHieWord emptyHieWireDigest (typeWordsList typeWordsValue)
          expectedLength = currentHieWireLength (preparedHieDepth corpus)
       in if hieWireWordCount digest == expectedLength
            then Right digest
            else
              Left
                ( UnexpectedHieWireLength
                    (preparedHieDepth corpus)
                    expectedLength
                    (hieWireWordCount digest)
                )

emptyHieWireDigest :: HieWireDigest
emptyHieWireDigest =
  HieWireDigest
    { hieWireWordCount = 0,
      hieWireWordHash = 14695981039346656037
    }

digestHieWord :: HieWireDigest -> Word64 -> HieWireDigest
digestHieWord digest wordValue =
  HieWireDigest
    { hieWireWordCount = hieWireWordCount digest + 1,
      hieWireWordHash = (hieWireWordHash digest * 1099511628211) + wordValue
    }

currentHieWireLength :: Int -> Int
currentHieWireLength depth =
  (7 * depth) + 7

doublingDagTable :: Int -> Array TypeIndex HieTypeFlat
doublingDagTable depth =
  array
    (0, fromIntegral depth)
    ( (0, HCoercionTy)
        : fmap
          ( \typeIndex ->
              ( typeIndex,
                HAppTy
                  (typeIndex - 1)
                  (HieArgs [(True, typeIndex - 1)])
              )
          )
          [1 .. fromIntegral depth]
    )

diamondFanoutCorpus :: Int -> (TypeIndex, Array TypeIndex HieTypeFlat)
diamondFanoutCorpus fanout =
  let branchIndices =
        [2 .. fromIntegral fanout + 1]
      rootIndex =
        fromIntegral fanout + 2
   in ( rootIndex,
        array
          (0, rootIndex)
          ( [ (0, HCoercionTy),
              (1, HCastTy 0)
            ]
              <> fmap
                (\branchIndex -> (branchIndex, HAppTy 1 (HieArgs [(True, 0)])))
                branchIndices
              <> [ ( rootIndex,
                     HAppTy
                       1
                       (HieArgs (fmap (\branchIndex -> (True, branchIndex)) branchIndices))
                   )
                 ]
          )
      )

deepForAllCorpus :: Int -> (TypeIndex, Array TypeIndex HieTypeFlat)
deepForAllCorpus depth =
  let binderNames =
        fmap hieBinderName [1 .. depth]
      innermostVariableIndex =
        1
      forallEntries =
        fmap
          ( \(entryOffset, binderName) ->
              let forallIndex =
                    fromIntegral entryOffset + 2
                  bodyIndex =
                    if entryOffset == 0
                      then innermostVariableIndex
                      else forallIndex - 1
               in ( forallIndex,
                    HForAllTy
                      ((binderName, 0), Specified)
                      bodyIndex
                  )
          )
          (zip [0 :: Int ..] (reverse binderNames))
      rootIndex =
        fromIntegral depth + 1
      innermostBinderName =
        maybe (hieBinderName 0) id (lastMaybe binderNames)
   in ( rootIndex,
        array
          (0, rootIndex)
          ( [ (0, HCoercionTy),
              (innermostVariableIndex, HTyVarTy innermostBinderName)
            ]
              <> forallEntries
          )
      )

variableRichDoublingCorpus ::
  Int ->
  (TypeIndex, Array TypeIndex HieTypeFlat)
variableRichDoublingCorpus requestedSize =
  let variableCount =
        max 1 requestedSize
      wrapperDepth =
        max 1 requestedSize
      variableEntries =
        fmap
          ( \variableOffset ->
              ( fromIntegral variableOffset,
                HTyVarTy (hieFreeVariableName (variableOffset + 1))
              )
          )
          [0 .. variableCount - 1]
      combinationEntries =
        fmap
          ( \combinationOffset ->
              let combinationIndex =
                    fromIntegral (variableCount + combinationOffset)
                  functionIndex =
                    if combinationOffset == 0
                      then 0
                      else combinationIndex - 1
                  argumentIndex =
                    fromIntegral (combinationOffset + 1)
               in ( combinationIndex,
                    HAppTy
                      functionIndex
                      (HieArgs [(True, argumentIndex)])
                  )
          )
          [0 .. variableCount - 2]
      combinedRootIndex =
        if variableCount == 1
          then 0
          else fromIntegral ((2 * variableCount) - 2)
      wrapperEntries =
        fmap
          ( \wrapperOffset ->
              let wrapperIndex =
                    combinedRootIndex + fromIntegral wrapperOffset
                  childIndex =
                    wrapperIndex - 1
               in ( wrapperIndex,
                    HAppTy
                      childIndex
                      (HieArgs [(True, childIndex)])
                  )
          )
          [1 .. wrapperDepth]
      rootIndex =
        combinedRootIndex + fromIntegral wrapperDepth
   in ( rootIndex,
        array
          (0, rootIndex)
          (variableEntries <> combinationEntries <> wrapperEntries)
      )

lastMaybe :: [value] -> Maybe value
lastMaybe =
  foldl' (\_ value -> Just value) Nothing

hieBinderName :: Int -> Name
hieBinderName binderIndex =
  mkSystemName
    (mkUnique 'h' (fromIntegral binderIndex))
    (mkTyVarOcc ("type" <> show binderIndex))

hieFreeVariableName :: Int -> Name
hieFreeVariableName variableIndex =
  mkSystemName
    (mkUnique 'v' (fromIntegral variableIndex))
    (mkTyVarOcc ("free" <> show variableIndex))

repeatedRootTable :: Int -> Array TypeIndex HieTypeFlat
repeatedRootTable rootCount =
  array
    (0, fromIntegral rootCount)
    ( (0, HCoercionTy)
        : fmap
          (\rootIndex -> (rootIndex, HCastTy 0))
          [1 .. fromIntegral rootCount]
    )

digestString :: Int -> String -> Int
digestString =
  foldl'
    (\digest character -> (digest * 16777619) + fromEnum character)

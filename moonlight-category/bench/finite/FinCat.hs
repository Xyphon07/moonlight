module FinCat
  ( compositionMapWeight,
    finCatBenchmarks,
    finCatExplicitCompositionMapViewWeight,
    finCatExplicitMorphismMapViewWeight,
    finCatWeight,
    finMorphismIdWeight,
    finMorphismWeight,
    finObjectIdWeight,
    morphismMapWeight,
    objectKeys,
    objectSetWeight,
    representativeCompositionPair,
    rnfFinCat,
    rnfMaybeFinMorphismPair,
  )
where

import BenchSupport (BenchSetup (..), prepareBenchValue)
import Control.DeepSeq (NFData (..))
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Category.Effect.Fixture.FinCat (sampleFinCat)
import Moonlight.Category.Pure.Category (composeMor)
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatValidationError,
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    FinMor,
    allMorphisms,
    allMorphismsFrom,
    allObjects,
    finCatExplicitCompositionMapView,
    finCatExplicitMorphismMapView,
    finCatHandle,
    finCatMorphismCount,
    finCatNonIdentityMorphismCount,
    objectCount,
    finCatObjects,
    finMorCategoryHandle,
    finMorId,
    finMorSourceId,
    finMorTargetId,
    finObjId,
    finObjectIdentityMor,
    mkFinCat,
    mkFinMorphism,
    mkFinObject,
  )
import Moonlight.Category.Pure.FiniteComposable
  ( ComposableChain,
    SizedComposableChain,
    appendComposableMorphism,
    chainDimension,
    chainMorphisms,
    chainVertices,
    enumerateComposableChains,
    sizedChainDimension,
    sizedChainValue,
    singletonComposableChain,
  )
import qualified Moonlight.Category.Presentation as Presentation
import Numeric.Natural (Natural)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

finCatBenchmarks :: Benchmark
finCatBenchmarks =
  bgroup
    "FinCat API"
    [ bgroup
        "FinPresentation strict-chain compilation"
        (finPresentationObjectCounts & fmap finPresentationBenchmark),
      bgroup
        "us-vs-world thin-total-order construction"
        (thinOrderCases & fmap thinOrderWorldBenchmark),
      bgroup
        "mkFinCat thin-total-order"
        (thinOrderCases & fmap mkFinCatBenchmark),
      bgroup
        "mkFinCat generic non-thin validation stress"
        (nonThinCases & fmap mkNonThinFinCatBenchmark),
      bgroup
        "prepared FinCat operations"
        (thinOrderCases & fmap preparedFinCatBenchmark),
      bench "appendComposableMorphism identity x1024" (nf repeatedChainAppendWeight 1024),
      chainProjectionBenchmarks
    ]
type ThinOrderCase :: Type
data ThinOrderCase = ThinOrderCase
  { thinOrderObjectCount :: !Int,
    thinOrderChainBound :: !Natural
  }
  deriving stock (Eq, Ord, Show)

thinOrderCases :: [ThinOrderCase]
thinOrderCases =
  [ ThinOrderCase 8 3,
    ThinOrderCase 16 3,
    ThinOrderCase 32 2
  ]

finPresentationObjectCounts :: [Int]
finPresentationObjectCounts =
  [8, 32, 128]

finPresentationBenchmark :: Int -> Benchmark
finPresentationBenchmark objectTotal =
  bench
    ("objects=" <> show objectTotal)
    (nf finPresentationCategoryWeight objectTotal)

finPresentationCategoryWeight :: Int -> Int
finPresentationCategoryWeight objectTotal =
  either
    (length . show)
    finCatWeight
    (strictChainPresentation objectTotal)

strictChainPresentation ::
  Int ->
  Either Presentation.FinCatBuildError FinCat
strictChainPresentation objectTotal =
  Presentation.finCategory $ do
    declaredObjects <-
      Presentation.objects
        ( fmap
            (\objectKey -> "x" <> show objectKey)
            (objectKeys objectTotal)
        )

    traverse_
      (uncurry Presentation.below)
      (zip declaredObjects (drop 1 declaredObjects))

thinOrderCaseLabel :: ThinOrderCase -> String
thinOrderCaseLabel benchCase =
  "objects="
    <> show (thinOrderObjectCount benchCase)
    <> " chain-bound="
    <> show (thinOrderChainBound benchCase)

type NonThinCase :: Type
data NonThinCase = NonThinCase
  { nonThinObjectCount :: !Int,
    nonThinParallelCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

nonThinCases :: [NonThinCase]
nonThinCases =
  [ NonThinCase 6 2,
    NonThinCase 8 2
  ]

nonThinCaseLabel :: NonThinCase -> String
nonThinCaseLabel benchCase =
  "objects="
    <> show (nonThinObjectCount benchCase)
    <> " parallel="
    <> show (nonThinParallelCount benchCase)

mkFinCatBenchmark :: ThinOrderCase -> Benchmark
mkFinCatBenchmark benchCase =
  bench (thinOrderCaseLabel benchCase) (nf mkThinOrderCategoryWeight benchCase)

thinOrderWorldBenchmark :: ThinOrderCase -> Benchmark
thinOrderWorldBenchmark benchCase =
  bgroup
    (thinOrderCaseLabel benchCase)
    [ bench "moonlight: mkFinCat validated finite category" (nf mkThinOrderCategoryWeight benchCase),
      bench "world: containers relation+composition maps" (nf rawThinOrderMapWeight (thinOrderObjectCount benchCase))
    ]

mkNonThinFinCatBenchmark :: NonThinCase -> Benchmark
mkNonThinFinCatBenchmark benchCase =
  bench (nonThinCaseLabel benchCase) (nf mkNonThinCategoryWeight benchCase)

preparedFinCatBenchmark :: ThinOrderCase -> Benchmark
preparedFinCatBenchmark benchCase =
  env (prepareBenchValue (preparedThinOrderCase benchCase)) $ \prepared ->
    bgroup
      (thinOrderCaseLabel benchCase)
      [ bench "allObjects/allMorphisms" (nf preparedFinCatCarrierWeight prepared),
        bench "compose generator triples" (nf preparedFinCatCompositionWeight prepared),
        bench "enumerateComposableChains" (nf preparedFinCatChainWeight prepared)
      ]

type PreparedFinCatCase :: Type
data PreparedFinCatCase = PreparedFinCatCase
  { preparedFinCatChainBound :: !Natural,
    preparedFinCatCategory :: !FinCat,
    preparedFinCatCompositions :: ![(FinMor, FinMor)]
  }

instance NFData PreparedFinCatCase where
  rnf prepared =
    preparedFinCatChainBound prepared
      `seq` rnfFinCat (preparedFinCatCategory prepared)
      `seq` rnfFinMorphismPairs (preparedFinCatCompositions prepared)
      `seq` ()

preparedThinOrderCase :: ThinOrderCase -> BenchSetup PreparedFinCatCase
preparedThinOrderCase benchCase =
  BenchSetup $ do
    categoryValue <- first (show . NonEmpty.toList) (thinTotalOrderCategory (thinOrderObjectCount benchCase))
    compositionPairs <- thinOrderCompositionPairs categoryValue (thinOrderObjectCount benchCase)
    pure
      PreparedFinCatCase
        { preparedFinCatChainBound = thinOrderChainBound benchCase,
          preparedFinCatCategory = categoryValue,
          preparedFinCatCompositions = compositionPairs
        }

mkThinOrderCategoryWeight :: ThinOrderCase -> Int
mkThinOrderCategoryWeight benchCase =
  either
    (length . NonEmpty.toList)
    finCatWeight
    (thinTotalOrderCategory (thinOrderObjectCount benchCase))

mkNonThinCategoryWeight :: NonThinCase -> Int
mkNonThinCategoryWeight benchCase =
  either
    (length . NonEmpty.toList)
    finCatWeight
    (nonThinTotalOrderCategory (nonThinObjectCount benchCase) (nonThinParallelCount benchCase))

rawThinOrderMapWeight :: Int -> Int
rawThinOrderMapWeight objectTotal =
  objectSetWeight (Set.fromAscList (FinObjectId <$> objectKeys objectTotal))
    + morphismMapWeight (Map.fromList (morphismBuckets objectTotal))
    + compositionMapWeight (Map.fromList (compositionEntries objectTotal))

preparedFinCatCarrierWeight :: PreparedFinCatCase -> Int
preparedFinCatCarrierWeight prepared =
  finCatWeight (preparedFinCatCategory prepared)
    + length (allObjects (preparedFinCatCategory prepared))
    + length (allMorphisms (preparedFinCatCategory prepared))
    + sourceBucketWeight (preparedFinCatCategory prepared)

preparedFinCatCompositionWeight :: PreparedFinCatCase -> Int
preparedFinCatCompositionWeight prepared =
  preparedFinCatCompositions prepared
    & List.foldl'
      ( \accumulated (leftMorphism, rightMorphism) ->
          accumulated
            + either
              (const 0)
              finMorphismWeight
              (composeMor (preparedFinCatCategory prepared) leftMorphism rightMorphism)
      )
      0

preparedFinCatChainWeight :: PreparedFinCatCase -> Int
preparedFinCatChainWeight prepared =
  enumerateComposableChains (preparedFinCatCategory prepared) (preparedFinCatChainBound prepared)
    & fmap sizedComposableChainWeight
    & sum

repeatedChainAppendWeight :: Int -> Int
repeatedChainAppendWeight appendCount =
  case mkFinObject sampleFinCat (FinObjectId 0) of
    Left _ -> 0
    Right startObject ->
      let identityMorphism = finObjectIdentityMor startObject
       in foldM
            (appendComposableMorphism sampleFinCat)
            (singletonComposableChain startObject)
            (replicate appendCount identityMorphism)
            & either
              (const 0)
              ( \chainValue ->
                  fromIntegral (chainDimension chainValue)
                    + sum (finMorphismWeight <$> chainMorphisms chainValue)
              )

chainProjectionBenchmarks :: Benchmark
chainProjectionBenchmarks =
  env (prepareBenchValue (repeatedChainProjectionSetup 1024)) $ \(PreparedComposableChain chainValue) ->
    bgroup
      "ComposableChain projections identity x1024"
      [ bench "chainMorphisms" (nf chainMorphismProjectionWeight chainValue),
        bench "chainVertices" (nf chainVertexProjectionWeight chainValue)
      ]

newtype PreparedComposableChain = PreparedComposableChain (ComposableChain FinCat)

instance NFData PreparedComposableChain where
  rnf (PreparedComposableChain chainValue) =
    chainMorphismProjectionWeight chainValue `seq`
      chainVertexProjectionWeight chainValue `seq`
        ()

repeatedChainProjectionSetup :: Int -> BenchSetup PreparedComposableChain
repeatedChainProjectionSetup appendCount =
  BenchSetup
    (PreparedComposableChain <$> runBenchSetup (repeatedIdentityChain appendCount))

repeatedIdentityChain :: Int -> BenchSetup (ComposableChain FinCat)
repeatedIdentityChain appendCount =
  BenchSetup $ do
    startObject <- first (const "sample FinCat is missing object 0") (mkFinObject sampleFinCat (FinObjectId 0))
    let identityMorphism = finObjectIdentityMor startObject
    first
      (const "the sample identity morphism did not append to its own chain")
      ( foldM
          (appendComposableMorphism sampleFinCat)
          (singletonComposableChain startObject)
          (replicate appendCount identityMorphism)
      )

chainMorphismProjectionWeight :: ComposableChain FinCat -> Int
chainMorphismProjectionWeight chainValue =
  sum (finMorphismWeight <$> chainMorphisms chainValue)

chainVertexProjectionWeight :: ComposableChain FinCat -> Int
chainVertexProjectionWeight chainValue =
  chainVertices chainValue
    & NonEmpty.toList
    & fmap (finObjectIdWeight . finObjId)
    & sum

sourceBucketWeight :: FinCat -> Int
sourceBucketWeight categoryValue =
  allObjects categoryValue
    & fmap
      ( \objectValue ->
          finObjectIdWeight (finObjId objectValue)
            + sum (finMorphismWeight <$> allMorphismsFrom categoryValue objectValue)
      )
    & sum

finCatWeight :: FinCat -> Int
finCatWeight categoryValue =
  finCatHandle categoryValue
    `seq` finCatCarrierWeight categoryValue

finCatCarrierWeight :: FinCat -> Int
finCatCarrierWeight categoryValue =
  objectSetWeight (finCatObjects categoryValue)
    + objectCount categoryValue
    + finCatMorphismCount categoryValue
    + finCatNonIdentityMorphismCount categoryValue

finCatExplicitMorphismMapViewWeight :: FinCat -> Int
finCatExplicitMorphismMapViewWeight =
  morphismMapWeight . finCatExplicitMorphismMapView

finCatExplicitCompositionMapViewWeight :: FinCat -> Int
finCatExplicitCompositionMapViewWeight =
  compositionMapWeight . finCatExplicitCompositionMapView

morphismMapWeight :: Map (FinObjectId, FinObjectId) [FinMorphismId] -> Int
morphismMapWeight =
  Map.foldlWithKey'
    ( \accumulated (sourceId, targetId) morphismIds ->
        accumulated
          + finObjectIdWeight sourceId
          + finObjectIdWeight targetId
          + sum (finMorphismIdWeight <$> morphismIds)
    )
    0

compositionMapWeight :: Map (FinMorphismId, FinMorphismId) FinMorphismId -> Int
compositionMapWeight =
  Map.foldlWithKey'
    ( \accumulated (leftId, rightId) resultId ->
        accumulated
          + finMorphismIdWeight leftId
          + finMorphismIdWeight rightId
          + finMorphismIdWeight resultId
    )
    0

sizedComposableChainWeight :: SizedComposableChain FinCat -> Int
sizedComposableChainWeight sizedChain =
  fromIntegral (sizedChainDimension sizedChain)
    + sum (finMorphismIdWeight . finMorId <$> chainMorphisms (sizedChainValue sizedChain))

thinTotalOrderCategory :: Int -> Either (NonEmpty FinCatValidationError) FinCat
thinTotalOrderCategory objectTotal =
  mkFinCat
    (Set.fromAscList (FinObjectId <$> objectKeys objectTotal))
    (Map.fromList (morphismBuckets objectTotal))
    (Map.fromList (compositionEntries objectTotal))

nonThinTotalOrderCategory :: Int -> Int -> Either (NonEmpty FinCatValidationError) FinCat
nonThinTotalOrderCategory objectTotal parallelCount =
  mkFinCat
    (Set.fromAscList (FinObjectId <$> objectKeys objectTotal))
    (Map.fromList (parallelMorphismBuckets objectTotal parallelCount))
    (Map.fromList (parallelCompositionEntries objectTotal parallelCount))

objectKeys :: Int -> [Int]
objectKeys objectTotal =
  [0 .. objectTotal - 1]

strictObjectPairs :: Int -> [(Int, Int)]
strictObjectPairs objectTotal =
  objectKeys objectTotal
    >>= (\sourceKey -> fmap (\targetKey -> (sourceKey, targetKey)) [sourceKey + 1 .. objectTotal - 1])

strictObjectTriples :: Int -> [(Int, Int, Int)]
strictObjectTriples objectTotal =
  objectKeys objectTotal
    >>= (\sourceKey -> [sourceKey + 1 .. objectTotal - 1] >>= middleEntries sourceKey)
  where
    middleEntries sourceKey middleKey =
      [middleKey + 1 .. objectTotal - 1]
        & fmap (\targetKey -> (sourceKey, middleKey, targetKey))

morphismBuckets :: Int -> [((FinObjectId, FinObjectId), [FinMorphismId])]
morphismBuckets objectTotal =
  strictObjectPairs objectTotal
    & fmap
      ( \(sourceKey, targetKey) ->
          ( (FinObjectId sourceKey, FinObjectId targetKey),
            [thinMorphismId sourceKey targetKey]
          )
      )

compositionEntries :: Int -> [((FinMorphismId, FinMorphismId), FinMorphismId)]
compositionEntries objectTotal =
  strictObjectTriples objectTotal
    & fmap
      ( \(sourceKey, middleKey, targetKey) ->
          ( (thinMorphismId middleKey targetKey, thinMorphismId sourceKey middleKey),
            thinMorphismId sourceKey targetKey
          )
      )

parallelMorphismBuckets :: Int -> Int -> [((FinObjectId, FinObjectId), [FinMorphismId])]
parallelMorphismBuckets objectTotal parallelCount =
  strictObjectPairs objectTotal
    & fmap
      ( \(sourceKey, targetKey) ->
          ( (FinObjectId sourceKey, FinObjectId targetKey),
            parallelMorphismId objectTotal parallelCount sourceKey targetKey <$> [0 .. parallelCount - 1]
          )
      )

parallelCompositionEntries :: Int -> Int -> [((FinMorphismId, FinMorphismId), FinMorphismId)]
parallelCompositionEntries objectTotal parallelCount =
  strictObjectTriples objectTotal
    >>= ( \(sourceKey, middleKey, targetKey) ->
            [ ( ( parallelMorphismId objectTotal parallelCount middleKey targetKey leftVariant,
                  parallelMorphismId objectTotal parallelCount sourceKey middleKey rightVariant
                ),
                parallelMorphismId objectTotal parallelCount sourceKey targetKey 0
              )
            | leftVariant <- [0 .. parallelCount - 1],
              rightVariant <- [0 .. parallelCount - 1]
            ]
        )

parallelMorphismId :: Int -> Int -> Int -> Int -> Int -> FinMorphismId
parallelMorphismId objectTotal parallelCount sourceKey targetKey variantKey =
  FinGeneratorMorphismId (FinGeneratorId ((((sourceKey * objectTotal) + targetKey) * parallelCount) + variantKey))

thinOrderCompositionPairs :: FinCat -> Int -> Either String [(FinMor, FinMor)]
thinOrderCompositionPairs categoryValue objectTotal =
  strictObjectTriples objectTotal
    & traverse
      ( \(sourceKey, middleKey, targetKey) ->
          (,)
            <$> finMorphismByKeys categoryValue middleKey targetKey
            <*> finMorphismByKeys categoryValue sourceKey middleKey
      )

finMorphismByKeys :: FinCat -> Int -> Int -> Either String FinMor
finMorphismByKeys categoryValue sourceKey targetKey =
  first
    show
    (mkFinMorphism categoryValue (thinMorphismId sourceKey targetKey))

thinMorphismId :: Int -> Int -> FinMorphismId
thinMorphismId sourceKey targetKey =
  FinGeneratorMorphismId (FinGeneratorId (sourceKey * 4096 + targetKey))

finMorphismWeight :: FinMor -> Int
finMorphismWeight morphism =
  finMorCategoryHandle morphism
    `seq` finMorphismIdWeight (finMorId morphism)
      + finObjectIdWeight (finMorSourceId morphism)
      + finObjectIdWeight (finMorTargetId morphism)

finObjectIdWeight :: FinObjectId -> Int
finObjectIdWeight (FinObjectId objectKey) =
  objectKey

finMorphismIdWeight :: FinMorphismId -> Int
finMorphismIdWeight morphismId =
  case morphismId of
    FinIdentityId (FinObjectId objectKey) -> objectKey
    FinGeneratorMorphismId (FinGeneratorId generatorKey) -> generatorKey

objectSetWeight :: Set FinObjectId -> Int
objectSetWeight =
  sum . fmap finObjectIdWeight . Set.toAscList

rnfFinCat :: FinCat -> ()
rnfFinCat categoryValue =
  finCatWeight categoryValue
    `seq` sourceBucketWeight categoryValue
    `seq` allMorphismsWeight categoryValue
    `seq` ()

allMorphismsWeight :: FinCat -> Int
allMorphismsWeight categoryValue =
  allMorphisms categoryValue
    & fmap finMorphismWeight
    & sum

rnfFinMorphismPairs :: [(FinMor, FinMor)] -> ()
rnfFinMorphismPairs morphismPairs =
  morphismPairs
    & List.foldl'
      ( \accumulated (leftMorphism, rightMorphism) ->
          accumulated + finMorphismWeight leftMorphism + finMorphismWeight rightMorphism
      )
      0
    & (`seq` ())

rnfMaybeFinMorphismPair :: Maybe (FinMor, FinMor) -> ()
rnfMaybeFinMorphismPair maybePair =
  case maybePair of
    Nothing -> ()
    Just (leftMorphism, rightMorphism) ->
      finMorphismWeight leftMorphism
        `seq` finMorphismWeight rightMorphism
        `seq` ()

representativeCompositionPair :: FinCat -> Maybe (FinMor, FinMor)
representativeCompositionPair categoryValue =
  allMorphisms categoryValue
    >>= ( \rightMorphism ->
            case mkFinObject categoryValue (finMorTargetId rightMorphism) of
              Left _ -> []
              Right middleObject ->
                allMorphismsFrom categoryValue middleObject
                  & filter nonIdentityMorphism
                  & fmap (,rightMorphism)
        )
    & filter (nonIdentityMorphism . snd)
    & firstMaybe

nonIdentityMorphism :: FinMor -> Bool
nonIdentityMorphism morphism =
  case finMorId morphism of
    FinIdentityId _ -> False
    FinGeneratorMorphismId _ -> True

firstMaybe :: [value] -> Maybe value
firstMaybe values =
  case values of
    [] -> Nothing
    firstValue : _ -> Just firstValue

module Invertibility
  ( invertibilityBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinGeneratorId (..),
    FinMorphismId (..),
    FinObjectId (..),
    FinMor,
    allObjects,
    finCatMorphismCount,
    objectCount,
    finMorId,
    finMorSourceId,
    finMorTargetId,
    finObjId,
    foldMapFinMorphisms,
    mkFinCat,
  )
import Moonlight.Category.Pure.Invertibility
  ( AutomorphismGroupoid,
    CoreGroupoid,
    InvertibilityIndex,
    automorphismGroupAt,
    automorphismGroupoid,
    automorphismGroupoidFromIndex,
    automorphismGroupoidObjects,
    coreGroupoid,
    coreGroupoidFromIndex,
    coreGroupoidMorphisms,
    coreGroupoidMorphismsBetween,
    coreGroupoidObjects,
    forgetAutomorphismGroupoidMorphism,
    forgetAutomorphismGroupoidObject,
    forgetCoreGroupoidMorphism,
    forgetCoreGroupoidObject,
    invertibilityIndex,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

data PreparedInvertibilityCase = PreparedInvertibilityCase
  { preparedObjectCount :: !Int,
    preparedCategory :: !FinCat
  }

data PreparedIndexedInvertibilityCase = PreparedIndexedInvertibilityCase
  { preparedIndexedObjectCount :: !Int,
    preparedIndexedCategory :: !FinCat,
    preparedIndex :: InvertibilityIndex FinCat
  }

instance NFData PreparedInvertibilityCase where
  rnf prepared =
    preparedObjectCount prepared
      `seq` finCatDeepWeight (preparedCategory prepared)
      `seq` ()

instance NFData PreparedIndexedInvertibilityCase where
  rnf prepared =
    preparedIndexedObjectCount prepared
      `seq` invertibilityIndexWeight (preparedIndexedCategory prepared) (preparedIndex prepared)
      `seq` ()

invertibilityBenchmarks :: Benchmark
invertibilityBenchmarks =
  bgroup
    "invertibility / core groupoid"
    [ bgroup
        "complete pair-groupoid construction"
        (fmap completePairGroupoidBenchmark [4, 8, 12]),
      bgroup
        "index construction by size"
        (fmap preparedInvertibilityIndexBenchmark [4, 8, 12]),
      preparedIndexedViewBenchmarks 12
    ]

completePairGroupoidBenchmark :: Int -> Benchmark
completePairGroupoidBenchmark categoryObjectCount =
  bench ("n=" <> show categoryObjectCount) (nf completePairGroupoidWeight categoryObjectCount)

preparedInvertibilityIndexBenchmark :: Int -> Benchmark
preparedInvertibilityIndexBenchmark categoryObjectCount =
  env (prepareCompletePairGroupoid categoryObjectCount) $ \prepared ->
    bench ("n=" <> show categoryObjectCount) (nf preparedInvertibilityIndexWeight prepared)

preparedIndexedViewBenchmarks :: Int -> Benchmark
preparedIndexedViewBenchmarks categoryObjectCount =
  env (prepareIndexedInvertibilityCase categoryObjectCount) $ \prepared ->
    bgroup
      ("precomputed index views n=" <> show categoryObjectCount)
      [ bench "coreGroupoidFromIndex" (nf preparedCoreFromIndexWeight prepared),
        bench "automorphismGroupoidFromIndex" (nf preparedAutomorphismFromIndexWeight prepared),
        bench "coreGroupoidMorphismsBetween all object pairs" (nf preparedCoreBetweenWeight prepared),
        bench "automorphismGroupAt all objects" (nf preparedAutomorphismAtWeight prepared),
        bench "direct coreGroupoid rebuilds index" (nf preparedDirectCoreGroupoidWeight prepared),
        bench "direct automorphismGroupoid rebuilds index" (nf preparedDirectAutomorphismGroupoidWeight prepared)
      ]

prepareCompletePairGroupoid :: Int -> IO PreparedInvertibilityCase
prepareCompletePairGroupoid categoryObjectCount =
  case completePairGroupoid categoryObjectCount of
    Left failure -> ioError (userError failure)
    Right categoryValue ->
      finCatDeepWeight categoryValue `seq` pure (PreparedInvertibilityCase categoryObjectCount categoryValue)

prepareIndexedInvertibilityCase :: Int -> IO PreparedIndexedInvertibilityCase
prepareIndexedInvertibilityCase categoryObjectCount = do
  PreparedInvertibilityCase _ categoryValue <- prepareCompletePairGroupoid categoryObjectCount
  let indexValue = invertibilityIndex categoryValue
      forcedWeight = invertibilityIndexWeight categoryValue indexValue
  forcedWeight `seq` pure (PreparedIndexedInvertibilityCase categoryObjectCount categoryValue indexValue)

completePairGroupoidWeight :: Int -> Int
completePairGroupoidWeight categoryObjectCount =
  completePairGroupoid categoryObjectCount
    & either length finCatDeepWeight

preparedInvertibilityIndexWeight :: PreparedInvertibilityCase -> Int
preparedInvertibilityIndexWeight prepared =
  let categoryValue = preparedCategory prepared
   in invertibilityIndexWeight categoryValue (invertibilityIndex categoryValue)

preparedCoreFromIndexWeight :: PreparedIndexedInvertibilityCase -> Int
preparedCoreFromIndexWeight prepared =
  let categoryValue = preparedIndexedCategory prepared
      indexValue = preparedIndex prepared
   in coreGroupoidWeight (coreGroupoidFromIndex categoryValue indexValue)

preparedAutomorphismFromIndexWeight :: PreparedIndexedInvertibilityCase -> Int
preparedAutomorphismFromIndexWeight prepared =
  let categoryValue = preparedIndexedCategory prepared
      indexValue = preparedIndex prepared
   in automorphismGroupoidWeight (automorphismGroupoidFromIndex categoryValue indexValue)

preparedCoreBetweenWeight :: PreparedIndexedInvertibilityCase -> Int
preparedCoreBetweenWeight prepared =
  let categoryValue = preparedIndexedCategory prepared
      indexValue = preparedIndex prepared
      groupoidValue = coreGroupoidFromIndex categoryValue indexValue
      objects = coreGroupoidObjects groupoidValue
   in objects
        & fmap
          ( \sourceObject ->
              objects
                & fmap
                  ( \targetObject ->
                      coreGroupoidMorphismsBetween groupoidValue sourceObject targetObject
                        & fmap (finMorphismWeight . forgetCoreGroupoidMorphism)
                        & sum
                  )
                & sum
          )
        & sum

preparedAutomorphismAtWeight :: PreparedIndexedInvertibilityCase -> Int
preparedAutomorphismAtWeight prepared =
  let categoryValue = preparedIndexedCategory prepared
      indexValue = preparedIndex prepared
      groupoidValue = automorphismGroupoidFromIndex categoryValue indexValue
   in automorphismGroupoidObjects groupoidValue
        & fmap
          ( \objectValue ->
              automorphismGroupAt groupoidValue objectValue
                & fmap (finMorphismWeight . forgetAutomorphismGroupoidMorphism)
                & sum
          )
        & sum

preparedDirectCoreGroupoidWeight :: PreparedIndexedInvertibilityCase -> Int
preparedDirectCoreGroupoidWeight prepared =
  let categoryValue = preparedIndexedCategory prepared
   in coreGroupoidWeight (coreGroupoid categoryValue)

preparedDirectAutomorphismGroupoidWeight :: PreparedIndexedInvertibilityCase -> Int
preparedDirectAutomorphismGroupoidWeight prepared =
  let categoryValue = preparedIndexedCategory prepared
   in automorphismGroupoidWeight (automorphismGroupoid categoryValue)

invertibilityIndexWeight :: FinCat -> InvertibilityIndex FinCat -> Int
invertibilityIndexWeight categoryValue indexValue =
  coreGroupoidWeight (coreGroupoidFromIndex categoryValue indexValue)
    + automorphismGroupoidWeight (automorphismGroupoidFromIndex categoryValue indexValue)

coreGroupoidWeight :: CoreGroupoid FinCat -> Int
coreGroupoidWeight groupoidValue =
  sum (fmap (finObjectWeight . finObjId . forgetCoreGroupoidObject) (coreGroupoidObjects groupoidValue))
    + sum (fmap (finMorphismWeight . forgetCoreGroupoidMorphism) (coreGroupoidMorphisms groupoidValue))

automorphismGroupoidWeight :: AutomorphismGroupoid FinCat -> Int
automorphismGroupoidWeight groupoidValue =
  sum (fmap (finObjectWeight . finObjId . forgetAutomorphismGroupoidObject) (automorphismGroupoidObjects groupoidValue))
    + automorphismWeight
  where
    automorphismWeight =
      automorphismGroupoidObjects groupoidValue
        & fmap
          ( \objectValue ->
              automorphismGroupAt groupoidValue objectValue
                & fmap (finMorphismWeight . forgetAutomorphismGroupoidMorphism)
                & sum
          )
        & sum

finCatDeepWeight :: FinCat -> Int
finCatDeepWeight categoryValue =
  finCatShapeWeight categoryValue
    + getSum (foldMapFinMorphisms (Sum . finMorphismWeight) categoryValue)

finCatShapeWeight :: FinCat -> Int
finCatShapeWeight categoryValue =
  objectCount categoryValue
    + finCatMorphismCount categoryValue
    + sum (fmap (finObjectWeight . finObjId) (allObjects categoryValue))

completePairGroupoid :: Int -> Either String FinCat
completePairGroupoid categoryObjectCount =
  mkFinCat objects morphismMap compositionMap
    & either (Left . show) Right
  where
    objectIds = fmap FinObjectId [0 .. categoryObjectCount - 1]
    objects = Set.fromList objectIds
    nonIdentityEndpoints =
      [ (sourceId, targetId)
      | sourceId <- objectIds,
        targetId <- objectIds,
        sourceId /= targetId
      ]
    morphismMap =
      nonIdentityEndpoints
        & fmap (\(sourceId, targetId) -> ((sourceId, targetId), [generatorMorphismId categoryObjectCount sourceId targetId]))
        & Map.fromList
    compositionMap =
      [ ( (generatorMorphismId categoryObjectCount middleId targetId, generatorMorphismId categoryObjectCount sourceId middleId),
          resultMorphismId categoryObjectCount sourceId targetId
        )
      | sourceId <- objectIds,
        middleId <- objectIds,
        targetId <- objectIds,
        sourceId /= middleId,
        middleId /= targetId
      ]
        & Map.fromList

generatorMorphismId :: Int -> FinObjectId -> FinObjectId -> FinMorphismId
generatorMorphismId categoryObjectCount (FinObjectId sourceId) (FinObjectId targetId) =
  FinGeneratorMorphismId (FinGeneratorId (sourceId * categoryObjectCount + targetId))

resultMorphismId :: Int -> FinObjectId -> FinObjectId -> FinMorphismId
resultMorphismId categoryObjectCount sourceId targetId =
  if sourceId == targetId
    then FinIdentityId sourceId
    else generatorMorphismId categoryObjectCount sourceId targetId

finObjectWeight :: FinObjectId -> Int
finObjectWeight (FinObjectId objectId) = objectId

finMorphismWeight :: FinMor -> Int
finMorphismWeight morphismValue =
  finMorphismIdWeight (finMorId morphismValue)
    + finObjectWeight (finMorSourceId morphismValue)
    + finObjectWeight (finMorTargetId morphismValue)

finMorphismIdWeight :: FinMorphismId -> Int
finMorphismIdWeight morphismId =
  case morphismId of
    FinIdentityId objectId -> finObjectWeight objectId
    FinGeneratorMorphismId (FinGeneratorId generatorId) -> generatorId

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module Adhesive.Subset
  ( finiteSubsetDPOBenchmarks,
  )
where

import BenchSupport (boolWeight)
import Control.DeepSeq (NFData (..))
import Data.Function ((&))
import Data.List qualified as List
import Moonlight.Category.Pure.Adhesive
  ( AdhesiveCategory (..),
    DenseIntSet,
    MonicMatchComponents (..),
    PBPOAdhesiveCategory,
    PBPOComplementWitness,
    PushoutComplementWitness,
    PushoutComplementComponents (..),
    denseIntSetDifference,
    denseIntSetFromAscList,
    denseIntSetIntersection,
    denseIntSetIsSubsetOf,
    denseIntSetSize,
    denseIntSetUnion,
    denseIntSetWeight,
    monicMatchArrow,
    pbpoComplement,
    pbpoComplementBorrowedLeg,
    pbpoComplementPullbackObject,
    pbpoComplementPullbackToBorrowed,
    pbpoComplementPullbackToMatch,
    pbpoComplementPushoutFromComplement,
    pbpoComplementPushoutFromMatch,
    pbpoComplementPushoutObject,
    pbpoComplementResidualLeg,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pushoutComplement,
    pushoutComplementBorrowedLeg,
    pushoutComplementObject,
    pushoutComplementResidualLeg,
    pushoutComplementSquareCommutes,
    witnessMonic,
  )
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Limits (HasPullbacks (..), HasPushouts (..), pullback, pushout)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

data SubsetCategory = SubsetCategory

data SubsetTwoMor

data SubsetCompositor = SubsetCompositor

newtype SubsetObject = SubsetObject
  { subsetObjectElements :: DenseIntSet
  }
  deriving stock (Eq, Ord, Show)

data SubsetMorphism = SubsetMorphism
  { subsetMorphismSource :: !SubsetObject,
    subsetMorphismTarget :: !SubsetObject
  }
  deriving stock (Eq, Ord, Show)

data SubsetRewriteCase = SubsetRewriteCase
  { subsetRewriteRuleLeg :: !SubsetMorphism,
    subsetRewriteMatch :: !SubsetMorphism
  }
  deriving stock (Eq, Ord, Show)

data PreparedSubsetRewriteBatch = PreparedSubsetRewriteBatch
  { preparedSubsetObjectSize :: !Int,
    preparedSubsetCases :: ![SubsetRewriteCase]
  }
  deriving stock (Eq, Show)

instance NFData SubsetObject where
  rnf (SubsetObject elements) =
    denseIntSetSize elements `seq` ()

instance NFData SubsetMorphism where
  rnf morphism =
    rnf (subsetMorphismSource morphism)
      `seq` rnf (subsetMorphismTarget morphism)

instance NFData SubsetRewriteCase where
  rnf rewriteCase =
    rnf (subsetRewriteRuleLeg rewriteCase)
      `seq` rnf (subsetRewriteMatch rewriteCase)

instance NFData PreparedSubsetRewriteBatch where
  rnf prepared =
    preparedSubsetObjectSize prepared
      `seq` rnf (preparedSubsetCases prepared)

instance Category SubsetCategory where
  type Ob SubsetCategory = SubsetObject
  type Mor SubsetCategory = SubsetMorphism
  type TwoMor SubsetCategory = SubsetTwoMor
  type Compositor SubsetCategory = SubsetCompositor

  identity _ objectValue =
    Right (SubsetMorphism objectValue objectValue)

  compose _ leftMorphism rightMorphism
    | subsetMorphismTarget rightMorphism == subsetMorphismSource leftMorphism =
        Right
          ( SubsetMorphism
              (subsetMorphismSource rightMorphism)
              (subsetMorphismTarget leftMorphism),
            SubsetCompositor
          )
    | otherwise =
        Left ()

  source _ =
    Right . subsetMorphismSource

  target _ =
    Right . subsetMorphismTarget

instance HasPullbacks SubsetCategory where
  pullback _ leftMorphism rightMorphism
    | subsetMorphismTarget leftMorphism == subsetMorphismTarget rightMorphism = do
        pullbackElements <-
          denseIntSetIntersection
            (subsetObjectElements (subsetMorphismSource leftMorphism))
            (subsetObjectElements (subsetMorphismSource rightMorphism))
        let pullbackObjectValue = SubsetObject pullbackElements
        pure
          ( pullbackObjectValue,
            SubsetMorphism pullbackObjectValue (subsetMorphismSource leftMorphism),
            SubsetMorphism pullbackObjectValue (subsetMorphismSource rightMorphism)
          )
    | otherwise =
        Nothing

  pullbackMediator _ leftMorphism rightMorphism coneLeft coneRight
    | subsetMorphismTarget leftMorphism == subsetMorphismTarget rightMorphism
        && subsetMorphismTarget coneLeft == subsetMorphismSource leftMorphism
        && subsetMorphismTarget coneRight == subsetMorphismSource rightMorphism
        && subsetMorphismSource coneLeft == subsetMorphismSource coneRight = do
        pullbackElements <-
          denseIntSetIntersection
            (subsetObjectElements (subsetMorphismSource leftMorphism))
            (subsetObjectElements (subsetMorphismSource rightMorphism))
        sourceContained <-
          denseIntSetIsSubsetOf
            (subsetObjectElements (subsetMorphismSource coneLeft))
            pullbackElements
        if sourceContained
          then Just (SubsetMorphism (subsetMorphismSource coneLeft) (SubsetObject pullbackElements))
          else Nothing
    | otherwise =
        Nothing

instance HasPushouts SubsetCategory where
  pushout _ leftMorphism rightMorphism
    | subsetMorphismSource leftMorphism == subsetMorphismSource rightMorphism = do
        pushoutElements <-
          denseIntSetUnion
            (subsetObjectElements (subsetMorphismTarget leftMorphism))
            (subsetObjectElements (subsetMorphismTarget rightMorphism))
        let pushoutObjectValue = SubsetObject pushoutElements
        pure
          ( pushoutObjectValue,
            SubsetMorphism (subsetMorphismTarget leftMorphism) pushoutObjectValue,
            SubsetMorphism (subsetMorphismTarget rightMorphism) pushoutObjectValue
          )
    | otherwise =
        Nothing

instance AdhesiveCategory SubsetCategory where
  monicMatchComponents _ morphism
    | subsetMorphismIsInclusion morphism =
        Just (MonicMatchComponents morphism)
    | otherwise =
        Nothing

  pushoutComplementComponents _ ruleLeg monicMatch
    | subsetMorphismIsInclusion ruleLeg
        && subsetMorphismIsInclusion matchArrow
        && subsetMorphismTarget ruleLeg == subsetMorphismSource matchArrow = do
        let kernelObject = subsetMorphismSource ruleLeg
            ruleObject = subsetMorphismTarget ruleLeg
            ambientObject = subsetMorphismTarget matchArrow
        ambientRemainder <- denseIntSetDifference (subsetObjectElements ambientObject) (subsetObjectElements ruleObject)
        complementElements <- denseIntSetUnion (subsetObjectElements kernelObject) ambientRemainder
        let complementObject = SubsetObject complementElements
        pure
          PushoutComplementComponents
            { pushoutComplementComponentObject = complementObject,
              pushoutComplementComponentBorrowedLeg = SubsetMorphism complementObject ambientObject,
              pushoutComplementComponentResidualLeg = SubsetMorphism kernelObject complementObject
            }
    | otherwise =
        Nothing
    where
      matchArrow =
        monicMatchArrow monicMatch

instance PBPOAdhesiveCategory SubsetCategory

finiteSubsetDPOBenchmarks :: Benchmark
finiteSubsetDPOBenchmarks =
  bgroup
    "finite-subset DPO/PBPO size curves"
    (fmap finiteSubsetDPOBenchmark [32, 128, 512])

finiteSubsetDPOBenchmark :: Int -> Benchmark
finiteSubsetDPOBenchmark objectSize =
  env (prepareSubsetRewriteBatch objectSize) $ \prepared ->
    bgroup
      ("ambient=" <> show objectSize <> ", cases=64")
      [ bench "pullback intersection witnesses" (nf subsetPullbackBatchWeight prepared),
        bench "pushout union witnesses" (nf subsetPushoutBatchWeight prepared),
        bench "DPO pushoutComplement witnesses" (nf subsetPushoutComplementBatchWeight prepared),
        bench "DPO square commute checks" (nf subsetPushoutComplementCommuteBatchWeight prepared),
        bench "PBPO default complement witnesses" (nf subsetPBPOComplementBatchWeight prepared),
        bench "PBPO pullback+pushout commute checks" (nf subsetPBPOCommuteBatchWeight prepared)
      ]

prepareSubsetRewriteBatch :: Int -> IO PreparedSubsetRewriteBatch
prepareSubsetRewriteBatch objectSize =
  case traverse (subsetRewriteCase objectSize) [0 .. 63] of
    Nothing ->
      ioError (userError ("failed to prepare finite subset DPO benchmark for ambient size " <> show objectSize))
    Just rewriteCases ->
      let prepared =
            PreparedSubsetRewriteBatch
              { preparedSubsetObjectSize = objectSize,
                preparedSubsetCases = rewriteCases
              }
       in rnf prepared `seq` pure prepared

subsetRewriteCase :: Int -> Int -> Maybe SubsetRewriteCase
subsetRewriteCase objectSize seed = do
  kernelObject <- subsetIntervalObject normalizedSize offset 0 kernelSize
  deletedObject <- subsetIntervalObject normalizedSize offset kernelSize deletedSize
  retainedObject <- subsetIntervalObject normalizedSize offset (kernelSize + deletedSize) retainedSize
  ruleObject <- subsetObjectUnion kernelObject deletedObject
  ambientObject <- subsetObjectUnion ruleObject retainedObject
  pure
    SubsetRewriteCase
      { subsetRewriteRuleLeg = SubsetMorphism kernelObject ruleObject,
        subsetRewriteMatch = SubsetMorphism ruleObject ambientObject
      }
  where
    normalizedSize =
      max 4 objectSize
    kernelSize =
      normalizedSize `div` 4
    deletedSize =
      normalizedSize `div` 4
    retainedSize =
      normalizedSize - kernelSize - deletedSize
    offset =
      (seed * 7) `mod` normalizedSize

subsetIntervalObject :: Int -> Int -> Int -> Int -> Maybe SubsetObject
subsetIntervalObject universeSize offset start count =
  SubsetObject <$> denseIntSetFromAscList universeSize values
  where
    values =
      List.sort (fmap (\value -> (offset + value) `mod` universeSize) [start .. start + count - 1])

subsetObjectUnion :: SubsetObject -> SubsetObject -> Maybe SubsetObject
subsetObjectUnion leftObject rightObject =
  SubsetObject <$> denseIntSetUnion (subsetObjectElements leftObject) (subsetObjectElements rightObject)

subsetMorphismIsInclusion :: SubsetMorphism -> Bool
subsetMorphismIsInclusion morphism =
  denseIntSetIsSubsetOf
    (subsetObjectElements (subsetMorphismSource morphism))
    (subsetObjectElements (subsetMorphismTarget morphism))
    == Just True

subsetPullbackBatchWeight :: PreparedSubsetRewriteBatch -> Int
subsetPullbackBatchWeight prepared =
  subsetBatchWeight subsetPullbackWeight prepared

subsetPushoutBatchWeight :: PreparedSubsetRewriteBatch -> Int
subsetPushoutBatchWeight prepared =
  subsetBatchWeight subsetPushoutWeight prepared

subsetPushoutComplementBatchWeight :: PreparedSubsetRewriteBatch -> Int
subsetPushoutComplementBatchWeight prepared =
  subsetBatchWeight subsetPushoutComplementWeight prepared

subsetPushoutComplementCommuteBatchWeight :: PreparedSubsetRewriteBatch -> Int
subsetPushoutComplementCommuteBatchWeight prepared =
  subsetBatchWeight subsetPushoutComplementCommuteWeight prepared

subsetPBPOComplementBatchWeight :: PreparedSubsetRewriteBatch -> Int
subsetPBPOComplementBatchWeight prepared =
  subsetBatchWeight subsetPBPOComplementWeight prepared

subsetPBPOCommuteBatchWeight :: PreparedSubsetRewriteBatch -> Int
subsetPBPOCommuteBatchWeight prepared =
  subsetBatchWeight subsetPBPOCommuteWeight prepared

subsetBatchWeight :: (SubsetRewriteCase -> Int) -> PreparedSubsetRewriteBatch -> Int
subsetBatchWeight weight prepared =
  preparedSubsetCases prepared
    & fmap weight
    & sum

subsetPullbackWeight :: SubsetRewriteCase -> Int
subsetPullbackWeight rewriteCase =
  case complementWitness rewriteCase of
    Nothing -> 0
    Just witness ->
      maybe
        0
        subsetPullbackTripleWeight
        (pullback SubsetCategory (pushoutComplementBorrowedLeg witness) (subsetRewriteMatch rewriteCase))

subsetPushoutWeight :: SubsetRewriteCase -> Int
subsetPushoutWeight rewriteCase =
  case complementWitness rewriteCase of
    Nothing -> 0
    Just witness ->
      maybe
        0
        subsetPushoutTripleWeight
        (pushout SubsetCategory (pushoutComplementResidualLeg witness) (subsetRewriteRuleLeg rewriteCase))

subsetPushoutComplementWeight :: SubsetRewriteCase -> Int
subsetPushoutComplementWeight rewriteCase =
  maybe 0 subsetPushoutComplementWitnessWeight (complementWitness rewriteCase)

subsetPushoutComplementCommuteWeight :: SubsetRewriteCase -> Int
subsetPushoutComplementCommuteWeight rewriteCase =
  maybe 0 (boolWeight . pushoutComplementSquareCommutes SubsetCategory) (complementWitness rewriteCase)

subsetPBPOComplementWeight :: SubsetRewriteCase -> Int
subsetPBPOComplementWeight rewriteCase =
  maybe 0 subsetPBPOComplementWitnessWeight (pbpoWitness rewriteCase)

subsetPBPOCommuteWeight :: SubsetRewriteCase -> Int
subsetPBPOCommuteWeight rewriteCase =
  maybe
    0
    ( \witness ->
        boolWeight (pbpoPullbackSquareCommutes SubsetCategory witness)
          + boolWeight (pbpoPushoutSquareCommutes SubsetCategory witness)
    )
    (pbpoWitness rewriteCase)

complementWitness :: SubsetRewriteCase -> Maybe (PushoutComplementWitness SubsetCategory)
complementWitness rewriteCase = do
  monicWitness <- witnessMonic SubsetCategory (subsetRewriteMatch rewriteCase)
  pushoutComplement SubsetCategory (subsetRewriteRuleLeg rewriteCase) monicWitness

pbpoWitness :: SubsetRewriteCase -> Maybe (PBPOComplementWitness SubsetCategory)
pbpoWitness rewriteCase = do
  monicWitness <- witnessMonic SubsetCategory (subsetRewriteMatch rewriteCase)
  pbpoComplement SubsetCategory (subsetRewriteRuleLeg rewriteCase) monicWitness

subsetPullbackTripleWeight :: (SubsetObject, SubsetMorphism, SubsetMorphism) -> Int
subsetPullbackTripleWeight (objectValue, leftLeg, rightLeg) =
  subsetObjectWeight objectValue
    + subsetMorphismWeight leftLeg
    + subsetMorphismWeight rightLeg

subsetPushoutTripleWeight :: (SubsetObject, SubsetMorphism, SubsetMorphism) -> Int
subsetPushoutTripleWeight =
  subsetPullbackTripleWeight

subsetPushoutComplementWitnessWeight :: PushoutComplementWitness SubsetCategory -> Int
subsetPushoutComplementWitnessWeight witness =
  subsetObjectWeight (pushoutComplementObject witness)
    + subsetMorphismWeight (pushoutComplementBorrowedLeg witness)
    + subsetMorphismWeight (pushoutComplementResidualLeg witness)

subsetPBPOComplementWitnessWeight :: PBPOComplementWitness SubsetCategory -> Int
subsetPBPOComplementWitnessWeight witness =
  subsetObjectWeight (pbpoComplementPullbackObject witness)
    + subsetMorphismWeight (pbpoComplementPullbackToBorrowed witness)
    + subsetMorphismWeight (pbpoComplementPullbackToMatch witness)
    + subsetObjectWeight (pbpoComplementPushoutObject witness)
    + subsetMorphismWeight (pbpoComplementPushoutFromComplement witness)
    + subsetMorphismWeight (pbpoComplementPushoutFromMatch witness)
    + subsetMorphismWeight (pbpoComplementBorrowedLeg witness)
    + subsetMorphismWeight (pbpoComplementResidualLeg witness)

subsetObjectWeight :: SubsetObject -> Int
subsetObjectWeight (SubsetObject elements) =
  denseIntSetSize elements + denseIntSetWeight elements

subsetMorphismWeight :: SubsetMorphism -> Int
subsetMorphismWeight morphism =
  subsetObjectWeight (subsetMorphismSource morphism)
    + subsetObjectWeight (subsetMorphismTarget morphism)

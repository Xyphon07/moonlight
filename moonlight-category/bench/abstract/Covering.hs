{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Covering
  ( coveringBenchmarks,
  )
where

import BenchSupport (sampleBatch512)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Moonlight.Category.Pure.CoveringProduct
  ( CoveringProduct,
    adjustCoveringProduct,
    foldMapCoveringProductWithWitness,
    indexCoveringProduct,
    mapCoveringProductWithWitness,
    replaceCoveringProduct,
    restrictCoveringProduct,
    tabulateCoveringProduct,
  )
import Moonlight.Category.Test.CoveringFixture
  ( DemoField,
    DemoFieldWitness (..),
    DemoSubsetWitness (..),
    embedDemoSubsetWitness,
    sameDemoFieldWitness,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

type DemoValue :: DemoField -> Type
newtype DemoValue (field :: DemoField) = DemoValue
  { unDemoValue :: Int
  }
  deriving stock (Eq, Show)

coveringBenchmarks :: Benchmark
coveringBenchmarks =
  bgroup
    "CoveringProduct / CoveringFamily"
    [ bench "index full product batch x512" (nf fullProductIndexBatchWeight sampleBatch512),
      bench "restrict subset then index batch x512" (nf restrictSubsetBatchWeight sampleBatch512),
      bench "adjust one witness then fold batch x512" (nf adjustProductBatchWeight sampleBatch512),
      bench "replace one witness then fold batch x512" (nf replaceProductBatchWeight sampleBatch512),
      bench "map with witness then fold batch x512" (nf mapWithWitnessBatchWeight sampleBatch512),
      bench "foldMap with existential witnesses batch x512" (nf coveringProductBatchWeight sampleBatch512)
    ]

demoProduct :: CoveringProduct DemoFieldWitness DemoValue
demoProduct =
  tabulateCoveringProduct
    ( \witness ->
        case witness of
          AlphaFieldWitness -> DemoValue 11
          BetaFieldWitness -> DemoValue 17
          GammaFieldWitness -> DemoValue 23
    )

fullProductIndexBatchWeight :: [Int] -> Int
fullProductIndexBatchWeight =
  sum . fmap (\seed -> seed + fullProductIndexWeight demoProduct)

restrictSubsetBatchWeight :: [Int] -> Int
restrictSubsetBatchWeight =
  sum . fmap (\seed -> seed + restrictSubsetWeight demoProduct)

adjustProductBatchWeight :: [Int] -> Int
adjustProductBatchWeight =
  sum . fmap (\seed -> adjustProductWeight seed demoProduct)

replaceProductBatchWeight :: [Int] -> Int
replaceProductBatchWeight =
  sum . fmap (\seed -> replaceProductWeight seed demoProduct)

mapWithWitnessBatchWeight :: [Int] -> Int
mapWithWitnessBatchWeight =
  sum . fmap (\seed -> seed + mapWithWitnessWeight demoProduct)

coveringProductBatchWeight :: [Int] -> Int
coveringProductBatchWeight =
  sum . fmap (\seed -> seed + coveringProductWeight demoProduct)

fullProductIndexWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
fullProductIndexWeight productValue =
  demoValueWeight (indexCoveringProduct productValue AlphaFieldWitness)
    + demoValueWeight (indexCoveringProduct productValue BetaFieldWitness)
    + demoValueWeight (indexCoveringProduct productValue GammaFieldWitness)

restrictSubsetWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
restrictSubsetWeight productValue =
  let restrictedProduct = restrictCoveringProduct embedDemoSubsetWitness productValue
   in demoValueWeight (indexCoveringProduct restrictedProduct AlphaSubsetWitness)
        + demoValueWeight (indexCoveringProduct restrictedProduct GammaSubsetWitness)

adjustProductWeight :: Int -> CoveringProduct DemoFieldWitness DemoValue -> Int
adjustProductWeight seed productValue =
  adjustCoveringProduct sameDemoFieldWitness BetaFieldWitness (\(DemoValue value) -> DemoValue (value + seed)) productValue
    & coveringProductWeight

replaceProductWeight :: Int -> CoveringProduct DemoFieldWitness DemoValue -> Int
replaceProductWeight seed productValue =
  replaceCoveringProduct sameDemoFieldWitness GammaFieldWitness (DemoValue seed) productValue
    & coveringProductWeight

mapWithWitnessWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
mapWithWitnessWeight productValue =
  mapCoveringProductWithWitness (\witness (DemoValue value) -> DemoValue (value + demoWitnessWeight witness)) productValue
    & coveringProductWeight

coveringProductWeight :: CoveringProduct DemoFieldWitness DemoValue -> Int
coveringProductWeight productValue =
  getSum
    ( foldMapCoveringProductWithWitness
        (\witness value -> Sum (demoWitnessWeight witness + demoValueWeight value))
        productValue
    )

demoWitnessWeight :: DemoFieldWitness field -> Int
demoWitnessWeight witness =
  case witness of
    AlphaFieldWitness -> 1
    BetaFieldWitness -> 2
    GammaFieldWitness -> 3

demoValueWeight :: DemoValue field -> Int
demoValueWeight (DemoValue value) = value

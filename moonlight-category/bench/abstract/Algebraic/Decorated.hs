module Algebraic.Decorated
  ( decoratedBenchmarks,
  )
where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.List qualified as List
import AbstractFixtures
  ( BenchCategory (..),
    benchLeftCospanLeg,
    benchLeftCospanRightLeg,
    benchRightCospanLeftLeg,
    benchRightCospanRightLeg,
  )
import Algebraic.StructuredCospan (structuredCospanWeight)
import BenchSupport (batchWeight, sampleBatch512)
import Moonlight.Category.Pure.DecoratedComposition
  ( CompositionResult (..),
    StructuredCompositionAlgebra (..),
    composeDecorated,
    composeDecoratedStructured,
    reconcileCompositionObligations,
  )
import Moonlight.Category.Pure.DecoratedPresentation
  ( DecoratedPresentation,
    compileDecoratedPresentation,
    foldDecoratedPresentation,
    presentationGlue,
    presentationLeaf,
  )
import Moonlight.Category.Pure.StructuredCospan (mkStructuredCospan)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

decoratedBenchmarks :: Benchmark
decoratedBenchmarks =
  bgroup
    "DecoratedPresentation / DecoratedComposition"
    [ bench "composeDecorated batch x512" (nf (batchWeight decoratedComposeWeight) sampleBatch512),
      bench "compileDecoratedPresentation batch x512" (nf decoratedPresentationCompileBatchWeight sampleBatch512),
      bench "compile left-skew obligations=512" (nf decoratedPresentationCompileWeight (leftSkewPresentation 512)),
      bench "compile left-skew obligations=2048" (nf decoratedPresentationCompileWeight (leftSkewPresentation 2048)),
      bench "foldDecoratedPresentation batch x512" (nf decoratedPresentationFoldBatchWeight sampleBatch512),
      bench "composeDecoratedStructured batch x512" (nf (batchWeight decoratedStructuredComposeWeight) sampleBatch512),
      bench "reconcileCompositionObligations batch x512" (nf reconcileBatchWeight sampleBatch512),
      bench "reconcile failing obligations=100000 budget=4" (nf reconcileDecisionWeight largeObligations)
    ]
decoratedComposeWeight :: Int -> Int
decoratedComposeWeight seed =
  composeDecorated (+) decoratedGlue seed (seed + 11, seed + 2) (seed + 17, seed + 3)
    & compositionResultWeight

decoratedPresentationCompileBatchWeight :: [Int] -> Int
decoratedPresentationCompileBatchWeight =
  sum . fmap (\seed -> decoratedPresentationCompileWeight (demoPresentation seed))

decoratedPresentationCompileWeight :: DecoratedPresentation Int Int Int -> Int
decoratedPresentationCompileWeight presentationValue =
  compileDecoratedPresentation (+) decoratedGlue presentationValue
    & compositionResultWeight

decoratedPresentationFoldBatchWeight :: [Int] -> Int
decoratedPresentationFoldBatchWeight =
  sum . fmap (\seed -> decoratedPresentationFoldWeight (demoPresentation seed))

decoratedPresentationFoldWeight :: DecoratedPresentation Int Int Int -> Int
decoratedPresentationFoldWeight presentationValue =
  foldDecoratedPresentation (\ir decoration -> ir + decoration) (\boundary left right -> boundary + left + right) presentationValue

decoratedStructuredComposeWeight :: Int -> Int
decoratedStructuredComposeWeight seed =
  composeDecoratedStructured BenchCategory structuredDecoratedAlgebra (+) seed (1, seed + 13) (2, seed + 17)
    & either (const 0) compositionResultWeight

reconcileBatchWeight :: [Int] -> Int
reconcileBatchWeight =
  sum . fmap (\seed -> seed + reconcileWeight [seed, seed + 1, seed + 2])

reconcileWeight :: [Int] -> Int
reconcileWeight obligations =
  reconcileCompositionObligations obligations 4
    & either (sum . toList) (const 1)

reconcileDecisionWeight :: [Int] -> Int
reconcileDecisionWeight obligations =
  reconcileCompositionObligations obligations 4
    & either (const 1) (const 0)

demoPresentation :: Int -> DecoratedPresentation Int Int Int
demoPresentation seed =
  presentationGlue
    (seed + 7)
    (presentationGlue (seed + 3) (presentationLeaf (seed + 11) (seed + 2)) (presentationLeaf (seed + 13) (seed + 5)))
    (presentationLeaf (seed + 17) (seed + 19))

leftSkewPresentation :: Int -> DecoratedPresentation Int Int Int
leftSkewPresentation obligationCount =
  [1 .. obligationCount]
    & List.foldl'
      ( \presentationValue obligation ->
          presentationGlue
            obligation
            presentationValue
            (presentationLeaf obligation obligation)
      )
      (presentationLeaf 0 0)

decoratedGlue :: Int -> (Int, Int) -> (Int, Int) -> (Int, [Int])
decoratedGlue boundaryValue (leftIR, leftDecoration) (rightIR, rightDecoration) =
  (boundaryValue + leftIR + rightIR, [leftDecoration + rightDecoration])

structuredDecoratedAlgebra :: StructuredCompositionAlgebra Int BenchCategory Int Int Int
structuredDecoratedAlgebra =
  StructuredCompositionAlgebra
    { toStructuredBoundary = \_ (irValue, decoration) ->
        case irValue of
          1 -> either (const Nothing) Just (mkStructuredCospan BenchCategory benchLeftCospanLeg benchLeftCospanRightLeg decoration)
          2 -> either (const Nothing) Just (mkStructuredCospan BenchCategory benchRightCospanLeftLeg benchRightCospanRightLeg decoration)
          _ -> Nothing,
      fromStructuredComposition = \boundaryValue (leftIR, _) (rightIR, _) composedBoundary ->
        (leftIR + rightIR + boundaryValue, [structuredCospanWeight composedBoundary])
    }

compositionResultWeight :: CompositionResult Int Int Int -> Int
compositionResultWeight resultValue =
  composedIR resultValue
    + composedDecoration resultValue
    + sum (composedObligations resultValue)

largeObligations :: [Int]
largeObligations = [0 .. 99999]

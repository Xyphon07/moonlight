
module FieldSpec
  ( tests,
  )
where

import Moonlight.Core (canInvert)
import Moonlight.LinAlg
  ( fromListMatrix,
    gf2One,
    gf2Zero,
    kernel,
    kernelBasisVectors,
    mult,
    pluDecompFullRank,
    pluLower,
    pluPermutation,
    pluUpper,
    rank,
    toListMatrix,
    toListVector,
  )
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "Field"
    [ testCase "pluDecompFullRank reconstructs matrix for non-singular input" testPluReconstruction,
      testCase "pluDecompFullRank captures row permutations for zero-leading pivots" testPluPivoting,
      testCase "rank over GF2 returns expected value" testRank,
      testCase "kernel over GF2 returns basis vectors" testKernel,
      testCase "GF2 rank skips all-zero column (zero is not invertible)" testGF2ZeroColumnRank,
      testCase "GF2 canInvert rejects zero" testGF2CanInvertZero
    ]

testPluReconstruction :: Assertion
testPluReconstruction =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [4.0, 3.0, 6.0, 3.0]
        pluValue <- pluDecompFullRank matrixValue
        let permutation = pluPermutation pluValue
            lower = pluLower pluValue
            upper = pluUpper pluValue
        lhs <- mult permutation matrixValue
        reconstructed <- mult lower upper
        pure (toListMatrix lhs, toListMatrix reconstructed)
   in extractRight result (\(lhsValues, rhsValues) -> assertEqual "PLU reconstruction" lhsValues rhsValues)

testPluPivoting :: Assertion
testPluPivoting =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [0.0, 1.0, 1.0, 1.0]
        pluValue <- pluDecompFullRank matrixValue
        let permutation = pluPermutation pluValue
            lower = pluLower pluValue
            upper = pluUpper pluValue
        lhs <- mult permutation matrixValue
        rhs <- mult lower upper
        pure (toListMatrix lhs, toListMatrix rhs)
   in extractRight result (\(lhsValues, rhsValues) -> assertEqual "PLU reconstruction with permutation" lhsValues rhsValues)

testRank :: Assertion
testRank =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [gf2One, gf2One, gf2One, gf2One]
        rank matrixValue
   in extractRight result (\value -> assertEqual "GF2 rank" 1 value)

testKernel :: Assertion
testKernel =
  let result = do
        matrixValue <- fromListMatrix @2 @2 [gf2One, gf2One, gf2One, gf2One]
        fmap kernelBasisVectors (kernel matrixValue)
   in extractRight result (\basis -> assertEqual "GF2 kernel basis" [[gf2One, gf2One]] (map toListVector basis))

testGF2ZeroColumnRank :: Assertion
testGF2ZeroColumnRank =
  let result = do
        matrixValue <- fromListMatrix @2 @3 [gf2Zero, gf2One, gf2Zero, gf2Zero, gf2Zero, gf2One]
        rank matrixValue
   in extractRight result (\value -> assertEqual "rank of matrix with all-zero first column" 2 value)

testGF2CanInvertZero :: Assertion
testGF2CanInvertZero = do
  assertBool "GF2Zero must not be invertible" (not (canInvert gf2Zero))
  assertBool "GF2One must be invertible" (canInvert gf2One)

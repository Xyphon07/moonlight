module SparsePackedSpec
  ( tests,
  )
where

import Data.Vector.Unboxed qualified as Unboxed
import Moonlight.LinAlg.Sparse
  ( PackedSparseApplyError (..),
    PackedSparseOperatorShapeError (..),
    applyPackedSparseOperatorDense,
    mkPackedSparseOperator,
    packedSparseEntry,
    packedSparseOperatorEntryCount,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "SparsePacked"
    [ testCase "packed integral sparse operator applies to dense vectors" testPackedApply,
      testCase "packed floating sparse operator applies to dense vectors" testPackedFloatingApply,
      testCase "packed floating zero coefficients vanish from sealed storage" testFloatingZeroCoefficientsVanish,
      testCase "packed floating operator rejects out-of-bounds entries" testFloatingRejectsOutOfBoundsEntry,
      testCase "packed floating apply rejects wrong source vector length" testFloatingRejectsWrongSourceVectorLength,
      testCase "packed operator rejects cardinalities beyond Int range before allocation" testRejectsCardinalityOutOfBounds,
      testCase "zero coefficients vanish from sealed storage" testZeroCoefficientsVanish,
      testCase "packed operator rejects out-of-bounds entries" testRejectsOutOfBoundsEntry,
      testCase "packed apply rejects wrong source vector length" testRejectsWrongSourceVectorLength
    ]

testPackedApply :: Assertion
testPackedApply =
  case mkPackedSparseOperator 3 2 entries of
    Left failureValue ->
      assertFailure ("packed operator construction failed: " <> show failureValue)
    Right packedOperator ->
      case applyPackedSparseOperatorDense packedOperator (Unboxed.fromList [2, 5, 7]) of
        Left failureValue ->
          assertFailure ("packed operator apply failed: " <> show failureValue)
        Right resultValue ->
          assertEqual "dense packed apply result" [23, -5] (Unboxed.toList resultValue)
  where
    entries =
      [ packedSparseEntry 0 0 (1 :: Int),
        packedSparseEntry 2 0 3,
        packedSparseEntry 1 1 (-1)
      ]

testPackedFloatingApply :: Assertion
testPackedFloatingApply =
  case mkPackedSparseOperator 3 2 entries of
    Left failureValue ->
      assertFailure ("packed floating operator construction failed: " <> show failureValue)
    Right packedOperator ->
      case applyPackedSparseOperatorDense packedOperator (Unboxed.fromList [2.0, 5.0, 7.0]) of
        Left failureValue ->
          assertFailure ("packed floating operator apply failed: " <> show failureValue)
        Right resultValue ->
          assertEqual "dense packed floating apply result" [2.75, -2.5] (Unboxed.toList resultValue)
  where
    entries =
      [ packedSparseEntry 0 0 (0.5 :: Double),
        packedSparseEntry 2 0 0.25,
        packedSparseEntry 1 1 (-0.5)
      ]

testFloatingZeroCoefficientsVanish :: Assertion
testFloatingZeroCoefficientsVanish =
  case mkPackedSparseOperator 2 2 [packedSparseEntry 0 0 (0.0 :: Double), packedSparseEntry 1 1 4.5] of
    Left failureValue ->
      assertFailure ("packed floating operator construction failed: " <> show failureValue)
    Right packedOperator ->
      assertEqual "nonzero packed floating entry count" 1 (packedSparseOperatorEntryCount packedOperator)

testFloatingRejectsOutOfBoundsEntry :: Assertion
testFloatingRejectsOutOfBoundsEntry =
  case mkPackedSparseOperator 1 1 [packedSparseEntry 1 0 (1.0 :: Double)] of
    Left (PackedSparseEntryOutOfBounds sourceOffset targetOffset sourceDimension targetDimension) ->
      assertEqual "out-of-bounds packed floating entry" (1, 0, 1, 1) (sourceOffset, targetOffset, sourceDimension, targetDimension)
    Left failureValue ->
      assertFailure ("expected out-of-bounds packed floating entry, received: " <> show failureValue)
    Right _ ->
      assertFailure "expected packed floating operator construction to reject out-of-bounds entry"

testFloatingRejectsWrongSourceVectorLength :: Assertion
testFloatingRejectsWrongSourceVectorLength =
  case mkPackedSparseOperator 2 1 [packedSparseEntry 1 0 (4.0 :: Double)] of
    Left failureValue ->
      assertFailure ("packed floating operator construction failed: " <> show failureValue)
    Right packedOperator ->
      case applyPackedSparseOperatorDense packedOperator (Unboxed.fromList [3.0]) of
        Left (PackedSparseInputLengthMismatch expectedLength actualLength) ->
          assertEqual "source vector length mismatch" (2, 1) (expectedLength, actualLength)
        Right _ ->
          assertFailure "expected packed floating operator apply to reject wrong source vector length"

testRejectsCardinalityOutOfBounds :: Assertion
testRejectsCardinalityOutOfBounds =
  case mkPackedSparseOperator oversizedCardinality 1 [packedSparseEntry 0 0 (1 :: Int)] of
    Left (PackedSparseCardinalityOutOfBounds rejectedCardinality) ->
      assertEqual "out-of-bounds cardinality" oversizedCardinality rejectedCardinality
    Left failureValue ->
      assertFailure ("expected cardinality failure, received: " <> show failureValue)
    Right _ ->
      assertFailure "expected packed operator construction to reject oversized cardinality"
  where
    oversizedCardinality = fromIntegral (maxBound :: Int) + 1

testZeroCoefficientsVanish :: Assertion
testZeroCoefficientsVanish =
  case mkPackedSparseOperator 2 2 [packedSparseEntry 0 0 (0 :: Int), packedSparseEntry 1 1 4] of
    Left failureValue ->
      assertFailure ("packed operator construction failed: " <> show failureValue)
    Right packedOperator ->
      assertEqual "nonzero packed entry count" 1 (packedSparseOperatorEntryCount packedOperator)

testRejectsOutOfBoundsEntry :: Assertion
testRejectsOutOfBoundsEntry =
  case mkPackedSparseOperator 1 1 [packedSparseEntry 1 0 (1 :: Int)] of
    Left (PackedSparseEntryOutOfBounds sourceOffset targetOffset sourceDimension targetDimension) ->
      assertEqual "out-of-bounds packed entry" (1, 0, 1, 1) (sourceOffset, targetOffset, sourceDimension, targetDimension)
    Left failureValue ->
      assertFailure ("expected out-of-bounds packed entry, received: " <> show failureValue)
    Right _ ->
      assertFailure "expected packed operator construction to reject out-of-bounds entry"

testRejectsWrongSourceVectorLength :: Assertion
testRejectsWrongSourceVectorLength =
  case mkPackedSparseOperator 2 1 [packedSparseEntry 1 0 (4 :: Int)] of
    Left failureValue ->
      assertFailure ("packed operator construction failed: " <> show failureValue)
    Right packedOperator ->
      case applyPackedSparseOperatorDense packedOperator (Unboxed.fromList [3]) of
        Left (PackedSparseInputLengthMismatch expectedLength actualLength) ->
          assertEqual "source vector length mismatch" (2, 1) (expectedLength, actualLength)
        Right _ ->
          assertFailure "expected packed operator apply to reject wrong source vector length"

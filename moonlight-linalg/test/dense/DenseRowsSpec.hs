{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module DenseRowsSpec (tests) where

import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Dense
  ( matrixRows,
    matrixShape,
    toListMatrix,
  )
import Moonlight.LinAlg.Dense.Rows
  ( hcatRowsExact,
    matrixProductRowsWith,
    matrixVectorProductRowsWith,
    mkDenseRows,
    mkDenseRowsFromFlat,
    mkDenseRowsWithShape,
    transposeRowsExact,
    vcatRowsExact,
  )
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( dynMatrixFromRows,
    dynMatrixToRows,
    mkDynMatrix,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( fromListMatrix,
    matrixToRows,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "DenseRows"
    [ testCase "mkDenseRows rejects ragged input" $
        expectLeft
          (InvariantViolation "dense row matrix is ragged at row 1 (expected 2 columns, got 1)")
          (mkDenseRows [[1 :: Int, 2], [3]])
    , testCase "mkDenseRowsWithShape validates row count" $
        expectLeft
          (InvariantViolation "dense row matrix row count mismatch: expected 2 rows but received 1")
          (mkDenseRowsWithShape 2 2 [[1 :: Int, 2]])
    , testCase "mkDenseRowsFromFlat rejects wrapped shape cardinality" $
        let wrappedDimension = 2 ^ (32 :: Int)
         in expectLeft
              (InvariantViolation "dense row matrix dimensions exceed Int cardinality")
              (mkDenseRowsFromFlat wrappedDimension wrappedDimension ([] :: [Int]))
    , testCase "matrixRows stores entries in row-major order" $
        fmap toListMatrix (matrixRows @2 @2 [[1 :: Int, 2], [3, 4]])
          @?= Right [1, 2, 3, 4]
    , testCase "matrixRows rejects static column mismatch" $
        expectLeft
          (InvariantViolation "dense row matrix is ragged at row 1 (expected 2 columns, got 1)")
          (matrixRows @2 @2 [[1 :: Int, 2], [3]])
    , testCase "matrixRows retains columns for zero-row matrices" $
        fmap matrixShape (matrixRows @0 @3 ([] :: [[Int]]))
          @?= Right (0, 3)
    , testCase "matrixToRows lowers static flat matrix through DenseRows" $
        (matrixToRows =<< fromListMatrix @2 @3 @Int [1, 2, 3, 4, 5, 6])
          @?= Right [[1, 2, 3], [4, 5, 6]]
    , testCase "matrixToRows preserves zero-column static row count" $
        (matrixToRows =<< fromListMatrix @2 @0 @Int [])
          @?= Right [[], []]
    , testCase "dynMatrixToRows lowers dynamic flat matrix through DenseRows" $
        (dynMatrixToRows =<< mkDynMatrix 2 3 [1 :: Int, 2, 3, 4, 5, 6])
          @?= Right [[1, 2, 3], [4, 5, 6]]
    , testCase "dynMatrixFromRows preserves zero-column dynamic row count" $
        (dynMatrixToRows =<< dynMatrixFromRows [[], [], [] :: [Int]])
          @?= Right [[], [], []]
    , testCase "transposeRowsExact preserves rectangular data" $
        transposeRowsExact [[1 :: Int, 2, 3], [4, 5, 6]]
          @?= Right [[1, 4], [2, 5], [3, 6]]
    , testCase "matrixVectorProductRowsWith rejects vector shape mismatch" $
        expectLeft
          (InvariantViolation "dense row matrix/vector shape mismatch (matrix=(2,2), vector=1)")
          (matrixVectorProductRowsWith (*) (+) (0 :: Int) [[1, 2], [3, 4]] [9])
    , testCase "matrixProductRowsWith rejects incompatible shapes" $
        expectLeft
          (InvariantViolation "dense row matrix product shape mismatch (left=(1,2), right=(1,1))")
          (matrixProductRowsWith (*) (+) (0 :: Int) [[1, 2]] [[3]])
    , testCase "hcatRowsExact rejects mismatched row counts" $
        expectLeft
          (InvariantViolation "dense horizontal concatenation requires equal row counts, got [(1,1),(2,1)]")
          (hcatRowsExact [[[1 :: Int]], [[2], [3]]])
    , testCase "vcatRowsExact rejects mismatched column counts" $
        expectLeft
          (InvariantViolation "dense vertical concatenation requires equal column counts, got [(1,1),(1,2)]")
          (vcatRowsExact [[[1 :: Int]], [[2, 3]]])
    ]

expectLeft :: (Eq e, Show e) => e -> Either e a -> Assertion
expectLeft expected value =
  case value of
    Left err ->
      err @?= expected
    Right _ ->
      assertFailure "expected Left, got Right"

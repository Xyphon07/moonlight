module GF2Spec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.Vector.Unboxed qualified as U
import Data.Vector qualified as V
import Moonlight.Core
  ( MoonlightError,
  )
import Numeric.Natural (Natural)
import Moonlight.LinAlg
  ( GF2 (..),
    GF2MatrixEntry (..),
    GF2PackedMatrixFailure (..),
    GF2SparseColumn,
    PackedRow,
    defaultGF2SparseReducerConfig,
    gf2SparseColumnRows,
    gf2PackedWords,
    kernelBasisGF2SparseColumns,
    mkGF2SparseColumn,
    mkGF2SparseReducerConfig,
    mkGF2PackedMatrix,
    mkGF2PackedMatrixFromRowMajor,
    packedRowIndices,
    rankGF2SparseColumns,
    rankGF2PackedMatrix,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertEqual,
    assertFailure,
    testCase,
  )

tests :: TestTree
tests =
  testGroup
    "GF2 packed matrix"
    [ testCase "rejects out-of-bounds entries" testRejectsOutOfBounds,
      testCase "rejects row-major length mismatch" testRejectsRowMajorLengthMismatch,
      testCase "rejects Natural dimensions outside Int range" testRejectsOversizedNaturalDimension,
      testCase "rejects wrapped row-major cardinality" testRejectsWrappedRowMajorCardinality,
      testCase "duplicate entries cancel by XOR" testDuplicateEntriesCancel,
      testCase "row-major rank matches known full-rank fixture" testRowMajorRank,
      testCase "sparse rank agrees with packed rank on generated matrices" testSparseRankAgreement,
      testCase "sparse kernel witnesses annihilate columns" testSparseKernelWitnessAnnihilation,
      testCase "sparse densify threshold preserves reduction semantics" testSparseThresholdCrossing
    ]

testRejectsOutOfBounds :: Assertion
testRejectsOutOfBounds =
  case mkGF2PackedMatrix 2 3 [GF2MatrixEntry 2 0] of
    Left (GF2PackedMatrixEntryOutOfBounds row column rowCount columnCount) ->
      assertEqual "out-of-bounds entry" (2, 0, 2, 3) (row, column, rowCount, columnCount)
    Left failureValue ->
      assertFailure ("unexpected packed matrix failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected out-of-bounds packed matrix construction to fail"

testRejectsRowMajorLengthMismatch :: Assertion
testRejectsRowMajorLengthMismatch =
  case mkGF2PackedMatrixFromRowMajor 2 2 [GF2One] of
    Left (GF2PackedMatrixFlatLengthMismatch expectedCount actualCount) ->
      assertEqual "flat length mismatch" (4, 1) (expectedCount, actualCount)
    Left failureValue ->
      assertFailure ("unexpected packed matrix failure: " <> show failureValue)
    Right _ ->
      assertFailure "expected row-major packed matrix construction to reject malformed length"

testRejectsOversizedNaturalDimension :: Assertion
testRejectsOversizedNaturalDimension =
  let oversizedDimension = fromIntegral (maxBound :: Int) + 1 :: Natural
   in case mkGF2PackedMatrix oversizedDimension 0 [] of
        Left (GF2PackedMatrixCardinalityOutOfBounds rowCount columnCount) ->
          assertEqual "out-of-range Natural shape" (oversizedDimension, 0) (rowCount, columnCount)
        Left failureValue ->
          assertFailure ("unexpected packed matrix failure: " <> show failureValue)
        Right _ ->
          assertFailure "expected oversized Natural dimension to fail"

testRejectsWrappedRowMajorCardinality :: Assertion
testRejectsWrappedRowMajorCardinality =
  let dimension = (2 :: Natural) ^ (32 :: Int)
   in case mkGF2PackedMatrixFromRowMajor dimension dimension [] of
        Left (GF2PackedMatrixCardinalityOutOfBounds rowCount columnCount) ->
          assertEqual "wrapped row-major shape" (dimension, dimension) (rowCount, columnCount)
        Left failureValue ->
          assertFailure ("unexpected packed matrix failure: " <> show failureValue)
        Right _ ->
          assertFailure "expected wrapped row-major cardinality to fail"

testDuplicateEntriesCancel :: Assertion
testDuplicateEntriesCancel =
  case mkGF2PackedMatrix 1 1 [GF2MatrixEntry 0 0, GF2MatrixEntry 0 0] of
    Left failureValue ->
      assertFailure ("packed matrix construction failed: " <> show failureValue)
    Right matrixValue -> do
      assertEqual "duplicate entry rank" 0 (rankGF2PackedMatrix matrixValue)
      assertEqual "duplicate entry storage" [0] (U.toList (gf2PackedWords matrixValue))

testRowMajorRank :: Assertion
testRowMajorRank =
  case mkGF2PackedMatrixFromRowMajor 2 2 [GF2One, GF2Zero, GF2One, GF2One] of
    Left failureValue ->
      assertFailure ("packed matrix construction failed: " <> show failureValue)
    Right matrixValue ->
      assertEqual "row-major rank" 2 (rankGF2PackedMatrix matrixValue)

testSparseRankAgreement :: Assertion
testSparseRankAgreement =
  traverse_
    assertGeneratedRankAgreement
    [ (0, 0, 1),
      (1, 3, 2),
      (4, 5, 3),
      (8, 9, 5),
      (17, 23, 7)
    ]

assertGeneratedRankAgreement :: (Int, Int, Int) -> Assertion
assertGeneratedRankAgreement (rowCount, columnCount, saltValue) =
  case ( mkGF2PackedMatrix (fromIntegral rowCount) (fromIntegral columnCount) (generatedEntries rowCount columnCount saltValue),
         generatedSparseColumns rowCount columnCount saltValue
       ) of
    (Left failureValue, _) ->
      assertFailure ("packed matrix construction failed: " <> show failureValue)
    (_, Left errorValue) ->
      assertFailure ("sparse column construction failed: " <> show errorValue)
    (Right packedMatrix, Right sparseColumns) ->
      case rankGF2SparseColumns defaultGF2SparseReducerConfig rowCount columnCount sparseColumns of
        Left errorValue ->
          assertFailure ("sparse rank failed: " <> show errorValue)
        Right sparseRank ->
          assertEqual
            ("generated sparse rank " <> show (rowCount, columnCount, saltValue))
            (rankGF2PackedMatrix packedMatrix)
            sparseRank

testSparseKernelWitnessAnnihilation :: Assertion
testSparseKernelWitnessAnnihilation =
  case dependentSparseColumns of
    Left errorValue ->
      assertFailure ("dependent sparse columns failed: " <> show errorValue)
    Right sparseColumns ->
      case kernelBasisGF2SparseColumns defaultGF2SparseReducerConfig 3 3 sparseColumns of
        Left errorValue ->
          assertFailure ("sparse kernel basis failed: " <> show errorValue)
        Right kernelBasis -> do
          assertEqual "sparse kernel dependency" [[0, 1, 2]] (packedRowIndices <$> V.toList kernelBasis)
          assertKernelBasisAnnihilates "dependent sparse kernel" sparseColumns kernelBasis

testSparseThresholdCrossing :: Assertion
testSparseThresholdCrossing =
  case thresholdSparseColumns of
    Left errorValue ->
      assertFailure ("threshold sparse columns failed: " <> show errorValue)
    Right sparseColumns ->
      case ( mkGF2SparseReducerConfig "threshold low fixture" 2,
             mkGF2SparseReducerConfig "threshold high fixture" 99
           ) of
        (Right lowConfig, Right highConfig) ->
          case ( rankGF2SparseColumns lowConfig 8 4 sparseColumns,
                 rankGF2SparseColumns highConfig 8 4 sparseColumns,
                 kernelBasisGF2SparseColumns lowConfig 8 4 sparseColumns,
                 kernelBasisGF2SparseColumns highConfig 8 4 sparseColumns
               ) of
            (Right lowRank, Right highRank, Right lowKernel, Right highKernel) -> do
              assertEqual "threshold rank" highRank lowRank
              assertEqual "threshold kernel width" (length (V.toList highKernel)) (length (V.toList lowKernel))
              assertKernelBasisAnnihilates "low-threshold sparse kernel" sparseColumns lowKernel
            resultValue ->
              assertFailure ("threshold reduction failed: " <> show resultValue)
        resultValue ->
          assertFailure ("threshold config failed: " <> show resultValue)

dependentSparseColumns :: Either MoonlightError (V.Vector GF2SparseColumn)
dependentSparseColumns =
  V.fromList
    <$> sequence
      [ mkGF2SparseColumn "dependent column 0" 3 0 [0, 2],
        mkGF2SparseColumn "dependent column 1" 3 1 [1],
        mkGF2SparseColumn "dependent column 2" 3 2 [0, 1, 2]
      ]

thresholdSparseColumns :: Either MoonlightError (V.Vector GF2SparseColumn)
thresholdSparseColumns =
  V.fromList
    <$> sequence
      [ mkGF2SparseColumn "threshold column 0" 8 0 [0, 1, 2, 3],
        mkGF2SparseColumn "threshold column 1" 8 1 [2, 3, 4, 5],
        mkGF2SparseColumn "threshold column 2" 8 2 [0, 1, 4, 5],
        mkGF2SparseColumn "threshold column 3" 8 3 [6, 7]
      ]

generatedSparseColumns :: Int -> Int -> Int -> Either MoonlightError (V.Vector GF2SparseColumn)
generatedSparseColumns rowCount columnCount saltValue =
  V.fromList
    <$> traverse
      ( \columnIndex ->
          mkGF2SparseColumn
            ("generated sparse column " <> show columnIndex)
            rowCount
            columnIndex
            (generatedSupport rowCount columnIndex saltValue)
      )
      [0 .. columnCount - 1]

generatedEntries :: Int -> Int -> Int -> [GF2MatrixEntry]
generatedEntries rowCount columnCount saltValue =
  [ GF2MatrixEntry rowIndex columnIndex
    | columnIndex <- [0 .. columnCount - 1],
      rowIndex <- generatedSupport rowCount columnIndex saltValue
  ]

generatedSupport :: Int -> Int -> Int -> [Int]
generatedSupport rowCount columnIndex saltValue =
  [ rowIndex
    | rowIndex <- [0 .. rowCount - 1],
      generatedBit rowIndex columnIndex saltValue
  ]

generatedBit :: Int -> Int -> Int -> Bool
generatedBit rowIndex columnIndex saltValue =
  rowIndex == columnIndex
    || ((rowIndex * 17 + columnIndex * 31 + saltValue * 13 + rowIndex * columnIndex) `mod` 11 == 0)

assertKernelBasisAnnihilates :: String -> V.Vector GF2SparseColumn -> V.Vector PackedRow -> Assertion
assertKernelBasisAnnihilates label sparseColumns kernelBasis =
  traverse_
    (assertKernelWitnessAnnihilates label sparseColumns)
    (packedRowIndices <$> V.toList kernelBasis)

assertKernelWitnessAnnihilates :: String -> V.Vector GF2SparseColumn -> [Int] -> Assertion
assertKernelWitnessAnnihilates label sparseColumns witnessColumns =
  case traverse (`lookupSparseColumnRows` sparseColumns) witnessColumns of
    Nothing ->
      assertFailure (label <> ": kernel witness referenced an absent column")
    Just supports ->
      assertEqual (label <> ": annihilated support") [] (foldl' xorSortedSupports [] supports)

lookupSparseColumnRows :: Int -> V.Vector GF2SparseColumn -> Maybe [Int]
lookupSparseColumnRows columnIndex sparseColumns =
  gf2SparseColumnRows <$> (sparseColumns V.!? columnIndex)

xorSortedSupports :: [Int] -> [Int] -> [Int]
xorSortedSupports leftRows rightRows =
  case (leftRows, rightRows) of
    ([], _) -> rightRows
    (_, []) -> leftRows
    (leftRow : remainingLeft, rightRow : remainingRight) ->
      case compare leftRow rightRow of
        LT -> leftRow : xorSortedSupports remainingLeft rightRows
        EQ -> xorSortedSupports remainingLeft remainingRight
        GT -> rightRow : xorSortedSupports leftRows remainingRight

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module AdvancedSpec
  ( tests,
  )
where

import Data.Foldable qualified as Foldable
import Data.List (isInfixOf, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg
  ( choleskyDecomp,
    canonicalCSRFromEntries,
    cooEntries,
    cooToDense,
    cooToCSR,
    csrCols,
    csrColumnIndicesVector,
    csrMatVecVector,
    csrRows,
    csrRowOffsetsVector,
    csrToCSC,
    cscToCOO,
    cscColumnOffsetsVector,
    cscRowIndicesVector,
    cscToDense,
    cscValuesVector,
    csrToCOO,
    csrToDense,
    csrValuesVector,
    denseToCOO,
    denseToCSC,
    denseToCSR,
    diagonalCSR,
    fromListMatrix,
    fromListVector,
    GraphEdge (..),
    graphLaplacianCSR,
    mkSparseCSC,
    mkSparseCSR,
    mkSparseCOO,
    SparseCSC,
    SparseCSR,
    mult,
    pathLaplacianCSR,
    qrDecompFullColumnRank,
    solveCG,
    solveDirect,
    solveGMRES,
    thinSvdFullColumnRank,
    symmetricEigen,
    toListMatrix,
    toListVector,
    transpose,
    tridiagonalCSR,
  )
import Moonlight.LinAlg.Pure.Dense.Field (PLU (..), pluDecompFullRank)
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "Advanced"
    [ testCase "sparse COO/CSR/CSC conversions round-trip dense matrices" testSparseConversions,
      QC.testProperty "counting CSR to CSC transpose agrees with sort-based conversion" propCountingCSRToCSCAgreesWithSort,
      testCase "COO constructor rejects out-of-bounds entries" testSparseCOORejectsOutOfBounds,
      testCase "COO to static dense rejects overflowing type-level shape" testCOOToDenseRejectsTypeLevelCardinalityOverflow,
      testCase "COO to static dense rejects type-level dimensions outside Int range" testCOOToDenseRejectsTypeLevelDimensionOverflow,
      testCase "COO to CSR combines duplicates and prunes zero storage entries" testCOOToCSRCombinesDuplicateAndPrunesZeroStorageEntries,
      testCase "canonical CSR assembly combines duplicates and prunes zeros" testCanonicalCSRFromEntriesCombinesDuplicatesAndPrunesZeros,
      testCase "canonical CSR assembly rejects out-of-bounds entries before pruning" testCanonicalCSRFromEntriesRejectsOutOfBoundsBeforePruning,
      testCase "structured sparse constructors produce canonical CSR layouts" testStructuredSparseConstructors,
      testCase "symmetric tridiagonal CSR has exact canonical storage" testTridiagonalCSR,
      testCase "one-vertex path Laplacian is the zero operator" testOneVertexPathLaplacian,
      testCase "weighted graph Laplacian canonicalizes parallel undirected edges" testGraphLaplacian,
      QC.testProperty "edge-level graph Laplacian agrees with coordinate expansion" propGraphLaplacianAgreesWithCoordinateExpansion,
      testCase "graph Laplacian rejects malformed graph declarations" testGraphLaplacianFailures,
      testCase "qrDecompFullColumnRank reconstructs dense input" testQrDecomp,
      testCase "choleskyDecomp reconstructs SPD matrix" testCholeskyDecomp,
      testCase "choleskyDecomp rejects non-symmetric matrix" testCholeskyRejectsNonSymmetric,
      testCase "symmetricEigen diagonalizes symmetric matrices" testSymmetricEigen,
      testCase "symmetricEigen reconstructs coupled symmetric matrices" testSymmetricEigenReconstructsCoupledMatrix,
      testCase "symmetricEigen matches Dirichlet second-difference spectrum" testSymmetricEigenDirichletSecondDifferenceSpectrum,
      testCase "symmetricEigen rejects non-finite entries" testSymmetricEigenRejectsNonFinite,
      testCase "thinSvdFullColumnRank reconstructs dense input with orthonormal factors" testSvdDecomp,
      testCase "solveDirect solves linear systems via PLU" testSolveDirect,
      testCase "solveDirect matches exact PLU semantics on generated systems" testSolveDirectGeneratedExactSemantics,
      testCase "qrDecompFullColumnRank reconstructs generated matrices" testQrGeneratedResiduals,
      testCase "choleskyDecomp reconstructs generated SPD matrices" testCholeskyGeneratedResiduals,
      testCase "solveCG converges on SPD systems" testSolveCg,
      testCase "solveGMRES converges on non-symmetric systems" testSolveGmres
    ]

assertApproxList :: String -> [Double] -> [Double] -> Assertion
assertApproxList label expected actual =
  let tolerance = 1.0e-6
      closeEnough left right = abs (left - right) <= tolerance
   in assertBool label (length expected == length actual && and (zipWith closeEnough expected actual))

testCOOToDenseRejectsTypeLevelCardinalityOverflow :: Assertion
testCOOToDenseRejectsTypeLevelCardinalityOverflow =
  case mkSparseCOO 0 0 ([] :: [(Int, Int, Double)]) >>= cooToDense @4294967296 @4294967296 of
    Left failureValue ->
      assertEqual
        "overflowing static shape"
        (InvariantViolation "static sparse/dense shape exceeds Int cardinality")
        failureValue
    Right _ ->
      assertFailure "expected overflowing type-level sparse/dense shape to fail"

testCOOToDenseRejectsTypeLevelDimensionOverflow :: Assertion
testCOOToDenseRejectsTypeLevelDimensionOverflow =
  case mkSparseCOO 0 0 ([] :: [(Int, Int, Double)]) >>= cooToDense @9223372036854775808 @0 of
    Left failureValue ->
      assertEqual
        "out-of-range static dimension"
        (InvariantViolation "static sparse/dense dimension exceeds Int cardinality")
        failureValue
    Right _ ->
      assertFailure "expected out-of-range type-level sparse/dense dimension to fail"

data GeneratedCSRCase = GeneratedCSRCase
  { generatedCSRRows :: !Int,
    generatedCSRCols :: !Int,
    generatedCSREntries :: ![(Int, Int, Double)]
  }
  deriving stock (Show)

instance QC.Arbitrary GeneratedCSRCase where
  arbitrary = do
    rowCount <- QC.chooseInt (0, 8)
    columnCount <- QC.chooseInt (0, 8)
    if rowCount == 0 || columnCount == 0
      then pure (GeneratedCSRCase rowCount columnCount [])
      else do
        entryCount <- QC.chooseInt (0, 32)
        randomEntries <-
          QC.vectorOf
            entryCount
            (generatedCOOEntry rowCount columnCount)
        let duplicateEntries =
              [ (0, 0, 1.0),
                (0, 0, 2.0)
              ]
                <> if columnCount > 1
                  then
                    [ (0, 1, 4.0),
                      (0, 1, -4.0)
                    ]
                  else []
        pure (GeneratedCSRCase rowCount columnCount (duplicateEntries <> randomEntries))

generatedCOOEntry :: Int -> Int -> QC.Gen (Int, Int, Double)
generatedCOOEntry rowCount columnCount = do
  rowIndex <-
    QC.chooseInt
      ( 0,
        if rowCount > 1
          then rowCount - 2
          else 0
      )
  columnIndex <- QC.chooseInt (0, columnCount - 1)
  entryValue <- QC.elements [-4.0, -2.0, -1.0, 0.0, 0.5, 1.0, 2.0, 4.0]
  pure (rowIndex, columnIndex, entryValue)

data GeneratedGraphCase = GeneratedGraphCase
  { generatedGraphVertices :: ![Int],
    generatedGraphEdges :: ![GraphEdge Int]
  }
  deriving stock (Show)

instance QC.Arbitrary GeneratedGraphCase where
  arbitrary = do
    vertexCount <- QC.chooseInt (0, 8)
    if vertexCount < 2
      then pure (GeneratedGraphCase [0 .. vertexCount - 1] [])
      else do
        edgeCount <- QC.chooseInt (0, 40)
        randomEdges <- QC.vectorOf edgeCount (generatedGraphEdge vertexCount)
        let parallelEdges =
              [ GraphEdge 0 1 0.5,
                GraphEdge 1 0 1.5
              ]
        pure (GeneratedGraphCase [0 .. vertexCount - 1] (parallelEdges <> randomEdges))

generatedGraphEdge :: Int -> QC.Gen (GraphEdge Int)
generatedGraphEdge vertexCount = do
  leftIndex <- QC.chooseInt (0, vertexCount - 1)
  offset <- QC.chooseInt (1, vertexCount - 1)
  reversedEdge <- QC.arbitrary
  weightValue <- QC.elements [0.0, 0.25, 0.5, 1.0, 2.0, 4.0]
  let rightIndex = (leftIndex + offset) `mod` vertexCount
  pure
    ( if reversedEdge
        then GraphEdge rightIndex leftIndex weightValue
        else GraphEdge leftIndex rightIndex weightValue
    )

propCountingCSRToCSCAgreesWithSort :: GeneratedCSRCase -> QC.Property
propCountingCSRToCSCAgreesWithSort GeneratedCSRCase {..} =
  case resultValue of
    Left err ->
      QC.counterexample ("unexpected sparse generation failure: " <> show err) False
    Right (countingFingerprint, sortedFingerprint) ->
      QC.counterexample
        ( "counting transpose = "
            <> show countingFingerprint
            <> ", sort transpose = "
            <> show sortedFingerprint
        )
        (countingFingerprint == sortedFingerprint)
  where
    resultValue = do
      cooValue <- mkSparseCOO generatedCSRRows generatedCSRCols generatedCSREntries
      csrValue <- cooToCSR cooValue
      countingCsc <- csrToCSC csrValue
      sortedCsc <- sortBasedCSRToCSC csrValue
      pure (cscFingerprint countingCsc, cscFingerprint sortedCsc)

sortBasedCSRToCSC :: SparseCSR Double -> Either MoonlightError (SparseCSC Double)
sortBasedCSRToCSC csrValue = do
  cooValue <- csrToCOO csrValue
  let orderedEntries =
        sortBy
          (comparing (\(rowIndex, columnIndex, _) -> (columnIndex, rowIndex)))
          (cooEntries cooValue)
      columnOffsets =
        offsetsFromSortedAxesForTest
          (csrCols csrValue)
          ((\(_, columnIndex, _) -> columnIndex) <$> orderedEntries)
      rowIndices = (\(rowIndex, _, _) -> rowIndex) <$> orderedEntries
      values = (\(_, _, entryValue) -> entryValue) <$> orderedEntries
  mkSparseCSC
    (csrRows csrValue)
    (csrCols csrValue)
    columnOffsets
    rowIndices
    values

cscFingerprint :: SparseCSC Double -> (U.Vector Int, U.Vector Int, U.Vector Double)
cscFingerprint cscValue =
  ( cscColumnOffsetsVector cscValue,
    cscRowIndicesVector cscValue,
    cscValuesVector cscValue
  )

propGraphLaplacianAgreesWithCoordinateExpansion :: GeneratedGraphCase -> QC.Property
propGraphLaplacianAgreesWithCoordinateExpansion GeneratedGraphCase {..} =
  case (graphLaplacianCSR generatedGraphVertices generatedGraphEdges, coordinateExpansionGraphLaplacian generatedGraphVertices generatedGraphEdges) of
    (Right edgeLevelValue, Right coordinateValue) ->
      QC.counterexample
        ( "edge-level = "
            <> show (csrFingerprint edgeLevelValue)
            <> ", coordinate = "
            <> show (csrFingerprint coordinateValue)
        )
        (csrFingerprint edgeLevelValue == csrFingerprint coordinateValue)
    (Left leftError, Left rightError) ->
      QC.counterexample
        ("both constructors rejected generated graph: " <> show (leftError, rightError))
        True
    otherResult ->
      QC.counterexample ("constructor disagreement: " <> show otherResult) False

coordinateExpansionGraphLaplacian :: [Int] -> [GraphEdge Int] -> Either MoonlightError (SparseCSR Double)
coordinateExpansionGraphLaplacian vertexOrder graphEdges = do
  indexedEdges <-
    catMaybes
      <$> traverse
        (coordinateExpansionGraphEdge (Map.fromList (zip vertexOrder [0 ..])))
        graphEdges
  let orderedEdges =
        sortBy
          (comparing (\(leftIndex, rightIndex, weightValue) -> (leftIndex, rightIndex, weightValue)))
          indexedEdges
      orderedEntries =
        fmap
          (\((rowIndex, columnIndex), entryValue) -> (rowIndex, columnIndex, entryValue))
          . filter ((/= 0.0) . snd)
          . Map.toAscList
          . foldl'
            ( \entryMap (rowIndex, columnIndex, entryValue) ->
                Map.insertWith (+) (rowIndex, columnIndex) entryValue entryMap
            )
            Map.empty
          . concatMap coordinateExpansionGraphEdgeContributions
          $ orderedEdges
      dimension = length vertexOrder
      rowOffsets =
        offsetsFromSortedAxesForTest
          dimension
          ((\(rowIndex, _, _) -> rowIndex) <$> orderedEntries)
      columnIndices = (\(_, columnIndex, _) -> columnIndex) <$> orderedEntries
      values = (\(_, _, entryValue) -> entryValue) <$> orderedEntries
  mkSparseCSR dimension dimension rowOffsets columnIndices values

coordinateExpansionGraphEdge ::
  Map.Map Int Int ->
  GraphEdge Int ->
  Either MoonlightError (Maybe (Int, Int, Double))
coordinateExpansionGraphEdge vertexIndices edgeValue
  | graphEdgeWeight edgeValue == 0.0 = Right Nothing
  | otherwise = do
      leftIndex <- requireGeneratedVertex "left" (graphEdgeLeft edgeValue) vertexIndices
      rightIndex <- requireGeneratedVertex "right" (graphEdgeRight edgeValue) vertexIndices
      pure
        ( Just
            ( min leftIndex rightIndex,
              max leftIndex rightIndex,
              graphEdgeWeight edgeValue
            )
        )

requireGeneratedVertex :: String -> Int -> Map.Map Int Int -> Either MoonlightError Int
requireGeneratedVertex endpointRole vertexValue vertexIndices =
  case Map.lookup vertexValue vertexIndices of
    Nothing ->
      Left
        ( InvariantViolation
            ( "generated graph "
                <> endpointRole
                <> " endpoint absent: "
                <> show vertexValue
            )
        )
    Just vertexIndex -> Right vertexIndex

coordinateExpansionGraphEdgeContributions :: (Int, Int, Double) -> [(Int, Int, Double)]
coordinateExpansionGraphEdgeContributions (leftIndex, rightIndex, weightValue) =
  [ (leftIndex, leftIndex, weightValue),
    (leftIndex, rightIndex, negate weightValue),
    (rightIndex, leftIndex, negate weightValue),
    (rightIndex, rightIndex, weightValue)
  ]

offsetsFromSortedAxesForTest :: Int -> [Int] -> [Int]
offsetsFromSortedAxesForTest axisCount sortedAxes =
  scanl
    (+)
    0
    ( (\axisIndex -> length (filter (== axisIndex) sortedAxes))
        <$> [0 .. axisCount - 1]
    )

csrFingerprint :: SparseCSR Double -> (U.Vector Int, U.Vector Int, U.Vector Double)
csrFingerprint csrValue =
  ( csrRowOffsetsVector csrValue,
    csrColumnIndicesVector csrValue,
    csrValuesVector csrValue
  )

testSparseConversions :: Assertion
testSparseConversions =
  let result = do
        denseMatrix <- fromListMatrix @3 @3 @Double [1.0, 0.0, 0.0, 0.0, 2.0, 3.0, 0.0, 0.0, 4.0]
        let cooMatrix = denseToCOO denseMatrix
            csrMatrix = denseToCSR denseMatrix
            cscMatrix = denseToCSC denseMatrix
        cooFromCsr <- csrToCOO csrMatrix
        cooFromCsc <- cscToCOO cscMatrix
        denseFromCsr <- csrToDense @3 @3 csrMatrix
        denseFromCsc <- cscToDense @3 @3 cscMatrix
        pure
          ( cooEntries cooMatrix,
            cooEntries cooFromCsr,
            cooEntries cooFromCsc,
            toListMatrix denseFromCsr,
            toListMatrix denseFromCsc
          )
   in extractRight result (\(baseEntries, csrEntries, cscEntries, csrDense, cscDense) -> do
        assertEqual "COO non-zero entries" baseEntries csrEntries
        assertEqual "CSC -> COO preserves entries" baseEntries cscEntries
        assertEqual "CSR round-trip dense payload" [1.0, 0.0, 0.0, 0.0, 2.0, 3.0, 0.0, 0.0, 4.0] csrDense
        assertEqual "CSC round-trip dense payload" [1.0, 0.0, 0.0, 0.0, 2.0, 3.0, 0.0, 0.0, 4.0] cscDense)

testSparseCOORejectsOutOfBounds :: Assertion
testSparseCOORejectsOutOfBounds =
  case mkSparseCOO 2 2 [(2, 0, 1.0 :: Double)] of
    Left (InvariantViolation message) ->
      assertBool "shape error should mention bounds" ("out of bounds" `isInfixOf` message)
    Left err ->
      assertFailure ("expected COO shape error, got: " <> show err)
    Right _ ->
      assertFailure "expected COO constructor to reject out-of-bounds entry"

testCOOToCSRCombinesDuplicateAndPrunesZeroStorageEntries :: Assertion
testCOOToCSRCombinesDuplicateAndPrunesZeroStorageEntries =
  let result = do
        cooValue <-
          mkSparseCOO
            2
            3
            [ (1, 2, 4.0 :: Double),
              (0, 1, 2.0),
              (0, 1, 3.0),
              (0, 2, 0.0)
            ]
        csrValue <- cooToCSR cooValue
        denseFromCsr <- csrToDense @2 @3 csrValue
        matvecResult <- csrMatVecVector csrValue (U.fromList [10.0, 20.0, 30.0])
        pure
          ( csrRowOffsetsVector csrValue,
            csrColumnIndicesVector csrValue,
            csrValuesVector csrValue,
            toListMatrix denseFromCsr,
            matvecResult
          )
   in extractRight result $ \(rowOffsets, columnIndices, values, denseValues, matvecValues) -> do
        assertEqual "COO -> CSR row offsets" (U.fromList [0, 1, 2]) rowOffsets
        assertEqual "COO -> CSR column indices" (U.fromList [1, 2]) columnIndices
        assertEqual "COO -> CSR values combine duplicates and prune explicit zero" (U.fromList [5.0, 4.0]) values
        assertEqual "dense conversion sums duplicate coordinates" [0.0, 5.0, 0.0, 0.0, 0.0, 4.0] denseValues
        assertEqual "matvec sums duplicate stored entries" (U.fromList [100.0, 120.0]) matvecValues

testCanonicalCSRFromEntriesCombinesDuplicatesAndPrunesZeros :: Assertion
testCanonicalCSRFromEntriesCombinesDuplicatesAndPrunesZeros =
  let result = do
        csrValue <-
          canonicalCSRFromEntries
            2
            3
            [ (0, 1, 2.0 :: Double),
              (0, 1, 3.0),
              (0, 2, 0.0),
              (1, 0, 5.0),
              (1, 0, -5.0),
              (1, 2, 4.0)
            ]
        denseFromCsr <- csrToDense @2 @3 csrValue
        pure
          ( csrRowOffsetsVector csrValue,
            csrColumnIndicesVector csrValue,
            csrValuesVector csrValue,
            toListMatrix denseFromCsr
          )
   in extractRight result $ \(rowOffsets, columnIndices, values, denseValues) -> do
        assertEqual "canonical CSR row offsets" (U.fromList [0, 1, 2]) rowOffsets
        assertEqual "canonical CSR column indices" (U.fromList [1, 2]) columnIndices
        assertEqual "canonical CSR values" (U.fromList [5.0, 4.0]) values
        assertEqual "canonical dense payload" [0.0, 5.0, 0.0, 0.0, 0.0, 4.0] denseValues

testCanonicalCSRFromEntriesRejectsOutOfBoundsBeforePruning :: Assertion
testCanonicalCSRFromEntriesRejectsOutOfBoundsBeforePruning =
  case canonicalCSRFromEntries 1 1 [(2, 0, 1.0 :: Double), (2, 0, -1.0)] of
    Left (InvariantViolation message) ->
      assertBool "error should mention out of bounds" ("out of bounds" `isInfixOf` message)
    Left other ->
      assertFailure ("expected InvariantViolation, got: " <> show other)
    Right _ ->
      assertFailure "canonical CSR must reject invalid entries even when duplicates sum to zero"

testStructuredSparseConstructors :: Assertion
testStructuredSparseConstructors =
  let result = do
        diagonalMatrix <- diagonalCSR [0.0 :: Double, 2.0, 0.0, 4.0]
        pathMatrix <- pathLaplacianCSR 4
        densePathMatrix <- csrToDense @4 @4 pathMatrix
        pure
          ( csrRowOffsetsVector diagonalMatrix,
            csrColumnIndicesVector diagonalMatrix,
            csrValuesVector diagonalMatrix,
            toListMatrix densePathMatrix
          )
   in extractRight result $ \(diagonalOffsets, diagonalColumns, diagonalValues, pathDenseValues) -> do
        assertEqual "diagonal CSR prunes zero diagonal entries" (U.fromList [0, 0, 1, 1, 2]) diagonalOffsets
        assertEqual "diagonal CSR column indices" (U.fromList [1, 3]) diagonalColumns
        assertEqual "diagonal CSR values" (U.fromList [2.0, 4.0]) diagonalValues
        assertEqual
          "path graph Laplacian dense payload"
          [ 1.0, -1.0, 0.0, 0.0,
            -1.0, 2.0, -1.0, 0.0,
            0.0, -1.0, 2.0, -1.0,
            0.0, 0.0, -1.0, 1.0
          ]
          pathDenseValues

testTridiagonalCSR :: Assertion
testTridiagonalCSR =
  let result =
        tridiagonalCSR
          [2.0 :: Double, 3.0, 4.0]
          [-1.0, -2.0]
   in extractRight result $ \matrixValue -> do
        assertEqual "row offsets" (U.fromList [0, 2, 5, 7]) (csrRowOffsetsVector matrixValue)
        assertEqual "column indices" (U.fromList [0, 1, 0, 1, 2, 1, 2]) (csrColumnIndicesVector matrixValue)
        assertEqual "values" (U.fromList [2.0, -1.0, -1.0, 3.0, -2.0, -2.0, 4.0]) (csrValuesVector matrixValue)

testOneVertexPathLaplacian :: Assertion
testOneVertexPathLaplacian =
  let result = do
        matrixValue <- pathLaplacianCSR 1
        denseValue <- csrToDense @1 @1 matrixValue
        pure (toListMatrix denseValue)
   in extractRight result $
        assertEqual "P1 Laplacian" [0.0]

testGraphLaplacian :: Assertion
testGraphLaplacian =
  let result = do
        matrixValue <-
          graphLaplacianCSR
            ["b", "a", "c"]
            [ GraphEdge "a" "b" 1.0,
              GraphEdge "b" "a" 2.0,
              GraphEdge "b" "c" 4.0
            ]
        denseValue <- csrToDense @3 @3 matrixValue
        pure
          ( csrRowOffsetsVector matrixValue,
            csrColumnIndicesVector matrixValue,
            csrValuesVector matrixValue,
            toListMatrix denseValue
          )
   in extractRight result $ \(offsets, columns, values, denseEntries) -> do
        assertEqual "offsets" (U.fromList [0, 3, 5, 7]) offsets
        assertEqual "columns" (U.fromList [0, 1, 2, 0, 1, 0, 2]) columns
        assertEqual "values" (U.fromList [7.0, -3.0, -4.0, -3.0, 3.0, -4.0, 4.0]) values
        assertEqual
          "dense Laplacian"
          [ 7.0, -3.0, -4.0,
            -3.0, 3.0, 0.0,
            -4.0, 0.0, 4.0
          ]
          denseEntries

testGraphLaplacianFailures :: Assertion
testGraphLaplacianFailures = do
  assertGraphFailure "duplicate vertices" (graphLaplacianCSR ["a", "a"] [])
  assertGraphFailure "unknown endpoint" (graphLaplacianCSR ["a"] [GraphEdge "a" "b" 1.0])
  assertGraphFailure "self loop" (graphLaplacianCSR ["a"] [GraphEdge "a" "a" 1.0])
  assertGraphFailure "negative weight" (graphLaplacianCSR ["a", "b"] [GraphEdge "a" "b" (-1.0)])
  assertGraphFailure "non-finite weight" (graphLaplacianCSR ["a", "b"] [GraphEdge "a" "b" (0.0 / 0.0)])

assertGraphFailure :: String -> Either MoonlightError value -> Assertion
assertGraphFailure label resultValue =
  case resultValue of
    Left _ -> pure ()
    Right _ -> assertFailure (label <> ": expected graph construction failure")

testQrDecomp :: Assertion
testQrDecomp =
  let result = do
        matrixValue <- fromListMatrix @3 @2 @Double [1.0, 1.0, 1.0, 0.0, 1.0, 2.0]
        (qMatrix, rMatrix) <- qrDecompFullColumnRank matrixValue
        reconstructed <- mult qMatrix rMatrix
        pure (toListMatrix reconstructed)
   in extractRight result (\values -> assertApproxList "QR reconstruction" [1.0, 1.0, 1.0, 0.0, 1.0, 2.0] values)

testCholeskyDecomp :: Assertion
testCholeskyDecomp =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [4.0, 2.0, 2.0, 3.0]
        lowerMatrix <- choleskyDecomp matrixValue
        transposedLower <- transpose lowerMatrix
        reconstructed <- mult lowerMatrix transposedLower
        pure (toListMatrix reconstructed)
   in extractRight result (\values -> assertApproxList "Cholesky reconstruction" [4.0, 2.0, 2.0, 3.0] values)

testCholeskyRejectsNonSymmetric :: Assertion
testCholeskyRejectsNonSymmetric =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [4.0, 1.0, 3.0, 3.0]
        choleskyDecomp matrixValue
   in case result of
        Left (InvariantViolation msg) -> assertBool "error should mention symmetric" ("symmetric" `isInfixOf` msg)
        Left other -> assertFailure ("expected InvariantViolation about symmetry, got: " <> show other)
        Right _ -> assertFailure "Cholesky should reject non-symmetric matrix"

testSymmetricEigen :: Assertion
testSymmetricEigen =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [2.0, 0.0, 0.0, 3.0]
        (eigenvalues, eigenvectors) <- symmetricEigen matrixValue
        pure (toListVector eigenvalues, toListMatrix eigenvectors)
   in extractRight result (\(values, vectors) -> do
        assertApproxList "eigenvalues" [3.0, 2.0] values
        assertApproxList "eigenvector matrix" [0.0, 1.0, 1.0, 0.0] vectors)

testSymmetricEigenReconstructsCoupledMatrix :: Assertion
testSymmetricEigenReconstructsCoupledMatrix =
  let sourceRows =
        [ 4.0, 1.0, 2.0,
          1.0, 3.0, 0.5,
          2.0, 0.5, 5.0
        ]
      result = do
        matrixValue <- fromListMatrix @3 @3 @Double sourceRows
        (eigenvalues, eigenvectors) <- symmetricEigen matrixValue
        diagonalized <- fromListMatrix @3 @3 @Double (diagonalMatrixEntries (toListVector eigenvalues))
        weightedEigenvectors <- mult eigenvectors diagonalized
        transposedEigenvectors <- transpose eigenvectors
        reconstructed <- mult weightedEigenvectors transposedEigenvectors
        pure (toListMatrix reconstructed)
   in extractRight result (assertApproxList "symmetric eigen reconstruction" sourceRows)

testSymmetricEigenDirichletSecondDifferenceSpectrum :: Assertion
testSymmetricEigenDirichletSecondDifferenceSpectrum =
  let result = do
        matrixValue <-
          fromListMatrix @3 @3 @Double
            [ 2.0, -1.0, 0.0,
              -1.0, 2.0, -1.0,
              0.0, -1.0, 2.0
            ]
        (eigenvalues, _) <- symmetricEigen matrixValue
        pure (toListVector eigenvalues)
      expected =
        [ 2.0 + sqrt 2.0,
          2.0,
          2.0 - sqrt 2.0
        ]
   in extractRight result (assertApproxList "Dirichlet second-difference spectrum" expected)

testSymmetricEigenRejectsNonFinite :: Assertion
testSymmetricEigenRejectsNonFinite =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [1.0, 0.0, 0.0, 0.0 / 0.0]
        symmetricEigen matrixValue
   in case result of
        Left (InvariantViolation msg) -> assertBool "error should mention finite" ("finite" `isInfixOf` msg)
        Left other -> assertFailure ("expected InvariantViolation about finite entries, got: " <> show other)
        Right _ -> assertFailure "symmetricEigen should reject NaN entries"

testSvdDecomp :: Assertion
testSvdDecomp =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [3.0, 0.0, 0.0, 2.0]
        (uMatrix, sMatrix, vTMatrix) <- thinSvdFullColumnRank matrixValue
        usMatrix <- mult uMatrix sMatrix
        reconstructed <- mult usMatrix vTMatrix
        uTMatrix <- transpose uMatrix
        uOrthogonality <- mult uTMatrix uMatrix
        vMatrix <- transpose vTMatrix
        vOrthogonality <- mult vTMatrix vMatrix
        pure (toListMatrix reconstructed, toListMatrix uOrthogonality, toListMatrix vOrthogonality)
   in extractRight result $ \(reconstructedValues, uOrthogonalityValues, vOrthogonalityValues) -> do
        assertApproxList "SVD reconstruction" [3.0, 0.0, 0.0, 2.0] reconstructedValues
        assertApproxList "SVD U orthonormality" (identityMatrixEntries 2) uOrthogonalityValues
        assertApproxList "SVD V orthonormality" (identityMatrixEntries 2) vOrthogonalityValues

testSolveDirect :: Assertion
testSolveDirect =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [3.0, 1.0, 1.0, 2.0]
        vectorValue <- fromListVector @2 @Double [9.0, 8.0]
        solution <- solveDirect matrixValue vectorValue
        pure (toListVector solution)
   in extractRight result (\values -> assertApproxList "direct solver solution" [2.0, 3.0] values)

testSolveDirectGeneratedExactSemantics :: Assertion
testSolveDirectGeneratedExactSemantics =
  Foldable.traverse_ assertSeed generatedSeeds
  where
    assertSeed seedValue =
      let matrixRational = generatedSolveMatrix seedValue
          solutionRational = generatedSolveSolution seedValue
          rhsRational = multiplySquareRowsVector 3 matrixRational solutionRational
          result = do
            exactSolution <- exactPluSolve3 matrixRational rhsRational
            matrixValue <- fromListMatrix @3 @3 @Double (fmap fromRational matrixRational)
            rhsValue <- fromListVector @3 @Double (fmap fromRational rhsRational)
            solutionValue <- solveDirect matrixValue rhsValue
            pure (fmap fromRational exactSolution, toListVector solutionValue, fmap fromRational rhsRational, fmap fromRational matrixRational)
       in extractRight result $ \(expected, actual, rhsValues, matrixValues) -> do
            assertApproxList ("generated exact solve seed " <> show seedValue) expected actual
            assertResidualBelow
              ("generated solve residual seed " <> show seedValue)
              1.0e-8
              (matrixVectorResidual 3 matrixValues actual rhsValues)

testQrGeneratedResiduals :: Assertion
testQrGeneratedResiduals =
  Foldable.traverse_ assertSeed generatedSeeds
  where
    assertSeed seedValue =
      let matrixEntries = generatedQrMatrix seedValue
          result = do
            matrixValue <- fromListMatrix @4 @3 @Double matrixEntries
            (qMatrix, rMatrix) <- qrDecompFullColumnRank matrixValue
            reconstructed <- mult qMatrix rMatrix
            pure (toListMatrix reconstructed)
       in extractRight result $
            assertResidualBelow
              ("generated QR residual seed " <> show seedValue)
              1.0e-8
              . maxAbsDifference matrixEntries

testCholeskyGeneratedResiduals :: Assertion
testCholeskyGeneratedResiduals =
  Foldable.traverse_ assertSeed generatedSeeds
  where
    assertSeed seedValue =
      let matrixEntries = generatedSpdMatrix seedValue
          result = do
            matrixValue <- fromListMatrix @3 @3 @Double matrixEntries
            lowerMatrix <- choleskyDecomp matrixValue
            transposedLower <- transpose lowerMatrix
            reconstructed <- mult lowerMatrix transposedLower
            pure (toListMatrix reconstructed)
       in extractRight result $
            assertResidualBelow
              ("generated Cholesky residual seed " <> show seedValue)
              1.0e-8
              . maxAbsDifference matrixEntries

testSolveCg :: Assertion
testSolveCg =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [4.0, 1.0, 1.0, 3.0]
        vectorValue <- fromListVector @2 @Double [1.0, 2.0]
        solution <- solveCG matrixValue vectorValue
        pure (toListVector solution)
   in extractRight result (\values -> assertApproxList "CG solver solution" [1.0 / 11.0, 7.0 / 11.0] values)

testSolveGmres :: Assertion
testSolveGmres =
  let result = do
        matrixValue <- fromListMatrix @2 @2 @Double [3.0, 2.0, 0.0, 1.0]
        vectorValue <- fromListVector @2 @Double [2.0, 1.0]
        solution <- solveGMRES matrixValue vectorValue
        pure (toListVector solution)
   in extractRight result (\values -> assertApproxList "GMRES solver solution" [0.0, 1.0] values)

diagonalMatrixEntries :: [Double] -> [Double]
diagonalMatrixEntries diagonalValues =
  let matrixSize = length diagonalValues
   in [ diagonalEntry rowIndex columnIndex
        | rowIndex <- [0 .. matrixSize - 1],
          columnIndex <- [0 .. matrixSize - 1]
      ]
  where
    diagonalEntry rowIndex columnIndex
      | rowIndex == columnIndex =
          case drop rowIndex diagonalValues of
            diagonalValue : _ -> diagonalValue
            [] -> 0.0
      | otherwise = 0.0

identityMatrixEntries :: Int -> [Double]
identityMatrixEntries matrixSize =
  [ if rowIndex == columnIndex then 1.0 else 0.0
    | rowIndex <- [0 .. matrixSize - 1],
      columnIndex <- [0 .. matrixSize - 1]
  ]

generatedSeeds :: [Int]
generatedSeeds = [1, 2, 3, 5, 8, 13]

generatedSolveMatrix :: Int -> [Rational]
generatedSolveMatrix seedValue =
  fmap fromIntegral [generatedSolveEntry seedValue rowIndex columnIndex | rowIndex <- [0 .. 2], columnIndex <- [0 .. 2]]

generatedSolveEntry :: Int -> Int -> Int -> Int
generatedSolveEntry seedValue rowIndex columnIndex
  | rowIndex == columnIndex = 12 + seedValue + rowIndex
  | otherwise = ((seedValue + rowIndex * 3 + columnIndex * 5) `mod` 5) - 2

generatedSolveSolution :: Int -> [Rational]
generatedSolveSolution seedValue =
  fmap fromIntegral [seedValue + 1, 3 - seedValue, seedValue * 2 - 5]

generatedQrMatrix :: Int -> [Double]
generatedQrMatrix seedValue =
  [ generatedQrEntry seedValue rowIndex columnIndex
    | rowIndex <- [0 .. 3],
      columnIndex <- [0 .. 2]
  ]

generatedQrEntry :: Int -> Int -> Int -> Double
generatedQrEntry seedValue rowIndex columnIndex
  | rowIndex == columnIndex = fromIntegral (8 + seedValue + columnIndex)
  | otherwise = fromIntegral (((seedValue + rowIndex * 2 + columnIndex * 3) `mod` 7) - 3) / 5.0

generatedSpdMatrix :: Int -> [Double]
generatedSpdMatrix seedValue =
  [ sum [generatedLowerEntry seedValue rowIndex k * generatedLowerEntry seedValue columnIndex k | k <- [0 .. 2]]
    | rowIndex <- [0 .. 2],
      columnIndex <- [0 .. 2]
  ]

generatedLowerEntry :: Int -> Int -> Int -> Double
generatedLowerEntry seedValue rowIndex columnIndex
  | columnIndex > rowIndex = 0.0
  | rowIndex == columnIndex = fromIntegral (4 + seedValue + rowIndex)
  | otherwise = fromIntegral (((seedValue + rowIndex * 3 + columnIndex * 2) `mod` 5) - 2) / 4.0

exactPluSolve3 :: [Rational] -> [Rational] -> Either MoonlightError [Rational]
exactPluSolve3 matrixValues rhsValues = do
  matrixValue <- fromListMatrix @3 @3 @Rational matrixValues
  pluValue <- pluDecompFullRank matrixValue
  let permutationRows = rowMajorRows 3 (toListMatrix (pluPermutation pluValue))
      lowerRows = rowMajorRows 3 (toListMatrix (pluLower pluValue))
      upperRows = rowMajorRows 3 (toListMatrix (pluUpper pluValue))
      permutedRhs = multiplyRowsVector permutationRows rhsValues
  forwardValues <- forwardSubstituteRational lowerRows permutedRhs
  backwardSubstituteRational upperRows forwardValues

forwardSubstituteRational :: [[Rational]] -> [Rational] -> Either MoonlightError [Rational]
forwardSubstituteRational lowerRows rhsValues = go 0 [] lowerRows rhsValues
  where
    go :: Int -> [Rational] -> [[Rational]] -> [Rational] -> Either MoonlightError [Rational]
    go !_ solvedValues [] [] = Right solvedValues
    go !rowIndex solvedValues (rowValues : remainingRows) (rhsValue : remainingRhs) = do
      diagonalValue <- requireTestEntry ("exact forward diagonal missing at row " <> show rowIndex) rowIndex rowValues
      if diagonalValue == 0
        then Left (InvariantViolation "exact forward substitution encountered zero diagonal")
        else
          let knownContribution = sum (zipWith (*) (take rowIndex rowValues) solvedValues)
              nextValue = (rhsValue - knownContribution) / diagonalValue
           in go (rowIndex + 1) (solvedValues <> [nextValue]) remainingRows remainingRhs
    go _ _ _ _ = Left (InvariantViolation "exact forward substitution shape mismatch")

backwardSubstituteRational :: [[Rational]] -> [Rational] -> Either MoonlightError [Rational]
backwardSubstituteRational upperRows rhsValues = go (length upperRows - 1) []
  where
    go !rowIndex solvedSuffix
      | rowIndex < 0 = Right solvedSuffix
      | otherwise = do
          rowValues <- requireTestEntry ("exact backward row missing at row " <> show rowIndex) rowIndex upperRows
          rhsValue <- requireTestEntry ("exact backward RHS missing at row " <> show rowIndex) rowIndex rhsValues
          diagonalValue <- requireTestEntry ("exact backward diagonal missing at row " <> show rowIndex) rowIndex rowValues
          if diagonalValue == 0
            then Left (InvariantViolation "exact backward substitution encountered zero diagonal")
            else
              let knownContribution = sum (zipWith (*) (drop (rowIndex + 1) rowValues) solvedSuffix)
                  nextValue = (rhsValue - knownContribution) / diagonalValue
               in go (rowIndex - 1) (nextValue : solvedSuffix)

rowMajorRows :: Int -> [a] -> [[a]]
rowMajorRows columnCount values
  | columnCount <= 0 = []
  | otherwise =
      case splitAt columnCount values of
        ([], []) -> []
        (rowValues, remainingValues) -> rowValues : rowMajorRows columnCount remainingValues

multiplyRowsVector :: Num a => [[a]] -> [a] -> [a]
multiplyRowsVector rows vectorValues =
  fmap (\rowValues -> sum (zipWith (*) rowValues vectorValues)) rows

multiplySquareRowsVector :: Num a => Int -> [a] -> [a] -> [a]
multiplySquareRowsVector columnCount matrixValues =
  multiplyRowsVector (rowMajorRows columnCount matrixValues)

matrixVectorResidual :: Int -> [Double] -> [Double] -> [Double] -> Double
matrixVectorResidual columnCount matrixValues vectorValues rhsValues =
  maxAbsDifference rhsValues (multiplySquareRowsVector columnCount matrixValues vectorValues)

maxAbsDifference :: [Double] -> [Double] -> Double
maxAbsDifference expected actual =
  maximum (0.0 : zipWith (\leftValue rightValue -> abs (leftValue - rightValue)) expected actual)

assertResidualBelow :: String -> Double -> Double -> Assertion
assertResidualBelow label tolerance residualValue =
  assertBool (label <> ": residual " <> show residualValue <> " exceeded " <> show tolerance) (residualValue <= tolerance)

requireTestEntry :: String -> Int -> [a] -> Either MoonlightError a
requireTestEntry label targetIndex values =
  case drop targetIndex values of
    entryValue : _ -> Right entryValue
    [] -> Left (InvariantViolation label)

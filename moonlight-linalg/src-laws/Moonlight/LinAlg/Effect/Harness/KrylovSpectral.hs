{-# LANGUAGE DataKinds #-}

module Moonlight.LinAlg.Effect.Harness.KrylovSpectral
  ( arnoldiRelationHoldsLaw,
    arnoldiBasisOrthonormalLaw,
    lanczosProjectionTridiagonalLaw,
    lanczosBasisOrthonormalLaw,
    thickRestartLockedPairsResidualBoundedLaw,
    selectedPairsResidualBoundedLaw,
    selectedPairsClusterOrthonormalLaw,
    tridiagonalSelectedValuesAgreeWithAllPairsLaw,
    diagonalSpectralValuesExactLaw,
    pathLaplacianSpectralValuesClosedFormLaw,
    eigenRequestRejectsOversubscriptionLaw,
  )
where

import Data.Bifunctor (first)
import Data.Vector qualified as Box
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (fieldValueValid)
import Moonlight.LinAlg
  ( EigenRequest (..),
    EigenSolveConfig,
    Eigenpairs,
    LinearOperator,
    OperatorSymmetry (..),
    SpectrumEnd (..),
    arnoldi,
    arnoldiBasisColumns,
    arnoldiHessenbergRows,
    defaultArnoldiConfig,
    defaultEigenSolveConfig,
    defaultLanczosConfig,
    diagonalLinearOperator,
    eigenpairCount,
    eigenpairResidualNorms,
    eigenpairValues,
    eigenpairVectorAt,
    lanczosAlphaDiagonal,
    lanczosBasisColumns,
    lanczosBetaOffDiagonal,
    lanczosStepsCompleted,
    lanczosSymmetric,
    mkNonNegativeConfigTolerance,
    mkPositiveCount,
    mkSparseCOO,
    cooToCSR,
    pathLaplacianLinearOperator,
    runOperatorU,
    selfAdjointCSRLinearOperator,
    solveEigenRequest,
    withArnoldiIterations,
    withEigenFallbackInitialVector,
    withEigenFallbackLanczosConfig,
    withLanczosIterations,
    withLanczosTolerance,
  )
import Moonlight.LinAlg.Effect.Harness.Core
  ( approxTolerance,
    assertApproxList,
    assertApproxListWith,
    assertRightProperty,
    orthonormalTolerance,
    residualTolerance,
  )
import Test.Tasty.QuickCheck qualified as QC

arnoldiRelationHoldsLaw :: QC.Property
arnoldiRelationHoldsLaw =
  assertRightProperty $ do
    iterationCount <- mapLeftShow (mkPositiveCount 2)
    operatorValue <- mapLeftShow (diagonalLinearOperator (U.fromList [1.0, 3.0]))
    decomposition <- mapLeftShow (arnoldi (withArnoldiIterations iterationCount defaultArnoldiConfig) operatorValue (U.fromList [1.0, 1.0]))
    relationHolds operatorValue (arnoldiBasisColumns decomposition) (arnoldiHessenbergRows decomposition)

arnoldiBasisOrthonormalLaw :: QC.Property
arnoldiBasisOrthonormalLaw =
  assertRightProperty $ do
    iterationCount <- mapLeftShow (mkPositiveCount 3)
    operatorValue <- mapLeftShow (diagonalLinearOperator (U.fromList [1.0, 2.0, 4.0]))
    decomposition <- mapLeftShow (arnoldi (withArnoldiIterations iterationCount defaultArnoldiConfig) operatorValue (U.fromList [1.0, 1.0, 1.0]))
    pure (orthonormalColumns (arnoldiBasisColumns decomposition))

lanczosProjectionTridiagonalLaw :: QC.Property
lanczosProjectionTridiagonalLaw =
  assertRightProperty $ do
    iterationCount <- mapLeftShow (mkPositiveCount 3)
    operatorValue <- mapLeftShow (pathLaplacianLinearOperator 4)
    decomposition <- mapLeftShow (lanczosSymmetric (withLanczosIterations iterationCount defaultLanczosConfig) operatorValue (U.fromList [1.0, 0.0, 0.0, 0.0]))
    let stepCount = lanczosStepsCompleted decomposition
    pure
      ( stepCount > 0
          && length (lanczosAlphaDiagonal decomposition) == stepCount
          && length (lanczosBetaOffDiagonal decomposition) == max 0 (stepCount - 1)
          && Box.length (lanczosBasisColumns decomposition) == stepCount
      )

lanczosBasisOrthonormalLaw :: QC.Property
lanczosBasisOrthonormalLaw =
  assertRightProperty $ do
    iterationCount <- mapLeftShow (mkPositiveCount 4)
    operatorValue <- mapLeftShow (pathLaplacianLinearOperator 5)
    decomposition <- mapLeftShow (lanczosSymmetric (withLanczosIterations iterationCount defaultLanczosConfig) operatorValue (U.fromList [1.0, 0.5, 0.25, 0.125, 0.0625]))
    pure (orthonormalColumns (lanczosBasisColumns decomposition))

thickRestartLockedPairsResidualBoundedLaw :: QC.Property
thickRestartLockedPairsResidualBoundedLaw =
  assertRightProperty $ do
    countValue <- mapLeftShow (mkPositiveCount 3)
    operatorValue <- genericPentadiagonalOperator 18
    solveConfig <- restartedSolveConfig 5 approxTolerance (restartSeedVector 18)
    pairs <- mapLeftShow (solveEigenRequest solveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue))
    pure (eigenpairCount pairs == 3 && eigenpairResidualsBounded pairs)

selectedPairsResidualBoundedLaw :: QC.Property
selectedPairsResidualBoundedLaw =
  assertRightProperty $ do
    countValue <- mapLeftShow (mkPositiveCount 2)
    operatorValue <- tridiagonalOperator [2.0, 2.5, 3.0, 3.5, 4.0] [-0.31, -0.27, -0.23, -0.19]
    pairs <- mapLeftShow (solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue))
    pure (eigenpairCount pairs == 2 && eigenpairResidualsBounded pairs)

selectedPairsClusterOrthonormalLaw :: QC.Property
selectedPairsClusterOrthonormalLaw =
  assertRightProperty $ do
    countValue <- mapLeftShow (mkPositiveCount 3)
    operatorValue <- mapLeftShow (diagonalLinearOperator (U.fromList [2.0, 2.0, 2.0, 5.0]))
    pairs <- mapLeftShow (solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue))
    pure (eigenpairCount pairs == 3 && orthonormalEigenpairs pairs)

tridiagonalSelectedValuesAgreeWithAllPairsLaw :: QC.Property
tridiagonalSelectedValuesAgreeWithAllPairsLaw =
  assertRightProperty $ do
    selectedCount <- mapLeftShow (mkPositiveCount 3)
    fullCount <- mapLeftShow (mkPositiveCount 5)
    operatorValue <- tridiagonalOperator [2.0, 2.5, 3.0, 3.5, 4.0] [-0.31, -0.27, -0.23, -0.19]
    selectedValues <- mapLeftShow (solveEigenRequest defaultEigenSolveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues selectedCount))
    allPairs <- mapLeftShow (solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest SmallestEigenvalues fullCount))
    pure (assertApproxListWith residualTolerance (U.toList selectedValues) (take 3 (U.toList (eigenpairValues allPairs))))

diagonalSpectralValuesExactLaw :: QC.Property
diagonalSpectralValuesExactLaw =
  assertRightProperty $ do
    countValue <- mapLeftShow (mkPositiveCount 2)
    operatorValue <- mapLeftShow (diagonalLinearOperator (U.fromList [3.0, -2.0, 7.0, 1.0]))
    values <- mapLeftShow (solveEigenRequest defaultEigenSolveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue))
    pure (U.toList values == [-2.0, 1.0])

pathLaplacianSpectralValuesClosedFormLaw :: QC.Property
pathLaplacianSpectralValuesClosedFormLaw =
  assertRightProperty $ do
    countValue <- mapLeftShow (mkPositiveCount 3)
    operatorValue <- mapLeftShow (pathLaplacianLinearOperator 5)
    values <- mapLeftShow (solveEigenRequest defaultEigenSolveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue))
    pure (assertApproxList (pathLaplacianValues 5 [0, 1, 2]) (U.toList values))

eigenRequestRejectsOversubscriptionLaw :: QC.Property
eigenRequestRejectsOversubscriptionLaw =
  assertRightProperty $ do
    countValue <- mapLeftShow (mkPositiveCount 4)
    operatorValue <- mapLeftShow (diagonalLinearOperator (U.fromList [1.0, 2.0, 3.0]))
    let resultValue = solveEigenRequest defaultEigenSolveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue)
    pure
      ( case resultValue of
          Left _ -> True
          Right _ -> False
      )

relationHolds ::
  LinearOperator symmetry ->
  Box.Vector (U.Vector Double) ->
  Box.Vector (U.Vector Double) ->
  Either String Bool
relationHolds operatorValue basisColumns hessenbergRows =
  let basisValues = Box.toList basisColumns
      hessenbergValues = U.toList <$> Box.toList hessenbergRows
      stepCount = Box.length hessenbergRows - 1
      relationAt columnIndex = do
        basisVector <- maybeToEither ("missing Arnoldi basis column " <> show columnIndex) (entryAt columnIndex basisValues)
        imageVector <- mapLeftShow (runOperatorU operatorValue basisVector)
        coefficients <- traverse (maybeToEither ("missing Arnoldi coefficient at column " <> show columnIndex) . entryAt columnIndex) hessenbergValues
        pure
          ( assertApproxList
              (U.toList imageVector)
              (U.toList (linearCombinationU (take (length basisValues) coefficients) basisValues))
              && assertApproxList [0.0] (drop (length basisValues) coefficients)
          )
   in fmap and (traverse relationAt [0 .. stepCount - 1])

orthonormalColumns :: Box.Vector (U.Vector Double) -> Bool
orthonormalColumns columns =
  and
    [ assertApproxListWith orthonormalTolerance [expectedValue] [vectorDotU leftColumn rightColumn]
      | (leftIndex, leftColumn) <- zip [0 :: Int ..] (Box.toList columns),
        (rightIndex, rightColumn) <- zip [0 :: Int ..] (Box.toList columns),
        leftIndex <= rightIndex,
        let expectedValue = if leftIndex == rightIndex then 1.0 else 0.0
    ]

orthonormalEigenpairs :: Eigenpairs -> Bool
orthonormalEigenpairs pairs =
  case traverse (`eigenpairVectorAt` pairs) [0 .. eigenpairCount pairs - 1] of
    Left _ -> False
    Right columns -> orthonormalColumns (Box.fromList columns)

eigenpairResidualsBounded :: Eigenpairs -> Bool
eigenpairResidualsBounded pairs =
  U.all (\residualNorm -> fieldValueValid residualNorm && residualNorm <= residualTolerance) (eigenpairResidualNorms pairs)

restartedSolveConfig :: Int -> Double -> U.Vector Double -> Either String EigenSolveConfig
restartedSolveConfig iterationLimit toleranceValue seedVector = do
  iterationCount <- mapLeftShow (mkPositiveCount iterationLimit)
  toleranceBound <- mapLeftShow (mkNonNegativeConfigTolerance toleranceValue)
  let lanczosConfig = withLanczosTolerance toleranceBound (withLanczosIterations iterationCount defaultLanczosConfig)
  pure (withEigenFallbackInitialVector seedVector (withEigenFallbackLanczosConfig lanczosConfig defaultEigenSolveConfig))

tridiagonalOperator :: [Double] -> [Double] -> Either String (LinearOperator 'SelfAdjointOperator)
tridiagonalOperator diagonalEntries offDiagonalEntries =
  mapLeftShow
    ( selfAdjointCSRLinearOperator
        =<< (mkSparseCOO (length diagonalEntries) (length diagonalEntries) (tridiagonalEntries diagonalEntries offDiagonalEntries) >>= cooToCSR)
    )

tridiagonalEntries :: [Double] -> [Double] -> [(Int, Int, Double)]
tridiagonalEntries diagonalEntries offDiagonalEntries =
  zipWith (\entryIndex entryValue -> (entryIndex, entryIndex, entryValue)) [0 ..] diagonalEntries
    <> concat
      ( zipWith
          ( \entryIndex entryValue ->
              [(entryIndex, entryIndex + 1, entryValue), (entryIndex + 1, entryIndex, entryValue)]
          )
          [0 ..]
          offDiagonalEntries
      )

genericPentadiagonalOperator :: Int -> Either String (LinearOperator 'SelfAdjointOperator)
genericPentadiagonalOperator dimension =
  mapLeftShow
    ( selfAdjointCSRLinearOperator
        =<< (mkSparseCOO dimension dimension (genericPentadiagonalEntries dimension) >>= cooToCSR)
    )

genericPentadiagonalEntries :: Int -> [(Int, Int, Double)]
genericPentadiagonalEntries dimension =
  diagonalEntries <> firstOffDiagonalEntries <> secondOffDiagonalEntries
  where
    diagonalEntries =
      (\rowIndex -> (rowIndex, rowIndex, 4.0 + 0.03 * fromIntegral (rowIndex `mod` 7)))
        <$> [0 .. dimension - 1]
    firstOffDiagonalEntries =
      symmetricBandEntries dimension 1 (\rowIndex -> -1.0 - 0.01 * fromIntegral (rowIndex `mod` 5))
    secondOffDiagonalEntries =
      symmetricBandEntries dimension 2 (\rowIndex -> -0.2 - 0.005 * fromIntegral (rowIndex `mod` 3))

symmetricBandEntries :: Int -> Int -> (Int -> Double) -> [(Int, Int, Double)]
symmetricBandEntries dimension offset entryValueAt =
  concat
    ( ( \rowIndex ->
          let columnIndex = rowIndex + offset
              entryValue = entryValueAt rowIndex
           in [(rowIndex, columnIndex, entryValue), (columnIndex, rowIndex, entryValue)]
      )
        <$> [0 .. dimension - offset - 1]
    )

restartSeedVector :: Int -> U.Vector Double
restartSeedVector dimension =
  U.generate dimension (\indexValue -> 1.0 / fromIntegral (indexValue + 1))

pathLaplacianValues :: Int -> [Int] -> [Double]
pathLaplacianValues dimension =
  fmap (\modeIndex -> 2.0 - 2.0 * cos (pi * fromIntegral modeIndex / fromIntegral dimension))

linearCombinationU :: [Double] -> [U.Vector Double] -> U.Vector Double
linearCombinationU coefficients basisVectors =
  case basisVectors of
    [] -> U.empty
    firstVector : _ ->
      foldr (U.zipWith (+)) (U.replicate (U.length firstVector) 0.0) (zipWith (\coefficient vectorValue -> U.map (* coefficient) vectorValue) coefficients basisVectors)

vectorDotU :: U.Vector Double -> U.Vector Double -> Double
vectorDotU leftVector rightVector =
  U.sum (U.zipWith (*) leftVector rightVector)

entryAt :: Int -> [value] -> Maybe value
entryAt targetIndex values =
  case drop targetIndex values of
    entryValue : _ -> Just entryValue
    [] -> Nothing

maybeToEither :: failure -> Maybe value -> Either failure value
maybeToEither failureValue value =
  case value of
    Just presentValue -> Right presentValue
    Nothing -> Left failureValue

mapLeftShow :: Show failure => Either failure value -> Either String value
mapLeftShow = first show

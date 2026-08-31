{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Moonlight.LinAlg.Pure.Dense.Decomposition
  ( qrDecompFullColumnRank,
    choleskyDecomp,
    symmetricEigen,
    symmetricEigenPairs,
    thinSvdFullColumnRank,
  )
where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Vector.Storable qualified as S
import GHC.TypeNats (KnownNat)
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Dense.DoubleFactorization
  ( choleskyLower,
    qrFullColumnRank,
  )
import Moonlight.LinAlg.Internal.Dense.OneSidedJacobiSVD qualified as JacobiSVD
import Moonlight.LinAlg.Pure.Dense.Rows (transposeRowsExact)
import Moonlight.LinAlg.Internal.Eigen.Input
  ( validateSymmetricEigenInput,
  )
import Moonlight.LinAlg.Internal.Eigen.Symmetric
  ( SymmetricEigenResult (..),
    symmetricEigenPairsDenseUnchecked,
  )
import Moonlight.LinAlg.Pure.Dense.Flat
  ( denseDoubleMatrixShape,
    denseDoubleMatrixToRowMajorVector,
    denseDoubleMatrixToRows,
    mkDenseDoubleMatrixRows,
  )
import Moonlight.LinAlg.Pure.Dense.Types (Matrix, Vector, fromListMatrix, fromListVector, toListMatrix)
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

qrDecompFullColumnRank ::
  forall r c.
  (KnownNat r, KnownNat c) =>
  Matrix r c Double ->
  Either MoonlightError (Matrix r c Double, Matrix c c Double)
qrDecompFullColumnRank matrixValue = do
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
  (qValues, rValues) <- qrFullColumnRank rowCount columnCount (toListMatrix matrixValue)
  qMatrix <- fromListMatrix @r @c qValues
  rMatrix <- fromListMatrix @c @c rValues
  pure (qMatrix, rMatrix)

choleskyDecomp ::
  forall n.
  KnownNat n =>
  Matrix n n Double ->
  Either MoonlightError (Matrix n n Double)
choleskyDecomp matrixValue = do
  let (matrixSize, _) = DenseTypes.matrixShape matrixValue
  lowerValues <- choleskyLower matrixSize (toListMatrix matrixValue)
  fromListMatrix @n @n lowerValues

symmetricEigenPairs :: Int -> [[Double]] -> Either MoonlightError [(Double, [Double])]
symmetricEigenPairs matrixSize matrixRows = do
  validateSymmetricEigenInput "symmetric eigen decomposition" matrixSize matrixRows
  matrixValue <- mkDenseDoubleMatrixRows matrixRows
  eigenResultToPairs <$> symmetricEigenPairsDenseUnchecked matrixSize matrixValue

symmetricEigen ::
  forall n.
  KnownNat n =>
  Matrix n n Double ->
  Either MoonlightError (Vector n Double, Matrix n n Double)
symmetricEigen matrixValue = do
  let (matrixSize, _) = DenseTypes.matrixShape matrixValue
  matrixRows <- DenseTypes.matrixToRows matrixValue
  validateSymmetricEigenInput "symmetric eigen decomposition" matrixSize matrixRows
  denseMatrix <- mkDenseDoubleMatrixRows matrixRows
  eigenResult <- symmetricEigenPairsDenseUnchecked matrixSize denseMatrix
  let orderedPairs = sortBy (flip (comparing fst)) (eigenResultToPairs eigenResult)
      eigenvalues = map fst orderedPairs
      eigenvectors = map snd orderedPairs
  eigenvalueVector <- fromListVector @n eigenvalues
  eigenvectorRows <- transposeRowsExact eigenvectors
  eigenvectorMatrix <- fromListMatrix @n @n (concat eigenvectorRows)
  pure (eigenvalueVector, eigenvectorMatrix)

diagonalRows :: [Double] -> [[Double]]
diagonalRows diagonalValues =
  let indexedDiagonalValues = zip [0 :: Int ..] diagonalValues
      size = length diagonalValues
   in map
        (\(rowIndex, diagonalValue) -> map (\columnIndex -> if rowIndex == columnIndex then diagonalValue else 0.0) [0 .. size - 1])
        indexedDiagonalValues

thinSvdFullColumnRank ::
  forall r c.
  (KnownNat r, KnownNat c) =>
  Matrix r c Double ->
  Either MoonlightError (Matrix r c Double, Matrix c c Double, Matrix c c Double)
thinSvdFullColumnRank matrixValue = do
  rows <- DenseTypes.matrixToRows matrixValue
  denseMatrix <- mkDenseDoubleMatrixRows rows
  JacobiSVD.ThinSvdResult {..} <-
    firstMoonlightSvdFailure (JacobiSVD.thinSvdFullColumnRank denseMatrix)
  let singularValues = S.toList thinSvdSingularValues
      sRows = diagonalRows singularValues
      uRows = denseDoubleMatrixToRows thinSvdLeftSingularVectors
      vTRows = denseDoubleMatrixToRows thinSvdRightSingularVectorsTransposed
  uMatrix <- fromListMatrix @r @c (concat uRows)
  sMatrix <- fromListMatrix @c @c (concat sRows)
  vTMatrix <- fromListMatrix @c @c (concat vTRows)
  pure (uMatrix, sMatrix, vTMatrix)

eigenResultToPairs :: SymmetricEigenResult -> [(Double, [Double])]
eigenResultToPairs SymmetricEigenResult {..} =
  fmap eigenPairAt [0 .. matrixSize - 1]
  where
    !(matrixSize, _) = denseDoubleMatrixShape symmetricEigenResultVectors
    eigenvectorPayload = denseDoubleMatrixToRowMajorVector symmetricEigenResultVectors

    eigenPairAt !columnIndex =
      ( symmetricEigenResultValues `S.unsafeIndex` columnIndex,
        fmap
          (\rowIndex -> eigenvectorPayload `S.unsafeIndex` (rowIndex * matrixSize + columnIndex))
          [0 .. matrixSize - 1]
      )

firstMoonlightSvdFailure :: Either JacobiSVD.ThinSvdFailure value -> Either MoonlightError value
firstMoonlightSvdFailure resultValue =
  case resultValue of
    Right value -> Right value
    Left failureValue -> Left (InvariantViolation (thinSvdFailureMessage failureValue))

thinSvdFailureMessage :: JacobiSVD.ThinSvdFailure -> String
thinSvdFailureMessage failureValue =
  case failureValue of
    JacobiSVD.ThinSvdNonFiniteInput ->
      "thin Jacobi SVD requires finite entries"
    JacobiSVD.ThinSvdDimensionViolation message ->
      message
    JacobiSVD.ThinSvdRankDeficient columnIndex singularValue ->
      "thin Jacobi SVD requires full column rank; column "
        <> show columnIndex
        <> " singular value "
        <> show singularValue
        <> " is below rank tolerance"
    JacobiSVD.ThinSvdSweepBudgetNonConvergence sweepBudget maximumCross ->
      "thin Jacobi SVD exhausted "
        <> show sweepBudget
        <> " sweeps; maximum normalized column cross="
        <> show maximumCross

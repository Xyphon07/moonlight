{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Internal.Eigen.Symmetric
  ( CertifiedSymmetricEigenResult (..),
    SymmetricEigenCertificationFailure (..),
    SymmetricEigenResult (..),
    certifySymmetricEigenResult,
    symmetricEigenPairsDenseUnchecked,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Primitive.PrimArray
  ( MutablePrimArray,
    readPrimArray,
    writePrimArray,
  )
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
  )
import Moonlight.LinAlg.Internal.Eigen.DenseWork
  ( MutableDenseWork (..),
    newDenseWork,
  )
import Moonlight.LinAlg.Internal.Eigen.Householder
  ( backtransformLower,
    tridiagonalizeLower,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels (forIndex)
import Moonlight.LinAlg.Internal.Eigen.Residual
  ( ResidualReport,
    residualReportPassesSymmetricEigenLimits,
    symmetricEigenResidual,
  )
import Moonlight.LinAlg.Internal.Eigen.Tridiagonal
  ( canonicalizeEigenvectorSigns,
    newIdentityEigenvectors,
    orthonormalizeDegenerateClusters,
    solveTridiagonalEigenvectors,
    sortEigenpairsAscending,
  )
import Moonlight.LinAlg.Pure.Dense.Flat
  ( DenseDoubleMatrix,
    denseDoubleMatrixShape,
    denseDoubleMatrixToRowMajorVector,
    trustedDenseDoubleMatrixRowMajor,
  )
import Prelude

data SymmetricEigenResult = SymmetricEigenResult
  { symmetricEigenResultValues :: !(S.Vector Double),
    symmetricEigenResultVectors :: !DenseDoubleMatrix
  }
  deriving stock (Eq, Show)

data CertifiedSymmetricEigenResult = CertifiedSymmetricEigenResult
  { certifiedSymmetricEigenResult :: !SymmetricEigenResult,
    certifiedSymmetricEigenResidualReport :: !ResidualReport
  }
  deriving stock (Eq, Show)

data SymmetricEigenCertificationFailure
  = SymmetricEigenCertificationShapeMismatch !String
  | SymmetricEigenCertificationResidualExceeded !ResidualReport
  deriving stock (Eq, Show)

symmetricEigenPairsDenseUnchecked :: Int -> DenseDoubleMatrix -> Either MoonlightError SymmetricEigenResult
symmetricEigenPairsDenseUnchecked !matrixSize matrixValue
  | matrixSize /= rowCount || matrixSize /= columnCount =
      Left
        ( InvariantViolation
            ( "symmetric eigen dense input shape mismatch: requested "
                <> show matrixSize
                <> " but received "
                <> show (rowCount, columnCount)
            )
        )
  | matrixSize < 0 =
      Left (InvariantViolation "symmetric eigen dense input dimension must be non-negative")
  | matrixSize == 0 =
      Right
        SymmetricEigenResult
          { symmetricEigenResultValues = S.empty,
            symmetricEigenResultVectors = trustedDenseDoubleMatrixRowMajor 0 0 S.empty
          }
  | matrixSize == 1 =
      Right
        SymmetricEigenResult
          { symmetricEigenResultValues = S.singleton (denseDoubleMatrixToRowMajorVector matrixValue `S.unsafeIndex` 0),
            symmetricEigenResultVectors = trustedDenseDoubleMatrixRowMajor 1 1 (S.singleton 1.0)
          }
  | otherwise = do
      entryCount <-
        first
          (const (InvariantViolation "symmetric eigen workspace cardinality exceeds Int range"))
          (checkedNonNegativeProduct matrixSize matrixSize)
      runST (symmetricEigenPairsST matrixSize entryCount matrixValue)
  where
    !(rowCount, columnCount) = denseDoubleMatrixShape matrixValue

certifySymmetricEigenResult ::
  DenseDoubleMatrix ->
  SymmetricEigenResult ->
  Either SymmetricEigenCertificationFailure CertifiedSymmetricEigenResult
certifySymmetricEigenResult matrixValue resultValue@SymmetricEigenResult {symmetricEigenResultValues, symmetricEigenResultVectors}
  | matrixRows /= matrixColumns =
      Left (SymmetricEigenCertificationShapeMismatch ("symmetric eigen certification requires square source matrix, received " <> show (matrixRows, matrixColumns)))
  | S.length symmetricEigenResultValues /= matrixRows =
      Left (SymmetricEigenCertificationShapeMismatch ("symmetric eigen certification eigenvalue count mismatch: expected " <> show matrixRows <> " but received " <> show (S.length symmetricEigenResultValues)))
  | vectorRows /= matrixRows || vectorColumns /= matrixRows =
      Left (SymmetricEigenCertificationShapeMismatch ("symmetric eigen certification eigenvector shape mismatch: expected " <> show (matrixRows, matrixRows) <> " but received " <> show (vectorRows, vectorColumns)))
  | residualReportPassesSymmetricEigenLimits reportValue =
      Right
        CertifiedSymmetricEigenResult
          { certifiedSymmetricEigenResult = resultValue,
            certifiedSymmetricEigenResidualReport = reportValue
          }
  | otherwise =
      Left (SymmetricEigenCertificationResidualExceeded reportValue)
  where
    !(matrixRows, matrixColumns) = denseDoubleMatrixShape matrixValue
    !(vectorRows, vectorColumns) = denseDoubleMatrixShape symmetricEigenResultVectors
    !reportValue = symmetricEigenResidual matrixValue symmetricEigenResultValues symmetricEigenResultVectors

symmetricEigenPairsST :: Int -> Int -> DenseDoubleMatrix -> ST s (Either MoonlightError SymmetricEigenResult)
symmetricEigenPairsST !matrixSize !entryCount matrixValue = do
  work <- newDenseWork matrixSize matrixSize
  copyLowerFlatToWork matrixSize matrixValue work
  (diagonalValues, offDiagonalValues, reflectorScalars) <- tridiagonalizeLower work
  eigenvectors <- newIdentityEigenvectors matrixSize
  solveResult <- solveTridiagonalEigenvectors matrixSize diagonalValues offDiagonalValues eigenvectors
  case solveResult of
    Left err -> pure (Left err)
    Right () -> do
      sortEigenpairsAscending matrixSize diagonalValues eigenvectors
      backtransformLower work reflectorScalars eigenvectors
      clusterResult <- orthonormalizeDegenerateClusters matrixSize diagonalValues eigenvectors
      case clusterResult of
        Left err -> pure (Left err)
        Right () -> do
          canonicalizeEigenvectorSigns matrixSize eigenvectors
          Right <$> eigenResultFromMutable matrixSize entryCount diagonalValues eigenvectors

copyLowerFlatToWork :: Int -> DenseDoubleMatrix -> MutableDenseWork s -> ST s ()
copyLowerFlatToWork !matrixSize matrixValue (MutableDenseWork rowCount _ workPayload) =
  forIndex 0 matrixSize $ \rowIndex ->
    forIndex 0 (rowIndex + 1) $ \columnIndex ->
      writePrimArray
        workPayload
        (rowIndex + columnIndex * rowCount)
        (payload `S.unsafeIndex` (rowIndex * matrixSize + columnIndex))
  where
    payload = denseDoubleMatrixToRowMajorVector matrixValue
{-# INLINE copyLowerFlatToWork #-}

eigenResultFromMutable ::
  Int ->
  Int ->
  MutablePrimArray s Double ->
  MutableDenseWork s ->
  ST s SymmetricEigenResult
eigenResultFromMutable !matrixSize !entryCount diagonalValues (MutableDenseWork rowCount _ eigenvectorPayload) = do
  eigenvalueBuffer <- SM.new matrixSize
  eigenvectorBuffer <- SM.new entryCount
  forIndex 0 matrixSize $ \columnIndex -> do
    eigenvalue <- readPrimArray diagonalValues columnIndex
    SM.unsafeWrite eigenvalueBuffer columnIndex eigenvalue
    forIndex 0 matrixSize $ \rowIndex -> do
      entryValue <- readPrimArray eigenvectorPayload (rowIndex + columnIndex * rowCount)
      SM.unsafeWrite eigenvectorBuffer (rowIndex * matrixSize + columnIndex) entryValue
  eigenvalues <- S.unsafeFreeze eigenvalueBuffer
  eigenvectorValues <- S.unsafeFreeze eigenvectorBuffer
  pure
    SymmetricEigenResult
      { symmetricEigenResultValues = eigenvalues,
        symmetricEigenResultVectors =
          trustedDenseDoubleMatrixRowMajor
            matrixSize
            matrixSize
            eigenvectorValues
      }

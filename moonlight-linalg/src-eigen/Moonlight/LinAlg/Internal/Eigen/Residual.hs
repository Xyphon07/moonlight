{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Internal.Eigen.Residual
  ( ResidualReport (..),
    residualReportPassesSymmetricEigenLimits,
    symmetricEigenResidual,
  )
where

import Data.Vector.Storable qualified as S
import Moonlight.LinAlg.Internal.Eigen.Kernels (epsDouble)
import Moonlight.LinAlg.Pure.Dense.Flat
  ( DenseDoubleMatrix,
    denseDoubleMatrixShape,
    denseDoubleMatrixToRowMajorVector,
  )
import Prelude

data ResidualReport = ResidualReport
  { residualMatrixNorm :: !Double,
    residualFrobenius :: !Double,
    residualOrthogonality :: !Double,
    residualScaled :: !Double,
    residualOrthogonalityScaled :: !Double
  }
  deriving stock (Eq, Show)

residualReportPassesSymmetricEigenLimits :: ResidualReport -> Bool
residualReportPassesSymmetricEigenLimits report =
  residualScaled report <= 1.0e7
    && residualOrthogonalityScaled report <= 1.0e7

symmetricEigenResidual :: DenseDoubleMatrix -> S.Vector Double -> DenseDoubleMatrix -> ResidualReport
symmetricEigenResidual matrixValue eigenvalues eigenvectors =
  let !(matrixSize, _) = denseDoubleMatrixShape matrixValue
      !matrixNorm = frobeniusNorm matrixValue
      !residualNorm = residualFrobeniusNorm matrixValue eigenvalues eigenvectors
      !orthogonalityNorm = orthogonalityFrobeniusNorm eigenvectors
      !dimensionScale = max 1.0 (fromIntegral matrixSize)
      !residualDenominator = max 1.0 matrixNorm * dimensionScale * epsDouble
      !orthogonalityDenominator = dimensionScale * epsDouble
   in ResidualReport
        { residualMatrixNorm = matrixNorm,
          residualFrobenius = residualNorm,
          residualOrthogonality = orthogonalityNorm,
          residualScaled = residualNorm / residualDenominator,
          residualOrthogonalityScaled = orthogonalityNorm / orthogonalityDenominator
        }

frobeniusNorm :: DenseDoubleMatrix -> Double
frobeniusNorm matrixValue =
  S.foldl' accumulateScaledNorm scaledNormZero (denseDoubleMatrixToRowMajorVector matrixValue)
    |> scaledNormValue

residualFrobeniusNorm :: DenseDoubleMatrix -> S.Vector Double -> DenseDoubleMatrix -> Double
residualFrobeniusNorm matrixValue eigenvalues eigenvectors =
  residualColumn 0 scaledNormZero |> scaledNormValue
  where
    !(matrixSize, _) = denseDoubleMatrixShape matrixValue
    matrixPayload = denseDoubleMatrixToRowMajorVector matrixValue
    eigenvectorPayload = denseDoubleMatrixToRowMajorVector eigenvectors

    residualColumn !columnIndex !normState
      | columnIndex >= matrixSize = normState
      | otherwise =
          residualRow columnIndex 0 normState
            |> residualColumn (columnIndex + 1)

    residualRow !columnIndex !rowIndex !normState
      | rowIndex >= matrixSize = normState
      | otherwise =
          let !lambdaValue = eigenvalues `S.unsafeIndex` columnIndex
              !vectorEntry = eigenvectorAt rowIndex columnIndex
              !residualEntry = matrixVectorEntry rowIndex columnIndex - lambdaValue * vectorEntry
           in residualRow columnIndex (rowIndex + 1) (accumulateScaledNorm normState residualEntry)

    matrixVectorEntry !rowIndex !columnIndex =
      dotAt 0 0.0
      where
        !rowOffset = rowIndex * matrixSize

        dotAt !entryIndex !accumulator
          | entryIndex >= matrixSize = accumulator
          | otherwise =
              let !matrixEntry = matrixPayload `S.unsafeIndex` (rowOffset + entryIndex)
                  !vectorEntry = eigenvectorAt entryIndex columnIndex
               in dotAt (entryIndex + 1) (accumulator + matrixEntry * vectorEntry)

    eigenvectorAt !rowIndex !columnIndex =
      eigenvectorPayload `S.unsafeIndex` (rowIndex * matrixSize + columnIndex)

orthogonalityFrobeniusNorm :: DenseDoubleMatrix -> Double
orthogonalityFrobeniusNorm eigenvectors =
  orthogonalityColumn 0 scaledNormZero |> scaledNormValue
  where
    !(matrixSize, _) = denseDoubleMatrixShape eigenvectors
    eigenvectorPayload = denseDoubleMatrixToRowMajorVector eigenvectors

    orthogonalityColumn !leftColumn !normState
      | leftColumn >= matrixSize = normState
      | otherwise =
          orthogonalityPair leftColumn leftColumn normState
            |> orthogonalityColumn (leftColumn + 1)

    orthogonalityPair !leftColumn !rightColumn !normState
      | rightColumn >= matrixSize = normState
      | otherwise =
          let !targetValue = if leftColumn == rightColumn then 1.0 else 0.0
              !weightValue = if leftColumn == rightColumn then 1.0 else sqrt 2.0
              !entryValue = weightValue * (columnDot leftColumn rightColumn - targetValue)
           in orthogonalityPair leftColumn (rightColumn + 1) (accumulateScaledNorm normState entryValue)

    columnDot !leftColumn !rightColumn = go 0 0.0
      where
        go !rowIndex !accumulator
          | rowIndex >= matrixSize = accumulator
          | otherwise =
              let !leftValue = eigenvectorAt rowIndex leftColumn
                  !rightValue = eigenvectorAt rowIndex rightColumn
               in go (rowIndex + 1) (accumulator + leftValue * rightValue)

    eigenvectorAt !rowIndex !columnIndex =
      eigenvectorPayload `S.unsafeIndex` (rowIndex * matrixSize + columnIndex)

data ScaledNorm = ScaledNorm !Double !Double

scaledNormZero :: ScaledNorm
scaledNormZero = ScaledNorm 0.0 1.0

accumulateScaledNorm :: ScaledNorm -> Double -> ScaledNorm
accumulateScaledNorm (ScaledNorm !scaleValue !scaledSum) !entryValue =
  let !entryAbs = abs entryValue
   in if entryAbs == 0.0
        then ScaledNorm scaleValue scaledSum
        else
          if scaleValue < entryAbs
            then
              let !scaledRatio = scaleValue / entryAbs
               in ScaledNorm entryAbs (1.0 + scaledSum * scaledRatio * scaledRatio)
            else
              let !scaledRatio = entryAbs / scaleValue
               in ScaledNorm scaleValue (scaledSum + scaledRatio * scaledRatio)
{-# INLINE accumulateScaledNorm #-}

scaledNormValue :: ScaledNorm -> Double
scaledNormValue (ScaledNorm !scaleValue !scaledSum) =
  if scaleValue == 0.0
    then 0.0
    else scaleValue * sqrt scaledSum
{-# INLINE scaledNormValue #-}

(|>) :: a -> (a -> b) -> b
(|>) value function = function value
{-# INLINE (|>) #-}

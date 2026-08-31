{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    newMutableDoubleVector,
    thawMutableDoubleVector,
    freezeMutableDoubleVector,
    copyImmutableIntoMutable,
    copyImmutableSquaredNormIntoMutable,
    copyMutableVector,
    dotMutableVector,
    normMutableVector,
    addScaledMutableVector,
    scaleMutableVector,
    scaledCopyMutableVector,
    addScaledPairIntoMutable,
    subtractMutableInto,
    csrMatVecIntoMutable,
    csrMatVecDotIntoMutable,
    residualIntoMutable,
    csrResidualSquaredIntoMutable,
    updateSolutionAndResidualSquaredMutable,
    updateDirectionMutable,
    initializeZeroJacobiMutable,
    divideByDiagonalDotAndCopyMutable,
    updateSolutionResidualJacobiMutable,
    divideByDiagonalAndDotIntoMutable,
    divideByDiagonalIntoMutable,
    multiplyByDiagonalIntoMutable,
    lowerTriangularSolveIntoMutable,
    upperTriangularSolveIntoMutable,
  )
where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Data.Primitive.ByteArray
  ( ByteArray,
    MutableByteArray,
    indexByteArray,
    readByteArray,
    writeByteArray,
  )
import Data.Vector.Primitive qualified as P
import Data.Vector.Primitive.Mutable qualified as PM
import Data.Vector.Unboxed.Base qualified as UB
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Moonlight.LinAlg.Pure.Sparse.Types
  ( CSRExecutionPlan (..),
    SparseCSR,
    csrColumnIndicesVector,
    csrRows,
    csrExecutionPlan,
    csrRowOffsetsVector,
    csrValuesVector,
  )
import Prelude

type MutableDoubleVector :: Type -> Type
type MutableDoubleVector s = MU.MVector s Double

newMutableDoubleVector :: Int -> ST s (MutableDoubleVector s)
newMutableDoubleVector !dimension =
  MU.replicate dimension 0.0
{-# INLINE newMutableDoubleVector #-}

thawMutableDoubleVector :: U.Vector Double -> ST s (MutableDoubleVector s)
thawMutableDoubleVector = U.thaw
{-# INLINE thawMutableDoubleVector #-}

freezeMutableDoubleVector :: MutableDoubleVector s -> ST s (U.Vector Double)
freezeMutableDoubleVector = U.freeze
{-# INLINE freezeMutableDoubleVector #-}

copyImmutableIntoMutable ::
  U.Vector Double ->
  MutableDoubleVector s ->
  ST s ()
copyImmutableIntoMutable sourceVector targetVector =
  go 0
  where
    !dimension = U.length sourceVector

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          MU.unsafeWrite
            targetVector
            indexValue
            (sourceVector `U.unsafeIndex` indexValue)
          go (indexValue + 1)
{-# INLINE copyImmutableIntoMutable #-}

copyImmutableSquaredNormIntoMutable ::
  U.Vector Double ->
  MutableDoubleVector s ->
  ST s Double
copyImmutableSquaredNormIntoMutable sourceVector targetVector =
  go 0 0.0
  where
    !dimension = U.length sourceVector

    go !indexValue !sumSquares
      | indexValue >= dimension = pure sumSquares
      | otherwise = do
          let !entryValue = sourceVector `U.unsafeIndex` indexValue
          MU.unsafeWrite targetVector indexValue entryValue
          go
            (indexValue + 1)
            (sumSquares + entryValue * entryValue)
{-# INLINE copyImmutableSquaredNormIntoMutable #-}

copyMutableVector ::
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
copyMutableVector sourceVector targetVector =
  MU.unsafeCopy targetVector sourceVector
{-# INLINE copyMutableVector #-}

dotMutableVector ::
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
dotMutableVector leftVector rightVector =
  go 0 0.0
  where
    !dimension = MU.length leftVector

    go !indexValue !accumulator
      | indexValue >= dimension = pure accumulator
      | otherwise = do
          leftValue <- MU.unsafeRead leftVector indexValue
          rightValue <- MU.unsafeRead rightVector indexValue
          go
            (indexValue + 1)
            (accumulator + leftValue * rightValue)
{-# INLINE dotMutableVector #-}

normMutableVector :: MutableDoubleVector s -> ST s Double
normMutableVector vectorValue =
  sqrt <$> dotMutableVector vectorValue vectorValue
{-# INLINE normMutableVector #-}

addScaledMutableVector ::
  Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
addScaledMutableVector !scaleValue sourceVector targetVector =
  go 0
  where
    !dimension = MU.length sourceVector

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          sourceValue <- MU.unsafeRead sourceVector indexValue
          targetValue <- MU.unsafeRead targetVector indexValue
          MU.unsafeWrite
            targetVector
            indexValue
            (targetValue + scaleValue * sourceValue)
          go (indexValue + 1)
{-# INLINE addScaledMutableVector #-}

scaleMutableVector ::
  Double ->
  MutableDoubleVector s ->
  ST s ()
scaleMutableVector !scaleValue targetVector =
  go 0
  where
    !dimension = MU.length targetVector

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          targetValue <- MU.unsafeRead targetVector indexValue
          MU.unsafeWrite
            targetVector
            indexValue
            (scaleValue * targetValue)
          go (indexValue + 1)
{-# INLINE scaleMutableVector #-}

scaledCopyMutableVector ::
  Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
scaledCopyMutableVector !scaleValue sourceVector targetVector =
  go 0
  where
    !dimension = MU.length sourceVector

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          sourceValue <- MU.unsafeRead sourceVector indexValue
          MU.unsafeWrite
            targetVector
            indexValue
            (scaleValue * sourceValue)
          go (indexValue + 1)
{-# INLINE scaledCopyMutableVector #-}

addScaledPairIntoMutable ::
  Double ->
  MutableDoubleVector s ->
  Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
addScaledPairIntoMutable
  !leftScale
  leftVector
  !rightScale
  rightVector
  targetVector =
    go 0
  where
    !dimension = MU.length targetVector

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          leftValue <- MU.unsafeRead leftVector indexValue
          rightValue <- MU.unsafeRead rightVector indexValue
          MU.unsafeWrite
            targetVector
            indexValue
            (leftScale * leftValue + rightScale * rightValue)
          go (indexValue + 1)
{-# INLINE addScaledPairIntoMutable #-}

subtractMutableInto ::
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
subtractMutableInto leftVector rightVector targetVector =
  addScaledPairIntoMutable
    1.0
    leftVector
    (-1.0)
    rightVector
    targetVector
{-# INLINE subtractMutableInto #-}

csrMatVecIntoMutable ::
  SparseCSR Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
csrMatVecIntoMutable sparseMatrix inputVector targetVector =
  case csrExecutionPlan sparseMatrix of
    CSRGeneral ->
      csrMatVecGeneralIntoMutable
        sparseMatrix
        inputVector
        targetVector
    CSRContiguousBand 2 2
      | csrRows sparseMatrix >= 5 ->
          pentadiagonalMatVecIntoMutable
            (csrRows sparseMatrix)
            (csrValuesVector sparseMatrix)
            inputVector
            targetVector
    CSRContiguousBand lowerBandwidth upperBandwidth ->
      contiguousBandMatVecIntoMutable
        (csrRows sparseMatrix)
        lowerBandwidth
        upperBandwidth
        (csrValuesVector sparseMatrix)
        inputVector
        targetVector
{-# INLINE csrMatVecIntoMutable #-}

csrMatVecGeneralIntoMutable ::
  SparseCSR Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
csrMatVecGeneralIntoMutable sparseMatrix inputVector targetVector =
  writeRows 0
  where
    !rowCount = csrRows sparseMatrix
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !coefficients = csrValuesVector sparseMatrix

    writeRows !rowIndex
      | rowIndex >= rowCount = pure ()
      | otherwise = do
          rowValue <-
            csrRowDotMutable
              rowOffsets
              columnIndices
              coefficients
              inputVector
              rowIndex
          MU.unsafeWrite targetVector rowIndex rowValue
          writeRows (rowIndex + 1)
{-# INLINE csrMatVecGeneralIntoMutable #-}

pentadiagonalMatVecIntoMutable ::
  Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
pentadiagonalMatVecIntoMutable
  rowCount
  (UB.V_Double (P.Vector coefficientBase _ coefficientArray))
  (UB.MV_Double (PM.MVector inputBase _ inputArray))
  (UB.MV_Double (PM.MVector targetBase _ targetArray)) =
    go 0
  where
    go !rowIndex
      | rowIndex >= rowCount = pure ()
      | otherwise = do
          rowValue <-
            pentadiagonalRowDotMutable
              rowCount
              coefficientBase
              coefficientArray
              inputBase
              inputArray
              rowIndex
          writeByteArray
            targetArray
            (targetBase + rowIndex)
            rowValue
          go (rowIndex + 1)
{-# INLINE pentadiagonalMatVecIntoMutable #-}

pentadiagonalMatVecDotIntoMutable ::
  Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
pentadiagonalMatVecDotIntoMutable
  rowCount
  (UB.V_Double (P.Vector coefficientBase _ coefficientArray))
  (UB.MV_Double (PM.MVector inputBase _ inputArray))
  (UB.MV_Double (PM.MVector targetBase _ targetArray)) =
    go 0 0.0
  where
    go !rowIndex !dotAccumulator
      | rowIndex >= rowCount = pure dotAccumulator
      | otherwise = do
          rowValue <-
            pentadiagonalRowDotMutable
              rowCount
              coefficientBase
              coefficientArray
              inputBase
              inputArray
              rowIndex
          inputValue <-
            readByteArray inputArray (inputBase + rowIndex)
          writeByteArray
            targetArray
            (targetBase + rowIndex)
            rowValue
          go
            (rowIndex + 1)
            (dotAccumulator + (inputValue :: Double) * rowValue)
{-# INLINE pentadiagonalMatVecDotIntoMutable #-}

pentadiagonalResidualSquaredIntoMutable ::
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
pentadiagonalResidualSquaredIntoMutable
  rowCount
  (UB.V_Double (P.Vector coefficientBase _ coefficientArray))
  (UB.V_Double (P.Vector rhsBase _ rhsArray))
  (UB.MV_Double (PM.MVector guessBase _ guessArray))
  (UB.MV_Double (PM.MVector residualBase _ residualArray)) =
    go 0 0.0
  where
    go !rowIndex !sumSquares
      | rowIndex >= rowCount = pure sumSquares
      | otherwise = do
          imageValue <-
            pentadiagonalRowDotMutable
              rowCount
              coefficientBase
              coefficientArray
              guessBase
              guessArray
              rowIndex
          let !rhsValue =
                ( indexByteArray
                    rhsArray
                    (rhsBase + rowIndex)
                    :: Double
                )
              !residualValue = rhsValue - imageValue
          writeByteArray
            residualArray
            (residualBase + rowIndex)
            residualValue
          go
            (rowIndex + 1)
            (sumSquares + residualValue * residualValue)
{-# INLINE pentadiagonalResidualSquaredIntoMutable #-}

pentadiagonalRowDotMutable ::
  Int ->
  Int ->
  ByteArray ->
  Int ->
  MutableByteArray s ->
  Int ->
  ST s Double
pentadiagonalRowDotMutable
  rowCount
  coefficientBase
  coefficientArray
  inputBase
  inputArray
  rowIndex
    | rowIndex == 0 = do
        input0 <- readInput 0
        input1 <- readInput 1
        input2 <- readInput 2
        pure
          ( coefficientAt 0 * input0
              + coefficientAt 1 * input1
              + coefficientAt 2 * input2
          )
    | rowIndex == 1 = do
        input0 <- readInput 0
        input1 <- readInput 1
        input2 <- readInput 2
        input3 <- readInput 3
        pure
          ( coefficientAt 3 * input0
              + coefficientAt 4 * input1
              + coefficientAt 5 * input2
              + coefficientAt 6 * input3
          )
    | rowIndex + 2 < rowCount = do
        let !entryIndex = 5 * rowIndex - 3
        input0 <- readInput (rowIndex - 2)
        input1 <- readInput (rowIndex - 1)
        input2 <- readInput rowIndex
        input3 <- readInput (rowIndex + 1)
        input4 <- readInput (rowIndex + 2)
        pure
          ( coefficientAt entryIndex * input0
              + coefficientAt (entryIndex + 1) * input1
              + coefficientAt (entryIndex + 2) * input2
              + coefficientAt (entryIndex + 3) * input3
              + coefficientAt (entryIndex + 4) * input4
          )
    | rowIndex + 1 < rowCount = do
        let !entryIndex = 5 * rowCount - 13
        input0 <- readInput (rowCount - 4)
        input1 <- readInput (rowCount - 3)
        input2 <- readInput (rowCount - 2)
        input3 <- readInput (rowCount - 1)
        pure
          ( coefficientAt entryIndex * input0
              + coefficientAt (entryIndex + 1) * input1
              + coefficientAt (entryIndex + 2) * input2
              + coefficientAt (entryIndex + 3) * input3
          )
    | otherwise = do
        let !entryIndex = 5 * rowCount - 9
        input0 <- readInput (rowCount - 3)
        input1 <- readInput (rowCount - 2)
        input2 <- readInput (rowCount - 1)
        pure
          ( coefficientAt entryIndex * input0
              + coefficientAt (entryIndex + 1) * input1
              + coefficientAt (entryIndex + 2) * input2
          )
  where
    coefficientAt !entryIndex =
      ( indexByteArray
          coefficientArray
          (coefficientBase + entryIndex)
          :: Double
      )
    readInput !columnIndex =
      readByteArray inputArray (inputBase + columnIndex)
{-# INLINE pentadiagonalRowDotMutable #-}

contiguousBandMatVecIntoMutable ::
  Int ->
  Int ->
  Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
contiguousBandMatVecIntoMutable
  rowCount
  lowerBandwidth
  upperBandwidth
  coefficients
  inputVector
  targetVector =
    writeRows 0 0
  where
    writeRows !rowIndex !entryIndex
      | rowIndex >= rowCount = pure ()
      | otherwise = do
          let !firstColumn = max 0 (rowIndex - lowerBandwidth)
              !lastColumn = min (rowCount - 1) (rowIndex + upperBandwidth)
              !entryCount = lastColumn - firstColumn + 1
          rowValue <-
            accumulateBand
              entryIndex
              firstColumn
              entryCount
              0.0
          MU.unsafeWrite targetVector rowIndex rowValue
          writeRows (rowIndex + 1) (entryIndex + entryCount)

    accumulateBand !entryIndex !columnIndex !remaining !accumulator
      | remaining <= 0 = pure accumulator
      | otherwise = do
          inputValue <- MU.unsafeRead inputVector columnIndex
          let !coefficient = coefficients `U.unsafeIndex` entryIndex
          accumulateBand
            (entryIndex + 1)
            (columnIndex + 1)
            (remaining - 1)
            (accumulator + coefficient * inputValue)
{-# INLINE contiguousBandMatVecIntoMutable #-}

csrMatVecDotIntoMutable ::
  SparseCSR Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
csrMatVecDotIntoMutable sparseMatrix inputVector targetVector =
  case csrExecutionPlan sparseMatrix of
    CSRGeneral ->
      csrMatVecDotGeneralIntoMutable
        sparseMatrix
        inputVector
        targetVector
    CSRContiguousBand 2 2
      | csrRows sparseMatrix >= 5 ->
          pentadiagonalMatVecDotIntoMutable
            (csrRows sparseMatrix)
            (csrValuesVector sparseMatrix)
            inputVector
            targetVector
    CSRContiguousBand lowerBandwidth upperBandwidth ->
      contiguousBandMatVecDotIntoMutable
        (csrRows sparseMatrix)
        lowerBandwidth
        upperBandwidth
        (csrValuesVector sparseMatrix)
        inputVector
        targetVector
{-# INLINE csrMatVecDotIntoMutable #-}

csrMatVecDotGeneralIntoMutable ::
  SparseCSR Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
csrMatVecDotGeneralIntoMutable sparseMatrix inputVector targetVector =
  writeRows 0 0.0
  where
    !rowCount = csrRows sparseMatrix
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !coefficients = csrValuesVector sparseMatrix

    writeRows !rowIndex !dotAccumulator
      | rowIndex >= rowCount = pure dotAccumulator
      | otherwise = do
          rowValue <-
            csrRowDotMutable
              rowOffsets
              columnIndices
              coefficients
              inputVector
              rowIndex
          inputValue <- MU.unsafeRead inputVector rowIndex
          MU.unsafeWrite targetVector rowIndex rowValue
          writeRows
            (rowIndex + 1)
            (dotAccumulator + inputValue * rowValue)
{-# INLINE csrMatVecDotGeneralIntoMutable #-}

contiguousBandMatVecDotIntoMutable ::
  Int ->
  Int ->
  Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
contiguousBandMatVecDotIntoMutable
  rowCount
  lowerBandwidth
  upperBandwidth
  coefficients
  inputVector
  targetVector =
    writeRows 0 0 0.0
  where
    writeRows !rowIndex !entryIndex !dotAccumulator
      | rowIndex >= rowCount = pure dotAccumulator
      | otherwise = do
          let !firstColumn = max 0 (rowIndex - lowerBandwidth)
              !lastColumn = min (rowCount - 1) (rowIndex + upperBandwidth)
              !entryCount = lastColumn - firstColumn + 1
          rowValue <-
            accumulateBand
              entryIndex
              firstColumn
              entryCount
              0.0
          inputValue <- MU.unsafeRead inputVector rowIndex
          MU.unsafeWrite targetVector rowIndex rowValue
          writeRows
            (rowIndex + 1)
            (entryIndex + entryCount)
            (dotAccumulator + inputValue * rowValue)

    accumulateBand !entryIndex !columnIndex !remaining !accumulator
      | remaining <= 0 = pure accumulator
      | otherwise = do
          inputValue <- MU.unsafeRead inputVector columnIndex
          let !coefficient = coefficients `U.unsafeIndex` entryIndex
          accumulateBand
            (entryIndex + 1)
            (columnIndex + 1)
            (remaining - 1)
            (accumulator + coefficient * inputValue)
{-# INLINE contiguousBandMatVecDotIntoMutable #-}

residualIntoMutable ::
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
residualIntoMutable
  sparseMatrix
  rhsValues
  guessVector
  imageVector
  residualVector = do
    csrMatVecIntoMutable sparseMatrix guessVector imageVector
    writeResidual 0
  where
    !dimension = U.length rhsValues

    writeResidual !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          imageValue <- MU.unsafeRead imageVector indexValue
          MU.unsafeWrite
            residualVector
            indexValue
            (rhsValues `U.unsafeIndex` indexValue - imageValue)
          writeResidual (indexValue + 1)
{-# INLINE residualIntoMutable #-}

csrResidualSquaredIntoMutable ::
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
csrResidualSquaredIntoMutable
  sparseMatrix
  rhsValues
  guessVector
  residualVector =
    case csrExecutionPlan sparseMatrix of
      CSRGeneral ->
        csrResidualSquaredGeneralIntoMutable
          sparseMatrix
          rhsValues
          guessVector
          residualVector
      CSRContiguousBand 2 2
        | csrRows sparseMatrix >= 5 ->
            pentadiagonalResidualSquaredIntoMutable
              (csrRows sparseMatrix)
              (csrValuesVector sparseMatrix)
              rhsValues
              guessVector
              residualVector
      CSRContiguousBand lowerBandwidth upperBandwidth ->
        contiguousBandResidualSquaredIntoMutable
          (csrRows sparseMatrix)
          lowerBandwidth
          upperBandwidth
          (csrValuesVector sparseMatrix)
          rhsValues
          guessVector
          residualVector
{-# INLINE csrResidualSquaredIntoMutable #-}

csrResidualSquaredGeneralIntoMutable ::
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
csrResidualSquaredGeneralIntoMutable
  sparseMatrix
  rhsValues
  guessVector
  residualVector =
    writeRows 0 0.0
  where
    !rowCount = csrRows sparseMatrix
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !coefficients = csrValuesVector sparseMatrix

    writeRows !rowIndex !sumSquares
      | rowIndex >= rowCount = pure sumSquares
      | otherwise = do
          imageValue <-
            csrRowDotMutable
              rowOffsets
              columnIndices
              coefficients
              guessVector
              rowIndex
          let !residualValue =
                rhsValues `U.unsafeIndex` rowIndex - imageValue
          MU.unsafeWrite residualVector rowIndex residualValue
          writeRows
            (rowIndex + 1)
            (sumSquares + residualValue * residualValue)
{-# INLINE csrResidualSquaredGeneralIntoMutable #-}

contiguousBandResidualSquaredIntoMutable ::
  Int ->
  Int ->
  Int ->
  U.Vector Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
contiguousBandResidualSquaredIntoMutable
  rowCount
  lowerBandwidth
  upperBandwidth
  coefficients
  rhsValues
  guessVector
  residualVector =
    writeRows 0 0 0.0
  where
    writeRows !rowIndex !entryIndex !sumSquares
      | rowIndex >= rowCount = pure sumSquares
      | otherwise = do
          let !firstColumn = max 0 (rowIndex - lowerBandwidth)
              !lastColumn = min (rowCount - 1) (rowIndex + upperBandwidth)
              !entryCount = lastColumn - firstColumn + 1
          imageValue <-
            accumulateBand
              entryIndex
              firstColumn
              entryCount
              0.0
          let !residualValue =
                rhsValues `U.unsafeIndex` rowIndex - imageValue
          MU.unsafeWrite residualVector rowIndex residualValue
          writeRows
            (rowIndex + 1)
            (entryIndex + entryCount)
            (sumSquares + residualValue * residualValue)

    accumulateBand !entryIndex !columnIndex !remaining !accumulator
      | remaining <= 0 = pure accumulator
      | otherwise = do
          guessValue <- MU.unsafeRead guessVector columnIndex
          let !coefficient = coefficients `U.unsafeIndex` entryIndex
          accumulateBand
            (entryIndex + 1)
            (columnIndex + 1)
            (remaining - 1)
            (accumulator + coefficient * guessValue)
{-# INLINE contiguousBandResidualSquaredIntoMutable #-}

updateSolutionAndResidualSquaredMutable ::
  Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
updateSolutionAndResidualSquaredMutable
  !alphaValue
  directionVector
  imageDirectionVector
  guessVector
  residualVector =
    go 0 0.0
  where
    !dimension = MU.length guessVector

    go !indexValue !sumSquares
      | indexValue >= dimension = pure sumSquares
      | otherwise = do
          directionValue <- MU.unsafeRead directionVector indexValue
          imageDirectionValue <-
            MU.unsafeRead imageDirectionVector indexValue
          guessValue <- MU.unsafeRead guessVector indexValue
          residualValue <- MU.unsafeRead residualVector indexValue
          let !nextGuessValue =
                guessValue + alphaValue * directionValue
              !nextResidualValue =
                residualValue - alphaValue * imageDirectionValue
          MU.unsafeWrite guessVector indexValue nextGuessValue
          MU.unsafeWrite residualVector indexValue nextResidualValue
          go
            (indexValue + 1)
            (sumSquares + nextResidualValue * nextResidualValue)
{-# INLINE updateSolutionAndResidualSquaredMutable #-}

updateDirectionMutable ::
  Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
updateDirectionMutable !betaValue sourceVector directionVector =
  go 0
  where
    !dimension = MU.length directionVector

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          sourceValue <- MU.unsafeRead sourceVector indexValue
          directionValue <- MU.unsafeRead directionVector indexValue
          MU.unsafeWrite
            directionVector
            indexValue
            (sourceValue + betaValue * directionValue)
          go (indexValue + 1)
{-# INLINE updateDirectionMutable #-}

initializeZeroJacobiMutable ::
  U.Vector Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s (Double, Double)
initializeZeroJacobiMutable
  rhsValues
  diagonalValues
  residualVector
  preconditionedResidualVector
  directionVector =
    go 0 0.0 0.0
  where
    !dimension = U.length rhsValues

    go !indexValue !residualSquared !rhoValue
      | indexValue >= dimension =
          pure (residualSquared, rhoValue)
      | otherwise = do
          let !residualValue = rhsValues `U.unsafeIndex` indexValue
              !preconditionedValue =
                residualValue
                  / (diagonalValues `U.unsafeIndex` indexValue)
          MU.unsafeWrite residualVector indexValue residualValue
          MU.unsafeWrite
            preconditionedResidualVector
            indexValue
            preconditionedValue
          MU.unsafeWrite
            directionVector
            indexValue
            preconditionedValue
          go
            (indexValue + 1)
            (residualSquared + residualValue * residualValue)
            (rhoValue + residualValue * preconditionedValue)
{-# INLINE initializeZeroJacobiMutable #-}

divideByDiagonalDotAndCopyMutable ::
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
divideByDiagonalDotAndCopyMutable
  diagonalValues
  sourceVector
  targetVector
  directionVector =
    go 0 0.0
  where
    !dimension = U.length diagonalValues

    go !indexValue !dotAccumulator
      | indexValue >= dimension = pure dotAccumulator
      | otherwise = do
          sourceValue <- MU.unsafeRead sourceVector indexValue
          let !targetValue =
                sourceValue
                  / (diagonalValues `U.unsafeIndex` indexValue)
          MU.unsafeWrite targetVector indexValue targetValue
          MU.unsafeWrite directionVector indexValue targetValue
          go
            (indexValue + 1)
            (dotAccumulator + sourceValue * targetValue)
{-# INLINE divideByDiagonalDotAndCopyMutable #-}

updateSolutionResidualJacobiMutable ::
  Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s (Double, Double)
updateSolutionResidualJacobiMutable
  !alphaValue
  diagonalValues
  directionVector
  imageDirectionVector
  guessVector
  residualVector
  preconditionedResidualVector =
    go 0 0.0 0.0
  where
    !dimension = MU.length guessVector

    go !indexValue !residualSquared !rhoValue
      | indexValue >= dimension =
          pure (residualSquared, rhoValue)
      | otherwise = do
          directionValue <- MU.unsafeRead directionVector indexValue
          imageDirectionValue <-
            MU.unsafeRead imageDirectionVector indexValue
          guessValue <- MU.unsafeRead guessVector indexValue
          residualValue <- MU.unsafeRead residualVector indexValue
          let !nextGuessValue =
                guessValue + alphaValue * directionValue
              !nextResidualValue =
                residualValue - alphaValue * imageDirectionValue
              !preconditionedValue =
                nextResidualValue
                  / (diagonalValues `U.unsafeIndex` indexValue)
          MU.unsafeWrite guessVector indexValue nextGuessValue
          MU.unsafeWrite residualVector indexValue nextResidualValue
          MU.unsafeWrite
            preconditionedResidualVector
            indexValue
            preconditionedValue
          go
            (indexValue + 1)
            (residualSquared + nextResidualValue * nextResidualValue)
            (rhoValue + nextResidualValue * preconditionedValue)
{-# INLINE updateSolutionResidualJacobiMutable #-}

divideByDiagonalAndDotIntoMutable ::
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
divideByDiagonalAndDotIntoMutable
  diagonalValues
  sourceVector
  targetVector =
    go 0 0.0
  where
    !dimension = U.length diagonalValues

    go !indexValue !dotAccumulator
      | indexValue >= dimension = pure dotAccumulator
      | otherwise = do
          sourceValue <- MU.unsafeRead sourceVector indexValue
          let !targetValue =
                sourceValue
                  / (diagonalValues `U.unsafeIndex` indexValue)
          MU.unsafeWrite targetVector indexValue targetValue
          go
            (indexValue + 1)
            (dotAccumulator + sourceValue * targetValue)
{-# INLINE divideByDiagonalAndDotIntoMutable #-}

divideByDiagonalIntoMutable ::
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
divideByDiagonalIntoMutable diagonalValues sourceVector targetVector =
  go 0
  where
    !dimension = U.length diagonalValues

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          sourceValue <- MU.unsafeRead sourceVector indexValue
          MU.unsafeWrite
            targetVector
            indexValue
            (sourceValue / (diagonalValues `U.unsafeIndex` indexValue))
          go (indexValue + 1)
{-# INLINE divideByDiagonalIntoMutable #-}

multiplyByDiagonalIntoMutable ::
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
multiplyByDiagonalIntoMutable diagonalValues sourceVector targetVector =
  go 0
  where
    !dimension = U.length diagonalValues

    go !indexValue
      | indexValue >= dimension = pure ()
      | otherwise = do
          sourceValue <- MU.unsafeRead sourceVector indexValue
          MU.unsafeWrite
            targetVector
            indexValue
            (sourceValue * (diagonalValues `U.unsafeIndex` indexValue))
          go (indexValue + 1)
{-# INLINE multiplyByDiagonalIntoMutable #-}

lowerTriangularSolveIntoMutable ::
  U.Vector Double ->
  SparseCSR Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
lowerTriangularSolveIntoMutable
  diagonalValues
  sparseMatrix
  rhsVector
  targetVector =
    solveRows 0
  where
    !dimension = csrRows sparseMatrix
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !coefficients = csrValuesVector sparseMatrix

    solveRows !rowIndex
      | rowIndex >= dimension = pure ()
      | otherwise = do
          rhsValue <- MU.unsafeRead rhsVector rowIndex
          knownProduct <-
            lowerKnownProduct
              rowOffsets
              columnIndices
              coefficients
              targetVector
              rowIndex
          MU.unsafeWrite
            targetVector
            rowIndex
            ( (rhsValue - knownProduct)
                / (diagonalValues `U.unsafeIndex` rowIndex)
            )
          solveRows (rowIndex + 1)
{-# INLINE lowerTriangularSolveIntoMutable #-}

upperTriangularSolveIntoMutable ::
  U.Vector Double ->
  SparseCSR Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
upperTriangularSolveIntoMutable
  diagonalValues
  sparseMatrix
  rhsVector
  targetVector =
    solveRows (dimension - 1)
  where
    !dimension = csrRows sparseMatrix
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !coefficients = csrValuesVector sparseMatrix

    solveRows !rowIndex
      | rowIndex < 0 = pure ()
      | otherwise = do
          rhsValue <- MU.unsafeRead rhsVector rowIndex
          knownProduct <-
            upperKnownProduct
              rowOffsets
              columnIndices
              coefficients
              targetVector
              rowIndex
          MU.unsafeWrite
            targetVector
            rowIndex
            ( (rhsValue - knownProduct)
                / (diagonalValues `U.unsafeIndex` rowIndex)
            )
          solveRows (rowIndex - 1)
{-# INLINE upperTriangularSolveIntoMutable #-}

csrRowDotMutable ::
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  Int ->
  ST s Double
csrRowDotMutable
  rowOffsets
  columnIndices
  coefficients
  inputVector
  rowIndex =
    go startIndex 0.0
  where
    !startIndex = rowOffsets `U.unsafeIndex` rowIndex
    !stopIndex = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    go !entryIndex !accumulator
      | entryIndex >= stopIndex = pure accumulator
      | otherwise = do
          let !columnIndex =
                columnIndices `U.unsafeIndex` entryIndex
              !coefficientValue =
                coefficients `U.unsafeIndex` entryIndex
          inputValue <- MU.unsafeRead inputVector columnIndex
          go
            (entryIndex + 1)
            (accumulator + coefficientValue * inputValue)
{-# INLINE csrRowDotMutable #-}

lowerKnownProduct ::
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  Int ->
  ST s Double
lowerKnownProduct
  rowOffsets
  columnIndices
  coefficients
  solutionVector
  rowIndex =
    go startIndex 0.0
  where
    !startIndex = rowOffsets `U.unsafeIndex` rowIndex
    !stopIndex = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    go !entryIndex !accumulator
      | entryIndex >= stopIndex = pure accumulator
      | otherwise =
          let !columnIndex =
                columnIndices `U.unsafeIndex` entryIndex
              !coefficientValue =
                coefficients `U.unsafeIndex` entryIndex
           in if columnIndex < rowIndex
                then do
                  solutionValue <-
                    MU.unsafeRead solutionVector columnIndex
                  go
                    (entryIndex + 1)
                    (accumulator + coefficientValue * solutionValue)
                else go (entryIndex + 1) accumulator
{-# INLINE lowerKnownProduct #-}

upperKnownProduct ::
  U.Vector Int ->
  U.Vector Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  Int ->
  ST s Double
upperKnownProduct
  rowOffsets
  columnIndices
  coefficients
  solutionVector
  rowIndex =
    go startIndex 0.0
  where
    !startIndex = rowOffsets `U.unsafeIndex` rowIndex
    !stopIndex = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    go !entryIndex !accumulator
      | entryIndex >= stopIndex = pure accumulator
      | otherwise =
          let !columnIndex =
                columnIndices `U.unsafeIndex` entryIndex
              !coefficientValue =
                coefficients `U.unsafeIndex` entryIndex
           in if columnIndex > rowIndex
                then do
                  solutionValue <-
                    MU.unsafeRead solutionVector columnIndex
                  go
                    (entryIndex + 1)
                    (accumulator + coefficientValue * solutionValue)
                else go (entryIndex + 1) accumulator
{-# INLINE upperKnownProduct #-}

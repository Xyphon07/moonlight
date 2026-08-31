{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Internal.Dense.DoubleFactorization
  ( choleskyLower,
    qrFullColumnRank,
    solveSquareLinearSystem,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Primitive.PrimArray qualified as PrimArray
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
    fieldValueValid,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels (hypotStable)
import Moonlight.LinAlg.Internal.Primitives (epsilon)
import Prelude

solveSquareLinearSystem :: Int -> [Double] -> [Double] -> Either MoonlightError [Double]
solveSquareLinearSystem !matrixSize matrixValues rightHandSideValues = do
  matrixEntryCount <- requireSquarePayload "direct solve" matrixSize matrixValues
  requireVectorPayload "direct solve" matrixSize rightHandSideValues
  runST $ do
    matrixWork <- newDoubleArray matrixEntryCount
    rhsWork <- newDoubleArray matrixSize
    pivots <- newIntArray matrixSize
    copyDoubleList matrixWork matrixValues
    copyDoubleList rhsWork rightHandSideValues
    factorResult <- factorPLU matrixSize matrixWork pivots
    case factorResult of
      Left err -> pure (Left err)
      Right () -> do
        applyPivotVector matrixSize pivots rhsWork
        forwardSolveUnitLower matrixSize matrixWork rhsWork
        backResult <- backwardSolveUpper matrixSize matrixWork rhsWork
        case backResult of
          Left err -> pure (Left err)
          Right () -> Right <$> freezeDoubleList rhsWork

qrFullColumnRank :: Int -> Int -> [Double] -> Either MoonlightError ([Double], [Double])
qrFullColumnRank !rowCount !columnCount matrixValues
  | rowCount < columnCount =
      Left (InvariantViolation "full-column-rank QR requires row count greater than or equal to column count")
  | otherwise = do
      matrixEntryCount <- requireMatrixPayload "QR decomposition" rowCount columnCount matrixValues
      upperEntryCount <-
        first
          (const (InvariantViolation "QR upper-factor cardinality exceeds non-negative Int range"))
          (checkedNonNegativeProduct columnCount columnCount)
      runST $ do
        matrixWork <- newDoubleArray matrixEntryCount
        reflectorScalars <- newDoubleArray columnCount
        copyDoubleList matrixWork matrixValues
        setDoubleArray reflectorScalars columnCount 0.0
        factorResult <- factorQR rowCount columnCount matrixWork reflectorScalars
        case factorResult of
          Left err -> pure (Left err)
          Right () -> do
            qValues <- formThinQ rowCount columnCount matrixEntryCount matrixWork reflectorScalars
            rValues <- extractUpperR rowCount columnCount upperEntryCount matrixWork
            pure (Right (qValues, rValues))

choleskyLower :: Int -> [Double] -> Either MoonlightError [Double]
choleskyLower !matrixSize matrixValues = do
  matrixEntryCount <- requireSquarePayload "Cholesky decomposition" matrixSize matrixValues
  runST $ do
    matrixWork <- newDoubleArray matrixEntryCount
    copyDoubleList matrixWork matrixValues
    symmetryResult <- checkSymmetricMatrix matrixSize matrixWork
    case symmetryResult of
      Left err -> pure (Left err)
      Right () -> do
        factorResult <- factorCholesky matrixSize matrixWork
        case factorResult of
          Left err -> pure (Left err)
          Right () -> do
            zeroStrictUpper matrixSize matrixWork
            Right <$> freezeDoubleList matrixWork

newDoubleArray :: Int -> ST s (PrimArray.MutablePrimArray s Double)
newDoubleArray = PrimArray.newPrimArray
{-# INLINE newDoubleArray #-}

newIntArray :: Int -> ST s (PrimArray.MutablePrimArray s Int)
newIntArray = PrimArray.newPrimArray
{-# INLINE newIntArray #-}

setDoubleArray :: PrimArray.MutablePrimArray s Double -> Int -> Double -> ST s ()
setDoubleArray !target !entryCount !entryValue =
  PrimArray.setPrimArray target 0 entryCount entryValue
{-# INLINE setDoubleArray #-}

copyDoubleList :: PrimArray.MutablePrimArray s Double -> [Double] -> ST s ()
copyDoubleList !target = go 0
  where
    go !_ [] = pure ()
    go !entryIndex (entryValue : restValues) = do
      PrimArray.writePrimArray target entryIndex entryValue
      go (entryIndex + 1) restValues
{-# INLINE copyDoubleList #-}

freezeDoubleList :: PrimArray.MutablePrimArray s Double -> ST s [Double]
freezeDoubleList !values =
  PrimArray.primArrayToList <$> PrimArray.unsafeFreezePrimArray values
{-# INLINE freezeDoubleList #-}

rowMajorIndex :: Int -> Int -> Int -> Int
rowMajorIndex !columnCount !rowIndex !columnIndex =
  rowIndex * columnCount + columnIndex
{-# INLINE rowMajorIndex #-}

readMatrix :: Int -> PrimArray.MutablePrimArray s Double -> Int -> Int -> ST s Double
readMatrix !columnCount !matrixValues !rowIndex !columnIndex =
  PrimArray.readPrimArray matrixValues (rowMajorIndex columnCount rowIndex columnIndex)
{-# INLINE readMatrix #-}

writeMatrix :: Int -> PrimArray.MutablePrimArray s Double -> Int -> Int -> Double -> ST s ()
writeMatrix !columnCount !matrixValues !rowIndex !columnIndex !entryValue =
  PrimArray.writePrimArray matrixValues (rowMajorIndex columnCount rowIndex columnIndex) entryValue
{-# INLINE writeMatrix #-}

readMatrixWithColumnCount :: Int -> PrimArray.MutablePrimArray s Double -> Int -> Int -> ST s Double
readMatrixWithColumnCount = readMatrix
{-# INLINE readMatrixWithColumnCount #-}

writeMatrixWithColumnCount :: Int -> PrimArray.MutablePrimArray s Double -> Int -> Int -> Double -> ST s ()
writeMatrixWithColumnCount = writeMatrix
{-# INLINE writeMatrixWithColumnCount #-}

requireSquarePayload :: String -> Int -> [Double] -> Either MoonlightError Int
requireSquarePayload label matrixSize matrixValues = do
  expectedLength <-
    first
      (const (InvariantViolation (label <> " square cardinality exceeds non-negative Int range")))
      (checkedNonNegativeProduct matrixSize matrixSize)
  if length matrixValues /= expectedLength
    then
      Left
        ( InvariantViolation
            ( label
                <> " square payload length mismatch: expected "
                <> show expectedLength
                <> " values but received "
                <> show (length matrixValues)
            )
        )
    else requireFiniteEntries label matrixValues *> Right expectedLength

requireMatrixPayload :: String -> Int -> Int -> [Double] -> Either MoonlightError Int
requireMatrixPayload label rowCount columnCount matrixValues = do
  expectedLength <-
    first
      (const (InvariantViolation (label <> " dense cardinality exceeds non-negative Int range")))
      (checkedNonNegativeProduct rowCount columnCount)
  if length matrixValues /= expectedLength
    then
      Left
        ( InvariantViolation
            ( label
                <> " dense payload length mismatch: expected "
                <> show expectedLength
                <> " values but received "
                <> show (length matrixValues)
            )
        )
    else requireFiniteEntries label matrixValues *> Right expectedLength

requireVectorPayload :: String -> Int -> [Double] -> Either MoonlightError ()
requireVectorPayload label vectorSize vectorValues
  | vectorSize < 0 =
      Left (InvariantViolation (label <> " requires a non-negative vector size"))
  | length vectorValues /= vectorSize =
      Left
        ( InvariantViolation
            ( label
                <> " vector payload length mismatch: expected "
                <> show vectorSize
                <> " values but received "
                <> show (length vectorValues)
            )
        )
  | otherwise =
      requireFiniteEntries label vectorValues

requireFiniteEntries :: String -> [Double] -> Either MoonlightError ()
requireFiniteEntries label values =
  if all fieldValueValid values
    then Right ()
    else Left (InvariantViolation (label <> " requires finite entries"))

checkSymmetricMatrix :: Int -> PrimArray.MutablePrimArray s Double -> ST s (Either MoonlightError ())
checkSymmetricMatrix !matrixSize !matrixWork = goRow 0
  where
    tolerance = sqrt epsilon

    goRow !rowIndex
      | rowIndex >= matrixSize = pure (Right ())
      | otherwise = goColumn rowIndex (rowIndex + 1)

    goColumn !rowIndex !columnIndex
      | columnIndex >= matrixSize = goRow (rowIndex + 1)
      | otherwise = do
          leftValue <- readMatrix matrixSize matrixWork rowIndex columnIndex
          rightValue <- readMatrix matrixSize matrixWork columnIndex rowIndex
          if abs (leftValue - rightValue) <= tolerance
            then goColumn rowIndex (columnIndex + 1)
            else pure (Left (InvariantViolation "Cholesky decomposition requires a symmetric matrix"))
{-# INLINE checkSymmetricMatrix #-}

factorPLU ::
  Int ->
  PrimArray.MutablePrimArray s Double ->
  PrimArray.MutablePrimArray s Int ->
  ST s (Either MoonlightError ())
factorPLU !matrixSize !matrixWork !pivotRows = go 0
  where
    go !pivotIndex
      | pivotIndex >= matrixSize = pure (Right ())
      | otherwise = do
          (selectedRow, selectedMagnitude) <- findPivotRow matrixSize matrixWork pivotIndex
          if selectedMagnitude <= epsilon
            then pure (Left (InvariantViolation ("direct solve failed during PLU factorization: non-invertible pivot at column " <> show pivotIndex)))
            else do
              PrimArray.writePrimArray pivotRows pivotIndex selectedRow
              swapMatrixRows matrixSize matrixWork pivotIndex selectedRow
              pivotValue <- readMatrix matrixSize matrixWork pivotIndex pivotIndex
              eliminatePLUColumn matrixSize matrixWork pivotIndex pivotValue
              go (pivotIndex + 1)

findPivotRow :: Int -> PrimArray.MutablePrimArray s Double -> Int -> ST s (Int, Double)
findPivotRow !matrixSize !matrixWork !columnIndex = do
  firstValue <- readMatrix matrixSize matrixWork columnIndex columnIndex
  go (columnIndex + 1) columnIndex (abs firstValue)
  where
    go !rowIndex !bestRow !bestMagnitude
      | rowIndex >= matrixSize = pure (bestRow, bestMagnitude)
      | otherwise = do
          candidateValue <- readMatrix matrixSize matrixWork rowIndex columnIndex
          let !candidateMagnitude = abs candidateValue
          if candidateMagnitude > bestMagnitude
            then go (rowIndex + 1) rowIndex candidateMagnitude
            else go (rowIndex + 1) bestRow bestMagnitude
{-# INLINE findPivotRow #-}

swapMatrixRows :: Int -> PrimArray.MutablePrimArray s Double -> Int -> Int -> ST s ()
swapMatrixRows !columnCount !matrixWork !leftRow !rightRow
  | leftRow == rightRow = pure ()
  | otherwise = go 0
  where
    go !columnIndex
      | columnIndex >= columnCount = pure ()
      | otherwise = do
          leftValue <- readMatrix columnCount matrixWork leftRow columnIndex
          rightValue <- readMatrix columnCount matrixWork rightRow columnIndex
          writeMatrix columnCount matrixWork leftRow columnIndex rightValue
          writeMatrix columnCount matrixWork rightRow columnIndex leftValue
          go (columnIndex + 1)
{-# INLINE swapMatrixRows #-}

eliminatePLUColumn :: Int -> PrimArray.MutablePrimArray s Double -> Int -> Double -> ST s ()
eliminatePLUColumn !matrixSize !matrixWork !pivotIndex !pivotValue =
  goRow (pivotIndex + 1)
  where
    goRow !rowIndex
      | rowIndex >= matrixSize = pure ()
      | otherwise = do
          factorEntry <- readMatrix matrixSize matrixWork rowIndex pivotIndex
          let !multiplier = factorEntry / pivotValue
          writeMatrix matrixSize matrixWork rowIndex pivotIndex multiplier
          updateTrailingRow rowIndex multiplier (pivotIndex + 1)
          goRow (rowIndex + 1)

    updateTrailingRow !rowIndex !multiplier !columnIndex
      | columnIndex >= matrixSize = pure ()
      | otherwise = do
          currentValue <- readMatrix matrixSize matrixWork rowIndex columnIndex
          pivotRowValue <- readMatrix matrixSize matrixWork pivotIndex columnIndex
          writeMatrix matrixSize matrixWork rowIndex columnIndex (currentValue - multiplier * pivotRowValue)
          updateTrailingRow rowIndex multiplier (columnIndex + 1)
{-# INLINE eliminatePLUColumn #-}

applyPivotVector :: Int -> PrimArray.MutablePrimArray s Int -> PrimArray.MutablePrimArray s Double -> ST s ()
applyPivotVector !matrixSize !pivotRows !rhsWork = go 0
  where
    go !pivotIndex
      | pivotIndex >= matrixSize = pure ()
      | otherwise = do
          selectedRow <- PrimArray.readPrimArray pivotRows pivotIndex
          swapRhsEntries rhsWork pivotIndex selectedRow
          go (pivotIndex + 1)
{-# INLINE applyPivotVector #-}

swapRhsEntries :: PrimArray.MutablePrimArray s Double -> Int -> Int -> ST s ()
swapRhsEntries !rhsWork !leftIndex !rightIndex
  | leftIndex == rightIndex = pure ()
  | otherwise = do
      leftValue <- PrimArray.readPrimArray rhsWork leftIndex
      rightValue <- PrimArray.readPrimArray rhsWork rightIndex
      PrimArray.writePrimArray rhsWork leftIndex rightValue
      PrimArray.writePrimArray rhsWork rightIndex leftValue
{-# INLINE swapRhsEntries #-}

forwardSolveUnitLower :: Int -> PrimArray.MutablePrimArray s Double -> PrimArray.MutablePrimArray s Double -> ST s ()
forwardSolveUnitLower !matrixSize !matrixWork !rhsWork = goRow 0
  where
    goRow !rowIndex
      | rowIndex >= matrixSize = pure ()
      | otherwise = do
          contribution <- lowerDot rowIndex 0 0.0
          rhsValue <- PrimArray.readPrimArray rhsWork rowIndex
          PrimArray.writePrimArray rhsWork rowIndex (rhsValue - contribution)
          goRow (rowIndex + 1)

    lowerDot !rowIndex !columnIndex !accumulator
      | columnIndex >= rowIndex = pure accumulator
      | otherwise = do
          lowerValue <- readMatrix matrixSize matrixWork rowIndex columnIndex
          solvedValue <- PrimArray.readPrimArray rhsWork columnIndex
          lowerDot rowIndex (columnIndex + 1) (accumulator + lowerValue * solvedValue)
{-# INLINE forwardSolveUnitLower #-}

backwardSolveUpper :: Int -> PrimArray.MutablePrimArray s Double -> PrimArray.MutablePrimArray s Double -> ST s (Either MoonlightError ())
backwardSolveUpper !matrixSize !matrixWork !rhsWork = goRow (matrixSize - 1)
  where
    goRow !rowIndex
      | rowIndex < 0 = pure (Right ())
      | otherwise = do
          contribution <- upperDot rowIndex (rowIndex + 1) 0.0
          diagonalValue <- readMatrix matrixSize matrixWork rowIndex rowIndex
          rhsValue <- PrimArray.readPrimArray rhsWork rowIndex
          if abs diagonalValue <= epsilon
            then pure (Left (InvariantViolation "direct solve failed during backward substitution: zero diagonal pivot"))
            else do
              PrimArray.writePrimArray rhsWork rowIndex ((rhsValue - contribution) / diagonalValue)
              goRow (rowIndex - 1)

    upperDot !rowIndex !columnIndex !accumulator
      | columnIndex >= matrixSize = pure accumulator
      | otherwise = do
          upperValue <- readMatrix matrixSize matrixWork rowIndex columnIndex
          solvedValue <- PrimArray.readPrimArray rhsWork columnIndex
          upperDot rowIndex (columnIndex + 1) (accumulator + upperValue * solvedValue)
{-# INLINE backwardSolveUpper #-}

factorQR ::
  Int ->
  Int ->
  PrimArray.MutablePrimArray s Double ->
  PrimArray.MutablePrimArray s Double ->
  ST s (Either MoonlightError ())
factorQR !rowCount !columnCount !matrixWork !reflectorScalars = go 0
  where
    go !columnIndex
      | columnIndex >= columnCount = pure (Right ())
      | otherwise = do
          reflectorResult <- makeHouseholderReflector rowCount columnCount matrixWork reflectorScalars columnIndex
          case reflectorResult of
            Left err -> pure (Left err)
            Right tauValue -> do
              applyQRReflectorToRemainder rowCount columnCount matrixWork columnIndex tauValue
              go (columnIndex + 1)

makeHouseholderReflector ::
  Int ->
  Int ->
  PrimArray.MutablePrimArray s Double ->
  PrimArray.MutablePrimArray s Double ->
  Int ->
  ST s (Either MoonlightError Double)
makeHouseholderReflector !rowCount !columnCount !matrixWork !reflectorScalars !columnIndex = do
  alphaValue <- readMatrix columnCount matrixWork columnIndex columnIndex
  tailNorm <- columnTailNorm rowCount columnCount matrixWork columnIndex
  if tailNorm == 0.0
    then
      if abs alphaValue <= epsilon
        then pure (Left (InvariantViolation "QR decomposition failed: dependent or zero column encountered"))
        else do
          PrimArray.writePrimArray reflectorScalars columnIndex 0.0
          pure (Right 0.0)
    else do
      let !normValue = hypotStable alphaValue tailNorm
          !betaValue =
            if alphaValue < 0.0 || isNegativeZero alphaValue
              then normValue
              else negate normValue
      if abs betaValue <= epsilon
        then pure (Left (InvariantViolation "QR decomposition failed: dependent or zero column encountered"))
        else do
          let !tauValue = (betaValue - alphaValue) / betaValue
              !scaleValue = 1.0 / (alphaValue - betaValue)
          scaleReflectorTail rowCount columnCount matrixWork columnIndex scaleValue
          writeMatrix columnCount matrixWork columnIndex columnIndex betaValue
          PrimArray.writePrimArray reflectorScalars columnIndex tauValue
          pure (Right tauValue)
{-# INLINE makeHouseholderReflector #-}

columnTailNorm :: Int -> Int -> PrimArray.MutablePrimArray s Double -> Int -> ST s Double
columnTailNorm !rowCount !columnCount !matrixWork !columnIndex =
  go (columnIndex + 1) 0.0 1.0
  where
    go !rowIndex !scaleValue !sumSquares
      | rowIndex >= rowCount = pure (scaleValue * sqrt sumSquares)
      | otherwise = do
          entryValue <- readMatrix columnCount matrixWork rowIndex columnIndex
          let !entryMagnitude = abs entryValue
          if entryMagnitude == 0.0
            then go (rowIndex + 1) scaleValue sumSquares
            else
              if scaleValue < entryMagnitude
                then
                  let !scaled = scaleValue / entryMagnitude
                   in go (rowIndex + 1) entryMagnitude (1.0 + sumSquares * scaled * scaled)
                else
                  let !scaled = entryMagnitude / scaleValue
                   in go (rowIndex + 1) scaleValue (sumSquares + scaled * scaled)
{-# INLINE columnTailNorm #-}

scaleReflectorTail :: Int -> Int -> PrimArray.MutablePrimArray s Double -> Int -> Double -> ST s ()
scaleReflectorTail !rowCount !columnCount !matrixWork !columnIndex !scaleValue =
  go (columnIndex + 1)
  where
    go !rowIndex
      | rowIndex >= rowCount = pure ()
      | otherwise = do
          entryValue <- readMatrix columnCount matrixWork rowIndex columnIndex
          writeMatrix columnCount matrixWork rowIndex columnIndex (entryValue * scaleValue)
          go (rowIndex + 1)
{-# INLINE scaleReflectorTail #-}

applyQRReflectorToRemainder :: Int -> Int -> PrimArray.MutablePrimArray s Double -> Int -> Double -> ST s ()
applyQRReflectorToRemainder !rowCount !columnCount !matrixWork !reflectorIndex !tauValue
  | tauValue == 0.0 = pure ()
  | otherwise = goColumn (reflectorIndex + 1)
  where
    goColumn !targetColumn
      | targetColumn >= columnCount = pure ()
      | otherwise = do
          dotValue <- matrixReflectorDot rowCount columnCount matrixWork reflectorIndex targetColumn
          let !scaledDot = tauValue * dotValue
          pivotValue <- readMatrix columnCount matrixWork reflectorIndex targetColumn
          writeMatrix columnCount matrixWork reflectorIndex targetColumn (pivotValue - scaledDot)
          updateTail targetColumn scaledDot (reflectorIndex + 1)
          goColumn (targetColumn + 1)

    updateTail !targetColumn !scaledDot !rowIndex
      | rowIndex >= rowCount = pure ()
      | otherwise = do
          reflectorEntry <- readMatrix columnCount matrixWork rowIndex reflectorIndex
          targetEntry <- readMatrix columnCount matrixWork rowIndex targetColumn
          writeMatrix columnCount matrixWork rowIndex targetColumn (targetEntry - reflectorEntry * scaledDot)
          updateTail targetColumn scaledDot (rowIndex + 1)
{-# INLINE applyQRReflectorToRemainder #-}

matrixReflectorDot :: Int -> Int -> PrimArray.MutablePrimArray s Double -> Int -> Int -> ST s Double
matrixReflectorDot !rowCount !columnCount !matrixWork !reflectorIndex !targetColumn = do
  pivotValue <- readMatrix columnCount matrixWork reflectorIndex targetColumn
  go (reflectorIndex + 1) pivotValue
  where
    go !rowIndex !accumulator
      | rowIndex >= rowCount = pure accumulator
      | otherwise = do
          reflectorEntry <- readMatrix columnCount matrixWork rowIndex reflectorIndex
          targetEntry <- readMatrix columnCount matrixWork rowIndex targetColumn
          go (rowIndex + 1) (accumulator + reflectorEntry * targetEntry)
{-# INLINE matrixReflectorDot #-}

formThinQ ::
  Int ->
  Int ->
  Int ->
  PrimArray.MutablePrimArray s Double ->
  PrimArray.MutablePrimArray s Double ->
  ST s [Double]
formThinQ !rowCount !columnCount !matrixEntryCount !matrixWork !reflectorScalars = do
  qWork <- newDoubleArray matrixEntryCount
  setDoubleArray qWork matrixEntryCount 0.0
  setThinIdentity rowCount columnCount qWork
  applyReflectors (columnCount - 1) qWork
  freezeDoubleList qWork
  where
    applyReflectors !reflectorIndex !qWork
      | reflectorIndex < 0 = pure ()
      | otherwise = do
          tauValue <- PrimArray.readPrimArray reflectorScalars reflectorIndex
          applyQRReflectorToQ rowCount columnCount matrixWork qWork reflectorIndex tauValue
          applyReflectors (reflectorIndex - 1) qWork
{-# INLINE formThinQ #-}

setThinIdentity :: Int -> Int -> PrimArray.MutablePrimArray s Double -> ST s ()
setThinIdentity !rowCount !columnCount !qWork =
  go 0
  where
    diagonalCount = min rowCount columnCount

    go !diagonalIndex
      | diagonalIndex >= diagonalCount = pure ()
      | otherwise = do
          writeMatrixWithColumnCount columnCount qWork diagonalIndex diagonalIndex 1.0
          go (diagonalIndex + 1)
{-# INLINE setThinIdentity #-}

applyQRReflectorToQ ::
  Int ->
  Int ->
  PrimArray.MutablePrimArray s Double ->
  PrimArray.MutablePrimArray s Double ->
  Int ->
  Double ->
  ST s ()
applyQRReflectorToQ !rowCount !columnCount !matrixWork !qWork !reflectorIndex !tauValue
  | tauValue == 0.0 = pure ()
  | otherwise = goColumn reflectorIndex
  where
    goColumn !targetColumn
      | targetColumn >= columnCount = pure ()
      | otherwise = do
          dotValue <- qReflectorDot targetColumn reflectorIndex 0.0
          let !scaledDot = tauValue * dotValue
          pivotValue <- readMatrixWithColumnCount columnCount qWork reflectorIndex targetColumn
          writeMatrixWithColumnCount columnCount qWork reflectorIndex targetColumn (pivotValue - scaledDot)
          updateTail targetColumn scaledDot (reflectorIndex + 1)
          goColumn (targetColumn + 1)

    qReflectorDot !targetColumn !rowIndex !accumulator
      | rowIndex >= rowCount = pure accumulator
      | rowIndex == reflectorIndex = do
          qEntry <- readMatrixWithColumnCount columnCount qWork rowIndex targetColumn
          qReflectorDot targetColumn (rowIndex + 1) (accumulator + qEntry)
      | otherwise = do
          reflectorEntry <- readMatrix columnCount matrixWork rowIndex reflectorIndex
          qEntry <- readMatrixWithColumnCount columnCount qWork rowIndex targetColumn
          qReflectorDot targetColumn (rowIndex + 1) (accumulator + reflectorEntry * qEntry)

    updateTail !targetColumn !scaledDot !rowIndex
      | rowIndex >= rowCount = pure ()
      | otherwise = do
          reflectorEntry <- readMatrix columnCount matrixWork rowIndex reflectorIndex
          qEntry <- readMatrixWithColumnCount columnCount qWork rowIndex targetColumn
          writeMatrixWithColumnCount columnCount qWork rowIndex targetColumn (qEntry - reflectorEntry * scaledDot)
          updateTail targetColumn scaledDot (rowIndex + 1)
{-# INLINE applyQRReflectorToQ #-}

extractUpperR :: Int -> Int -> Int -> PrimArray.MutablePrimArray s Double -> ST s [Double]
extractUpperR !rowCount !columnCount !upperEntryCount !matrixWork = do
  rWork <- newDoubleArray upperEntryCount
  setDoubleArray rWork upperEntryCount 0.0
  goRow 0 rWork
  freezeDoubleList rWork
  where
    goRow !rowIndex !rWork
      | rowIndex >= columnCount = pure ()
      | otherwise = do
          goColumn rowIndex rowIndex rWork
          goRow (rowIndex + 1) rWork

    goColumn !rowIndex !columnIndex !rWork
      | columnIndex >= columnCount = pure ()
      | rowIndex >= rowCount = pure ()
      | otherwise = do
          entryValue <- readMatrix columnCount matrixWork rowIndex columnIndex
          writeMatrix columnCount rWork rowIndex columnIndex entryValue
          goColumn rowIndex (columnIndex + 1) rWork
{-# INLINE extractUpperR #-}

factorCholesky :: Int -> PrimArray.MutablePrimArray s Double -> ST s (Either MoonlightError ())
factorCholesky !matrixSize !matrixWork = goColumn 0
  where
    goColumn !columnIndex
      | columnIndex >= matrixSize = pure (Right ())
      | otherwise = do
          diagonalContribution <- lowerSelfDot columnIndex 0 0.0
          diagonalInput <- readMatrix matrixSize matrixWork columnIndex columnIndex
          let !diagonalResidual = diagonalInput - diagonalContribution
          if diagonalResidual <= 0.0 || not (fieldValueValid diagonalResidual)
            then pure (Left (InvariantViolation "Cholesky decomposition failed: matrix is not positive-definite"))
            else do
              let !diagonalValue = sqrt diagonalResidual
              writeMatrix matrixSize matrixWork columnIndex columnIndex diagonalValue
              updateColumnTail columnIndex diagonalValue (columnIndex + 1)
              goColumn (columnIndex + 1)

    lowerSelfDot !rowIndex !columnIndex !accumulator
      | columnIndex >= rowIndex = pure accumulator
      | otherwise = do
          lowerValue <- readMatrix matrixSize matrixWork rowIndex columnIndex
          lowerSelfDot rowIndex (columnIndex + 1) (accumulator + lowerValue * lowerValue)

    lowerCrossDot !leftRow !rightRow !columnIndex !accumulator
      | columnIndex >= rightRow = pure accumulator
      | otherwise = do
          leftValue <- readMatrix matrixSize matrixWork leftRow columnIndex
          rightValue <- readMatrix matrixSize matrixWork rightRow columnIndex
          lowerCrossDot leftRow rightRow (columnIndex + 1) (accumulator + leftValue * rightValue)

    updateColumnTail !columnIndex !diagonalValue !rowIndex
      | rowIndex >= matrixSize = pure ()
      | otherwise = do
          crossContribution <- lowerCrossDot rowIndex columnIndex 0 0.0
          inputValue <- readMatrix matrixSize matrixWork rowIndex columnIndex
          writeMatrix matrixSize matrixWork rowIndex columnIndex ((inputValue - crossContribution) / diagonalValue)
          updateColumnTail columnIndex diagonalValue (rowIndex + 1)
{-# INLINE factorCholesky #-}

zeroStrictUpper :: Int -> PrimArray.MutablePrimArray s Double -> ST s ()
zeroStrictUpper !matrixSize !matrixWork = goRow 0
  where
    goRow !rowIndex
      | rowIndex >= matrixSize = pure ()
      | otherwise = do
          goColumn rowIndex (rowIndex + 1)
          goRow (rowIndex + 1)

    goColumn !rowIndex !columnIndex
      | columnIndex >= matrixSize = pure ()
      | otherwise = do
          writeMatrix matrixSize matrixWork rowIndex columnIndex 0.0
          goColumn rowIndex (columnIndex + 1)
{-# INLINE zeroStrictUpper #-}

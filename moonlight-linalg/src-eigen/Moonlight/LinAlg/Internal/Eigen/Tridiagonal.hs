{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Internal.Eigen.Tridiagonal
  ( canonicalizeEigenvectorSigns,
    eigenpairsFromMutable,
    newIdentityEigenvectors,
    orthonormalizeDegenerateClusters,
    sortEigenpairsAscending,
    solveTridiagonalEigenvectors,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
  ( MutablePrimArray,
    readPrimArray,
    writePrimArray,
  )
import Moonlight.Core (MoonlightError (..), fieldValueValid)
import Moonlight.LinAlg.Internal.Eigen.DenseWork
  ( MutableDenseWork (..),
    newDenseWork,
    setIdentityDenseWork,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels
  ( copySignMagnitude,
    epsDouble,
    forIndex,
    hypotStable,
    maxFiniteDouble,
    safeMinimumDouble,
  )
import Prelude

newIdentityEigenvectors :: Int -> ST s (MutableDenseWork s)
newIdentityEigenvectors !matrixSize = do
  eigenvectors <- newDenseWork matrixSize matrixSize
  setIdentityDenseWork eigenvectors
  pure eigenvectors

solveTridiagonalEigenvectors ::
  Int ->
  MutablePrimArray s Double ->
  MutablePrimArray s Double ->
  MutableDenseWork s ->
  ST s (Either MoonlightError ())
solveTridiagonalEigenvectors !matrixSize diagonalValues offDiagonalValues eigenvectors = do
  when (matrixSize > 0) $
    writePrimArray offDiagonalValues (matrixSize - 1) 0.0
  scaleValue <- scaleTridiagonal matrixSize diagonalValues offDiagonalValues
  solveResult <- solveAllIndices 0
  unscaleDiagonal matrixSize diagonalValues scaleValue
  pure solveResult
  where
    !iterationLimit = max 64 (matrixSize * 128)

    solveAllIndices !splitIndex
      | splitIndex >= matrixSize = pure (Right ())
      | otherwise = do
          indexResult <- convergeIndex splitIndex 0
          case indexResult of
            Left err -> pure (Left err)
            Right () -> solveAllIndices (splitIndex + 1)

    convergeIndex !splitIndex !iterationCount
      | iterationCount >= iterationLimit = do
          maxOffDiagonal <- maximumOffDiagonalMagnitude splitIndex matrixSize offDiagonalValues
          pure
            ( Left
                ( InvariantViolation
                    ( "tridiagonal eigensolve exhausted implicit-QL iteration budget at block "
                        <> show (splitIndex, matrixSize - 1)
                        <> " after "
                        <> show iterationCount
                        <> " iterations; max off-diagonal="
                        <> show maxOffDiagonal
                    )
                )
            )
      | otherwise = do
          activeIndex <- findActiveSplit splitIndex
          if activeIndex == splitIndex
            then pure (Right ())
            else do
              implicitQLStep splitIndex activeIndex diagonalValues offDiagonalValues eigenvectors
              convergeIndex splitIndex (iterationCount + 1)

    findActiveSplit !splitIndex = go splitIndex
      where
        go !candidateIndex
          | candidateIndex >= matrixSize - 1 = pure (matrixSize - 1)
          | otherwise = do
              offDiagonal <- readPrimArray offDiagonalValues candidateIndex
              leftDiagonal <- readPrimArray diagonalValues candidateIndex
              rightDiagonal <- readPrimArray diagonalValues (candidateIndex + 1)
              if negligibleOffDiagonal offDiagonal leftDiagonal rightDiagonal
                then writePrimArray offDiagonalValues candidateIndex 0.0 >> pure candidateIndex
                else go (candidateIndex + 1)

implicitQLStep ::
  Int ->
  Int ->
  MutablePrimArray s Double ->
  MutablePrimArray s Double ->
  MutableDenseWork s ->
  ST s ()
implicitQLStep !splitIndex !activeIndex diagonalValues offDiagonalValues (MutableDenseWork eigenvectorRowCount _ eigenvectorPayload) = do
  leftDiagonal <- readPrimArray diagonalValues splitIndex
  nextDiagonal <- readPrimArray diagonalValues (splitIndex + 1)
  leftOffDiagonal <- readPrimArray offDiagonalValues splitIndex
  activeDiagonal <- readPrimArray diagonalValues activeIndex
  let !shiftRatio = (nextDiagonal - leftDiagonal) / (2.0 * leftOffDiagonal)
      !shiftRadius = hypotStable shiftRatio 1.0
      !shiftDenominator = shiftRatio + copySignMagnitude shiftRadius shiftRatio
      !initialShift = activeDiagonal - leftDiagonal + (leftOffDiagonal / shiftDenominator)
  sweepDown (activeIndex - 1) 1.0 1.0 initialShift 0.0
  where
    sweepDown !indexValue !previousCosine !previousSine !currentShift !currentCorrection
      | indexValue < splitIndex = do
          updatedLeftDiagonal <- readPrimArray diagonalValues splitIndex
          writePrimArray diagonalValues splitIndex (updatedLeftDiagonal - currentCorrection)
          writePrimArray offDiagonalValues splitIndex currentShift
          writePrimArray offDiagonalValues activeIndex 0.0
      | otherwise = do
          currentOffDiagonal <- readPrimArray offDiagonalValues indexValue
          currentDiagonal <- readPrimArray diagonalValues indexValue
          nextDiagonal <- readPrimArray diagonalValues (indexValue + 1)
          let !fValue = previousSine * currentOffDiagonal
              !bValue = previousCosine * currentOffDiagonal
          rotateWithGivens indexValue bValue currentDiagonal nextDiagonal currentCorrection fValue currentShift

    rotateWithGivens !indexValue !bValue !currentDiagonal !nextDiagonal !currentCorrection !fValue !gValue
      | fValue == 0.0 && gValue == 0.0 =
          finishRotation indexValue bValue currentDiagonal nextDiagonal currentCorrection 1.0 0.0 0.0
      | abs fValue >= abs gValue =
          let !normalizedCosine = gValue / fValue
              !radius = hypotStable normalizedCosine 1.0
              !sineValue = 1.0 / radius
              !cosineValue = normalizedCosine * sineValue
           in finishRotation indexValue bValue currentDiagonal nextDiagonal currentCorrection cosineValue sineValue (fValue * radius)
      | otherwise =
          let !normalizedSine = fValue / gValue
              !radius = hypotStable normalizedSine 1.0
              !cosineValue = 1.0 / radius
              !sineValue = normalizedSine * cosineValue
           in finishRotation indexValue bValue currentDiagonal nextDiagonal currentCorrection cosineValue sineValue (gValue * radius)

    finishRotation !indexValue !bValue !currentDiagonal !nextDiagonal !currentCorrection !nextCosine !nextSine !updatedOffDiagonal = do
      let !nextDiagonalBase = nextDiagonal - currentCorrection
          !rotationRadius = ((currentDiagonal - nextDiagonalBase) * nextSine) + (2.0 * nextCosine * bValue)
          !nextCorrection = nextSine * rotationRadius
          !updatedNextDiagonal = nextDiagonalBase + nextCorrection
          !nextShift = (nextCosine * rotationRadius) - bValue
      writePrimArray offDiagonalValues (indexValue + 1) updatedOffDiagonal
      writePrimArray diagonalValues (indexValue + 1) updatedNextDiagonal
      rotateEigenvectorColumnsAt indexValue (indexValue + 1) nextCosine nextSine
      sweepDown (indexValue - 1) nextCosine nextSine nextShift nextCorrection

    rotateEigenvectorColumnsAt !leftColumn !rightColumn !cosineValue !sineValue = rotateRows 0
      where
        !leftBase = leftColumn * eigenvectorRowCount
        !rightBase = rightColumn * eigenvectorRowCount
        rotateRows !rowIndex
          | rowIndex >= eigenvectorRowCount = pure ()
          | otherwise = do
              leftEntry <- readPrimArray eigenvectorPayload (leftBase + rowIndex)
              rightEntry <- readPrimArray eigenvectorPayload (rightBase + rowIndex)
              writePrimArray eigenvectorPayload (leftBase + rowIndex) (cosineValue * leftEntry - sineValue * rightEntry)
              writePrimArray eigenvectorPayload (rightBase + rowIndex) (sineValue * leftEntry + cosineValue * rightEntry)
              rotateRows (rowIndex + 1)

scaleTridiagonal :: Int -> MutablePrimArray s Double -> MutablePrimArray s Double -> ST s Double
scaleTridiagonal !matrixSize diagonalValues offDiagonalValues = do
  maximumMagnitude <- maximumTridiagonalMagnitude matrixSize diagonalValues offDiagonalValues
  let !safeMaximum = sqrt maxFiniteDouble * 0.25
      !safeMinimum = sqrt safeMinimumDouble / epsDouble
      !scaleValue
        | maximumMagnitude == 0.0 = 1.0
        | maximumMagnitude > safeMaximum = safeMaximum / maximumMagnitude
        | maximumMagnitude < safeMinimum = safeMinimum / maximumMagnitude
        | otherwise = 1.0
  when (scaleValue /= 1.0) $ do
    forIndex 0 matrixSize $ \indexValue -> do
      diagonalEntry <- readPrimArray diagonalValues indexValue
      writePrimArray diagonalValues indexValue (scaleValue * diagonalEntry)
    forIndex 0 (max 0 (matrixSize - 1)) $ \indexValue -> do
      offDiagonalEntry <- readPrimArray offDiagonalValues indexValue
      writePrimArray offDiagonalValues indexValue (scaleValue * offDiagonalEntry)
  pure scaleValue
{-# INLINE scaleTridiagonal #-}

unscaleDiagonal :: Int -> MutablePrimArray s Double -> Double -> ST s ()
unscaleDiagonal !matrixSize diagonalValues !scaleValue =
  when (scaleValue /= 1.0) $ do
    let !inverseScale = 1.0 / scaleValue
    forIndex 0 matrixSize $ \indexValue -> do
      diagonalEntry <- readPrimArray diagonalValues indexValue
      writePrimArray diagonalValues indexValue (inverseScale * diagonalEntry)
{-# INLINE unscaleDiagonal #-}

maximumTridiagonalMagnitude :: Int -> MutablePrimArray s Double -> MutablePrimArray s Double -> ST s Double
maximumTridiagonalMagnitude !matrixSize diagonalValues offDiagonalValues = do
  diagonalMaximum <- maximumArrayMagnitude 0 matrixSize diagonalValues 0.0
  maximumArrayMagnitude 0 (max 0 (matrixSize - 1)) offDiagonalValues diagonalMaximum
{-# INLINE maximumTridiagonalMagnitude #-}

maximumOffDiagonalMagnitude :: Int -> Int -> MutablePrimArray s Double -> ST s Double
maximumOffDiagonalMagnitude !startIndex !matrixSize offDiagonalValues =
  maximumArrayMagnitude startIndex (max startIndex (matrixSize - 1)) offDiagonalValues 0.0
{-# INLINE maximumOffDiagonalMagnitude #-}

maximumArrayMagnitude :: Int -> Int -> MutablePrimArray s Double -> Double -> ST s Double
maximumArrayMagnitude !startIndex !stopIndex arrayValues !initialMaximum = go startIndex initialMaximum
  where
    go !indexValue !currentMaximum
      | indexValue >= stopIndex = pure currentMaximum
      | otherwise = do
          entryValue <- readPrimArray arrayValues indexValue
          go (indexValue + 1) (max currentMaximum (abs entryValue))
{-# INLINE maximumArrayMagnitude #-}

negligibleOffDiagonal :: Double -> Double -> Double -> Bool
negligibleOffDiagonal !offDiagonal !leftDiagonal !rightDiagonal =
  abs offDiagonal <= (64.0 * epsDouble * (abs leftDiagonal + abs rightDiagonal)) + safeMinimumDouble
{-# INLINE negligibleOffDiagonal #-}

sortEigenpairsAscending :: Int -> MutablePrimArray s Double -> MutableDenseWork s -> ST s ()
sortEigenpairsAscending !matrixSize diagonalValues eigenvectors =
  forIndex 0 matrixSize $ \targetIndex -> do
    minimumIndex <- findMinimumIndex targetIndex (targetIndex + 1)
    when (minimumIndex /= targetIndex) $ do
      targetValue <- readPrimArray diagonalValues targetIndex
      minimumValue <- readPrimArray diagonalValues minimumIndex
      writePrimArray diagonalValues targetIndex minimumValue
      writePrimArray diagonalValues minimumIndex targetValue
      swapDenseColumnsTight eigenvectors targetIndex minimumIndex
  where
    findMinimumIndex !bestIndex !candidateIndex
      | candidateIndex >= matrixSize = pure bestIndex
      | otherwise = do
          bestValue <- readPrimArray diagonalValues bestIndex
          candidateValue <- readPrimArray diagonalValues candidateIndex
          if candidateValue < bestValue
            then findMinimumIndex candidateIndex (candidateIndex + 1)
            else findMinimumIndex bestIndex (candidateIndex + 1)

orthonormalizeDegenerateClusters :: Int -> MutablePrimArray s Double -> MutableDenseWork s -> ST s (Either MoonlightError ())
orthonormalizeDegenerateClusters !matrixSize diagonalValues eigenvectors = processCluster 0
  where
    processCluster !clusterStart
      | clusterStart >= matrixSize = pure (Right ())
      | otherwise = do
          clusterStop <- findClusterStop clusterStart (clusterStart + 1)
          clusterResult <- orthonormalizeColumns clusterStart clusterStop
          case clusterResult of
            Left err -> pure (Left err)
            Right () -> processCluster clusterStop

    findClusterStop !clusterStart !candidateIndex
      | candidateIndex >= matrixSize = pure matrixSize
      | otherwise = do
          leftValue <- readPrimArray diagonalValues (candidateIndex - 1)
          rightValue <- readPrimArray diagonalValues candidateIndex
          if sameEigenCluster leftValue rightValue
            then findClusterStop clusterStart (candidateIndex + 1)
            else pure candidateIndex

    orthonormalizeColumns !clusterStart !clusterStop = normalizeColumnAt clusterStart
      where
        normalizeColumnAt !columnIndex
          | columnIndex >= clusterStop = pure (Right ())
          | otherwise = do
              subtractPriorColumns clusterStart columnIndex
              normalized <- normalizeEigenvectorColumn eigenvectors columnIndex
              if normalized
                then normalizeColumnAt (columnIndex + 1)
                else pure (Left (InvariantViolation ("symmetric eigen decomposition produced zero eigenvector at column " <> show columnIndex)))

    subtractPriorColumns !priorIndex !columnIndex
      | priorIndex >= columnIndex = pure ()
      | otherwise = do
          projection <- dotDenseColumnsTight eigenvectors priorIndex columnIndex
          addScaledColumn eigenvectors priorIndex columnIndex (negate projection)
          subtractPriorColumns (priorIndex + 1) columnIndex

sameEigenCluster :: Double -> Double -> Bool
sameEigenCluster !leftValue !rightValue =
  abs (leftValue - rightValue) <= 128.0 * epsDouble * max 1.0 (max (abs leftValue) (abs rightValue))
{-# INLINE sameEigenCluster #-}

addScaledColumn :: MutableDenseWork s -> Int -> Int -> Double -> ST s ()
addScaledColumn (MutableDenseWork rowCount _ payload) !sourceColumn !targetColumn !scaleValue =
  forIndex 0 rowCount $ \rowIndex -> do
    sourceEntry <- readPrimArray payload (sourceBase + rowIndex)
    targetEntry <- readPrimArray payload (targetBase + rowIndex)
    writePrimArray payload (targetBase + rowIndex) (targetEntry + scaleValue * sourceEntry)
  where
    !sourceBase = sourceColumn * rowCount
    !targetBase = targetColumn * rowCount
{-# INLINE addScaledColumn #-}

normalizeEigenvectorColumn :: forall s. MutableDenseWork s -> Int -> ST s Bool
normalizeEigenvectorColumn eigenvectors !columnIndex = do
  normValue <- columnNorm eigenvectors columnIndex
  if normValue <= 0.0 || not (fieldValueValid normValue)
    then pure False
    else scaleDenseColumnTight eigenvectors columnIndex (1.0 / normValue) >> pure True
  where
    columnNorm :: MutableDenseWork s -> Int -> ST s Double
    columnNorm (MutableDenseWork rowCount _ payload) !targetColumn = go 0 0.0 1.0
      where
        !targetBase = targetColumn * rowCount
        go !rowIndex !scaleValue !scaledSum
          | rowIndex >= rowCount =
              if scaleValue == 0.0
                then pure 0.0
                else pure (scaleValue * sqrt scaledSum)
          | otherwise = do
              entryValue <- readPrimArray payload (targetBase + rowIndex)
              let !entryAbs = abs entryValue
              if entryAbs == 0.0
                then go (rowIndex + 1) scaleValue scaledSum
                else
                  if scaleValue < entryAbs
                    then
                      let !scaledRatio = scaleValue / entryAbs
                       in go (rowIndex + 1) entryAbs (1.0 + scaledSum * scaledRatio * scaledRatio)
                    else
                      let !scaledRatio = entryAbs / scaleValue
                       in go (rowIndex + 1) scaleValue (scaledSum + scaledRatio * scaledRatio)
{-# INLINE normalizeEigenvectorColumn #-}

canonicalizeEigenvectorSigns :: Int -> MutableDenseWork s -> ST s ()
canonicalizeEigenvectorSigns !matrixSize eigenvectors =
  forIndex 0 matrixSize $ \columnIndex -> do
    (_, maximumMagnitude, representativeValue) <- maximumMagnitudeInColumn eigenvectors columnIndex
    when (maximumMagnitude > 0.0 && representativeValue < 0.0) $
      scaleDenseColumnTight eigenvectors columnIndex (-1.0)

maximumMagnitudeInColumn :: MutableDenseWork s -> Int -> ST s (Int, Double, Double)
maximumMagnitudeInColumn (MutableDenseWork rowCount _ payload) !columnIndex = go 0 0 0.0 0.0
  where
    !columnBase = columnIndex * rowCount
    go !rowIndex !bestIndex !bestMagnitude !bestValue
      | rowIndex >= rowCount = pure (bestIndex, bestMagnitude, bestValue)
      | otherwise = do
          entryValue <- readPrimArray payload (columnBase + rowIndex)
          let !entryMagnitude = abs entryValue
          if entryMagnitude > bestMagnitude
            then go (rowIndex + 1) rowIndex entryMagnitude entryValue
            else go (rowIndex + 1) bestIndex bestMagnitude bestValue
{-# INLINE maximumMagnitudeInColumn #-}

swapDenseColumnsTight :: MutableDenseWork s -> Int -> Int -> ST s ()
swapDenseColumnsTight (MutableDenseWork rowCount _ payload) !leftColumn !rightColumn =
  when (leftColumn /= rightColumn) $
    forIndex 0 rowCount $ \rowIndex -> do
      leftValue <- readPrimArray payload (leftBase + rowIndex)
      rightValue <- readPrimArray payload (rightBase + rowIndex)
      writePrimArray payload (leftBase + rowIndex) rightValue
      writePrimArray payload (rightBase + rowIndex) leftValue
  where
    !leftBase = leftColumn * rowCount
    !rightBase = rightColumn * rowCount
{-# INLINE swapDenseColumnsTight #-}

dotDenseColumnsTight :: MutableDenseWork s -> Int -> Int -> ST s Double
dotDenseColumnsTight (MutableDenseWork rowCount _ payload) !leftColumn !rightColumn = go 0 0.0
  where
    !leftBase = leftColumn * rowCount
    !rightBase = rightColumn * rowCount
    go !rowIndex !accumulator
      | rowIndex >= rowCount = pure accumulator
      | otherwise = do
          leftValue <- readPrimArray payload (leftBase + rowIndex)
          rightValue <- readPrimArray payload (rightBase + rowIndex)
          go (rowIndex + 1) (accumulator + leftValue * rightValue)
{-# INLINE dotDenseColumnsTight #-}

scaleDenseColumnTight :: MutableDenseWork s -> Int -> Double -> ST s ()
scaleDenseColumnTight (MutableDenseWork rowCount _ payload) !columnIndex !scaleValue =
  forIndex 0 rowCount $ \rowIndex -> do
    entryValue <- readPrimArray payload (columnBase + rowIndex)
    writePrimArray payload (columnBase + rowIndex) (scaleValue * entryValue)
  where
    !columnBase = columnIndex * rowCount
{-# INLINE scaleDenseColumnTight #-}

eigenpairsFromMutable :: Int -> MutablePrimArray s Double -> MutableDenseWork s -> ST s [(Double, [Double])]
eigenpairsFromMutable !matrixSize diagonalValues (MutableDenseWork rowCount _ eigenvectorPayload) = collectColumns 0 []
  where
    collectColumns !columnIndex !revPairs
      | columnIndex >= matrixSize = pure (reverse revPairs)
      | otherwise = do
          eigenvalue <- readPrimArray diagonalValues columnIndex
          eigenvector <- collectColumnEntries columnIndex 0 []
          collectColumns (columnIndex + 1) ((eigenvalue, eigenvector) : revPairs)

    collectColumnEntries !columnIndex !rowIndex !revEntries
      | rowIndex >= matrixSize = pure (reverse revEntries)
      | otherwise = do
          entryValue <- readPrimArray eigenvectorPayload (rowIndex + columnIndex * rowCount)
          collectColumnEntries columnIndex (rowIndex + 1) (entryValue : revEntries)

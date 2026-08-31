{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Internal.Eigen.Householder
  ( backtransformLower,
    tridiagonalizeLower,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
  ( MutablePrimArray,
    newPrimArray,
    readPrimArray,
    setPrimArray,
    writePrimArray,
  )
import Moonlight.LinAlg.Internal.Eigen.DenseWork
  ( MutableDenseWork (..),
    readDenseWork,
    writeDenseWork,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels
  ( copySignMagnitude,
    forDescendingIndex,
    forIndex,
    hypotStable,
  )
import Prelude

tridiagonalizeLower ::
  MutableDenseWork s ->
  ST s (MutablePrimArray s Double, MutablePrimArray s Double, MutablePrimArray s Double)
tridiagonalizeLower work@(MutableDenseWork matrixSize _ _) = do
  diagonalValues <- newPrimArray matrixSize
  offDiagonalValues <- newPrimArray matrixSize
  reflectorScalars <- newPrimArray matrixSize
  matrixTimesReflector <- newPrimArray matrixSize
  setPrimArray offDiagonalValues 0 matrixSize 0.0
  setPrimArray reflectorScalars 0 matrixSize 0.0
  forIndex 0 matrixSize $ \pivotIndex -> do
    diagonalEntry <- readDenseWork work pivotIndex pivotIndex
    writePrimArray diagonalValues pivotIndex diagonalEntry
    when (pivotIndex < matrixSize - 1) $ do
      (reflectorScalar, reflectedSubDiagonal) <- makeHouseholderColumn pivotIndex work
      writePrimArray reflectorScalars pivotIndex reflectorScalar
      writePrimArray offDiagonalValues pivotIndex reflectedSubDiagonal
      when (reflectorScalar /= 0.0) $ do
        writeDenseWork work (pivotIndex + 1) pivotIndex 1.0
        symmetricLowerMatrixVector pivotIndex work matrixTimesReflector
        scaleScratch (matrixSize - pivotIndex - 1) reflectorScalar matrixTimesReflector
        reflectorDot <- dotImplicitReflector pivotIndex work matrixTimesReflector
        let !rankTwoCorrection = (-0.5) * reflectorScalar * reflectorDot
        addScaledImplicitReflector pivotIndex work matrixTimesReflector rankTwoCorrection
        rankTwoUpdateLower pivotIndex work matrixTimesReflector
        writeDenseWork work (pivotIndex + 1) pivotIndex reflectedSubDiagonal
  pure (diagonalValues, offDiagonalValues, reflectorScalars)

makeHouseholderColumn :: Int -> MutableDenseWork s -> ST s (Double, Double)
makeHouseholderColumn !pivotIndex work@(MutableDenseWork matrixSize _ _) =
  let !firstRow = pivotIndex + 1
      !reflectorLength = matrixSize - firstRow
   in if reflectorLength <= 0
        then pure (0.0, 0.0)
        else do
          firstEntry <- readDenseWork work firstRow pivotIndex
          tailNorm <- columnTailNorm work pivotIndex (firstRow + 1) matrixSize
          if tailNorm == 0.0
            then pure (0.0, firstEntry)
            else do
              let !sourceNorm = hypotStable firstEntry tailNorm
                  !reflectedHead = negate (copySignMagnitude sourceNorm firstEntry)
                  !reflectorScalar = (reflectedHead - firstEntry) / reflectedHead
                  !tailScale = 1.0 / (firstEntry - reflectedHead)
              scaleColumnTail work pivotIndex (firstRow + 1) matrixSize tailScale
              pure (reflectorScalar, reflectedHead)
{-# INLINE makeHouseholderColumn #-}

columnTailNorm :: MutableDenseWork s -> Int -> Int -> Int -> ST s Double
columnTailNorm work !columnIndex !startRow !stopRow = go startRow 0.0 1.0
  where
    go !rowIndex !scaleValue !scaledSum
      | rowIndex >= stopRow =
          if scaleValue == 0.0
            then pure 0.0
            else pure (scaleValue * sqrt scaledSum)
      | otherwise = do
          rawEntry <- readDenseWork work rowIndex columnIndex
          let !entryAbs = abs rawEntry
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
{-# INLINE columnTailNorm #-}

scaleColumnTail :: MutableDenseWork s -> Int -> Int -> Int -> Double -> ST s ()
scaleColumnTail work !columnIndex !startRow !stopRow !scaleValue =
  forIndex startRow stopRow $ \rowIndex -> do
    entryValue <- readDenseWork work rowIndex columnIndex
    writeDenseWork work rowIndex columnIndex (scaleValue * entryValue)
{-# INLINE scaleColumnTail #-}

implicitReflectorEntry :: Int -> MutableDenseWork s -> Int -> ST s Double
implicitReflectorEntry !pivotIndex work !localIndex =
  if localIndex == 0
    then pure 1.0
    else readDenseWork work (pivotIndex + 1 + localIndex) pivotIndex
{-# INLINE implicitReflectorEntry #-}

symmetricLowerMatrixVector ::
  Int ->
  MutableDenseWork s ->
  MutablePrimArray s Double ->
  ST s ()
symmetricLowerMatrixVector !pivotIndex work@(MutableDenseWork matrixSize _ _) scratchValues = do
  let !startRow = pivotIndex + 1
      !dimension = matrixSize - startRow
  setPrimArray scratchValues 0 dimension 0.0
  forIndex 0 dimension $ \columnLocalIndex -> do
    reflectorColumnEntry <- implicitReflectorEntry pivotIndex work columnLocalIndex
    diagonalEntry <- readDenseWork work (startRow + columnLocalIndex) (startRow + columnLocalIndex)
    scratchColumnEntry <- readPrimArray scratchValues columnLocalIndex
    writePrimArray scratchValues columnLocalIndex (scratchColumnEntry + diagonalEntry * reflectorColumnEntry)
    forIndex (columnLocalIndex + 1) dimension $ \rowLocalIndex -> do
      lowerEntry <- readDenseWork work (startRow + rowLocalIndex) (startRow + columnLocalIndex)
      reflectorRowEntry <- implicitReflectorEntry pivotIndex work rowLocalIndex
      rowAccumulator <- readPrimArray scratchValues rowLocalIndex
      writePrimArray scratchValues rowLocalIndex (rowAccumulator + lowerEntry * reflectorColumnEntry)
      columnAccumulator <- readPrimArray scratchValues columnLocalIndex
      writePrimArray scratchValues columnLocalIndex (columnAccumulator + lowerEntry * reflectorRowEntry)
{-# INLINE symmetricLowerMatrixVector #-}

scaleScratch :: Int -> Double -> MutablePrimArray s Double -> ST s ()
scaleScratch !entryCount !scaleValue scratchValues =
  forIndex 0 entryCount $ \entryIndex -> do
    entryValue <- readPrimArray scratchValues entryIndex
    writePrimArray scratchValues entryIndex (scaleValue * entryValue)
{-# INLINE scaleScratch #-}

dotImplicitReflector :: Int -> MutableDenseWork s -> MutablePrimArray s Double -> ST s Double
dotImplicitReflector !pivotIndex work@(MutableDenseWork matrixSize _ _) scratchValues =
  let !dimension = matrixSize - pivotIndex - 1
   in go 0 0.0 dimension
  where
    go !localIndex !accumulator !dimension
      | localIndex >= dimension = pure accumulator
      | otherwise = do
          reflectorEntry <- implicitReflectorEntry pivotIndex work localIndex
          scratchEntry <- readPrimArray scratchValues localIndex
          go (localIndex + 1) (accumulator + reflectorEntry * scratchEntry) dimension
{-# INLINE dotImplicitReflector #-}

addScaledImplicitReflector ::
  Int ->
  MutableDenseWork s ->
  MutablePrimArray s Double ->
  Double ->
  ST s ()
addScaledImplicitReflector !pivotIndex work@(MutableDenseWork matrixSize _ _) scratchValues !scaleValue =
  let !dimension = matrixSize - pivotIndex - 1
   in forIndex 0 dimension $ \localIndex -> do
        reflectorEntry <- implicitReflectorEntry pivotIndex work localIndex
        scratchEntry <- readPrimArray scratchValues localIndex
        writePrimArray scratchValues localIndex (scratchEntry + scaleValue * reflectorEntry)
{-# INLINE addScaledImplicitReflector #-}

rankTwoUpdateLower :: Int -> MutableDenseWork s -> MutablePrimArray s Double -> ST s ()
rankTwoUpdateLower !pivotIndex work@(MutableDenseWork matrixSize _ _) updateVector = do
  let !startRow = pivotIndex + 1
      !dimension = matrixSize - startRow
  forIndex 0 dimension $ \columnLocalIndex -> do
    reflectorColumnEntry <- implicitReflectorEntry pivotIndex work columnLocalIndex
    updateColumnEntry <- readPrimArray updateVector columnLocalIndex
    forIndex columnLocalIndex dimension $ \rowLocalIndex -> do
      reflectorRowEntry <- implicitReflectorEntry pivotIndex work rowLocalIndex
      updateRowEntry <- readPrimArray updateVector rowLocalIndex
      matrixEntry <- readDenseWork work (startRow + rowLocalIndex) (startRow + columnLocalIndex)
      writeDenseWork
        work
        (startRow + rowLocalIndex)
        (startRow + columnLocalIndex)
        (matrixEntry - reflectorRowEntry * updateColumnEntry - updateRowEntry * reflectorColumnEntry)
{-# INLINE rankTwoUpdateLower #-}

backtransformLower ::
  MutableDenseWork s ->
  MutablePrimArray s Double ->
  MutableDenseWork s ->
  ST s ()
backtransformLower reflectors reflectorScalars eigenvectors@(MutableDenseWork matrixSize _ _) =
  forDescendingIndex (matrixSize - 2) 0 $ \pivotIndex -> do
    reflectorScalar <- readPrimArray reflectorScalars pivotIndex
    when (reflectorScalar /= 0.0) $ do
      let !startRow = pivotIndex + 1
          !dimension = matrixSize - startRow
      forIndex 0 matrixSize $ \columnIndex -> do
        firstComponent <- readDenseWork eigenvectors startRow columnIndex
        reflectorProduct <- dotTail 1 dimension firstComponent pivotIndex columnIndex startRow
        let !projectionScale = reflectorScalar * reflectorProduct
        writeDenseWork eigenvectors startRow columnIndex (firstComponent - projectionScale)
        forIndex 1 dimension $ \localIndex -> do
          reflectorEntry <- readDenseWork reflectors (startRow + localIndex) pivotIndex
          eigenvectorEntry <- readDenseWork eigenvectors (startRow + localIndex) columnIndex
          writeDenseWork eigenvectors (startRow + localIndex) columnIndex (eigenvectorEntry - projectionScale * reflectorEntry)
  where
    dotTail !localIndex !dimension !accumulator !pivotIndex !columnIndex !startRow
      | localIndex >= dimension = pure accumulator
      | otherwise = do
          reflectorEntry <- readDenseWork reflectors (startRow + localIndex) pivotIndex
          eigenvectorEntry <- readDenseWork eigenvectors (startRow + localIndex) columnIndex
          dotTail (localIndex + 1) dimension (accumulator + reflectorEntry * eigenvectorEntry) pivotIndex columnIndex startRow

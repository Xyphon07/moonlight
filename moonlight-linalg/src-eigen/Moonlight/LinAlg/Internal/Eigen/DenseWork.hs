{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Internal.Eigen.DenseWork
  ( MutableDenseWork (..),
    denseWorkIndex,
    dotDenseColumns,
    newDenseWork,
    readDenseWork,
    scaleDenseColumn,
    setIdentityDenseWork,
    swapDenseColumns,
    writeDenseWork,
  )
where

import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
  ( MutablePrimArray,
    newPrimArray,
    readPrimArray,
    setPrimArray,
    writePrimArray,
  )
import Moonlight.LinAlg.Internal.Eigen.Kernels (forIndex)
import Prelude

data MutableDenseWork s = MutableDenseWork
  { denseWorkRows :: !Int,
    denseWorkColumns :: !Int,
    denseWorkPayload :: !(MutablePrimArray s Double)
  }

denseWorkIndex :: Int -> Int -> Int -> Int
denseWorkIndex !rowCount !rowIndex !columnIndex = rowIndex + (columnIndex * rowCount)
{-# INLINE denseWorkIndex #-}

newDenseWork :: Int -> Int -> ST s (MutableDenseWork s)
newDenseWork !rowCount !columnCount = do
  payload <- newPrimArray (rowCount * columnCount)
  setPrimArray payload 0 (rowCount * columnCount) 0.0
  pure
    MutableDenseWork
      { denseWorkRows = rowCount,
        denseWorkColumns = columnCount,
        denseWorkPayload = payload
      }
{-# INLINE newDenseWork #-}

readDenseWork :: MutableDenseWork s -> Int -> Int -> ST s Double
readDenseWork (MutableDenseWork rowCount _ payload) !rowIndex !columnIndex =
  readPrimArray payload (denseWorkIndex rowCount rowIndex columnIndex)
{-# INLINE readDenseWork #-}

writeDenseWork :: MutableDenseWork s -> Int -> Int -> Double -> ST s ()
writeDenseWork (MutableDenseWork rowCount _ payload) !rowIndex !columnIndex !entryValue =
  writePrimArray payload (denseWorkIndex rowCount rowIndex columnIndex) entryValue
{-# INLINE writeDenseWork #-}

setIdentityDenseWork :: MutableDenseWork s -> ST s ()
setIdentityDenseWork work@(MutableDenseWork rowCount columnCount payload) = do
  setPrimArray payload 0 (rowCount * columnCount) 0.0
  forIndex 0 (min rowCount columnCount) $ \indexValue ->
    writeDenseWork work indexValue indexValue 1.0
{-# INLINE setIdentityDenseWork #-}

swapDenseColumns :: MutableDenseWork s -> Int -> Int -> ST s ()
swapDenseColumns work@(MutableDenseWork rowCount _ _) !leftColumn !rightColumn =
  if leftColumn == rightColumn
    then pure ()
    else
      forIndex 0 rowCount $ \rowIndex -> do
        leftValue <- readDenseWork work rowIndex leftColumn
        rightValue <- readDenseWork work rowIndex rightColumn
        writeDenseWork work rowIndex leftColumn rightValue
        writeDenseWork work rowIndex rightColumn leftValue
{-# INLINE swapDenseColumns #-}

dotDenseColumns :: MutableDenseWork s -> Int -> Int -> ST s Double
dotDenseColumns work@(MutableDenseWork rowCount _ _) !leftColumn !rightColumn = go 0 0.0
  where
    go !rowIndex !accumulator
      | rowIndex >= rowCount = pure accumulator
      | otherwise = do
          leftValue <- readDenseWork work rowIndex leftColumn
          rightValue <- readDenseWork work rowIndex rightColumn
          go (rowIndex + 1) (accumulator + leftValue * rightValue)
{-# INLINE dotDenseColumns #-}

scaleDenseColumn :: MutableDenseWork s -> Int -> Double -> ST s ()
scaleDenseColumn work@(MutableDenseWork rowCount _ _) !columnIndex !scaleValue =
  forIndex 0 rowCount $ \rowIndex -> do
    entryValue <- readDenseWork work rowIndex columnIndex
    writeDenseWork work rowIndex columnIndex (scaleValue * entryValue)
{-# INLINE scaleDenseColumn #-}

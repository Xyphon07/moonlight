module Moonlight.LinAlg.Internal.Storage
  ( checkFlatLength,
    chunkRows,
    matrixMultiplyList,
    matrixTransposeList,
    matrixZipList,
    matrixMapList,
    unchunkRows,
  )
where

import Data.Bifunctor (first)
import Moonlight.LinAlg.Pure.Dense.Rows (transposeRowsExact)
import Moonlight.LinAlg.Internal.DenseList (dotProductWith)
import Moonlight.Core
  ( AdditiveMonoid (..),
    MoonlightError (..),
    MultiplicativeMonoid (..),
    Semiring,
    checkedNonNegativeProduct,
  )
import Prelude

checkFlatLength :: Int -> Int -> [a] -> Either MoonlightError ()
checkFlatLength rowCount columnCount values
  | rowCount < 0 || columnCount < 0 =
      Left (InvariantViolation "matrix dimensions must be non-negative")
  | otherwise = do
      expectedLength <-
        first
          (const (InvariantViolation "matrix dimensions exceed Int cardinality"))
          (checkedNonNegativeProduct rowCount columnCount)
      if expectedLength /= length values
        then
          Left
            ( InvariantViolation
                ( "flat payload length mismatch: expected "
                    <> show expectedLength
                    <> " values but received "
                    <> show (length values)
                )
            )
        else Right ()

chunkRows :: Int -> [a] -> Either MoonlightError [[a]]
chunkRows columnCount values
  | columnCount <= 0 && not (null values) = Left (InvariantViolation "column count must be positive when payload is non-empty")
  | columnCount <= 0 = Right []
  | otherwise = Right (go values)
  where
    go [] = []
    go rest =
      let (rowValues, nextValues) = splitAt columnCount rest
       in rowValues : go nextValues

unchunkRows :: [[a]] -> [a]
unchunkRows = concat

matrixMapList :: Int -> Int -> (a -> b) -> [a] -> Either MoonlightError [b]
matrixMapList rowCount columnCount fn values =
  checkFlatLength rowCount columnCount values *> pure (map fn values)

matrixZipList ::
  Int ->
  Int ->
  Int ->
  Int ->
  (a -> b -> c) ->
  [a] ->
  [b] ->
  Either MoonlightError [c]
matrixZipList leftRows leftCols rightRows rightCols fn leftValues rightValues
  | leftRows /= rightRows || leftCols /= rightCols =
      Left
        ( InvariantViolation
            ( "matrix shape mismatch: left "
                <> show (leftRows, leftCols)
                <> " right "
                <> show (rightRows, rightCols)
            )
        )
  | otherwise =
      checkFlatLength leftRows leftCols leftValues
        *> checkFlatLength rightRows rightCols rightValues
        *> pure (zipWith fn leftValues rightValues)

matrixTransposeList :: Int -> Int -> [a] -> Either MoonlightError [a]
matrixTransposeList rowCount columnCount values = do
  checkFlatLength rowCount columnCount values
  rows <- chunkRows columnCount values
  unchunkRows <$> transposeRowsExact rows

matrixMultiplyList ::
  Semiring a =>
  Int ->
  Int ->
  Int ->
  Int ->
  [a] ->
  [a] ->
  Either MoonlightError [a]
matrixMultiplyList leftRows leftCols rightRows rightCols leftValues rightValues
  | leftCols /= rightRows =
      Left
        ( InvariantViolation
            ( "matrix multiplication shape mismatch: left "
                <> show (leftRows, leftCols)
                <> " right "
                <> show (rightRows, rightCols)
            )
        )
  | otherwise = do
      checkFlatLength leftRows leftCols leftValues
      checkFlatLength rightRows rightCols rightValues
      leftRowValues <- chunkRows leftCols leftValues
      rightRowValues <- chunkRows rightCols rightValues
      rightColumns <- transposeRowsExact rightRowValues
      productRows <-
        traverse
          ( \rowValues ->
              traverse
                ( \columnValues ->
                    case dotProductWith mul add zero rowValues columnValues of
                      Left err -> Left (InvariantViolation err)
                      Right dotProductValue -> Right dotProductValue
                )
                rightColumns
          )
          leftRowValues
      pure (unchunkRows productRows)

module Moonlight.Homology.Pure.Matrix.Validated
  ( ValidatedMatrix,
    matrixRowCount,
    matrixColumnCount,
    matrixRows,
    mkValidatedMatrix,
    validatedMatrixFromRows,
    zeroValidatedMatrix,
    validatedMatrixFromColumns,
    transposeValidatedMatrix,
    validatedDiagonal,
    validatedColumnAt,
    selectValidatedRows,
    selectValidatedColumns,
    applyValidatedMatrix,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))

type ValidatedMatrix :: Type -> Type
data ValidatedMatrix a = ValidatedMatrix
  { matrixRowCount :: Int,
    matrixColumnCount :: Int,
    matrixRows :: [[a]]
  }
  deriving stock (Eq, Show)

mkValidatedMatrix :: Int -> Int -> [[a]] -> Either HomologyFailure (ValidatedMatrix a)
mkValidatedMatrix rowCount columnCount rows
  | rowCount < 0 || columnCount < 0 =
      Left (InvalidMatrixShape "validated matrix received a negative shape")
  | length rows /= rowCount =
      Left
        ( InvalidMatrixShape
            ( "validated matrix expected "
                <> show rowCount
                <> " rows but received "
                <> show (length rows)
            )
        )
  | not (all ((== columnCount) . length) rows) =
      Left
        ( InvalidMatrixShape
            ( "validated matrix expected every row to have width "
                <> show columnCount
            )
        )
  | otherwise =
      Right
        ValidatedMatrix
          { matrixRowCount = rowCount,
            matrixColumnCount = columnCount,
            matrixRows = rows
          }

validatedMatrixFromRows :: [[a]] -> Either HomologyFailure (ValidatedMatrix a)
validatedMatrixFromRows rows =
  mkValidatedMatrix
    (length rows)
    (inferredColumnCount rows)
    rows

zeroValidatedMatrix :: Num a => Int -> Int -> Either HomologyFailure (ValidatedMatrix a)
zeroValidatedMatrix rowCount columnCount =
  mkValidatedMatrix
    rowCount
    columnCount
    (replicate rowCount (replicate columnCount 0))

validatedMatrixFromColumns :: Int -> [[a]] -> Either HomologyFailure (ValidatedMatrix a)
validatedMatrixFromColumns rowCount columnVectors = do
  _ <- traverse (validateVectorDimension rowCount) columnVectors
  mkValidatedMatrix
    rowCount
    (length columnVectors)
    ( if null columnVectors
        then replicate rowCount []
        else List.transpose columnVectors
    )

transposeValidatedMatrix :: ValidatedMatrix a -> ValidatedMatrix a
transposeValidatedMatrix matrixValue =
  ValidatedMatrix
    { matrixRowCount = matrixColumnCount matrixValue,
      matrixColumnCount = matrixRowCount matrixValue,
      matrixRows =
        if matrixRowCount matrixValue == 0
          then replicate (matrixColumnCount matrixValue) []
          else List.transpose (matrixRows matrixValue)
    }

validatedDiagonal :: ValidatedMatrix a -> [a]
validatedDiagonal matrixValue =
  matrixRows matrixValue
    & zip [0 :: Int ..]
    & mapMaybe (\(indexValue, rowValue) -> safeElementAt indexValue rowValue)

validatedColumnAt ::
  Int ->
  ValidatedMatrix a ->
  Either HomologyFailure [a]
validatedColumnAt columnIndexValue matrixValue
  | columnIndexValue < 0 || columnIndexValue >= matrixColumnCount matrixValue =
      Left (InvalidMatrixShape "column selection index is outside the matrix bounds")
  | otherwise =
      -- Every row is validated to width 'matrixColumnCount', so after the
      -- bounds check the element exists in each row; for a zero-row matrix
      -- the column is correctly []. (Going through 'List.transpose' here
      -- loses legal columns of 0×n matrices: transpose [] = [].)
      Right (mapMaybe (safeElementAt columnIndexValue) (matrixRows matrixValue))

selectValidatedRows ::
  [Int] ->
  ValidatedMatrix a ->
  Either HomologyFailure (ValidatedMatrix a)
selectValidatedRows rowIndices matrixValue = do
  selectedRows <-
    traverse
      ( \rowIndexValue ->
          maybe
            (Left (InvalidMatrixShape "row selection index is outside the matrix bounds"))
            Right
            (safeElementAt rowIndexValue (matrixRows matrixValue))
      )
      rowIndices
  mkValidatedMatrix
    (length selectedRows)
    (matrixColumnCount matrixValue)
    selectedRows

selectValidatedColumns ::
  [Int] ->
  ValidatedMatrix a ->
  Either HomologyFailure (ValidatedMatrix a)
selectValidatedColumns columnIndices matrixValue = do
  selectedRows <-
    traverse
      ( \rowValue ->
          traverse
            ( \columnIndexValue ->
                maybe
                  (Left (InvalidMatrixShape "column selection index is outside the matrix bounds"))
                  Right
                  (safeElementAt columnIndexValue rowValue)
            )
            columnIndices
      )
      (matrixRows matrixValue)
  mkValidatedMatrix
    (matrixRowCount matrixValue)
    (length columnIndices)
    selectedRows

applyValidatedMatrix ::
  Num a =>
  ValidatedMatrix a ->
  [a] ->
  Either HomologyFailure [a]
applyValidatedMatrix matrixValue vectorValue = do
  _ <- validateVectorDimension (matrixColumnCount matrixValue) vectorValue
  pure
    ( matrixRows matrixValue
        & fmap (\rowValue -> sum (zipWith (*) rowValue vectorValue))
    )

safeElementAt :: Int -> [a] -> Maybe a
safeElementAt indexValue _
  | indexValue < 0 = Nothing
safeElementAt indexValue values =
  case drop indexValue values of
    value : _ -> Just value
    [] -> Nothing

validateVectorDimension :: Int -> [a] -> Either HomologyFailure [a]
validateVectorDimension expectedDimension vectorValue =
  if length vectorValue == expectedDimension
    then Right vectorValue
    else
      Left
        ( InvalidMatrixShape
            ( "vector length "
                <> show (length vectorValue)
                <> " does not match the expected matrix width "
                <> show expectedDimension
            )
        )

inferredColumnCount :: [[a]] -> Int
inferredColumnCount rows =
  case rows of
    rowValue : _ -> length rowValue
    [] -> 0

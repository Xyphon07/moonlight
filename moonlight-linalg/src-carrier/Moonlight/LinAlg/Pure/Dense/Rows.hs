{-# LANGUAGE DerivingStrategies #-}

-- | Validated rectangular row authoring surface.
--
-- `DenseRows` exists to seal rectangular nested-list input and return precise
-- shape errors. It is deliberately not the hot dense-storage owner; use vector,
-- sparse, tridiagonal, or native kernels for benchmark-sensitive work.
module Moonlight.LinAlg.Pure.Dense.Rows
  ( DenseRows,
    mkDenseRows,
    mkDenseRowsWithShape,
    mkDenseRowsFromFlat,
    denseRowsShape,
    denseRowsToLists,
    transposeRowsExact,
    zipRowsExactWith,
    matrixVectorProductRowsWith,
    matrixProductRowsWith,
    hcatRowsExact,
    vcatRowsExact,
  )
where

import Data.Bifunctor (first)
import Data.Kind (Type)
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
  )
import Prelude

type DenseRows :: Type -> Type
data DenseRows a = DenseRows
  { denseRowCount :: !Int,
    denseColumnCount :: !Int,
    denseRowsData :: ![[a]]
  }
  deriving stock (Eq, Show)

mkDenseRows :: [[a]] -> Either MoonlightError (DenseRows a)
mkDenseRows rowValues =
  case rowValues of
    [] ->
      Right (DenseRows 0 0 [])
    firstRow : _ ->
      mkDenseRowsWithShape
        (length rowValues)
        (length firstRow)
        rowValues

mkDenseRowsWithShape :: Int -> Int -> [[a]] -> Either MoonlightError (DenseRows a)
mkDenseRowsWithShape expectedRowCount expectedColumnCount rowValues
  | expectedRowCount < 0 =
      Left
        ( InvariantViolation
            ( "dense row matrix row count must be non-negative, received "
                <> show expectedRowCount
            )
        )
  | expectedColumnCount < 0 =
      Left
        ( InvariantViolation
            ( "dense row matrix column count must be non-negative, received "
                <> show expectedColumnCount
            )
        )
  | actualRowCount /= expectedRowCount =
      Left
        ( InvariantViolation
            ( "dense row matrix row count mismatch: expected "
                <> show expectedRowCount
                <> " rows but received "
                <> show actualRowCount
            )
        )
  | otherwise =
      case firstMismatchedRowWidth expectedColumnCount rowValues of
        Nothing ->
          Right
            DenseRows
              { denseRowCount = expectedRowCount,
                denseColumnCount = expectedColumnCount,
                denseRowsData = rowValues
              }
        Just (rowIndex, actualColumnCount) ->
          Left
            ( InvariantViolation
                ( "dense row matrix is ragged at row "
                    <> show rowIndex
                    <> " (expected "
                    <> show expectedColumnCount
                    <> " columns, got "
                    <> show actualColumnCount
                    <> ")"
                )
            )
  where
    actualRowCount = length rowValues

firstMismatchedRowWidth :: Int -> [[a]] -> Maybe (Int, Int)
firstMismatchedRowWidth expectedColumnCount =
  foldr firstMismatch Nothing . zip [0 :: Int ..]
  where
    firstMismatch (rowIndex, rowValues) remainingMismatch =
      let actualColumnCount = length rowValues
       in if actualColumnCount == expectedColumnCount
            then remainingMismatch
            else Just (rowIndex, actualColumnCount)

mkDenseRowsFromFlat :: Int -> Int -> [a] -> Either MoonlightError (DenseRows a)
mkDenseRowsFromFlat rowCount columnCount values
  | rowCount < 0 || columnCount < 0 =
      Left (InvariantViolation "dense row matrix dimensions must be non-negative")
  | otherwise = do
      expectedLength <-
        first
          (const (InvariantViolation "dense row matrix dimensions exceed Int cardinality"))
          (checkedNonNegativeProduct rowCount columnCount)
      if expectedLength /= length values
        then
          Left
            ( InvariantViolation
                ( "dense row flat payload length mismatch: expected "
                    <> show expectedLength
                    <> " values but received "
                    <> show (length values)
                )
            )
        else
          if columnCount == 0
            then mkDenseRowsWithShape rowCount columnCount (replicate rowCount [])
            else mkDenseRowsWithShape rowCount columnCount (flatRows rowCount columnCount values)

flatRows :: Int -> Int -> [a] -> [[a]]
flatRows remainingRows columnCount values
  | remainingRows <= 0 = []
  | otherwise =
      let (rowValues, restValues) = splitAt columnCount values
       in rowValues : flatRows (remainingRows - 1) columnCount restValues

denseRowsShape :: DenseRows a -> (Int, Int)
denseRowsShape denseRowsValue =
  (denseRowCount denseRowsValue, denseColumnCount denseRowsValue)

denseRowsToLists :: DenseRows a -> [[a]]
denseRowsToLists = denseRowsData

transposeRowsExact :: [[a]] -> Either MoonlightError [[a]]
transposeRowsExact =
  fmap (denseRowsToLists . transposeDenseRows) . mkDenseRows

zipRowsExactWith :: (left -> right -> result) -> [[left]] -> [[right]] -> Either MoonlightError [[result]]
zipRowsExactWith combine leftRows rightRows = do
  leftDenseRows <- mkDenseRows leftRows
  rightDenseRows <- mkDenseRows rightRows
  denseRowsToLists <$> zipDenseRowsWith combine leftDenseRows rightDenseRows

matrixVectorProductRowsWith ::
  (entry -> value -> product) ->
  (product -> accumulator -> accumulator) ->
  accumulator ->
  [[entry]] ->
  [value] ->
  Either MoonlightError [accumulator]
matrixVectorProductRowsWith multiply append zeroValue rowValues vectorValue =
  do
    denseRowsValue <- mkDenseRows rowValues
    if length vectorValue /= denseColumnCount denseRowsValue
      then
        Left
          ( InvariantViolation
              ( "dense row matrix/vector shape mismatch (matrix="
                  <> show (denseRowsShape denseRowsValue)
                  <> ", vector="
                  <> show (length vectorValue)
                  <> ")"
              )
          )
      else
        Right
          ( (\rowValue -> trustedDotProductWith multiply append zeroValue rowValue vectorValue)
              <$> denseRowsData denseRowsValue
          )

matrixProductRowsWith ::
  (left -> right -> product) ->
  (product -> accumulator -> accumulator) ->
  accumulator ->
  [[left]] ->
  [[right]] ->
  Either MoonlightError [[accumulator]]
matrixProductRowsWith multiply append zeroValue leftRows rightRows = do
  leftDenseRows <- mkDenseRows leftRows
  rightDenseRows <- mkDenseRows rightRows
  if denseColumnCount leftDenseRows /= denseRowCount rightDenseRows
    then
      Left
        ( InvariantViolation
            ( "dense row matrix product shape mismatch (left="
                <> show (denseRowsShape leftDenseRows)
                <> ", right="
                <> show (denseRowsShape rightDenseRows)
                <> ")"
            )
        )
    else
      let rightColumns = denseRowsData (transposeDenseRows rightDenseRows)
       in Right
            ( (\leftRow -> trustedDotProductWith multiply append zeroValue leftRow <$> rightColumns)
                <$> denseRowsData leftDenseRows
            )

trustedDotProductWith ::
  (left -> right -> product) ->
  (product -> accumulator -> accumulator) ->
  accumulator ->
  [left] ->
  [right] ->
  accumulator
trustedDotProductWith multiply append zeroValue left right =
  foldr append zeroValue (zipWith multiply left right)
{-# INLINE trustedDotProductWith #-}

hcatRowsExact :: [[[a]]] -> Either MoonlightError [[a]]
hcatRowsExact rowMatrices = do
  denseRowMatrices <- traverse mkDenseRows rowMatrices
  case denseRowMatrices of
    [] ->
      Right []
    firstDenseRows : remainingDenseRows ->
      if all (\denseRowsValue -> denseRowCount denseRowsValue == denseRowCount firstDenseRows) remainingDenseRows
        then
          pure
            ( foldr
                (zipWith (++))
                (replicate (denseRowCount firstDenseRows) [])
                (map denseRowsData denseRowMatrices)
            )
        else
          Left
            ( InvariantViolation
                ( "dense horizontal concatenation requires equal row counts, got "
                    <> show (map denseRowsShape denseRowMatrices)
                )
            )

vcatRowsExact :: [[[a]]] -> Either MoonlightError [[a]]
vcatRowsExact rowMatrices = do
  denseRowMatrices <- traverse mkDenseRows rowMatrices
  case denseRowMatrices of
    [] ->
      Right []
    firstDenseRows : remainingDenseRows ->
      if all (\denseRowsValue -> denseColumnCount denseRowsValue == denseColumnCount firstDenseRows) remainingDenseRows
        then pure (denseRowMatrices >>= denseRowsData)
        else
          Left
            ( InvariantViolation
                ( "dense vertical concatenation requires equal column counts, got "
                    <> show (map denseRowsShape denseRowMatrices)
                )
            )

transposeDenseRows :: DenseRows a -> DenseRows a
transposeDenseRows denseRowsValue =
  DenseRows
    { denseRowCount = denseColumnCount denseRowsValue,
      denseColumnCount = denseRowCount denseRowsValue,
      denseRowsData = foldr (zipWith (:)) (replicate (denseColumnCount denseRowsValue) []) (denseRowsData denseRowsValue)
    }

zipDenseRowsWith :: (left -> right -> result) -> DenseRows left -> DenseRows right -> Either MoonlightError (DenseRows result)
zipDenseRowsWith combine leftDenseRows rightDenseRows
  | denseRowsShape leftDenseRows /= denseRowsShape rightDenseRows =
      Left
        ( InvariantViolation
            ( "dense row matrix zip shape mismatch (left="
                <> show (denseRowsShape leftDenseRows)
                <> ", right="
                <> show (denseRowsShape rightDenseRows)
                <> ")"
            )
        )
  | otherwise =
      Right
        ( DenseRows
          { denseRowCount = denseRowCount leftDenseRows,
            denseColumnCount = denseColumnCount leftDenseRows,
            denseRowsData =
              zipWith
                (zipWith combine)
                (denseRowsData leftDenseRows)
                (denseRowsData rightDenseRows)
          }
        )

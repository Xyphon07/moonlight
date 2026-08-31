module Moonlight.LinAlg.Internal.Backend.RowOps
  ( swapAt,
    identityRows,
    permutationRows,
    swapLowerPrefix,
    rowEliminate,
    findPivotRow,
  )
where

import Data.List (findIndex)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    Field (..),
    MoonlightError (..),
    MultiplicativeMonoid (..),
  )
import Moonlight.LinAlg.Internal.Primitives
  ( ColumnIndex,
    MatrixIndex,
    RowIndex,
    replaceRowChecked,
    requireColumnEntry,
    requireRow,
    rowIndices,
    rowIndexInt,
    selectAt,
    selectAtIndex,
    swapAtIndexChecked,
  )
import Prelude

swapAt :: MatrixIndex axis -> MatrixIndex axis -> [a] -> Either MoonlightError [a]
swapAt leftIndex rightIndex values =
  swapAtIndexChecked
    (InvariantViolation ("row operation swap out of bounds at indices " <> show (leftIndex, rightIndex)))
    leftIndex
    rightIndex
    values

identityRows :: (AdditiveGroup a, MultiplicativeMonoid a) => Int -> [[a]]
identityRows size =
  map
    (\rowIndex -> map (\columnIndex -> if rowIndex == columnIndex then one else zero) [0 .. size - 1])
    [0 .. size - 1]

permutationRows :: (AdditiveGroup a, MultiplicativeMonoid a) => [Int] -> [[a]]
permutationRows permutationIndices =
  map
    (\sourceRowIndex -> map (\columnIndex -> if columnIndex == sourceRowIndex then one else zero) [0 .. length permutationIndices - 1])
    permutationIndices

swapLowerPrefix :: Int -> RowIndex -> RowIndex -> [[a]] -> Either MoonlightError [[a]]
swapLowerPrefix prefixLength leftIndex rightIndex rows =
  requireRow
    (InvariantViolation ("row-prefix swap left row missing at index " <> show leftIndex))
    leftIndex
    rows
    >>= \leftRow ->
      requireRow
        (InvariantViolation ("row-prefix swap right row missing at index " <> show rightIndex))
        rightIndex
        rows
        >>= \rightRow ->
          let swappedLeft = take prefixLength rightRow <> drop prefixLength leftRow
              swappedRight = take prefixLength leftRow <> drop prefixLength rightRow
           in replaceRowChecked
                (InvariantViolation ("row-prefix swap could not replace left row at index " <> show leftIndex))
                leftIndex
                swappedLeft
                rows
                >>= replaceRowChecked
                  (InvariantViolation ("row-prefix swap could not replace right row at index " <> show rightIndex))
                  rightIndex
                  swappedRight

rowEliminate :: (Field a, Eq a) => [a] -> [a] -> ColumnIndex -> Either MoonlightError [a]
rowEliminate pivotRow rowValues pivotColumn =
  requireColumnEntry
    (InvariantViolation ("row elimination missing pivot column " <> show pivotColumn))
    pivotColumn
    rowValues
    >>= \factor ->
      if factor == zero
        then Right rowValues
        else Right (zipWith (\entry pivotEntry -> entry `sub` (factor `mul` pivotEntry)) rowValues pivotRow)

findPivotRow :: Field a => RowIndex -> ColumnIndex -> [[a]] -> Either MoonlightError (Maybe RowIndex)
findPivotRow pivotRow pivotColumn rows =
  fmap
    (\candidateFlags ->
        findIndex id candidateFlags
          >>= \offsetIndex ->
            selectAt (rowIndexInt pivotRow + offsetIndex) (rowIndices (length rows))
    )
    (traverse candidateFlag (drop (rowIndexInt pivotRow) rows))
  where
    candidateFlag rowValues =
      case selectAtIndex pivotColumn rowValues of
        Nothing -> Left (InvariantViolation ("pivot search missing column " <> show pivotColumn))
        Just entryValue
          | not (fieldValueValid entryValue) ->
              Left (InvariantViolation ("pivot search encountered an invalid field value at column " <> show pivotColumn))
          | otherwise -> Right (canInvert entryValue)

module Moonlight.LinAlg.Internal.Backend.Elimination
  ( EliminationScope (..),
    PivotResult (..),
    EliminationState (..),
    EliminationConfig (..),
    runElimination,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Backend.RowOps (swapAt)
import Moonlight.LinAlg.Internal.Primitives
  ( ColumnIndex,
    RowIndex,
    mkRowIndex,
    replaceRowChecked,
    requireRow,
    rowIndexInt,
    rowIndices,
  )
import Prelude

type EliminationScope :: Type
data EliminationScope
  = ForwardOnly
  | FullReduction

type PivotResult :: Type
data PivotResult
  = PivotFound RowIndex
  | NoPivotSkip
  | NoPivotFail

type EliminationState :: Type -> Type -> Type
data EliminationState a s = EliminationState
  { elimRows :: [[a]],
    elimSide :: s,
    elimPivots :: [ColumnIndex]
  }

type EliminationConfig :: Type -> Type -> Type
data EliminationConfig a s = EliminationConfig
  { elimSelectPivot :: Int -> ColumnIndex -> [[a]] -> Either MoonlightError PivotResult,
    elimCandidateColumns :: Int -> [ColumnIndex] -> [ColumnIndex],
    elimNormalizePivot :: RowIndex -> ColumnIndex -> [[a]] -> Either MoonlightError [[a]],
    elimScope :: EliminationScope,
    elimReduceRow :: [a] -> [a] -> ColumnIndex -> RowIndex -> RowIndex -> s -> Either MoonlightError ([a], s),
    elimOnSwap :: RowIndex -> RowIndex -> s -> Either MoonlightError s,
    elimMaxSteps :: Int
  }

runElimination ::
  EliminationConfig a s ->
  [[a]] ->
  s ->
  [ColumnIndex] ->
  Either MoonlightError (EliminationState a s)
runElimination config initialRows initialSide initialColumns =
  go 0 initialRows initialSide initialColumns []
  where
    go step rows side remainingCols pivotsSoFar
      | step >= elimMaxSteps config =
          Right (EliminationState rows side (reverse pivotsSoFar))
      | otherwise =
          tryColumns step rows side remainingCols pivotsSoFar (elimCandidateColumns config step remainingCols)

    tryColumns _step rows side _remainingCols pivotsSoFar [] =
      Right (EliminationState rows side (reverse pivotsSoFar))
    tryColumns step rows side remainingCols pivotsSoFar (col : moreCols) = do
      pivotResult <- elimSelectPivot config step col rows
      case pivotResult of
        NoPivotFail ->
          Left (InvariantViolation ("elimination failed: no pivot at step " <> show step))
        NoPivotSkip ->
          tryColumns step rows side remainingCols pivotsSoFar moreCols
        PivotFound sourceRow -> do
          let targetRowInt = step
              rowCount = length rows
          targetRow <- targetRowIndex rowCount targetRowInt
          swappedRows <- swapAt targetRow sourceRow rows
          swappedSide <- elimOnSwap config targetRow sourceRow side
          normalizedRows <- elimNormalizePivot config targetRow col swappedRows
          pivotRowValues <-
            requireRow
              (InvariantViolation ("elimination pivot row missing at index " <> show targetRow))
              targetRow
              normalizedRows
          let targetIndices = case elimScope config of
                ForwardOnly -> drop (step + 1) (rowIndices rowCount)
                FullReduction -> filter (\ri -> rowIndexInt ri /= targetRowInt) (rowIndices rowCount)
          (eliminatedRows, finalSide) <-
            foldM
              ( \(currentRows, currentSide) ri -> do
                  targetRowValues <-
                    requireRow
                      (InvariantViolation ("elimination target row missing at index " <> show ri))
                      ri
                      currentRows
                  (reducedRow, nextSide) <- elimReduceRow config pivotRowValues targetRowValues col targetRow ri currentSide
                  nextRows <-
                    replaceRowChecked
                      (InvariantViolation ("elimination row replacement failed at index " <> show ri))
                      ri
                      reducedRow
                      currentRows
                  Right (nextRows, nextSide)
              )
              (normalizedRows, swappedSide)
              targetIndices
          let nextCols = dropWhile (<= col) remainingCols
          go (step + 1) eliminatedRows finalSide nextCols (col : pivotsSoFar)

    targetRowIndex rowCount idx =
      mkRowIndex
        (InvariantViolation ("elimination target row index out of bounds: " <> show idx))
        rowCount
        idx

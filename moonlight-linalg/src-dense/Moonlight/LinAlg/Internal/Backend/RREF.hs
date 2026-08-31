{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Internal.Backend.RREF
  ( RREF (..),
    KernelBasis (..),
    rrefRowsFrom,
    rrefFromMatrix,
    rankPure,
    kernelPure,
  )
where

import Data.Kind (Type)
import GHC.TypeNats (KnownNat, Nat)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    Field (..),
    MoonlightError (..),
    MultiplicativeMonoid (..),
    requireInvertible,
  )
import Moonlight.LinAlg.Internal.Backend.Elimination
  ( EliminationConfig (..),
    EliminationScope (..),
    EliminationState (..),
    PivotResult (..),
    runElimination,
  )
import Moonlight.LinAlg.Internal.Backend.RowOps
  ( findPivotRow,
  )
import Moonlight.LinAlg.Internal.Primitives
  ( ColumnIndex,
    RowIndex,
    columnIndexInt,
    columnIndices,
    mkRowIndex,
    replaceAt,
    requireColumnEntry,
    requireRow,
    rowIndexInt,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    Vector,
    fromListVector,
  )
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

type RREF :: Type -> Type -> Type
data RREF pivot a = RREF
  { rrefPivotColumns :: [pivot],
    rrefRows :: [[a]]
  }
  deriving stock (Eq, Show)

type KernelBasis :: Nat -> Type -> Type
newtype KernelBasis c a = KernelBasis
  { kernelBasisVectors :: [Vector c a]
  }

rrefConfigAt ::
  (Field a, Eq a) =>
  Int ->
  Int ->
  EliminationConfig a ()
rrefConfigAt rowCount pivotRow =
  EliminationConfig
    { elimSelectPivot = \step col rs -> do
        pivotRowIndex <-
          mkRowIndex
            (InvariantViolation ("RREF pivot row out of bounds at index " <> show (pivotRow + step)))
            rowCount
            (pivotRow + step)
        findPivotRow pivotRowIndex col rs >>= \maybePivot ->
          case maybePivot of
            Nothing -> Right NoPivotSkip
            Just pivotIndex -> Right (PivotFound pivotIndex),
      elimCandidateColumns = \_ cols -> cols,
      elimNormalizePivot = rrefNormalizePivot,
      elimScope = FullReduction,
      elimReduceRow = rrefReduceRow,
      elimOnSwap = \_ _ s -> Right s,
      elimMaxSteps = rowCount - pivotRow
    }

rrefNormalizePivot ::
  (Field a) =>
  RowIndex ->
  ColumnIndex ->
  [[a]] ->
  Either MoonlightError [[a]]
rrefNormalizePivot pivotRow pivotColumn rows = do
  pivotRowValues <-
    requireRow
      (InvariantViolation ("RREF pivot row missing after swap at row " <> show pivotRow))
      pivotRow
      rows
  pivotValue <-
    requireColumnEntry
      (InvariantViolation ("RREF pivot column missing at column " <> show pivotColumn))
      pivotColumn
      pivotRowValues
  pivotInverse <-
    requireInvertible
      (InvariantViolation ("RREF pivot at column " <> show pivotColumn <> " is not invertible"))
      pivotValue
  let normalizedRow = map (\entry -> entry `mul` pivotInverse) pivotRowValues
  Right (replaceAt (rowIndexInt pivotRow) normalizedRow rows)

rrefReduceRow ::
  (Field a, Eq a) =>
  [a] ->
  [a] ->
  ColumnIndex ->
  RowIndex ->
  RowIndex ->
  () ->
  Either MoonlightError ([a], ())
rrefReduceRow pivotRowValues targetRowValues pivotColumn _ _ sideState = do
  factorEntry <-
    requireColumnEntry
      (InvariantViolation ("RREF elimination missing pivot column " <> show pivotColumn))
      pivotColumn
      targetRowValues
  if factorEntry == zero
    then Right (targetRowValues, sideState)
    else Right (zipWith (\entry pivotEntry -> entry `sub` (factorEntry `mul` pivotEntry)) targetRowValues pivotRowValues, sideState)

rrefRowsFrom :: (Field a, Eq a) => Int -> Int -> Int -> [[a]] -> Either MoonlightError (RREF Int a)
rrefRowsFrom rowCount columnCount pivotRow rows
  | pivotRow < 0 = Left (InvariantViolation ("RREF pivot row cannot be negative: " <> show pivotRow))
  | otherwise = do
      result <-
        runElimination
          (rrefConfigAt rowCount pivotRow)
          rows
          ()
          (columnIndices columnCount)
      Right
        RREF
          { rrefPivotColumns = map columnIndexInt (elimPivots result),
            rrefRows = elimRows result
          }

rrefFromMatrixIndexed ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, Field a) =>
  Matrix r c a ->
  Either MoonlightError (RREF ColumnIndex a)
rrefFromMatrixIndexed matrixValue = do
  rows <- DenseTypes.matrixToRows matrixValue
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
  result <-
    runElimination
      (rrefConfigAt rowCount 0)
      rows
      ()
      (columnIndices columnCount)
  Right
    RREF
      { rrefPivotColumns = elimPivots result,
        rrefRows = elimRows result
      }

rrefFromMatrix ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, Field a) =>
  Matrix r c a ->
  Either MoonlightError (RREF Int a)
rrefFromMatrix matrixValue =
  fmap
    ( \rrefValue ->
        RREF
          { rrefPivotColumns = map columnIndexInt (rrefPivotColumns rrefValue),
            rrefRows = rrefRows rrefValue
          }
    )
    (rrefFromMatrixIndexed matrixValue)

rankPure ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, Field a) =>
  Matrix r c a ->
  Either MoonlightError Int
rankPure matrixValue =
  fmap (length . rrefPivotColumns) (rrefFromMatrixIndexed matrixValue)

kernelPure ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, Field a) =>
  Matrix r c a ->
  Either MoonlightError (KernelBasis c a)
kernelPure matrixValue = do
  rrefValue <- rrefFromMatrixIndexed matrixValue
  let (_, columnCount) = DenseTypes.matrixShape matrixValue
      allColumns = columnIndices columnCount
      pivotColumns = rrefPivotColumns rrefValue
      reducedRows = rrefRows rrefValue
      pivotRows = zip pivotColumns reducedRows
      freeColumns = filter (\columnIndex -> columnIndex `notElem` pivotColumns) allColumns
      basisVector freeColumn =
        let entryAt columnIndex =
              if columnIndex == freeColumn
                then one
                else
                  case lookup columnIndex pivotRows of
                    Nothing -> zero
                    Just pivotRowValues ->
                      maybe zero neg (lookup freeColumn (zip allColumns pivotRowValues))
         in fromListVector (map entryAt allColumns)
  KernelBasis <$> traverse basisVector freeColumns

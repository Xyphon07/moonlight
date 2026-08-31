{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Internal.Backend.PLU
  ( PLU (..),
    pluDecompPure,
  )
where

import Data.Kind (Type)
import GHC.TypeNats (KnownNat, Nat)
import Moonlight.Core
  ( AdditiveGroup (..),
    Field (..),
    MoonlightError (..),
    MultiplicativeMonoid (..),
    requireInvertible,
  )
import Moonlight.Core (note, safeIndex)
import Moonlight.LinAlg.Internal.Backend.Elimination
  ( EliminationConfig (..),
    EliminationScope (..),
    EliminationState (..),
    PivotResult (..),
    runElimination,
  )
import Moonlight.LinAlg.Internal.Backend.RowOps
  ( findPivotRow,
    identityRows,
    permutationRows,
    swapLowerPrefix,
  )
import Moonlight.LinAlg.Internal.Primitives
  ( ColumnIndex,
    RowIndex,
    columnIndices,
    mkRowIndex,
    replaceColumnEntryChecked,
    replaceRowChecked,
    requireColumnEntry,
    requireRow,
    rowIndexInt,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    fromListMatrix,
  )
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

type PLU :: Nat -> Nat -> Type -> Type
data PLU r c a = PLU
  { pluPermutation :: Matrix r r a,
    pluLower :: Matrix r r a,
    pluUpper :: Matrix r c a
  }

type PLUSideState :: Type -> Type
data PLUSideState a = PLUSideState
  { pluSidePermutation :: [Int],
    pluSideLower :: [[a]],
    pluSideStep :: Int
  }

pluConfig ::
  Field a =>
  Int ->
  Int ->
  EliminationConfig a (PLUSideState a)
pluConfig rowCount columnCount =
  EliminationConfig
    { elimSelectPivot = pluSelectPivot rowCount,
      elimCandidateColumns = \_ cols -> take 1 cols,
      elimNormalizePivot = \_ _ rows -> Right rows,
      elimScope = ForwardOnly,
      elimReduceRow = pluReduceRow,
      elimOnSwap = pluOnSwap,
      elimMaxSteps = min rowCount columnCount
    }

pluSelectPivot ::
  Field a =>
  Int ->
  Int ->
  ColumnIndex ->
  [[a]] ->
  Either MoonlightError PivotResult
pluSelectPivot rowCount step col rows = do
  pivotRowIndex <-
    mkRowIndex
      (InvariantViolation ("PLU pivot row out of bounds at index " <> show step))
      rowCount
      step
  findPivotRow pivotRowIndex col rows >>= \maybePivot ->
    case maybePivot of
      Nothing ->
        Right NoPivotFail
      Just pivotIndex ->
        Right (PivotFound pivotIndex)

pluOnSwap ::
  RowIndex ->
  RowIndex ->
  PLUSideState a ->
  Either MoonlightError (PLUSideState a)
pluOnSwap targetRow sourceRow sideState = do
  let step = pluSideStep sideState
  swappedPermutation <-
    swapPermutationAt targetRow sourceRow (pluSidePermutation sideState)
  swappedLower <- swapLowerPrefix step targetRow sourceRow (pluSideLower sideState)
  Right (sideState {pluSidePermutation = swappedPermutation, pluSideLower = swappedLower, pluSideStep = step + 1})

pluReduceRow ::
  (Field a) =>
  [a] ->
  [a] ->
  ColumnIndex ->
  RowIndex ->
  RowIndex ->
  PLUSideState a ->
  Either MoonlightError ([a], PLUSideState a)
pluReduceRow pivotRowValues targetRowValues pivotColumn _pivotRow targetRow sideState = do
  pivotValue <-
    requireColumnEntry
      (InvariantViolation ("PLU pivot entry missing at column " <> show pivotColumn))
      pivotColumn
      pivotRowValues
  pivotInverse <-
    requireInvertible
      (InvariantViolation ("PLU decomposition failed: pivot is not invertible at column " <> show pivotColumn))
      pivotValue
  factorEntry <-
    requireColumnEntry
      (InvariantViolation ("PLU factor entry missing at pivot column " <> show pivotColumn))
      pivotColumn
      targetRowValues
  let factor = factorEntry `mul` pivotInverse
      updatedRow = zipWith (\entry pivotEntry -> entry `sub` (factor `mul` pivotEntry)) targetRowValues pivotRowValues
  currentLRow <-
    requireRow
      (InvariantViolation ("PLU lower row missing at index " <> show targetRow))
      targetRow
      (pluSideLower sideState)
  updatedLRow <-
    replaceColumnEntryChecked
      (InvariantViolation ("PLU lower factor placement failed at row " <> show targetRow <> ", column " <> show pivotColumn))
      pivotColumn
      factor
      currentLRow
  updatedLRows <-
    replaceRowChecked
      (InvariantViolation ("PLU lower row replacement failed at index " <> show targetRow))
      targetRow
      updatedLRow
      (pluSideLower sideState)
  Right (updatedRow, sideState {pluSideLower = updatedLRows})

swapPermutationAt :: RowIndex -> RowIndex -> [Int] -> Either MoonlightError [Int]
swapPermutationAt targetRow sourceRow permIndices = do
  let targetIdx = rowIndexInt targetRow
      sourceIdx = rowIndexInt sourceRow
  if targetIdx == sourceIdx
    then Right permIndices
    else do
      targetVal <-
        note (InvariantViolation ("PLU permutation swap out of bounds at index " <> show targetIdx))
          (safeIndex targetIdx permIndices)
      sourceVal <-
        note (InvariantViolation ("PLU permutation swap out of bounds at index " <> show sourceIdx))
          (safeIndex sourceIdx permIndices)
      Right (replaceAtPure sourceIdx targetVal (replaceAtPure targetIdx sourceVal permIndices))

replaceAtPure :: Int -> a -> [a] -> [a]
replaceAtPure idx val xs =
  zipWith (\i x -> if i == idx then val else x) [0 :: Int ..] xs

pluDecompPure ::
  forall r c a.
  (KnownNat r, KnownNat c, Field a) =>
  Matrix r c a ->
  Either MoonlightError (PLU r c a)
pluDecompPure matrixValue = do
  initialRows <- DenseTypes.matrixToRows matrixValue
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
      initialSide =
        PLUSideState
          { pluSidePermutation = [0 .. rowCount - 1],
            pluSideLower = identityRows rowCount,
            pluSideStep = 0
          }
  eliminationResult <-
    runElimination
      (pluConfig rowCount columnCount)
      initialRows
      initialSide
      (columnIndices columnCount)
  let finalSide = elimSide eliminationResult
  pMatrix <- fromListMatrix @r @r (concat (permutationRows (pluSidePermutation finalSide)))
  lMatrix <- fromListMatrix @r @r (concat (pluSideLower finalSide))
  uMatrix <- fromListMatrix @r @c (concat (elimRows eliminationResult))
  pure
    PLU
      { pluPermutation = pMatrix,
        pluLower = lMatrix,
        pluUpper = uMatrix
      }

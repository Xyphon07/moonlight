{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( finiteDouble,
    validateSparseSystemInput,
    validateSparseSolverConfiguration,
    sparseDiagonal,
    solverEpsilon,
    shiftedDiagonalValue,
  )
where

import Data.Vector.Unboxed qualified as U
import Moonlight.Core (fieldValueValid)
import Moonlight.LinAlg.Pure.Sparse.Solver.Types (SparseIterativeFailure (..))
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    csrCols,
    csrColumnIndicesVector,
    csrRows,
    csrRowOffsetsVector,
    csrValuesVector,
  )
import Prelude

finiteDouble :: Double -> Bool
finiteDouble = fieldValueValid
{-# INLINE finiteDouble #-}

validateSparseSystemInput ::
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure ()
validateSparseSystemInput sparseMatrix rhsValues initialGuess
  | dimension /= csrCols sparseMatrix =
      Left (SparseInvalidInput "sparse iterative solver expects a square system matrix")
  | U.length rhsValues /= dimension =
      Left (SparseInvalidInput "sparse iterative solver expects RHS length equal to matrix dimension")
  | U.length initialGuess /= dimension =
      Left (SparseInvalidInput "sparse iterative solver expects initial guess length equal to matrix dimension")
  | U.any (not . fieldValueValid) (csrValuesVector sparseMatrix) =
      Left (SparseInvalidInput "sparse iterative solver matrix entries must be finite")
  | U.any (not . fieldValueValid) rhsValues =
      Left (SparseInvalidInput "sparse iterative solver right-hand side must be finite")
  | U.any (not . fieldValueValid) initialGuess =
      Left (SparseInvalidInput "sparse iterative solver initial guess must be finite")
  | otherwise = Right ()
  where
    dimension = csrRows sparseMatrix

validateSparseSolverConfiguration ::
  String ->
  Double ->
  Int ->
  Either SparseIterativeFailure ()
validateSparseSolverConfiguration methodName toleranceValue iterationLimit
  | not (fieldValueValid toleranceValue) =
      Left (SparseInvalidInput (methodName <> " tolerance must be finite"))
  | toleranceValue < 0.0 =
      Left (SparseInvalidInput (methodName <> " tolerance must be non-negative"))
  | iterationLimit < 0 =
      Left (SparseInvalidInput (methodName <> " iteration limit must be non-negative"))
  | otherwise = Right ()

sparseDiagonal ::
  SparseCSR Double ->
  Either SparseIterativeFailure (U.Vector Double)
sparseDiagonal sparseMatrix =
  Right
    ( U.generate
        (csrRows sparseMatrix)
        (diagonalAt sparseMatrix)
    )

diagonalAt :: SparseCSR Double -> Int -> Double
diagonalAt sparseMatrix rowIndex =
  findDiagonal startOffset
  where
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !values = csrValuesVector sparseMatrix
    !startOffset = rowOffsets `U.unsafeIndex` rowIndex
    !endOffset = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    findDiagonal !entryIndex
      | entryIndex >= endOffset = 0.0
      | otherwise =
          let !columnIndex =
                columnIndices `U.unsafeIndex` entryIndex
           in case compare columnIndex rowIndex of
                LT -> findDiagonal (entryIndex + 1)
                EQ -> values `U.unsafeIndex` entryIndex
                GT -> 0.0
{-# INLINE diagonalAt #-}

solverEpsilon :: Double
solverEpsilon = 1.0e-12

shiftedDiagonalValue :: Double -> Double -> Double
shiftedDiagonalValue shiftValue diagonalValue
  | diagonalValue >= 0.0 = diagonalValue + shiftValue
  | otherwise = diagonalValue - shiftValue

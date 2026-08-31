module Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( SparsePreconditioner (..),
    applySparsePreconditionerMutable,
    applySparsePreconditionerAndDotMutable,
    preconditionerDimension,
    compileSparsePreconditioner,
  )
where

import Control.Monad.ST (ST)
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (fieldValueValid)
import Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( shiftedDiagonalValue,
    solverEpsilon,
    sparseDiagonal,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.IncompleteCholesky0
  ( IC0Factor,
    applyIC0FactorAndDotMutable,
    applyIC0FactorMutable,
    ic0FactorDimension,
    incompleteCholesky0Factor,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    copyMutableVector,
    divideByDiagonalAndDotIntoMutable,
    divideByDiagonalIntoMutable,
    dotMutableVector,
    lowerTriangularSolveIntoMutable,
    multiplyByDiagonalIntoMutable,
    scaleMutableVector,
    upperTriangularSolveIntoMutable,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( SparseIterativeFailure (..),
    SparsePreconditionerFamily (..),
  )
import Moonlight.LinAlg.Pure.Sparse.Types (SparseCSR, csrRows)
import Prelude

type SparsePreconditioner :: Type
data SparsePreconditioner
  = IdentitySparsePreconditioner !Int
  | DiagonalSparsePreconditioner !(U.Vector Double)
  | SsorSparsePreconditioner !Double !(U.Vector Double) !(U.Vector Double) !(SparseCSR Double)
  | IncompleteCholesky0SparsePreconditioner !IC0Factor

preconditionerDimension :: SparsePreconditioner -> Int
preconditionerDimension preconditionerValue =
  case preconditionerValue of
    IdentitySparsePreconditioner dimension -> dimension
    DiagonalSparsePreconditioner diagonalValues -> U.length diagonalValues
    SsorSparsePreconditioner _ diagonalValues _ _ -> U.length diagonalValues
    IncompleteCholesky0SparsePreconditioner factorValue -> ic0FactorDimension factorValue

applySparsePreconditionerMutable ::
  SparsePreconditioner ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
applySparsePreconditionerMutable preconditionerValue sourceVector scratchA scratchB targetVector =
  case preconditionerValue of
    IdentitySparsePreconditioner _ ->
      copyMutableVector sourceVector targetVector
    DiagonalSparsePreconditioner diagonalValues ->
      divideByDiagonalIntoMutable diagonalValues sourceVector targetVector
    SsorSparsePreconditioner omegaValue diagonalValues scaledDiagonalValues sparseMatrix -> do
      lowerTriangularSolveIntoMutable scaledDiagonalValues sparseMatrix sourceVector scratchA
      multiplyByDiagonalIntoMutable diagonalValues scratchA scratchB
      upperTriangularSolveIntoMutable scaledDiagonalValues sparseMatrix scratchB targetVector
      scaleMutableVector ((2.0 - omegaValue) / omegaValue) targetVector
    IncompleteCholesky0SparsePreconditioner factorValue ->
      applyIC0FactorMutable factorValue sourceVector scratchA targetVector

applySparsePreconditionerAndDotMutable ::
  SparsePreconditioner ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
applySparsePreconditionerAndDotMutable
  preconditionerValue
  sourceVector
  scratchA
  scratchB
  targetVector =
    case preconditionerValue of
      IdentitySparsePreconditioner _ -> do
        copyMutableVector sourceVector targetVector
        dotMutableVector sourceVector sourceVector
      DiagonalSparsePreconditioner diagonalValues ->
        divideByDiagonalAndDotIntoMutable
          diagonalValues
          sourceVector
          targetVector
      SsorSparsePreconditioner
        omegaValue
        diagonalValues
        scaledDiagonalValues
        sparseMatrix -> do
          lowerTriangularSolveIntoMutable
            scaledDiagonalValues
            sparseMatrix
            sourceVector
            scratchA
          multiplyByDiagonalIntoMutable
            diagonalValues
            scratchA
            scratchB
          upperTriangularSolveIntoMutable
            scaledDiagonalValues
            sparseMatrix
            scratchB
            targetVector
          scaleMutableVector
            ((2.0 - omegaValue) / omegaValue)
            targetVector
          dotMutableVector sourceVector targetVector
      IncompleteCholesky0SparsePreconditioner factorValue ->
        applyIC0FactorAndDotMutable
          factorValue
          sourceVector
          scratchA
          targetVector
{-# INLINE applySparsePreconditionerAndDotMutable #-}

compileSparsePreconditioner :: SparsePreconditionerFamily -> SparseCSR Double -> Either SparseIterativeFailure SparsePreconditioner
compileSparsePreconditioner preconditionerFamily sparseMatrix =
  case preconditionerFamily of
    IdentitySparsePreconditionerFamily ->
      Right (IdentitySparsePreconditioner (csrRows sparseMatrix))
    DiagonalJacobiSparsePreconditionerFamily ->
      diagonalPreconditioner sparseMatrix
    ShiftedDiagonalJacobiSparsePreconditionerFamily shiftValue ->
      shiftedDiagonalPreconditioner shiftValue sparseMatrix
    SsorSparsePreconditionerFamily omegaValue ->
      ssorPreconditioner omegaValue sparseMatrix
    IncompleteCholesky0SparsePreconditionerFamily configValue ->
      IncompleteCholesky0SparsePreconditioner
        <$> incompleteCholesky0Factor configValue sparseMatrix

diagonalPreconditioner :: SparseCSR Double -> Either SparseIterativeFailure SparsePreconditioner
diagonalPreconditioner sparseMatrix = do
  diagonalValues <- sparseDiagonal sparseMatrix
  if U.all strictlyNonZero diagonalValues
    then Right (DiagonalSparsePreconditioner diagonalValues)
    else Left (SparseInvalidInput "diagonal preconditioner requires a strictly non-zero diagonal")

shiftedDiagonalPreconditioner :: Double -> SparseCSR Double -> Either SparseIterativeFailure SparsePreconditioner
shiftedDiagonalPreconditioner shiftValue sparseMatrix
  | shiftValue <= solverEpsilon =
      Left (SparseInvalidInput "shifted diagonal preconditioner requires a strictly positive shift")
  | otherwise = do
      diagonalValues <- sparseDiagonal sparseMatrix
      let shiftedDiagonalValues = U.map (shiftedDiagonalValue shiftValue) diagonalValues
      if U.all strictlyNonZero shiftedDiagonalValues
        then Right (DiagonalSparsePreconditioner shiftedDiagonalValues)
        else Left (SparseInvalidInput "shifted diagonal preconditioner requires a non-degenerate shifted diagonal")

ssorPreconditioner :: Double -> SparseCSR Double -> Either SparseIterativeFailure SparsePreconditioner
ssorPreconditioner omegaValue sparseMatrix
  | omegaValue <= solverEpsilon || omegaValue >= 2.0 - solverEpsilon =
      Left (SparseInvalidInput "SSOR preconditioner requires a relaxation parameter strictly between 0 and 2")
  | otherwise = do
      diagonalValues <- sparseDiagonal sparseMatrix
      let scaledDiagonalValues = U.map (/ omegaValue) diagonalValues
      if U.all strictlyNonZero scaledDiagonalValues
        then Right (SsorSparsePreconditioner omegaValue diagonalValues scaledDiagonalValues sparseMatrix)
        else Left (SparseInvalidInput "SSOR preconditioner requires a strictly non-zero diagonal")

strictlyNonZero :: Double -> Bool
strictlyNonZero value =
  fieldValueValid value && abs value > solverEpsilon

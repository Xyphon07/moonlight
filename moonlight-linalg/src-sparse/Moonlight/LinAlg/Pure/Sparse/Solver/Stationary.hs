{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.Stationary
  ( solveSparseJacobi,
    solveSparseRichardson,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Moonlight.Core (fieldValueValid)
import Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( validateSparseSolverConfiguration,
    validateSparseSystemInput,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    addScaledMutableVector,
    freezeMutableDoubleVector,
    newMutableDoubleVector,
    normMutableVector,
    residualIntoMutable,
    scaledCopyMutableVector,
    thawMutableDoubleVector,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( SparsePreconditioner,
    applySparsePreconditionerMutable,
    compileSparsePreconditioner,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( SparseIterativeFailure (..),
    SparseIterativeResult (..),
    SparsePreconditionerFamily (..),
    SparseStationaryIterationConfig (..),
  )
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    csrRowOffsetsVector,
    csrRows,
    csrValuesVector,
  )
import Prelude

solveSparseJacobi ::
  SparseStationaryIterationConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseJacobi config sparseMatrix rhsValues initialGuess = do
  validateSparseStationaryInput
    "Jacobi"
    validJacobiDamping
    config
    sparseMatrix
    rhsValues
    initialGuess
  preconditioner <- compileSparsePreconditioner DiagonalJacobiSparsePreconditionerFamily sparseMatrix
  runValidatedSparseStationary config sparseMatrix rhsValues initialGuess (JacobiStationaryStep (ssicDamping config) preconditioner)

-- | Solve by damped Richardson iteration under the caller-owned precondition
-- that the operator is symmetric positive-definite. The step is derived from
-- the maximum absolute row sum, an upper bound on the spectral radius for a
-- symmetric operator.
solveSparseRichardson ::
  SparseStationaryIterationConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseRichardson config sparseMatrix rhsValues initialGuess = do
  validateSparseStationaryInput
    "Richardson"
    validRichardsonDamping
    config
    sparseMatrix
    rhsValues
    initialGuess
  stepSize <- conservativeRichardsonStep sparseMatrix
  runValidatedSparseStationary config sparseMatrix rhsValues initialGuess (RichardsonStationaryStep (ssicDamping config * stepSize))

runValidatedSparseStationary ::
  SparseStationaryIterationConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  StationaryStep ->
  Either SparseIterativeFailure SparseIterativeResult
runValidatedSparseStationary SparseStationaryIterationConfig {..} sparseMatrix rhsValues initialGuess stepKind =
  runST (solveSparseStationaryMutable ssicTolerance ssicIterationLimit sparseMatrix rhsValues initialGuess stepKind)

validateSparseStationaryInput ::
  String ->
  (Double -> Bool) ->
  SparseStationaryIterationConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure ()
validateSparseStationaryInput methodName validDamping config sparseMatrix rhsValues initialGuess = do
  validateSparseSystemInput sparseMatrix rhsValues initialGuess
  validateSparseSolverConfiguration methodName (ssicTolerance config) (ssicIterationLimit config)
  if validDamping (ssicDamping config)
    then Right ()
    else Left (SparseInvalidInput (methodName <> " damping is outside its finite admissible range"))

validJacobiDamping :: Double -> Bool
validJacobiDamping dampingValue =
  fieldValueValid dampingValue && dampingValue > 0.0 && dampingValue <= 1.0

validRichardsonDamping :: Double -> Bool
validRichardsonDamping dampingValue =
  fieldValueValid dampingValue && dampingValue > 0.0 && dampingValue < 2.0

type StationaryStep :: Type
data StationaryStep
  = JacobiStationaryStep !Double !SparsePreconditioner
  | RichardsonStationaryStep !Double

solveSparseStationaryMutable ::
  Double ->
  Int ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  StationaryStep ->
  ST s (Either SparseIterativeFailure SparseIterativeResult)
solveSparseStationaryMutable !toleranceValue !iterationLimit sparseMatrix rhsValues initialGuess stepKind = do
  let !dimension = csrRows sparseMatrix
  currentVector <- thawMutableDoubleVector initialGuess
  residualVector <- newMutableDoubleVector dimension
  stepVector <- newMutableDoubleVector dimension
  imageVector <- newMutableDoubleVector dimension
  preconditionerScratchA <- newMutableDoubleVector dimension
  preconditionerScratchB <- newMutableDoubleVector dimension
  residualIntoMutable sparseMatrix rhsValues currentVector imageVector residualVector
  initialResidualNorm <- normMutableVector residualVector
  if initialResidualNorm <= toleranceValue
    then Right <$> freezeStationaryResult 0 initialResidualNorm currentVector
    else do
      finalState <-
        U.foldM'
          (stationaryIteration toleranceValue sparseMatrix rhsValues currentVector residualVector stepVector imageVector preconditionerScratchA preconditionerScratchB stepKind)
          StationaryRunning
          (U.enumFromN 0 iterationLimit)
      stationaryResultFromState iterationLimit currentVector finalState

type StationaryState :: Type
data StationaryState
  = StationaryRunning
  | StationaryConverged !Int !Double

stationaryIteration ::
  Double ->
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  StationaryStep ->
  StationaryState ->
  Int ->
  ST s StationaryState
stationaryIteration !toleranceValue sparseMatrix rhsValues currentVector residualVector stepVector imageVector preconditionerScratchA preconditionerScratchB stepKind stepState iterationIndex =
  case stepState of
    StationaryConverged _ _ -> pure stepState
    StationaryRunning -> do
      writeStationaryStep stepKind residualVector preconditionerScratchA preconditionerScratchB stepVector
      addScaledMutableVector 1.0 stepVector currentVector
      residualIntoMutable sparseMatrix rhsValues currentVector imageVector residualVector
      nextResidualNorm <- normMutableVector residualVector
      if nextResidualNorm <= toleranceValue
        then pure (StationaryConverged (iterationIndex + 1) nextResidualNorm)
        else pure StationaryRunning

writeStationaryStep ::
  StationaryStep ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
writeStationaryStep stepKind residualVector scratchA scratchB stepVector =
  case stepKind of
    JacobiStationaryStep dampingValue preconditioner -> do
      applySparsePreconditionerMutable preconditioner residualVector scratchA scratchB stepVector
      scaledCopyMutableVector dampingValue stepVector stepVector
    RichardsonStationaryStep richardsonScale ->
      scaledCopyMutableVector richardsonScale residualVector stepVector

stationaryResultFromState :: Int -> MutableDoubleVector s -> StationaryState -> ST s (Either SparseIterativeFailure SparseIterativeResult)
stationaryResultFromState iterationLimit currentVector stepState =
  case stepState of
    StationaryRunning -> pure (Left (SparseIterationBudgetExceeded iterationLimit))
    StationaryConverged iterationCount residualNormValue -> Right <$> freezeStationaryResult iterationCount residualNormValue currentVector

freezeStationaryResult :: Int -> Double -> MutableDoubleVector s -> ST s SparseIterativeResult
freezeStationaryResult iterationCount residualNormValue currentVector = do
  solutionVector <- freezeMutableDoubleVector currentVector
  pure
    SparseIterativeResult
      { sparseSolution = solutionVector,
        sparseIterations = iterationCount,
        sparseResidualNorm = residualNormValue
      }

-- For a symmetric positive-definite operator, the maximum absolute row sum
-- bounds the largest eigenvalue. Damping in (0,2) therefore yields a
-- conservative Richardson step without pretending the diagonal is a spectral
-- bound.
conservativeRichardsonStep :: SparseCSR Double -> Either SparseIterativeFailure Double
conservativeRichardsonStep sparseMatrix
  | not (fieldValueValid operatorBound) =
      Left (SparseInvalidInput "Richardson absolute row-sum bound is non-finite")
  | operatorBound <= 0.0 =
      Left (SparseInvalidInput "Richardson iteration requires a non-zero operator bound")
  | otherwise = Right (1.0 / operatorBound)
  where
    rowOffsets = csrRowOffsetsVector sparseMatrix
    matrixValues = csrValuesVector sparseMatrix
    rowAbsoluteSum rowIndex =
      let startOffset = rowOffsets `U.unsafeIndex` rowIndex
          endOffset = rowOffsets `U.unsafeIndex` (rowIndex + 1)
       in U.sum (U.map abs (U.slice startOffset (endOffset - startOffset) matrixValues))
    operatorBound =
      U.maximum
        (U.cons 0.0 (U.generate (csrRows sparseMatrix) rowAbsoluteSum))

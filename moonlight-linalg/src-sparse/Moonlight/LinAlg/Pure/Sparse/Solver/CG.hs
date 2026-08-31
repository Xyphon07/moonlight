{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.CG
  ( solveSparseCG,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Moonlight.Core (fieldValueValid)
import Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( validateSparseSolverConfiguration,
    validateSparseSystemInput,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    copyImmutableSquaredNormIntoMutable,
    copyMutableVector,
    csrMatVecDotIntoMutable,
    csrResidualSquaredIntoMutable,
    divideByDiagonalDotAndCopyMutable,
    freezeMutableDoubleVector,
    initializeZeroJacobiMutable,
    updateDirectionMutable,
    updateSolutionAndResidualSquaredMutable,
    updateSolutionResidualJacobiMutable,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( SparsePreconditioner (..),
    applySparsePreconditionerAndDotMutable,
    compileSparsePreconditioner,
    preconditionerDimension,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( SparseConjugateGradientConfig (..),
    SparseIterativeFailure (..),
    SparseIterativeResult (..),
  )
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    csrRows,
  )
import Prelude

solveSparseCG ::
  SparseConjugateGradientConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseCG
  SparseConjugateGradientConfig {..}
  sparseMatrix
  rhsValues
  initialGuess = do
    validateSparseSystemInput
      sparseMatrix
      rhsValues
      initialGuess
    validateSparseSolverConfiguration
      "CG"
      scgcTolerance
      scgcIterationLimit
    preconditioner <-
      compileSparsePreconditioner
        scgcPreconditionerFamily
        sparseMatrix
    validateCgPreconditioner sparseMatrix preconditioner
    let !zeroInitialGuess = U.all (== 0.0) initialGuess
    case preconditioner of
      IdentitySparsePreconditioner _ ->
        runST
          ( solveIdentityCgMutable
              scgcTolerance
              scgcIterationLimit
              zeroInitialGuess
              sparseMatrix
              rhsValues
              initialGuess
          )
      DiagonalSparsePreconditioner diagonalValues
        | uniformDiagonal diagonalValues ->
            -- For M = dI with d > 0, all factors of d cancel exactly from
            -- alpha, beta, and the represented search direction.
            runST
              ( solveIdentityCgMutable
                  scgcTolerance
                  scgcIterationLimit
                  zeroInitialGuess
                  sparseMatrix
                  rhsValues
                  initialGuess
              )
        | otherwise ->
            runST
              ( solveJacobiCgMutable
                  scgcTolerance
                  scgcIterationLimit
                  zeroInitialGuess
                  diagonalValues
                  sparseMatrix
                  rhsValues
                  initialGuess
              )
      SsorSparsePreconditioner {} ->
        runST
          ( solveGenericPcgMutable
              scgcTolerance
              scgcIterationLimit
              zeroInitialGuess
              preconditioner
              sparseMatrix
              rhsValues
              initialGuess
          )
      IncompleteCholesky0SparsePreconditioner {} ->
        runST
          ( solveGenericPcgMutable
              scgcTolerance
              scgcIterationLimit
              zeroInitialGuess
              preconditioner
              sparseMatrix
              rhsValues
              initialGuess
          )

solveIdentityCgMutable ::
  Double ->
  Int ->
  Bool ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  ST s (Either SparseIterativeFailure SparseIterativeResult)
solveIdentityCgMutable
  !toleranceValue
  !iterationLimit
  zeroInitialGuess
  sparseMatrix
  rhsValues
  initialGuess = do
    let !dimension = csrRows sparseMatrix
    guessVector <- U.thaw initialGuess
    residualVector <- MU.unsafeNew dimension
    directionVector <- MU.unsafeNew dimension
    imageDirectionVector <- MU.unsafeNew dimension

    initialResidualSquared <-
      if zeroInitialGuess
        then
          copyImmutableSquaredNormIntoMutable
            rhsValues
            residualVector
        else
          csrResidualSquaredIntoMutable
            sparseMatrix
            rhsValues
            guessVector
            residualVector

    case residualNormFromSquared initialResidualSquared of
      Left failureValue -> pure (Left failureValue)
      Right initialResidualNorm
        | initialResidualNorm <= toleranceValue ->
            Right
              <$> freezeCgResult
                0
                initialResidualNorm
                guessVector
        | otherwise -> do
            copyMutableVector residualVector directionVector
            let iterateCg !iterationCount !residualSquared
                  | iterationCount >= iterationLimit =
                      pure
                        ( Left
                            ( SparseIterationBudgetExceeded
                                iterationLimit
                            )
                        )
                  | otherwise = do
                      denominator <-
                        csrMatVecDotIntoMutable
                          sparseMatrix
                          directionVector
                          imageDirectionVector
                      if not (positiveFinite denominator)
                        then
                          pure
                            ( Left
                                ( SparseInvalidInput
                                    "CG requires p^T A p > 0; matrix is not SPD or arithmetic broke down"
                                )
                            )
                        else do
                          let !alphaValue =
                                residualSquared / denominator
                          if not (fieldValueValid alphaValue)
                            then
                              pure
                                ( Left
                                    ( SparseInvalidInput
                                        "CG produced a non-finite alpha"
                                    )
                                )
                            else do
                              nextResidualSquared <-
                                updateSolutionAndResidualSquaredMutable
                                  alphaValue
                                  directionVector
                                  imageDirectionVector
                                  guessVector
                                  residualVector
                              case residualNormFromSquared nextResidualSquared of
                                Left failureValue ->
                                  pure (Left failureValue)
                                Right nextResidualNorm ->
                                  let !nextIteration = iterationCount + 1
                                   in if nextResidualNorm <= toleranceValue
                                        then certifyOrRestart nextIteration
                                        else do
                                          let !betaValue =
                                                nextResidualSquared
                                                  / residualSquared
                                          if not (fieldValueValid betaValue)
                                            then
                                              pure
                                                ( Left
                                                    ( SparseInvalidInput
                                                        "CG produced a non-finite beta"
                                                    )
                                                )
                                            else do
                                              updateDirectionMutable
                                                betaValue
                                                residualVector
                                                directionVector
                                              iterateCg
                                                nextIteration
                                                nextResidualSquared

                certifyOrRestart !iterationCount = do
                  trueResidualSquared <-
                    csrResidualSquaredIntoMutable
                      sparseMatrix
                      rhsValues
                      guessVector
                      residualVector
                  case residualNormFromSquared trueResidualSquared of
                    Left failureValue -> pure (Left failureValue)
                    Right trueResidualNorm
                      | trueResidualNorm <= toleranceValue ->
                          Right
                            <$> freezeCgResult
                              iterationCount
                              trueResidualNorm
                              guessVector
                      | otherwise -> do
                          copyMutableVector
                            residualVector
                            directionVector
                          iterateCg
                            iterationCount
                            trueResidualSquared

            iterateCg 0 initialResidualSquared

solveJacobiCgMutable ::
  Double ->
  Int ->
  Bool ->
  U.Vector Double ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  ST s (Either SparseIterativeFailure SparseIterativeResult)
solveJacobiCgMutable
  !toleranceValue
  !iterationLimit
  zeroInitialGuess
  diagonalValues
  sparseMatrix
  rhsValues
  initialGuess = do
    let !dimension = csrRows sparseMatrix
    guessVector <- U.thaw initialGuess
    residualVector <- MU.unsafeNew dimension
    preconditionedResidualVector <- MU.unsafeNew dimension
    directionVector <- MU.unsafeNew dimension
    imageDirectionVector <- MU.unsafeNew dimension

    (initialResidualSquared, initialRho) <-
      if zeroInitialGuess
        then
          initializeZeroJacobiMutable
            rhsValues
            diagonalValues
            residualVector
            preconditionedResidualVector
            directionVector
        else do
          residualSquared <-
            csrResidualSquaredIntoMutable
              sparseMatrix
              rhsValues
              guessVector
              residualVector
          rhoValue <-
            divideByDiagonalDotAndCopyMutable
              diagonalValues
              residualVector
              preconditionedResidualVector
              directionVector
          pure (residualSquared, rhoValue)

    case residualNormFromSquared initialResidualSquared of
      Left failureValue -> pure (Left failureValue)
      Right initialResidualNorm
        | initialResidualNorm <= toleranceValue ->
            Right
              <$> freezeCgResult
                0
                initialResidualNorm
                guessVector
        | not (positiveFinite initialRho) ->
            pure
              ( Left
                  ( SparseInvalidInput
                      "Jacobi PCG requires r^T M^-1 r > 0"
                  )
              )
        | otherwise -> do
            let iteratePcg !iterationCount !rhoValue
                  | iterationCount >= iterationLimit =
                      pure
                        ( Left
                            ( SparseIterationBudgetExceeded
                                iterationLimit
                            )
                        )
                  | otherwise = do
                      denominator <-
                        csrMatVecDotIntoMutable
                          sparseMatrix
                          directionVector
                          imageDirectionVector
                      if not (positiveFinite denominator)
                        then
                          pure
                            ( Left
                                ( SparseInvalidInput
                                    "Jacobi PCG requires p^T A p > 0; matrix is not SPD or arithmetic broke down"
                                )
                            )
                        else do
                          let !alphaValue = rhoValue / denominator
                          if not (fieldValueValid alphaValue)
                            then
                              pure
                                ( Left
                                    ( SparseInvalidInput
                                        "Jacobi PCG produced a non-finite alpha"
                                    )
                                )
                            else do
                              (nextResidualSquared, nextRho) <-
                                updateSolutionResidualJacobiMutable
                                  alphaValue
                                  diagonalValues
                                  directionVector
                                  imageDirectionVector
                                  guessVector
                                  residualVector
                                  preconditionedResidualVector
                              case residualNormFromSquared nextResidualSquared of
                                Left failureValue ->
                                  pure (Left failureValue)
                                Right nextResidualNorm ->
                                  let !nextIteration = iterationCount + 1
                                   in if nextResidualNorm <= toleranceValue
                                        then certifyOrRestart nextIteration
                                        else
                                          if not (positiveFinite nextRho)
                                            then
                                              pure
                                                ( Left
                                                    ( SparseInvalidInput
                                                        "Jacobi PCG encountered non-positive r^T M^-1 r"
                                                    )
                                                )
                                            else do
                                              let !betaValue =
                                                    nextRho / rhoValue
                                              if not (fieldValueValid betaValue)
                                                then
                                                  pure
                                                    ( Left
                                                        ( SparseInvalidInput
                                                            "Jacobi PCG produced a non-finite beta"
                                                        )
                                                    )
                                                else do
                                                  updateDirectionMutable
                                                    betaValue
                                                    preconditionedResidualVector
                                                    directionVector
                                                  iteratePcg
                                                    nextIteration
                                                    nextRho

                certifyOrRestart !iterationCount = do
                  trueResidualSquared <-
                    csrResidualSquaredIntoMutable
                      sparseMatrix
                      rhsValues
                      guessVector
                      residualVector
                  case residualNormFromSquared trueResidualSquared of
                    Left failureValue -> pure (Left failureValue)
                    Right trueResidualNorm
                      | trueResidualNorm <= toleranceValue ->
                          Right
                            <$> freezeCgResult
                              iterationCount
                              trueResidualNorm
                              guessVector
                      | otherwise -> do
                          restartedRho <-
                            divideByDiagonalDotAndCopyMutable
                              diagonalValues
                              residualVector
                              preconditionedResidualVector
                              directionVector
                          if not (positiveFinite restartedRho)
                            then
                              pure
                                ( Left
                                    ( SparseInvalidInput
                                        "Jacobi PCG residual replacement produced non-positive r^T M^-1 r"
                                    )
                                )
                            else
                              iteratePcg
                                iterationCount
                                restartedRho

            iteratePcg 0 initialRho

solveGenericPcgMutable ::
  Double ->
  Int ->
  Bool ->
  SparsePreconditioner ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  ST s (Either SparseIterativeFailure SparseIterativeResult)
solveGenericPcgMutable
  !toleranceValue
  !iterationLimit
  zeroInitialGuess
  preconditioner
  sparseMatrix
  rhsValues
  initialGuess = do
    let !dimension = csrRows sparseMatrix
    guessVector <- U.thaw initialGuess
    residualVector <- MU.unsafeNew dimension
    preconditionedResidualVector <- MU.unsafeNew dimension
    directionVector <- MU.unsafeNew dimension
    imageDirectionVector <- MU.unsafeNew dimension
    preconditionerScratchA <- MU.unsafeNew dimension
    preconditionerScratchB <- MU.unsafeNew dimension

    initialResidualSquared <-
      if zeroInitialGuess
        then
          copyImmutableSquaredNormIntoMutable
            rhsValues
            residualVector
        else
          csrResidualSquaredIntoMutable
            sparseMatrix
            rhsValues
            guessVector
            residualVector

    case residualNormFromSquared initialResidualSquared of
      Left failureValue -> pure (Left failureValue)
      Right initialResidualNorm
        | initialResidualNorm <= toleranceValue ->
            Right
              <$> freezeCgResult
                0
                initialResidualNorm
                guessVector
        | otherwise -> do
            initialRho <-
              applySparsePreconditionerAndDotMutable
                preconditioner
                residualVector
                preconditionerScratchA
                preconditionerScratchB
                preconditionedResidualVector
            if not (positiveFinite initialRho)
              then
                pure
                  ( Left
                      ( SparseInvalidInput
                          "PCG requires r^T M^-1 r > 0"
                      )
                  )
              else do
                copyMutableVector
                  preconditionedResidualVector
                  directionVector
                let iteratePcg !iterationCount !rhoValue
                      | iterationCount >= iterationLimit =
                          pure
                            ( Left
                                ( SparseIterationBudgetExceeded
                                    iterationLimit
                                )
                            )
                      | otherwise = do
                          denominator <-
                            csrMatVecDotIntoMutable
                              sparseMatrix
                              directionVector
                              imageDirectionVector
                          if not (positiveFinite denominator)
                            then
                              pure
                                ( Left
                                    ( SparseInvalidInput
                                        "PCG requires p^T A p > 0; matrix is not SPD or arithmetic broke down"
                                    )
                                )
                            else do
                              let !alphaValue = rhoValue / denominator
                              if not (fieldValueValid alphaValue)
                                then
                                  pure
                                    ( Left
                                        ( SparseInvalidInput
                                            "PCG produced a non-finite alpha"
                                        )
                                    )
                                else do
                                  nextResidualSquared <-
                                    updateSolutionAndResidualSquaredMutable
                                      alphaValue
                                      directionVector
                                      imageDirectionVector
                                      guessVector
                                      residualVector
                                  case residualNormFromSquared nextResidualSquared of
                                    Left failureValue ->
                                      pure (Left failureValue)
                                    Right nextResidualNorm ->
                                      let !nextIteration = iterationCount + 1
                                       in if nextResidualNorm <= toleranceValue
                                            then certifyOrRestart nextIteration
                                            else do
                                              nextRho <-
                                                applySparsePreconditionerAndDotMutable
                                                  preconditioner
                                                  residualVector
                                                  preconditionerScratchA
                                                  preconditionerScratchB
                                                  preconditionedResidualVector
                                              if not (positiveFinite nextRho)
                                                then
                                                  pure
                                                    ( Left
                                                        ( SparseInvalidInput
                                                            "PCG encountered non-positive r^T M^-1 r"
                                                        )
                                                    )
                                                else do
                                                  let !betaValue =
                                                        nextRho / rhoValue
                                                  if not (fieldValueValid betaValue)
                                                    then
                                                      pure
                                                        ( Left
                                                            ( SparseInvalidInput
                                                                "PCG produced a non-finite beta"
                                                            )
                                                        )
                                                    else do
                                                      updateDirectionMutable
                                                        betaValue
                                                        preconditionedResidualVector
                                                        directionVector
                                                      iteratePcg
                                                        nextIteration
                                                        nextRho

                    certifyOrRestart !iterationCount = do
                      trueResidualSquared <-
                        csrResidualSquaredIntoMutable
                          sparseMatrix
                          rhsValues
                          guessVector
                          residualVector
                      case residualNormFromSquared trueResidualSquared of
                        Left failureValue -> pure (Left failureValue)
                        Right trueResidualNorm
                          | trueResidualNorm <= toleranceValue ->
                              Right
                                <$> freezeCgResult
                                  iterationCount
                                  trueResidualNorm
                                  guessVector
                          | otherwise -> do
                              restartedRho <-
                                applySparsePreconditionerAndDotMutable
                                  preconditioner
                                  residualVector
                                  preconditionerScratchA
                                  preconditionerScratchB
                                  preconditionedResidualVector
                              if not (positiveFinite restartedRho)
                                then
                                  pure
                                    ( Left
                                        ( SparseInvalidInput
                                            "PCG residual replacement produced non-positive r^T M^-1 r"
                                        )
                                    )
                                else do
                                  copyMutableVector
                                    preconditionedResidualVector
                                    directionVector
                                  iteratePcg
                                    iterationCount
                                    restartedRho

                iteratePcg 0 initialRho

freezeCgResult ::
  Int ->
  Double ->
  MutableDoubleVector s ->
  ST s SparseIterativeResult
freezeCgResult iterationCount residualNormValue guessVector = do
  solutionVector <- freezeMutableDoubleVector guessVector
  pure
    SparseIterativeResult
      { sparseSolution = solutionVector,
        sparseIterations = iterationCount,
        sparseResidualNorm = residualNormValue
      }

validateCgPreconditioner ::
  SparseCSR Double ->
  SparsePreconditioner ->
  Either SparseIterativeFailure ()
validateCgPreconditioner sparseMatrix preconditioner
  | preconditionerDimension preconditioner /= csrRows sparseMatrix =
      Left
        ( SparseInvalidInput
            "CG preconditioner dimension must equal matrix dimension"
        )
  | otherwise =
      case preconditioner of
        IdentitySparsePreconditioner _ -> Right ()
        DiagonalSparsePreconditioner diagonalValues
          | U.all positiveFinite diagonalValues -> Right ()
          | otherwise ->
              Left
                ( SparseInvalidInput
                    "CG requires a finite positive-definite diagonal preconditioner"
                )
        SsorSparsePreconditioner
          omegaValue
          diagonalValues
          scaledDiagonalValues
          _
            | omegaValue > 0.0
                && omegaValue < 2.0
                && U.all positiveFinite diagonalValues
                && U.all positiveFinite scaledDiagonalValues ->
                Right ()
            | otherwise ->
                Left
                  ( SparseInvalidInput
                      "CG requires an SPD SSOR preconditioner with omega in (0,2) and positive diagonal"
                  )
        IncompleteCholesky0SparsePreconditioner _ -> Right ()

residualNormFromSquared ::
  Double ->
  Either SparseIterativeFailure Double
residualNormFromSquared squaredNorm
  | not (fieldValueValid squaredNorm) =
      Left
        ( SparseInvalidInput
            "CG residual squared norm became non-finite"
        )
  | squaredNorm < 0.0 =
      Left
        ( SparseInvalidInput
            "CG residual squared norm became negative"
        )
  | otherwise = Right (sqrt squaredNorm)

uniformDiagonal :: U.Vector Double -> Bool
uniformDiagonal diagonalValues
  | U.null diagonalValues = False
  | otherwise =
      let !firstValue = diagonalValues `U.unsafeIndex` 0
       in U.all (== firstValue) diagonalValues
{-# INLINE uniformDiagonal #-}

positiveFinite :: Double -> Bool
positiveFinite value = value > 0.0 && fieldValueValid value
{-# INLINE positiveFinite #-}

{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.GMRES
  ( solveSparseGMRES,
  )
where

import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Moonlight.Core
  ( checkedNonNegativeProduct,
    checkedNonNegativeSum,
    fieldValueValid,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Common
  ( solverEpsilon,
    validateSparseSolverConfiguration,
    validateSparseSystemInput,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    addScaledMutableVector,
    copyMutableVector,
    csrMatVecIntoMutable,
    dotMutableVector,
    freezeMutableDoubleVector,
    newMutableDoubleVector,
    residualIntoMutable,
    scaleMutableVector,
    thawMutableDoubleVector,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( SparsePreconditioner,
    applySparsePreconditionerMutable,
    compileSparsePreconditioner,
    preconditionerDimension,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( SparseGMRESConfig (..),
    SparseIterativeFailure (..),
    SparseIterativeResult (..),
  )
import Moonlight.LinAlg.Pure.Sparse.Types (SparseCSR, csrRows)
import Prelude

solveSparseGMRES ::
  SparseGMRESConfig ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  Either SparseIterativeFailure SparseIterativeResult
solveSparseGMRES config sparseMatrix rhsValues initialGuess = do
  validateSparseSystemInput sparseMatrix rhsValues initialGuess
  validateSparseSolverConfiguration "GMRES" (sgcTolerance config) (sgcIterationLimit config)
  if sgcRestartDimension config <= 0
    then Left (SparseInvalidInput "GMRES restart dimension must be positive")
    else Right ()
  workspaceSizes <-
    checkedGMRESWorkspaceSizes
      (csrRows sparseMatrix)
      (sgcRestartDimension config)
  preconditioner <- compileSparsePreconditioner (sgcPreconditionerFamily config) sparseMatrix
  validateGmresPreconditioner sparseMatrix preconditioner
  runST (solveSparseGMRESMutable config preconditioner sparseMatrix rhsValues initialGuess workspaceSizes)

solveSparseGMRESMutable ::
  SparseGMRESConfig ->
  SparsePreconditioner ->
  SparseCSR Double ->
  U.Vector Double ->
  U.Vector Double ->
  GMRESWorkspaceSizes ->
  ST s (Either SparseIterativeFailure SparseIterativeResult)
solveSparseGMRESMutable config preconditioner sparseMatrix rhsValues initialGuess workspaceSizes =
  runExceptT $ do
    let !restartDimension = sgcRestartDimension config
        !restartCycles = restartCycleCount (sgcIterationLimit config) restartDimension
    workspace <- lift (newGMRESWorkspace workspaceSizes)
    currentGuess <- lift (thawMutableDoubleVector initialGuess)
    finalState <-
      U.foldM'
        (gmresRestartCycle config preconditioner sparseMatrix rhsValues currentGuess workspace)
        (GmresRunning 0)
        (U.enumFromN 0 restartCycles)
    gmresResultFromState (sgcIterationLimit config) currentGuess finalState

type GMRESWorkspace :: Type -> Type
data GMRESWorkspace s = GMRESWorkspace
  { gmresBasisPayload :: !(MutableDoubleVector s),
    gmresPreconditionedPayload :: !(MutableDoubleVector s),
    gmresHessenbergPayload :: !(MutableDoubleVector s),
    gmresCosines :: !(MutableDoubleVector s),
    gmresSines :: !(MutableDoubleVector s),
    gmresProjectedResidual :: !(MutableDoubleVector s),
    gmresYValues :: !(MutableDoubleVector s),
    gmresResidualVector :: !(MutableDoubleVector s),
    gmresImageVector :: !(MutableDoubleVector s),
    gmresWorkVector :: !(MutableDoubleVector s),
    gmresPreconditionerScratchA :: !(MutableDoubleVector s),
    gmresPreconditionerScratchB :: !(MutableDoubleVector s),
    gmresDimension :: !Int,
    gmresRestartDimension :: !Int,
    gmresKrylovColumnCount :: !Int
  }

type GMRESWorkspaceSizes :: Type
data GMRESWorkspaceSizes = GMRESWorkspaceSizes
  { gmresWorkspaceDimension :: !Int,
    gmresWorkspaceRestartDimension :: !Int,
    gmresWorkspaceKrylovColumnCount :: !Int,
    gmresWorkspaceBasisPayloadLength :: !Int,
    gmresWorkspacePreconditionedPayloadLength :: !Int,
    gmresWorkspaceHessenbergPayloadLength :: !Int
  }

checkedGMRESWorkspaceSizes ::
  Int ->
  Int ->
  Either SparseIterativeFailure GMRESWorkspaceSizes
checkedGMRESWorkspaceSizes dimension restartDimension = do
  krylovColumnCount <-
    checkedWorkspaceCardinality "GMRES restart dimension plus one"
      (checkedNonNegativeSum restartDimension 1)
  basisPayloadLength <-
    checkedWorkspaceCardinality "GMRES basis workspace"
      (checkedNonNegativeProduct krylovColumnCount dimension)
  preconditionedPayloadLength <-
    checkedWorkspaceCardinality "GMRES preconditioned basis workspace"
      (checkedNonNegativeProduct restartDimension dimension)
  hessenbergPayloadLength <-
    checkedWorkspaceCardinality "GMRES Hessenberg workspace"
      (checkedNonNegativeProduct krylovColumnCount restartDimension)
  Right
    GMRESWorkspaceSizes
      { gmresWorkspaceDimension = dimension,
        gmresWorkspaceRestartDimension = restartDimension,
        gmresWorkspaceKrylovColumnCount = krylovColumnCount,
        gmresWorkspaceBasisPayloadLength = basisPayloadLength,
        gmresWorkspacePreconditionedPayloadLength = preconditionedPayloadLength,
        gmresWorkspaceHessenbergPayloadLength = hessenbergPayloadLength
      }

checkedWorkspaceCardinality ::
  String ->
  Either cardinalityFailure Int ->
  Either SparseIterativeFailure Int
checkedWorkspaceCardinality workspaceName =
  first
    (const (SparseInvalidInput (workspaceName <> " exceeds non-negative Int cardinality")))

newGMRESWorkspace :: GMRESWorkspaceSizes -> ST s (GMRESWorkspace s)
newGMRESWorkspace workspaceSizes = do
  let !dimension = gmresWorkspaceDimension workspaceSizes
      !restartDimension = gmresWorkspaceRestartDimension workspaceSizes
      !krylovColumnCount = gmresWorkspaceKrylovColumnCount workspaceSizes
  basisPayload <- newMutableDoubleVector (gmresWorkspaceBasisPayloadLength workspaceSizes)
  preconditionedPayload <- newMutableDoubleVector (gmresWorkspacePreconditionedPayloadLength workspaceSizes)
  hessenbergPayload <- newMutableDoubleVector (gmresWorkspaceHessenbergPayloadLength workspaceSizes)
  cosines <- newMutableDoubleVector restartDimension
  sines <- newMutableDoubleVector restartDimension
  projectedResidual <- newMutableDoubleVector krylovColumnCount
  yValues <- newMutableDoubleVector restartDimension
  residualVector <- newMutableDoubleVector dimension
  imageVector <- newMutableDoubleVector dimension
  workVector <- newMutableDoubleVector dimension
  preconditionerScratchA <- newMutableDoubleVector dimension
  preconditionerScratchB <- newMutableDoubleVector dimension
  pure
    GMRESWorkspace
      { gmresBasisPayload = basisPayload,
        gmresPreconditionedPayload = preconditionedPayload,
        gmresHessenbergPayload = hessenbergPayload,
        gmresCosines = cosines,
        gmresSines = sines,
        gmresProjectedResidual = projectedResidual,
        gmresYValues = yValues,
        gmresResidualVector = residualVector,
        gmresImageVector = imageVector,
        gmresWorkVector = workVector,
        gmresPreconditionerScratchA = preconditionerScratchA,
        gmresPreconditionerScratchB = preconditionerScratchB,
        gmresDimension = dimension,
        gmresRestartDimension = restartDimension,
        gmresKrylovColumnCount = krylovColumnCount
      }

type GmresState :: Type
data GmresState
  = GmresRunning !Int
  | GmresConverged !Int !Double

gmresRestartCycle ::
  SparseGMRESConfig ->
  SparsePreconditioner ->
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  GMRESWorkspace s ->
  GmresState ->
  Int ->
  ExceptT SparseIterativeFailure (ST s) GmresState
gmresRestartCycle config preconditioner sparseMatrix rhsValues currentGuess workspace stateValue _ =
  case stateValue of
    GmresConverged _ _ -> pure stateValue
    GmresRunning totalIterations ->
      if totalIterations >= sgcIterationLimit config
        then pure stateValue
        else do
          betaValue <-
            lift $ do
              residualIntoMutable sparseMatrix rhsValues currentGuess (gmresImageVector workspace) (gmresResidualVector workspace)
              stableNormMutableVector (gmresResidualVector workspace)
          unless (fieldValueValid betaValue) $
            throwE (SparseInvalidInput "GMRES residual norm is not representable as a finite Double")
          if betaValue <= sgcTolerance config
            then pure (GmresConverged totalIterations betaValue)
            else do
              lift (prepareRestartBasis betaValue workspace)
              arnoldiState <-
                U.foldM'
                  (gmresArnoldiStep config preconditioner sparseMatrix workspace)
                  (ArnoldiRunning 0 betaValue)
                  (U.enumFromN 0 (min (gmresRestartDimension workspace) (sgcIterationLimit config - totalIterations)))
              applyArnoldiCorrection currentGuess workspace arnoldiState
              trueResidualNorm <- lift (gmresTrueResidualNorm sparseMatrix rhsValues currentGuess workspace)
              except (gmresStateAfterArnoldi config totalIterations arnoldiState trueResidualNorm)

type ArnoldiState :: Type
data ArnoldiState
  = ArnoldiRunning !Int !Double
  | ArnoldiConverged !Int !Double
  | ArnoldiHappyBreakdown !Int !Double

gmresArnoldiStep ::
  SparseGMRESConfig ->
  SparsePreconditioner ->
  SparseCSR Double ->
  GMRESWorkspace s ->
  ArnoldiState ->
  Int ->
  ExceptT SparseIterativeFailure (ST s) ArnoldiState
gmresArnoldiStep config preconditioner sparseMatrix workspace stateValue _ =
  case stateValue of
    ArnoldiConverged _ _ -> pure stateValue
    ArnoldiHappyBreakdown _ _ -> pure stateValue
    ArnoldiRunning completedSteps residualNormValue ->
      if residualNormValue <= sgcTolerance config
        then pure (ArnoldiConverged completedSteps residualNormValue)
        else do
          let basisVector = basisColumn workspace completedSteps
              preconditionedVector = preconditionedColumn workspace completedSteps
          nextBasisNorm <-
            lift $ do
              applySparsePreconditionerMutable
                preconditioner
                basisVector
                (gmresPreconditionerScratchA workspace)
                (gmresPreconditionerScratchB workspace)
                preconditionedVector
              csrMatVecIntoMutable sparseMatrix preconditionedVector (gmresWorkVector workspace)
              orthogonalizeAgainstBasis workspace completedSteps
              stableNormMutableVector (gmresWorkVector workspace)
          unless (fieldValueValid nextBasisNorm) $ throwE (SparseInvalidInput "GMRES Arnoldi norm is not representable as a finite Double")
          lift $ do
            writeHessenbergEntry workspace (completedSteps + 1) completedSteps nextBasisNorm
            writeNextBasisColumn workspace completedSteps nextBasisNorm
          residualAfterRotation <-
            applyPreviousRotations workspace completedSteps
              *> applyNextRotation workspace completedSteps
          let !nextCompletedSteps = completedSteps + 1
          if nextBasisNorm <= solverEpsilon
            then pure (ArnoldiHappyBreakdown nextCompletedSteps residualAfterRotation)
            else
              if residualAfterRotation <= sgcTolerance config
                then pure (ArnoldiConverged nextCompletedSteps residualAfterRotation)
                else pure (ArnoldiRunning nextCompletedSteps residualAfterRotation)

prepareRestartBasis :: Double -> GMRESWorkspace s -> ST s ()
prepareRestartBasis !betaValue workspace = do
  MU.set (gmresProjectedResidual workspace) 0.0
  MU.unsafeWrite (gmresProjectedResidual workspace) 0 betaValue
  copyMutableVector (gmresResidualVector workspace) (basisColumn workspace 0)
  scaleMutableVector (1.0 / betaValue) (basisColumn workspace 0)

orthogonalizeAgainstBasis :: GMRESWorkspace s -> Int -> ST s ()
orthogonalizeAgainstBasis workspace stepIndex =
  U.foldM' orthogonalizeColumn () (U.enumFromN 0 (stepIndex + 1))
  where
    orthogonalizeColumn () basisIndex = do
      coefficientValue <- dotMutableVector (gmresWorkVector workspace) (basisColumn workspace basisIndex)
      writeHessenbergEntry workspace basisIndex stepIndex coefficientValue
      addScaledMutableVector (negate coefficientValue) (basisColumn workspace basisIndex) (gmresWorkVector workspace)

writeNextBasisColumn :: GMRESWorkspace s -> Int -> Double -> ST s ()
writeNextBasisColumn workspace stepIndex nextBasisNorm =
  if nextBasisNorm <= solverEpsilon
    then pure ()
    else do
      copyMutableVector (gmresWorkVector workspace) (basisColumn workspace (stepIndex + 1))
      scaleMutableVector (1.0 / nextBasisNorm) (basisColumn workspace (stepIndex + 1))

applyPreviousRotations :: GMRESWorkspace s -> Int -> ExceptT SparseIterativeFailure (ST s) ()
applyPreviousRotations workspace stepIndex =
  U.mapM_ applyRotation (U.enumFromN 0 stepIndex)
  where
    applyRotation rotationIndex = do
      (cosValue, sinValue, firstEntry, secondEntry) <-
        lift $ do
          cosValue <- MU.unsafeRead (gmresCosines workspace) rotationIndex
          sinValue <- MU.unsafeRead (gmresSines workspace) rotationIndex
          firstEntry <- readHessenbergEntry workspace rotationIndex stepIndex
          secondEntry <- readHessenbergEntry workspace (rotationIndex + 1) stepIndex
          pure (cosValue, sinValue, firstEntry, secondEntry)
      let !firstRotated = cosValue * firstEntry + sinValue * secondEntry
          !secondRotated = negate sinValue * firstEntry + cosValue * secondEntry
      unless (fieldValueValid firstRotated && fieldValueValid secondRotated) $
        throwE (SparseInvalidInput "GMRES previous Givens rotation produced a non-finite Hessenberg entry")
      lift $ do
        writeHessenbergEntry workspace rotationIndex stepIndex firstRotated
        writeHessenbergEntry workspace (rotationIndex + 1) stepIndex secondRotated

applyNextRotation :: GMRESWorkspace s -> Int -> ExceptT SparseIterativeFailure (ST s) Double
applyNextRotation workspace stepIndex = do
  (diagonalEntry, subdiagonalEntry) <-
    lift $ do
      diagonalEntry <- readHessenbergEntry workspace stepIndex stepIndex
      subdiagonalEntry <- readHessenbergEntry workspace (stepIndex + 1) stepIndex
      pure (diagonalEntry, subdiagonalEntry)
  GivensRotation cosValue sinValue rValue <-
    except (gmresGivensCoefficients diagonalEntry subdiagonalEntry)
  (projectedEntry, projectedNext) <-
    lift $ do
      projectedEntry <- MU.unsafeRead (gmresProjectedResidual workspace) stepIndex
      projectedNext <- MU.unsafeRead (gmresProjectedResidual workspace) (stepIndex + 1)
      pure (projectedEntry, projectedNext)
  let !rotatedEntry = cosValue * projectedEntry + sinValue * projectedNext
      !rotatedNext = negate sinValue * projectedEntry + cosValue * projectedNext
  unless (fieldValueValid rotatedEntry && fieldValueValid rotatedNext) $
    throwE (SparseInvalidInput "GMRES Givens rotation produced a non-finite projected residual")
  lift $ do
    MU.unsafeWrite (gmresCosines workspace) stepIndex cosValue
    MU.unsafeWrite (gmresSines workspace) stepIndex sinValue
    writeHessenbergEntry workspace stepIndex stepIndex rValue
    writeHessenbergEntry workspace (stepIndex + 1) stepIndex 0.0
    MU.unsafeWrite (gmresProjectedResidual workspace) stepIndex rotatedEntry
    MU.unsafeWrite (gmresProjectedResidual workspace) (stepIndex + 1) rotatedNext
  pure (abs rotatedNext)

applyArnoldiCorrection :: MutableDoubleVector s -> GMRESWorkspace s -> ArnoldiState -> ExceptT SparseIterativeFailure (ST s) ()
applyArnoldiCorrection currentGuess workspace arnoldiState =
  case arnoldiStepCount arnoldiState of
    0 -> pure ()
    stepCount -> do
      solveProjectedUpperTriangular workspace stepCount
      lift $ do
        MU.set (gmresWorkVector workspace) 0.0
        U.foldM' addColumnContribution () (U.enumFromN 0 stepCount)
        addScaledMutableVector 1.0 (gmresWorkVector workspace) currentGuess
  where
    addColumnContribution () columnIndex = do
      coefficientValue <- MU.unsafeRead (gmresYValues workspace) columnIndex
      addScaledMutableVector coefficientValue (preconditionedColumn workspace columnIndex) (gmresWorkVector workspace)

gmresTrueResidualNorm ::
  SparseCSR Double ->
  U.Vector Double ->
  MutableDoubleVector s ->
  GMRESWorkspace s ->
  ST s Double
gmresTrueResidualNorm sparseMatrix rhsValues currentGuess workspace = do
  residualIntoMutable sparseMatrix rhsValues currentGuess (gmresImageVector workspace) (gmresResidualVector workspace)
  stableNormMutableVector (gmresResidualVector workspace)

solveProjectedUpperTriangular :: GMRESWorkspace s -> Int -> ExceptT SparseIterativeFailure (ST s) ()
solveProjectedUpperTriangular workspace stepCount = do
  projectedScale <- lift (projectedUpperScale workspace stepCount)
  unless (fieldValueValid projectedScale && projectedScale > 0.0) $
    throwE (SparseInvalidInput "GMRES projected triangular solve has no finite non-zero scale")
  U.mapM_ (solveRow projectedScale) (U.enumFromN 0 stepCount)
  where
    solveRow projectedScale reverseOffset = do
      let !rowIndex = stepCount - reverseOffset - 1
      (laterProduct, rhsValue, diagonalEntry) <-
        lift $ do
          laterProduct <- projectedLaterProduct workspace stepCount rowIndex
          rhsValue <- MU.unsafeRead (gmresProjectedResidual workspace) rowIndex
          diagonalEntry <- readHessenbergEntry workspace rowIndex rowIndex
          pure (laterProduct, rhsValue, diagonalEntry)
      let !numerator = rhsValue - laterProduct
          !diagonalThreshold = solverEpsilon * projectedScale
      unless (fieldValueValid rhsValue && fieldValueValid laterProduct && fieldValueValid numerator && fieldValueValid diagonalEntry) $
        throwE (SparseInvalidInput "GMRES projected triangular solve encountered non-finite arithmetic")
      when (abs diagonalEntry <= diagonalThreshold) $
        throwE (SparseInvalidInput "GMRES projected triangular solve encountered a zero or scale-negligible diagonal")
      let !solutionValue = numerator / diagonalEntry
      unless (fieldValueValid solutionValue) $
        throwE (SparseInvalidInput "GMRES projected triangular solve would write a non-finite correction")
      lift (MU.unsafeWrite (gmresYValues workspace) rowIndex solutionValue)

projectedLaterProduct :: GMRESWorkspace s -> Int -> Int -> ST s Double
projectedLaterProduct workspace stepCount rowIndex =
  U.foldM' accumulateLater 0.0 (U.enumFromN (rowIndex + 1) (stepCount - rowIndex - 1))
  where
    accumulateLater !accumulator columnIndex = do
      hEntry <- readHessenbergEntry workspace rowIndex columnIndex
      yValue <- MU.unsafeRead (gmresYValues workspace) columnIndex
      pure (accumulator + hEntry * yValue)

projectedUpperScale :: GMRESWorkspace s -> Int -> ST s Double
projectedUpperScale workspace stepCount =
  U.foldM'
    accumulateRowScale
    0.0
    (U.enumFromN 0 stepCount)
  where
    accumulateRowScale currentScale rowIndex =
      U.foldM'
        (\rowScale columnIndex -> max rowScale . abs <$> readHessenbergEntry workspace rowIndex columnIndex)
        currentScale
        (U.enumFromN rowIndex (stepCount - rowIndex))

gmresStateAfterArnoldi :: SparseGMRESConfig -> Int -> ArnoldiState -> Double -> Either SparseIterativeFailure GmresState
gmresStateAfterArnoldi config totalIterations arnoldiState trueResidualNorm
  | not (fieldValueValid trueResidualNorm) =
      Left (SparseInvalidInput "GMRES restart produced a non-finite true residual")
  | ArnoldiHappyBreakdown _ projectedResidualNorm <- arnoldiState,
    trueResidualNorm > sgcTolerance config =
      Left
        ( SparseInvalidInput
            ( "GMRES happy breakdown did not certify the true residual; projected residual "
                <> show projectedResidualNorm
                <> ", true residual "
                <> show trueResidualNorm
            )
        )
  | trueResidualNorm <= sgcTolerance config =
      Right (GmresConverged nextTotal trueResidualNorm)
  | otherwise = Right (GmresRunning nextTotal)
  where
    !nextTotal = totalIterations + arnoldiStepCount arnoldiState

gmresResultFromState :: Int -> MutableDoubleVector s -> GmresState -> ExceptT SparseIterativeFailure (ST s) SparseIterativeResult
gmresResultFromState iterationLimit currentGuess stateValue =
  case stateValue of
    GmresRunning _ -> throwE (SparseIterationBudgetExceeded iterationLimit)
    GmresConverged iterationCount residualNormValue -> do
      solutionVector <- lift (freezeMutableDoubleVector currentGuess)
      pure
        SparseIterativeResult
          { sparseSolution = solutionVector,
            sparseIterations = iterationCount,
            sparseResidualNorm = residualNormValue
          }

arnoldiStepCount :: ArnoldiState -> Int
arnoldiStepCount stateValue =
  case stateValue of
    ArnoldiRunning stepCount _ -> stepCount
    ArnoldiConverged stepCount _ -> stepCount
    ArnoldiHappyBreakdown stepCount _ -> stepCount

type GivensRotation :: Type
data GivensRotation = GivensRotation !Double !Double !Double

gmresGivensCoefficients :: Double -> Double -> Either SparseIterativeFailure GivensRotation
gmresGivensCoefficients diagonalEntry subdiagonalEntry
  | not (fieldValueValid diagonalEntry && fieldValueValid subdiagonalEntry) =
      Left (SparseInvalidInput "GMRES Givens rotation requires finite Hessenberg entries")
  | scaleValue == 0.0 = Right (GivensRotation 1.0 0.0 0.0)
  | otherwise =
      let scaledDiagonal = diagonalEntry / scaleValue
          scaledSubdiagonal = subdiagonalEntry / scaleValue
          radius = scaleValue * sqrt (scaledDiagonal * scaledDiagonal + scaledSubdiagonal * scaledSubdiagonal)
       in if fieldValueValid radius && radius > 0.0
            then Right (GivensRotation (diagonalEntry / radius) (subdiagonalEntry / radius) radius)
            else Left (SparseInvalidInput "GMRES Givens radius is not representable as a finite positive Double")
  where
    scaleValue = max (abs diagonalEntry) (abs subdiagonalEntry)

basisColumn :: GMRESWorkspace s -> Int -> MutableDoubleVector s
basisColumn workspace columnIndex =
  MU.unsafeSlice (columnIndex * gmresDimension workspace) (gmresDimension workspace) (gmresBasisPayload workspace)
{-# INLINE basisColumn #-}

preconditionedColumn :: GMRESWorkspace s -> Int -> MutableDoubleVector s
preconditionedColumn workspace columnIndex =
  MU.unsafeSlice (columnIndex * gmresDimension workspace) (gmresDimension workspace) (gmresPreconditionedPayload workspace)
{-# INLINE preconditionedColumn #-}

readHessenbergEntry :: GMRESWorkspace s -> Int -> Int -> ST s Double
readHessenbergEntry workspace rowIndex columnIndex =
  MU.unsafeRead (gmresHessenbergPayload workspace) (hessenbergOffset workspace rowIndex columnIndex)
{-# INLINE readHessenbergEntry #-}

writeHessenbergEntry :: GMRESWorkspace s -> Int -> Int -> Double -> ST s ()
writeHessenbergEntry workspace rowIndex columnIndex value =
  MU.unsafeWrite (gmresHessenbergPayload workspace) (hessenbergOffset workspace rowIndex columnIndex) value
{-# INLINE writeHessenbergEntry #-}

hessenbergOffset :: GMRESWorkspace s -> Int -> Int -> Int
hessenbergOffset workspace rowIndex columnIndex =
  rowIndex + columnIndex * gmresKrylovColumnCount workspace
{-# INLINE hessenbergOffset #-}

restartCycleCount :: Int -> Int -> Int
restartCycleCount iterationLimit restartDimension =
  if iterationLimit <= 0
    then 0
    else
      iterationLimit `quot` restartDimension
        + if iterationLimit `rem` restartDimension == 0 then 0 else 1

validateGmresPreconditioner :: SparseCSR Double -> SparsePreconditioner -> Either SparseIterativeFailure ()
validateGmresPreconditioner sparseMatrix preconditioner =
  if preconditionerDimension preconditioner == csrRows sparseMatrix
    then Right ()
    else Left (SparseInvalidInput "GMRES requires preconditioner dimension equal to matrix dimension")

stableNormMutableVector :: MutableDoubleVector s -> ST s Double
stableNormMutableVector vectorValue = do
  (scaleValue, scaledSumSquares) <-
    U.foldM'
      accumulateScaledSquare
      (0.0, 1.0)
      (U.enumFromN 0 (MU.length vectorValue))
  pure
    ( if scaleValue == 0.0
        then 0.0
        else scaleValue * sqrt scaledSumSquares
    )
  where
    accumulateScaledSquare (!scaleValue, !scaledSumSquares) entryIndex = do
      entryValue <- abs <$> MU.unsafeRead vectorValue entryIndex
      pure (accumulateEntry scaleValue scaledSumSquares entryValue)

    accumulateEntry :: Double -> Double -> Double -> (Double, Double)
    accumulateEntry scaleValue scaledSumSquares entryValue
      | not (fieldValueValid entryValue) = (entryValue, entryValue)
      | entryValue == 0.0 = (scaleValue, scaledSumSquares)
      | scaleValue < entryValue =
          ( entryValue,
            1.0 + scaledSumSquares * (scaleValue / entryValue) * (scaleValue / entryValue)
          )
      | otherwise =
          ( scaleValue,
            scaledSumSquares + (entryValue / scaleValue) * (entryValue / scaleValue)
          )
{-# INLINE stableNormMutableVector #-}

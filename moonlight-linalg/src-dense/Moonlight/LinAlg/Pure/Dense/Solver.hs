{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Pure.Dense.Solver
  ( solveDirect,
    solveCG,
    solveGMRES,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.Vector as Box
import qualified Data.Vector.Unboxed as U
import GHC.TypeNats (KnownNat)
import Moonlight.Core
  ( MoonlightError (..),
    checkedNonNegativeProduct,
    checkedNonNegativeSum,
  )
import Moonlight.LinAlg.Internal.Dense.DoubleFactorization (solveSquareLinearSystem)
import Moonlight.LinAlg.Internal.Primitives
  ( ColumnIndex,
    RowIndex,
    addVector,
    dotProduct,
    epsilon,
    linearCombination,
    matrixVectorProduct,
    mkColumnIndex,
    mkRowIndex,
    replaceColumnEntryChecked,
    replaceRowChecked,
    requireRow,
    rowIndices,
    scaleVector,
    subVector,
    vectorNorm,
  )
import Moonlight.LinAlg.Pure.Dense.Types (Matrix, Vector, fromListVector, toListMatrix, toListVector)
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

solveDirect ::
  forall n.
  KnownNat n =>
  Matrix n n Double ->
  Vector n Double ->
  Either MoonlightError (Vector n Double)
solveDirect matrixValue rightHandSide = do
  let (dimension, _) = DenseTypes.matrixShape matrixValue
  solutionValues <-
    solveSquareLinearSystem
      dimension
      (toListMatrix matrixValue)
      (toListVector rightHandSide)
  fromListVector @n solutionValues

solveCG ::
  forall n.
  KnownNat n =>
  Matrix n n Double ->
  Vector n Double ->
  Either MoonlightError (Vector n Double)
solveCG matrixValue rightHandSide = do
  matrixRows <- DenseTypes.matrixToRows matrixValue
  let (dimension, _) = DenseTypes.matrixShape matrixValue
  iterationLimit <- checkedSolverProduct "CG iteration budget" dimension 20
  let rhsValues = toListVector rightHandSide
      initialGuess = replicate dimension 0.0
      cgTolerance = 1.0e-10
      boundedIterationLimit = max 1 iterationLimit
  initialResidual <- subVector rhsValues =<< matrixVectorProduct matrixRows initialGuess
  initialResidualNormSquared <- dotProduct initialResidual initialResidual
  let iterateCg iterationIndex guessVector residualVector directionVector residualNormSquared
        | sqrt residualNormSquared <= cgTolerance = Right guessVector
        | iterationIndex >= boundedIterationLimit = Left (InvariantViolation "CG solver exhausted iteration budget")
        | otherwise = do
            imageDirection <- matrixVectorProduct matrixRows directionVector
            denominator <- dotProduct directionVector imageDirection
            directionNorm <- vectorNorm directionVector
            imageDirectionNorm <- vectorNorm imageDirection
            if nearZeroConjugateGradientDenominator denominator directionNorm imageDirectionNorm
              then Left (InvariantViolation "CG solver encountered near-zero denominator")
              else do
                let alphaValue = residualNormSquared / denominator
                nextGuess <- addVector guessVector (scaleVector alphaValue directionVector)
                nextResidual <- subVector residualVector (scaleVector alphaValue imageDirection)
                nextResidualNormSquared <- dotProduct nextResidual nextResidual
                if sqrt nextResidualNormSquared <= cgTolerance
                  then Right nextGuess
                  else do
                    let betaValue = nextResidualNormSquared / residualNormSquared
                    nextDirection <- addVector nextResidual (scaleVector betaValue directionVector)
                    iterateCg (iterationIndex + 1) nextGuess nextResidual nextDirection nextResidualNormSquared
  solutionValues <- iterateCg 0 initialGuess initialResidual initialResidual initialResidualNormSquared
  fromListVector @n solutionValues

nearZeroConjugateGradientDenominator :: Double -> Double -> Double -> Bool
nearZeroConjugateGradientDenominator denominator directionNorm imageDirectionNorm =
  abs denominator <= epsilon * directionNorm * imageDirectionNorm

updateEntry :: RowIndex -> ColumnIndex -> Double -> [[Double]] -> Either MoonlightError [[Double]]
updateEntry rowIndex columnIndex value matrixRows = do
  rowValues <-
    requireRow
      (InvariantViolation ("GMRES update entry row missing at index " <> show rowIndex))
      rowIndex
      matrixRows
  updatedRow <-
    replaceColumnEntryChecked
      (InvariantViolation ("GMRES update entry column missing at index " <> show columnIndex))
      columnIndex
      value
      rowValues
  replaceRowChecked
    (InvariantViolation ("GMRES row replacement failed at index " <> show rowIndex))
    rowIndex
    updatedRow
    matrixRows

solveHessenbergLeastSquares :: [[Double]] -> [Double] -> Either MoonlightError [Double]
solveHessenbergLeastSquares hRows rhsValues =
  let rows = Box.fromList (fmap U.fromList hRows)
      rhs = U.fromList rhsValues
      colCount = if Box.null rows then 0 else U.length (rows Box.! 0)
      givensRotation :: Double -> Double -> (Double, Double)
      givensRotation aVal bVal
        | abs bVal <= 1.0e-15 = (1.0, 0.0)
        | abs aVal <= 1.0e-15 = (0.0, if bVal >= 0 then 1.0 else -1.0)
        | otherwise =
            let rVal = sqrt (aVal * aVal + bVal * bVal)
             in (aVal / rVal, bVal / rVal)
      applyRotation ::
        Double ->
        Double ->
        Int ->
        Box.Vector (U.Vector Double) ->
        U.Vector Double ->
        (Box.Vector (U.Vector Double), U.Vector Double)
      applyRotation cs sn pivotCol rs b =
        let rowI = rs Box.! pivotCol
            rowJ = rs Box.! (pivotCol + 1)
            rotatedI = U.zipWith (\ri rj -> cs * ri + sn * rj) rowI rowJ
            rotatedJ = U.zipWith (\ri rj -> negate sn * ri + cs * rj) rowI rowJ
            updatedRs = rs Box.// [(pivotCol, rotatedI), (pivotCol + 1, rotatedJ)]
            bI = b U.! pivotCol
            bJ = b U.! (pivotCol + 1)
            updatedB = b U.// [(pivotCol, cs * bI + sn * bJ), (pivotCol + 1, negate sn * bI + cs * bJ)]
         in (updatedRs, updatedB)
      eliminateSubdiagonal col (rs, b)
        | col >= colCount = (rs, b)
        | col + 1 >= Box.length rs = (rs, b)
        | otherwise =
            let aVal = (rs Box.! col) U.! col
                bVal = (rs Box.! (col + 1)) U.! col
                (cs, sn) = givensRotation aVal bVal
                (nextRs, nextB) = applyRotation cs sn col rs b
             in eliminateSubdiagonal (col + 1) (nextRs, nextB)
      (triangularRows, transformedRhs) = eliminateSubdiagonal 0 (rows, rhs)
      backSolve col solution
        | col < 0 = solution
        | otherwise =
            let rowVec = triangularRows Box.! col
                diagVal = rowVec U.! col
                trailingLen = colCount - col - 1
                trailingSum =
                  if trailingLen <= 0
                    then 0.0
                    else U.sum (U.zipWith (*) (U.slice (col + 1) trailingLen rowVec) (U.slice (col + 1) trailingLen solution))
                rhsVal = transformedRhs U.! col
                solvedVal = if abs diagVal <= 1.0e-15 then 0.0 else (rhsVal - trailingSum) / diagVal
             in backSolve (col - 1) (solution U.// [(col, solvedVal)])
   in Right (U.toList (backSolve (colCount - 1) (U.replicate colCount 0.0)))

solveGMRES ::
  forall n.
  KnownNat n =>
  Matrix n n Double ->
  Vector n Double ->
  Either MoonlightError (Vector n Double)
solveGMRES matrixValue rightHandSide = do
  matrixRows <- DenseTypes.matrixToRows matrixValue
  let (dimension, _) = DenseTypes.matrixShape matrixValue
      maxIterations = max 1 dimension
  krylovRowCount <- checkedSolverSum "GMRES Krylov row count" maxIterations 1
  _ <- checkedSolverProduct "GMRES Hessenberg workspace" krylovRowCount maxIterations
  let rhsValues = toListVector rightHandSide
      gmresTolerance = 1.0e-10
  betaValue <- vectorNorm rhsValues
  if betaValue <= gmresTolerance
    then fromListVector @n (replicate dimension 0.0)
    else do
      let firstBasis = scaleVector (1.0 / betaValue) rhsValues
          initialHessenberg = replicate krylovRowCount (replicate maxIterations 0.0)
          arnoldiStep iterationIndex basisVectors hessenbergRows
            | iterationIndex >= maxIterations = Right (basisVectors, hessenbergRows, maxIterations)
            | otherwise = do
                iterationBasisIndex <-
                  mkRowIndex
                    (InvariantViolation ("GMRES basis index out of bounds at iteration " <> show iterationIndex))
                    (length basisVectors)
                    iterationIndex
                iterationColumnIndex <-
                  mkColumnIndex
                    (InvariantViolation ("GMRES Hessenberg column out of bounds at iteration " <> show iterationIndex))
                    maxIterations
                    iterationIndex
                currentBasis <-
                  requireRow
                    (InvariantViolation ("GMRES basis lookup failed at iteration " <> show iterationBasisIndex))
                    iterationBasisIndex
                    basisVectors
                initialVector <- matrixVectorProduct matrixRows currentBasis
                let orthogonalize (workingVector, workingHessenberg) (basisIndex, basisVectorValue) = do
                      coefficient <- dotProduct basisVectorValue workingVector
                      nextVector <- subVector workingVector (scaleVector coefficient basisVectorValue)
                      nextHessenberg <- updateEntry basisIndex iterationColumnIndex coefficient workingHessenberg
                      Right (nextVector, nextHessenberg)
                (reducedVector, filledHessenberg) <-
                  foldM orthogonalize (initialVector, hessenbergRows) (zip (take (iterationIndex + 1) (rowIndices (length basisVectors))) basisVectors)
                nextNorm <- vectorNorm reducedVector
                nextRowIndex <-
                  mkRowIndex
                    (InvariantViolation ("GMRES Hessenberg row out of bounds at iteration " <> show (iterationIndex + 1)))
                    (length filledHessenberg)
                    (iterationIndex + 1)
                completedHessenberg <- updateEntry nextRowIndex iterationColumnIndex nextNorm filledHessenberg
                if nextNorm <= gmresTolerance
                  then Right (basisVectors, completedHessenberg, iterationIndex + 1)
                  else
                    let nextBasis = basisVectors <> [scaleVector (1.0 / nextNorm) reducedVector]
                     in arnoldiStep (iterationIndex + 1) nextBasis completedHessenberg
      (basisVectors, hessenbergRows, iterationCount) <- arnoldiStep 0 [firstBasis] initialHessenberg
      let reducedHessenberg = map (take iterationCount) (take (iterationCount + 1) hessenbergRows)
          leastSquaresRhs = betaValue : replicate iterationCount 0.0
      coefficients <- solveHessenbergLeastSquares reducedHessenberg leastSquaresRhs
      solutionValues <- linearCombination (zip coefficients (take iterationCount basisVectors))
      fromListVector @n solutionValues

checkedSolverProduct :: String -> Int -> Int -> Either MoonlightError Int
checkedSolverProduct context leftFactor rightFactor =
  first
    (const (InvariantViolation (context <> " exceeds Int range")))
    (checkedNonNegativeProduct leftFactor rightFactor)

checkedSolverSum :: String -> Int -> Int -> Either MoonlightError Int
checkedSolverSum context leftTerm rightTerm =
  first
    (const (InvariantViolation (context <> " exceeds Int range")))
    (checkedNonNegativeSum leftTerm rightTerm)

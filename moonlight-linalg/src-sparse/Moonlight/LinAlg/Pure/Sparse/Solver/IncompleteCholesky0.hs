{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Pure.Sparse.Solver.IncompleteCholesky0
  ( IC0Factor,
    ic0FactorDimension,
    incompleteCholesky0Factor,
    applyIC0FactorMutable,
    applyIC0FactorAndDotMutable,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Moonlight.Core (fieldValueValid)
import Moonlight.LinAlg.Pure.Sparse.Solver.Common (solverEpsilon)
import Moonlight.LinAlg.Pure.Sparse.Solver.Mutable
  ( MutableDoubleVector,
    copyMutableVector,
    dotMutableVector,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Types
  ( IC0Config (..),
    SparseIterativeFailure (..),
  )
import Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCSR,
    csrCols,
    csrColumnIndicesVector,
    csrRows,
    csrRowOffsetsVector,
    csrValuesVector,
  )
import Prelude

type IC0Factor :: Type
data IC0Factor = IC0Factor
  { ic0FactorDimension :: !Int,
    ic0FactorRowOffsets :: !(U.Vector Int),
    ic0FactorColumnIndices :: !(U.Vector Int),
    ic0FactorValues :: !(U.Vector Double),
    ic0FactorDiagonal :: !(U.Vector Double),
    ic0FactorPivots :: !(U.Vector Double)
  }
  deriving stock (Eq, Show)

type IC0SymbolicPattern :: Type
data IC0SymbolicPattern = IC0SymbolicPattern
  { ic0PatternRowOffsets :: !(U.Vector Int),
    ic0PatternColumnIndices :: !(U.Vector Int),
    ic0PatternValues :: !(U.Vector Double),
    ic0PatternDiagonalValues :: !(U.Vector Double),
    ic0PatternSuspectedNullspace :: !Bool
  }

type IC0RowPayload :: Type
data IC0RowPayload = IC0RowPayload
  { ic0RowColumns :: ![Int],
    ic0RowValues :: ![Double],
    ic0RowDiagonal :: !(Maybe Double),
    ic0RowSum :: !Double
  }

incompleteCholesky0Factor ::
  IC0Config ->
  SparseCSR Double ->
  Either SparseIterativeFailure IC0Factor
incompleteCholesky0Factor configValue sparseMatrix = do
  shiftValue <- validateIC0Shift configValue
  validateIC0Shape sparseMatrix
  symbolicPattern <- ic0SymbolicPattern sparseMatrix
  factorIC0SymbolicPattern
    shiftValue
    symbolicPattern

applyIC0FactorMutable ::
  IC0Factor ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
applyIC0FactorMutable factorValue sourceVector scratchVector targetVector = do
  ic0ForwardSolveIntoMutable factorValue sourceVector scratchVector
  ic0BackwardSolveIntoMutable factorValue scratchVector targetVector
{-# INLINE applyIC0FactorMutable #-}

applyIC0FactorAndDotMutable ::
  IC0Factor ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s Double
applyIC0FactorAndDotMutable factorValue sourceVector scratchVector targetVector = do
  applyIC0FactorMutable factorValue sourceVector scratchVector targetVector
  dotMutableVector sourceVector targetVector
{-# INLINE applyIC0FactorAndDotMutable #-}

validateIC0Shift :: IC0Config -> Either SparseIterativeFailure Double
validateIC0Shift configValue =
  case ic0DiagonalShift configValue of
    Nothing -> Right 0.0
    Just shiftValue
      | fieldValueValid shiftValue && shiftValue >= 0.0 -> Right shiftValue
      | otherwise -> Left (SparseInvalidDiagonalShift shiftValue)

validateIC0Shape :: SparseCSR Double -> Either SparseIterativeFailure ()
validateIC0Shape sparseMatrix
  | csrRows sparseMatrix /= csrCols sparseMatrix =
      Left (SparseNonSquareSparsePreconditioner (csrRows sparseMatrix) (csrCols sparseMatrix))
  | otherwise = Right ()

ic0SymbolicPattern ::
  SparseCSR Double ->
  Either SparseIterativeFailure IC0SymbolicPattern
ic0SymbolicPattern sparseMatrix = do
  rows <- traverse (ic0RowPayload sparseMatrix) [0 .. dimension - 1]
  let diagonalValues = traverse ic0DiagonalFromPayload (zip [0 ..] rows)
  case diagonalValues of
    Left failureValue -> Left failureValue
    Right rowDiagonals ->
      let !rowCounts = length . ic0RowColumns <$> rows
          !rowOffsets = U.fromList (scanl (+) 0 rowCounts)
          !columnIndices = U.fromList (ic0RowColumns =<< rows)
          !lowerValues = U.fromList (ic0RowValues =<< rows)
          !diagonalVector = U.fromList rowDiagonals
          !nullspaceLike =
            all
              (\rowValue -> abs (ic0RowSum rowValue) <= solverEpsilon)
              rows
       in Right
            IC0SymbolicPattern
              { ic0PatternRowOffsets = rowOffsets,
                ic0PatternColumnIndices = columnIndices,
                ic0PatternValues = lowerValues,
                ic0PatternDiagonalValues = diagonalVector,
                ic0PatternSuspectedNullspace = nullspaceLike
              }
  where
    !dimension = csrRows sparseMatrix

ic0DiagonalFromPayload ::
  (Int, IC0RowPayload) ->
  Either SparseIterativeFailure Double
ic0DiagonalFromPayload (rowIndex, rowValue) =
  case ic0RowDiagonal rowValue of
    Nothing -> Left (SparseMissingDiagonal rowIndex)
    Just diagonalValue -> Right diagonalValue

ic0RowPayload ::
  SparseCSR Double ->
  Int ->
  Either SparseIterativeFailure IC0RowPayload
ic0RowPayload sparseMatrix rowIndex =
  collectEntries startOffset [] [] Nothing 0.0
  where
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !values = csrValuesVector sparseMatrix
    !startOffset = rowOffsets `U.unsafeIndex` rowIndex
    !endOffset = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    collectEntries !entryIndex !columnsRev !valuesRev !diagonalValue !rowSum
      | entryIndex >= endOffset =
          Right
            IC0RowPayload
              { ic0RowColumns = reverse columnsRev,
                ic0RowValues = reverse valuesRev,
                ic0RowDiagonal = diagonalValue,
                ic0RowSum = rowSum
              }
      | otherwise =
          let !columnIndex = columnIndices `U.unsafeIndex` entryIndex
              !entryValue = values `U.unsafeIndex` entryIndex
              !nextRowSum = rowSum + entryValue
           in if not (fieldValueValid entryValue)
                then Left (SparseNonFiniteUpdate rowIndex columnIndex entryValue)
                else
                  case compare columnIndex rowIndex of
                    LT ->
                      case findCSRValue sparseMatrix columnIndex rowIndex of
                        Nothing -> Left (SparseStructuralAsymmetry rowIndex columnIndex)
                        Just _ ->
                          collectEntries
                            (entryIndex + 1)
                            (columnIndex : columnsRev)
                            (entryValue : valuesRev)
                            diagonalValue
                            nextRowSum
                    EQ ->
                      collectEntries
                        (entryIndex + 1)
                        columnsRev
                        valuesRev
                        (Just entryValue)
                        nextRowSum
                    GT ->
                      case findCSRValue sparseMatrix columnIndex rowIndex of
                        Nothing -> Left (SparseStructuralAsymmetry rowIndex columnIndex)
                        Just _ ->
                          collectEntries
                            (entryIndex + 1)
                            columnsRev
                            valuesRev
                            diagonalValue
                            nextRowSum

factorIC0SymbolicPattern ::
  Double ->
  IC0SymbolicPattern ->
  Either SparseIterativeFailure IC0Factor
factorIC0SymbolicPattern !shiftValue symbolicPattern =
  runST $ do
    lowerValues <- U.thaw (ic0PatternValues symbolicPattern)
    diagonalValues <- MU.unsafeNew dimension
    pivotValues <- MU.unsafeNew dimension
    resultValue <-
      factorRows
        lowerValues
        diagonalValues
        pivotValues
        0
    case resultValue of
      Left failureValue -> pure (Left failureValue)
      Right () -> do
        frozenValues <- U.unsafeFreeze lowerValues
        frozenDiagonals <- U.unsafeFreeze diagonalValues
        frozenPivots <- U.unsafeFreeze pivotValues
        pure
          ( Right
              IC0Factor
                { ic0FactorDimension = dimension,
                  ic0FactorRowOffsets = ic0PatternRowOffsets symbolicPattern,
                  ic0FactorColumnIndices = ic0PatternColumnIndices symbolicPattern,
                  ic0FactorValues = frozenValues,
                  ic0FactorDiagonal = frozenDiagonals,
                  ic0FactorPivots = frozenPivots
                }
          )
  where
    !rowOffsets = ic0PatternRowOffsets symbolicPattern
    !columnIndices = ic0PatternColumnIndices symbolicPattern
    !matrixValues = ic0PatternValues symbolicPattern
    !matrixDiagonal = ic0PatternDiagonalValues symbolicPattern
    !dimension = U.length matrixDiagonal

    factorRows ::
      MU.MVector s Double ->
      MU.MVector s Double ->
      MU.MVector s Double ->
      Int ->
      ST s (Either SparseIterativeFailure ())
    factorRows lowerValues diagonalValues pivotValues !rowIndex
      | rowIndex >= dimension = pure (Right ())
      | otherwise = do
          let !rowStart = rowOffsets `U.unsafeIndex` rowIndex
              !rowEnd = rowOffsets `U.unsafeIndex` (rowIndex + 1)
          offDiagonalResult <-
            factorStrictLowerRow
              lowerValues
              diagonalValues
              rowIndex
              rowStart
          case offDiagonalResult of
            Left failureValue -> pure (Left failureValue)
            Right () -> do
              correctionValue <- lowerRowSquared lowerValues rowStart rowEnd 0.0
              let !pivotValue =
                    (matrixDiagonal `U.unsafeIndex` rowIndex)
                      + shiftValue
                      - correctionValue
              if not (fieldValueValid pivotValue)
                then pure (Left (SparseNonFiniteUpdate rowIndex rowIndex pivotValue))
                else
                  if pivotValue <= solverEpsilon
                    then
                      pure
                        ( Left
                            ( if ic0PatternSuspectedNullspace symbolicPattern
                                then SparseSuspectedNullspaceUnanchoredLaplacian rowIndex pivotValue
                                else SparseNonpositivePivot rowIndex pivotValue
                            )
                        )
                    else do
                      MU.unsafeWrite pivotValues rowIndex pivotValue
                      MU.unsafeWrite diagonalValues rowIndex (sqrt pivotValue)
                      factorRows
                        lowerValues
                        diagonalValues
                        pivotValues
                        (rowIndex + 1)

    factorStrictLowerRow ::
      MU.MVector s Double ->
      MU.MVector s Double ->
      Int ->
      Int ->
      ST s (Either SparseIterativeFailure ())
    factorStrictLowerRow lowerValues diagonalValues !rowIndex !entryIndex
      | entryIndex >= rowEnd = pure (Right ())
      | otherwise = do
          let !columnIndex = columnIndices `U.unsafeIndex` entryIndex
              !matrixValue = matrixValues `U.unsafeIndex` entryIndex
          correctionValue <-
            lowerIntersectionProduct
              lowerValues
              rowIndex
              columnIndex
              rowStart
              (rowOffsets `U.unsafeIndex` columnIndex)
              0.0
          pivotDiagonal <- MU.unsafeRead diagonalValues columnIndex
          let !factorValue = (matrixValue - correctionValue) / pivotDiagonal
          if not (fieldValueValid factorValue)
            then pure (Left (SparseNonFiniteUpdate rowIndex columnIndex factorValue))
            else do
              MU.unsafeWrite lowerValues entryIndex factorValue
              factorStrictLowerRow
                lowerValues
                diagonalValues
                rowIndex
                (entryIndex + 1)
      where
        !rowStart = rowOffsets `U.unsafeIndex` rowIndex
        !rowEnd = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    lowerIntersectionProduct ::
      MU.MVector s Double ->
      Int ->
      Int ->
      Int ->
      Int ->
      Double ->
      ST s Double
    lowerIntersectionProduct lowerValues !rowIndex !columnIndex !leftEntry !rightEntry !accumulator =
      let !leftEnd = rowOffsets `U.unsafeIndex` (rowIndex + 1)
          !rightEnd = rowOffsets `U.unsafeIndex` (columnIndex + 1)
       in if leftEntry >= leftEnd || rightEntry >= rightEnd
            then pure accumulator
            else
              let !leftColumn = columnIndices `U.unsafeIndex` leftEntry
                  !rightColumn = columnIndices `U.unsafeIndex` rightEntry
               in if leftColumn >= columnIndex || rightColumn >= columnIndex
                    then pure accumulator
                    else
                      case compare leftColumn rightColumn of
                        LT ->
                          lowerIntersectionProduct
                            lowerValues
                            rowIndex
                            columnIndex
                            (leftEntry + 1)
                            rightEntry
                            accumulator
                        EQ -> do
                          leftValue <- MU.unsafeRead lowerValues leftEntry
                          rightValue <- MU.unsafeRead lowerValues rightEntry
                          lowerIntersectionProduct
                            lowerValues
                            rowIndex
                            columnIndex
                            (leftEntry + 1)
                            (rightEntry + 1)
                            (accumulator + leftValue * rightValue)
                        GT ->
                          lowerIntersectionProduct
                            lowerValues
                            rowIndex
                            columnIndex
                            leftEntry
                            (rightEntry + 1)
                            accumulator

lowerRowSquared ::
  MU.MVector s Double ->
  Int ->
  Int ->
  Double ->
  ST s Double
lowerRowSquared lowerValues !entryIndex !endEntry !accumulator
  | entryIndex >= endEntry = pure accumulator
  | otherwise = do
      factorValue <- MU.unsafeRead lowerValues entryIndex
      lowerRowSquared
        lowerValues
        (entryIndex + 1)
        endEntry
        (accumulator + factorValue * factorValue)
{-# INLINE lowerRowSquared #-}

ic0ForwardSolveIntoMutable ::
  IC0Factor ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
ic0ForwardSolveIntoMutable factorValue sourceVector targetVector =
  solveRows 0
  where
    !dimension = ic0FactorDimension factorValue
    !rowOffsets = ic0FactorRowOffsets factorValue
    !columnIndices = ic0FactorColumnIndices factorValue
    !factorValues = ic0FactorValues factorValue
    !diagonalValues = ic0FactorDiagonal factorValue

    solveRows !rowIndex
      | rowIndex >= dimension = pure ()
      | otherwise = do
          rhsValue <- MU.unsafeRead sourceVector rowIndex
          knownProduct <-
            lowerKnownProduct
              columnIndices
              factorValues
              targetVector
              (rowOffsets `U.unsafeIndex` rowIndex)
              (rowOffsets `U.unsafeIndex` (rowIndex + 1))
              0.0
          MU.unsafeWrite
            targetVector
            rowIndex
            ( (rhsValue - knownProduct)
                / (diagonalValues `U.unsafeIndex` rowIndex)
            )
          solveRows (rowIndex + 1)
{-# INLINE ic0ForwardSolveIntoMutable #-}

ic0BackwardSolveIntoMutable ::
  IC0Factor ->
  MutableDoubleVector s ->
  MutableDoubleVector s ->
  ST s ()
ic0BackwardSolveIntoMutable factorValue sourceVector targetVector = do
  copyMutableVector sourceVector targetVector
  solveRows (dimension - 1)
  where
    !dimension = ic0FactorDimension factorValue
    !rowOffsets = ic0FactorRowOffsets factorValue
    !columnIndices = ic0FactorColumnIndices factorValue
    !factorValues = ic0FactorValues factorValue
    !diagonalValues = ic0FactorDiagonal factorValue

    solveRows !rowIndex
      | rowIndex < 0 = pure ()
      | otherwise = do
          rhsValue <- MU.unsafeRead targetVector rowIndex
          let !solutionValue = rhsValue / (diagonalValues `U.unsafeIndex` rowIndex)
          MU.unsafeWrite targetVector rowIndex solutionValue
          scatterLowerTranspose
            (rowOffsets `U.unsafeIndex` rowIndex)
            (rowOffsets `U.unsafeIndex` (rowIndex + 1))
            solutionValue
          solveRows (rowIndex - 1)

    scatterLowerTranspose !entryIndex !endEntry !solutionValue
      | entryIndex >= endEntry = pure ()
      | otherwise = do
          let !columnIndex = columnIndices `U.unsafeIndex` entryIndex
              !factorEntry = factorValues `U.unsafeIndex` entryIndex
          targetValue <- MU.unsafeRead targetVector columnIndex
          MU.unsafeWrite
            targetVector
            columnIndex
            (targetValue - factorEntry * solutionValue)
          scatterLowerTranspose
            (entryIndex + 1)
            endEntry
            solutionValue
{-# INLINE ic0BackwardSolveIntoMutable #-}

lowerKnownProduct ::
  U.Vector Int ->
  U.Vector Double ->
  MutableDoubleVector s ->
  Int ->
  Int ->
  Double ->
  ST s Double
lowerKnownProduct columnIndices factorValues targetVector !entryIndex !endEntry !accumulator
  | entryIndex >= endEntry = pure accumulator
  | otherwise = do
      let !columnIndex = columnIndices `U.unsafeIndex` entryIndex
          !factorValue = factorValues `U.unsafeIndex` entryIndex
      targetValue <- MU.unsafeRead targetVector columnIndex
      lowerKnownProduct
        columnIndices
        factorValues
        targetVector
        (entryIndex + 1)
        endEntry
        (accumulator + factorValue * targetValue)
{-# INLINE lowerKnownProduct #-}

findCSRValue :: SparseCSR Double -> Int -> Int -> Maybe Double
findCSRValue sparseMatrix rowIndex columnIndex =
  binarySearch startOffset endOffset
  where
    !rowOffsets = csrRowOffsetsVector sparseMatrix
    !columnIndices = csrColumnIndicesVector sparseMatrix
    !values = csrValuesVector sparseMatrix
    !startOffset = rowOffsets `U.unsafeIndex` rowIndex
    !endOffset = rowOffsets `U.unsafeIndex` (rowIndex + 1)

    binarySearch !lo !hi
      | lo >= hi = Nothing
      | otherwise =
          let !mid = lo + ((hi - lo) `div` 2)
              !midColumn = columnIndices `U.unsafeIndex` mid
           in case compare midColumn columnIndex of
                LT -> binarySearch (mid + 1) hi
                EQ -> Just (values `U.unsafeIndex` mid)
                GT -> binarySearch lo mid
{-# INLINE findCSRValue #-}

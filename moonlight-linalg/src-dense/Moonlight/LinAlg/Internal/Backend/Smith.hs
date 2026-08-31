{-# LANGUAGE AllowAmbiguousTypes #-}

module Moonlight.LinAlg.Internal.Backend.Smith
  ( SmithNormalForm (..),
    SmithDiagonalForm (..),
    smithNormalFormPure,
    smithDiagonalFormPure,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Vector qualified as Box
import GHC.TypeNats (KnownNat, Nat)
import Moonlight.Algebra.Pure.Ring
  ( EuclideanDomain (..),
    GCDDomain (..),
    IntegralDomain (..),
    mkNonZeroDivisor,
  )
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MoonlightError (..),
    MultiplicativeMonoid (..),
    checkedNonNegativeProduct,
  )
import Moonlight.LinAlg.Internal.Backend.RowOps (identityRows)
import Moonlight.LinAlg.Internal.Backend.RowStore
  ( RowStore,
    columnStore,
    replaceColumnStore,
    replaceRowStore,
    rowStoreFlatten,
    rowStoreFromRows,
    rowStoreRowAt,
    rowStoreShape,
    rowStoreValueAt,
    swapColumnsStore,
    swapRowsStore,
  )
import Moonlight.LinAlg.Internal.Primitives
  ( ColumnIndex,
    RowIndex,
    columnIndexInt,
    columnIndices,
    mkColumnIndex,
    mkRowIndex,
    rowIndexInt,
    rowIndices,
  )
import Moonlight.LinAlg.Pure.Dense.Types
  ( Matrix,
    fromListMatrix,
  )
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

type SmithNormalForm :: Nat -> Nat -> Type -> Type
data SmithNormalForm r c a = SmithNormalForm
  { smithLeft :: Matrix r r a,
    smithDiagonal :: Matrix r c a,
    smithRight :: Matrix c c a,
    smithLeftInverse :: Matrix r r a,
    smithRightInverse :: Matrix c c a
  }

type SmithDiagonalForm :: Nat -> Nat -> Type -> Type
newtype SmithDiagonalForm r c a = SmithDiagonalForm
  { smithDiagonalMatrix :: Matrix r c a
  }

type SmithWitnessState :: Type -> Type
data SmithWitnessState a = SmithWitnessState
  { smithWitnessLeft :: RowStore a,
    smithWitnessRight :: RowStore a,
    smithWitnessLeftInverse :: RowStore a,
    smithWitnessRightInverse :: RowStore a
  }
  deriving stock (Eq)

type SmithState :: Type -> Type
data SmithState a = SmithState
  { smithStateMatrix :: RowStore a,
    smithStateWitness :: Maybe (SmithWitnessState a)
  }
  deriving stock (Eq)

matrixValueAt :: RowIndex -> ColumnIndex -> RowStore a -> Either MoonlightError a
matrixValueAt rowIndex columnIndex =
  rowStoreValueAt
    (InvariantViolation ("Smith normal form entry lookup failed at " <> show (rowIndex, columnIndex)))
    rowIndex
    columnIndex

rowIndexAsColumn :: Int -> RowIndex -> Either MoonlightError ColumnIndex
rowIndexAsColumn columnCount rowIndex =
  mkColumnIndex
    (InvariantViolation ("Smith inverse witness row/column conversion failed at row " <> show rowIndex))
    columnCount
    (rowIndexInt rowIndex)

columnIndexAsRow :: Int -> ColumnIndex -> Either MoonlightError RowIndex
columnIndexAsRow rowCount columnIndex =
  mkRowIndex
    (InvariantViolation ("Smith inverse witness column/row conversion failed at column " <> show columnIndex))
    rowCount
    (columnIndexInt columnIndex)

swapRowsState :: RowIndex -> RowIndex -> SmithState a -> Either MoonlightError (SmithState a)
swapRowsState leftIndex rightIndex stateValue = do
  swappedMatrix <-
    swapRowsStore
      (InvariantViolation ("Smith row swap failed at " <> show (leftIndex, rightIndex)))
      leftIndex
      rightIndex
      (smithStateMatrix stateValue)
  swappedWitness <- traverse swapWitness (smithStateWitness stateValue)
  Right
    stateValue
      { smithStateMatrix = swappedMatrix,
        smithStateWitness = swappedWitness
      }
  where
    swapWitness witnessValue = do
      swappedLeft <-
        swapRowsStore
          (InvariantViolation ("Smith left witness row swap failed at " <> show (leftIndex, rightIndex)))
          leftIndex
          rightIndex
          (smithWitnessLeft witnessValue)
      leftColumn <- rowIndexAsColumn (fst (rowStoreShape (smithWitnessLeftInverse witnessValue))) leftIndex
      rightColumn <- rowIndexAsColumn (fst (rowStoreShape (smithWitnessLeftInverse witnessValue))) rightIndex
      swappedLeftInverse <-
        swapColumnsStore
          (InvariantViolation ("Smith left inverse column swap failed at " <> show (leftColumn, rightColumn)))
          leftColumn
          rightColumn
          (smithWitnessLeftInverse witnessValue)
      Right
        witnessValue
          { smithWitnessLeft = swappedLeft,
            smithWitnessLeftInverse = swappedLeftInverse
          }

swapColsState :: ColumnIndex -> ColumnIndex -> SmithState a -> Either MoonlightError (SmithState a)
swapColsState leftIndex rightIndex stateValue = do
  swappedMatrix <-
    swapColumnsStore
      (InvariantViolation ("Smith column swap failed at " <> show (leftIndex, rightIndex)))
      leftIndex
      rightIndex
      (smithStateMatrix stateValue)
  swappedWitness <- traverse swapWitness (smithStateWitness stateValue)
  Right
    stateValue
      { smithStateMatrix = swappedMatrix,
        smithStateWitness = swappedWitness
      }
  where
    swapWitness witnessValue = do
      swappedRight <-
        swapColumnsStore
          (InvariantViolation ("Smith right witness column swap failed at " <> show (leftIndex, rightIndex)))
          leftIndex
          rightIndex
          (smithWitnessRight witnessValue)
      leftRow <- columnIndexAsRow (fst (rowStoreShape (smithWitnessRightInverse witnessValue))) leftIndex
      rightRow <- columnIndexAsRow (fst (rowStoreShape (smithWitnessRightInverse witnessValue))) rightIndex
      swappedRightInverse <-
        swapRowsStore
          (InvariantViolation ("Smith right inverse row swap failed at " <> show (leftRow, rightRow)))
          leftRow
          rightRow
          (smithWitnessRightInverse witnessValue)
      Right
        witnessValue
          { smithWitnessRight = swappedRight,
            smithWitnessRightInverse = swappedRightInverse
          }

rowLinearCombination ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  a ->
  Box.Vector a ->
  a ->
  Box.Vector a ->
  Either MoonlightError (Box.Vector a)
rowLinearCombination leftCoefficient leftRow rightCoefficient rightRow =
  if Box.length leftRow == Box.length rightRow
    then
      Right
        ( Box.zipWith
            (\leftEntry rightEntry -> (leftCoefficient `mul` leftEntry) `add` (rightCoefficient `mul` rightEntry))
            leftRow
            rightRow
        )
    else Left (InvariantViolation "Smith row combination length mismatch")

rowCombine ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  Box.Vector a ->
  Box.Vector a ->
  a ->
  Either MoonlightError (Box.Vector a)
rowCombine targetRow sourceRow coefficient =
  rowLinearCombination one targetRow (neg coefficient) sourceRow

rowAddScaled ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  Box.Vector a ->
  Box.Vector a ->
  a ->
  Either MoonlightError (Box.Vector a)
rowAddScaled targetRow sourceRow coefficient =
  rowLinearCombination one targetRow coefficient sourceRow

replaceRowPair ::
  MoonlightError ->
  RowIndex ->
  Box.Vector a ->
  RowIndex ->
  Box.Vector a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
replaceRowPair failure leftIndex leftRow rightIndex rightRow rows =
  replaceRowStore failure leftIndex leftRow rows
    >>= replaceRowStore failure rightIndex rightRow

replaceColumnPair ::
  MoonlightError ->
  ColumnIndex ->
  Box.Vector a ->
  ColumnIndex ->
  Box.Vector a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
replaceColumnPair failure leftIndex leftColumn rightIndex rightColumn rows =
  replaceColumnStore failure leftIndex leftColumn rows
    >>= replaceColumnStore failure rightIndex rightColumn

rowPairTransform ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  MoonlightError ->
  RowIndex ->
  RowIndex ->
  a ->
  a ->
  a ->
  a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
rowPairTransform failure leftIndex rightIndex aa ab ba bb rows = do
  leftRow <- rowStoreRowAt failure leftIndex rows
  rightRow <- rowStoreRowAt failure rightIndex rows
  transformedLeft <- rowLinearCombination aa leftRow ab rightRow
  transformedRight <- rowLinearCombination ba leftRow bb rightRow
  replaceRowPair failure leftIndex transformedLeft rightIndex transformedRight rows

columnPairTransform ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  MoonlightError ->
  ColumnIndex ->
  ColumnIndex ->
  a ->
  a ->
  a ->
  a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
columnPairTransform failure leftIndex rightIndex aa ab ba bb rows = do
  leftColumn <- columnStore failure leftIndex rows
  rightColumn <- columnStore failure rightIndex rows
  transformedLeft <- rowLinearCombination aa leftColumn ab rightColumn
  transformedRight <- rowLinearCombination ba leftColumn bb rightColumn
  replaceColumnPair failure leftIndex transformedLeft rightIndex transformedRight rows

columnAddScaledInRows ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  ColumnIndex ->
  ColumnIndex ->
  a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
columnAddScaledInRows targetIndex sourceIndex coefficient rows = do
  targetColumn <- columnStore (InvariantViolation ("Smith column lookup failed at column " <> show targetIndex)) targetIndex rows
  sourceColumn <- columnStore (InvariantViolation ("Smith column lookup failed at column " <> show sourceIndex)) sourceIndex rows
  updatedColumn <- rowAddScaled targetColumn sourceColumn coefficient
  replaceColumnStore
    (InvariantViolation ("Smith column replacement failed at column " <> show targetIndex))
    targetIndex
    updatedColumn
    rows

rowAddScaledInRows ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  RowIndex ->
  RowIndex ->
  a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
rowAddScaledInRows targetIndex sourceIndex coefficient rows = do
  targetRow <-
    rowStoreRowAt
      (InvariantViolation ("Smith inverse witness target row missing at index " <> show targetIndex))
      targetIndex
      rows
  sourceRow <-
    rowStoreRowAt
      (InvariantViolation ("Smith inverse witness source row missing at index " <> show sourceIndex))
      sourceIndex
      rows
  updatedRow <- rowAddScaled targetRow sourceRow coefficient
  replaceRowStore
    (InvariantViolation ("Smith inverse witness row replacement failed at index " <> show targetIndex))
    targetIndex
    updatedRow
    rows

rowCombineState ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  RowIndex ->
  RowIndex ->
  a ->
  SmithState a ->
  Either MoonlightError (SmithState a)
rowCombineState targetIndex sourceIndex coefficient stateValue = do
  sourceMatrixRow <-
    rowStoreRowAt
      (InvariantViolation ("Smith row-combine source matrix row missing at index " <> show sourceIndex))
      sourceIndex
      (smithStateMatrix stateValue)
  targetMatrixRow <-
    rowStoreRowAt
      (InvariantViolation ("Smith row-combine target matrix row missing at index " <> show targetIndex))
      targetIndex
      (smithStateMatrix stateValue)
  updatedMatrixRow <- rowCombine targetMatrixRow sourceMatrixRow coefficient
  updatedMatrixRows <-
    replaceRowStore
      (InvariantViolation ("Smith row-combine matrix replacement failed at index " <> show targetIndex))
      targetIndex
      updatedMatrixRow
      (smithStateMatrix stateValue)
  updatedWitness <- traverse updateWitness (smithStateWitness stateValue)
  Right
    stateValue
      { smithStateMatrix = updatedMatrixRows,
        smithStateWitness = updatedWitness
      }
  where
    updateWitness witnessValue = do
      sourceLeftRow <-
        rowStoreRowAt
          (InvariantViolation ("Smith row-combine source witness row missing at index " <> show sourceIndex))
          sourceIndex
          (smithWitnessLeft witnessValue)
      targetLeftRow <-
        rowStoreRowAt
          (InvariantViolation ("Smith row-combine target witness row missing at index " <> show targetIndex))
          targetIndex
          (smithWitnessLeft witnessValue)
      updatedLeftRow <- rowCombine targetLeftRow sourceLeftRow coefficient
      updatedLeftRows <-
        replaceRowStore
          (InvariantViolation ("Smith row-combine witness replacement failed at index " <> show targetIndex))
          targetIndex
          updatedLeftRow
          (smithWitnessLeft witnessValue)
      sourceColumn <- rowIndexAsColumn (fst (rowStoreShape (smithWitnessLeftInverse witnessValue))) sourceIndex
      targetColumn <- rowIndexAsColumn (fst (rowStoreShape (smithWitnessLeftInverse witnessValue))) targetIndex
      updatedLeftInverseRows <-
        columnAddScaledInRows
          sourceColumn
          targetColumn
          coefficient
          (smithWitnessLeftInverse witnessValue)
      Right
        witnessValue
          { smithWitnessLeft = updatedLeftRows,
            smithWitnessLeftInverse = updatedLeftInverseRows
          }

colCombineState ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  ColumnIndex ->
  ColumnIndex ->
  a ->
  SmithState a ->
  Either MoonlightError (SmithState a)
colCombineState targetIndex sourceIndex coefficient stateValue = do
  matrixTarget <- columnStore (InvariantViolation ("Smith column lookup failed at column " <> show targetIndex)) targetIndex (smithStateMatrix stateValue)
  matrixSource <- columnStore (InvariantViolation ("Smith column lookup failed at column " <> show sourceIndex)) sourceIndex (smithStateMatrix stateValue)
  updatedMatrixColumn <- rowCombine matrixTarget matrixSource coefficient
  updatedMatrix <-
    replaceColumnStore
      (InvariantViolation ("Smith column replacement failed at column " <> show targetIndex))
      targetIndex
      updatedMatrixColumn
      (smithStateMatrix stateValue)
  updatedWitness <- traverse updateWitness (smithStateWitness stateValue)
  Right
    stateValue
      { smithStateMatrix = updatedMatrix,
        smithStateWitness = updatedWitness
      }
  where
    updateWitness witnessValue = do
      rightTarget <- columnStore (InvariantViolation ("Smith right witness column lookup failed at column " <> show targetIndex)) targetIndex (smithWitnessRight witnessValue)
      rightSource <- columnStore (InvariantViolation ("Smith right witness column lookup failed at column " <> show sourceIndex)) sourceIndex (smithWitnessRight witnessValue)
      updatedRightColumn <- rowCombine rightTarget rightSource coefficient
      updatedRight <-
        replaceColumnStore
          (InvariantViolation ("Smith right witness column replacement failed at column " <> show targetIndex))
          targetIndex
          updatedRightColumn
          (smithWitnessRight witnessValue)
      sourceRow <- columnIndexAsRow (fst (rowStoreShape (smithWitnessRightInverse witnessValue))) sourceIndex
      targetRow <- columnIndexAsRow (fst (rowStoreShape (smithWitnessRightInverse witnessValue))) targetIndex
      updatedRightInverseRows <-
        rowAddScaledInRows
          sourceRow
          targetRow
          coefficient
          (smithWitnessRightInverse witnessValue)
      Right
        witnessValue
          { smithWitnessRight = updatedRight,
            smithWitnessRightInverse = updatedRightInverseRows
          }

exactQuotient :: EuclideanDomain a => String -> a -> a -> Either MoonlightError a
exactQuotient context numerator denominator = do
  (quotientValue, remainderValue) <- divideWithRemainderChecked context numerator denominator
  if isZero remainderValue
    then Right quotientValue
    else Left (InvariantViolation ("Smith exact quotient had nonzero remainder during " <> context))

divideWithRemainderChecked :: EuclideanDomain a => String -> a -> a -> Either MoonlightError (a, a)
divideWithRemainderChecked context numerator denominator =
  case mkNonZeroDivisor denominator of
    Nothing -> Left (InvariantViolation ("Smith division received a zero divisor during " <> context))
    Just divisor -> Right (divideWithRemainder numerator divisor)

dividesNonZero :: EuclideanDomain a => a -> a -> Bool
dividesNonZero denominator numerator =
  case mkNonZeroDivisor denominator of
    Nothing -> False
    Just divisor -> isZero (snd (divideWithRemainder numerator divisor))

gcdCombineRowsState ::
  EuclideanDomain a =>
  RowIndex ->
  RowIndex ->
  ColumnIndex ->
  SmithState a ->
  Either MoonlightError (SmithState a)
gcdCombineRowsState pivotRow candidateRow pivotColumn stateValue = do
  pivotValue <- matrixValueAt pivotRow pivotColumn (smithStateMatrix stateValue)
  entryValue <- matrixValueAt candidateRow pivotColumn (smithStateMatrix stateValue)
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  pivotQuotient <- exactQuotient "row gcd pivot quotient" pivotValue gcdValue
  entryQuotient <- exactQuotient "row gcd entry quotient" entryValue gcdValue
  updatedMatrix <-
    rowPairTransform
      (InvariantViolation ("Smith row gcd transform failed at " <> show (pivotRow, candidateRow)))
      pivotRow
      candidateRow
      pivotCoefficient
      entryCoefficient
      (neg entryQuotient)
      pivotQuotient
      (smithStateMatrix stateValue)
  updatedWitness <- traverse (updateWitness pivotCoefficient entryCoefficient pivotQuotient entryQuotient) (smithStateWitness stateValue)
  Right
    stateValue
      { smithStateMatrix = updatedMatrix,
        smithStateWitness = updatedWitness
      }
  where
    updateWitness pivotCoefficient entryCoefficient pivotQuotient entryQuotient witnessValue = do
      updatedLeft <-
        rowPairTransform
          (InvariantViolation ("Smith left witness row gcd transform failed at " <> show (pivotRow, candidateRow)))
          pivotRow
          candidateRow
          pivotCoefficient
          entryCoefficient
          (neg entryQuotient)
          pivotQuotient
          (smithWitnessLeft witnessValue)
      pivotColumnWitness <- rowIndexAsColumn (fst (rowStoreShape (smithWitnessLeftInverse witnessValue))) pivotRow
      candidateColumnWitness <- rowIndexAsColumn (fst (rowStoreShape (smithWitnessLeftInverse witnessValue))) candidateRow
      updatedLeftInverse <-
        columnPairTransform
          (InvariantViolation ("Smith left inverse row gcd transform failed at " <> show (pivotColumnWitness, candidateColumnWitness)))
          pivotColumnWitness
          candidateColumnWitness
          pivotQuotient
          entryQuotient
          (neg entryCoefficient)
          pivotCoefficient
          (smithWitnessLeftInverse witnessValue)
      Right
        witnessValue
          { smithWitnessLeft = updatedLeft,
            smithWitnessLeftInverse = updatedLeftInverse
          }

gcdCombineColsState ::
  EuclideanDomain a =>
  RowIndex ->
  ColumnIndex ->
  ColumnIndex ->
  SmithState a ->
  Either MoonlightError (SmithState a)
gcdCombineColsState pivotRow pivotColumn candidateColumn stateValue = do
  pivotValue <- matrixValueAt pivotRow pivotColumn (smithStateMatrix stateValue)
  entryValue <- matrixValueAt pivotRow candidateColumn (smithStateMatrix stateValue)
  let (gcdValue, pivotCoefficient, entryCoefficient) = extendedGcdDomain pivotValue entryValue
  pivotQuotient <- exactQuotient "column gcd pivot quotient" pivotValue gcdValue
  entryQuotient <- exactQuotient "column gcd entry quotient" entryValue gcdValue
  updatedMatrix <-
    columnPairTransform
      (InvariantViolation ("Smith column gcd transform failed at " <> show (pivotColumn, candidateColumn)))
      pivotColumn
      candidateColumn
      pivotCoefficient
      entryCoefficient
      (neg entryQuotient)
      pivotQuotient
      (smithStateMatrix stateValue)
  updatedWitness <- traverse (updateWitness pivotCoefficient entryCoefficient pivotQuotient entryQuotient) (smithStateWitness stateValue)
  Right
    stateValue
      { smithStateMatrix = updatedMatrix,
        smithStateWitness = updatedWitness
      }
  where
    updateWitness pivotCoefficient entryCoefficient pivotQuotient entryQuotient witnessValue = do
      updatedRight <-
        columnPairTransform
          (InvariantViolation ("Smith right witness column gcd transform failed at " <> show (pivotColumn, candidateColumn)))
          pivotColumn
          candidateColumn
          pivotCoefficient
          entryCoefficient
          (neg entryQuotient)
          pivotQuotient
          (smithWitnessRight witnessValue)
      pivotRowWitness <- columnIndexAsRow (fst (rowStoreShape (smithWitnessRightInverse witnessValue))) pivotColumn
      candidateRowWitness <- columnIndexAsRow (fst (rowStoreShape (smithWitnessRightInverse witnessValue))) candidateColumn
      updatedRightInverse <-
        rowPairTransform
          (InvariantViolation ("Smith right inverse column gcd transform failed at " <> show (pivotRowWitness, candidateRowWitness)))
          pivotRowWitness
          candidateRowWitness
          pivotQuotient
          entryQuotient
          (neg entryCoefficient)
          pivotCoefficient
          (smithWitnessRightInverse witnessValue)
      Right
        witnessValue
          { smithWitnessRight = updatedRight,
            smithWitnessRightInverse = updatedRightInverse
          }

findPivot :: EuclideanDomain a => RowIndex -> ColumnIndex -> RowStore a -> Either MoonlightError (Maybe (RowIndex, ColumnIndex, a))
findPivot startRow startCol rows =
  let (rowCount, columnCount) = rowStoreShape rows
   in traverse
        ( \rowIndex ->
            traverse
              ( \columnIndex ->
                  fmap
                    (\value -> if isZero value then Nothing else Just (rowIndex, columnIndex, value))
                    (matrixValueAt rowIndex columnIndex rows)
              )
              (dropWhile (< startCol) (columnIndices columnCount))
        )
        (dropWhile (< startRow) (rowIndices rowCount))
        >>= \candidateRows ->
          let candidateTriples = mapMaybe id (concat candidateRows)
           in Right
                ( if null candidateTriples
                    then Nothing
                    else Just (minimumBy (comparing (degree . (\(_, _, value) -> value))) candidateTriples)
                )

columnCleared :: IntegralDomain a => RowIndex -> ColumnIndex -> RowStore a -> Either MoonlightError Bool
columnCleared pivotRow pivotColumn rows =
  fmap
    and
    ( traverse
        ( \rowIndex ->
            if rowIndex == pivotRow
              then Right True
              else fmap isZero (matrixValueAt rowIndex pivotColumn rows)
        )
        (rowIndices (fst (rowStoreShape rows)))
    )

rowCleared :: IntegralDomain a => RowIndex -> ColumnIndex -> RowStore a -> Either MoonlightError Bool
rowCleared pivotRow pivotColumn rows =
  rowStoreRowAt
    (InvariantViolation ("Smith row-clear pivot row missing at index " <> show pivotRow))
    pivotRow
    rows
    >>= \pivotRowValues ->
      Right
        ( all
            (\(columnIndex, value) -> columnIndex == columnIndexInt pivotColumn || isZero value)
            (zip [0 :: Int ..] (Box.toList pivotRowValues))
        )

clearColumn ::
  forall a.
  EuclideanDomain a =>
  RowIndex ->
  ColumnIndex ->
  SmithState a ->
  Either MoonlightError (SmithState a)
clearColumn pivotRow pivotColumn stateValue =
  let rows = smithStateMatrix stateValue
      (rowCount, _) = rowStoreShape rows
   in traverse
        ( \rowIndex ->
            fmap
              (\entryValue -> if rowIndex /= pivotRow && not (isZero entryValue) then Just rowIndex else Nothing)
              (matrixValueAt rowIndex pivotColumn rows)
        )
        (rowIndices rowCount)
        >>= \candidateMarks ->
          case mapMaybe id candidateMarks of
            [] -> Right stateValue
            candidateRow : _ -> do
              pivotValue <- matrixValueAt pivotRow pivotColumn rows
              entryValue <- matrixValueAt candidateRow pivotColumn rows
              if isZero pivotValue
                then Left (InvariantViolation "Smith normal form pivot became zero during column reduction")
                else do
                  (quotientValue, remainderValue) <- divideWithRemainderChecked "column reduction" entryValue pivotValue
                  reducedState <-
                    if isZero remainderValue
                      then rowCombineState candidateRow pivotRow quotientValue stateValue
                      else gcdCombineRowsState pivotRow candidateRow pivotColumn stateValue
                  clearColumn pivotRow pivotColumn reducedState

clearRow ::
  forall a.
  EuclideanDomain a =>
  RowIndex ->
  ColumnIndex ->
  SmithState a ->
  Either MoonlightError (SmithState a)
clearRow pivotRow pivotColumn stateValue =
  let rows = smithStateMatrix stateValue
   in rowStoreRowAt
        (InvariantViolation ("Smith row reduction pivot row missing at index " <> show pivotRow))
        pivotRow
        rows
        >>= \pivotRowValues ->
          case map fst (filter (\(columnIndex, value) -> columnIndex /= pivotColumn && not (isZero value)) (zip (columnIndices (Box.length pivotRowValues)) (Box.toList pivotRowValues))) of
            [] -> Right stateValue
            candidateCol : _ -> do
              pivotValue <- matrixValueAt pivotRow pivotColumn rows
              entryValue <- matrixValueAt pivotRow candidateCol rows
              if isZero pivotValue
                then Left (InvariantViolation "Smith normal form pivot became zero during row reduction")
                else do
                  (quotientValue, remainderValue) <- divideWithRemainderChecked "row reduction" entryValue pivotValue
                  reducedState <-
                    if isZero remainderValue
                      then colCombineState candidateCol pivotColumn quotientValue stateValue
                      else gcdCombineColsState pivotRow pivotColumn candidateCol stateValue
                  clearRow pivotRow pivotColumn reducedState

normalizePivot ::
  forall a.
  EuclideanDomain a =>
  RowIndex ->
  ColumnIndex ->
  Int ->
  SmithState a ->
  Either MoonlightError (SmithState a)
normalizePivot pivotRow pivotColumn remainingBudget stateValue
  | remainingBudget <= 0 = Left (InvariantViolation "Smith normal form normalization exhausted iteration budget")
  | otherwise = do
      clearedColumn <- columnCleared pivotRow pivotColumn (smithStateMatrix stateValue)
      clearedRow <- rowCleared pivotRow pivotColumn (smithStateMatrix stateValue)
      if clearedColumn && clearedRow
        then Right stateValue
        else do
          columnReduced <- clearColumn pivotRow pivotColumn stateValue
          rowReduced <- clearRow pivotRow pivotColumn columnReduced
          if smithStateMatrix rowReduced == smithStateMatrix stateValue
            then Left (InvariantViolation "Smith normal form normalization stalled before reaching diagonal form")
            else normalizePivot pivotRow pivotColumn (remainingBudget - 1) rowReduced

smithStep ::
  forall a.
  EuclideanDomain a =>
  Int ->
  Int ->
  Int ->
  Int ->
  SmithState a ->
  Either MoonlightError (SmithState a)
smithStep pivotIndex rowCount columnCount normalizationBudget stateValue
  | pivotIndex >= min rowCount columnCount = Right stateValue
  | otherwise = do
      pivotRowIndex <-
        mkRowIndex
          (InvariantViolation ("Smith normal form pivot row out of bounds at index " <> show pivotIndex))
          rowCount
          pivotIndex
      pivotColumnIndex <-
        mkColumnIndex
          (InvariantViolation ("Smith normal form pivot column out of bounds at index " <> show pivotIndex))
          columnCount
          pivotIndex
      pivotCandidate <- findPivot pivotRowIndex pivotColumnIndex (smithStateMatrix stateValue)
      case pivotCandidate of
        Nothing -> Right stateValue
        Just (pivotRow, pivotCol, _) -> do
          pivotMoved <- swapRowsState pivotRowIndex pivotRow stateValue >>= swapColsState pivotColumnIndex pivotCol
          normalized <- normalizePivot pivotRowIndex pivotColumnIndex normalizationBudget pivotMoved
          smithStep (pivotIndex + 1) rowCount columnCount normalizationBudget normalized

enforceDivisibilityChain ::
  forall a.
  EuclideanDomain a =>
  Int ->
  Int ->
  Int ->
  Int ->
  SmithState a ->
  Either MoonlightError (SmithState a)
enforceDivisibilityChain rowCount columnCount normalizationBudget divisibilityBudget stateValue =
  go divisibilityBudget stateValue
  where
    diagSize = min rowCount columnCount

    go remainingBudget currentState
      | remainingBudget <= 0 =
          case findViolation 0 (smithStateMatrix currentState) of
            Nothing -> Right currentState
            Just _ -> Left (InvariantViolation "Smith normal form divisibility chain exhausted iteration budget")
      | otherwise =
          case findViolation 0 (smithStateMatrix currentState) of
            Nothing -> Right currentState
            Just violationIndex -> do
              rowI <-
                mkRowIndex
                  (InvariantViolation ("divisibility chain row index out of bounds at " <> show violationIndex))
                  rowCount
                  violationIndex
              rowJ <-
                mkRowIndex
                  (InvariantViolation ("divisibility chain row index out of bounds at " <> show (violationIndex + 1)))
                  rowCount
                  (violationIndex + 1)
              colI <-
                mkColumnIndex
                  (InvariantViolation ("divisibility chain column index out of bounds at " <> show violationIndex))
                  columnCount
                  violationIndex
              colJ <-
                mkColumnIndex
                  (InvariantViolation ("divisibility chain column index out of bounds at " <> show (violationIndex + 1)))
                  columnCount
                  (violationIndex + 1)
              combined <- rowCombineState rowI rowJ (neg one) currentState
              normalizedI <- normalizePivot rowI colI normalizationBudget combined
              normalizedJ <- normalizePivot rowJ colJ normalizationBudget normalizedI
              go (remainingBudget - 1) normalizedJ

    findViolation idx matrixRows
      | idx + 1 >= diagSize = Nothing
      | otherwise =
          case diagonalPair idx matrixRows of
            Left _ -> Nothing
            Right (dI, dJ)
              | isZero dI -> findViolation (idx + 1) matrixRows
              | isZero dJ -> findViolation (idx + 1) matrixRows
              | dividesNonZero dI dJ -> findViolation (idx + 1) matrixRows
              | otherwise -> Just idx

    diagonalPair idx matrixRows = do
      rowI <- mkRowIndex (InvariantViolation "divisibility diagonal lookup") rowCount idx
      colI <- mkColumnIndex (InvariantViolation "divisibility diagonal lookup") columnCount idx
      rowJ <- mkRowIndex (InvariantViolation "divisibility diagonal lookup") rowCount (idx + 1)
      colJ <- mkColumnIndex (InvariantViolation "divisibility diagonal lookup") columnCount (idx + 1)
      dI <- matrixValueAt rowI colI matrixRows
      dJ <- matrixValueAt rowJ colJ matrixRows
      Right (dI, dJ)

smithStateFromRows ::
  [[a]] ->
  Maybe (SmithWitnessState a) ->
  SmithState a
smithStateFromRows rows witnessValue =
  SmithState
    { smithStateMatrix = rowStoreFromRows rows,
      smithStateWitness = witnessValue
    }

fullWitnessState ::
  (AdditiveGroup a, MultiplicativeMonoid a) =>
  Int ->
  Int ->
  SmithWitnessState a
fullWitnessState rowCount columnCount =
  SmithWitnessState
    { smithWitnessLeft = rowStoreFromRows (identityRows rowCount),
      smithWitnessRight = rowStoreFromRows (identityRows columnCount),
      smithWitnessLeftInverse = rowStoreFromRows (identityRows rowCount),
      smithWitnessRightInverse = rowStoreFromRows (identityRows columnCount)
    }

runSmithState ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Maybe (SmithWitnessState a) ->
  Matrix r c a ->
  Either MoonlightError (SmithState a)
runSmithState witnessValue matrixValue = do
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
      diagonalSize = min rowCount columnCount
  matrixCardinality <- checkedSmithProduct "matrix normalization budget" rowCount columnCount
  normalizationCardinality <- checkedSmithProduct "matrix normalization budget" matrixCardinality 2
  divisibilityCardinality <- checkedSmithProduct "divisibility-chain budget" diagonalSize diagonalSize
  let normalizationBudget = max 1 normalizationCardinality
  rows <- DenseTypes.matrixToRows matrixValue
  let initialState = smithStateFromRows rows witnessValue
  steppedState <- smithStep 0 rowCount columnCount normalizationBudget initialState
  repairedState <- enforceDivisibilityChain rowCount columnCount normalizationBudget divisibilityCardinality steppedState
  normalizeDiagonalUnits rowCount columnCount repairedState

checkedSmithProduct :: String -> Int -> Int -> Either MoonlightError Int
checkedSmithProduct context leftFactor rightFactor =
  first
    (const (InvariantViolation ("Smith " <> context <> " exceeds Int range")))
    (checkedNonNegativeProduct leftFactor rightFactor)

normalizeDiagonalUnits ::
  forall a.
  EuclideanDomain a =>
  Int ->
  Int ->
  SmithState a ->
  Either MoonlightError (SmithState a)
normalizeDiagonalUnits rowCount columnCount stateValue =
  foldM normalizeAt stateValue [0 .. min rowCount columnCount - 1]
  where
    normalizeAt currentState indexValue = do
      rowIndex <- mkRowIndex (InvariantViolation ("Smith unit normalization row index failed at " <> show indexValue)) rowCount indexValue
      columnIndex <- mkColumnIndex (InvariantViolation ("Smith unit normalization column index failed at " <> show indexValue)) columnCount indexValue
      entryValue <- matrixValueAt rowIndex columnIndex (smithStateMatrix currentState)
      let canonicalValue = gcdDomain entryValue zero
      if canonicalValue == entryValue
        then Right currentState
        else do
          (unitValue, remainderValue) <- divideWithRemainderChecked "unit normalization" entryValue canonicalValue
          if remainderValue == zero
            then do
              inverseUnit <-
                case unitInverse unitValue of
                  Just value -> Right value
                  Nothing -> Left (InvariantViolation ("Smith unit normalization met a nonunit cofactor at " <> show indexValue))
              scaledMatrix <- scaleRowStore rowIndex inverseUnit (smithStateMatrix currentState)
              scaledWitness <- traverse (scaleWitness rowIndex unitValue inverseUnit) (smithStateWitness currentState)
              Right
                currentState
                  { smithStateMatrix = scaledMatrix,
                    smithStateWitness = scaledWitness
                  }
            else Left (InvariantViolation ("Smith unit normalization division was inexact at " <> show indexValue))

    scaleWitness :: RowIndex -> a -> a -> SmithWitnessState a -> Either MoonlightError (SmithWitnessState a)
    scaleWitness rowIndex unitValue inverseUnit witnessValue = do
      scaledLeft <- scaleRowStore rowIndex inverseUnit (smithWitnessLeft witnessValue)
      witnessColumn <- rowIndexAsColumn (fst (rowStoreShape (smithWitnessLeftInverse witnessValue))) rowIndex
      scaledLeftInverse <- scaleColumnStore witnessColumn unitValue (smithWitnessLeftInverse witnessValue)
      Right
        witnessValue
          { smithWitnessLeft = scaledLeft,
            smithWitnessLeftInverse = scaledLeftInverse
          }

scaleRowStore ::
  MultiplicativeMonoid a =>
  RowIndex ->
  a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
scaleRowStore rowIndex factor store = do
  let failure = InvariantViolation ("Smith unit normalization row scale failed at " <> show rowIndex)
  rowValue <- rowStoreRowAt failure rowIndex store
  replaceRowStore failure rowIndex (Box.map (mul factor) rowValue) store

scaleColumnStore ::
  MultiplicativeMonoid a =>
  ColumnIndex ->
  a ->
  RowStore a ->
  Either MoonlightError (RowStore a)
scaleColumnStore columnIndex factor store = do
  let failure = InvariantViolation ("Smith unit normalization column scale failed at " <> show columnIndex)
  columnValue <- columnStore failure columnIndex store
  replaceColumnStore failure columnIndex (Box.map (mul factor) columnValue) store

smithNormalFormPure ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (SmithNormalForm r c a)
smithNormalFormPure matrixValue = do
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
  finalState <- runSmithState (Just (fullWitnessState rowCount columnCount)) matrixValue
  witnessValue <-
    case smithStateWitness finalState of
      Just value -> Right value
      Nothing -> Left (InvariantViolation "Smith full decomposition lost witness state")
  leftMatrix <- fromListMatrix @r @r (rowStoreFlatten (smithWitnessLeft witnessValue))
  diagonalMatrix <- fromListMatrix @r @c (rowStoreFlatten (smithStateMatrix finalState))
  rightMatrix <- fromListMatrix @c @c (rowStoreFlatten (smithWitnessRight witnessValue))
  leftInverseMatrix <- fromListMatrix @r @r (rowStoreFlatten (smithWitnessLeftInverse witnessValue))
  rightInverseMatrix <- fromListMatrix @c @c (rowStoreFlatten (smithWitnessRightInverse witnessValue))
  pure
    SmithNormalForm
      { smithLeft = leftMatrix,
        smithDiagonal = diagonalMatrix,
        smithRight = rightMatrix,
        smithLeftInverse = leftInverseMatrix,
        smithRightInverse = rightInverseMatrix
      }

smithDiagonalFormPure ::
  forall r c a.
  (KnownNat r, KnownNat c, EuclideanDomain a) =>
  Matrix r c a ->
  Either MoonlightError (SmithDiagonalForm r c a)
smithDiagonalFormPure matrixValue = do
  finalState <- runSmithState Nothing matrixValue
  diagonalMatrix <- fromListMatrix @r @c (rowStoreFlatten (smithStateMatrix finalState))
  pure (SmithDiagonalForm diagonalMatrix)

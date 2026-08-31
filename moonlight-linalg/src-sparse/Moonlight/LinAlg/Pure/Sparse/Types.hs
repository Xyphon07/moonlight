{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.LinAlg.Pure.Sparse.Types
  ( SparseCOO,
    mkSparseCOO,
    cooRows,
    cooCols,
    cooEntries,
    SparseCSR,
    mkSparseCSR,
    csrRows,
    csrCols,
    csrRowOffsetsVector,
    csrColumnIndicesVector,
    csrValuesVector,
    CSRExecutionPlan (..),
    csrExecutionPlan,
    SparseCSC,
    mkSparseCSC,
    cscRows,
    cscCols,
    cscColumnOffsetsVector,
    cscRowIndicesVector,
    cscValuesVector,
    denseToCOO,
    denseToCSR,
    denseToCSC,
    cooToCSR,
    cooToCSC,
    canonicalCSRFromValidEntriesUnchecked,
    csrFromCanonicalVectorsUnchecked,
    csrFromCanonicalVectorsWithPlanUnchecked,
    csrToCOO,
    cscToCOO,
    cooToDense,
    csrToDense,
    cscToDense,
    csrToCSC,
    cscToCSR,
    csrMatVecVector,
    validateCOO,
    validateCOOEntries,
    validateCSR,
    validateCSC,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Proxy (Proxy (..))
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import GHC.TypeNats (KnownNat, natVal)
import Moonlight.Core
  ( AdditiveGroup (..),
    AdditiveMonoid (..),
    MoonlightError (..),
    checkedNaturalToInt,
    checkedNonNegativeProduct,
  )
import Moonlight.LinAlg.Internal.VectorOps
  ( csrContiguousBandMatVecValidatedU,
    csrMatVecValidatedU,
  )
import Moonlight.LinAlg.Pure.Dense.Types (Matrix, fromListMatrix, toListMatrix)
import qualified Moonlight.LinAlg.Pure.Dense.Types as DenseTypes
import Prelude

type SparseCOO :: Type -> Type
data SparseCOO a = SparseCOO
  { cooRows :: Int,
    cooCols :: Int,
    cooEntries :: [(Int, Int, a)]
  }
  deriving stock (Eq, Show)

type CSRExecutionPlan :: Type
data CSRExecutionPlan
  = CSRGeneral
  | CSRContiguousBand !Int !Int
  deriving stock (Eq, Show)

type SparseCSR :: Type -> Type
data SparseCSR a = SparseCSR
  { csrRows :: Int,
    csrCols :: Int,
    csrRowOffsetsVector :: U.Vector Int,
    csrColumnIndicesVector :: U.Vector Int,
    csrValuesVector :: U.Vector a,
    csrExecutionPlan :: !CSRExecutionPlan
  }

instance (Eq a, U.Unbox a) => Eq (SparseCSR a) where
  left == right =
    csrRows left == csrRows right
      && csrCols left == csrCols right
      && csrRowOffsetsVector left == csrRowOffsetsVector right
      && csrColumnIndicesVector left == csrColumnIndicesVector right
      && csrValuesVector left == csrValuesVector right

instance (Show a, U.Unbox a) => Show (SparseCSR a) where
  showsPrec precedence csrValue =
    showParen (precedence > 10) $
      showString "SparseCSR {csrRows = "
        . shows (csrRows csrValue)
        . showString ", csrCols = "
        . shows (csrCols csrValue)
        . showString ", csrRowOffsets = "
        . shows (U.toList (csrRowOffsetsVector csrValue))
        . showString ", csrColumnIndices = "
        . shows (U.toList (csrColumnIndicesVector csrValue))
        . showString ", csrValues = "
        . shows (U.toList (csrValuesVector csrValue))
        . showString "}"

type SparseCSC :: Type -> Type
data SparseCSC a = SparseCSC
  { cscRows :: Int,
    cscCols :: Int,
    cscColumnOffsetsVector :: U.Vector Int,
    cscRowIndicesVector :: U.Vector Int,
    cscValuesVector :: U.Vector a
  }

instance (Eq a, U.Unbox a) => Eq (SparseCSC a) where
  left == right =
    cscRows left == cscRows right
      && cscCols left == cscCols right
      && cscColumnOffsetsVector left == cscColumnOffsetsVector right
      && cscRowIndicesVector left == cscRowIndicesVector right
      && cscValuesVector left == cscValuesVector right

instance (Show a, U.Unbox a) => Show (SparseCSC a) where
  showsPrec precedence cscValue =
    showParen (precedence > 10) $
      showString "SparseCSC {cscRows = "
        . shows (cscRows cscValue)
        . showString ", cscCols = "
        . shows (cscCols cscValue)
        . showString ", cscColumnOffsets = "
        . shows (U.toList (cscColumnOffsetsVector cscValue))
        . showString ", cscRowIndices = "
        . shows (U.toList (cscRowIndicesVector cscValue))
        . showString ", cscValues = "
        . shows (U.toList (cscValuesVector cscValue))
        . showString "}"

mkSparseCSR :: (Eq a, AdditiveMonoid a, U.Unbox a) => Int -> Int -> [Int] -> [Int] -> [a] -> Either MoonlightError (SparseCSR a)
mkSparseCSR rows cols offsets colIndices values = do
  let !offsetVector = U.fromList offsets
      !columnVector = U.fromList colIndices
      !valueVector = U.fromList values
      !unplanned =
        SparseCSR
          { csrRows = rows,
            csrCols = cols,
            csrRowOffsetsVector = offsetVector,
            csrColumnIndicesVector = columnVector,
            csrValuesVector = valueVector,
            csrExecutionPlan = CSRGeneral
          }
  validateCSR unplanned
  pure
    unplanned
      { csrExecutionPlan =
          detectCSRExecutionPlan
            rows
            cols
            offsetVector
            columnVector
      }

mkSparseCOO :: Int -> Int -> [(Int, Int, a)] -> Either MoonlightError (SparseCOO a)
mkSparseCOO rows cols entries = do
  validateCOOEntries rows cols entries
  Right (SparseCOO rows cols entries)

mkSparseCSC :: (Eq a, AdditiveMonoid a, U.Unbox a) => Int -> Int -> [Int] -> [Int] -> [a] -> Either MoonlightError (SparseCSC a)
mkSparseCSC rows cols offsets rowIndices values = do
  let csc =
        SparseCSC
          rows
          cols
          (U.fromList offsets)
          (U.fromList rowIndices)
          (U.fromList values)
  validateCSC csc
  Right csc

denseToCOO ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, AdditiveGroup a) =>
  Matrix r c a ->
  SparseCOO a
denseToCOO matrixValue =
  let (rowCount, columnCount) = DenseTypes.matrixShape matrixValue
      indexedValues = zip [0 ..] (toListMatrix matrixValue)
      toEntry (flatIndex, value) =
        if value == zero
          then Nothing
          else
            let rowIndex = flatIndex `div` columnCount
                columnIndex = flatIndex `mod` columnCount
             in Just (rowIndex, columnIndex, value)
   in SparseCOO rowCount columnCount (mapMaybe toEntry indexedValues)

denseToCSR ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, AdditiveGroup a, U.Unbox a) =>
  Matrix r c a ->
  SparseCSR a
denseToCSR = cooToCSRSortedUniqueUnchecked . denseToCOO

denseToCSC ::
  forall r c a.
  (KnownNat r, KnownNat c, Eq a, AdditiveGroup a, U.Unbox a) =>
  Matrix r c a ->
  SparseCSC a
denseToCSC = cooToCSCSortedUniqueUnchecked . denseToCOO

cooToCSR :: (Eq a, AdditiveGroup a, U.Unbox a) => SparseCOO a -> Either MoonlightError (SparseCSR a)
cooToCSR cooValue =
  validateCOO cooValue *> pure (cooToCSRUnchecked cooValue)

cooToCSRUnchecked :: (Eq a, AdditiveGroup a, U.Unbox a) => SparseCOO a -> SparseCSR a
cooToCSRUnchecked cooValue =
  canonicalCSRFromValidEntriesUnchecked
    (cooRows cooValue)
    (cooCols cooValue)
    (cooEntries cooValue)
{-# INLINE cooToCSRUnchecked #-}

cooToCSRSortedUniqueUnchecked :: U.Unbox a => SparseCOO a -> SparseCSR a
cooToCSRSortedUniqueUnchecked cooValue =
  csrFromSortedEntries (cooRows cooValue) (cooCols cooValue) (cooEntries cooValue)

csrFromSortedEntries :: U.Unbox a => Int -> Int -> [(Int, Int, a)] -> SparseCSR a
csrFromSortedEntries rowCount columnCount orderedEntries =
  let !offsetVector =
        U.fromList
          ( offsetsFromSortedAxes
              rowCount
              ((\(rowIndex, _, _) -> rowIndex) <$> orderedEntries)
          )
      !columnVector =
        U.fromList
          ((\(_, columnIndex, _) -> columnIndex) <$> orderedEntries)
      !valueVector =
        U.fromList
          ((\(_, _, value) -> value) <$> orderedEntries)
   in csrFromCanonicalVectorsUnchecked rowCount columnCount offsetVector columnVector valueVector

csrFromCanonicalVectorsUnchecked ::
  Int ->
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector a ->
  SparseCSR a
csrFromCanonicalVectorsUnchecked rowCount columnCount offsetVector columnVector valueVector =
  csrFromCanonicalVectorsWithPlanUnchecked
    rowCount
    columnCount
    offsetVector
    columnVector
    valueVector
    ( detectCSRExecutionPlan
        rowCount
        columnCount
        offsetVector
        columnVector
    )

csrFromCanonicalVectorsWithPlanUnchecked ::
  Int ->
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector a ->
  CSRExecutionPlan ->
  SparseCSR a
csrFromCanonicalVectorsWithPlanUnchecked rowCount columnCount offsetVector columnVector valueVector executionPlan =
  SparseCSR
    { csrRows = rowCount,
      csrCols = columnCount,
      csrRowOffsetsVector = offsetVector,
      csrColumnIndicesVector = columnVector,
      csrValuesVector = valueVector,
      csrExecutionPlan = executionPlan
    }

canonicalCSRFromValidEntriesUnchecked ::
  (Eq a, AdditiveGroup a, U.Unbox a) =>
  Int ->
  Int ->
  [(Int, Int, a)] ->
  SparseCSR a
canonicalCSRFromValidEntriesUnchecked rowCount columnCount entries =
  let SparseEntryVectors offsetVector columnVector valueVector =
        canonicalCSRVectorsFromValidEntries rowCount columnCount entries
   in csrFromCanonicalVectorsUnchecked rowCount columnCount offsetVector columnVector valueVector
{-# INLINE canonicalCSRFromValidEntriesUnchecked #-}

canonicalCSRVectorsFromValidEntries ::
  (Eq a, AdditiveGroup a, U.Unbox a) =>
  Int ->
  Int ->
  [(Int, Int, a)] ->
  SparseEntryVectors a
canonicalCSRVectorsFromValidEntries rowCount columnCount entries =
  compactCompressedRows columnCount (compressEntriesByRow rowCount entries)

compressEntriesByRow ::
  U.Unbox a =>
  Int ->
  [(Int, Int, a)] ->
  SparseEntryVectors a
compressEntriesByRow rowCount entries =
  runST $ do
    let !entryCount = length entries
    rowCounts <- MU.replicate rowCount 0
    traverse_ (\(rowIndex, _, _) -> incrementMutableInt rowCounts rowIndex) entries
    rowOffsets <- MU.replicate (rowCount + 1) 0
    prefixMutableCountsWithStarts rowCount rowCounts rowOffsets
    columnVector <- MU.unsafeNew entryCount
    valueVector <- MU.unsafeNew entryCount
    traverse_ (scatterCompressedEntry rowCounts columnVector valueVector) entries
    frozenOffsets <- U.unsafeFreeze rowOffsets
    frozenColumns <- U.unsafeFreeze columnVector
    frozenValues <- U.unsafeFreeze valueVector
    pure
      SparseEntryVectors
        { sparseEntryOffsets = frozenOffsets,
          sparseEntryIndices = frozenColumns,
          sparseEntryValues = frozenValues
        }

scatterCompressedEntry ::
  U.Unbox a =>
  MU.MVector s Int ->
  MU.MVector s Int ->
  MU.MVector s a ->
  (Int, Int, a) ->
  ST s ()
scatterCompressedEntry nextOffsets columnVector valueVector (rowIndex, columnIndex, entryValue) = do
  targetIndex <- MU.unsafeRead nextOffsets rowIndex
  MU.unsafeWrite columnVector targetIndex columnIndex
  MU.unsafeWrite valueVector targetIndex entryValue
  MU.unsafeWrite nextOffsets rowIndex (targetIndex + 1)

compactCompressedRows ::
  forall a.
  (Eq a, AdditiveGroup a, U.Unbox a) =>
  Int ->
  SparseEntryVectors a ->
  SparseEntryVectors a
compactCompressedRows columnCount compressedEntries =
  runST $ do
    markerSlots <- MU.replicate columnCount (-1)
    uniqueColumns <- MU.unsafeNew entryCount
    uniqueValues <- MU.unsafeNew entryCount
    compactOffsets <- MU.replicate (rowCount + 1) 0
    compactColumns <- MU.unsafeNew entryCount
    compactValues <- MU.unsafeNew entryCount
    finalCount <-
      compactRows
        markerSlots
        uniqueColumns
        uniqueValues
        compactOffsets
        compactColumns
        compactValues
        0
        0
        0
    frozenOffsets <- U.unsafeFreeze compactOffsets
    frozenColumns <- U.unsafeFreeze compactColumns
    frozenValues <- U.unsafeFreeze compactValues
    pure
      SparseEntryVectors
        { sparseEntryOffsets = frozenOffsets,
          sparseEntryIndices = U.slice 0 finalCount frozenColumns,
          sparseEntryValues = U.slice 0 finalCount frozenValues
        }
  where
    !rowOffsets = sparseEntryOffsets compressedEntries
    !rawColumns = sparseEntryIndices compressedEntries
    !rawValues = sparseEntryValues compressedEntries
    !rowCount = U.length rowOffsets - 1
    !entryCount = U.length rawValues

    compactRows ::
      forall s.
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s a ->
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s a ->
      Int ->
      Int ->
      Int ->
      ST s Int
    compactRows
      markerSlots
      uniqueColumns
      uniqueValues
      compactOffsets
      compactColumns
      compactValues
      !rowIndex
      !uniqueCount
      !compactCount
        | rowIndex >= rowCount = do
            MU.unsafeWrite compactOffsets rowCount compactCount
            pure compactCount
        | otherwise = do
            MU.unsafeWrite compactOffsets rowIndex compactCount
            let !entryStart = rowOffsets `U.unsafeIndex` rowIndex
                !entryStop = rowOffsets `U.unsafeIndex` (rowIndex + 1)
            uniqueStop <-
              combineCompressedRow
                markerSlots
                uniqueColumns
                uniqueValues
                uniqueCount
                uniqueCount
                entryStart
                entryStop
            rowPairs <- collectNonZeroPairs uniqueColumns uniqueValues uniqueCount uniqueStop []
            compactStop <-
              writeSortedPairs
                compactColumns
                compactValues
                compactCount
                (sortBy (comparing fst) rowPairs)
            compactRows
              markerSlots
              uniqueColumns
              uniqueValues
              compactOffsets
              compactColumns
              compactValues
              (rowIndex + 1)
              uniqueStop
              compactStop

    combineCompressedRow ::
      forall s.
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s a ->
      Int ->
      Int ->
      Int ->
      Int ->
      ST s Int
    combineCompressedRow
      markerSlots
      uniqueColumns
      uniqueValues
      !uniqueStart
      !uniqueNext
      !entryIndex
      !entryStop
        | entryIndex >= entryStop = pure uniqueNext
        | entryValue == zero =
            combineCompressedRow
              markerSlots
              uniqueColumns
              uniqueValues
              uniqueStart
              uniqueNext
              (entryIndex + 1)
              entryStop
        | otherwise = do
            markerSlot <- MU.unsafeRead markerSlots columnIndex
            if markerSlot >= uniqueStart
              then do
                currentValue <- MU.unsafeRead uniqueValues markerSlot
                MU.unsafeWrite uniqueValues markerSlot (add entryValue currentValue)
                combineCompressedRow
                  markerSlots
                  uniqueColumns
                  uniqueValues
                  uniqueStart
                  uniqueNext
                  (entryIndex + 1)
                  entryStop
              else do
                MU.unsafeWrite markerSlots columnIndex uniqueNext
                MU.unsafeWrite uniqueColumns uniqueNext columnIndex
                MU.unsafeWrite uniqueValues uniqueNext entryValue
                combineCompressedRow
                  markerSlots
                  uniqueColumns
                  uniqueValues
                  uniqueStart
                  (uniqueNext + 1)
                  (entryIndex + 1)
                  entryStop
      where
        !columnIndex = rawColumns `U.unsafeIndex` entryIndex
        !entryValue = rawValues `U.unsafeIndex` entryIndex

    collectNonZeroPairs ::
      forall s.
      MU.MVector s Int ->
      MU.MVector s a ->
      Int ->
      Int ->
      [(Int, a)] ->
      ST s [(Int, a)]
    collectNonZeroPairs uniqueColumns uniqueValues !entryIndex !entryStop !rowPairs
      | entryIndex >= entryStop = pure rowPairs
      | otherwise = do
          entryValue <- MU.unsafeRead uniqueValues entryIndex
          if entryValue == zero
            then collectNonZeroPairs uniqueColumns uniqueValues (entryIndex + 1) entryStop rowPairs
            else do
              columnIndex <- MU.unsafeRead uniqueColumns entryIndex
              collectNonZeroPairs uniqueColumns uniqueValues (entryIndex + 1) entryStop ((columnIndex, entryValue) : rowPairs)

    writeSortedPairs ::
      forall s.
      MU.MVector s Int ->
      MU.MVector s a ->
      Int ->
      [(Int, a)] ->
      ST s Int
    writeSortedPairs _ _ !entryIndex [] =
      pure entryIndex
    writeSortedPairs compactColumns compactValues !entryIndex ((columnIndex, entryValue) : rowPairs) = do
      MU.unsafeWrite compactColumns entryIndex columnIndex
      MU.unsafeWrite compactValues entryIndex entryValue
      writeSortedPairs compactColumns compactValues (entryIndex + 1) rowPairs

cooToCSC :: (Eq a, AdditiveGroup a, U.Unbox a) => SparseCOO a -> Either MoonlightError (SparseCSC a)
cooToCSC cooValue =
  validateCOO cooValue *> pure (cooToCSCUnchecked cooValue)

cooToCSCUnchecked :: (Eq a, AdditiveGroup a, U.Unbox a) => SparseCOO a -> SparseCSC a
cooToCSCUnchecked cooValue =
  csrToCSCUnchecked (cooToCSRUnchecked cooValue)

cooToCSCSortedUniqueUnchecked :: U.Unbox a => SparseCOO a -> SparseCSC a
cooToCSCSortedUniqueUnchecked cooValue =
  csrToCSCUnchecked (cooToCSRSortedUniqueUnchecked cooValue)

type AxisOffsetState :: Type
data AxisOffsetState = AxisOffsetState
  { axisOffsetCurrent :: !Int,
    axisOffsetEntryCount :: !Int,
    axisOffsetsRev :: [Int]
  }

type SparseEntryVectors :: Type -> Type
data SparseEntryVectors a = SparseEntryVectors
  { sparseEntryOffsets :: !(U.Vector Int),
    sparseEntryIndices :: !(U.Vector Int),
    sparseEntryValues :: !(U.Vector a)
  }

offsetsFromSortedAxes :: Int -> [Int] -> [Int]
offsetsFromSortedAxes axisCount =
  axisOffsets
    . closeOffsetAxes axisCount
    . foldl' acceptAxisOffset initialAxisOffsetState
  where
    axisOffsets =
      reverse . axisOffsetsRev

initialAxisOffsetState :: AxisOffsetState
initialAxisOffsetState =
  AxisOffsetState
    { axisOffsetCurrent = 0,
      axisOffsetEntryCount = 0,
      axisOffsetsRev = [0]
    }

acceptAxisOffset :: AxisOffsetState -> Int -> AxisOffsetState
acceptAxisOffset stateValue axisIndex =
  let closedState = closeOffsetAxes axisIndex stateValue
   in closedState {axisOffsetEntryCount = axisOffsetEntryCount closedState + 1}

closeOffsetAxes :: Int -> AxisOffsetState -> AxisOffsetState
closeOffsetAxes targetAxis stateValue
  | axisOffsetCurrent stateValue >= targetAxis = stateValue
  | otherwise =
      let closedAxisCount = targetAxis - axisOffsetCurrent stateValue
       in stateValue
            { axisOffsetCurrent = targetAxis,
              axisOffsetsRev =
                replicate closedAxisCount (axisOffsetEntryCount stateValue)
                  <> axisOffsetsRev stateValue
            }

incrementMutableInt :: MU.MVector s Int -> Int -> ST s ()
incrementMutableInt values !entryIndex = do
  currentValue <- MU.unsafeRead values entryIndex
  MU.unsafeWrite values entryIndex (currentValue + 1)
{-# INLINE incrementMutableInt #-}

prefixMutableCountsWithStarts ::
  Int ->
  MU.MVector s Int ->
  MU.MVector s Int ->
  ST s ()
prefixMutableCountsWithStarts axisCount counts offsets =
  go 0 0
  where
    go !axisIndex !runningTotal
      | axisIndex >= axisCount =
          MU.unsafeWrite offsets axisCount runningTotal
      | otherwise = do
          axisCountValue <- MU.unsafeRead counts axisIndex
          MU.unsafeWrite offsets axisIndex runningTotal
          MU.unsafeWrite counts axisIndex runningTotal
          go (axisIndex + 1) (runningTotal + axisCountValue)

isMonotonicVector :: U.Vector Int -> Bool
isMonotonicVector values =
  U.and (U.zipWith (<=) values (U.drop 1 values))

vectorEndpoints :: U.Vector Int -> Maybe (Int, Int)
vectorEndpoints values =
  (,) <$> values U.!? 0 <*> values U.!? (U.length values - 1)

detectCSRExecutionPlan ::
  Int ->
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  CSRExecutionPlan
detectCSRExecutionPlan rowCount columnCount rowOffsets columnIndices
  | rowCount /= columnCount = CSRGeneral
  | rowCount <= 0 = CSRContiguousBand 0 0
  | U.null columnIndices = CSRGeneral
  | otherwise =
      case discoverBandwidth 0 0 0 of
        Nothing -> CSRGeneral
        Just (!lowerBandwidth, !upperBandwidth)
          | verifyRows lowerBandwidth upperBandwidth 0 ->
              CSRContiguousBand lowerBandwidth upperBandwidth
          | otherwise -> CSRGeneral
  where
    discoverBandwidth !rowIndex !lowerBandwidth !upperBandwidth
      | rowIndex >= rowCount =
          Just (lowerBandwidth, upperBandwidth)
      | otherwise =
          let !startIndex = rowOffsets `U.unsafeIndex` rowIndex
              !stopIndex = rowOffsets `U.unsafeIndex` (rowIndex + 1)
           in if startIndex >= stopIndex
                then Nothing
                else
                  let !firstColumn =
                        columnIndices `U.unsafeIndex` startIndex
                      !lastColumn =
                        columnIndices `U.unsafeIndex` (stopIndex - 1)
                   in discoverBandwidth
                        (rowIndex + 1)
                        (max lowerBandwidth (rowIndex - firstColumn))
                        (max upperBandwidth (lastColumn - rowIndex))

    verifyRows !lowerBandwidth !upperBandwidth !rowIndex
      | rowIndex >= rowCount = True
      | otherwise =
          let !startIndex = rowOffsets `U.unsafeIndex` rowIndex
              !stopIndex = rowOffsets `U.unsafeIndex` (rowIndex + 1)
              !firstExpected = max 0 (rowIndex - lowerBandwidth)
              !lastExpected = min (columnCount - 1) (rowIndex + upperBandwidth)
              !expectedCount = lastExpected - firstExpected + 1
           in stopIndex - startIndex == expectedCount
                && verifyColumns startIndex stopIndex firstExpected
                && verifyRows lowerBandwidth upperBandwidth (rowIndex + 1)

    verifyColumns !entryIndex !stopIndex !expectedColumn
      | entryIndex >= stopIndex = True
      | columnIndices `U.unsafeIndex` entryIndex /= expectedColumn = False
      | otherwise =
          verifyColumns
            (entryIndex + 1)
            stopIndex
            (expectedColumn + 1)
{-# INLINE detectCSRExecutionPlan #-}

validateCOO :: SparseCOO a -> Either MoonlightError ()
validateCOO cooValue =
  validateCOOEntries (cooRows cooValue) (cooCols cooValue) (cooEntries cooValue)

validateCOOEntries :: Int -> Int -> [(Int, Int, a)] -> Either MoonlightError ()
validateCOOEntries rowCount columnCount entries
  | rowCount < 0 || columnCount < 0 =
      Left (InvariantViolation "COO dimensions must be non-negative")
  | any invalidEntry entries =
      Left (InvariantViolation "COO entry index out of bounds")
  | otherwise =
      Right ()
  where
    invalidEntry (rowIndex, columnIndex, _) =
      rowIndex < 0
        || rowIndex >= rowCount
        || columnIndex < 0
        || columnIndex >= columnCount

validateCSR :: (Eq a, AdditiveMonoid a, U.Unbox a) => SparseCSR a -> Either MoonlightError ()
validateCSR csrValue =
  let offsets = csrRowOffsetsVector csrValue
      columnIndices = csrColumnIndicesVector csrValue
      values = csrValuesVector csrValue
      valueCount = U.length values
   in case vectorEndpoints offsets of
        Nothing -> Left (InvariantViolation "CSR row offsets must be non-empty")
        Just (firstOffset, lastOffset) ->
          validateCSRPayload csrValue offsets columnIndices values valueCount firstOffset lastOffset

validateCSRPayload :: (Eq a, AdditiveMonoid a, U.Unbox a) => SparseCSR a -> U.Vector Int -> U.Vector Int -> U.Vector a -> Int -> Int -> Int -> Either MoonlightError ()
validateCSRPayload csrValue offsets columnIndices values valueCount firstOffset lastOffset
  | csrRows csrValue < 0 || csrCols csrValue < 0 =
      Left (InvariantViolation "CSR dimensions must be non-negative")
  | U.length offsets /= csrRows csrValue + 1 =
      Left (InvariantViolation "CSR row-offset length must equal row count + 1")
  | U.length columnIndices /= valueCount =
      Left (InvariantViolation "CSR column-index and value payload lengths must match")
  | U.any (< 0) offsets =
      Left (InvariantViolation "CSR row offsets must be non-negative")
  | not (isMonotonicVector offsets) =
      Left (InvariantViolation "CSR row offsets must be monotonically non-decreasing")
  | firstOffset /= 0 =
      Left (InvariantViolation "CSR row offsets must begin at 0")
  | lastOffset /= valueCount =
      Left (InvariantViolation "CSR terminal row offset must equal value count")
  | U.any (\columnIndex -> columnIndex < 0 || columnIndex >= csrCols csrValue) columnIndices =
      Left (InvariantViolation "CSR column index out of bounds")
  | not (csrRowsStrictlyCanonical csrValue offsets columnIndices) =
      Left (InvariantViolation "CSR column indices must be strictly increasing within each row")
  | U.any (== zero) values =
      Left (InvariantViolation "CSR values must not store exact zeros")
  | otherwise = Right ()

csrRowsStrictlyCanonical :: SparseCSR a -> U.Vector Int -> U.Vector Int -> Bool
csrRowsStrictlyCanonical csrValue offsets columnIndices =
  U.all csrRowStrictlyCanonical (U.enumFromN 0 (csrRows csrValue))
  where
    csrRowStrictlyCanonical rowIndex =
      let startOffset = offsets `U.unsafeIndex` rowIndex
          stopOffset = offsets `U.unsafeIndex` (rowIndex + 1)
          rowColumns = U.slice startOffset (stopOffset - startOffset) columnIndices
       in U.and (U.zipWith (<) rowColumns (U.drop 1 rowColumns))

csrToCOO :: U.Unbox a => SparseCSR a -> Either MoonlightError (SparseCOO a)
csrToCOO =
  Right . csrToCOOUnchecked

csrToCOOUnchecked :: U.Unbox a => SparseCSR a -> SparseCOO a
csrToCOOUnchecked csrValue =
  SparseCOO
    { cooRows = csrRows csrValue,
      cooCols = csrCols csrValue,
      cooEntries =
        U.toList
          ( U.zip3
              (offsetAxisIndicesVector (csrRows csrValue) (csrRowOffsetsVector csrValue))
              (csrColumnIndicesVector csrValue)
              (csrValuesVector csrValue)
          )
    }

validateCSC :: (Eq a, AdditiveMonoid a, U.Unbox a) => SparseCSC a -> Either MoonlightError ()
validateCSC cscValue =
  let offsets = cscColumnOffsetsVector cscValue
      rowIndices = cscRowIndicesVector cscValue
      values = cscValuesVector cscValue
      valueCount = U.length values
   in case vectorEndpoints offsets of
        Nothing -> Left (InvariantViolation "CSC column offsets must be non-empty")
        Just (firstOffset, lastOffset) ->
          validateCSCPayload cscValue offsets rowIndices values valueCount firstOffset lastOffset

validateCSCPayload :: (Eq a, AdditiveMonoid a, U.Unbox a) => SparseCSC a -> U.Vector Int -> U.Vector Int -> U.Vector a -> Int -> Int -> Int -> Either MoonlightError ()
validateCSCPayload cscValue offsets rowIndices values valueCount firstOffset lastOffset
  | cscRows cscValue < 0 || cscCols cscValue < 0 =
      Left (InvariantViolation "CSC dimensions must be non-negative")
  | U.length offsets /= cscCols cscValue + 1 =
      Left (InvariantViolation "CSC column-offset length must equal column count + 1")
  | U.length rowIndices /= valueCount =
      Left (InvariantViolation "CSC row-index and value payload lengths must match")
  | U.any (< 0) offsets =
      Left (InvariantViolation "CSC column offsets must be non-negative")
  | not (isMonotonicVector offsets) =
      Left (InvariantViolation "CSC column offsets must be monotonically non-decreasing")
  | firstOffset /= 0 =
      Left (InvariantViolation "CSC column offsets must begin at 0")
  | lastOffset /= valueCount =
      Left (InvariantViolation "CSC terminal column offset must equal value count")
  | U.any (\rowIndex -> rowIndex < 0 || rowIndex >= cscRows cscValue) rowIndices =
      Left (InvariantViolation "CSC row index out of bounds")
  | not (cscColumnsStrictlyCanonical cscValue offsets rowIndices) =
      Left (InvariantViolation "CSC row indices must be strictly increasing within each column")
  | U.any (== zero) values =
      Left (InvariantViolation "CSC values must not store exact zeros")
  | otherwise = Right ()

cscColumnsStrictlyCanonical :: SparseCSC a -> U.Vector Int -> U.Vector Int -> Bool
cscColumnsStrictlyCanonical cscValue offsets rowIndices =
  U.all cscColumnStrictlyCanonical (U.enumFromN 0 (cscCols cscValue))
  where
    cscColumnStrictlyCanonical columnIndex =
      let startOffset = offsets `U.unsafeIndex` columnIndex
          stopOffset = offsets `U.unsafeIndex` (columnIndex + 1)
          columnRows = U.slice startOffset (stopOffset - startOffset) rowIndices
       in U.and (U.zipWith (<) columnRows (U.drop 1 columnRows))

cscToCOO :: U.Unbox a => SparseCSC a -> Either MoonlightError (SparseCOO a)
cscToCOO =
  Right . cscToCOOUnchecked

cscToCOOUnchecked :: U.Unbox a => SparseCSC a -> SparseCOO a
cscToCOOUnchecked cscValue =
  SparseCOO
    { cooRows = cscRows cscValue,
      cooCols = cscCols cscValue,
      cooEntries =
        U.toList
          ( U.zip3
              (cscRowIndicesVector cscValue)
              (offsetAxisIndicesVector (cscCols cscValue) (cscColumnOffsetsVector cscValue))
              (cscValuesVector cscValue)
          )
    }

offsetAxisIndicesVector :: Int -> U.Vector Int -> U.Vector Int
offsetAxisIndicesVector axisCount offsets =
  runST $ do
    let !entryCount = offsets `U.unsafeIndex` axisCount
    axisVector <- MU.unsafeNew entryCount
    fillOffsetAxisIndices axisVector 0
    U.unsafeFreeze axisVector
  where
    fillOffsetAxisIndices :: forall s. MU.MVector s Int -> Int -> ST s ()
    fillOffsetAxisIndices axisVector !axisIndex
      | axisIndex >= axisCount = pure ()
      | otherwise = do
          let !entryStart = offsets `U.unsafeIndex` axisIndex
              !entryStop = offsets `U.unsafeIndex` (axisIndex + 1)
          fillAxisSpan axisVector axisIndex entryStart entryStop
          fillOffsetAxisIndices axisVector (axisIndex + 1)

    fillAxisSpan :: forall s. MU.MVector s Int -> Int -> Int -> Int -> ST s ()
    fillAxisSpan axisVector !axisIndex !entryIndex !entryStop
      | entryIndex >= entryStop = pure ()
      | otherwise = do
          MU.unsafeWrite axisVector entryIndex axisIndex
          fillAxisSpan axisVector axisIndex (entryIndex + 1) entryStop

combineEntries ::
  AdditiveGroup a =>
  [(Int, Int, a)] ->
  Map (Int, Int) a
combineEntries =
  foldl'
    (\entryMap (rowIndex, columnIndex, value) -> Map.insertWith add (rowIndex, columnIndex) value entryMap)
    Map.empty

cooToDense ::
  forall r c a.
  (KnownNat r, KnownNat c, AdditiveGroup a) =>
  SparseCOO a ->
  Either MoonlightError (Matrix r c a)
cooToDense cooValue = do
  rowCount <- checkedSparseStaticDimension @r
  columnCount <- checkedSparseStaticDimension @c
  _ <-
    first
      (const (InvariantViolation "static sparse/dense shape exceeds Int cardinality"))
      (checkedNonNegativeProduct rowCount columnCount)
  if cooRows cooValue /= rowCount || cooCols cooValue /= columnCount
    then
      Left
        ( InvariantViolation
            ( "COO shape does not match static matrix dimensions: expected "
                <> show (rowCount, columnCount)
                <> " but received "
                <> show (cooRows cooValue, cooCols cooValue)
            )
        )
    else
      if any (sparseDenseEntryOutOfBounds rowCount columnCount) (cooEntries cooValue)
        then Left (InvariantViolation "COO entry index out of bounds")
        else
          let entryMap = combineEntries (cooEntries cooValue)
              flatValues =
                concatMap
                  (\rowIndex -> map (\columnIndex -> Map.findWithDefault zero (rowIndex, columnIndex) entryMap) [0 .. columnCount - 1])
                  [0 .. rowCount - 1]
           in fromListMatrix @r @c flatValues

checkedSparseStaticDimension :: forall n. KnownNat n => Either MoonlightError Int
checkedSparseStaticDimension =
  first
    (const (InvariantViolation "static sparse/dense dimension exceeds Int cardinality"))
    (checkedNaturalToInt (natVal (Proxy @n)))

sparseDenseEntryOutOfBounds :: Int -> Int -> (Int, Int, a) -> Bool
sparseDenseEntryOutOfBounds rowCount columnCount (rowIndex, columnIndex, _) =
  rowIndex < 0
    || rowIndex >= rowCount
    || columnIndex < 0
    || columnIndex >= columnCount

csrToDense ::
  forall r c a.
  (KnownNat r, KnownNat c, AdditiveGroup a, U.Unbox a) =>
  SparseCSR a ->
  Either MoonlightError (Matrix r c a)
csrToDense csrValue = csrToCOO csrValue >>= cooToDense

cscToDense ::
  forall r c a.
  (KnownNat r, KnownNat c, AdditiveGroup a, U.Unbox a) =>
  SparseCSC a ->
  Either MoonlightError (Matrix r c a)
cscToDense cscValue = cscToCOO cscValue >>= cooToDense

csrToCSC :: U.Unbox a => SparseCSR a -> Either MoonlightError (SparseCSC a)
csrToCSC csrValue =
  Right (csrToCSCUnchecked csrValue)
{-# INLINE csrToCSC #-}

csrToCSCUnchecked :: U.Unbox a => SparseCSR a -> SparseCSC a
csrToCSCUnchecked csrValue =
  let SparseEntryVectors offsetVector rowVector valueVector =
        countingTransposeCompressed
          (csrRows csrValue)
          (csrCols csrValue)
          (csrRowOffsetsVector csrValue)
          (csrColumnIndicesVector csrValue)
          (csrValuesVector csrValue)
   in SparseCSC
        { cscRows = csrRows csrValue,
          cscCols = csrCols csrValue,
          cscColumnOffsetsVector = offsetVector,
          cscRowIndicesVector = rowVector,
          cscValuesVector = valueVector
        }
{-# INLINE csrToCSCUnchecked #-}

cscToCSR :: U.Unbox a => SparseCSC a -> Either MoonlightError (SparseCSR a)
cscToCSR cscValue =
  Right (cscToCSRUnchecked cscValue)

cscToCSRUnchecked :: U.Unbox a => SparseCSC a -> SparseCSR a
cscToCSRUnchecked cscValue =
  let SparseEntryVectors offsetVector columnVector valueVector =
        countingTransposeCompressed
          (cscCols cscValue)
          (cscRows cscValue)
          (cscColumnOffsetsVector cscValue)
          (cscRowIndicesVector cscValue)
          (cscValuesVector cscValue)
   in csrFromCanonicalVectorsUnchecked
        (cscRows cscValue)
        (cscCols cscValue)
        offsetVector
        columnVector
        valueVector

countingTransposeCompressed ::
  forall a.
  U.Unbox a =>
  Int ->
  Int ->
  U.Vector Int ->
  U.Vector Int ->
  U.Vector a ->
  SparseEntryVectors a
countingTransposeCompressed majorCount minorCount majorOffsets minorIndices values =
  runST $ do
    minorCounts <- MU.replicate minorCount 0
    countMinorOccurrences minorCounts 0
    minorOffsets <- MU.replicate (minorCount + 1) 0
    prefixMutableCountsWithStarts minorCount minorCounts minorOffsets
    majorIndices <- MU.unsafeNew entryCount
    transposedValues <- MU.unsafeNew entryCount
    scatterMajorEntries minorCounts majorIndices transposedValues 0
    frozenOffsets <- U.unsafeFreeze minorOffsets
    frozenIndices <- U.unsafeFreeze majorIndices
    frozenValues <- U.unsafeFreeze transposedValues
    pure
      SparseEntryVectors
        { sparseEntryOffsets = frozenOffsets,
          sparseEntryIndices = frozenIndices,
          sparseEntryValues = frozenValues
        }
  where
    !entryCount = U.length values

    countMinorOccurrences :: forall s. MU.MVector s Int -> Int -> ST s ()
    countMinorOccurrences minorCounts !entryIndex
      | entryIndex >= entryCount = pure ()
      | otherwise = do
          let !minorIndex = minorIndices `U.unsafeIndex` entryIndex
          incrementMutableInt minorCounts minorIndex
          countMinorOccurrences minorCounts (entryIndex + 1)

    scatterMajorEntries ::
      forall s.
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s a ->
      Int ->
      ST s ()
    scatterMajorEntries nextOffsets majorIndices transposedValues !majorIndex
      | majorIndex >= majorCount = pure ()
      | otherwise = do
          let !entryStart = majorOffsets `U.unsafeIndex` majorIndex
              !entryStop = majorOffsets `U.unsafeIndex` (majorIndex + 1)
          scatterMajorSpan nextOffsets majorIndices transposedValues majorIndex entryStart entryStop
          scatterMajorEntries nextOffsets majorIndices transposedValues (majorIndex + 1)

    scatterMajorSpan ::
      forall s.
      MU.MVector s Int ->
      MU.MVector s Int ->
      MU.MVector s a ->
      Int ->
      Int ->
      Int ->
      ST s ()
    scatterMajorSpan nextOffsets majorIndices transposedValues !majorIndex !entryIndex !entryStop
      | entryIndex >= entryStop = pure ()
      | otherwise = do
          let !minorIndex = minorIndices `U.unsafeIndex` entryIndex
              !entryValue = values `U.unsafeIndex` entryIndex
          targetIndex <- MU.unsafeRead nextOffsets minorIndex
          MU.unsafeWrite majorIndices targetIndex majorIndex
          MU.unsafeWrite transposedValues targetIndex entryValue
          MU.unsafeWrite nextOffsets minorIndex (targetIndex + 1)
          scatterMajorSpan nextOffsets majorIndices transposedValues majorIndex (entryIndex + 1) entryStop
{-# INLINE countingTransposeCompressed #-}

csrMatVecVector :: SparseCSR Double -> U.Vector Double -> Either MoonlightError (U.Vector Double)
csrMatVecVector csrValue vectorValue =
  if U.length vectorValue /= csrCols csrValue
    then
      Left
        ( InvariantViolation
            ( "CSR matvec dimension mismatch: expected "
                <> show (csrCols csrValue)
                <> " but received "
                <> show (U.length vectorValue)
            )
        )
    else
      Right
        ( case csrExecutionPlan csrValue of
            CSRGeneral ->
              csrMatVecValidatedU
                (csrRows csrValue)
                (csrRowOffsetsVector csrValue)
                (csrColumnIndicesVector csrValue)
                (csrValuesVector csrValue)
                vectorValue
            CSRContiguousBand lowerBandwidth upperBandwidth ->
              csrContiguousBandMatVecValidatedU
                (csrRows csrValue)
                lowerBandwidth
                upperBandwidth
                (csrValuesVector csrValue)
                vectorValue
        )

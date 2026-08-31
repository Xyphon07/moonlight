{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.LinAlg.Pure.Dense.GF2
  ( GF2 (..),
    gf2Zero,
    gf2One,
    gf2FromBool,
    gf2ToBool,
    PackedRow,
    packedRowWidth,
    packedRowNonZeroCount,
    emptyPackedRow,
    unitPackedRow,
    packedRowFromIndices,
    packedRowIndices,
    packedRowMember,
    packedRowIsZero,
    packedRowXor,
    packedRowRemap,
    PackedLinearMap,
    packedLinearMapDomain,
    packedLinearMapCodomain,
    packedLinearMapColumns,
    packedLinearMapFromColumns,
    packedLinearMapFromEntries,
    zeroPackedLinearMap,
    identityPackedLinearMap,
    applyPackedLinearMap,
    composePackedLinearMaps,
    addPackedLinearMaps,
    packedLinearMapIsZero,
    PackedSpan,
    emptyPackedSpan,
    packedSpanFromRows,
    reducePackedRow,
    admitPackedRow,
    ColumnReduction (..),
    reducePackedColumns,
    PackedCoordinateSolver,
    packedCoordinateSolver,
    coordinatesInPackedBasis,
    inverseFromPackedBasisColumns,
    GF2MatrixEntry (..),
    GF2PackedMatrix,
    gf2PackedRows,
    gf2PackedColumns,
    gf2PackedWordsPerRow,
    gf2PackedWords,
    GF2PackedMatrixFailure (..),
    mkGF2PackedMatrix,
    mkGF2PackedMatrixFromRowMajor,
    rankGF2PackedMatrix,
    gf2PackedMatrixLinearMap,
    inverseGF2PackedMatrix,
    GF2SparseColumn,
    gf2SparseColumnIndex,
    gf2SparseColumnRows,
    mkGF2SparseColumn,
    GF2SparseReducerConfig,
    gf2SparseDensifyThreshold,
    mkGF2SparseReducerConfig,
    defaultGF2SparseReducerConfig,
    GF2SparseColumnReduction (..),
    reduceGF2SparseColumns,
    rankGF2SparseColumns,
    independentGF2SparseColumns,
    kernelBasisGF2SparseColumns,
  )
where

import Data.Bifunctor (first)
import Data.Bits
  ( testBit,
  )
import Data.Kind
  ( Type,
  )
import Data.Maybe
  ( listToMaybe,
  )
import Data.Vector
  ( Vector,
  )
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Word
  ( Word64,
  )
import Moonlight.Core
  ( MoonlightError,
    checkedNaturalToInt,
    checkedNonNegativeProduct,
  )
import Moonlight.LinAlg.Internal.GF2.SparseColumn
  ( GF2SparseColumn,
    GF2SparseColumnReduction (..),
    GF2SparseReducerConfig,
    defaultGF2SparseReducerConfig,
    gf2SparseColumnIndex,
    gf2SparseColumnRows,
    gf2SparseDensifyThreshold,
    independentGF2SparseColumns,
    kernelBasisGF2SparseColumns,
    mkGF2SparseColumn,
    mkGF2SparseReducerConfig,
    rankGF2SparseColumns,
    reduceGF2SparseColumns,
  )
import Moonlight.LinAlg.Internal.GF2.Xor
  ( ColumnReduction (..),
    PackedCoordinateSolver,
    PackedLinearMap,
    PackedRow,
    PackedSpan,
    addPackedLinearMaps,
    admitPackedRow,
    applyPackedLinearMap,
    composePackedLinearMaps,
    coordinatesInPackedBasis,
    emptyPackedRow,
    emptyPackedSpan,
    identityPackedLinearMap,
    inverseFromPackedBasisColumns,
    packedCoordinateSolver,
    packedLinearMapCodomain,
    packedLinearMapColumns,
    packedLinearMapDomain,
    packedLinearMapFromColumns,
    packedLinearMapFromEntries,
    packedLinearMapIsZero,
    packedRowFromIndices,
    packedRowIndices,
    packedRowIsZero,
    packedRowMember,
    packedRowNonZeroCount,
    packedRowRemap,
    packedRowWidth,
    packedRowXor,
    packedSpanFromRows,
    reducePackedColumns,
    reducePackedRow,
    unitPackedRow,
    zeroPackedLinearMap,
  )
import Moonlight.LinAlg.Internal.Discrete
  ( GF2 (..),
    PackedBitMatrix (..),
    gf2FromBool,
    gf2One,
    gf2ToBool,
    gf2Zero,
    matrixRowWords,
    packedBitMatrixFromRowMajor,
    packedBitMatrixFromXorEntries,
    rankPackedRows,
  )
import Numeric.Natural
  ( Natural,
  )

type GF2MatrixEntry :: Type
data GF2MatrixEntry = GF2MatrixEntry
  { gf2EntryRow :: !Int,
    gf2EntryColumn :: !Int
  }
  deriving stock (Eq, Ord, Show)

type GF2PackedMatrix :: Type
type GF2PackedMatrix = PackedBitMatrix

gf2PackedRows :: GF2PackedMatrix -> Int
gf2PackedRows =
  packedRows

gf2PackedColumns :: GF2PackedMatrix -> Int
gf2PackedColumns =
  packedCols

gf2PackedWordsPerRow :: GF2PackedMatrix -> Int
gf2PackedWordsPerRow =
  packedWordsPerRow

gf2PackedWords :: GF2PackedMatrix -> U.Vector Word64
gf2PackedWords =
  packedWords

type GF2PackedMatrixFailure :: Type
data GF2PackedMatrixFailure
  = GF2PackedMatrixEntryOutOfBounds !Int !Int !Int !Int
  | GF2PackedMatrixFlatLengthMismatch !Int !Int
  | GF2PackedMatrixCardinalityOutOfBounds !Natural !Natural
  deriving stock (Eq, Show)

mkGF2PackedMatrix ::
  Natural ->
  Natural ->
  [GF2MatrixEntry] ->
  Either GF2PackedMatrixFailure GF2PackedMatrix
mkGF2PackedMatrix rowCountValue columnCountValue entries = do
  (rowCount, columnCount) <- checkedGF2PackedDimensions rowCountValue columnCountValue
  case firstOutOfBoundsEntry rowCount columnCount entries of
    Just entryValue ->
      Left
        ( GF2PackedMatrixEntryOutOfBounds
            (gf2EntryRow entryValue)
            (gf2EntryColumn entryValue)
            rowCount
            columnCount
        )
    Nothing ->
      Right
        ( packedBitMatrixFromXorEntries
            rowCount
            columnCount
            (entryCoordinates <$> entries)
        )

mkGF2PackedMatrixFromRowMajor ::
  Natural ->
  Natural ->
  [GF2] ->
  Either GF2PackedMatrixFailure GF2PackedMatrix
mkGF2PackedMatrixFromRowMajor rowCountValue columnCountValue values = do
  (rowCount, columnCount) <- checkedGF2PackedDimensions rowCountValue columnCountValue
  expectedEntryCount <-
    mapGF2CardinalityFailure rowCountValue columnCountValue
      (checkedNonNegativeProduct rowCount columnCount)
  let actualEntryCount = length values
  if actualEntryCount /= expectedEntryCount
    then Left (GF2PackedMatrixFlatLengthMismatch expectedEntryCount actualEntryCount)
    else
      Right
        ( packedBitMatrixFromRowMajor
            rowCount
            columnCount
            values
        )

checkedGF2PackedDimensions ::
  Natural ->
  Natural ->
  Either GF2PackedMatrixFailure (Int, Int)
checkedGF2PackedDimensions rowCountValue columnCountValue = do
  rowCount <-
    mapGF2CardinalityFailure rowCountValue columnCountValue
      (checkedNaturalToInt rowCountValue)
  columnCount <-
    mapGF2CardinalityFailure rowCountValue columnCountValue
      (checkedNaturalToInt columnCountValue)
  let wordsPerRow =
        columnCount `quot` 64
          + if columnCount `rem` 64 == 0 then 0 else 1
  _ <-
    mapGF2CardinalityFailure rowCountValue columnCountValue
      (checkedNonNegativeProduct rowCount wordsPerRow)
  Right (rowCount, columnCount)

mapGF2CardinalityFailure ::
  Natural ->
  Natural ->
  Either cardinalityFailure value ->
  Either GF2PackedMatrixFailure value
mapGF2CardinalityFailure rowCountValue columnCountValue =
  first
    (const (GF2PackedMatrixCardinalityOutOfBounds rowCountValue columnCountValue))

rankGF2PackedMatrix :: GF2PackedMatrix -> Int
rankGF2PackedMatrix matrixValue =
  rankPackedRows
    (packedCols matrixValue)
    (matrixRowWords matrixValue <$> [0 .. packedRows matrixValue - 1])

gf2PackedMatrixLinearMap :: GF2PackedMatrix -> Either MoonlightError PackedLinearMap
gf2PackedMatrixLinearMap matrixValue = do
  columnRows <- gf2PackedMatrixColumnRows "gf2PackedMatrixLinearMap" matrixValue
  packedLinearMapFromColumns
    "gf2PackedMatrixLinearMap"
    (packedCols matrixValue)
    (packedRows matrixValue)
    columnRows

inverseGF2PackedMatrix :: GF2PackedMatrix -> Either MoonlightError (Maybe PackedLinearMap)
inverseGF2PackedMatrix matrixValue
  | packedRows matrixValue /= packedCols matrixValue =
      Right Nothing
  | otherwise = do
      columnRows <- gf2PackedMatrixColumnRows "inverseGF2PackedMatrix" matrixValue
      reductionValue <- reducePackedColumns "inverseGF2PackedMatrix" (packedRows matrixValue) columnRows
      if V.length (crIndependentIndices reductionValue) == packedCols matrixValue
        then Just <$> inverseFromPackedBasisColumns "inverseGF2PackedMatrix" columnRows
        else Right Nothing

gf2PackedMatrixColumnRows :: String -> GF2PackedMatrix -> Either MoonlightError (Vector PackedRow)
gf2PackedMatrixColumnRows context matrixValue =
  V.fromList
    <$> traverse
      ( \columnIndex ->
          packedRowFromIndices
            (context <> ": column " <> show columnIndex)
            (packedRows matrixValue)
            (columnSupport columnIndex)
      )
      [0 .. packedCols matrixValue - 1]
  where
    columnSupport columnIndex =
      filter
        (\rowIndex -> matrixEntryPresent rowIndex columnIndex)
        [0 .. packedRows matrixValue - 1]

    matrixEntryPresent rowIndex columnIndex =
      let rowWords = matrixRowWords matrixValue rowIndex
          wordIndex = columnIndex `div` 64
          bitIndex = columnIndex `mod` 64
       in maybe False (`testBit` bitIndex) (rowWords U.!? wordIndex)

firstOutOfBoundsEntry :: Int -> Int -> [GF2MatrixEntry] -> Maybe GF2MatrixEntry
firstOutOfBoundsEntry rowCount columnCount =
  listToMaybe . filter (not . entryWithinBounds rowCount columnCount)

entryWithinBounds :: Int -> Int -> GF2MatrixEntry -> Bool
entryWithinBounds rowCount columnCount entry =
  gf2EntryRow entry >= 0
    && gf2EntryRow entry < rowCount
    && gf2EntryColumn entry >= 0
    && gf2EntryColumn entry < columnCount

entryCoordinates :: GF2MatrixEntry -> (Int, Int)
entryCoordinates entry =
  (gf2EntryRow entry, gf2EntryColumn entry)

{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( RowMajorBlock,
    mkRowMajorBlock,
    rowMajorBlockRows,
    rowMajorBlockColumns,
    rowMajorBlockPayload,
    transposeRowMajorBlock,
    symmetrizeRowMajorBlockLower,
    rowMajorBlockEntry,
    SymmetricBlockTridiagonal,
    mkSymmetricBlockTridiagonal,
    symmetricBlockTridiagonalDimension,
    symmetricBlockTridiagonalBlockCount,
    symmetricBlockTridiagonalBandwidth,
    symmetricBlockTridiagonalEntry,
    blockOffsets,
    diagonalPayloadOffsets,
    diagonalLowerPacked,
    couplingPayloadOffsets,
    lowerCouplingPayload,
    applySymmetricBlockTridiagonalU,
    symmetricBlockTridiagonalUpperBound,
    symmetricBlockTridiagonalFrobeniusNorm,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.ST (ST, runST)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Vector qualified as Box
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as MU
import Moonlight.Core
  ( MoonlightError (..),
    checkedNaturalToInt,
    checkedNonNegativeProduct,
    fieldValueValid,
  )
import Numeric.Natural (Natural)
import Prelude

type RowMajorBlock :: Type
data RowMajorBlock = RowMajorBlock
  { rowMajorBlockRows :: !Int,
    rowMajorBlockColumns :: !Int,
    rowMajorBlockPayload :: !(U.Vector Double)
  }
  deriving stock (Eq, Show)

mkRowMajorBlock :: Int -> Int -> U.Vector Double -> Either MoonlightError RowMajorBlock
mkRowMajorBlock rowCount columnCount payload
  | rowCount <= 0 || columnCount <= 0 =
      Left (InvariantViolation "row-major block dimensions must be positive")
  | otherwise = do
      expectedLength <-
        first
          (const (InvariantViolation "row-major block dimensions exceed Int cardinality"))
          (checkedNonNegativeProduct rowCount columnCount)
      if U.length payload /= expectedLength
        then
          Left
            ( InvariantViolation
                ( "row-major block payload length mismatch: expected "
                    <> show expectedLength
                    <> " but received "
                    <> show (U.length payload)
                )
            )
        else
          if U.any (not . fieldValueValid) payload
            then Left (InvariantViolation "row-major block entries must be finite")
            else
              Right
                RowMajorBlock
                  { rowMajorBlockRows = rowCount,
                    rowMajorBlockColumns = columnCount,
                    rowMajorBlockPayload = payload
                  }

rowMajorBlockEntry :: RowMajorBlock -> Int -> Int -> Double
rowMajorBlockEntry blockValue rowIndex columnIndex =
  doubleAt (rowMajorBlockPayload blockValue) (rowIndex * rowMajorBlockColumns blockValue + columnIndex)
{-# INLINE rowMajorBlockEntry #-}

transposeRowMajorBlock :: RowMajorBlock -> RowMajorBlock
transposeRowMajorBlock blockValue =
  RowMajorBlock
    { rowMajorBlockRows = rowMajorBlockColumns blockValue,
      rowMajorBlockColumns = rowMajorBlockRows blockValue,
      rowMajorBlockPayload =
        U.generate
          (U.length (rowMajorBlockPayload blockValue))
          transposeEntry
    }
  where
    transposeEntry payloadIndex =
      let targetColumnCount = rowMajorBlockRows blockValue
          rowIndex = payloadIndex `quot` targetColumnCount
          columnIndex = payloadIndex `rem` targetColumnCount
       in rowMajorBlockEntry blockValue columnIndex rowIndex
{-# INLINE transposeRowMajorBlock #-}

symmetrizeRowMajorBlockLower :: RowMajorBlock -> Either MoonlightError RowMajorBlock
symmetrizeRowMajorBlockLower blockValue
  | rowMajorBlockRows blockValue /= rowMajorBlockColumns blockValue =
      Left (InvariantViolation "lower-authoritative symmetrization requires a square block")
  | otherwise =
      mkRowMajorBlock
        blockSize
        blockSize
        (U.generate (U.length (rowMajorBlockPayload blockValue)) mirroredLowerEntry)
  where
    blockSize = rowMajorBlockRows blockValue
    mirroredLowerEntry payloadIndex =
      let rowIndex = payloadIndex `quot` blockSize
          columnIndex = payloadIndex `rem` blockSize
       in if columnIndex <= rowIndex
            then rowMajorBlockEntry blockValue rowIndex columnIndex
            else rowMajorBlockEntry blockValue columnIndex rowIndex

type SymmetricBlockTridiagonal :: Type
data SymmetricBlockTridiagonal = SymmetricBlockTridiagonal
  { blockOffsets :: !(U.Vector Int),
    diagonalPayloadOffsets :: !(U.Vector Int),
    diagonalLowerPacked :: !(U.Vector Double),
    couplingPayloadOffsets :: !(U.Vector Int),
    lowerCouplingPayload :: !(U.Vector Double)
  }
  deriving stock (Eq, Show)

mkSymmetricBlockTridiagonal ::
  Box.Vector RowMajorBlock ->
  Box.Vector RowMajorBlock ->
  Either MoonlightError SymmetricBlockTridiagonal
mkSymmetricBlockTridiagonal diagonalBlocks lowerCouplingBlocks = do
  if Box.null diagonalBlocks
    then Left (InvariantViolation "symmetric block tridiagonal requires at least one diagonal block")
    else Right ()
  traverse_ validateDiagonalBlock (Box.toList diagonalBlocks)
  let blockSizes = rowMajorBlockRows <$> diagonalBlocks
      expectedCouplingCount = max 0 (Box.length diagonalBlocks - 1)
  if Box.length lowerCouplingBlocks /= expectedCouplingCount
    then
      Left
        ( InvariantViolation
            ( "symmetric block tridiagonal coupling count mismatch: expected "
                <> show expectedCouplingCount
                <> " but received "
                <> show (Box.length lowerCouplingBlocks)
            )
        )
    else Right ()
  traverse_
    (validateCouplingBlock blockSizes)
    (zip [0 :: Int ..] (Box.toList lowerCouplingBlocks))
  let diagonalPayloads = packLowerBlock <$> diagonalBlocks
      couplingPayloads = rowMajorBlockPayload <$> lowerCouplingBlocks
  blockOffsetValues <- checkedOffsetsFromSizes "block offsets" (Box.toList blockSizes)
  diagonalOffsetValues <- checkedOffsetsFromSizes "diagonal payload offsets" (U.length <$> Box.toList diagonalPayloads)
  couplingOffsetValues <- checkedOffsetsFromSizes "coupling payload offsets" (U.length <$> Box.toList couplingPayloads)
  Right
    SymmetricBlockTridiagonal
      { blockOffsets = blockOffsetValues,
        diagonalPayloadOffsets = diagonalOffsetValues,
        diagonalLowerPacked = U.concat (Box.toList diagonalPayloads),
        couplingPayloadOffsets = couplingOffsetValues,
        lowerCouplingPayload = U.concat (Box.toList couplingPayloads)
      }

validateDiagonalBlock :: RowMajorBlock -> Either MoonlightError ()
validateDiagonalBlock blockValue
  | rowMajorBlockRows blockValue /= rowMajorBlockColumns blockValue =
      Left (InvariantViolation "symmetric block tridiagonal diagonal blocks must be square")
  | otherwise =
      if U.and (U.generate (U.length (rowMajorBlockPayload blockValue)) symmetricEntry)
        then Right ()
        else Left (InvariantViolation "symmetric block tridiagonal diagonal block is not exactly symmetric")
  where
    symmetricEntry payloadIndex =
      let blockSize = rowMajorBlockRows blockValue
          rowIndex = payloadIndex `quot` blockSize
          columnIndex = payloadIndex `rem` blockSize
       in rowMajorBlockEntry blockValue rowIndex columnIndex == rowMajorBlockEntry blockValue columnIndex rowIndex

validateCouplingBlock :: Box.Vector Int -> (Int, RowMajorBlock) -> Either MoonlightError ()
validateCouplingBlock blockSizes (couplingIndex, blockValue) =
  let expectedRows = intBoxAt blockSizes (couplingIndex + 1)
      expectedColumns = intBoxAt blockSizes couplingIndex
   in if rowMajorBlockRows blockValue /= expectedRows || rowMajorBlockColumns blockValue /= expectedColumns
        then
          Left
            ( InvariantViolation
                ( "symmetric block tridiagonal coupling block "
                    <> show couplingIndex
                    <> " shape mismatch: expected "
                    <> show (expectedRows, expectedColumns)
                    <> " but received "
                    <> show (rowMajorBlockRows blockValue, rowMajorBlockColumns blockValue)
                )
            )
        else Right ()

packLowerBlock :: RowMajorBlock -> U.Vector Double
packLowerBlock blockValue =
  U.concat
    ( ( \rowIndex ->
          U.generate
            (rowIndex + 1)
            (\columnIndex -> rowMajorBlockEntry blockValue rowIndex columnIndex)
      )
        <$> [0 .. rowMajorBlockRows blockValue - 1]
    )

checkedOffsetsFromSizes :: String -> [Int] -> Either MoonlightError (U.Vector Int)
checkedOffsetsFromSizes context sizes
  | any (< 0) sizes = Left cardinalityFailure
  | otherwise =
      U.fromList
        <$> traverse
          (first (const cardinalityFailure) . checkedNaturalToInt)
          (scanl (+) 0 (fromIntegral <$> sizes :: [Natural]))
  where
    cardinalityFailure =
      InvariantViolation ("symmetric block tridiagonal " <> context <> " exceed Int cardinality")

symmetricBlockTridiagonalDimension :: SymmetricBlockTridiagonal -> Int
symmetricBlockTridiagonalDimension blockValue =
  intAt (blockOffsets blockValue) (U.length (blockOffsets blockValue) - 1)

symmetricBlockTridiagonalBlockCount :: SymmetricBlockTridiagonal -> Int
symmetricBlockTridiagonalBlockCount blockValue =
  max 0 (U.length (blockOffsets blockValue) - 1)

symmetricBlockTridiagonalBandwidth :: SymmetricBlockTridiagonal -> Int
symmetricBlockTridiagonalBandwidth blockValue =
  let blockCount = symmetricBlockTridiagonalBlockCount blockValue
      diagonalWidths =
        U.generate blockCount (\blockIndex -> blockSizeAt blockValue blockIndex - 1)
      couplingWidths =
        U.generate
          (max 0 (blockCount - 1))
          (\couplingIndex -> blockSizeAt blockValue couplingIndex + blockSizeAt blockValue (couplingIndex + 1) - 1)
   in U.maximum (U.concat [diagonalWidths, couplingWidths])

symmetricBlockTridiagonalEntry :: SymmetricBlockTridiagonal -> Int -> Int -> Either MoonlightError Double
symmetricBlockTridiagonalEntry blockValue rowIndex columnIndex
  | rowIndex < 0 || rowIndex >= dimension || columnIndex < 0 || columnIndex >= dimension =
      Left
        ( InvariantViolation
            ( "symmetric block tridiagonal entry index out of bounds: "
                <> show (rowIndex, columnIndex)
                <> " for dimension "
                <> show dimension
            )
        )
  | otherwise =
      case (blockLocalIndex blockValue rowIndex, blockLocalIndex blockValue columnIndex) of
        (Just (rowBlockIndex, rowLocalIndex), Just (columnBlockIndex, columnLocalIndex)) ->
          Right (entryFromLocal rowBlockIndex rowLocalIndex columnBlockIndex columnLocalIndex)
        _ ->
          Left (InvariantViolation "symmetric block tridiagonal entry index missing from block map")
  where
    dimension = symmetricBlockTridiagonalDimension blockValue

    entryFromLocal rowBlockIndex rowLocalIndex columnBlockIndex columnLocalIndex =
      case compare rowBlockIndex columnBlockIndex of
        EQ -> diagonalEntry blockValue rowBlockIndex rowLocalIndex columnLocalIndex
        GT ->
          if rowBlockIndex == columnBlockIndex + 1
            then couplingEntry blockValue columnBlockIndex rowLocalIndex columnLocalIndex
            else 0.0
        LT ->
          if columnBlockIndex == rowBlockIndex + 1
            then couplingEntry blockValue rowBlockIndex columnLocalIndex rowLocalIndex
            else 0.0

applySymmetricBlockTridiagonalU :: SymmetricBlockTridiagonal -> U.Vector Double -> Either MoonlightError (U.Vector Double)
applySymmetricBlockTridiagonalU blockValue inputVector
  | U.length inputVector /= symmetricBlockTridiagonalDimension blockValue =
      Left
        ( InvariantViolation
            ( "symmetric block tridiagonal input dimension mismatch: expected "
                <> show (symmetricBlockTridiagonalDimension blockValue)
                <> " but received "
                <> show (U.length inputVector)
            )
        )
  | otherwise =
      Right
        (runST (applySymmetricBlockTridiagonalST blockValue inputVector))

applySymmetricBlockTridiagonalST :: SymmetricBlockTridiagonal -> U.Vector Double -> ST s (U.Vector Double)
applySymmetricBlockTridiagonalST blockValue inputVector = do
  let dimension = symmetricBlockTridiagonalDimension blockValue
  outputVector <- MU.unsafeNew dimension
  U.foldM'
    (writeApplyBlock blockValue inputVector outputVector)
    ()
    (U.enumFromN 0 (symmetricBlockTridiagonalBlockCount blockValue))
  U.unsafeFreeze outputVector

writeApplyBlock ::
  SymmetricBlockTridiagonal ->
  U.Vector Double ->
  MU.MVector s Double ->
  () ->
  Int ->
  ST s ()
writeApplyBlock blockValue inputVector outputVector () blockIndex =
  U.foldM'
    (writeApplyLocalRow blockValue inputVector outputVector blockIndex blockStart)
    ()
    (U.enumFromN 0 (blockSizeAt blockValue blockIndex))
  where
    blockStart = intAt (blockOffsets blockValue) blockIndex
{-# INLINE writeApplyBlock #-}

writeApplyLocalRow ::
  SymmetricBlockTridiagonal ->
  U.Vector Double ->
  MU.MVector s Double ->
  Int ->
  Int ->
  () ->
  Int ->
  ST s ()
writeApplyLocalRow blockValue inputVector outputVector blockIndex blockStart () localRow =
  MU.unsafeWrite
    outputVector
    (blockStart + localRow)
    (applyBlockEntry blockValue inputVector blockIndex localRow)
{-# INLINE writeApplyLocalRow #-}

symmetricBlockTridiagonalUpperBound :: SymmetricBlockTridiagonal -> Double
symmetricBlockTridiagonalUpperBound blockValue =
  let dimension = symmetricBlockTridiagonalDimension blockValue
   in if dimension <= 0
        then 0.0
        else U.maximum (U.generate dimension (rowAbsSum blockValue))

symmetricBlockTridiagonalFrobeniusNorm :: SymmetricBlockTridiagonal -> Double
symmetricBlockTridiagonalFrobeniusNorm blockValue =
  sqrt
    ( diagonalPackedWeightedSumSquares blockValue
        + 2.0 * U.foldl' (\accumulator entryValue -> accumulator + squared entryValue) 0.0 (lowerCouplingPayload blockValue)
    )

diagonalPackedWeightedSumSquares :: SymmetricBlockTridiagonal -> Double
diagonalPackedWeightedSumSquares blockValue =
  U.foldl'
    (\accumulator blockIndex ->
      accumulator
        + sumIndexRange
          (blockSizeAt blockValue blockIndex)
          (\localRow ->
            sumIndexRange
              (localRow + 1)
              (\localColumn ->
                (if localRow == localColumn then 1.0 else 2.0)
                  * squared (diagonalEntry blockValue blockIndex localRow localColumn)
              )
          )
    )
    0.0
    (U.enumFromN 0 (symmetricBlockTridiagonalBlockCount blockValue))

squared :: Double -> Double
squared value = value * value
{-# INLINE squared #-}

applyBlockEntry :: SymmetricBlockTridiagonal -> U.Vector Double -> Int -> Int -> Double
applyBlockEntry blockValue inputVector blockIndex localRow =
  diagonalContribution blockValue inputVector blockIndex localRow
    + lowerContribution blockValue inputVector blockIndex localRow
    + upperContribution blockValue inputVector blockIndex localRow
{-# INLINE applyBlockEntry #-}

rowAbsSum :: SymmetricBlockTridiagonal -> Int -> Double
rowAbsSum blockValue rowIndex =
  case blockLocalIndex blockValue rowIndex of
    Nothing -> 0.0
    Just (blockIndex, localRow) ->
      diagonalAbsSum blockValue blockIndex localRow
        + lowerAbsSum blockValue blockIndex localRow
        + upperAbsSum blockValue blockIndex localRow

blockLocalIndex :: SymmetricBlockTridiagonal -> Int -> Maybe (Int, Int)
blockLocalIndex blockValue rowIndex =
  U.foldl' selectBlock Nothing (U.enumFromN 0 (symmetricBlockTridiagonalBlockCount blockValue))
  where
    selectBlock selectedBlock blockIndex =
      selectedBlock
        <|> let startOffset = intAt (blockOffsets blockValue) blockIndex
                stopOffset = intAt (blockOffsets blockValue) (blockIndex + 1)
             in if rowIndex >= startOffset && rowIndex < stopOffset
                  then Just (blockIndex, rowIndex - startOffset)
                  else Nothing

blockSizeAt :: SymmetricBlockTridiagonal -> Int -> Int
blockSizeAt blockValue blockIndex =
  (intAt (blockOffsets blockValue) (blockIndex + 1))
    - (intAt (blockOffsets blockValue) blockIndex)
{-# INLINE blockSizeAt #-}

diagonalContribution :: SymmetricBlockTridiagonal -> U.Vector Double -> Int -> Int -> Double
diagonalContribution blockValue inputVector blockIndex localRow =
  let blockStart = intAt (blockOffsets blockValue) blockIndex
      blockSize = blockSizeAt blockValue blockIndex
   in sumIndexRange
        blockSize
        ( \localColumn ->
            diagonalEntry blockValue blockIndex localRow localColumn
              * doubleAt inputVector (blockStart + localColumn)
        )
{-# INLINE diagonalContribution #-}

lowerContribution :: SymmetricBlockTridiagonal -> U.Vector Double -> Int -> Int -> Double
lowerContribution blockValue inputVector blockIndex localRow
  | blockIndex <= 0 = 0.0
  | otherwise =
      let couplingIndex = blockIndex - 1
          previousStart = intAt (blockOffsets blockValue) couplingIndex
          previousSize = blockSizeAt blockValue couplingIndex
       in sumIndexRange
            previousSize
            ( \localColumn ->
                couplingEntry blockValue couplingIndex localRow localColumn
                  * doubleAt inputVector (previousStart + localColumn)
            )
{-# INLINE lowerContribution #-}

upperContribution :: SymmetricBlockTridiagonal -> U.Vector Double -> Int -> Int -> Double
upperContribution blockValue inputVector blockIndex localRow
  | blockIndex + 1 >= symmetricBlockTridiagonalBlockCount blockValue = 0.0
  | otherwise =
      let nextStart = intAt (blockOffsets blockValue) (blockIndex + 1)
          nextSize = blockSizeAt blockValue (blockIndex + 1)
       in sumIndexRange
            nextSize
            ( \nextLocalRow ->
                couplingEntry blockValue blockIndex nextLocalRow localRow
                  * doubleAt inputVector (nextStart + nextLocalRow)
            )
{-# INLINE upperContribution #-}

diagonalAbsSum :: SymmetricBlockTridiagonal -> Int -> Int -> Double
diagonalAbsSum blockValue blockIndex localRow =
  sumIndexRange
    (blockSizeAt blockValue blockIndex)
    (\localColumn -> abs (diagonalEntry blockValue blockIndex localRow localColumn))
{-# INLINE diagonalAbsSum #-}

lowerAbsSum :: SymmetricBlockTridiagonal -> Int -> Int -> Double
lowerAbsSum blockValue blockIndex localRow
  | blockIndex <= 0 = 0.0
  | otherwise =
      sumIndexRange
        (blockSizeAt blockValue (blockIndex - 1))
        (\localColumn -> abs (couplingEntry blockValue (blockIndex - 1) localRow localColumn))
{-# INLINE lowerAbsSum #-}

upperAbsSum :: SymmetricBlockTridiagonal -> Int -> Int -> Double
upperAbsSum blockValue blockIndex localRow
  | blockIndex + 1 >= symmetricBlockTridiagonalBlockCount blockValue = 0.0
  | otherwise =
      sumIndexRange
        (blockSizeAt blockValue (blockIndex + 1))
        (\nextLocalRow -> abs (couplingEntry blockValue blockIndex nextLocalRow localRow))
{-# INLINE upperAbsSum #-}

sumIndexRange :: Int -> (Int -> Double) -> Double
sumIndexRange count valueAt =
  U.foldl' (\accumulator indexValue -> accumulator + valueAt indexValue) 0.0 (U.enumFromN 0 count)
{-# INLINE sumIndexRange #-}

diagonalEntry :: SymmetricBlockTridiagonal -> Int -> Int -> Int -> Double
diagonalEntry blockValue blockIndex localRow localColumn
  | localColumn <= localRow =
      doubleAt (diagonalLowerPacked blockValue) (diagonalPayloadStart blockValue blockIndex + packedLowerIndex localRow localColumn)
  | otherwise =
      doubleAt (diagonalLowerPacked blockValue) (diagonalPayloadStart blockValue blockIndex + packedLowerIndex localColumn localRow)
{-# INLINE diagonalEntry #-}

couplingEntry :: SymmetricBlockTridiagonal -> Int -> Int -> Int -> Double
couplingEntry blockValue couplingIndex localRow localColumn =
  let couplingStart = intAt (couplingPayloadOffsets blockValue) couplingIndex
      couplingColumns = blockSizeAt blockValue couplingIndex
   in doubleAt (lowerCouplingPayload blockValue) (couplingStart + localRow * couplingColumns + localColumn)
{-# INLINE couplingEntry #-}

diagonalPayloadStart :: SymmetricBlockTridiagonal -> Int -> Int
diagonalPayloadStart blockValue blockIndex =
  intAt (diagonalPayloadOffsets blockValue) blockIndex
{-# INLINE diagonalPayloadStart #-}

packedLowerIndex :: Int -> Int -> Int
packedLowerIndex rowIndex columnIndex =
  rowIndex * (rowIndex + 1) `quot` 2 + columnIndex
{-# INLINE packedLowerIndex #-}

intAt :: U.Vector Int -> Int -> Int
intAt values indexValue =
  maybe 0 id (values U.!? indexValue)
{-# INLINE intAt #-}

intBoxAt :: Box.Vector Int -> Int -> Int
intBoxAt values indexValue =
  maybe 0 id (values Box.!? indexValue)
{-# INLINE intBoxAt #-}

doubleAt :: U.Vector Double -> Int -> Double
doubleAt values indexValue =
  maybe 0.0 id (values U.!? indexValue)
{-# INLINE doubleAt #-}

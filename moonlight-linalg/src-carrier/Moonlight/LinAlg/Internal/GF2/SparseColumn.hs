{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.LinAlg.Internal.GF2.SparseColumn
  ( GF2SparseColumn
  , gf2SparseColumnIndex
  , gf2SparseColumnRows
  , mkGF2SparseColumn
  , GF2SparseReducerConfig
  , gf2SparseDensifyThreshold
  , mkGF2SparseReducerConfig
  , defaultGF2SparseReducerConfig
  , GF2SparseColumnReduction (..)
  , reduceGF2SparseColumns
  , rankGF2SparseColumns
  , independentGF2SparseColumns
  , kernelBasisGF2SparseColumns
  ) where

import Control.Monad (foldM, unless)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.GF2.Xor
  ( PackedRow
  , packedRowFromIndices
  , packedRowIndices
  , packedRowIsZero
  , packedRowNonZeroCount
  , packedRowXor
  , unitPackedRow
  )

-- | Sparse low-pivot GF2 column reduction with optional packed fallback.
type GF2SparseColumn :: Type
data GF2SparseColumn = GF2SparseColumn
  { gf2SparseColumnIndex :: !Int
  , gf2SparseColumnRows :: ![Int]
  }
  deriving stock (Eq, Show)

type GF2SparseReducerConfig :: Type
data GF2SparseReducerConfig = GF2SparseReducerConfig
  { gf2SparseDensifyThreshold :: !Int
  }
  deriving stock (Eq, Show)

type GF2SparseColumnReduction :: Type
data GF2SparseColumnReduction = GF2SparseColumnReduction
  { gf2SparseReductionRank :: !Int
  , gf2SparseIndependentColumns :: !(Vector Int)
  , gf2SparseKernelBasis :: !(Vector PackedRow)
  }
  deriving stock (Eq, Show)

type SparseColumnBody :: Type
data SparseColumnBody
  = SparseRows ![Int]
  | PackedRows !PackedRow
  deriving stock (Eq, Show)

type TrackedSparseBasisColumn :: Type
data TrackedSparseBasisColumn = TrackedSparseBasisColumn
  { tsbcData :: !SparseColumnBody
  , tsbcWitness :: !PackedRow
  }
  deriving stock (Eq, Show)

mkGF2SparseColumn :: String -> Int -> Int -> [Int] -> Either MoonlightError GF2SparseColumn
mkGF2SparseColumn context rowCount columnIndex rowsValue = do
  unless (rowCount >= 0)
    (Left (InvariantViolation (context <> ": negative sparse GF2 row count " <> show rowCount)))
  unless (columnIndex >= 0)
    (Left (InvariantViolation (context <> ": negative sparse GF2 column index " <> show columnIndex)))
  traverse_ validateRow rowsValue
  Right
    GF2SparseColumn
      { gf2SparseColumnIndex = columnIndex
      , gf2SparseColumnRows = canonicalGF2Support rowsValue
      }
  where
    validateRow rowIndex
      | rowIndex < 0 || rowIndex >= rowCount =
          Left
            ( InvariantViolation
                ( context
                    <> ": sparse GF2 row "
                    <> show rowIndex
                    <> " is outside row count "
                    <> show rowCount
                )
            )
      | otherwise = Right ()

mkGF2SparseReducerConfig :: String -> Int -> Either MoonlightError GF2SparseReducerConfig
mkGF2SparseReducerConfig context thresholdValue
  | thresholdValue < 0 =
      Left (InvariantViolation (context <> ": negative sparse GF2 densify threshold " <> show thresholdValue))
  | otherwise =
      Right GF2SparseReducerConfig {gf2SparseDensifyThreshold = thresholdValue}

defaultGF2SparseReducerConfig :: GF2SparseReducerConfig
defaultGF2SparseReducerConfig =
  GF2SparseReducerConfig {gf2SparseDensifyThreshold = 64}

reduceGF2SparseColumns ::
  GF2SparseReducerConfig ->
  Int ->
  Int ->
  Vector GF2SparseColumn ->
  Either MoonlightError GF2SparseColumnReduction
reduceGF2SparseColumns configValue rowCount columnCount columnsValue = do
  unless (rowCount >= 0 && columnCount >= 0)
    (Left (InvariantViolation ("reduceGF2SparseColumns: negative sparse GF2 shape " <> show (rowCount, columnCount))))
  orderedColumns <- validateColumnCover columnCount columnsValue
  (_, independentReversed, kernelReversed) <-
    foldM
      (reduceColumn configValue rowCount columnCount)
      (IntMap.empty, [], [])
      (V.toList orderedColumns)
  let independentColumns = V.fromList (reverse independentReversed)
  Right
    GF2SparseColumnReduction
      { gf2SparseReductionRank = V.length independentColumns
      , gf2SparseIndependentColumns = independentColumns
      , gf2SparseKernelBasis = V.fromList (reverse kernelReversed)
      }

rankGF2SparseColumns ::
  GF2SparseReducerConfig ->
  Int ->
  Int ->
  Vector GF2SparseColumn ->
  Either MoonlightError Int
rankGF2SparseColumns configValue rowCount columnCount columnsValue =
  gf2SparseReductionRank <$> reduceGF2SparseColumns configValue rowCount columnCount columnsValue

independentGF2SparseColumns ::
  GF2SparseReducerConfig ->
  Int ->
  Int ->
  Vector GF2SparseColumn ->
  Either MoonlightError (Vector Int)
independentGF2SparseColumns configValue rowCount columnCount columnsValue =
  gf2SparseIndependentColumns <$> reduceGF2SparseColumns configValue rowCount columnCount columnsValue

kernelBasisGF2SparseColumns ::
  GF2SparseReducerConfig ->
  Int ->
  Int ->
  Vector GF2SparseColumn ->
  Either MoonlightError (Vector PackedRow)
kernelBasisGF2SparseColumns configValue rowCount columnCount columnsValue =
  gf2SparseKernelBasis <$> reduceGF2SparseColumns configValue rowCount columnCount columnsValue

reduceColumn ::
  GF2SparseReducerConfig ->
  Int ->
  Int ->
  (IntMap TrackedSparseBasisColumn, [Int], [PackedRow]) ->
  GF2SparseColumn ->
  Either MoonlightError (IntMap TrackedSparseBasisColumn, [Int], [PackedRow])
reduceColumn configValue rowCount columnCount (basisColumns, independentReversed, kernelReversed) columnValue = do
  witnessValue <- unitPackedRow "reduceGF2SparseColumns: witness" columnCount (gf2SparseColumnIndex columnValue)
  initialBody <- normalizeRows configValue rowCount (gf2SparseColumnRows columnValue)
  (reducedData, reducedWitness) <- reduceSparseTracked configValue rowCount basisColumns initialBody witnessValue
  case sparseBodyLowPivot reducedData of
    Nothing -> Right (basisColumns, independentReversed, reducedWitness : kernelReversed)
    Just pivotIndex ->
      Right
        ( IntMap.insert
            pivotIndex
            TrackedSparseBasisColumn
              { tsbcData = reducedData
              , tsbcWitness = reducedWitness
              }
            basisColumns
        , gf2SparseColumnIndex columnValue : independentReversed
        , kernelReversed
        )

reduceSparseTracked ::
  GF2SparseReducerConfig ->
  Int ->
  IntMap TrackedSparseBasisColumn ->
  SparseColumnBody ->
  PackedRow ->
  Either MoonlightError (SparseColumnBody, PackedRow)
reduceSparseTracked configValue rowCount basisColumns dataValue witnessValue =
  case sparseBodyLowPivot dataValue of
    Nothing -> Right (dataValue, witnessValue)
    Just pivotIndex ->
      case IntMap.lookup pivotIndex basisColumns of
        Nothing -> Right (dataValue, witnessValue)
        Just TrackedSparseBasisColumn {tsbcData, tsbcWitness} -> do
          reducedData <- sparseBodyXor configValue rowCount dataValue tsbcData
          reducedWitness <- packedRowXor "reduceGF2SparseColumns: witness xor" witnessValue tsbcWitness
          reduceSparseTracked configValue rowCount basisColumns reducedData reducedWitness

sparseBodyXor ::
  GF2SparseReducerConfig ->
  Int ->
  SparseColumnBody ->
  SparseColumnBody ->
  Either MoonlightError SparseColumnBody
sparseBodyXor configValue rowCount leftBody rightBody =
  case (leftBody, rightBody) of
    (PackedRows leftPacked, PackedRows rightPacked) ->
      packedRowXor "reduceGF2SparseColumns: packed sparse body xor" leftPacked rightPacked
        >>= normalizePacked configValue
    _ ->
      normalizeRows
        configValue
        rowCount
        (xorSortedSupports (sparseBodyRows leftBody) (sparseBodyRows rightBody))

normalizeRows :: GF2SparseReducerConfig -> Int -> [Int] -> Either MoonlightError SparseColumnBody
normalizeRows configValue rowCount rowsValue
  | supportPastThreshold configValue rowsValue =
      PackedRows <$> packedRowFromIndices "reduceGF2SparseColumns: densified sparse body" rowCount rowsValue
  | otherwise = Right (SparseRows rowsValue)

normalizePacked :: GF2SparseReducerConfig -> PackedRow -> Either MoonlightError SparseColumnBody
normalizePacked configValue packedValue
  | packedRowIsZero packedValue = Right (SparseRows [])
  | packedRowNonZeroCount packedValue >= gf2SparseDensifyThreshold configValue =
      Right (PackedRows packedValue)
  | otherwise = Right (SparseRows (packedRowIndices packedValue))

supportPastThreshold :: GF2SparseReducerConfig -> [Int] -> Bool
supportPastThreshold configValue rowsValue =
  not (null rowsValue) && length rowsValue >= gf2SparseDensifyThreshold configValue

sparseBodyLowPivot :: SparseColumnBody -> Maybe Int
sparseBodyLowPivot bodyValue =
  foldl' (\_ rowIndex -> Just rowIndex) Nothing (sparseBodyRows bodyValue)

sparseBodyRows :: SparseColumnBody -> [Int]
sparseBodyRows bodyValue =
  case bodyValue of
    SparseRows rowsValue -> rowsValue
    PackedRows packedValue -> packedRowIndices packedValue

validateColumnCover :: Int -> Vector GF2SparseColumn -> Either MoonlightError (Vector GF2SparseColumn)
validateColumnCover columnCount columnsValue = do
  unless (V.length columnsValue == columnCount)
    ( Left
        ( InvariantViolation
            ( "reduceGF2SparseColumns: received "
                <> show (V.length columnsValue)
                <> " sparse GF2 columns for column count "
                <> show columnCount
            )
        )
    )
  traverse_ validateIndexedColumn (zip [0 .. columnCount - 1] orderedColumns)
  Right (V.fromList orderedColumns)
  where
    orderedColumns =
      sortOn gf2SparseColumnIndex (V.toList columnsValue)

    validateIndexedColumn (expectedIndex, columnValue)
      | gf2SparseColumnIndex columnValue == expectedIndex = Right ()
      | otherwise =
          Left
            ( InvariantViolation
                ( "reduceGF2SparseColumns: sparse GF2 column cover expected index "
                    <> show expectedIndex
                    <> " but found "
                    <> show (gf2SparseColumnIndex columnValue)
                )
            )

canonicalGF2Support :: [Int] -> [Int]
canonicalGF2Support =
  IntMap.keys . foldl' toggleRow IntMap.empty
  where
    toggleRow supportMap rowIndex =
      case IntMap.lookup rowIndex supportMap of
        Nothing -> IntMap.insert rowIndex () supportMap
        Just () -> IntMap.delete rowIndex supportMap

xorSortedSupports :: [Int] -> [Int] -> [Int]
xorSortedSupports leftRows rightRows =
  case (leftRows, rightRows) of
    ([], _) -> rightRows
    (_, []) -> leftRows
    (leftRow : remainingLeft, rightRow : remainingRight) ->
      case compare leftRow rightRow of
        LT -> leftRow : xorSortedSupports remainingLeft rightRows
        EQ -> xorSortedSupports remainingLeft remainingRight
        GT -> rightRow : xorSortedSupports leftRows remainingRight

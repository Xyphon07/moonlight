module Moonlight.LinAlg.Internal.Backend.RowStore
  ( RowStore,
    rowStoreFromRows,
    rowStoreToRows,
    rowStoreFlatten,
    rowStoreShape,
    rowStoreRowAt,
    rowStoreRowAtInt,
    rowStoreValueAt,
    rowStoreValueAtInt,
    replaceRowStore,
    replaceRowStoreAtInt,
    swapRowsStore,
    swapRowsStoreAtInt,
    swapColumnsStore,
    columnStore,
    replaceColumnStore,
    traverseRowStoreWithIndex,
  )
where

import Data.Kind (Type)
import Data.Vector qualified as Box
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Internal.Primitives
  ( ColumnIndex,
    RowIndex,
    columnIndexInt,
    rowIndexInt,
  )
import Prelude

type RowStore :: Type -> Type
newtype RowStore a = RowStore (Box.Vector (Box.Vector a))
  deriving stock (Eq, Show)

rowStoreFromRows :: [[a]] -> RowStore a
rowStoreFromRows =
  RowStore . Box.fromList . fmap Box.fromList

rowStoreToRows :: RowStore a -> [[a]]
rowStoreToRows (RowStore rows) =
  Box.toList (fmap Box.toList rows)

rowStoreFlatten :: RowStore a -> [a]
rowStoreFlatten =
  concat . rowStoreToRows

rowStoreShape :: RowStore a -> (Int, Int)
rowStoreShape (RowStore rows) =
  ( Box.length rows,
    maybe 0 Box.length (rows Box.!? 0)
  )

rowStoreRowAt :: MoonlightError -> RowIndex -> RowStore a -> Either MoonlightError (Box.Vector a)
rowStoreRowAt failure rowIndex =
  rowStoreRowAtInt failure (rowIndexInt rowIndex)

rowStoreRowAtInt :: MoonlightError -> Int -> RowStore a -> Either MoonlightError (Box.Vector a)
rowStoreRowAtInt failure rowIndex (RowStore rows) =
  maybe (Left failure) Right (rows Box.!? rowIndex)

rowStoreValueAt :: MoonlightError -> RowIndex -> ColumnIndex -> RowStore a -> Either MoonlightError a
rowStoreValueAt failure rowIndex columnIndex =
  rowStoreValueAtInt failure (rowIndexInt rowIndex) (columnIndexInt columnIndex)

rowStoreValueAtInt :: MoonlightError -> Int -> Int -> RowStore a -> Either MoonlightError a
rowStoreValueAtInt failure rowIndex columnIndex store =
  rowStoreRowAtInt failure rowIndex store
    >>= \rowValues -> maybe (Left failure) Right (rowValues Box.!? columnIndex)

replaceRowStore :: MoonlightError -> RowIndex -> Box.Vector a -> RowStore a -> Either MoonlightError (RowStore a)
replaceRowStore failure rowIndex =
  replaceRowStoreAtInt failure (rowIndexInt rowIndex)

replaceRowStoreAtInt :: MoonlightError -> Int -> Box.Vector a -> RowStore a -> Either MoonlightError (RowStore a)
replaceRowStoreAtInt failure rowIndex replacement (RowStore rows) =
  case rows Box.!? rowIndex of
    Nothing -> Left failure
    Just _ -> Right (RowStore (rows Box.// [(rowIndex, replacement)]))

swapRowsStore :: MoonlightError -> RowIndex -> RowIndex -> RowStore a -> Either MoonlightError (RowStore a)
swapRowsStore failure leftIndex rightIndex =
  swapRowsStoreAtInt failure (rowIndexInt leftIndex) (rowIndexInt rightIndex)

swapRowsStoreAtInt :: MoonlightError -> Int -> Int -> RowStore a -> Either MoonlightError (RowStore a)
swapRowsStoreAtInt failure leftIndex rightIndex (RowStore rows) =
  case (rows Box.!? leftIndex, rows Box.!? rightIndex) of
    (Just leftRow, Just rightRow) ->
      Right (RowStore (rows Box.// [(leftIndex, rightRow), (rightIndex, leftRow)]))
    _ -> Left failure

swapColumnsStore :: MoonlightError -> ColumnIndex -> ColumnIndex -> RowStore a -> Either MoonlightError (RowStore a)
swapColumnsStore failure leftIndex rightIndex =
  traverseRowStoreWithIndex
    ( \_ rowValues ->
        swapVectorAt failure (columnIndexInt leftIndex) (columnIndexInt rightIndex) rowValues
    )

columnStore :: MoonlightError -> ColumnIndex -> RowStore a -> Either MoonlightError (Box.Vector a)
columnStore failure columnIndex (RowStore rows) =
  traverse
    (\rowValues -> maybe (Left failure) Right (rowValues Box.!? columnIndexInt columnIndex))
    rows

replaceColumnStore :: MoonlightError -> ColumnIndex -> Box.Vector a -> RowStore a -> Either MoonlightError (RowStore a)
replaceColumnStore failure columnIndex columnValues store@(RowStore rows)
  | Box.length columnValues /= fst (rowStoreShape store) = Left failure
  | otherwise =
      traverseRowStoreWithIndex
        ( \rowIndex rowValues ->
            case columnValues Box.!? rowIndex of
              Nothing -> Left failure
              Just columnValue ->
                replaceVectorAt failure (columnIndexInt columnIndex) columnValue rowValues
        )
        (RowStore rows)

traverseRowStoreWithIndex ::
  (Int -> Box.Vector a -> Either MoonlightError (Box.Vector b)) ->
  RowStore a ->
  Either MoonlightError (RowStore b)
traverseRowStoreWithIndex transform (RowStore rows) =
  RowStore <$> Box.imapM transform rows

swapVectorAt :: MoonlightError -> Int -> Int -> Box.Vector a -> Either MoonlightError (Box.Vector a)
swapVectorAt failure leftIndex rightIndex values =
  case (values Box.!? leftIndex, values Box.!? rightIndex) of
    (Just leftValue, Just rightValue) ->
      Right (values Box.// [(leftIndex, rightValue), (rightIndex, leftValue)])
    _ -> Left failure

replaceVectorAt :: MoonlightError -> Int -> a -> Box.Vector a -> Either MoonlightError (Box.Vector a)
replaceVectorAt failure indexValue replacement values =
  case values Box.!? indexValue of
    Nothing -> Left failure
    Just _ -> Right (values Box.// [(indexValue, replacement)])

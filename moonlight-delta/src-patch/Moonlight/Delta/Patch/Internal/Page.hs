{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Page
  ( ColumnView (..),
    validateAlignedPageBoundary,
    invertPage,
    replaceRecordedPage,
    replacePageEntryAfter,
    columnView,
    columnMaybeAt,
    columnEndpointFromView,
    advancePackedIndex,
    rowCellAt,
    pageMinimumKey,
    pageLookupIndex,
    pageForKey,
    pageForInsertion,
    minimumKey,
    maximumKey,
    pageKeyAt,
  )
where

import Data.Bits (Bits (complement, shiftL, testBit, (.&.), (.|.)))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray
  ( emptySmallArray,
    indexSmallArray,
    sizeofSmallArray,
    smallArrayFromList,
  )
import Data.Word (Word64)
import Moonlight.Delta.Patch.Internal.Cell
import Moonlight.Delta.Patch.Internal.Types
import Prelude

data ColumnView value = ColumnView
  { columnViewMask :: {-# UNPACK #-} !Word64,
    columnViewValues :: !(ValueColumn value)
  }

validateAlignedPageBoundary ::
  forall key value error.
  (Ord key, Eq value) =>
  (key -> Endpoint value -> Endpoint value -> error) ->
  key ->
  Page key value ->
  EndpointColumn value ->
  key ->
  Page key value ->
  EndpointColumn value ->
  BoundaryResult error
validateAlignedPageBoundary makeError actualMaximum actualPage actualColumn requiredMaximum requiredPage requiredColumn
  | pageCount actualPage /= pageCount requiredPage =
      PageBoundaryDiverged
  | otherwise =
      case (firstPageKeyMismatch actualMaximum actualPage requiredMaximum requiredPage, firstEndpointMismatch count actualColumn requiredColumn) of
        (Nothing, Nothing) ->
          PageBoundaryMatched
        (Just _keyMismatch, Nothing) ->
          PageBoundaryDiverged
        (Nothing, Just endpointMismatch) ->
          endpointFailure endpointMismatch
        (Just keyMismatch, Just endpointMismatch)
          | keyMismatch <= endpointMismatch ->
              PageBoundaryDiverged
          | otherwise ->
              endpointFailure endpointMismatch
  where
    !count = pageCount actualPage

    endpointFailure endpointMismatch =
      PageBoundaryRejected
        ( makeError
            (pageKeyAt requiredMaximum requiredPage endpointMismatch)
            (endpointAtByScan actualColumn endpointMismatch)
            (endpointAtByScan requiredColumn endpointMismatch)
        )
{-# INLINABLE validateAlignedPageBoundary #-}

firstPageKeyMismatch ::
  Ord key =>
  key ->
  Page key leftValue ->
  key ->
  Page key rightValue ->
  Maybe Int
firstPageKeyMismatch leftMaximum leftPage rightMaximum rightPage =
  case firstPrefixKeyMismatch (pagePrefixKeys leftPage) (pagePrefixKeys rightPage) of
    Just mismatch ->
      Just mismatch
    Nothing
      | compare leftMaximum rightMaximum == EQ ->
          Nothing
      | otherwise ->
          Just (leftCount - 1)
  where
    !leftCount = pageCount leftPage

{-# INLINE firstPageKeyMismatch #-}

firstPrefixKeyMismatch :: Ord key => KeyColumn key -> KeyColumn key -> Maybe Int
firstPrefixKeyMismatch left right
  | leftCount /= rightCount =
      Just 0
  | otherwise =
      case (left, right) of
        (IntRangeKeys leftStart leftSize, IntRangeKeys rightStart rightSize)
          | leftSize /= rightSize ->
              Just 0
          | leftStart == rightStart ->
              Nothing
          | otherwise ->
              Just 0
        (IntAffineKeys leftStart leftStep leftSize, IntAffineKeys rightStart rightStep rightSize)
          | leftSize /= rightSize ->
              Just 0
          | leftStart /= rightStart ->
              Just 0
          | leftSize <= 1 || leftStep == rightStep ->
              Nothing
          | otherwise ->
              Just 1
        (IntRangeKeys leftStart leftSize, IntAffineKeys rightStart rightStep rightSize)
          | leftSize /= rightSize ->
              Just 0
          | leftStart /= rightStart ->
              Just 0
          | leftSize <= 1 || rightStep == 1 ->
              Nothing
          | otherwise ->
              Just 1
        (IntAffineKeys leftStart leftStep leftSize, IntRangeKeys rightStart rightSize)
          | leftSize /= rightSize ->
              Just 0
          | leftStart /= rightStart ->
              Just 0
          | leftSize <= 1 || leftStep == 1 ->
              Nothing
          | otherwise ->
              Just 1
        _ ->
          scan 0
  where
    !leftCount = keyColumnCount left
    !rightCount = keyColumnCount right

    scan !index
      | index == leftCount =
          Nothing
      | compare (keyColumnAt left index) (keyColumnAt right index) == EQ =
          scan (index + 1)
      | otherwise =
          Just index
{-# INLINABLE firstPrefixKeyMismatch #-}

firstEndpointMismatch :: Eq value => Int -> EndpointColumn value -> EndpointColumn value -> Maybe Int
firstEndpointMismatch count left right =
  case (columnView count left, columnView count right) of
    (leftView, rightView) ->
      scan leftView rightView 0 0 0
  where
    scan !leftView !rightView !index !leftPackedIndex !rightPackedIndex
      | index == count =
          Nothing
      | endpointsEqualValue
          (columnEndpointFromView leftView index leftPackedIndex)
          (columnEndpointFromView rightView index rightPackedIndex) =
          scan
            leftView
            rightView
            (index + 1)
            (advancePackedIndex leftView index leftPackedIndex)
            (advancePackedIndex rightView index rightPackedIndex)
      | otherwise =
          Just index
{-# INLINABLE firstEndpointMismatch #-}


replaceRecordedPage :: Ord key => key -> key -> Page key value -> Map key (Page key value) -> Map key (Page key value)
replaceRecordedPage oldMaximum updatedMaximum updatedPage =
  Map.insert updatedMaximum updatedPage . Map.delete oldMaximum
{-# INLINE replaceRecordedPage #-}

replacePageEntryAfter :: (PatchKey key, PatchValue value) => key -> Endpoint value -> Int -> key -> Page key value -> (key, Page key value)
replacePageEntryAfter key after rowIndex maximumKey page =
  let !count = pageCount page
      (!updatedMaximum, !updatedPrefixKeys) = replacePageKeyAt key rowIndex maximumKey page
      !updatedAfterColumn = replaceColumnEndpointAt count rowIndex after (pageAfterColumn page)
   in ( updatedMaximum,
        page
          { pagePrefixKeys = updatedPrefixKeys,
            pageAfterColumn = updatedAfterColumn
          }
      )
{-# INLINABLE replacePageEntryAfter #-}

replacePageKeyAt :: PatchKey key => key -> Int -> key -> Page key value -> (key, KeyColumn key)
replacePageKeyAt key rowIndex maximumKey page =
  if rowIndex + 1 == pageCount page
    then (key, pagePrefixKeys page)
    else (maximumKey, rebuildPrefixKeys key rowIndex page)
{-# INLINE replacePageKeyAt #-}

rebuildPrefixKeys :: PatchKey key => key -> Int -> Page key value -> KeyColumn key
rebuildPrefixKeys replacement replacementIndex page =
  buildKeyColumn (smallArrayFromList (collect 0))
  where
    !count = pageCount page - 1

    collect !index
      | index == count =
          []
      | index == replacementIndex =
          replacement : collect (index + 1)
      | otherwise =
          keyColumnAt (pagePrefixKeys page) index : collect (index + 1)
{-# INLINE rebuildPrefixKeys #-}

replaceColumnEndpointAt :: PatchValue value => Int -> Int -> Endpoint value -> EndpointColumn value -> EndpointColumn value
replaceColumnEndpointAt count replacementIndex replacement column =
  columnFromEndpoints count (collect 0 0)
  where
    !view = columnView count column

    collect !index !packedIndex
      | index == count =
          []
      | index == replacementIndex =
          let !nextPackedIndex = advancePackedIndex view index packedIndex
           in replacement : collect (index + 1) nextPackedIndex
      | otherwise =
          let !endpoint = columnEndpointFromView view index packedIndex
              !nextPackedIndex = advancePackedIndex view index packedIndex
           in endpoint : collect (index + 1) nextPackedIndex
{-# INLINE replaceColumnEndpointAt #-}

invertPage :: Page key value -> Page key value
invertPage page =
  page
    { pageBeforeColumn = pageAfterColumn page,
      pageAfterColumn = pageBeforeColumn page
    }
{-# INLINE invertPage #-}

columnFromEndpoints :: forall value. PatchValue value => Int -> [Endpoint value] -> EndpointColumn value
columnFromEndpoints count endpoints =
  let (!mask, !valuesReversed) =
        List.foldl' collectEndpoint (0 :: Word64, []) (List.zip [0 :: Int ..] endpoints)
      !values = List.reverse valuesReversed
   in if mask == lowBits count
        then AllPresent (valueColumnFromList values)
        else Presence mask (valueColumnFromList values)
  where
    collectEndpoint :: (Word64, [value]) -> (Int, Endpoint value) -> (Word64, [value])
    collectEndpoint (!mask, !valuesReversed) (index, endpoint) =
      case endpoint of
        EndpointAbsent ->
          (mask, valuesReversed)
        EndpointPresent value ->
          (mask .|. bitAt index, value : valuesReversed)
{-# INLINE columnFromEndpoints #-}

valueColumnFromList :: PatchValue value => [value] -> ValueColumn value
valueColumnFromList values =
  case values of
    [] ->
      DenseValues emptySmallArray
    _ ->
      valueColumnFromArray (smallArrayFromList values)
{-# INLINABLE valueColumnFromList #-}


bitAt :: Int -> Word64
bitAt index =
  (1 :: Word64) `shiftL` index
{-# INLINE bitAt #-}

lowBits :: Int -> Word64
lowBits count
  | count <= 0 =
      0
  | count >= pageCapacity =
      complement 0
  | otherwise =
      bitAt count - 1
{-# INLINE lowBits #-}


columnView :: Int -> EndpointColumn value -> ColumnView value
columnView !count column =
  case column of
    AllPresent values ->
      ColumnView (lowBits count) values
    Presence mask values ->
      ColumnView (mask .&. lowBits count) values
{-# INLINE columnView #-}

columnMaybeAt :: ColumnView value -> Int -> Int -> Maybe value
columnMaybeAt (ColumnView mask values) !logicalIndex !packedIndex =
  if testBit mask logicalIndex
    then Just (valueColumnAt values packedIndex)
    else Nothing
{-# INLINE columnMaybeAt #-}

columnEndpointFromView :: ColumnView value -> Int -> Int -> Endpoint value
columnEndpointFromView view logicalIndex packedIndex =
  case columnMaybeAt view logicalIndex packedIndex of
    Nothing -> EndpointAbsent
    Just value -> EndpointPresent value
{-# INLINE columnEndpointFromView #-}


advancePackedIndex :: ColumnView value -> Int -> Int -> Int
advancePackedIndex (ColumnView mask _values) logicalIndex packedIndex =
  if testBit mask logicalIndex
    then packedIndex + 1
    else packedIndex
{-# INLINE advancePackedIndex #-}


endpointAtByScan :: EndpointColumn value -> Int -> Endpoint value
endpointAtByScan column target =
  case columnView (target + 1) column of
    view -> go view 0 0
  where
    go view !index !packedIndex
      | index == target =
          columnEndpointFromView view index packedIndex
      | otherwise =
          go view (index + 1) (advancePackedIndex view index packedIndex)
{-# INLINE endpointAtByScan #-}

endpointsEqualValue :: Eq value => Endpoint value -> Endpoint value -> Bool
endpointsEqualValue left right =
  case (left, right) of
    (EndpointAbsent, EndpointAbsent) ->
      True
    (EndpointPresent leftValue, EndpointPresent rightValue) ->
      leftValue == rightValue
    _ ->
      False
{-# INLINE endpointsEqualValue #-}

rowCellAt :: Page key value -> Int -> CellPatch value
rowCellAt page index =
  cellFromEndpointPair
    (endpointAtByScan (pageBeforeColumn page) index)
    (endpointAtByScan (pageAfterColumn page) index)
{-# INLINE rowCellAt #-}

pageMinimumKey :: key -> Page key value -> key
pageMinimumKey maximumKey page =
  pageKeyAt maximumKey page 0
{-# INLINE pageMinimumKey #-}

pageLookupIndex :: Ord key => key -> key -> Page key value -> Maybe Int
pageLookupIndex target maximumKey page =
  search 0 (pageCount page - 1)
  where
    search low high
      | low > high =
          Nothing
      | otherwise =
          let !middle = (low + high) `quot` 2
              !middleKey = pageKeyAt maximumKey page middle
           in case compare target middleKey of
                LT -> search low (middle - 1)
                GT -> search (middle + 1) high
                EQ -> Just middle
{-# INLINABLE pageLookupIndex #-}

pageForKey :: Ord key => key -> Map key (Page key value) -> Maybe (key, Page key value)
pageForKey key pages =
  Map.lookupGE key pages
{-# INLINE pageForKey #-}

pageForInsertion :: Ord key => key -> Map key (Page key value) -> Maybe (key, Page key value)
pageForInsertion key pages =
  case Map.lookupGE key pages of
    Just pageEntry -> Just pageEntry
    Nothing -> Map.lookupMax pages
{-# INLINE pageForInsertion #-}

minimumKey :: Patch key value -> Maybe key
minimumKey patch =
  case patch of
    SmallPatch cells
      | sizeofSmallArray cells == 0 ->
          Nothing
      | otherwise ->
          case indexSmallArray cells 0 of
            Cell key _cell ->
              Just key
    PagedPatch _count pages ->
      case Map.lookupMin pages of
        Nothing ->
          Nothing
        Just (maximumKey, page) ->
          Just (pageKeyAt maximumKey page 0)
{-# INLINE minimumKey #-}

maximumKey :: Patch key value -> Maybe key
maximumKey patch =
  case patch of
    SmallPatch cells
      | sizeofSmallArray cells == 0 ->
          Nothing
      | otherwise ->
          case indexSmallArray cells (sizeofSmallArray cells - 1) of
            Cell key _cell ->
              Just key
    PagedPatch _count pages ->
      fmap fst (Map.lookupMax pages)
{-# INLINE maximumKey #-}

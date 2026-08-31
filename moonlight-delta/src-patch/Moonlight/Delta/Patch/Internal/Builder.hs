{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Builder
  ( Builder,
    newBuilder,
    appendTransition,
    appendPageCopy,
    finishBuilder,
    toPaged,
    toPagedMap,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bits (setBit, testBit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray
  ( MutablePrimArray,
    newPrimArray,
    readPrimArray,
    writePrimArray,
  )
import Data.Primitive.SmallArray
  ( SmallArray,
    SmallMutableArray,
    copySmallMutableArray,
    emptySmallArray,
    indexSmallArray,
    newSmallArray,
    readSmallArray,
    sizeofSmallArray,
    unsafeFreezeSmallArray,
    writeSmallArray,
  )
import Data.STRef
  ( STRef,
    modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,
  )
import Data.Word (Word64)
import Moonlight.Delta.Patch.Internal.Cell
  ( cellAfterEndpoint,
    cellBeforeEndpoint,
  )
import Moonlight.Delta.Patch.Internal.Page
  ( ColumnView (..),
    columnView,
  )
import Moonlight.Delta.Patch.Internal.Types
  ( Endpoint (..),
    EndpointColumn (..),
    Cell (..),
    PatchKey,
    PatchValue,
    Patch (..),
    Page (..),
    buildKeyColumn,
    pageKeyAt,
    valueColumnAt,
    valueColumnFromArray,
  )
import Prelude

bufferCapacity :: Int
bufferCapacity = 96
{-# INLINE bufferCapacity #-}

sealedPrefixSize :: Int
sealedPrefixSize = 64
{-# INLINE sealedPrefixSize #-}

retainedSuffixSize :: Int
retainedSuffixSize = 32
{-# INLINE retainedSuffixSize #-}

data OpenArrays s key value = OpenArrays
  { openKeys :: !(SmallMutableArray s key),
    openBefore :: !(SmallMutableArray s (Endpoint value)),
    openAfter :: !(SmallMutableArray s (Endpoint value)),
    openCount :: !(MutablePrimArray s Int)
  }

data OpenBuffer s key value
  = OpenEmpty
  | OpenNonEmpty !(OpenArrays s key value)

data Builder s key value = Builder
  { totalCount :: !(MutablePrimArray s Int),
    openBuffer :: !(STRef s (OpenBuffer s key value)),
    pagesReverse :: !(STRef s [(key, Page key value)])
  }

newBuilder :: ST s (Builder s key value)
newBuilder = do
  totalCount <- newPrimArray 1
  writePrimArray totalCount 0 0
  openBuffer <- newSTRef OpenEmpty
  pagesReverse <- newSTRef []
  pure
    Builder
      { totalCount = totalCount,
        openBuffer = openBuffer,
        pagesReverse = pagesReverse
      }
{-# INLINE newBuilder #-}

appendTransition ::
  (PatchKey key, PatchValue value) =>
  Builder s key value ->
  key ->
  Endpoint value ->
  Endpoint value ->
  ST s ()
appendTransition builder@Builder {totalCount, openBuffer} !key !before !after = do
  readSTRef openBuffer >>= \case
    OpenEmpty -> do
      keys <- newSmallArray bufferCapacity key
      beforeValues <- newSmallArray bufferCapacity before
      afterValues <- newSmallArray bufferCapacity after
      count <- newPrimArray 1
      writePrimArray count 0 1
      writeSTRef
        openBuffer
        ( OpenNonEmpty
            OpenArrays
              { openKeys = keys,
                openBefore = beforeValues,
                openAfter = afterValues,
                openCount = count
              }
        )
    OpenNonEmpty arrays -> do
      count <- readPrimArray (openCount arrays) 0
      nextIndex <-
        if count == bufferCapacity
          then do
            sealPrefixAndRetainSuffix builder arrays
            pure retainedSuffixSize
          else pure count
      writeSmallArray (openKeys arrays) nextIndex key
      writeSmallArray (openBefore arrays) nextIndex before
      writeSmallArray (openAfter arrays) nextIndex after
      writePrimArray (openCount arrays) 0 (nextIndex + 1)
  total <- readPrimArray totalCount 0
  writePrimArray totalCount 0 (total + 1)
{-# INLINE appendTransition #-}

sealPrefixAndRetainSuffix :: (PatchKey key, PatchValue value) => Builder s key value -> OpenArrays s key value -> ST s ()
sealPrefixAndRetainSuffix builder arrays = do
  commitRange builder arrays 0 sealedPrefixSize
  shiftRetainedSuffix arrays
  writePrimArray (openCount arrays) 0 retainedSuffixSize
{-# INLINE sealPrefixAndRetainSuffix #-}

shiftRetainedSuffix :: OpenArrays s key value -> ST s ()
shiftRetainedSuffix OpenArrays {openKeys, openBefore, openAfter} =
  go 0
  where
    go !index
      | index == retainedSuffixSize =
          pure ()
      | otherwise = do
          let !sourceIndex = sealedPrefixSize + index
          key <- readSmallArray openKeys sourceIndex
          before <- readSmallArray openBefore sourceIndex
          after <- readSmallArray openAfter sourceIndex
          writeSmallArray openKeys index key
          writeSmallArray openBefore index before
          writeSmallArray openAfter index after
          go (index + 1)
{-# INLINE shiftRetainedSuffix #-}

commitRange :: (PatchKey key, PatchValue value) => Builder s key value -> OpenArrays s key value -> Int -> Int -> ST s ()
commitRange Builder {pagesReverse} OpenArrays {openKeys, openBefore, openAfter} !start !rangeLength =
  if rangeLength <= 0
    then pure ()
    else do
      let !maximumIndex = start + rangeLength - 1
          !prefixLength = rangeLength - 1
      maximumKey <- readSmallArray openKeys maximumIndex
      prefixKeys <- freezeSlice openKeys start prefixLength
      beforeColumn <- compressColumn openBefore start rangeLength
      afterColumn <- compressColumn openAfter start rangeLength
      let !page =
            Page
              { pageCount = rangeLength,
                pagePrefixKeys = buildKeyColumn prefixKeys,
                pageBeforeColumn = beforeColumn,
                pageAfterColumn = afterColumn
              }
      modifySTRef' pagesReverse ((maximumKey, page) :)
{-# INLINE commitRange #-}

freezeSlice :: SmallMutableArray s value -> Int -> Int -> ST s (SmallArray value)
freezeSlice _ _ 0 =
  pure emptySmallArray
freezeSlice source !start !rangeLength = do
  seed <- readSmallArray source start
  target <- newSmallArray rangeLength seed
  copySmallMutableArray target 0 source start rangeLength
  unsafeFreezeSmallArray target
{-# INLINE freezeSlice #-}

compressColumn :: forall s value. PatchValue value => SmallMutableArray s (Endpoint value) -> Int -> Int -> ST s (EndpointColumn value)
compressColumn source !start !rangeLength = do
  (mask, presentCount, firstPresent) <- scan 0 0 0 Nothing
  values <-
    case firstPresent of
      Nothing ->
        pure emptySmallArray
      Just seed -> do
        target <- newSmallArray presentCount seed
        fill target 0 0
        unsafeFreezeSmallArray target
  if presentCount == rangeLength
    then pure (AllPresent (valueColumnFromArray values))
    else pure (Presence mask (valueColumnFromArray values))
  where
    scan :: Int -> Word64 -> Int -> Maybe value -> ST s (Word64, Int, Maybe value)
    scan !logicalIndex !mask !presentCount !firstPresent
      | logicalIndex == rangeLength =
          pure (mask, presentCount, firstPresent)
      | otherwise = do
          endpoint <- readSmallArray source (start + logicalIndex)
          case endpoint of
            EndpointAbsent ->
              scan (logicalIndex + 1) mask presentCount firstPresent
            EndpointPresent value ->
              scan
                (logicalIndex + 1)
                (setBit mask logicalIndex)
                (presentCount + 1)
                (case firstPresent of Nothing -> Just value; Just existing -> Just existing)

    fill :: SmallMutableArray s value -> Int -> Int -> ST s ()
    fill target !logicalIndex !packedIndex
      | logicalIndex == rangeLength =
          pure ()
      | otherwise = do
          endpoint <- readSmallArray source (start + logicalIndex)
          case endpoint of
            EndpointAbsent ->
              fill target (logicalIndex + 1) packedIndex
            EndpointPresent value -> do
              writeSmallArray target packedIndex value
              fill target (logicalIndex + 1) (packedIndex + 1)
{-# INLINE compressColumn #-}

appendPageCopy :: (PatchKey key, PatchValue value) => Builder s key value -> key -> Page key value -> ST s ()
appendPageCopy builder maximumKey page =
  case
      ( columnView (pageCount page) (pageBeforeColumn page),
        columnView (pageCount page) (pageAfterColumn page)
      )
    of
      (ColumnView beforeMask beforeValues, ColumnView afterMask afterValues) ->
        go 0 0 0 beforeMask beforeValues afterMask afterValues
  where
    go !logicalIndex !beforePackedIndex !afterPackedIndex !beforeMask !beforeValues !afterMask !afterValues
      | logicalIndex == pageCount page =
          pure ()
      | otherwise = do
          let !key = pageKeyAt maximumKey page logicalIndex
              !beforePresent = testBit beforeMask logicalIndex
              !afterPresent = testBit afterMask logicalIndex
              !before =
                if beforePresent
                  then EndpointPresent (valueColumnAt beforeValues beforePackedIndex)
                  else EndpointAbsent
              !after =
                if afterPresent
                  then EndpointPresent (valueColumnAt afterValues afterPackedIndex)
                  else EndpointAbsent
              !nextBeforePackedIndex =
                if beforePresent
                  then beforePackedIndex + 1
                  else beforePackedIndex
              !nextAfterPackedIndex =
                if afterPresent
                  then afterPackedIndex + 1
                  else afterPackedIndex
          appendTransition builder key before after
          go
            (logicalIndex + 1)
            nextBeforePackedIndex
            nextAfterPackedIndex
            beforeMask
            beforeValues
            afterMask
            afterValues
{-# INLINE appendPageCopy #-}

finishBuilderPages ::
  (PatchKey key, PatchValue value) =>
  Builder s key value ->
  ST s (Int, Map key (Page key value))
finishBuilderPages builder@Builder {totalCount, openBuffer, pagesReverse} = do
  readSTRef openBuffer >>= \case
    OpenEmpty ->
      pure ()
    OpenNonEmpty arrays -> do
      count <- readPrimArray (openCount arrays) 0
      if count <= sealedPrefixSize
        then commitRange builder arrays 0 count
        else do
          let !leftCount = count - retainedSuffixSize
          commitRange builder arrays 0 leftCount
          commitRange builder arrays leftCount retainedSuffixSize
  totalCount <- readPrimArray totalCount 0
  pagesReverse <- readSTRef pagesReverse
  pure (totalCount, Map.fromDistinctAscList (reverse pagesReverse))
{-# INLINE finishBuilderPages #-}

finishBuilder :: (PatchKey key, PatchValue value) => Builder s key value -> ST s (Patch key value)
finishBuilder builder = do
  (totalCount, pages) <- finishBuilderPages builder
  pure (PagedPatch totalCount pages)
{-# INLINE finishBuilder #-}

promoteCells ::
  (PatchKey key, PatchValue value) =>
  SmallArray (Cell key value) ->
  (Int, Map key (Page key value))
promoteCells cells =
  runST $ do
    builder <- newBuilder
    appendSmallCells builder cells 0 (sizeofSmallArray cells)
    finishBuilderPages builder
{-# INLINABLE promoteCells #-}

appendSmallCells ::
  (PatchKey key, PatchValue value) =>
  Builder s key value ->
  SmallArray (Cell key value) ->
  Int ->
  Int ->
  ST s ()
appendSmallCells builder cells !index !count
  | index == count =
      pure ()
  | otherwise =
      case indexSmallArray cells index of
        Cell key cell -> do
          appendTransition builder key (cellBeforeEndpoint cell) (cellAfterEndpoint cell)
          appendSmallCells builder cells (index + 1) count
{-# INLINABLE appendSmallCells #-}

toPaged :: (PatchKey key, PatchValue value) => Patch key value -> Patch key value
toPaged patch =
  case patch of
    PagedPatch _count _pages ->
      patch
    SmallPatch cells ->
      let (!count, !pages) = promoteCells cells
       in PagedPatch count pages
{-# INLINABLE toPaged #-}

toPagedMap :: (PatchKey key, PatchValue value) => Patch key value -> Map key (Page key value)
toPagedMap patch =
  case patch of
    PagedPatch _count pages ->
      pages
    SmallPatch cells ->
      snd (promoteCells cells)
{-# INLINABLE toPagedMap #-}

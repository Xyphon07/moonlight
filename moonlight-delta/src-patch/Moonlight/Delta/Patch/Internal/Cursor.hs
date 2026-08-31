{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Cursor
  ( AscCursor (..),
    ascCursor,
    ascAdvance,
    Cursor (CursorEnd),
    cursor,
    fromAsc,
    advanceRow,
    advancePage,
    cursorPageStart,
    currentKey,
    currentRow,
    beforeMaybe,
    afterMaybe,
  )
where

import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict (Map)
import Moonlight.Delta.Patch.Internal.Page
  ( ColumnView,
    advancePackedIndex,
    columnEndpointFromView,
    columnMaybeAt,
    columnView,
  )
import Moonlight.Delta.Patch.Internal.Types
  ( Endpoint,
    Page (..),
    pageKeyAt,
  )
import Prelude

data AscStack key value
  = AscStackEnd
  | AscStackNode !key value !(Map key value) !(AscStack key value)

data AscCursor key value
  = AscEnd
  | AscCursor !key value !(Map key value) !(AscStack key value)

ascCursor :: forall key value. Map key value -> AscCursor key value
ascCursor tree =
  descend tree AscStackEnd
  where
    descend :: Map key value -> AscStack key value -> AscCursor key value
    descend entries stack =
      case entries of
        MapInternal.Tip ->
          popStack stack
        MapInternal.Bin _ key value left right ->
          descend left (AscStackNode key value right stack)

    popStack :: AscStack key value -> AscCursor key value
    popStack stack =
      case stack of
        AscStackEnd ->
          AscEnd
        AscStackNode key value right rest ->
          AscCursor key value right rest
{-# INLINE ascCursor #-}

ascAdvance :: forall key value. AscCursor key value -> AscCursor key value
ascAdvance cursor =
  case cursor of
    AscEnd ->
      AscEnd
    AscCursor _key _value right stack ->
      descend right stack
  where
    descend :: Map key value -> AscStack key value -> AscCursor key value
    descend entries remaining =
      case entries of
        MapInternal.Tip ->
          popStack remaining
        MapInternal.Bin _ key value left nextRight ->
          descend left (AscStackNode key value nextRight remaining)

    popStack :: AscStack key value -> AscCursor key value
    popStack remaining =
      case remaining of
        AscStackEnd ->
          AscEnd
        AscStackNode key value nextRight rest ->
          AscCursor key value nextRight rest
{-# INLINE ascAdvance #-}

data PatchEndpointSide
  = PatchBefore
  | PatchAfter

data ColumnCursor (side :: PatchEndpointSide) value = ColumnCursor
  { columnCursorPackedIndex :: {-# UNPACK #-} !Int,
    columnCursorView :: {-# UNPACK #-} !(ColumnView value)
  }

data ActiveCursor key value = ActiveCursor
  { activeCursorMaximumKey :: !key,
    activeCursorPage :: !(Page key value),
    activeCursorLogicalIndex :: {-# UNPACK #-} !Int,
    activeCursorBefore :: {-# UNPACK #-} !(ColumnCursor 'PatchBefore value),
    activeCursorAfter :: {-# UNPACK #-} !(ColumnCursor 'PatchAfter value),
    activeCursorRemainingPages :: !(AscCursor key (Page key value))
  }

data Cursor key value
  = CursorEnd
  | CursorActive {-# UNPACK #-} !(ActiveCursor key value)

cursor :: Map key (Page key value) -> Cursor key value
cursor =
  fromAsc . ascCursor
{-# INLINE cursor #-}

fromAsc :: AscCursor key (Page key value) -> Cursor key value
fromAsc cursor =
  case cursor of
    AscEnd ->
      CursorEnd
    AscCursor maximumKey page _right _stack ->
      CursorActive
        ActiveCursor
          { activeCursorMaximumKey = maximumKey,
            activeCursorPage = page,
            activeCursorLogicalIndex = 0,
            activeCursorBefore =
              ColumnCursor
                { columnCursorPackedIndex = 0,
                  columnCursorView = columnView (pageCount page) (pageBeforeColumn page)
                },
            activeCursorAfter =
              ColumnCursor
                { columnCursorPackedIndex = 0,
                  columnCursorView = columnView (pageCount page) (pageAfterColumn page)
                },
            activeCursorRemainingPages = ascAdvance cursor
          }
{-# INLINE fromAsc #-}

advancePage :: Cursor key value -> Cursor key value
advancePage cursor =
  case cursor of
    CursorEnd ->
      CursorEnd
    CursorActive activeCursor ->
      fromAsc (activeCursorRemainingPages activeCursor)
{-# INLINE advancePage #-}

advanceRow :: Cursor key value -> Cursor key value
advanceRow cursor =
  case cursor of
    CursorEnd ->
      CursorEnd
    CursorActive activeCursor ->
      let !logicalIndex = activeCursorLogicalIndex activeCursor
          !nextLogicalIndex = logicalIndex + 1
       in if nextLogicalIndex == pageCount (activeCursorPage activeCursor)
            then fromAsc (activeCursorRemainingPages activeCursor)
            else
              CursorActive
                activeCursor
                  { activeCursorLogicalIndex = nextLogicalIndex,
                    activeCursorBefore = advanceColumnCursor logicalIndex (activeCursorBefore activeCursor),
                    activeCursorAfter = advanceColumnCursor logicalIndex (activeCursorAfter activeCursor)
                  }
{-# INLINE advanceRow #-}

advanceColumnCursor :: Int -> ColumnCursor side value -> ColumnCursor side value
advanceColumnCursor logicalIndex columnCursor =
  columnCursor
    { columnCursorPackedIndex =
      advancePackedIndex
        (columnCursorView columnCursor)
        logicalIndex
        (columnCursorPackedIndex columnCursor)
    }
{-# INLINE advanceColumnCursor #-}

cursorPageStart :: Cursor key value -> Maybe (key, Page key value)
cursorPageStart cursor =
  case cursor of
    CursorEnd ->
      Nothing
    CursorActive activeCursor
      | activeCursorLogicalIndex activeCursor == 0 ->
          Just (activeCursorMaximumKey activeCursor, activeCursorPage activeCursor)
      | otherwise ->
          Nothing
{-# INLINE cursorPageStart #-}

currentKey :: Cursor key value -> Maybe key
currentKey cursor =
  case cursor of
    CursorEnd ->
      Nothing
    CursorActive activeCursor ->
      Just
        ( pageKeyAt
            (activeCursorMaximumKey activeCursor)
            (activeCursorPage activeCursor)
            (activeCursorLogicalIndex activeCursor)
        )
{-# INLINE currentKey #-}

currentRow :: Cursor key value -> Maybe (key, Endpoint value, Endpoint value)
currentRow cursor =
  case cursor of
    CursorEnd ->
      Nothing
    CursorActive activeCursor ->
      let !logicalIndex = activeCursorLogicalIndex activeCursor
       in Just
            ( pageKeyAt
                (activeCursorMaximumKey activeCursor)
                (activeCursorPage activeCursor)
                logicalIndex,
              columnCursorEndpoint logicalIndex (activeCursorBefore activeCursor),
              columnCursorEndpoint logicalIndex (activeCursorAfter activeCursor)
            )
{-# INLINE currentRow #-}

columnCursorEndpoint :: Int -> ColumnCursor side value -> Endpoint value
columnCursorEndpoint logicalIndex columnCursor =
  columnEndpointFromView
    (columnCursorView columnCursor)
    logicalIndex
    (columnCursorPackedIndex columnCursor)
{-# INLINE columnCursorEndpoint #-}

beforeMaybe :: Cursor key value -> Maybe value
beforeMaybe =
  cursorColumnValue activeCursorBefore
{-# INLINE beforeMaybe #-}

afterMaybe :: Cursor key value -> Maybe value
afterMaybe =
  cursorColumnValue activeCursorAfter
{-# INLINE afterMaybe #-}

cursorColumnValue :: (ActiveCursor key value -> ColumnCursor side value) -> Cursor key value -> Maybe value
cursorColumnValue selectColumn cursor =
  case cursor of
    CursorEnd ->
      Nothing
    CursorActive activeCursor ->
      columnCursorValue
        (activeCursorLogicalIndex activeCursor)
        (selectColumn activeCursor)
{-# INLINE cursorColumnValue #-}

columnCursorValue :: Int -> ColumnCursor side value -> Maybe value
columnCursorValue logicalIndex columnCursor =
  columnMaybeAt
    (columnCursorView columnCursor)
    logicalIndex
    (columnCursorPackedIndex columnCursor)
{-# INLINE columnCursorValue #-}

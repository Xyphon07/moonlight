{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Construction
  ( empty,
    singleton,
    fromList,
    fromAscList,
    toAscList,
    lookup,
    mapMaybeWithKey,
    foldWithKey,
    foldWithKey',
    traverseWithKey,
    invert,
    diff,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bits (testBit)
import Data.Foldable (traverse_)
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray
  ( SmallArray,
    emptySmallArray,
    indexSmallArray,
    mapSmallArray',
    sizeofSmallArray,
    smallArrayFromList,
  )
import Moonlight.Delta.Patch.Internal.Builder
import Moonlight.Delta.Patch.Internal.Cell
import Moonlight.Delta.Patch.Internal.Cursor
import Moonlight.Delta.Patch.Internal.Page
import Moonlight.Delta.Patch.Internal.Types
import Prelude hiding (lookup)

empty :: Patch key value
empty =
  SmallPatch emptySmallArray
{-# INLINE empty #-}

singleton :: key -> CellPatch value -> Patch key value
singleton key cell =
  SmallPatch (smallArrayFromList [Cell key cell])
{-# INLINE singleton #-}

fromList :: (PatchKey key, PatchValue value) => [(key, CellPatch value)] -> Patch key value
fromList =
  fromMapEntries . Map.fromList
{-# INLINABLE fromList #-}

fromAscList :: (PatchKey key, PatchValue value) => [(key, CellPatch value)] -> Patch key value
fromAscList entries =
  case consolidateAscending entries of
    Nothing ->
      fromList entries
    Just distinctEntries ->
      fromDistinctAscList distinctEntries
{-# INLINABLE fromAscList #-}

smallFromDistinctAscList :: [(key, CellPatch value)] -> Patch key value
smallFromDistinctAscList entries =
  SmallPatch (smallArrayFromList (fmap (uncurry Cell) entries))
{-# INLINE smallFromDistinctAscList #-}

entriesShorterThan :: Int -> [a] -> Bool
entriesShorterThan limit =
  go limit
  where
    go :: Int -> [entry] -> Bool
    go remaining entries
      | remaining <= 0 =
          False
      | otherwise =
          case entries of
            [] ->
              True
            _ : rest ->
              go (remaining - 1) rest
{-# INLINE entriesShorterThan #-}

consolidateAscending :: forall key value. Ord key => [(key, value)] -> Maybe [(key, value)]
consolidateAscending entries =
  case entries of
    [] ->
      Just []
    firstEntry : remainingEntries ->
      reverse <$> collect firstEntry remainingEntries []
  where
    collect :: (key, value) -> [(key, value)] -> [(key, value)] -> Maybe [(key, value)]
    collect currentEntry [] accumulated =
      Just (currentEntry : accumulated)
    collect currentEntry@(key, _value) (nextEntry@(nextKey, _nextValue) : remainingEntries) accumulated =
      case compare key nextKey of
        LT ->
          collect nextEntry remainingEntries (currentEntry : accumulated)
        EQ ->
          collect nextEntry remainingEntries accumulated
        GT ->
          Nothing
{-# INLINABLE consolidateAscending #-}

fromDistinctAscList :: (PatchKey key, PatchValue value) => [(key, CellPatch value)] -> Patch key value
fromDistinctAscList entries =
  if entriesShorterThan smallFormThreshold entries
    then smallFromDistinctAscList entries
    else runST $ do
      builder <- newBuilder
      traverse_ (uncurry (appendCell builder)) entries
      finishBuilder builder
{-# INLINABLE fromDistinctAscList #-}

fromMapEntries :: (PatchKey key, PatchValue value) => Map key (CellPatch value) -> Patch key value
fromMapEntries entries
  | Map.size entries < smallFormThreshold =
      smallFromDistinctAscList (Map.toAscList entries)
  | otherwise =
      runST $ do
        builder <- newBuilder
        appendMapCells builder entries
        finishBuilder builder
{-# INLINABLE fromMapEntries #-}

appendMapCells :: (PatchKey key, PatchValue value) => Builder s key value -> Map key (CellPatch value) -> ST s ()
appendMapCells builder entries =
  case entries of
    MapInternal.Tip ->
      pure ()
    MapInternal.Bin _treeSize key cell left right -> do
      appendMapCells builder left
      appendCell builder key cell
      appendMapCells builder right
{-# INLINABLE appendMapCells #-}

appendCell :: (PatchKey key, PatchValue value) => Builder s key value -> key -> CellPatch value -> ST s ()
appendCell builder key cell =
  appendTransition builder key (cellBeforeEndpoint cell) (cellAfterEndpoint cell)
{-# INLINE appendCell #-}

toAscList :: Patch key value -> [(key, CellPatch value)]
toAscList =
  foldWithKey
    (\key rest -> (key, AssertAbsent) : rest)
    (\key after rest -> (key, Insert after) : rest)
    (\key before rest -> (key, Delete before) : rest)
    (\key before after rest -> (key, Replace before after) : rest)
    []
{-# INLINE toAscList #-}

lookup :: Ord key => key -> Patch key value -> Maybe (CellPatch value)
lookup key patch =
  case patch of
    SmallPatch cells ->
      lookupSmall key cells 0 (sizeofSmallArray cells)
    PagedPatch _count pages ->
      case pageForKey key pages of
        Nothing ->
          Nothing
        Just (maximumKey, page) ->
          fmap (rowCellAt page) (pageLookupIndex key maximumKey page)
{-# INLINABLE lookup #-}

lookupSmall :: Ord key => key -> SmallArray (Cell key value) -> Int -> Int -> Maybe (CellPatch value)
lookupSmall key cells !index !count
  | index == count =
      Nothing
  | otherwise =
      case indexSmallArray cells index of
        Cell currentKey cell ->
          case compare key currentKey of
            LT ->
              Nothing
            EQ ->
              Just cell
            GT ->
              lookupSmall key cells (index + 1) count
{-# INLINABLE lookupSmall #-}

mapMaybeWithKey :: (key -> CellPatch value -> Maybe value') -> Patch key value -> Map key value'
mapMaybeWithKey project patch =
  Map.fromDistinctAscList
    ( foldWithKey
        (\key rest -> emit key AssertAbsent rest)
        (\key after rest -> emit key (Insert after) rest)
        (\key before rest -> emit key (Delete before) rest)
        (\key before after rest -> emit key (Replace before after) rest)
        []
        patch
    )
  where
    emit key cell rest =
      case project key cell of
        Nothing -> rest
        Just !projected -> (key, projected) : rest
{-# INLINE mapMaybeWithKey #-}

foldPageRight ::
  (key -> result -> result) ->
  (key -> value -> result -> result) ->
  (key -> value -> result -> result) ->
  (key -> value -> value -> result -> result) ->
  key ->
  Page key value ->
  result ->
  result
foldPageRight onAssertAbsent onInsert onDelete onReplace maximumKey page initial =
  case (columnView count beforeColumn, columnView count afterColumn) of
    (ColumnView beforeMask beforeValues, ColumnView afterMask afterValues) ->
      go beforeMask beforeValues afterMask afterValues 0 0 0
  where
    !count = pageCount page
    !beforeColumn = pageBeforeColumn page
    !afterColumn = pageAfterColumn page

    go !beforeMask !beforeValues !afterMask !afterValues !index !beforePacked !afterPacked
      | index == count =
          initial
      | otherwise =
          let !key = pageKeyAt maximumKey page index
              !beforePresent = testBit beforeMask index
              !afterPresent = testBit afterMask index
              !nextBeforePacked = if beforePresent then beforePacked + 1 else beforePacked
              !nextAfterPacked = if afterPresent then afterPacked + 1 else afterPacked
              rest = go beforeMask beforeValues afterMask afterValues (index + 1) nextBeforePacked nextAfterPacked
           in case (beforePresent, afterPresent) of
                (False, False) -> onAssertAbsent key rest
                (False, True) -> onInsert key (valueColumnAt afterValues afterPacked) rest
                (True, False) -> onDelete key (valueColumnAt beforeValues beforePacked) rest
                (True, True) -> onReplace key (valueColumnAt beforeValues beforePacked) (valueColumnAt afterValues afterPacked) rest
{-# INLINE foldPageRight #-}

foldPageLeftStrict ::
  (result -> key -> result) ->
  (result -> key -> value -> result) ->
  (result -> key -> value -> result) ->
  (result -> key -> value -> value -> result) ->
  result ->
  key ->
  Page key value ->
  result
foldPageLeftStrict onAssertAbsent onInsert onDelete onReplace initial maximumKey page =
  case (columnView count beforeColumn, columnView count afterColumn) of
    (ColumnView beforeMask beforeValues, ColumnView afterMask afterValues) ->
      go beforeMask beforeValues afterMask afterValues 0 0 0 initial
  where
    !count = pageCount page
    !beforeColumn = pageBeforeColumn page
    !afterColumn = pageAfterColumn page

    go !beforeMask !beforeValues !afterMask !afterValues !index !beforePacked !afterPacked !accumulator
      | index == count =
          accumulator
      | otherwise =
          let !key = pageKeyAt maximumKey page index
              !beforePresent = testBit beforeMask index
              !afterPresent = testBit afterMask index
              !nextAccumulator =
                case (beforePresent, afterPresent) of
                  (False, False) -> onAssertAbsent accumulator key
                  (False, True) -> onInsert accumulator key (valueColumnAt afterValues afterPacked)
                  (True, False) -> onDelete accumulator key (valueColumnAt beforeValues beforePacked)
                  (True, True) -> onReplace accumulator key (valueColumnAt beforeValues beforePacked) (valueColumnAt afterValues afterPacked)
              !nextBeforePacked = if beforePresent then beforePacked + 1 else beforePacked
              !nextAfterPacked = if afterPresent then afterPacked + 1 else afterPacked
           in go beforeMask beforeValues afterMask afterValues (index + 1) nextBeforePacked nextAfterPacked nextAccumulator
{-# INLINE foldPageLeftStrict #-}

foldWithKey ::
  (key -> result -> result) ->
  (key -> value -> result -> result) ->
  (key -> value -> result -> result) ->
  (key -> value -> value -> result -> result) ->
  result ->
  Patch key value ->
  result
foldWithKey onAssertAbsent onInsert onDelete onReplace initial patch =
  case patch of
    SmallPatch cells ->
      foldSmallRight onAssertAbsent onInsert onDelete onReplace initial cells
    PagedPatch _count pages ->
      Map.foldrWithKey (foldPageRight onAssertAbsent onInsert onDelete onReplace) initial pages
{-# INLINE foldWithKey #-}

foldSmallRight ::
  (key -> result -> result) ->
  (key -> value -> result -> result) ->
  (key -> value -> result -> result) ->
  (key -> value -> value -> result -> result) ->
  result ->
  SmallArray (Cell key value) ->
  result
foldSmallRight onAssertAbsent onInsert onDelete onReplace initial cells =
  go 0
  where
    !count = sizeofSmallArray cells
    go !index
      | index == count =
          initial
      | otherwise =
          case indexSmallArray cells index of
            Cell key cell ->
              let rest = go (index + 1)
               in case cell of
                    AssertAbsent ->
                      onAssertAbsent key rest
                    Insert after ->
                      onInsert key after rest
                    Delete before ->
                      onDelete key before rest
                    Replace before after ->
                      onReplace key before after rest
{-# INLINE foldSmallRight #-}

foldWithKey' ::
  (result -> key -> result) ->
  (result -> key -> value -> result) ->
  (result -> key -> value -> result) ->
  (result -> key -> value -> value -> result) ->
  result ->
  Patch key value ->
  result
foldWithKey' onAssertAbsent onInsert onDelete onReplace initial patch =
  case patch of
    SmallPatch cells ->
      foldSmallLeftStrict onAssertAbsent onInsert onDelete onReplace initial cells
    PagedPatch _count pages ->
      Map.foldlWithKey' (foldPageLeftStrict onAssertAbsent onInsert onDelete onReplace) initial pages
{-# INLINE foldWithKey' #-}

foldSmallLeftStrict ::
  (result -> key -> result) ->
  (result -> key -> value -> result) ->
  (result -> key -> value -> result) ->
  (result -> key -> value -> value -> result) ->
  result ->
  SmallArray (Cell key value) ->
  result
foldSmallLeftStrict onAssertAbsent onInsert onDelete onReplace initial cells =
  go 0 initial
  where
    !count = sizeofSmallArray cells
    go !index !accumulator
      | index == count =
          accumulator
      | otherwise =
          case indexSmallArray cells index of
            Cell key cell ->
              let !nextAccumulator =
                    case cell of
                      AssertAbsent ->
                        onAssertAbsent accumulator key
                      Insert after ->
                        onInsert accumulator key after
                      Delete before ->
                        onDelete accumulator key before
                      Replace before after ->
                        onReplace accumulator key before after
               in go (index + 1) nextAccumulator
{-# INLINE foldSmallLeftStrict #-}

traverseWithKey ::
  (Applicative effect, PatchKey key, PatchValue value') =>
  (key -> CellPatch value -> effect (CellPatch value')) ->
  Patch key value ->
  effect (Patch key value')
traverseWithKey step patch =
  fromAscList <$> traverse traverseCell (toAscList patch)
  where
    traverseCell (key, cell) =
      fmap (\nextCell -> (key, nextCell)) (step key cell)
{-# INLINE traverseWithKey #-}

invert :: Patch key value -> Patch key value
invert patch =
  case patch of
    SmallPatch cells ->
      SmallPatch (mapSmallArray' invertCell cells)
    PagedPatch count pages ->
      PagedPatch count (Map.map invertPage pages)
{-# INLINE invert #-}

invertCell :: Cell key value -> Cell key value
invertCell (Cell key cell) =
  Cell key (invertCellPatch cell)
{-# INLINE invertCell #-}

invertCellPatch :: CellPatch value -> CellPatch value
invertCellPatch cell =
  case cell of
    AssertAbsent ->
      AssertAbsent
    Insert after ->
      Delete after
    Delete before ->
      Insert before
    Replace before after ->
      Replace after before
{-# INLINE invertCellPatch #-}

diff :: forall key value. (PatchKey key, PatchValue value) => Map key value -> Map key value -> Patch key value
diff before after =
  normalize
    ( runST $ do
        builder <- newBuilder
        appendDiff builder (ascCursor before) (ascCursor after)
        finishBuilder builder
    )
  where
    appendDiff :: Builder s key value -> AscCursor key value -> AscCursor key value -> ST s ()
    appendDiff builder beforeCursor afterCursor =
      case (beforeCursor, afterCursor) of
        (AscEnd, AscEnd) ->
          pure ()
        (AscCursor beforeKey beforeValue _ _, AscEnd) -> do
          appendTransition builder beforeKey (EndpointPresent beforeValue) EndpointAbsent
          appendDiff builder (ascAdvance beforeCursor) AscEnd
        (AscEnd, AscCursor afterKey afterValue _ _) -> do
          appendTransition builder afterKey EndpointAbsent (EndpointPresent afterValue)
          appendDiff builder AscEnd (ascAdvance afterCursor)
        (AscCursor beforeKey beforeValue _ _, AscCursor afterKey afterValue _ _) ->
          case compare beforeKey afterKey of
            LT -> do
              appendTransition builder beforeKey (EndpointPresent beforeValue) EndpointAbsent
              appendDiff builder (ascAdvance beforeCursor) afterCursor
            GT -> do
              appendTransition builder afterKey EndpointAbsent (EndpointPresent afterValue)
              appendDiff builder beforeCursor (ascAdvance afterCursor)
            EQ -> do
              if beforeValue == afterValue
                then pure ()
                else appendTransition builder afterKey (EndpointPresent beforeValue) (EndpointPresent afterValue)
              appendDiff builder (ascAdvance beforeCursor) (ascAdvance afterCursor)
{-# INLINABLE diff #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Compose.Range
  ( disjointComposition,
    splitPatchRange,
    appendPatch,
    appendPages,
    pageToAscCells,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch.Internal.Builder
import Moonlight.Delta.Patch.Internal.Cell
  ( cellAfterEndpoint,
    cellBeforeEndpoint,
  )
import Moonlight.Delta.Patch.Internal.Page
import Moonlight.Delta.Patch.Internal.Types
import Prelude

disjointComposition :: Ord key => Patch key value -> Patch key value -> Maybe (Patch key value)
disjointComposition
  (PagedPatch olderCount olderPages)
  (PagedPatch newerCount newerPages) =
    case (Map.lookupMax olderPages, Map.lookupMin newerPages) of
      (Just (olderMaximum, _), Just (newerMaximum, newerFirstPage))
        | olderMaximum < pageMinimumKey newerMaximum newerFirstPage ->
            Just (PagedPatch (olderCount + newerCount) (MapInternal.link2 olderPages newerPages))
      _ ->
        case (Map.lookupMax newerPages, Map.lookupMin olderPages) of
          (Just (newerMaximum, _), Just (olderMaximum, olderFirstPage))
            | newerMaximum < pageMinimumKey olderMaximum olderFirstPage ->
                Just (PagedPatch (olderCount + newerCount) (MapInternal.link2 newerPages olderPages))
          _ -> Nothing
disjointComposition _ _ =
  Nothing
{-# INLINABLE disjointComposition #-}

splitPatchRange ::
  (PatchKey key, PatchValue value) =>
  key ->
  key ->
  Patch key value ->
  (Patch key value, Patch key value, Patch key value)
splitPatchRange minimumKey maximumKey patch =
  let (!beforePages, !atOrAfterMinimum) =
        splitPagesLessThan minimumKey (pagesOf patch)
      (!middlePages, !afterPages) =
        splitPagesLessOrEqual maximumKey atOrAfterMinimum
   in (patchFromPages beforePages, patchFromPages middlePages, patchFromPages afterPages)
{-# INLINABLE splitPatchRange #-}

patchFromPages :: Map.Map key (Page key value) -> Patch key value
patchFromPages pages =
  PagedPatch (pageMapEntryCount pages) pages
{-# INLINE patchFromPages #-}

pageMapEntryCount :: Map.Map key (Page key value) -> Int
pageMapEntryCount =
  Map.foldlWithKey' (\count _ page -> count + pageCount page) 0
{-# INLINE pageMapEntryCount #-}

appendPatch :: Patch key value -> Patch key value -> Patch key value
appendPatch left right =
  PagedPatch
    (entryCount left + entryCount right)
    (appendPages (pagesOf left) (pagesOf right))
{-# INLINE appendPatch #-}

appendPages :: Map.Map key value -> Map.Map key value -> Map.Map key value
appendPages left right =
  case (left, right) of
    (MapInternal.Tip, _) ->
      right
    (_, MapInternal.Tip) ->
      left
    _ ->
      MapInternal.link2 left right
{-# INLINE appendPages #-}

splitPagesLessThan ::
  (PatchKey key, PatchValue value) =>
  key ->
  Map.Map key (Page key value) ->
  (Map.Map key (Page key value), Map.Map key (Page key value))
splitPagesLessThan target pages =
  case pageForKey target pages of
    Nothing ->
      (pages, Map.empty)
    Just (maximumKey, page) ->
      let (!beforePages, !_found, !afterPages) =
            Map.splitLookup maximumKey pages
          !minimumKey =
            pageMinimumKey maximumKey page
       in if target <= minimumKey
            then (beforePages, appendPages (Map.singleton maximumKey page) afterPages)
            else
              let !splitIndex =
                    pageLowerBound target maximumKey page
                  !beforeSlice =
                    pageSlicePages maximumKey page 0 splitIndex
                  !afterSlice =
                    pageSlicePages maximumKey page splitIndex (pageCount page - splitIndex)
               in ( appendPages beforePages beforeSlice,
                    appendPages afterSlice afterPages
                  )
{-# INLINABLE splitPagesLessThan #-}

splitPagesLessOrEqual ::
  (PatchKey key, PatchValue value) =>
  key ->
  Map.Map key (Page key value) ->
  (Map.Map key (Page key value), Map.Map key (Page key value))
splitPagesLessOrEqual target pages =
  case pageForKey target pages of
    Nothing ->
      (pages, Map.empty)
    Just (maximumKey, page) ->
      let (!beforePages, !_found, !afterPages) =
            Map.splitLookup maximumKey pages
          !minimumKey =
            pageMinimumKey maximumKey page
       in if target < minimumKey
            then (beforePages, appendPages (Map.singleton maximumKey page) afterPages)
            else
              if maximumKey <= target
                then (appendPages beforePages (Map.singleton maximumKey page), afterPages)
                else
                  let !splitIndex =
                        pageUpperBound target maximumKey page
                      !beforeSlice =
                        pageSlicePages maximumKey page 0 splitIndex
                      !afterSlice =
                        pageSlicePages maximumKey page splitIndex (pageCount page - splitIndex)
                   in ( appendPages beforePages beforeSlice,
                        appendPages afterSlice afterPages
                      )
{-# INLINABLE splitPagesLessOrEqual #-}


pageLowerBound :: Ord key => key -> key -> Page key value -> Int
pageLowerBound target maximumKey page =
  search 0 (pageCount page)
  where
    search !low !high
      | low == high =
          low
      | otherwise =
          let !middle = (low + high) `quot` 2
              !middleKey = pageKeyAt maximumKey page middle
           in case compare middleKey target of
                LT -> search (middle + 1) high
                EQ -> middle
                GT -> search low middle
{-# INLINABLE pageLowerBound #-}

pageUpperBound :: Ord key => key -> key -> Page key value -> Int
pageUpperBound target maximumKey page =
  search 0 (pageCount page)
  where
    search !low !high
      | low == high =
          low
      | otherwise =
          let !middle = (low + high) `quot` 2
              !middleKey = pageKeyAt maximumKey page middle
           in case compare middleKey target of
                GT -> search low middle
                _ -> search (middle + 1) high
{-# INLINABLE pageUpperBound #-}

pageSlicePages ::
  forall key value.
  (PatchKey key, PatchValue value) =>
  key ->
  Page key value ->
  Int ->
  Int ->
  Map.Map key (Page key value)
pageSlicePages maximumKey page !start !sliceLength
  | sliceLength <= 0 =
      Map.empty
  | start <= 0 && sliceLength == pageCount page =
      Map.singleton maximumKey page
  | otherwise =
      pagesOf $
        runST $ do
          builder <- newBuilder
          appendSliceRows builder 0
          finishBuilder builder
  where
    appendSliceRows :: Builder s key value -> Int -> ST s ()
    appendSliceRows builder !offset
      | offset == sliceLength =
          pure ()
      | otherwise = do
          let !rowIndex = start + offset
              !cell = rowCellAt page rowIndex
          appendTransition
            builder
            (pageKeyAt maximumKey page rowIndex)
            (cellBeforeEndpoint cell)
            (cellAfterEndpoint cell)
          appendSliceRows builder (offset + 1)
{-# INLINABLE pageSlicePages #-}

pageToAscCells :: key -> Page key value -> [(key, CellPatch value)]
pageToAscCells maximumKey page =
  fmap
    (\index -> (pageKeyAt maximumKey page index, rowCellAt page index))
    [0 .. pageCount page - 1]
{-# INLINE pageToAscCells #-}

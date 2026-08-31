{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Compose.Record
  ( recordApplied,
    recordMany,
  )
where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Moonlight.Delta.Patch.Internal.Builder
  ( toPagedMap,
  )
import Moonlight.Delta.Patch.Internal.Cell
  ( cellAfterEndpoint,
    cellBeforeEndpoint,
    cellFromEndpointPair,
    endpointToMaybe,
  )
import Moonlight.Delta.Patch.Internal.Compose.Range
import Moonlight.Delta.Patch.Internal.Construction
  ( fromAscList,
    singleton,
  )
import Moonlight.Delta.Patch.Internal.Page
import Moonlight.Delta.Patch.Internal.Types
import Prelude

data InsertCellResult key value = InsertCellResult !Bool ![(key, CellPatch value)]

recordApplied ::
  forall key value.
  (PatchKey key, PatchValue value) =>
  key ->
  CellPatch value ->
  Patch key value ->
  Either (ComposeError key value) (Patch key value)
recordApplied key latest patch =
  case patch of
    SmallPatch cells ->
      fmap fromAscList (insertOrReplaceSmall (smallCellsToAscList cells))
    PagedPatch entryCount pages ->
      case pageForInsertion key pages of
        Nothing ->
          Right (singleton key latest)
        Just (maximumKey, page) ->
          case pageLookupIndex key maximumKey page of
            Just rowIndex ->
              replaceRecordedRow entryCount pages maximumKey page rowIndex
            Nothing ->
              Right (insertRecordedRow entryCount pages maximumKey page)
  where
    !latestBefore = cellBeforeEndpoint latest
    !latestAfter = cellAfterEndpoint latest

    insertOrReplaceSmall [] =
      Right [(key, latest)]
    insertOrReplaceSmall (row@(rowKey, rowCell) : rest) =
      case compare key rowKey of
        LT ->
          Right ((key, latest) : row : rest)
        EQ ->
          let !olderAfter = cellAfterEndpoint rowCell
           in if olderAfter /= latestBefore
                then
                  Left
                    ComposeBoundaryMismatch
                      { boundaryKey = key,
                        olderAfter = endpointToMaybe olderAfter,
                        newerBefore = endpointToMaybe latestBefore
                      }
                else
                  let !combined = cellFromEndpointPair (cellBeforeEndpoint rowCell) latestAfter
                   in Right ((key, combined) : rest)
        GT ->
          fmap (row :) (insertOrReplaceSmall rest)

    replaceRecordedRow entryCount pages maximumKey page rowIndex =
      let !olderAfter = cellAfterEndpoint (rowCellAt page rowIndex)
       in if olderAfter /= latestBefore
            then
              Left
                ComposeBoundaryMismatch
                  { boundaryKey = key,
                    olderAfter = endpointToMaybe olderAfter,
                    newerBefore = endpointToMaybe latestBefore
                  }
            else
              let (!updatedMaximum, !updatedPage) =
                    replacePageEntryAfter key latestAfter rowIndex maximumKey page
               in Right
                    (PagedPatch entryCount (replaceRecordedPage maximumKey updatedMaximum updatedPage pages))

    insertRecordedRow entryCount pages maximumKey page =
      let InsertCellResult inserted localRows =
            insertCell (pageToAscCells maximumKey page)
          !localPatch =
            fromAscList localRows
          (!leftPages, !_removedPage, !rightPages) =
            Map.splitLookup maximumKey pages
       in PagedPatch
            ( entryCount
                + if inserted
                  then 1
                  else 0
            )
            (appendPages leftPages (appendPages (toPagedMap localPatch) rightPages))

    insertCell rows =
      case rows of
        [] ->
          InsertCellResult True [(key, latest)]
        row@(rowKey, _rowCell) : rest ->
          case compare key rowKey of
            LT ->
              InsertCellResult True ((key, latest) : rows)
            EQ ->
              InsertCellResult False ((key, latest) : rest)
            GT ->
              let InsertCellResult inserted insertedRows = insertCell rest
               in InsertCellResult inserted (row : insertedRows)
{-# INLINABLE recordApplied #-}

recordMany ::
  forall edits key value.
  (Foldable edits, PatchKey key, PatchValue value) =>
  edits (key, CellPatch value) ->
  Either (ComposeError key value) (Patch key value)
recordMany edits =
  fmap temporalRowsToPatch (Foldable.foldlM recordOne Map.empty edits)
  where
    recordOne ::
      Map.Map key (Endpoint value, Endpoint value) ->
      (key, CellPatch value) ->
      Either (ComposeError key value) (Map.Map key (Endpoint value, Endpoint value))
    recordOne accumulated (key, patch) =
      let !before = cellBeforeEndpoint patch
          !after = cellAfterEndpoint patch
       in case Map.lookup key accumulated of
            Nothing ->
              Right (Map.insert key (before, after) accumulated)
            Just (initial, current)
              | current == before ->
                  Right (Map.insert key (initial, after) accumulated)
              | otherwise ->
                  Left
                    ComposeBoundaryMismatch
                      { boundaryKey = key,
                        olderAfter = endpointToMaybe current,
                        newerBefore = endpointToMaybe before
                      }

    temporalRowsToPatch ::
      Map.Map key (Endpoint value, Endpoint value) ->
      Patch key value
    temporalRowsToPatch =
      fromAscList
        . fmap
          ( \(key, (initial, current)) ->
              (key, cellFromEndpointPair initial current)
          )
        . Map.toAscList
{-# INLINABLE recordMany #-}

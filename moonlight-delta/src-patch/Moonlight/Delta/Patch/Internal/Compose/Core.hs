{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Compose.Core
  ( compose,
    ComposeResult (..),
    Coverage (..),
    NewKeyPolicy (..),
    composeWith,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bits (testBit)
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray
  ( SmallArray,
    indexSmallArray,
    sizeofSmallArray,
  )
import Moonlight.Delta.Patch.Internal.Builder
import Moonlight.Delta.Patch.Internal.Cell
  ( cellAfterEndpoint,
    cellBeforeEndpoint,
    cellFromEndpointPair,
    endpointToMaybe,
  )
import Moonlight.Delta.Patch.Internal.Compose.Aligned
import Moonlight.Delta.Patch.Internal.Compose.Range
import Moonlight.Delta.Patch.Internal.Construction
  ( fromAscList,
  )
import Moonlight.Delta.Patch.Internal.Cursor
import Moonlight.Delta.Patch.Internal.Page
import Moonlight.Delta.Patch.Internal.Types
import Prelude hiding (null)

composeBoundaryError :: key -> Maybe value -> Maybe value -> ComposeError key value
composeBoundaryError key olderAfter newerBefore =
  ComposeBoundaryMismatch
    { boundaryKey = key,
      olderAfter = olderAfter,
      newerBefore = newerBefore
    }
{-# INLINE composeBoundaryError #-}

data Coverage = Coverage
  { coverageOlderOnly :: !Bool,
    coverageNewerOnly :: !Bool
  }

emptyCoverage :: Coverage
emptyCoverage =
  Coverage
    { coverageOlderOnly = False,
      coverageNewerOnly = False
    }
{-# INLINE emptyCoverage #-}

markOlderOnly :: Coverage -> Coverage
markOlderOnly coverage =
  coverage {coverageOlderOnly = True}
{-# INLINE markOlderOnly #-}

markNewerOnly :: Coverage -> Coverage
markNewerOnly coverage =
  coverage {coverageNewerOnly = True}
{-# INLINE markNewerOnly #-}

combineCoverage :: Coverage -> Coverage -> Coverage
combineCoverage left right =
  Coverage
    { coverageOlderOnly = coverageOlderOnly left || coverageOlderOnly right,
      coverageNewerOnly = coverageNewerOnly left || coverageNewerOnly right
    }
{-# INLINE combineCoverage #-}

data NewKeyPolicy state key value error
  = IgnoreNewKeys
  | CheckNewKeys !(state -> key -> Maybe value -> Either error state)

data ComposeResult state key value = ComposeResult
  { patch :: !(Patch key value),
    state :: !state,
    coverage :: !Coverage
  }

compose ::
  forall key value.
  (PatchKey key, PatchValue value) =>
  Patch key value ->
  Patch key value ->
  Either (ComposeError key value) (Patch key value)
compose newer older =
  fmap normalize (composeCanonical newer older)
{-# INLINABLE compose #-}

composeCanonical ::
  forall key value.
  (PatchKey key, PatchValue value) =>
  Patch key value ->
  Patch key value ->
  Either (ComposeError key value) (Patch key value)
composeCanonical newer older
  | null newer =
      Right older
  | null older =
      Right newer
  | SmallPatch olderCells <- older,
    SmallPatch newerCells <- newer =
      composeSmallSmall olderCells newerCells
  | otherwise =
      let !olderPaged = toPaged older
          !newerPaged = toPaged newer
       in case disjointComposition olderPaged newerPaged of
            Just result ->
              Right result
            Nothing -> do
              ComposeResult {patch} <-
                composeWith IgnoreNewKeys () composeBoundaryError olderPaged newerPaged
              pure patch
{-# INLINABLE composeCanonical #-}

composeSmallSmall ::
  (PatchKey key, PatchValue value) =>
  SmallArray (Cell key value) ->
  SmallArray (Cell key value) ->
  Either (ComposeError key value) (Patch key value)
composeSmallSmall olderCells newerCells =
  fmap fromAscList (mergeSmallCells olderCells newerCells)
{-# INLINABLE composeSmallSmall #-}

mergeSmallCells ::
  forall key value.
  (Ord key, Eq value) =>
  SmallArray (Cell key value) ->
  SmallArray (Cell key value) ->
  Either (ComposeError key value) [(key, CellPatch value)]
mergeSmallCells olderCells newerCells =
  go 0 0
  where
    !olderLength = sizeofSmallArray olderCells
    !newerLength = sizeofSmallArray newerCells

    go :: Int -> Int -> Either (ComposeError key value) [(key, CellPatch value)]
    go !olderIndex !newerIndex
      | olderIndex == olderLength && newerIndex == newerLength =
          Right []
      | olderIndex == olderLength =
          case indexSmallArray newerCells newerIndex of
            Cell key cell ->
              fmap ((key, cell) :) (go olderIndex (newerIndex + 1))
      | newerIndex == newerLength =
          case indexSmallArray olderCells olderIndex of
            Cell key cell ->
              fmap ((key, cell) :) (go (olderIndex + 1) newerIndex)
      | otherwise =
          case (indexSmallArray olderCells olderIndex, indexSmallArray newerCells newerIndex) of
            (Cell olderKey olderCell, Cell newerKey newerCell) ->
              case compare olderKey newerKey of
                LT ->
                  fmap ((olderKey, olderCell) :) (go (olderIndex + 1) newerIndex)
                GT ->
                  fmap ((newerKey, newerCell) :) (go olderIndex (newerIndex + 1))
                EQ ->
                  let !olderAfter = cellAfterEndpoint olderCell
                      !newerBefore = cellBeforeEndpoint newerCell
                   in if olderAfter /= newerBefore
                        then
                          Left
                            ( composeBoundaryError
                                olderKey
                                (endpointToMaybe olderAfter)
                                (endpointToMaybe newerBefore)
                            )
                        else
                          let !combined =
                                cellFromEndpointPair
                                  (cellBeforeEndpoint olderCell)
                                  (cellAfterEndpoint newerCell)
                           in fmap ((newerKey, combined) :) (go (olderIndex + 1) (newerIndex + 1))
{-# INLINABLE mergeSmallCells #-}

composeWith ::
  forall state key value error.
  (PatchKey key, PatchValue value) =>
  NewKeyPolicy state key value error ->
  state ->
  (key -> Maybe value -> Maybe value -> error) ->
  Patch key value ->
  Patch key value ->
  Either error (ComposeResult state key value)
composeWith newKeyPolicy initialState makeBoundaryError olderInput newerInput
  | Just patch <- disjointComposition older newer = do
      nextState <- checkNewPatch newKeyPolicy initialState newer
      Right
        ComposeResult
          { patch = patch,
            state = nextState,
            coverage =
              Coverage
                { coverageOlderOnly = True,
                  coverageNewerOnly = True
                }
          }
  | Just rangewise <- composeOverlappingRanges newKeyPolicy initialState makeBoundaryError older newer =
      rangewise
  | otherwise =
      case tryAlignedTree makeBoundaryError (pagesOf older) (pagesOf newer) of
        Left failure ->
          Left failure
        Right (Just pages) ->
          Right
            ComposeResult
              { patch =
                  PagedPatch (entryCount older) pages,
                state = initialState,
                coverage = emptyCoverage
              }
        Right Nothing ->
          composePagewise newKeyPolicy initialState makeBoundaryError older newer
  where
    older = toPaged olderInput
    newer = toPaged newerInput
{-# INLINABLE composeWith #-}

composeOverlappingRanges ::
  forall state key value error.
  (PatchKey key, PatchValue value) =>
  NewKeyPolicy state key value error ->
  state ->
  (key -> Maybe value -> Maybe value -> error) ->
  Patch key value ->
  Patch key value ->
  Maybe (Either error (ComposeResult state key value))
composeOverlappingRanges policy initialState makeBoundaryError older newer =
  case (patchBounds older, patchBounds newer) of
    (Just (olderMinimum, olderMaximum), Just (newerMinimum, newerMaximum)) ->
      let !overlapMinimum = max olderMinimum newerMinimum
          !overlapMaximum = min olderMaximum newerMaximum
          !hasExterior =
            olderMinimum < overlapMinimum
              || newerMinimum < overlapMinimum
              || olderMaximum > overlapMaximum
              || newerMaximum > overlapMaximum
       in if hasExterior
            then
              Just $ do
                let (!olderBefore, !olderMiddle, !olderAfter) =
                      splitPatchRange overlapMinimum overlapMaximum older
                    (!newerBefore, !newerMiddle, !newerAfter) =
                      splitPatchRange overlapMinimum overlapMaximum newer
                beforeResult <- composeWith policy initialState makeBoundaryError olderBefore newerBefore
                middleResult <-
                  composeWith
                    policy
                    (state beforeResult)
                    makeBoundaryError
                    olderMiddle
                    newerMiddle
                afterResult <-
                  composeWith
                    policy
                    (state middleResult)
                    makeBoundaryError
                    olderAfter
                    newerAfter
                Right
                  ComposeResult
                    { patch =
                        appendPatch
                          (patch beforeResult)
                          ( appendPatch
                              (patch middleResult)
                              (patch afterResult)
                          ),
                      state = state afterResult,
                      coverage =
                        combineCoverage
                          (coverage beforeResult)
                          ( combineCoverage
                              (coverage middleResult)
                              (coverage afterResult)
                          )
                    }
            else Nothing
    _ ->
      Nothing
{-# INLINABLE composeOverlappingRanges #-}

patchBounds :: Patch key value -> Maybe (key, key)
patchBounds patch =
  (,) <$> minimumKey patch <*> maximumKey patch
{-# INLINE patchBounds #-}

checkNewPatch ::
  NewKeyPolicy state key value error ->
  state ->
  Patch key value ->
  Either error state
checkNewPatch policy initialState patch =
  go initialState (ascCursor (pagesOf patch))
  where
    go !state AscEnd =
      Right state
    go !state cursor@(AscCursor maximumKey page _ _) = do
      nextState <- checkNewPage policy state maximumKey page
      go nextState (ascAdvance cursor)
{-# INLINABLE checkNewPatch #-}

composePagewise ::
  forall state key value error.
  (PatchKey key, PatchValue value) =>
  NewKeyPolicy state key value error ->
  state ->
  (key -> Maybe value -> Maybe value -> error) ->
  Patch key value ->
  Patch key value ->
  Either error (ComposeResult state key value)
composePagewise policy initialState makeBoundaryError older newer =
  go initialState 0 emptyCoverage [] (ascCursor (pagesOf older)) (ascCursor (pagesOf newer))
  where
    go !state !entryCount !coverage !pagesReverse AscEnd AscEnd =
      Right
        ComposeResult
          { patch =
              PagedPatch entryCount (Map.fromDistinctAscList (reverse pagesReverse)),
            state = state,
            coverage = coverage
          }
    go !state !entryCount !coverage !pagesReverse olderCursor AscEnd =
      drainOlder state entryCount coverage pagesReverse olderCursor
    go !state !entryCount !coverage !pagesReverse AscEnd newerCursor =
      drainNewer state entryCount coverage pagesReverse newerCursor
    go !state !entryCount !coverage !pagesReverse olderCursor@(AscCursor olderMaximum olderPage _ _) newerCursor@(AscCursor newerMaximum newerPage _ _) =
      case compare olderMaximum newerMaximum of
        EQ ->
          case tryAlignedPage makeBoundaryError olderMaximum olderPage newerMaximum newerPage of
            Left failure ->
              Left failure
            Right (Just outputPage) ->
              go
                state
                (entryCount + pageCount newerPage)
                coverage
                (outputPage : pagesReverse)
                (ascAdvance olderCursor)
                (ascAdvance newerCursor)
            Right Nothing ->
              composeRowsFrom policy state makeBoundaryError entryCount coverage pagesReverse olderCursor newerCursor
        LT
          | olderMaximum < pageMinimumKey newerMaximum newerPage ->
              go
                state
                (entryCount + pageCount olderPage)
                (markOlderOnly coverage)
                ((olderMaximum, olderPage) : pagesReverse)
                (ascAdvance olderCursor)
                newerCursor
        GT
          | newerMaximum < pageMinimumKey olderMaximum olderPage -> do
              nextState <- checkNewPage policy state newerMaximum newerPage
              go
                nextState
                (entryCount + pageCount newerPage)
                (markNewerOnly coverage)
                ((newerMaximum, newerPage) : pagesReverse)
                olderCursor
                (ascAdvance newerCursor)
        _ ->
          composeRowsFrom policy state makeBoundaryError entryCount coverage pagesReverse olderCursor newerCursor

    drainOlder !state !entryCount !coverage !pagesReverse cursor =
      case cursor of
        AscEnd ->
          go state entryCount coverage pagesReverse AscEnd AscEnd
        AscCursor maximumKey page _ _ ->
          drainOlder
            state
            (entryCount + pageCount page)
            (markOlderOnly coverage)
            ((maximumKey, page) : pagesReverse)
            (ascAdvance cursor)

    drainNewer !state !entryCount !coverage !pagesReverse cursor =
      case cursor of
        AscEnd ->
          go state entryCount coverage pagesReverse AscEnd AscEnd
        AscCursor maximumKey page _ _ -> do
          nextState <- checkNewPage policy state maximumKey page
          drainNewer
            nextState
            (entryCount + pageCount page)
            (markNewerOnly coverage)
            ((maximumKey, page) : pagesReverse)
            (ascAdvance cursor)
{-# INLINABLE composePagewise #-}

checkNewPage :: NewKeyPolicy state key value error -> state -> key -> Page key value -> Either error state
checkNewPage IgnoreNewKeys state _ _ =
  Right state
checkNewPage (CheckNewKeys check) initialState maximumKey page =
  case columnView (pageCount page) (pageBeforeColumn page) of
    ColumnView mask values ->
      go initialState 0 0 mask values
  where
    go !state !logicalIndex !packedIndex !mask !values
      | logicalIndex == pageCount page =
          Right state
      | otherwise =
          let !present = testBit mask logicalIndex
              !before =
                if present
                  then Just (valueColumnAt values packedIndex)
                  else Nothing
              !key = pageKeyAt maximumKey page logicalIndex
           in case check state key before of
                Left failure ->
                  Left failure
                Right nextState ->
                  go
                    nextState
                    (logicalIndex + 1)
                    (if present then packedIndex + 1 else packedIndex)
                    mask
                    values
{-# INLINABLE checkNewPage #-}

composeRowsFrom ::
  forall state key value error.
  (PatchKey key, PatchValue value) =>
  NewKeyPolicy state key value error ->
  state ->
  (key -> Maybe value -> Maybe value -> error) ->
  Int ->
  Coverage ->
  [(key, Page key value)] ->
  AscCursor key (Page key value) ->
  AscCursor key (Page key value) ->
  Either error (ComposeResult state key value)
composeRowsFrom policy initialState makeBoundaryError prefixEntryCount initialCoverage prefixPagesReverse olderPages newerPages =
  runST $ do
    builder <- newBuilder
    let (!keptEntryCount, !keptPagesReverse, !seedPage) =
          case prefixPagesReverse of
            [] -> (prefixEntryCount, [], Nothing)
            page@(_, patchPage) : remaining -> (prefixEntryCount - pageCount patchPage, remaining, Just page)
    case seedPage of
      Nothing -> pure ()
      Just (maximumKey, page) -> appendPageCopy builder maximumKey page
    merged <-
      mergeRows
        policy
        makeBoundaryError
        builder
        initialState
        initialCoverage
        (fromAsc olderPages)
        (fromAsc newerPages)
    case merged of
      Left failure ->
        pure (Left failure)
      Right (finalState, finalCoverage) -> do
        tailPatch <- finishBuilder builder
        let !prefixMap = Map.fromDistinctAscList (reverse keptPagesReverse)
            !resultPages = appendPages prefixMap (pagesOf tailPatch)
        pure
          ( Right
              ComposeResult
                { patch =
                    PagedPatch (keptEntryCount + entryCount tailPatch) resultPages,
                  state = finalState,
                  coverage = finalCoverage
                }
          )
{-# INLINABLE composeRowsFrom #-}

mergeRows ::
  forall state key value error s.
  (PatchKey key, PatchValue value) =>
  NewKeyPolicy state key value error ->
  (key -> Maybe value -> Maybe value -> error) ->
  Builder s key value ->
  state ->
  Coverage ->
  Cursor key value ->
  Cursor key value ->
  ST s (Either error (state, Coverage))
mergeRows policy makeBoundaryError builder =
  go
  where
    go !state !coverage CursorEnd CursorEnd =
      pure (Right (state, coverage))
    go !state !coverage olderCursor CursorEnd = do
      copyOlderTail olderCursor
      pure (Right (state, markOlderOnly coverage))
    go !state !coverage CursorEnd newerCursor =
      copyNewerTail state coverage newerCursor
    go !state !coverage olderCursor newerCursor
      | Just (olderMaximum, olderPage) <- cursorPageStart olderCursor,
        Just (newerMaximum, newerPage) <- cursorPageStart newerCursor =
          case compare olderMaximum newerMaximum of
            EQ ->
              case tryAlignedPage makeBoundaryError olderMaximum olderPage newerMaximum newerPage of
                Left failure ->
                  pure (Left failure)
                Right (Just (resultMaximum, resultPage)) -> do
                  appendPageCopy builder resultMaximum resultPage
                  go state coverage (advancePage olderCursor) (advancePage newerCursor)
                Right Nothing ->
                  mergeCurrentRows state coverage olderCursor newerCursor
            LT
              | olderMaximum < pageMinimumKey newerMaximum newerPage -> do
                  appendPageCopy builder olderMaximum olderPage
                  go state (markOlderOnly coverage) (advancePage olderCursor) newerCursor
            GT
              | newerMaximum < pageMinimumKey olderMaximum olderPage ->
                  case checkNewPage policy state newerMaximum newerPage of
                    Left failure -> pure (Left failure)
                    Right nextState -> do
                      appendPageCopy builder newerMaximum newerPage
                      go nextState (markNewerOnly coverage) olderCursor (advancePage newerCursor)
            _ ->
              mergeCurrentRows state coverage olderCursor newerCursor
      | otherwise =
          mergeCurrentRows state coverage olderCursor newerCursor

    mergeCurrentRows !state !coverage olderCursor newerCursor =
      case (currentRow olderCursor, currentRow newerCursor) of
        (Just (olderKey, olderBefore, olderAfter), Just (newerKey, newerBefore, newerAfter)) ->
          case compare olderKey newerKey of
            LT -> do
              appendTransition builder olderKey olderBefore olderAfter
              go state (markOlderOnly coverage) (advanceRow olderCursor) newerCursor
            GT ->
              case checkNewRow policy state newerKey (endpointToMaybe newerBefore) of
                Left failure -> pure (Left failure)
                Right nextState -> do
                  appendTransition builder newerKey newerBefore newerAfter
                  go nextState (markNewerOnly coverage) olderCursor (advanceRow newerCursor)
            EQ ->
              if olderAfter /= newerBefore
                then pure (Left (makeBoundaryError newerKey (endpointToMaybe olderAfter) (endpointToMaybe newerBefore)))
                else do
                  appendTransition builder newerKey olderBefore newerAfter
                  go state coverage (advanceRow olderCursor) (advanceRow newerCursor)
        _ ->
          pure (Right (state, coverage))

    copyOlderTail CursorEnd =
      pure ()
    copyOlderTail cursor =
      case currentRow cursor of
        Just (key, before, after) -> do
          appendTransition builder key before after
          copyOlderTail (advanceRow cursor)
        Nothing -> pure ()

    copyNewerTail !state !coverage CursorEnd =
      pure (Right (state, coverage))
    copyNewerTail !state !coverage cursor =
      case currentRow cursor of
        Just (key, before, after) ->
          case checkNewRow policy state key (endpointToMaybe before) of
            Left failure -> pure (Left failure)
            Right nextState -> do
              appendTransition builder key before after
              copyNewerTail nextState (markNewerOnly coverage) (advanceRow cursor)
        Nothing -> pure (Right (state, coverage))
{-# INLINABLE mergeRows #-}

checkNewRow :: NewKeyPolicy state key value error -> state -> key -> Maybe value -> Either error state
checkNewRow IgnoreNewKeys state _ _ =
  Right state
checkNewRow (CheckNewKeys check) state key before =
  check state key before
{-# INLINE checkNewRow #-}

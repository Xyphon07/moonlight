{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Apply
  ( apply,
    shouldUseSparse,
    applyTrusted,
    applyTrustedEdit,
  )
where

import Data.Bits (testBit)
import Data.List qualified as List
-- Data.Map.Internal: balance-aware spine traversal; semi-stable API accepted deliberately, pinned by containers < 0.9.
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray
  ( SmallArray,
    indexSmallArray,
    sizeofSmallArray,
  )
import Data.Word (Word64)
import Moonlight.Delta.Patch.Internal.Cell
import Moonlight.Delta.Patch.Internal.Cursor
import Moonlight.Delta.Patch.Internal.Page
import Moonlight.Delta.Patch.Internal.Types
import Prelude hiding (null)

data EditResult key value
  = EditUnchanged
  | EditChanged !(Map key value)

data MergeCursor key value = MergeCursor !(AscCursor key value) !(Cursor key value)

data MergeOutput key value = MergeOutput !key !value !(MergeCursor key value)

data BuildResult key value = BuildResult !(Map key value) !(MergeCursor key value)

apply ::
  forall key value.
  (Ord key, Eq value) =>
  Patch key value ->
  Map key value ->
  Either (ApplyError key value) (Map key value)
apply patch state =
  case patch of
    SmallPatch cells ->
      applySmall cells state
    PagedPatch _count _pages ->
      case applyAlignedWhole patch state of
        Left failure ->
          Left failure
        Right (Just result) ->
          Right result
        Right Nothing
          | useSparsePlan patch state ->
              applySparse patch state
          | otherwise ->
              applyDense patch state
{-# INLINABLE apply #-}

applySmall ::
  forall key value.
  (Ord key, Eq value) =>
  SmallArray (Cell key value) ->
  Map key value ->
  Either (ApplyError key value) (Map key value)
applySmall cells state =
  go 0 state
  where
    !count = sizeofSmallArray cells

    go :: Int -> Map key value -> Either (ApplyError key value) (Map key value)
    go !index !current
      | index == count =
          Right current
      | otherwise =
          case indexSmallArray cells index of
            Cell key cell -> do
              result <- checkedEdit key (cellBeforeEndpoint cell) (cellAfterEndpoint cell) current
              case result of
                EditUnchanged ->
                  go (index + 1) current
                EditChanged changed ->
                  go (index + 1) changed
{-# INLINABLE applySmall #-}

useSparsePlan :: Patch key value -> Map key value -> Bool
useSparsePlan patch state =
  shouldUseSparse (Map.size state) (entryCount patch)
{-# INLINE useSparsePlan #-}

shouldUseSparse :: Int -> Int -> Bool
shouldUseSparse stateSize patchSize
  | patchSize <= 64 =
      True
  | stateSize <= 0 =
      False
  | otherwise =
      patchSize <= stateSize `quot` (mapLookupFactor (stateSize + 1) + 6)
{-# INLINE shouldUseSparse #-}

mapLookupFactor :: Int -> Int
mapLookupFactor value =
  go 0 value
  where
    go :: Int -> Int -> Int
    go !result remaining
      | remaining <= 1 = result
      | otherwise = go (result + 1) (remaining `quot` 2)
{-# INLINE mapLookupFactor #-}

applySparse ::
  forall key value.
  (Ord key, Eq value) =>
  Patch key value ->
  Map key value ->
  Either (ApplyError key value) (Map key value)
applySparse patch state = do
  (!appliedPrefix, !remainingState) <- applySparsePages (pagesOf patch) Map.empty state
  pure (appendOrdered appliedPrefix remainingState)
{-# INLINABLE applySparse #-}

applySparsePages ::
  forall key value.
  (Ord key, Eq value) =>
  Map key (Page key value) ->
  Map key value ->
  Map key value ->
  Either (ApplyError key value) (Map key value, Map key value)
applySparsePages MapInternal.Tip !accumulated !remaining =
  Right (accumulated, remaining)
applySparsePages (MapInternal.Bin _ maximumKey page left right) !accumulated !remaining = do
  (!afterLeft, !remainingAfterLeft) <- applySparsePages left accumulated remaining
  let !minimumKey = pageMinimumKey maximumKey page
      (!untouched, !atOrAfterMinimum) = splitLessThan minimumKey remainingAfterLeft
      (!localState, !remainingAfterPage) = splitLessOrEqual maximumKey atOrAfterMinimum
  if shouldMergePage page localState
    then do
      !localResult <- applyOnePage maximumKey page localState
      let !afterPage =
            appendOrdered afterLeft (appendOrdered untouched localResult)
      applySparsePages right afterPage remainingAfterPage
    else do
      !updatedRemaining <-
        applySparsePagePointwise
          maximumKey
          page
          (appendOrdered untouched (appendOrdered localState remainingAfterPage))
      applySparsePages right afterLeft updatedRemaining
{-# INLINABLE applySparsePages #-}

shouldMergePage :: Page key value -> Map key value -> Bool
shouldMergePage page localState =
  let !localSize = Map.size localState
      !pageSize = pageCount page
   in localSize <= pageSize * (mapLookupFactor (localSize + 1) + 8)
{-# INLINE shouldMergePage #-}

applySparsePagePointwise ::
  forall key value.
  (Ord key, Eq value) =>
  key ->
  Page key value ->
  Map key value ->
  Either (ApplyError key value) (Map key value)
applySparsePagePointwise maximumKey page =
  case
      ( columnView count (pageBeforeColumn page),
        columnView count (pageAfterColumn page)
      )
    of
      (ColumnView beforeMask beforeValues, ColumnView afterMask afterValues) ->
        go 0 0 0 beforeMask beforeValues afterMask afterValues
  where
    !count = pageCount page

    go :: Int -> Int -> Int -> Word64 -> ValueColumn value -> Word64 -> ValueColumn value -> Map key value -> Either (ApplyError key value) (Map key value)
    go !index !beforePacked !afterPacked !beforeMask !beforeValues !afterMask !afterValues !state
      | index == count =
          Right state
      | otherwise = do
          let !patchKey = pageKeyAt maximumKey page index
              !beforePresent = testBit beforeMask index
              !afterPresent = testBit afterMask index
              !before =
                if beforePresent
                  then EndpointPresent (valueColumnAt beforeValues beforePacked)
                  else EndpointAbsent
              !after =
                if afterPresent
                  then EndpointPresent (valueColumnAt afterValues afterPacked)
                  else EndpointAbsent
              !nextBeforePacked =
                if beforePresent
                  then beforePacked + 1
                  else beforePacked
              !nextAfterPacked =
                if afterPresent
                  then afterPacked + 1
                  else afterPacked
          result <- checkedEdit patchKey before after state
          let !nextState =
                case result of
                  EditUnchanged ->
                    state
                  EditChanged changed ->
                    changed
          go (index + 1) nextBeforePacked nextAfterPacked beforeMask beforeValues afterMask afterValues nextState
{-# INLINABLE applySparsePagePointwise #-}

applyOnePage ::
  forall key value.
  (Ord key, Eq value) =>
  key ->
  Page key value ->
  Map key value ->
  Either (ApplyError key value) (Map key value)
applyOnePage maximumKey page state =
  case
      ( columnView count (pageBeforeColumn page),
        columnView count (pageAfterColumn page)
      )
    of
      (ColumnView beforeMask beforeValues, ColumnView afterMask afterValues) ->
        Map.fromDistinctAscList <$> merge beforeMask beforeValues afterMask afterValues 0 0 0 (Map.toAscList state) []
  where
    !count = pageCount page

    merge !_beforeMask !_beforeValues !_afterMask !_afterValues !logicalIndex !_beforePacked !_afterPacked stateRows !reversedOutput
      | logicalIndex == count =
          Right (List.reverse reversedOutput <> stateRows)
    merge !beforeMask !beforeValues !afterMask !afterValues !logicalIndex !beforePacked !afterPacked stateRows !reversedOutput =
      let !patchKey = pageKeyAt maximumKey page logicalIndex
          !beforePresent = testBit beforeMask logicalIndex
          !afterPresent = testBit afterMask logicalIndex
          !expected =
            if beforePresent
              then EndpointPresent (valueColumnAt beforeValues beforePacked)
              else EndpointAbsent
          !after =
            if afterPresent
              then EndpointPresent (valueColumnAt afterValues afterPacked)
              else EndpointAbsent
          !nextBeforePacked =
            if beforePresent
              then beforePacked + 1
              else beforePacked
          !nextAfterPacked =
            if afterPresent
              then afterPacked + 1
              else afterPacked
          continue =
            merge
              beforeMask
              beforeValues
              afterMask
              afterValues
              (logicalIndex + 1)
              nextBeforePacked
              nextAfterPacked
       in case stateRows of
            [] -> do
              output <- applyEndpoints patchKey expected after EndpointAbsent
              continue [] (prependOutput output reversedOutput)
            rows@((stateKey, stateValue) : stateRest) ->
              case compare patchKey stateKey of
                LT -> do
                  output <- applyEndpoints patchKey expected after EndpointAbsent
                  continue rows (prependOutput output reversedOutput)
                GT ->
                  merge
                    beforeMask
                    beforeValues
                    afterMask
                    afterValues
                    logicalIndex
                    beforePacked
                    afterPacked
                    stateRest
                    ((stateKey, stateValue) : reversedOutput)
                EQ -> do
                  output <- applyEndpoints patchKey expected after (EndpointPresent stateValue)
                  continue stateRest (prependOutput output reversedOutput)

    prependOutput :: Maybe row -> [row] -> [row]
    prependOutput Nothing rows =
      rows
    prependOutput (Just row) rows =
      row : rows
{-# INLINABLE applyOnePage #-}

applyEndpoints ::
  Eq value =>
  key ->
  Endpoint value ->
  Endpoint value ->
  Endpoint value ->
  Either (ApplyError key value) (Maybe (key, value))
applyEndpoints patchKey expected after actual =
  if expected == actual
    then
      Right
        ( case after of
            EndpointAbsent ->
              Nothing
            EndpointPresent value ->
              Just (patchKey, value)
        )
    else
      Left
        ApplyBeforeMismatch
          { mismatchKey = patchKey,
            expectedBefore = endpointToMaybe expected,
            actualBefore = endpointToMaybe actual
          }
{-# INLINE applyEndpoints #-}

appendOrdered :: Map key value -> Map key value -> Map key value
appendOrdered left right =
  case (left, right) of
    (MapInternal.Tip, _) ->
      right
    (_, MapInternal.Tip) ->
      left
    _ ->
      MapInternal.link2 left right
{-# INLINE appendOrdered #-}

splitLessThan :: Ord key => key -> Map key value -> (Map key value, Map key value)
splitLessThan key entries =
  case Map.splitLookup key entries of
    (less, Nothing, greater) ->
      (less, greater)
    (less, Just value, greater) ->
      (less, Map.insert key value greater)
{-# INLINE splitLessThan #-}

splitLessOrEqual :: Ord key => key -> Map key value -> (Map key value, Map key value)
splitLessOrEqual key entries =
  case Map.splitLookup key entries of
    (less, Nothing, greater) ->
      (less, greater)
    (less, Just value, greater) ->
      (Map.insert key value less, greater)
{-# INLINE splitLessOrEqual #-}

applyAlignedWhole ::
  forall key value.
  (Ord key, Eq value) =>
  Patch key value ->
  Map key value ->
  Either (ApplyError key value) (Maybe (Map key value))
applyAlignedWhole patch state
  | entryCount patch /= Map.size state =
      Right Nothing
  | otherwise = do
      result <- rebuild state (cursor (pagesOf patch))
      case result of
        Nothing ->
          Right Nothing
        Just (!tree, !remainingPatch) ->
          case remainingPatch of
            CursorEnd ->
              Right (Just tree)
            _activeCursor ->
              Right Nothing
  where
    rebuild ::
      Map key value ->
      Cursor key value ->
      Either (ApplyError key value) (Maybe (Map key value, Cursor key value))
    rebuild MapInternal.Tip cursor =
      Right (Just (MapInternal.Tip, cursor))
    rebuild (MapInternal.Bin stateCount stateKey stateValue left right) cursor = do
      maybeLeft <- rebuild left cursor
      case maybeLeft of
        Nothing ->
          Right Nothing
        Just (!rebuiltLeft, !afterLeft) ->
          case currentKey afterLeft of
            Just patchKey ->
              case compare patchKey stateKey of
                EQ ->
                  case beforeMaybe afterLeft of
                    Just expected
                      | expected == stateValue ->
                          case afterMaybe afterLeft of
                            Nothing ->
                              Right Nothing
                            Just afterValue -> do
                              maybeRight <- rebuild right (advanceRow afterLeft)
                              case maybeRight of
                                Nothing ->
                                  Right Nothing
                                Just (!rebuiltRight, !afterRight) ->
                                  Right
                                    ( Just
                                        ( MapInternal.Bin stateCount patchKey afterValue rebuiltLeft rebuiltRight,
                                          afterRight
                                        )
                                    )
                    maybeExpected ->
                      Left
                        ApplyBeforeMismatch
                          { mismatchKey = patchKey,
                            expectedBefore = maybeExpected,
                            actualBefore = Just stateValue
                          }
                _ ->
                  Right Nothing
            Nothing ->
              Right Nothing
{-# INLINABLE applyAlignedWhole #-}

checkedEdit ::
  forall key value.
  (Ord key, Eq value) =>
  key ->
  Endpoint value ->
  Endpoint value ->
  Map key value ->
  Either (ApplyError key value) (EditResult key value)
checkedEdit patchKey before after =
  descend
  where
    !expected = endpointToMaybe before

    descend entries =
      case entries of
        MapInternal.Tip ->
          case expected of
            Just _ ->
              Left (ApplyBeforeMismatch patchKey expected Nothing)
            Nothing ->
              case after of
                EndpointAbsent ->
                  Right EditUnchanged
                EndpointPresent newValue ->
                  Right (EditChanged (Map.singleton patchKey newValue))
        MapInternal.Bin nodeSize existingKey existingValue left right ->
          case compare patchKey existingKey of
            LT -> do
              result <- descend left
              pure
                ( case result of
                    EditUnchanged -> EditUnchanged
                    EditChanged changedLeft -> EditChanged (MapInternal.link existingKey existingValue changedLeft right)
                )
            GT -> do
              result <- descend right
              pure
                ( case result of
                    EditUnchanged -> EditUnchanged
                    EditChanged changedRight -> EditChanged (MapInternal.link existingKey existingValue left changedRight)
                )
            EQ
              | expected /= Just existingValue ->
                  Left (ApplyBeforeMismatch patchKey expected (Just existingValue))
              | otherwise ->
                  case after of
                    EndpointAbsent ->
                      Right (EditChanged (MapInternal.link2 left right))
                    EndpointPresent newValue ->
                      Right (EditChanged (MapInternal.Bin nodeSize patchKey newValue left right))
{-# INLINABLE checkedEdit #-}

applyDense ::
  forall key value.
  (Ord key, Eq value) =>
  Patch key value ->
  Map key value ->
  Either (ApplyError key value) (Map key value)
applyDense patch state =
  case applyOutputCountCandidate patch state of
    Nothing ->
      applySparse patch state
    Just outputCount ->
      case buildCheckedApplyMap outputCount initialCursor of
        Left failure ->
          Left failure
        Right Nothing ->
          applySparse patch state
        Right (Just (BuildResult result remaining)) ->
          case drainCheckedApplyCursor remaining of
            Left failure ->
              Left failure
            Right True ->
              Right result
            Right False ->
              applySparse patch state
  where
    !initialCursor =
      MergeCursor (ascCursor state) (cursor (pagesOf patch))
{-# INLINABLE applyDense #-}

applyOutputCountCandidate :: Patch key value -> Map key value -> Maybe Int
applyOutputCountCandidate patch state =
  let !outputCount = Map.size state + netSizeDelta patch
   in if outputCount < 0 then Nothing else Just outputCount
{-# INLINE applyOutputCountCandidate #-}

buildCheckedApplyMap ::
  (Ord key, Eq value) =>
  Int ->
  MergeCursor key value ->
  Either (ApplyError key value) (Maybe (BuildResult key value))
buildCheckedApplyMap !outputCount cursor
  | outputCount <= 0 =
      Right (Just (BuildResult MapInternal.Tip cursor))
  | otherwise = do
      let !leftCount = outputCount `quot` 2
          !rightCount = outputCount - leftCount - 1
      maybeLeft <- buildCheckedApplyMap leftCount cursor
      case maybeLeft of
        Nothing ->
          Right Nothing
        Just (BuildResult left afterLeft) -> do
          maybeMiddle <- nextApplyChecked afterLeft
          case maybeMiddle of
            Nothing ->
              Right Nothing
            Just (MergeOutput key value afterMiddle) -> do
              maybeRight <- buildCheckedApplyMap rightCount afterMiddle
              case maybeRight of
                Nothing ->
                  Right Nothing
                Just (BuildResult right afterRight) ->
                  Right (Just (BuildResult (MapInternal.Bin outputCount key value left right) afterRight))
{-# INLINABLE buildCheckedApplyMap #-}

drainCheckedApplyCursor ::
  (Ord key, Eq value) =>
  MergeCursor key value ->
  Either (ApplyError key value) Bool
drainCheckedApplyCursor cursor =
  case nextApplyChecked cursor of
    Left failure ->
      Left failure
    Right Nothing ->
      Right True
    Right (Just _) ->
      Right False
{-# INLINABLE drainCheckedApplyCursor #-}

buildTrustedApplyMap :: Ord key => Int -> MergeCursor key value -> Maybe (BuildResult key value)
buildTrustedApplyMap !outputCount cursor
  | outputCount <= 0 =
      Just (BuildResult MapInternal.Tip cursor)
  | otherwise = do
      let !leftCount = outputCount `quot` 2
          !rightCount = outputCount - leftCount - 1
      BuildResult left afterLeft <- buildTrustedApplyMap leftCount cursor
      MergeOutput key value afterMiddle <- nextApplyTrusted afterLeft
      BuildResult right afterRight <- buildTrustedApplyMap rightCount afterMiddle
      Just (BuildResult (MapInternal.Bin outputCount key value left right) afterRight)
{-# INLINABLE buildTrustedApplyMap #-}

nextApplyChecked ::
  forall key value.
  (Ord key, Eq value) =>
  MergeCursor key value ->
  Either (ApplyError key value) (Maybe (MergeOutput key value))
nextApplyChecked (MergeCursor stateCursor cursor') =
  case (stateCursor, cursor') of
    (AscEnd, CursorEnd) ->
      Right Nothing
    (AscCursor stateKey stateValue _ _, CursorEnd) ->
      Right (Just (MergeOutput stateKey stateValue (MergeCursor (ascAdvance stateCursor) CursorEnd)))
    (AscEnd, _) ->
      consumeAbsentStateRow AscEnd cursor'
    (AscCursor stateKey stateValue _ _, _) ->
      case currentKey cursor' of
        Nothing ->
          Right (Just (MergeOutput stateKey stateValue (MergeCursor (ascAdvance stateCursor) cursor')))
        Just patchKey ->
          case compare stateKey patchKey of
            LT ->
              Right (Just (MergeOutput stateKey stateValue (MergeCursor (ascAdvance stateCursor) cursor')))
            GT ->
              consumeAbsentStateRow stateCursor cursor'
            EQ ->
              let !expected = beforeMaybe cursor'
               in if expected /= Just stateValue
                    then Left (ApplyBeforeMismatch patchKey expected (Just stateValue))
                    else emitPatchAfter patchKey (MergeCursor (ascAdvance stateCursor) (advanceRow cursor')) cursor'
  where
    consumeAbsentStateRow ::
      AscCursor key value ->
      Cursor key value ->
      Either (ApplyError key value) (Maybe (MergeOutput key value))
    consumeAbsentStateRow stateCursor' cursor =
      case currentKey cursor of
        Nothing ->
          Right Nothing
        Just key ->
          case beforeMaybe cursor of
            Just expected ->
              Left (ApplyBeforeMismatch key (Just expected) Nothing)
            Nothing ->
              emitPatchAfter key (MergeCursor stateCursor' (advanceRow cursor)) cursor

    emitPatchAfter ::
      key ->
      MergeCursor key value ->
      Cursor key value ->
      Either (ApplyError key value) (Maybe (MergeOutput key value))
    emitPatchAfter key nextCursor cursor =
      case afterMaybe cursor of
        Nothing ->
          nextApplyChecked nextCursor
        Just value ->
          Right (Just (MergeOutput key value nextCursor))
{-# INLINABLE nextApplyChecked #-}

nextApplyTrusted :: forall key value. Ord key => MergeCursor key value -> Maybe (MergeOutput key value)
nextApplyTrusted (MergeCursor stateCursor cursor') =
  case (stateCursor, cursor') of
    (AscEnd, CursorEnd) ->
      Nothing
    (AscCursor stateKey stateValue _ _, CursorEnd) ->
      Just (MergeOutput stateKey stateValue (MergeCursor (ascAdvance stateCursor) CursorEnd))
    (AscEnd, _) ->
      emitTrustedPatchRow AscEnd cursor'
    (AscCursor stateKey stateValue _ _, _) ->
      case currentKey cursor' of
        Nothing ->
          Just (MergeOutput stateKey stateValue (MergeCursor (ascAdvance stateCursor) cursor'))
        Just patchKey ->
          case compare stateKey patchKey of
            LT ->
              Just (MergeOutput stateKey stateValue (MergeCursor (ascAdvance stateCursor) cursor'))
            GT ->
              emitTrustedPatchRow stateCursor cursor'
            EQ ->
              emitTrustedPatchRow (ascAdvance stateCursor) cursor'
  where
    emitTrustedPatchRow :: AscCursor key value -> Cursor key value -> Maybe (MergeOutput key value)
    emitTrustedPatchRow stateCursor' cursor =
      case currentKey cursor of
        Nothing ->
          Nothing
        Just key ->
          let !nextCursor =
                MergeCursor stateCursor' (advanceRow cursor)
           in case afterMaybe cursor of
                Nothing ->
                  nextApplyTrusted nextCursor
                Just value ->
                  Just (MergeOutput key value nextCursor)
{-# INLINABLE nextApplyTrusted #-}


applyTrusted :: Ord key => Patch key value -> Map key value -> Map key value
applyTrusted patch state =
  case patch of
    SmallPatch cells ->
      applyTrustedSmall cells state
    PagedPatch _count _pages
      | useSparsePlan patch state ->
          applyTrustedPointwise patch state
      | otherwise ->
          applyTrustedDense patch state

applyTrustedSmall :: Ord key => SmallArray (Cell key value) -> Map key value -> Map key value
applyTrustedSmall cells state =
  go 0 state
  where
    !count = sizeofSmallArray cells

    go !index !current
      | index == count =
          current
      | otherwise =
          case indexSmallArray cells index of
            Cell key cell ->
              go (index + 1) (applyTrustedEdit key (cellAfter cell) current)
{-# INLINABLE applyTrustedSmall #-}

applyTrustedDense :: forall key value. Ord key => Patch key value -> Map key value -> Map key value
applyTrustedDense patch state =
  case applyOutputCountCandidate patch state of
    Nothing ->
      applyTrustedPointwise patch state
    Just outputCount ->
      case buildTrustedApplyMap outputCount initialCursor of
        Just (BuildResult result remaining)
          | drainTrustedApplyCursor remaining ->
              result
        _ ->
          applyTrustedPointwise patch state
  where
    !initialCursor =
      MergeCursor (ascCursor state) (cursor (pagesOf patch))
{-# INLINABLE applyTrustedDense #-}

drainTrustedApplyCursor :: Ord key => MergeCursor key value -> Bool
drainTrustedApplyCursor cursor =
  case nextApplyTrusted cursor of
    Nothing ->
      True
    Just _ ->
      False
{-# INLINABLE drainTrustedApplyCursor #-}

applyTrustedPointwise :: forall key value. Ord key => Patch key value -> Map key value -> Map key value
applyTrustedPointwise patch =
  go (cursor (pagesOf patch))
  where
    go :: Cursor key value -> Map key value -> Map key value
    go CursorEnd !state =
      state
    go cursor !state =
      case currentKey cursor of
        Nothing ->
          state
        Just key ->
          go (advanceRow cursor) (applyTrustedEdit key (afterMaybe cursor) state)
{-# INLINABLE applyTrustedPointwise #-}

applyTrustedEdit :: Ord key => key -> Maybe value -> Map key value -> Map key value
applyTrustedEdit patchKey after initialEntries =
  case descend initialEntries of
    EditUnchanged ->
      initialEntries
    EditChanged changed ->
      changed
  where
    descend currentEntries =
      case currentEntries of
        MapInternal.Tip ->
          case after of
            Nothing ->
              EditUnchanged
            Just newValue ->
              EditChanged (Map.singleton patchKey newValue)
        MapInternal.Bin nodeSize existingKey existingValue left right ->
          case compare patchKey existingKey of
            LT ->
              case descend left of
                EditUnchanged ->
                  EditUnchanged
                EditChanged changedLeft ->
                  EditChanged (MapInternal.link existingKey existingValue changedLeft right)
            GT ->
              case descend right of
                EditUnchanged ->
                  EditUnchanged
                EditChanged changedRight ->
                  EditChanged (MapInternal.link existingKey existingValue left changedRight)
            EQ ->
              case after of
                Nothing ->
                  EditChanged (MapInternal.link2 left right)
                Just newValue ->
                  EditChanged (MapInternal.Bin nodeSize patchKey newValue left right)
{-# INLINABLE applyTrustedEdit #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- Loop-local rebinding (state/cursor/coverage) is the engine idiom here; shadowing is deliberate.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Moonlight.Delta.Patch.Internal.Replay
  ( replay,
  )
where

import Data.Bits (testBit)
import Data.Foldable qualified as Foldable
-- Data.Map.Internal: balance-aware spine traversal; semi-stable API accepted deliberately, pinned by containers < 0.9.
import Data.Map.Internal qualified as MapInternal
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray
  ( indexSmallArray,
    sizeofSmallArray,
  )
import Moonlight.Delta.Patch.Internal.Apply
  ( apply,
    applyTrusted,
    applyTrustedEdit,
    shouldUseSparse,
  )
import Moonlight.Delta.Patch.Internal.Cell
  ( cellAfter,
    cellBefore,
  )
import Moonlight.Delta.Patch.Internal.Compose.Core
  ( ComposeResult (..),
    Coverage (..),
    NewKeyPolicy (..),
    composeWith,
  )
import Moonlight.Delta.Patch.Internal.Compose.SameSupport
  ( SameSupport,
    extendSameSupport,
    reflexiveSupport,
    sameSupportLatest,
    spliceSameSupport,
    validateBoundarySameSupport,
  )
import Moonlight.Delta.Patch.Internal.Cursor
  ( AscCursor (..),
    Cursor (CursorEnd),
    ascAdvance,
    ascCursor,
    advanceRow,
    afterMaybe,
    beforeMaybe,
    currentKey,
    cursor,
  )
import Moonlight.Delta.Patch.Internal.Types
  ( ApplyError (..),
    EndpointColumn (..),
    Cell (..),
    Patch (..),
    PatchKey,
    PatchValue,
    Page (..),
    ReplayError (..),
    pageCapacity,
    entryCount,
    valueColumnAt,
  )
import Numeric.Natural (Natural)
import Prelude

data StateProbe key value
  = ProbeFresh !(Map key value) !(AscCursor key value)
  | ProbeAfter !(Map key value) !key !(AscCursor key value)
  | ProbeRandom !(Map key value)

newStateProbe :: Map key value -> StateProbe key value
newStateProbe state =
  ProbeFresh state (ascCursor state)
{-# INLINE newStateProbe #-}

probeInitialState :: Ord key => key -> StateProbe key value -> (Maybe value, StateProbe key value)
probeInitialState key probe =
  case probe of
    ProbeFresh state cursor ->
      let (!result, !nextCursor) = lookupAscending key cursor
       in (result, ProbeAfter state key nextCursor)
    ProbeAfter state previousKey cursor
      | previousKey < key ->
          let (!result, !nextCursor) = lookupAscending key cursor
           in (result, ProbeAfter state key nextCursor)
      | otherwise ->
          (Map.lookup key state, ProbeRandom state)
    ProbeRandom state ->
      (Map.lookup key state, ProbeRandom state)
{-# INLINABLE probeInitialState #-}

lookupAscending :: Ord key => key -> AscCursor key value -> (Maybe value, AscCursor key value)
lookupAscending _ AscEnd =
  (Nothing, AscEnd)
lookupAscending target cursor@(AscCursor key value _ _) =
  case compare key target of
    LT -> lookupAscending target (ascAdvance cursor)
    EQ -> (Just value, cursor)
    GT -> (Nothing, cursor)
{-# INLINABLE lookupAscending #-}

data ReplayAccumulator key value
  = ReplayStable !(SameSupport key value)
  | ReplayGeneral !(Patch key value)
  | ReplayArena !(Map key (Maybe value, Maybe value))

data SingletonReplayArena key value
  = SingletonArenaAscending !key ![(key, (Maybe value, Maybe value))]
  | SingletonArenaMap !(Map key (Maybe value, Maybe value))

data ArenaBulkChanges key value
  = ArenaNoChanges
  | ArenaInsertions !(Map key value)
  | ArenaDeletions !(Map key ())
  | ArenaMixedChanges !(Map key value) !(Map key ())

data AscendingArenaRows key value = AscendingArenaRows ![(key, value)] ![(key, ())]

data ReplayMergeCursor key value = ReplayMergeCursor !(AscCursor key value) !(AscCursor key (Maybe value, Maybe value))

data ReplayOutput key value = ReplayOutput !key !value !(ReplayMergeCursor key value)

data BuildReplayResult key value = BuildReplayResult !(Map key value) !(ReplayMergeCursor key value)

replay ::
  forall patches key value.
  (Foldable patches, PatchKey key, PatchValue value) =>
  patches (Patch key value) ->
  Map key value ->
  Either (ReplayError key value) (Map key value)
replay patches initialState =
  case Foldable.toList patches of
    [] ->
      Right initialState
    first : remaining -> do
      probe <- validateFirstTouches 0 (newStateProbe initialState) first
      continue 1 probe (ReplayStable (reflexiveSupport first)) remaining
  where
    continue !_patchIndex !_probe accumulator [] =
      Right
        ( case accumulator of
            ReplayStable sameSupport ->
              applyTrusted (spliceSameSupport sameSupport) initialState
            ReplayGeneral patch ->
              applyTrusted patch initialState
            ReplayArena arena ->
              applyArenaDirect arena initialState
        )
    continue !patchIndex !probe accumulator (nextPatch : remaining) =
      case accumulator of
        ReplayStable stableRun ->
          let !latest = sameSupportLatest stableRun
           in case validateBoundarySameSupport (replayBoundaryError patchIndex) latest nextPatch of
                Left failure ->
                  Left failure
                Right (Just latestToNext) ->
                  continue
                    (patchIndex + 1)
                    probe
                    (ReplayStable (extendSameSupport stableRun latestToNext))
                    remaining
                Right Nothing ->
                  composeDivergent patchIndex probe (spliceSameSupport stableRun) nextPatch remaining
        ReplayGeneral accumulated ->
          case validateBoundarySameSupport (replayBoundaryError patchIndex) accumulated nextPatch of
            Left failure ->
              Left failure
            Right (Just sameSupport) ->
              continue (patchIndex + 1) probe (ReplayStable sameSupport) remaining
            Right Nothing ->
              composeDivergent patchIndex probe accumulated nextPatch remaining
        ReplayArena arena -> do
          nextArena <- applyPatchToArena patchIndex initialState nextPatch arena
          continue (patchIndex + 1) probe (ReplayArena nextArena) remaining

    composeDivergent !patchIndex !probe !accumulated !nextPatch remaining
      | useSequentialReplay accumulated nextPatch =
          replaySingletonArenaFrom
            patchIndex
            initialState
            probe
            (singletonArenaFromPatch accumulated)
            (nextPatch : remaining)
      | otherwise = do
          ComposeResult
            { patch = combined,
              state = nextProbe,
              coverage = coverage
            } <-
            composeWith
              (CheckNewKeys (validateInitialBefore patchIndex))
              probe
              (replayBoundaryError patchIndex)
              accumulated
              nextPatch
          let !nextAccumulator =
                if useReplayArena coverage accumulated nextPatch
                  then ReplayArena (arenaFromPatch combined)
                  else
                    if coverageOlderOnly coverage
                      then ReplayGeneral combined
                      else ReplayStable (reflexiveSupport combined)
          continue (patchIndex + 1) nextProbe nextAccumulator remaining
{-# INLINABLE replay #-}
{-# SPECIALIZE replay ::
  (PatchKey key, PatchValue value) =>
  [Patch key value] ->
  Map key value ->
  Either (ReplayError key value) (Map key value)
  #-}

replaySequentiallyFrom ::
  (Ord key, Eq value) =>
  Natural ->
  Map key value ->
  [Patch key value] ->
  Either (ReplayError key value) (Map key value)
replaySequentiallyFrom !_patchIndex !state [] =
  Right state
replaySequentiallyFrom !patchIndex !state (patch : remaining) =
  case apply patch state of
    Left applyError ->
      Left
        ReplayApplyError
          { replayIndex = patchIndex,
            replayApply = applyError
          }
    Right !nextState ->
      replaySequentiallyFrom (patchIndex + 1) nextState remaining
{-# INLINABLE replaySequentiallyFrom #-}

replaySingletonArenaFrom ::
  (Ord key, Eq value) =>
  Natural ->
  Map key value ->
  StateProbe key value ->
  SingletonReplayArena key value ->
  [Patch key value] ->
  Either (ReplayError key value) (Map key value)
replaySingletonArenaFrom !_patchIndex !initialState !_probe !arena [] =
  Right (applySingletonReplayArena arena initialState)
replaySingletonArenaFrom !patchIndex !initialState !probe !arena patches@(patch : remaining) =
  case singletonPatchRow patch of
    Nothing ->
      replaySequentiallyFrom patchIndex (applySingletonReplayArena arena initialState) patches
    Just (key, expectedBefore, currentAfter) ->
      case applySingletonToReplayArena patchIndex key expectedBefore currentAfter probe arena of
        Left failure ->
          Left failure
        Right (!nextProbe, !nextArena) ->
          replaySingletonArenaFrom (patchIndex + 1) initialState nextProbe nextArena remaining
{-# INLINABLE replaySingletonArenaFrom #-}

singletonArenaFromPatch :: Ord key => Patch key value -> SingletonReplayArena key value
singletonArenaFromPatch patch =
  case singletonPatchRow patch of
    Just (key, before, after) ->
      SingletonArenaAscending key [(key, (before, after))]
    Nothing ->
      SingletonArenaMap (arenaFromPatch patch)
{-# INLINABLE singletonArenaFromPatch #-}

applySingletonReplayArena :: Ord key => SingletonReplayArena key value -> Map key value -> Map key value
applySingletonReplayArena arena initialState =
  case arena of
    SingletonArenaAscending _ rows ->
      applyArenaBulkChanges (ascendingSingletonArenaBulkChanges rows) initialState
    SingletonArenaMap entries ->
      applyArenaDirect entries initialState
{-# INLINABLE applySingletonReplayArena #-}


ascendingSingletonArenaBulkChanges :: [(key, (Maybe value, Maybe value))] -> ArenaBulkChanges key value
ascendingSingletonArenaBulkChanges rows =
  let !(AscendingArenaRows insertionRows deletionRows) =
        Foldable.foldl' collectRow (AscendingArenaRows [] []) rows
      !insertions = Map.fromDistinctAscList insertionRows
      !deletions = Map.fromDistinctAscList deletionRows
   in case (Map.null insertions, Map.null deletions) of
        (True, True) ->
          ArenaNoChanges
        (False, True) ->
          ArenaInsertions insertions
        (True, False) ->
          ArenaDeletions deletions
        (False, False) ->
          ArenaMixedChanges insertions deletions
  where
    collectRow ::
      AscendingArenaRows key value ->
      (key, (Maybe value, Maybe value)) ->
      AscendingArenaRows key value
    collectRow (AscendingArenaRows insertions deletions) (key, (_initial, current)) =
      case current of
        Nothing ->
          AscendingArenaRows insertions ((key, ()) : deletions)
        Just value ->
          AscendingArenaRows ((key, value) : insertions) deletions
{-# INLINABLE ascendingSingletonArenaBulkChanges #-}


applyArenaBulkChanges :: Ord key => ArenaBulkChanges key value -> Map key value -> Map key value
applyArenaBulkChanges changes initialState =
  case changes of
    ArenaNoChanges ->
      initialState
    ArenaInsertions insertions ->
      Map.union insertions initialState
    ArenaDeletions deletions ->
      Map.difference initialState deletions
    ArenaMixedChanges insertions deletions ->
      Map.union insertions (Map.difference initialState deletions)
{-# INLINABLE applyArenaBulkChanges #-}

applySingletonToReplayArena ::
  (Ord key, Eq value) =>
  Natural ->
  key ->
  Maybe value ->
  Maybe value ->
  StateProbe key value ->
  SingletonReplayArena key value ->
  Either (ReplayError key value) (StateProbe key value, SingletonReplayArena key value)
applySingletonToReplayArena patchIndex key expectedBefore currentAfter probe arena =
  case arena of
    SingletonArenaAscending latestKey rows
      | latestKey < key ->
          let (!initialBefore, !nextProbe) = probeInitialState key probe
           in if expectedBefore == initialBefore
                then
                  Right
                    ( nextProbe,
                      SingletonArenaAscending key ((key, (initialBefore, currentAfter)) : rows)
                    )
                else
                  Left
                    ReplayApplyError
                      { replayIndex = patchIndex,
                        replayApply =
                          ApplyBeforeMismatch
                            { mismatchKey = key,
                              expectedBefore = expectedBefore,
                              actualBefore = initialBefore
                            }
                      }
      | otherwise ->
          promote (Map.fromDistinctAscList (reverse rows))
    SingletonArenaMap entries ->
      promote entries
  where
    promote entries = do
      (!nextProbe, !nextEntries) <- applySingletonToArena patchIndex key expectedBefore currentAfter probe entries
      pure (nextProbe, SingletonArenaMap nextEntries)
{-# INLINABLE applySingletonToReplayArena #-}

singletonPatchRow :: Patch key value -> Maybe (key, Maybe value, Maybe value)
singletonPatchRow patch
  | entryCount patch /= 1 =
      Nothing
  | otherwise =
      case patch of
        SmallPatch cells ->
          case indexSmallArray cells 0 of
            Cell key cell ->
              Just (key, cellBefore cell, cellAfter cell)
        PagedPatch _count pages ->
          case pages of
            MapInternal.Bin _ key page MapInternal.Tip MapInternal.Tip
              | pageCount page == 1 ->
                  Just
                    ( key,
                      singletonEndpointMaybe (pageBeforeColumn page),
                      singletonEndpointMaybe (pageAfterColumn page)
                    )
            _ ->
              Nothing
{-# INLINE singletonPatchRow #-}

singletonEndpointMaybe :: EndpointColumn value -> Maybe value
singletonEndpointMaybe column =
  case column of
    AllPresent values ->
      Just (valueColumnAt values 0)
    Presence mask values ->
      if testBit mask 0
        then Just (valueColumnAt values 0)
        else Nothing
{-# INLINE singletonEndpointMaybe #-}

applySingletonToArena ::
  (Ord key, Eq value) =>
  Natural ->
  key ->
  Maybe value ->
  Maybe value ->
  StateProbe key value ->
  Map key (Maybe value, Maybe value) ->
  Either (ReplayError key value) (StateProbe key value, Map key (Maybe value, Maybe value))
applySingletonToArena patchIndex key expectedBefore currentAfter probe =
  descend
  where
    descend entries =
      case entries of
        MapInternal.Tip ->
          let (!initialBefore, !nextProbe) = probeInitialState key probe
           in validate initialBefore nextProbe (Map.singleton key (initialBefore, currentAfter))
        MapInternal.Bin nodeSize existingKey existingEntry left right ->
          case compare key existingKey of
            LT -> do
              (!nextProbe, !updatedLeft) <- descend left
              pure (nextProbe, MapInternal.link existingKey existingEntry updatedLeft right)
            GT -> do
              (!nextProbe, !updatedRight) <- descend right
              pure (nextProbe, MapInternal.link existingKey existingEntry left updatedRight)
            EQ ->
              let (!initialBefore, !actualBefore) = existingEntry
               in validate
                    actualBefore
                    probe
                    (MapInternal.Bin nodeSize key (initialBefore, currentAfter) left right)

    validate actualBefore nextProbe updatedArena
      | expectedBefore == actualBefore =
          Right (nextProbe, updatedArena)
      | otherwise =
          Left
            ReplayApplyError
              { replayIndex = patchIndex,
                replayApply =
                  ApplyBeforeMismatch
                    { mismatchKey = key,
                      expectedBefore = expectedBefore,
                      actualBefore = actualBefore
                    }
              }
{-# INLINABLE applySingletonToArena #-}

useSequentialReplay :: Patch key patch -> Patch key patch -> Bool
useSequentialReplay accumulated nextPatch =
  entryCount accumulated == 1
    && entryCount nextPatch == 1
{-# INLINE useSequentialReplay #-}

useReplayArena :: Coverage -> Patch key value -> Patch key value -> Bool
useReplayArena coverage accumulated nextPatch =
  coverageOlderOnly coverage
    && not (coverageNewerOnly coverage)
    && entryCount nextPatch <= entryCount accumulated `div` sparseSubsetReplayFactor
{-# INLINE useReplayArena #-}

sparseSubsetReplayFactor :: Int
sparseSubsetReplayFactor = 4
{-# INLINE sparseSubsetReplayFactor #-}

arenaFromPatch :: Ord key => Patch key value -> Map key (Maybe value, Maybe value)
arenaFromPatch =
  foldPatchRows
    ( \arena key initial current ->
        insertArenaEntry key (initial, current) arena
    )
    Map.empty
{-# INLINABLE arenaFromPatch #-}

applyPatchToArena ::
  forall key value.
  (Ord key, Eq value) =>
  Natural ->
  Map key value ->
  Patch key value ->
  Map key (Maybe value, Maybe value) ->
  Either (ReplayError key value) (Map key (Maybe value, Maybe value))
applyPatchToArena patchIndex initialState patch initialArena =
  case patch of
    SmallPatch cells ->
      goSmall cells 0 initialArena
    PagedPatch _count pages ->
      go initialArena (cursor pages)
  where
    go !arena CursorEnd =
      Right arena
    go !arena cursor =
      case currentKey cursor of
        Nothing ->
          Right arena
        Just key ->
          case stepArena key (beforeMaybe cursor) (afterMaybe cursor) arena of
            Left failure ->
              Left failure
            Right nextArena ->
              go nextArena (advanceRow cursor)

    goSmall cells !index !arena
      | index == sizeofSmallArray cells =
          Right arena
      | otherwise =
          case indexSmallArray cells index of
            Cell key cell ->
              case stepArena key (cellBefore cell) (cellAfter cell) arena of
                Left failure ->
                  Left failure
                Right nextArena ->
                  goSmall cells (index + 1) nextArena

    stepArena !key !expected !currentAfter !arena =
      let (!initialBefore, !actualBefore) =
            case Map.lookup key arena of
              Just (initial, current) ->
                (initial, current)
              Nothing ->
                let !initial = Map.lookup key initialState
                 in (initial, initial)
       in if expected == actualBefore
            then Right (insertArenaEntry key (initialBefore, currentAfter) arena)
            else
              Left
                ReplayApplyError
                  { replayIndex = patchIndex,
                    replayApply =
                      ApplyBeforeMismatch
                        { mismatchKey = key,
                          expectedBefore = expected,
                          actualBefore = actualBefore
                        }
                  }
{-# INLINABLE applyPatchToArena #-}

insertArenaEntry :: Ord key => key -> (Maybe value, Maybe value) -> Map key (Maybe value, Maybe value) -> Map key (Maybe value, Maybe value)
insertArenaEntry patchKey entry =
  descend
  where
    descend entries =
      case entries of
        MapInternal.Tip ->
          Map.singleton patchKey entry
        MapInternal.Bin nodeSize existingKey existingEntry left right ->
          case compare patchKey existingKey of
            LT ->
              MapInternal.link existingKey existingEntry (descend left) right
            GT ->
              MapInternal.link existingKey existingEntry left (descend right)
            EQ ->
              MapInternal.Bin nodeSize patchKey entry left right
{-# INLINABLE insertArenaEntry #-}

applyArenaDirect :: Ord key => Map key (Maybe value, Maybe value) -> Map key value -> Map key value
applyArenaDirect arena initialState
  | useArenaPointwiseFinalization arena initialState =
      applyArenaPointwise arena initialState
  | useArenaBulkFinalization arena initialState =
      applyArenaBulk arena initialState
  | otherwise =
      case replayOutputCountCandidate arena initialState of
        Nothing ->
          applyArenaBulk arena initialState
        Just outputCount ->
          case buildReplayMap outputCount initialCursor of
            Just (BuildReplayResult result remaining)
              | drainReplayCursor remaining ->
                  result
            _ ->
              applyArenaBulk arena initialState
  where
    !initialCursor =
      ReplayMergeCursor (ascCursor initialState) (ascCursor arena)
{-# INLINABLE applyArenaDirect #-}

useArenaPointwiseFinalization :: Map key patch -> Map key value -> Bool
useArenaPointwiseFinalization arena initialState =
  Map.size arena <= pageCapacity
    && shouldUseSparse (Map.size initialState) (Map.size arena)
{-# INLINE useArenaPointwiseFinalization #-}

useArenaBulkFinalization :: Map key patch -> Map key value -> Bool
useArenaBulkFinalization arena initialState =
  shouldUseSparse (Map.size initialState) (Map.size arena)
{-# INLINE useArenaBulkFinalization #-}

replayOutputCountCandidate :: Map key (Maybe value, Maybe value) -> Map key value -> Maybe Int
replayOutputCountCandidate arena initialState =
  let !outputCount = Map.size initialState + arenaNetSizeDelta arena
   in if outputCount < 0 then Nothing else Just outputCount
{-# INLINE replayOutputCountCandidate #-}

arenaNetSizeDelta :: Map key (Maybe value, Maybe value) -> Int
arenaNetSizeDelta =
  Map.foldl' addEntry 0
  where
    addEntry :: Int -> (Maybe value, Maybe value) -> Int
    addEntry !total entry =
      total + arenaEntryNetSizeDelta entry
{-# INLINE arenaNetSizeDelta #-}

arenaEntryNetSizeDelta :: (Maybe value, Maybe value) -> Int
arenaEntryNetSizeDelta endpoints =
  case endpoints of
    (Nothing, Just _) ->
      1
    (Just _, Nothing) ->
      -1
    _ ->
      0
{-# INLINE arenaEntryNetSizeDelta #-}

drainReplayCursor :: Ord key => ReplayMergeCursor key value -> Bool
drainReplayCursor cursor =
  case nextReplayOutput cursor of
    Nothing ->
      True
    Just _ ->
      False
{-# INLINABLE drainReplayCursor #-}

buildReplayMap :: Ord key => Int -> ReplayMergeCursor key value -> Maybe (BuildReplayResult key value)
buildReplayMap !outputCount cursor
  | outputCount <= 0 =
      Just (BuildReplayResult MapInternal.Tip cursor)
  | otherwise = do
      let !leftCount = outputCount `quot` 2
          !rightCount = outputCount - leftCount - 1
      BuildReplayResult left afterLeft <- buildReplayMap leftCount cursor
      ReplayOutput key value afterMiddle <- nextReplayOutput afterLeft
      BuildReplayResult right afterRight <- buildReplayMap rightCount afterMiddle
      Just (BuildReplayResult (MapInternal.Bin outputCount key value left right) afterRight)
{-# INLINABLE buildReplayMap #-}

nextReplayOutput :: forall key value. Ord key => ReplayMergeCursor key value -> Maybe (ReplayOutput key value)
nextReplayOutput (ReplayMergeCursor stateCursor arenaCursor) =
  case (stateCursor, arenaCursor) of
    (AscEnd, AscEnd) ->
      Nothing
    (AscCursor stateKey stateValue _ _, AscEnd) ->
      Just (ReplayOutput stateKey stateValue (ReplayMergeCursor (ascAdvance stateCursor) AscEnd))
    (AscEnd, AscCursor arenaKey (_initial, current) _ _) ->
      emitArenaCurrent arenaKey current (ReplayMergeCursor AscEnd (ascAdvance arenaCursor))
    (AscCursor stateKey stateValue _ _, AscCursor arenaKey (_initial, current) _ _) ->
      case compare stateKey arenaKey of
        LT ->
          Just (ReplayOutput stateKey stateValue (ReplayMergeCursor (ascAdvance stateCursor) arenaCursor))
        GT ->
          emitArenaCurrent arenaKey current (ReplayMergeCursor stateCursor (ascAdvance arenaCursor))
        EQ ->
          emitArenaCurrent arenaKey current (ReplayMergeCursor (ascAdvance stateCursor) (ascAdvance arenaCursor))
  where
    emitArenaCurrent :: key -> Maybe value -> ReplayMergeCursor key value -> Maybe (ReplayOutput key value)
    emitArenaCurrent key current nextCursor =
      case current of
        Nothing ->
          nextReplayOutput nextCursor
        Just value ->
          Just (ReplayOutput key value nextCursor)
{-# INLINABLE nextReplayOutput #-}

applyArenaPointwise :: forall key value. Ord key => Map key (Maybe value, Maybe value) -> Map key value -> Map key value
applyArenaPointwise arena initialState =
  Map.foldlWithKey' applyArenaEntry initialState arena
  where
    applyArenaEntry :: Map key value -> key -> (Maybe value, Maybe value) -> Map key value
    applyArenaEntry state key (_initial, current) =
      applyTrustedEdit key current state
{-# INLINABLE applyArenaPointwise #-}

applyArenaBulk :: Ord key => Map key (Maybe value, Maybe value) -> Map key value -> Map key value
applyArenaBulk arena initialState =
  let !insertions = Map.mapMaybe arenaCurrentPresent arena
      !deletions = Map.mapMaybe arenaCurrentAbsent arena
   in Map.union insertions (Map.difference initialState deletions)
  where
    arenaCurrentPresent :: (Maybe value, Maybe value) -> Maybe value
    arenaCurrentPresent (_initial, current) =
      current

    arenaCurrentAbsent :: (Maybe value, Maybe value) -> Maybe ()
    arenaCurrentAbsent (_initial, current) =
      case current of
        Nothing ->
          Just ()
        Just _ ->
          Nothing
{-# INLINABLE applyArenaBulk #-}

foldPatchRows ::
  (result -> key -> Maybe value -> Maybe value -> result) ->
  result ->
  Patch key value ->
  result
foldPatchRows step initial patch =
  case patch of
    SmallPatch cells ->
      goSmall initial cells 0
    PagedPatch _count pages ->
      go initial (cursor pages)
  where
    go !result CursorEnd =
      result
    go !result cursor =
      case currentKey cursor of
        Nothing ->
          result
        Just key ->
          go
            (step result key (beforeMaybe cursor) (afterMaybe cursor))
            (advanceRow cursor)

    goSmall !result cells !index
      | index == sizeofSmallArray cells =
          result
      | otherwise =
          case indexSmallArray cells index of
            Cell key cell ->
              goSmall
                (step result key (cellBefore cell) (cellAfter cell))
                cells
                (index + 1)
{-# INLINABLE foldPatchRows #-}

validateFirstTouches ::
  forall key value.
  (Ord key, Eq value) =>
  Natural ->
  StateProbe key value ->
  Patch key value ->
  Either (ReplayError key value) (StateProbe key value)
validateFirstTouches patchIndex =
  go
  where
    go !probe patch =
      case patch of
        SmallPatch cells ->
          scanSmall probe cells 0
        PagedPatch _count pages ->
          scan probe (cursor pages)

    scan !probe CursorEnd =
      Right probe
    scan !probe cursor =
      case currentKey cursor of
        Nothing ->
          Right probe
        Just key ->
          case validateInitialBefore patchIndex probe key (beforeMaybe cursor) of
            Left failure ->
              Left failure
            Right nextProbe ->
              scan nextProbe (advanceRow cursor)

    scanSmall !probe cells !index
      | index == sizeofSmallArray cells =
          Right probe
      | otherwise =
          case indexSmallArray cells index of
            Cell key cell ->
              case validateInitialBefore patchIndex probe key (cellBefore cell) of
                Left failure ->
                  Left failure
                Right nextProbe ->
                  scanSmall nextProbe cells (index + 1)
{-# INLINABLE validateFirstTouches #-}

validateInitialBefore ::
  forall key value.
  (Ord key, Eq value) =>
  Natural ->
  StateProbe key value ->
  key ->
  Maybe value ->
  Either (ReplayError key value) (StateProbe key value)
validateInitialBefore patchIndex probe key expected =
  let (!actual, !nextProbe) = probeInitialState key probe
   in if expected == actual
        then Right nextProbe
        else
          Left
            ReplayApplyError
              { replayIndex = patchIndex,
                replayApply =
                  ApplyBeforeMismatch
                    { mismatchKey = key,
                      expectedBefore = expected,
                      actualBefore = actual
                    }
              }
{-# INLINABLE validateInitialBefore #-}

replayBoundaryError :: Natural -> key -> Maybe value -> Maybe value -> ReplayError key value
replayBoundaryError patchIndex key olderAfter newerBefore =
  ReplayApplyError
    { replayIndex = patchIndex,
      replayApply =
        ApplyBeforeMismatch
          { mismatchKey = key,
            expectedBefore = newerBefore,
            actualBefore = olderAfter
          }
    }
{-# INLINE replayBoundaryError #-}

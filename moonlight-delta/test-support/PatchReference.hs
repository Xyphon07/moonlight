{-# LANGUAGE ScopedTypeVariables #-}

module PatchReference
  ( compose,
    apply,
    replay,
  )
where

import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Map.Merge.Strict qualified as MapMerge
import Moonlight.Delta.Patch
  ( ApplyError (..),
    CellPatch,
    ComposeError (..),
    PatchKey,
    PatchValue,
    Patch,
    ReplayError (..),
  )
import Moonlight.Delta.Patch qualified as Patch
import Numeric.Natural
  ( Natural,
  )
import Prelude
  ( Either (Left, Right),
    Eq,
    Foldable,
    Maybe (Just, Nothing),
    Ord,
    fmap,
    (+),
    (.),
    (==),
  )

compose ::
  forall key value.
  (PatchKey key, PatchValue value) =>
  Patch key value ->
  Patch key value ->
  Either (ComposeError key value) (Patch key value)
compose newer older =
  fmap (Patch.fromAscList . Map.toAscList)
    ( MapMerge.mergeA
        MapMerge.preserveMissing
        MapMerge.preserveMissing
        (MapMerge.zipWithAMatched composeMatched)
        (Map.fromDistinctAscList (Patch.toAscList newer))
        (Map.fromDistinctAscList (Patch.toAscList older))
    )
  where
    composeMatched ::
      key ->
      CellPatch value ->
      CellPatch value ->
      Either (ComposeError key value) (CellPatch value)
    composeMatched key newerCell olderCell =
      if Patch.cellAfter olderCell == Patch.cellBefore newerCell
        then
          Right
            ( Patch.cellFromEndpoints
                (Patch.cellBefore olderCell)
                (Patch.cellAfter newerCell)
            )
        else
          Left
            ComposeBoundaryMismatch
              { boundaryKey = key,
                olderAfter = Patch.cellAfter olderCell,
                newerBefore = Patch.cellBefore newerCell
              }
{-# INLINABLE compose #-}

apply ::
  forall key value.
  (Ord key, Eq value) =>
  Patch key value ->
  Map key value ->
  Either (ApplyError key value) (Map key value)
apply patch state =
  MapMerge.mergeA
    (MapMerge.traverseMaybeMissing applyToMissing)
    MapMerge.preserveMissing
    (MapMerge.zipWithMaybeAMatched applyToPresent)
    (Map.fromDistinctAscList (Patch.toAscList patch))
    state
  where
    applyToMissing ::
      key ->
      CellPatch value ->
      Either (ApplyError key value) (Maybe value)
    applyToMissing key cell =
      applyCell key cell Nothing

    applyToPresent ::
      key ->
      CellPatch value ->
      value ->
      Either (ApplyError key value) (Maybe value)
    applyToPresent key cell actualValue =
      applyCell key cell (Just actualValue)
{-# INLINABLE apply #-}

replay ::
  forall patches key value.
  (Foldable patches, PatchKey key, PatchValue value) =>
  patches (Patch key value) ->
  Map key value ->
  Either (ReplayError key value) (Map key value)
replay patches initialState =
  case Foldable.foldlM applyStep (0, initialState) patches of
    Left err ->
      Left err
    Right (_nextIndex, state) ->
      Right state
  where
    applyStep ::
      (Natural, Map key value) ->
      Patch key value ->
      Either (ReplayError key value) (Natural, Map key value)
    applyStep (index, state) patch =
      case apply patch state of
        Left err ->
          Left
            ReplayApplyError
              { replayIndex = index,
                replayApply = err
              }
        Right nextState ->
          Right (index + 1, nextState)
{-# INLINABLE replay #-}

applyCell ::
  Eq value =>
  key ->
  CellPatch value ->
  Maybe value ->
  Either (ApplyError key value) (Maybe value)
applyCell key cell actualBefore =
  if Patch.cellBefore cell == actualBefore
    then Right (Patch.cellAfter cell)
    else
      Left
        ApplyBeforeMismatch
          { mismatchKey = key,
            expectedBefore = Patch.cellBefore cell,
            actualBefore = actualBefore
          }
{-# INLINE applyCell #-}


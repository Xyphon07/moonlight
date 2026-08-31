{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Delta.Patch.Internal.Compose.SameSupport
  ( SameSupport,
    reflexiveSupport,
    extendSameSupport,
    sameSupportLatest,
    validateBoundarySameSupport,
    spliceSameSupport,
  )
where

import Control.Monad.ST (ST, runST)
import Moonlight.Delta.Patch.Internal.Builder
import Moonlight.Delta.Patch.Internal.Cell
  ( cellAfterEndpoint,
    cellBeforeEndpoint,
    endpointToMaybe,
  )
import Moonlight.Delta.Patch.Internal.Cursor
import Moonlight.Delta.Patch.Internal.Types
import Prelude

data SameSupport key value = SameSupport !(Patch key value) !(Patch key value)

reflexiveSupport :: Patch key value -> SameSupport key value
reflexiveSupport patch =
  SameSupport patch patch
{-# INLINE reflexiveSupport #-}

sameSupportLatest :: SameSupport key value -> Patch key value
sameSupportLatest (SameSupport _ latest) =
  latest
{-# INLINE sameSupportLatest #-}

extendSameSupport :: SameSupport key value -> SameSupport key value -> SameSupport key value
extendSameSupport (SameSupport first _) (SameSupport _ latest) =
  SameSupport first latest
{-# INLINE extendSameSupport #-}

validateBoundarySameSupport ::
  forall key value error.
  (Ord key, Eq value) =>
  (key -> Maybe value -> Maybe value -> error) ->
  Patch key value ->
  Patch key value ->
  Either error (Maybe (SameSupport key value))
validateBoundarySameSupport makeBoundaryError older newer
  | entryCount older /= entryCount newer =
      Right Nothing
  | otherwise =
      go (cellsForInstance older) (cellsForInstance newer)
  where
    go [] [] =
      Right (Just (SameSupport older newer))
    go [] _ =
      Right Nothing
    go _ [] =
      Right Nothing
    go ((olderKey, olderCell) : olderRest) ((newerKey, newerCell) : newerRest) =
      case compare olderKey newerKey of
        LT -> Right Nothing
        GT -> Right Nothing
        EQ ->
          let !olderAfter = endpointToMaybe (cellAfterEndpoint olderCell)
              !newerBefore = endpointToMaybe (cellBeforeEndpoint newerCell)
           in if olderAfter /= newerBefore
                then Left (makeBoundaryError newerKey olderAfter newerBefore)
                else go olderRest newerRest
{-# INLINABLE validateBoundarySameSupport #-}

spliceSameSupport :: (PatchKey key, PatchValue value) => SameSupport key value -> Patch key value
spliceSameSupport (SameSupport first latest) =
  runST $ do
    builder <- newBuilder
    zipRows builder (cursor (toPagedMap first)) (cursor (toPagedMap latest))
    finishBuilder builder
  where
    zipRows :: (PatchKey key, PatchValue value) => Builder s key value -> Cursor key value -> Cursor key value -> ST s ()
    zipRows _ CursorEnd CursorEnd =
      pure ()
    zipRows builder firstCursor latestCursor =
      case (currentRow firstCursor, currentRow latestCursor) of
        (Just (_firstKey, before, _firstAfter), Just (latestKey, _latestBefore, after)) -> do
          appendTransition builder latestKey before after
          zipRows builder (advanceRow firstCursor) (advanceRow latestCursor)
        _ -> pure ()
{-# INLINABLE spliceSameSupport #-}


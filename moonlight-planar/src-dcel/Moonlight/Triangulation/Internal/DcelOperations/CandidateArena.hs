{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The legalization candidate stack: discipline, growth, and seeding.
module Moonlight.Triangulation.Internal.DcelOperations.CandidateArena
  ( CandidateDiscipline (..)
  , growLegalizationArena
  , seedStarScratch
  , seedGenericPairInArena
  , seedGenericEdges
  ) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Moonlight.Triangulation.Internal.OperationState
  ( LegalizationArena (..)
  , OperationState
  , legalizationArena
  , legalizationArenaLength
  , readScratch
  , storeLegalizationArena
  )
import Moonlight.Triangulation.Internal.PackedIndex (packIndex)

-- | Every normalization epoch is homogeneous. A star epoch turns each edge so
-- the inserted vertex is the opposite apex; a generic epoch consumes the
-- directed edge exactly as seeded. Keeping that fact at the epoch boundary
-- prevents every candidate from carrying and decoding a tag for a distinction
-- that cannot vary inside the stack.
data CandidateDiscipline
  = StarCandidates !Int
  | GenericCandidates

-- | Seed candidates from the scratch arena, returning the new stack top.
-- Transaction-sized preallocation covers the normal path; rare adversarial
-- overflow grows the operation-owned vector without changing LIFO order.
seedStarScratch :: OperationState s -> Int -> Int -> ST s Int
seedStarScratch operation top candidateCount = do
  initialArena <- legalizationArena operation
  arena <- growLegalizationArena initialArena (top + candidateCount)
  when (legalizationArenaLength arena /= legalizationArenaLength initialArena) (storeLegalizationArena operation arena)
  let LegalizationArena values = arena
  forM_ [0 .. candidateCount - 1] $ \index -> do
    edge <- readScratch operation index
    MUV.unsafeWrite values (top + index) (packIndex edge)
  pure (top + candidateCount)

-- | Append the fixed two-edge section produced by one closed hull turn.
-- Materializing @[left, right]@ only to count, zip and traverse it made the
-- dominant sweep rewrite pay list traffic for an arity known by construction.
-- The arena is explicit because a circle sweep borrows it once and glues it
-- back to the operation once, rather than performing three reference lookups
-- around every inserted point.
seedGenericPairInArena
  :: LegalizationArena s
  -> Int
  -> Int
  -> Int
  -> ST s (LegalizationArena s, Int)
seedGenericPairInArena initialArena top left right = do
  let !nextTop = top + 2
  arena <- growLegalizationArena initialArena nextTop
  let LegalizationArena values = arena
  MUV.unsafeWrite values top (packIndex left)
  MUV.unsafeWrite values (top + 1) (packIndex right)
  pure (arena, nextTop)
{-# INLINE seedGenericPairInArena #-}

-- | Seed generic candidates from a list, returning the new stack top.
seedGenericEdges :: OperationState s -> Int -> [Int] -> ST s Int
seedGenericEdges operation top edges = do
  initialArena <- legalizationArena operation
  let !count = length edges
  arena <- growLegalizationArena initialArena (top + count)
  when (legalizationArenaLength arena /= legalizationArenaLength initialArena) (storeLegalizationArena operation arena)
  let LegalizationArena values = arena
  forM_ (zip [0 ..] edges) $ \(!index, !edge) ->
    MUV.unsafeWrite values (top + index) (packIndex edge)
  pure (top + count)

growLegalizationArena :: LegalizationArena s -> Int -> ST s (LegalizationArena s)
growLegalizationArena arena@(LegalizationArena values) required
  | required <= current = pure arena
  | otherwise = LegalizationArena <$> MUV.grow values (max (required - current) (max 1 current))
 where
  !current = MUV.length values
{-# INLINE growLegalizationArena #-}

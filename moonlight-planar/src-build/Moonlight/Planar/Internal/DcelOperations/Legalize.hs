{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | The seeding entry points that drive one legalization epoch.
module Moonlight.Planar.Internal.DcelOperations.Legalize
  ( legalizeScratch
  , legalizeStarEdge
  , legalizeDenseStarEdgeInArena
  , legalizeEdges
  , legalizeEdgesPinned
  , legalizeCavityFanScratch
  ) where

import Moonlight.Planar.BuildStats (BuildMetric (EdgeFlips, LegalizationMaxStack))

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Moonlight.Planar.Internal.DcelOperations.CandidateArena
  ( CandidateDiscipline (..)
  , growLegalizationArena
  , seedGenericEdges
  , seedStarScratch
  )
import Moonlight.Planar.Internal.DcelOperations.FlipRule
  ( LegalizationLaw (..)
  )
import Moonlight.Planar.Internal.DcelOperations.Normalize
  ( LegalizationDrain
  , drainDenseUnconstrainedStarLegalization
  , drainLegalization
  )
import Moonlight.Planar.Internal.Mutable
  ( DenseMutableDcel
  , MutableDcel
  , MutableTopology
  )
import Moonlight.Planar.Internal.OperationState
  ( Counter (..)
  , LegalizationArena (..)
  , OperationState
  , addCounter
  , legalizationArena
  , legalizationArenaLength
  , maxCounter
  , readScratch
  , storeLegalizationArena
  )
import Moonlight.Planar.Internal.PackedIndex (packIndex)
import Moonlight.Planar.Internal.Probe (KnownProbe, Probe (..))

legalizeEdges :: MutableDcel s vertex directed undirected face -> OperationState s -> [Int] -> ST s ()
legalizeEdges mutable operation initial = do
  top <- seedGenericEdges operation 0 initial
  (flips, maxDepth) <- drainLegalization @'ProbeOff mutable operation top GenericCandidates ValidMesh
  addCounter operation (Reported EdgeFlips) flips
  maxCounter operation (Reported LegalizationMaxStack) maxDepth

-- | Legalize the new seam and the source-prefix cavity whose current incident
-- face handles are both unprotected. The selector is evaluated dynamically as
-- flips propagate; it therefore admits the complete local exterior cavity
-- rather than only the zipper's first seeds. The seam owner certifies the
-- resulting cavity boundary after immutable publication.
legalizeEdgesPinned
  :: forall s vertex directed undirected face.
     (Int -> Bool)
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> [Int]
  -> ST s ()
legalizeEdgesPinned targetFaceProtected mutable operation initial = do
  top <- seedGenericEdges operation 0 initial
  (flips, maxDepth) <-
    drainLegalization
      @'ProbeOff
      mutable
      operation
      top
      GenericCandidates
      (SeamPreserving targetFaceProtected)
  addCounter operation (Reported EdgeFlips) flips
  maxCounter operation (Reported LegalizationMaxStack) maxDepth

-- | Repair the fan that fills a removed vertex's hole, draining cavity
-- candidates already written into the operation-owned scratch section. The
-- fan is a valid combinatorial filling but not yet a triangulation — a link
-- polygon that is non-convex at the fan origin yields an inverted triangle —
-- so this drain carries 'CavityRepair' rather than the insertion law: it
-- flips on the incircle determinant alone, and only the fan's own edges may
-- flip. Removal discovers and constructs the cavity inside that same
-- transaction; materializing a list merely to seed the legalization arena
-- would duplicate the local program.
legalizeCavityFanScratch
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> Int
  -> ST s ()
legalizeCavityFanScratch mutable operation cavityFloor scratchOffset candidateCount = do
  initialArena <- legalizationArena operation
  arena <- growLegalizationArena initialArena candidateCount
  when (legalizationArenaLength arena /= legalizationArenaLength initialArena) (storeLegalizationArena operation arena)
  let LegalizationArena values = arena
  traverse_
    (\index -> do
       edge <- readScratch operation (scratchOffset + index)
       MUV.unsafeWrite values index (packIndex edge)
    )
    [0 .. candidateCount - 1]
  (flips, maxDepth) <-
    drainLegalization
      @'ProbeOff
      mutable
      operation
      candidateCount
      GenericCandidates
      (CavityRepair cavityFloor)
  addCounter operation (Reported EdgeFlips) flips
  maxCounter operation (Reported LegalizationMaxStack) maxDepth

legalizeScratch :: forall p s vertex directed undirected face. KnownProbe p => MutableDcel s vertex directed undirected face -> OperationState s -> Int -> Int -> ST s ()
legalizeScratch mutable operation vertex candidateCount = do
  top <- seedStarScratch operation 0 candidateCount
  (flips, maxDepth) <- drainLegalization @p mutable operation top (StarCandidates vertex) ValidMesh
  addCounter operation (Reported EdgeFlips) flips
  maxCounter operation (Reported LegalizationMaxStack) maxDepth

-- | Normalize one newly covered outer edge against the inserted star vertex.
-- The circle sweep always covers exactly one edge; routing that singleton
-- through the shared scratch section merely materializes a one-element range
-- before immediately copying it into this arena. This is the same star-law
-- section written at its actual arity.
legalizeStarEdge
  :: forall p mutable s vertex directed undirected face
   . (KnownProbe p, MutableTopology mutable)
  => mutable s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> ST s (Int, Int)
legalizeStarEdge mutable operation vertex edge = do
  arena <- legalizationArena operation
  let LegalizationArena values = arena
  MUV.unsafeWrite values 0 (packIndex edge)
  drainLegalization @p mutable operation 1 (StarCandidates vertex) ValidMesh
{-# INLINE legalizeStarEdge #-}

-- | The monomorphic fresh-build interpreter for the singleton star epoch,
-- consuming and returning the sweep's borrowed candidate section.
legalizeDenseStarEdgeInArena
  :: DenseMutableDcel s vertex directed undirected face
  -> LegalizationArena s
  -> Int
  -> Int
  -> ST s (LegalizationDrain s ())
legalizeDenseStarEdgeInArena dense arena vertex edge = do
  let LegalizationArena values = arena
  MUV.unsafeWrite values 0 (packIndex edge)
  drainDenseUnconstrainedStarLegalization dense arena 1 vertex
{-# NOINLINE legalizeDenseStarEdgeInArena #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | The normalization procedure of the flip rewrite system.
module Moonlight.Planar.Internal.DcelOperations.Normalize
  ( drainLegalization
  , LegalizationDrain (..)
  , drainDenseUnconstrainedStarLegalization
  , drainDenseUnconstrainedGenericLegalization
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (shiftR)
import Data.STRef (readSTRef)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)
import Moonlight.Planar.Internal.DcelOperations.CandidateArena
  ( CandidateDiscipline (..)
  , growLegalizationArena
  )
import Moonlight.Planar.Internal.DcelOperations.FlipRewrite (applyFlip)
import Moonlight.Planar.Internal.DcelOperations.FlipRule
  ( LegalizationLaw (..)
  , diagonalFires
  , illegalDiagonal
  )
import Moonlight.Planar.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Planar.Internal.Mutable
  ( DenseMutableDcel
  , MutableDcel (..)
  , MutableTopology (..)
  )
import Moonlight.Planar.Internal.OperationState
  (DiagnosticMetric (..),  Counter (..)
  , LegalizationArena (..)
  , OperationState
  , legalizationArena
  , legalizationArenaLength
  , storeLegalizationArena
  )
import Moonlight.Planar.Internal.PackedIndex (packIndex)
import Moonlight.Planar.Internal.Probe
  ( KnownProbe (..)
  , Probe (..)
  , ProbeCounter
  )

-- | The authoritative result of one normalization section. The arena is part
-- of the result because an adversarial frontier may grow it; callers that
-- borrow the section for several local rewrites can therefore compose those
-- rewrites without bouncing through the operation's reference between them.
data LegalizationDrain s counter = LegalizationDrain
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  !counter
  !(LegalizationArena s)

-- | The normalization procedure of a confluent terminating rewrite system, and
-- one canonical legalization engine because a normalization procedure is what
-- it is.
--
-- The objects are the triangulations of a fixed point set. The single rule is
-- the Lawson flip: a locally illegal diagonal is replaced by the other
-- diagonal of its quadrilateral. A normal form is a mesh with no illegal
-- diagonal left to fire on.
--
-- /Termination/ is by the lifted-paraboloid potential. Send each point to
-- @(x, y, x² + y²)@ and read a triangulation as a piecewise-linear surface
-- over the point set; @illegalDiagonal@ is exactly the test that the flip
-- lowers that surface, so every rewrite strictly decreases it, and a finite
-- point set has finitely many triangulations. Exact cocircularity is the one
-- case where the surface does not move — the four lifted points are coplanar
-- and both diagonals give the same surface — so there the potential is the
-- diagonal's own key order, and the rule fires only downward in it. Without
-- that tie-break a cocircular quadrilateral flips forever.
--
-- /Confluence/ is Delaunay's theorem, in its strong form: a triangulation with
-- no locally illegal diagonal is globally Delaunay. Local normality is thus
-- global normality, the normal form is unique, and every rewrite order reaches
-- it. That is what licenses the arena below to be a LIFO stack rather than a
-- priority queue, and it is why callers may seed it in whatever order is
-- cheapest to produce — the fan first, or the hull turns first, or both
-- interleaved — without any of them changing the mesh that comes out.
--
-- All callers differ only in how they seed the arena; topology mutation and
-- propagation have exactly one owner.
-- The stack top, the maximum top, and the flip count are strict loop
-- variables, returned once when the drain finishes — the mesh reports nothing
-- per candidate, and 'applyFlip' reports nothing at all. The phantom
-- 'KnownProbe' parameter counts popped candidates for the instrumented lane
-- and is erased everywhere else.
--
-- A popped candidate is read once. Turning it against the star vertex, judging
-- it, and rewriting it are three questions about the same two half-edge
-- records, and the apex the turn looks for is the apex the judgement needs, so
-- one pass over the quadrilateral answers all three.
drainLegalization
  :: forall p mutable s vertex directed undirected face
   . (KnownProbe p, MutableTopology mutable)
  => mutable s vertex directed undirected face
  -> OperationState s
  -> Int
  -> CandidateDiscipline
  -> LegalizationLaw
  -> ST s (Int, Int)
drainLegalization topology operation seededTop discipline law = do
  let !mutable = topologyOwner topology
  -- No constraint can appear during a drain, so a mesh holding none at entry
  -- never needs the per-candidate protection read.
  constrained <- readSTRef (mdConstraintCount mutable)
  initialArena <- legalizationArena operation
  LegalizationDrain flips maxTop candidates finalArena <-
    drainLegalizationInArena
      @p
      topology
      initialArena
      (constrained /= 0)
      seededTop
      discipline
      law
  when (legalizationArenaLength finalArena /= legalizationArenaLength initialArena) $
    storeLegalizationArena operation finalArena
  probeCharge @p operation (Diagnostic LegalizationCandidates) candidates
  pure (flips, maxTop)
{-# INLINE drainLegalization #-}

-- | Interpret the single normalization law in an explicitly borrowed arena.
-- The boolean is the already-established constraint obstruction: a fresh
-- unconstrained build passes 'False', while the general entry derives it once
-- from the mutable owner above.
drainLegalizationInArena
  :: forall p mutable s vertex directed undirected face
   . (KnownProbe p, MutableTopology mutable)
  => mutable s vertex directed undirected face
  -> LegalizationArena s
  -> Bool
  -> Int
  -> CandidateDiscipline
  -> LegalizationLaw
  -> ST s (LegalizationDrain s (ProbeCounter p))
drainLegalizationInArena topology (LegalizationArena initialArena) guarded seededTop discipline law = do
  let -- The cavity-repair floor pins the fan border. Ordinary legalization
      -- pins nothing; seam legalization instead uses its incident-face
      -- selector below, so both copied and appended pairs obey the same
      -- protection law.
      !floorPair = case law of
        ValidMesh -> 0
        CavityRepair floorEdge -> floorEdge
        SeamPreserving _ -> 0

      eligible rawEdge = do
        unconstrained <-
          if guarded
            then not <$> topologyReadConstraint topology rawEdge
            else pure True
        if not unconstrained
          then pure False
          else
            case law of
              SeamPreserving targetFaceProtected -> do
                leftFace <- topologyReadFace topology rawEdge
                rightFace <- topologyReadFace topology (reverseIndex rawEdge)
                pure
                  ( not (targetFaceProtected leftFace)
                      && not (targetFaceProtected rightFace)
                  )
              _ -> pure (rawEdge `shiftR` 1 >= floorPair)

      loopStar
        :: Int
        -> Double
        -> Double
        -> MUV.MVector s Word32
        -> Int
        -> Int
        -> Int
        -> ProbeCounter p
        -> ST s (Int, Int, ProbeCounter p, MUV.MVector s Word32)
      loopStar !starVertex !starX !starY !arena !top !maxTop !flips !candidates
        | top <= 0 = pure (flips, maxTop, candidates, arena)
        | otherwise = do
            let !nextTop = top - 1
            packedWord <- MUV.unsafeRead arena nextTop
            let !rawEdge = fromIntegral packedWord :: Int
            mayFire <- eligible rawEdge
            if not mayFire
              then loopStar starVertex starX starY arena nextTop maxTop flips (probeBump @p candidates)
              else do
                let !rawTwin = reverseIndex rawEdge
                rawFace <- topologyReadFace topology rawEdge
                rawTwinFace <- topologyReadFace topology rawTwin
                if rawFace == 0 || rawTwinFace == 0
                  then loopStar starVertex starX starY arena nextTop maxTop flips (probeBump @p candidates)
                  else do
                    rawBefore <- topologyReadPrevious topology rawEdge
                    rawTwinBefore <- topologyReadPrevious topology rawTwin
                    rawTwinApex <- topologyReadOrigin topology rawTwinBefore
                    a <- topologyReadOrigin topology rawEdge
                    b <- topologyReadOrigin topology rawTwin
                    ax <- topologyReadPointX topology a
                    ay <- topologyReadPointY topology a
                    bx <- topologyReadPointX topology b
                    by <- topologyReadPointY topology b
                    dx <- topologyReadPointX topology rawTwinApex
                    dy <- topologyReadPointY topology rawTwinApex
                    -- A star candidate is oriented against the inserted apex,
                    -- exactly the premise used by the ordinary insertion
                    -- legalizer. The two convexity predicates in the generic
                    -- valid-mesh law merely re-prove that premise for every
                    -- pop; the in-circle rule alone owns this section.
                    if not (illegalDiagonal ax ay bx by starX starY dx dy)
                      then loopStar starVertex starX starY arena nextTop maxTop flips (probeBump @p candidates)
                      else do
                        -- Star seeding is directional: the inserted vertex is
                        -- the previous-origin apex of every candidate. A flip
                        -- preserves that proof for precisely these two
                        -- directed neighbours, so neither an undirected tag nor
                        -- a rediscovery read belongs in this epoch.
                        rawNext <- topologyReadNext topology rawEdge
                        rawTwinNext <- topologyReadNext topology rawTwin
                        applyFlip
                          topology
                          rawEdge
                          rawTwin
                          rawNext
                          rawBefore
                          rawTwinNext
                          rawTwinBefore
                          rawFace
                          rawTwinFace
                          a
                          b
                          starVertex
                          rawTwinApex
                        let !addedTop = nextTop + 2
                        grown <- growValues arena addedTop
                        MUV.unsafeWrite grown nextTop (packIndex rawTwinBefore)
                        MUV.unsafeWrite grown (nextTop + 1) (packIndex rawTwinNext)
                        loopStar
                          starVertex
                          starX
                          starY
                          grown
                          addedTop
                          (max maxTop addedTop)
                          (flips + 1)
                          (probeBump @p candidates)

      loopGeneric
        :: MUV.MVector s Word32
        -> Int
        -> Int
        -> Int
        -> ProbeCounter p
        -> ST s (Int, Int, ProbeCounter p, MUV.MVector s Word32)
      loopGeneric !arena !top !maxTop !flips !candidates
        | top <= 0 = pure (flips, maxTop, candidates, arena)
        | otherwise = do
            let !nextTop = top - 1
            packedWord <- MUV.unsafeRead arena nextTop
            let !edge = fromIntegral packedWord :: Int
            mayFire <- eligible edge
            if not mayFire
              then loopGeneric arena nextTop maxTop flips (probeBump @p candidates)
              else do
                let !twin = reverseIndex edge
                leftFace <- topologyReadFace topology edge
                rightFace <- topologyReadFace topology twin
                if leftFace == 0 || rightFace == 0
                  then loopGeneric arena nextTop maxTop flips (probeBump @p candidates)
                  else do
                    edgePrevious <- topologyReadPrevious topology edge
                    twinPrevious <- topologyReadPrevious topology twin
                    c <- topologyReadOrigin topology edgePrevious
                    d <- topologyReadOrigin topology twinPrevious
                    a <- topologyReadOrigin topology edge
                    b <- topologyReadOrigin topology twin
                    ax <- topologyReadPointX topology a
                    ay <- topologyReadPointY topology a
                    bx <- topologyReadPointX topology b
                    by <- topologyReadPointY topology b
                    cx <- topologyReadPointX topology c
                    cy <- topologyReadPointY topology c
                    dx <- topologyReadPointX topology d
                    dy <- topologyReadPointY topology d
                    if not (diagonalFires law ax ay bx by cx cy dx dy)
                      then loopGeneric arena nextTop maxTop flips (probeBump @p candidates)
                      else do
                        edgeNext <- topologyReadNext topology edge
                        twinNext <- topologyReadNext topology twin
                        applyFlip
                          topology
                          edge
                          twin
                          edgeNext
                          edgePrevious
                          twinNext
                          twinPrevious
                          leftFace
                          rightFace
                          a
                          b
                          c
                          d
                        let !addedTop = nextTop + 4
                        grown <- growValues arena addedTop
                        MUV.unsafeWrite grown nextTop (packIndex edgeNext)
                        MUV.unsafeWrite grown (nextTop + 1) (packIndex edgePrevious)
                        MUV.unsafeWrite grown (nextTop + 2) (packIndex twinNext)
                        MUV.unsafeWrite grown (nextTop + 3) (packIndex twinPrevious)
                        loopGeneric
                          grown
                          addedTop
                          (max maxTop addedTop)
                          (flips + 1)
                          (probeBump @p candidates)
  drained <-
    case discipline of
      StarCandidates starVertex -> do
        starX <- topologyReadPointX topology starVertex
        starY <- topologyReadPointY topology starVertex
        loopStar starVertex starX starY initialArena seededTop seededTop 0 (probeZero @p)
      GenericCandidates ->
        loopGeneric initialArena seededTop seededTop 0 (probeZero @p)
  let (!flips, !maxTop, !candidates, !finalArena) = drained
  pure (LegalizationDrain flips maxTop candidates (LegalizationArena finalArena))
 where
  growValues :: MUV.MVector s Word32 -> Int -> ST s (MUV.MVector s Word32)
  growValues values required = do
    LegalizationArena grown <-
      growLegalizationArena (LegalizationArena values) required
    pure grown
{-# INLINE drainLegalizationInArena #-}

-- | The dense, fresh-build interpreter for a star epoch. Fresh construction
-- proves the absence of constrained edges; the borrowed arena is returned so
-- the surrounding sweep can glue several epochs before restoring operation
-- ownership.
drainDenseUnconstrainedStarLegalization
  :: DenseMutableDcel s vertex directed undirected face
  -> LegalizationArena s
  -> Int
  -> Int
  -> ST s (LegalizationDrain s ())
drainDenseUnconstrainedStarLegalization dense arena top starVertex =
  drainLegalizationInArena
    @'ProbeOff
    dense
    arena
    False
    top
    (StarCandidates starVertex)
    ValidMesh
{-# INLINE drainDenseUnconstrainedStarLegalization #-}

-- | The matching dense interpreter for a generic legalization epoch. Keeping
-- both monomorphic boundaries out of the circle-sweep worker prevents the
-- normalizer and flip rewrite from being copied into every insertion
-- continuation; both still descend through 'drainLegalizationInArena'.
drainDenseUnconstrainedGenericLegalization
  :: DenseMutableDcel s vertex directed undirected face
  -> LegalizationArena s
  -> Int
  -> ST s (LegalizationDrain s ())
drainDenseUnconstrainedGenericLegalization dense arena top =
  drainLegalizationInArena
    @'ProbeOff
    dense
    arena
    False
    top
    GenericCandidates
    ValidMesh
{-# INLINE drainDenseUnconstrainedGenericLegalization #-}

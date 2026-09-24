{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The firing condition of the flip rule, and the laws restricting it.
module Moonlight.Planar.Internal.DcelOperations.FlipRule
  ( LegalizationLaw (..)
  , diagonalFires
  , illegalDiagonal
  , isFlippableEdge
  ) where

import Control.Monad.ST (ST)
import Moonlight.Planar.Internal.BoundaryCycle (orderedPair)
import Moonlight.Planar.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Planar.Internal.Mutable
  ( MutableDcel
  , readConstraint
  , readFace
  , readOrigin
  , readPointX
  , readPointY
  , readPrevious
  )
import Moonlight.Planar.Internal.Predicates
  ( inCircleCoordinates
  , orient2dCoordinates
  )

-- | Whether the flip rule may fire on a quadrilateral @a b c d@ under a law,
-- where @ab@ is the diagonal and @c@, @d@ the apexes opposite it. The
-- coordinates arrive raw, in the order the mutable accessors carry.
--
-- Both interpreters reach the same incircle rule with different eligibility
-- proofs. 'ValidMesh' arrives from two consistently oriented incident faces;
-- 'CavityRepair' restricts firing to the new fan through its floor witness.
-- The drain reaches this with the quadrilateral already in hand; the firing
-- condition itself has one owner.
diagonalFires
  :: LegalizationLaw
  -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> Bool
diagonalFires law ax ay bx by cx cy dx dy =
  case law of
    -- A valid triangulation already proves the two incident triangles are
    -- consistently oriented. If their union is concave, the opposite apex is
    -- outside the first triangle's circumcircle and the in-circle rule refuses
    -- the flip; re-running two exact orientation predicates merely re-proves
    -- that premise for every candidate. This is the same lawful section used
    -- by Spade's removal legalizer: the diagonal rule alone decides.
    ValidMesh -> illegalDiagonal ax ay bx by cx cy dx dy
    CavityRepair _ -> illegalDiagonal ax ay bx by cx cy dx dy
    SeamPreserving _ -> illegalDiagonal ax ay bx by cx cy dx dy
{-# INLINE diagonalFires #-}

-- | Whether a diagonal is locally illegal — the firing condition of the flip
-- rule @drainLegalization@ normalizes under, and the step at which its
-- potential strictly decreases.
--
-- Lift the quadrilateral to @z = x² + y²@ and the in-circle sign is the
-- orientation of the lifted tetrahedron: 'GT' says the fourth point is below
-- the plane of the other three, so the current diagonal spans a fold the flip
-- pushes downward. 'EQ' says the four lifted points are coplanar and the flip
-- changes nothing about the surface — so the ordering on diagonal keys stands
-- in as the potential, and the rule fires only downward in it. That tie-break
-- is not a convention for picking among equals; it is what stops a cocircular
-- quadrilateral from flipping forever.
illegalDiagonal
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Bool
illegalDiagonal ax ay bx by cx cy dx dy =
  case inCircleCoordinates ax ay bx by cx cy dx dy of
    GT -> True
    LT -> False
    EQ -> orderedPair (cx, cy) (dx, dy) < orderedPair (ax, ay) (bx, by)
{-# INLINE illegalDiagonal #-}

isFlippableEdge :: MutableDcel s vertex directed undirected face -> Int -> ST s Bool
isFlippableEdge mutable edge = do
  protected <- readConstraint mutable edge
  if protected
    then pure False
    else do
      let !reverseEdgeEdge = reverseIndex edge
      leftFace <- readFace mutable edge
      rightFace <- readFace mutable reverseEdgeEdge
      if leftFace == 0 || rightFace == 0
        then pure False
        else do
          edgePrevious <- readPrevious mutable edge
          reverseEdgePrevious <- readPrevious mutable reverseEdgeEdge
          a <- readOrigin mutable edge
          b <- readOrigin mutable reverseEdgeEdge
          c <- readOrigin mutable edgePrevious
          d <- readOrigin mutable reverseEdgePrevious
          ax <- readPointX mutable a
          ay <- readPointY mutable a
          bx <- readPointX mutable b
          by <- readPointY mutable b
          cx <- readPointX mutable c
          cy <- readPointY mutable c
          dx <- readPointX mutable d
          dy <- readPointY mutable d
          pure
            ( orient2dCoordinates cx cy dx dy bx by == GT
                && orient2dCoordinates dx dy cx cy ax ay == GT
            )
{-# INLINE isFlippableEdge #-}

-- | The rewrite strategy: which candidates the flip rule is allowed to fire
-- on. All laws leave the normal form alone — they restrict where the rule may
-- be applied, never what a legal diagonal is.
--
-- 'ValidMesh' legalizes a mesh that is already a triangulation. There a
-- non-convex quadrilateral's diagonal is necessarily locally Delaunay, so the
-- incircle rule itself declines the flip; separately re-running two exact
-- orientation predicates would only re-prove the incident-face premise.
--
-- 'CavityRepair' legalizes the fan that fills a removed vertex's hole, which is
-- not yet a triangulation. That implication fails on an inverted quadrilateral,
-- and the inverted one is exactly the one that must flip, so the short-circuit
-- becomes a refusal to perform the repair. The carried index is the undirected
-- edge count taken before the fan was built: every edge at or above it was
-- appended by the fan, and only those may flip, so a transiently inverted
-- neighbourhood cannot flip the cavity's border away.
--
-- 'SeamPreserving' admits a candidate exactly when neither incident target
-- face handle is selected by the caller. Face handles survive a flip, so this
-- protection is inductive while legalization propagates through the complete
-- unselected seam cavity.
data LegalizationLaw
  = ValidMesh
  | CavityRepair {-# UNPACK #-} !Int
  | SeamPreserving !(Int -> Bool)

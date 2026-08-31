{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The flip rewrite itself, on a quadrilateral.
module Moonlight.Triangulation.Internal.DcelOperations.FlipRewrite
  ( flipEdge
  , applyFlip
  ) where

import Control.Monad (unless)
import Control.Monad.ST (ST)
import Moonlight.Triangulation.Internal.HandleDefs (UndirectedEdgeId (..))
import Moonlight.Triangulation.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Triangulation.Internal.Mutable
  ( MutableDcel
  , MutableTopology (..)
  , payloadsPristine
  , readConstraint
  , readFace
  , readNext
  , readOrigin
  , readPrevious
  , resetEdgeData
  , resetFaceData
  )
import Moonlight.Triangulation.Internal.Types (BuildError (ConstrainedEdgeFlipRefused))

flipEdge :: MutableDcel s vertex directed undirected face -> Int -> ST s (Either BuildError ())
flipEdge mutable edge = do
  protected <- readConstraint mutable edge
  if protected
    then pure (Left (ConstrainedEdgeFlipRefused (UndirectedEdgeId (fromIntegral (edge `quot` 2)))))
    else do
      let !twin = reverseIndex edge
      edgeNext <- readNext mutable edge
      edgePrevious <- readPrevious mutable edge
      twinNext <- readNext mutable twin
      twinPrevious <- readPrevious mutable twin
      leftFace <- readFace mutable edge
      rightFace <- readFace mutable twin
      a <- readOrigin mutable edge
      b <- readOrigin mutable twin
      c <- readOrigin mutable edgePrevious
      d <- readOrigin mutable twinPrevious
      applyFlip mutable edge twin edgeNext edgePrevious twinNext twinPrevious leftFace rightFace a b c d
      pure (Right ())

-- | The rewrite itself, from a quadrilateral the caller already holds. The
-- decision that licenses a flip reads the same two half-edge records the
-- rewrite consumes, so the drain hands its neighbourhood straight here rather
-- than making 'flipEdge' fetch it a second time; 'flipEdge' is that fetch, for
-- callers arriving with nothing but an index.
applyFlip
  :: MutableTopology mutable
  => mutable s vertex directed undirected face
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> ST s ()
applyFlip topology edge twin edgeNext edgePrevious twinNext twinPrevious leftFace rightFace a b c d = do
  let !mutable = topologyOwner topology
  -- Rewrite only fields whose denotation changes. Re-stating both complete
  -- triangles writes the two unchanged face labels and face anchors, then
  -- needlessly redirects the two new diagonal endpoints even though each
  -- already owns another live outgoing edge. The local quadrilateral proof
  -- above names every changed adjacency, so the minimal section is exact.
  topologyWriteOrigin topology edge c
  topologyWriteOrigin topology twin d
  topologyWriteNext topology edgeNext edge
  topologyWritePrevious topology edgeNext twinPrevious
  topologyWriteNext topology edge twinPrevious
  topologyWritePrevious topology edge edgeNext
  topologyWriteNext topology twinPrevious edgeNext
  topologyWritePrevious topology twinPrevious edge
  topologyWriteFace topology twinPrevious leftFace
  topologyWriteFaceEdge topology leftFace edge
  topologyWriteNext topology twinNext twin
  topologyWritePrevious topology twinNext edgePrevious
  topologyWriteNext topology twin edgePrevious
  topologyWritePrevious topology twin twinNext
  topologyWriteNext topology edgePrevious twinNext
  topologyWritePrevious topology edgePrevious twin
  topologyWriteFace topology edgePrevious rightFace
  topologyWriteFaceEdge topology rightFace twin
  -- The diagonal AB is gone and CD stands in its slot; both triangles have
  -- swapped a corner. Three elements changed what they are, so three labels go.
  -- Each reset carries the same test; a site doing several states it once.
  unless (payloadsPristine mutable) $ do
    resetEdgeData mutable (edge `quot` 2)
    resetFaceData mutable leftFace
    resetFaceData mutable rightFace
  topologyWriteVertexOut topology a twinNext
  topologyWriteVertexOut topology b edgeNext
{-# INLINE applyFlip #-}

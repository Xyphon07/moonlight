{-# LANGUAGE BangPatterns #-}

-- | Whole-mesh traversal by identifier range, each range paired with the
-- strict fold that consumes it.
module Moonlight.Planar.Handles.Iterators.FixedIterators
  ( vertices
  , directedEdges
  , undirectedEdges
  , allFaces
  , innerFaces
  , foldVertices'
  , foldDirectedEdges'
  , foldUndirectedEdges'
  , foldAllFaces'
  , foldInnerFaces'
  ) where

import Moonlight.Planar.Dcel
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Types

-- | Every vertex identifier, ascending.
--
-- The enumerators are producers, so their bodies have to reach the consumer's
-- module: a fold over one of these should be a loop over an index, not a walk
-- over cons cells built by a call it could not see into.
vertices :: Triangulation mode vertex directed undirected face -> [VertexId]
vertices triangulation = [VertexId (fromIntegral index) | index <- [0 .. numVertices triangulation - 1]]
{-# INLINE vertices #-}

-- | Every directed-edge identifier, ascending.
directedEdges :: Triangulation mode vertex directed undirected face -> [DirectedEdgeId]
directedEdges triangulation = [DirectedEdgeId (fromIntegral index) | index <- [0 .. numDirectedEdges triangulation - 1]]
{-# INLINE directedEdges #-}

-- | Every undirected-edge identifier, ascending.
undirectedEdges :: Triangulation mode vertex directed undirected face -> [UndirectedEdgeId]
undirectedEdges triangulation = [UndirectedEdgeId (fromIntegral index) | index <- [0 .. numUndirectedEdges triangulation - 1]]
{-# INLINE undirectedEdges #-}

-- | Every face identifier, the outer face first.
allFaces :: Triangulation mode vertex directed undirected face -> [FaceId]
allFaces triangulation = [FaceId (fromIntegral index) | index <- [0 .. numFaces triangulation - 1]]
{-# INLINE allFaces #-}

-- | Every face identifier except the outer face.
innerFaces :: Triangulation mode vertex directed undirected face -> [FaceId]
innerFaces triangulation = [FaceId (fromIntegral index) | index <- [1 .. numFaces triangulation - 1]]
{-# INLINE innerFaces #-}

-- | Strict fold over 'vertices'; the rest of the family follows.
foldVertices' :: Triangulation mode vertex directed undirected face -> (a -> VertexId -> a) -> a -> a
foldVertices' triangulation step = foldRange (numVertices triangulation) (VertexId . fromIntegral) step
{-# INLINE foldVertices' #-}

-- | Strict fold over every directed-edge identifier.
foldDirectedEdges' :: Triangulation mode vertex directed undirected face -> (a -> DirectedEdgeId -> a) -> a -> a
foldDirectedEdges' triangulation step = foldRange (numDirectedEdges triangulation) (DirectedEdgeId . fromIntegral) step
{-# INLINE foldDirectedEdges' #-}

-- | Strict fold over every undirected-edge identifier.
foldUndirectedEdges' :: Triangulation mode vertex directed undirected face -> (a -> UndirectedEdgeId -> a) -> a -> a
foldUndirectedEdges' triangulation step = foldRange (numUndirectedEdges triangulation) (UndirectedEdgeId . fromIntegral) step
{-# INLINE foldUndirectedEdges' #-}

-- | Strict fold over every face, including the outer face.
foldAllFaces' :: Triangulation mode vertex directed undirected face -> (a -> FaceId -> a) -> a -> a
foldAllFaces' triangulation step = foldRange (numFaces triangulation) (FaceId . fromIntegral) step
{-# INLINE foldAllFaces' #-}

-- | Strict fold over every inner face.
foldInnerFaces' :: Triangulation mode vertex directed undirected face -> (a -> FaceId -> a) -> a -> a
foldInnerFaces' triangulation step initial = go 1 initial
 where
  !end = numFaces triangulation
  go !index !accumulator
    | index >= end = accumulator
    | otherwise = go (index + 1) (step accumulator (FaceId (fromIntegral index)))
{-# INLINE foldInnerFaces' #-}

foldRange :: Int -> (Int -> b) -> (a -> b -> a) -> a -> a
foldRange end make step = go 0
 where
  go !index !accumulator
    | index >= end = accumulator
    | otherwise = go (index + 1) (step accumulator (make index))
{-# INLINE foldRange #-}

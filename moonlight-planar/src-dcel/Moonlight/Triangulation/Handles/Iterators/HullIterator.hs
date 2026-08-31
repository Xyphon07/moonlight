-- | The convex hull, walked as its edges or as its vertices.
module Moonlight.Triangulation.Handles.Iterators.HullIterator
  ( hullEdges
  , hullVertices
  , foldHullEdges'
  , foldHullVertices'
  ) where

import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Types

-- | The convex hull as directed edges, in order.
hullEdges :: Triangulation mode vertex directed undirected face -> [DirectedEdgeId]
hullEdges triangulation = faceDirectedEdges triangulation outerFace
{-# INLINE hullEdges #-}

-- | The convex hull as vertices, in order.
hullVertices :: Triangulation mode vertex directed undirected face -> [VertexId]
hullVertices triangulation = map (origin triangulation) (hullEdges triangulation)
{-# INLINE hullVertices #-}

-- | Strict fold over 'hullEdges'.
foldHullEdges' :: Triangulation mode vertex directed undirected face -> (a -> DirectedEdgeId -> a) -> a -> a
foldHullEdges' triangulation = foldFaceDirectedEdges' triangulation outerFace

-- | Strict fold over 'hullVertices'.
foldHullVertices' :: Triangulation mode vertex directed undirected face -> (a -> VertexId -> a) -> a -> a
foldHullVertices' triangulation step = foldHullEdges' triangulation (\acc edge -> step acc (origin triangulation edge))

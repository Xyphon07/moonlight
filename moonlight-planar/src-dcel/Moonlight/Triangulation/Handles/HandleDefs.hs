-- | Opaque compact resident indices and lawful orientation operations. The
-- indices do not encode mesh ownership: use "Moonlight.Triangulation.Handles.Scoped"
-- for local traversal or "Moonlight.Triangulation.Handles.Dynamic" when a handle
-- must escape its mesh scope.
module Moonlight.Triangulation.Handles.HandleDefs
  ( VertexId
  , unVertexId
  , FaceId
  , unFaceId
  , DirectedEdgeId
  , unDirectedEdgeId
  , UndirectedEdgeId
  , unUndirectedEdgeId
  , reverseEdge
  , asUndirected
  , normalizedDirected
  , reversedDirected
  , directedPair
  , isNormalized
  ) where

import Moonlight.Triangulation.Internal.HandleDefs

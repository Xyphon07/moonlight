-- | Opaque compact resident indices and lawful orientation operations. The
-- indices do not encode mesh ownership: use "Moonlight.Planar.Handles.Scoped"
-- for local traversal or "Moonlight.Planar.Handles.Dynamic" when a handle
-- must escape its mesh scope.
module Moonlight.Planar.Handles.HandleDefs
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

import Moonlight.Planar.Internal.HandleDefs

{-# LANGUAGE DataKinds #-}

-- | Zero-copy Voronoi views that retain the owning triangulation.
module Moonlight.Planar.Voronoi.Handles
  ( VoronoiFaceHandle
  , DirectedVoronoiEdgeHandle
  , UndirectedVoronoiEdgeHandle
  , VoronoiVertexHandle
  , voronoiFaceHandle
  , directedVoronoiEdgeHandle
  , undirectedVoronoiEdgeHandle
  , vertexAsVoronoiFaceH
  , directedEdgeAsVoronoiH
  , undirectedEdgeAsVoronoiH
  , innerFaceAsVoronoiVertexH
  , fixVoronoiFace
  , fixDirectedVoronoiEdge
  , fixUndirectedVoronoiEdge
  , fixVoronoiVertex
  , voronoiEdgeReverseH
  , voronoiEdgeNextH
  , voronoiEdgePreviousH
  , voronoiEdgeFromH
  , voronoiEdgeToH
  , voronoiEdgeFaceH
  , voronoiEdgeAsUndirectedH
  , voronoiEdgeAsDelaunayH
  , voronoiFaceSiteH
  , voronoiFaceAdjacentEdgesH
  , voronoiVertexPositionH
  , voronoiVertexOutEdgesH
  , voronoiVertexAsDelaunayFaceH
  , voronoiVertexAsOuterEdgeH
  , voronoiEdgeDirectionH
  , voronoiEdgeGeometryH
  ) where

import Moonlight.Planar.Handles.Dynamic
import Moonlight.Planar.Math (midpoint, negateVector, normalizeVector)
import Moonlight.Planar.Types
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Voronoi

-- | Zero-copy dual handles. Each dual value contains exactly one primal handle,
-- so it cannot accidentally combine an identifier from one triangulation with
-- the storage of another.
newtype VoronoiFaceHandle mode vertex directed undirected face =
  VoronoiFaceHandle (VertexHandle mode vertex directed undirected face)

-- | An owning handle to a directed dual edge.
newtype DirectedVoronoiEdgeHandle mode vertex directed undirected face =
  DirectedVoronoiEdgeHandle (DirectedEdgeHandle mode vertex directed undirected face)

-- | An owning handle to an undirected dual edge.
newtype UndirectedVoronoiEdgeHandle mode vertex directed undirected face =
  UndirectedVoronoiEdgeHandle (UndirectedEdgeHandle mode vertex directed undirected face)

-- | An owning handle to a finite or ideal dual vertex.
data VoronoiVertexHandle mode vertex directed undirected face
  = InnerVoronoiVertexHandle
      !(FaceHandle InnerTag mode vertex directed undirected face)
  | OuterVoronoiVertexHandle
      !(DirectedVoronoiEdgeHandle mode vertex directed undirected face)

instance Show (VoronoiFaceHandle mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixVoronoiFace

instance Show (DirectedVoronoiEdgeHandle mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixDirectedVoronoiEdge

instance Show (UndirectedVoronoiEdgeHandle mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixUndirectedVoronoiEdge

instance Show (VoronoiVertexHandle mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixVoronoiVertex

-- | Admit a fixed Voronoi face identifier into a triangulation.
voronoiFaceHandle
  :: Triangulation mode vertex directed undirected face
  -> VoronoiFaceId
  -> Maybe (VoronoiFaceHandle mode vertex directed undirected face)
voronoiFaceHandle triangulation (VoronoiFaceId site) =
  VoronoiFaceHandle <$> vertexHandle triangulation site

-- | View an admitted primal vertex as its Voronoi face.
vertexAsVoronoiFaceH
  :: VertexHandle mode vertex directed undirected face
  -> VoronoiFaceHandle mode vertex directed undirected face
vertexAsVoronoiFaceH = VoronoiFaceHandle

-- | Admit a fixed directed Voronoi edge identifier into a triangulation.
directedVoronoiEdgeHandle
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> Maybe (DirectedVoronoiEdgeHandle mode vertex directed undirected face)
directedVoronoiEdgeHandle triangulation (DirectedVoronoiEdgeId edge) =
  DirectedVoronoiEdgeHandle <$> directedEdgeHandle triangulation edge

-- | View an admitted primal directed edge in the dual handle family.
directedEdgeAsVoronoiH
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> DirectedVoronoiEdgeHandle mode vertex directed undirected face
directedEdgeAsVoronoiH = DirectedVoronoiEdgeHandle

-- | Admit a fixed undirected Voronoi edge identifier into a triangulation.
undirectedVoronoiEdgeHandle
  :: Triangulation mode vertex directed undirected face
  -> UndirectedVoronoiEdgeId
  -> Maybe (UndirectedVoronoiEdgeHandle mode vertex directed undirected face)
undirectedVoronoiEdgeHandle triangulation (UndirectedVoronoiEdgeId edge) =
  UndirectedVoronoiEdgeHandle <$> undirectedEdgeHandle triangulation edge

-- | View an admitted primal undirected edge in the dual handle family.
undirectedEdgeAsVoronoiH
  :: UndirectedEdgeHandle mode vertex directed undirected face
  -> UndirectedVoronoiEdgeHandle mode vertex directed undirected face
undirectedEdgeAsVoronoiH = UndirectedVoronoiEdgeHandle

-- | View an admitted bounded primal face as a finite dual vertex.
innerFaceAsVoronoiVertexH
  :: FaceHandle InnerTag mode vertex directed undirected face
  -> VoronoiVertexHandle mode vertex directed undirected face
innerFaceAsVoronoiVertexH = InnerVoronoiVertexHandle

-- | Forget ownership and recover a fixed Voronoi face identifier.
fixVoronoiFace :: VoronoiFaceHandle mode vertex directed undirected face -> VoronoiFaceId
fixVoronoiFace (VoronoiFaceHandle site) = VoronoiFaceId (fixVertex site)
{-# INLINE fixVoronoiFace #-}

-- | Forget ownership and recover a fixed directed dual edge identifier.
fixDirectedVoronoiEdge
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
fixDirectedVoronoiEdge (DirectedVoronoiEdgeHandle edge) =
  DirectedVoronoiEdgeId (fixDirectedEdge edge)
{-# INLINE fixDirectedVoronoiEdge #-}

-- | Forget ownership and recover a fixed undirected dual edge identifier.
fixUndirectedVoronoiEdge
  :: UndirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> UndirectedVoronoiEdgeId
fixUndirectedVoronoiEdge (UndirectedVoronoiEdgeHandle edge) =
  UndirectedVoronoiEdgeId (fixUndirectedEdge edge)
{-# INLINE fixUndirectedVoronoiEdge #-}

-- | Forget ownership and recover a fixed dual vertex identifier.
fixVoronoiVertex
  :: VoronoiVertexHandle mode vertex directed undirected face
  -> VoronoiVertexId
fixVoronoiVertex (InnerVoronoiVertexHandle face) =
  InnerVoronoiVertex (fixedFaceId (fixFace face))
fixVoronoiVertex (OuterVoronoiVertexHandle edge) =
  OuterVoronoiVertex (fixDirectedVoronoiEdge edge)
{-# INLINE fixVoronoiVertex #-}

-- | Reverse an admitted directed dual edge.
voronoiEdgeReverseH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> DirectedVoronoiEdgeHandle mode vertex directed undirected face
voronoiEdgeReverseH (DirectedVoronoiEdgeHandle edge) =
  DirectedVoronoiEdgeHandle (directedEdgeReverse edge)

-- | Next admitted dual edge around its Voronoi face.
voronoiEdgeNextH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> DirectedVoronoiEdgeHandle mode vertex directed undirected face
voronoiEdgeNextH (DirectedVoronoiEdgeHandle edge) =
  DirectedVoronoiEdgeHandle (directedEdgeCounterClockwise edge)

-- | Previous admitted dual edge around its Voronoi face.
voronoiEdgePreviousH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> DirectedVoronoiEdgeHandle mode vertex directed undirected face
voronoiEdgePreviousH (DirectedVoronoiEdgeHandle edge) =
  DirectedVoronoiEdgeHandle (directedEdgeClockwise edge)

-- | Origin finite or ideal vertex of an admitted dual edge.
voronoiEdgeFromH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> VoronoiVertexHandle mode vertex directed undirected face
voronoiEdgeFromH edge@(DirectedVoronoiEdgeHandle primal) =
  case faceAsInner (directedEdgeFace primal) of
    Just inner -> InnerVoronoiVertexHandle inner
    Nothing -> OuterVoronoiVertexHandle edge

-- | Destination finite or ideal vertex of an admitted dual edge.
voronoiEdgeToH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> VoronoiVertexHandle mode vertex directed undirected face
voronoiEdgeToH = voronoiEdgeFromH . voronoiEdgeReverseH

-- | Voronoi face to the left of an admitted directed dual edge.
voronoiEdgeFaceH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> VoronoiFaceHandle mode vertex directed undirected face
voronoiEdgeFaceH (DirectedVoronoiEdgeHandle edge) =
  VoronoiFaceHandle (directedEdgeFrom edge)

-- | Forget orientation while retaining ownership.
voronoiEdgeAsUndirectedH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> UndirectedVoronoiEdgeHandle mode vertex directed undirected face
voronoiEdgeAsUndirectedH (DirectedVoronoiEdgeHandle edge) =
  UndirectedVoronoiEdgeHandle (directedEdgeAsUndirected edge)

-- | Recover the owning primal directed-edge handle.
voronoiEdgeAsDelaunayH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> DirectedEdgeHandle mode vertex directed undirected face
voronoiEdgeAsDelaunayH (DirectedVoronoiEdgeHandle edge) = edge

-- | Recover the owning primal site handle.
voronoiFaceSiteH
  :: VoronoiFaceHandle mode vertex directed undirected face
  -> VertexHandle mode vertex directed undirected face
voronoiFaceSiteH (VoronoiFaceHandle site) = site

-- | Admitted directed boundary of a Voronoi face.
voronoiFaceAdjacentEdgesH
  :: VoronoiFaceHandle mode vertex directed undirected face
  -> [DirectedVoronoiEdgeHandle mode vertex directed undirected face]
voronoiFaceAdjacentEdgesH (VoronoiFaceHandle site) =
  map DirectedVoronoiEdgeHandle (vertexHandleOutEdges site)

-- | Position of a finite admitted dual vertex.
voronoiVertexPositionH
  :: VoronoiVertexHandle mode vertex directed undirected face
  -> Maybe (Point)
voronoiVertexPositionH (InnerVoronoiVertexHandle face) = innerFaceCircumcenter face
voronoiVertexPositionH (OuterVoronoiVertexHandle _) = Nothing

-- | Directed dual edges leaving a finite admitted dual vertex.
voronoiVertexOutEdgesH
  :: VoronoiVertexHandle mode vertex directed undirected face
  -> Maybe [DirectedVoronoiEdgeHandle mode vertex directed undirected face]
voronoiVertexOutEdgesH (InnerVoronoiVertexHandle face) =
  Just (map DirectedVoronoiEdgeHandle (faceAdjacentEdges face))
voronoiVertexOutEdgesH (OuterVoronoiVertexHandle _) = Nothing

-- | Recover the primal bounded face underlying a finite dual vertex.
voronoiVertexAsDelaunayFaceH
  :: VoronoiVertexHandle mode vertex directed undirected face
  -> Maybe (FaceHandle InnerTag mode vertex directed undirected face)
voronoiVertexAsDelaunayFaceH (InnerVoronoiVertexHandle face) = Just face
voronoiVertexAsDelaunayFaceH (OuterVoronoiVertexHandle _) = Nothing

-- | Recover the primal hull edge underlying an ideal dual vertex.
voronoiVertexAsOuterEdgeH
  :: VoronoiVertexHandle mode vertex directed undirected face
  -> Maybe (DirectedVoronoiEdgeHandle mode vertex directed undirected face)
voronoiVertexAsOuterEdgeH (InnerVoronoiVertexHandle _) = Nothing
voronoiVertexAsOuterEdgeH (OuterVoronoiVertexHandle edge) = Just edge

-- | Unit direction of the dual edge orthogonal to the primal edge.
voronoiEdgeDirectionH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> Point
voronoiEdgeDirectionH (DirectedVoronoiEdgeHandle primal) =
  let (Point ax ay, Point bx by) = directedEdgePositions primal
   in Point (ay - by) (bx - ax)

-- | Geometric realization of an admitted dual edge.
voronoiEdgeGeometryH
  :: DirectedVoronoiEdgeHandle mode vertex directed undirected face
  -> Maybe (VoronoiEdgeGeometry)
voronoiEdgeGeometryH edge@(DirectedVoronoiEdgeHandle primal) =
  case (voronoiEdgeFromH edge, voronoiEdgeToH edge) of
    (InnerVoronoiVertexHandle fromFace, InnerVoronoiVertexHandle toFace) ->
      VoronoiSegment <$> innerFaceCircumcenter fromFace <*> innerFaceCircumcenter toFace
    (InnerVoronoiVertexHandle fromFace, OuterVoronoiVertexHandle _) -> do
      start <- innerFaceCircumcenter fromFace
      pure (VoronoiRay start (normalizeVector (voronoiEdgeDirectionH edge)))
    (OuterVoronoiVertexHandle _, InnerVoronoiVertexHandle toFace) -> do
      end <- innerFaceCircumcenter toFace
      pure (VoronoiRay end (normalizeVector (negateVector (voronoiEdgeDirectionH edge))))
    (OuterVoronoiVertexHandle _, OuterVoronoiVertexHandle _) ->
      let (from, to) = directedEdgePositions primal
       in Just (VoronoiLine (midpoint from to) (normalizeVector (voronoiEdgeDirectionH edge)))

-- The handle verbs cross the same component boundary as the fixed-index ones
-- and carry their unfoldings for the same reason. 'voronoiVertexPositionH'
-- reaches its arithmetic through 'innerFaceCircumcenter', whose own worker
-- publishes no unfolding, so it stops at that call until the dcel layer says
-- otherwise.

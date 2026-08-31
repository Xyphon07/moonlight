{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Voronoi dual, addressed by its own handle family: every cell, edge and
-- vertex is a view of the Delaunay mesh rather than a second structure.
module Moonlight.Triangulation.Voronoi
  ( VoronoiFaceId (..)
  , DirectedVoronoiEdgeId (..)
  , UndirectedVoronoiEdgeId (..)
  , VoronoiVertexId (..)
  , VoronoiEdgeGeometry (..)
  , voronoiFaces
  , directedVoronoiEdges
  , undirectedVoronoiEdges
  , asDelaunayDirectedEdge
  , asDelaunayUndirectedEdge
  , asDirectedVoronoiEdge
  , asUndirectedVoronoiEdge
  , reverseVoronoiEdge
  , voronoiNext
  , voronoiPrevious
  , voronoiFrom
  , voronoiTo
  , voronoiIncidentFace
  , voronoiFaceSite
  , voronoiFaceAdjacentEdges
  , voronoiVertexPosition
  , voronoiVertexOutgoingEdges
  , voronoiDirectionVector
  , voronoiEdgeGeometry
  , faceCircumcenter
  ) where

import Control.DeepSeq (NFData)
import Moonlight.Triangulation.Dcel
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (directedEdges, undirectedEdges, vertices)
import Moonlight.Triangulation.Math
import Moonlight.Triangulation.Types

-- | A Voronoi face, identified by its primal site.
--
-- The dual uses the exact same fixed-index space as the primal DCEL. These
-- newtypes add semantic separation without allocating or owning any dual mesh.
newtype VoronoiFaceId = VoronoiFaceId { unVoronoiFaceId :: VertexId }
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | An oriented Voronoi edge, identified by its primal directed edge.
newtype DirectedVoronoiEdgeId = DirectedVoronoiEdgeId { unDirectedVoronoiEdgeId :: DirectedEdgeId }
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | An unoriented Voronoi edge, identified by its primal undirected edge.
newtype UndirectedVoronoiEdgeId = UndirectedVoronoiEdgeId { unUndirectedVoronoiEdgeId :: UndirectedEdgeId }
  deriving stock (Show)
  deriving newtype (Eq, Ord, NFData)

-- | A finite dual vertex or an ideal endpoint of an unbounded dual edge.
data VoronoiVertexId
  = InnerVoronoiVertex !FaceId
  | OuterVoronoiVertex !DirectedVoronoiEdgeId
  deriving stock (Eq, Ord, Show)

-- | Finite segment, half-infinite ray, or infinite line in the dual.
data VoronoiEdgeGeometry
  = VoronoiSegment !(Point) !(Point)
  | VoronoiRay !(Point) !(Point)
  | VoronoiLine !(Point) !(Point)
  deriving stock (Eq, Ord, Show)

-- | Voronoi faces in primal vertex-handle order.
voronoiFaces :: Triangulation mode vertex directed undirected face -> [VoronoiFaceId]
voronoiFaces triangulation = map VoronoiFaceId (vertices triangulation)
{-# INLINE voronoiFaces #-}

-- | Directed Voronoi edges in primal directed-edge order.
--
-- Every primal directed edge is one directed edge in the dual. Boundary
-- handles naturally represent half-infinite edges through OuterVoronoiVertex.
directedVoronoiEdges :: Triangulation mode vertex directed undirected face -> [DirectedVoronoiEdgeId]
directedVoronoiEdges triangulation = map DirectedVoronoiEdgeId (directedEdges triangulation)
{-# INLINE directedVoronoiEdges #-}

-- | Undirected Voronoi edges in primal undirected-edge order.
undirectedVoronoiEdges :: Triangulation mode vertex directed undirected face -> [UndirectedVoronoiEdgeId]
undirectedVoronoiEdges triangulation = map UndirectedVoronoiEdgeId (undirectedEdges triangulation)
{-# INLINE undirectedVoronoiEdges #-}

-- | Recover the primal directed edge.
asDelaunayDirectedEdge :: DirectedVoronoiEdgeId -> DirectedEdgeId
asDelaunayDirectedEdge = unDirectedVoronoiEdgeId
{-# INLINE asDelaunayDirectedEdge #-}

-- | Recover the primal undirected edge.
asDelaunayUndirectedEdge :: UndirectedVoronoiEdgeId -> UndirectedEdgeId
asDelaunayUndirectedEdge = unUndirectedVoronoiEdgeId
{-# INLINE asDelaunayUndirectedEdge #-}

-- | View a primal directed edge in the dual handle family.
asDirectedVoronoiEdge :: DirectedEdgeId -> DirectedVoronoiEdgeId
asDirectedVoronoiEdge = DirectedVoronoiEdgeId
{-# INLINE asDirectedVoronoiEdge #-}

-- | View a primal undirected edge in the dual handle family.
asUndirectedVoronoiEdge :: UndirectedEdgeId -> UndirectedVoronoiEdgeId
asUndirectedVoronoiEdge = UndirectedVoronoiEdgeId
{-# INLINE asUndirectedVoronoiEdge #-}

-- | Reverse a directed Voronoi edge.
reverseVoronoiEdge :: DirectedVoronoiEdgeId -> DirectedVoronoiEdgeId
reverseVoronoiEdge (DirectedVoronoiEdgeId edge) = DirectedVoronoiEdgeId (reverseEdge edge)
{-# INLINE reverseVoronoiEdge #-}

-- | Next dual edge around the incident Voronoi face.
--
-- Dual next/previous rotate around the primal origin site.
voronoiNext
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> DirectedVoronoiEdgeId
voronoiNext triangulation (DirectedVoronoiEdgeId edge) =
  DirectedVoronoiEdgeId (counterClockwise triangulation edge)
{-# INLINE voronoiNext #-}

-- | Previous dual edge around the incident Voronoi face.
voronoiPrevious
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> DirectedVoronoiEdgeId
voronoiPrevious triangulation (DirectedVoronoiEdgeId edge) =
  DirectedVoronoiEdgeId (clockwise triangulation edge)
{-# INLINE voronoiPrevious #-}

-- | Origin dual vertex, including ideal endpoints on the hull.
voronoiFrom
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> VoronoiVertexId
voronoiFrom triangulation edge@(DirectedVoronoiEdgeId primal)
  | face == outerFace = OuterVoronoiVertex edge
  | otherwise = InnerVoronoiVertex face
 where
  face = incidentFace triangulation primal
{-# INLINE voronoiFrom #-}

-- | Destination dual vertex, including ideal endpoints on the hull.
voronoiTo
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> VoronoiVertexId
voronoiTo triangulation = voronoiFrom triangulation . reverseVoronoiEdge
{-# INLINE voronoiTo #-}

-- | Voronoi face to the left of a directed dual edge.
voronoiIncidentFace
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> VoronoiFaceId
voronoiIncidentFace triangulation (DirectedVoronoiEdgeId edge) =
  VoronoiFaceId (origin triangulation edge)
{-# INLINE voronoiIncidentFace #-}

-- | Primal site represented by a Voronoi face.
voronoiFaceSite :: VoronoiFaceId -> VertexId
voronoiFaceSite = unVoronoiFaceId
{-# INLINE voronoiFaceSite #-}

-- | Directed boundary of a Voronoi face.
voronoiFaceAdjacentEdges
  :: Triangulation mode vertex directed undirected face
  -> VoronoiFaceId
  -> [DirectedVoronoiEdgeId]
voronoiFaceAdjacentEdges triangulation (VoronoiFaceId site) =
  map DirectedVoronoiEdgeId (vertexOutgoingEdges triangulation site)
{-# INLINE voronoiFaceAdjacentEdges #-}

-- | Position of a finite dual vertex; ideal hull endpoints have no position.
--
-- A caller reaches this through 'voronoiFrom' or 'voronoiTo', which build the
-- endpoint sum immediately before it is taken apart again. Only an unfolding at
-- the consumer lets the two meet, so the constructor never reaches the heap.
voronoiVertexPosition
  :: Triangulation mode vertex directed undirected face
  -> VoronoiVertexId
  -> Maybe (Point)
voronoiVertexPosition triangulation vertex = case vertex of
  InnerVoronoiVertex face -> faceCircumcenter triangulation face
  OuterVoronoiVertex _ -> Nothing
{-# INLINE voronoiVertexPosition #-}

-- | Directed dual edges leaving a finite dual vertex.
voronoiVertexOutgoingEdges
  :: Triangulation mode vertex directed undirected face
  -> VoronoiVertexId
  -> Maybe [DirectedVoronoiEdgeId]
voronoiVertexOutgoingEdges triangulation vertex = case vertex of
  OuterVoronoiVertex _ -> Nothing
  InnerVoronoiVertex face ->
    map DirectedVoronoiEdgeId . faceDirectedEdges triangulation <$> nonOuter face
 where
  nonOuter face
    | face == outerFace = Nothing
    | otherwise = Just face
{-# INLINE voronoiVertexOutgoingEdges #-}

-- | Unit direction of the dual edge orthogonal to a primal edge.
voronoiDirectionVector
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> Point
voronoiDirectionVector triangulation (DirectedVoronoiEdgeId edge) =
  case vertexPoint triangulation (origin triangulation edge) of
    Point ax ay -> case vertexPoint triangulation (destination triangulation edge) of
      Point bx by -> Point (ay - by) (bx - ax)

-- | Circumcenter of a bounded primal face.
--
-- The circumcentre stands on the absolute vertex positions, which is a
-- different value from the query-relative one the Sibson pipeline caches: that
-- one rescales the differences it was handed, so translating the inputs moves
-- the rounding and the two do not differ by the translation. The interpolation
-- workspace's plane therefore cannot serve this function, and the repetition
-- here is across calls rather than within one — a sweep recomputes each face
-- once per incident dual edge, while a single call touches two distinct faces.
-- That threefold repetition is what a dual which is a view rather than a
-- structure costs, and the referent pays it identically, so no cache is owed.
faceCircumcenter
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> Maybe (Point)
faceCircumcenter triangulation face
  | face == outerFace = Nothing
  | otherwise = case adjacentEdge triangulation face of
      Nothing -> Nothing
      Just e0 ->
        let !e1 = next triangulation e0
            !e2 = next triangulation e1
         in if next triangulation e2 /= e0
              then Nothing
              else
                circumcenter
                  (vertexPoint triangulation (origin triangulation e0))
                  (vertexPoint triangulation (origin triangulation e1))
                  (vertexPoint triangulation (origin triangulation e2))
{-# INLINE faceCircumcenter #-}

-- | Geometric realization of one dual edge.
--
-- The endpoint classification 'voronoiFrom' and 'voronoiTo' publish is two face
-- reads and two comparisons; taken through those observations it is also two
-- sum values built and immediately scrutinized. The geometry reads the faces
-- itself so that the classification stays in registers.
voronoiEdgeGeometry
  :: Triangulation mode vertex directed undirected face
  -> DirectedVoronoiEdgeId
  -> Maybe (VoronoiEdgeGeometry)
voronoiEdgeGeometry triangulation edge@(DirectedVoronoiEdgeId primal)
  | innerFrom, innerTo =
      VoronoiSegment <$> faceCircumcenter triangulation fromFace <*> faceCircumcenter triangulation toFace
  | innerFrom = do
      start <- faceCircumcenter triangulation fromFace
      pure (VoronoiRay start (normalizeVector (voronoiDirectionVector triangulation edge)))
  | innerTo = do
      end <- faceCircumcenter triangulation toFace
      pure (VoronoiRay end (normalizeVector (negateVector (voronoiDirectionVector triangulation edge))))
  | otherwise =
      let !center = midpoint
            (vertexPoint triangulation (origin triangulation primal))
            (vertexPoint triangulation (destination triangulation primal))
       in Just (VoronoiLine center (normalizeVector (voronoiDirectionVector triangulation edge)))
 where
  !fromFace = incidentFace triangulation primal
  !toFace = incidentFace triangulation (reverseEdge primal)
  !innerFrom = fromFace /= outerFace
  !innerTo = toFace /= outerFace

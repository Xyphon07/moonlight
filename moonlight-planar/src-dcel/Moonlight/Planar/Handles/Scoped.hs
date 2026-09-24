{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Zero-cost mesh-scoped identifiers. A scope is introduced only by
-- 'withScopedTriangulation', so an identifier from one mesh cannot be supplied
-- to observations of another mesh. Use owning handles when a handle must
-- escape the continuation.
module Moonlight.Planar.Handles.Scoped
  ( ScopedTriangulation
  , ScopedVertex
  , ScopedDirectedEdge
  , ScopedUndirectedEdge
  , ScopedFace
  , withScopedTriangulation
  , scopedVertices
  , scopedDirectedEdges
  , scopedUndirectedEdges
  , scopedFaces
  , scopedInnerFaces
  , scopedOuterFace
  , scopedVertexPoint
  , scopedVertexData
  , scopedDirectedEdgeData
  , scopedUndirectedEdgeData
  , scopedFaceData
  , scopedVertexOutEdge
  , scopedAdjacentEdge
  , scopedOrigin
  , scopedDestination
  , scopedReverseEdge
  , scopedAsUndirected
  , scopedNext
  , scopedPrevious
  , scopedIncidentFace
  , scopedIsOuterDirectedEdge
  , scopedIsBoundaryEdge
  , scopedIsConstraintEdge
  , scopedUndirectedEndpoints
  , scopedFaceDirectedEdges
  , scopedFaceVertices
  , scopedVertexOutgoingEdges
  ) where

import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Handles.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  , asUndirected
  , reverseEdge
  )
import qualified Moonlight.Planar.Handles.Iterators.FixedIterators as Fixed
import Moonlight.Planar.Point (Point)
import Moonlight.Planar.Types (Triangulation)

type role ScopedTriangulation nominal nominal representational representational representational representational
newtype ScopedTriangulation scope mode vertex directed undirected face =
  ScopedTriangulation (Triangulation mode vertex directed undirected face)

type role ScopedVertex nominal
newtype ScopedVertex scope = ScopedVertex VertexId
  deriving stock (Eq, Ord, Show)

type role ScopedDirectedEdge nominal
newtype ScopedDirectedEdge scope = ScopedDirectedEdge DirectedEdgeId
  deriving stock (Eq, Ord, Show)

type role ScopedUndirectedEdge nominal
newtype ScopedUndirectedEdge scope = ScopedUndirectedEdge UndirectedEdgeId
  deriving stock (Eq, Ord, Show)

type role ScopedFace nominal
newtype ScopedFace scope = ScopedFace FaceId
  deriving stock (Eq, Ord, Show)

-- | Introduce one nominal scope for all identifiers derived from this mesh.
withScopedTriangulation
  :: Triangulation mode vertex directed undirected face
  -> (forall scope. ScopedTriangulation scope mode vertex directed undirected face -> result)
  -> result
withScopedTriangulation triangulation use = use (ScopedTriangulation triangulation)
{-# INLINE withScopedTriangulation #-}

scopedVertices
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> [ScopedVertex scope]
scopedVertices (ScopedTriangulation triangulation) =
  fmap ScopedVertex (Fixed.vertices triangulation)

scopedDirectedEdges
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> [ScopedDirectedEdge scope]
scopedDirectedEdges (ScopedTriangulation triangulation) =
  fmap ScopedDirectedEdge (Fixed.directedEdges triangulation)

scopedUndirectedEdges
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> [ScopedUndirectedEdge scope]
scopedUndirectedEdges (ScopedTriangulation triangulation) =
  fmap ScopedUndirectedEdge (Fixed.undirectedEdges triangulation)

scopedFaces
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> [ScopedFace scope]
scopedFaces (ScopedTriangulation triangulation) =
  fmap ScopedFace (Fixed.allFaces triangulation)

scopedInnerFaces
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> [ScopedFace scope]
scopedInnerFaces (ScopedTriangulation triangulation) =
  fmap ScopedFace (Fixed.innerFaces triangulation)

scopedOuterFace
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedFace scope
scopedOuterFace _ = ScopedFace Dcel.outerFace
{-# INLINE scopedOuterFace #-}

scopedVertexPoint
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedVertex scope
  -> Point
scopedVertexPoint (ScopedTriangulation triangulation) (ScopedVertex vertex) =
  Dcel.vertexPoint triangulation vertex
{-# INLINE scopedVertexPoint #-}

scopedVertexData
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedVertex scope
  -> vertex
scopedVertexData (ScopedTriangulation triangulation) (ScopedVertex vertex) =
  Dcel.vertexData triangulation vertex
{-# INLINE scopedVertexData #-}

scopedDirectedEdgeData
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedDirectedEdge scope
  -> directed
scopedDirectedEdgeData (ScopedTriangulation triangulation) (ScopedDirectedEdge edge) =
  Dcel.directedEdgeData triangulation edge
{-# INLINE scopedDirectedEdgeData #-}

scopedUndirectedEdgeData
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedUndirectedEdge scope
  -> undirected
scopedUndirectedEdgeData (ScopedTriangulation triangulation) (ScopedUndirectedEdge edge) =
  Dcel.undirectedEdgeData triangulation edge
{-# INLINE scopedUndirectedEdgeData #-}

scopedFaceData
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedFace scope
  -> face
scopedFaceData (ScopedTriangulation triangulation) (ScopedFace face) =
  Dcel.faceData triangulation face
{-# INLINE scopedFaceData #-}

scopedVertexOutEdge
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedVertex scope
  -> Maybe (ScopedDirectedEdge scope)
scopedVertexOutEdge (ScopedTriangulation triangulation) (ScopedVertex vertex) =
  ScopedDirectedEdge <$> Dcel.vertexOutEdge triangulation vertex

scopedAdjacentEdge
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedFace scope
  -> Maybe (ScopedDirectedEdge scope)
scopedAdjacentEdge (ScopedTriangulation triangulation) (ScopedFace face) =
  ScopedDirectedEdge <$> Dcel.adjacentEdge triangulation face

scopedOrigin
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedDirectedEdge scope
  -> ScopedVertex scope
scopedOrigin (ScopedTriangulation triangulation) (ScopedDirectedEdge edge) =
  ScopedVertex (Dcel.origin triangulation edge)
{-# INLINE scopedOrigin #-}

scopedDestination
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedDirectedEdge scope
  -> ScopedVertex scope
scopedDestination (ScopedTriangulation triangulation) (ScopedDirectedEdge edge) =
  ScopedVertex (Dcel.destination triangulation edge)
{-# INLINE scopedDestination #-}

scopedReverseEdge :: ScopedDirectedEdge scope -> ScopedDirectedEdge scope
scopedReverseEdge (ScopedDirectedEdge edge) = ScopedDirectedEdge (reverseEdge edge)
{-# INLINE scopedReverseEdge #-}

scopedAsUndirected :: ScopedDirectedEdge scope -> ScopedUndirectedEdge scope
scopedAsUndirected (ScopedDirectedEdge edge) = ScopedUndirectedEdge (asUndirected edge)
{-# INLINE scopedAsUndirected #-}

scopedNext
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedDirectedEdge scope
  -> ScopedDirectedEdge scope
scopedNext (ScopedTriangulation triangulation) (ScopedDirectedEdge edge) =
  ScopedDirectedEdge (Dcel.next triangulation edge)
{-# INLINE scopedNext #-}

scopedPrevious
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedDirectedEdge scope
  -> ScopedDirectedEdge scope
scopedPrevious (ScopedTriangulation triangulation) (ScopedDirectedEdge edge) =
  ScopedDirectedEdge (Dcel.previous triangulation edge)
{-# INLINE scopedPrevious #-}

scopedIncidentFace
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedDirectedEdge scope
  -> ScopedFace scope
scopedIncidentFace (ScopedTriangulation triangulation) (ScopedDirectedEdge edge) =
  ScopedFace (Dcel.incidentFace triangulation edge)
{-# INLINE scopedIncidentFace #-}

scopedIsOuterDirectedEdge
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedDirectedEdge scope
  -> Bool
scopedIsOuterDirectedEdge (ScopedTriangulation triangulation) (ScopedDirectedEdge edge) =
  Dcel.isOuterDirectedEdge triangulation edge
{-# INLINE scopedIsOuterDirectedEdge #-}

scopedIsBoundaryEdge
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedUndirectedEdge scope
  -> Bool
scopedIsBoundaryEdge (ScopedTriangulation triangulation) (ScopedUndirectedEdge edge) =
  Dcel.isBoundaryEdge triangulation edge
{-# INLINE scopedIsBoundaryEdge #-}

scopedIsConstraintEdge
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedUndirectedEdge scope
  -> Bool
scopedIsConstraintEdge (ScopedTriangulation triangulation) (ScopedUndirectedEdge edge) =
  Dcel.isConstraintEdge triangulation edge
{-# INLINE scopedIsConstraintEdge #-}

scopedUndirectedEndpoints
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedUndirectedEdge scope
  -> (ScopedVertex scope, ScopedVertex scope)
scopedUndirectedEndpoints (ScopedTriangulation triangulation) (ScopedUndirectedEdge edge) =
  let (firstVertex, secondVertex) = Dcel.undirectedEndpoints triangulation edge
   in (ScopedVertex firstVertex, ScopedVertex secondVertex)

scopedFaceDirectedEdges
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedFace scope
  -> [ScopedDirectedEdge scope]
scopedFaceDirectedEdges (ScopedTriangulation triangulation) (ScopedFace face) =
  fmap ScopedDirectedEdge (Dcel.faceDirectedEdges triangulation face)

scopedFaceVertices
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedFace scope
  -> [ScopedVertex scope]
scopedFaceVertices (ScopedTriangulation triangulation) (ScopedFace face) =
  fmap ScopedVertex (Dcel.faceVertices triangulation face)

scopedVertexOutgoingEdges
  :: ScopedTriangulation scope mode vertex directed undirected face
  -> ScopedVertex scope
  -> [ScopedDirectedEdge scope]
scopedVertexOutgoingEdges (ScopedTriangulation triangulation) (ScopedVertex vertex) =
  fmap ScopedDirectedEdge (Dcel.vertexOutgoingEdges triangulation vertex)

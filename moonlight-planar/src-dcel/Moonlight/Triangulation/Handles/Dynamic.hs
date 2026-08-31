{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Owning handles that prevent identifiers from crossing mesh boundaries.
module Moonlight.Triangulation.Handles.Dynamic
  ( InnerTag
  , PossiblyOuterTag
  , FixedFaceHandle
  , asPossiblyOuter
  , fixedFaceId
  , VertexHandle
  , DirectedEdgeHandle
  , UndirectedEdgeHandle
  , FaceHandle
  , vertexHandle
  , directedEdgeHandle
  , undirectedEdgeHandle
  , faceHandle
  , innerFaceHandle
  , outerFaceHandle
  , fixVertex
  , fixDirectedEdge
  , fixUndirectedEdge
  , fixFace
  , vertexHandleData
  , vertexHandlePosition
  , vertexHandleOutEdge
  , vertexHandleOutEdges
  , directedEdgeDataH
  , directedEdgeFrom
  , directedEdgeTo
  , directedEdgeVertices
  , directedEdgePositions
  , directedEdgeReverse
  , directedEdgeNext
  , directedEdgePrevious
  , directedEdgeClockwise
  , directedEdgeCounterClockwise
  , directedEdgeFace
  , directedEdgeAsUndirected
  , directedEdgeIsOuter
  , directedEdgeSideQuery
  , directedEdgeOppositeVertex
  , directedEdgeOppositePosition
  , directedEdgeProjectionFactor
  , directedEdgeNearestPoint
  , undirectedEdgeDataH
  , undirectedEdgeAsDirected
  , undirectedEdgeVertices
  , undirectedEdgeIsConstraint
  , undirectedEdgeIsBoundary
  , faceDataH
  , faceIsOuter
  , faceAsInner
  , faceAdjacentEdge
  , faceAdjacentEdges
  , innerFaceVertices
  , innerFaceCircumcenter
  , innerFacePositions
  , innerFaceBarycentric
  ) where

import Moonlight.Triangulation.Dcel qualified as Dcel
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Internal.Predicates qualified as Admitted
import Moonlight.Triangulation.LineSideInfo (LineSideInfo)
import Moonlight.Triangulation.Math qualified as Math
import Moonlight.Triangulation.Types

-- | A face handle that is statically known not to denote the outer face.
data InnerTag

-- | A face handle that may denote the unique outer face.
data PossiblyOuterTag

type role FixedFaceHandle nominal
-- | Fixed face identifier refined by whether it may denote the outer face.
newtype FixedFaceHandle tag = FixedFaceHandle { unFixedFaceHandle :: FaceId }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | Forget the proof that a fixed face is bounded.
asPossiblyOuter :: FixedFaceHandle InnerTag -> FixedFaceHandle PossiblyOuterTag
asPossiblyOuter (FixedFaceHandle face) = FixedFaceHandle face
{-# INLINE asPossiblyOuter #-}

-- | Recover the unrefined face identifier.
fixedFaceId :: FixedFaceHandle tag -> FaceId
fixedFaceId (FixedFaceHandle face) = face
{-# INLINE fixedFaceId #-}

-- | Admitted vertex paired with its owning triangulation.
data VertexHandle mode vertex directed undirected face = VertexHandle
  !(Triangulation mode vertex directed undirected face)
  !VertexId

-- | Admitted directed edge paired with its owning triangulation.
data DirectedEdgeHandle mode vertex directed undirected face = DirectedEdgeHandle
  !(Triangulation mode vertex directed undirected face)
  !DirectedEdgeId

-- | Admitted undirected edge paired with its owning triangulation.
data UndirectedEdgeHandle mode vertex directed undirected face = UndirectedEdgeHandle
  !(Triangulation mode vertex directed undirected face)
  !UndirectedEdgeId

-- | Admitted face paired with its owning triangulation and outer-face proof.
data FaceHandle tag mode vertex directed undirected face = FaceHandle
  !(Triangulation mode vertex directed undirected face)
  !(FixedFaceHandle tag)

instance Show (VertexHandle mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixVertex

instance Show (DirectedEdgeHandle mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixDirectedEdge

instance Show (UndirectedEdgeHandle mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixUndirectedEdge

instance Show (FaceHandle tag mode vertex directed undirected face) where
  showsPrec precedence = showsPrec precedence . fixFace

-- | Admit a vertex identifier into a triangulation.
vertexHandle
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> Maybe (VertexHandle mode vertex directed undirected face)
vertexHandle triangulation vertex
  | vertexIdIndex vertex < Dcel.numVertices triangulation = Just (VertexHandle triangulation vertex)
  | otherwise = Nothing

-- | Admit a directed-edge identifier into a triangulation.
directedEdgeHandle
  :: Triangulation mode vertex directed undirected face
  -> DirectedEdgeId
  -> Maybe (DirectedEdgeHandle mode vertex directed undirected face)
directedEdgeHandle triangulation edge
  | directedEdgeIdIndex edge < Dcel.numDirectedEdges triangulation = Just (DirectedEdgeHandle triangulation edge)
  | otherwise = Nothing

-- | Admit an undirected-edge identifier into a triangulation.
undirectedEdgeHandle
  :: Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> Maybe (UndirectedEdgeHandle mode vertex directed undirected face)
undirectedEdgeHandle triangulation edge
  | undirectedEdgeIdIndex edge < Dcel.numUndirectedEdges triangulation = Just (UndirectedEdgeHandle triangulation edge)
  | otherwise = Nothing

-- | Admit a face identifier that may denote the outer face.
faceHandle
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> Maybe (FaceHandle PossiblyOuterTag mode vertex directed undirected face)
faceHandle triangulation face
  | faceIdIndex face < Dcel.numFaces triangulation = Just (FaceHandle triangulation (FixedFaceHandle face))
  | otherwise = Nothing

-- | Admit a face identifier while proving that it is bounded.
innerFaceHandle
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> Maybe (FaceHandle InnerTag mode vertex directed undirected face)
innerFaceHandle triangulation face
  | face == Dcel.outerFace = Nothing
  | otherwise = do
      FaceHandle _ (FixedFaceHandle valid) <- faceHandle triangulation face
      pure (FaceHandle triangulation (FixedFaceHandle valid))

-- | Owning handle to the unique unbounded face.
outerFaceHandle
  :: Triangulation mode vertex directed undirected face
  -> FaceHandle PossiblyOuterTag mode vertex directed undirected face
outerFaceHandle triangulation = FaceHandle triangulation (FixedFaceHandle Dcel.outerFace)

-- | Forget ownership and recover the vertex identifier.
fixVertex :: VertexHandle mode vertex directed undirected face -> VertexId
fixVertex (VertexHandle _ vertex) = vertex
{-# INLINE fixVertex #-}

-- | Forget ownership and recover the directed-edge identifier.
fixDirectedEdge :: DirectedEdgeHandle mode vertex directed undirected face -> DirectedEdgeId
fixDirectedEdge (DirectedEdgeHandle _ edge) = edge
{-# INLINE fixDirectedEdge #-}

-- | Forget ownership and recover the undirected-edge identifier.
fixUndirectedEdge :: UndirectedEdgeHandle mode vertex directed undirected face -> UndirectedEdgeId
fixUndirectedEdge (UndirectedEdgeHandle _ edge) = edge
{-# INLINE fixUndirectedEdge #-}

-- | Forget ownership while retaining the outer-face refinement.
fixFace :: FaceHandle tag mode vertex directed undirected face -> FixedFaceHandle tag
fixFace (FaceHandle _ face) = face
{-# INLINE fixFace #-}

-- | Vertex annotation through an owning handle.
vertexHandleData :: VertexHandle mode vertex directed undirected face -> vertex
vertexHandleData (VertexHandle triangulation vertex) = Dcel.vertexData triangulation vertex
{-# INLINE vertexHandleData #-}

-- | Authoritative vertex position through an owning handle.
vertexHandlePosition
  :: VertexHandle mode vertex directed undirected face
  -> Point
vertexHandlePosition (VertexHandle triangulation vertex) = (Dcel.vertexPoint triangulation vertex)
{-# INLINE vertexHandlePosition #-}

-- | One outgoing edge of a connected vertex.
vertexHandleOutEdge
  :: VertexHandle mode vertex directed undirected face
  -> Maybe (DirectedEdgeHandle mode vertex directed undirected face)
vertexHandleOutEdge (VertexHandle triangulation vertex) = DirectedEdgeHandle triangulation <$> Dcel.vertexOutEdge triangulation vertex

-- | Directed edges leaving a vertex in ring order.
vertexHandleOutEdges
  :: VertexHandle mode vertex directed undirected face
  -> [DirectedEdgeHandle mode vertex directed undirected face]
vertexHandleOutEdges (VertexHandle triangulation vertex) = map (DirectedEdgeHandle triangulation) (Dcel.vertexOutgoingEdges triangulation vertex)

-- | Directed-edge annotation through an owning handle.
directedEdgeDataH :: DirectedEdgeHandle mode vertex directed undirected face -> directed
directedEdgeDataH (DirectedEdgeHandle triangulation edge) = Dcel.directedEdgeData triangulation edge
{-# INLINE directedEdgeDataH #-}

-- | Origin vertex of an owning directed edge.
directedEdgeFrom :: DirectedEdgeHandle mode vertex directed undirected face -> VertexHandle mode vertex directed undirected face
directedEdgeFrom (DirectedEdgeHandle triangulation edge) = VertexHandle triangulation (Dcel.origin triangulation edge)
{-# INLINE directedEdgeFrom #-}

-- | Destination vertex of an owning directed edge.
directedEdgeTo :: DirectedEdgeHandle mode vertex directed undirected face -> VertexHandle mode vertex directed undirected face
directedEdgeTo (DirectedEdgeHandle triangulation edge) = VertexHandle triangulation (Dcel.destination triangulation edge)
{-# INLINE directedEdgeTo #-}

-- | Origin and destination of an owning directed edge.
directedEdgeVertices
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> (VertexHandle mode vertex directed undirected face, VertexHandle mode vertex directed undirected face)
directedEdgeVertices edge = (directedEdgeFrom edge, directedEdgeTo edge)

-- | Origin and destination positions of an owning directed edge.
directedEdgePositions
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> (Point, Point)
directedEdgePositions edge = (vertexHandlePosition (directedEdgeFrom edge), vertexHandlePosition (directedEdgeTo edge))
{-# INLINE directedEdgePositions #-}

-- | Reverse an owning directed edge.
directedEdgeReverse :: DirectedEdgeHandle mode vertex directed undirected face -> DirectedEdgeHandle mode vertex directed undirected face
directedEdgeReverse (DirectedEdgeHandle triangulation edge) = DirectedEdgeHandle triangulation (reverseEdge edge)
{-# INLINE directedEdgeReverse #-}

-- | Next owning edge around the incident face.
directedEdgeNext :: DirectedEdgeHandle mode vertex directed undirected face -> DirectedEdgeHandle mode vertex directed undirected face
directedEdgeNext (DirectedEdgeHandle triangulation edge) = DirectedEdgeHandle triangulation (Dcel.next triangulation edge)
{-# INLINE directedEdgeNext #-}

-- | Previous owning edge around the incident face.
directedEdgePrevious :: DirectedEdgeHandle mode vertex directed undirected face -> DirectedEdgeHandle mode vertex directed undirected face
directedEdgePrevious (DirectedEdgeHandle triangulation edge) = DirectedEdgeHandle triangulation (Dcel.previous triangulation edge)
{-# INLINE directedEdgePrevious #-}

-- | Previous owning edge around its origin vertex.
directedEdgeClockwise :: DirectedEdgeHandle mode vertex directed undirected face -> DirectedEdgeHandle mode vertex directed undirected face
directedEdgeClockwise (DirectedEdgeHandle triangulation edge) = DirectedEdgeHandle triangulation (Dcel.clockwise triangulation edge)
{-# INLINE directedEdgeClockwise #-}

-- | Next owning edge around its origin vertex.
directedEdgeCounterClockwise :: DirectedEdgeHandle mode vertex directed undirected face -> DirectedEdgeHandle mode vertex directed undirected face
directedEdgeCounterClockwise (DirectedEdgeHandle triangulation edge) = DirectedEdgeHandle triangulation (Dcel.counterClockwise triangulation edge)
{-# INLINE directedEdgeCounterClockwise #-}

-- | Owning handle to the incident face.
directedEdgeFace
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> FaceHandle PossiblyOuterTag mode vertex directed undirected face
directedEdgeFace (DirectedEdgeHandle triangulation edge) = FaceHandle triangulation (FixedFaceHandle (Dcel.incidentFace triangulation edge))

-- | Forget the orientation of an owning edge.
directedEdgeAsUndirected
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> UndirectedEdgeHandle mode vertex directed undirected face
directedEdgeAsUndirected (DirectedEdgeHandle triangulation edge) = UndirectedEdgeHandle triangulation (asUndirected edge)

-- | Whether the owning edge is incident to the outer face.
directedEdgeIsOuter :: DirectedEdgeHandle mode vertex directed undirected face -> Bool
directedEdgeIsOuter = faceIsOuter . directedEdgeFace

-- | Exact side of the owning edge's oriented line.
directedEdgeSideQuery
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> QueryPoint
  -> LineSideInfo
directedEdgeSideQuery edge query =
  let (from, to) = directedEdgePositions edge
   in Admitted.sideQuery from to (queryPointValue query)

-- | Undirected-edge annotation through an owning handle.
undirectedEdgeDataH :: UndirectedEdgeHandle mode vertex directed undirected face -> undirected
undirectedEdgeDataH (UndirectedEdgeHandle triangulation edge) = Dcel.undirectedEdgeData triangulation edge
{-# INLINE undirectedEdgeDataH #-}

-- | Normalized directed orientation of an owning undirected edge.
undirectedEdgeAsDirected
  :: UndirectedEdgeHandle mode vertex directed undirected face
  -> DirectedEdgeHandle mode vertex directed undirected face
undirectedEdgeAsDirected (UndirectedEdgeHandle triangulation edge) = DirectedEdgeHandle triangulation (normalizedDirected edge)

-- | Endpoints of an owning undirected edge.
undirectedEdgeVertices
  :: UndirectedEdgeHandle mode vertex directed undirected face
  -> (VertexHandle mode vertex directed undirected face, VertexHandle mode vertex directed undirected face)
undirectedEdgeVertices = directedEdgeVertices . undirectedEdgeAsDirected

-- | Face annotation through an owning handle.
faceDataH :: FaceHandle tag mode vertex directed undirected face -> face
faceDataH (FaceHandle triangulation (FixedFaceHandle face)) = Dcel.faceData triangulation face
{-# INLINE faceDataH #-}

-- | Whether the owning face is the unique unbounded face.
faceIsOuter :: FaceHandle tag mode vertex directed undirected face -> Bool
faceIsOuter (FaceHandle _ (FixedFaceHandle face)) = face == Dcel.outerFace
{-# INLINE faceIsOuter #-}

-- | Refine an owning face handle by excluding the outer face.
faceAsInner
  :: FaceHandle PossiblyOuterTag mode vertex directed undirected face
  -> Maybe (FaceHandle InnerTag mode vertex directed undirected face)
faceAsInner handle@(FaceHandle triangulation (FixedFaceHandle face))
  | faceIsOuter handle = Nothing
  | otherwise = Just (FaceHandle triangulation (FixedFaceHandle face))

-- | One owning edge on the face boundary.
faceAdjacentEdge
  :: FaceHandle tag mode vertex directed undirected face
  -> Maybe (DirectedEdgeHandle mode vertex directed undirected face)
faceAdjacentEdge (FaceHandle triangulation (FixedFaceHandle face)) = DirectedEdgeHandle triangulation <$> Dcel.adjacentEdge triangulation face

-- | Owning directed boundary of a face.
faceAdjacentEdges
  :: FaceHandle tag mode vertex directed undirected face
  -> [DirectedEdgeHandle mode vertex directed undirected face]
faceAdjacentEdges (FaceHandle triangulation (FixedFaceHandle face)) = map (DirectedEdgeHandle triangulation) (Dcel.faceDirectedEdges triangulation face)

-- | Three owning vertices of a bounded face.
innerFaceVertices
  :: FaceHandle InnerTag mode vertex directed undirected face
  -> Maybe
       ( VertexHandle mode vertex directed undirected face
       , VertexHandle mode vertex directed undirected face
       , VertexHandle mode vertex directed undirected face
       )
innerFaceVertices (FaceHandle triangulation (FixedFaceHandle face)) =
  (\(a, b, c) -> (VertexHandle triangulation a, VertexHandle triangulation b, VertexHandle triangulation c))
    <$> Dcel.innerFaceVertices triangulation face

-- | Circumcenter of an owning bounded face.
innerFaceCircumcenter
  :: FaceHandle InnerTag mode vertex directed undirected face
  -> Maybe (Point)
innerFaceCircumcenter face = do
  (a, b, c) <- innerFaceVertices face
  Math.circumcenter (vertexHandlePosition a) (vertexHandlePosition b) (vertexHandlePosition c)

-- | Vertex opposite an owning directed edge in its bounded incident face.
directedEdgeOppositeVertex
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> Maybe (VertexHandle mode vertex directed undirected face)
directedEdgeOppositeVertex (DirectedEdgeHandle triangulation edge)
  | Dcel.incidentFace triangulation edge == Dcel.outerFace = Nothing
  | otherwise =
      Just (VertexHandle triangulation (Dcel.destination triangulation (Dcel.next triangulation edge)))

-- | Position opposite an owning directed edge in its bounded incident face.
directedEdgeOppositePosition
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> Maybe (Point)
directedEdgeOppositePosition = fmap vertexHandlePosition . directedEdgeOppositeVertex
{-# INLINE directedEdgeOppositePosition #-}

-- | Projection parameter of a point onto an owning directed edge's line.
directedEdgeProjectionFactor
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> Point
  -> Double
directedEdgeProjectionFactor edge query =
  let (from, to) = directedEdgePositions edge
   in Math.projectionFactor from to query

-- | Nearest point on the closed owning directed edge.
directedEdgeNearestPoint
  :: DirectedEdgeHandle mode vertex directed undirected face
  -> Point
  -> Point
directedEdgeNearestPoint edge query =
  let (from@(Point ax ay), to@(Point bx by)) = directedEdgePositions edge
      factor = max 0 (min 1 (Math.projectionFactor from to query))
   in Point (ax + factor * (bx - ax)) (ay + factor * (by - ay))

-- | Whether an owning undirected edge is constrained.
undirectedEdgeIsConstraint
  :: UndirectedEdgeHandle mode vertex directed undirected face
  -> Bool
undirectedEdgeIsConstraint (UndirectedEdgeHandle triangulation edge) =
  Dcel.isConstraintEdge triangulation edge

-- | Whether an owning undirected edge touches the outer face.
undirectedEdgeIsBoundary
  :: UndirectedEdgeHandle mode vertex directed undirected face
  -> Bool
undirectedEdgeIsBoundary (UndirectedEdgeHandle triangulation edge) =
  Dcel.isBoundaryEdge triangulation edge

-- | Positions of the three vertices of an owning bounded face.
innerFacePositions
  :: FaceHandle InnerTag mode vertex directed undirected face
  -> Maybe (Point, Point, Point)
innerFacePositions face = do
  (a, b, c) <- innerFaceVertices face
  pure (vertexHandlePosition a, vertexHandlePosition b, vertexHandlePosition c)
{-# INLINE innerFacePositions #-}

-- | Barycentric coordinates in an owning bounded face.
innerFaceBarycentric
  :: FaceHandle InnerTag mode vertex directed undirected face
  -> Point
  -> Maybe (Double, Double, Double)
innerFaceBarycentric face query = do
  (a, b, c) <- innerFacePositions face
  Math.barycentricCoordinates a b c query

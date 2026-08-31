{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Constant-time fixed-index observations and persistent payload updates over
-- the immutable DCEL. This is the explicit unchecked resident-index kernel;
-- ordinary consumers should use scoped or owning handles so indices cannot
-- cross mesh boundaries.
module Moonlight.Triangulation.Dcel
  ( outerFace
  , numVertices
  , numDirectedEdges
  , numUndirectedEdges
  , numFaces
  , numInnerFaces
  , vertexPoint
  , vertexPoints
  , vertexData
  , directedEdgeData
  , undirectedEdgeData
  , faceData
  , setVertexData
  , setDirectedEdgeData
  , setUndirectedEdgeData
  , setFaceData
  , mapVertices
  , mapDirectedEdges
  , mapUndirectedEdges
  , mapFaces
  , imapUndirectedEdges
  , imapFaces
  , vertexOutEdge
  , adjacentEdge
  , origin
  , destination
  , next
  , previous
  , incidentFace
  , isOuterDirectedEdge
  , isBoundaryEdge
  , isConstraintEdge
  , numConstraints
  , undirectedEndpoints
  , faceDirectedEdges
  , faceVertices
  , innerFaceDirectedEdges
  , innerFaceDirectedEdgeTriples
  , innerFaceVertices
  , innerFaceVertexTriples
  , vertexOutgoingEdges
  , clockwise
  , counterClockwise
  , foldFaceDirectedEdges'
  , foldVertexOutgoingEdges'
  , topologyIndexBytes
  , geometryTopologyBytes
  ) where

import qualified Data.Vector as V
import Data.Word (Word8)
import GHC.Exts (build)
import Moonlight.Triangulation.Internal.BoxedPaged (boxedUnsafeIndex, boxedUpdate)
import Moonlight.Triangulation.Internal.Paged (pagedLength, pagedUnsafeIndex)
import Moonlight.Triangulation.Internal.HandleDefs
import Moonlight.Triangulation.Internal.PackedIndex (unpackOptionalIndex)
import Moonlight.Triangulation.Internal.Representation
import Moonlight.Triangulation.Internal.Types
import Moonlight.Triangulation.Scalar (scalarByteSize)

-- | The unique unbounded face, always stored at index zero.
outerFace :: FaceId
outerFace = FaceId 0

-- | Number of vertices in the mesh.
numVertices :: Triangulation mode vertex directed undirected face -> Int
numVertices = pagedLength . triPointX
{-# INLINE numVertices #-}

-- | Number of oriented half-edges in the mesh.
numDirectedEdges :: Triangulation mode vertex directed undirected face -> Int
numDirectedEdges = (`quot` 4) . pagedLength . triHalfTopology
{-# INLINE numDirectedEdges #-}

-- | Number of twin pairs in the mesh.
numUndirectedEdges :: Triangulation mode vertex directed undirected face -> Int
numUndirectedEdges triangulation = numDirectedEdges triangulation `quot` 2
{-# INLINE numUndirectedEdges #-}

-- | Number of faces, including 'outerFace'.
numFaces :: Triangulation mode vertex directed undirected face -> Int
numFaces = pagedLength . triFaceEdge
{-# INLINE numFaces #-}

-- | Number of bounded triangular faces.
numInnerFaces :: Triangulation mode vertex directed undirected face -> Int
numInnerFaces triangulation = max 0 (numFaces triangulation - 1)
{-# INLINE numInnerFaces #-}

-- | Authoritative geometric position of an admitted vertex handle.
vertexPoint :: Triangulation mode vertex directed undirected face -> VertexId -> Point
vertexPoint triangulation vertex =
  let !index = vertexIdIndex vertex
   in Point (pagedUnsafeIndex (triPointX triangulation) index) (pagedUnsafeIndex (triPointY triangulation) index)
{-# INLINE vertexPoint #-}

-- | Dense vertex positions in handle order.
vertexPoints :: Triangulation mode vertex directed undirected face -> V.Vector Point
vertexPoints triangulation =
  V.generate (numVertices triangulation) (vertexPoint triangulation . VertexId . fromIntegral)
{-# INLINE vertexPoints #-}

-- | Annotation carried by an admitted vertex handle.
vertexData :: Triangulation mode vertex directed undirected face -> VertexId -> vertex
vertexData triangulation vertex =
  boxedUnsafeIndex (triVertexData triangulation) (vertexIdIndex vertex)
{-# INLINE vertexData #-}

-- | Annotation carried by an admitted directed edge.
directedEdgeData :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> directed
directedEdgeData triangulation edge =
  boxedUnsafeIndex (triDirectedData triangulation) (directedEdgeIdIndex edge)
{-# INLINE directedEdgeData #-}

-- | Annotation carried by an admitted undirected edge.
undirectedEdgeData :: Triangulation mode vertex directed undirected face -> UndirectedEdgeId -> undirected
undirectedEdgeData triangulation edge =
  boxedUnsafeIndex (triUndirectedData triangulation) (undirectedEdgeIdIndex edge)
{-# INLINE undirectedEdgeData #-}

-- | Annotation carried by an admitted face.
faceData :: Triangulation mode vertex directed undirected face -> FaceId -> face
faceData triangulation face =
  boxedUnsafeIndex (triFaceData triangulation) (faceIdIndex face)
{-# INLINE faceData #-}

-- | Replace one vertex annotation without changing geometry or topology.
setVertexData
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> vertex
  -> Triangulation mode vertex directed undirected face
setVertexData triangulation vertex payload =
  triangulation{triVertexData = boxedUpdate (vertexIdIndex vertex) payload (triVertexData triangulation)}

-- | Replace one directed-edge annotation without changing geometry or topology.
setDirectedEdgeData
  :: Triangulation mode vertex directed undirected face
  -> DirectedEdgeId
  -> directed
  -> Triangulation mode vertex directed undirected face
setDirectedEdgeData triangulation edge payload =
  triangulation{triDirectedData = boxedUpdate (directedEdgeIdIndex edge) payload (triDirectedData triangulation)}

-- | Replace one undirected-edge annotation without changing geometry or topology.
setUndirectedEdgeData
  :: Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> undirected
  -> Triangulation mode vertex directed undirected face
setUndirectedEdgeData triangulation edge payload =
  triangulation{triUndirectedData = boxedUpdate (undirectedEdgeIdIndex edge) payload (triUndirectedData triangulation)}

-- | Replace one face annotation without changing geometry or topology.
setFaceData
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> face
  -> Triangulation mode vertex directed undirected face
setFaceData triangulation face payload =
  triangulation{triFaceData = boxedUpdate (faceIdIndex face) payload (triFaceData triangulation)}

-- | One outgoing directed edge, if the vertex is connected.
vertexOutEdge :: Triangulation mode vertex directed undirected face -> VertexId -> Maybe DirectedEdgeId
vertexOutEdge triangulation vertex =
  DirectedEdgeId . fromIntegral <$> unpackOptionalIndex (pagedUnsafeIndex (triVertexOut triangulation) (vertexIdIndex vertex))
{-# INLINE vertexOutEdge #-}

-- | One boundary edge of a face, if the face has a boundary.
adjacentEdge :: Triangulation mode vertex directed undirected face -> FaceId -> Maybe DirectedEdgeId
adjacentEdge triangulation face =
  DirectedEdgeId . fromIntegral <$> unpackOptionalIndex (pagedUnsafeIndex (triFaceEdge triangulation) (faceIdIndex face))
{-# INLINE adjacentEdge #-}

-- | Origin vertex of an admitted directed edge.
origin :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> VertexId
origin triangulation edge =
  VertexId (pagedUnsafeIndex (triHalfTopology triangulation) (4 * directedEdgeIdIndex edge))
{-# INLINE origin #-}

-- | Destination vertex of an admitted directed edge.
destination :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> VertexId
destination triangulation = origin triangulation . reverseEdge
{-# INLINE destination #-}

-- | Next directed edge around the incident face.
next :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> DirectedEdgeId
next triangulation edge =
  DirectedEdgeId (pagedUnsafeIndex (triHalfTopology triangulation) (4 * directedEdgeIdIndex edge + 1))
{-# INLINE next #-}

-- | Previous directed edge around the incident face.
previous :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> DirectedEdgeId
previous triangulation edge =
  DirectedEdgeId (pagedUnsafeIndex (triHalfTopology triangulation) (4 * directedEdgeIdIndex edge + 2))
{-# INLINE previous #-}

-- | Face on the left of an admitted directed edge.
incidentFace :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> FaceId
incidentFace triangulation edge =
  FaceId (pagedUnsafeIndex (triHalfTopology triangulation) (4 * directedEdgeIdIndex edge + 3))
{-# INLINE incidentFace #-}

-- | Whether the directed edge is incident to the unbounded face.
isOuterDirectedEdge :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> Bool
isOuterDirectedEdge triangulation edge = incidentFace triangulation edge == outerFace
{-# INLINE isOuterDirectedEdge #-}

-- | Whether either orientation is incident to the outer face.
isBoundaryEdge :: Triangulation mode vertex directed undirected face -> UndirectedEdgeId -> Bool
isBoundaryEdge triangulation edge =
  let (forward, backward) = directedPair edge
   in isOuterDirectedEdge triangulation forward || isOuterDirectedEdge triangulation backward
{-# INLINE isBoundaryEdge #-}

-- | Whether the edge belongs to the constrained-edge section.
isConstraintEdge :: Triangulation mode vertex directed undirected face -> UndirectedEdgeId -> Bool
isConstraintEdge triangulation edge =
  pagedUnsafeIndex (triConstraint triangulation) (undirectedEdgeIdIndex edge) /= (0 :: Word8)
{-# INLINE isConstraintEdge #-}

-- | Number of constrained undirected edges.
numConstraints :: Triangulation mode vertex directed undirected face -> Int
numConstraints = triConstraintCount
{-# INLINE numConstraints #-}

-- | Endpoints in the normalized orientation.
undirectedEndpoints :: Triangulation mode vertex directed undirected face -> UndirectedEdgeId -> (VertexId, VertexId)
undirectedEndpoints triangulation edge =
  let forward = normalizedDirected edge
   in (origin triangulation forward, destination triangulation forward)
{-# INLINE undirectedEndpoints #-}

-- | Boundary cycle of a face in traversal order.
faceDirectedEdges :: Triangulation mode vertex directed undirected face -> FaceId -> [DirectedEdgeId]
faceDirectedEdges triangulation face =
  case adjacentEdge triangulation face of
    Nothing -> []
    Just start -> circularWalk triangulation start (next triangulation)
{-# INLINE faceDirectedEdges #-}

-- | Origins along a face boundary cycle, in traversal order.
faceVertices :: Triangulation mode vertex directed undirected face -> FaceId -> [VertexId]
faceVertices triangulation = map (origin triangulation) . faceDirectedEdges triangulation
{-# INLINE faceVertices #-}

-- | The three directed edges of a bounded triangular face.
innerFaceDirectedEdges :: Triangulation mode vertex directed undirected face -> FaceId -> Maybe (DirectedEdgeId, DirectedEdgeId, DirectedEdgeId)
innerFaceDirectedEdges triangulation face
  | face == outerFace || faceIdIndex face >= numFaces triangulation = Nothing
  | otherwise = do
      e0 <- adjacentEdge triangulation face
      let !e1 = next triangulation e0
          !e2 = next triangulation e1
      if next triangulation e2 == e0
        then Just (e0, e1, e2)
        else Nothing
{-# INLINE innerFaceDirectedEdges #-}

-- | Dense bounded-face directed-edge triples in face-handle order.
innerFaceDirectedEdgeTriples
  :: Triangulation mode vertex directed undirected face
  -> V.Vector (DirectedEdgeId, DirectedEdgeId, DirectedEdgeId)
innerFaceDirectedEdgeTriples triangulation =
  V.generate (numInnerFaces triangulation) $ \innerFaceIndex ->
    let !faceIndex = innerFaceIndex + 1
        !firstEdge =
          DirectedEdgeId
            (pagedUnsafeIndex (triFaceEdge triangulation) faceIndex)
        !secondEdge = next triangulation firstEdge
     in (firstEdge, secondEdge, next triangulation secondEdge)
{-# INLINE innerFaceDirectedEdgeTriples #-}

-- | The three vertices of a bounded triangular face.
innerFaceVertices :: Triangulation mode vertex directed undirected face -> FaceId -> Maybe (VertexId, VertexId, VertexId)
innerFaceVertices triangulation face = do
  (e0, e1, e2) <- innerFaceDirectedEdges triangulation face
  pure (origin triangulation e0, origin triangulation e1, origin triangulation e2)
{-# INLINE innerFaceVertices #-}

-- | Dense bounded-face vertex triples in face-handle order.
innerFaceVertexTriples
  :: Triangulation mode vertex directed undirected face
  -> V.Vector (VertexId, VertexId, VertexId)
innerFaceVertexTriples triangulation =
  fmap
    (\(firstEdge, secondEdge, thirdEdge) ->
      ( origin triangulation firstEdge
      , origin triangulation secondEdge
      , origin triangulation thirdEdge
      )
    )
    (innerFaceDirectedEdgeTriples triangulation)
{-# INLINE innerFaceVertexTriples #-}

-- | Counter-clockwise ring of directed edges originating at a vertex.
vertexOutgoingEdges :: Triangulation mode vertex directed undirected face -> VertexId -> [DirectedEdgeId]
vertexOutgoingEdges triangulation vertex =
  case vertexOutEdge triangulation vertex of
    Nothing -> []
    Just start -> circularWalk triangulation start (counterClockwise triangulation)
{-# INLINE vertexOutgoingEdges #-}

-- | Previous directed edge around its origin vertex.
clockwise :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> DirectedEdgeId
clockwise triangulation edge = next triangulation (reverseEdge edge)
{-# INLINE clockwise #-}

-- | Next directed edge around its origin vertex.
counterClockwise :: Triangulation mode vertex directed undirected face -> DirectedEdgeId -> DirectedEdgeId
counterClockwise triangulation edge = reverseEdge (previous triangulation edge)
{-# INLINE counterClockwise #-}

-- | Strict fold over the directed edges around a face.
foldFaceDirectedEdges'
  :: Triangulation mode vertex directed undirected face -> FaceId
  -> (a -> DirectedEdgeId -> a)
  -> a
  -> a
foldFaceDirectedEdges' triangulation face step initial =
  case adjacentEdge triangulation face of
    Nothing -> initial
    Just start -> circularFold triangulation start (next triangulation) step initial
{-# INLINE foldFaceDirectedEdges' #-}

-- | Strict fold over the directed edges originating at a vertex.
foldVertexOutgoingEdges'
  :: Triangulation mode vertex directed undirected face -> VertexId
  -> (a -> DirectedEdgeId -> a)
  -> a
  -> a
foldVertexOutgoingEdges' triangulation vertex step initial =
  case vertexOutEdge triangulation vertex of
    Nothing -> initial
    Just start -> circularFold triangulation start (counterClockwise triangulation) step initial
{-# INLINE foldVertexOutgoingEdges' #-}

-- Constraint bytes are intentionally reported separately by the CDT layer.
-- | Bytes occupied by vertex, half-edge, and face topology indices.
topologyIndexBytes :: Triangulation mode vertex directed undirected face -> Integer
topologyIndexBytes triangulation =
  4 * toInteger
    ( pagedLength (triVertexOut triangulation)
        + pagedLength (triHalfTopology triangulation)
        + pagedLength (triFaceEdge triangulation)
    )

-- | Bytes occupied by authoritative coordinates and topology indices.
geometryTopologyBytes :: Triangulation mode vertex directed undirected face -> Integer
geometryTopologyBytes triangulation =
  2 * toInteger scalarByteSize * toInteger (numVertices triangulation)
    + topologyIndexBytes triangulation

-- | The cycle reached from an edge by repeated advance, in visit order.
--
-- Emitted forwards. Consing in reverse and reversing at the end is the right
-- shape for a strict accumulator, and the wrong one for a producer: it built
-- the ring twice and handed back a list no consumer could fuse with. The
-- guard bound is on the steps taken, which is what it was before — the two
-- forms stop after the same edges.
circularWalk
  :: Triangulation mode vertex directed undirected face -> DirectedEdgeId
  -> (DirectedEdgeId -> DirectedEdgeId)
  -> [DirectedEdgeId]
circularWalk triangulation start advance =
  build
    ( \(link :: DirectedEdgeId -> result -> result) (stop :: result) ->
        let go :: Int -> DirectedEdgeId -> Bool -> result
            go !remaining !current !visited
              | remaining <= 0 = stop
              | visited && current == start = stop
              | otherwise = link current (go (remaining - 1) (advance current) True)
         in go (numDirectedEdges triangulation + 1) start False
    )
{-# INLINE circularWalk #-}

circularFold
  :: forall mode vertex directed undirected face a.
     Triangulation mode vertex directed undirected face -> DirectedEdgeId
  -> (DirectedEdgeId -> DirectedEdgeId)
  -> (a -> DirectedEdgeId -> a)
  -> a
  -> a
circularFold triangulation start advance step =
  go (numDirectedEdges triangulation + 1) start False
 where
  go :: Int -> DirectedEdgeId -> Bool -> a -> a
  go !remaining !current !visited !accumulator
    | remaining <= 0 = accumulator
    | visited && current == start = accumulator
    | otherwise =
        let !nextAccumulator = step accumulator current
         in go (remaining - 1) (advance current) True nextAccumulator
{-# INLINE circularFold #-}

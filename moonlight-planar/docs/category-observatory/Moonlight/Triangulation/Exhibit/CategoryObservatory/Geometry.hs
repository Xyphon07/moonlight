module Moonlight.Triangulation.Exhibit.CategoryObservatory.Geometry
  ( positionForObject,
    projectMesh,
  )
where

import Data.Foldable (foldMap')
import Moonlight.Homology.Pure.Topology.CellCategory (ComplexOb (..))
import Moonlight.Homology.Pure.Topology.CellComplex (CellComplex2D (..))
import Moonlight.Triangulation.CellComplex (DCELComplex)
import Moonlight.Triangulation.Dcel
  ( faceVertices,
    undirectedEndpoints,
    vertexPoint,
  )
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Identifier
  ( edgeIdentifier,
    edgeObjectIdentifier,
    faceIdentifier,
    faceObjectIdentifier,
    vertexIdentifier,
    vertexObjectIdentifier,
  )
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Model
  ( ExhibitError (..),
    MeshEdge (..),
    MeshFace (..),
    MeshProjection (..),
    MeshVertex (..),
    Position (..),
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId,
    UndirectedEdgeId,
    VertexId,
  )
import Moonlight.Triangulation.Types
  ( DelaunayTriangulation,
    Point (..),
  )

projectMesh :: DelaunayTriangulation () -> DCELComplex -> MeshProjection
projectMesh triangulation complexValue =
  MeshProjection
    { meshVertices = fmap (projectMeshVertex triangulation) (vertices complexValue),
      meshEdges = fmap (projectMeshEdge triangulation) (edges complexValue),
      meshFaces = fmap (projectMeshFace triangulation) (faces complexValue)
    }

projectMeshVertex :: DelaunayTriangulation () -> VertexId -> MeshVertex
projectMeshVertex triangulation vertexValue =
  MeshVertex
    { meshVertexIdentifier = vertexIdentifier vertexValue,
      meshVertexObjectIdentifier = vertexObjectIdentifier vertexValue,
      meshVertexPosition = pointPosition (vertexPoint triangulation vertexValue)
    }

projectMeshEdge :: DelaunayTriangulation () -> UndirectedEdgeId -> MeshEdge
projectMeshEdge triangulation edgeValue =
  let (sourceVertex, targetVertex) = undirectedEndpoints triangulation edgeValue
   in MeshEdge
        { meshEdgeIdentifier = edgeIdentifier edgeValue,
          meshEdgeObjectIdentifier = edgeObjectIdentifier edgeValue,
          meshEdgeVertexIdentifiers = fmap vertexIdentifier [sourceVertex, targetVertex]
        }

projectMeshFace :: DelaunayTriangulation () -> FaceId -> MeshFace
projectMeshFace triangulation faceValue =
  MeshFace
    { meshFaceIdentifier = faceIdentifier faceValue,
      meshFaceObjectIdentifier = faceObjectIdentifier faceValue,
      meshFaceVertexIdentifiers = fmap vertexIdentifier (faceVertices triangulation faceValue)
    }

positionForObject ::
  DelaunayTriangulation () ->
  ComplexOb DCELComplex ->
  Either ExhibitError Position
positionForObject triangulation objectValue =
  case objectValue of
    VertexOb vertexValue ->
      Right (pointPosition (vertexPoint triangulation vertexValue))
    EdgeOb edgeValue ->
      let (sourceVertex, targetVertex) = undirectedEndpoints triangulation edgeValue
       in Right
            ( midpoint
                (pointPosition (vertexPoint triangulation sourceVertex))
                (pointPosition (vertexPoint triangulation targetVertex))
            )
    FaceOb faceValue ->
      centroid faceValue
        (fmap (pointPosition . vertexPoint triangulation) (faceVertices triangulation faceValue))

data PositionSum = PositionSum Double Double

instance Semigroup PositionSum where
  PositionSum leftX leftY <> PositionSum rightX rightY =
    PositionSum (leftX + rightX) (leftY + rightY)

instance Monoid PositionSum where
  mempty = PositionSum 0 0

centroid :: FaceId -> [Position] -> Either ExhibitError Position
centroid faceValue positions =
  case positions of
    [] -> Left (ExhibitFaceWithoutVertices faceValue)
    _ ->
      let PositionSum summedX summedY =
            foldMap'
              (\positionValue -> PositionSum (positionX positionValue) (positionY positionValue))
              positions
          positionCount = fromIntegral (length positions)
       in Right (Position (summedX / positionCount) (summedY / positionCount))

midpoint :: Position -> Position -> Position
midpoint firstPosition secondPosition =
  Position
    { positionX = (positionX firstPosition + positionX secondPosition) / 2,
      positionY = (positionY firstPosition + positionY secondPosition) / 2
    }

pointPosition :: Point -> Position
pointPosition pointValue =
  Position
    { positionX = pointX pointValue,
      positionY = pointY pointValue
    }

module Moonlight.Triangulation.Exhibit.CategoryObservatory.Identifier
  ( objectIdentifier,
    objectKind,
    objectLabel,
    morphismKind,
    vertexIdentifier,
    edgeIdentifier,
    faceIdentifier,
    vertexObjectIdentifier,
    edgeObjectIdentifier,
    faceObjectIdentifier,
  )
where

import Moonlight.Homology.Pure.Topology.CellCategory
  ( ComplexMor (..),
    ComplexOb (..),
  )
import Moonlight.Triangulation.CellComplex (DCELComplex)
import Moonlight.Triangulation.Exhibit.CategoryObservatory.Model
  ( CategoryMorphismKind (..),
    CategoryObjectKind (..),
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId,
    UndirectedEdgeId,
    VertexId,
    unFaceId,
    unUndirectedEdgeId,
    unVertexId,
  )

objectIdentifier :: ComplexOb DCELComplex -> String
objectIdentifier objectValue =
  case objectValue of
    VertexOb vertexValue -> vertexObjectIdentifier vertexValue
    EdgeOb edgeValue -> edgeObjectIdentifier edgeValue
    FaceOb faceValue -> faceObjectIdentifier faceValue

objectKind :: ComplexOb DCELComplex -> CategoryObjectKind
objectKind objectValue =
  case objectValue of
    VertexOb _ -> VertexObject
    EdgeOb _ -> EdgeObject
    FaceOb _ -> FaceObject

objectLabel :: ComplexOb DCELComplex -> String
objectLabel objectValue =
  case objectValue of
    VertexOb vertexValue -> "v" <> show (unVertexId vertexValue)
    EdgeOb edgeValue -> "e" <> show (unUndirectedEdgeId edgeValue)
    FaceOb faceValue -> "f" <> show (unFaceId faceValue)

morphismKind :: ComplexMor DCELComplex -> CategoryMorphismKind
morphismKind morphismValue =
  case morphismValue of
    IdentityMor _ -> IdentityMorphism
    FaceToEdge {} -> FaceEdgeMorphism
    EdgeToVertex _ _ -> EdgeVertexMorphism
    FaceToVertex {} -> FaceVertexComposite

vertexIdentifier :: VertexId -> String
vertexIdentifier vertexValue = "vertex-" <> show (unVertexId vertexValue)

edgeIdentifier :: UndirectedEdgeId -> String
edgeIdentifier edgeValue = "edge-" <> show (unUndirectedEdgeId edgeValue)

faceIdentifier :: FaceId -> String
faceIdentifier faceValue = "face-" <> show (unFaceId faceValue)

vertexObjectIdentifier :: VertexId -> String
vertexObjectIdentifier vertexValue = "object-" <> vertexIdentifier vertexValue

edgeObjectIdentifier :: UndirectedEdgeId -> String
edgeObjectIdentifier edgeValue = "object-" <> edgeIdentifier edgeValue

faceObjectIdentifier :: FaceId -> String
faceObjectIdentifier faceValue = "object-" <> faceIdentifier faceValue

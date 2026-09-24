module Moonlight.Planar.Exhibit.CategoryObservatory.Identifier
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
import Moonlight.Planar.CellComplex (DCELComplex, DCELCellEdge (..), faceCutFace, faceCutComponent)
import Moonlight.Planar.Internal.Incidence (FaceBoundaryComponent (..))
import Moonlight.Planar.Exhibit.CategoryObservatory.Model
  ( CategoryMorphismKind (..),
    CategoryObjectKind (..),
  )
import Moonlight.Planar.Internal.HandleDefs
  ( FaceId,
    VertexId,
    unDirectedEdgeId,
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
    EdgeOb edgeValue -> "e" <> edgeIdentitySuffix edgeValue
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

edgeIdentifier :: DCELCellEdge -> String
edgeIdentifier edgeValue = "edge-" <> edgeIdentitySuffix edgeValue

edgeIdentitySuffix :: DCELCellEdge -> String
edgeIdentitySuffix edgeValue =
  case edgeValue of
    GeometricCellEdge edge -> show (unUndirectedEdgeId edge)
    FaceBoundaryCut cut ->
      "cut-" <> show (unFaceId (faceCutFace cut)) <> "-" <>
        (case faceCutComponent cut of
          BoundaryCycleRoot root -> "cycle-" <> show (unDirectedEdgeId root)
          IsolatedBoundaryVertex vertex -> "vertex-" <> show (unVertexId vertex))

faceIdentifier :: FaceId -> String
faceIdentifier faceValue = "face-" <> show (unFaceId faceValue)

vertexObjectIdentifier :: VertexId -> String
vertexObjectIdentifier vertexValue = "object-" <> vertexIdentifier vertexValue

edgeObjectIdentifier :: DCELCellEdge -> String
edgeObjectIdentifier edgeValue = "object-" <> edgeIdentifier edgeValue

faceObjectIdentifier :: FaceId -> String
faceObjectIdentifier faceValue = "object-" <> faceIdentifier faceValue

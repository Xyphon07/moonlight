{-# LANGUAGE DeriveAnyClass #-}

module Moonlight.Planar.Exhibit.CategoryObservatory.Model
  ( Exhibit (..),
    MeshProjection (..),
    MeshVertex (..),
    MeshEdge (..),
    MeshFace (..),
    Position (..),
    CategoryProjection (..),
    CategoryObject (..),
    CategoryObjectKind (..),
    CategoryMorphism (..),
    CategoryMorphismKind (..),
    NerveProjection (..),
    NerveSimplexCount (..),
    NerveTwoSimplex (..),
    CellInventory (..),
    MorphismInventory (..),
    ExhibitError (..),
    MorphismEndpoint (..),
    renderExhibitError,
  )
where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Moonlight.Planar.CellSet (CellSelectionError)
import Moonlight.Planar.CellComplex (DCELError)
import Moonlight.Planar.Internal.HandleDefs (FaceId)
import Moonlight.Planar.Types (BuildError)
import Numeric.Natural (Natural)

data Exhibit = Exhibit
  { schemaVersion :: Int,
    exhibitTitle :: String,
    mesh :: MeshProjection,
    category :: CategoryProjection,
    nerve :: NerveProjection
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshProjection = MeshProjection
  { meshVertices :: [MeshVertex],
    meshEdges :: [MeshEdge],
    meshFaces :: [MeshFace]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshVertex = MeshVertex
  { meshVertexIdentifier :: String,
    meshVertexObjectIdentifier :: String,
    meshVertexPosition :: Position
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshEdge = MeshEdge
  { meshEdgeIdentifier :: String,
    meshEdgeObjectIdentifier :: String,
    meshEdgeVertexIdentifiers :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MeshFace = MeshFace
  { meshFaceIdentifier :: String,
    meshFaceObjectIdentifier :: String,
    meshFaceVertexIdentifiers :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Position = Position
  { positionX :: Double,
    positionY :: Double
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryProjection = CategoryProjection
  { categoryObjects :: [CategoryObject],
    categoryMorphisms :: [CategoryMorphism]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryObject = CategoryObject
  { categoryObjectIdentifier :: String,
    categoryObjectKind :: CategoryObjectKind,
    categoryObjectLabel :: String,
    categoryObjectPosition :: Position
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryObjectKind
  = VertexObject
  | EdgeObject
  | FaceObject
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data CategoryMorphism = CategoryMorphism
  { categoryMorphismIdentifier :: String,
    categoryMorphismKind :: CategoryMorphismKind,
    categoryMorphismSourceObject :: String,
    categoryMorphismTargetObject :: String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CategoryMorphismKind
  = IdentityMorphism
  | FaceEdgeMorphism
  | EdgeVertexMorphism
  | FaceVertexComposite
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data NerveProjection = NerveProjection
  { nerveTwoSimplices :: [NerveTwoSimplex],
    nerveSimplexCounts :: [NerveSimplexCount]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data NerveSimplexCount = NerveSimplexCount
  { nerveDimension :: Natural,
    normalizedSimplexCount :: Int,
    unnormalizedSimplexCount :: Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data NerveTwoSimplex = NerveTwoSimplex
  { nerveSimplexIdentifier :: String,
    nerveSimplexObjectIdentifiers :: [String]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data CellInventory = CellInventory
  { cellVertexCount :: Int,
    cellEdgeCount :: Int,
    cellFaceCount :: Int
  }
  deriving stock (Eq, Show)

data MorphismInventory = MorphismInventory
  { identityMorphismCount :: Int,
    faceEdgeMorphismCount :: Int,
    edgeVertexMorphismCount :: Int,
    faceVertexMorphismCount :: Int
  }
  deriving stock (Eq, Show)

instance Semigroup MorphismInventory where
  MorphismInventory leftIdentities leftFaceEdges leftEdgeVertices leftFaceVertices
    <> MorphismInventory rightIdentities rightFaceEdges rightEdgeVertices rightFaceVertices =
      MorphismInventory
        (leftIdentities + rightIdentities)
        (leftFaceEdges + rightFaceEdges)
        (leftEdgeVertices + rightEdgeVertices)
        (leftFaceVertices + rightFaceVertices)

instance Monoid MorphismInventory where
  mempty = MorphismInventory 0 0 0 0

data ExhibitError
  = ExhibitTriangulationBuildFailed BuildError
  | ExhibitCellSelectionFailed CellSelectionError
  | ExhibitCellComplexFailed DCELError
  | ExhibitAbstractCutPositionUnavailable FaceId
  | ExhibitMorphismEndpointUnavailable MorphismEndpoint Int
  | ExhibitFaceWithoutVertices FaceId
  | ExhibitMalformedTwoSimplex Int
  | ExhibitInventoryMismatch CellInventory CellInventory
  | ExhibitCategoryEnumerationMismatch MorphismInventory MorphismInventory
  | ExhibitNerveCountMismatch Int Int
  deriving stock (Show)

data MorphismEndpoint
  = MorphismSource
  | MorphismTarget
  deriving stock (Show)

renderExhibitError :: ExhibitError -> String
renderExhibitError = show

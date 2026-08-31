module Moonlight.Homology.Pure.Topology.CellComplex
  ( CellTypes (..),
    Dimension (..),
    CellRef (..),
    cellDimension,
    OrientedEdge (..),
    CellComplex2D (..),
    ValidateComplex2D (..),
    isBoundaryEdge,
    isInteriorEdge,
    eulerCharacteristic,
  )
where

import Data.Kind (Constraint, Type)
import Moonlight.Algebra.Pure.Orientation (Orientation)

type CellTypes :: Type -> Constraint
class
  ( Eq (Vertex c),
    Ord (Vertex c),
    Show (Vertex c),
    Eq (Edge c),
    Ord (Edge c),
    Show (Edge c),
    Eq (Face c),
    Ord (Face c),
    Show (Face c)
  ) =>
  CellTypes c
  where
  type Vertex c
  type Edge c
  type Face c

type Dimension :: Type
data Dimension
  = Dim0
  | Dim1
  | Dim2
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type CellRef :: Type -> Type -> Type -> Type
data CellRef vertex edge face
  = CellVertexRef vertex
  | CellEdgeRef edge
  | CellFaceRef face
  deriving stock (Eq, Ord, Show, Read)

cellDimension :: CellRef vertex edge face -> Dimension
cellDimension cellReference =
  case cellReference of
    CellVertexRef _ -> Dim0
    CellEdgeRef _ -> Dim1
    CellFaceRef _ -> Dim2

type OrientedEdge :: Type -> Type
data OrientedEdge edge = OrientedEdge
  { orientedEdge :: edge,
    edgeOrientation :: Orientation
  }
  deriving stock (Eq, Ord, Show, Read)

type CellComplex2D :: Type -> Constraint
class CellTypes c => CellComplex2D c where
  vertices :: c -> [Vertex c]
  edges :: c -> [Edge c]
  faces :: c -> [Face c]

  edgeBoundary :: c -> Edge c -> (Vertex c, Vertex c)
  faceBoundary :: c -> Face c -> [OrientedEdge (Edge c)]

  edgesAtVertex :: c -> Vertex c -> [Edge c]
  facesAtEdge :: c -> Edge c -> (Maybe (Face c), Maybe (Face c))

type ValidateComplex2D :: Type -> Constraint
class CellComplex2D c => ValidateComplex2D c where
  type ValidationIssue c
  validateComplex :: c -> [ValidationIssue c]

isBoundaryEdge :: CellComplex2D c => c -> Edge c -> Bool
isBoundaryEdge complex edge =
  case facesAtEdge complex edge of
    (Nothing, _) -> True
    (_, Nothing) -> True
    (Just _, Just _) -> False

isInteriorEdge :: CellComplex2D c => c -> Edge c -> Bool
isInteriorEdge complex edge =
  case facesAtEdge complex edge of
    (Just _, Just _) -> True
    _ -> False

eulerCharacteristic :: CellComplex2D c => c -> Int
eulerCharacteristic complex =
  length (vertices complex)
    - length (edges complex)
    + length (faces complex)

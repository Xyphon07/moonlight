{-# LANGUAGE EmptyDataDeriving #-}

-- | The admitted Moonlight triangulation cell section as a generic
-- 'CellComplex2D', an integral cellular chain complex, and an exact filtered
-- alpha complex. The 'ExactCellSet' remains the semantic owner: this module
-- supplies only the incidence and Homology interpretations required by
-- downstream topology.
module Moonlight.Triangulation.CellComplex
  ( DCELComplex,
    DCELError (..),
    fromExactCellSet,
    finiteChainComplex,
    filteredAlphaComplex,
  )
where

import Data.Bifunctor (first)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as Vector
import Moonlight.Algebra.Pure.Orientation (Orientation (..))
import Moonlight.Homology.Boundary
  ( BoundaryIncidence
  , BoundaryIncidenceShapeError
  , BoundaryEntry
  , FiniteChainComplex
  , emptyBoundaryIncidence
  , emptyBoundaryIncidenceOf
  , mkBoundaryEntryFromInts
  , mkBoundaryIncidenceFromOrderedColumns
  , mkBoundaryIncidenceFromOrderedEntries
  , mkFiniteChainComplexChecked
  , targetIndex
  )
import Moonlight.Homology.Chain
  ( HomologicalDegree (..)
  , HomologyFailure
  )
import Moonlight.Homology.Persistence
  ( FilteredFiniteChainComplex
  , mkFilteredFiniteChainComplex
  )
import Moonlight.Homology.Pure.Topology.CellComplex
  ( CellComplex2D (..)
  , CellRef (..)
  , CellTypes (..)
  , OrientedEdge (..)
  , ValidateComplex2D (..)
  )
import Moonlight.Homology.Topology (BasisCellRef (..))
import Moonlight.Triangulation.Alpha
  ( AlphaBirth
  , AlphaFiltration
  , alphaEdgeBirth
  , alphaFaceBirth
  , alphaFiltrationCellSet
  , alphaVertexBirth
  )
import Moonlight.Triangulation.Dcel qualified as Dcel
import Moonlight.Triangulation.Internal.HandleDefs
  ( DirectedEdgeId,
    FaceId (..),
    UndirectedEdgeId (..),
    VertexId (..),
    asUndirected,
    directedPair,
    isNormalized,
  )
import Moonlight.Triangulation.Internal.CellSet (ExactCellSet (..))
import Moonlight.Triangulation.Types (Triangulation)

-- | An incidence view of one already-validated, downward-closed exact cell
-- selection. It deliberately has no independent cell inventory.
newtype DCELComplex = DCELComplex ExactCellSet

-- | 'ExactCellSet' construction discharges every closure and handle
-- obligation before this view exists. Incidence materialization, Homology's
-- independent chain-law seal, and the exact-birth join retain typed
-- obstructions at their respective boundaries.
type DCELCellRef = CellRef VertexId UndirectedEdgeId FaceId

data DCELError
  = DCELBoundaryCellMissing !DCELCellRef !DCELCellRef
  | DCELBoundaryIncidenceInvalid !BoundaryIncidenceShapeError
  | DCELChainComplexInvalid !HomologyFailure
  | DCELAlphaBirthMissing !DCELCellRef
  | DCELFilteredComplexInvalid !HomologyFailure
  deriving stock (Eq, Show)

fromExactCellSet :: ExactCellSet -> DCELComplex
fromExactCellSet = DCELComplex

-- | Canonical integral cellular chains in ascending resident-handle order.
-- Degree one uses target minus source; degree two uses the DCEL's oriented
-- face boundary. The Homology boundary seals the result only after checking
-- shape and @d . d = 0@; no unchecked chain constructor crosses the public
-- package boundary.
finiteChainComplex :: DCELComplex -> Either DCELError (FiniteChainComplex Int)
finiteChainComplex complexValue =
  let basis = dcelBasis complexValue
   in finiteChainComplexWithBasis complexValue basis

-- | Lower the exact alpha section into Homology without converting its birth
-- order through binary64. Persistence remains wholly owned by Homology.
filteredAlphaComplex
  :: AlphaFiltration
  -> Either DCELError (FilteredFiniteChainComplex AlphaBirth Int)
filteredAlphaComplex filtration =
  case alphaFiltrationCellSet filtration of
    ExactCellSet triangulation _ _ _ -> do
      finite <- residentAlphaFiniteChainComplex triangulation
      vertexBirthAssignments <-
        traverse
          ( residentAlphaBirthAssignment
              CellVertexRef
              residentVertexBasisRef
              (alphaVertexBirth filtration)
          )
          (vertexHandlesOf triangulation)
      edgeBirthAssignments <-
        traverse
          ( residentAlphaBirthAssignment
              CellEdgeRef
              residentEdgeBasisRef
              (alphaEdgeBirth filtration)
          )
          (undirectedEdgesOf triangulation)
      faceBirthAssignments <-
        traverse
          ( residentAlphaBirthAssignment
              CellFaceRef
              residentFaceBasisRef
              (alphaFaceBirth filtration)
          )
          (innerFacesOf triangulation)
      first DCELFilteredComplexInvalid
        ( mkFilteredFiniteChainComplex
            finite
            (vertexBirthAssignments <> edgeBirthAssignments <> faceBirthAssignments)
        )

-- | The opaque 'AlphaFiltration' constructor admits the entire resident DCEL,
-- whose handle ranges are dense. This local section therefore lowers those
-- handles directly to basis indices while retaining Homology's independent
-- shape and nilpotence seal. Sparse 'ExactCellSet' values continue through the
-- generic map-indexed 'finiteChainComplex' path.
residentAlphaFiniteChainComplex
  :: Triangulation mode vertex directed undirected face
  -> Either DCELError (FiniteChainComplex Int)
residentAlphaFiniteChainComplex triangulation = do
  let degreeOneColumns =
        Vector.generate
          (Dcel.numUndirectedEdges triangulation)
          (residentEdgeBoundaryEntries triangulation . UndirectedEdgeId . fromIntegral)
      degreeTwoColumns =
        Vector.imap
          residentFaceBoundaryEntries
          (Dcel.innerFaceDirectedEdgeTriples triangulation)
  degreeOneBoundary <-
    first DCELBoundaryIncidenceInvalid
      ( mkBoundaryIncidenceFromOrderedColumns
          (fromIntegral (Dcel.numUndirectedEdges triangulation))
          (fromIntegral (Dcel.numVertices triangulation))
          degreeOneColumns
      )
  degreeTwoBoundary <-
    first DCELBoundaryIncidenceInvalid
      ( mkBoundaryIncidenceFromOrderedColumns
          (fromIntegral (Dcel.numInnerFaces triangulation))
          (fromIntegral (Dcel.numUndirectedEdges triangulation))
          degreeTwoColumns
      )
  let degreeZeroBoundary =
        emptyBoundaryIncidenceOf
          (fromIntegral (Dcel.numVertices triangulation))
          0
      boundaryAt (HomologicalDegree degreeValue) =
        case degreeValue of
          0 -> degreeZeroBoundary
          1 -> degreeOneBoundary
          2 -> degreeTwoBoundary
          _ -> emptyBoundaryIncidence
  first DCELChainComplexInvalid
    (mkFiniteChainComplexChecked (HomologicalDegree 2) boundaryAt)

residentEdgeBoundaryEntries
  :: Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> [BoundaryEntry Int]
residentEdgeBoundaryEntries triangulation edgeValue =
  let (sourceVertex, targetVertex) = Dcel.undirectedEndpoints triangulation edgeValue
      sourceEntry =
        mkBoundaryEntryFromInts
          (basisIndexOfEdge edgeValue)
          (basisIndexOfVertex sourceVertex)
          (-1)
      targetEntry =
        mkBoundaryEntryFromInts
          (basisIndexOfEdge edgeValue)
          (basisIndexOfVertex targetVertex)
          1
   in if targetIndex sourceEntry <= targetIndex targetEntry
        then [sourceEntry, targetEntry]
        else [targetEntry, sourceEntry]

residentFaceBoundaryEntries
  :: Int
  -> (DirectedEdgeId, DirectedEdgeId, DirectedEdgeId)
  -> [BoundaryEntry Int]
residentFaceBoundaryEntries faceIndexValue (firstEdge, secondEdge, thirdEdge) =
  sortThreeBoundaryEntries
    (residentFaceBoundaryEntry faceIndexValue firstEdge)
    (residentFaceBoundaryEntry faceIndexValue secondEdge)
    (residentFaceBoundaryEntry faceIndexValue thirdEdge)

sortThreeBoundaryEntries
  :: BoundaryEntry Int
  -> BoundaryEntry Int
  -> BoundaryEntry Int
  -> [BoundaryEntry Int]
sortThreeBoundaryEntries firstEntry secondEntry thirdEntry =
  let (firstLow, firstHigh) = orderedBoundaryPair firstEntry secondEntry
      (secondLow, finalHigh) = orderedBoundaryPair firstHigh thirdEntry
      (finalLow, finalMiddle) = orderedBoundaryPair firstLow secondLow
   in [finalLow, finalMiddle, finalHigh]

orderedBoundaryPair
  :: BoundaryEntry Int
  -> BoundaryEntry Int
  -> (BoundaryEntry Int, BoundaryEntry Int)
orderedBoundaryPair firstEntry secondEntry =
  if targetIndex firstEntry <= targetIndex secondEntry
    then (firstEntry, secondEntry)
    else (secondEntry, firstEntry)

residentFaceBoundaryEntry :: Int -> DirectedEdgeId -> BoundaryEntry Int
residentFaceBoundaryEntry faceIndexValue directedEdge =
  mkBoundaryEntryFromInts
    faceIndexValue
    (basisIndexOfEdge (asUndirected directedEdge))
    (if isNormalized directedEdge then 1 else -1)

residentAlphaBirthAssignment
  :: (cell -> DCELCellRef)
  -> (cell -> BasisCellRef)
  -> (cell -> Maybe AlphaBirth)
  -> cell
  -> Either DCELError (BasisCellRef, AlphaBirth)
residentAlphaBirthAssignment cellReference basisReference birthAt cell =
  maybe
    (Left (DCELAlphaBirthMissing (cellReference cell)))
    (Right . (,) (basisReference cell))
    (birthAt cell)

residentVertexBasisRef :: VertexId -> BasisCellRef
residentVertexBasisRef vertexValue =
  BasisCellRef (HomologicalDegree 0) (basisIndexOfVertex vertexValue)

residentEdgeBasisRef :: UndirectedEdgeId -> BasisCellRef
residentEdgeBasisRef edgeValue =
  BasisCellRef (HomologicalDegree 1) (basisIndexOfEdge edgeValue)

residentFaceBasisRef :: FaceId -> BasisCellRef
residentFaceBasisRef faceValue =
  BasisCellRef (HomologicalDegree 2) (basisIndexOfFace faceValue)

basisIndexOfVertex :: VertexId -> Int
basisIndexOfVertex (VertexId rawVertex) = fromIntegral rawVertex

basisIndexOfEdge :: UndirectedEdgeId -> Int
basisIndexOfEdge (UndirectedEdgeId rawEdge) = fromIntegral rawEdge

basisIndexOfFace :: FaceId -> Int
basisIndexOfFace (FaceId rawFace) = fromIntegral rawFace - 1

undirectedEdgesOf
  :: Triangulation mode vertex directed undirected face
  -> [UndirectedEdgeId]
undirectedEdgesOf triangulation =
  fmap (UndirectedEdgeId . fromIntegral) [0 .. Dcel.numUndirectedEdges triangulation - 1]

vertexHandlesOf
  :: Triangulation mode vertex directed undirected face
  -> [VertexId]
vertexHandlesOf triangulation =
  fmap (VertexId . fromIntegral) [0 .. Dcel.numVertices triangulation - 1]

innerFacesOf
  :: Triangulation mode vertex directed undirected face
  -> [FaceId]
innerFacesOf triangulation =
  fmap (FaceId . fromIntegral) [1 .. Dcel.numFaces triangulation - 1]

data DCELBasis = DCELBasis
  { dcelVertexBasis :: !(Map VertexId BasisCellRef)
  , dcelEdgeBasis :: !(Map UndirectedEdgeId BasisCellRef)
  , dcelFaceBasis :: !(Map FaceId BasisCellRef)
  }

dcelBasis :: DCELComplex -> DCELBasis
dcelBasis complexValue =
  DCELBasis
    { dcelVertexBasis = basisMap 0 (vertices complexValue)
    , dcelEdgeBasis = basisMap 1 (edges complexValue)
    , dcelFaceBasis = basisMap 2 (faces complexValue)
    }

basisMap :: Ord cell => Int -> [cell] -> Map cell BasisCellRef
basisMap degreeValue cells =
  Map.fromAscList
    ( zipWith
        (\indexValue cell -> (cell, BasisCellRef (HomologicalDegree degreeValue) indexValue))
        [0 ..]
        cells
    )

finiteChainComplexWithBasis
  :: DCELComplex
  -> DCELBasis
  -> Either DCELError (FiniteChainComplex Int)
finiteChainComplexWithBasis complexValue basis = do
  degreeOneEntries <-
    concat
      <$> traverse
        (edgeBoundaryEntries complexValue basis)
        (Map.toAscList (dcelEdgeBasis basis))
  degreeTwoEntries <-
    concat
      <$> traverse
        (faceBoundaryEntries complexValue basis)
        (Map.toAscList (dcelFaceBasis basis))
  degreeOneBoundary <-
    first DCELBoundaryIncidenceInvalid
      ( mkBoundaryIncidenceFromOrderedEntries
          (fromIntegral (Map.size (dcelEdgeBasis basis)))
          (fromIntegral (Map.size (dcelVertexBasis basis)))
          degreeOneEntries
      )
  degreeTwoBoundary <-
    first DCELBoundaryIncidenceInvalid
      ( mkBoundaryIncidenceFromOrderedEntries
          (fromIntegral (Map.size (dcelFaceBasis basis)))
          (fromIntegral (Map.size (dcelEdgeBasis basis)))
          degreeTwoEntries
      )
  let degreeZeroBoundary =
        emptyBoundaryIncidenceOf
          (fromIntegral (Map.size (dcelVertexBasis basis)))
          0
      boundaryAt :: HomologicalDegree -> BoundaryIncidence Int
      boundaryAt (HomologicalDegree degreeValue) =
        case degreeValue of
          0 -> degreeZeroBoundary
          1 -> degreeOneBoundary
          2 -> degreeTwoBoundary
          _ -> emptyBoundaryIncidence
  first DCELChainComplexInvalid
    (mkFiniteChainComplexChecked (HomologicalDegree 2) boundaryAt)

edgeBoundaryEntries
  :: DCELComplex
  -> DCELBasis
  -> (UndirectedEdgeId, BasisCellRef)
  -> Either DCELError [BoundaryEntry Int]
edgeBoundaryEntries complexValue basis (edgeValue, edgeBasisRef) = do
  let (sourceVertex, targetVertex) = edgeBoundary complexValue edgeValue
      sourceCell = CellEdgeRef edgeValue
  sourceBasisRef <-
    requireBoundaryCell
      sourceCell
      (CellVertexRef sourceVertex)
      sourceVertex
      (dcelVertexBasis basis)
  targetBasisRef <-
    requireBoundaryCell
      sourceCell
      (CellVertexRef targetVertex)
      targetVertex
      (dcelVertexBasis basis)
  pure
    ( List.sortOn
        targetIndex
        [ mkBoundaryEntryFromInts
            (cellIndex edgeBasisRef)
            (cellIndex sourceBasisRef)
            (-1)
        , mkBoundaryEntryFromInts
            (cellIndex edgeBasisRef)
            (cellIndex targetBasisRef)
            1
        ]
    )

faceBoundaryEntries
  :: DCELComplex
  -> DCELBasis
  -> (FaceId, BasisCellRef)
  -> Either DCELError [BoundaryEntry Int]
faceBoundaryEntries complexValue basis (faceValue, faceBasisRef) =
  List.sortOn targetIndex
    <$> traverse
      boundaryEntry
      (faceBoundary complexValue faceValue)
 where
  boundaryEntry orientedBoundary = do
    let edgeValue = orientedEdge orientedBoundary
    edgeBasisRef <-
      requireBoundaryCell
        (CellFaceRef faceValue)
        (CellEdgeRef edgeValue)
        edgeValue
        (dcelEdgeBasis basis)
    pure
      ( mkBoundaryEntryFromInts
          (cellIndex faceBasisRef)
          (cellIndex edgeBasisRef)
          (orientationCoefficient (edgeOrientation orientedBoundary))
      )

orientationCoefficient :: Orientation -> Int
orientationCoefficient orientation =
  case orientation of
    Positive -> 1
    Negative -> -1

requireBoundaryCell
  :: Ord cell
  => DCELCellRef
  -> DCELCellRef
  -> cell
  -> Map cell BasisCellRef
  -> Either DCELError BasisCellRef
requireBoundaryCell sourceCell targetCell cell basis =
  maybe
    (Left (DCELBoundaryCellMissing sourceCell targetCell))
    Right
    (Map.lookup cell basis)

instance CellTypes DCELComplex where
  type Vertex DCELComplex = VertexId
  type Edge DCELComplex = UndirectedEdgeId
  type Face DCELComplex = FaceId

instance CellComplex2D DCELComplex where
  vertices (DCELComplex (ExactCellSet _ selectedVertices _ _)) =
    fmap (VertexId . fromIntegral) (IntMap.keys selectedVertices)

  edges (DCELComplex (ExactCellSet _ _ selectedEdges _)) =
    fmap (UndirectedEdgeId . fromIntegral) (IntSet.toAscList selectedEdges)

  faces (DCELComplex (ExactCellSet _ _ _ selectedFaces)) =
    fmap (FaceId . fromIntegral) (IntSet.toAscList selectedFaces)

  edgeBoundary (DCELComplex (ExactCellSet triangulation _ _ _)) =
    Dcel.undirectedEndpoints triangulation

  faceBoundary (DCELComplex (ExactCellSet triangulation _ _ _)) face =
    fmap orientedBoundaryEdge (Dcel.faceDirectedEdges triangulation face)
    where
      orientedBoundaryEdge directedEdge =
        OrientedEdge
          { orientedEdge = asUndirected directedEdge,
            edgeOrientation =
              if isNormalized directedEdge
                then Positive
                else Negative
          }

  edgesAtVertex complexValue@(DCELComplex (ExactCellSet triangulation _ _ _)) vertex =
    filter (edgeContainsVertex triangulation vertex) (edges complexValue)

  facesAtEdge (DCELComplex (ExactCellSet triangulation _ _ selectedFaces)) edge =
    let (forward, backward) = directedPair edge
        selectedIncidentFace directedEdge =
          let face@(FaceId rawFace) = Dcel.incidentFace triangulation directedEdge
           in if IntSet.member (fromIntegral rawFace) selectedFaces
                then Just face
                else Nothing
     in (selectedIncidentFace forward, selectedIncidentFace backward)

instance ValidateComplex2D DCELComplex where
  type ValidationIssue DCELComplex = DCELError
  validateComplex _ = []

edgeContainsVertex ::
  Triangulation mode vertex directed undirected face ->
  VertexId ->
  UndirectedEdgeId ->
  Bool
edgeContainsVertex triangulation vertex edge =
  let (sourceVertex, targetVertex) = Dcel.undirectedEndpoints triangulation edge
   in vertex == sourceVertex || vertex == targetVertex

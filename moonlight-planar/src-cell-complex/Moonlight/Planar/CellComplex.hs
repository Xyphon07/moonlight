{-# LANGUAGE ScopedTypeVariables #-}

-- | An admitted exact geometric cell selection as a generic
-- 'CellComplex2D', an integral cellular chain complex, and an exact filtered
-- alpha complex. The 'ExactCellSet' remains the semantic owner: this module
-- supplies only the incidence and Homology interpretations required by
-- downstream topology.
module Moonlight.Planar.CellComplex
  ( DCELComplex,
    DCELCellEdge (..),
    FaceCut,
    faceCutFace,
    faceCutComponent,
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
import GHC.Exts (build)
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
import Moonlight.Planar.Alpha
  ( AlphaBirth
  , AlphaFiltration
  , alphaEdgeBirth
  , alphaFaceBirth
  , alphaVertexBirth
  , withAlphaResidentTriangulation
  )
import Moonlight.Planar.Dcel qualified as Dcel
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId,
    FaceId (..),
    UndirectedEdgeId (..),
    VertexId (..),
    asUndirected,
    directedPair,
    isNormalized,
  )
import Moonlight.Planar.Internal.CellSet (ExactCellSet (..))
import Moonlight.Planar.Internal.Incidence
  ( FaceBoundaryComponent (..),
    PlanarIncidence,
    circularEdgeWalk,
    faceInnerBoundaryComponents,
    incidenceAdjacentEdge,
    incidenceDirectedEdgeCount,
    incidenceIncidentFace,
    incidenceNext,
    incidenceNonCellularFaces,
    incidenceOrigin,
    incidenceUndirectedEndpoints,
  )
import Moonlight.Planar.Types (Triangulation)

-- | The cellular refinement of one closed geometric selection. The two cut
-- indices are derived views of its face components, never a second selection.
-- Native triangulations have empty cut indices and retain every native handle.
data DCELComplex = DCELComplex
  !ExactCellSet
  !(IntMap.IntMap [FaceCut])
  !(IntMap.IntMap [FaceCut])

-- | Geometric atoms retain their original identities. A cut is an abstract
-- arc in one face, not a fabricated resident DCEL edge or a new exact segment.
data DCELCellEdge
  = GeometricCellEdge !UndirectedEdgeId
  | FaceBoundaryCut !FaceCut
  deriving stock (Eq, Ord, Show)

-- Construction fixes endpoints from the sealed face-component incidence.
-- The owning face and component root give restriction-stable cut identities.
data FaceCut = FaceCut !FaceId !FaceBoundaryComponent !VertexId !VertexId
  deriving stock (Eq, Ord, Show)

faceCutFace :: FaceCut -> FaceId
faceCutFace (FaceCut face _ _ _) = face

faceCutComponent :: FaceCut -> FaceBoundaryComponent
faceCutComponent (FaceCut _ component _ _) = component

-- | Incidence materialization, Homology's independent chain-law seal, and
-- the exact-birth join retain typed obstructions at their respective boundaries.
type DCELCellRef = CellRef VertexId DCELCellEdge FaceId

data DCELError
  = DCELBoundaryCellMissing !DCELCellRef !DCELCellRef
  | DCELBoundaryIncidenceInvalid !BoundaryIncidenceShapeError
  | DCELChainComplexInvalid !HomologyFailure
  | DCELAlphaBirthMissing !DCELCellRef
  | DCELFilteredComplexInvalid !HomologyFailure
  | DCELBoundedFaceAnchorMissing !FaceId
  deriving stock (Eq, Show)

-- | Cut each disconnected inner boundary component to its bounded face's
-- outer component. Planar arrangement admission supplies the genus-zero face
-- and its oriented boundary-component decomposition; the standard cut-system
-- construction then gives one disk characteristic map. A slit or cut appears
-- twice oppositely in its attaching walk. Chain nilpotence checks the resulting
-- coefficients, but is not the proof that a holed stratum was a disk.
--
-- Only the sparse noncellular-face support is visited. Native triangulations
-- have empty support, so their construction remains constant-time.
fromExactCellSet :: ExactCellSet -> Either DCELError DCELComplex
fromExactCellSet cells@(ExactCellSet incidence _ _ selectedFaces) =
  case filter (\(FaceId rawFace) -> IntSet.member (fromIntegral rawFace) selectedFaces) (incidenceNonCellularFaces incidence) of
    [] -> Right (DCELComplex cells IntMap.empty IntMap.empty)
    nonCellularFaces -> do
      cutsByFace <-
        IntMap.fromDistinctAscList <$> traverse
          (\face@(FaceId rawFace) -> do
            root <- maybe (Left (DCELBoundedFaceAnchorMissing face)) Right (incidenceAdjacentEdge incidence face)
            let sourceVertex = incidenceOrigin incidence root
                cuts = List.sort
                  (fmap
                    (\component -> FaceCut face component sourceVertex (componentBaseVertex incidence component))
                    (faceInnerBoundaryComponents incidence face))
            pure (fromIntegral rawFace, cuts))
          nonCellularFaces
      let cutsByVertex =
            fmap List.sort
              ( IntMap.fromListWith (<>)
                  [ (basisIndexOfVertex vertex, [cut])
                  | cuts <- IntMap.elems cutsByFace,
                    cut@(FaceCut _ _ sourceVertex targetVertex) <- cuts,
                    vertex <- [sourceVertex, targetVertex]
                  ]
              )
      pure (DCELComplex cells cutsByFace cutsByVertex)

componentBaseVertex :: PlanarIncidence -> FaceBoundaryComponent -> VertexId
componentBaseVertex incidence component =
  case component of
    BoundaryCycleRoot root -> incidenceOrigin incidence root
    IsolatedBoundaryVertex vertex -> vertex

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
  withAlphaResidentTriangulation
    (\triangulation -> do
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
              (CellEdgeRef . GeometricCellEdge)
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
    )
    filtration

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
  , dcelEdgeBasis :: !(Map DCELCellEdge BasisCellRef)
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
  -> (DCELCellEdge, BasisCellRef)
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
    ( case orderedBoundaryPair
        ( mkBoundaryEntryFromInts
            (cellIndex edgeBasisRef)
            (cellIndex sourceBasisRef)
            (-1)
        )
        ( mkBoundaryEntryFromInts
            (cellIndex edgeBasisRef)
            (cellIndex targetBasisRef)
            1
        ) of
        (lowerEntry, higherEntry) -> [lowerEntry, higherEntry]
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
  type Edge DCELComplex = DCELCellEdge
  type Face DCELComplex = FaceId

instance CellComplex2D DCELComplex where
  vertices (DCELComplex (ExactCellSet _ selectedVertices _ _) _ _) =
    build
      (\emit finish ->
        IntMap.foldrWithKey
          (\rawVertex _ remaining -> emit (VertexId (fromIntegral rawVertex)) remaining)
          finish
          selectedVertices)
  {-# INLINE vertices #-}

  edges (DCELComplex (ExactCellSet _ _ selectedEdges _) cutsByFace _) =
    if IntMap.null cutsByFace
      then build foldGeometricEdges
      else build
        (\emit finish ->
          foldGeometricEdges emit
            (IntMap.foldr (\cuts remaining -> foldr (emit . FaceBoundaryCut) remaining cuts) finish cutsByFace))
    where
      foldGeometricEdges :: (DCELCellEdge -> result -> result) -> result -> result
      foldGeometricEdges emit finish =
        IntSet.foldr
          (\rawEdge remaining -> emit (GeometricCellEdge (UndirectedEdgeId (fromIntegral rawEdge))) remaining)
          finish
          selectedEdges
      {-# INLINE foldGeometricEdges #-}
  {-# INLINE edges #-}

  faces (DCELComplex (ExactCellSet _ _ _ selectedFaces) _ _) =
    build
      (\emit finish ->
        IntSet.foldr
          (\rawFace remaining -> emit (FaceId (fromIntegral rawFace)) remaining)
          finish
          selectedFaces)
  {-# INLINE faces #-}

  edgeBoundary (DCELComplex (ExactCellSet incidence _ _ _) _ _) edgeValue =
    case edgeValue of
      GeometricCellEdge edge -> incidenceUndirectedEndpoints incidence edge
      FaceBoundaryCut (FaceCut _ _ sourceVertex targetVertex) -> (sourceVertex, targetVertex)
  {-# INLINE edgeBoundary #-}

  faceBoundary (DCELComplex (ExactCellSet incidence _ _ _) cutsByFace _) face@(FaceId rawFace) =
    build
      (\(emit :: OrientedEdge DCELCellEdge -> result -> result) (finish :: result) ->
        let geometricBoundary :: DirectedEdgeId -> result -> result
            geometricBoundary root remaining =
              foldr
                (emit . orientedBoundaryEdge)
                remaining
                (circularEdgeWalk (incidenceDirectedEdgeCount incidence) root (incidenceNext incidence))
            cutBoundary :: FaceCut -> result -> result
            cutBoundary cut remaining =
              let innerTail = emit (OrientedEdge (FaceBoundaryCut cut) Negative) remaining
               in emit (OrientedEdge (FaceBoundaryCut cut) Positive)
                    (case faceCutComponent cut of
                       BoundaryCycleRoot root -> geometricBoundary root innerTail
                       IsolatedBoundaryVertex _ -> innerTail)
            cutTail :: result
            cutTail = foldr cutBoundary finish (IntMap.findWithDefault [] (fromIntegral rawFace) cutsByFace)
         in maybe cutTail (\root -> geometricBoundary root cutTail) (incidenceAdjacentEdge incidence face))
    where
      orientedBoundaryEdge :: DirectedEdgeId -> OrientedEdge DCELCellEdge
      orientedBoundaryEdge directedEdge =
        OrientedEdge
          { orientedEdge = GeometricCellEdge (asUndirected directedEdge),
            edgeOrientation =
              if isNormalized directedEdge
                then Positive
                else Negative
          }
  {-# INLINE faceBoundary #-}

  edgesAtVertex (DCELComplex (ExactCellSet incidence _ selectedEdges _) _ cutsByVertex) vertex =
    case IntMap.lookup (basisIndexOfVertex vertex) cutsByVertex of
      Nothing -> build foldIncidentGeometricEdges
      Just cuts -> build
        (\emit finish -> foldIncidentGeometricEdges emit (foldr (emit . FaceBoundaryCut) finish cuts))
    where
      foldIncidentGeometricEdges :: (DCELCellEdge -> result -> result) -> result -> result
      foldIncidentGeometricEdges emit finish =
        IntSet.foldr
          (\rawEdge remaining ->
            let edge = UndirectedEdgeId (fromIntegral rawEdge)
             in if edgeContainsVertex incidence vertex edge
                  then emit (GeometricCellEdge edge) remaining
                  else remaining)
          finish
          selectedEdges
      {-# INLINE foldIncidentGeometricEdges #-}
  {-# INLINE edgesAtVertex #-}

  facesAtEdge (DCELComplex (ExactCellSet incidence _ _ selectedFaces) _ _) edgeValue =
    let selectedFace face@(FaceId rawFace) =
          if IntSet.member (fromIntegral rawFace) selectedFaces then Just face else Nothing
     in case edgeValue of
          GeometricCellEdge edge ->
            let (forward, backward) = directedPair edge
             in (selectedFace (incidenceIncidentFace incidence forward), selectedFace (incidenceIncidentFace incidence backward))
          FaceBoundaryCut cut ->
            let owner = selectedFace (faceCutFace cut)
             in (owner, owner)

instance ValidateComplex2D DCELComplex where
  type ValidationIssue DCELComplex = DCELError
  validateComplex _ = []

edgeContainsVertex ::
  PlanarIncidence ->
  VertexId ->
  UndirectedEdgeId ->
  Bool
edgeContainsVertex incidence vertex edge =
  let (sourceVertex, targetVertex) = incidenceUndirectedEndpoints incidence edge
   in vertex == sourceVertex || vertex == targetVertex

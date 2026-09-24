{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Invariant-bearing finite closed cell selections. Geometry-free incidence
-- and the exact coordinates of precisely the selected vertices remain sealed
-- together, without retaining native coordinates, payloads or search caches.
module Moonlight.Planar.Internal.CellSet
  ( ExactCellSet (..)
  , CellSelectionError (..)
  , exactCellSet
  , residentExactCellSet
  , closeFaceCellSet
  , closeExactCellSetWith
  , exactCellSetVertexCount
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , foldExactCellVertices
  , foldExactCellEdges
  , foldExactCellFaces
  , exactCellSetIsFaceClosure
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import GHC.Generics (Generic)
import Moonlight.Planar.Dcel
  ( numFaces
  , numUndirectedEdges
  , numVertices
  , vertexPoint
  )
import Moonlight.Planar.Internal.Incidence
  ( PlanarIncidence
  , faceBoundaryComponents
  , faceIsolatedVertices
  , incidenceEdgeCount
  , incidenceFaceCount
  , incidenceUndirectedEndpoints
  , incidenceVertexCount
  , nativePlanarIncidence
  )
import Moonlight.Planar.Exact (ExactPoint, exactPointFromPoint)
import Moonlight.Planar.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , asUndirected
  , faceIdIndex
  , undirectedEdgeIdIndex
  , vertexIdIndex
  )
import Moonlight.Planar.Internal.Representation (Triangulation)
import Moonlight.Planar.Point (PointValidationError)

-- | The keys of the exact-point map are the selected vertices; a second
-- vertex set would merely be a disagreeable copy.
data ExactCellSet = ExactCellSet
  !PlanarIncidence
  !(IntMap.IntMap ExactPoint)
  !IntSet.IntSet
  !IntSet.IntSet

data ClosedCellIds = ClosedCellIds
  { closedVertexIds :: !IntSet.IntSet
  , closedEdgeIds :: !IntSet.IntSet
  , closedFaceIds :: !IntSet.IntSet
  }

data CellSelectionError
  = CellVertexOutOfRange !VertexId !Int
  | CellEdgeOutOfRange !UndirectedEdgeId !Int
  | CellFaceOutOfRange !FaceId !Int
  | CellOuterFaceSelected
  | CellCoordinateInvalid !VertexId !PointValidationError
  | CellEdgeBoundaryMissing !UndirectedEdgeId !VertexId
  | CellFaceEdgeMissing !FaceId !UndirectedEdgeId
  | CellFaceVertexMissing !FaceId !VertexId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The strict authoring boundary validates handles and closure once, then
-- converts only selected vertices.
exactCellSet
  :: Triangulation mode vertex directed undirected face
  -> [VertexId]
  -> [UndirectedEdgeId]
  -> [FaceId]
  -> Either CellSelectionError ExactCellSet
exactCellSet triangulation selectedVertices selectedEdges selectedFaces = do
  let incidence = nativePlanarIncidence triangulation
      verticesSet = vertexSet selectedVertices
      edgesSet = edgeSet selectedEdges
      facesSet = faceSet selectedFaces
  validateHandles incidence selectedVertices selectedEdges selectedFaces
  traverse_ (validateEdgeClosure incidence verticesSet) selectedEdges
  traverse_ (validateFaceClosure incidence verticesSet edgesSet) selectedFaces
  sealCellSet
    (ordinaryExactPoint triangulation)
    incidence
    (ClosedCellIds verticesSet edgesSet facesSet)

-- | Seal the complete resident DCEL without rechecking ranges or closure
-- already established by its dense handle families. Exact coordinates are
-- still admitted at this representation boundary; only impossible handle
-- refusals and repeated incidence walks disappear.
residentExactCellSet
  :: Triangulation mode vertex directed undirected face
  -> Either CellSelectionError ExactCellSet
residentExactCellSet triangulation = do
  let vertexIndices = [0 .. numVertices triangulation - 1]
      edgeIndices = [0 .. numUndirectedEdges triangulation - 1]
      faceIndices = [1 .. numFaces triangulation - 1]
  exactPoints <-
    IntMap.fromDistinctAscList
      <$> traverse
        ( \residentIndex -> do
            point <- ordinaryExactPoint triangulation (VertexId (fromIntegral residentIndex))
            pure (residentIndex, point)
        )
        vertexIndices
  pure
    ( ExactCellSet
        (nativePlanarIncidence triangulation)
        exactPoints
        (IntSet.fromDistinctAscList edgeIndices)
        (IntSet.fromDistinctAscList faceIndices)
    )

-- | Close a bounded face selection over all resident boundary cells. Face
-- handles are author input and are checked; constructed closure is not then
-- pointlessly proved a second time.
closeFaceCellSet
  :: Triangulation mode vertex directed undirected face
  -> [FaceId]
  -> Either CellSelectionError ExactCellSet
closeFaceCellSet triangulation selectedFaces = do
  let incidence = nativePlanarIncidence triangulation
  validateHandles incidence [] [] selectedFaces
  sealCellSet
    (ordinaryExactPoint triangulation)
    incidence
    (closeCellIds incidence [] [] selectedFaces)

-- | Seal exact-arrangement closure derived by its trusted downstream owner.
-- Handles come from this admitted incidence, so no coordinate projection,
-- native mesh or repeated handle validation is introduced.
closeExactCellSetWith
  :: (VertexId -> Either CellSelectionError ExactPoint)
  -> PlanarIncidence
  -> [VertexId]
  -> [UndirectedEdgeId]
  -> [FaceId]
  -> Either CellSelectionError ExactCellSet
closeExactCellSetWith exactPointAt incidence explicitVertices explicitEdges selectedFaces =
  sealCellSet
    exactPointAt
    incidence
    (closeCellIds incidence explicitVertices explicitEdges selectedFaces)

ordinaryExactPoint
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> Either CellSelectionError ExactPoint
ordinaryExactPoint triangulation vertex =
  first (CellCoordinateInvalid vertex)
    (exactPointFromPoint (vertexPoint triangulation vertex))

sealCellSet
  :: (VertexId -> Either CellSelectionError ExactPoint)
  -> PlanarIncidence
  -> ClosedCellIds
  -> Either CellSelectionError ExactCellSet
sealCellSet exactPointAt incidence closed = do
  exactPoints <- exactPointsFor exactPointAt (closedVertexIds closed)
  pure
    ( ExactCellSet
        incidence
        exactPoints
        (closedEdgeIds closed)
        (closedFaceIds closed)
    )

closeCellIds
  :: PlanarIncidence
  -> [VertexId]
  -> [UndirectedEdgeId]
  -> [FaceId]
  -> ClosedCellIds
closeCellIds incidence explicitVertices explicitEdges selectedFaces =
  let faceEdges =
        concatMap
          (map asUndirected . concat . faceBoundaryComponents incidence)
          selectedFaces
      edges = edgeSet (explicitEdges <> faceEdges)
      closedEdges = map (UndirectedEdgeId . fromIntegral) (IntSet.toAscList edges)
      edgeVertices =
        concatMap
          (\edge ->
             let (from, to) = incidenceUndirectedEndpoints incidence edge
              in [from, to])
          closedEdges
      vertices =
        vertexSet
          ( explicitVertices
              <> edgeVertices
              <> concatMap (faceIsolatedVertices incidence) selectedFaces
          )
   in ClosedCellIds
        { closedVertexIds = vertices
        , closedEdgeIds = edges
        , closedFaceIds = faceSet selectedFaces
        }

exactPointsFor
  :: (VertexId -> Either CellSelectionError ExactPoint)
  -> IntSet.IntSet
  -> Either CellSelectionError (IntMap.IntMap ExactPoint)
exactPointsFor exactPointAt selected =
  IntMap.fromAscList
    <$> traverse
      (\index -> do
         point <- exactPointAt (VertexId (fromIntegral index))
         pure (index, point))
      (IntSet.toAscList selected)

validateHandles
  :: PlanarIncidence
  -> [VertexId]
  -> [UndirectedEdgeId]
  -> [FaceId]
  -> Either CellSelectionError ()
validateHandles incidence selectedVertices selectedEdges selectedFaces = do
  traverse_ validateVertex selectedVertices
  traverse_ validateEdge selectedEdges
  traverse_ validateFace selectedFaces
 where
  validateVertex vertex
    | vertexIdIndex vertex < incidenceVertexCount incidence = Right ()
    | otherwise = Left (CellVertexOutOfRange vertex (incidenceVertexCount incidence))
  validateEdge edge
    | undirectedEdgeIdIndex edge < incidenceEdgeCount incidence = Right ()
    | otherwise = Left (CellEdgeOutOfRange edge (incidenceEdgeCount incidence))
  validateFace face
    | faceIdIndex face == 0 = Left CellOuterFaceSelected
    | faceIdIndex face < incidenceFaceCount incidence = Right ()
    | otherwise = Left (CellFaceOutOfRange face (incidenceFaceCount incidence))

validateEdgeClosure
  :: PlanarIncidence
  -> IntSet.IntSet
  -> UndirectedEdgeId
  -> Either CellSelectionError ()
validateEdgeClosure incidence selectedVertices edge =
  traverse_ requireVertex [from, to]
 where
  (from, to) = incidenceUndirectedEndpoints incidence edge
  requireVertex vertex
    | IntSet.member (vertexIdIndex vertex) selectedVertices = Right ()
    | otherwise = Left (CellEdgeBoundaryMissing edge vertex)

validateFaceClosure
  :: PlanarIncidence
  -> IntSet.IntSet
  -> IntSet.IntSet
  -> FaceId
  -> Either CellSelectionError ()
validateFaceClosure incidence selectedVertices selectedEdges face = do
  traverse_ (requireEdge . asUndirected) (concat (faceBoundaryComponents incidence face))
  traverse_ requireVertex (faceIsolatedVertices incidence face)
 where
  requireEdge edge
    | IntSet.member (undirectedEdgeIdIndex edge) selectedEdges = Right ()
    | otherwise = Left (CellFaceEdgeMissing face edge)
  requireVertex vertex
    | IntSet.member (vertexIdIndex vertex) selectedVertices = Right ()
    | otherwise = Left (CellFaceVertexMissing face vertex)

vertexSet :: [VertexId] -> IntSet.IntSet
vertexSet = IntSet.fromList . map vertexIdIndex

edgeSet :: [UndirectedEdgeId] -> IntSet.IntSet
edgeSet = IntSet.fromList . map undirectedEdgeIdIndex

faceSet :: [FaceId] -> IntSet.IntSet
faceSet = IntSet.fromList . map faceIdIndex

exactCellSetVertexCount :: ExactCellSet -> Int
exactCellSetVertexCount (ExactCellSet _ selected _ _) = IntMap.size selected

exactCellSetEdgeCount :: ExactCellSet -> Int
exactCellSetEdgeCount (ExactCellSet _ _ selected _) = IntSet.size selected

exactCellSetFaceCount :: ExactCellSet -> Int
exactCellSetFaceCount (ExactCellSet _ _ _ selected) = IntSet.size selected

foldExactCellVertices
  :: (accumulator -> VertexId -> ExactPoint -> accumulator)
  -> accumulator
  -> ExactCellSet
  -> accumulator
foldExactCellVertices step initial (ExactCellSet _ selected _ _) =
  IntMap.foldlWithKey'
    (\accumulator index point ->
       step accumulator (VertexId (fromIntegral index)) point)
    initial
    selected

foldExactCellEdges
  :: (accumulator -> UndirectedEdgeId -> accumulator)
  -> accumulator
  -> ExactCellSet
  -> accumulator
foldExactCellEdges step initial (ExactCellSet _ _ selected _) =
  IntSet.foldl'
    (\accumulator index -> step accumulator (UndirectedEdgeId (fromIntegral index)))
    initial
    selected

foldExactCellFaces
  :: (accumulator -> FaceId -> accumulator)
  -> accumulator
  -> ExactCellSet
  -> accumulator
foldExactCellFaces step initial (ExactCellSet _ _ _ selected) =
  IntSet.foldl'
    (\accumulator index -> step accumulator (FaceId (fromIntegral index)))
    initial
    selected

-- | Whether the value contains exactly the downward closure of its selected
-- faces, with no additional isolated vertex or edge cells. This is the precise
-- admission condition for the conventional polygonal perimeter projection.
exactCellSetIsFaceClosure :: ExactCellSet -> Bool
exactCellSetIsFaceClosure (ExactCellSet incidence points edges faces) =
  let selectedFaces =
        map (FaceId . fromIntegral) (IntSet.toAscList faces)
      closed = closeCellIds incidence [] [] selectedFaces
   in IntMap.keysSet points == closedVertexIds closed
        && edges == closedEdgeIds closed
        && faces == closedFaceIds closed

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

-- | Invariant-bearing finite closed cell selections. The resident DCEL and
-- the exact coordinates of precisely the selected vertices remain sealed
-- together; no detached handle mask or whole-mesh projection exists.
module Moonlight.Triangulation.Internal.CellSet
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
import Moonlight.Triangulation.Dcel
  ( faceDirectedEdges
  , numFaces
  , numUndirectedEdges
  , numVertices
  , undirectedEndpoints
  , vertexPoint
  )
import Moonlight.Triangulation.Exact (ExactPoint, exactPointFromPoint)
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , asUndirected
  , faceIdIndex
  , undirectedEdgeIdIndex
  , vertexIdIndex
  )
import Moonlight.Triangulation.Internal.Representation (Triangulation)
import Moonlight.Triangulation.Internal.Types (PointValidationError)

-- | The keys of the exact-point map are the selected vertices; a second
-- vertex set would merely be a disagreeable copy.
data ExactCellSet where
  ExactCellSet
    :: !(Triangulation mode vertex directed undirected face)
    -> !(IntMap.IntMap ExactPoint)
    -> !IntSet.IntSet
    -> !IntSet.IntSet
    -> ExactCellSet

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
  validateHandles triangulation selectedVertices selectedEdges selectedFaces
  let verticesSet = vertexSet selectedVertices
      edgesSet = edgeSet selectedEdges
      facesSet = faceSet selectedFaces
  traverse_ (validateEdgeClosure triangulation verticesSet) selectedEdges
  traverse_ (validateFaceClosure triangulation edgesSet) selectedFaces
  sealCellSet
    (ordinaryExactPoint triangulation)
    triangulation
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
        triangulation
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
  validateHandles triangulation [] [] selectedFaces
  sealCellSet
    (ordinaryExactPoint triangulation)
    triangulation
    (closeCellIds triangulation [] [] selectedFaces)

-- | Seal closure derived by a trusted downstream owner. Overlay supplies
-- handles from this very DCEL, so repeating range and closure validation would
-- protect against an impossible phase while charging every selection for it.
closeExactCellSetWith
  :: (VertexId -> Either CellSelectionError ExactPoint)
  -> Triangulation mode vertex directed undirected face
  -> [VertexId]
  -> [UndirectedEdgeId]
  -> [FaceId]
  -> Either CellSelectionError ExactCellSet
closeExactCellSetWith exactPointAt triangulation explicitVertices explicitEdges selectedFaces =
  sealCellSet
    exactPointAt
    triangulation
    (closeCellIds triangulation explicitVertices explicitEdges selectedFaces)

ordinaryExactPoint
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> Either CellSelectionError ExactPoint
ordinaryExactPoint triangulation vertex =
  first (CellCoordinateInvalid vertex)
    (exactPointFromPoint (vertexPoint triangulation vertex))

sealCellSet
  :: (VertexId -> Either CellSelectionError ExactPoint)
  -> Triangulation mode vertex directed undirected face
  -> ClosedCellIds
  -> Either CellSelectionError ExactCellSet
sealCellSet exactPointAt triangulation closed = do
  exactPoints <- exactPointsFor exactPointAt (closedVertexIds closed)
  pure
    ( ExactCellSet
        triangulation
        exactPoints
        (closedEdgeIds closed)
        (closedFaceIds closed)
    )

closeCellIds
  :: Triangulation mode vertex directed undirected face
  -> [VertexId]
  -> [UndirectedEdgeId]
  -> [FaceId]
  -> ClosedCellIds
closeCellIds triangulation explicitVertices explicitEdges selectedFaces =
  let faceEdges =
        concatMap
          (map asUndirected . faceDirectedEdges triangulation)
          selectedFaces
      edges = edgeSet (explicitEdges <> faceEdges)
      closedEdges = map (UndirectedEdgeId . fromIntegral) (IntSet.toAscList edges)
      edgeVertices =
        concatMap
          (\edge ->
             let (from, to) = undirectedEndpoints triangulation edge
              in [from, to])
          closedEdges
      vertices =
        vertexSet
          ( explicitVertices
              <> edgeVertices
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
  :: Triangulation mode vertex directed undirected face
  -> [VertexId]
  -> [UndirectedEdgeId]
  -> [FaceId]
  -> Either CellSelectionError ()
validateHandles triangulation selectedVertices selectedEdges selectedFaces = do
  traverse_ validateVertex selectedVertices
  traverse_ validateEdge selectedEdges
  traverse_ validateFace selectedFaces
 where
  validateVertex vertex
    | vertexIdIndex vertex < numVertices triangulation = Right ()
    | otherwise = Left (CellVertexOutOfRange vertex (numVertices triangulation))
  validateEdge edge
    | undirectedEdgeIdIndex edge < numUndirectedEdges triangulation = Right ()
    | otherwise = Left (CellEdgeOutOfRange edge (numUndirectedEdges triangulation))
  validateFace face
    | faceIdIndex face == 0 = Left CellOuterFaceSelected
    | faceIdIndex face < numFaces triangulation = Right ()
    | otherwise = Left (CellFaceOutOfRange face (numFaces triangulation))

validateEdgeClosure
  :: Triangulation mode vertex directed undirected face
  -> IntSet.IntSet
  -> UndirectedEdgeId
  -> Either CellSelectionError ()
validateEdgeClosure triangulation selectedVertices edge =
  traverse_ requireVertex [from, to]
 where
  (from, to) = undirectedEndpoints triangulation edge
  requireVertex vertex
    | IntSet.member (vertexIdIndex vertex) selectedVertices = Right ()
    | otherwise = Left (CellEdgeBoundaryMissing edge vertex)

validateFaceClosure
  :: Triangulation mode vertex directed undirected face
  -> IntSet.IntSet
  -> FaceId
  -> Either CellSelectionError ()
validateFaceClosure triangulation selectedEdges face =
  traverse_ (requireEdge . asUndirected) (faceDirectedEdges triangulation face)
 where
  requireEdge edge
    | IntSet.member (undirectedEdgeIdIndex edge) selectedEdges = Right ()
    | otherwise = Left (CellFaceEdgeMissing face edge)

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
exactCellSetIsFaceClosure (ExactCellSet triangulation points edges faces) =
  let selectedFaces =
        map (FaceId . fromIntegral) (IntSet.toAscList faces)
      closed = closeCellIds triangulation [] [] selectedFaces
   in IntMap.keysSet points == closedVertexIds closed
        && edges == closedEdgeIds closed
        && faces == closedFaceIds closed

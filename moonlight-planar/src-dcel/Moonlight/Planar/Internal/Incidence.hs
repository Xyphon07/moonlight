{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Immutable geometry-free planar incidence. Native meshes share their three
-- packed topology planes; exact arrangements admit the same paired-edge layout
-- once, with sparse additional boundary components and isolated vertices.
module Moonlight.Planar.Internal.Incidence
  ( PlanarIncidence
  , PlanarIncidenceError (..)
  , FaceBoundaryComponent (..)
  , nativePlanarIncidence
  , admitPlanarIncidence
  , validatePlanarIncidence
  , validateIncidenceEdgeLinks
  , incidenceVertexCount
  , incidenceDirectedEdgeCount
  , incidenceEdgeCount
  , incidenceFaceCount
  , incidenceNonCellularFaces
  , incidenceVertexOutEdge
  , incidenceAdjacentEdge
  , incidenceOrigin
  , incidenceDestination
  , incidenceNext
  , incidencePrevious
  , incidenceIncidentFace
  , incidenceUndirectedEndpoints
  , incidenceVertexOutgoingEdges
  , faceBoundaryComponents
  , faceInnerBoundaryComponents
  , faceIsolatedVertices
  , faceEulerContribution
  , packedOptionalEdge
  , packedOrigin
  , packedNext
  , packedPrevious
  , packedIncidentFace
  , circularEdgeWalk
  , circularEdgeFold
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (unless)
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (maybeToList)
import qualified Data.List as List
import Data.Word (Word32)
import GHC.Exts (build)
import GHC.Generics (Generic)
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.PackedIndex (indexLimit, unpackOptionalIndex)
import Moonlight.Planar.Internal.Paged (Paged, pagedLength, pagedUnsafeIndex)
import Moonlight.Planar.Internal.Representation (Triangulation (..))
import Moonlight.Planar.Internal.Types (PlanarIncidenceError (..))

-- No Generic or exported record updates: packed admission is the only raw
-- construction boundary. The sparse maps describe components, not geometry.
data PlanarIncidence = PlanarIncidence
  !(Paged Word32)
  !(Paged Word32)
  !(Paged Word32)
  !(IntMap.IntMap [DirectedEdgeId])
  !(IntMap.IntMap IntSet.IntSet)
  deriving stock (Eq, Show)

instance NFData PlanarIncidence where
  rnf (PlanarIncidence vertices topology faces components isolated) =
    rnf vertices `seq` rnf topology `seq` rnf faces `seq` rnf components `seq` rnf isolated

data FaceBoundaryComponent
  = BoundaryCycleRoot !DirectedEdgeId
  | IsolatedBoundaryVertex !VertexId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | O(1) projection retaining the exact same immutable paged arrays. The only
-- native disconnected zero-cell is the singleton mesh, owned by face zero.
nativePlanarIncidence :: Triangulation mode vertex directed undirected face -> PlanarIncidence
nativePlanarIncidence mesh =
  PlanarIncidence
    (triVertexOut mesh)
    (triHalfTopology mesh)
    (triFaceEdge mesh)
    IntMap.empty
    ( if pagedLength (triVertexOut mesh) == 1 && pagedLength (triHalfTopology mesh) == 0
        then IntMap.singleton 0 (IntSet.singleton 0)
        else IntMap.empty
    )
{-# INLINE nativePlanarIncidence #-}

-- | Admit one exact arrangement's packed incidence. Geometry, angular order,
-- component orientation and containment are owned by arrangement construction;
-- this boundary checks only the stored combinatorial obligations.
admitPlanarIncidence
  :: Paged Word32
  -> Paged Word32
  -> Paged Word32
  -> IntMap.IntMap [DirectedEdgeId]
  -> IntMap.IntMap IntSet.IntSet
  -> Either PlanarIncidenceError PlanarIncidence
admitPlanarIncidence vertices topology faces components isolated = do
  let incidence = PlanarIncidence vertices topology faces components isolated
  validatePlanarIncidence incidence
  pure incidence

-- | Discharge the same finite laws for either packed admission or the native
-- O(1) projection. Ranges precede every linked read and orbit traversal.
validatePlanarIncidence :: PlanarIncidence -> Either PlanarIncidenceError ()
validatePlanarIncidence incidence@(PlanarIncidence _ topology _ components isolated) = do
  let vertexCount = incidenceVertexCount incidence
      directedCount = incidenceDirectedEdgeCount incidence
      faceCount = incidenceFaceCount incidence
      topologyWords = pagedLength topology
      vertexIds = fmap (VertexId . fromIntegral) [0 .. vertexCount - 1]
      edgeIds = fmap (DirectedEdgeId . fromIntegral) [0 .. directedCount - 1]
      faceIds = fmap (FaceId . fromIntegral) [0 .. faceCount - 1]
  unless
    ( topologyWords `rem` 8 == 0 && faceCount > 0
        && vertexCount <= indexLimit && directedCount <= indexLimit && faceCount <= indexLimit
    )
    (Left (IncidencePackedLayoutInvalid vertexCount topologyWords faceCount))
  traverse_ (validateEdgeRanges incidence) edgeIds
  traverse_ (validateIncidenceEdgeLinks incidence) edgeIds
  traverse_ (validateVertexRoot incidence) vertexIds
  traverse_ (validateSparseFace incidence) (IntMap.keys components <> IntMap.keys isolated)
  traverse_ (validateFaceRoots incidence) faceIds
  let edgeUniverse = IntSet.fromDistinctAscList [0 .. directedCount - 1]
      faceEdges = concatMap (concat . faceBoundaryComponents incidence) faceIds
      starEdges = concatMap (incidenceVertexOutgoingEdges incidence) vertexIds
  validateCoverage IncidenceBoundaryCoverageInvalid edgeUniverse (fmap directedEdgeIdIndex faceEdges)
  validateCoverage IncidenceVertexStarCoverageInvalid edgeUniverse (fmap directedEdgeIdIndex starEdges)
  let isolatedUniverse = IntSet.fromDistinctAscList
        [vertexIdIndex vertex | vertex <- vertexIds, incidenceVertexOutEdge incidence vertex == Nothing]
  validateCoverage IncidenceIsolatedOwnershipInvalid isolatedUniverse
    (concatMap IntSet.toAscList (IntMap.elems isolated))

validateEdgeRanges :: PlanarIncidence -> DirectedEdgeId -> Either PlanarIncidenceError ()
validateEdgeRanges incidence edge = do
  let vertex = incidenceOrigin incidence edge
      nextEdge = incidenceNext incidence edge
      previousEdge = incidencePrevious incidence edge
      face = incidenceIncidentFace incidence edge
  unless (vertexIdIndex vertex < incidenceVertexCount incidence)
    (Left (IncidenceVertexOriginInvalid edge vertex))
  unless (directedEdgeIdIndex nextEdge < incidenceDirectedEdgeCount incidence
          && directedEdgeIdIndex previousEdge < incidenceDirectedEdgeCount incidence)
    (Left (IncidenceEdgeLinksInvalid edge nextEdge previousEdge))
  unless (faceIdIndex face < incidenceFaceCount incidence)
    (Left (IncidenceEdgeFaceInvalid edge face))

-- | Shared local link law after edge/next/previous/twin ranges are discharged.
-- Complete admission and certified native closures use this same predicate.
validateIncidenceEdgeLinks :: PlanarIncidence -> DirectedEdgeId -> Either PlanarIncidenceError ()
validateIncidenceEdgeLinks incidence edge = do
  let nextEdge = incidenceNext incidence edge
  unless (incidencePrevious incidence nextEdge == edge
          && incidenceNext incidence (incidencePrevious incidence edge) == edge
          && incidenceIncidentFace incidence nextEdge == incidenceIncidentFace incidence edge)
    (Left (IncidenceEdgeLinkMismatch edge))
  unless (incidenceDestination incidence edge == incidenceOrigin incidence nextEdge)
    (Left (IncidenceEdgeEndpointMismatch edge))

validateVertexRoot :: PlanarIncidence -> VertexId -> Either PlanarIncidenceError ()
validateVertexRoot incidence vertex =
  case incidenceVertexOutEdge incidence vertex of
    Nothing -> Right ()
    root@(Just edge) ->
      unless (directedEdgeIdIndex edge < incidenceDirectedEdgeCount incidence
              && incidenceOrigin incidence edge == vertex)
        (Left (IncidenceVertexRootInvalid vertex root))

validateSparseFace :: PlanarIncidence -> Int -> Either PlanarIncidenceError ()
validateSparseFace incidence index =
  unless (index >= 0 && index < incidenceFaceCount incidence)
    (Left (IncidenceSparseFaceInvalid index))

validateFaceRoots :: PlanarIncidence -> FaceId -> Either PlanarIncidenceError ()
validateFaceRoots incidence face = do
  unless (face == FaceId 0 || incidenceAdjacentEdge incidence face /= Nothing)
    (Left (IncidenceBoundedFaceWithoutBoundary face))
  traverse_
    (\root -> unless (directedEdgeIdIndex root < incidenceDirectedEdgeCount incidence
                      && incidenceIncidentFace incidence root == face)
        (Left (IncidenceFaceRootInvalid face root)))
    (faceCycleRoots incidence face)

validateCoverage
  :: (IntSet.IntSet -> IntSet.IntSet -> PlanarIncidenceError)
  -> IntSet.IntSet
  -> [Int]
  -> Either PlanarIncidenceError ()
validateCoverage obstruction expected submitted =
  let (observed, repeated) = List.foldl' recordOccurrence (IntSet.empty, IntSet.empty) submitted
   in unless (observed == expected && IntSet.null repeated)
        (Left (obstruction (IntSet.difference expected observed)
          (IntSet.union repeated (IntSet.difference observed expected))))
 where
  -- Coverage needs membership and duplication, not an allocated integer count
  -- at every edge. IntSet keeps dense handle universes in packed bitmap leaves.
  recordOccurrence (!observed, !repeated) index =
    if IntSet.member index observed
      then (observed, IntSet.insert index repeated)
      else (IntSet.insert index observed, repeated)

incidenceVertexCount :: PlanarIncidence -> Int
incidenceVertexCount (PlanarIncidence vertices _ _ _ _) = pagedLength vertices
{-# INLINE incidenceVertexCount #-}

incidenceDirectedEdgeCount :: PlanarIncidence -> Int
incidenceDirectedEdgeCount (PlanarIncidence _ topology _ _ _) = pagedLength topology `quot` 4
{-# INLINE incidenceDirectedEdgeCount #-}

incidenceEdgeCount :: PlanarIncidence -> Int
incidenceEdgeCount incidence = incidenceDirectedEdgeCount incidence `quot` 2
{-# INLINE incidenceEdgeCount #-}

incidenceFaceCount :: PlanarIncidence -> Int
incidenceFaceCount (PlanarIncidence _ _ faces _ _) = pagedLength faces
{-# INLINE incidenceFaceCount #-}

-- | Sparse support of bounded faces with holes or punctures. Native meshes
-- answer immediately from their empty maps, without a face scan.
incidenceNonCellularFaces :: PlanarIncidence -> [FaceId]
incidenceNonCellularFaces (PlanarIncidence _ _ _ components isolated) =
  fmap (FaceId . fromIntegral) . IntSet.toAscList . IntSet.delete 0 $
    IntSet.union
      (IntMap.keysSet (IntMap.filter (not . null) components))
      (IntMap.keysSet (IntMap.filter (not . IntSet.null) isolated))

incidenceVertexOutEdge :: PlanarIncidence -> VertexId -> Maybe DirectedEdgeId
incidenceVertexOutEdge (PlanarIncidence vertices _ _ _ _) = packedOptionalEdge vertices . vertexIdIndex
{-# INLINE incidenceVertexOutEdge #-}

incidenceAdjacentEdge :: PlanarIncidence -> FaceId -> Maybe DirectedEdgeId
incidenceAdjacentEdge (PlanarIncidence _ _ faces _ _) = packedOptionalEdge faces . faceIdIndex
{-# INLINE incidenceAdjacentEdge #-}

incidenceOrigin :: PlanarIncidence -> DirectedEdgeId -> VertexId
incidenceOrigin (PlanarIncidence _ topology _ _ _) = packedOrigin topology
{-# INLINE incidenceOrigin #-}

incidenceDestination :: PlanarIncidence -> DirectedEdgeId -> VertexId
incidenceDestination incidence = incidenceOrigin incidence . reverseEdge
{-# INLINE incidenceDestination #-}

incidenceNext :: PlanarIncidence -> DirectedEdgeId -> DirectedEdgeId
incidenceNext (PlanarIncidence _ topology _ _ _) = packedNext topology
{-# INLINE incidenceNext #-}

incidencePrevious :: PlanarIncidence -> DirectedEdgeId -> DirectedEdgeId
incidencePrevious (PlanarIncidence _ topology _ _ _) = packedPrevious topology
{-# INLINE incidencePrevious #-}

incidenceIncidentFace :: PlanarIncidence -> DirectedEdgeId -> FaceId
incidenceIncidentFace (PlanarIncidence _ topology _ _ _) = packedIncidentFace topology
{-# INLINE incidenceIncidentFace #-}

incidenceUndirectedEndpoints :: PlanarIncidence -> UndirectedEdgeId -> (VertexId, VertexId)
incidenceUndirectedEndpoints incidence edge =
  let forward = normalizedDirected edge
   in (incidenceOrigin incidence forward, incidenceDestination incidence forward)
{-# INLINE incidenceUndirectedEndpoints #-}

incidenceVertexOutgoingEdges :: PlanarIncidence -> VertexId -> [DirectedEdgeId]
incidenceVertexOutgoingEdges incidence vertex =
  case incidenceVertexOutEdge incidence vertex of
    Nothing -> []
    Just root -> circularEdgeWalk (incidenceDirectedEdgeCount incidence) root
      (reverseEdge . incidencePrevious incidence)
{-# INLINE incidenceVertexOutgoingEdges #-}

faceCycleRoots :: PlanarIncidence -> FaceId -> [DirectedEdgeId]
faceCycleRoots incidence@(PlanarIncidence _ _ _ components _) face =
  maybeToList (incidenceAdjacentEdge incidence face)
    <> IntMap.findWithDefault [] (faceIdIndex face) components

faceBoundaryComponents :: PlanarIncidence -> FaceId -> [[DirectedEdgeId]]
faceBoundaryComponents incidence =
  fmap (\root -> circularEdgeWalk (incidenceDirectedEdgeCount incidence) root (incidenceNext incidence))
    . faceCycleRoots incidence

faceIsolatedVertices :: PlanarIncidence -> FaceId -> [VertexId]
faceIsolatedVertices (PlanarIncidence _ _ _ _ isolated) face =
  fmap (VertexId . fromIntegral)
    (IntSet.toAscList (IntMap.findWithDefault IntSet.empty (faceIdIndex face) isolated))

faceInnerBoundaryComponents :: PlanarIncidence -> FaceId -> [FaceBoundaryComponent]
faceInnerBoundaryComponents incidence@(PlanarIncidence _ _ _ components _) face =
  fmap BoundaryCycleRoot
    ( if face == FaceId 0
        then faceCycleRoots incidence face
        else IntMap.findWithDefault [] (faceIdIndex face) components
    )
    <> fmap IsolatedBoundaryVertex (faceIsolatedVertices incidence face)

-- | Compactly supported Euler contribution of the relatively open face.
faceEulerContribution :: PlanarIncidence -> FaceId -> Int
faceEulerContribution incidence face = 1 - length (faceInnerBoundaryComponents incidence face)

-- The fixed packed readers below were extracted from Dcel. Native hot-path
-- callers pass their resident planes directly: no incidence wrapper allocates.
packedOptionalEdge :: Paged Word32 -> Int -> Maybe DirectedEdgeId
packedOptionalEdge plane index = DirectedEdgeId . fromIntegral <$> unpackOptionalIndex (pagedUnsafeIndex plane index)
{-# INLINE packedOptionalEdge #-}

packedOrigin :: Paged Word32 -> DirectedEdgeId -> VertexId
packedOrigin topology edge = VertexId (pagedUnsafeIndex topology (4 * directedEdgeIdIndex edge))
{-# INLINE packedOrigin #-}

packedNext :: Paged Word32 -> DirectedEdgeId -> DirectedEdgeId
packedNext topology edge = DirectedEdgeId (pagedUnsafeIndex topology (4 * directedEdgeIdIndex edge + 1))
{-# INLINE packedNext #-}

packedPrevious :: Paged Word32 -> DirectedEdgeId -> DirectedEdgeId
packedPrevious topology edge = DirectedEdgeId (pagedUnsafeIndex topology (4 * directedEdgeIdIndex edge + 2))
{-# INLINE packedPrevious #-}

packedIncidentFace :: Paged Word32 -> DirectedEdgeId -> FaceId
packedIncidentFace topology edge = FaceId (pagedUnsafeIndex topology (4 * directedEdgeIdIndex edge + 3))
{-# INLINE packedIncidentFace #-}

-- | Shared fusible cycle producer, extracted unchanged from the native DCEL.
circularEdgeWalk :: Int -> DirectedEdgeId -> (DirectedEdgeId -> DirectedEdgeId) -> [DirectedEdgeId]
circularEdgeWalk directedCount start advance =
  build
    ( \(link :: DirectedEdgeId -> result -> result) (stop :: result) ->
        let go :: Int -> DirectedEdgeId -> Bool -> result
            go !remaining !current !visited
              | remaining <= 0 = stop
              | visited && current == start = stop
              | otherwise = link current (go (remaining - 1) (advance current) True)
         in go (directedCount + 1) start False
    )
{-# INLINE circularEdgeWalk #-}

circularEdgeFold
  :: forall accumulator. Int -> DirectedEdgeId -> (DirectedEdgeId -> DirectedEdgeId)
  -> (accumulator -> DirectedEdgeId -> accumulator) -> accumulator -> accumulator
circularEdgeFold directedCount start advance step =
  go (directedCount + 1) start False
 where
  go :: Int -> DirectedEdgeId -> Bool -> accumulator -> accumulator
  go !remaining !current !visited !accumulator
    | remaining <= 0 = accumulator
    | visited && current == start = accumulator
    | otherwise =
        let !nextAccumulator = step accumulator current
         in go (remaining - 1) (advance current) True nextAccumulator
{-# INLINE circularEdgeFold #-}

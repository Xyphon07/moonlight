-- | Trusted exact-coordinate publication from admitted planar incidence.
-- The callback-bearing entrances are internal because only a sealed
-- downstream carrier may prove that its exact coordinate section belongs to
-- the supplied topology.
module Moonlight.Planar.Internal.Region.Publication
  ( labelledPlanarLayer
  , labelledPlanarLayerFromExactCoordinates
  , planarRegionFromSelectedIncidence
  , planarLayerFromAdmittedComponents
  , polygonComponentFromBoundaryCoordinates
  ) where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import qualified Data.Graph as Graph
import Data.List (partition, sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Planar.Dcel (vertexPoint)
import Moonlight.Planar.Exact
  ( ExactPoint
  , exactOnClosedSegment
  , exactOrient2d
  , exactPointFromPoint
  )
import Moonlight.Planar.FloodFillIterator
  ( BoundaryLoop
  , BoundaryObstruction (..)
  , RegionBoundary
  , boundaryLoopVertices
  , componentBoundary
  , faceComponents
  , labelledRegionBoundaries
  , regionBoundaryHoleLoops
  , regionBoundaryOuterLoop
  )
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId
  , FaceId (..)
  , VertexId
  , reverseEdge
  )
import Moonlight.Planar.Internal.BoundaryCycle
  ( rotateCycleLeast
  , simplifyBoundaryCycle
  , traceOrientedBoundaryCircuits
  )
import Moonlight.Planar.Internal.Incidence
  ( PlanarIncidence
  , faceBoundaryComponents
  , incidenceDestination
  , incidenceFaceCount
  , incidenceIncidentFace
  , incidenceOrigin
  )
import Moonlight.Planar.Internal.Region.Types
  ( ExactLoop (..)
  , PlanarLayer (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  , RegionPublicationError (..)
  , RegionValidationError (..)
  )
import Moonlight.Planar.Internal.Representation (Triangulation)

-- | Publish bounded resident face labels through the existing component and
-- boundary owners, then lift only traced boundary coordinates exactly.
labelledPlanarLayer
  :: Ord label
  => label
  -> Triangulation mode vertex directed undirected face
  -> (FaceId -> label)
  -> Either RegionPublicationError (PlanarLayer label)
labelledPlanarLayer outside triangulation labelFace = do
  labelledBoundaries <-
    first RegionBoundaryObstruction
      (labelledRegionBoundaries triangulation labelFace)
  labelledComponents <-
    traverse
      (\(label, boundary) ->
         (label,) <$> polygonComponentFromBoundaryCoordinates exactPointAt boundary)
      [ pair
      | pair@(label, _) <- labelledBoundaries
      , label /= outside
      ]
  pure (planarLayerFromAdmittedComponents outside labelledComponents)
 where
  exactPointAt vertex =
    first (RegionCoordinateObstruction vertex)
      (exactPointFromPoint (vertexPoint triangulation vertex))

-- | Publish bounded face labels using the exact-coordinate section belonging
-- to the resident carrier. Topology has already been proved by
-- 'labelledRegionBoundaries'; this path performs exact simplification but does
-- not send derived loops back through authoring event sweeps.
labelledPlanarLayerFromExactCoordinates
  :: Ord label
  => label
  -> Triangulation mode vertex directed undirected face
  -> (VertexId -> Either RegionPublicationError ExactPoint)
  -> (FaceId -> Either RegionPublicationError label)
  -> Either RegionPublicationError (PlanarLayer label)
labelledPlanarLayerFromExactCoordinates outside triangulation exactPointAt labelFace = do
  labelledComponents <-
    traverse
      (\(labelResult, component) -> do
         label <- labelResult
         boundary <-
           first RegionBoundaryObstruction
             (componentBoundary triangulation component)
         (label,) <$> polygonComponentFromBoundaryCoordinates exactPointAt boundary)
      [ pair
      | pair@(labelResult, _) <- faceComponents triangulation labelFace
      , labelResult /= Right outside
      ]
  pure (planarLayerFromAdmittedComponents outside labelledComponents)

-- | Publish exactly the selected bounded faces, cancelling each internal
-- twin pair before tracing. Face boundary components are not polygon loops:
-- a face can have holes, bridges, or a pinched boundary. Edge-connected face
-- components are therefore glued first, then their frontier is projected.
--
-- Separate components may touch at vertices. Multiple outgoing frontier
-- darts in one connected component instead witness a pinch and refuse only
-- this strict polygon observation; the underlying exact selection remains
-- valid. Isolated vertices and bridges internal to a selected face make no
-- contribution to its closed polygon boundary.
planarRegionFromSelectedIncidence
  :: PlanarIncidence
  -> (VertexId -> Either RegionPublicationError ExactPoint)
  -> (FaceId -> Bool)
  -> Either RegionPublicationError PlanarRegion
planarRegionFromSelectedIncidence incidence exactPointAt selected
  | selected (FaceId 0) = Left RegionUnboundedSelection
  | otherwise =
      PlanarRegion . sort
        <$> traverse
          (publishComponent . concat . Graph.flattenSCC)
          ( Graph.stronglyConnComp
              [ (edges, face, map oppositeFace edges)
              | (face, edges) <- selectedFaceEdges
              ]
          )
 where
  selectedFaceEdges =
    [ (face, concat (faceBoundaryComponents incidence face))
    | index <- [1 .. incidenceFaceCount incidence - 1]
    , let face = FaceId (fromIntegral index)
    , selected face
    ]
  selectedFaces = Set.fromDistinctAscList (map fst selectedFaceEdges)
  oppositeFace = incidenceIncidentFace incidence . reverseEdge

  publishComponent
    :: [DirectedEdgeId]
    -> Either RegionPublicationError PolygonComponent
  publishComponent componentEdges = do
    let frontier =
          Set.fromList
            (filter ((`Set.notMember` selectedFaces) . oppositeFace) componentEdges)
        outgoing =
          Map.fromListWith (<>)
            [ (incidenceOrigin incidence edge, [edge])
            | edge <- Set.toAscList frontier
            ]
    traverse_ rejectPinch (Map.toAscList outgoing)
    circuits <-
      first RegionBoundaryObstruction
        ( traceOrientedBoundaryCircuits
            (incidenceOrigin incidence)
            (incidenceDestination incidence)
            BoundaryCycleDidNotClose
            outgoing
            frontier
        )
    classified <-
      traverse
        (\vertices -> traverse exactPointAt vertices >>= admittedDerivedLoop)
        circuits
    let (outerLoops, holeLoops) = partition ((== GT) . fst) classified
    case map snd outerLoops of
      [outer] -> Right (PolygonComponent outer (sort (map snd holeLoops)))
      outer ->
        Left
          (RegionBoundaryObstruction (BoundaryOuterLoopCardinality (length outer)))

  rejectPinch
    :: (VertexId, [DirectedEdgeId])
    -> Either RegionPublicationError ()
  rejectPinch (vertex, firstEdge : secondEdge : _) =
    Left (RegionBoundaryObstruction (BoundaryPinch vertex firstEdge secondEdge))
  rejectPinch _ = Right ()

-- | Glue already-admitted, pairwise interior-disjoint components by label.
-- Both DCEL publication and exact overlay cells reach this point only after
-- their topology owner has proved those obligations.
planarLayerFromAdmittedComponents
  :: Ord label
  => label
  -> [(label, PolygonComponent)]
  -> PlanarLayer label
planarLayerFromAdmittedComponents outside labelledComponents =
  PlanarLayer
    outside
    ( Map.map
        (PlanarRegion . sort)
        ( Map.fromListWith (<>)
            [(label, [component]) | (label, component) <- labelledComponents]
        )
    )

-- | Convert one already-traced resident component boundary against the exact
-- coordinate carrier admitted for that same resident topology.
polygonComponentFromBoundaryCoordinates
  :: (VertexId -> Either RegionPublicationError ExactPoint)
  -> RegionBoundary
  -> Either RegionPublicationError PolygonComponent
polygonComponentFromBoundaryCoordinates exactPointAt boundary = do
  outer <- convertLoop (regionBoundaryOuterLoop boundary)
  holes <- traverse convertLoop (regionBoundaryHoleLoops boundary)
  pure (PolygonComponent outer (sort holes))
 where
  convertLoop :: BoundaryLoop -> Either RegionPublicationError ExactLoop
  convertLoop loop = do
    points <- traverse exactPointAt (boundaryLoopVertices loop)
    snd <$> admittedDerivedLoop points

-- | Boundary descent already proves simplicity, winding, and component
-- compatibility. Exact simplification remains necessary because the exact
-- carrier may expose a collinearity that the embedded boundary retained.
admittedDerivedLoop
  :: NonEmpty ExactPoint
  -> Either RegionPublicationError (Ordering, ExactLoop)
admittedDerivedLoop points = do
  (winding, simplified) <-
    first RegionValidationObstruction
      ( simplifyBoundaryCycle
          RegionLoopDegenerate
          (\previous current next ->
             exactOrient2d previous current next == EQ
               && exactOnClosedSegment previous next current)
          exactOrient2d
          id
          (NonEmpty.toList points)
      )
  pure (winding, ExactLoop (rotateCycleLeast simplified))
{-# INLINE admittedDerivedLoop #-}

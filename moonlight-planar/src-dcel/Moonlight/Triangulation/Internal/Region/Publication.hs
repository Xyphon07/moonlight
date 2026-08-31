-- | Trusted exact-coordinate publication from the one resident DCEL boundary
-- owner. The callback-bearing entrances are internal because only a sealed
-- downstream carrier may prove that its exact coordinate section belongs to
-- the supplied topology.
module Moonlight.Triangulation.Internal.Region.Publication
  ( labelledPlanarLayer
  , labelledPlanarLayerFromExactCoordinates
  , planarLayerFromAdmittedComponents
  , polygonComponentFromBoundaryCoordinates
  ) where

import Data.Bifunctor (first)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Moonlight.Triangulation.Dcel (vertexPoint)
import Moonlight.Triangulation.Exact
  ( ExactPoint
  , exactOnClosedSegment
  , exactOrient2d
  , exactPointFromPoint
  )
import Moonlight.Triangulation.FloodFillIterator
  ( BoundaryLoop
  , RegionBoundary
  , boundaryLoopVertices
  , componentBoundary
  , faceComponents
  , labelledRegionBoundaries
  , regionBoundaryHoleLoops
  , regionBoundaryOuterLoop
  )
import Moonlight.Triangulation.Internal.HandleDefs (FaceId, VertexId)
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( rotateCycleLeast
  , simplifyBoundaryCycle
  )
import Moonlight.Triangulation.Internal.Region.Types
  ( ExactLoop (..)
  , PlanarLayer (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  , RegionPublicationError (..)
  , RegionValidationError (..)
  )
import Moonlight.Triangulation.Internal.Representation (Triangulation)

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
    admittedDerivedLoop points

-- | Boundary descent already proves simplicity, winding, and component
-- compatibility. Exact simplification remains necessary because the exact
-- carrier may expose a collinearity that the embedded boundary retained.
admittedDerivedLoop
  :: NonEmpty ExactPoint
  -> Either RegionPublicationError ExactLoop
admittedDerivedLoop points = do
  (_, simplified) <-
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
  pure (ExactLoop (rotateCycleLeast simplified))

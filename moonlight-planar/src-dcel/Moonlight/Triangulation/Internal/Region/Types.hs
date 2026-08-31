{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The invariant-bearing exact region carriers. Public construction and
-- validation remain in "Moonlight.Triangulation.Region"; downstream build-tier
-- algorithms import this owner only when their algebra proves the constructors'
-- obligations directly.
module Moonlight.Triangulation.Internal.Region.Types
  ( ExactLoop (..)
  , PolygonComponent (..)
  , PlanarRegion (..)
  , RegionPointLocation (..)
  , PlanarLayer (..)
  , RegionValidationError (..)
  , RegionPublicationError (..)
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Moonlight.Triangulation.Exact
  ( ExactPoint
  , SegmentRelation
  )
import Moonlight.Triangulation.FloodFillIterator (BoundaryObstruction)
import Moonlight.Triangulation.Internal.HandleDefs (FaceId, VertexId)
import Moonlight.Triangulation.Internal.ExactSegmentEvents
  ( ExactSegmentEventObstruction
  )
import Moonlight.Triangulation.Internal.Types (PointValidationError)

-- | One admitted, simple exact cycle. Its first point is the least exact point
-- on the cycle, so equality does not retain an authoring rotation.
newtype ExactLoop = ExactLoop (NonEmpty ExactPoint)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | One connected polygonal component: a counter-clockwise outer cycle and
-- zero or more clockwise holes.
data PolygonComponent = PolygonComponent
  { polygonOuterLoop :: !ExactLoop
  , polygonHoleLoops :: ![ExactLoop]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A finite union of components with pairwise-disjoint interiors.
newtype PlanarRegion = PlanarRegion [PolygonComponent]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Exact point position relative to a closed region.
data RegionPointLocation
  = RegionExterior
  | RegionOnBoundary
  | RegionInterior
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A finite labelled planar layer. The outside label is implicit and may not
-- also own a bounded region.
data PlanarLayer label = PlanarLayer
  { planarLayerOutsideLabel :: !label
  , planarLayerRegions :: !(Map label PlanarRegion)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Typed authoring obstructions. Indices are cycle/component positions in the
-- submitted value and every geometric relation preserves its exact witness.
data RegionValidationError
  = RegionLoopDegenerate ![ExactPoint]
  | RegionLoopSelfRelation !Int !Int !SegmentRelation
  | RegionOuterLoopWinding !Ordering
  | RegionHoleLoopWinding !Int !Ordering
  | RegionHoleLocation !Int !RegionPointLocation
  | RegionBoundaryRelation !Int !Int !SegmentRelation
  | RegionComponentInteriorOverlap !Int !Int
  | RegionLayerInteriorOverlap !Int !Int
  | RegionOutsideLabelUsed
  | RegionSegmentEventsInvalid !ExactSegmentEventObstruction
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Typed failures while publishing an existing triangulation as exact region
-- values. The handle is retained when a resident coordinate is inadmissible.
data RegionPublicationError
  = RegionBoundaryObstruction !BoundaryObstruction
  | RegionCoordinateObstruction !VertexId !PointValidationError
  | RegionCoordinateMissing !VertexId
  | RegionFaceLabelMissing !FaceId
  | RegionValidationObstruction !RegionValidationError
  | RegionUnboundedSelection
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

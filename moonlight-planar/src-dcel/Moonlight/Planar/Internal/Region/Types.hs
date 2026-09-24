{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The invariant-bearing exact region carriers. Public construction and
-- validation remain in "Moonlight.Planar.Region"; downstream build-tier
-- algorithms import this owner only when their algebra proves the constructors'
-- obligations directly.
module Moonlight.Planar.Internal.Region.Types
  ( ExactLoop (..)
  , ConvexPolygon (..)
  , PolygonComponent (..)
  , polygonOuterLoop
  , polygonHoleLoops
  , PlanarRegion (..)
  , RegionPointLocation (..)
  , PlanarLayer (..)
  , planarLayerOutsideLabel
  , planarLayerRegions
  , RegionValidationError (..)
  , RegionPublicationError (..)
  ) where

import Control.DeepSeq (NFData (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Moonlight.Planar.Exact
  ( ExactPoint
  , SegmentRelation
  )
import Moonlight.Planar.FloodFillIterator (BoundaryObstruction)
import Moonlight.Planar.Internal.HandleDefs (FaceId, VertexId)
import Moonlight.Planar.Internal.ExactSegmentEvents
  ( ExactSegmentEventObstruction
  )
import Moonlight.Planar.Point (PointValidationError)

-- | One admitted, simple exact cycle. Its first point is the least exact point
-- on the cycle, so equality does not retain an authoring rotation.
newtype ExactLoop = ExactLoop (NonEmpty ExactPoint)
  deriving stock (Eq, Ord, Show)

instance NFData ExactLoop where
  rnf (ExactLoop points) = rnf points
  {-# INLINE rnf #-}

-- | One canonical, strictly convex counter-clockwise exact loop. Checked
-- construction belongs to "Moonlight.Planar.Convex"; geometric algorithms
-- may publish directly only when their algebra discharges these obligations.
newtype ConvexPolygon = ConvexPolygon ExactLoop
  deriving stock (Eq, Ord, Show)

instance NFData ConvexPolygon where
  rnf (ConvexPolygon loop) = rnf loop
  {-# INLINE rnf #-}

-- | One connected polygonal component: a counter-clockwise outer cycle and
-- zero or more clockwise holes.
data PolygonComponent = PolygonComponent !ExactLoop ![ExactLoop]
  deriving stock (Eq, Ord, Show)

instance NFData PolygonComponent where
  rnf (PolygonComponent outer holes) = rnf outer `seq` rnf holes
  {-# INLINE rnf #-}

-- | Read-only outer boundary; changing it requires component admission.
polygonOuterLoop :: PolygonComponent -> ExactLoop
polygonOuterLoop (PolygonComponent outer _) = outer
{-# INLINE polygonOuterLoop #-}

-- | Read-only hole boundaries; changing them requires component admission.
polygonHoleLoops :: PolygonComponent -> [ExactLoop]
polygonHoleLoops (PolygonComponent _ holes) = holes
{-# INLINE polygonHoleLoops #-}

-- | A finite union of components with pairwise-disjoint interiors.
newtype PlanarRegion = PlanarRegion [PolygonComponent]
  deriving stock (Eq, Ord, Show)

instance NFData PlanarRegion where
  rnf (PlanarRegion components) = rnf components
  {-# INLINE rnf #-}

-- | Exact point position relative to a closed region.
data RegionPointLocation
  = RegionExterior
  | RegionOnBoundary
  | RegionInterior
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A finite labelled planar layer. The outside label is implicit and may not
-- also own a bounded region.
data PlanarLayer label = PlanarLayer !label !(Map label PlanarRegion)
  deriving stock (Eq, Ord, Show)

instance NFData label => NFData (PlanarLayer label) where
  rnf (PlanarLayer outside regions) = rnf outside `seq` rnf regions
  {-# INLINE rnf #-}

-- | Read-only outside label; relabelling requires layer admission.
planarLayerOutsideLabel :: PlanarLayer label -> label
planarLayerOutsideLabel (PlanarLayer outside _) = outside
{-# INLINE planarLayerOutsideLabel #-}

-- | Read-only bounded regions; replacement requires layer admission.
planarLayerRegions :: PlanarLayer label -> Map label PlanarRegion
planarLayerRegions (PlanarLayer _ regions) = regions
{-# INLINE planarLayerRegions #-}

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

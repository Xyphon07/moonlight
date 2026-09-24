{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}

-- | The one exact overlay carrier. Incidence belongs to the shared planar
-- owner; geometry, source provenance, and application labels occupy separate
-- immutable payload planes. No resident binary64 realization is retained.
module Moonlight.Planar.Internal.Overlay.Types
  ( BoundaryLoopRef (..)
  , BoundaryFeature (..)
  , OverlaySourceId (..)
  , OverlayLabelId (..)
  , BoundaryRef (..)
  , BoundaryVertexRef
  , BoundaryEdgeRef
  , OverlayVertexOrigin (..)
  , OverlayEdgeOrigin (..)
  , OverlayVertex (..)
  , OverlayReceipt (..)
  , OverlayArrangementObstruction (..)
  , OverlayCellWitness (..)
  , OverlayError (..)
  , OverlayResult (..)
  , OverlaySelectionKind (..)
  , OverlaySelectionError (..)
  ) where

import Control.DeepSeq (NFData (..))
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Moonlight.Planar.CellSet (CellSelectionError)
import Moonlight.Planar.Exact (ExactGeometryError, ExactPoint)
import Moonlight.Planar.Internal.ExactRational (ExactArithmeticError)
import Moonlight.Planar.Internal.ExactSegmentEvents (ExactSegmentEventObstruction)
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  )
import Moonlight.Planar.Internal.Incidence
  ( PlanarIncidence
  , PlanarIncidenceError
  )
import Moonlight.Planar.Internal.Region.Types (RegionPublicationError)

-- | Position in the submitted source family, independent of application labels.
newtype OverlaySourceId = OverlaySourceId {overlaySourceIndex :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A source-local label coordinate. Zero is the source's outside state;
-- public authoring retains the typed table that interprets all other values.
newtype OverlayLabelId = OverlayLabelId {overlayLabelIndex :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data BoundaryLoopRef
  = BoundaryOuterLoop
  | BoundaryHoleLoop !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data BoundaryFeature = BoundaryVertexFeature | BoundaryEdgeFeature

-- | Provenance uses source ordinals, not a binary operand or a universal sum
-- of the application's label types.
data BoundaryRef (feature :: BoundaryFeature) = BoundaryRef
  { boundaryRefSource :: !OverlaySourceId
  , boundaryRefComponent :: !Int
  , boundaryRefLoop :: !BoundaryLoopRef
  , boundaryRefLocalIndex :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

type BoundaryVertexRef = BoundaryRef 'BoundaryVertexFeature
type BoundaryEdgeRef = BoundaryRef 'BoundaryEdgeFeature

data OverlayVertexOrigin = OverlayVertexOrigin
  { overlayOriginVertices :: ![BoundaryVertexRef]
  , overlayOriginEdges :: ![BoundaryEdgeRef]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

newtype OverlayEdgeOrigin = OverlayEdgeOrigin
  { overlayEdgeSources :: NonEmpty BoundaryEdgeRef
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayVertex = OverlayVertex
  { overlayExactPoint :: !ExactPoint
  , overlayVertexOrigin :: !OverlayVertexOrigin
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayReceipt = OverlayReceipt
  { overlayInputSources :: !Int
  , overlayInputSegments :: !Int
  , overlayRelationEvents :: !Int
  , overlayExactCrossings :: !Int
  , overlayOverlapIntervals :: !Int
  , overlayAtomicEdges :: !Int
  , overlayOutputVertices :: !Int
  , overlayArrangementCells :: !Int
  , overlayBoundaryOrbits :: !Int
  , overlayProvenanceIncidences :: !Int
  , overlayTotalRelationChecks :: !Int
  , overlaySweepMaximumHeight :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayArrangementObstruction
  = OverlaySourceOutsideLabelInvalid !OverlaySourceId !OverlayLabelId
  | OverlaySourceSideConflict
      !OverlaySourceId !OverlayEdgeOrigin !(NonEmpty OverlayLabelId)
  | OverlayRotationDegenerate !ExactPoint
  | OverlayBoundaryOrbitMissing !DirectedEdgeId
  | OverlayBoundaryOrbitNotClosed !DirectedEdgeId !DirectedEdgeId
  | OverlayTransitionSourceMismatch
      !FaceId !UndirectedEdgeId !OverlaySourceId !OverlayLabelId !OverlayLabelId
  | OverlayFaceLabelConflict
      !FaceId !UndirectedEdgeId !(IntMap OverlayLabelId) !(IntMap OverlayLabelId)
  | OverlayPackedCapacityExceeded !Int !Int !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data OverlayCellWitness
  = OverlayVertexMissing !VertexId
  | OverlayEdgeMissing !UndirectedEdgeId
  | OverlayFaceMissing !FaceId
  | OverlayFaceInteriorUnavailable !FaceId
  | OverlayExactVertexMissing !ExactPoint
  | OverlaySourceLabelMissing !OverlaySourceId !OverlayLabelId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data OverlayError
  = OverlayExactArithmetic !ExactArithmeticError
  | OverlayExactGeometryInvalid !ExactGeometryError
  | OverlaySegmentEventsInvalid !ExactSegmentEventObstruction
  | OverlayArrangementInvalid !OverlayArrangementObstruction
  | OverlayIncidenceInvalid !PlanarIncidenceError
  | OverlayRegionPublicationFailed !RegionPublicationError
  | OverlayProvenanceIncomplete !OverlayCellWitness
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Face zero is the single unbounded face and carries the outside label.
-- Polygon cells and binary64 triangulations are not construction prerequisites
-- or competing stored representations.
-- Functor/Foldable/Traversable act only on this label plane: relabelling and
-- fallible decoding cannot change geometry, incidence or source provenance.
data OverlayResult labels = OverlayResult
  { overlayResultIncidence :: !PlanarIncidence
  , overlayResultVertices :: !(Vector OverlayVertex)
  , overlayResultEdges :: !(Vector OverlayEdgeOrigin)
  , overlayResultLabels :: !(Vector labels)
  , overlayResultReceipt :: !OverlayReceipt
  }
  deriving stock (Functor, Foldable, Traversable)

instance NFData labels => NFData (OverlayResult labels) where
  rnf result =
    rnf (overlayResultIncidence result)
      `seq` rnf (overlayResultVertices result)
      `seq` rnf (overlayResultEdges result)
      `seq` rnf (overlayResultLabels result)
      `seq` rnf (overlayResultReceipt result)

data OverlaySelectionKind
  = ClosedUnionSelection
  | ClosedIntersectionSelection
  | RegularizedDifferenceSelection
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlaySelectionError
  = OverlaySelectionContainsUnboundedCell !OverlaySelectionKind
  | OverlaySelectionProvenance !OverlayCellWitness
  | OverlaySelectionInvalid !CellSelectionError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

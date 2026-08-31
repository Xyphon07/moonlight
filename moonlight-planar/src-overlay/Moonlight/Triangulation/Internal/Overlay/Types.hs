{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}

-- | Closed vocabulary and opaque carrier representation for exact labelled
-- overlay. Construction lives in the public build-tier module; this module
-- exists so every invariant-bearing payload shares one owner.
module Moonlight.Triangulation.Internal.Overlay.Types
  ( BoundaryLoopRef (..)
  , OverlayOperand (..)
  , BoundaryRef (..)
  , BoundaryVertexRef
  , BoundaryEdgeRef
  , OverlayVertexOrigin (..)
  , OverlayEdgeOrigin (..)
  , OverlaySupport (..)
  , overlaySupportLabels
  , OverlayCellSupport (..)
  , OverlayCellId (..)
  , OverlayCellGeometry (..)
  , OverlayCell (..)
  , OverlayFace (..)
  , OverlayVertex (..)
  , OverlayEdge (..)
  , OverlayReceipt (..)
  , OverlayArrangementObstruction (..)
  , OverlayCellWitness (..)
  , OverlayError (..)
  , OverlayResult (..)
  , OverlaySelectionKind (..)
  , OverlaySelectionError (..)
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Moonlight.Triangulation.CellSet (CellSelectionError)
import Moonlight.Triangulation.Exact
  ( ExactPoint
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId
  , UndirectedEdgeId
  , VertexId
  )
import Moonlight.Triangulation.Internal.Cdt.Types (CdtError)
import Moonlight.Triangulation.Internal.ExactRational (ExactArithmeticError)
import Moonlight.Triangulation.Internal.ExactSegmentEvents (ExactSegmentEventObstruction)
import Moonlight.Triangulation.Embedding (OverlayEmbeddingObstruction)
import Moonlight.Triangulation.Internal.Region.Types
  ( ExactLoop
  , PolygonComponent
  , RegionPublicationError
  )
import Moonlight.Triangulation.Internal.Representation (Triangulation)
import Moonlight.Triangulation.Internal.Types
  ( ConstraintMode (Constrained)
  )

-- | Which cycle inside a polygon component supplied a source reference.
data BoundaryLoopRef
  = BoundaryOuterLoop
  | BoundaryHoleLoop !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayOperand
  = LeftOverlayOperand
  | RightOverlayOperand

-- | Phantom source cell kind; both provenance axes share one physical payload.
data BoundaryFeature = BoundaryVertexFeature | BoundaryEdgeFeature

data BoundaryRef (feature :: BoundaryFeature) (operand :: OverlayOperand) = BoundaryRef
  { boundaryRefComponent :: !Int
  , boundaryRefLoop :: !BoundaryLoopRef
  , boundaryRefLocalIndex :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

type BoundaryVertexRef operand = BoundaryRef 'BoundaryVertexFeature operand
type BoundaryEdgeRef operand = BoundaryRef 'BoundaryEdgeFeature operand

-- | Complete typed source closure of one arrangement vertex.
data OverlayVertexOrigin = OverlayVertexOrigin
  { overlayOriginLeftVertices :: ![BoundaryVertexRef 'LeftOverlayOperand]
  , overlayOriginRightVertices :: ![BoundaryVertexRef 'RightOverlayOperand]
  , overlayOriginLeftEdges :: ![BoundaryEdgeRef 'LeftOverlayOperand]
  , overlayOriginRightEdges :: ![BoundaryEdgeRef 'RightOverlayOperand]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Nonempty typed source-edge provenance of one exact atomic interval.
data OverlayEdgeOrigin = OverlayEdgeOrigin
  { overlayEdgeLeftSources :: ![BoundaryEdgeRef 'LeftOverlayOperand]
  , overlayEdgeRightSources :: ![BoundaryEdgeRef 'RightOverlayOperand]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Sorted nonempty labels whose closures contain one relatively open cell.
newtype OverlaySupport label = OverlaySupport (NonEmpty label)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

overlaySupportLabels :: OverlaySupport label -> NonEmpty label
overlaySupportLabels (OverlaySupport labels) = labels

data OverlayCellSupport leftLabel rightLabel = OverlayCellSupport
  { overlaySupportLeft :: !(OverlaySupport leftLabel)
  , overlaySupportRight :: !(OverlaySupport rightLabel)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

newtype OverlayCellId = OverlayCellId Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayCellGeometry
  = BoundedOverlayCell !PolygonComponent
  | UnboundedOverlayCell ![ExactLoop]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayCell leftLabel rightLabel = OverlayCell
  { overlayCellLeft :: !leftLabel
  , overlayCellRight :: !rightLabel
  , overlayCellGeometry :: !OverlayCellGeometry
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A resident triangle either realizes an exact two-cell or collapses under
-- the authoritative exact coordinates. A collapsed triangle descends to the
-- unique adjacent exact cell through representation diagonals, but exact
-- region operations must not mistake its binary64 area for exact area.
data OverlayFace
  = OverlayCellFace
      { overlayFaceCellId :: !OverlayCellId
      }
  | OverlayCollapsedFace
      { overlayFaceCellId :: !OverlayCellId
      }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Exact geometry and provenance are intrinsic to a vertex. Label support is
-- the finite union of incident cell descriptors and is therefore a view.
data OverlayVertex = OverlayVertex
  { overlayExactPoint :: !ExactPoint
  , overlayVertexOrigin :: !OverlayVertexOrigin
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Boundary provenance is intrinsic; label support is derived from the two
-- incident face-cell references.
data OverlayEdge
  = OverlayBoundary !OverlayEdgeOrigin
  | OverlayDiagonal
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayReceipt = OverlayReceipt
  { overlayInputSegments :: !Int
  , overlayRelationEvents :: !Int
  , overlayExactCrossings :: !Int
  , overlayOverlapIntervals :: !Int
  , overlayAtomicEdges :: !Int
  , overlayOutputVertices :: !Int
  , overlayArrangementCells :: !Int
  , overlayResidentFaces :: !Int
  , overlayEmbeddingCandidates :: !Int
  , overlayTotalRelationChecks :: !Int
  , overlaySweepMaximumHeight :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data OverlayArrangementObstruction leftLabel rightLabel
  = OverlayLeftSourceSideConflict
      !OverlayEdgeOrigin
      !(NonEmpty leftLabel)
  | OverlayRightSourceSideConflict
      !OverlayEdgeOrigin
      !(NonEmpty rightLabel)
  | OverlayRotationDegenerate !ExactPoint
  | OverlayFaceComponentEmpty
  | OverlayCellCycleDidNotClose !ExactPoint !ExactPoint
  | OverlayTransitionSourceMismatch
      !FaceId
      !UndirectedEdgeId
      !(leftLabel, rightLabel)
      !(leftLabel, rightLabel)
  | OverlayResidentFaceLabelConflict
      !FaceId
      !UndirectedEdgeId
      !(leftLabel, rightLabel)
      !(leftLabel, rightLabel)
  | OverlayResidentFaceArity !FaceId !Int
  | OverlayResidentFaceOrientationReversed !FaceId
  | OverlayCollapsedFacesUnowned !(NonEmpty FaceId)
  | OverlayCollapsedFacesAmbiguous
      !(NonEmpty FaceId)
      !(NonEmpty OverlayCellId)
  | OverlayDuplicateCellSignature !PolygonComponent
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data OverlayCellWitness
  = OverlayAtomicConstraintMissing !ExactPoint !ExactPoint
  | OverlayAtomicConstraintOrientationMismatch
      !UndirectedEdgeId
      !ExactPoint
      !ExactPoint
  | OverlayUnexpectedConstraint !UndirectedEdgeId
  | OverlayBoundaryEdgeNotConstrained !UndirectedEdgeId
  | OverlayResidentFaceUnassigned !FaceId
  | OverlayCellPayloadMissing !OverlayCellId
  | OverlayVertexSupportMissing !VertexId
  | OverlayEdgeSupportMissing !UndirectedEdgeId
  | OverlayEmbeddedVertexCountMismatch !Int !Int
  | OverlayExactVertexMissing !ExactPoint
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data OverlayError leftLabel rightLabel
  = OverlayExactArithmetic !ExactArithmeticError
  | OverlaySegmentEventsInvalid !ExactSegmentEventObstruction
  | OverlayArrangementInvalid !(OverlayArrangementObstruction leftLabel rightLabel)
  | OverlayEmbeddingRefused !(NonEmpty OverlayEmbeddingObstruction)
  | OverlayBuildFailed !CdtError
  | OverlayRegionPublicationFailed !RegionPublicationError
  | OverlayProvenanceIncomplete !OverlayCellWitness
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The singular exact subdivision carrier. Its embedded DCEL is a derived
-- binary64 realization; exact coordinates remain in the vertex payload plane.
data OverlayResult leftLabel rightLabel = OverlayResult
  { overlayResultTriangulation
      :: !( Triangulation
              'Constrained
              OverlayVertex
              ()
              OverlayEdge
              OverlayFace
          )
  , overlayResultCells :: !(Vector (OverlayCell leftLabel rightLabel))
  , overlayResultOutsideLabels :: !(leftLabel, rightLabel)
  , overlayResultReceipt :: !OverlayReceipt
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

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

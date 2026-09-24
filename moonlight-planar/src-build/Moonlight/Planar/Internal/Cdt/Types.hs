{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The shared vocabulary of the constrained layer: its refusals, its published
-- results, and the transaction-local state its interpreters carry.
module Moonlight.Planar.Internal.Cdt.Types
  ( CdtError (..)
  , CanonicalSegment (..)
  , ConstraintConflict (..)
  , ConstrainedUnionError (..)
  , ConstrainedSeamSide (..)
  , ConstrainedSeamFaceEvidence (..)
  , ConstrainedSeamResult (..)
  , CorridorObstruction (..)
  , ConstraintRecoveryResult (..)
  , ConstraintResult
  , ConstraintOutcome (..)
  , ConstraintBatchStats (..)
  , ConstraintBatchResult (..)
  , ConstrainedExtensionResult (..)
  , ConstraintSplitBatchResult
  , ConstraintBatchAccumulator (..)
  , ConstraintRequestAccumulator (..)
  , ConstraintWorkspace (..)
  , MutableConstraintProgram (..)
  , MutableConstraintOutcome (..)
  , MutableProgramCursor (..)
  , MutablePlanWalk (..)
  , MutableConstraintScan (..)
  , ConstraintProgramAccumulator (..)
  , CdtBuildResult (..)
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector as V
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  )
import Moonlight.Planar.Internal.Growable (GrowableWord32)
import Moonlight.Planar.Internal.Paged (PublicationStats)
import Moonlight.Planar.Internal.Representation (BuildResult, Triangulation)
import Moonlight.Planar.Internal.Types
import Moonlight.Planar.BuildStats (BuildStats)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.IntersectionIterator (Intersection)
import GHC.Generics (Generic)

-- | Typed refusal surface for constrained construction and corridor recovery.
data CdtError
  = CdtBuildError !BuildError
  | InvalidConstraintVertex !VertexId
  | ConstraintIntersection !UndirectedEdgeId
  -- ^ The first resident constraint the corridor from the source vertex
  -- meets; a request crossing several is refused on the nearest one.
  | ConstraintInputConflicts !(V.Vector (Int, Int))
  | ConstraintCorridorObstructed !CorridorObstruction
  | ConstraintBatchCardinalityMismatch
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | ConstraintEndpointIndexOutOfRange
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | ConstraintSplitBudgetExhausted
      !VertexId
      !VertexId
      {-# UNPACK #-} !Int
  | ConstraintSplitIntersectionIndeterminate
      !DirectedEdgeId
  | ConstraintEdgeIndexOutOfRange
      !UndirectedEdgeId
      {-# UNPACK #-} !Int
  | ConstraintRecoverySafetyBudgetExhausted
      !VertexId
      !VertexId
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | ConstraintRecoveryStripExhausted
      !VertexId
      !VertexId
      {-# UNPACK #-} !Int
  | ConstraintRecoveryStripUnflippable
      !VertexId
      !VertexId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Geometric identity of one constraint segment. The constructor is private:
-- endpoints are always canonical points in ascending order, so direction and
-- edge numbering cannot leak into union witnesses.
data CanonicalSegment = CanonicalSegment
  { -- | Lesser endpoint under the point ordering.
    segmentStart :: !(Point)
  , -- | Greater endpoint under the point ordering.
    segmentEnd :: !(Point)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A pair of properly crossing constraints, ordered independently of operand
-- and traversal order.
data ConstraintConflict = ConstraintConflict
  { -- | Lesser segment under the canonical segment ordering.
    conflictFirstSegment :: !(CanonicalSegment)
  , -- | Greater segment under the canonical segment ordering.
    conflictSecondSegment :: !(CanonicalSegment)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Complete obstruction surface for atomic constrained union.
data ConstrainedUnionError
  = ConstraintUnionConflicts !(NonEmpty (ConstraintConflict))
  | ConstraintUnionConstructionFailed !(CdtError)
  | ConstraintUnionSiteMissing !(Point)
  | ConstraintUnionNotSeparated
  -- | Local publication would otherwise enumerate a dense resident payload
  -- plane. Compact geometry-only payloads before requesting a seam.
  | ConstraintUnionRequiresGeometryOnlyPublication
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Source-local side presented to a separated seam's face-preservation
-- selector. The resident side keeps its handles; incoming face handles are
-- translated only inside the join interpreter.
data ConstrainedSeamSide
  = SeamResident
  | SeamIncoming
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Exact proof that one active source face survives as one target face. The
-- three points are stored in ascending order, so the witness is independent
-- of local face rotation and handle numbering.
data ConstrainedSeamFaceEvidence = ConstrainedSeamFaceEvidence
  { constrainedSeamSourceFace :: !FaceId
    -- ^ Face handle in the source operand.
  , constrainedSeamTargetFace :: !FaceId
    -- ^ Corresponding face handle in the published result.
  , constrainedSeamFaceFirstPoint :: !(Point)
    -- ^ Least vertex position under canonical point order.
  , constrainedSeamFaceSecondPoint :: !(Point)
    -- ^ Middle vertex position under canonical point order.
  , constrainedSeamFaceThirdPoint :: !(Point)
    -- ^ Greatest vertex position under canonical point order.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Atomic publication of a source-preserving constrained seam and the proof
-- needed to transport solved face-indexed data. The receipt is derived during
-- the ordered append/seam interpretation; canonical numbering remains an
-- explicit observation rather than hidden work in this local operation.
data ConstrainedSeamResult vertex = ConstrainedSeamResult
  { constrainedSeamResultTriangulation
      :: !(Triangulation 'Constrained vertex () () ())
    -- ^ Published constrained union.
  , constrainedSeamLeftFaceCount :: {-# UNPACK #-} !Int
    -- ^ Stable size of the resident source face block. In selective mode this
    -- is a handle-partition witness, not proof that every resident face was
    -- selected.
  , constrainedSeamRightFaceEvidence
      :: !(V.Vector (ConstrainedSeamFaceEvidence))
    -- ^ Exact transport evidence only for selected incoming source faces.
  , constrainedSeamJoinFaces :: !(V.Vector FaceId)
    -- ^ Exact final J component, including rewritten exterior source filler.
  , constrainedSeamLeftConstraintCount :: {-# UNPACK #-} !Int
    -- ^ Stable number of left-source constraints.
  , constrainedSeamConstraintStats :: !ConstraintBatchStats
    -- ^ Constraint recovery work performed by the seam.
  , constrainedSeamBuildStats :: !BuildStats
    -- ^ Topology work performed by the seam.
  , constrainedSeamPublicationStats :: !PublicationStats
    -- ^ Page publication work measured by the transaction owner.
  , constrainedSeamCachedFrontierPointReads :: {-# UNPACK #-} !Int
    -- ^ Frontier point reads performed while interpreting the cached seam.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance Eq vertex => Eq (ConstrainedSeamResult vertex)
deriving stock instance Show vertex => Show (ConstrainedSeamResult vertex)

-- | Structural witness that corridor recovery could not complete.
data CorridorObstruction
  = CorridorWalkDidNotTerminate {-# UNPACK #-} !Int
  | CorridorBoundaryMissing !VertexId !VertexId
  | CorridorProgramMalformed {-# UNPACK #-} !Int
  | CorridorTargetMissing !VertexId !VertexId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | One constrained publication and its path receipt. The path carrier names
-- whether the caller requested one recovered path or an ordered family; the
-- mesh and edge count have one representation in either case.
data ConstraintRecoveryResult pathReceipt vertex directed undirected face = ConstraintRecoveryResult
  { constraintRecoveryTriangulation :: !(Triangulation 'Constrained vertex directed undirected face)
    -- ^ Published constrained triangulation.
  , constraintRecoveryPathReceipt :: !pathReceipt
    -- ^ Recovered path evidence at the operation's requested multiplicity.
  , constraintRecoveryAddedEdges :: {-# UNPACK #-} !Int
    -- ^ Edges created while recovering the path evidence.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq pathReceipt, Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (ConstraintRecoveryResult pathReceipt vertex directed undirected face)
deriving stock instance
  (Show pathReceipt, Show vertex, Show directed, Show undirected, Show face)
  => Show (ConstraintRecoveryResult pathReceipt vertex directed undirected face)

-- | Atomic result of admitting one constraint segment.
type ConstraintResult vertex directed undirected face =
  ConstraintRecoveryResult (V.Vector DirectedEdgeId) vertex directed undirected face

-- | Per-request outcome in a constraint batch.
data ConstraintOutcome
  = ConstraintAccepted
      !(V.Vector DirectedEdgeId)
      {-# UNPACK #-} !Int
  | ConstraintRejected
      !UndirectedEdgeId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Aggregate work and disposition counts for a constraint batch.
data ConstraintBatchStats = ConstraintBatchStats
  { constraintBatchRequests :: {-# UNPACK #-} !Int
    -- ^ Requests interpreted.
  , constraintBatchAccepted :: {-# UNPACK #-} !Int
    -- ^ Requests admitted.
  , constraintBatchRejected :: {-# UNPACK #-} !Int
    -- ^ Requests refused by an existing constraint.
  , constraintBatchCorridors :: {-# UNPACK #-} !Int
    -- ^ Recovery corridors opened.
  , constraintBatchReusedFaces :: {-# UNPACK #-} !Int
    -- ^ Existing faces retained while recovering corridors.
  , constraintBatchCrossedEdges :: {-# UNPACK #-} !Int
    -- ^ Edges crossed while tracing corridors.
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData)

-- | Atomic publication of one constraint batch and its receipts.
data ConstraintBatchResult vertex directed undirected face = ConstraintBatchResult
  { constraintBatchTriangulation :: !(Triangulation 'Constrained vertex directed undirected face)
    -- ^ Published constrained triangulation.
  , constraintBatchOutcomes :: !(V.Vector ConstraintOutcome)
    -- ^ Outcomes in request order.
  , constraintBatchStats :: !ConstraintBatchStats
    -- ^ Aggregate work performed by the batch.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (ConstraintBatchResult vertex directed undirected face)
deriving stock instance
  (Show vertex, Show directed, Show undirected, Show face)
  => Show (ConstraintBatchResult vertex directed undirected face)

-- | Receipt of one asymmetric constrained extension. The triangulation and
-- all telemetry arise from the same sealed transaction: base sites were
-- resident, incoming sites were inserted, and only incoming constraints were
-- interpreted. A later refinement is deliberately a separate operation over
-- this immutable result, not a hidden continuation of this transaction.
data ConstrainedExtensionResult vertex directed undirected face = ConstrainedExtensionResult
  { constrainedExtensionConstraintBatch :: {-# UNPACK #-} !(ConstraintBatchResult vertex directed undirected face)
    -- ^ Published extension and incoming-constraint receipts.
  , constrainedExtensionBuildStats :: !BuildStats
    -- ^ Topology work performed by the extension.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

deriving stock instance
  (Eq vertex, Eq directed, Eq undirected, Eq face)
  => Eq (ConstrainedExtensionResult vertex directed undirected face)
deriving stock instance
  (Show vertex, Show directed, Show undirected, Show face)
  => Show (ConstrainedExtensionResult vertex directed undirected face)

-- | One published mesh carrying every requested division. The paths are
-- per-request receipts in traversal order; a later request may have rewritten
-- topology an earlier path names, which is the same as-traversed reading the
-- singleton path already carries.
type ConstraintSplitBatchResult vertex directed undirected face =
  ConstraintRecoveryResult (V.Vector (V.Vector DirectedEdgeId)) vertex directed undirected face

data ConstraintBatchAccumulator = ConstraintBatchAccumulator
  { accumulatedConstraintOutcomes :: ![ConstraintOutcome]
  , accumulatedConstraintStats :: !ConstraintBatchStats
  }

data ConstraintRequestAccumulator = ConstraintRequestAccumulator
  { accumulatedRequestPath :: ![DirectedEdgeId]
  , accumulatedRequestAddedEdges :: {-# UNPACK #-} !Int
  , accumulatedRequestCorridors :: {-# UNPACK #-} !Int
  , accumulatedRequestReusedFaces :: {-# UNPACK #-} !Int
  , accumulatedRequestCrossedEdges :: {-# UNPACK #-} !Int
  }

data ConstraintWorkspace s = ConstraintWorkspace
  { constraintProgramWords :: !(GrowableWord32 s)
  , constraintWalkBudget :: {-# UNPACK #-} !Int
  }

data MutableConstraintProgram = MutableConstraintProgram
  { mutableProgramWordCount :: {-# UNPACK #-} !Int
  , mutableProgramPieceCount :: {-# UNPACK #-} !Int
  }

-- | What one request did to the thawed mesh. A rejection is a value: the batch
-- interpreter records it and carries on, the singleton verbs abandon the
-- transaction on it, and both readings are lawful because an accepted request
-- may lawfully obstruct a later one.
data MutableConstraintOutcome
  = MutableConstraintRejected !UndirectedEdgeId
  | MutableConstraintAccepted !ConstraintRequestAccumulator

data MutableProgramCursor = MutableProgramCursor
  { mutableCursorAt :: !VertexId
  , mutableCursorHeader :: {-# UNPACK #-} !Int
  , mutableCursorWriteAt :: {-# UNPACK #-} !Int
  , mutableCursorConflictCount :: {-# UNPACK #-} !Int
  , mutableCursorPieceCount :: {-# UNPACK #-} !Int
  , mutableCursorAfterOverlap :: !Bool
  }

data MutablePlanWalk
  = MutablePlanActive !Intersection !MutableProgramCursor
  | MutablePlanComplete !MutableConstraintProgram
  | MutablePlanBlocked !DirectedEdgeId
  | MutablePlanFailed !CorridorObstruction

data MutableConstraintScan
  = MutableConstraintScanBlocked !DirectedEdgeId
  | MutableConstraintScanAdmitted !MutableConstraintProgram

data ConstraintProgramAccumulator = ConstraintProgramAccumulator
  { accumulatedProgramCursor :: {-# UNPACK #-} !Int
  , accumulatedProgramRequest :: !ConstraintRequestAccumulator
  }

-- | Result of maximal constrained construction, including rejected requests.
data CdtBuildResult vertex directed undirected face = CdtBuildResult
  { cdtAcceptedBuild :: {-# UNPACK #-} !(BuildResult 'Constrained vertex directed undirected face)
    -- ^ Canonical build result containing every admitted constraint.
  , cdtRejectedConstraints :: !(V.Vector (Int, Int))
    -- ^ Input-index pairs that could not be admitted.
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

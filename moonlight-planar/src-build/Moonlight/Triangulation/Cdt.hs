-- | The constraint layer: the partial map from sites and segments, defined
-- exactly on realizable segment sets and naming its witness where it is not.
module Moonlight.Triangulation.Cdt
  ( ConstrainedDelaunayTriangulation
  , CdtError (..)
  , CorridorObstruction (..)
  , ConstraintRecoveryResult (..)
  , ConstraintResult
  , ConstraintOutcome (..)
  , ConstraintBatchStats (..)
  , ConstraintBatchResult (..)
  , ConstrainedExtensionResult (..)
  , ConstrainedSeamSide (..)
  , ConstrainedSeamFaceEvidence
  , constrainedSeamSourceFace
  , constrainedSeamTargetFace
  , constrainedSeamFaceFirstPoint
  , constrainedSeamFaceSecondPoint
  , constrainedSeamFaceThirdPoint
  , ConstrainedSeamResult
  , constrainedSeamResultTriangulation
  , constrainedSeamLeftFaceCount
  , constrainedSeamRightFaceEvidence
  , constrainedSeamJoinFaces
  , constrainedSeamLeftConstraintCount
  , constrainedSeamConstraintStats
  , constrainedSeamBuildStats
  , constrainedSeamPublicationStats
  , constrainedSeamCachedFrontierPointReads
  , ConstraintSplitBatchResult
  , CdtBuildResult (..)
  , constrainedDelaunay
  , constrainedDelaunayMaximal
  , fromDelaunay
  , constraintEdges
  , CanonicalSegment
  , segmentStart
  , segmentEnd
  , ConstraintConflict
  , conflictFirstSegment
  , conflictSecondSegment
  , ConstrainedUnionError (..)
  , constraintSegments
  , unionConstrainedWith
  , unionConstrained
  , joinSeparatedConstrained
  , extendConstrainedWith
  , constraintStorageBytes
  , existsConstraint
  , canAddConstraint
  , intersectsConstraint
  , getConflictingEdgesBetweenPoints
  , getConflictingEdgesBetweenVertices
  , recoverConstraints
  , addConstraintEdge
  , addConstraintEdges
  , addConstraintAndSplit
  , addConstraintsAndSplit
  , removeConstraintEdge
  , outerRegionFaces
  , boundedRegionFaces
  ) where

import Moonlight.Triangulation.Internal.Cdt.Batch (recoverConstraints)
import Moonlight.Triangulation.Internal.Cdt.Build
  ( constrainedDelaunay
  , constrainedDelaunayMaximal
  , fromDelaunay
  )
import Moonlight.Triangulation.Internal.Cdt.Query
  ( canAddConstraint
  , constraintEdges
  , constraintStorageBytes
  , existsConstraint
  , getConflictingEdgesBetweenPoints
  , getConflictingEdgesBetweenVertices
  , intersectsConstraint
  )
import Moonlight.Triangulation.Internal.Cdt.Region
  ( boundedRegionFaces
  , outerRegionFaces
  )
import Moonlight.Triangulation.Internal.Cdt.Segment
  ( addConstraintEdge
  , addConstraintEdges
  , removeConstraintEdge
  )
import Moonlight.Triangulation.Internal.Cdt.Split
  ( addConstraintAndSplit
  , addConstraintsAndSplit
  )
import Moonlight.Triangulation.Internal.Cdt.Types
  ( CanonicalSegment (..)
  , CdtBuildResult (..)
  , CdtError (..)
  , ConstrainedUnionError (..)
  , ConstraintBatchResult (..)
  , ConstraintBatchStats (..)
  , ConstrainedExtensionResult (..)
  , ConstrainedSeamSide (..)
  , ConstrainedSeamFaceEvidence (..)
  , ConstrainedSeamResult (..)
  , ConstraintConflict (..)
  , ConstraintOutcome (..)
  , ConstraintRecoveryResult (..)
  , ConstraintResult
  , ConstraintSplitBatchResult
  , CorridorObstruction (..)
  )
import Moonlight.Triangulation.Internal.Cdt.Union
  ( constraintSegments
  , extendConstrainedWith
  , joinSeparatedConstrained
  , unionConstrained
  , unionConstrainedWith
  )
import Moonlight.Triangulation.Internal.Representation
  ( ConstrainedDelaunayTriangulation
  )

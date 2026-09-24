-- | The constraint layer: the partial map from sites and segments, defined
-- exactly on realizable segment sets and naming its witness where it is not.
module Moonlight.Planar.Cdt
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

import Moonlight.Planar.Internal.Cdt.Batch (recoverConstraints)
import Moonlight.Planar.Internal.Cdt.Build
  ( constrainedDelaunay
  , constrainedDelaunayMaximal
  , fromDelaunay
  )
import Moonlight.Planar.Internal.Cdt.Query
  ( canAddConstraint
  , constraintEdges
  , constraintStorageBytes
  , existsConstraint
  , getConflictingEdgesBetweenPoints
  , getConflictingEdgesBetweenVertices
  , intersectsConstraint
  )
import Moonlight.Planar.Internal.Cdt.Region
  ( boundedRegionFaces
  , outerRegionFaces
  )
import Moonlight.Planar.Internal.Cdt.Segment
  ( addConstraintEdge
  , addConstraintEdges
  , removeConstraintEdge
  )
import Moonlight.Planar.Internal.Cdt.Split
  ( addConstraintAndSplit
  , addConstraintsAndSplit
  )
import Moonlight.Planar.Internal.Cdt.Types
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
import Moonlight.Planar.Internal.Cdt.Union
  ( constraintSegments
  , extendConstrainedWith
  , joinSeparatedConstrained
  , unionConstrained
  , unionConstrainedWith
  )
import Moonlight.Planar.Internal.Representation
  ( ConstrainedDelaunayTriangulation
  )

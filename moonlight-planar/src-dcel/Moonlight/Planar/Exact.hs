-- | Public exact-geometry capabilities. The implementation is private so
-- proof-preserving owners can share admitted descent without exposing raw
-- construction of invariant-bearing carriers.
module Moonlight.Planar.Exact
  ( ExactRational
  , ExactArithmeticError (..)
  , exactRational
  , exactRationalNumerator
  , exactRationalDenominator
  , exactRationalBitWidth
  , exactDivide
  , PositiveExact
  , UnitInterval
  , ScalarRefinementError (..)
  , positiveExact
  , positiveExactValue
  , unitInterval
  , unitIntervalValue
  , unitZero
  , unitHalf
  , unitOne
  , positiveOne
  , positiveTwo
  , exactHalf
  , exactThird
  , blendPositive
  , divideByPositive
  , ratioPositive
  , multiplyPositive
  , positiveSumSquares
  , ExactBounds
  , exactPointsBounds
  , boundsUnion
  , boundsOverlap
  , pointInBounds
  , boundsMinimumX
  , boundsMaximumX
  , boundsMinimumY
  , boundsMaximumY
  , ExactPoint
  , exactPoint
  , exactPointCoordinates
  , exactPointCross
  , exactPointBitWidth
  , ExactAffineLine
  , ExactHalfPlaneError (..)
  , exactAffineLine
  , exactAffineLineCoefficients
  , oppositeExactAffineLine
  , exactAffineLineIntersection
  , ExactClosedHalfPlane
  , exactClosedHalfPlane
  , exactClosedHalfPlaneFromDirectedEdge
  , exactClosedHalfPlaneLine
  , classifyExactPoint
  , ExactRetainedPolygon
  , exactRetainedPolygon
  , exactRetainedPolygonPoints
  , exactRetainedPolygonBoundary
  , ExactClipDisposition (..)
  , ExactClipError (..)
  , ExactClipReceipt (..)
  , emptyExactClipReceipt
  , exactClipRetainedPolygon
  , ExactVector (..)
  , exactVectorFromPoints
  , addExactVectors
  , exactCross
  , compareExactVectorAngle
  , translateExactPoint
  , ExactRay
  , exactRay
  , exactRayOrigin
  , exactRayDirection
  , ExactSegment
  , ExactGeometryError (..)
  , exactSegment
  , exactSegmentEndpoints
  , exactPointFromPoint
  , exactPointFromQueryPoint
  , exactPointToEmbeddingCandidate
  , exactOrient2d
  , exactOnClosedSegment
  , SegmentRelation (..)
  , allSegmentRelations
  , exactSegmentRelation
  , ExactIntersectionError (..)
  , exactLineIntersection
  , exactSupportingLineIntersection
  ) where

import Moonlight.Planar.Internal.Exact
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational, ExactArithmeticError (..), exactRational, exactRationalNumerator
  , exactRationalDenominator, exactRationalBitWidth, exactDivide
  , PositiveExact, UnitInterval, ScalarRefinementError (..)
  , positiveExact, positiveExactValue, unitInterval, unitIntervalValue
  , unitZero, unitHalf, unitOne, positiveOne, positiveTwo, exactHalf, exactThird
  , blendPositive, divideByPositive, ratioPositive, multiplyPositive, positiveSumSquares )
import Moonlight.Planar.Internal.SegmentRelation (SegmentRelation (..), allSegmentRelations)

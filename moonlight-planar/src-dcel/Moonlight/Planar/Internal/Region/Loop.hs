-- | One admitted loop prepared once for the relation and location questions
-- every region check asks of it, and exact point location in a closed chain
-- of exact points. Region admission and curve-region certification both read
-- these; neither re-derives them.
module Moonlight.Planar.Internal.Region.Loop
  ( LoopRelationWitness (..)
  , PreparedLoop
  , prepareLoop
  , preparedBounds
  , firstPreparedPoint
  , loopSegments
  , crossLoopRelations
  , preparedPointLocation
  , pointLocationInLoop
  , pointLocationInCycle
  , loopWinding
  , cycleWinding
  ) where

import Data.Bifunctor (first)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Moonlight.Planar.Exact
  ( ExactBounds
  , boundsOverlap
  , exactPointsBounds
  , pointInBounds
  , ExactPoint
  , ExactSegment
  , SegmentRelation
  , exactOnClosedSegment
  , exactOrient2d
  , exactPointCoordinates
  , exactPointCross
  , exactSegment
  )
import Moonlight.Planar.Internal.BoundaryCycle (cyclePairs)
import Moonlight.Planar.Internal.ExactRational (exactSignum)
import Moonlight.Planar.Internal.ExactSegmentEvents
  ( ExactSweepSegmentId (..)
  , exactSegmentEventPlan
  , exactSegmentRelationMap
  )
import Moonlight.Planar.Internal.Region.Bounds (exactLoopBounds)
import Moonlight.Planar.Internal.Region.Types
  ( ExactLoop (..)
  , RegionPointLocation (..)
  , RegionValidationError (..)
  )

data LoopRelationWitness = LoopRelationWitness
  !Int
  !Int
  !ExactSegment
  !ExactSegment
  !SegmentRelation

-- | One admitted loop with the bounds and segment vector every relation
-- question reads, built once per loop rather than once per pair asked.
data PreparedLoop = PreparedLoop
  !ExactLoop
  !ExactBounds
  !(V.Vector ExactSegment)

prepareLoop :: ExactLoop -> Either RegionValidationError PreparedLoop
prepareLoop loop@(ExactLoop points) =
  PreparedLoop loop (exactLoopBounds loop) <$> loopSegments points

preparedBounds :: PreparedLoop -> ExactBounds
preparedBounds (PreparedLoop _ bounds _) = bounds

firstPreparedPoint :: PreparedLoop -> ExactPoint
firstPreparedPoint (PreparedLoop (ExactLoop (point :| _)) _ _) = point

loopSegments
  :: NonEmpty ExactPoint
  -> Either RegionValidationError (V.Vector ExactSegment)
loopSegments points =
  V.fromList
    <$> traverse
      (\(from, to) ->
         first (const (RegionLoopDegenerate (NonEmpty.toList points)))
           (exactSegment from to))
      (cyclePairs points)

crossLoopRelations
  :: PreparedLoop
  -> PreparedLoop
  -> Either RegionValidationError [LoopRelationWitness]
crossLoopRelations (PreparedLoop _ leftBounds leftSegments) (PreparedLoop _ rightBounds rightSegments)
  | not (boundsOverlap leftBounds rightBounds) = Right []
  | otherwise = do
      let leftCount = V.length leftSegments
      plan <-
        first RegionSegmentEventsInvalid
          (exactSegmentEventPlan (leftSegments <> rightSegments))
      pure
        [ LoopRelationWitness
            leftIndex
            (rightIndex - leftCount)
            (leftSegments V.! leftIndex)
            (rightSegments V.! (rightIndex - leftCount))
            relation
        | ((ExactSweepSegmentId leftIndex, ExactSweepSegmentId rightIndex), relation) <-
            Map.toAscList (exactSegmentRelationMap plan)
        , leftIndex < leftCount
        , rightIndex >= leftCount
        ]

pointLocationInLoop :: ExactLoop -> ExactPoint -> RegionPointLocation
pointLocationInLoop (ExactLoop points) = pointLocationInCycle points

preparedPointLocation :: PreparedLoop -> ExactPoint -> RegionPointLocation
preparedPointLocation (PreparedLoop (ExactLoop points) bounds _) =
  pointLocationInCycleWithin bounds points

-- | Where a point lies against the closed cycle through the given points:
-- 'RegionOnBoundary' when it lies on a cycle edge, and otherwise the parity of
-- the cycle's edges crossing the rightward ray from it. Any closed point cycle
-- is read, simple or not; for one that is not simple the answer is that
-- parity. Curve nesting reads it on certified chord cycles.
pointLocationInCycle :: NonEmpty ExactPoint -> ExactPoint -> RegionPointLocation
pointLocationInCycle points = pointLocationInCycleWithin (exactPointsBounds points) points

pointLocationInCycleWithin :: ExactBounds -> NonEmpty ExactPoint -> ExactPoint -> RegionPointLocation
pointLocationInCycleWithin bounds points query
  | not (pointInBounds query bounds) = RegionExterior
  | any (\(from, to) -> exactOnClosedSegment from to query) edges = RegionOnBoundary
  | odd (length (filter crossesRay edges)) = RegionInterior
  | otherwise = RegionExterior
 where
  edges = cyclePairs points
  (_, py) = exactPointCoordinates query
  crossesRay (from, to) =
    let (_, ay) = exactPointCoordinates from
        (_, by) = exactPointCoordinates to
        orientation = exactOrient2d from to query
     in (ay <= py && py < by && orientation == GT)
          || (by <= py && py < ay && orientation == LT)

loopWinding :: ExactLoop -> Ordering
loopWinding (ExactLoop points) = cycleWinding points

-- | The sign of the signed area enclosed by the closed chain through the
-- given points: 'GT' counter-clockwise.
cycleWinding :: NonEmpty ExactPoint -> Ordering
cycleWinding points =
  exactSignum
    ( List.foldl'
        (\signedArea (from, to) ->
           signedArea + exactPointCross from to)
        0
        (cyclePairs points)
    )

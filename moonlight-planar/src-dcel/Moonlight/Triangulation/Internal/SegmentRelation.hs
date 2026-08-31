{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | The complete closed-segment relation vocabulary and its one policy owner.
module Moonlight.Triangulation.Internal.SegmentRelation
  ( SegmentRelation (..)
  , allSegmentRelations
  , segmentRelationWith
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | The complete exact-predicate relation between two closed segments. There
-- is one vocabulary owner; traversal and constrained-union consumers derive
-- their booleans and obstruction policy from it rather than cloning slightly
-- different orientation formulae.
data SegmentRelation
  = SegmentsDisjoint
  | SegmentsDuplicate
  | SegmentsShareEndpoint
  | SegmentsProperlyCross
  | SegmentEndpointTouchesInterior
  | SegmentsCollinearlyOverlap
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Every segment relation in constructor order.
allSegmentRelations :: [SegmentRelation]
allSegmentRelations = [minBound .. maxBound]

-- | Classify two closed segments using the supplied point observations.
segmentRelationWith
  :: (point -> point -> Bool)
  -- ^ Point equality.
  -> (point -> point -> Ordering)
  -- ^ Lexicographic point ordering.
  -> (point -> point -> point -> Ordering)
  -- ^ Orientation of an ordered triple.
  -> (point -> point -> point -> Bool)
  -- ^ Membership of the third point in the closed segment.
  -> point
  -> point
  -> point
  -> point
  -> SegmentRelation
segmentRelationWith equalPoint comparePoint orientation onSegment a b c d
  | sameUndirectedSegment = SegmentsDuplicate
  | sharesEndpoint = SegmentsShareEndpoint
  | opposite abC abD && opposite cdA cdB = SegmentsProperlyCross
  | abC == EQ && abD == EQ && cdA == EQ && cdB == EQ = collinearRelation
  | endpointTouches = SegmentEndpointTouchesInterior
  | otherwise = SegmentsDisjoint
 where
  !abC = orientation a b c
  !abD = orientation a b d
  !cdA = orientation c d a
  !cdB = orientation c d b
  sameUndirectedSegment =
    (equalPoint a c && equalPoint b d)
      || (equalPoint a d && equalPoint b c)
  sharesEndpoint =
    equalPoint a c
      || equalPoint a d
      || equalPoint b c
      || equalPoint b d
  endpointTouches =
    (abC == EQ && onSegment a b c)
      || (abD == EQ && onSegment a b d)
      || (cdA == EQ && onSegment c d a)
      || (cdB == EQ && onSegment c d b)
  collinearRelation =
    let !overlapLower = maximumPoint (minimumPoint a b) (minimumPoint c d)
        !overlapUpper = minimumPoint (maximumPoint a b) (maximumPoint c d)
     in case comparePoint overlapLower overlapUpper of
          LT -> SegmentsCollinearlyOverlap
          EQ -> SegmentEndpointTouchesInterior
          GT -> SegmentsDisjoint
  opposite left right =
    (left == LT && right == GT) || (left == GT && right == LT)
  minimumPoint left right =
    case comparePoint left right of
      GT -> right
      _ -> left
  maximumPoint left right =
    case comparePoint left right of
      LT -> right
      _ -> left
{-# INLINE segmentRelationWith #-}

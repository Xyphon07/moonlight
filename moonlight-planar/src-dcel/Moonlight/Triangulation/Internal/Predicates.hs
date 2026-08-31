{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | Exact predicates over points already admitted by a construction or query
-- boundary.  This module is package-private: public callers must carry
-- 'QueryPoint' evidence through "Moonlight.Triangulation.Math" instead of
-- presenting arbitrary IEEE-754 payloads to an exact classifier.
module Moonlight.Triangulation.Internal.Predicates
  ( orient2dCoordinates
  , inCircleCoordinates
  , orient2d
  , sideQuery
  , inCircle
  , onClosedSegment
  , segmentRelation
  , segmentsProperlyCross
  , segmentsIntersect
  , inDiametralCircle
  ) where

import Moonlight.Triangulation.Internal.Dyadic
  ( exactDiametralDot
  , exactInCircleDet
  , exactOrientSignDouble
  )
import Moonlight.Triangulation.Internal.SegmentRelation
  ( SegmentRelation (..)
  , segmentRelationWith
  )
import Moonlight.Triangulation.LineSideInfo (LineSideInfo, fromOrdering)
import Moonlight.Triangulation.Scalar
  ( isFinite
  , scalarCcwErrorBound
  , scalarInCircleErrorBound
  )
import Moonlight.Triangulation.Internal.Types (Point (..))

-- The coordinate kernels are package-private because their raw 'Double'
-- arguments carry no finite-range evidence. Every public exact classifier
-- descends through 'QueryPoint'; internal hot paths call these only after their
-- construction boundary has admitted the stored coordinates.
orient2dCoordinates
  :: Double -> Double -> Double -> Double -> Double -> Double
  -> Ordering
orient2dCoordinates ax ay bx by cx cy
  | abs determinant > scalarCcwErrorBound * determinantSum = compare determinant 0
  | otherwise = exactOrientSignDouble ax ay bx by cx cy
 where
  !left = (ax - cx) * (by - cy)
  !right = (ay - cy) * (bx - cx)
  !determinant = left - right
  !determinantSum = abs left + abs right
{-# INLINE orient2dCoordinates #-}

inCircleCoordinates
  :: Double -> Double -> Double -> Double
  -> Double -> Double -> Double -> Double
  -> Ordering
inCircleCoordinates ax ay bx by cx cy dx dy
  | abs determinant > scalarInCircleErrorBound * permanent = compare determinant 0
  | otherwise = compare (exactInCircleDet ax ay bx by cx cy dx dy) 0
 where
  !adx = ax - dx
  !ady = ay - dy
  !bdx = bx - dx
  !bdy = by - dy
  !cdx = cx - dx
  !cdy = cy - dy
  !abdet = adx * bdy - bdx * ady
  !bcdet = bdx * cdy - cdx * bdy
  !cadet = cdx * ady - adx * cdy
  !alift = adx * adx + ady * ady
  !blift = bdx * bdx + bdy * bdy
  !clift = cdx * cdx + cdy * cdy
  !determinant = alift * bcdet + blift * cadet + clift * abdet
  !permanent =
    (abs (bdx * cdy) + abs (cdx * bdy)) * alift
      + (abs (cdx * ady) + abs (adx * cdy)) * blift
      + (abs (adx * bdy) + abs (bdx * ady)) * clift
{-# INLINE inCircleCoordinates #-}

-- | Exact orientation of points whose coordinates have already been admitted.
orient2d :: Point -> Point -> Point -> Ordering
orient2d (Point ax ay) (Point bx by) (Point cx cy) =
  orient2dCoordinates ax ay bx by cx cy
{-# INLINE orient2d #-}

-- | Exact side of an admitted oriented line.
sideQuery :: Point -> Point -> Point -> LineSideInfo
sideQuery from to query = fromOrdering (orient2d from to query)
{-# INLINE sideQuery #-}

-- | Ordering of the admitted oriented incircle determinant.
inCircle :: Point -> Point -> Point -> Point -> Ordering
inCircle
  (Point ax ay)
  (Point bx by)
  (Point cx cy)
  (Point dx dy) =
    inCircleCoordinates ax ay bx by cx cy dx dy
{-# INLINE inCircle #-}

-- | Whether an admitted point lies on an admitted closed segment.
onClosedSegment :: Point -> Point -> Point -> Bool
onClosedSegment from@(Point fromX fromY) to@(Point toX toY) query@(Point queryX queryY) =
  orient2d from to query == EQ
    && queryX >= min fromX toX
    && queryX <= max fromX toX
    && queryY >= min fromY toY
    && queryY <= max fromY toY
{-# INLINE onClosedSegment #-}

-- | Exact relation between admitted closed segments.
segmentRelation :: Point -> Point -> Point -> Point -> SegmentRelation
segmentRelation = segmentRelationWith (==) compare orient2d onClosedSegment

-- | Whether two admitted closed segments share any point.
segmentsIntersect :: Point -> Point -> Point -> Point -> Bool
segmentsIntersect firstFrom firstTo secondFrom secondTo =
  segmentRelation firstFrom firstTo secondFrom secondTo /= SegmentsDisjoint

-- | Whether two admitted segments cross away from their endpoints.
segmentsProperlyCross :: Point -> Point -> Point -> Point -> Bool
segmentsProperlyCross firstFrom firstTo secondFrom secondTo =
  opposite (orient2d firstFrom firstTo secondFrom) (orient2d firstFrom firstTo secondTo)
    && opposite (orient2d secondFrom secondTo firstFrom) (orient2d secondFrom secondTo firstTo)
 where
  opposite LT GT = True
  opposite GT LT = True
  opposite _ _ = False

-- | Whether an admitted point lies in the closed diametral disk of an admitted
-- segment.
inDiametralCircle :: Point -> Point -> Point -> Bool
inDiametralCircle (Point ax ay) (Point bx by) (Point px py)
  | isFinite dot && abs dot > scalarCcwErrorBound * dotSum = dot < 0
  | otherwise = exactDiametralDot ax ay bx by px py <= 0
 where
  !left = (ax - px) * (bx - px)
  !right = (ay - py) * (by - py)
  !dot = left + right
  !dotSum = abs left + abs right
{-# INLINE inDiametralCircle #-}

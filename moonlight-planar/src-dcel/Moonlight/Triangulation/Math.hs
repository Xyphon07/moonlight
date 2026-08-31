{-# LANGUAGE BangPatterns #-}

-- | Robust planar predicates and derived Euclidean constructions.
module Moonlight.Triangulation.Math
  ( orient2d
  , sideQuery
  , inCircle
  , orientDetApprox
  , inCircleDetApprox
  , onClosedSegment
  , SegmentRelation (..)
  , allSegmentRelations
  , segmentRelation
  , segmentsProperlyCross
  , segmentsIntersect
  , squaredDistance
  , squaredDistanceWide
  , segmentDistanceSquared
  , segmentDistanceSquaredWide
  , distance
  , negateVector
  , normalizeVector
  , midpoint
  , centroid
  , triangleArea
  , triangleRadiusEdgeRatio
  , triangleRadiusEdgeRatioSquaredWithArea
  , triangleRadiusEdgeRatioSquaredWithAreaAndSides
  , circumcenter
  , barycentricCoordinates
  , inDiametralCircle
  , projectionFactor
  , canonicalPoint
  , canonicalCoordinate
  , validateCoordinate
  , mkQueryPoint
  , validatePoint
  , mitigateUnderflow
  , isFinite
  ) where

import Moonlight.Triangulation.Internal.Dyadic
  ( exactBarycentricDeterminants
  , integerRatioToDouble
  )
import Moonlight.Triangulation.Internal.Predicates qualified as Admitted
import Moonlight.Triangulation.Internal.SegmentRelation
  ( SegmentRelation (..)
  , allSegmentRelations
  )
import Moonlight.Triangulation.LineSideInfo (LineSideInfo)
import Moonlight.Triangulation.Scalar
  ( canonicalScalarZero
  , isFinite
  , maximumAllowedCoordinate
  , minimumAllowedCoordinate
  , scalarCcwErrorBound
  )
import Moonlight.Triangulation.Internal.Types
  ( BuildError (..)
  , CoordinateError (..)
  , Point (..)
  , PointValidationError (..)
  , QueryPoint (..)
  )

-- | Exact relation between two admitted closed segments.
segmentRelation
  :: QueryPoint
  -> QueryPoint
  -> QueryPoint
  -> QueryPoint
  -> SegmentRelation
segmentRelation firstFrom firstTo secondFrom secondTo =
  Admitted.segmentRelation
    (queryPointValue firstFrom)
    (queryPointValue firstTo)
    (queryPointValue secondFrom)
    (queryPointValue secondTo)

-- | Whether two admitted closed segments share any point.
segmentsIntersect
  :: QueryPoint
  -> QueryPoint
  -> QueryPoint
  -> QueryPoint
  -> Bool
segmentsIntersect firstFrom firstTo secondFrom secondTo =
  Admitted.segmentsIntersect
    (queryPointValue firstFrom)
    (queryPointValue firstTo)
    (queryPointValue secondFrom)
    (queryPointValue secondTo)

-- | The proper-crossing section of 'segmentRelation'. Consumers which reject
-- only that constructor need not compute the collinear and endpoint-touch
-- distinctions required by the complete ADT after either side already proves
-- separation.
segmentsProperlyCross
  :: QueryPoint
  -> QueryPoint
  -> QueryPoint
  -> QueryPoint
  -> Bool
segmentsProperlyCross firstFrom firstTo secondFrom secondTo =
  Admitted.segmentsProperlyCross
    (queryPointValue firstFrom)
    (queryPointValue firstTo)
    (queryPointValue secondFrom)
    (queryPointValue secondTo)

-- | Classify a coordinate outside the exact-predicate input domain.
validateCoordinate :: Double -> Maybe CoordinateError
validateCoordinate value
  | isNaN value = Just CoordinateNaN
  | isInfinite value = Just CoordinateInfinite
  | value /= 0 && abs value < minimumAllowedCoordinate = Just CoordinateTooSmall
  | abs value > maximumAllowedCoordinate = Just CoordinateTooLarge
  | otherwise = Nothing

-- | Admit and normalize a finite point for read-only geometric queries.
mkQueryPoint :: Point -> Either PointValidationError (QueryPoint)
mkQueryPoint point@(Point x y) = do
  maybe (Right ()) (Left . InvalidPointX) (validateCoordinate x)
  maybe (Right ()) (Left . InvalidPointY) (validateCoordinate y)
  Right (QueryPoint (canonicalPoint point))

-- | Validate a construction point while retaining its optional input slot.
validatePoint :: Maybe Int -> Point -> Either BuildError (QueryPoint)
validatePoint slot point@(Point x y) =
  case mkQueryPoint point of
    Left (InvalidPointX reason) -> Left (InvalidCoordinate slot x reason)
    Left (InvalidPointY reason) -> Left (InvalidCoordinate slot y reason)
    Right queryPoint -> Right queryPoint

-- | Round coordinates below the robust-predicate input floor toward zero.
-- The operation never changes a coordinate already accepted by
-- 'validateCoordinate'.
mitigateUnderflow :: Point -> Point
mitigateUnderflow (Point x y) = Point (mitigate x) (mitigate y)
 where
  mitigate :: Double -> Double
  mitigate value
    | value /= 0 && abs value < minimumAllowedCoordinate = 0
    | otherwise = value

-- | Canonicalize both coordinate components for point identity.
canonicalPoint :: Point -> Point
canonicalPoint (Point x y) = Point (canonicalCoordinate x) (canonicalCoordinate y)
{-# INLINE canonicalPoint #-}

-- | Round a signed zero to the canonical zero. The law that makes two points
-- at the same position compare equal lives here; @canonicalPoint@ is its
-- component-wise form and coordinate-carrying callers use it directly so no
-- t'Point' is built only to be taken apart again.
canonicalCoordinate :: Double -> Double
canonicalCoordinate = canonicalScalarZero
{-# INLINE canonicalCoordinate #-}

-- | Fast approximate signed orientation determinant.
orientDetApprox :: Point -> Point -> Point -> Double
orientDetApprox (Point ax ay) (Point bx by) (Point cx cy) =
  (ax - cx) * (by - cy) - (ay - cy) * (bx - cx)
{-# INLINE orientDetApprox #-}

-- | Exact orientation ordering of three admitted points.
orient2d :: QueryPoint -> QueryPoint -> QueryPoint -> Ordering
orient2d first second third =
  Admitted.orient2d
    (queryPointValue first)
    (queryPointValue second)
    (queryPointValue third)
{-# INLINE orient2d #-}

-- | Exact side of an admitted oriented line.
sideQuery :: QueryPoint -> QueryPoint -> QueryPoint -> LineSideInfo
sideQuery from to query =
  Admitted.sideQuery
    (queryPointValue from)
    (queryPointValue to)
    (queryPointValue query)
{-# INLINE sideQuery #-}

-- | Fast approximate oriented in-circle determinant.
inCircleDetApprox
  :: Point -> Point -> Point -> Point -> Double
inCircleDetApprox
  (Point ax ay)
  (Point bx by)
  (Point cx cy)
  (Point dx dy) =
    alift * bcdet + blift * cadet + clift * abdet
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
{-# INLINE inCircleDetApprox #-}

-- | Ordering of the admitted oriented incircle determinant. For a
-- counter-clockwise triangle, 'GT' means the fourth point lies strictly inside
-- its circumcircle.
inCircle
  :: QueryPoint -> QueryPoint -> QueryPoint -> QueryPoint -> Ordering
inCircle first second third query =
  Admitted.inCircle
    (queryPointValue first)
    (queryPointValue second)
    (queryPointValue third)
    (queryPointValue query)
{-# INLINE inCircle #-}

-- | Whether an admitted point lies on an admitted closed segment.
onClosedSegment :: QueryPoint -> QueryPoint -> QueryPoint -> Bool
onClosedSegment from to query =
  Admitted.onClosedSegment
    (queryPointValue from)
    (queryPointValue to)
    (queryPointValue query)
{-# INLINE onClosedSegment #-}

-- | Squared Euclidean distance.
squaredDistance :: Point -> Point -> Double
squaredDistance (Point ax ay) (Point bx by) =
  let !dx = ax - bx
      !dy = ay - by
   in dx * dx + dy * dy
{-# INLINE squaredDistance #-}

-- | Squared Euclidean distance in the mesh's Binary64 coordinate domain.
squaredDistanceWide :: Point -> Point -> Double
squaredDistanceWide = squaredDistance
{-# INLINE squaredDistanceWide #-}

-- | Squared distance from a point to a closed segment.
segmentDistanceSquared
  :: Point -> Point -> Point -> Double
segmentDistanceSquared from@(Point ax ay) to@(Point bx by) point@(Point px py)
  | lengthSquared == 0 = squaredDistance from point
  | factor <= 0 = squaredDistance from point
  | factor >= 1 = squaredDistance to point
  | otherwise = squaredDistance point (Point (ax + factor * dx) (ay + factor * dy))
 where
  !dx = bx - ax
  !dy = by - ay
  !lengthSquared = dx * dx + dy * dy
  !factor = ((px - ax) * dx + (py - ay) * dy) / lengthSquared
{-# INLINE segmentDistanceSquared #-}

-- | Comparison form retained beside 'segmentDistanceSquared' for callers that
-- state metric intent explicitly.
segmentDistanceSquaredWide
  :: Point -> Point -> Point -> Double
segmentDistanceSquaredWide = segmentDistanceSquared
{-# INLINE segmentDistanceSquaredWide #-}

-- | Euclidean distance.
distance :: Point -> Point -> Double
distance left right = sqrt (squaredDistance left right)
{-# INLINE distance #-}

-- | Negate a point interpreted as a vector.
negateVector :: Point -> Point
negateVector (Point x y) = Point (-x) (-y)
{-# INLINE negateVector #-}

-- | Normalize a point interpreted as a vector without overflowing its length.
normalizeVector :: Point -> Point
normalizeVector (Point x y)
  | scale == 0 = Point 0 0
  | otherwise =
      let !scaledX = x / scale
          !scaledY = y / scale
          !length' = sqrt (scaledX * scaledX + scaledY * scaledY)
       in Point (scaledX / length') (scaledY / length')
 where
  !scale = max (abs x) (abs y)
{-# INLINE normalizeVector #-}

-- | Midpoint of two points.
midpoint :: Point -> Point -> Point
midpoint (Point ax ay) (Point bx by) = Point (0.5 * ax + 0.5 * bx) (0.5 * ay + 0.5 * by)
{-# INLINE midpoint #-}

-- | Centroid of three points.
centroid :: Point -> Point -> Point -> Point
centroid (Point ax ay) (Point bx by) (Point cx cy) =
  Point (ax + (bx - ax) / 3 + (cx - ax) / 3) (ay + (by - ay) / 3 + (cy - ay) / 3)
{-# INLINE centroid #-}

-- | Unsigned area of a triangle.
triangleArea :: Point -> Point -> Point -> Double
triangleArea a b c = 0.5 * abs (orientDetApprox a b c)
{-# INLINE triangleArea #-}

-- | Circumradius divided by shortest edge length.
triangleRadiusEdgeRatio
  :: Point -> Point -> Point -> Maybe Double
triangleRadiusEdgeRatio p0 p1 p2
  | area <= 0 || shortest <= 0 = Nothing
  | not (isFinite ratio) = Nothing
  | otherwise = Just ratio
 where
  !area = triangleArea p0 p1 p2
  !side01 = distance p0 p1
  !side12 = distance p1 p2
  !side20 = distance p2 p0
  !shortest = min side01 (min side12 side20)
  !otherProduct
    | side01 <= side12 && side01 <= side20 = side12 * side20
    | side12 <= side20 = side20 * side01
    | otherwise = side01 * side12
  !ratio = otherProduct / (4 * area)

-- | The square of 'triangleRadiusEdgeRatio', for a triangle whose area the
-- caller already has.
--
-- The ratio is only ever compared against a bound, and both sides are
-- non-negative, so the comparison can be made between squares. That is the
-- whole reason to have this: it settles the same question without the three
-- square roots the lengths would need, on the path taken by every face
-- refinement considers.
--
-- The area is a parameter and a degenerate triangle answers with an infinity
-- rather than an absence, because the caller on that path has already computed
-- the area to ask the area question and does nothing with the absence but
-- compare an infinity in its place.
triangleRadiusEdgeRatioSquaredWithArea
  :: Double -> Point -> Point -> Point -> Double
triangleRadiusEdgeRatioSquaredWithArea area p0 p1 p2 =
  triangleRadiusEdgeRatioSquaredWithAreaAndSides
    area
    (squaredDistance p0 p1)
    (squaredDistance p1 p2)
    (squaredDistance p2 p0)

-- | The same squared ratio from side squares already paid for by a caller.
triangleRadiusEdgeRatioSquaredWithAreaAndSides
  :: Double -> Double -> Double -> Double -> Double
triangleRadiusEdgeRatioSquaredWithAreaAndSides area side01 side12 side20
  | area <= 0 || shortest <= 0 = 1 / 0
  | not (isFinite ratio) = 1 / 0
  | otherwise = ratio
 where
  !shortest = min side01 (min side12 side20)
  !otherProduct
    | side01 <= side12 && side01 <= side20 = side12 * side20
    | side12 <= side20 = side20 * side01
    | otherwise = side01 * side12
  !ratio = otherProduct / (16 * area * area)

-- The scale the determinants are divided by cancels out of the quotient
-- exactly, so the computation works on the unscaled differences and divides
-- once per coordinate. Scaling would only matter against overflow, and the
-- validated coordinate domain (|x| <= 3.3e60) keeps every intermediate below
-- 1e183, five orders below the Double ceiling; the four divisions it cost
-- are the circumcentre's hot-path price. Identical points answer through the
-- denominator, which is exactly zero exactly when they are collinear.
-- | Circumcenter of a nondegenerate triangle.
circumcenter
  :: Point -> Point -> Point -> Maybe (Point)
circumcenter (Point ax ay) (Point bx by) (Point cx cy)
  | denominator == 0 = Nothing
  | not (isFinite resultX && isFinite resultY) = Nothing
  | otherwise = Just (canonicalPoint (Point resultX resultY))
 where
  !bax = bx - ax
  !bay = by - ay
  !cax = cx - ax
  !cay = cy - ay
  !bLength = bax * bax + bay * bay
  !cLength = cax * cax + cay * cay
  !denominator = 2 * (bax * cay - bay * cax)
  !offsetX = (cay * bLength - bay * cLength) / denominator
  !offsetY = (bax * cLength - cax * bLength) / denominator
  !resultX = ax + offsetX
  !resultY = ay + offsetY

-- | Barycentric coordinates of a point in a nondegenerate triangle.
barycentricCoordinates
  :: Point -> Point -> Point -> Point
  -> Maybe (Double, Double, Double)
barycentricCoordinates a@(Point ax ay) b@(Point bx by) c@(Point cx cy) query@(Point qx qy)
  | all reliable [denominatorInfo, weightAInfo, weightBInfo, weightCInfo] =
      if denominator == 0
        then Nothing
        else Just (weightA / denominator, weightB / denominator, weightC / denominator)
  | exactDenominator == 0 = Nothing
  | otherwise =
      Just
        ( integerRatioToDouble exactWeightA exactDenominator
        , integerRatioToDouble exactWeightB exactDenominator
        , integerRatioToDouble exactWeightC exactDenominator
        )
 where
  !denominatorInfo@(denominator, _) = determinantInfo a b c
  !weightAInfo@(weightA, _) = determinantInfo query b c
  !weightBInfo@(weightB, _) = determinantInfo a query c
  !weightCInfo@(weightC, _) = determinantInfo a b query
  (!exactDenominator, !exactWeightA, !exactWeightB, !exactWeightC) =
    exactBarycentricDeterminants ax ay bx by cx cy qx qy

  reliable (determinant, determinantSum) =
    isFinite determinant && abs determinant > scalarCcwErrorBound * determinantSum

  determinantInfo :: Point -> Point -> Point -> (Double, Double)
  determinantInfo (Point px py) (Point rx ry) (Point sx sy) =
    let !left = (px - sx) * (ry - sy)
        !right = (py - sy) * (rx - sx)
     in (left - right, abs left + abs right)

-- | Whether an admitted point lies in the closed diametral disk of an admitted
-- segment, decided by the sign of @(a-p)·(b-p)@.
--
-- The approximation is two coordinate differences and one product per term
-- combined additively, which is the arithmetic shape 'orient2d' is analysed
-- under: the forward error of @fl(fl(a⊖b) ⊗ fl(c⊖d))@ combined by one rounded
-- addition does not depend on the sign of that combination. The orientation
-- coefficient therefore transfers unchanged, and the exact dot product remains
-- the oracle for the uncertain band.
inDiametralCircle :: QueryPoint -> QueryPoint -> QueryPoint -> Bool
inDiametralCircle from to query =
  Admitted.inDiametralCircle
    (queryPointValue from)
    (queryPointValue to)
    (queryPointValue query)
{-# INLINE inDiametralCircle #-}

-- | Projection parameter of a point onto an oriented segment line.
projectionFactor :: Point -> Point -> Point -> Double
projectionFactor (Point ax ay) (Point bx by) (Point qx qy)
  | lengthSquared == 0 = 0
  | otherwise = ((qx - ax) * dx + (qy - ay) * dy) / lengthSquared
 where
  !dx = bx - ax
  !dy = by - ay
  !lengthSquared = dx * dx + dy * dy
{-# INLINE projectionFactor #-}

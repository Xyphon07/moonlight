-- | Exact morphology on admitted convex polygons: origin anchoring, linear
-- edge-angle convolution, exact slab decomposition, and support-half-plane erosion.
module Moonlight.Planar.Internal.Minkowski.Operations
  ( structuringElement
  , structuringElementPolygon
  , convexMinkowskiSum
  , convexMinkowskiPolygon
  , erodeConvexBy
  , addExactPoints
  , subtractExactPoints
  ) where

import Data.Bifunctor (first)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Planar.Convex
  ( convexPolygonPoints
  , convexPolygonRegion
  , convexPolygonFromRetained
  , retainTranslatedConvexPolygon
  )
import Moonlight.Planar.Exact
  ( ExactClipDisposition (..)
  , ExactClipReceipt
  , ExactClosedHalfPlane
  , ExactPoint
  , ExactVector (..)
  , addExactVectors
  , exactClipRetainedPolygon
  , exactClosedHalfPlaneFromDirectedEdge
  , compareExactVectorAngle
  , exactVectorFromPoints
  , exactPoint
  , exactPointCoordinates
  , translateExactPoint
  )
import Moonlight.Planar.Internal.BoundaryCycle
  ( cyclePairs
  , cyclePairsNonEmpty
  , rotateCycleLeast
  , rotateCycleLeastBy
  )
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational
  )
import Moonlight.Planar.Internal.Minkowski.Types
  ( MinkowskiError (..)
  , StructuringElement (..)
  )
import Moonlight.Planar.Internal.Region.Types
  ( ConvexPolygon (..)
  , ExactLoop (..)
  , PlanarRegion
  , RegionPointLocation (..)
  )
import Moonlight.Planar.Region
  ( regionPointLocation
  )

structuringElement
  :: ConvexPolygon
  -> Either MinkowskiError StructuringElement
structuringElement polygon =
  let origin = exactPoint 0 0
      location = regionPointLocation (convexPolygonRegion polygon) origin
   in case location of
        RegionExterior -> Left (MinkowskiOriginOutside location)
        _ -> Right (StructuringElement polygon)

structuringElementPolygon :: StructuringElement -> ConvexPolygon
structuringElementPolygon (StructuringElement polygon) = polygon

convexMinkowskiSum
  :: ConvexPolygon
  -> ConvexPolygon
  -> PlanarRegion
convexMinkowskiSum left right =
  convexPolygonRegion (convexMinkowskiPolygon left right)

convexMinkowskiPolygon
  :: ConvexPolygon
  -> ConvexPolygon
  -> ConvexPolygon
convexMinkowskiPolygon left right =
  let leftPoints = rotateCycleLeastBy pointSweepKey (convexPolygonPoints left)
      rightPoints = rotateCycleLeastBy pointSweepKey (convexPolygonPoints right)
      start = addExactPoints (NonEmpty.head leftPoints) (NonEmpty.head rightPoints)
      directions =
        mergeDirections
          (edgeDirections leftPoints)
          (edgeDirections rightPoints)
      directionList = NonEmpty.toList directions
      scanned = scanl translateExactPoint start directionList
      resultPoints = start :| take (length directionList - 1) (drop 1 scanned)
   in ConvexPolygon (ExactLoop (rotateCycleLeast resultPoints))

-- | Erode one convex polygon by another through the strongest translated
-- support half-plane for each source edge. The canonical sorted exact descent
-- retains those source lines; a lower-dimensional residual is represented by
-- the empty polygonal region at this two-dimensional publication boundary.
erodeConvexBy
  :: ConvexPolygon
  -> ConvexPolygon
  -> Either MinkowskiError (Maybe ConvexPolygon, ExactClipReceipt)
erodeConvexBy source kernel = do
  let sourcePoints = convexPolygonPoints source
      kernelPoints = convexPolygonPoints kernel
      firstKernel = NonEmpty.head kernelPoints
      retained = retainTranslatedConvexPolygon
        (exactVectorFromPoints firstKernel (exactPoint 0 0)) source
  halfPlanes <-
    traverse
      (uncurry (strongestHalfPlane kernelPoints))
      (cyclePairs sourcePoints)
  (disposition, clipReceipt) <-
    first MinkowskiClipFailed (exactClipRetainedPolygon retained halfPlanes)
  let eroded =
        case disposition of
          ExactClipFullDimensional clipped ->
            Just (convexPolygonFromRetained clipped)
          ExactClipLowerDimensional _ -> Nothing
          ExactClipEmpty -> Nothing
  pure (eroded, clipReceipt)

edgeDirections :: NonEmpty ExactPoint -> NonEmpty ExactVector
edgeDirections = fmap (uncurry exactVectorFromPoints) . cyclePairsNonEmpty

mergeDirections
  :: NonEmpty ExactVector
  -> NonEmpty ExactVector
  -> NonEmpty ExactVector
mergeDirections (left :| leftTail) (right :| rightTail) =
  case compareExactVectorAngle left right of
    LT -> left :| mergeRemaining leftTail (right : rightTail)
    GT -> right :| mergeRemaining (left : leftTail) rightTail
    EQ -> addExactVectors left right :| mergeRemaining leftTail rightTail

mergeRemaining :: [ExactVector] -> [ExactVector] -> [ExactVector]
mergeRemaining [] right = right
mergeRemaining left [] = left
mergeRemaining left@(leftHead : leftTail) right@(rightHead : rightTail) =
  case compareExactVectorAngle leftHead rightHead of
    LT -> leftHead : mergeRemaining leftTail right
    GT -> rightHead : mergeRemaining left rightTail
    EQ -> addExactVectors leftHead rightHead : mergeRemaining leftTail rightTail

pointSweepKey :: ExactPoint -> (ExactRational, ExactRational)
pointSweepKey point =
  let (x, y) = exactPointCoordinates point
   in (y, x)

strongestHalfPlane
  :: NonEmpty ExactPoint
  -> ExactPoint
  -> ExactPoint
  -> Either MinkowskiError ExactClosedHalfPlane
strongestHalfPlane kernelPoints from to =
  let direction = exactVectorFromPoints from to
      supportPoint =
        case kernelPoints of
          initial :| remaining ->
            List.foldl'
              (\selected candidate ->
                 if directionPointCross direction candidate
                      < directionPointCross direction selected
                   then candidate
                   else selected)
              initial
              remaining
   in first MinkowskiInvalidHalfPlane
        ( exactClosedHalfPlaneFromDirectedEdge
            (subtractExactPoints from supportPoint)
            (subtractExactPoints to supportPoint)
        )

directionPointCross :: ExactVector -> ExactPoint -> ExactRational
directionPointCross (ExactVector directionX directionY) point =
  let (x, y) = exactPointCoordinates point
   in directionX * y - directionY * x

addExactPoints :: ExactPoint -> ExactPoint -> ExactPoint
addExactPoints left right =
  let (leftX, leftY) = exactPointCoordinates left
      (rightX, rightY) = exactPointCoordinates right
   in exactPoint (leftX + rightX) (leftY + rightY)

subtractExactPoints :: ExactPoint -> ExactPoint -> ExactPoint
subtractExactPoints left right =
  let (leftX, leftY) = exactPointCoordinates left
      (rightX, rightY) = exactPointCoordinates right
   in exactPoint (leftX - rightX) (leftY - rightY)

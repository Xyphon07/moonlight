{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact convex polygons: checked admission, canonical hull construction,
-- retained-polygon publication, and geometry observations. Morphology consumes
-- this owner; convex geometry has no dependency on morphology or overlay.
module Moonlight.Planar.Convex
  ( ConvexPolygon
  , ConvexError (..)
  , convexPolygon
  , convexPolygonPoints
  , convexPolygonBoundary
  , convexPolygonComponent
  , convexPolygonRegion
  , convexPolygonFromRetained
  , retainConvexPolygon
  , retainTranslatedConvexPolygon
  , convexPolygonFromLoop
  , convexHullPolygon
  , reflectConvexPolygon
  , convexPolygonCentroid
  , decomposeRegionIntoConvexSlabs
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Moonlight.Planar.Exact
  ( ExactArithmeticError
  , ExactPoint
  , ExactRational
  , ExactRetainedPolygon
  , ExactVector
  , exactDivide
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , exactRetainedPolygonPoints
  , translateExactPoint
  )
import Moonlight.Planar.Internal.Exact (retainAdmittedConvexCycle)
import Moonlight.Planar.Internal.BoundaryCycle
  ( cyclePairs
  , consecutivePairs
  , firstNonCounterClockwiseTurn
  , rotateCycleLeast
  )
import Moonlight.Planar.Internal.ExactRational (exactRationalFromDyadic)
import Moonlight.Planar.Internal.Region.Types
  ( ConvexPolygon (..)
  , ExactLoop (..)
  , PlanarRegion (..)
  , PolygonComponent (..)
  , RegionValidationError
  )
import Moonlight.Planar.Region (exactLoop, exactLoopPoints, regionBoundaryEdges)

-- | Convex admission and exact geometry failures, independent of any consumer.
data ConvexError
  = ConvexInvalidLoop !RegionValidationError
  | ConvexNonConvexTurn !Int !Ordering
  | ConvexHullDegenerate ![ExactPoint]
  | ConvexExactArithmetic !ExactArithmeticError
  | ConvexSlabOddCrossings !ExactRational !ExactRational !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

convexPolygon :: NonEmpty ExactPoint -> Either ConvexError ConvexPolygon
convexPolygon submitted = do
  loop <- first ConvexInvalidLoop (exactLoop submitted)
  case firstNonCounterClockwiseTurn exactOrient2d (exactLoopPoints loop) of
    Just (index, turn) -> Left (ConvexNonConvexTurn index turn)
    Nothing -> Right (ConvexPolygon loop)

convexPolygonPoints :: ConvexPolygon -> NonEmpty ExactPoint
convexPolygonPoints (ConvexPolygon loop) = exactLoopPoints loop

convexPolygonComponent :: ConvexPolygon -> PolygonComponent
convexPolygonComponent (ConvexPolygon loop) = PolygonComponent loop []

convexPolygonRegion :: ConvexPolygon -> PlanarRegion
convexPolygonRegion = PlanarRegion . pure . convexPolygonComponent

-- | Strict convexity and simplicity descend from the retained-line carrier;
-- publication only rotates the cycle to its canonical least point.
convexPolygonFromRetained :: ExactRetainedPolygon -> ConvexPolygon
convexPolygonFromRetained = admittedConvexPolygon . exactRetainedPolygonPoints

-- | Retain the existing simple, strictly convex cycle without readmission.
retainConvexPolygon :: ConvexPolygon -> ExactRetainedPolygon
retainConvexPolygon = retainAdmittedConvexCycle . convexPolygonPoints

-- | Exact translation preserves admission and the cycle's starting position.
retainTranslatedConvexPolygon :: ExactVector -> ConvexPolygon -> ExactRetainedPolygon
retainTranslatedConvexPolygon offset =
  retainAdmittedConvexCycle . fmap (`translateExactPoint` offset) . convexPolygonPoints

admittedConvexPolygon :: NonEmpty ExactPoint -> ConvexPolygon
admittedConvexPolygon = ConvexPolygon . ExactLoop . rotateCycleLeast

-- | A simple loop already discharges intersection checks. Only strict
-- counter-clockwise convexity remains to admit the stronger carrier.
convexPolygonFromLoop :: ExactLoop -> Maybe ConvexPolygon
convexPolygonFromLoop loop =
  case firstNonCounterClockwiseTurn exactOrient2d (exactLoopPoints loop) of
    Nothing -> Just (ConvexPolygon loop)
    Just _ -> Nothing

convexHullPolygon :: NonEmpty ExactPoint -> Either ConvexError ConvexPolygon
convexHullPolygon submitted =
  let points = NonEmpty.toList submitted
   in case convexHullPoints points of
        Nothing -> Left (ConvexHullDegenerate points)
        Just hullPoints -> Right (admittedConvexPolygon hullPoints)

reflectConvexPolygon :: ConvexPolygon -> ConvexPolygon
reflectConvexPolygon =
  admittedConvexPolygon . fmap negateExactPoint . convexPolygonPoints

-- | The arithmetic mean of the vertices, a strict interior witness for every
-- admitted polygon. This is not an area-density centroid.
convexPolygonCentroid :: ConvexPolygon -> Either ConvexError ExactPoint
convexPolygonCentroid polygon = do
  let points = convexPolygonPoints polygon
      count = fromIntegral (NonEmpty.length points)
      (sumX, sumY) =
        List.foldl'
          (\(accumulatedX, accumulatedY) point ->
             let (x, y) = exactPointCoordinates point
              in (accumulatedX + x, accumulatedY + y))
          (0, 0)
          points
  x <- first ConvexExactArithmetic (exactDivide sumX count)
  y <- first ConvexExactArithmetic (exactDivide sumY count)
  pure (exactPoint x y)

convexHullPoints :: [ExactPoint] -> Maybe (NonEmpty ExactPoint)
convexHullPoints submitted =
  case Set.toAscList (Set.fromList submitted) of
    firstPoint : secondPoint : thirdPoint : remaining ->
      let ordered = firstPoint : secondPoint : thirdPoint : remaining
          lower = dropFinal (reverse (List.foldl' hullStep [] ordered))
          upper = dropFinal (reverse (List.foldl' hullStep [] (reverse ordered)))
       in case lower <> upper of
            firstHullPoint : secondHullPoint : thirdHullPoint : hullTail ->
              Just (firstHullPoint :| (secondHullPoint : thirdHullPoint : hullTail))
            _ -> Nothing
    _ -> Nothing

hullStep :: [ExactPoint] -> ExactPoint -> [ExactPoint]
hullStep (current : previous : remaining) candidate
  | exactOrient2d previous current candidate /= GT =
      hullStep (previous : remaining) candidate
hullStep hull candidate = candidate : hull

dropFinal :: [value] -> [value]
dropFinal values =
  case reverse values of
    _ : remaining -> reverse remaining
    [] -> []

negateExactPoint :: ExactPoint -> ExactPoint
negateExactPoint point =
  let (x, y) = exactPointCoordinates point
   in exactPoint (negate x) (negate y)

-- | One exact source edge restricted to a vertical slab. Its midpoint height
-- orders crossings; the two endpoint values retain the actual affine edge,
-- rather than a rounded segment reconstructed from a sample.
data SlabCrossing = SlabCrossing !ExactRational !ExactPoint !ExactPoint

-- | Partition an admitted polygonal region, including holes and disconnected
-- components, at its distinct vertex abscissae. No boundary vertex lies in an
-- open slab, so its crossing order is fixed and even-odd adjacent pairs bound
-- exact convex trapezoids (or triangles at a collapsed endpoint). Slab closure
-- overlaps have area zero; their union is exactly the source region. Immutable
-- endpoint event groups restrict each slab to its active edges; dormant source
-- boundaries are not rescanned or interpolated at unrelated abscissae.
decomposeRegionIntoConvexSlabs
  :: PlanarRegion
  -> Either ConvexError [ConvexPolygon]
decomposeRegionIntoConvexSlabs region =
  concat <$> sequenceA
    (snd (List.mapAccumL descendActiveSlab IntMap.empty (consecutivePairs abscissae)))
 where
  boundaries = regionBoundaryEdges region
  abscissae = Set.toAscList (Set.fromList (fmap (fst . exactPointCoordinates . fst) boundaries))
  half = exactRationalFromDyadic 1 (-1)
  indexedNonvertical =
    [ (index, min fromX toX, max fromX toX, edge)
    | (index, edge@(from, to)) <- zip [0 ..] boundaries
    , let fromX = fst (exactPointCoordinates from)
          toX = fst (exactPointCoordinates to)
    , fromX /= toX
    ]
  starts = Map.fromListWith IntMap.union
    [(fromX, IntMap.singleton index edge) | (index, fromX, _, edge) <- indexedNonvertical]
  ends = Map.fromListWith IntSet.union
    [(toX, IntSet.singleton index) | (index, _, toX, _) <- indexedNonvertical]

  descendActiveSlab
    :: IntMap.IntMap (ExactPoint, ExactPoint)
    -> (ExactRational, ExactRational)
    -> (IntMap.IntMap (ExactPoint, ExactPoint), Either ConvexError [ConvexPolygon])
  descendActiveSlab active bounds@(leftX, _) =
    let entering = Map.findWithDefault IntMap.empty leftX starts
        leaving = Map.findWithDefault IntSet.empty leftX ends
        activeSection = IntMap.union entering (IntMap.withoutKeys active leaving)
     in (activeSection, decomposeSlab activeSection bounds)

  decomposeSlab
    :: IntMap.IntMap (ExactPoint, ExactPoint)
    -> (ExactRational, ExactRational)
    -> Either ConvexError [ConvexPolygon]
  decomposeSlab active (leftX, rightX) = do
    let middleX = half * (leftX + rightX)
    crossings <- traverse (restrictEdge leftX rightX middleX) (IntMap.elems active)
    let ordered = List.sortOn crossingHeight crossings
        count = length ordered
        pairs = [pair | (index, pair) <- zip [0 :: Int ..] (consecutivePairs ordered), even index]
    if odd count
      then Left (ConvexSlabOddCrossings leftX rightX count)
      else catMaybes <$> traverse crossingPolygon pairs

  restrictEdge
    :: ExactRational
    -> ExactRational
    -> ExactRational
    -> (ExactPoint, ExactPoint)
    -> Either ConvexError SlabCrossing
  restrictEdge leftX rightX middleX (from, to) = do
    let (fromX, fromY) = exactPointCoordinates from
        (toX, toY) = exactPointCoordinates to
    slope <- first ConvexExactArithmetic (exactDivide (toY - fromY) (toX - fromX))
    let heightAt x = fromY + slope * (x - fromX)
    pure (SlabCrossing (heightAt middleX) (exactPoint leftX (heightAt leftX)) (exactPoint rightX (heightAt rightX)))

  crossingHeight :: SlabCrossing -> ExactRational
  crossingHeight (SlabCrossing height _ _) = height

  crossingPolygon :: (SlabCrossing, SlabCrossing) -> Either ConvexError (Maybe ConvexPolygon)
  crossingPolygon (SlabCrossing lowerHeight lowerLeft lowerRight, SlabCrossing upperHeight upperLeft upperRight)
    | lowerHeight == upperHeight = Right Nothing
    | otherwise = Just <$>
        (convexHullPolygon (lowerLeft :| [lowerRight, upperRight, upperLeft]))


-- | Counter-clockwise boundary of an admitted convex polygon.
convexPolygonBoundary :: ConvexPolygon -> [(ExactPoint, ExactPoint)]
convexPolygonBoundary = cyclePairs . convexPolygonPoints

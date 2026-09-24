-- | Conservative geometry layout, not painted-ink bounds. Stroke centerlines
-- contribute geometry; width, clipping and opacity do not change this view.
module Moonlight.Planar.Illustration.Layout
  ( GeometryEnvelope
  , geometryEnvelope
  , transformGeometryEnvelope
  , geometrySupport
  , geometryBounds
  , LayoutAxis (..)
  , alignMotif
  , arrangeMotifs
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Max (..))
import Data.Sequence (Seq, ViewL (..))
import qualified Data.Sequence as Seq
import Data.Traversable (mapAccumL)
import Moonlight.Planar.Affine
  ( Affine2, composeAffine2, identityAffine2, transformPoint, translationAffineIso2 )
import Moonlight.Planar.Curve
  ( CurveStep, Subpath (..), closedTrailSteps, curveStepEnd, locatedValue
  , location, pathSubpaths, stepControlPoints, trailSteps )
import Moonlight.Planar.Exact
  ( ExactBounds, ExactPoint, ExactRational, ExactVector (..), UnitInterval
  , boundsMaximumX, boundsMaximumY, boundsMinimumX, boundsMinimumY
  , exactPointCoordinates, exactPointsBounds, translateExactPoint, unitIntervalValue )
import Moonlight.Planar.Illustration (Picture, PictureAlgebra (..), foldPicture)
import Moonlight.Planar.Illustration.Motif (Motif, motifPicture, transformMotif)

-- Finite control support. Representation equality is not envelope equality.
newtype GeometryEnvelope = GeometryEnvelope (Seq ExactPoint)

instance Semigroup GeometryEnvelope where
  GeometryEnvelope a <> GeometryEnvelope b = GeometryEnvelope (a <> b)

instance Monoid GeometryEnvelope where
  mempty = GeometryEnvelope Seq.empty

-- | Each control point is transformed once after accumulating enclosing maps.
-- A located empty trail retains its anchor; an empty path/picture stays empty.
geometryEnvelope :: Picture part -> GeometryEnvelope
geometryEnvelope picture = foldPicture algebra picture identityAffine2
 where
  algebra :: PictureAlgebra part (Affine2 -> GeometryEnvelope)
  algebra = PictureAlgebra
    { paintSequence = \children frame -> foldMap ($ frame) children
    , paintFill = \_ _ contours frame -> foldMap (subpathEnvelope frame . ClosedSubpath) contours
    , paintStroke = \_ curve frame -> foldMap (subpathEnvelope frame) (pathSubpaths curve)
    , paintClip = \_ _ child -> child
    , paintPlace = \local child outer -> child (composeAffine2 outer local)
    , paintOpacity = \_ child -> child
    , paintAnnotation = \_ child -> child
    }

subpathEnvelope :: Affine2 -> Subpath -> GeometryEnvelope
subpathEnvelope frame subpath = case subpath of
  OpenSubpath value -> controlsEnvelope frame (location value) (trailSteps (locatedValue value))
  ClosedSubpath value -> controlsEnvelope frame (location value) (closedTrailSteps (locatedValue value))

controlsEnvelope :: Affine2 -> ExactPoint -> Seq CurveStep -> GeometryEnvelope
controlsEnvelope frame anchor steps = GeometryEnvelope $
  Seq.singleton (transformPoint frame anchor) <> foldMap id (snd (mapAccumL advance anchor steps))
 where
  advance :: ExactPoint -> CurveStep -> (ExactPoint, Seq ExactPoint)
  advance origin step =
    ( translateExactPoint origin (curveStepEnd step)
    , Seq.fromList (toList (transformPoint frame . translateExactPoint origin <$> stepControlPoints step))
    )

transformGeometryEnvelope :: Affine2 -> GeometryEnvelope -> GeometryEnvelope
transformGeometryEnvelope frame (GeometryEnvelope points) =
  GeometryEnvelope (transformPoint frame <$> points)

-- | Maximum unnormalised directional projection; Nothing is the empty case.
geometrySupport :: ExactVector -> GeometryEnvelope -> Maybe ExactRational
geometrySupport (ExactVector x y) (GeometryEnvelope points) =
  getMax <$> foldMap (Just . Max . project) points
 where
  project :: ExactPoint -> ExactRational
  project point = let (px, py) = exactPointCoordinates point in x * px + y * py

geometryBounds :: GeometryEnvelope -> Maybe ExactBounds
geometryBounds (GeometryEnvelope points) = case Seq.viewl points of
  EmptyL -> Nothing
  first :< rest -> Just (exactPointsBounds (first :| toList rest))

data LayoutAxis = Horizontal | Vertical
  deriving stock (Eq, Ord, Show)

-- | Align a minimum (0), midpoint (1/2), maximum (1), or interpolated edge.
-- Empty geometry remains unchanged; the perpendicular coordinate is retained.
alignMotif :: LayoutAxis -> UnitInterval -> ExactRational -> Motif port part -> Motif port part
alignMotif axis fraction target value = case geometryBounds (geometryEnvelope (motifPicture value)) of
  Nothing -> value
  Just bounds ->
    let (lo, hi) = axisInterval axis bounds
        q = unitIntervalValue fraction
     in shiftMotif axis (target - ((1 - q) * lo + q * hi)) value

-- | Measure each child once, preserving order and the first nonempty position.
-- Negative gaps overlap. Empty geometry neither consumes nor advances a gap.
arrangeMotifs :: LayoutAxis -> ExactRational -> Seq (Motif port part) -> Seq (Motif port part)
arrangeMotifs axis gap values = snd (mapAccumL arrange Nothing (measure <$> values))
 where
  measure :: Motif port part -> (Motif port part, Maybe (ExactRational, ExactRational))
  measure value = (value, axisInterval axis <$> geometryBounds (geometryEnvelope (motifPicture value)))
  arrange :: Maybe ExactRational -> (Motif port part, Maybe (ExactRational, ExactRational))
          -> (Maybe ExactRational, Motif port part)
  arrange edge (value, Nothing) = (edge, value)
  arrange Nothing (value, Just (_, hi)) = (Just hi, value)
  arrange (Just edge) (value, Just (lo, hi)) =
    let delta = edge + gap - lo
     in (Just (hi + delta), shiftMotif axis delta value)

axisInterval :: LayoutAxis -> ExactBounds -> (ExactRational, ExactRational)
axisInterval Horizontal bounds = (boundsMinimumX bounds, boundsMaximumX bounds)
axisInterval Vertical bounds = (boundsMinimumY bounds, boundsMaximumY bounds)

shiftMotif :: LayoutAxis -> ExactRational -> Motif port part -> Motif port part
shiftMotif axis delta = transformMotif (translationAffineIso2 displacement)
 where
  displacement = case axis of
    Horizontal -> ExactVector delta 0
    Vertical -> ExactVector 0 delta

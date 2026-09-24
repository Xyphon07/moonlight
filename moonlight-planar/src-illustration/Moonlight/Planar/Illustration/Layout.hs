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

import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup (Max (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Traversable (mapAccumL)
import Moonlight.Planar.Affine
  ( Affine2, composeAffine2, identityAffine2, transformPoint, translationAffineIso2 )
import Moonlight.Planar.Curve
  ( CurveStep, Subpath (..), closedTrailSteps, curveStepEnd, locatedValue
  , location, pathSubpaths, stepControlPoints, trailSteps )
import Moonlight.Planar.Exact
  ( ExactBounds, ExactPoint, ExactRational, ExactVector (..), UnitInterval
  , boundsMaximumX, boundsMaximumY, boundsMinimumX, boundsMinimumY, boundsUnion
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
geometryEnvelope = GeometryEnvelope . foldControls (Seq.|>) Seq.empty

-- The bounds of that same support, reduced without materialising it.
controlBounds :: Picture part -> Maybe ExactBounds
controlBounds = foldControls includeBounds Nothing

-- The sole traversal of located controls: a strict left fold under the fully
-- composed frame. A trail visits its anchor, then each step's controls after
-- the start, since that start is the previous end (or the anchor) already visited.
foldControls :: forall acc part. (acc -> ExactPoint -> acc) -> acc -> Picture part -> acc
foldControls visit start picture = foldPicture algebra picture identityAffine2 start
 where
  algebra :: PictureAlgebra part (Affine2 -> acc -> acc)
  algebra = PictureAlgebra
    { paintSequence = \children frame acc -> Foldable.foldl' (\inner child -> child frame inner) acc children
    , paintFill = \_ _ contours frame acc ->
        Foldable.foldl' (\inner -> subpath frame inner . ClosedSubpath) acc contours
    , paintStroke = \_ curve frame acc -> Foldable.foldl' (subpath frame) acc (pathSubpaths curve)
    , paintClip = \_ _ child -> child
    , paintPlace = \local child outer -> child (composeAffine2 outer local)
    , paintOpacity = \_ child -> child
    , paintAnnotation = \_ child -> child
    }
  subpath :: Affine2 -> acc -> Subpath -> acc
  subpath frame acc value = case value of
    OpenSubpath located -> trail frame acc (location located) (trailSteps (locatedValue located))
    ClosedSubpath located -> trail frame acc (location located) (closedTrailSteps (locatedValue located))
  trail :: Affine2 -> acc -> ExactPoint -> Seq CurveStep -> acc
  trail frame acc anchor steps = case Foldable.foldl' advance (Cursor anchor (emit acc anchor)) steps of
    Cursor _ total -> total
   where
    emit :: acc -> ExactPoint -> acc
    emit inner point = let !placed = transformPoint frame point in visit inner placed
    advance :: Cursor acc -> CurveStep -> Cursor acc
    advance (Cursor origin inner) step = Cursor (translateExactPoint origin (curveStepEnd step))
      (Foldable.foldl' (\current -> emit current . translateExactPoint origin) inner
        (NonEmpty.tail (stepControlPoints step)))

-- A step's start and the running total, both forced at every step.
data Cursor acc = Cursor !ExactPoint !acc

-- Strict: each union is forced before the next point arrives.
includeBounds :: Maybe ExactBounds -> ExactPoint -> Maybe ExactBounds
includeBounds bounds point = Just $! maybe single (`boundsUnion` single) bounds
 where
  single :: ExactBounds
  single = exactPointsBounds (point :| [])

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
geometryBounds (GeometryEnvelope points) = Foldable.foldl' includeBounds Nothing points

data LayoutAxis = Horizontal | Vertical
  deriving stock (Eq, Ord, Show)

-- | Align a minimum (0), midpoint (1/2), maximum (1), or interpolated edge.
-- Empty geometry remains unchanged; the perpendicular coordinate is retained.
alignMotif :: LayoutAxis -> UnitInterval -> ExactRational -> Motif port part -> Motif port part
alignMotif axis fraction target value = case controlBounds (motifPicture value) of
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
  measure value = (value, axisInterval axis <$> controlBounds (motifPicture value))
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

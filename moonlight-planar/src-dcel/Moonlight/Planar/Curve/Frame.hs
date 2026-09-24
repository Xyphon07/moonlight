-- | Approximately unit tangent frames on canonical curves, and finite runs of
-- stations placed by arc fraction.
--
-- A nonzero rational tangent @t@ has an exact direction and perpendicular,
-- but its unit normalization is generally irrational. A frame therefore
-- scales @t@ and its perpendicular by one positive rational @r@, a lower
-- bound of @1 / |t|@ taken from the radical-length owner. The frame's columns
-- are @r t@ and @(-r t_y, r t_x)@: exactly orthogonal, of equal length and
-- nonsingular. Their squared length @r^2 |t|^2@ is an exact rational in
-- @[1 - tolerance, 1]@, so the length itself lies in the same interval. The
-- second column is the mathematical left normal; in y-down drawing space it
-- appears on the viewer's right of the direction of travel.
--
-- A frame is read at one of two sites. A sampled site answers an arc-length
-- request and keeps that request's distance residual. An exact site is a
-- step and parameter of the source itself: it carries no residual, since no
-- distance was requested. Either may lie on a join between steps. A join is
-- regular only where both steps' tangents point the same way; a corner has
-- no unique tangent and refuses, as does a stationary tangent.
module Moonlight.Planar.Curve.Frame
  ( FramePolicy
  , framePolicy
  , exactTrailSite
  , FrameSite (..)
  , FrameError (..)
  , MeasuredFrame
  , regularFrame
  , measuredFrameIso
  , measuredFrameSite
  , measuredFrameTangent
  , measuredFrameScaleSquared
  , FractionRun
  , RunError (..)
  , fractionRun
  , runFractions
  , sampleRun
  ) where

import Data.Bifunctor (first)
import Data.Bits (toIntegralSized)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)
import Moonlight.Planar.Affine (AffineIso2, affine2, affineIso2)
import Moonlight.Planar.Curve (Subpath (..), stepJetFirst)
import Moonlight.Planar.Curve.Measure
  ( ArcSample, MeasureError, MeasuredTrail, measuredSource, pointAtFraction, sampleSite )
import Moonlight.Planar.Exact
  ( ExactRational, ExactVector (..), PositiveExact, UnitInterval, divideByPositive
  , exactCross, exactPointCoordinates, positiveExact, positiveExactValue, unitIntervalValue )
import Moonlight.Planar.Internal.CurveSource
  ( TrailSite, joinNeighbourJet, selectSite, siteJet, siteParameter, sitePoint, siteStepIndex )
import Moonlight.Planar.Internal.ExactRational (unitIntervalRun)
import Moonlight.Planar.Internal.Length
  ( RadicalPrecision, euclideanLengthEnclosure, lengthEnclosureLower )

-- | The precision of the inverse-speed enclosure and the largest admitted
-- deficit of a frame's squared scale below one.
data FramePolicy = FramePolicy !RadicalPrecision !PositiveExact
  deriving stock (Eq, Show)

framePolicy :: RadicalPrecision -> PositiveExact -> FramePolicy
framePolicy = FramePolicy

-- | The site at an exact parameter of a source's step, selected by its index
-- among the source's actual steps. At a join the selection is the side: the
-- earlier step at parameter one or the later step at parameter zero.
exactTrailSite :: Subpath -> Int -> UnitInterval -> Either FrameError TrailSite
exactTrailSite source index parameter =
  maybe (Left (SiteStepOutOfRange index)) Right (selectSite source index parameter)

data FrameSite
  = SampledSite !ArcSample
  | ExactSite !TrailSite
  deriving stock (Eq, Show)

-- | Refusals name the site's step and parameter. A normalization refusal
-- carries the squared scale reached and the policy's deficit.
data FrameError
  = SiteStepOutOfRange !Int
  | StationaryFrame !Int !UnitInterval
  | CornerFrame !Int !UnitInterval
  | NormalizationUnresolved !Int !UnitInterval !ExactRational !ExactRational
  deriving stock (Eq, Show)

-- | The site, the frame, the exact tangent it normalizes, and the exact
-- squared length of either column.
data MeasuredFrame = MeasuredFrame !FrameSite !AffineIso2 !ExactVector !ExactRational
  deriving stock (Eq, Show)

measuredFrameSite :: MeasuredFrame -> FrameSite
measuredFrameSite (MeasuredFrame site _ _ _) = site

-- | Maps local @+x@ to the scaled tangent and local @+y@ to the scaled left
-- normal, with the origin at the site's point. It is fed to attachment as it
-- is; nothing downstream renormalizes it.
measuredFrameIso :: MeasuredFrame -> AffineIso2
measuredFrameIso (MeasuredFrame _ frame _ _) = frame

-- | The first derivative of the site's step at the site's parameter.
measuredFrameTangent :: MeasuredFrame -> ExactVector
measuredFrameTangent (MeasuredFrame _ _ tangent _) = tangent

-- | The squared length of each column, in @[1 - tolerance, 1]@.
measuredFrameScaleSquared :: MeasuredFrame -> ExactRational
measuredFrameScaleSquared (MeasuredFrame _ _ _ scale) = scale

regularFrame :: FramePolicy -> FrameSite -> Either FrameError MeasuredFrame
regularFrame (FramePolicy precision tolerance) frameSite = do
  speedSquared <- first (const (StationaryFrame index parameter)) (positiveExact (dot tangent tangent))
  case joinNeighbourJet site of
    Just neighbour
      | let other = stepJetFirst neighbour
      , exactCross other tangent /= 0 || dot other tangent <= 0 ->
          Left (CornerFrame index parameter)
    _ -> Right ()
  -- |t / |t|^2| = 1 / |t|, enclosed from below by a rational r.
  let ExactVector x y = tangent
      scale = lengthEnclosureLower (euclideanLengthEnclosure precision
        [(divideByPositive x speedSquared, divideByPositive y speedSquared)])
      column = ExactVector (scale * x) (scale * y)
      ExactVector u v = column
      scaleSquared = scale * scale * positiveExactValue speedSquared
      deficit = positiveExactValue tolerance
      refused = NormalizationUnresolved index parameter scaleSquared deficit
      (px, py) = exactPointCoordinates (sitePoint site)
  if scaleSquared < 1 - deficit
    then Left refused
    else maybe (Left refused) (\frame -> Right (MeasuredFrame frameSite frame tangent scaleSquared))
      (affineIso2 (affine2 column (ExactVector (negate v) u) (ExactVector px py)))
 where
  site = case frameSite of
    SampledSite sample -> sampleSite sample
    ExactSite exact -> exact
  tangent = stepJetFirst (siteJet site)
  index = siteStepIndex site
  parameter = siteParameter site

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector a b) (ExactVector c d) = a * c + b * d

-- | Evenly spaced arc fractions, both ends included, in order. The count is
-- the placement budget; more than one station needs positive spacing.
newtype FractionRun = FractionRun (NonEmpty UnitInterval)
  deriving stock (Eq, Show)

-- | An empty run, a run whose spacing is not positive, a closed source asked
-- to place a station at both ends of its seam, or a refused station.
data RunError
  = EmptyRun !Int
  | NonPositiveSpacing !UnitInterval !UnitInterval
  | RunRepeatsSeam
  | RunStationRefused !MeasureError
  deriving stock (Eq, Show)

-- The interval count is admitted as a 'Natural' in one total step, on
-- 'Integer' so that no 'Int' subtraction can wrap.
fractionRun :: Int -> UnitInterval -> UnitInterval -> Either RunError FractionRun
fractionRun count start end = case toIntegralSized (toInteger count - 1) of
  Nothing -> Left (EmptyRun count)
  Just intervals
    | intervals > (0 :: Natural) && unitIntervalValue end <= unitIntervalValue start ->
        Left (NonPositiveSpacing start end)
    | otherwise -> Right (FractionRun (unitIntervalRun intervals start end))

runFractions :: FractionRun -> NonEmpty UnitInterval
runFractions (FractionRun fractions) = fractions

-- | One sample per station, in the run's order. On a closed source fraction
-- zero and fraction one are the same seam point, so a run holding both would
-- place two stations there; it is refused rather than silently merged.
sampleRun :: FractionRun -> MeasuredTrail -> Either RunError (NonEmpty ArcSample)
sampleRun (FractionRun fractions) trail
  | closed && unitIntervalValue (NonEmpty.head fractions) == 0
      && unitIntervalValue (NonEmpty.last fractions) == 1 = Left RunRepeatsSeam
  | otherwise = first RunStationRefused (traverse (`pointAtFraction` trail) fractions)
 where
  closed = case measuredSource trail of
    ClosedSubpath _ -> True
    OpenSubpath _ -> False

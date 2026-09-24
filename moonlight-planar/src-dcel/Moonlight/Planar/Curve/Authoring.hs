-- | Exact interpolation and transverse profiles for semantic authoring.
-- These constructors immediately produce the canonical curve algebra; they
-- retain neither an evaluator nor a second spline representation. Uniform
-- knot spacing is a parameter convention, not arc length. Interpolation does
-- not guarantee simplicity, absence of overshoot, or preservation of topology.
module Moonlight.Planar.Curve.Authoring
  ( Knot (..)
  , cardinalOpen
  , cardinalClosed
  , ProfileStation (..)
  , profileRails
  , profileOutline
  , polygonTrail
  , bowedTrail
  ) where

import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve
  ( ClosedTrail, CurveStep, Located, OpenTrail, closeWith, curveStep
  , curveStepShape, hermiteStep, line, locate, locatedValue, location
  , openTrail, quadratic, reverseTrail, trailDisplacement )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), UnitInterval, addExactVectors
  , exactHalf, exactVectorFromPoints, translateExactPoint, unitIntervalValue )
import Moonlight.Planar.Internal.BoundaryCycle (consecutivePairs, cyclicTriples)

-- | An interpolated landmark or a landmark with independently prescribed
-- incoming and outgoing derivatives. Explicit derivatives ignore tension;
-- distinct directions make intentional corners without a global refit.
data Knot
  = Interpolating !ExactPoint
  | ExplicitJets !ExactPoint !ExactVector !ExactVector
  deriving stock (Eq, Show)

-- | Uniform cardinal interpolation. At tension zero, interior derivatives
-- are half the neighbor secant (Catmull-Rom). Endpoint derivatives use the
-- one-sided secant; tension one zeros only the automatically derived jets.
-- A singleton is a located empty trail; repeated points remain lawful input.
cardinalOpen :: UnitInterval -> NonEmpty Knot -> Located OpenTrail
cardinalOpen tension knots@(initial :| remaining) = assembleOpen (firstJet :| otherJets)
 where
  scale = 1 - unitIntervalValue tension
  firstJet = resolve (case remaining of
    [] -> zero
    next : _ -> scaleVector scale (between initial next)) initial
  otherJets = fmap derive
    (zip3 (NonEmpty.toList knots) remaining (fmap Just (drop 1 remaining) <> [Nothing]))
  derive :: (Knot, Knot, Maybe Knot) -> KnotJet
  derive (previous, current, next) = resolve automatic current
   where
    automatic = case next of
      Nothing -> scaleVector scale (between previous current)
      Just following -> scaleVector (scale * exactHalf) (between previous following)

-- | Periodic uniform cardinal interpolation, including the closing segment.
-- A singleton is a constant closed curve unless explicit nonzero jets specify
-- a returning cubic. No minimum polygon vertex count is invented here.
cardinalClosed :: UnitInterval -> NonEmpty Knot -> Located ClosedTrail
cardinalClosed tension knots@(initial :| _) =
  case fmap derive (cyclicTriples (NonEmpty.toList knots)) of
    [] -> assembleClosed (resolve zero initial :| [])
    firstJet : remaining -> assembleClosed (firstJet :| remaining)
 where
  scale = (1 - unitIntervalValue tension) * exactHalf
  derive :: (Knot, Knot, Knot) -> KnotJet
  derive (previous, current, next) = resolve (scaleVector scale (between previous next)) current

-- | A center landmark and a signed transverse half-span, not a Euclidean
-- unit normal. Width, taper, skew, and asymmetry remain rational and affine
-- equivariant. A zero half-span joins the two rails at that station.
data ProfileStation = ProfileStation !ExactPoint !ExactVector
  deriving stock (Eq, Show)

-- | Interpolate center + half-span and center - half-span with one tension.
-- The cardinal operator is affine-linear in the landmarks, so this is exactly
-- the interpolated centerline plus/minus the interpolated transverse profile.
profileRails
  :: UnitInterval -> NonEmpty ProfileStation -> (Located OpenTrail, Located OpenTrail)
profileRails tension stations =
  (cardinalOpen tension (fmap (Interpolating . positiveSide) stations),
   cardinalOpen tension (fmap (Interpolating . negativeSide) stations))

-- | Traverse the positive rail, connect the terminal stations, return along
-- the negative rail, and close at the root. End connectors are straight.
-- This is a transverse ribbon, not a constant-distance offset or a guarantee
-- that the outline is a simple region.
profileOutline :: UnitInterval -> NonEmpty ProfileStation -> Located ClosedTrail
profileOutline tension stations = locate (location positiveRail) (closeWith line prefix)
 where
  (positiveRail, negativeRail) = profileRails tension stations
  positiveTrail = locatedValue positiveRail
  negativeTrail = locatedValue negativeRail
  positiveEnd = translateExactPoint (location positiveRail) (trailDisplacement positiveTrail)
  negativeEnd = translateExactPoint (location negativeRail) (trailDisplacement negativeTrail)
  connector = openTrail (Seq.singleton (curveStep line (exactVectorFromPoints positiveEnd negativeEnd)))
  prefix = positiveTrail <> connector <> reverseTrail negativeTrail

-- | Straight-edged authored contour. Unlike Region.exactLoop, this preserves
-- source order and makes no simplicity or dimensionality claim.
polygonTrail :: NonEmpty ExactPoint -> Located ClosedTrail
polygonTrail points@(initial :| _) = locate initial (closeWith line (openTrail (Seq.fromList
  (fmap (\(a,b) -> curveStep line (exactVectorFromPoints a b))
    (consecutivePairs (NonEmpty.toList points))))))

-- | A quadratic bow whose midpoint displacement from the chord midpoint is
-- @bow * perpendicular(chord)@. Bow is dimensionless and signed. This is not
-- a circular arc and performs no irrational unit-normal normalization.
bowedTrail :: ExactRational -> ExactPoint -> ExactPoint -> Located OpenTrail
bowedTrail bow start end = locate start (openTrail (Seq.singleton
  (curveStep (quadratic control) chord)))
 where
  chord@(ExactVector x y) = exactVectorFromPoints start end
  control = addExactVectors (scaleVector exactHalf chord)
    (scaleVector (2 * bow) (ExactVector (negate y) x))

data KnotJet = KnotJet !ExactPoint !ExactVector !ExactVector

resolve :: ExactVector -> Knot -> KnotJet
resolve automatic knot = case knot of
  Interpolating point -> KnotJet point automatic automatic
  ExplicitJets point incoming outgoing -> KnotJet point incoming outgoing

knotPoint :: Knot -> ExactPoint
knotPoint (Interpolating point) = point
knotPoint (ExplicitJets point _ _) = point

between :: Knot -> Knot -> ExactVector
between a b = exactVectorFromPoints (knotPoint a) (knotPoint b)

jetSpan :: KnotJet -> KnotJet -> CurveStep
jetSpan (KnotJet a _ outgoing) (KnotJet b incoming _) =
  hermiteStep (exactVectorFromPoints a b) outgoing incoming

assembleOpen :: NonEmpty KnotJet -> Located OpenTrail
assembleOpen jets@(KnotJet anchor _ _ :| _) = locate anchor (openTrail (Seq.fromList
  (fmap (uncurry jetSpan) (consecutivePairs (NonEmpty.toList jets)))))

assembleClosed :: NonEmpty KnotJet -> Located ClosedTrail
assembleClosed jets@(initial :| remaining) =
  let authored = assembleOpen jets
      final = Foldable.foldl' (\_ current -> current) initial remaining
   in locate (location authored)
        (closeWith (curveStepShape (jetSpan final initial)) (locatedValue authored))

positiveSide :: ProfileStation -> ExactPoint
positiveSide (ProfileStation center halfSpan) = translateExactPoint center halfSpan

negativeSide :: ProfileStation -> ExactPoint
negativeSide (ProfileStation center halfSpan) = translateExactPoint center (scaleVector (-1) halfSpan)

scaleVector :: ExactRational -> ExactVector -> ExactVector
scaleVector factor (ExactVector x y) = ExactVector (factor*x) (factor*y)

zero :: ExactVector
zero = ExactVector 0 0

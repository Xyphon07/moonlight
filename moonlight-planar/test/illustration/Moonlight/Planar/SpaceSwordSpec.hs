module Moonlight.Planar.SpaceSwordSpec (tests) where

import Control.Monad (unless)
import Data.Foldable (toList, traverse_)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (affineColumns, affineIsoMap)
import Moonlight.Planar.Curve
  ( CurveStep, Located, OpenTrail, Path, Subpath (..), jetStep, locate, locatedValue
  , location, openTrail, path, splitStep, stepControlPoints, stepJetFirst, trailSteps
  , transformPath )
import Moonlight.Planar.Curve.Authoring (profileRails)
import Moonlight.Planar.Curve.Frame
  ( FrameSite (..), MeasuredFrame, measuredFrameIso, measuredFrameScaleSquared
  , measuredFrameSite, measuredFrameTangent )
import Moonlight.Planar.Curve.Measure
  ( ArcSample, MeasurePolicy, lengthBounds, lengthEnclosureLower, lengthEnclosureUpper
  , measureSubpath, sampleResidual, sampleSite, siteParameter, sitePoint, siteStepIndex )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), exactHalf, exactPoint, exactPointCoordinates
  , exactRational, unitHalf, unitIntervalValue, unitOne )
import Moonlight.Planar.Exhibit.SpaceSword
import Moonlight.Planar.Illustration (Picture, PictureAlgebra (..), foldPicture)

tests :: IO ()
tests = do
  let chargeOnly = defaultSpaceSwordControls { bladeCharge = unitOne }
      sweepOnly = defaultSpaceSwordControls { bladeSweep = 40 }
      blade = [BladeAura, BladeShell, BladeCore, BladeRunes, IonWake]
  baseline <- require (spaceSwordPicture defaultSpaceSwordControls)
  overcharged <- require (spaceSwordPicture overchargedSpaceSwordControls)
  charged <- require (spaceSwordPicture chargeOnly)
  swept <- require (spaceSwordPicture sweepOnly)
  traverse_ (checkPart baseline overcharged blade) [minBound .. maxBound]
  traverse_ (checkPart baseline charged [BladeAura, BladeCore, BladeRunes, IonWake]) [minBound .. maxBound]
  traverse_ (checkPart baseline swept blade) [minBound .. maxBound]
  check "semantic charge/sweep edit changes the sword" (baseline /= overcharged)
  base <- require (runeFrames defaultSpaceSwordControls)
  chargedRunes <- require (runeFrames chargeOnly)
  sweptRunes <- require (runeFrames sweepOnly)
  check "charge leaves the rune stations and frames fixed" (base == chargedRunes)
  check "sweep moves every rune frame"
    (and (NonEmpty.zipWith (\a b -> measuredFrameIso a /= measuredFrameIso b) base sweptRunes))
  traverse_ runeLaws [defaultSpaceSwordControls, overchargedSpaceSwordControls, chargeOnly, sweepOnly]
  traverse_ centerlineIsRailMidpoint [defaultSpaceSwordControls, overchargedSpaceSwordControls]
  putStrLn "space sword: complete typed composition, measured rune run and selective edits ok"

-- Station distance and frame scale are separate bounds. Each rune sample is
-- checked against an independent measurement of the centerline up to its
-- site; each frame against the jet of the centerline's own step there.
runeLaws :: SpaceSwordControls -> IO ()
runeLaws controls = do
  frames <- require (runeFrames controls)
  (measuring, _) <- require runeMeasurePolicy
  fractions <- traverse require [exactRational 1 5, exactRational 1 2, exactRational 4 5]
  let centerline = channelCenterline (unitIntervalValue (bladeCharge controls)) (bladeSweep controls)
  total <- lengthBounds <$> require (measureSubpath measuring (OpenSubpath centerline))
  samples <- traverse sampleOf (toList frames)
  check "three runes" (length samples == 3)
  check "runes follow the run's order along the centerline"
    (and (zipWith (<) (key <$> samples) (drop 1 (key <$> samples))))
  traverse_ (\(fraction, sample, frame) -> do
      let residual = sampleResidual sample
      check "station residual within the arc-length tolerance" (residual <= exactHalf ^ (3 :: Int))
      (lower, upper) <- prefixLength measuring centerline sample
      check "station distance agrees with an independent prefix measurement"
        (lower - fraction * lengthEnclosureUpper total <= residual
          && fraction * lengthEnclosureLower total - upper <= residual)
      tangent <- stepTangent centerline sample
      let (column, normal, offset) = affineColumns (affineIsoMap (measuredFrameIso frame))
          ExactVector cx cy = column
          (px, py) = exactPointCoordinates (sitePoint (sampleSite sample))
      check "frame tangent is the step's own jet" (measuredFrameTangent frame == tangent)
      check "frame tangent column points along the jet" (cross column tangent == 0 && dot column tangent > 0)
      check "frame columns are the tangent and its left normal" (normal == ExactVector (negate cy) cx)
      check "frame origin is the station" (offset == ExactVector px py)
      check "frame scale within its normalization bound"
        (dot column column == measuredFrameScaleSquared frame
          && measuredFrameScaleSquared frame <= 1
          && 1 - measuredFrameScaleSquared frame <= exactHalf ^ (20 :: Int)))
    (zip3 fractions samples (toList frames))
 where
  key :: ArcSample -> (Int, ExactRational)
  key sample = (siteStepIndex (sampleSite sample), unitIntervalValue (siteParameter (sampleSite sample)))

sampleOf :: MeasuredFrame -> IO ArcSample
sampleOf frame = case measuredFrameSite frame of
  SampledSite sample -> pure sample
  ExactSite _ -> fail "rune frame is not an arc-length station"

-- The centerline's steps before the sample, then its own step cut at the
-- sample's parameter, measured afresh with the same policy.
prefixLength :: MeasurePolicy -> Located OpenTrail -> ArcSample -> IO (ExactRational, ExactRational)
prefixLength measuring centerline sample = do
  step <- sampleCurveStep centerline sample
  let steps = trailSteps (locatedValue centerline)
      prefix = Seq.take (siteStepIndex (sampleSite sample)) steps Seq.|> fst (splitStep (siteParameter (sampleSite sample)) step)
  measured <- require (measureSubpath measuring (OpenSubpath (locate (location centerline) (openTrail prefix))))
  let bounds = lengthBounds measured
  pure (lengthEnclosureLower bounds, lengthEnclosureUpper bounds)

stepTangent :: Located OpenTrail -> ArcSample -> IO ExactVector
stepTangent centerline sample = do
  step <- sampleCurveStep centerline sample
  pure (stepJetFirst (jetStep (siteParameter (sampleSite sample)) step))

sampleCurveStep :: Located OpenTrail -> ArcSample -> IO CurveStep
sampleCurveStep centerline sample =
  maybe (fail "sample step outside the centerline") pure
    (Seq.lookup (siteStepIndex (sampleSite sample)) (trailSteps (locatedValue centerline)))

-- The derived centerline is the exact average of the channel's two rails at
-- the channel's tension, one half.
centerlineIsRailMidpoint :: SpaceSwordControls -> IO ()
centerlineIsRailMidpoint controls = do
  let charge = unitIntervalValue (bladeCharge controls)
      sweep = bladeSweep controls
      (positive, negative) = profileRails unitHalf (channelStations charge sweep)
      center = channelCenterline charge sweep
      controlsOf :: Located OpenTrail -> Seq [ExactVector]
      controlsOf = fmap (toList . stepControlPoints) . trailSteps . locatedValue
  check "centerline anchor is the rails' midpoint"
    (location center == midpointPoint (location positive) (location negative))
  check "centerline controls are the rails' midpoints"
    (Seq.length (controlsOf center) == Seq.length (controlsOf positive)
      && Seq.length (controlsOf center) == Seq.length (controlsOf negative)
      && controlsOf center == Seq.zipWith (zipWith midpointVector) (controlsOf positive) (controlsOf negative))

midpointPoint :: ExactPoint -> ExactPoint -> ExactPoint
midpointPoint a b =
  let (ax, ay) = exactPointCoordinates a
      (bx, by) = exactPointCoordinates b
   in exactPoint ((ax + bx) * exactHalf) ((ay + by) * exactHalf)

midpointVector :: ExactVector -> ExactVector -> ExactVector
midpointVector (ExactVector ax ay) (ExactVector bx by) =
  ExactVector ((ax + bx) * exactHalf) ((ay + by) * exactHalf)

cross :: ExactVector -> ExactVector -> ExactRational
cross (ExactVector a b) (ExactVector c d) = a * d - b * c

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector a b) (ExactVector c d) = a * c + b * d

checkPart
  :: Picture SpaceSwordPart
  -> Picture SpaceSwordPart
  -> [SpaceSwordPart]
  -> SpaceSwordPart
  -> IO ()
checkPart baseline edited affected part = do
  let original = observedPaths part baseline
      candidate = observedPaths part edited
  check ("nonempty space-sword part: " <> spaceSwordPartName part) (not (null original))
  check ("space-sword control dependency: " <> spaceSwordPartName part)
    ((original /= candidate) == (part `elem` affected))

-- A pure test projection of the existing Picture algebra. It preserves
-- placement and annotation ownership; it is not another geometry evaluator.
observedPaths :: SpaceSwordPart -> Picture SpaceSwordPart -> [Path]
observedPaths selected picture = foldPicture algebra picture Nothing
 where
  algebra :: PictureAlgebra SpaceSwordPart (Maybe SpaceSwordPart -> [Path])
  algebra = PictureAlgebra
    { paintSequence = \children inherited -> concatMap ($ inherited) (toList children)
    , paintFill = \_ _ contours inherited ->
        retain inherited (path (Seq.fromList (ClosedSubpath <$> NonEmpty.toList contours)))
    , paintStroke = \_ curve inherited -> retain inherited curve
    , paintClip = \_ _ child -> child
    , paintPlace = \frame child inherited -> transformPath frame <$> child inherited
    , paintOpacity = \_ child -> child
    , paintAnnotation = \part child _ -> child (Just part)
    }
  retain :: Maybe SpaceSwordPart -> Path -> [Path]
  retain inherited curve = if inherited == Just selected then [curve] else []

require :: Show e => Either e a -> IO a
require = either (fail . show) pure

check :: String -> Bool -> IO ()
check label condition = unless condition (fail label)

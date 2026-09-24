-- | New-capability receipts for certified curve topology and proximity on the
-- illustration study's own contours. Certification, plain lowering of the
-- same contours, and the certified lowering that runs both and checks the
-- lowered loops are timed separately, so the certificate's cost reads apart
-- from the lowering's.
module Moonlight.Planar.RegionBench (benchmarks) where

import BenchMeasure (requireRight, timedProjection)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (identityAffine2)
import Moonlight.Planar.Curve
  ( ClosedTrail, Located, Subpath (..), closedTrailSteps, locate, locatedValue, openTrail )
import Moonlight.Planar.Curve.Frame (exactTrailSite)
import Moonlight.Planar.Curve.Lowering (LoweringPolicy, loweredSpans, lowerClosedTrail, loweringPolicy)
import Moonlight.Planar.Curve.Measure
  ( MeasurePolicy, distance, lengthEnclosureLower, lengthEnclosureWidth, measurePolicy
  , radicalPrecision, sitePoint )
import Moonlight.Planar.Curve.Proximity
  ( ClearanceVerdict (..), ContactWitness (..), clearance, curveDistance, distanceBounds
  , distanceCandidates )
import Moonlight.Planar.Curve.Region
  ( CurveComponent (..), SubdivisionBudget, certifiedPieceCounts, certifySimpleRegion
  , lowerSimpleRegion, subdivisionBudget )
import Moonlight.Planar.Exact (exactRational, positiveExact, positiveOne, unitZero)
import Moonlight.Planar.Exhibit.IllustrationStudy (StudyPart (..), defaultControls, partContour)
import Moonlight.Planar.Region (planarRegionComponents, polygonHoleLoops)

data RegionFamily
  = BladeFuller
  | KnightMask
  deriving stock (Eq, Show)

benchmarks :: IO ()
benchmarks = do
  putStrLn "region-benchmark: new capability; no baseline or speedup claim"
  putStrLn "region-benchmark: lowering epsilon=1 identity metric, max-depth=20, max-leaves=8192; topology max-depth=12, max-pieces=4096, max-bits=4096"
  putStrLn "region-benchmark: certify = curve certificate alone; lower = plain lowering of the same contours; certified-lowering = both plus the lowered-loop check"
  lowering <- requireRight (loweringPolicy positiveOne identityAffine2 20 8192)
  topology <- requireRight (subdivisionBudget 12 4096 4096)
  traverse_ (benchmarkRegion lowering topology) [BladeFuller, KnightMask]
  putStrLn "proximity-benchmark: tolerance=1/100 absolute; radical-precision=64; max-depth=24, max-leaves=20000, max-bits=4096"
  measuring <- proximityPolicy
  hornBaseClearance measuring
  fullerDistance measuring

benchmarkRegion :: LoweringPolicy -> SubdivisionBudget -> RegionFamily -> IO ()
benchmarkRegion lowering topology family = do
  let label = "region-" <> show family
      components = region family
      contours = concatMap (\(CurveComponent outer holes) -> outer : holes) components
      pieces = map snd . certifiedPieceCounts
  _ <- evaluate (force contours)
  evidence <- timedProjection (label <> "-certify") pieces
    (requireRight (certifySimpleRegion topology components))
  putStrLn (label <> "-certify-receipt: pieces=" <> show (pieces evidence))
  lowered <- timedProjection (label <> "-lower") (fmap (length . loweredSpans))
    (requireRight (traverse (lowerClosedTrail lowering) contours))
  putStrLn (label <> "-lower-receipt: spans=" <> show (length . loweredSpans <$> lowered))
  (planar, paths, _) <- timedProjection (label <> "-certified-lowering")
    (\(planar, paths, certified) ->
        (length . polygonHoleLoops <$> planarRegionComponents planar, length . loweredSpans <$> paths, pieces certified))
    (requireRight (lowerSimpleRegion lowering topology components))
  putStrLn (label <> "-certified-lowering-receipt: holes=" <> show (length . polygonHoleLoops <$> planarRegionComponents planar)
    <> " spans=" <> show (length . loweredSpans <$> paths))

region :: RegionFamily -> [CurveComponent]
region BladeFuller = [CurveComponent (part Blade) [part Fuller]]
region KnightMask = [CurveComponent (part Mask) []]

part :: StudyPart -> Located ClosedTrail
part = partContour defaultControls

proximityPolicy :: IO MeasurePolicy
proximityPolicy = do
  tolerance <- requireRight (exactRational 1 100) >>= requireRight . positiveExact
  precision <- requireRight (radicalPrecision 64)
  measurePolicy tolerance precision <$> requireRight (subdivisionBudget 24 20000 4096)

-- The left horn's base, its closing step, against the mask curve at zero.
hornBaseClearance :: MeasurePolicy -> IO ()
hornBaseClearance measuring = do
  let contour = part LeftHorn
      source = ClosedSubpath contour
      steps = closedTrailSteps (locatedValue contour)
      closing = Seq.length steps - 1
  start <- requireRight (exactTrailSite source closing unitZero)
  base <- maybe (fail "left horn has no closing step") pure (Seq.lookup closing steps)
  zero <- requireRight (distance 0)
  subject <- evaluate (force (OpenSubpath (locate (sitePoint start) (openTrail (Seq.singleton base))), ClosedSubpath (part Mask)))
  verdict <- timedProjection "proximity-HornBase-clearance" id
    (requireRight (uncurry (clearance measuring zero) subject))
  putStrLn ("proximity-HornBase-clearance-receipt: " <> describe verdict)
 where
  describe :: ClearanceVerdict -> String
  describe verdict = case verdict of
    ClearanceHolds enclosure -> "holds lower=" <> show (lengthEnclosureLower enclosure)
    ClearanceViolated (CloserThan _ _) -> "violated closer-than"
    ClearanceViolated (SharedPoint _ _) -> "violated shared-point"
    ClearanceViolated TransversalCrossing {} -> "violated crossing"
    ClearanceUnresolved _ candidates obligation -> "unresolved " <> show obligation <> " candidates=" <> show (Seq.length candidates)

-- The fuller's distance from the blade within the tolerance.
fullerDistance :: MeasurePolicy -> IO ()
fullerDistance measuring = do
  subject <- evaluate (force (ClosedSubpath (part Fuller), ClosedSubpath (part Blade)))
  observation <- timedProjection "proximity-FullerBlade-distance" id
    (requireRight (uncurry (curveDistance measuring) subject))
  let enclosure = distanceBounds observation
  putStrLn ("proximity-FullerBlade-distance-receipt: lower=" <> show (lengthEnclosureLower enclosure)
    <> " width=" <> show (lengthEnclosureWidth enclosure)
    <> " candidates=" <> show (Seq.length (distanceCandidates observation)))

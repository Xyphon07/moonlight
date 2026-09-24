-- | Regular-frame and fraction-run laws against exact oracles: dyadic speeds
-- normalize exactly, joins are compared with each step's own jet, and every
-- refusal is reached by a concrete source.
module Moonlight.Planar.CurveFrameSpec (tests) where

import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (affineColumns, affineIsoMap)
import Moonlight.Planar.Curve
  ( CurveStep, Subpath (..), curveStep, jetStep, line, locate, openTrail, quadratic
  , stepJetFirst )
import Moonlight.Planar.Curve.Authoring (polygonTrail)
import Moonlight.Planar.Curve.Frame
import Moonlight.Planar.Curve.Measure
  ( measurePolicy, measureSubpath, pointAtFraction, radicalPrecision, sampleResidual
  , sampleSite, siteParameter, sitePoint, siteStepIndex, subdivisionBudget )
import Moonlight.Planar.Exact
  ( ExactRational, ExactVector (..), UnitInterval, exactHalf, exactPoint, exactPointCoordinates
  , exactRational, positiveExact, unitHalf, unitInterval, unitIntervalValue, unitOne, unitZero )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = sequence_
  [ testExactSites
  , testRefusals
  , testJoins
  , testNormalization
  , testSampledSites
  , testRuns
  , putStrLn "curve frame: ok"
  ]

policy :: IO FramePolicy
policy = framing 64 40

-- | Precision bits, and the admitted squared-scale deficit @2^-deficit@.
framing :: Int -> Int -> IO FramePolicy
framing bits deficit = do
  precision <- requireRight "precision" (radicalPrecision bits)
  tolerance <- requireRight "tolerance" (positiveExact (exactHalf ^ deficit))
  pure (framePolicy precision tolerance)

open :: [CurveStep] -> Subpath
open = OpenSubpath . locate (exactPoint 1 2) . openTrail . Seq.fromList

exactFrame :: FramePolicy -> Subpath -> Int -> UnitInterval -> IO MeasuredFrame
exactFrame framingPolicy source index parameter = do
  site <- requireRight "exact site" (exactTrailSite source index parameter)
  requireRight "regular frame" (regularFrame framingPolicy (ExactSite site))

frameRefusal :: FramePolicy -> Subpath -> Int -> UnitInterval -> Either FrameError ()
frameRefusal framingPolicy source index parameter =
  exactTrailSite source index parameter >>= fmap (const ()) . regularFrame framingPolicy . ExactSite

columns :: MeasuredFrame -> (ExactVector, ExactVector, ExactVector)
columns = affineColumns . affineIsoMap . measuredFrameIso

-- A speed of four has the dyadic inverse 1/4: the frame is exactly unit.
-- A 3-4-5 tangent normalizes from below within the policy's deficit, and
-- either way the columns are the scaled tangent and its left normal.
testExactSites :: IO ()
testExactSites = do
  framingPolicy <- policy
  upright <- exactFrame framingPolicy (open [curveStep line (ExactVector 0 4)]) 0 unitHalf
  assertEqual "dyadic speed gives an exactly unit frame"
    (ExactVector 0 1, ExactVector (-1) 0, ExactVector 1 4) (columns upright)
  assertEqual "exactly unit squared scale" 1 (measuredFrameScaleSquared upright)
  diagonal <- exactFrame framingPolicy (open [curveStep line (ExactVector 3 4)]) 0 unitHalf
  let (tangent@(ExactVector tx ty), normal, offset) = columns diagonal
      scale = measuredFrameScaleSquared diagonal
  assertEqual "site point is the frame origin" (ExactVector (1 + 3 * exactHalf) 4) offset
  assertEqual "tangent column is parallel to the jet" 0 (cross tangent (ExactVector 3 4))
  assertEqual "normal column is the left normal" (ExactVector (negate ty) tx) normal
  assertEqual "squared scale is the column's squared length" (dot tangent tangent) scale
  assertEqual "squared scale within the deficit, never above one" True
    (scale <= 1 && 1 - scale <= exactHalf ^ (40 :: Int))
  assertEqual "exact site keeps its jet" (ExactVector 3 4) (measuredFrameTangent diagonal)
  site <- requireRight "site" (exactTrailSite (open [curveStep line (ExactVector 3 4)]) 0 unitHalf)
  assertEqual "exact site selection" (0, unitHalf) (siteStepIndex site, siteParameter site)

testRefusals :: IO ()
testRefusals = do
  framingPolicy <- policy
  let single = open [curveStep line (ExactVector 3 4)]
  assertEqual "index beyond the source refused" (Left (SiteStepOutOfRange 1))
    (() <$ exactTrailSite single 1 unitZero)
  assertEqual "negative index refused" (Left (SiteStepOutOfRange (-1)))
    (() <$ exactTrailSite single (-1) unitZero)
  assertEqual "stationary tangent refused" (Left (StationaryFrame 0 unitHalf))
    (frameRefusal framingPolicy (open [curveStep line (ExactVector 0 0)]) 0 unitHalf)
  -- A quadratic whose control coincides with its start is stationary there.
  assertEqual "stationary quadratic start refused" (Left (StationaryFrame 0 unitZero))
    (frameRefusal framingPolicy (open [curveStep (quadratic (ExactVector 0 0)) (ExactVector 4 0)]) 0 unitZero)

-- A join is one point on two steps; the selected side is the step read. A
-- smooth join frames identically from both sides; a corner or a reversal
-- refuses from both; a closed polygon's seam is a join.
testJoins :: IO ()
testJoins = do
  framingPolicy <- policy
  let straight = open [curveStep line (ExactVector 2 0), curveStep line (ExactVector 4 0)]
      corner = open [curveStep line (ExactVector 3 0), curveStep line (ExactVector 0 4)]
      reversal = open [curveStep line (ExactVector 2 0), curveStep line (ExactVector (-4) 0)]
      square = ClosedSubpath (polygonTrail (exactPoint 0 0 :| [exactPoint 4 0, exactPoint 4 4, exactPoint 0 4]))
  before <- exactFrame framingPolicy straight 0 unitOne
  after <- exactFrame framingPolicy straight 1 unitZero
  assertEqual "a smooth join frames alike from either side" (columns before) (columns after)
  assertEqual "the smooth frame is unit along x" (ExactVector 1 0, ExactVector 0 1, ExactVector 3 2) (columns after)
  traverse_ (\(label, source, index, parameter) ->
      assertEqual label (Left (CornerFrame index parameter)) (frameRefusal framingPolicy source index parameter))
    [ ("corner refused before the join", corner, 0, unitOne)
    , ("corner refused after the join", corner, 1, unitZero)
    , ("reversal refused before the join", reversal, 0, unitOne)
    , ("reversal refused after the join", reversal, 1, unitZero)
    , ("closed seam corner refused", square, 0, unitZero)
    , ("closed seam corner refused on the closing step", square, 3, unitOne) ]
  interior <- exactFrame framingPolicy corner 1 unitHalf
  assertEqual "a corner's steps frame away from the join"
    (ExactVector 0 1, ExactVector (-1) 0, ExactVector 4 4) (columns interior)
  site <- requireRight "join site" (exactTrailSite corner 1 unitZero)
  assertEqual "the selected side is the step read" (1, unitZero) (siteStepIndex site, siteParameter site)

-- The inverse speed is enclosed at the policy's precision; a squared scale
-- beneath the admitted deficit is refused rather than returned.
testNormalization :: IO ()
testNormalization = do
  coarse <- framing 1 20
  assertEqual "coarse inverse speed refused" (Left (NormalizationUnresolved 0 unitHalf 0 (exactHalf ^ (20 :: Int))))
    (frameRefusal coarse (open [curveStep line (ExactVector 3 4)]) 0 unitHalf)
  -- With a deficit of one the zero scale would pass the bound, but a zero
  -- frame is singular and is refused all the same.
  permissive <- framing 1 0
  assertEqual "zero scale refused under any deficit" (Left (NormalizationUnresolved 0 unitHalf 0 1))
    (frameRefusal permissive (open [curveStep line (ExactVector 3 4)]) 0 unitHalf)
  fine <- framing 32 20
  accepted <- exactFrame fine (open [curveStep line (ExactVector 3 4)]) 0 unitHalf
  let scale = measuredFrameScaleSquared accepted
  assertEqual "a moderate precision reaches the deficit" True (scale <= 1 && 1 - scale <= exactHalf ^ (20 :: Int))

-- A sampled frame reads the sample's own step at its own parameter.
testSampledSites :: IO ()
testSampledSites = do
  framingPolicy <- policy
  tolerance <- requireRight "tolerance" (positiveExact (exactHalf ^ (10 :: Int)))
  precision <- requireRight "precision" (radicalPrecision 64)
  budget <- requireRight "measure budget" (subdivisionBudget 24 4096 4096)
  let measuring = measurePolicy tolerance precision budget
  let step = curveStep (quadratic (ExactVector 3 8)) (ExactVector 6 0)
  measured <- requireRight "measured" (measureSubpath measuring (open [step]))
  sample <- requireRight "sample" (pointAtFraction unitHalf measured)
  frame <- requireRight "frame" (regularFrame framingPolicy (SampledSite sample))
  let (tangent, _, offset) = columns frame
      jet = stepJetFirst (jetStep (siteParameter (sampleSite sample)) step)
      (px, py) = exactPointCoordinates (sitePoint (sampleSite sample))
  assertEqual "sampled frame reads the step's jet" jet (measuredFrameTangent frame)
  assertEqual "sampled frame points along the jet" True (cross tangent jet == 0 && dot tangent jet > 0)
  assertEqual "sampled frame origin is the sample point" (ExactVector px py) offset
  assertEqual "sampled site keeps its residual" (Just (sampleResidual sample)) (case measuredFrameSite frame of
    SampledSite kept -> Just (sampleResidual kept)
    ExactSite _ -> Nothing)
  assertEqual "single-step sample" 0 (siteStepIndex (sampleSite sample))

testRuns :: IO ()
testRuns = do
  fifth <- exact 1 5 >>= requireRight "fifth" . unitInterval
  fourFifths <- exact 4 5 >>= requireRight "four fifths" . unitInterval
  assertEqual "an empty run refused" (Left (EmptyRun 0)) (() <$ fractionRun 0 fifth fourFifths)
  assertEqual "zero spacing refused" (Left (NonPositiveSpacing fifth fifth))
    (() <$ fractionRun 2 fifth fifth)
  assertEqual "reversed run refused" (Left (NonPositiveSpacing fourFifths fifth))
    (() <$ fractionRun 3 fourFifths fifth)
  single <- requireRight "single" (fractionRun 1 fourFifths fourFifths)
  assertEqual "a single station is its start" [unitIntervalValue fourFifths] (unitIntervalValue <$> toList (runFractions single))
  three <- requireRight "three" (fractionRun 3 fifth fourFifths)
  assertEqual "three evenly spaced stations, ends included" [unitIntervalValue fifth, exactHalf, unitIntervalValue fourFifths]
    (unitIntervalValue <$> toList (runFractions three))
  tolerance <- requireRight "tolerance" (positiveExact (exactHalf ^ (10 :: Int)))
  precision <- requireRight "precision" (radicalPrecision 64)
  budget <- requireRight "measure budget" (subdivisionBudget 24 4096 4096)
  let measuring = measurePolicy tolerance precision budget
  whole <- requireRight "whole" (fractionRun 5 unitZero unitOne)
  square <- requireRight "square" (measureSubpath measuring
    (ClosedSubpath (polygonTrail (exactPoint 0 0 :| [exactPoint 4 0, exactPoint 4 4, exactPoint 0 4]))))
  assertEqual "a closed run holding both ends of the seam refused" (Left RunRepeatsSeam) (() <$ sampleRun whole square)
  segment <- requireRight "segment" (measureSubpath measuring (open [curveStep line (ExactVector 8 0)]))
  samples <- requireRight "open run" (sampleRun whole segment)
  assertEqual "an open run places both ends, in order"
    [exactPoint 1 2, exactPoint 3 2, exactPoint 5 2, exactPoint 7 2, exactPoint 9 2] (sitePoint . sampleSite <$> toList samples)
 where
  exact :: Integer -> Integer -> IO ExactRational
  exact a b = requireRight "exact" (exactRational a b)

cross :: ExactVector -> ExactVector -> ExactRational
cross (ExactVector a b) (ExactVector c d) = a * d - b * c

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector a b) (ExactVector c d) = a * c + b * d

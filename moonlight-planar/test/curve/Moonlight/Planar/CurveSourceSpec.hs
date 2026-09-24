-- | Source-span provenance laws: a selected site is its step's exact point,
-- the one measurement reaches by subdivision; a join's neighbour wraps at a
-- closed seam; unit parameters are subdivided and spaced exactly.
module Moonlight.Planar.CurveSourceSpec (tests) where

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve
  ( Subpath (..), circle, cubic, curveStep, evaluateStep, line, locate, openTrail, quadratic
  , rationalQuadratic )
import Moonlight.Planar.Curve.Measure
  ( JoinSide (..), MeasuredTrail, measurePolicy, measureSubpath, pointAtFraction, subdivisionBudget
  , radicalPrecision, sampleSite )
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), UnitInterval, exactPoint, exactRational, positiveExact
  , positiveTwo, translateExactPoint, unitInterval, unitOne, unitZero )
import Moonlight.Planar.Internal.CurveSource
  ( TrailSite, joinNeighbourJet, selectSite, siteJet, siteJoinSide, siteParameter, sitePoint
  , siteSource, siteStep, siteStepIndex, sourceStepCurve, sourceStepStart, sourceSteps )
import Moonlight.Planar.Internal.ExactRational (unitIntervalRun, unitMidpoint)
import Support (assertEqual, requireJust, requireRight)

tests :: IO ()
tests = sequence_
  [ testSelectedSites
  , testJoinNeighbours
  , testUnitParameters
  , putStrLn "curve source: ok"
  ]

-- A site selected at a measured sample's step and parameter is the sample's
-- site, point and join side included: evaluation and the measurement's exact
-- subdivision agree, which is the point law 'trailSite' leaves to its callers.
testSelectedSites :: IO ()
testSelectedSites = do
  tolerance <- exact 1 1000 >>= requireRight "tolerance" . positiveExact
  precision <- requireRight "precision" (radicalPrecision 128)
  budget <- requireRight "budget" (subdivisionBudget 24 4096 4096)
  let policy = measurePolicy tolerance precision budget
  weight <- exact 3 2 >>= requireRight "weight" . positiveExact
  let mixed = OpenSubpath (locate (exactPoint 2 7) (openTrail (Seq.fromList
        [ curveStep (quadratic (ExactVector 1 3)) (ExactVector 4 1)
        , curveStep (cubic (ExactVector 1 (-2)) (ExactVector 3 2)) (ExactVector 5 0)
        , curveStep (rationalQuadratic (ExactVector 2 2) weight positiveTwo) (ExactVector 3 (-1))
        , curveStep line (ExactVector 0 2)
        ])))
      lap = ClosedSubpath (circle positiveTwo)
  fractions <- traverse (uncurry unit) [(0, 1), (1, 7), (1, 3), (1, 2), (5, 6), (1, 1)]
  traverse_ (\source -> do
    measured <- requireRight "measured source" (measureSubpath policy source)
    traverse_ (sampleAgrees measured) fractions)
    [mixed, lap]

sampleAgrees :: MeasuredTrail -> UnitInterval -> IO ()
sampleAgrees measured fraction = do
  site <- sampleSite <$> requireRight "fraction sample" (pointAtFraction fraction measured)
  selected <- requireJust "selected site"
    (selectSite (siteSource site) (siteStepIndex site) (siteParameter site))
  assertEqual "a selected site is the measured sample's site" site selected
  assertEqual "a measured site is its step's exact point" (evaluatedPoint site) (sitePoint site)

-- The point law 'trailSite' leaves to its callers: a site's point is its
-- step's located start translated by the step's value at its parameter.
evaluatedPoint :: TrailSite -> ExactPoint
evaluatedPoint site =
  translateExactPoint (sourceStepStart step) (evaluateStep (siteParameter site) (sourceStepCurve step))
 where
  step = siteStep site

testJoinNeighbours :: IO ()
testJoinNeighbours = do
  let lap = ClosedSubpath (circle positiveTwo)
      final = Seq.length (sourceSteps lap) - 1
  seamEnd <- requireJust "seam end" (selectSite lap final unitOne)
  seamStart <- requireJust "seam start" (selectSite lap 0 unitZero)
  assertEqual "a closed trail's last step ends before its seam" BeforeJoin (siteJoinSide seamEnd)
  assertEqual "a closed trail's first step starts after its seam" AfterJoin (siteJoinSide seamStart)
  assertEqual "before the seam, the neighbour is the first step's start"
    (Just (siteJet seamStart)) (joinNeighbourJet seamEnd)
  assertEqual "after the seam, the neighbour is the last step's end"
    (Just (siteJet seamEnd)) (joinNeighbourJet seamStart)

  let open = OpenSubpath (locate (exactPoint 2 7) (openTrail (Seq.fromList
        [curveStep line (ExactVector 3 4), curveStep line (ExactVector 6 8)])))
  start <- requireJust "open start" (selectSite open 0 unitZero)
  finish <- requireJust "open end" (selectSite open 1 unitOne)
  beforeJoin <- requireJust "before the join" (selectSite open 0 unitOne)
  afterJoin <- requireJust "after the join" (selectSite open 1 unitZero)
  assertEqual "an open trail's ends have no neighbour"
    (Nothing, Nothing) (joinNeighbourJet start, joinNeighbourJet finish)
  assertEqual "the step after the join starts at the join" (exactPoint 5 11) (sourceStepStart (siteStep afterJoin))
  assertEqual "an open join's neighbours are the adjacent steps"
    (Just (siteJet afterJoin), Just (siteJet beforeJoin))
    (joinNeighbourJet beforeJoin, joinNeighbourJet afterJoin)
  assertEqual "a missing step selects no site"
    (Nothing, Nothing) (selectSite open 2 unitZero, selectSite open (-1) unitZero)

testUnitParameters :: IO ()
testUnitParameters = do
  fifth <- unit 1 5
  fourFifths <- unit 4 5
  half <- unit 1 2
  third <- unit 1 3
  fiveTwelfths <- unit 5 12
  assertEqual "two intervals are three points, both ends included"
    (fifth :| [half, fourFifths]) (unitIntervalRun 2 fifth fourFifths)
  assertEqual "zero intervals are the first point alone" (fifth :| []) (unitIntervalRun 0 fifth fourFifths)
  assertEqual "one interval is its two ends" (fifth :| [fourFifths]) (unitIntervalRun 1 fifth fourFifths)
  assertEqual "a run may descend" (fourFifths :| [half, fifth]) (unitIntervalRun 2 fourFifths fifth)
  assertEqual "the midpoint is exact" fiveTwelfths (unitMidpoint third half)
  assertEqual "the midpoint of the ends is one half" half (unitMidpoint unitZero unitOne)

unit :: Integer -> Integer -> IO UnitInterval
unit n d = exact n d >>= requireRight "unit parameter" . unitInterval

exact :: Integer -> Integer -> IO ExactRational
exact n d = requireRight "exact rational" (exactRational n d)

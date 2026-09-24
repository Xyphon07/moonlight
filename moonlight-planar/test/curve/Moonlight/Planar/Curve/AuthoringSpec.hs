module Moonlight.Planar.Curve.AuthoringSpec (tests) where

import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Planar.Affine
  ( affine2, transformPoint, transformVector )
import Moonlight.Planar.Curve
  ( ClosedTrail, CurveStep, Located, OpenTrail, closedTrailSteps, curveStepEnd
  , endJet, evaluateStep, joinContinuity, JoinContinuity (..), locatedValue
  , location, openTrail, reverseClosedTrail, reverseLocatedTrail, startJet, trailDisplacement
  , trailSteps, transformLocatedClosedTrail, transformLocatedTrail )
import Moonlight.Planar.Curve.Authoring
import Moonlight.Planar.Exact
  ( ExactPoint, ExactVector (..), exactHalf, exactPoint, exactPointCoordinates
  , translateExactPoint, unitHalf, unitOne, unitZero )
import Support (assertEqual)

tests :: IO ()
tests = sequence_
  [ testInterpolation
  , testExplicitCorners
  , testTensionAndReversal
  , testAffineNaturality
  , testProfiles
  , testPolygonAndBow
  , putStrLn "curve-authoring: ok"
  ]

landmarks :: NonEmpty ExactPoint
landmarks = exactPoint 0 0 :| [exactPoint 3 5, exactPoint 8 6, exactPoint 12 0]

testInterpolation :: IO ()
testInterpolation = do
  let knots = Interpolating <$> landmarks
      authored = cardinalOpen unitZero knots
      closed = cardinalClosed unitZero knots
      openSteps = toList (trailSteps (locatedValue authored))
      cycleSteps = toList (closedTrailSteps (locatedValue closed))
  assertEqual "open interpolation visits every landmark" (NonEmpty.toList landmarks) (anchors authored)
  assertEqual "one span per consecutive open knot" 3 (length openSteps)
  assertEqual "closed interpolation visits every landmark and closes"
    (NonEmpty.toList landmarks <> [exactPoint 0 0]) (closedAnchors closed)
  assertEqual "one span per cyclic knot" 4 (length cycleSteps)
  assertEqual "regular interior C1 joins" [ParametricJoin, ParametricJoin]
    (zipWith joinContinuity openSteps (drop 1 openSteps))
  assertEqual "regular periodic C1 joins" (replicate 4 ParametricJoin)
    (zipWith joinContinuity cycleSteps (drop 1 cycleSteps <> take 1 cycleSteps))
  let single = cardinalOpen unitZero (Interpolating (exactPoint 7 9) :| [])
      singleClosed = cardinalClosed unitZero (Interpolating (exactPoint 7 9) :| [])
  assertEqual "singleton open retains location" [exactPoint 7 9] (anchors single)
  assertEqual "singleton closed is constant" [exactPoint 7 9, exactPoint 7 9] (closedAnchors singleClosed)
  assertEqual "repeated points require no divide or admission"
    [exactPoint 2 3, exactPoint 2 3]
    (anchors (cardinalOpen unitZero (Interpolating (exactPoint 2 3) :| [Interpolating (exactPoint 2 3)])))

testExplicitCorners :: IO ()
testExplicitCorners = do
  let incoming = ExactVector 3 0
      outgoing = ExactVector 0 6
      knots = Interpolating (exactPoint 0 0) :|
        [ExplicitJets (exactPoint 2 2) incoming outgoing, Interpolating (exactPoint 4 7)]
      spans = toList (trailSteps (locatedValue (cardinalOpen unitOne knots)))
  assertEqual "explicit corner survives maximum automatic tension" [CornerJoin]
    (zipWith joinContinuity spans (drop 1 spans))
  assertEqual "explicit incoming jet" [incoming] (endJet <$> take 1 spans)
  assertEqual "explicit outgoing jet" [outgoing] (startJet <$> drop 1 spans)
  let returning = cardinalClosed unitZero (ExplicitJets (exactPoint 0 0) incoming outgoing :| [])
  assertEqual "single explicit closed knot retains returning cubic jets"
    [(outgoing,incoming)]
    ((\step -> (startJet step,endJet step)) <$> toList (closedTrailSteps (locatedValue returning)))

testTensionAndReversal :: IO ()
testTensionAndReversal = do
  let knots = Interpolating <$> landmarks
      smooth = toList (trailSteps (locatedValue (cardinalOpen unitZero knots)))
      tense = toList (trailSteps (locatedValue (cardinalOpen unitOne knots)))
      half = toList (trailSteps (locatedValue (cardinalOpen unitHalf knots)))
  assertEqual "maximum tension zeros automatic jets" (replicate 3 (zero,zero))
    ((\step -> (startJet step,endJet step)) <$> tense)
  assertEqual "tension scales derived jets"
    ((\step -> (halfVector (startJet step),halfVector (endJet step))) <$> smooth)
    ((\step -> (startJet step,endJet step)) <$> half)
  assertEqual "open interpolation commutes with reversal"
    (reverseLocatedTrail (cardinalOpen unitHalf knots))
    (cardinalOpen unitHalf (NonEmpty.reverse knots))
  let initial :| remaining = knots
      closed = cardinalClosed unitHalf knots
      reversed = cardinalClosed unitHalf (initial :| reverse remaining)
  assertEqual "closed reversal preserves authored first landmark" (location closed) (location reversed)
  assertEqual "periodic interpolation commutes with anchored reversal"
    (reverseClosedTrail (locatedValue closed)) (locatedValue reversed)

testAffineNaturality :: IO ()
testAffineNaturality = do
  let transforms =
        [ affine2 (ExactVector 2 1) (ExactVector (-3) 4) (ExactVector 7 8)
        , affine2 (ExactVector (-1) 0) (ExactVector 0 1) (ExactVector 2 3)
        , affine2 (ExactVector 1 2) (ExactVector 2 4) zero
        ]
      knots = Interpolating (exactPoint 0 0) :|
        [ExplicitJets (exactPoint 4 6) (ExactVector 3 2) (ExactVector (-2) 5), Interpolating (exactPoint 8 0)]
  traverse_ (\transform -> do
    let mapped = fmap (\knot -> case knot of
          Interpolating point -> Interpolating (transformPoint transform point)
          ExplicitJets point incoming outgoing -> ExplicitJets (transformPoint transform point)
            (transformVector transform incoming) (transformVector transform outgoing)) knots
        mappedStations = fmap (\(ProfileStation center width) ->
          ProfileStation (transformPoint transform center) (transformVector transform width)) stations
    assertEqual "cardinal open affine naturality"
      (transformLocatedTrail transform (cardinalOpen unitHalf knots)) (cardinalOpen unitHalf mapped)
    assertEqual "cardinal closed affine naturality"
      (transformLocatedClosedTrail transform (cardinalClosed unitHalf knots)) (cardinalClosed unitHalf mapped)
    assertEqual "profile affine naturality"
      (transformLocatedClosedTrail transform (profileOutline unitHalf stations))
      (profileOutline unitHalf mappedStations)) transforms

stations :: NonEmpty ProfileStation
stations = ProfileStation (exactPoint 0 0) zero :|
  [ProfileStation (exactPoint 3 6) (ExactVector 4 1), ProfileStation (exactPoint 8 10) zero]

testProfiles :: IO ()
testProfiles = do
  let (positive,negative) = profileRails unitZero stations
      outline = profileOutline unitZero stations
      centerKnots = fmap (\(ProfileStation center _) -> Interpolating center) stations
      centerline = cardinalOpen unitZero centerKnots
      samples = zip3 (locatedSteps positive) (locatedSteps negative) (locatedSteps centerline)
  assertEqual "positive station interpolation"
    [exactPoint 0 0, exactPoint 7 7, exactPoint 8 10] (anchors positive)
  assertEqual "negative station interpolation"
    [exactPoint 0 0, exactPoint (-1) 5, exactPoint 8 10] (anchors negative)
  assertEqual "tapered root rails meet" (location positive) (location negative)
  assertEqual "tapered tip rails meet" (endpoint positive) (endpoint negative)
  assertEqual "profile outline derives closure" zero
    (trailDisplacement (openTrail (closedTrailSteps (locatedValue outline))))
  traverse_ (\(p,n,c) -> assertEqual "rail midpoint is interpolated centerline"
    (twicePoint (sample c)) (sumPoints (sample p) (sample n))) samples
  let reversedStations = NonEmpty.reverse stations
      (reversePositive,reverseNegative) = profileRails unitZero reversedStations
  assertEqual "positive rail reversal" (reverseLocatedTrail positive) reversePositive
  assertEqual "negative rail reversal" (reverseLocatedTrail negative) reverseNegative
 where
  sample :: (ExactPoint,CurveStep) -> ExactPoint
  sample (origin,step) = translateExactPoint origin (evaluateStep unitHalf step)
  sumPoints :: ExactPoint -> ExactPoint -> ExactVector
  sumPoints a b = let (ax,ay) = exactPointCoordinates a; (bx,by) = exactPointCoordinates b
                  in ExactVector (ax+bx) (ay+by)
  twicePoint :: ExactPoint -> ExactVector
  twicePoint point = sumPoints point point

testPolygonAndBow :: IO ()
testPolygonAndBow = do
  let points = exactPoint 1 2 :| [exactPoint 5 2, exactPoint 5 7]
      outline = polygonTrail points
      positiveBow = bowedTrail exactHalf (exactPoint 1 2) (exactPoint 5 2)
      negativeBow = bowedTrail (negate exactHalf) (exactPoint 1 2) (exactPoint 5 2)
  assertEqual "polygon preserves input order and closes"
    [exactPoint 1 2, exactPoint 5 2, exactPoint 5 7, exactPoint 1 2] (closedAnchors outline)
  assertEqual "bow retains endpoints" [exactPoint 1 2,exactPoint 5 2] (anchors positiveBow)
  assertEqual "positive bow exact midpoint sagitta" [exactPoint 3 4] (midpoints positiveBow)
  assertEqual "negative bow exact midpoint sagitta" [exactPoint 3 0] (midpoints negativeBow)
  assertEqual "bow reversal negates signed local bow" (reverseLocatedTrail positiveBow)
    (bowedTrail (negate exactHalf) (exactPoint 5 2) (exactPoint 1 2))
  assertEqual "zero chord remains constant" [exactPoint 2 3]
    (midpoints (bowedTrail 100 (exactPoint 2 3) (exactPoint 2 3)))

anchors :: Located OpenTrail -> [ExactPoint]
anchors value = scanl (\point step -> translateExactPoint point (curveStepEnd step))
  (location value) (toList (trailSteps (locatedValue value)))

closedAnchors :: Located ClosedTrail -> [ExactPoint]
closedAnchors value = scanl (\point step -> translateExactPoint point (curveStepEnd step))
  (location value) (toList (closedTrailSteps (locatedValue value)))

locatedSteps :: Located OpenTrail -> [(ExactPoint,CurveStep)]
locatedSteps value = zip (anchors value) (toList (trailSteps (locatedValue value)))

endpoint :: Located OpenTrail -> ExactPoint
endpoint value = translateExactPoint (location value) (trailDisplacement (locatedValue value))

midpoints :: Located OpenTrail -> [ExactPoint]
midpoints = fmap (\(origin,step) -> translateExactPoint origin (evaluateStep unitHalf step)) . locatedSteps

halfVector :: ExactVector -> ExactVector
halfVector (ExactVector x y) = ExactVector (exactHalf*x) (exactHalf*y)

zero :: ExactVector
zero = ExactVector 0 0

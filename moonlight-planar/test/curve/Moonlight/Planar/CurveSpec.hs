-- | Exact laws of the authored curve algebra, independently of flattening.
module Moonlight.Planar.CurveSpec (tests) where

import Control.Monad (when)
import Data.Foldable (toList, traverse_)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
  ( Affine2, affine2, composeAffine2, identityAffine2, transformPoint, transformVector )
import Moonlight.Planar.Curve
import Moonlight.Planar.Exact
  ( ExactRational, ExactVector (..), ScalarRefinementError (..), UnitInterval
  , addExactVectors, blendPositive, divideByPositive, exactHalf, exactPoint
  , exactPointCoordinates, exactRational, exactThird, positiveExact, positiveExactValue
  , positiveOne, positiveSumSquares, positiveTwo, ratioPositive
  , translateExactPoint, unitHalf, unitInterval, unitIntervalValue, unitOne, unitZero
  )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = sequence_
  [ testRefinements
  , testParameterizedLaws
  , testTrails
  , testCircles
  , testJets
  , testAffineActions
  , testSubdivision
  , testRestriction
  , testStepJets
  , testRationalJetOracles
  , putStrLn "curve: ok"
  ]

testRefinements :: IO ()
testRefinements = do
  assertEqual "zero is not positive" (Left (ExactNotPositive 0)) (positiveExact 0)
  assertEqual "negative is not positive" (Left (ExactNotPositive (-1))) (positiveExact (-1))
  assertEqual "negative parameter refused" (Left (ExactOutsideUnitInterval (-1))) (unitInterval (-1))
  assertEqual "parameter above one refused" (Left (ExactOutsideUnitInterval 2)) (unitInterval 2)
  assertEqual "zero parameter admitted" (Right unitZero) (unitInterval 0)
  assertEqual "one parameter admitted" (Right unitOne) (unitInterval 1)
  assertEqual "positive blend at zero" positiveOne (blendPositive unitZero positiveOne positiveTwo)
  assertEqual "positive blend at one" positiveTwo (blendPositive unitOne positiveOne positiveTwo)
  assertEqual "positive blend at half" (1 + exactHalf)
    (positiveExactValue (blendPositive unitHalf positiveOne positiveTwo))
  assertEqual "positive division" exactHalf (divideByPositive 1 positiveTwo)
  assertEqual "positive ratio" exactHalf (positiveExactValue (ratioPositive positiveOne positiveTwo))
  assertEqual "zero norm" Nothing (positiveSumSquares 0 0)
  assertEqual "nonzero norm has witness" (Just 25) (positiveExactValue <$> positiveSumSquares (-3) 4)
  assertEqual "polynomial quadratic canonicalization"
    (quadratic (ExactVector 2 3))
    (rationalQuadratic (ExactVector 2 3) positiveOne positiveOne)

parameters :: IO [UnitInterval]
parameters = traverse admit [(0,1), (1,7), (1,3), (1,2), (4,5), (1,1)]
 where
  admit (n,d) = requireRight "rational parameter" (exactRational n d)
    >>= requireRight "unit parameter" . unitInterval

steps :: [CurveStep]
steps =
  [ curveStep line (ExactVector 3 (-2))
  , curveStep (quadratic (ExactVector 4 5)) (ExactVector (-2) 3)
  , curveStep (cubic (ExactVector 2 8) (ExactVector (-5) 4)) (ExactVector 4 1)
  , curveStep (rationalQuadratic (ExactVector 0 1) positiveOne positiveTwo) (ExactVector (-1) 1)
  , curveStep (rationalQuadratic (ExactVector 5 (-3)) positiveTwo positiveOne) (ExactVector 2 7)
  , curveStep (cubic (ExactVector 5 4) (ExactVector (-5) 4)) zero
  , curveStep line zero
  , curveStep (rationalQuadratic zero positiveOne positiveTwo) zero
  ]

testParameterizedLaws :: IO ()
testParameterizedLaws = do
  ts <- parameters
  traverse_ (checkStep ts) steps
 where
  checkStep :: [UnitInterval] -> CurveStep -> IO ()
  checkStep ts step = do
    assertEqual "evaluation starts at zero" zero (evaluateStep unitZero step)
    assertEqual "evaluation ends at displacement" (curveStepEnd step) (evaluateStep unitOne step)
    assertEqual "reversal involution" step (reverseStep (reverseStep step))
    let (left, right) = splitStep unitHalf step
    assertEqual "split endpoint" (evaluateStep unitHalf step) (curveStepEnd left)
    assertEqual "split displacement"
      (curveStepEnd step) (addExactVectors (curveStepEnd left) (curveStepEnd right))
    traverse_ (checkParameter step left right) ts
  checkParameter :: CurveStep -> CurveStep -> CurveStep -> UnitInterval -> IO ()
  checkParameter step left right t = do
    let value = unitIntervalValue t
    reversed <- requireRight "reversed parameter" (unitInterval (1 - value))
    firstHalf <- requireRight "first half parameter" (unitInterval (exactHalf * value))
    secondHalf <- requireRight "second half parameter" (unitInterval (exactHalf * (1 + value)))
    assertEqual "reversal parameterization"
      (evaluateStep reversed step)
      (addExactVectors (curveStepEnd step) (evaluateStep t (reverseStep step)))
    assertEqual "left split reconstruction" (evaluateStep firstHalf step) (evaluateStep t left)
    assertEqual "right split reconstruction" (evaluateStep secondHalf step)
      (addExactVectors (curveStepEnd left) (evaluateStep t right))

testTrails :: IO ()
testTrails = do
  let a = openTrail (Seq.fromList steps)
      b = openTrail (Seq.singleton (curveStep line (ExactVector 4 2)))
      c = openTrail (Seq.singleton (curveStep (quadratic (ExactVector 2 3)) zero))
      closed = closeWith (cubic (ExactVector 5 3) (ExactVector 2 1)) a
      stationaryClosed = closeWith (quadratic (ExactVector 3 2)) mempty
      anchored = locate (exactPoint 7 11) a
      p = path (Seq.singleton (OpenSubpath anchored))
      q = path (Seq.singleton (ClosedSubpath (locate (exactPoint 2 3) closed)))
  assertEqual "left trail identity" a (mempty <> a)
  assertEqual "right trail identity" a (a <> mempty)
  assertEqual "trail associativity" ((a <> b) <> c) (a <> (b <> c))
  assertEqual "displacement homomorphism"
    (addExactVectors (trailDisplacement a) (trailDisplacement b)) (trailDisplacement (a <> b))
  assertEqual "trail reversal involution" a (reverseTrail (reverseTrail a))
  assertEqual "located reversal involution" anchored (reverseLocatedTrail (reverseLocatedTrail anchored))
  assertEqual "closed displacement" zero (trailDisplacement (openTrail (closedTrailSteps closed)))
  assertEqual "closed reversal involution" closed (reverseClosedTrail (reverseClosedTrail closed))
  assertEqual "single closing shape reversal involution" stationaryClosed
    (reverseClosedTrail (reverseClosedTrail stationaryClosed))
  assertEqual "path identity" p (mempty <> p <> mempty)
  assertEqual "path associativity" ((p <> q) <> p) (p <> (q <> p))
  assertEqual "path retains separate placements" 2 (Seq.length (pathSubpaths (p <> q)))

testCircles :: IO ()
testCircles = do
  ts <- parameters
  let located = circle positiveTwo
      segments = toList (closedTrailSteps (locatedValue located))
      anchors = scanl (\anchor step -> translateExactPoint anchor (curveStepEnd step))
        (location located) segments
  assertEqual "circle closure" zero
    (trailDisplacement (openTrail (closedTrailSteps (locatedValue located))))
  traverse_ (\(anchor, step) -> traverse_ (checkCircle anchor step) ts) (zip anchors segments)
  assertEqual "collapsed ellipse remains lawful authored geometry" zero
    (trailDisplacement (openTrail (closedTrailSteps (locatedValue (ellipse zero zero)))))
 where
  checkCircle anchor step t =
    let (x,y) = exactPointCoordinates (translateExactPoint anchor (evaluateStep t step))
     in assertEqual "exact circle equation" 4 (x*x + y*y)

testJets :: IO ()
testJets = do
  let start = ExactVector 3 9
      finish = ExactVector (-6) 12
      hermite = hermiteStep (ExactVector 5 7) start finish
      segment = curveStep line (ExactVector 1 0)
  assertEqual "Hermite initial jet" start (startJet hermite)
  assertEqual "Hermite final jet" finish (endJet hermite)
  assertEqual "C1 join" ParametricJoin (joinContinuity segment segment)
  assertEqual "G1 not C1" GeometricJoin
    (joinContinuity segment (curveStep line (ExactVector 2 0)))
  assertEqual "opposite jets are corner" CornerJoin
    (joinContinuity segment (curveStep line (ExactVector (-1) 0)))
  assertEqual "zero jet is not smoothness evidence" StationaryJoin
    (joinContinuity segment (curveStep line zero))

testAffineActions :: IO ()
testAffineActions = do
  ts <- parameters
  let a = affine2 (ExactVector (-2) 1) (ExactVector 3 4) (ExactVector 8 5)
      b = affine2 (ExactVector 1 2) (ExactVector 2 4) (ExactVector (-3) 2)
      translation = affine2 (ExactVector 1 0) (ExactVector 0 1) (ExactVector 8 5)
      anchor = exactPoint 2 7
      open = openTrail (Seq.fromList steps)
      closed = locatedValue (circle positiveOne)
      picturePath = path (Seq.fromList
        [OpenSubpath (locate anchor open), ClosedSubpath (locate anchor closed)])
  assertEqual "path affine identity" picturePath (transformPath identityAffine2 picturePath)
  assertEqual "path affine composition" (transformPath (composeAffine2 a b) picturePath)
    (transformPath a (transformPath b picturePath))
  assertEqual "relative trail ignores translation" open (transformTrail translation open)
  assertEqual "relative closed trail ignores translation" closed (transformClosedTrail translation closed)
  assertEqual "located action translates anchor only"
    (locate (transformPoint translation anchor) open)
    (transformLocatedTrail translation (locate anchor open))
  assertEqual "located closed action translates anchor only"
    (locate (transformPoint translation anchor) closed)
    (transformLocatedClosedTrail translation (locate anchor closed))
  assertEqual "affine trail action preserves concatenation"
    (transformTrail a open <> transformTrail a open)
    (transformTrail a (open <> open))
  assertEqual "closed action retains closure" zero
    (trailDisplacement (openTrail (closedTrailSteps (transformClosedTrail b closed))))
  traverse_ (\step -> do
    let transformed = transformStep a step
    assertEqual "relative step ignores translation" step (transformStep translation step)
    traverse_ (\t -> do
      let (left, right) = splitStep t step
      assertEqual "subdivision commutes with affine action"
        (transformStep a left, transformStep a right)
        (splitStep t transformed)
      assertEqual "evaluation commutes with affine action"
        (transformVector a (evaluateStep t step)) (evaluateStep t transformed)
      assertEqual "point and vector actions agree at every placed sample"
        (transformPoint a (translateExactPoint anchor (evaluateStep t step)))
        (translateExactPoint (transformPoint a anchor) (evaluateStep t transformed))) ts) steps

-- | The shared fixtures plus positive weights at both extremes in both
-- positions, and curves collapsed to a point or onto a segment.
lawSteps :: IO [CurveStep]
lawSteps = do
  large <- requireRight "large weight" (positiveExact 1000)
  let small = ratioPositive positiveOne large
      extreme u v = curveStep (rationalQuadratic (ExactVector 5 (-3)) u v) (ExactVector 2 7)
  pure (steps <>
    [ extreme small large
    , extreme large small
    , extreme small small
    , extreme large large
    , curveStep (quadratic zero) zero
    , curveStep (cubic zero zero) zero
    , curveStep (rationalQuadratic zero large small) zero
    , curveStep (cubic (ExactVector 1 2) (ExactVector 2 4)) (ExactVector 3 6)
    , curveStep (rationalQuadratic (ExactVector 1 1) small large) (ExactVector 3 3)
    ])

testSubdivision :: IO ()
testSubdivision = do
  ts <- parameters
  fixtures <- lawSteps
  traverse_ (\step -> traverse_ (checkSplit ts step) ts) fixtures
 where
  checkSplit :: [UnitInterval] -> CurveStep -> UnitInterval -> IO ()
  checkSplit ts step t = do
    let (left, right) = splitStep t step
        s = unitIntervalValue t
    assertEqual "split point" (evaluateStep t step) (curveStepEnd left)
    assertEqual "split displacement"
      (curveStepEnd step) (addExactVectors (curveStepEnd left) (curveStepEnd right))
    traverse_ (\u -> do
      let w = unitIntervalValue u
      inner <- unit "left source parameter" (s * w)
      outer <- unit "right source parameter" (s + (1 - s) * w)
      let innerJet = jetStep inner step
          outerJet = jetStep outer step
          leftJet = jetStep u left
          rightJet = jetStep u right
      assertEqual "left split parameterization" (evaluateStep inner step) (evaluateStep u left)
      assertEqual "right split parameterization" (evaluateStep outer step)
        (addExactVectors (curveStepEnd left) (evaluateStep u right))
      assertEqual "left first derivative chain factor"
        (scale s (stepJetFirst innerJet)) (stepJetFirst leftJet)
      assertEqual "left second derivative chain factor"
        (scale (s * s) (stepJetSecond innerJet)) (stepJetSecond leftJet)
      assertEqual "right first derivative chain factor"
        (scale (1 - s) (stepJetFirst outerJet)) (stepJetFirst rightJet)
      assertEqual "right second derivative chain factor"
        (scale ((1 - s) * (1 - s)) (stepJetSecond outerJet)) (stepJetSecond rightJet)) ts

testRestriction :: IO ()
testRestriction = do
  ts <- parameters
  fixtures <- lawSteps
  third <- unit "third" exactThird
  full <- requireRight "full span" (parameterSpan unitZero unitOne)
  assertEqual "reversed span refused"
    (Left (ReversedParameterSpan unitHalf third)) (parameterSpan unitHalf third)
  traverse_ (\step -> do
    let located = locate anchor step
    assertEqual "full-span restriction is the located step" located (restrictStep full located)
    traverse_ (\from -> traverse_ (checkSpan ts located from) ts) ts) fixtures
 where
  anchor = exactPoint 2 7
  checkSpan :: [UnitInterval] -> Located CurveStep -> UnitInterval -> UnitInterval -> IO ()
  checkSpan ts located from to
    | from > to = assertEqual "reversed span refused"
        (Left (ReversedParameterSpan from to)) (parameterSpan from to)
    | otherwise = do
        range <- requireRight "ordered span" (parameterSpan from to)
        let restricted = restrictStep range located
            step = locatedValue located
            local = locatedValue restricted
            start = unitIntervalValue from
            width = unitIntervalValue to - start
            place = translateExactPoint (location located)
            placeLocal = translateExactPoint (location restricted)
        assertEqual "span retains its endpoints"
          (from, to) (parameterSpanFrom range, parameterSpanTo range)
        assertEqual "restriction is anchored at the source point"
          (place (evaluateStep from step)) (location restricted)
        when (from == to) $
          assertEqual "equal-endpoint restriction is stationary"
            True (all (== zero) (stepControlPoints local))
        traverse_ (\u -> do
          source <- unit "restricted source parameter" (start + width * unitIntervalValue u)
          let sourceJet = jetStep source step
              localJet = jetStep u local
          assertEqual "restriction parameterization"
            (place (evaluateStep source step)) (placeLocal (evaluateStep u local))
          assertEqual "restriction first derivative chain factor"
            (scale width (stepJetFirst sourceJet)) (stepJetFirst localJet)
          assertEqual "restriction second derivative chain factor"
            (scale (width * width) (stepJetSecond sourceJet)) (stepJetSecond localJet)) ts

testStepJets :: IO ()
testStepJets = do
  ts <- parameters
  fixtures <- lawSteps
  assertEqual "stationary step has a zero jet"
    (zero, zero, zero) (jetParts (jetStep unitHalf (curveStep line zero)))
  traverse_ (\step -> do
    assertEqual "start jet is the jet at zero" (startJet step) (stepJetFirst (jetStep unitZero step))
    assertEqual "end jet is the jet at one" (endJet step) (stepJetFirst (jetStep unitOne step))
    traverse_ (checkJet ts step) ts) fixtures
 where
  placements =
    [ affine2 (ExactVector (-2) 1) (ExactVector 3 4) (ExactVector 8 5)
    , affine2 (ExactVector 1 2) (ExactVector 2 4) (ExactVector (-3) 2)
    ]
  checkJet :: [UnitInterval] -> CurveStep -> UnitInterval -> IO ()
  checkJet ts step u = do
    let jet = jetStep u step
        w = unitIntervalValue u
    mirrored <- unit "mirrored parameter" (1 - w)
    let mirror = jetStep mirrored step
        reversed = jetStep u (reverseStep step)
    assertEqual "jet value is evaluation" (evaluateStep u step) (stepJetValue jet)
    assertEqual "reversed jet value"
      (stepJetValue mirror) (addExactVectors (curveStepEnd step) (stepJetValue reversed))
    assertEqual "reversed first derivative" (scale (-1) (stepJetFirst mirror)) (stepJetFirst reversed)
    assertEqual "reversed second derivative" (stepJetSecond mirror) (stepJetSecond reversed)
    traverse_ (\placement -> assertEqual "jet commutes with the affine linear part"
      (mapJet placement jet) (jetParts (jetStep u (transformStep placement step)))) placements
    -- Independent of the Bernstein difference forms: a polynomial's Taylor
    -- series terminates, so evaluation elsewhere is exactly recovered.
    traverse_ (\third -> traverse_ (\v -> do
      let h = unitIntervalValue v - w
      assertEqual "polynomial Taylor expansion terminates" (evaluateStep v step)
        (addExactVectors (stepJetValue jet)
          (addExactVectors (scale h (stepJetFirst jet))
            (addExactVectors (scale (h * h * exactHalf) (stepJetSecond jet))
              (scale (h * h * h * exactHalf * exactThird) third))))) ts)
      (polynomialThirdDerivative step)

polynomialThirdDerivative :: CurveStep -> Maybe ExactVector
polynomialThirdDerivative step = case shapeView (curveStepShape step) of
  LinearView -> Just zero
  QuadraticView _ -> Just zero
  CubicView a b ->
    Just (scale 6 (addExactVectors (curveStepEnd step) (scale 3 (addExactVectors a (scale (-1) b)))))
  RationalQuadraticView {} -> Nothing

-- | Rational derivatives against oracles that share none of the quotient
-- arithmetic: the circle's constant radius, and the numerator and weight
-- polynomials differentiated independently in the power basis.
testRationalJetOracles :: IO ()
testRationalJetOracles = do
  ts <- parameters
  fixtures <- lawSteps
  let located = circle positiveTwo
      segments = toList (closedTrailSteps (locatedValue located))
      anchors = scanl (\anchor step -> translateExactPoint anchor (curveStepEnd step))
        (location located) segments
  traverse_ (\(anchor, step) -> traverse_ (checkCircle anchor step) ts) (zip anchors segments)
  traverse_ (\step -> traverse_ (checkQuotient step) ts) (fixtures <> segments)
 where
  checkCircle anchor step t = do
    let jet = jetStep t step
        (x, y) = exactPointCoordinates (translateExactPoint anchor (stepJetValue jet))
        radius = ExactVector x y
        first = stepJetFirst jet
    assertEqual "circle velocity is tangent" 0 (dot radius first)
    assertEqual "circle second-order radius law" 0 (dot first first + dot radius (stepJetSecond jet))
  checkQuotient step parameter = case shapeView (curveStepShape step) of
    RationalQuadraticView control u v -> do
      let t = unitIntervalValue parameter
          s = 1 - t
          a = positiveExactValue u
          b = positiveExactValue v
          e = curveStepEnd step
          jet = jetStep parameter step
          value = stepJetValue jet
          first = stepJetFirst jet
          w0 = s * s + 2 * a * t * s + b * t * t
          w1 = 2 * a * (1 - 2 * t) - 2 * s + 2 * b * t
          w2 = 2 - 4 * a + 2 * b
          n0 = addExactVectors (scale (2 * a * t * s) control) (scale (b * t * t) e)
          n1 = addExactVectors (scale (2 * a * (1 - 2 * t)) control) (scale (2 * b * t) e)
          n2 = addExactVectors (scale (-4 * a) control) (scale (2 * b) e)
      assertEqual "rational numerator is weight times value" n0 (scale w0 value)
      assertEqual "rational first quotient rule" n1
        (addExactVectors (scale w1 value) (scale w0 first))
      assertEqual "rational second quotient rule" n2
        (addExactVectors (scale w2 value)
          (addExactVectors (scale (2 * w1) first) (scale w0 (stepJetSecond jet))))
    _ -> pure ()

unit :: String -> ExactRational -> IO UnitInterval
unit label = requireRight label . unitInterval

scale :: ExactRational -> ExactVector -> ExactVector
scale s (ExactVector x y) = ExactVector (s * x) (s * y)

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector ax ay) (ExactVector bx by) = ax * bx + ay * by

jetParts :: StepJet -> (ExactVector, ExactVector, ExactVector)
jetParts jet = (stepJetValue jet, stepJetFirst jet, stepJetSecond jet)

mapJet :: Affine2 -> StepJet -> (ExactVector, ExactVector, ExactVector)
mapJet placement jet =
  let (value, first, second) = jetParts jet
   in (transformVector placement value, transformVector placement first, transformVector placement second)

zero :: ExactVector
zero = ExactVector 0 0

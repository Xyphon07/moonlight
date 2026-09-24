-- | Exact laws of the authored curve algebra, independently of flattening.
module Moonlight.Planar.CurveSpec (tests) where

import Data.Foldable (toList, traverse_)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine
  ( affine2, composeAffine2, identityAffine2, transformPoint, transformVector )
import Moonlight.Planar.Curve
import Moonlight.Planar.Exact
  ( ExactVector (..), ScalarRefinementError (..), UnitInterval
  , addExactVectors, blendPositive, divideByPositive, exactHalf, exactPoint
  , exactPointCoordinates, exactRational, positiveExact, positiveExactValue
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
    let (left, right) = splitStepHalf step
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
        (left,right) = splitStepHalf step
    assertEqual "relative step ignores translation" step (transformStep translation step)
    assertEqual "subdivision commutes with affine action"
      (transformStep a left, transformStep a right)
      (splitStepHalf transformed)
    traverse_ (\t -> do
      assertEqual "evaluation commutes with affine action"
        (transformVector a (evaluateStep t step)) (evaluateStep t transformed)
      assertEqual "point and vector actions agree at every placed sample"
        (transformPoint a (translateExactPoint anchor (evaluateStep t step)))
        (translateExactPoint (transformPoint a anchor) (evaluateStep t transformed))) ts) steps

zero :: ExactVector
zero = ExactVector 0 0

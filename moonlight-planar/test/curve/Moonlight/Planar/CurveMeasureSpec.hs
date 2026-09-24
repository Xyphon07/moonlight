-- | Certified arc-length laws against oracles independent of the enclosure
-- kernel: exact integer lengths, exact squaring of irrational ones, a Machin
-- bracket of pi, and a closed-form cusp length. Sampling is never the oracle.
module Moonlight.Planar.CurveMeasureSpec (tests) where

import Data.Foldable (toList, traverse_)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve
  ( CurveStep, Located, OpenTrail, Subpath (..), circle, cubic, curveStep, line
  , locate, locatedValue, location, openTrail, quadratic, rationalQuadratic
  , reverseClosedTrail, reverseLocatedTrail )
import Moonlight.Planar.Curve.Measure
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), exactHalf, exactPoint
  , exactPointCoordinates, exactRational, exactRationalDenominator, exactRationalNumerator
  , positiveExact, positiveOne, positiveTwo, ratioPositive, translateExactPoint, unitHalf
  , unitInterval, unitOne, unitZero )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = sequence_
  [ testPolicies
  , testStraightTrails
  , testStationaryTrails
  , testCircle
  , testCusp
  , testExtremeWeights
  , testBudgets
  , testRanges
  , testBisectionBudget
  , testQueryBudget
  , testSites
  , putStrLn "curve measure: ok"
  ]

testPolicies :: IO ()
testPolicies = do
  tolerance <- exact 1 100 >>= requireRight "tolerance" . positiveExact
  precision <- requireRight "precision" (radicalPrecision 128)
  assertEqual "negative depth refused" (Left (InvalidBudgetDepth (-1))) (() <$ subdivisionBudget (-1) 8 64)
  assertEqual "empty leaf budget refused" (Left (InvalidBudgetLeaves 0)) (() <$ subdivisionBudget 4 0 64)
  assertEqual "empty bit budget refused" (Left (InvalidBudgetBits 0)) (() <$ subdivisionBudget 4 8 0)
  budget <- requireRight "budget" (subdivisionBudget 4 8 64)
  let policy = measurePolicy tolerance precision budget
  assertEqual "policy reads back its parts" (tolerance, precision, budget)
    (measureTolerance policy, measurePrecision policy, measureBudget policy)
  assertEqual "negative distance refused" (Left (NegativeDistance (-1))) (() <$ distance (-1))
  assertEqual "zero distance admitted" (Right 0) (distanceValue <$> distance 0)

testStraightTrails :: IO ()
testStraightTrails = do
  policy <- measuring 1000 24 4096 128 4096
  pythagorean <- measureOpen policy [curveStep line (ExactVector 3 4)]
  assertEqual "a 3-4-5 line has exact length five" (5, 5) (endpoints (lengthBounds pythagorean))

  -- sqrt (1/9 + 1/4) = sqrt 13 / 6, checked by exact squaring.
  third <- exact 1 3
  half <- exact 1 2
  thirteenThirtySixths <- exact 13 36
  irrational <- measureOpen policy [curveStep line (ExactVector third half)]
  let bounds = lengthBounds irrational
  assertEqual "irrational line length is enclosed" True
    (encloses thirteenThirtySixths (lengthEnclosureLower bounds) (lengthEnclosureUpper bounds))
  assertEqual "irrational line width within tolerance" True
    (lengthEnclosureWidth bounds <= toleranceOf 1000)

  let segments = [ExactVector 3 4, ExactVector 6 8]
  unequal <- measureOpen policy (map (curveStep line) segments)
  assertEqual "unequal segments sum exactly" (15, 15) (endpoints (lengthBounds unequal))
  let corner = [ExactVector 4 0, ExactVector 0 3]
  cornered <- measureOpen policy (map (curveStep line) corner)
  assertEqual "a corner sums its legs" (7, 7) (endpoints (lengthBounds cornered))

  start <- sampleAt 0 unequal
  assertEqual "distance zero is the anchor" anchor (sitePoint (sampleSite start))
  checkLinearResidual "distance zero" segments [5, 10] 0 start
  finish <- sampleAt 15 unequal
  assertEqual "the total distance is the end" (exactPoint 11 19) (sitePoint (sampleSite finish))
  assertEqual "the end is exact" 0 (sampleResidual finish)
  middle <- sampleAt 10 unequal
  assertEqual "mid-span sample lies in the second step" 1 (siteStepIndex (sampleSite middle))
  assertEqual "mid-span sample parameter" unitHalf (siteParameter (sampleSite middle))
  assertEqual "mid-span sample point" (exactPoint 8 15) (sitePoint (sampleSite middle))
  checkLinearResidual "mid-span" segments [5, 10] 10 middle
  offCentre <- exact 37 7
  oblique <- sampleAt offCentre unequal
  checkLinearResidual "off-centre" segments [5, 10] offCentre oblique
  atCorner <- sampleAt 4 cornered
  assertEqual "a corner distance is the corner" (exactPoint 6 7) (sitePoint (sampleSite atCorner))
  checkLinearResidual "corner" corner [4, 3] 4 atCorner
  total <- requireRight "total" (distance 16)
  assertEqual "a distance beyond the total is refused"
    (Left (DistanceBeyondTrail 16 (lengthBounds unequal))) (() <$ pointAtLength total unequal)

  reversed <- requireRight "reversed" (measureSubpath policy
    (OpenSubpath (reverseLocatedTrail (locate anchor (openTrail (Seq.fromList (map (curveStep line) corner)))))))
  assertEqual "reversal keeps the length bounds" (lengthBounds cornered) (lengthBounds reversed)

testStationaryTrails :: IO ()
testStationaryTrails = do
  policy <- measuring 1000 24 4096 128 4096
  stationary <- measureOpen policy
    [curveStep line zero, curveStep (cubic zero zero) zero, curveStep (quadratic zero) zero]
  assertEqual "a stationary trail has zero length" (0, 0) (endpoints (lengthBounds stationary))
  still <- sampleAt 0 stationary
  assertEqual "a stationary trail samples its anchor" (anchor, 0) (sitePoint (sampleSite still), sampleResidual still)
  beyond <- requireRight "beyond" (distance 1)
  assertEqual "a stationary trail refuses positive distance"
    (Left (DistanceBeyondTrail 1 (lengthBounds stationary))) (() <$ pointAtLength beyond stationary)
  halfway <- requireRight "halfway" (pointAtFraction unitHalf stationary)
  assertEqual "a stationary fraction is the anchor" anchor (sitePoint (sampleSite halfway))

  padded <- measureOpen policy
    [curveStep line zero, curveStep line (ExactVector 3 4), curveStep line zero]
  assertEqual "stationary spans add nothing" (5, 5) (endpoints (lengthBounds padded))
  arrival <- sampleAt 5 padded
  assertEqual "the full distance reaches the displaced point" (exactPoint 5 11) (sitePoint (sampleSite arrival))

  empty <- requireRight "empty" (measureSubpath policy (OpenSubpath (locate anchor (mempty :: OpenTrail))))
  assertEqual "an empty trail has zero length" (0, 0) (endpoints (lengthBounds empty))
  origin <- requireRight "origin" (distance 0)
  assertEqual "an empty trail has no source span to sample"
    (Left EmptyTrailSample) (() <$ pointAtLength origin empty)

testCircle :: IO ()
testCircle = do
  let (piLow, piHigh) = piBracket
      source = ClosedSubpath (circle positiveTwo)
      west = exactPoint (-2) 0
  fine <- measuring 1000 32 8192 128 8192
  coarse <- measuring 100 32 8192 128 8192
  measured <- requireRight "circle" (measureSubpath fine source)
  rough <- requireRight "coarse circle" (measureSubpath coarse source)
  let bounds = lengthBounds measured
  assertEqual "the measured trail retains its source" source (measuredSource measured)
  assertEqual "circumference 4 pi is enclosed" True
    (rational (lengthEnclosureLower bounds) <= 4 * piLow && 4 * piHigh <= rational (lengthEnclosureUpper bounds))
  assertEqual "circle width within tolerance" True (lengthEnclosureWidth bounds <= toleranceOf 1000)
  assertEqual "summed enclosures stay on the precision's dyadic grid" True
    (all (\value -> (2 ^ (128 :: Int)) `mod` exactRationalDenominator value == 0)
      [lengthEnclosureLower bounds, lengthEnclosureUpper bounds])
  assertEqual "a tighter tolerance nests its bounds" True
    (lengthEnclosureLower (lengthBounds rough) <= lengthEnclosureLower bounds
      && lengthEnclosureUpper bounds <= lengthEnclosureUpper (lengthBounds rough))
  assertEqual "spans are ordered cumulative enclosures" True
    (and (zipWith (\before after -> lengthEnclosureUpper (measuredSpanPrefix before) <= lengthEnclosureUpper (measuredSpanPrefix after))
      (toList (measuredSpans measured)) (drop 1 (toList (measuredSpans measured)))))

  reversed <- requireRight "reversed circle"
    (measureSubpath fine (ClosedSubpath (locate (location (circle positiveTwo))
      (reverseClosedTrail (locatedValue (circle positiveTwo))))))
  assertEqual "reversal keeps the circle's bounds" bounds (lengthBounds reversed)

  -- The half circumference 2 pi ends at (-2, 0); a chord is no longer than
  -- the arc it spans, so the sample's chord to (-2, 0) is bounded by its
  -- residual plus the request's distance from 2 pi.
  halfCircumference <- fromRatio (2 * piLow)
  opposite <- sampleAt halfCircumference measured
  assertEqual "a length sample lies on the circle" 4 (squaredNorm (sitePoint (sampleSite opposite)))
  assertEqual "a length sample is within its residual of the half circumference" True
    (squaredDistance west (sitePoint (sampleSite opposite))
      <= square (rational (sampleResidual opposite) + 2 * (piHigh - piLow)))
  assertEqual "a length residual is within tolerance" True (sampleResidual opposite <= toleranceOf 1000)
  midway <- requireRight "half fraction" (pointAtFraction unitHalf measured)
  assertEqual "a fraction sample lies on the circle" 4 (squaredNorm (sitePoint (sampleSite midway)))
  assertEqual "the half fraction is within its residual of the opposite point" True
    (squaredDistance west (sitePoint (sampleSite midway)) <= square (rational (sampleResidual midway)))
  seam <- requireRight "seam" (pointAtFraction unitZero measured)
  assertEqual "fraction zero is the seam" (exactPoint 2 0) (sitePoint (sampleSite seam))
  lap <- requireRight "lap" (pointAtFraction unitOne measured)
  assertEqual "fraction one closes the lap within its residual" True
    (squaredDistance (exactPoint 2 0) (sitePoint (sampleSite lap)) <= square (rational (sampleResidual lap)))
  quarter <- exact 1 4 >>= requireRight "quarter fraction" . unitInterval
  quarterSample <- requireRight "quarter sample" (pointAtFraction quarter measured)
  assertEqual "the quarter fraction is within its residual of the north point" True
    (squaredDistance (exactPoint 0 2) (sitePoint (sampleSite quarterSample)) <= square (rational (sampleResidual quarterSample)))

testCusp :: IO ()
testCusp = do
  -- x' = 3(1-2t)^2 and y' = 3(1-2t): a cusp at t = 1/2, of length 2 sqrt 2 - 1.
  policy <- measuring 1000 32 8192 128 8192
  measured <- measureOpen policy [curveStep (cubic (ExactVector 1 1) (ExactVector 0 1)) (ExactVector 1 0)]
  let bounds = lengthBounds measured
      shifted value = rational value + 1
  assertEqual "the cusp length 2 sqrt 2 - 1 is enclosed" True
    (square (shifted (lengthEnclosureLower bounds)) <= 8 && 8 <= square (shifted (lengthEnclosureUpper bounds)))
  assertEqual "cusp width within tolerance" True (lengthEnclosureWidth bounds <= toleranceOf 1000)
  cusp <- sampleAt (lengthEnclosureLower bounds * exactHalf) measured
  assertEqual "a cusp sample residual is within tolerance" True (sampleResidual cusp <= toleranceOf 1000)

  -- x = 3t(1-t)(1-2t) on the axis turns at t = (3 -+ sqrt 3)/6, where x is
  -- +-sqrt 3 / 6, so the arc is 2 sqrt 3 / 3 and its square is 4/3. A span
  -- straddling an irrational turning point never meets a purely relative
  -- allowance; the parameter half of the budget accepts it.
  reversing <- measureOpen policy [curveStep (cubic (ExactVector 1 0) (ExactVector (-1) 0)) zero]
  let turning = lengthBounds reversing
  fourThirds <- exact 4 3
  assertEqual "a reversal at irrational parameters is enclosed" True
    (encloses fourThirds (lengthEnclosureLower turning) (lengthEnclosureUpper turning))
  assertEqual "reversal width within tolerance" True (lengthEnclosureWidth turning <= toleranceOf 1000)
  midway <- sampleAt (lengthEnclosureLower turning * exactHalf) reversing
  assertEqual "a reversal sample residual is within tolerance" True (sampleResidual midway <= toleranceOf 1000)

testExtremeWeights :: IO ()
testExtremeWeights = do
  policy <- measuring 1000 64 16384 128 16384
  large <- exact 1000 1 >>= requireRight "large weight" . positiveExact
  huge <- exact 1000000 1 >>= requireRight "huge weight" . positiveExact
  let small = ratioPositive positiveOne large
      control = ExactVector 5 (-3)
      end = ExactVector 2 7
      conic u v = curveStep (rationalQuadratic control u v) end
  traverse_ (\(label, step) -> do
    measured <- measureOpen policy [step]
    reversed <- requireRight label (measureSubpath policy
      (OpenSubpath (reverseLocatedTrail (locate anchor (openTrail (Seq.singleton step))))))
    let bounds = lengthBounds measured
    assertEqual (label <> " width within tolerance") True (lengthEnclosureWidth bounds <= toleranceOf 1000)
    assertEqual (label <> " reversal keeps the bounds") bounds (lengthBounds reversed))
    [ ("weights 1/1000 then 1000", conic small large)
    , ("weights 1000 then 1/1000", conic large small)
    , ("weights 1000 then 10^6", conic large huge)
    ]
  -- Weights (1, k, k^2) reparameterize the polynomial quadratic: one arc,
  -- so the two independently certified enclosures must intersect.
  polynomial <- measureOpen policy [curveStep (quadratic control) end]
  reparameterized <- measureOpen policy [conic large huge]
  let a = lengthBounds polynomial
      b = lengthBounds reparameterized
  assertEqual "a reparameterized conic has the polynomial's length" True
    (max (lengthEnclosureLower a) (lengthEnclosureLower b) <= min (lengthEnclosureUpper a) (lengthEnclosureUpper b))

testBudgets :: IO ()
testBudgets = do
  let source = ClosedSubpath (circle positiveTwo)
  leafy <- measuring 1000 32 4 128 8192
  assertEqual "leaf budget exhaustion refuses" (Just (MeasureBudgetExhausted LeavesExhausted))
    (obligation (measureSubpath leafy source))
  shallow <- measuring 1000 1 8192 128 8192
  assertEqual "depth budget exhaustion refuses" (Just (MeasureBudgetExhausted DepthExhausted))
    (obligation (measureSubpath shallow source))
  narrow <- measuring 1000 32 8192 128 8
  assertEqual "bit budget exhaustion refuses" True
    (case obligation (measureSubpath narrow source) of
      Just (MeasureBudgetExhausted (BitsExhausted width)) -> width > 8
      _ -> False)
  -- At one bit, sqrt 2 is enclosed by [1, 3/2]: the chord and polygon widths
  -- of 1/2 each exceed the span's allowance of (1/200)(1/(3/2) + 1) = 1/120,
  -- while a zero-width comparison of the identical chord and polygon would pass.
  coarse <- measuring 100 32 8192 1 8192
  share <- exact 1 120
  assertEqual "rounding beyond the allowance refuses as precision"
    (Left (SpanRefused 0 unitZero unitOne (PrecisionExhausted 1 share)))
    (() <$ measureSubpath coarse (OpenSubpath (locate anchor (openTrail (Seq.singleton (curveStep line (ExactVector 1 1)))))))

  -- Tiny coordinates do not admit an oversized weight: the source is refused
  -- at the offending step before any length is observed.
  huge <- requireRight "huge weight" (positiveExact (2 ^ (400 :: Int)))
  half <- exact 1 2
  small <- measuring 1000 32 8192 128 64
  let tiny = ExactVector half half
      weighted = OpenSubpath (located
        [curveStep line tiny, curveStep (rationalQuadratic tiny huge positiveOne) tiny])
  assertEqual "an oversized weight refuses before measurement" True
    (case measureSubpath small weighted of
      Left (SpanRefused 1 from to (MeasureBudgetExhausted (BitsExhausted width))) -> (from, to) == (unitZero, unitOne) && width > 64
      _ -> False)
  -- The same budget admits the line at 128 bits of radical precision but not
  -- at 4096, whose dyadic enclosure endpoints alone exceed it.
  let diagonal = OpenSubpath (located [curveStep line (ExactVector 1 1)])
  modest <- measuring 1000 32 8192 128 512
  _ <- requireRight "modest precision" (measureSubpath modest diagonal)
  lavish <- measuring 1000 32 8192 4096 512
  assertEqual "precision beyond the bit budget refuses" True
    (case measureSubpath lavish diagonal of
      Left (SpanRefused 0 from to (MeasureBudgetExhausted (BitsExhausted width))) -> (from, to) == (unitZero, unitOne) && width > 512
      _ -> False)

-- A two-bit budget admits the unit line, the request 1/2 and the halves, but
-- not the quarter parameters; bisecting toward 1/2 refuses the child
-- [1/2, 3/4] before observing it rather than answering with unadmitted bits.
testBisectionBudget :: IO ()
testBisectionBudget = do
  policy <- measuring 4 32 8192 128 2
  measured <- requireRight "unit line" (measureSubpath policy
    (OpenSubpath (locate (exactPoint 0 0) (openTrail (Seq.singleton (curveStep line (ExactVector 1 0)))))))
  half <- exact 1 2
  threeQuarters <- exact 3 4 >>= requireRight "three quarters" . unitInterval
  target <- requireRight "distance" (distance half)
  assertEqual "a bisection child beyond the bit budget refuses"
    (Left (SpanRefused 0 unitHalf threeQuarters (MeasureBudgetExhausted (BitsExhausted 3)))) (() <$ pointAtLength target measured)

-- An exact unit root stays small at 128 bits of precision, so a 16-bit
-- budget admits the line; a request whose own denominator exceeds that
-- budget is refused before any span is consulted.
testQueryBudget :: IO ()
testQueryBudget = do
  policy <- measuring 4 32 8192 128 16
  measured <- requireRight "unit line" (measureSubpath policy
    (OpenSubpath (locate (exactPoint 0 0) (openTrail (Seq.singleton (curveStep line (ExactVector 1 0)))))))
  assertEqual "an exact root at high precision is admitted under a small budget"
    (1, 1) (endpoints (lengthBounds measured))
  target <- exact 1 (2 ^ (1000 :: Int)) >>= requireRight "distance" . distance
  assertEqual "a request beyond the bit budget is refused at admission"
    (Left (RequestRefused (MeasureBudgetExhausted (BitsExhausted 1001)))) (() <$ pointAtLength target measured)

-- The irrational diagonal's rational upper bound lies strictly beyond sqrt 2,
-- so requesting it is undecided, never answered by the trail's end; its lower
-- bound is answered. A fraction reserves its share of the width first.
testRanges :: IO ()
testRanges = do
  policy <- measuring 1000 32 8192 128 4096
  let diagonal = OpenSubpath (located [curveStep line (ExactVector 1 1)])
  measured <- requireRight "diagonal" (measureSubpath policy diagonal)
  let bounds = lengthBounds measured
  assertEqual "the diagonal's enclosure is not exact" True
    (lengthEnclosureLower bounds < lengthEnclosureUpper bounds)
  upper <- requireRight "upper distance" (distance (lengthEnclosureUpper bounds))
  assertEqual "the upper endpoint of an inexact length is unresolved"
    (Left (DistanceUnresolved (lengthEnclosureUpper bounds) bounds)) (() <$ pointAtLength upper measured)
  lower <- sampleAt (lengthEnclosureLower bounds) measured
  assertEqual "the lower endpoint is answered within tolerance" True (sampleResidual lower <= toleranceOf 1000)

  -- Tolerance one at one bit: the enclosure [1, 3/2] leaves a quarter of
  -- uncertainty at the half fraction, reserved before the inverse runs.
  loose <- measuring 1 32 8192 1 4096
  coarse <- requireRight "coarse diagonal" (measureSubpath loose diagonal)
  threeHalves <- exact 3 2
  assertEqual "one bit encloses the diagonal in [1, 3/2]" (1, threeHalves) (endpoints (lengthBounds coarse))
  half <- requireRight "half fraction" (pointAtFraction unitHalf coarse)
  threeQuarters <- exact 3 4
  assertEqual "the inverse runs at 3/4 and the reserve of 1/4 widens it"
    (threeQuarters, unitZero, unitHalf) (sampleResidual half, sampleParameterFrom half, sampleParameterTo half)
  assertEqual "the reserved fraction residual is within tolerance" True (sampleResidual half <= 1)

-- A sample retains its source and names the step it lies on, that step's
-- located start, and its side of any join.
testSites :: IO ()
testSites = do
  policy <- measuring 1000 24 4096 128 4096
  let source = OpenSubpath (located (map (curveStep line) [ExactVector 3 4, ExactVector 6 8]))
  measured <- requireRight "unequal lines" (measureSubpath policy source)
  join <- sampleAt 5 measured
  assertEqual "a sample retains its source" source (siteSource (sampleSite join))
  traverse_ (\value -> sampleAt value measured >>= \sample ->
    assertEqual "every sample's source is its trail's" (measuredSource measured) (siteSource (sampleSite sample)))
    [0, 5, 10, 15]
  assertEqual "the join distance lies after the join"
    (1, AfterJoin, exactPoint 5 11)
    (siteStepIndex (sampleSite join), siteJoinSide (sampleSite join), sitePoint (sampleSite join))
  start <- sampleAt 0 measured
  assertEqual "an open trail's start is away from any join" AwayFromJoin (siteJoinSide (sampleSite start))
  finish <- sampleAt 15 measured
  assertEqual "an open trail's end is away from any join"
    (1, unitOne, AwayFromJoin)
    (siteStepIndex (sampleSite finish), siteParameter (sampleSite finish), siteJoinSide (sampleSite finish))
  lap <- requireRight "circle" (measureSubpath policy (ClosedSubpath (circle positiveTwo)))
  seam <- requireRight "seam" (pointAtFraction unitZero lap)
  assertEqual "a closed trail's seam is a join" (0, AfterJoin) (siteStepIndex (sampleSite seam), siteJoinSide (sampleSite seam))

-- Straight-trail residual law: the arc distance of a point on step @i@ is the
-- rational length before it plus the exact distance from that step's start.
checkLinearResidual :: String -> [ExactVector] -> [ExactRational] -> ExactRational -> ArcSample -> IO ()
checkLinearResidual label segments lengths target sample =
  let index = siteStepIndex (sampleSite sample)
      before = sum (take index lengths)
      stepStart = foldl translateExactPoint anchor (take index segments)
      reach = squaredDistance stepStart (sitePoint (sampleSite sample))
      residual = rational (sampleResidual sample)
      low = rational (target - before) - residual
      high = rational (target - before) + residual
   in do
        assertEqual (label <> " residual within tolerance") True (sampleResidual sample <= toleranceOf 1000)
        assertEqual (label <> " arc distance within residual") True
          ((low <= 0 || square low <= reach) && high >= 0 && reach <= square high)

-- Machin's formula over alternating arctangent series: consecutive partial
-- sums of a series with decreasing terms bracket its limit.
piBracket :: (Rational, Rational)
piBracket = (16 * low5 - 4 * high239, 16 * high5 - 4 * low239)
 where
  (low5, high5) = arctangent (1 % 5)
  (low239, high239) = arctangent (1 % 239)
  arctangent :: Rational -> (Rational, Rational)
  arctangent x =
    let partial n = sum [(-1) ^ k * x ^ (2 * k + 1) / fromInteger (2 * k + 1) | k <- [0 .. n - 1 :: Integer]]
     in (min (partial 12) (partial 13), max (partial 12) (partial 13))

measuring :: Integer -> Int -> Int -> Int -> Int -> IO MeasurePolicy
measuring reciprocal depth leaves bits width = do
  tolerance <- exact 1 reciprocal >>= requireRight "tolerance" . positiveExact
  precision <- requireRight "precision" (radicalPrecision bits)
  budget <- requireRight "measure budget" (subdivisionBudget depth leaves width)
  pure (measurePolicy tolerance precision budget)

measureOpen :: MeasurePolicy -> [CurveStep] -> IO MeasuredTrail
measureOpen policy stepsValue = requireRight "measured trail"
  (measureSubpath policy (OpenSubpath (located stepsValue)))

located :: [CurveStep] -> Located OpenTrail
located = locate anchor . openTrail . Seq.fromList

sampleAt :: ExactRational -> MeasuredTrail -> IO ArcSample
sampleAt value trail = requireRight "distance" (distance value)
  >>= \target -> requireRight "length sample" (pointAtLength target trail)

obligation :: Either MeasureError a -> Maybe MeasureObligation
obligation (Left (SpanRefused _ _ _ refused)) = Just refused
obligation _ = Nothing

endpoints :: LengthEnclosure -> (ExactRational, ExactRational)
endpoints bounds = (lengthEnclosureLower bounds, lengthEnclosureUpper bounds)

encloses :: ExactRational -> ExactRational -> ExactRational -> Bool
encloses squared lower upper = lower <= upper && lower * lower <= squared && squared <= upper * upper

anchor :: ExactPoint
anchor = exactPoint 2 7

zero :: ExactVector
zero = ExactVector 0 0

exact :: Integer -> Integer -> IO ExactRational
exact n d = requireRight "exact rational" (exactRational n d)

fromRatio :: Rational -> IO ExactRational
fromRatio value = exact (numerator value) (denominator value)

toleranceOf :: Integer -> ExactRational
toleranceOf reciprocal = either (const 0) id (exactRational 1 reciprocal)

rational :: ExactRational -> Rational
rational value = exactRationalNumerator value % exactRationalDenominator value

square :: Rational -> Rational
square value = value * value

squaredNorm :: ExactPoint -> ExactRational
squaredNorm point = let (x, y) = exactPointCoordinates point in x * x + y * y

squaredDistance :: ExactPoint -> ExactPoint -> Rational
squaredDistance a b =
  let (ax, ay) = exactPointCoordinates a
      (bx, by) = exactPointCoordinates b
   in square (rational (bx - ax)) + square (rational (by - ay))

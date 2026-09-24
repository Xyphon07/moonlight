-- | Curve proximity laws against closed-form distances: a decided distance
-- encloses the true one within the tolerance, a violated clearance carries
-- sites on both curves or a crossing certificate, and contact the search
-- cannot reach stays unresolved with the budget it spent. Distance and
-- contact are separate obligations: a tangency's distance is decided while
-- its contact is not.
module Moonlight.Planar.CurveProximitySpec (tests) where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import qualified Data.Sequence as Seq
import Moonlight.Planar.Curve
  ( CurveStep, Subpath (..), circle, curveStep, line, locate, openTrail, quadratic )
import Moonlight.Planar.Curve.Measure
  ( BudgetObligation (..), Distance, LengthEnclosure, MeasurePolicy, TrailSite, distance
  , lengthEnclosureLower, lengthEnclosureUpper, lengthEnclosureWidth, measurePolicy
  , radicalPrecision, siteParameter, sitePoint, subdivisionBudget )
import Moonlight.Planar.Curve.Proximity
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), exactPoint, exactPointCoordinates, exactRational
  , positiveExact, positiveOne, positiveTwo, unitInterval )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = sequence_
  [ testTangency
  , testSharedPoints
  , testConcentric
  , testCrossing
  , testFarLine
  , testRefusals
  , putStrLn "curve proximity: ok"
  ]

-- The parabola y = x^2 against its tangent at (1/3, 1/9). The contact lies
-- at parameters 2/3 and 1/3, which no halving reaches: the distance is
-- decided within the tolerance, and its lower end is exactly the true
-- distance zero, while clearance at zero stays unresolved at the depth
-- budget, neither holding nor violated.
testTangency :: IO ()
testTangency = do
  policy <- searching 24 20000 4096
  let parabola = open (p (-1) 1) [curveStep (quadratic (v 1 (-2))) (v 2 0)]
      tangent = open (p 0 (r (-1) 9)) [curveStep line (v 1 (r 2 3))]
      onParabola point = let (x, y) = exactPointCoordinates point in y == x * x
      onTangent point = let (x, y) = exactPointCoordinates point in y == r 2 3 * x - r 1 9
  observation <- requireRight "tangent distance" (curveDistance policy parabola tangent)
  let enclosure = distanceBounds observation
      (first, second) = distanceWitness observation
  assertEqual "tangent distance lower end is the contact" 0 (lengthEnclosureLower enclosure)
  assert "tangent distance within the tolerance" (lengthEnclosureWidth enclosure <= r 1 100)
  assert "tangent witness lies on both curves" (onParabola (sitePoint first) && onTangent (sitePoint second))
  assert "tangent witness within the upper end"
    (squaredBetween first second <= lengthEnclosureUpper enclosure * lengthEnclosureUpper enclosure)
  zero <- threshold 0 1
  case clearance policy zero parabola tangent of
    Right (ClearanceUnresolved reached _ DepthExhausted) ->
      assertEqual "unresolved tangency encloses the contact" 0 (lengthEnclosureLower reached)
    other -> failWith "tangent contact is unresolved at the depth budget" other
  hundredth <- threshold 1 100
  case clearance policy hundredth parabola tangent of
    Right (ClearanceViolated (CloserThan a b)) -> do
      assert "closer sites lie on both curves" (onParabola (sitePoint a) && onTangent (sitePoint b))
      assert "closer sites within the threshold" (squaredBetween a b <= r 1 10000)
    other -> failWith "tangent is closer than a hundredth" other

-- An exact common point is a shared point, never a crossing representative:
-- the quarter's endpoint (0, 1) on the chord y = 1 at its parameter 1/3, and
-- a circle against itself.
testSharedPoints :: IO ()
testSharedPoints = do
  policy <- searching 24 20000 4096
  zero <- threshold 0 1
  third <- requireRight "third" (unitInterval (r 1 3))
  let chord = open (p (-1) 1) [curveStep line (v 3 0)]
      unit = ClosedSubpath (circle positiveOne)
  case clearance policy zero unit chord of
    Right (ClearanceViolated (SharedPoint a b)) -> do
      assertEqual "quarter endpoint is the shared point" (p 0 1, p 0 1) (sitePoint a, sitePoint b)
      assertEqual "shared point at the chord's third" third (siteParameter b)
    other -> failWith "quarter endpoint on the chord is shared" other
  case clearance policy zero unit unit of
    Right (ClearanceViolated (SharedPoint a b)) -> assertEqual "circle meets itself" (sitePoint a) (sitePoint b)
    other -> failWith "same circle twice is shared" other
  traverse_
    (\(label, other) -> do
        observation <- requireRight label (curveDistance policy unit other)
        assertEqual label (0, 0) (endpoints (distanceBounds observation)))
    [("quarter endpoint on the chord at distance zero", chord), ("same circle twice at distance zero", unit)]

-- Concentric circles of radii one and two are exactly one apart everywhere:
-- the distance is enclosed within the tolerance, clearance holds below one
-- and, being strict, is violated at one by two sites exactly one apart.
testConcentric :: IO ()
testConcentric = do
  policy <- searching 24 20000 4096
  let inner = ClosedSubpath (circle positiveOne)
      outer = ClosedSubpath (circle positiveTwo)
  observation <- requireRight "concentric distance" (curveDistance policy inner outer)
  let enclosure = distanceBounds observation
  assert "concentric distance encloses one"
    (lengthEnclosureLower enclosure <= 1 && 1 <= lengthEnclosureUpper enclosure)
  assert "concentric distance within the tolerance" (lengthEnclosureWidth enclosure <= r 1 100)
  zero <- threshold 0 1
  below <- threshold 99 100
  one <- threshold 1 1
  traverse_
    (\(label, level) -> case clearance policy level inner outer of
        Right (ClearanceHolds _) -> pure ()
        other -> failWith label other)
    [("concentric circles are disjoint", zero), ("concentric clearance holds below one", below)]
  case clearance policy one inner outer of
    Right (ClearanceViolated (CloserThan a b)) -> assertEqual "concentric sites one apart" 1 (squaredBetween a b)
    other -> failWith "concentric clearance is strict at one" other

-- The unit circle crosses the diagonal y = x at an irrational point: contact
-- is certified by the crossing, with no point claimed.
testCrossing :: IO ()
testCrossing = do
  policy <- searching 24 20000 4096
  zero <- threshold 0 1
  let unit = ClosedSubpath (circle positiveOne)
      diagonal = open (p (-2) (-2)) [curveStep line (v 4 4)]
  case clearance policy zero unit diagonal of
    Right (ClearanceViolated TransversalCrossing {}) -> pure ()
    other -> failWith "circle crosses the diagonal" other
  observation <- requireRight "crossing distance" (curveDistance policy unit diagonal)
  assertEqual "crossing distance lower end is zero" 0 (lengthEnclosureLower (distanceBounds observation))

-- The segment from (5, 5) to (6, 5) is 5 sqrt 2 - 1 from the unit circle,
-- checked by exact squaring of the enclosure's ends.
testFarLine :: IO ()
testFarLine = do
  policy <- searching 24 20000 4096
  let unit = ClosedSubpath (circle positiveOne)
      far = open (p 5 5) [curveStep line (v 1 0)]
  observation <- requireRight "far distance" (curveDistance policy unit far)
  let (lower, upper) = endpoints (distanceBounds observation)
  assert "far distance encloses 5 sqrt 2 - 1" ((lower + 1) * (lower + 1) <= 50 && 50 <= (upper + 1) * (upper + 1))
  assert "far distance within the tolerance" (upper - lower <= r 1 100)
  six <- threshold 6 1
  seven <- threshold 7 1
  case clearance policy six unit far of
    Right (ClearanceHolds _) -> pure ()
    other -> failWith "far segment clears six" other
  case clearance policy seven unit far of
    Right (ClearanceViolated (CloserThan a b)) -> assert "far sites within seven" (squaredBetween a b <= 49)
    other -> failWith "far segment is closer than seven" other

-- Refusals name what ran out: an empty curve; a source step too wide for
-- the bit budget, located controls and so endpoint sites included; a
-- displacement too wide before any enclosure, refused with none; an enclosure or refinement too wide after
-- one, refused with the last admitted; a threshold too wide, on entry; more
-- step pairs than the leaf budget; a leaf budget spent before the
-- tolerance; and a radical precision too coarse for the tolerance.
testRefusals :: IO ()
testRefusals = do
  generous <- searching 24 20000 4096
  let unit = ClosedSubpath (circle positiveOne)
      outer = ClosedSubpath (circle positiveTwo)
      empty = open (p 0 0) []
  assertEqual "empty curve refused" (Left EmptyProximitySource) (() <$ curveDistance generous empty unit)
  narrow <- searching 24 20000 4
  let wide = open (p (r 1 1000) 5) [curveStep line (v 1 0)]
  case curveDistance narrow unit wide of
    Left (ProximitySourceRefused span' (BitsExhausted _)) ->
      assertEqual "wide source step named" (wide, 0) (curveSpanSource span', curveSpanStep span')
    other -> failWith "source wider than the bit budget" (() <$ other)
  -- Two stationary points: both sources fit four bits, their displacement
  -- (30, 30) and its square 1800 do not, before any enclosure exists.
  let upperPoint = open (p 15 15) [curveStep line (v 0 0)]
      lowerPoint = open (p (-15) (-15)) [curveStep line (v 0 0)]
      unenclosed :: Int -> Either ProximityError ()
      unenclosed width = Left (ProximityUnenclosed (ProximityBudgetExhausted (BitsExhausted width)))
  zero <- threshold 0 1
  assertEqual "displacement wider than the bit budget" (unenclosed 11) (() <$ curveDistance narrow upperPoint lowerPoint)
  assertEqual "displacement wider than the bit budget, at clearance" (unenclosed 11) (() <$ clearance narrow zero upperPoint lowerPoint)
  -- A line from (15, 0) by (15, 0): its start and relative controls fit four
  -- bits, its located end (30, 0), the endpoint site, does not, and the span
  -- admission that owns located controls refuses it before any site exists.
  let long = open (p 15 0) [curveStep line (v 15 0)]
      origin = open (p 0 0) [curveStep line (v 0 0)]
      endpointRefused :: Either ProximityError value -> Maybe (Subpath, Int, BudgetObligation)
      endpointRefused result = case result of
        Left (ProximitySourceRefused span' obligation) -> Just (curveSpanSource span', curveSpanStep span', obligation)
        _ -> Nothing
  assertEqual "endpoint site wider than the bit budget" (Just (long, 0, BitsExhausted 5))
    (endpointRefused (curveDistance narrow long origin))
  assertEqual "endpoint site wider than the bit budget, at clearance" (Just (long, 0, BitsExhausted 5))
    (endpointRefused (clearance narrow zero long origin))
  -- The first enclosure, [0, 1], is exact; the next lies on the 2^-64 grid
  -- of the radical precision, wider than four bits, so the refusal carries
  -- the last enclosure admitted.
  case curveDistance narrow unit outer of
    Left (ProximityRefused (ProximityBudgetExhausted (BitsExhausted 65)) reached _) ->
      assertEqual "last admitted enclosure" (0, 1) (lengthEnclosureLower reached, lengthEnclosureUpper reached)
    other -> failWith "enclosure wider than the bit budget" (() <$ other)
  coarseNarrow <- policyWith 1 24 20000 4
  case curveDistance coarseNarrow unit outer of
    Left (ProximityRefused (ProximityBudgetExhausted (BitsExhausted width)) reached _) -> do
      assert "refinement wider than four bits" (width > 4)
      assert "refused refinement still encloses one" (lengthEnclosureLower reached <= 1 && 1 <= lengthEnclosureUpper reached)
    other -> failWith "refinement wider than the bit budget" (() <$ other)
  -- A threshold of 2^-1000 is refused on entry, before it is squared or
  -- compared; at zero the same policy decides.
  sixteen <- policyWith 1 24 20000 16
  tiny <- threshold 1 (2 ^ (1000 :: Int))
  let far = open (p 5 5) [curveStep line (v 1 0)]
  assertEqual "threshold wider than the bit budget"
    (Left (ProximityRequestRefused (ProximityBudgetExhausted (BitsExhausted 1001)))) (() <$ clearance sixteen tiny unit far)
  case clearance sixteen zero unit far of
    Right (ClearanceHolds _) -> pure ()
    other -> failWith "the same policy decides at zero" other
  few <- searching 24 8 4096
  assertEqual "step pairs beyond the leaf budget" (Left (ProximityStepPairsRefused 4 4)) (() <$ curveDistance few unit outer)
  -- Ten billion step pairs are refused from the two step counts: building or
  -- enumerating them would not finish in the suite's time.
  let rail offset = open (p 0 offset) (replicate 100000 (curveStep line (v 1 0)))
  assertEqual "step pairs decided from the counts" (Left (ProximityStepPairsRefused 100000 100000))
    (() <$ curveDistance generous (rail 0) (rail 1))
  assertEqual "step pairs decided from the counts, at clearance" (Left (ProximityStepPairsRefused 100000 100000))
    (() <$ clearance generous zero (rail 0) (rail 1))
  spent <- searching 24 20 4096
  case curveDistance spent unit outer of
    Left (ProximityRefused (ProximityBudgetExhausted LeavesExhausted) reached candidates) -> do
      assert "exhausted distance still encloses one" (lengthEnclosureLower reached <= 1 && 1 <= lengthEnclosureUpper reached)
      assert "exhausted distance reports its candidates" (not (Seq.null candidates))
    other -> failWith "leaf budget spent before the tolerance" (() <$ other)
  coarse <- policyWith 1 24 20000 4096
  case curveDistance coarse unit outer of
    Left (ProximityRefused (ProximityPrecisionExhausted rounding tolerance) _ _) ->
      assert "rounding exceeds the tolerance" (rounding > tolerance)
    other -> failWith "precision too coarse for the tolerance" (() <$ other)

searching :: Int -> Int -> Int -> IO MeasurePolicy
searching = policyWith 64

policyWith :: Int -> Int -> Int -> Int -> IO MeasurePolicy
policyWith bits depth leaves width = do
  tolerance <- requireRight "tolerance" (positiveExact (r 1 100))
  precision <- requireRight "precision" (radicalPrecision bits)
  budget <- requireRight "proximity budget" (subdivisionBudget depth leaves width)
  pure (measurePolicy tolerance precision budget)

threshold :: Integer -> Integer -> IO Distance
threshold numerator denominator = requireRight "threshold" (distance (r numerator denominator))

open :: ExactPoint -> [CurveStep] -> Subpath
open origin steps = OpenSubpath (locate origin (openTrail (Seq.fromList steps)))

squaredBetween :: TrailSite -> TrailSite -> ExactRational
squaredBetween a b =
  let (ax, ay) = exactPointCoordinates (sitePoint a)
      (bx, by) = exactPointCoordinates (sitePoint b)
   in (bx - ax) * (bx - ax) + (by - ay) * (by - ay)

endpoints :: LengthEnclosure -> (ExactRational, ExactRational)
endpoints enclosure = (lengthEnclosureLower enclosure, lengthEnclosureUpper enclosure)

failWith :: Show value => String -> value -> IO ()
failWith label value = fail (label <> ": " <> show value)

assert :: String -> Bool -> IO ()
assert label condition = unless condition (fail label)

r :: Integer -> Integer -> ExactRational
r numerator denominator = either (const 0) id (exactRational numerator denominator)

v :: ExactRational -> ExactRational -> ExactVector
v = ExactVector

p :: ExactRational -> ExactRational -> ExactPoint
p = exactPoint

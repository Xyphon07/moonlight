-- | Predicate, binary64 format, and paraboloid lifting laws.
module Moonlight.Planar.PredicateSpec
  ( tests
  ) where

import Control.Monad ( forM_, unless )
import Moonlight.Planar.Math (inCircle, orient2d)
import Moonlight.Planar.Point (mitigateUnderflow, mkQueryPoint)
import Moonlight.Planar.Internal.Types (validatePoint)
import Moonlight.Planar.Scalar ( scalarBinaryFormat, scalarUnitRoundoff,
  BinaryFormat(formatMantissaDigits, formatRadix) )
import Moonlight.Planar.Point (Point(Point))
import Support ( assertEqual, requireRight, randomPoints )
import Moonlight.Planar.Internal.Predicates qualified as Admitted


tests :: IO ()
tests =
  sequence_
    [ testPredicates
    , testParaboloidLift
    , testScalarFormat
    ]

testScalarFormat :: IO ()
testScalarFormat = do
  let binary64 = scalarBinaryFormat
  assertEqual "binary64 radix" 2 (formatRadix binary64)
  assertEqual "binary64 mantissa" 53 (formatMantissaDigits binary64)
  assertEqual "binary64 unit roundoff" (encodeFloat 1 (-53)) (scalarUnitRoundoff :: Double)

testPredicates :: IO ()
testPredicates = do
  let rawA, rawB, rawC :: Point
      rawA = Point 0 0
      rawB = Point 1 0
      rawC = Point 0 1
  a <- requireRight "predicate point a" (mkQueryPoint rawA)
  b <- requireRight "predicate point b" (mkQueryPoint rawB)
  c <- requireRight "predicate point c" (mkQueryPoint rawC)
  midpointQuery <- requireRight "predicate midpoint" (mkQueryPoint (Point 0.5 0))
  inside <- requireRight "incircle inside point" (mkQueryPoint (Point 0.25 0.25))
  boundary <- requireRight "incircle boundary point" (mkQueryPoint (Point 1 1))
  outside <- requireRight "incircle outside point" (mkQueryPoint (Point 2 2))
  assertEqual "orientation left" GT (orient2d a b c)
  assertEqual "orientation right" LT (orient2d b a c)
  assertEqual "orientation collinear" EQ (orient2d a b midpointQuery)
  assertEqual "incircle inside" GT (inCircle a b c inside)
  assertEqual "incircle boundary" EQ (inCircle a b c boundary)
  assertEqual "incircle outside" LT (inCircle a b c outside)
  assertEqual "underflow mitigation" (Point 0 1) (mitigateUnderflow (Point 1.0e-44 1 :: Point))
  _ <- requireRight "point validation" (validatePoint Nothing (Point 0 1 :: Point))
  let large = encodeFloat 1 180 :: Double
      ulp = encodeFloat 1 128 :: Double
  largeA <- requireRight "large predicate point a" (mkQueryPoint (Point large large))
  largeB <- requireRight "large predicate point b" (mkQueryPoint (Point (large + ulp) large))
  largeC <- requireRight "large predicate point c" (mkQueryPoint (Point large (large + ulp)))
  largeD <- requireRight "large predicate point d" (mkQueryPoint (Point (large + ulp) (large + ulp)))
  assertEqual
    "large exact orientation"
    GT
    (orient2d largeA largeB largeC)
  assertEqual
    "large exact cocircularity"
    EQ
    (inCircle largeA largeB largeC largeD)

testParaboloidLift :: IO ()
testParaboloidLift = do
  let lifted :: Point -> (Rational, Rational, Rational)
      lifted (Point x y) =
        let (rx, ry) = (toRational x, toRational y) in (rx, ry, rx * rx + ry * ry)
      minor3
        :: (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> Rational
      minor3 (a1, a2, a3) (b1, b2, b3) (c1, c2, c3) =
        a1 * (b2 * c3 - b3 * c2) - a2 * (b1 * c3 - b3 * c1) + a3 * (b1 * c2 - b2 * c1)
      -- Laplace expansion of the lifted determinant along its column of ones.
      liftedOrientation
        :: (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> (Rational, Rational, Rational)
        -> Rational
      liftedOrientation a b c d =
        negate (minor3 b c d) + minor3 a c d - minor3 a b d + minor3 a b c
      predicted :: Point -> Point -> Point -> Point -> Ordering
      predicted a b c d = compare (liftedOrientation (lifted a) (lifted b) (lifted c) (lifted d)) 0
      measured :: Point -> Point -> Point -> Point -> Ordering
      measured (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy) =
        Admitted.inCircleCoordinates ax ay bx by cx cy dx dy
      quadruples :: [Point] -> [(Point, Point, Point, Point)]
      quadruples (a : b : c : d : rest) = (a, b, c, d) : quadruples rest
      quadruples _ = []

  forM_ (quadruples (randomPoints 0x51ca_0b17 1600)) $ \(a, b, c, d) ->
    assertEqual ("in-circle is the lifted orientation at " <> show (a, b, c, d))
      (predicted a b c d) (measured a b c d)

  -- Exactly cocircular quadruples, which are exactly the ones the floating
  -- filter must decline to answer. Every Pythagorean point of the radius-5
  -- circle is representable without rounding, so 'EQ' here is a fact about the
  -- geometry rather than about the arithmetic.
  let ring =
        [ Point x y
        | (x, y) <-
            [ (5, 0), (0, 5), (-5, 0), (0, -5)
            , (3, 4), (4, 3), (-3, 4), (-4, 3)
            , (3, -4), (4, -3), (-3, -4), (-4, -3)
            ]
        ] :: [Point]
      indexed = zip [(0 :: Int) ..] ring
      cocircular =
        [ (a, b, c, d)
        | (i, a) <- indexed, (j, b) <- indexed, j > i
        , (k, c) <- indexed, k > j, (l, d) <- indexed, l > k
        ]
  unless (length cocircular == 495) $ fail ("expected 495 cocircular quadruples, got " <> show (length cocircular))
  forM_ cocircular $ \(a, b, c, d) -> do
    assertEqual ("cocircular points are cocircular at " <> show (a, b, c, d)) EQ (measured a b c d)
    assertEqual ("the lift agrees on cocircularity at " <> show (a, b, c, d)) EQ (predicted a b c d)

  -- The geometry the sign means, stated once against a circle anyone can read.
  let (a, b, c) = (Point 1 0, Point 0 1, Point (-1) 0) :: (Point, Point, Point)
  assertEqual "the centre is inside the circle" GT (measured a b c (Point 0 0))
  assertEqual "the antipode is on the circle" EQ (measured a b c (Point 0 (-1)))
  assertEqual "a distant point is outside" LT (measured a b c (Point 2 2))

-- | A payload labels an element, and an element is its geometry. Every rewrite
-- an insertion performs — an edge split, a face split, a Lawson flip — hands
-- some slot a different element to hold, and the label the displaced one
-- carried does not describe what took its place.
--
-- So: label every element of a triangulation by its own key, insert a point,
-- and demand that an element whose key survives still carries exactly the label
-- it was given while every element whose key is new carries the default and
-- nothing else. Both directions are checked; either alone is satisfiable by a
-- store that throws everything away.
--
-- The flip is the load-bearing case. Legalization is confluent, so which flips
-- fire and in what order is not observable in the topology that comes out. A
-- payload that rode through a flip would make it observable in the payload
-- plane, and a triangulation that is a normal form in one component and a
-- history in another is not a normal form.

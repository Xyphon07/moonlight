-- | Curve certificate and certified region laws: every returned witness
-- discharges the strict inequality it advertises; a region is lowered only
-- inside the certified domain, with a crossing reported by its certificate,
-- an unresolved tangency or cusp by the budget it spent, and a lowered
-- polygon that nests otherwise than its curves refused.
module Moonlight.Planar.CurveCertificateSpec (tests) where

import Control.Monad (unless)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (identityAffine2)
import Moonlight.Planar.Curve
  ( ClosedTrail, CurveStep, Located, closeWith, cubic, curveStep, curveStepEnd, line, locate
  , openTrail, quadratic, rationalQuadratic, stepControlPoints )
import Moonlight.Planar.Curve.Lowering (LoweringPolicy, loweringPolicy)
import Moonlight.Planar.Curve.Region
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), divideByPositive, exactOrient2d, exactPoint
  , exactPointCoordinates, exactRational, positiveExact, positiveOne, positiveTwo, unitOne, unitZero )
import qualified Moonlight.Planar.Internal.CurveCertificate as Certificate
import Moonlight.Planar.Internal.CurveCertificate
  ( CrossingVerdict (..), halfPlaneWitness, hullGap, hullSeparation, jointWitness
  , monotoneWitness, separatingAxis, stationaryPiece )
import Moonlight.Planar.Region (RegionPointLocation (..), regionPointLocation)
import Support (assertEqual, requireRight)

tests :: IO ()
tests = sequence_
  [ testHalfPlane
  , testSeparation
  , testPieceWitnesses
  , testCrossingVerdicts
  , testCertifiedRegions
  , testRefusedRegions
  , testBudgetRefusals
  , testPolygonTopology
  , putStrLn "curve certificate: ok"
  ]

-- A returned half-plane witness is strictly positive on every nonzero input,
-- and one exists whenever the nonzero inputs lie in an open half-plane.
testHalfPlane :: IO ()
testHalfPlane = do
  let cases =
        [ ("antipodal pair", [v 1 0, v (-1) 0], False)
        , ("zero vectors only", [v 0 0], False)
        , ("open cone wider than a right angle", [v 1 0, v (-1) 1, v 0 1], True)
        , ("parallel inputs", [v 2 1, v 4 2, v 0 0], True)
        , ("three directions spanning the plane", [v 1 0, v (-1) 1, v (-1) (-1)], False)
        ]
  traverse_
    (\(label, vectors, expected) -> case halfPlaneWitness vectors of
        Nothing -> assertEqual ("half-plane: " <> label) expected False
        Just direction -> do
          assertEqual ("half-plane: " <> label) expected True
          assert ("half-plane witness is strict: " <> label)
            (all (\vector -> vector == v 0 0 || dot direction vector > 0) vectors))
    cases

-- A returned axis puts every point of the first set strictly below every
-- point of the second; touching or crossing hulls have none. The hull gap
-- is the distance between the hulls.
testSeparation :: IO ()
testSeparation = do
  let cases =
        [ ("touching segments", p 0 0 :| [p 1 1], p 1 1 :| [p 2 0], False)
        , ("collinear disjoint segments", p 0 0 :| [p 1 0], p 2 0 :| [p 3 0], True)
        , ("crossing segments", p 0 0 :| [p 2 2], p 0 2 :| [p 2 0], False)
        , ("point beside a triangle", p 3 3 :| [], p 0 0 :| [p 2 0, p 0 2], True)
        , ("point on a triangle edge", p 1 1 :| [], p 0 0 :| [p 2 0, p 0 2], False)
        ]
  traverse_
    (\(label, lower, upper, expected) -> case separatingAxis lower upper of
        Nothing -> assertEqual ("separation: " <> label) expected False
        Just axis -> do
          assertEqual ("separation: " <> label) expected True
          assert ("separating axis is strict: " <> label)
            (all (\a -> all (\b -> project axis a < project axis b) upper) lower))
    cases
  -- The hull gap is the hulls' distance, exactly in square, against a brute
  -- force independent of the candidate axes: when the hulls meet, zero, and
  -- otherwise the least squared distance from a point of either set to a
  -- segment between two points of the other. Diagonals inside a hull are
  -- never nearer than its edges, so the brute force is the hull distance.
  traverse_
    (\(label, lower, upper, squared) -> do
        assertEqual ("hull gap by hand: " <> label) squared (squaredGap lower upper)
        assertEqual ("hull gap by brute force: " <> label) (bruteGap lower upper) (squaredGap lower upper))
    [ ("separated boxes", p 0 0 :| [p 1 0, p 1 1, p 0 1], p 3 2 :| [p 4 2, p 4 3, p 3 3], 5)
    , ("vertex nearest an edge interior", p 3 3 :| [], p 0 0 :| [p 2 0, p 0 2], 8)
    , ("edge interior nearest a vertex, either way round", p 0 0 :| [p 2 0, p 0 2], p 3 3 :| [], 8)
    , ("point off a triangle vertex", p 5 (-1) :| [], p 0 0 :| [p 2 0, p 0 2], 10)
    , ("collinear disjoint segments", p 0 0 :| [p 1 0], p 2 0 :| [p 3 0], 1)
    , ("parallel edges", p 0 0 :| [p 2 0, p 1 (-1)], p 1 (r 1 2) :| [p 3 (r 1 2), p 2 1], r 1 4)
    , ("overlapping hulls", p 0 0 :| [p 2 2], p 0 2 :| [p 2 0], 0)
    , ("touching segments", p 0 0 :| [p 1 1], p 1 1 :| [p 2 0], 0)
    , ("coincident points", p 1 1 :| [], p 1 1 :| [], 0)
    ]
  -- Every segment on a lattice against a fixed triangle.
  let triangle = p 0 0 :| [p 2 0, p 0 2]
      lattice = [p (fromInteger x) (fromInteger y) | x <- [-1 .. 4], y <- [-1 .. 4]]
      mismatches =
        [ (a, b)
        | a <- lattice, b <- lattice
        , let segment = a :| [b]
        , squaredGap triangle segment /= bruteGap triangle segment ]
  assertEqual "hull gap by brute force over lattice segments" [] mismatches
 where
  squaredGap lower upper = let gap = hullGap lower upper in dot gap gap

-- The squared distance between two hulls without the candidate axes: zero
-- when a segment of one meets a segment of the other or a point of one lies
-- in the other's triangle fan, and otherwise the least point-to-segment
-- distance.
bruteGap :: NonEmpty ExactPoint -> NonEmpty ExactPoint -> ExactRational
bruteGap lower upper
  | meets = 0
  | otherwise = case distances of
      nearest : others -> foldr min nearest others
      [] -> 0
 where
  lows = toList lower
  highs = toList upper
  segments :: [ExactPoint] -> [(ExactPoint, ExactPoint)]
  segments points = [(a, b) | a <- points, b <- points]
  meets =
    or [segmentsMeet a b c d | (a, b) <- segments lows, (c, d) <- segments highs]
      || any (inFan lows) highs || any (inFan highs) lows
  distances = [pointSegment x s | x <- lows, s <- segments highs] <> [pointSegment x s | x <- highs, s <- segments lows]

-- Whether a point lies in some triangle of three of the points, boundary
-- included: the hull is the union of such triangles.
inFan :: [ExactPoint] -> ExactPoint -> Bool
inFan points x =
  or [ all (/= LT) orientations || all (/= GT) orientations
     | a <- points, b <- points, c <- points
     , exactOrient2d a b c /= EQ
     , let orientations = [exactOrient2d a b x, exactOrient2d b c x, exactOrient2d c a x] ]

segmentsMeet :: ExactPoint -> ExactPoint -> ExactPoint -> ExactPoint -> Bool
segmentsMeet a b c d =
  (o1 /= o2 && o3 /= o4 && all (/= EQ) [o1, o2, o3, o4])
    || (o1 == EQ && within a b c) || (o2 == EQ && within a b d)
    || (o3 == EQ && within c d a) || (o4 == EQ && within c d b)
 where
  o1 = exactOrient2d a b c
  o2 = exactOrient2d a b d
  o3 = exactOrient2d c d a
  o4 = exactOrient2d c d b
  within s t x =
    let (sx, sy) = exactPointCoordinates s
        (tx, ty) = exactPointCoordinates t
        (xx, xy) = exactPointCoordinates x
     in min sx tx <= xx && xx <= max sx tx && min sy ty <= xy && xy <= max sy ty

pointSegment :: ExactPoint -> (ExactPoint, ExactPoint) -> ExactRational
pointSegment x (s, t) = squaredLength (difference (offset nearest) (offset x))
 where
  direction = difference (offset t) (offset s)
  along = case positiveExact (dot direction direction) of
    Right length2 -> max 0 (min 1 (divideByPositive (dot (difference (offset x) (offset s)) direction) length2))
    Left _ -> 0
  nearest = let (sx, sy) = exactPointCoordinates s; ExactVector dx dy = direction in p (sx + along * dx) (sy + along * dy)
  offset point = let (px, py) = exactPointCoordinates point in ExactVector px py
  squaredLength vector = dot vector vector

testPieceWitnesses :: IO ()
testPieceWitnesses = do
  let sCurve = curveStep (cubic (v 1 2) (v 2 (-1))) (v 3 1)
      loopy = curveStep (cubic (v 2 1) (v (-1) 1)) (v 1 0)
      bulge = curveStep (rationalQuadratic (v 1 1) positiveOne positiveTwo) (v 0 2)
  assertEqual "monotone: s-curve" True (advances sCurve)
  assertEqual "monotone: rational bulge" True (advances bulge)
  assertEqual "monotone: looping cubic" False (advances loopy)
  assertEqual "monotone: stationary step" False (advances (curveStep line (v 0 0)))
  assertEqual "stationary: zero line" True (stationaryPiece (curveStep line (v 0 0)))
  assertEqual "stationary: returning cubic moves" False (stationaryPiece (curveStep (cubic (v 1 0) (v 1 0)) (v 0 0)))
  let joins =
        [ ("smooth continuation", curveStep line (v 1 0), curveStep line (v 1 1), True)
        , ("sharp corner", curveStep line (v 1 0), curveStep line (v (-1) 1), True)
        , ("reversal cusp", curveStep line (v 1 0), curveStep line (v (-1) 0), False)
        , ( "quadratic cusp"
          , curveStep (quadratic (v 1 0)) (v 1 1), curveStep (quadratic (v 0 (-1))) (v 1 (-1)), False )
        ]
  traverse_
    (\(label, incoming, outgoing, expected) -> case jointWitness incoming outgoing of
        Nothing -> assertEqual ("joint: " <> label) expected False
        Just direction -> do
          assertEqual ("joint: " <> label) expected True
          assert ("joint witness is strict: " <> label)
            (all (\c -> c == curveStepEnd incoming || dot direction (difference c (curveStepEnd incoming)) > 0)
                (toList (stepControlPoints incoming))
              && all (\c -> c == v 0 0 || dot direction c < 0) (toList (stepControlPoints outgoing))))
    joins
 where
  advances step = case monotoneWitness step of
    Nothing -> False
    Just direction -> all (\edge -> edge == v 0 0 || dot direction edge > 0) (edges step)
  edges step = let points = toList (stepControlPoints step) in zipWith difference (drop 1 points) points

-- The verdict needs both cones and all four endpoints clear; the parabola
-- y = x^2 against its tangent at (1/3, 1/9) never receives one.
testCrossingVerdicts :: IO ()
testCrossingVerdicts = do
  budget <- requireRight "budget" (subdivisionBudget 12 4096 4096)
  narrow <- requireRight "one bit" (subdivisionBudget 12 4096 1)
  let crossingVerdict = Certificate.crossingVerdict budget
      monotoneQuadratic = curveStep (quadratic (v (r 1 2) 0)) (v 1 1)
      antiDiagonal = curveStep line (v 1 (-1))
      parabola = curveStep (quadratic (v 1 (-2))) (v 2 0)
      tangent = curveStep line (v 1 (r 2 3))
  assertEqual "crossing: monotone quadratic meets the anti-diagonal once" (Right (Just True))
    (fmap single <$> crossingVerdict (p 0 0) monotoneQuadratic (p 0 1) antiDiagonal)
  assertEqual "crossing: a far anti-diagonal is certified clear" (Right (Just False))
    (fmap single <$> crossingVerdict (p 0 0) monotoneQuadratic (p 2 3) antiDiagonal)
  assertEqual "crossing: a tangent line has no verdict" (Right Nothing)
    (fmap single <$> crossingVerdict (p (-1) 1) parabola (p 0 (r (-1) 9)) tangent)
  assertEqual "crossing: a non-monotone parabola has no verdict" (Right Nothing)
    (fmap single <$> crossingVerdict (p (-1) 1) parabola (p (-1) (r 1 2)) (curveStep line (v 2 0)))
  -- A certificate is returned only admitted: under one bit it is refused.
  case Certificate.crossingVerdict narrow (p 0 0) monotoneQuadratic (p 0 1) antiDiagonal of
    Left (BitsExhausted width) -> assert "certificate wider than one bit" (width > 1)
    other -> fail ("certificate admitted under one bit: " <> show (fmap single <$> other))
  assertEqual "hull: a far line is separated from the parabola" True
    (hullSeparation (p (-1) 1) parabola (p (-1) 5) (curveStep line (v 2 0)) /= Nothing)
 where
  single verdict = case verdict of
    SingleCrossing _ -> True
    NoCrossing _ -> False

testCertifiedRegions :: IO ()
testCertifiedRegions = do
  budget <- requireRight "budget" (subdivisionBudget 12 4096 4096)
  policy <- fine
  (region, receipts, evidence) <- requireRight "square with hole"
    (lowerSimpleRegion policy budget [CurveComponent square [squareHole]])
  assertEqual "one receipt per contour" 2 (length receipts)
  assertEqual "square pieces" [(ContourRef 0 OuterContour, 4), (ContourRef 0 (HoleContour 0), 4)]
    (certifiedPieceCounts evidence)
  -- Away from every piece hull the certificate agrees with the admitted
  -- polygon; on a hull it does not decide.
  traverse_
    (\point -> assertEqual "certified location agrees with the polygon"
        (Just (regionPointLocation region point)) (certifiedPointLocation evidence point))
    [p 3 3, p (r 3 2) (r 3 2), p 5 5, p (-1) 2]
  assertEqual "no certified location on a hull" [Nothing, Nothing]
    (map (certifiedPointLocation evidence) [p 1 1, p 2 1])
  (_, _, rings) <- requireRight "concentric circles"
    (lowerSimpleRegion policy budget [CurveComponent (ring True 0 0 2) [ring False 0 0 1]])
  assertEqual "annulus interior" (Just RegionInterior) (certifiedPointLocation rings (p (r 3 2) 0))
  assertEqual "annulus hole" (Just RegionExterior) (certifiedPointLocation rings (p 0 0))
  shallow <- requireRight "depth one" (subdivisionBudget 1 4096 4096)
  requireRight "concentric circles within one halving"
    (certifySimpleRegion shallow [CurveComponent (ring True 0 0 2) [ring False 0 0 1]])
    >>= assertEqual "one halving per quarter" [(ContourRef 0 OuterContour, 8), (ContourRef 0 (HoleContour 0), 8)]
      . certifiedPieceCounts
  -- A zero-length connector is a stationary step, contracted at its joint.
  requireRight "zero-length connector"
    (certifySimpleRegion budget [CurveComponent (polygon (p 0 0) [v 4 0, v 0 0, v 0 4, v (-4) 0]) []])
    >>= assertEqual "connector contracted" [(ContourRef 0 OuterContour, 4)] . certifiedPieceCounts

testRefusedRegions :: IO ()
testRefusedRegions = do
  budget <- requireRight "budget" (subdivisionBudget 12 4096 4096)
  let certify = certifySimpleRegion budget
      bowTie = polygon (p 0 0) [v 2 2, v (-2) 0, v 2 (-2)]
      cusp = closedSteps (p 0 0)
        [ curveStep (quadratic (v 1 0)) (v 1 1), curveStep (quadratic (v 0 (-1))) (v 1 (-1))
        , curveStep line (v 0 (-1)), curveStep line (v (-2) 0) ]
  case certify [CurveComponent bowTie []] of
    Left (CertifiedSelfCrossing (ContourSpan _ 0 _ _) (ContourSpan _ 2 _ _) _) -> pure ()
    other -> failWith "bow-tie crossing is certified between its first and third steps" other
  -- Contact at a source step's endpoint is certified with its exact point:
  -- the hole touches the outer circle at (2, 0), a quarter endpoint of both;
  -- a hole circle's quarter endpoint (0, 2) lies inside the square's left
  -- side; a square submitted as its own hole shares all four vertices.
  let outerHole point = Just (Contact (ContourRef 0 OuterContour) (ContourRef 0 (HoleContour 0)) point)
  assertEqual "tangent hole at a shared quarter endpoint" (outerHole (p 2 0))
    (contactAt (certify [CurveComponent (ring True 0 0 2) [ring False 1 0 1]]))
  assertEqual "quarter endpoint inside a straight side" (outerHole (p 0 2))
    (contactAt (certify [CurveComponent square [ring False 1 2 1]]))
  case contactAt (certify [CurveComponent square [square]]) of
    Just (Contact (ContourRef 0 OuterContour) (ContourRef 0 (HoleContour 0)) vertex)
      | vertex `elem` [p 0 0, p 4 0, p 4 4, p 0 4] -> pure ()
    other -> fail ("square as its own hole shares a vertex: " <> show other)
  -- A hole circle touching the outer circle at (3, 4), inside a step of
  -- each: no endpoint is shared, and the tangency stays unresolved.
  case certify [CurveComponent (ring True 0 0 5) [ring False (r 12 5) (r 16 5) 1]] of
    Left (UnresolvedContact (ContourSpan (ContourRef 0 OuterContour) _ _ _) (ContourSpan (ContourRef 0 (HoleContour 0)) _ _ _) DepthExhausted) -> pure ()
    other -> failWith "interior tangency is unresolved contact" other
  case certify [CurveComponent cusp []] of
    Left (UnseparatedJoin (ContourSpan _ 0 _ _) (ContourSpan _ 1 _ _) DepthExhausted) -> pure ()
    other -> failWith "cusp is an unseparated joint" other
  assertEqual "stationary contour" (Just (DegenerateContour (ContourRef 0 OuterContour)))
    (refusal (certify [CurveComponent (polygon (p 0 0) [v 0 0]) []]))
  assertEqual "clockwise outer" (Just (ContourWindingRefused (ContourRef 0 OuterContour) LT))
    (refusal (certify [CurveComponent (ring False 0 0 2) []]))
  assertEqual "counter-clockwise hole"
    (Just (ContourWindingRefused (ContourRef 0 (HoleContour 0)) GT))
    (refusal (certify [CurveComponent (ring True 0 0 2) [ring True 0 0 1]]))
  case certify [CurveComponent (ring True 0 0 2) [], CurveComponent (ring True 3 0 2) []] of
    Left (CertifiedContourCrossing (ContourSpan (ContourRef 0 _) _ _ _) (ContourSpan (ContourRef 1 _) _ _ _) _) -> pure ()
    other -> failWith "overlapping circles cross between components" other

testBudgetRefusals :: IO ()
testBudgetRefusals = do
  few <- requireRight "three leaves" (subdivisionBudget 12 3 4096)
  narrow <- requireRight "two bits" (subdivisionBudget 12 4096 2)
  flat <- requireRight "no depth" (subdivisionBudget 0 4096 4096)
  assertEqual "leaves below the source steps"
    (Just (SourceSpanRefused (ContourSpan (ContourRef 0 OuterContour) 3 unitZero unitOne) LeavesExhausted))
    (refusal (certifySimpleRegion few [CurveComponent square []]))
  assertEqual "bits below the source coordinates"
    (Just (SourceSpanRefused (ContourSpan (ContourRef 0 OuterContour) 0 unitZero unitOne) (BitsExhausted 3)))
    (refusal (certifySimpleRegion narrow [CurveComponent square []]))
  -- The same square's start and relative controls fit four bits wherever it
  -- sits; its located controls do not once it sits at (12, 12), where its
  -- first step ends at (16, 12).
  fourBits <- requireRight "four bits" (subdivisionBudget 12 4096 4)
  requireRight "square at the origin within four bits" (certifySimpleRegion fourBits [CurveComponent square []])
    >>= assertEqual "four sides" [(ContourRef 0 OuterContour, 4)] . certifiedPieceCounts
  assertEqual "located controls beyond the source coordinates"
    (Just (SourceSpanRefused (ContourSpan (ContourRef 0 OuterContour) 0 unitZero unitOne) (BitsExhausted 5)))
    (refusal (certifySimpleRegion fourBits [CurveComponent (polygon (p 12 12) [v 4 0, v 0 4, v (-4) 0]) []]))
  case certifySimpleRegion flat [CurveComponent (ring True 0 0 2) [ring False 0 0 1]] of
    Left (UnresolvedContact _ _ DepthExhausted) -> pure ()
    other -> failWith "a whole-quarter circle needs a halving" other
  assertEqual "invalid budgets" [Left (InvalidBudgetDepth (-1)), Left (InvalidBudgetLeaves 0), Left (InvalidBudgetBits 0)]
    (map (fmap (const ())) [subdivisionBudget (-1) 1 1, subdivisionBudget 0 0 1, subdivisionBudget 0 1 0])

-- The curves nest a small triangle inside the circle of radius 2; lowered at
-- tolerance 2 without subdivision the circle is its chord square, which the
-- triangle lies outside. Both polygons are admitted; their nesting is not the
-- curves'.
testPolygonTopology :: IO ()
testPolygonTopology = do
  budget <- requireRight "budget" (subdivisionBudget 12 4096 4096)
  coarse <- requireRight "chord-square lowering" (loweringPolicy positiveTwo identityAffine2 0 64)
  let triangle = polygon (p (r 13 10) (r 13 10)) [v (r 1 10) 0, v (r (-1) 10) (r 1 10)]
      components = [CurveComponent (ring True 0 0 2) [], CurveComponent triangle []]
  case certifySimpleRegion budget components of
    Right _ -> pure ()
    Left obstruction -> failWith "the curves themselves are certified" (Left obstruction :: Either TopologyObstruction ())
  case lowerSimpleRegion coarse budget components of
    Left (CurveTopologyRefused (PolygonTopologyDiffers (ContourRef 1 OuterContour) (ContourRef 0 OuterContour))) -> pure ()
    other -> failWith "chord square nests otherwise than the circle" (() <$ other)

fine :: IO LoweringPolicy
fine = requireRight "lowering policy" (loweringPolicy positiveOne identityAffine2 20 8192)

square :: Located ClosedTrail
square = polygon (p 0 0) [v 4 0, v 0 4, v (-4) 0]

squareHole :: Located ClosedTrail
squareHole = polygon (p 1 1) [v 0 1, v 1 0, v 0 (-1)]

-- The circle of radius k about (cx, cy) from (cx + k, cy), by four rational
-- quarters, counter-clockwise or clockwise.
ring :: Bool -> ExactRational -> ExactRational -> ExactRational -> Located ClosedTrail
ring counterClockwise cx cy k = closedSteps (p (cx + k) cy) (map quarter quarters)
 where
  quarter (control, end) = curveStep (rationalQuadratic (scale control) positiveOne positiveTwo) (scale end)
  scale (ExactVector x y) = ExactVector (k * x) (k * y)
  quarters
    | counterClockwise = [(v 0 1, v (-1) 1), (v (-1) 0, v (-1) (-1)), (v 0 (-1), v 1 (-1)), (v 1 0, v 1 1)]
    | otherwise = [(v 0 (-1), v (-1) (-1)), (v (-1) 0, v (-1) 1), (v 0 1, v 1 1), (v 1 0, v 1 (-1))]

closedSteps :: ExactPoint -> [CurveStep] -> Located ClosedTrail
closedSteps origin steps = locate origin (closeWith line (openTrail (Seq.fromList steps)))

polygon :: ExactPoint -> [ExactVector] -> Located ClosedTrail
polygon origin = closedSteps origin . map (curveStep line)

-- A certified contact by its two contours and its point.
data Contact = Contact ContourRef ContourRef ExactPoint
  deriving stock (Eq, Show)

contactAt :: Either TopologyObstruction value -> Maybe Contact
contactAt result = case result of
  Left (CertifiedContact (ContourSpan first _ _ _) (ContourSpan second _ _ _) point) -> Just (Contact first second point)
  _ -> Nothing

refusal :: Either obstruction value -> Maybe obstruction
refusal = either Just (const Nothing)

failWith :: Show obstruction => String -> Either obstruction value -> IO ()
failWith label result = fail (label <> ": " <> either show (const "admitted") result)

assert :: String -> Bool -> IO ()
assert label condition = unless condition (fail label)

project :: ExactVector -> ExactPoint -> ExactRational
project (ExactVector ax ay) point = let (x, y) = exactPointCoordinates point in ax * x + ay * y

dot :: ExactVector -> ExactVector -> ExactRational
dot (ExactVector ax ay) (ExactVector bx by) = ax * bx + ay * by

difference :: ExactVector -> ExactVector -> ExactVector
difference (ExactVector ax ay) (ExactVector bx by) = ExactVector (ax - bx) (ay - by)

r :: Integer -> Integer -> ExactRational
r numerator denominator = either (const 0) id (exactRational numerator denominator)

v :: ExactRational -> ExactRational -> ExactVector
v = ExactVector

p :: ExactRational -> ExactRational -> ExactPoint
p = exactPoint

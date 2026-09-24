module Moonlight.Planar.CurveLoweringSpec (tests) where

import Control.Monad (unless)
import Control.Exception (TypeError, displayException, evaluate, try)
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import Moonlight.Planar.Affine (Affine2, affine2, identityAffine2)
import Moonlight.Planar.Curve
  ( CurveStep, ClosedTrail, Located, curveStep, line, quadratic, cubic
  , rationalQuadratic, locate, openTrail, closeWith )
import Moonlight.Planar.Curve.Lowering
import Moonlight.Planar.Curve.Region
import qualified Moonlight.Planar.CurveLoweringTypeErrors as Rejected
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactVector (..), exactPoint, exactPointCoordinates
  , exactHalf, positiveExact, positiveOne, positiveTwo )
import Moonlight.Planar.Region (RegionPointLocation (..), regionPointLocation)

tests :: IO ()
tests = do
  testPolicyRefusals
  testFiniteChord
  testSubdivision
  testMetric
  testConic
  testRegion
  testCertificateAdmission
  putStrLn "curve lowering: ok"

testPolicyRefusals :: IO ()
testPolicyRefusals = do
  assertRefusal "negative depth" (loweringPolicy positiveOne identityAffine2 (-1) 10)
  assertRefusal "zero leaves" (loweringPolicy positiveOne identityAffine2 10 0)
  assertRefusal "zero tolerance" (positiveExact 0)

testFiniteChord :: IO ()
testFiniteChord = do
  policy <- makePolicy 1 identityAffine2 0 10
  straight <- requireRight "line" (lowerStep policy (atOrigin (curveStep line (vector 1 0))))
  assertEqual "line has no deviation" [0] (map spanSquaredBound (loweredSpans straight))
  assertRefusal "finite chord catches collinear overshoot"
    (lowerStep policy (atOrigin (curveStep (quadratic (vector 10 0)) (vector 1 0))))
  assertRefusal "zero chord is not a constant curve"
    (lowerStep policy (atOrigin (curveStep (cubic (vector 0 10) (vector 10 0)) (vector 0 0))))
  constant <- requireRight "constant source" (lowerStep policy (atOrigin (curveStep line (vector 0 0))))
  assertEqual "constant bound" [0] (map spanSquaredBound (loweredSpans constant))

testSubdivision :: IO ()
testSubdivision = do
  policy <- makePolicy exactHalf identityAffine2 12 1024
  let curve = atOrigin (curveStep (cubic (vector 0 8) (vector 8 8)) (vector 8 0))
  lowered <- requireRight "adaptive cubic" (lowerStep policy curve)
  let spans = loweredSpans lowered
      points = loweredPoints lowered
  assert "adaptive output subdivides" (length spans > 1)
  assert "every leaf carries its admitted bound" (all ((<= exactHalf * exactHalf) . spanSquaredBound) spans)
  assertEqual "start retained" (point 0 0) (NonEmpty.head points)
  assertEqual "end retained" (point 8 0) (NonEmpty.last points)
  assertEqual "parameter start" [0] (take 1 (map spanParameterFrom spans))
  assertEqual "parameter end" [1] (take 1 (reverse (map spanParameterTo spans)))
  assert "source intervals glue exactly"
    (and (zipWith (\left right -> spanParameterTo left == spanParameterFrom right) spans (drop 1 spans)))
  assertEqual "shared seam reversal reuses samples" (NonEmpty.reverse points)
    (loweredPoints (reverseLoweredPath lowered))
  assertEqual "reversal involution" lowered (reverseLoweredPath (reverseLoweredPath lowered))
  smallBudget <- makePolicy exactHalf identityAffine2 12 1
  case lowerStep smallBudget curve of
    Left (LeafBudgetExhausted _ _ _) -> pure ()
    other -> fail ("expected leaf budget refusal, received " <> show other)
  let twoLines = locate (point 0 0) (openTrail (Seq.fromList
        [curveStep line (vector 1 0), curveStep line (vector 0 1)]))
  case lowerOpenTrail smallBudget twoLines of
    Left (LeafBudgetExhausted 1 0 1) -> pure ()
    other -> fail ("leaf budget must span the whole trail: " <> show other)
  empty <- requireRight "empty open trail" (lowerOpenTrail policy (locate (point 3 4) (openTrail Seq.empty)))
  assertEqual "empty open trail retains location" [point 3 4] (NonEmpty.toList (loweredPoints empty))

testMetric :: IO ()
testMetric = do
  let curve = atOrigin (curveStep (quadratic (vector exactHalf exactHalf)) (vector 1 0))
      stretch = affine2 (vector 1 0) (vector 0 100) (vector 0 0)
  local <- makePolicy 1 identityAffine2 0 10
  output <- makePolicy 1 stretch 0 10
  _ <- requireRight "local bound admits" (lowerStep local curve)
  assertRefusal "output metric sees anisotropic magnification" (lowerStep output curve)
  translated <- makePolicy 1 (affine2 (vector 1 0) (vector 0 1) (vector 100 200)) 0 10
  localResult <- requireRight "local result" (lowerStep local curve)
  translatedResult <- requireRight "translated metric" (lowerStep translated curve)
  assertEqual "metric does not relocate samples" (loweredPoints localResult) (loweredPoints translatedResult)
  assertEqual "metric translation preserves bounds" (loweredSpans localResult) (loweredSpans translatedResult)
  assertEqual "receipt retains its metric" identityAffine2 (loweredMetric localResult)

testConic :: IO ()
testConic = do
  policy <- makePolicy (exactHalf * exactHalf) identityAffine2 12 1024
  let quarter = locate (point 1 0)
        (curveStep (rationalQuadratic (vector 0 1) positiveOne positiveTwo) (vector (-1) 1))
  lowered <- requireRight "positive rational circular arc" (lowerStep policy quarter)
  assert "exact conic samples lie on the unit circle"
    (all (\p -> let (x, y) = exactPointCoordinates p in x*x + y*y == 1) (loweredPoints lowered))

testRegion :: IO ()
testRegion = do
  policy <- makePolicy 1 identityAffine2 8 100
  budget <- requireRight "topology budget" (subdivisionBudget 8 256 256)
  let outer = closed (point 0 0) [vector 4 0, vector 0 4, vector (-4) 0]
      hole = closed (point 1 1) [vector 0 1, vector 1 0, vector 0 (-1)]
  (region, receipts, _) <- requireRight "explicit polygon with hole"
    (lowerSimpleRegion policy budget [CurveComponent outer [hole]])
  assertEqual "one receipt per contour" 2 (length receipts)
  assertEqual "outer interior" RegionInterior (regionPointLocation region (point 3 3))
  assertEqual "hole exterior" RegionExterior
    (regionPointLocation region (point (1 + exactHalf) (1 + exactHalf)))
  let crossing = closed (point 0 0) [vector 2 2, vector (-2) 0, vector 2 (-2)]
  assertRefusal "sampled crossing refuses simple region"
    (lowerSimpleRegion policy budget [CurveComponent crossing []])
  assertRefusal "outer winding cannot masquerade as a hole"
    (lowerSimpleRegion policy budget [CurveComponent outer [outer]])

testCertificateAdmission :: IO ()
testCertificateAdmission = do
  policy <- makePolicy 1 identityAffine2 0 1
  lowered <- requireRight "admitted span for public-client probe"
    (lowerStep policy (atOrigin (curveStep line (vector 1 0))))
  case loweredSpans lowered of
    [sample] -> do
      let (observed, forgedField) = Rejected.recordSpanBound sample
      assertEqual "ordinary bound observation remains lawful" 0 observed
      result <- try (evaluate (forgedField `seq` ())) :: IO (Either TypeError ())
      case result of
        Left failure ->
          assert "record capability rejected for the expected reason"
            ("HasField" `isInfixOf` displayException failure
              && "spanSquaredBound" `isInfixOf` displayException failure)
        Right () -> fail "LoweredSpan leaked its record-update capability"
    _ -> fail "one linear step must produce one admitted span"

closed :: ExactPoint -> [ExactVector] -> Located ClosedTrail
closed origin directions = locate origin (closeWith line (openTrail (Seq.fromList (map (curveStep line) directions))))

atOrigin :: CurveStep -> Located CurveStep
atOrigin = locate (point 0 0)

point :: ExactRational -> ExactRational -> ExactPoint
point = exactPoint

vector :: ExactRational -> ExactRational -> ExactVector
vector = ExactVector

makePolicy :: ExactRational -> Affine2 -> Int -> Int -> IO LoweringPolicy
makePolicy epsilon metric depth leaves = do
  tolerance <- requireRight "positive test tolerance" (positiveExact epsilon)
  requireRight "test lowering policy" (loweringPolicy tolerance metric depth leaves)

requireRight :: Show obstruction => String -> Either obstruction value -> IO value
requireRight label = either (fail . ((label <> ": ") <>) . show) pure

assertRefusal :: String -> Either obstruction value -> IO ()
assertRefusal label result = case result of
  Left _ -> pure ()
  Right _ -> fail (label <> ": unexpectedly admitted")

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual = assert
  (label <> ": expected " <> show expected <> ", received " <> show actual)
  (expected == actual)

assert :: String -> Bool -> IO ()
assert label condition = unless condition (fail label)

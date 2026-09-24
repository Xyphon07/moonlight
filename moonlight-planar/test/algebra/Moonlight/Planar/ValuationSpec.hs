-- | Exact intrinsic-volume fixtures and common-subdivision inclusion-exclusion.
module Moonlight.Planar.ValuationSpec (tests) where

import Data.Foldable (traverse_)
import Data.Ratio ((%), numerator, denominator)
import Moonlight.Planar.Exact (ExactPoint, exactPoint, exactRationalNumerator, exactRationalDenominator)
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational
  , PositiveExact
  , exactRational
  , positiveOne
  , positiveTwo
  )
import Moonlight.Planar.Internal.Length
  ( ExactLengthExpression
  , LengthEnclosure
  , LengthError (..)
  , RadicalPrecision
  , RadicalPrecisionError (..)
  , certifiedInterval
  , expressionEnclosure
  , lengthEnclosureLower
  , lengthEnclosureUpper
  , lengthEnclosureWidth
  , normalizeLengthContributions
  , publicationPrecision
  , radicalPrecision
  , radicalPrecisionBits
  , squareRootEnclosure
  )
import Moonlight.Planar.Overlay
  ( overlayClosedIntersection
  , overlayClosedUnion
  , overlayLayers
  , overlaySelectedRegion
  )
import Moonlight.Planar.Region
  ( PlanarRegion
  , emptyPlanarRegion
  , planarRegion
  , planarRegionComponents
  )
import Moonlight.Planar.Valuation
  ( CertifiedInterval (..)
  , ExactPlanarMoments
  , ExactLengthTerm
  , PlanarValuations
  , ValuationError (ValuationCellSetNotPureRegion)
  , cellSetPerimeter
  , cellValuations
  , exactAreaValue
  , exactLengthBounds
  , exactLengthExpression
  , exactLengthTerms
  , exactPlanarFirstX
  , exactPlanarFirstY
  , exactPlanarMeasure
  , exactPlanarSecondXX
  , exactPlanarSecondXY
  , exactPlanarSecondYY
  , eulerCharacteristicValue
  , lengthCoefficient
  , polygonComponentArea
  , polygonComponentMoments
  , orientedBoundaryMoments
  , regionPerimeter
  , regionValuations
  , scaleExactPlanarMoments
  , lengthRadicand
  , valuationArea
  , valuationEuler
  , valuationIntrinsic1
  )
import Moonlight.Planar.OverlayFixtures (singletonLayer)
import Support
  ( annulusRegion, assertEqual, polygonComponentOf, polygonRegion
  , rectangleComponent, rectangleRegion, requireRight
  )

tests :: IO ()
tests = do
  testRegionGoldenValues
  testExactPlanarMoments
  testOrientedMomentRationalOracle
  testClosedCellInclusionExclusion
  testDimensionalCellFixtures
  testMetricInvariance
  testRadicalNormalForm
  testLengthEnclosures
  putStrLn "valuation: ok"

testExactPlanarMoments :: IO ()
testExactPlanarMoments = do
  unit <- rectangleComponent 0 0 1 1
  oneHalfValue <- exactValue 1 2
  oneThird <- exactValue 1 3
  oneQuarter <- exactValue 1 4
  let unitMoments = polygonComponentMoments unit
  assertEqual
    "area is the measure projection"
    (exactPlanarMeasure unitMoments)
    (exactAreaValue (polygonComponentArea unit))
  assertEqual
    "unit-square moments"
    (1, oneHalfValue, oneHalfValue, oneThird, oneQuarter, oneThird)
    (momentDigest unitMoments)

  translated <- rectangleComponent 2 (-1) 4 2
  assertEqual
    "translated-rectangle moments"
    (6, 18, 3, 56, 9, 6)
    (momentDigest (polygonComponentMoments translated))

  annulus <- annulusRegion (0, 0, 3, 3) (1, 1, 2, 2)
  seventyFourThirds <- exactValue 74 3
  let annulusMoments = foldMap polygonComponentMoments (planarRegionComponents annulus)
  assertEqual
    "annulus moments respect clockwise holes"
    (8, 12, 12, seventyFourThirds, 18, seventyFourThirds)
    (momentDigest annulusMoments)
  assertEqual
    "moment monoid and scalar action"
    (momentDigest (unitMoments <> unitMoments))
    (momentDigest (scaleExactPlanarMoments 2 unitMoments))

momentDigest
  :: ExactPlanarMoments
  -> (ExactRational, ExactRational, ExactRational, ExactRational, ExactRational, ExactRational)
momentDigest moments =
  ( exactPlanarMeasure moments
  , exactPlanarFirstX moments
  , exactPlanarFirstY moments
  , exactPlanarSecondXX moments
  , exactPlanarSecondXY moments
  , exactPlanarSecondYY moments
  )

exactValue :: Integer -> Integer -> IO ExactRational
exactValue numeratorValue denominatorValue =
  requireRight "exact valuation fixture" (exactRational numeratorValue denominatorValue)

testRegionGoldenValues :: IO ()
testRegionGoldenValues = do
  assertRegionValuations "empty" emptyPlanarRegion 0 0 []

  unit <- rectangleRegion 0 0 1 1
  assertRegionValuations "unit square" unit 1 1 [(2, 1)]
  unitPerimeter <- requireRight "unit-square perimeter" (regionPerimeter unit)
  assertLength "unit-square perimeter" [(4, 1)] (exactLengthTerms (exactLengthExpression unitPerimeter))
  assertContains "unit-square perimeter bounds" 4 (exactLengthBounds unitPerimeter)

  -- Outer 3×3 square: four edges of length √9 = 3, half-perimeter 6. Hole:
  -- half-perimeter 2. The unnormalized shape 2·√1 + 2·√9 folds to 8.
  annulus <- annulusRegion (0, 0, 3, 3) (1, 1, 2, 2)
  assertRegionValuations "annulus" annulus 0 8 [(8, 1)]

  lowerLeft <- rectangleComponent 0 0 1 1
  upperRight <- rectangleComponent 1 1 2 2
  cornerTouch <- requireRight "corner-touch region" (planarRegion [lowerLeft, upperRight])
  assertRegionValuations "corner-touch squares" cornerTouch 1 2 [(4, 1)]

  right <- rectangleComponent 1 0 2 1
  edgeTouch <- requireRight "edge-touch region" (planarRegion [lowerLeft, right])
  assertRegionValuations "edge-sharing squares" edgeTouch 1 2 [(3, 1)]

  -- The double cell's bottom edge covers the unit cell's top edge from a
  -- shared corner: collinear, positive overlap, shared endpoint. The contact
  -- is one segment (V − E = 2 − 1), so Euler 1 + 1 − 1. Area 1 + 2. Intrinsic
  -- one-volume: half-perimeters 2 + 3 minus the shared unit length, which is
  -- 2·√1 + (1·√1 + 1·√4) − 1·√1 before normalization and 4 after.
  doubleAbove <- rectangleComponent 0 1 2 2
  overlapTouch <-
    requireRight "shared-endpoint overlap region" (planarRegion [lowerLeft, doubleAbove])
  assertRegionValuations "shared-endpoint collinear overlap" overlapTouch 1 3 [(4, 1)]

testClosedCellInclusionExclusion :: IO ()
testClosedCellInclusionExclusion = do
  leftRegion <- rectangleRegion 0 0 1 1
  rightRegion <- rectangleRegion 1 0 2 1
  leftLayer <- singletonLayer False True leftRegion
  rightLayer <- singletonLayer False True rightRegion
  overlay <- requireRight "edge-sharing valuation overlay" (overlayLayers leftLayer rightLayer)
  left <- requireRight "left closed cells" (overlayClosedUnion id (const False) overlay)
  right <- requireRight "right closed cells" (overlayClosedUnion (const False) id overlay)
  union <- requireRight "union closed cells" (overlayClosedUnion id id overlay)
  intersection <-
    requireRight
      "intersection closed cells"
      (overlayClosedIntersection id id overlay)
  leftValues <- requireRight "left valuations" (cellValuations left)
  rightValues <- requireRight "right valuations" (cellValuations right)
  unionValues <- requireRight "union valuations" (cellValuations union)
  sharedEdgeValues <- requireRight "intersection valuations" (cellValuations intersection)
  assertEqual
    "Euler inclusion-exclusion retains the shared edge"
    (1, 1, 1, 1)
    ( eulerCharacteristicValue (valuationEuler leftValues)
    , eulerCharacteristicValue (valuationEuler rightValues)
    , eulerCharacteristicValue (valuationEuler unionValues)
    , eulerCharacteristicValue (valuationEuler sharedEdgeValues)
    )
  assertEqual
    "area inclusion-exclusion retains zero-dimensional measure"
    ( 1
    , 1
    , 2
    , 0
    )
    ( areaExact leftValues
    , areaExact rightValues
    , areaExact unionValues
    , areaExact sharedEdgeValues
    )
  assertLength
    "left intrinsic one-volume"
    [(2, 1)]
    (exactLengthTerms (exactLengthExpression (valuationIntrinsic1 leftValues)))
  assertLength
    "right intrinsic one-volume"
    [(2, 1)]
    (exactLengthTerms (exactLengthExpression (valuationIntrinsic1 rightValues)))
  assertLength
    "union intrinsic one-volume"
    [(3, 1)]
    (exactLengthTerms (exactLengthExpression (valuationIntrinsic1 unionValues)))
  assertLength
    "shared-edge intrinsic one-volume"
    [(1, 1)]
    (exactLengthTerms (exactLengthExpression (valuationIntrinsic1 sharedEdgeValues)))
  case cellSetPerimeter intersection of
    Left ValuationCellSetNotPureRegion -> pure ()
    other ->
      fail
        ( "an isolated selected edge was accepted as a perimeter: "
            <> show other
        )
  published <-
    requireRight
      "published edge-sharing union"
      (overlaySelectedRegion (uncurry (||)) overlay)
  publishedValues <- requireRight "published union valuations" (regionValuations published)
  assertEqual
    "cell and published region valuations agree"
    (valuationDigest unionValues)
    (valuationDigest publishedValues)

testDimensionalCellFixtures :: IO ()
testDimensionalCellFixtures = do
  disjoint <- intersectionValues (0, 0, 1, 1) (2, 0, 3, 1)
  assertEqual
    "empty intersection valuations"
    (0, 0, [])
    (exactDigest disjoint)

  point <- intersectionValues (0, 0, 1, 1) (1, 1, 2, 2)
  assertEqual
    "point intersection valuations"
    (1, 0, [])
    (exactDigest point)

  -- The intersection is the 1×2 rectangle (1,0)–(2,2): half-perimeter 3,
  -- which is 1·√1 + 1·√4 before normalization.
  area <- intersectionValues (0, 0, 2, 2) (1, 0, 3, 2)
  assertEqual
    "two-dimensional intersection valuations"
    (1, 2, [(3, 1)])
    (exactDigest area)

testMetricInvariance :: IO ()
testMetricInvariance = do
  original <- polygonRegion [(0, 0), (1, 0), (0, 1)]
  translated <- polygonRegion [(5, -3), (6, -3), (5, -2)]
  quarterTurned <- polygonRegion [(0, 0), (0, 1), (-1, 0)]
  originalValues <- requireRight "original triangle valuations" (regionValuations original)
  translatedValues <- requireRight "translated triangle valuations" (regionValuations translated)
  quarterTurnedValues <- requireRight "quarter-turned triangle valuations" (regionValuations quarterTurned)
  assertEqual "translation invariance" (exactDigest originalValues) (exactDigest translatedValues)
  assertEqual "quarter-turn invariance" (exactDigest originalValues) (exactDigest quarterTurnedValues)
  perimeter <- requireRight "irrational triangle perimeter" (regionPerimeter original)
  assertContains
    "certified radical perimeter"
    (2 + sqrt 2)
    (exactLengthBounds perimeter)

testRadicalNormalForm :: IO ()
testRadicalNormalForm = do
  -- Diamonds with edge vectors (1, 1) and (2, 2): half-perimeters 2·√2 and
  -- 2·√8 = 4·√2. The square factor of the trial prime 2 folds, and the two
  -- lengths are one term. Areas 2 + 8.
  small <- polygonComponentOf [(0, 1), (1, 0), (2, 1), (1, 2)]
  large <- polygonComponentOf [(4, 2), (6, 0), (8, 2), (6, 4)]
  diamonds <- requireRight "diamond region" (planarRegion [small, large])
  assertRegionValuations "diamonds share one radical" diamonds 2 10 [(6, 2)]

  -- Rhombi with edge vectors (536, 201) and (8, 3): squared lengths
  -- 327697 = 67²·73 and 73. The prime 67 is beyond the trial primes and
  -- 327697 is not a square, so only the class test (327697·73 = 4891²) can
  -- fold 2·√327697 into 134·√73; with the second rhombus's 2·√73 the sum is
  -- 136·√73. Areas 327697 + 73.
  wide <- polygonComponentOf [(0, 0), (536, 201), (335, 737), (-201, 536)]
  narrow <- polygonComponentOf [(1000, 0), (1008, 3), (1005, 11), (997, 8)]
  rhombi <- requireRight "rhombus region" (planarRegion [wide, narrow])
  assertRegionValuations
    "radical classes merge beyond the trial primes"
    rhombi
    2
    327770
    [(136, 73)]

intersectionValues
  :: (Integer, Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> IO PlanarValuations
intersectionValues leftBounds rightBounds = do
  leftRegion <- uncurryRectangle leftBounds
  rightRegion <- uncurryRectangle rightBounds
  leftLayer <- singletonLayer False True leftRegion
  rightLayer <- singletonLayer False True rightRegion
  overlay <- requireRight "dimensional valuation overlay" (overlayLayers leftLayer rightLayer)
  selected <-
    requireRight
      "dimensional closed intersection"
      (overlayClosedIntersection id id overlay)
  requireRight "dimensional cell valuations" (cellValuations selected)

uncurryRectangle
  :: (Integer, Integer, Integer, Integer)
  -> IO PlanarRegion
uncurryRectangle (minimumX, minimumY, maximumX, maximumY) =
  rectangleRegion minimumX minimumY maximumX maximumY

assertRegionValuations
  :: String
  -> PlanarRegion
  -> Int
  -> Integer
  -> [(Integer, Integer)]
  -> IO ()
assertRegionValuations label region expectedEuler expectedArea expectedLength = do
  values <- requireRight label (regionValuations region)
  assertEqual
    (label <> " Euler")
    expectedEuler
    (eulerCharacteristicValue (valuationEuler values))
  assertEqual
    (label <> " area")
    (fromInteger expectedArea)
    (exactAreaValue (valuationArea values))
  assertLength
    (label <> " intrinsic one-volume")
    expectedLength
    (exactLengthTerms (exactLengthExpression (valuationIntrinsic1 values)))

assertLength :: String -> [(Integer, Integer)] -> [ExactLengthTerm] -> IO ()
assertLength label expected actual =
  assertEqual
    label
    [ (fromInteger coefficient, radicand)
    | (coefficient, radicand) <- expected
    ]
    [ (lengthCoefficient term, lengthRadicand term)
    | term <- actual
    ]

assertContains :: String -> Double -> CertifiedInterval -> IO ()
assertContains label expected interval =
  if intervalLower interval <= expected && expected <= intervalUpper interval
    then pure ()
    else fail (label <> ": interval does not contain " <> show expected <> ": " <> show interval)

valuationDigest
  :: PlanarValuations
  -> (Int, ExactRational, CertifiedInterval)
valuationDigest values =
  ( eulerCharacteristicValue (valuationEuler values)
  , exactAreaValue (valuationArea values)
  , exactLengthBounds (valuationIntrinsic1 values)
  )

exactDigest
  :: PlanarValuations
  -> (Int, ExactRational, [(ExactRational, Integer)])
exactDigest values =
  ( eulerCharacteristicValue (valuationEuler values)
  , exactAreaValue (valuationArea values)
  , [ (lengthCoefficient term, lengthRadicand term)
    | term <- exactLengthTerms (exactLengthExpression (valuationIntrinsic1 values))
    ]
  )

areaExact :: PlanarValuations -> ExactRational
areaExact values =
  exactAreaValue (valuationArea values)

-- Independent normalized Rational formulas, deliberately not expressed through
-- the production raw-sum carrier or its homogeneous-integer implementation.
rationalEdgeMoments :: ((Rational, Rational), (Rational, Rational)) -> [Rational]
rationalEdgeMoments ((x0, y0), (x1, y1)) =
  let cross = x0 * y1 - y0 * x1
   in [ cross / 2
      , (x0 + x1) * cross / 6
      , (y0 + y1) * cross / 6
      , (x0 * x0 + x0 * x1 + x1 * x1) * cross / 12
      , (2 * x0 * y0 + x0 * y1 + x1 * y0 + 2 * x1 * y1) * cross / 24
      , (y0 * y0 + y0 * y1 + y1 * y1) * cross / 12
      ]

rationalBoundaryMoments :: [((Rational, Rational), (Rational, Rational))] -> [Rational]
rationalBoundaryMoments = foldr (zipWith (+) . rationalEdgeMoments) [0, 0, 0, 0, 0, 0]

rationalMomentValues :: ExactPlanarMoments -> [Rational]
rationalMomentValues moments =
  map (\value -> exactRationalNumerator value % exactRationalDenominator value)
    [ exactPlanarMeasure moments, exactPlanarFirstX moments, exactPlanarFirstY moments
    , exactPlanarSecondXX moments, exactPlanarSecondXY moments, exactPlanarSecondYY moments
    ]

momentEdgeFamilies :: [(String, [((Rational, Rational), (Rational, Rational))])]
momentEdgeFamilies =
  [ ("integer", [((n, 2*n+1), (n+3, 1-n)) | n <- [-4 .. 3]])
  , ("common denominator", [((n/64, (2*n+1)/64), ((n+3)/64, (1-n)/64)) | n <- [-4 .. 3]])
  , ("mixed coprime denominators", [((n/101, (2*n+1)/103), ((n+3)/107, (1-n)/109)) | n <- [-4 .. 3]])
  , ("high bit width", [(((huge+n)/p, (huge-n)/q), ((huge+2*n+1)/r, (n-huge)/s)) | n <- [-4 .. 3]])
  , ("zero cross", [((n/101, 2*n/101), ((n+3)/107, 2*(n+3)/107)) | n <- [-4 .. 3]])
  ]
 where
  huge = fromInteger (2 ^ (191 :: Int))
  p = fromInteger (2 ^ (127 :: Int) - 1)
  q = fromInteger (2 ^ (107 :: Int) - 1)
  r = fromInteger (2 ^ (89 :: Int) - 1)
  s = fromInteger (2 ^ (61 :: Int) - 1)

admitRationalMomentPoint :: (Rational, Rational) -> IO ExactPoint
admitRationalMomentPoint (x, y) =
  exactPoint <$> exactValue (numerator x) (denominator x)
    <*> exactValue (numerator y) (denominator y)

admitRationalMomentEdge :: ((Rational, Rational), (Rational, Rational)) -> IO (ExactPoint, ExactPoint)
admitRationalMomentEdge (from, to) =
  (,) <$> admitRationalMomentPoint from <*> admitRationalMomentPoint to

testOrientedMomentRationalOracle :: IO ()
testOrientedMomentRationalOracle = do
  traverse_ checkFamily momentEdgeFamilies
  putStrLn "oriented moments: 40 Rational edge oracles, reversal, subdivision and gluing passed"
 where
  checkFamily :: (String, [((Rational, Rational), (Rational, Rational))]) -> IO ()
  checkFamily (label, edges) = do
    admitted <- traverse admitRationalMomentEdge edges
    let actual = orientedBoundaryMoments admitted
    assertEqual (label <> " six independent Rational formulas")
      (rationalBoundaryMoments edges) (rationalMomentValues actual)
    traverse_
      (\(edge, exactEdge) ->
        assertEqual (label <> " individual edge")
          (rationalEdgeMoments edge)
          (rationalMomentValues (orientedBoundaryMoments [exactEdge])))
      (zip edges admitted)
    assertEqual (label <> " orientation reversal")
      (map negate (rationalMomentValues actual))
      (rationalMomentValues (orientedBoundaryMoments (map (\(from, to) -> (to, from)) (reverse admitted))))
    subdivided <- traverse admitRationalMomentEdge
      (concatMap (\(from@(x0,y0), to@(x1,y1)) ->
        let middle = ((x0+x1)/2, (y0+y1)/2)
         in [(from,middle),(middle,to)]) edges)
    assertEqual (label <> " exact midpoint subdivision") actual (orientedBoundaryMoments subdivided)
    let (prefix, suffix) = splitAt 3 admitted
    assertEqual (label <> " finite gluing") actual
      (orientedBoundaryMoments prefix <> orientedBoundaryMoments suffix)
    assertEqual (label <> " empty boundary identity") actual
      (orientedBoundaryMoments [] <> actual)

-- | The shared length owner at admitted precisions. Containment is checked
-- by exact squaring, not against a floating square root.
testLengthEnclosures :: IO ()
testLengthEnclosures = do
  assertEqual "zero precision refused" (Left (NonPositiveRadicalPrecision 0)) (radicalPrecision 0)
  assertEqual "negative precision refused"
    (Left (NonPositiveRadicalPrecision (-3))) (radicalPrecision (-3))
  -- The doubled shift count stays representable: the boundary is admitted,
  -- one past it refused. Neither is used to take a root.
  assertEqual "largest representable precision admitted"
    (Right (maxBound `quot` 2)) (radicalPrecisionBits <$> radicalPrecision (maxBound `quot` 2))
  assertEqual "unrepresentable precision refused"
    (Left (UnrepresentableRadicalPrecision (maxBound `quot` 2 + 1)))
    (radicalPrecisionBits <$> radicalPrecision (maxBound `quot` 2 + 1))
  precisions <- traverse (requireRight "radical precision" . radicalPrecision) [1, 16, 64, 128, 256]
  oneThird <- exactValue 1 3
  nineQuarters <- exactValue 9 4
  threeHalves <- exactValue 3 2
  tiny <- exactValue 1 (2 ^ (80 :: Int))
  let squares = [0, 1, 2, 3, 4, 327697, 10 ^ (40 :: Int) + 1, oneThird, nineQuarters, tiny]
  traverse_ (checkSquare precisions) squares
  traverse_ (\precision -> do
    zeroRoot <- requireRight "zero root" (squareRootEnclosure precision 0)
    assertEqual "zero has a zero-width root" (0, 0) (endpoints zeroRoot)
    exactRoot <- requireRight "exact dyadic root" (squareRootEnclosure precision nineQuarters)
    assertEqual "an exact dyadic square has a zero-width root" (threeHalves, threeHalves) (endpoints exactRoot)
    empty <- requireRight "empty expression" (expressionEnclosure precision (lengthExpression []))
    assertEqual "the empty expression encloses zero exactly" (0, 0) (endpoints empty)
    assertEqual "negative square refused" (Left (LengthNegativeSquare (-1))) (squareRootEnclosure precision (-1))) precisions

  -- Positive coefficients scale each class's root enclosure; the normalizer
  -- folds sqrt 8 onto 2 sqrt 2 before any root is bounded.
  traverse_ (\precision -> do
    rootTwo <- requireRight "root two" (squareRootEnclosure precision 2)
    rootThree <- requireRight "root three" (squareRootEnclosure precision 3)
    let expected =
          ( 2 * lengthEnclosureLower rootTwo + lengthEnclosureLower rootThree
          , 2 * lengthEnclosureUpper rootTwo + lengthEnclosureUpper rootThree )
    scaled <- requireRight "scaled sum"
      (expressionEnclosure precision (lengthExpression [(positiveTwo, 2), (positiveOne, 3)]))
    merged <- requireRight "merged sum"
      (expressionEnclosure precision (lengthExpression [(positiveOne, 8), (positiveOne, 3)]))
    assertEqual "coefficients scale their root enclosures" expected (endpoints scaled)
    assertEqual "square classes merge before rounding" expected (endpoints merged)) precisions

  -- The irrational triangle's perimeter 2 + sqrt 2 at growing precision.
  triangle <- polygonRegion [(0, 0), (1, 0), (0, 1)]
  perimeter <- requireRight "irrational triangle perimeter" (regionPerimeter triangle)
  enclosures <- traverse
    (\precision -> requireRight "perimeter enclosure"
      (expressionEnclosure precision (exactLengthExpression perimeter))) precisions
  traverse_ (\(coarse, fine) -> do
    assertEqual "finer enclosures nest" True (nested fine coarse)
    assertEqual "finer enclosures are no wider" True
      (lengthEnclosureWidth fine <= lengthEnclosureWidth coarse)) (zip enclosures (drop 1 enclosures))
  assertEqual "enclosures contain 2 + sqrt 2" True
    (all (\enclosure -> containsRoot 2 (lengthEnclosureLower enclosure - 2)
      (lengthEnclosureUpper enclosure - 2)) enclosures)
  published <- requireRight "publication enclosure"
    (expressionEnclosure publicationPrecision (exactLengthExpression perimeter))
  assertEqual "valuation publishes the owner's 128-bit enclosure"
    (certifiedInterval published) (exactLengthBounds perimeter)
  assertEqual "binary64 publication is outward" True (outward published)
  traverse_ (\enclosure -> assertEqual "binary64 publication is outward" True (outward enclosure)) enclosures
 where
  endpoints enclosure = (lengthEnclosureLower enclosure, lengthEnclosureUpper enclosure)
  lengthExpression :: [(PositiveExact, ExactRational)] -> ExactLengthExpression
  lengthExpression = normalizeLengthContributions id
  checkSquare :: [RadicalPrecision] -> ExactRational -> IO ()
  checkSquare precisions square = do
    roots <- traverse (\precision -> requireRight "square root" (squareRootEnclosure precision square)) precisions
    assertEqual "roots enclose by exact squaring" True
      (all (\root -> containsRoot square (lengthEnclosureLower root) (lengthEnclosureUpper root)) roots)
    assertEqual "root width is at most one unit in the last place" True
      (and (zipWith (\bits root -> lengthEnclosureWidth root * 2 ^ bits <= 1) [1 :: Int, 16, 64, 128, 256] roots))
    traverse_ (\(coarse, fine) -> assertEqual "root enclosures nest" True (nested fine coarse))
      (zip roots (drop 1 roots))
  nested :: LengthEnclosure -> LengthEnclosure -> Bool
  nested inner outer =
    lengthEnclosureLower outer <= lengthEnclosureLower inner
      && lengthEnclosureUpper inner <= lengthEnclosureUpper outer
  containsRoot :: ExactRational -> ExactRational -> ExactRational -> Bool
  containsRoot square lower upper = lower <= upper && (lower <= 0 || lower * lower <= square)
    && upper >= 0 && square <= upper * upper
  outward :: LengthEnclosure -> Bool
  outward enclosure =
    let interval = certifiedInterval enclosure
     in toRational (intervalLower interval) <= rational (lengthEnclosureLower enclosure)
          && rational (lengthEnclosureUpper enclosure) <= toRational (intervalUpper interval)
  rational :: ExactRational -> Rational
  rational value = exactRationalNumerator value % exactRationalDenominator value

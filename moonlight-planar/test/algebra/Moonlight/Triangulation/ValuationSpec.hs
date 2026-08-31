-- | Exact intrinsic-volume fixtures and common-subdivision inclusion-exclusion.
module Moonlight.Triangulation.ValuationSpec (tests) where

import Moonlight.Triangulation.AlgebraFixtures
  ( annulusRegion
  , insideLayer
  , polygonRegion
  , rectangleComponent
  , rectangleRegion
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , exactRational
  )
import Moonlight.Triangulation.Overlay
  ( overlayClosedIntersection
  , overlayClosedUnion
  , overlayLayers
  , overlaySelectedRegion
  )
import Moonlight.Triangulation.Region
  ( PlanarRegion
  , emptyPlanarRegion
  , planarRegion
  , planarRegionComponents
  )
import Moonlight.Triangulation.Valuation
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
  , regionPerimeter
  , regionValuations
  , scaleExactPlanarMoments
  , squaredLength
  , valuationArea
  , valuationEuler
  , valuationIntrinsic1
  )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = do
  testRegionGoldenValues
  testExactPlanarMoments
  testClosedCellInclusionExclusion
  testDimensionalCellFixtures
  testMetricInvariance
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
exactValue numerator denominator =
  requireRight "exact valuation fixture" (exactRational numerator denominator)

testRegionGoldenValues :: IO ()
testRegionGoldenValues = do
  assertRegionValuations "empty" emptyPlanarRegion 0 0 []

  unit <- rectangleRegion 0 0 1 1
  assertRegionValuations "unit square" unit 1 1 [(2, 1)]
  unitPerimeter <- requireRight "unit-square perimeter" (regionPerimeter unit)
  assertLength "unit-square perimeter" [(4, 1)] (exactLengthTerms (exactLengthExpression unitPerimeter))
  assertContains "unit-square perimeter bounds" 4 (exactLengthBounds unitPerimeter)

  annulus <- annulusRegion (0, 0, 3, 3) (1, 1, 2, 2)
  assertRegionValuations "annulus" annulus 0 8 [(2, 1), (2, 9)]

  lowerLeft <- rectangleComponent 0 0 1 1
  upperRight <- rectangleComponent 1 1 2 2
  cornerTouch <- requireRight "corner-touch region" (planarRegion [lowerLeft, upperRight])
  assertRegionValuations "corner-touch squares" cornerTouch 1 2 [(4, 1)]

  right <- rectangleComponent 1 0 2 1
  edgeTouch <- requireRight "edge-touch region" (planarRegion [lowerLeft, right])
  assertRegionValuations "edge-sharing squares" edgeTouch 1 2 [(3, 1)]

testClosedCellInclusionExclusion :: IO ()
testClosedCellInclusionExclusion = do
  leftRegion <- rectangleRegion 0 0 1 1
  rightRegion <- rectangleRegion 1 0 2 1
  leftLayer <- insideLayer leftRegion
  rightLayer <- insideLayer rightRegion
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

  area <- intersectionValues (0, 0, 2, 2) (1, 0, 3, 2)
  assertEqual
    "two-dimensional intersection valuations"
    ( 1
    , 2
    , [ (1, 1)
      , (1, 4)
      ]
    )
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

intersectionValues
  :: (Integer, Integer, Integer, Integer)
  -> (Integer, Integer, Integer, Integer)
  -> IO PlanarValuations
intersectionValues leftBounds rightBounds = do
  leftRegion <- uncurryRectangle leftBounds
  rightRegion <- uncurryRectangle rightBounds
  leftLayer <- insideLayer leftRegion
  rightLayer <- insideLayer rightRegion
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
    [ (fromInteger coefficient, fromInteger square)
    | (coefficient, square) <- expected
    ]
    [ (lengthCoefficient term, squaredLength term)
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
  -> (Int, ExactRational, [(ExactRational, ExactRational)])
exactDigest values =
  ( eulerCharacteristicValue (valuationEuler values)
  , exactAreaValue (valuationArea values)
  , [ (lengthCoefficient term, squaredLength term)
    | term <- exactLengthTerms (exactLengthExpression (valuationIntrinsic1 values))
    ]
  )

areaExact :: PlanarValuations -> ExactRational
areaExact values =
  exactAreaValue (valuationArea values)

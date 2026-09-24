-- | Exact convex convolution and residual-morphology laws.
module Moonlight.Planar.MinkowskiSpec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Moonlight.Planar.Exact
  ( ExactPoint
  , ExactRational
  , exactPoint
  , exactPointToEmbeddingCandidate
  , exactRational
  )
import Moonlight.Planar.Convex (convexPolygon)
import Moonlight.Planar.Minkowski
  ( MinkowskiOperation (..)
  , closeWith
  , convexMinkowskiSum
  , erodeBy
  , minkowskiGeneratedPieces
  , minkowskiExactCrossings
  , minkowskiOperation
  , minkowskiOverlayPasses
  , minkowskiSum
  , openWith
  , polygonOffset
  , structuringElement
  )
import Moonlight.Planar.Region
  ( PlanarRegion
  , PolygonComponent
  , emptyPlanarRegion
  , exactLoop
  , exactLoopPoints
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  )
import Moonlight.Planar.Point (queryPointValue)
import Moonlight.Planar.Valuation
  ( exactAreaValue
  , regionValuations
  , valuationArea
  )
import Support (annulusRegion, polygonRegion, rectangleRegion, rectangleComponent, assertEqual, requireRight)

tests :: IO ()
tests = do
  testConvexConvolution
  testGeneralAddition
  testOffsetClosure
  testConvexMorphology
  testGeneralErosion
  testHoledAndNeckedErosion
  testTinyExactNonconvexMorphology
  testTinyExactAnnulusMorphology
  testTinyExactDisconnectedMorphology
  testTinyRationalSlantedMorphology
  putStrLn "minkowski: ok"

testConvexConvolution :: IO ()
testConvexConvolution = do
  leftComponent <- rectangleComponent 0 0 1 1
  rightComponent <- rectangleComponent 0 0 1 1
  left <-
    requireRight
      "left convex polygon"
      (convexPolygon (exactLoopPoints (polygonOuterLoop leftComponent)))
  right <-
    requireRight
      "right convex polygon"
      (convexPolygon (exactLoopPoints (polygonOuterLoop rightComponent)))
  let result = convexMinkowskiSum left right
  assertRegionArea "convex square sum" 4 result
  assertEqual
    "convex convolution is commutative after canonical publication"
    result
    (convexMinkowskiSum right left)

testGeneralAddition :: IO ()
testGeneralAddition = do
  first <- rectangleComponent 0 0 1 1
  second <- rectangleComponent 3 0 4 1
  disconnected <- requireRight "disconnected source" (planarRegion [first, second])
  kernel <- rectangleRegion 0 0 1 1
  (sumRegion, receipt) <-
    requireRight "general disconnected sum" (minkowskiSum disconnected kernel)
  assertEqual "general sum operation receipt" MinkowskiAddition (minkowskiOperation receipt)
  assertEqual "general sum generated one piece per source component" 2 (minkowskiGeneratedPieces receipt)
  assertEqual "general sum remains disconnected" 2 (length (planarRegionComponents sumRegion))
  assertRegionArea "general disconnected sum" 8 sumRegion
  (swapped, _) <- requireRight "swapped disconnected sum" (minkowskiSum kernel disconnected)
  assertEqual "general Minkowski sum is commutative" sumRegion swapped
  (annihilated, _) <-
    requireRight "empty Minkowski annihilator" (minkowskiSum disconnected emptyPlanarRegion)
  assertEqual "empty region annihilates Minkowski addition" emptyPlanarRegion annihilated
  firstRegion <- requireRight "first distributive operand" (planarRegion [first])
  secondRegion <- requireRight "second distributive operand" (planarRegion [second])
  (firstSum, _) <- requireRight "first distributed sum" (minkowskiSum firstRegion kernel)
  (secondSum, _) <- requireRight "second distributed sum" (minkowskiSum secondRegion kernel)
  distributed <-
    requireRight
      "distributed union"
      (planarRegion (planarRegionComponents firstSum <> planarRegionComponents secondSum))
  assertEqual "Minkowski addition distributes over disjoint union" distributed sumRegion

  associativityThird <- rectangleRegion (-2) 0 0 1
  (leftPair, _) <- requireRight "associative left pair" (minkowskiSum firstRegion kernel)
  (leftAssociated, _) <-
    requireRight "left-associated Minkowski sum" (minkowskiSum leftPair associativityThird)
  (rightPair, _) <- requireRight "associative right pair" (minkowskiSum kernel associativityThird)
  (rightAssociated, _) <-
    requireRight "right-associated Minkowski sum" (minkowskiSum firstRegion rightPair)
  assertEqual "Minkowski addition is associative" leftAssociated rightAssociated
  concave <-
    polygonRegion
      [ (0, 0), (3, 0), (3, 1), (1, 1), (1, 3), (0, 3) ]
  (concaveSum, concaveReceipt) <-
    requireRight "concave exact sum" (minkowskiSum concave kernel)
  expectedConcaveSum <-
    polygonRegion
      [ (0, 0), (4, 0), (4, 2), (2, 2), (2, 4), (0, 4) ]
  assertEqual "decomposition-independent nonconvex sum" expectedConcaveSum concaveSum
  if minkowskiOverlayPasses concaveReceipt > 0
    then pure ()
    else fail "nonconvex addition bypassed exact piece union"

-- Dilation must be closed over its own exact published image. This fixture
-- used to admit the first offset and then refuse the second when rounded
-- resident wedges were mistaken for exact two-cells.
testOffsetClosure :: IO ()
testOffsetClosure = do
  source <-
    polygonRegion
      [ (-9, -1)
      , (-5, -6)
      , (0, -13)
      , (7, -9)
      , (9, 1)
      , (10, 10)
      , (5, 12)
      , (-7, 8)
      ]
  shoulder <-
    requireRight
      "unit octagon shoulder"
      (exactRational 1592262918131443 2251799813685248)
  kernel <-
    requireRight
      "unit octagon kernel"
      (convexPolygon (centeredOctagon 1 shoulder))
  element <- requireRight "unit octagon element" (structuringElement kernel)
  (firstOffset, _) <-
    requireRight "first closure-regression offset" (polygonOffset element source)
  (secondOffset, secondReceipt) <-
    requireRight
      "offset remains closed over its own published image"
      (polygonOffset element firstOffset)
  assertEqual
    "offset-of-offset remains one full-dimensional component"
    1
    (length (planarRegionComponents secondOffset))
  if minkowskiOverlayPasses secondReceipt > 0
    then pure ()
    else fail "offset closure regression did not exercise overlay descent"

centeredOctagon :: ExactRational -> ExactRational -> NonEmpty ExactPoint
centeredOctagon radius shoulder =
  exactPoint radius 0
    :| [ exactPoint shoulder shoulder
       , exactPoint 0 radius
       , exactPoint (-shoulder) shoulder
       , exactPoint (-radius) 0
       , exactPoint (-shoulder) (-shoulder)
       , exactPoint 0 (-radius)
       , exactPoint shoulder (-shoulder)
       ]

testConvexMorphology :: IO ()
testConvexMorphology = do
  source <- rectangleRegion (-2) (-2) 2 2
  kernelComponent <- rectangleComponent (-1) (-1) 1 1
  kernelPolygon <-
    requireRight
      "centred square kernel"
      (convexPolygon (exactLoopPoints (polygonOuterLoop kernelComponent)))
  element <- requireRight "centred structuring element" (structuringElement kernelPolygon)
  (eroded, erosionReceipt) <- requireRight "convex erosion" (erodeBy element source)
  expectedErosion <- rectangleRegion (-1) (-1) 1 1
  assertEqual "convex support-half-plane erosion" expectedErosion eroded
  assertEqual "convex erosion needs no overlay" 0 (minkowskiOverlayPasses erosionReceipt)
  assertEqual
    "convex erosion reports exact HPI line intersections"
    4
    (minkowskiExactCrossings erosionReceipt)
  exactFitSource <- rectangleRegion (-1) (-1) 1 1
  (lowerDimensionalResidual, _) <-
    requireRight "exact-fit regularized erosion" (erodeBy element exactFitSource)
  assertEqual
    "point-only erosion residual regularizes to the empty polygonal region"
    emptyPlanarRegion
    lowerDimensionalResidual
  (emptyResidual, _) <- requireRight "empty source erosion" (erodeBy element emptyPlanarRegion)
  assertEqual "empty source erodes to empty" emptyPlanarRegion emptyResidual
  (expanded, _) <- requireRight "convex offset" (polygonOffset element source)
  expectedExpansion <- rectangleRegion (-3) (-3) 3 3
  assertEqual "convex offset" expectedExpansion expanded
  (opened, _) <- requireRight "convex opening" (openWith element source)
  assertEqual "convex opening is idempotent on the square" source opened
  (openedTwice, _) <- requireRight "second convex opening" (openWith element opened)
  assertEqual "opening is idempotent" opened openedTwice
  (closed, _) <- requireRight "convex closing" (closeWith element source)
  assertEqual "convex closing is idempotent on the square" source closed
  (closedTwice, _) <- requireRight "second convex closing" (closeWith element closed)
  assertEqual "closing is idempotent" closed closedTwice
  cornerKernelComponent <- rectangleComponent 0 0 1 1
  cornerKernel <-
    requireRight
      "corner-anchored kernel"
      (convexPolygon (exactLoopPoints (polygonOuterLoop cornerKernelComponent)))
  cornerElement <- requireRight "corner-anchored element" (structuringElement cornerKernel)
  cornerSource <- rectangleRegion 0 0 4 4
  (cornerErosion, _) <- requireRight "corner-anchored erosion" (erodeBy cornerElement cornerSource)
  expectedCornerErosion <- rectangleRegion 0 0 3 3
  assertEqual "kernel origin controls erosion translation" expectedCornerErosion cornerErosion

testGeneralErosion :: IO ()
testGeneralErosion = do
  left <- rectangleComponent 0 0 4 4
  right <- rectangleComponent 6 0 10 4
  source <- requireRight "two-component erosion source" (planarRegion [left, right])
  kernelComponent <- rectangleComponent (-1) (-1) 1 1
  kernelPolygon <-
    requireRight
      "general erosion kernel"
      (convexPolygon (exactLoopPoints (polygonOuterLoop kernelComponent)))
  element <- requireRight "general erosion element" (structuringElement kernelPolygon)
  (eroded, receipt) <- requireRight "general erosion" (erodeBy element source)
  expectedLeft <- rectangleComponent 1 1 3 3
  expectedRight <- rectangleComponent 7 1 9 3
  expected <- requireRight "expected general erosion" (planarRegion [expectedLeft, expectedRight])
  assertEqual "general residual erosion" expected eroded
  if minkowskiOverlayPasses receipt > 0
    then pure ()
    else fail "general erosion bypassed the overlay candidate arrangement"

testHoledAndNeckedErosion :: IO ()
testHoledAndNeckedErosion = do
  kernelComponent <- rectangleComponent (-1) (-1) 1 1
  kernelPolygon <-
    requireRight
      "holed erosion kernel"
      (convexPolygon (exactLoopPoints (polygonOuterLoop kernelComponent)))
  element <- requireRight "holed erosion element" (structuringElement kernelPolygon)
  sourceAnnulus <- annulusRegion (0, 0, 10, 10) (4, 4, 6, 6)
  expectedAnnulus <- annulusRegion (1, 1, 9, 9) (3, 3, 7, 7)
  (erodedAnnulus, _) <- requireRight "annulus erosion" (erodeBy element sourceAnnulus)
  assertEqual "erosion shrinks the outer boundary and expands holes" expectedAnnulus erodedAnnulus

  necked <-
    polygonRegion
      [ (0, 0)
      , (8, 0)
      , (8, 3)
      , (12, 3)
      , (12, 0)
      , (20, 0)
      , (20, 8)
      , (12, 8)
      , (12, 5)
      , (8, 5)
      , (8, 8)
      , (0, 8)
      ]
  wideKernelComponent <- rectangleComponent (-2) (-2) 2 2
  wideKernelPolygon <-
    requireRight
      "neck erosion kernel"
      (convexPolygon (exactLoopPoints (polygonOuterLoop wideKernelComponent)))
  wideElement <- requireRight "neck erosion element" (structuringElement wideKernelPolygon)
  (separated, _) <- requireRight "narrow-neck erosion" (erodeBy wideElement necked)
  expectedLeft <- rectangleComponent 2 2 6 6
  expectedRight <- rectangleComponent 14 2 18 6
  expectedSeparated <-
    requireRight "expected separated erosion" (planarRegion [expectedLeft, expectedRight])
  assertEqual "erosion removes a neck narrower than the kernel" expectedSeparated separated

-- Scaling and an invertible rational shear commute with both Minkowski
-- addition and erosion. The expected L boundaries are authored analytically;
-- no production decomposition, clipping, or overlay builds this oracle.
testTinyExactNonconvexMorphology :: IO ()
testTinyExactNonconvexMorphology = do
  epsilon <- requireRight "tiny exact morphology scale" (exactRational 1 (2 ^ (80 :: Int)))
  checkNonconvexAffineMorphology "tiny axis-aligned nonconvex morphology"
    (scaledPoint 1 1 epsilon)
    (scaledPoint 0 0 epsilon)
    (epsilon * epsilon)

testTinyRationalSlantedMorphology :: IO ()
testTinyRationalSlantedMorphology = do
  epsilon <- requireRight "tiny slanted morphology scale" (exactRational 1 (2 ^ (80 :: Int)))
  oneThird <- requireRight "rational shear" (exactRational 1 3)
  oneFifth <- requireRight "rational height scale" (exactRational 1 5)
  let skewPoint :: ExactRational -> ExactRational -> (Integer, Integer) -> ExactPoint
      skewPoint offsetX offsetY (x, y) =
        exactPoint
          (offsetX + epsilon * (fromInteger x + oneThird * fromInteger y))
          (offsetY + epsilon * oneFifth * fromInteger y)
  checkNonconvexAffineMorphology "tiny rational-slanted nonconvex morphology"
    (skewPoint 1 1) (skewPoint 0 0) (epsilon * epsilon * oneFifth)

checkNonconvexAffineMorphology
  :: String
  -> ((Integer, Integer) -> ExactPoint)
  -> ((Integer, Integer) -> ExactPoint)
  -> ExactRational
  -> IO ()
checkNonconvexAffineMorphology label sourcePoint kernelPoint determinant = do
  source <- regionFromMappedPoints sourcePoint
    ((0, 0) :| [(6, 0), (6, 2), (2, 2), (2, 6), (0, 6)])
  kernel <- regionFromMappedPoints kernelPoint (rectangleCoordinates 0 0 1 1)
  assertNoFaithfulBinary64Boundary label source
  expectedSum <- regionFromMappedPoints sourcePoint
    ((0, 0) :| [(7, 0), (7, 3), (3, 3), (3, 7), (0, 7)])
  (summed, _) <- requireRight (label <> " sum") (minkowskiSum source kernel)
  (commuted, _) <- requireRight (label <> " commuted sum") (minkowskiSum kernel source)
  assertEqual (label <> " exact analytic sum boundary") expectedSum summed
  assertEqual (label <> " exact sum commutativity") summed commuted
  assertRegionArea (label <> " exact sum area") (33 * determinant) summed
  polygon <- requireRight (label <> " convex kernel")
    (convexPolygon (fmap kernelPoint (rectangleCoordinates 0 0 1 1)))
  element <- requireRight (label <> " corner-anchored kernel") (structuringElement polygon)
  expectedErosion <- regionFromMappedPoints sourcePoint
    ((0, 0) :| [(5, 0), (5, 1), (1, 1), (1, 5), (0, 5)])
  (eroded, _) <- requireRight (label <> " erosion") (erodeBy element source)
  assertEqual (label <> " exact analytic erosion boundary") expectedErosion eroded
  assertRegionArea (label <> " exact erosion area") (9 * determinant) eroded

testTinyExactAnnulusMorphology :: IO ()
testTinyExactAnnulusMorphology = do
  epsilon <- requireRight "tiny annulus scale" (exactRational 1 (2 ^ (80 :: Int)))
  let sourcePoint = scaledPoint 1 1 epsilon
      kernelPoint = scaledPoint 0 0 epsilon
  annulus <- mappedComponent sourcePoint (rectangleCoordinates 0 0 10 10)
    [NonEmpty.reverse (rectangleCoordinates 3 3 7 7)]
  source <- requireRight "tiny annulus region" (planarRegion [annulus])
  assertNoFaithfulBinary64Boundary "tiny annulus" source
  kernelPolygon <- requireRight "tiny annulus centred kernel"
    (convexPolygon (fmap kernelPoint (rectangleCoordinates (-1) (-1) 1 1)))
  element <- requireRight "tiny annulus structuring element" (structuringElement kernelPolygon)
  expectedDilatedComponent <- mappedComponent sourcePoint (rectangleCoordinates (-1) (-1) 11 11)
    [NonEmpty.reverse (rectangleCoordinates 4 4 6 6)]
  expectedDilated <- requireRight "tiny annulus expected dilation" (planarRegion [expectedDilatedComponent])
  (dilated, _) <- requireRight "tiny annulus exact dilation" (polygonOffset element source)
  assertEqual "tiny annulus dilation shrinks the open hole exactly" expectedDilated dilated
  assertRegionArea "tiny annulus dilated exact area" (140 * epsilon * epsilon) dilated
  expectedErodedComponent <- mappedComponent sourcePoint (rectangleCoordinates 1 1 9 9)
    [NonEmpty.reverse (rectangleCoordinates 2 2 8 8)]
  expectedEroded <- requireRight "tiny annulus expected erosion" (planarRegion [expectedErodedComponent])
  (eroded, _) <- requireRight "tiny annulus exact erosion" (erodeBy element source)
  assertEqual "tiny annulus erosion expands the hole and shrinks the outer loop" expectedEroded eroded
  assertRegionArea "tiny annulus eroded exact area" (28 * epsilon * epsilon) eroded

testTinyExactDisconnectedMorphology :: IO ()
testTinyExactDisconnectedMorphology = do
  epsilon <- requireRight "tiny disconnected scale" (exactRational 1 (2 ^ (80 :: Int)))
  let sourcePoint = scaledPoint 1 1 epsilon
      kernelPoint = scaledPoint 0 0 epsilon
  left <- mappedComponent sourcePoint (rectangleCoordinates 0 0 4 4) []
  right <- mappedComponent sourcePoint (rectangleCoordinates 8 0 12 4) []
  source <- requireRight "tiny disconnected source" (planarRegion [right, left])
  assertNoFaithfulBinary64Boundary "tiny disconnected source" source
  kernelPolygon <- requireRight "tiny disconnected centred kernel"
    (convexPolygon (fmap kernelPoint (rectangleCoordinates (-1) (-1) 1 1)))
  element <- requireRight "tiny disconnected structuring element" (structuringElement kernelPolygon)
  expectedExpandedLeft <- mappedComponent sourcePoint (rectangleCoordinates (-1) (-1) 5 5) []
  expectedExpandedRight <- mappedComponent sourcePoint (rectangleCoordinates 7 (-1) 13 5) []
  expectedExpanded <- requireRight "tiny disconnected expected dilation"
    (planarRegion [expectedExpandedLeft, expectedExpandedRight])
  (expanded, _) <- requireRight "tiny disconnected exact dilation" (polygonOffset element source)
  assertEqual "tiny dilation preserves the exact gap between components" expectedExpanded expanded
  assertEqual "tiny disconnected dilation component count" 2 (length (planarRegionComponents expanded))
  assertRegionArea "tiny disconnected dilated area" (72 * epsilon * epsilon) expanded
  expectedErodedLeft <- mappedComponent sourcePoint (rectangleCoordinates 1 1 3 3) []
  expectedErodedRight <- mappedComponent sourcePoint (rectangleCoordinates 9 1 11 3) []
  expectedEroded <- requireRight "tiny disconnected expected erosion"
    (planarRegion [expectedErodedLeft, expectedErodedRight])
  (eroded, _) <- requireRight "tiny disconnected exact erosion" (erodeBy element source)
  assertEqual "tiny erosion retains both exact components" expectedEroded eroded
  assertRegionArea "tiny disconnected eroded area" (8 * epsilon * epsilon) eroded

scaledPoint :: ExactRational -> ExactRational -> ExactRational -> (Integer, Integer) -> ExactPoint
scaledPoint offsetX offsetY scale (x, y) =
  exactPoint (offsetX + scale * fromInteger x) (offsetY + scale * fromInteger y)

rectangleCoordinates :: Integer -> Integer -> Integer -> Integer -> NonEmpty (Integer, Integer)
rectangleCoordinates minimumX minimumY maximumX maximumY =
  (minimumX, minimumY) :| [(maximumX, minimumY), (maximumX, maximumY), (minimumX, maximumY)]

mappedComponent
  :: ((Integer, Integer) -> ExactPoint)
  -> NonEmpty (Integer, Integer)
  -> [NonEmpty (Integer, Integer)]
  -> IO PolygonComponent
mappedComponent pointAt outer holes = do
  outerLoop <- requireRight "exact morphology outer loop" (exactLoop (fmap pointAt outer))
  holeLoops <- traverse (requireRight "exact morphology hole loop" . exactLoop . fmap pointAt) holes
  requireRight "exact morphology component" (polygonComponent outerLoop holeLoops)

regionFromMappedPoints :: ((Integer, Integer) -> ExactPoint) -> NonEmpty (Integer, Integer) -> IO PlanarRegion
regionFromMappedPoints pointAt points = do
  component <- mappedComponent pointAt points []
  requireRight "exact morphology region" (planarRegion [component])

assertNoFaithfulBinary64Boundary :: String -> PlanarRegion -> IO ()
assertNoFaithfulBinary64Boundary label region = do
  let points = concatMap
        (\component -> concatMap (NonEmpty.toList . exactLoopPoints)
          (polygonOuterLoop component : polygonHoleLoops component))
        (planarRegionComponents region)
  projected <- traverse (requireRight (label <> " optional projection") . exactPointToEmbeddingCandidate) points
  assertEqual (label <> " has genuinely distinct exact boundary points") True (Set.size (Set.fromList points) > 1)
  assertEqual (label <> " cannot use a faithful binary64 boundary") 1
    (Set.size (Set.fromList (fmap queryPointValue projected)))

assertRegionArea :: String -> ExactRational -> PlanarRegion -> IO ()
assertRegionArea label expected region = do
  valuations <- requireRight label (regionValuations region)
  assertEqual
    label
    expected
    (exactAreaValue (valuationArea valuations))

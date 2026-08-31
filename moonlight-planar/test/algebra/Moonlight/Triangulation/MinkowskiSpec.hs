-- | Exact convex convolution and residual-morphology laws.
module Moonlight.Triangulation.MinkowskiSpec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Triangulation.AlgebraFixtures
  ( annulusRegion
  , polygonRegion
  , rectangleComponent
  , rectangleRegion
  )
import Moonlight.Triangulation.Exact (ExactPoint, exactPoint)
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , exactRational
  )
import Moonlight.Triangulation.Minkowski
  ( MinkowskiOperation (..)
  , closeWith
  , convexMinkowskiSum
  , convexPolygon
  , convexPolygonPoints
  , erodeBy
  , minkowskiGeneratedPieces
  , minkowskiExactCrossings
  , minkowskiOperation
  , minkowskiOverlayPasses
  , minkowskiSum
  , openWith
  , polygonOffset
  , reflectConvexPolygon
  , structuringElement
  )
import Moonlight.Triangulation.Region
  ( PlanarRegion
  , emptyPlanarRegion
  , exactLoopPoints
  , planarRegion
  , planarRegionComponents
  , polygonOuterLoop
  )
import Moonlight.Triangulation.Valuation
  ( exactAreaValue
  , regionValuations
  , valuationArea
  )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = do
  testConvexConvolution
  testConvexReflection
  testGeneralAddition
  testOffsetClosure
  testConvexMorphology
  testGeneralErosion
  testHoledAndNeckedErosion
  putStrLn "minkowski: ok"

testConvexReflection :: IO ()
testConvexReflection = do
  triangle <-
    requireRight
      "asymmetric convex triangle"
      (convexPolygon (exactPoint 0 0 :| [exactPoint 3 0, exactPoint 1 2]))
  let reflected = reflectConvexPolygon triangle
  readmitted <-
    requireRight
      "reflected convex polygon preserves counter-clockwise orientation"
      (convexPolygon (convexPolygonPoints reflected))
  assertEqual "reflected carrier remains admissible" reflected readmitted
  assertEqual
    "convex reflection is involutive"
    triangle
    (reflectConvexPolygon reflected)

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
    requireRight "concave triangulated sum" (minkowskiSum concave kernel)
  expectedConcaveSum <-
    polygonRegion
      [ (0, 0), (4, 0), (4, 2), (2, 2), (2, 4), (0, 4) ]
  assertEqual "triangulated nonconvex sum" expectedConcaveSum concaveSum
  if minkowskiOverlayPasses concaveReceipt > 0
    then pure ()
    else fail "nonconvex addition bypassed CDT decomposition and overlay union"

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

assertRegionArea :: String -> Integer -> PlanarRegion -> IO ()
assertRegionArea label expected region = do
  valuations <- requireRight label (regionValuations region)
  assertEqual
    label
    (fromInteger expected)
    (exactAreaValue (valuationArea valuations))

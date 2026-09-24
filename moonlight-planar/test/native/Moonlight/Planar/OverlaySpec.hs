-- | Labelled arrangement construction, normalization, and source provenance.
module Moonlight.Planar.OverlaySpec (tests) where

import Control.Monad ( when )
import Moonlight.Planar.CellSet ( exactCellSetEdgeCount, exactCellSetFaceCount,
  exactCellSetVertexCount )
import Moonlight.Planar.Exact ( exactPointCoordinates, exactRationalDenominator )
import Moonlight.Planar.Overlay ( BoundaryRef(boundaryRefSource),
  OverlayEdgeOrigin(overlayEdgeSources), OverlayReceipt(overlayExactCrossings, overlayInputSegments,
  overlayAtomicEdges, overlayOverlapIntervals), OverlaySourceId(OverlaySourceId),
  OverlayVertex(overlayExactPoint), overlayArrangementEdges, overlayArrangementVertices,
  overlayCells, overlayClosedIntersection, overlayConfusion, overlayLayers, overlayPlanarLayer,
  overlayReceipt, overlaySelectedRegion )
import Moonlight.Planar.OverlayAssertions ( assertOverlayIntegrity, unboundedLoopCount )
import Moonlight.Planar.OverlayFixtures ( singletonLayer )
import Moonlight.Planar.Region ( planarLayerRegions, polygonOuterLoop, exactLoopPoints, planarLayer,
  planarRegion, planarRegionComponents, RegionPublicationError(RegionUnboundedSelection) )
import Moonlight.Planar.Valuation ( exactAreaValue )
import Support ( assertEqual, integerPoint, rectangleComponent, requireRight, rectangleRegion,
  polygonRegion )
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

tests :: IO ()
tests =
  sequence_
    [ testOverlappingSquares
    , testSeamAndOverlapNormalization
    , testNestedAndNonDyadicOverlay
    , testCancelledSeamCrossesLabelTransition
    ]

testOverlappingSquares :: IO ()
testOverlappingSquares = do
  left <- rectangleRegion 0 0 2 2 >>= singletonLayer "left-outside" "left"
  right <- rectangleRegion 1 (-1) 3 1 >>= singletonLayer "right-outside" "right"
  result <- requireRight "overlapping square overlay" (overlayLayers left right)
  assertOverlayIntegrity "overlapping square" result
  let receipt = overlayReceipt result
  assertEqual "overlay source segment count" 8 (overlayInputSegments receipt)
  assertEqual "overlay has two proper crossings" 2 (overlayExactCrossings receipt)
  when (overlayAtomicEdges receipt < 8) $
    fail ("overlay lost atomic edges: " <> show receipt)
  intersection <-
    requireRight
      "closed square intersection"
      (overlayClosedIntersection (== "left") (== "right") result)
  when
    ( exactCellSetVertexCount intersection < 4
        || exactCellSetEdgeCount intersection < 4
        || exactCellSetFaceCount intersection < 1
    )
    (fail "closed intersection omitted incidence closure")
  published <-
    requireRight
      "selected square intersection publication"
      (overlaySelectedRegion (== ("left", "right")) result)
  assertEqual
    "selected square intersection component count"
    1
    (length (planarRegionComponents published))
  assertEqual
    "unbounded selected publication refusal"
    (Left RegionUnboundedSelection)
    (overlaySelectedRegion (const True) result)
  merged <-
    requireRight
      "selected adjacent overlay cells merge"
      (overlaySelectedRegion (\(leftLabel, _) -> leftLabel == "left") result)
  assertEqual
    "selected adjacent overlay cells publish one component"
    [4]
    (map (length . exactLoopPoints . polygonOuterLoop) (planarRegionComponents merged))

testSeamAndOverlapNormalization :: IO ()
testSeamAndOverlapNormalization = do
  first <- rectangleComponent 0 0 1 1
  second <- rectangleComponent 1 0 2 1
  joinedRegion <- requireRight "same-label joined region" (planarRegion [first, second])
  joinedLayer <-
    requireRight
      "same-label joined layer"
      (planarLayer "outside" (Map.singleton "inside" joinedRegion))
  emptyLayer <- requireRight "empty normalization layer" (planarLayer "void" Map.empty)
  seamResult <- requireRight "same-label seam overlay" (overlayLayers joinedLayer emptyLayer)
  assertOverlayIntegrity "same-label seam" seamResult
  assertEqual "joined rectangle has one unbounded boundary cycle" (Just 1) (unboundedLoopCount seamResult)
  assertEqual
    "same-label shared boundary is removed before topology"
    6
    (overlayAtomicEdges (overlayReceipt seamResult))
  seamPublication <- requireRight "publish same-label seam" (overlayPlanarLayer seamResult)
  assertEqual
    "the implicit outside pair is never duplicated as a bounded layer key"
    Nothing
    ( Map.lookup
        ("outside", "void")
        (planarLayerRegions seamPublication)
    )
  assertEqual
    "same-label seam dissolves to the rectangle boundary"
    [4]
    ( map
        (length . exactLoopPoints . polygonOuterLoop)
        ( maybe
            []
            planarRegionComponents
            (Map.lookup ("inside", "void") (planarLayerRegions seamPublication))
        )
    )

  duplicateLeft <- rectangleRegion 0 0 2 2 >>= singletonLayer "left-outside" "left"
  duplicateRight <- rectangleRegion 0 0 2 2 >>= singletonLayer "right-outside" "right"
  duplicateResult <-
    requireRight "cross-operand duplicate boundary overlay" (overlayLayers duplicateLeft duplicateRight)
  assertOverlayIntegrity "duplicate boundary" duplicateResult
  assertEqual
    "duplicate boundaries normalize to one atomic cycle"
    4
    (overlayAtomicEdges (overlayReceipt duplicateResult))
  assertEqual "duplicate atomic support retains both typed source origins" True
    (all
      ((== Set.fromList [OverlaySourceId 0, OverlaySourceId 1])
        . Set.fromList . fmap boundaryRefSource . NonEmpty.toList . overlayEdgeSources . snd)
      (overlayArrangementEdges duplicateResult))

  partialLeft <- rectangleRegion 0 0 3 2 >>= singletonLayer "left-outside" "left"
  partialRight <- rectangleRegion 1 0 4 1 >>= singletonLayer "right-outside" "right"
  partialResult <-
    requireRight "partial collinear overlap" (overlayLayers partialLeft partialRight)
  assertOverlayIntegrity "partial collinear overlap" partialResult
  if overlayOverlapIntervals (overlayReceipt partialResult) > 0
    then pure ()
    else fail "partial collinear overlap emitted no overlap event"

testNestedAndNonDyadicOverlay :: IO ()
testNestedAndNonDyadicOverlay = do
  outer <- rectangleRegion 0 0 4 4 >>= singletonLayer "left-outside" "left"
  inner <- rectangleRegion 1 1 3 3 >>= singletonLayer "right-outside" "right"
  nested <- requireRight "nested overlay" (overlayLayers outer inner)
  assertOverlayIntegrity "nested" nested
  assertEqual "nested overlay unbounded boundary is outermost only" (Just 1) (unboundedLoopCount nested)
  assertEqual
    "nested overlay has unbounded, shell, and intersection cells"
    3
    (length (overlayCells nested))

  leftTriangle <-
    polygonRegion [(0, 0), (4, 0), (0, 4)] >>= singletonLayer "left-outside" "left"
  rightTriangle <-
    polygonRegion [(1, -1), (3, -1), (2, 2)] >>= singletonLayer "right-outside" "right"
  nonDyadic <-
    requireRight "non-dyadic proper-crossing overlay" (overlayLayers leftTriangle rightTriangle)
  assertOverlayIntegrity "non-dyadic proper crossing" nonDyadic
  let hasNonDyadicCoordinate =
        any
          (\(_, vertex) ->
             let (x, y) = exactPointCoordinates (overlayExactPoint vertex)
              in any
                   (not . isPowerOfTwo . exactRationalDenominator)
                   [x, y])
          (overlayArrangementVertices nonDyadic)
  if overlayExactCrossings (overlayReceipt nonDyadic) > 0 && hasNonDyadicCoordinate
    then pure ()
    else fail "proper-crossing overlay lost its non-dyadic exact witness"

-- A cancelled same-source seam crosses a retained label-change support.
-- The resulting event remains a vertex, and each subinterval must transport
-- only the source labels whose transition is present on that support.
testCancelledSeamCrossesLabelTransition :: IO ()
testCancelledSeamCrossesLabelTransition = do
  leftComponent <- rectangleComponent 0 0 2 4
  rightComponent <- rectangleComponent 2 0 4 4
  leftRegion <- requireRight "left label region" (planarRegion [leftComponent])
  rightRegion <- requireRight "right label region" (planarRegion [rightComponent])
  labelled <- requireRight "adjacent distinct labels"
    (planarLayer "void" (Map.fromList [("a", leftRegion), ("b", rightRegion)]))
  lower <- rectangleComponent 1 1 3 2
  upper <- rectangleComponent 1 2 3 3
  maskRegion <- requireRight "mask with cancellable internal seam" (planarRegion [lower, upper])
  mask <- requireRight "same-label mask source" (planarLayer "off" (Map.singleton "on" maskRegion))
  result <- requireRight "cancelled seam across retained transition" (overlayLayers labelled mask)
  assertOverlayIntegrity "cancelled seam across retained transition" result
  masses <- requireRight "interval-wise sparse source transitions" (overlayConfusion result)
  assertEqual "cancellation preserves both labels across all transition intervals"
    (Map.fromList [(("a", "off"), 6), (("a", "on"), 2), (("b", "off"), 6), (("b", "on"), 2)])
    (fmap exactAreaValue masses)
  assertEqual "cancelled seam crossing remains an exact provenance event" True
    (Set.member (integerPoint 2 2)
      (Set.fromList (fmap (overlayExactPoint . snd) (overlayArrangementVertices result))))

isPowerOfTwo :: Integer -> Bool
isPowerOfTwo value =
  value > 0 && value `elem` takeWhile (<= value) (iterate (* 2) 1)

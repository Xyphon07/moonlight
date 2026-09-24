-- | Closed selection, exterior marginals, and lower-dimensional incidence.
module Moonlight.Planar.OverlaySelectionSpec (tests) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.CellSet ( ExactCellSet, exactCellSetEdgeCount, exactCellSetFaceCount,
  exactCellSetVertexCount )
import Moonlight.Planar.Exact ( ExactPoint )
import Moonlight.Planar.Internal.Incidence ( faceBoundaryComponents )
import Moonlight.Planar.Internal.Overlay.Types ( overlayResultIncidence )
import Moonlight.Planar.Overlay ( BoundaryRef(boundaryRefSource), OverlayResult,
  OverlaySelectionError(OverlaySelectionContainsUnboundedCell),
  OverlaySelectionKind(RegularizedDifferenceSelection, ClosedUnionSelection,
  ClosedIntersectionSelection), OverlaySourceId(OverlaySourceId), OverlayVertex(overlayExactPoint,
  overlayVertexOrigin), OverlayVertexOrigin(..), overlayArrangementVertices, overlayCells,
  overlayClosedIntersection, overlayClosedUnion, overlayLayers, overlayRegularizedDifference,
  overlaySelectedRegion )
import Moonlight.Planar.OverlayAssertions ( assertOverlayIntegrity, unboundedLoopCount )
import Moonlight.Planar.OverlayFixtures ( singletonLayer )
import Moonlight.Planar.Region ( exactLoop, polygonComponent, planarLayer, planarRegion,
  planarRegionComponents )
import Moonlight.Planar.Valuation ( cellValuations, eulerCharacteristicValue, exactAreaValue,
  valuationArea, valuationEuler )
import Support ( assertEqual, integerPoint, requireRight, rectangleRegion, annulusRegion )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

tests :: IO ()
tests =
  sequence_
    [ testLowerDimensionalIntersections
    , testSelectorRefusalsAndOperandSwap
    , testExteriorMarginalClosures
    , testNestedVertexContactClosure
    ]

testLowerDimensionalIntersections :: IO ()
testLowerDimensionalIntersections = do
  left <- rectangleRegion 0 0 1 1 >>= singletonLayer "left-outside" "left"
  pointTouching <- rectangleRegion 1 1 2 2 >>= singletonLayer "right-outside" "right"
  pointResult <- requireRight "point-touching overlay" (overlayLayers left pointTouching)
  assertOverlayIntegrity "point-touching" pointResult
  assertEqual
    "point-touching outside is one boundary walk with a repeated contact vertex"
    (Just 1)
    (unboundedLoopCount pointResult)
  pointIntersection <-
    requireRight
      "point-only closed intersection"
      (overlayClosedIntersection (== "left") (== "right") pointResult)
  assertEqual
    "point-only closed intersection retains only its zero-cell"
    (1, 0, 0)
    ( exactCellSetVertexCount pointIntersection
    , exactCellSetEdgeCount pointIntersection
    , exactCellSetFaceCount pointIntersection
    )

  edgeTouching <- rectangleRegion 1 0 2 1 >>= singletonLayer "right-outside" "right"
  edgeResult <- requireRight "edge-touching overlay" (overlayLayers left edgeTouching)
  assertOverlayIntegrity "edge-touching" edgeResult
  edgeIntersection <-
    requireRight
      "edge-only closed intersection"
      (overlayClosedIntersection (== "left") (== "right") edgeResult)
  assertEqual
    "edge-only closed intersection retains its closure"
    (2, 1, 0)
    ( exactCellSetVertexCount edgeIntersection
    , exactCellSetEdgeCount edgeIntersection
    , exactCellSetFaceCount edgeIntersection
    )
  publishedEdgeOnly <-
    requireRight
      "edge-only polygon publication"
      (overlaySelectedRegion (== ("left", "right")) edgeResult)
  assertEqual
    "edge-only selection is not fabricated into a polygon"
    0
    (length (planarRegionComponents publishedEdgeOnly))

testSelectorRefusalsAndOperandSwap :: IO ()
testSelectorRefusalsAndOperandSwap = do
  left <- rectangleRegion 0 0 2 2 >>= singletonLayer "left-outside" "left"
  right <- rectangleRegion 1 (-1) 3 1 >>= singletonLayer "right-outside" "right"
  result <- requireRight "selector refusal overlay" (overlayLayers left right)
  assertSelectionRefusal
    "closed union refuses selected outside cell"
    (OverlaySelectionContainsUnboundedCell ClosedUnionSelection)
    (overlayClosedUnion (== "left-outside") (const False) result)
  assertSelectionRefusal
    "closed intersection refuses selected outside cell"
    (OverlaySelectionContainsUnboundedCell ClosedIntersectionSelection)
    ( overlayClosedIntersection
        (== "left-outside")
        (== "right-outside")
        result
    )
  assertSelectionRefusal
    "regularized difference refuses selected outside cell"
    (OverlaySelectionContainsUnboundedCell RegularizedDifferenceSelection)
    ( overlayRegularizedDifference
        (== "left-outside")
        (== "right")
        result
    )

  swapped <- requireRight "operand-swapped overlay" (overlayLayers right left)
  assertOverlayIntegrity "operand-swapped" swapped
  assertEqual
    "operand swap preserves exact arrangement vertices"
    (Set.fromList (map (overlayExactPoint . snd) (overlayArrangementVertices result)))
    (Set.fromList (map (overlayExactPoint . snd) (overlayArrangementVertices swapped)))
  assertEqual
    "operand swap preserves cells and exchanges labels"
    [ (face, (rightLabel, leftLabel))
    | (face, (leftLabel, rightLabel)) <- overlayCells result
    ]
    (overlayCells swapped)
  assertEqual
    "operand swap exchanges typed vertex provenance"
    (vertexOriginCensus True result)
    (vertexOriginCensus False swapped)

-- A selected exterior marginal need not select the unbounded joint face.
-- Its bounded intersection can include an entire lower-dimensional component
-- that is not in the downward closure of any selected bounded face.
testExteriorMarginalClosures :: IO ()
testExteriorMarginalClosures = do
  square <- rectangleRegion 0 0 4 4 >>= singletonLayer "outside" "inside"
  sameSquare <- requireRight "same-support exterior intersection" (overlayLayers square square)
  boundary <- requireRight "outside closure intersected with inside closure"
    (overlayClosedIntersection (== "outside") (== "inside") sameSquare)
  assertEqual "exterior/interior marginal intersection retains the closed boundary"
    (4, 4, 0)
    (exactCellSetVertexCount boundary, exactCellSetEdgeCount boundary, exactCellSetFaceCount boundary)
  boundaryValues <- requireRight "exterior/interior boundary valuation" (cellValuations boundary)
  assertEqual "closed square boundary has Euler zero and no area"
    (0, 0)
    (eulerCharacteristicValue (valuationEuler boundaryValues), exactAreaValue (valuationArea boundaryValues))

  region <- annulusRegion (0, 0, 4, 4) (1, 1, 3, 3)
  annulus <- requireRight "exterior annulus layer" (planarLayer "outside" (Map.singleton "annulus" region))
  result <- requireRight "annulus exterior against enclosing square" (overlayLayers annulus square)
  selected <- requireRight "bounded exterior disk plus detached outer boundary"
    (overlayClosedIntersection (== "outside") (== "inside") result)
  assertEqual "exterior intersection keeps a face and an independent edge cycle"
    (8, 8, 1)
    (exactCellSetVertexCount selected, exactCellSetEdgeCount selected, exactCellSetFaceCount selected)
  values <- requireRight "exterior disk and boundary valuation" (cellValuations selected)
  assertEqual "disjoint disk plus edge cycle has Euler one and exact cavity area"
    (1, 4)
    (eulerCharacteristicValue (valuationEuler values), exactAreaValue (valuationArea values))

-- Tangency joins the outer and inner boundary walks at a single vertex.
-- The selected open face is cellular, but its closure is not a disk: the
-- repeated contact vertex must survive incidence rather than split the walk.
testNestedVertexContactClosure :: IO ()
testNestedVertexContactClosure = do
  square <- rectangleRegion 0 0 4 4 >>= singletonLayer "outside" "inside"
  diamondLoop <- requireRight "internally tangent diamond loop"
    (exactLoop (integerPoint 0 2 :| [integerPoint 1 1, integerPoint 2 2, integerPoint 1 3]))
  diamondComponent <- requireRight "internally tangent diamond component" (polygonComponent diamondLoop [])
  diamondRegion <- requireRight "internally tangent diamond region" (planarRegion [diamondComponent])
  diamond <- requireRight "internally tangent diamond layer"
    (planarLayer "outside" (Map.singleton "inside" diamondRegion))
  result <- requireRight "nested single-vertex contact overlay" (overlayLayers square diamond)
  assertOverlayIntegrity "nested single-vertex contact" result
  assertEqual "tangent complement retains one raw boundary orbit"
    [1]
    [length (faceBoundaryComponents (overlayResultIncidence result) face)
    | (face, labels) <- overlayCells result, labels == ("inside", "outside")]
  complement <- requireRight "tangent complement exact closure"
    (overlayRegularizedDifference (== "inside") (== "inside") result)
  values <- requireRight "tangent complement exact valuation" (cellValuations complement)
  assertEqual "contact does not fill the diamond hole in the closure"
    (0, 14)
    (eulerCharacteristicValue (valuationEuler values), exactAreaValue (valuationArea values))

assertSelectionRefusal
  :: String
  -> OverlaySelectionError
  -> Either OverlaySelectionError ExactCellSet
  -> IO ()
assertSelectionRefusal label expected actual =
  case actual of
    Left obstruction -> assertEqual label expected obstruction
    Right _ -> fail (label <> ": unbounded selection was truncated")

vertexOriginCensus
  :: Bool
  -> OverlayResult (String, String)
  -> Map.Map ExactPoint (Int, Int, Int, Int)
vertexOriginCensus preserveSides result =
  Map.fromList
    [ ( overlayExactPoint vertex
      , if preserveSides
          then census origin
          else swapCensus (census origin)
      )
    | (_, vertex) <- overlayArrangementVertices result
    , let origin = overlayVertexOrigin vertex
    ]
 where
  census :: OverlayVertexOrigin -> (Int, Int, Int, Int)
  census origin =
    ( length (filter ((== OverlaySourceId 0) . boundaryRefSource) (overlayOriginVertices origin))
    , length (filter ((== OverlaySourceId 1) . boundaryRefSource) (overlayOriginVertices origin))
    , length (filter ((== OverlaySourceId 0) . boundaryRefSource) (overlayOriginEdges origin))
    , length (filter ((== OverlaySourceId 1) . boundaryRefSource) (overlayOriginEdges origin))
    )
  swapCensus :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
  swapCensus (leftVertices, rightVertices, leftEdges, rightEdges) =
    (rightVertices, leftVertices, rightEdges, leftEdges)

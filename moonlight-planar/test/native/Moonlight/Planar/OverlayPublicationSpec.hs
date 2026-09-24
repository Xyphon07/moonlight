-- | Grouped polygon publication, boundary walks, and retained source vertices.
module Moonlight.Planar.OverlayPublicationSpec (tests) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.Internal.Incidence ( incidenceEdgeCount, incidenceFaceCount,
  incidenceVertexCount )
import Moonlight.Planar.Internal.Overlay.Types ( overlayResultIncidence )
import Moonlight.Planar.Overlay ( OverlayVertex(overlayExactPoint), overlayArrangementVertices,
  overlayCells, overlayClosedUnion, overlayLayers, overlayPlanarLayer )
import Moonlight.Planar.OverlayAssertions ( assertOverlayIntegrity, unboundedLoopCount )
import Moonlight.Planar.OverlayFixtures ( singletonLayer )
import Moonlight.Planar.Region ( exactLoop, polygonComponent, PolygonComponent, planarLayerRegions,
  planarLayer, planarRegion, planarRegionComponents )
import Moonlight.Planar.Valuation ( cellValuations, eulerCharacteristicValue, exactAreaValue,
  valuationArea, valuationEuler )
import Support ( assertEqual, integerPoint, rectangleComponent, requireRight, rectangleRegion,
  annulusRegion )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

tests :: IO ()
tests =
  sequence_
    [ testOutsidePairRemainsImplicit
    , testDisconnectedGroupedPublication
    , testMultiwayPointBoundaryCycles
    , testExactBoundaryWithoutDiagonals
    , testCancelledSeamsRetainIsolatedVertex
    ]

testOutsidePairRemainsImplicit :: IO ()
testOutsidePairRemainsImplicit = do
  region <- annulusRegion (0, 0, 4, 4) (1, 1, 3, 3)
  left <-
    requireRight
      "annulus layer"
      (planarLayer "outside" (Map.singleton "annulus" region))
  right <- requireRight "annulus empty layer" (planarLayer "void" Map.empty)
  result <- requireRight "annulus overlay" (overlayLayers left right)
  published <- requireRight "publish annulus exact arrangement" (overlayPlanarLayer result)
  assertEqual
    "bounded cavities remain represented by the implicit outside pair"
    Nothing
    ( Map.lookup
        ("outside", "void")
        (planarLayerRegions published)
    )

testDisconnectedGroupedPublication :: IO ()
testDisconnectedGroupedPublication = do
  first <- rectangleComponent 0 0 1 1
  second <- rectangleComponent 3 0 4 1
  region <- requireRight "two-island region" (planarRegion [first, second])
  left <-
    requireRight
      "two-island layer"
      (planarLayer "outside" (Map.singleton "island" region))
  right <- requireRight "empty right layer" (planarLayer "void" Map.empty)
  result <- requireRight "two-island overlay" (overlayLayers left right)
  assertEqual "two islands give two unbounded boundary cycles" (Just 2) (unboundedLoopCount result)
  published <- requireRight "publish disconnected exact arrangement" (overlayPlanarLayer result)
  let components =
        maybe
          []
          planarRegionComponents
          (Map.lookup ("island", "void") (planarLayerRegions published))
  assertEqual "equal labels group after component descent" 2 (length components)
  reversedRegion <- requireRight "reversed two-island region" (planarRegion [second, first])
  reversedLeft <-
    requireRight
      "reversed two-island layer"
      (planarLayer "outside" (Map.singleton "island" reversedRegion))
  reversedResult <- requireRight "reversed two-island overlay" (overlayLayers reversedLeft right)
  assertEqual
    "component construction order cannot perturb stable cells"
    (overlayCells result)
    (overlayCells reversedResult)

testMultiwayPointBoundaryCycles :: IO ()
testMultiwayPointBoundaryCycles = do
  components <-
    traverse
      triangleComponent
      [ ((0, 0), (2, 0), (1, 1))
      , ((0, 0), (-1, 1), (-2, 0))
      , ((0, 0), (-1, -1), (1, -1))
      ]
  region <- requireRight "three point-touching components" (planarRegion components)
  left <-
    requireRight
      "three point-touching layer"
      (planarLayer "outside" (Map.singleton "inside" region))
  right <- requireRight "empty multiway right layer" (planarLayer "void" Map.empty)
  result <- requireRight "three-way point-touching overlay" (overlayLayers left right)
  assertOverlayIntegrity "three-way point-touching" result
  assertEqual
    "multiway point contact retains one unsplit outside boundary walk"
    (Just 1)
    (unboundedLoopCount result)

testExactBoundaryWithoutDiagonals :: IO ()
testExactBoundaryWithoutDiagonals = do
  left <- rectangleRegion 0 0 2 2 >>= singletonLayer "outside" "inside"
  right <- requireRight "empty exact-boundary layer" (planarLayer "void" Map.empty)
  result <- requireRight "exact boundary-only arrangement" (overlayLayers left right)
  assertOverlayIntegrity "exact boundary-only arrangement" result
  let incidence = overlayResultIncidence result
  assertEqual
    "a square arrangement has no invented diagonal or triangular face"
    (4, 4, 2)
    (incidenceVertexCount incidence, incidenceEdgeCount incidence, incidenceFaceCount incidence)

testCancelledSeamsRetainIsolatedVertex :: IO ()
testCancelledSeamsRetainIsolatedVertex = do
  components <- traverse triangleComponent
    [ ((0, 0), (2, 0), (1, 1))
    , ((2, 0), (2, 2), (1, 1))
    , ((2, 2), (0, 2), (1, 1))
    , ((0, 2), (0, 0), (1, 1))
    ]
  region <- requireRight "four-triangle source fan" (planarRegion components)
  left <- requireRight "same-label source fan" (planarLayer "outside" (Map.singleton "inside" region))
  right <- requireRight "empty fan counterpart" (planarLayer "void" Map.empty)
  result <- requireRight "cancelled source fan seams" (overlayLayers left right)
  assertOverlayIntegrity "cancelled source fan" result
  let incidence = overlayResultIncidence result
  assertEqual "seam cancellation keeps the isolated center provenance vertex"
    (5, 4, 2)
    (incidenceVertexCount incidence, incidenceEdgeCount incidence, incidenceFaceCount incidence)
  assertEqual "source center survives without a manufactured edge" True
    (Set.member (integerPoint 1 1) (Set.fromList (fmap (overlayExactPoint . snd) (overlayArrangementVertices result))))
  cells <- requireRight "closed disk with isolated source vertex" (overlayClosedUnion (== "inside") (const False) result)
  values <- requireRight "isolated-vertex face Euler contribution" (cellValuations cells)
  assertEqual "punctured open face plus its selected isolated point is a disk"
    (1, 4) (eulerCharacteristicValue (valuationEuler values), exactAreaValue (valuationArea values))

triangleComponent
  :: ((Integer, Integer), (Integer, Integer), (Integer, Integer))
  -> IO PolygonComponent
triangleComponent (firstPoint, secondPoint, thirdPoint) = do
  loop <-
    requireRight
      "overlay triangle loop"
      ( exactLoop
          ( uncurry integerPoint firstPoint
              :| [uncurry integerPoint secondPoint, uncurry integerPoint thirdPoint]
          )
      )
  requireRight "overlay triangle component" (polygonComponent loop [])

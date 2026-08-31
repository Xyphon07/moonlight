-- | Focused exact-region authoring and ordinary publication acceptance.
module Moonlight.Triangulation.RegionSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Moonlight.Triangulation
  ( Point (..)
  , buildTriangulation
  , delaunay
  , unitElementDefaults
  )
import Moonlight.Triangulation.CellSet
  ( CellSelectionError (..)
  , closeFaceCellSet
  , exactCellSet
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , exactCellSetVertexCount
  )
import Moonlight.Triangulation.Dcel (outerFace)
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( innerFaces
  , undirectedEdges
  )
import Moonlight.Triangulation.Internal.Region.Publication
  ( labelledPlanarLayerFromExactCoordinates
  )
import Moonlight.Triangulation.FloodFillIterator (BoundaryObstruction (BoundaryPinch))
import Moonlight.Triangulation.NativeSpec
  ( regionFaceSatisfies
  , regionMesh
  , regionMeshFromPoints
  )
import Moonlight.Triangulation.Region
import Support (assertEqual, integerPoint, rectangleComponent, requireRight)

tests :: IO ()
tests = do
  testExactAuthoring
  testOrdinaryPublication
  testExactCellSetAdmission
  testGroupedPublicationFixtures
  testPinchPublicationRefusal

testExactAuthoring :: IO ()
testExactAuthoring = do
  firstComponent <- rectangleComponent 0 0 2 2
  secondComponent <- rectangleComponent 4 0 5 1
  region <- requireRight "disconnected exact region" (planarRegion [firstComponent, secondComponent])
  layer <-
    requireRight
      "disconnected labelled layer"
      (planarLayer "outside" (Map.singleton "land" region))
  assertEqual
    "disconnected exact components remain separate"
    2
    ( maybe
        0
        (length . planarRegionComponents)
        (Map.lookup "land" (planarLayerRegions layer))
    )
  assertEqual
    "interior exact point location"
    RegionInterior
    (regionPointLocation region (integerPoint 1 1))
  assertEqual
    "exterior exact point location"
    RegionExterior
    (regionPointLocation region (integerPoint 3 1))

testOrdinaryPublication :: IO ()
testOrdinaryPublication = do
  built <-
    requireRight
      "ordinary square triangulation"
      ( delaunay
          unitElementDefaults
          (V.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2])
      )
  layer <-
    requireRight
      "ordinary square labelled publication"
      (labelledPlanarLayer "outside" (buildTriangulation built) (const "inside"))
  case
    labelledPlanarLayerFromExactCoordinates
      "outside"
      (buildTriangulation built)
      (\vertex -> Left (RegionCoordinateMissing vertex))
      (const (Right "inside")) of
    Left (RegionCoordinateMissing _) -> pure ()
    other -> fail ("missing exact publication coordinate produced " <> show other)
  assertEqual
    "ordinary publication omits outside label"
    ["inside"]
    (Map.keys (planarLayerRegions layer))
  let components =
        maybe [] planarRegionComponents (Map.lookup "inside" (planarLayerRegions layer))
  assertEqual "ordinary square component count" 1 (length components)
  assertEqual
    "ordinary square drops resident diagonal"
    [4]
    (map (length . exactLoopPoints . polygonOuterLoop) components)

testExactCellSetAdmission :: IO ()
testExactCellSetAdmission = do
  built <-
    requireRight
      "cell-set triangle"
      (delaunay unitElementDefaults (V.fromList [Point 0 0, Point 2 0, Point 0 2]))
  let triangulation = buildTriangulation built
  face <-
    case innerFaces triangulation of
      [singleFace] -> pure singleFace
      faces -> fail ("cell-set triangle faces: " <> show faces)
  closed <- requireRight "closed face cell set" (closeFaceCellSet triangulation [face])
  assertEqual
    "face cell set carries its complete downward closure"
    (3, 3, 1)
    ( exactCellSetVertexCount closed
    , exactCellSetEdgeCount closed
    , exactCellSetFaceCount closed
    )
  case exactCellSet triangulation [] [] [outerFace] of
    Left CellOuterFaceSelected -> pure ()
    _ -> fail "cell set admitted the unbounded outer face"
  edge <-
    case undirectedEdges triangulation of
      firstEdge : _ -> pure firstEdge
      [] -> fail "cell-set triangle has no edge"
  case exactCellSet triangulation [] [edge] [] of
    Left (CellEdgeBoundaryMissing failedEdge _) ->
      assertEqual "edge closure witness" edge failedEdge
    _ -> fail "cell set admitted an edge without its boundary vertices"

testGroupedPublicationFixtures :: IO ()
testGroupedPublicationFixtures = do
  triangle <-
    regionMeshFromPoints
      "published single triangle"
      [Point 0 0, Point 2 0, Point 0 2]
  triangleLayer <-
    requireRight
      "published single triangle layer"
      (labelledPlanarLayer (0 :: Int) triangle (const 1))
  assertComponentShape "published single triangle" 1 3 [] triangleLayer

  concave <- regionMesh "published concave L" 2 2
  let concaveLabel =
        regionFaceSatisfies concave (\(Point x y) -> not (x > 1 && y > 1))
  concaveLayer <-
    requireRight
      "published concave L layer"
      (labelledPlanarLayer False concave concaveLabel)
  assertComponentShape "published concave L" True 6 [] concaveLayer

  annulus <- regionMesh "published annulus" 3 3
  let annulusLabel =
        regionFaceSatisfies annulus
          (\(Point x y) -> not (x > 1 && x < 2 && y > 1 && y < 2))
  annulusLayer <-
    requireRight
      "published annulus layer"
      (labelledPlanarLayer False annulus annulusLabel)
  assertComponentShape "published annulus" True 4 [4] annulusLayer

  twoHoles <- regionMesh "published two holes" 5 3
  let twoHoleLabel =
        regionFaceSatisfies twoHoles $ \(Point x y) ->
          let cell = (floor x :: Int, floor y :: Int)
           in cell /= (1, 1) && cell /= (3, 1)
  twoHoleLayer <-
    requireRight
      "published two-hole layer"
      (labelledPlanarLayer False twoHoles twoHoleLabel)
  assertComponentShape "published two holes" True 4 [4, 4] twoHoleLayer

  disconnected <- regionMesh "published disconnected islands" 3 1
  let islandLabel =
        regionFaceSatisfies disconnected (\(Point x _) -> x < 1 || x > 2)
  disconnectedLayer <-
    requireRight
      "published disconnected layer"
      (labelledPlanarLayer False disconnected islandLabel)
  assertEqual
    "published equal label keeps disconnected components"
    2
    (length (componentsFor True disconnectedLayer))

  islandInHole <- regionMesh "published island in hole" 3 3
  let islandInHoleLabel face =
        if regionFaceSatisfies islandInHole
             (\(Point x y) -> x > 1 && x < 2 && y > 1 && y < 2)
             face
          then (2 :: Int)
          else 1
  nestedLayer <-
    requireRight
      "published island-in-hole layer"
      (labelledPlanarLayer 0 islandInHole islandInHoleLabel)
  assertComponentShape "published shell around island" 1 4 [4] nestedLayer
  assertComponentShape "published island inside hole" 2 4 [] nestedLayer

testPinchPublicationRefusal :: IO ()
testPinchPublicationRefusal = do
  pinched <- regionMesh "published pinch" 3 3
  let selected =
        regionFaceSatisfies pinched $ \(Point x y) ->
          let cell = (floor x :: Int, floor y :: Int)
           in cell /= (0, 0) && cell /= (1, 1)
  case labelledPlanarLayer False pinched selected of
    Left (RegionBoundaryObstruction BoundaryPinch {}) -> pure ()
    other -> fail ("pinched publication produced " <> show other)

assertComponentShape
  :: Ord label
  => String
  -> label
  -> Int
  -> [Int]
  -> PlanarLayer label
  -> IO ()
assertComponentShape label regionLabel expectedOuterVertices expectedHoleVertices layer =
  case componentsFor regionLabel layer of
    [component] -> do
      assertEqual
        (label <> " outer vertices")
        expectedOuterVertices
        (length (exactLoopPoints (polygonOuterLoop component)))
      assertEqual
        (label <> " hole vertices")
        expectedHoleVertices
        (map (length . exactLoopPoints) (polygonHoleLoops component))
    components ->
      fail (label <> ": expected one component, got " <> show (length components))

componentsFor :: Ord label => label -> PlanarLayer label -> [PolygonComponent]
componentsFor label =
  maybe [] planarRegionComponents . Map.lookup label . planarLayerRegions

-- | Resident boundary, exact-region authoring, and publication laws.
module Moonlight.Planar.RegionSpec
  ( tests
  ) where

import Control.Monad ( when )
import Data.Foldable ( toList, traverse_ )
import Data.List ( sort )
import Data.List.NonEmpty ( NonEmpty(..) )
import Moonlight.Planar.BoundaryFixtures ( requireLabelledComponent, requireComponentBoundary,
  assertLoopWinding, assertBoundaryShape )
import Moonlight.Planar.BulkLoad ( delaunay )
import Moonlight.Planar.CellSet ( CellSelectionError(..), closeFaceCellSet, exactCellSet,
  exactCellSetEdgeCount, exactCellSetFaceCount, exactCellSetVertexCount )
import Moonlight.Planar.Dcel ( destination, outerFace, vertexOutgoingEdges, vertexPoint )
import Moonlight.Planar.FloodFillIterator ( BoundaryObstruction(BoundaryPinch,
  BoundaryComponentFaceOutOfRange), componentBoundary, componentBoundaryLoops, faceComponents,
  BoundaryLoop(..), BoundaryOrientation(BoundaryClockwise, BoundaryCounterClockwise), FaceComponent,
  RegionBoundary(regionBoundaryOuterLoop, regionBoundaryHoleLoops) )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( undirectedEdges, innerFaces )
import Moonlight.Planar.Internal.HandleDefs ( FaceId(FaceId) )
import Moonlight.Planar.Internal.Region.Publication ( labelledPlanarLayerFromExactCoordinates )
import Moonlight.Planar.Math ( centroid )
import Moonlight.Planar.MeshFixtures ( requirePointBuild, pointMeshOf )
import Moonlight.Planar.Region ( PolygonComponent, labelledPlanarLayer, planarLayerRegions,
  polygonHoleLoops, polygonOuterLoop, exactLoopPoints, planarLayer, planarRegion,
  planarRegionComponents, regionPointLocation, PlanarLayer, RegionPointLocation(RegionExterior,
  RegionInterior), RegionPublicationError(RegionBoundaryObstruction, RegionCoordinateMissing) )
import Moonlight.Planar.Point (Point(Point, pointX))
import Moonlight.Planar.Types (unitElementDefaults, BuildResult(buildTriangulation), DelaunayTriangulation, Triangulation)
import Support ( assertEqual, requireRight, integerPoint, rectangleComponent )
import qualified Moonlight.Planar.Dcel as Dcel
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V


tests :: IO ()
tests =
  sequence_
    [ testFaceComponentsAndBoundaries
    , testExactAuthoring
    , testOrdinaryPublication
    , testExactCellSetAdmission
    , testGroupedPublicationFixtures
    , testPinchPublicationRefusal
    ]

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
    pointMeshOf
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

testFaceComponentsAndBoundaries :: IO ()
testFaceComponentsAndBoundaries = do
  emptyBuild <- requirePointBuild "empty region components" []
  collinearBuild <-
    requirePointBuild
      "collinear region components"
      [Point 0 0, Point 1 0, Point 2 0]
  assertEqual
    "empty mesh has no face components"
    ([] :: [(Bool, FaceComponent)])
    (faceComponents (buildTriangulation emptyBuild) (const True))
  assertEqual
    "collinear mesh has no face components"
    ([] :: [(Bool, FaceComponent)])
    (faceComponents (buildTriangulation collinearBuild) (const True))

  triangle <-
    pointMeshOf
      "single triangle region"
      [Point 0 0, Point 2 0, Point 0 2]
  triangleBoundary <-
    requireComponentBoundary "single triangle" True triangle (const True)
  assertBoundaryShape "single triangle" triangle 3 [] triangleBoundary

  square <-
    pointMeshOf
      "uniform square region"
      [Point 0 0, Point 2 0, Point 2 2, Point 0 2]
  squareComponent <-
    requireLabelledComponent
      "uniform square component"
      True
      (faceComponents square (const True))
  assertEqual
    "component provenance mismatch is typed before DCEL lookup"
    (Left (BoundaryComponentFaceOutOfRange (FaceId 2) 2))
    (componentBoundary triangle squareComponent)
  assertEqual
    "component loop provenance mismatch is typed before DCEL lookup"
    (Left (BoundaryComponentFaceOutOfRange (FaceId 2) 2))
    (componentBoundaryLoops triangle squareComponent)
  squareBoundary <- requireRight "uniform square boundary" (componentBoundary square squareComponent)
  squareLoops <-
    requireRight
      "uniform square oriented boundary loops"
      (componentBoundaryLoops square squareComponent)
  repeatedSquareBoundary <-
    requireRight "repeated uniform square boundary" (componentBoundary square squareComponent)
  assertEqual "boundary extraction is deterministic" squareBoundary repeatedSquareBoundary
  assertEqual
    "ordinary oriented loops agree with strict boundary"
    (regionBoundaryOuterLoop squareBoundary :| regionBoundaryHoleLoops squareBoundary)
    squareLoops
  assertEqual
    "uniform square drops its Delaunay diagonal"
    (Set.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2])
    (loopPointSet square (regionBoundaryOuterLoop squareBoundary))
  assertBoundaryShape "uniform square" square 4 [] squareBoundary

  let splitComponents =
        faceComponents square (regionFaceSatisfies square (\point -> pointX point < 1))
  assertEqual "split square component count" 2 (length splitComponents)
  traverse_
    (\(_, component) -> do
       boundary <- requireRight "split square boundary" (componentBoundary square component)
       assertBoundaryShape "split square component" square 3 [] boundary)
    splitComponents

  collinearHull <-
    pointMeshOf
      "collinear hull simplification"
      [ Point 0 0
      , Point 1 0
      , Point 2 0
      , Point 2 2
      , Point 0 2
      , Point 1 1
      ]
  collinearBoundary <-
    requireComponentBoundary "collinear hull" True collinearHull (const True)
  assertEqual
    "exact simplification drops a redundant hull site"
    (Set.fromList [Point 0 0, Point 2 0, Point 2 2, Point 0 2])
    (loopPointSet collinearHull (regionBoundaryOuterLoop collinearBoundary))
  assertEqual
    "resident boundary retains every hull edge endpoint"
    (Set.fromList [Point 0 0, Point 1 0, Point 2 0, Point 2 2, Point 0 2])
    (residentLoopPointSet collinearHull (regionBoundaryOuterLoop collinearBoundary))
  assertEqual
    "resident boundary follows exact DCEL edges"
    True
    (residentBoundaryIsEdgeCycle collinearHull (regionBoundaryOuterLoop collinearBoundary))

  lShape <- regionMesh "concave L region" 2 2
  let inL = regionFaceSatisfies lShape (\(Point x y) -> not (x > 1 && y > 1))
  lBoundary <- requireComponentBoundary "concave L" True lShape inL
  assertEqual
    "concave L boundary"
    ( Set.fromList
        [ Point 0 0
        , Point 2 0
        , Point 2 1
        , Point 1 1
        , Point 1 2
        , Point 0 2
        ]
    )
    (loopPointSet lShape (regionBoundaryOuterLoop lBoundary))
  assertBoundaryShape "concave L" lShape 6 [] lBoundary

  annulus <- regionMesh "annulus region" 3 3
  let outsideCenter =
        regionFaceSatisfies annulus (\(Point x y) -> not (x > 1 && x < 2 && y > 1 && y < 2))
  annulusBoundary <-
    requireComponentBoundary "annulus outer" True annulus outsideCenter
  assertBoundaryShape "annulus" annulus 4 [4] annulusBoundary

  twoHoles <- regionMesh "ordered hole regions" 5 3
  let outsideTwoCells =
        regionFaceSatisfies twoHoles $ \(Point x y) ->
          let cell = (floor x :: Int, floor y :: Int)
           in cell /= (1, 1) && cell /= (3, 1)
  twoHoleBoundary <-
    requireComponentBoundary "two-hole outer" True twoHoles outsideTwoCells
  assertBoundaryShape "two-hole" twoHoles 4 [4, 4] twoHoleBoundary

  disconnected <- regionMesh "disconnected equal labels" 3 1
  let outsideMiddle =
        regionFaceSatisfies disconnected (\(Point x _) -> x < 1 || x > 2)
      disconnectedComponents = faceComponents disconnected outsideMiddle
      equalLabelComponents =
        [component | (True, component) <- disconnectedComponents]
  assertEqual "equal labels remain two disconnected components" 2 (length equalLabelComponents)
  assertEqual
    "face component order is deterministic"
    disconnectedComponents
    (faceComponents disconnected outsideMiddle)

  pinched <- regionMesh "pinched region" 3 3
  let pinchedSelection =
        regionFaceSatisfies pinched $ \(Point x y) ->
          let cell = (floor x :: Int, floor y :: Int)
           in cell /= (0, 0) && cell /= (1, 1)
  pinchedComponent <-
    requireLabelledComponent
      "pinched selected component"
      True
      (faceComponents pinched pinchedSelection)
  pinchedLoops <-
    requireRight
      "pinched oriented boundary loops"
      (componentBoundaryLoops pinched pinchedComponent)
  let pinchedLoopList = toList pinchedLoops
  assertEqual "pinched simple loop count" 2 (length pinchedLoopList)
  assertEqual
    "pinched simple loop vertex counts"
    [4, 6]
    (sort (fmap (length . boundaryLoopVertices) pinchedLoopList))
  assertEqual
    "pinched loop orientations"
    [BoundaryCounterClockwise, BoundaryClockwise]
    (sort (fmap boundaryLoopOrientation pinchedLoopList))
  traverse_
    (\loop -> do
       let loopVertexIds = toList (boundaryLoopVertices loop)
       assertEqual
         "pinched loop has no repeated vertex"
         (length loopVertexIds)
         (Set.size (Set.fromList loopVertexIds))
       assertLoopWinding
         "pinched loop winding agrees with orientation"
         (case boundaryLoopOrientation loop of
            BoundaryCounterClockwise -> GT
            BoundaryClockwise -> LT)
         pinched
         loop)
    pinchedLoopList
  case componentBoundary pinched pinchedComponent of
    Left (BoundaryPinch vertex firstEdge secondEdge) -> do
      assertEqual "pinch vertex" (Point 1 1) (vertexPoint pinched vertex)
      when (firstEdge == secondEdge) $
        fail ("pinch repeated one outgoing edge: " <> show firstEdge)
    other -> fail ("pinched boundary produced " <> show other)

regionMesh :: String -> Int -> Int -> IO (DelaunayTriangulation Point)
regionMesh label widthInCells heightInCells =
  pointMeshOf
    label
    [ Point (fromIntegral x) (fromIntegral y)
    | y <- [0 .. heightInCells]
    , x <- [0 .. widthInCells]
    ]

regionFaceCentroid
  :: Triangulation mode vertex directed undirected face
  -> FaceId
  -> Maybe Point
regionFaceCentroid triangulation face =
  (\(first, second, third) ->
     centroid
       (vertexPoint triangulation first)
       (vertexPoint triangulation second)
       (vertexPoint triangulation third))
    <$> Dcel.innerFaceVertices triangulation face

regionFaceSatisfies
  :: Triangulation mode vertex directed undirected face
  -> (Point -> Bool)
  -> FaceId
  -> Bool
regionFaceSatisfies triangulation predicate =
  maybe False predicate . regionFaceCentroid triangulation

loopPointSet
  :: Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> Set.Set Point
loopPointSet triangulation =
  Set.fromList
    . fmap (vertexPoint triangulation)
    . toList
    . boundaryLoopVertices

residentLoopPointSet
  :: Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> Set.Set Point
residentLoopPointSet triangulation =
  Set.fromList
    . fmap (vertexPoint triangulation)
    . toList
    . boundaryLoopResidentVertices

residentBoundaryIsEdgeCycle
  :: Triangulation mode vertex directed undirected face
  -> BoundaryLoop
  -> Bool
residentBoundaryIsEdgeCycle triangulation loop =
  let first :| remaining = boundaryLoopResidentVertices loop
      adjacentPairs = zip (first : remaining) (remaining <> [first])
   in all
        (\(fromVertex, toVertex) ->
           any
             ((== toVertex) . destination triangulation)
             (vertexOutgoingEdges triangulation fromVertex))
        adjacentPairs

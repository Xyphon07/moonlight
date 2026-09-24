{-# LANGUAGE LambdaCase #-}

-- | Resident handles, intersection traversal, interpolation, and Voronoi dual laws.
module Moonlight.Planar.DcelSpec
  ( tests
  ) where

import Control.Monad ( forM_, unless, when )
import Control.Monad.ST ( stToIO )
import Data.Maybe ( isJust )
import Moonlight.Planar.Dcel ( geometryTopologyBytes, innerFaceDirectedEdgeTriples,
  innerFaceVertexTriples, next, numDirectedEdges, numFaces, numInnerFaces, numUndirectedEdges,
  numVertices, origin, previous, topologyIndexBytes, undirectedEndpoints, vertexPoint, vertexPoints
  )
import Moonlight.Planar.FloodFillIterator ( circleMetric, edgesInCircle, verticesInCircle,
  verticesInRectangle, CircleMetricError(InvalidCircleRadius, InvalidCircleCenter) )
import Moonlight.Planar.Handles.Iterators.FixedIterators ( directedEdges, undirectedEdges, vertices,
  innerFaces )
import Moonlight.Planar.Internal.HandleDefs ( VertexId(VertexId), asUndirected, directedPair,
  reverseEdge )
import Moonlight.Planar.Interpolation ( newNaturalNeighborWorkspace, workspaceBytes,
  estimateGradients, foldNaturalNeighborWeights, interpolateNaturalNeighbor,
  interpolateNaturalNeighborGradient, naturalNeighborWeights,
  InterpolationStats(interpolationNaturalNeighbors), NaturalNeighborResult(naturalNeighborValues) )
import Moonlight.Planar.IntersectionIterator ( lineIntersections, Intersection(..) )
import Moonlight.Planar.Math ( segmentDistanceSquared )
import Moonlight.Planar.MeshFixtures ( requirePointBuild )
import Moonlight.Planar.Point (Point(Point), PointValidationError(InvalidPointX, InvalidPointY))
import Moonlight.Planar.Scalar (CoordinateError(CoordinateInfinite, CoordinateNaN), NonFiniteValue(ValueNegativeInfinity, ValueNaN, ValuePositiveInfinity), RadiusSquaredError(NonFiniteRadiusSquared,
  NegativeRadiusSquared))
import Moonlight.Planar.Types (BuildResult(buildTriangulation))
import Moonlight.Planar.Voronoi ( asDelaunayDirectedEdge, directedVoronoiEdges, reverseVoronoiEdge,
  undirectedVoronoiEdges, voronoiEdgeGeometry, voronoiFaceSite, voronoiFaces, voronoiIncidentFace,
  voronoiNext, voronoiPrevious )
import Support ( assertEqual, assertValid, requireQueryPoint, requireRight, requireJust )
import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Handles.Dynamic qualified as Dynamic
import Moonlight.Planar.Handles.Iterators.DynamicIterators qualified as DynamicIterators
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Planar.Voronoi.Handles qualified as VoronoiDynamic


tests :: IO ()
tests =
  sequence_
    [ testHandlesAndFiniteDcel
    , testSibsonInterpolation
    , testVoronoiDual
    , testTraversal
    ]

reverseTraversalEvent :: Intersection -> Intersection
reverseTraversalEvent = \case
  EdgeIntersection edge -> EdgeIntersection (reverseEdge edge)
  EdgeOverlap edge -> EdgeOverlap (reverseEdge edge)
  VertexIntersection vertex -> VertexIntersection vertex

testHandlesAndFiniteDcel :: IO ()
testHandlesAndFiniteDcel = do
  built <- requirePointBuild "handle algebra" [Point 0 0, Point 3 0, Point 0 2, Point 0.4 0.7]
  let triangulation = buildTriangulation built
  assertValid "handle algebra" triangulation
  assertEqual "one outer face" (numInnerFaces triangulation + 1) (numFaces triangulation)
  forM_ (directedEdges triangulation) $ \edge -> do
    assertEqual "double reversal" edge (reverseEdge (reverseEdge edge))
    assertEqual "next/previous" edge (previous triangulation (next triangulation edge))
    assertEqual "previous/next" edge (next triangulation (previous triangulation edge))
  forM_ (undirectedEdges triangulation) $ \edge -> do
    let (forward, backward) = directedPair edge
    assertEqual "pair reversal" backward (reverseEdge forward)
    assertEqual "undirected projection" edge (asUndirected forward)
  assertEqual "vertex iterator" (numVertices triangulation) (length (vertices triangulation))
  assertEqual
    "dense vertex projection"
    (V.fromList (fmap (vertexPoint triangulation) (vertices triangulation)))
    (vertexPoints triangulation)
  assertEqual "edge iterator" (numUndirectedEdges triangulation) (length (undirectedEdges triangulation))
  assertEqual "face iterator" (numInnerFaces triangulation) (length (innerFaces triangulation))
  assertEqual
    "dense inner-face directed-edge projection"
    (Just (V.toList (innerFaceDirectedEdgeTriples triangulation)))
    (traverse (Dcel.innerFaceDirectedEdges triangulation) (innerFaces triangulation))
  assertEqual
    "dense inner-face projection"
    (Just (V.toList (innerFaceVertexTriples triangulation)))
    (traverse (Dcel.innerFaceVertices triangulation) (innerFaces triangulation))
  assertEqual "dynamic vertex iterator" (numVertices triangulation) (length (DynamicIterators.vertexHandles triangulation))
  assertEqual "dynamic edge iterator" (numDirectedEdges triangulation) (length (DynamicIterators.directedEdgeHandles triangulation))
  assertEqual "dynamic face iterator" (numInnerFaces triangulation) (length (DynamicIterators.innerFaceHandles triangulation))
  unless (geometryTopologyBytes triangulation > topologyIndexBytes triangulation) $
    fail "geometry byte accounting omitted coordinates"
  vertex0 <- requireJust "dynamic vertex handle" (Dynamic.vertexHandle triangulation (VertexId 0))
  assertEqual "dynamic vertex fix" (VertexId 0) (Dynamic.fixVertex vertex0)
  assertEqual "dynamic vertex position" (vertexPoint triangulation (VertexId 0)) (Dynamic.vertexHandlePosition vertex0)
  case Dynamic.vertexHandleOutEdge vertex0 of
    Nothing -> fail "connected dynamic vertex has no outgoing edge"
    Just edge -> do
      assertEqual "dynamic edge reversal" (Dynamic.fixDirectedEdge edge) (Dynamic.fixDirectedEdge (Dynamic.directedEdgeReverse (Dynamic.directedEdgeReverse edge)))
      assertEqual "dynamic edge next/previous" (Dynamic.fixDirectedEdge edge) (Dynamic.fixDirectedEdge (Dynamic.directedEdgePrevious (Dynamic.directedEdgeNext edge)))
      let face = Dynamic.directedEdgeFace edge
      if Dynamic.faceIsOuter face
        then pure ()
        else case Dynamic.faceAsInner face of
          Nothing -> fail "non-outer dynamic face did not refine to InnerTag"
          Just inner -> do
            _ <- requireJust "inner dynamic face vertices" (Dynamic.innerFaceVertices inner)
            case Dynamic.innerFaceCircumcenter inner of
              Nothing -> fail "inner dynamic face has no circumcenter"
              Just _ -> pure ()

testSibsonInterpolation :: IO ()
testSibsonInterpolation = do
  built <- requirePointBuild "sibson" (gridPoints 9 9)
  let triangulation = buildTriangulation built
      queryPoint = Point 3.25 4.4
      linear vertex = let Point x y = vertexPoint triangulation vertex in 2 * x - 3 * y + 5
      expected = let Point x y = queryPoint in 2 * x - 3 * y + 5
  query <- requireQueryPoint "Sibson query" queryPoint
  workspace <- stToIO (newNaturalNeighborWorkspace triangulation)
  result <- stToIO (naturalNeighborWeights workspace Nothing query)
  let weights = naturalNeighborValues result
  unless (V.length weights >= 3) $ fail "Sibson query did not discover a natural-neighbor cavity"
  assertNear "Sibson partition" 1.0e-11 1 (V.sum (V.map snd weights))
  unless (V.all ((>= (-1.0e-12)) . snd) weights) $ fail "Sibson produced a negative weight"
  (folded, _, foldedStats) <- stToIO (foldNaturalNeighborWeights
    (\total vertex weight -> total + weight * linear vertex)
    0
    workspace
    Nothing
    query)
  assertNear "allocation-free Sibson fold" 2.0e-9 expected folded
  assertEqual "Sibson fold neighbor count" (V.length weights) (interpolationNaturalNeighbors foldedStats)
  (interpolated, _) <- stToIO (interpolateNaturalNeighbor linear workspace Nothing query)
  case interpolated of
    Nothing -> fail "Sibson interpolation rejected an interior query"
    Just value -> assertNear "Sibson affine precision" 2.0e-9 expected value
  let gradients = estimateGradients linear triangulation
  V.forM_ gradients $ \(gx, gy) -> do
    assertNear "planar gradient x" 1.0e-9 2 gx
    assertNear "planar gradient y" 1.0e-9 (-3) gy
  let gradient (VertexId raw) = gradients V.! fromIntegral raw
  (gradientValue, _) <- stToIO (interpolateNaturalNeighborGradient linear gradient 0.5 workspace Nothing query)
  case gradientValue of
    Nothing -> fail "gradient natural-neighbor interpolation rejected an interior query"
    Just value -> assertNear "gradient affine precision" 2.0e-9 expected value
  unless (workspaceBytes workspace > 0) $ fail "Sibson workspace byte accounting is empty"

testVoronoiDual :: IO ()
testVoronoiDual = do
  built <- requirePointBuild "voronoi" [Point 0 0, Point 2 0, Point 0 2, Point 2 2, Point 1 1]
  let triangulation = buildTriangulation built
  assertEqual "Voronoi face count" (numVertices triangulation) (length (voronoiFaces triangulation))
  assertEqual "directed dual edge count" (numDirectedEdges triangulation) (length (directedVoronoiEdges triangulation))
  assertEqual "undirected dual edge count" (numUndirectedEdges triangulation) (length (undirectedVoronoiEdges triangulation))
  forM_ (directedVoronoiEdges triangulation) $ \edge -> do
    assertEqual "dual double reversal" edge (reverseVoronoiEdge (reverseVoronoiEdge edge))
    assertEqual "dual next/previous" edge (voronoiPrevious triangulation (voronoiNext triangulation edge))
    assertEqual
      "dual face/site"
      (origin triangulation (asDelaunayDirectedEdge edge))
      (voronoiFaceSite (voronoiIncidentFace triangulation edge))
    case voronoiEdgeGeometry triangulation edge of
      Nothing -> fail "valid dual edge has no geometry"
      fixedGeometry@(Just _) -> do
        owning <- requireJust "dynamic Voronoi edge" (VoronoiDynamic.directedVoronoiEdgeHandle triangulation edge)
        assertEqual "fixed/owning dual geometry" fixedGeometry (VoronoiDynamic.voronoiEdgeGeometryH owning)
  case directedVoronoiEdges triangulation of
    [] -> fail "Voronoi test produced no directed dual edge"
    first : _ -> do
      handle <- requireJust "dynamic Voronoi edge" (VoronoiDynamic.directedVoronoiEdgeHandle triangulation first)
      assertEqual "dynamic dual fix" first (VoronoiDynamic.fixDirectedVoronoiEdge handle)
      assertEqual "dynamic dual reversal" first (VoronoiDynamic.fixDirectedVoronoiEdge (VoronoiDynamic.voronoiEdgeReverseH (VoronoiDynamic.voronoiEdgeReverseH handle)))
      assertEqual "dynamic dual/primal conversion" (asDelaunayDirectedEdge first) (Dynamic.fixDirectedEdge (VoronoiDynamic.voronoiEdgeAsDelaunayH handle))
      let dualFace = VoronoiDynamic.voronoiEdgeFaceH handle
      assertEqual "dynamic dual face site" (origin triangulation (asDelaunayDirectedEdge first)) (Dynamic.fixVertex (VoronoiDynamic.voronoiFaceSiteH dualFace))
      let source = VoronoiDynamic.voronoiEdgeFromH handle
      case VoronoiDynamic.voronoiVertexAsDelaunayFaceH source of
        Just inner -> case VoronoiDynamic.voronoiVertexPositionH source of
          Nothing -> fail "inner dynamic Voronoi vertex has no position"
          Just voronoiPosition -> assertEqual "inner dynamic Voronoi position" (Dynamic.innerFaceCircumcenter inner) (Just voronoiPosition)
        Nothing -> unless (isJust (VoronoiDynamic.voronoiVertexAsOuterEdgeH source)) $
          fail "outer dynamic Voronoi vertex has no defining edge"

testTraversal :: IO ()
testTraversal = do
  built <- requirePointBuild "traversal" [Point (-3) 0, Point (-1) (-2), Point (-1) 2, Point 1 (-2), Point 1 2, Point 3 0]
  forwardStart <- requireQueryPoint "forward traversal start" (Point (-4) 0)
  forwardEnd <- requireQueryPoint "forward traversal end" (Point 4 0)
  let triangulation = buildTriangulation built
      forward = lineIntersections triangulation forwardStart forwardEnd
      backward = lineIntersections triangulation forwardEnd forwardStart
  when (null forward) $ fail "ordered line traversal crossed nothing"
  assertEqual
    "reversing the segment reverses crossing order and orientation"
    (map reverseTraversalEvent forward)
    (reverse backward)
  interiorEnd <- requireQueryPoint "outside traversal interior end" (Point 0.25 0.5)
  let outsideToInterior = lineIntersections triangulation forwardStart interiorEnd
      interiorToOutside = lineIntersections triangulation interiorEnd forwardStart
  when (null outsideToInterior) $ fail "outside-to-interior traversal crossed nothing"
  assertEqual
    "endpoint-directed outside traversal preserves order and orientation"
    (map reverseTraversalEvent outsideToInterior)
    (reverse interiorToOutside)
  missFrom <- requireQueryPoint "outside traversal miss from" (Point (-4) 4)
  missTo <- requireQueryPoint "outside traversal miss to" (Point 4 4)
  assertEqual
    "outside segment missing the hull reports no intersections"
    []
    (lineIntersections triangulation missFrom missTo)
  circleEdges <- Set.fromList <$> requireRight "circle edge query" (edgesInCircle triangulation (Point 0 0) 4)
  let bruteCircle = Set.fromList
        [ edge
        | edge <- undirectedEdges triangulation
        , let (a, b) = undirectedEndpoints triangulation edge
        , segmentDistanceSquared (vertexPoint triangulation a) (vertexPoint triangulation b) (Point 0 0) <= 4
        ]
  assertEqual "circle edge flood" bruteCircle circleEdges
  assertEqual
    "negative circle metric refusal"
    (Left (InvalidCircleRadius (NegativeRadiusSquared (-1))))
    (circleMetric (Point 0 0 :: Point) (-1))
  assertEqual
    "negative circle edge-query refusal"
    (Left (InvalidCircleRadius (NegativeRadiusSquared (-1))))
    (edgesInCircle triangulation (Point 0 0) (-1))
  assertEqual
    "negative circle vertex-query refusal"
    (Left (InvalidCircleRadius (NegativeRadiusSquared (-1))))
    (verticesInCircle triangulation (Point 0 0) (-1))
  assertEqual
    "NaN circle metric refusal"
    (Left (InvalidCircleRadius (NonFiniteRadiusSquared ValueNaN)))
    (circleMetric (Point 0 0 :: Point) (0 / 0))
  assertEqual
    "NaN circle center-x refusal"
    (Left (InvalidCircleCenter (InvalidPointX CoordinateNaN)))
    (circleMetric (Point (0 / 0) 0 :: Point) 1)
  assertEqual
    "positive-infinite circle center-y refusal"
    (Left (InvalidCircleCenter (InvalidPointY CoordinateInfinite)))
    (edgesInCircle triangulation (Point 0 (1 / 0)) 1)
  assertEqual
    "negative-infinite circle center-x refusal"
    (Left (InvalidCircleCenter (InvalidPointX CoordinateInfinite)))
    (verticesInCircle triangulation (Point ((-1) / 0) 0) 1)
  assertEqual
    "infinite circle edge-query refusal"
    (Left (InvalidCircleRadius (NonFiniteRadiusSquared ValuePositiveInfinity)))
    (edgesInCircle triangulation (Point 0 0) (1 / 0))
  assertEqual
    "negative-infinite circle vertex-query refusal"
    (Left (InvalidCircleRadius (NonFiniteRadiusSquared ValueNegativeInfinity)))
    (verticesInCircle triangulation (Point 0 0) ((-1) / 0))
  rectangleVertices <-
    Set.fromList
      <$> requireRight
        "rectangle query"
        (verticesInRectangle triangulation (Point (-1.1) (-2.1)) (Point 1.1 2.1))
  let bruteVertices = Set.fromList
        [ vertex
        | vertex <- vertices triangulation
        , let Point x y = vertexPoint triangulation vertex
        , x >= (-1.1), x <= 1.1, y >= (-2.1), y <= 2.1
        ]
  assertEqual "rectangle vertex flood" bruteVertices rectangleVertices

assertNear :: String -> Double -> Double -> Double -> IO ()
assertNear label tolerance expected actual =
  unless (abs (expected - actual) <= tolerance * max 1 (max (abs expected) (abs actual))) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

gridPoints :: Int -> Int -> [Point]
gridPoints width height =
  [ Point (fromIntegral x + jitter x y) (fromIntegral y + jitter y x)
  | y <- [0 .. height - 1]
  , x <- [0 .. width - 1]
  ]
 where
  jitter :: Int -> Int -> Double
  jitter a b = fromIntegral ((a * 17 + b * 31) `mod` 11) * 1.0e-5

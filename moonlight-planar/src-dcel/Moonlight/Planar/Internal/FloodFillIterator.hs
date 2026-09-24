{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Shape queries and face flood fills over immutable triangulations.
module Moonlight.Planar.Internal.FloodFillIterator
  ( DistanceMetric (..)
  , CircleMetric
  , CircleMetricError (..)
  , RectangleMetric
  , RectangleMetricError (..)
  , circleMetric
  , rectangleMetric
  , edgesInShape
  , verticesInShape
  , edgesInCircle
  , verticesInCircle
  , edgesInRectangle
  , verticesInRectangle
  , floodFillFaces
  , floodFillFacesWithRejectedEdges
  , outerFaceFloodFill
  , facesAtEvenBarrierDepth
  , FaceComponent
  , faceComponentFaces
  , BoundaryOrientation (..)
  , BoundaryLoop
  , boundaryLoopOrientation
  , boundaryLoopResidentVertices
  , boundaryLoopVertices
  , RegionBoundary
  , regionBoundaryOuterLoop
  , regionBoundaryHoleLoops
  , BoundaryObstruction (..)
  , faceComponents
  , faceComponentsBy
  , labelledRegionBoundaries
  , componentBoundaryLoops
  , componentBoundary
  , componentBoundaryForFaces
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (partition, unfoldr)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Moonlight.Planar.Dcel
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Handles.Iterators.FixedIterators (undirectedEdges)
import Moonlight.Planar.Internal.BoundaryCycle
  ( simplifyBoundaryCycle
  , traceOrientedBoundaryCircuits
  )
import Moonlight.Planar.Internal.Predicates
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.Math (segmentDistanceSquaredWide, squaredDistanceWide)
import Moonlight.Planar.PointLocation
import Moonlight.Planar.Types
import Moonlight.Planar.Point (Point (..), QueryPoint, queryPointValue, PointValidationError (..))
import Moonlight.Planar.Scalar (RadiusSquared, RadiusSquaredError (..), mkRadiusSquared, radiusSquaredValue)

-- | One non-empty connected set of equally labelled bounded face indices.
newtype FaceComponent = FaceComponent IntSet.IntSet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Component faces in ascending DCEL order.
faceComponentFaces :: FaceComponent -> [FaceId]
faceComponentFaces (FaceComponent faces) =
  fmap (FaceId . fromIntegral) (IntSet.toAscList faces)

-- | Winding carried explicitly by a simple boundary loop.
data BoundaryOrientation
  = BoundaryCounterClockwise
  | BoundaryClockwise
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | One non-empty simple boundary loop. Counter-clockwise loops contribute
-- filled area under nonzero winding; clockwise loops subtract holes.
data BoundaryLoop = BoundaryLoop
  { boundaryLoopOrientation :: !BoundaryOrientation
  -- | Exact resident DCEL boundary chain. Consecutive vertices, including
  -- the closing pair, are endpoints of one boundary half-edge.
  , boundaryLoopResidentVertices :: !(NonEmpty VertexId)
  -- | Collinearity-reduced polygon observation of the same resident chain.
  , boundaryLoopVertices :: !(NonEmpty VertexId)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The authoritative boundary of one face component.
data RegionBoundary = RegionBoundary
  { regionBoundaryOuterLoop :: !BoundaryLoop
  , regionBoundaryHoleLoops :: ![BoundaryLoop]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Typed failure to descend boundary half-edges or project their oriented
-- loops into one strict polygon component.
data BoundaryObstruction
  = BoundaryComponentFaceOutOfRange !FaceId {-# UNPACK #-} !Int
  | BoundaryPinch !VertexId !DirectedEdgeId !DirectedEdgeId
  | BoundaryCycleDidNotClose !DirectedEdgeId !DirectedEdgeId
  | BoundaryLoopDegenerate ![VertexId]
  | BoundaryOuterLoopCardinality !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Complete boundary edges refined by whether each vertex has one successor.
data BoundaryGraph
  = SimpleBoundaryGraph !IntSet.IntSet !(IntMap.IntMap DirectedEdgeId)
  | PinchedBoundaryGraph
      !IntSet.IntSet
      !VertexId
      !DirectedEdgeId
      !DirectedEdgeId

-- | A query shape that can admit points, test edges, and supply a location
-- seed.
class DistanceMetric metric where
  metricContainsPoint :: metric -> Point -> Bool
  metricIntersectsEdge :: metric -> Point -> Point -> Bool
  metricStartPoint :: metric -> QueryPoint

-- | An admitted center and squared radius.
data CircleMetric = CircleMetric !QueryPoint !RadiusSquared
  deriving stock (Eq, Ord, Show)

-- | Typed refusal for an invalid circle query.
data CircleMetricError
  = InvalidCircleCenter !PointValidationError
  | InvalidCircleRadius !RadiusSquaredError
  deriving stock (Eq, Ord, Show)

-- | Admitted lower corner, upper corner, and center of an axis-aligned box.
data RectangleMetric = RectangleMetric !(QueryPoint) !(QueryPoint) !(QueryPoint)
  deriving stock (Eq, Ord, Show)

-- | Typed refusal for an invalid rectangle query.
data RectangleMetricError
  = InvalidRectangleLower !PointValidationError
  | InvalidRectangleUpper !PointValidationError
  | InvalidRectangleCenter !PointValidationError
  deriving stock (Eq, Ord, Show)

-- | A circle metric, or why the radius is unusable.
circleMetric :: Point -> Double -> Either CircleMetricError CircleMetric
circleMetric center radiusSquared =
  CircleMetric
    <$> first InvalidCircleCenter (mkQueryPoint center)
    <*> first InvalidCircleRadius (mkRadiusSquared radiusSquared)

-- | An axis-aligned rectangle metric, or why the corners are unusable.
rectangleMetric :: Point -> Point -> Either RectangleMetricError RectangleMetric
rectangleMetric lower@(Point lowerX lowerY) upper@(Point upperX upperY) = do
  queryLower <- either (Left . InvalidRectangleLower) Right (mkQueryPoint lower)
  queryUpper <- either (Left . InvalidRectangleUpper) Right (mkQueryPoint upper)
  queryCenter <-
    either
      (Left . InvalidRectangleCenter)
      Right
      (mkQueryPoint (Point ((lowerX + upperX) * 0.5) ((lowerY + upperY) * 0.5)))
  Right (RectangleMetric queryLower queryUpper queryCenter)

instance DistanceMetric CircleMetric where
  metricContainsPoint (CircleMetric center radius) point =
    squaredDistanceWide (queryPointValue center) point <= radiusSquaredValue radius
  metricIntersectsEdge (CircleMetric center radius) from to =
    segmentDistanceSquaredWide from to (queryPointValue center) <= radiusSquaredValue radius
  metricStartPoint (CircleMetric center _) = center

instance DistanceMetric RectangleMetric where
  metricContainsPoint (RectangleMetric lower upper _) (Point x y) =
    lowerX <= upperX && lowerY <= upperY && x >= lowerX && x <= upperX && y >= lowerY && y <= upperY
   where
    Point lowerX lowerY = queryPointValue lower
    Point upperX upperY = queryPointValue upper
  metricIntersectsEdge rectangle from to =
    metricContainsPoint rectangle from
      || metricContainsPoint rectangle to
      || segmentRectangleIntersection rectangle from to
  metricStartPoint (RectangleMetric _ _ center) = center

-- | Edges meeting a circle.
edgesInCircle :: Triangulation mode vertex directed undirected face -> Point -> Double -> Either CircleMetricError [UndirectedEdgeId]
edgesInCircle triangulation center radiusSquared =
  edgesInShape triangulation <$> circleMetric center radiusSquared

-- | Vertices inside a circle.
verticesInCircle :: Triangulation mode vertex directed undirected face -> Point -> Double -> Either CircleMetricError [VertexId]
verticesInCircle triangulation center radiusSquared =
  verticesInShape triangulation <$> circleMetric center radiusSquared

-- | Edges meeting an axis-aligned rectangle.
edgesInRectangle :: Triangulation mode vertex directed undirected face -> Point -> Point -> Either RectangleMetricError [UndirectedEdgeId]
edgesInRectangle triangulation lower upper = edgesInShape triangulation <$> rectangleMetric lower upper

-- | Vertices inside an axis-aligned rectangle.
verticesInRectangle :: Triangulation mode vertex directed undirected face -> Point -> Point -> Either RectangleMetricError [VertexId]
verticesInRectangle triangulation lower upper = verticesInShape triangulation <$> rectangleMetric lower upper

-- | Edges meeting any metric shape.
edgesInShape :: DistanceMetric metric => Triangulation mode vertex directed undirected face -> metric -> [UndirectedEdgeId]
edgesInShape triangulation metric
  | numVertices triangulation <= 1 = []
  | numInnerFaces triangulation == 0 =
      [edge | edge <- undirectedEdges triangulation, edgeInside edge]
  | otherwise =
      let starts = shapeStartFaces triangulation metric
          (_, accepted, _) = floodFillFacesWithEdges triangulation starts edgeInside
       in map (UndirectedEdgeId . fromIntegral) (IntSet.toAscList accepted)
 where
  edgeInside edge =
    let (fromVertex, toVertex) = undirectedEndpoints triangulation edge
     in metricIntersectsEdge metric (vertexPoint triangulation fromVertex) (vertexPoint triangulation toVertex)

-- | Vertices inside any metric shape.
verticesInShape :: DistanceMetric metric => Triangulation mode vertex directed undirected face -> metric -> [VertexId]
verticesInShape triangulation metric =
  [ vertex
  | vertex <- candidateVertices
  , metricContainsPoint metric (vertexPoint triangulation vertex)
  ]
 where
  edges = edgesInShape triangulation metric
  set = List.foldl' addEndpoints IntSet.empty edges
  addEndpoints acc edge =
    let (from, to) = undirectedEndpoints triangulation edge
     in IntSet.insert (vertexIdIndex from) (IntSet.insert (vertexIdIndex to) acc)
  candidateVertices
    | numVertices triangulation == 1 = [VertexId 0]
    | otherwise = map (VertexId . fromIntegral) (IntSet.toAscList set)

-- | Reach inner faces from the supplied seeds by crossing only admitted edges.
floodFillFaces
  :: Triangulation mode vertex directed undirected face -> [FaceId]
  -> (UndirectedEdgeId -> Bool)
  -> [FaceId]
floodFillFaces triangulation starts canCross =
  let (faces, _, _) = floodFillFacesWithEdges triangulation starts canCross
   in faces

-- | The reached section and the edge pairs whose crossing was rejected while
-- descending it. This package-internal observation retains the boundary
-- witness instead of requiring a later face-wide rediscovery.
floodFillFacesWithRejectedEdges
  :: Triangulation mode vertex directed undirected face -> [FaceId]
  -> (UndirectedEdgeId -> Bool)
  -> ([FaceId], [UndirectedEdgeId])
floodFillFacesWithRejectedEdges triangulation starts canCross =
  let (faces, _, rejected) = floodFillFacesWithEdges triangulation starts canCross
   in (faces, fmap (UndirectedEdgeId . fromIntegral) (IntSet.toAscList rejected))

floodFillFacesWithEdges
  :: Triangulation mode vertex directed undirected face -> [FaceId]
  -> (UndirectedEdgeId -> Bool)
  -> ([FaceId], IntSet.IntSet, IntSet.IntSet)
floodFillFacesWithEdges triangulation starts canCross =
  let (faces, accepted, rejected) = go initialStack initialVisited IntSet.empty IntSet.empty []
   in (reverse faces, accepted, rejected)
 where
  valid face = face /= outerFace && faceIdIndex face < numFaces triangulation
  (initialStack, initialVisited) = List.foldl' enqueueStart ([], IntSet.empty) starts

  enqueueStart state face
    | valid face = enqueue face state
    | otherwise = state

  go [] _ accepted rejected result = (result, accepted, rejected)
  go (face : stack) visited accepted rejected result =
    let (stack', visited', accepted', rejected') =
          foldFaceDirectedEdges'
            triangulation
            face
            expand
            (stack, visited, accepted, rejected)
     in go stack' visited' accepted' rejected' (face : result)

  expand (stack, visited, accepted, rejected) edge =
    let undirected = asUndirected edge
        edgeIndex = undirectedEdgeIdIndex undirected
        adjacent = incidentFace triangulation (reverseEdge edge)
        edgeAdmission
          | IntSet.member edgeIndex accepted = (True, accepted, rejected)
          | IntSet.member edgeIndex rejected = (False, accepted, rejected)
          | canCross undirected = (True, IntSet.insert edgeIndex accepted, rejected)
          | otherwise = (False, accepted, IntSet.insert edgeIndex rejected)
        (crosses, accepted', rejected') = edgeAdmission
        (stack', visited') =
          if crosses && valid adjacent
            then enqueue adjacent (stack, visited)
            else (stack, visited)
     in (stack', visited', accepted', rejected')

  enqueue face (stack, visited)
    | IntSet.member index visited = (stack, visited)
    | otherwise = (face : stack, IntSet.insert index visited)
   where
    index = faceIdIndex face

-- | Connected components of equally labelled bounded faces. Labels are
-- evaluated once; the component carrier is the same 'IntSet' used by descent.
faceComponents
  :: Eq label
  => Triangulation mode vertex directed undirected face
  -> (FaceId -> label)
  -> [(label, FaceComponent)]
faceComponents triangulation labelFace =
  faceComponentsFromLabels triangulation labelAt (const True)
 where
  labels =
    V.generate
      (numInnerFaces triangulation)
      (\index -> labelFace (FaceId (fromIntegral (index + 1))))
  labelAt faceIndex = labels V.!? (faceIndex - 1)

-- | Connected components over a selected face section and admitted adjacency.
-- A missing label removes a face from the section; the edge predicate states
-- which overlaps glue. This is the common descent used by ordinary labelled
-- regions and exact-overlay charts whose representation diagonals alone may
-- connect collapsed resident faces.
faceComponentsBy
  :: Eq label
  => Triangulation mode vertex directed undirected face
  -> (FaceId -> Maybe label)
  -> (UndirectedEdgeId -> Bool)
  -> [(label, FaceComponent)]
faceComponentsBy triangulation labelFace =
  faceComponentsFromLabels triangulation labelAt
 where
  labels =
    V.generate
      (numInnerFaces triangulation)
      (\index -> labelFace (FaceId (fromIntegral (index + 1))))
  labelAt faceIndex = labels V.!? (faceIndex - 1) >>= id

faceComponentsFromLabels
  :: Eq label
  => Triangulation mode vertex directed undirected face
  -> (Int -> Maybe label)
  -> (UndirectedEdgeId -> Bool)
  -> [(label, FaceComponent)]
faceComponentsFromLabels triangulation labelAt canCross =
  unfoldr descend initialUnvisited
 where
  initialUnvisited =
    IntSet.fromRange (1, numFaces triangulation - 1)

  descend remaining =
    case IntSet.minView remaining of
      Nothing -> Nothing
      Just (seedIndex, unseeded) ->
        case labelAt seedIndex of
          Nothing -> descend unseeded
          Just componentLabel ->
            let unvisited =
                  collectComponent componentLabel (Seq.singleton seedIndex) unseeded
                componentFaces = IntSet.difference remaining unvisited
             in Just ((componentLabel, FaceComponent componentFaces), unvisited)

  collectComponent componentLabel queued unvisited =
    case Seq.viewl queued of
      Seq.EmptyL -> unvisited
      faceIndex Seq.:< remainingQueue ->
        let face = FaceId (fromIntegral faceIndex)
            (expandedQueue, remainingUnvisited) =
              foldFaceDirectedEdges'
                triangulation
                face
                (admitAdjacent componentLabel)
                (remainingQueue, unvisited)
         in collectComponent
              componentLabel
              expandedQueue
              remainingUnvisited

  admitAdjacent componentLabel (queued, unvisited) edge =
    if
      canCross (asUndirected edge)
        && adjacent /= outerFace
        && IntSet.member adjacentIndex unvisited
        && labelAt adjacentIndex == Just componentLabel
      then
        ( queued Seq.|> adjacentIndex
        , IntSet.delete adjacentIndex unvisited
        )
      else (queued, unvisited)
   where
    adjacent = incidentFace triangulation (reverseEdge edge)
    adjacentIndex = faceIdIndex adjacent
{-# INLINE faceComponentsFromLabels #-}

-- | Descend every equally labelled bounded-face component through the one
-- authoritative boundary tracer. Components are converted independently;
-- callers may group equal labels only after this descent has succeeded.
labelledRegionBoundaries
  :: Eq label
  => Triangulation mode vertex directed undirected face
  -> (FaceId -> label)
  -> Either BoundaryObstruction [(label, RegionBoundary)]
labelledRegionBoundaries triangulation labelFace =
  traverse
    (\(label, component) -> (label,) <$> componentBoundary triangulation component)
    (faceComponents triangulation labelFace)

-- | Extract the non-empty oriented boundary chain of one component. Boundary
-- half-edges retain their incident component face on the left. If several
-- boundary arms meet at one vertex, Euler descent consumes every half-edge
-- once and repeated-vertex splitting publishes finitely many simple loops.
componentBoundaryLoops
  :: Triangulation mode vertex directed undirected face
  -> FaceComponent
  -> Either BoundaryObstruction (NonEmpty BoundaryLoop)
componentBoundaryLoops triangulation component = do
  graph <- componentBoundaryGraph triangulation component
  loops <-
    case graph of
      SimpleBoundaryGraph edges outgoing ->
        traceBoundaryLoops triangulation outgoing edges
      PinchedBoundaryGraph edges _ _ _ ->
        tracePinchedBoundaryLoops triangulation edges
  case NonEmpty.nonEmpty loops of
    Just nonEmptyLoops -> Right nonEmptyLoops
    Nothing -> Left (BoundaryOuterLoopCardinality 0)

-- | Project one component into a strict polygon boundary. A pinched oriented
-- chain remains available through 'componentBoundaryLoops', but it is not one
-- lawful 'RegionBoundary' and is refused here with its first pinch witness.
componentBoundary
  :: Triangulation mode vertex directed undirected face
  -> FaceComponent
  -> Either BoundaryObstruction RegionBoundary
componentBoundary triangulation component = do
  graph <- componentBoundaryGraph triangulation component
  case graph of
    PinchedBoundaryGraph _ vertex firstEdge secondEdge ->
      Left (BoundaryPinch vertex firstEdge secondEdge)
    SimpleBoundaryGraph edges outgoing ->
      traceBoundaryLoops triangulation outgoing edges
        >>= regionBoundaryFromLoops

-- | Project exactly the supplied face section into one boundary. This is the
-- selected-section descent used by local extension: it does not label or
-- flood the complement, and therefore its work is proportional to the faces
-- named by the caller plus their constant-degree boundary edges.
componentBoundaryForFaces
  :: Triangulation mode vertex directed undirected face
  -> Set.Set FaceId
  -> Either BoundaryObstruction RegionBoundary
componentBoundaryForFaces triangulation selectedFaces = do
  component <- selectedFaceComponent triangulation selectedFaces
  componentBoundary triangulation component

selectedFaceComponent
  :: Triangulation mode vertex directed undirected face
  -> Set.Set FaceId
  -> Either BoundaryObstruction FaceComponent
selectedFaceComponent triangulation selectedFaces =
  case IntSet.lookupMin indices of
    Just 0 -> Left (BoundaryComponentFaceOutOfRange (FaceId 0) (numFaces triangulation))
    _ ->
      case IntSet.lookupGE (numFaces triangulation) indices of
        Just invalid ->
          Left
            ( BoundaryComponentFaceOutOfRange
                (FaceId (fromIntegral invalid))
                (numFaces triangulation)
            )
        Nothing -> Right (FaceComponent indices)
 where
  indices = IntSet.fromList (fmap faceIdIndex (Set.toAscList selectedFaces))

componentBoundaryGraph
  :: Triangulation mode vertex directed undirected face
  -> FaceComponent
  -> Either BoundaryObstruction BoundaryGraph
componentBoundaryGraph triangulation (FaceComponent componentFaces) =
  case IntSet.lookupGE (numFaces triangulation) componentFaces of
    Just invalid ->
      Left
        ( BoundaryComponentFaceOutOfRange
            (FaceId (fromIntegral invalid))
            (numFaces triangulation)
        )
    Nothing ->
      Right
        ( IntSet.foldl'
            collectFace
            (SimpleBoundaryGraph IntSet.empty IntMap.empty)
            componentFaces
        )
 where
  collectFace graph (face :: Int) =
    foldFaceDirectedEdges'
      triangulation
      (FaceId (fromIntegral face))
      insertBoundaryEdge
      graph

  insertBoundaryEdge graph edge
    | IntSet.member adjacentIndex componentFaces = graph
    | otherwise =
        case graph of
          PinchedBoundaryGraph edges pinchVertex firstEdge secondEdge ->
            PinchedBoundaryGraph
              (insertEdge edges)
              pinchVertex
              firstEdge
              secondEdge
          SimpleBoundaryGraph edges outgoing ->
            case IntMap.lookup residentIndex outgoing of
              Nothing ->
                SimpleBoundaryGraph
                  (insertEdge edges)
                  (IntMap.insert residentIndex edge outgoing)
              Just previousEdge ->
                PinchedBoundaryGraph
                  (insertEdge edges)
                  vertex
                  previousEdge
                  edge
   where
    adjacentIndex = faceIdIndex (incidentFace triangulation (reverseEdge edge))
    vertex = origin triangulation edge
    residentIndex = vertexIdIndex vertex
    insertEdge = IntSet.insert (directedEdgeIdIndex edge)

regionBoundaryFromLoops
  :: [BoundaryLoop]
  -> Either BoundaryObstruction RegionBoundary
regionBoundaryFromLoops loops =
  case outerLoops of
    [outerLoop] ->
      Right
        RegionBoundary
          { regionBoundaryOuterLoop = outerLoop
          , regionBoundaryHoleLoops = holeLoops
          }
    _ -> Left (BoundaryOuterLoopCardinality (length outerLoops))
 where
  (outerLoops, holeLoops) =
    partition
      ((== BoundaryCounterClockwise) . boundaryLoopOrientation)
      loops

tracePinchedBoundaryLoops
  :: Triangulation mode vertex directed undirected face
  -> IntSet.IntSet
  -> Either BoundaryObstruction [BoundaryLoop]
tracePinchedBoundaryLoops triangulation boundaryEdges = do
  cycles <-
    traceOrientedBoundaryCircuits
      (origin triangulation)
      (destination triangulation)
      BoundaryCycleDidNotClose
      outgoingByVertex
      orientedEdges
  traverse
    (simplifyBoundaryLoop triangulation . NonEmpty.toList)
    cycles
 where
  orientedEdges =
    Set.fromDistinctAscList
      ( fmap
          (DirectedEdgeId . fromIntegral)
          (IntSet.toAscList boundaryEdges)
      )
  outgoingByVertex =
    Set.foldr
      (\edge -> Map.insertWith (<>) (origin triangulation edge) [edge])
      Map.empty
      orientedEdges

traceBoundaryLoops
  :: Triangulation mode vertex directed undirected face
  -> IntMap.IntMap DirectedEdgeId
  -> IntSet.IntSet
  -> Either BoundaryObstruction [BoundaryLoop]
traceBoundaryLoops triangulation outgoingByVertex = descend []
 where
  descend loops unvisited =
    case IntSet.minView unvisited of
      Nothing -> Right (reverse loops)
      Just (rawStart, _) -> do
        let start = DirectedEdgeId (fromIntegral rawStart)
        (vertices, remaining) <- traceCycle start start unvisited []
        loop <- simplifyBoundaryLoop triangulation vertices
        descend (loop : loops) remaining

  traceCycle start current unvisited reversedVertices =
    let remaining = IntSet.delete (directedEdgeIdIndex current) unvisited
        accumulated = origin triangulation current : reversedVertices
        target = destination triangulation current
     in case IntMap.lookup (vertexIdIndex target) outgoingByVertex of
          Just successor
            | successor == start -> Right (reverse accumulated, remaining)
            | IntSet.member (directedEdgeIdIndex successor) remaining ->
                traceCycle start successor remaining accumulated
            | otherwise -> Left (BoundaryCycleDidNotClose start successor)
          Nothing -> Left (BoundaryCycleDidNotClose start current)

simplifyBoundaryLoop
  :: Triangulation mode vertex directed undirected face
  -> [VertexId]
  -> Either BoundaryObstruction BoundaryLoop
simplifyBoundaryLoop triangulation vertices = do
  (windingOrder, simplifiedVertices) <-
    simplifyBoundaryCycle
      BoundaryLoopDegenerate
      redundant
      winding
      key
      vertices
  residentVertices <-
    maybe
      (Left (BoundaryLoopDegenerate vertices))
      Right
      (NonEmpty.nonEmpty vertices)
  case windingOrder of
    GT -> Right (BoundaryLoop BoundaryCounterClockwise residentVertices simplifiedVertices)
    LT -> Right (BoundaryLoop BoundaryClockwise residentVertices simplifiedVertices)
    EQ -> Left (BoundaryLoopDegenerate (NonEmpty.toList simplifiedVertices))
 where
  point vertex = vertexPoint triangulation vertex
  redundant previousVertex current nextVertex =
    orient2d (point previousVertex) (point current) (point nextVertex) == EQ
      && onClosedSegment (point previousVertex) (point nextVertex) (point current)
  winding previousVertex current nextVertex =
    orient2d (point previousVertex) (point current) (point nextVertex)
  key vertex = (point vertex, vertex)

-- | Inner faces separated from the outer face by an even minimum number of
-- barriers. A 0–1 BFS floods freely within one depth before crossing a barrier,
-- so a free-ended barrier can be walked around at depth zero while nested
-- closed barriers alternate outside and inside.
facesAtEvenBarrierDepth
  :: Triangulation mode vertex directed undirected face
  -> (UndirectedEdgeId -> Bool)
  -> [FaceId]
facesAtEvenBarrierDepth triangulation isBarrier =
  concat (evenLayers (barrierDepthLayers triangulation isBarrier))
 where
  evenLayers :: [[FaceId]] -> [[FaceId]]
  evenLayers (outsideLayer : _insideLayer : deeper) =
    outsideLayer : evenLayers deeper
  evenLayers shallow = shallow

barrierDepthLayers
  :: Triangulation mode vertex directed undirected face
  -> (UndirectedEdgeId -> Bool)
  -> [[FaceId]]
barrierDepthLayers triangulation isBarrier =
  map (filter (/= outerFace)) (layers IntSet.empty [outerFace])
 where
  known face = faceIdIndex face < numFaces triangulation

  layers visited frontier = case flood visited [] frontier of
    ([], _) -> []
    (layer, visited') -> layer : layers visited' (concatMap (neighbours isBarrier) layer)

  flood visited acc [] = (reverse acc, visited)
  flood visited acc (face : rest)
    | not (known face) || IntSet.member (faceIdIndex face) visited = flood visited acc rest
    | otherwise =
        flood
          (IntSet.insert (faceIdIndex face) visited)
          (face : acc)
          (neighbours (not . isBarrier) face <> rest)

  neighbours admit face =
    [ incidentFace triangulation (reverseEdge edge)
    | edge <- faceDirectedEdges triangulation face
    , admit (asUndirected edge)
    ]

-- | Faces reachable from the outer face without crossing a barrier edge.
outerFaceFloodFill :: Triangulation mode vertex directed undirected face -> (UndirectedEdgeId -> Bool) -> [FaceId]
outerFaceFloodFill triangulation canCross = floodFillFaces triangulation starts canCross
 where
  starts =
    [ face
    | outerEdge <- faceDirectedEdges triangulation outerFace
    , let edge = asUndirected outerEdge
    , canCross edge
    , let face = incidentFace triangulation (reverseEdge outerEdge)
    , face /= outerFace
    ]

-- | The faces a shape's start point lands in.
shapeStartFaces :: DistanceMetric metric => Triangulation mode vertex directed undirected face -> metric -> [FaceId]
shapeStartFaces triangulation metric =
  case locatePoint triangulation (metricStartPoint metric) of
    InFace face -> [face]
    OnEdge edge -> filter (/= outerFace) [incidentFace triangulation edge, incidentFace triangulation (reverseEdge edge)]
    OnVertex vertex ->
      intSetToFaces
        ( List.foldl'
            (\set edge ->
              let face = incidentFace triangulation edge
               in if face == outerFace then set else IntSet.insert (faceIdIndex face) set
            )
            IntSet.empty
            (vertexOutgoingEdges triangulation vertex)
        )
    OutsideConvexHull _ ->
      [ incidentFace triangulation (reverseEdge edge)
      | edge <- faceDirectedEdges triangulation outerFace
      , let from = vertexPoint triangulation (origin triangulation edge)
      , let to = vertexPoint triangulation (destination triangulation edge)
      , metricIntersectsEdge metric from to
      , incidentFace triangulation (reverseEdge edge) /= outerFace
      ]
    EmptyTriangulation -> []
 where
  intSetToFaces = map (FaceId . fromIntegral) . IntSet.toAscList

segmentRectangleIntersection :: RectangleMetric -> Point -> Point -> Bool
segmentRectangleIntersection (RectangleMetric lowerQuery upperQuery _) from to
  | lx > ux || ly > uy = False
  | lower == upper = onClosedSegment from to lower
  | otherwise = any (uncurry (segmentsIntersect from to)) boundaries
 where
  lower@(Point lx ly) = queryPointValue lowerQuery
  upper@(Point ux uy) = queryPointValue upperQuery
  boundaries =
    [ (Point lx ly, Point lx uy)
    , (Point lx uy, Point ux uy)
    , (Point ux uy, Point ux ly)
    , (Point ux ly, Point lx ly)
    ]

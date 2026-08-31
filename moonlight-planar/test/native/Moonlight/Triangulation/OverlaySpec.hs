-- | Focused common-refinement, provenance, selector, and grouped-publication
-- acceptance.
module Moonlight.Triangulation.OverlaySpec (tests) where

import Control.Monad (foldM, unless, when)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Triangulation.CellSet
  ( ExactCellSet
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , exactCellSetVertexCount
  )
import Moonlight.Triangulation.Dcel
  ( faceData
  , isConstraintEdge
  , undirectedEdgeData
  , undirectedEndpoints
  , vertexData
  )
import Moonlight.Triangulation.Exact
  ( ExactPoint
  , ExactIntersectionError
  , ExactSegment
  , SegmentRelation (..)
  , exactLineIntersection
  , exactOnClosedSegment
  , exactPointCoordinates
  , exactSegment
  , exactSegmentEndpoints
  , exactSegmentRelation
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( exactRationalDenominator
  )
import Moonlight.Triangulation.Internal.ExactSegmentEvents
  ( ExactSegmentEventPlan
  , ExactSweepSegmentId (..)
  , exactSegmentEventPlan
  , exactSegmentRelationMap
  , exactSegmentSplitPoints
  , exactSegmentSweepMaximumHeight
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( innerFaces
  , undirectedEdges
  )
import Moonlight.Triangulation.Internal.Overlay.Arrangement (certifyArrangement)
import Moonlight.Triangulation.Internal.Overlay.Resident
  ( OverlayDiagonalSchedule (..)
  , residentOverlay
  )
import Moonlight.Triangulation.Overlay
import Moonlight.Triangulation.Region
import Support
  ( assertEqual
  , assertValid
  , integerPoint
  , rectangleComponent
  , requireRight
  )

tests :: IO ()
tests = do
  testExactSegmentEventPlan
  testOverlappingSquares
  testLowerDimensionalIntersections
  testSeamAndOverlapNormalization
  testOutsidePairRemainsImplicit
  testNestedAndNonDyadicOverlay
  testSelectorRefusalsAndOperandSwap
  testDisconnectedGroupedPublication
  testMultiwayPointBoundaryCycles
  testDiagonalScheduleIndependence

testExactSegmentEventPlan :: IO ()
testExactSegmentEventPlan = do
  sourceFixture <-
    traverse
      (uncurry integerSegment)
      [ ((0, 0), (4, 0))
      , ((4, 0), (0, 0))
      , ((2, 0), (6, 0))
      , ((4, 0), (4, 4))
      , ((1, -1), (1, 0))
      , ((0, -1), (4, 1))
      , ((10, 10), (11, 10))
      ]
  verticalAndMultiway <-
    traverse
      (uncurry integerSegment)
      [ ((0, -4), (0, 4))
      , ((-4, 0), (4, 0))
      , ((-3, -3), (3, 3))
      , ((-3, 3), (3, -3))
      , ((0, 1), (0, 5))
      , ((0, 4), (0, 7))
      ]
  grid <-
    traverse
      (uncurry integerSegment)
      ( [((-1, y), (5, y)) | y <- [0 .. 4]]
          <> [((x, -1), (x, 5)) | x <- [0 .. 4]]
      )
  collinearOverlaps <-
    traverse
      (uncurry integerSegment)
      [ ((0, 0), (8, 0))
      , ((1, 0), (3, 0))
      , ((2, 0), (6, 0))
      , ((5, 0), (9, 0))
      , ((8, 0), (10, 0))
      , ((11, 0), (12, 0))
      ]
  traverse_
    compareEventPlanWithOracle
    [sourceFixture, verticalAndMultiway, grid, collinearOverlaps]
  let allRelations =
        Set.fromList
          [ exactSegmentRelation a b c d
          | (leftIndex, left) <- zip [0 :: Int ..] sourceFixture
          , right <- drop (leftIndex + 1) sourceFixture
          , let (a, b) = exactSegmentEndpoints left
                (c, d) = exactSegmentEndpoints right
          ]
  assertEqual
    "source fixture covers every segment relation"
    ( Set.fromList
        [ SegmentsDisjoint
        , SegmentsProperlyCross
        , SegmentsShareEndpoint
        , SegmentEndpointTouchesInterior
        , SegmentsCollinearlyOverlap
        , SegmentsDuplicate
        ]
    )
    allRelations

compareEventPlanWithOracle :: [ExactSegment] -> IO ()
compareEventPlanWithOracle segments = do
  let vector = V.fromList segments
  plan <- requireRight "exact event sweep" (exactSegmentEventPlan vector)
  expectedSplits <- requireRight "quadratic split oracle" (quadraticSplitPoints segments)
  assertEqual
    "sweep relation map agrees with quadratic oracle"
    (quadraticRelationMap segments)
    (exactSegmentRelationMap plan)
  traverse_
    (assertSegmentSplits plan expectedSplits)
    [0 .. length segments - 1]
  let heightLimit = 2 * ceilingLog2 (length segments + 1)
  if exactSegmentSweepMaximumHeight plan <= heightLimit
    then pure ()
    else
      fail
        ( "AVL height exceeded conservative logarithmic bound: "
            <> show (exactSegmentSweepMaximumHeight plan, heightLimit)
        )

assertSegmentSplits
  :: ExactSegmentEventPlan
  -> Map.Map Int (Set.Set ExactPoint)
  -> Int
  -> IO ()
assertSegmentSplits plan expected segmentIndex =
  assertEqual
    ("sweep split points agree for segment " <> show segmentIndex)
    (Map.findWithDefault Set.empty segmentIndex expected)
    (Set.fromList (exactSegmentSplitPoints plan (ExactSweepSegmentId segmentIndex)))

quadraticRelationMap
  :: [ExactSegment]
  -> Map.Map (ExactSweepSegmentId, ExactSweepSegmentId) SegmentRelation
quadraticRelationMap segments =
  Map.fromList
    [ ((ExactSweepSegmentId leftIndex, ExactSweepSegmentId rightIndex), relation)
    | (leftIndex, left) <- zip [0 :: Int ..] segments
    , (rightIndex, right) <- zip [leftIndex + 1 ..] (drop (leftIndex + 1) segments)
    , let (a, b) = exactSegmentEndpoints left
          (c, d) = exactSegmentEndpoints right
          relation = exactSegmentRelation a b c d
    , relation /= SegmentsDisjoint
    ]

quadraticSplitPoints
  :: [ExactSegment]
  -> Either ExactIntersectionError (Map.Map Int (Set.Set ExactPoint))
quadraticSplitPoints segments =
  foldM addRelation initial (segmentPairs segments)
 where
  initial =
    Map.fromList
      [ (index, Set.fromList [from, to])
      | (index, segment) <- zip [0 :: Int ..] segments
      , let (from, to) = exactSegmentEndpoints segment
      ]
  addRelation
    :: Map.Map Int (Set.Set ExactPoint)
    -> (Int, ExactSegment, Int, ExactSegment)
    -> Either ExactIntersectionError (Map.Map Int (Set.Set ExactPoint))
  addRelation splitPoints (leftIndex, left, rightIndex, right) = do
    witnesses <- relationSplitWitnesses left right
    pure
      ( Map.insertWith Set.union rightIndex (Set.fromList witnesses)
          (Map.insertWith Set.union leftIndex (Set.fromList witnesses) splitPoints)
      )

segmentPairs :: [value] -> [(Int, value, Int, value)]
segmentPairs values =
  [ (leftIndex, left, rightIndex, right)
  | (leftIndex, left) <- zip [0 :: Int ..] values
  , (rightIndex, right) <- zip [leftIndex + 1 ..] (drop (leftIndex + 1) values)
  ]

relationSplitWitnesses
  :: ExactSegment
  -> ExactSegment
  -> Either ExactIntersectionError [ExactPoint]
relationSplitWitnesses left right =
  case exactSegmentRelation a b c d of
    SegmentsDisjoint -> Right []
    SegmentsProperlyCross -> (: []) <$> exactLineIntersection left right
    SegmentsDuplicate -> Right []
    _ ->
      Right
        ( Set.toAscList
            ( Set.fromList
                [ point
                | point <- [a, b, c, d]
                , exactOnClosedSegment a b point
                , exactOnClosedSegment c d point
                ]
            )
        )
 where
  (a, b) = exactSegmentEndpoints left
  (c, d) = exactSegmentEndpoints right

ceilingLog2 :: Int -> Int
ceilingLog2 target = length (takeWhile (< target) (iterate (* 2) 1))

integerSegment :: (Integer, Integer) -> (Integer, Integer) -> IO ExactSegment
integerSegment (fromX, fromY) (toX, toY) =
  requireRight
    "integer exact segment"
    (exactSegment (integerPoint fromX fromY) (integerPoint toX toY))

testOverlappingSquares :: IO ()
testOverlappingSquares = do
  left <- singletonSquareLayer "left-outside" "left" 0 0 2 2
  right <- singletonSquareLayer "right-outside" "right" 1 (-1) 3 1
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

testLowerDimensionalIntersections :: IO ()
testLowerDimensionalIntersections = do
  left <- singletonSquareLayer "left-outside" "left" 0 0 1 1
  pointTouching <- singletonSquareLayer "right-outside" "right" 1 1 2 2
  pointResult <- requireRight "point-touching overlay" (overlayLayers left pointTouching)
  assertOverlayIntegrity "point-touching" pointResult
  assertEqual
    "point-touching unbounded cell keeps two simple boundary cycles"
    (Just 2)
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

  edgeTouching <- singletonSquareLayer "right-outside" "right" 1 0 2 1
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
  let seamPublication = overlayPlanarLayer seamResult
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

  duplicateLeft <- singletonSquareLayer "left-outside" "left" 0 0 2 2
  duplicateRight <- singletonSquareLayer "right-outside" "right" 0 0 2 2
  duplicateResult <-
    requireRight "cross-operand duplicate boundary overlay" (overlayLayers duplicateLeft duplicateRight)
  assertOverlayIntegrity "duplicate boundary" duplicateResult
  assertEqual
    "duplicate boundaries normalize to one atomic cycle"
    4
    (overlayAtomicEdges (overlayReceipt duplicateResult))

  partialLeft <- singletonSquareLayer "left-outside" "left" 0 0 3 2
  partialRight <- singletonSquareLayer "right-outside" "right" 1 0 4 1
  partialResult <-
    requireRight "partial collinear overlap" (overlayLayers partialLeft partialRight)
  assertOverlayIntegrity "partial collinear overlap" partialResult
  if overlayOverlapIntervals (overlayReceipt partialResult) > 0
    then pure ()
    else fail "partial collinear overlap emitted no overlap event"

testOutsidePairRemainsImplicit :: IO ()
testOutsidePairRemainsImplicit = do
  outer <-
    requireRight
      "annulus outer loop"
      ( exactLoop
          ( integerPoint 0 0
              :| [integerPoint 4 0, integerPoint 4 4, integerPoint 0 4]
          )
      )
  hole <-
    requireRight
      "annulus hole loop"
      ( exactLoop
          ( integerPoint 1 1
              :| [integerPoint 1 3, integerPoint 3 3, integerPoint 3 1]
          )
      )
  component <- requireRight "annulus component" (polygonComponent outer [hole])
  region <- requireRight "annulus region" (planarRegion [component])
  left <-
    requireRight
      "annulus layer"
      (planarLayer "outside" (Map.singleton "annulus" region))
  right <- requireRight "annulus empty layer" (planarLayer "void" Map.empty)
  result <- requireRight "annulus overlay" (overlayLayers left right)
  let published = overlayPlanarLayer result
  assertEqual
    "bounded cavities remain represented by the implicit outside pair"
    Nothing
    ( Map.lookup
        ("outside", "void")
        (planarLayerRegions published)
    )

testSelectorRefusalsAndOperandSwap :: IO ()
testSelectorRefusalsAndOperandSwap = do
  left <- singletonSquareLayer "left-outside" "left" 0 0 2 2
  right <- singletonSquareLayer "right-outside" "right" 1 (-1) 3 1
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
    [ (overlayCellGeometry cell, (overlayCellRight cell, overlayCellLeft cell))
    | (_, cell) <- overlayCells result
    ]
    [ (overlayCellGeometry cell, (overlayCellLeft cell, overlayCellRight cell))
    | (_, cell) <- overlayCells swapped
    ]
  assertEqual
    "operand swap exchanges typed vertex provenance"
    (vertexOriginCensus True result)
    (vertexOriginCensus False swapped)

testNestedAndNonDyadicOverlay :: IO ()
testNestedAndNonDyadicOverlay = do
  outer <- singletonSquareLayer "left-outside" "left" 0 0 4 4
  inner <- singletonSquareLayer "right-outside" "right" 1 1 3 3
  nested <- requireRight "nested overlay" (overlayLayers outer inner)
  assertOverlayIntegrity "nested" nested
  assertEqual "nested overlay unbounded boundary is outermost only" (Just 1) (unboundedLoopCount nested)
  assertEqual
    "nested overlay has unbounded, shell, and intersection cells"
    3
    (length (overlayCells nested))

  leftTriangle <-
    triangleLayer
      "left-outside"
      "left"
      ((0, 0), (4, 0), (0, 4))
  rightTriangle <-
    triangleLayer
      "right-outside"
      "right"
      ((1, -1), (3, -1), (2, 2))
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

assertSelectionRefusal
  :: String
  -> OverlaySelectionError
  -> Either OverlaySelectionError ExactCellSet
  -> IO ()
assertSelectionRefusal label expected actual =
  case actual of
    Left obstruction -> assertEqual label expected obstruction
    Right _ -> fail (label <> ": unbounded selection was truncated")

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
  let published = overlayPlanarLayer result
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
    "multiway point contact descends to three simple unbounded cycles"
    (Just 3)
    (unboundedLoopCount result)

testDiagonalScheduleIndependence :: IO ()
testDiagonalScheduleIndependence = do
  left <- singletonSquareLayer "outside" "inside" 0 0 2 2
  right <- requireRight "empty diagonal-policy layer" (planarLayer "void" Map.empty)
  certified <-
    requireRight
      "diagonal-policy arrangement certification"
      (certifyArrangement left right)
  canonical <-
    requireRight
      "canonical resident diagonal schedule"
      (residentOverlay CanonicalOverlayDiagonals ("outside", "void") certified)
  alternate <-
    requireRight
      "alternate resident diagonal schedule"
      (residentOverlay FlipFirstAdmissibleDiagonal ("outside", "void") certified)
  assertOverlayIntegrity "canonical diagonal schedule" canonical
  assertOverlayIntegrity "alternate diagonal schedule" alternate
  assertEqual
    "exact cells are independent of resident diagonal policy"
    (overlayCells canonical)
    (overlayCells alternate)
  when (residentDiagonalKeys canonical == residentDiagonalKeys alternate) $
    fail "alternate diagonal fixture did not change the resident topology"

residentDiagonalKeys
  :: OverlayResult leftLabel rightLabel
  -> Set.Set (ExactPoint, ExactPoint)
residentDiagonalKeys result =
  Set.fromList
    [ if fromPoint <= toPoint
        then (fromPoint, toPoint)
        else (toPoint, fromPoint)
    | edge <- undirectedEdges triangulation
    , OverlayDiagonal <- [undirectedEdgeData triangulation edge]
    , let (fromVertex, toVertex) = undirectedEndpoints triangulation edge
          fromPoint = overlayExactPoint (vertexData triangulation fromVertex)
          toPoint = overlayExactPoint (vertexData triangulation toVertex)
    ]
 where
  triangulation = overlayEmbeddedTriangulation result

assertOverlayIntegrity
  :: String
  -> OverlayResult leftLabel rightLabel
  -> IO ()
assertOverlayIntegrity label result = do
  let triangulation = overlayEmbeddedTriangulation result
      boundaryEdges = overlayArrangementEdges result
      cellIds = Set.fromList (map fst (overlayCells result))
      residentFaceIds =
        Set.fromList
          [ overlayFaceCellId (faceData triangulation face)
          | face <- innerFaces triangulation
          ]
  assertValid (label <> " embedded triangulation") triangulation
  assertEqual
    (label <> " atomic constraint correspondence")
    (overlayAtomicEdges (overlayReceipt result))
    (length boundaryEdges)
  unless (all (isConstraintEdge triangulation . fst) boundaryEdges) $
    fail (label <> ": boundary payload names an unconstrained edge")
  unless (residentFaceIds `Set.isSubsetOf` cellIds) $
    fail (label <> ": resident face names no exact cell")
  unless
    ( all
        (vertexOriginIsNonEmpty . overlayVertexOrigin . snd)
        (overlayArrangementVertices result)
    )
    (fail (label <> ": exact vertex lost all typed origins"))
  unless (all (edgeOriginIsNonEmpty . snd) boundaryEdges) $
    fail (label <> ": atomic edge lost all typed origins")
  atomicSegments <-
    traverse
      (\(edge, _) ->
         let (fromVertex, toVertex) = undirectedEndpoints triangulation edge
          in requireRight
               (label <> " atomic segment")
               ( exactSegment
                   (overlayExactPoint (vertexData triangulation fromVertex))
                   (overlayExactPoint (vertexData triangulation toVertex))
               ))
      boundaryEdges
  atomicPlan <-
    requireRight
      (label <> " atomic endpoint-incidence proof")
      (exactSegmentEventPlan (V.fromList atomicSegments))
  assertEqual
    (label <> " atomics have only endpoint-incidence relations")
    (endpointIncidenceRelations atomicSegments)
    (exactSegmentRelationMap atomicPlan)

endpointIncidenceRelations
  :: [ExactSegment]
  -> Map.Map (ExactSweepSegmentId, ExactSweepSegmentId) SegmentRelation
endpointIncidenceRelations segments =
  Map.fromList
    [ ( (ExactSweepSegmentId leftIndex, ExactSweepSegmentId rightIndex)
      , SegmentsShareEndpoint
      )
    | (leftIndex, left) <- zip [0 :: Int ..] segments
    , (rightIndex, right) <- zip [leftIndex + 1 ..] (drop (leftIndex + 1) segments)
    , let (leftFrom, leftTo) = exactSegmentEndpoints left
          (rightFrom, rightTo) = exactSegmentEndpoints right
    , not
        ( Set.null
            ( Set.intersection
                (Set.fromList [leftFrom, leftTo])
                (Set.fromList [rightFrom, rightTo])
            )
        )
    ]

vertexOriginIsNonEmpty :: OverlayVertexOrigin -> Bool
vertexOriginIsNonEmpty origin =
  not
    ( null (overlayOriginLeftVertices origin)
        && null (overlayOriginRightVertices origin)
        && null (overlayOriginLeftEdges origin)
        && null (overlayOriginRightEdges origin)
    )

edgeOriginIsNonEmpty :: OverlayEdgeOrigin -> Bool
edgeOriginIsNonEmpty origin =
  not
    (null (overlayEdgeLeftSources origin) && null (overlayEdgeRightSources origin))

vertexOriginCensus
  :: Bool
  -> OverlayResult String String
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
    ( length (overlayOriginLeftVertices origin)
    , length (overlayOriginRightVertices origin)
    , length (overlayOriginLeftEdges origin)
    , length (overlayOriginRightEdges origin)
    )
  swapCensus :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
  swapCensus (leftVertices, rightVertices, leftEdges, rightEdges) =
    (rightVertices, leftVertices, rightEdges, leftEdges)

unboundedLoopCount :: OverlayResult leftLabel rightLabel -> Maybe Int
unboundedLoopCount result =
  case
    [ length loops
    | (cellId, cell) <- overlayCells result
    , cellId == OverlayCellId 0
    , UnboundedOverlayCell loops <- [overlayCellGeometry cell]
    ] of
    [count] -> Just count
    _ -> Nothing

singletonSquareLayer
  :: String
  -> String
  -> Integer
  -> Integer
  -> Integer
  -> Integer
  -> IO (PlanarLayer String)
singletonSquareLayer outside inside minX minY maxX maxY = do
  component <- rectangleComponent minX minY maxX maxY
  region <- requireRight "singleton square region" (planarRegion [component])
  requireRight "singleton square layer" (planarLayer outside (Map.singleton inside region))

triangleLayer
  :: String
  -> String
  -> ((Integer, Integer), (Integer, Integer), (Integer, Integer))
  -> IO (PlanarLayer String)
triangleLayer outside inside (firstPoint, secondPoint, thirdPoint) = do
  component <- triangleComponent (firstPoint, secondPoint, thirdPoint)
  region <- requireRight "overlay triangle region" (planarRegion [component])
  requireRight "overlay triangle layer" (planarLayer outside (Map.singleton inside region))

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

isPowerOfTwo :: Integer -> Bool
isPowerOfTwo value =
  value > 0 && value `elem` takeWhile (<= value) (iterate (* 2) 1)

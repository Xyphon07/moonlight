-- | Exact segment-event sweep against an independent quadratic oracle.
module Moonlight.Planar.ExactSegmentEventsSpec (tests) where

import Control.Monad ( foldM )
import Data.Foldable ( traverse_ )
import Moonlight.Planar.Exact ( SegmentRelation(..), ExactPoint, exactLineIntersection,
  exactOnClosedSegment, exactSegment, exactSegmentEndpoints, exactSegmentRelation,
  ExactIntersectionError, ExactSegment )
import Moonlight.Planar.Internal.ExactSegmentEvents ( ExactSegmentEventPlan,
  ExactSweepSegmentId(..), exactSegmentEventPlan, exactSegmentRelationMap, exactSegmentSplitPoints,
  exactSegmentSweepMaximumHeight )
import Support ( assertEqual, integerPoint, requireRight )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

tests :: IO ()
tests = testExactSegmentEventPlan

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

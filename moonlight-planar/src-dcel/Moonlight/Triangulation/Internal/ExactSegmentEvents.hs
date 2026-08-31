{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | One exact owner for non-disjoint relations and split points in a finite
-- segment family. Collinear intervals descend by supporting-line sections;
-- non-collinear intersections descend through an immutable Bentley--Ottmann
-- status tree. Callers attach provenance only after this geometry glues.
module Moonlight.Triangulation.Internal.ExactSegmentEvents
  ( ExactSweepSegmentId (..)
  , ExactSegmentEvent (..)
  , ExactSegmentEventObstruction (..)
  , ExactSegmentEventPlan
  , exactSegmentEventPlan
  , exactSegmentEvents
  , exactSegmentSplitPoints
  , exactSegmentRelationMap
  , exactSegmentPairChecks
  , exactSegmentSweepMaximumHeight
  ) where

import Control.DeepSeq (NFData)
import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM)
import Data.List (sortBy)
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Moonlight.Triangulation.Exact
  ( ExactIntersectionError
  , ExactPoint
  , ExactSegment
  , SegmentRelation (..)
  , exactOnClosedSegment
  , exactPointCoordinates
  , exactSegmentEndpoints
  , exactSegmentRelation
  , exactSupportingLineIntersection
  )
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( orderedPair
  , unorderedPairs
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , exactRationalDenominator
  , exactRationalNumerator
  )

newtype ExactSweepSegmentId = ExactSweepSegmentId Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data ExactSegmentEvent
  = ExactProperCrossing !ExactSweepSegmentId !ExactSweepSegmentId !ExactPoint
  | ExactEndpointTouch !ExactSweepSegmentId !ExactSweepSegmentId !ExactPoint
  | ExactSharedEndpoint !ExactSweepSegmentId !ExactSweepSegmentId !ExactPoint
  | ExactDuplicateSegments !ExactSweepSegmentId !ExactSweepSegmentId
  | ExactCollinearOverlap
      !ExactSweepSegmentId
      !ExactSweepSegmentId
      !ExactPoint
      !ExactPoint
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data ExactSegmentEventObstruction
  = ExactSweepIntersectionObstruction
      !ExactSweepSegmentId
      !ExactSweepSegmentId
      !ExactIntersectionError
  | ExactSweepRelationWitnessMissing
      !ExactSweepSegmentId
      !ExactSweepSegmentId
      !SegmentRelation
  | ExactSweepSegmentMissing !ExactSweepSegmentId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ExactSegmentEventPlan = ExactSegmentEventPlan
  { plannedEvents :: !(Map (ExactSweepSegmentId, ExactSweepSegmentId) ExactSegmentEvent)
  , plannedSplitPoints :: !(V.Vector [ExactPoint])
  , plannedPairChecks :: !Int
  , plannedMaximumHeight :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data SegmentMeta = SegmentMeta
  { metaId :: !ExactSweepSegmentId
  , metaSegment :: !ExactSegment
  , metaLow :: !ExactPoint
  , metaHigh :: !ExactPoint
  , metaVertical :: !Bool
  }

data SupportingLine = SupportingLine !Integer !Integer !Integer
  deriving stock (Eq, Ord, Show)

data EventBundle = EventBundle
  { eventStarts :: !(Set ExactSweepSegmentId)
  , eventEnds :: !(Set ExactSweepSegmentId)
  , eventScheduled :: !(Set (ExactSweepSegmentId, ExactSweepSegmentId))
  }

emptyEventBundle :: EventBundle
emptyEventBundle = EventBundle Set.empty Set.empty Set.empty

mergeEventBundle :: EventBundle -> EventBundle -> EventBundle
mergeEventBundle left right =
  EventBundle
    { eventStarts = eventStarts left <> eventStarts right
    , eventEnds = eventEnds left <> eventEnds right
    , eventScheduled = eventScheduled left <> eventScheduled right
    }

data EventAccumulation = EventAccumulation
  { accumulatedEvents :: !(Map (ExactSweepSegmentId, ExactSweepSegmentId) ExactSegmentEvent)
  , accumulatedSplits :: !(IntMap.IntMap (Set ExactPoint))
  , accumulatedPairChecks :: !Int
  , accumulatedMaximumHeight :: !Int
  }

data StatusTree
  = StatusEmpty
  | StatusNode !Int !StatusTree !SegmentMeta !StatusTree

data PointChange = PointChange
  { changedPoint :: !ExactPoint
  , changedContinuing :: !(Set ExactSweepSegmentId)
  }

data StatusSide = StatusBefore | StatusAfter
  deriving stock (Eq)

-- | Build the complete exact event plan. The private sweep reports its maximum
-- AVL height and relation checks so benchmarks can distinguish output growth
-- from residual orchestration.
exactSegmentEventPlan
  :: V.Vector ExactSegment
  -> Either ExactSegmentEventObstruction ExactSegmentEventPlan
exactSegmentEventPlan segments = do
  let metas = V.imap segmentMeta segments
      initialAccumulation =
        EventAccumulation
          { accumulatedEvents = Map.empty
          , accumulatedSplits = IntMap.empty
          , accumulatedPairChecks = 0
          , accumulatedMaximumHeight = 0
          }
  let lineGroups = supportingLineGroups metas
  afterCollinear <- foldM recordCollinearGroup initialAccumulation (Map.elems lineGroups)
  let initialQueue = V.foldl' insertEndpointEvents Map.empty metas
  completed <- sweep metas initialQueue StatusEmpty afterCollinear
  pure
    ExactSegmentEventPlan
      { plannedEvents = accumulatedEvents completed
      , plannedSplitPoints = finalizeSplitPoints segments (accumulatedSplits completed)
      , plannedPairChecks = accumulatedPairChecks completed
      , plannedMaximumHeight = accumulatedMaximumHeight completed
      }

finalizeSplitPoints
  :: V.Vector ExactSegment
  -> IntMap.IntMap (Set ExactPoint)
  -> V.Vector [ExactPoint]
finalizeSplitPoints segments splitPoints =
  V.imap
    (\index segment ->
       let (from, to) = exactSegmentEndpoints segment
           eventPoints = IntMap.findWithDefault Set.empty index splitPoints
        in sortAlong segment (Set.toList (Set.insert from (Set.insert to eventPoints))))
    segments

segmentMeta :: Int -> ExactSegment -> SegmentMeta
segmentMeta index segment =
  let (firstPoint, secondPoint) = exactSegmentEndpoints segment
      low = min firstPoint secondPoint
      high = max firstPoint secondPoint
      (lowX, _) = exactPointCoordinates low
      (highX, _) = exactPointCoordinates high
   in SegmentMeta
        { metaId = ExactSweepSegmentId index
        , metaSegment = segment
        , metaLow = low
        , metaHigh = high
        , metaVertical = lowX == highX
        }

supportingLineGroups
  :: V.Vector SegmentMeta
  -> Map SupportingLine [SegmentMeta]
supportingLineGroups =
  V.foldl'
    (\groups meta -> Map.insertWith (<>) (supportingLine meta) [meta] groups)
    Map.empty

supportingLine :: SegmentMeta -> SupportingLine
supportingLine meta =
  let (from, to) = exactSegmentEndpoints (metaSegment meta)
      (fromX, fromY) = exactPointCoordinates from
      (toX, toY) = exactPointCoordinates to
      deltaX = toX - fromX
      deltaY = toY - fromY
      a = deltaY
      b = negate deltaX
      c = deltaX * fromY - deltaY * fromX
      denominators = map exactRationalDenominator [a, b, c]
      commonDenominator = List.foldl' lcm 1 denominators
      integerCoefficient coefficient =
        exactRationalNumerator coefficient
          * (commonDenominator `quot` exactRationalDenominator coefficient)
      integerA = integerCoefficient a
      integerB = integerCoefficient b
      integerC = integerCoefficient c
      commonDivisor = gcd (abs integerA) (gcd (abs integerB) (abs integerC))
      sign
        | integerA < 0 = -1
        | integerA == 0 && integerB < 0 = -1
        | otherwise = 1
      normalize coefficient = sign * (coefficient `quot` commonDivisor)
   in SupportingLine (normalize integerA) (normalize integerB) (normalize integerC)

recordCollinearGroup
  :: EventAccumulation
  -> [SegmentMeta]
  -> Either ExactSegmentEventObstruction EventAccumulation
recordCollinearGroup initial metas =
  fst <$> foldM descend (initial, []) ordered
 where
  ordered = sortBy compareInterval metas
  compareInterval left right =
    compare (metaLow left, metaHigh left, metaId left) (metaLow right, metaHigh right, metaId right)
  descend (accumulation, active) current = do
    let retained = filter (\candidate -> metaHigh candidate >= metaLow current) active
    updated <-
      foldM
        (\accumulated candidate -> recordRelation accumulated candidate current)
        accumulation
        retained
    pure (updated, current : retained)

insertEndpointEvents
  :: Map ExactPoint EventBundle
  -> SegmentMeta
  -> Map ExactPoint EventBundle
insertEndpointEvents queue meta =
  insertBundle (metaHigh meta) (emptyEventBundle{eventEnds = Set.singleton (metaId meta)})
    (insertBundle (metaLow meta) (emptyEventBundle{eventStarts = Set.singleton (metaId meta)}) queue)

insertBundle
  :: ExactPoint
  -> EventBundle
  -> Map ExactPoint EventBundle
  -> Map ExactPoint EventBundle
insertBundle = Map.insertWith mergeEventBundle

sweep
  :: V.Vector SegmentMeta
  -> Map ExactPoint EventBundle
  -> StatusTree
  -> EventAccumulation
  -> Either ExactSegmentEventObstruction EventAccumulation
sweep metas queue status accumulation =
  case Map.lookupMin queue of
    Nothing -> Right accumulation
    Just (firstPoint, _) -> do
      let (currentX, _) = exactPointCoordinates firstPoint
          (batch, laterQueue) =
            Map.spanAntitone
              (\point -> fst (exactPointCoordinates point) == currentX)
              queue
          startedIds =
            Set.toAscList
              (Map.foldl' (\ids bundle -> ids <> eventStarts bundle) Set.empty batch)
      started <- traverse (requireMeta metas) startedIds
      let startingIds = map metaId (filter (not . metaVertical) started)
          verticalIds = map metaId (filter metaVertical started)
      withVerticals <-
        recordVerticalRelations
          metas
          currentX
          startingIds
          verticalIds
          status
          accumulation
      (withEvents, removals, insertions, changes) <-
        foldM
          (processPoint metas status)
          (withVerticals, Set.empty, Set.empty, [])
          (Map.toAscList batch)
      statusWithout <-
        foldM
          (deleteStatus metas StatusBefore currentX)
          status
          (Set.toAscList removals)
      statusAfter <-
        foldM
          (insertStatus metas StatusAfter currentX)
          statusWithout
          (Set.toAscList insertions)
      (scheduledQueue, scheduledAccumulation) <-
        foldM
          (scheduleAroundChange metas currentX statusAfter)
          (laterQueue, withEvents)
          changes
      let measured =
            scheduledAccumulation
              { accumulatedMaximumHeight =
                  max
                    (accumulatedMaximumHeight scheduledAccumulation)
                    (statusHeight statusAfter)
              }
      sweep metas scheduledQueue statusAfter measured

recordVerticalRelations
  :: V.Vector SegmentMeta
  -> ExactRational
  -> [ExactSweepSegmentId]
  -> [ExactSweepSegmentId]
  -> StatusTree
  -> EventAccumulation
  -> Either ExactSegmentEventObstruction EventAccumulation
recordVerticalRelations metas currentX startingIds verticalIds status accumulation = do
  temporaryStatus <-
    foldM (insertStatus metas StatusAfter currentX) status startingIds
  foldM (recordVertical temporaryStatus) accumulation verticalIds
 where
  recordVertical temporaryStatus accumulated verticalId = do
    vertical <- requireMeta metas verticalId
    let (_, lowY) = exactPointCoordinates (metaLow vertical)
        (_, highY) = exactPointCoordinates (metaHigh vertical)
        candidates = statusRangeByY currentX lowY highY temporaryStatus
    foldM
      (\current candidateId -> do
         candidate <- requireMeta metas candidateId
         recordRelation current vertical candidate)
      accumulated
      candidates

processPoint
  :: V.Vector SegmentMeta
  -> StatusTree
  -> ( EventAccumulation
     , Set ExactSweepSegmentId
     , Set ExactSweepSegmentId
     , [PointChange]
     )
  -> (ExactPoint, EventBundle)
  -> Either
      ExactSegmentEventObstruction
      ( EventAccumulation
      , Set ExactSweepSegmentId
      , Set ExactSweepSegmentId
      , [PointChange]
      )
processPoint metas status (accumulation, removals, insertions, changes) (point, bundle) = do
  let (x, y) = exactPointCoordinates point
      activeAtPoint = Set.fromList (statusAtY x y status)
      scheduledIds =
        Set.fromList
          [ segmentId
          | (leftId, rightId) <- Set.toList (eventScheduled bundle)
          , segmentId <- [leftId, rightId]
          ]
      candidates = eventStarts bundle <> eventEnds bundle <> activeAtPoint <> scheduledIds
  incident <-
    Set.fromList
      <$> filterM
        (\segmentId -> do
           meta <- requireMeta metas segmentId
           let (from, to) = exactSegmentEndpoints (metaSegment meta)
           pure (exactOnClosedSegment from to point))
        (Set.toAscList candidates)
  withRelations <- recordIncidentPairs metas accumulation incident
  metasAtPoint <- traverse (requireMeta metas) (Set.toAscList incident)
  let removable =
        Set.fromList
          [ metaId meta
          | meta <- metasAtPoint
          , not (metaVertical meta)
          , Set.member (metaId meta) activeAtPoint
              || Set.member (metaId meta) (eventEnds bundle)
          ]
      continuing =
        Set.fromList
          [ metaId meta
          | meta <- metasAtPoint
          , not (metaVertical meta)
          , fst (exactPointCoordinates (metaHigh meta)) > x
          ]
  pure
    ( withRelations
    , removals <> removable
    , insertions <> continuing
    , PointChange point continuing : changes
    )

recordIncidentPairs
  :: V.Vector SegmentMeta
  -> EventAccumulation
  -> Set ExactSweepSegmentId
  -> Either ExactSegmentEventObstruction EventAccumulation
recordIncidentPairs metas initial incident =
  foldM
    (\accumulation (leftId, rightId) -> do
       left <- requireMeta metas leftId
       right <- requireMeta metas rightId
       recordRelation accumulation left right)
    initial
    (unorderedPairs (Set.toAscList incident))

recordRelation
  :: EventAccumulation
  -> SegmentMeta
  -> SegmentMeta
  -> Either ExactSegmentEventObstruction EventAccumulation
recordRelation accumulation firstMeta secondMeta =
  let (leftMeta, rightMeta) =
        if metaId firstMeta <= metaId secondMeta
          then (firstMeta, secondMeta)
          else (secondMeta, firstMeta)
      leftId = metaId leftMeta
      rightId = metaId rightMeta
      relation = relationOf (metaSegment leftMeta) (metaSegment rightMeta)
      checked = accumulation{accumulatedPairChecks = accumulatedPairChecks accumulation + 1}
   in case relation of
        SegmentsDisjoint -> Right checked
        _ -> do
          (event, splitPoints) <-
            relationEvent leftId rightId (metaSegment leftMeta) (metaSegment rightMeta) relation
          let ExactSweepSegmentId leftIndex = leftId
              ExactSweepSegmentId rightIndex = rightId
              splitSet = Set.fromList splitPoints
              withLeft =
                IntMap.insertWith Set.union leftIndex splitSet (accumulatedSplits checked)
              withBoth = IntMap.insertWith Set.union rightIndex splitSet withLeft
          Right
            checked
              { accumulatedEvents =
                  Map.insert
                    (leftId, rightId)
                    event
                    (accumulatedEvents checked)
              , accumulatedSplits = withBoth
              }

relationOf :: ExactSegment -> ExactSegment -> SegmentRelation
relationOf left right =
  let (a, b) = exactSegmentEndpoints left
      (c, d) = exactSegmentEndpoints right
   in exactSegmentRelation a b c d

relationEvent
  :: ExactSweepSegmentId
  -> ExactSweepSegmentId
  -> ExactSegment
  -> ExactSegment
  -> SegmentRelation
  -> Either ExactSegmentEventObstruction (ExactSegmentEvent, [ExactPoint])
relationEvent leftId rightId left right relation =
  case relation of
    SegmentsDisjoint -> missing
    SegmentsDuplicate -> Right (ExactDuplicateSegments leftId rightId, [])
    SegmentsProperlyCross -> do
      crossing <-
        either
          (Left . ExactSweepIntersectionObstruction leftId rightId)
          Right
          (exactSupportingLineIntersection left right)
      Right (ExactProperCrossing leftId rightId crossing, [crossing])
    SegmentsShareEndpoint ->
      uniqueWitness (ExactSharedEndpoint leftId rightId) (uniqueShared left right)
    SegmentEndpointTouchesInterior ->
      uniqueWitness (ExactEndpointTouch leftId rightId) (uniqueTouch left right)
    SegmentsCollinearlyOverlap ->
      let lower = max (min a b) (min c d)
          upper = min (max a b) (max c d)
       in if lower < upper
            then Right (ExactCollinearOverlap leftId rightId lower upper, [lower, upper])
            else missing
 where
  (a, b) = exactSegmentEndpoints left
  (c, d) = exactSegmentEndpoints right
  uniqueWitness make witness =
    case witness of
      Just point -> Right (make point, [point])
      Nothing -> missing
  missing = Left (ExactSweepRelationWitnessMissing leftId rightId relation)

scheduleAroundChange
  :: V.Vector SegmentMeta
  -> ExactRational
  -> StatusTree
  -> (Map ExactPoint EventBundle, EventAccumulation)
  -> PointChange
  -> Either
      ExactSegmentEventObstruction
      (Map ExactPoint EventBundle, EventAccumulation)
scheduleAroundChange metas currentX status state change =
  case Set.toAscList (changedContinuing change) of
    [] ->
      let (_, y) = exactPointCoordinates (changedPoint change)
          (below, above) = statusBelowAbove currentX y status
       in scheduleMaybePair metas currentX below above state
    continuingIds -> do
      continuing <- traverse (requireMeta metas) continuingIds
      let ordered = sortBy (statusCompare StatusAfter currentX) continuing
      case ordered of
        [] -> Right state
        lowest : remaining ->
          let highest = List.foldl' (\_ current -> current) lowest remaining
              below = statusPredecessor currentX lowest status
              above = statusSuccessor currentX highest status
           in scheduleMaybePair metas currentX below (Just (metaId lowest)) state
                >>= scheduleMaybePair metas currentX (Just (metaId highest)) above

scheduleMaybePair
  :: V.Vector SegmentMeta
  -> ExactRational
  -> Maybe ExactSweepSegmentId
  -> Maybe ExactSweepSegmentId
  -> (Map ExactPoint EventBundle, EventAccumulation)
  -> Either
      ExactSegmentEventObstruction
      (Map ExactPoint EventBundle, EventAccumulation)
scheduleMaybePair _ _ Nothing _ state = Right state
scheduleMaybePair _ _ _ Nothing state = Right state
scheduleMaybePair metas currentX (Just firstId) (Just secondId) (queue, accumulation)
  | firstId == secondId = Right (queue, accumulation)
  | otherwise = do
      firstMeta <- requireMeta metas firstId
      secondMeta <- requireMeta metas secondId
      let relation = relationOf (metaSegment firstMeta) (metaSegment secondMeta)
          checked = accumulation{accumulatedPairChecks = accumulatedPairChecks accumulation + 1}
      witness <-
        either
          (Left . ExactSweepIntersectionObstruction firstId secondId)
          Right
          (relationWitnessPoint (metaSegment firstMeta) (metaSegment secondMeta) relation)
      case witness of
        Just point
          | fst (exactPointCoordinates point) > currentX ->
              let pair = orderedPair firstId secondId
                  bundle = emptyEventBundle{eventScheduled = Set.singleton pair}
               in Right (insertBundle point bundle queue, checked)
        _ -> Right (queue, checked)

relationWitnessPoint
  :: ExactSegment
  -> ExactSegment
  -> SegmentRelation
  -> Either ExactIntersectionError (Maybe ExactPoint)
relationWitnessPoint left right relation =
  case relation of
    SegmentsProperlyCross -> Just <$> exactSupportingLineIntersection left right
    SegmentEndpointTouchesInterior -> Right (uniqueTouch left right)
    SegmentsShareEndpoint -> Right (uniqueShared left right)
    _ -> Right Nothing

uniqueShared :: ExactSegment -> ExactSegment -> Maybe ExactPoint
uniqueShared left right =
  let (a, b) = exactSegmentEndpoints left
      (c, d) = exactSegmentEndpoints right
   in case Set.toAscList (Set.intersection (Set.fromList [a, b]) (Set.fromList [c, d])) of
        [point] -> Just point
        _ -> Nothing

uniqueTouch :: ExactSegment -> ExactSegment -> Maybe ExactPoint
uniqueTouch left right =
  let (a, b) = exactSegmentEndpoints left
      (c, d) = exactSegmentEndpoints right
      points =
        Set.toAscList
          ( Set.fromList
              ( [point | point <- [a, b], exactOnClosedSegment c d point]
                  <> [point | point <- [c, d], exactOnClosedSegment a b point]
              )
          )
   in case points of
        [point] -> Just point
        _ -> Nothing

statusCompare
  :: StatusSide
  -> ExactRational
  -> SegmentMeta
  -> SegmentMeta
  -> Ordering
statusCompare side x left right =
  case compareOrdinateAt x left right of
    EQ ->
      case compareSlope left right of
        EQ -> compare (metaId left) (metaId right)
        slopeOrder -> if side == StatusAfter then slopeOrder else invertOrdering slopeOrder
    order -> order

compareOrdinateAt :: ExactRational -> SegmentMeta -> SegmentMeta -> Ordering
compareOrdinateAt x left right =
  let (leftNumerator, leftDenominator) = ordinateFraction x left
      (rightNumerator, rightDenominator) = ordinateFraction x right
   in compare
        (leftNumerator * rightDenominator)
        (rightNumerator * leftDenominator)

compareSlope :: SegmentMeta -> SegmentMeta -> Ordering
compareSlope left right =
  let (leftRise, leftRun) = slopeFraction left
      (rightRise, rightRun) = slopeFraction right
   in compare
        (leftRise * rightRun)
        (rightRise * leftRun)

ordinateFraction :: ExactRational -> SegmentMeta -> (ExactRational, ExactRational)
ordinateFraction x meta =
  let (fromX, fromY) = exactPointCoordinates (metaLow meta)
      (toX, toY) = exactPointCoordinates (metaHigh meta)
      run = toX - fromX
      numerator = fromY * run + (x - fromX) * (toY - fromY)
   in (numerator, run)

slopeFraction :: SegmentMeta -> (ExactRational, ExactRational)
slopeFraction meta =
  let (fromX, fromY) = exactPointCoordinates (metaLow meta)
      (toX, toY) = exactPointCoordinates (metaHigh meta)
   in (toY - fromY, toX - fromX)

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

insertStatus
  :: V.Vector SegmentMeta
  -> StatusSide
  -> ExactRational
  -> StatusTree
  -> ExactSweepSegmentId
  -> Either ExactSegmentEventObstruction StatusTree
insertStatus metas side x tree segmentId = do
  meta <- requireMeta metas segmentId
  pure (statusInsert (statusCompare side x) meta tree)

deleteStatus
  :: V.Vector SegmentMeta
  -> StatusSide
  -> ExactRational
  -> StatusTree
  -> ExactSweepSegmentId
  -> Either ExactSegmentEventObstruction StatusTree
deleteStatus metas side x tree segmentId = do
  meta <- requireMeta metas segmentId
  pure (statusDelete (statusCompare side x) meta tree)

statusInsert
  :: (SegmentMeta -> SegmentMeta -> Ordering)
  -> SegmentMeta
  -> StatusTree
  -> StatusTree
statusInsert compareIds value tree =
  case tree of
    StatusEmpty -> statusNode StatusEmpty value StatusEmpty
    StatusNode _ left current right ->
      case compareIds value current of
        LT -> statusBalance (statusNode (statusInsert compareIds value left) current right)
        GT -> statusBalance (statusNode left current (statusInsert compareIds value right))
        EQ -> tree

statusDelete
  :: (SegmentMeta -> SegmentMeta -> Ordering)
  -> SegmentMeta
  -> StatusTree
  -> StatusTree
statusDelete compareIds value tree =
  case tree of
    StatusEmpty -> StatusEmpty
    StatusNode _ left current right ->
      case compareIds value current of
        LT -> statusBalance (statusNode (statusDelete compareIds value left) current right)
        GT -> statusBalance (statusNode left current (statusDelete compareIds value right))
        EQ -> statusMerge left right

statusMerge :: StatusTree -> StatusTree -> StatusTree
statusMerge left StatusEmpty = left
statusMerge left right =
  case statusDeleteLeast right of
    Nothing -> left
    Just (least, remaining) -> statusBalance (statusNode left least remaining)

statusDeleteLeast :: StatusTree -> Maybe (SegmentMeta, StatusTree)
statusDeleteLeast StatusEmpty = Nothing
statusDeleteLeast (StatusNode _ StatusEmpty value right) = Just (value, right)
statusDeleteLeast (StatusNode _ left value right) = do
  (least, remaining) <- statusDeleteLeast left
  pure (least, statusBalance (statusNode remaining value right))

statusHeight :: StatusTree -> Int
statusHeight StatusEmpty = 0
statusHeight (StatusNode height _ _ _) = height

statusNode :: StatusTree -> SegmentMeta -> StatusTree -> StatusTree
statusNode left value right =
  StatusNode
    (1 + max (statusHeight left) (statusHeight right))
    left
    value
    right

statusBalance :: StatusTree -> StatusTree
statusBalance tree =
  case tree of
    StatusEmpty -> StatusEmpty
    StatusNode _ left value right
      | statusHeight left - statusHeight right > 1 -> balanceLeft left value right
      | statusHeight right - statusHeight left > 1 -> balanceRight left value right
      | otherwise -> statusNode left value right

balanceLeft :: StatusTree -> SegmentMeta -> StatusTree -> StatusTree
balanceLeft left value right =
  case left of
    StatusNode _ leftLeft leftValue leftRight
      | statusHeight leftLeft >= statusHeight leftRight ->
          statusNode leftLeft leftValue (statusNode leftRight value right)
      | otherwise ->
          case leftRight of
            StatusNode _ middleLeft middleValue middleRight ->
              statusNode
                (statusNode leftLeft leftValue middleLeft)
                middleValue
                (statusNode middleRight value right)
            StatusEmpty -> statusNode left value right
    StatusEmpty -> statusNode left value right

balanceRight :: StatusTree -> SegmentMeta -> StatusTree -> StatusTree
balanceRight left value right =
  case right of
    StatusNode _ rightLeft rightValue rightRight
      | statusHeight rightRight >= statusHeight rightLeft ->
          statusNode (statusNode left value rightLeft) rightValue rightRight
      | otherwise ->
          case rightLeft of
            StatusNode _ middleLeft middleValue middleRight ->
              statusNode
                (statusNode left value middleLeft)
                middleValue
                (statusNode middleRight rightValue rightRight)
            StatusEmpty -> statusNode left value right
    StatusEmpty -> statusNode left value right

statusAtY
  :: ExactRational
  -> ExactRational
  -> StatusTree
  -> [ExactSweepSegmentId]
statusAtY x y = descend
 where
  descend StatusEmpty = []
  descend (StatusNode _ left meta right) =
    case compareMetaToY x y meta of
      LT -> descend right
      GT -> descend left
      EQ -> descend left <> [metaId meta] <> descend right

statusRangeByY
  :: ExactRational
  -> ExactRational
  -> ExactRational
  -> StatusTree
  -> [ExactSweepSegmentId]
statusRangeByY x lower upper = descend
 where
  descend StatusEmpty = []
  descend (StatusNode _ left meta right) =
    let below = compareMetaToY x lower meta == LT
        above = compareMetaToY x upper meta == GT
     in if below
          then descend right
          else
            if above
              then descend left
              else descend left <> [metaId meta] <> descend right

compareMetaToY :: ExactRational -> ExactRational -> SegmentMeta -> Ordering
compareMetaToY x y meta =
  let (numerator, denominator) = ordinateFraction x meta
   in compare numerator (y * denominator)

statusBelowAbove
  :: ExactRational
  -> ExactRational
  -> StatusTree
  -> (Maybe ExactSweepSegmentId, Maybe ExactSweepSegmentId)
statusBelowAbove x y = descend Nothing Nothing
 where
  descend below above StatusEmpty = (below, above)
  descend below above (StatusNode _ left meta right) =
    case compareMetaToY x y meta of
      LT -> descend (Just (metaId meta)) above right
      GT -> descend below (Just (metaId meta)) left
      EQ -> (statusGreatest left <|> below, statusLeast right <|> above)

statusPredecessor
  :: ExactRational
  -> SegmentMeta
  -> StatusTree
  -> Maybe ExactSweepSegmentId
statusPredecessor x target = descend Nothing
 where
  descend candidate StatusEmpty = candidate
  descend candidate (StatusNode _ left current right) =
    case statusCompare StatusAfter x target current of
      LT -> descend candidate left
      GT -> descend (Just (metaId current)) right
      EQ -> statusGreatest left <|> candidate

statusSuccessor
  :: ExactRational
  -> SegmentMeta
  -> StatusTree
  -> Maybe ExactSweepSegmentId
statusSuccessor x target = descend Nothing
 where
  descend candidate StatusEmpty = candidate
  descend candidate (StatusNode _ left current right) =
    case statusCompare StatusAfter x target current of
      LT -> descend (Just (metaId current)) left
      GT -> descend candidate right
      EQ -> statusLeast right <|> candidate

statusLeast :: StatusTree -> Maybe ExactSweepSegmentId
statusLeast StatusEmpty = Nothing
statusLeast (StatusNode _ StatusEmpty value _) = Just (metaId value)
statusLeast (StatusNode _ left _ _) = statusLeast left

statusGreatest :: StatusTree -> Maybe ExactSweepSegmentId
statusGreatest StatusEmpty = Nothing
statusGreatest (StatusNode _ _ value StatusEmpty) = Just (metaId value)
statusGreatest (StatusNode _ _ _ right) = statusGreatest right

lookupMeta :: V.Vector SegmentMeta -> ExactSweepSegmentId -> Maybe SegmentMeta
lookupMeta metas (ExactSweepSegmentId index) = metas V.!? index

requireMeta
  :: V.Vector SegmentMeta
  -> ExactSweepSegmentId
  -> Either ExactSegmentEventObstruction SegmentMeta
requireMeta metas segmentId =
  case lookupMeta metas segmentId of
    Just meta -> Right meta
    Nothing -> Left (ExactSweepSegmentMissing segmentId)

exactSegmentEvents :: ExactSegmentEventPlan -> [ExactSegmentEvent]
exactSegmentEvents = Map.elems . plannedEvents

exactSegmentSplitPoints
  :: ExactSegmentEventPlan
  -> ExactSweepSegmentId
  -> [ExactPoint]
exactSegmentSplitPoints plan (ExactSweepSegmentId segmentIndex) =
  maybe [] id (plannedSplitPoints plan V.!? segmentIndex)

exactSegmentRelationMap
  :: ExactSegmentEventPlan
  -> Map (ExactSweepSegmentId, ExactSweepSegmentId) SegmentRelation
exactSegmentRelationMap = Map.map eventRelation . plannedEvents

eventRelation :: ExactSegmentEvent -> SegmentRelation
eventRelation event =
  case event of
    ExactProperCrossing {} -> SegmentsProperlyCross
    ExactEndpointTouch {} -> SegmentEndpointTouchesInterior
    ExactSharedEndpoint {} -> SegmentsShareEndpoint
    ExactDuplicateSegments {} -> SegmentsDuplicate
    ExactCollinearOverlap {} -> SegmentsCollinearlyOverlap

exactSegmentPairChecks :: ExactSegmentEventPlan -> Int
exactSegmentPairChecks = plannedPairChecks

exactSegmentSweepMaximumHeight :: ExactSegmentEventPlan -> Int
exactSegmentSweepMaximumHeight = plannedMaximumHeight

sortAlong :: ExactSegment -> [ExactPoint] -> [ExactPoint]
sortAlong segment =
  let (from, to) = exactSegmentEndpoints segment
   in if from <= to then Set.toAscList . Set.fromList else Set.toDescList . Set.fromList

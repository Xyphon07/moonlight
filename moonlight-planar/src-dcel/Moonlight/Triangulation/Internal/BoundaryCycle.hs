-- | Shared pure algebra for descending oriented boundary graphs into simple
-- cycles, then simplifying and classifying those cycles.
module Moonlight.Triangulation.Internal.BoundaryCycle
  ( traceOrientedBoundaryCircuits
  , simplifyBoundaryCycle
  , rotateCycleLeast
  , rotateCycleLeastBy
  , consecutivePairs
  , unorderedPairs
  , orderedPair
  , cyclePairs
  , cyclePairsNonEmpty
  , cyclicTriples
  , firstNonCounterClockwiseTurn
  , admitsSimpleCycleEdgeRelation
  ) where

import Data.List (tails)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Moonlight.Triangulation.Internal.SegmentRelation
  ( SegmentRelation (..)
  )

-- | Consume every supplied oriented edge exactly once and split point contacts
-- into vertex-simple cycles. The outgoing lists carry the caller's required
-- local angular or identifier order. A malformed graph is translated directly
-- through the caller's obstruction constructor; this shared worker owns no
-- disposable error vocabulary.
traceOrientedBoundaryCircuits
  :: (Ord vertex, Ord edge)
  => (edge -> vertex)
  -> (edge -> vertex)
  -> (edge -> edge -> obstruction)
  -> Map vertex [edge]
  -> Set edge
  -> Either obstruction [NonEmpty vertex]
traceOrientedBoundaryCircuits edgeOrigin edgeDestination obstruction outgoing edges =
  descend edges []
 where
  descend remaining cycles =
    case Set.lookupMin remaining of
      Nothing -> Right (reverse cycles)
      Just seed ->
        let (untraced, circuitEdges) =
              eulerCircuit remaining (edgeOrigin seed)
         in case NonEmpty.nonEmpty circuitEdges of
              Nothing -> Left (obstruction seed seed)
              Just circuit ->
                case splitCircuit circuit of
                  Left failedEdge -> Left (obstruction seed failedEdge)
                  Right splitCycles ->
                    descend
                      untraced
                      (reverse (NonEmpty.toList splitCycles) <> cycles)

  -- Hierholzer descent: delete each chosen edge, then prepend it while
  -- backtracking. No graph-state stream or branch search is materialized.
  eulerCircuit remaining start = walk remaining start [] []
   where
    walk untraced current incomingEdges circuit =
      case
          List.find
            (`Set.member` untraced)
            (Map.findWithDefault [] current outgoing) of
        Just edge ->
          walk
            (Set.delete edge untraced)
            (edgeDestination edge)
            (edge : incomingEdges)
            circuit
        Nothing ->
          case incomingEdges of
            edge : previousEdges ->
              walk
                untraced
                (edgeOrigin edge)
                previousEdges
                (edge : circuit)
            [] -> (untraced, circuit)

  -- The resident path stays simple. Closing at a resident vertex emits and
  -- removes exactly that suffix, partitioning the Euler circuit into cycles.
  splitCircuit circuit =
    walk
      start
      0
      (Map.singleton start 0)
      []
      []
      (NonEmpty.toList circuit)
   where
    start = edgeOrigin (NonEmpty.head circuit)

    walk current depth depths reversedEdges cycles remaining =
      case remaining of
        [] ->
          case
              ( depth
              , reversedEdges
              , NonEmpty.nonEmpty (reverse cycles)
              ) of
            (0, [], Just simpleCycles) -> Right simpleCycles
            _ -> Left (NonEmpty.last circuit)
        edge : rest
          | edgeOrigin edge /= current -> Left edge
          | otherwise ->
              let target = edgeDestination edge
               in case Map.lookup target depths of
                    Nothing ->
                      walk
                        target
                        (depth + 1)
                        (Map.insert target (depth + 1) depths)
                        (edge : reversedEdges)
                        cycles
                        rest
                    Just repeatedDepth ->
                      let suffixLength = depth - repeatedDepth
                          suffixEdges = take suffixLength reversedEdges
                          simpleCycle =
                            fmap edgeOrigin
                              ( NonEmpty.reverse
                                  (edge :| suffixEdges)
                              )
                       in walk
                            target
                            repeatedDepth
                            ( foldr
                                (Map.delete . edgeDestination)
                                depths
                                suffixEdges
                            )
                            (drop suffixLength reversedEdges)
                            (simpleCycle : cycles)
                            rest
{-# INLINABLE traceOrientedBoundaryCircuits #-}

-- | Remove precisely the vertices admitted by @isRedundant@ until a fixed
-- point is reached, then classify the winding at the least keyed retained
-- vertex. The returned cycle preserves the tracer's start; publication layers
-- may rotate their value-level observation independently. The caller supplies
-- its obstruction constructor so the shared worker does not allocate a
-- disposable intermediate error vocabulary at either specialization.
simplifyBoundaryCycle
  :: (Eq value, Ord key)
  => ([value] -> obstruction)
  -> (value -> value -> value -> Bool)
  -> (value -> value -> value -> Ordering)
  -> (value -> key)
  -> [value]
  -> Either obstruction (Ordering, NonEmpty value)
simplifyBoundaryCycle obstruction isRedundant orientation key = descend
 where
  descend values@(_ : _ : _ : _) =
    let triples = cyclicTriples values
        retained =
          [ current
          | (previousValue, current, nextValue) <- triples
          , not (isRedundant previousValue current nextValue)
          ]
     in if retained == values
          then classify values triples
          else descend retained
  descend values = Left (obstruction values)

  classify values triples =
    case triples of
      [] -> Left (obstruction values)
      firstTriple : remainingTriples ->
        let (previousValue, current, nextValue) =
              List.foldl' chooseLeast firstTriple remainingTriples
            winding = orientation previousValue current nextValue
         in case (winding, values) of
              (EQ, _) -> Left (obstruction values)
              (_, initialValue : rest) -> Right (winding, initialValue :| rest)
              _ -> Left (obstruction values)

  chooseLeast selected@(_, selectedValue, _) candidate@(_, candidateValue, _)
    | key candidateValue < key selectedValue = candidate
    | otherwise = selected
{-# INLINE simplifyBoundaryCycle #-}

-- | Choose the least value as a cycle's observational origin without changing
-- its orientation. Boundary publication and generated convex geometry share
-- this one canonical rotation owner.
rotateCycleLeast :: Ord value => NonEmpty value -> NonEmpty value
rotateCycleLeast = rotateCycleLeastBy id
{-# INLINE rotateCycleLeast #-}

-- | Choose the least keyed value as a cycle's observational origin.
rotateCycleLeastBy
  :: Ord key
  => (value -> key)
  -> NonEmpty value
  -> NonEmpty value
rotateCycleLeastBy key values =
  case break ((== minimumKey) . key) asList of
    (before, selected : after) -> selected :| (after <> before)
    _ -> values
 where
  asList = NonEmpty.toList values
  minimumKey =
    List.foldl'
      (\selected candidate -> min selected (key candidate))
      (key (NonEmpty.head values))
      (NonEmpty.tail values)
{-# INLINE rotateCycleLeastBy #-}

-- | Every adjacent pair in a linear sequence.
consecutivePairs :: [value] -> [(value, value)]
consecutivePairs values = zip values (drop 1 values)
{-# INLINE consecutivePairs #-}

-- | Every unordered pair exactly once.
unorderedPairs :: [value] -> [(value, value)]
unorderedPairs values =
  [(left, right) | left : remaining <- tails values, right <- remaining]
{-# INLINE unorderedPairs #-}

-- | Canonically orient an unordered pair.
orderedPair :: Ord value => value -> value -> (value, value)
orderedPair left right
  | left <= right = (left, right)
  | otherwise = (right, left)
{-# INLINE orderedPair #-}

-- | Every directed edge of a non-empty cycle in cycle order.
cyclePairs :: NonEmpty value -> [(value, value)]
cyclePairs = NonEmpty.toList . cyclePairsNonEmpty
{-# INLINE cyclePairs #-}

-- | The non-empty form of 'cyclePairs'. A singleton cycle has its sole value
-- as both ends of its sole cyclic edge.
cyclePairsNonEmpty :: NonEmpty value -> NonEmpty (value, value)
cyclePairsNonEmpty values@(firstValue :| remaining) =
  NonEmpty.zip values successors
 where
  successors =
    case remaining of
      [] -> firstValue :| []
      nextValue : rest -> nextValue :| (rest <> [firstValue])
{-# INLINE cyclePairsNonEmpty #-}

-- | Consecutive cyclic triples, one centered at every value.
cyclicTriples :: [value] -> [(value, value, value)]
cyclicTriples values =
  case values of
    initial : second : remaining ->
      let final = List.foldl' (\_ current -> current) initial (second : remaining)
       in zip3
            (final : values)
            values
            (second : remaining <> [initial])
    _ -> []
{-# INLINE cyclicTriples #-}

-- | Locate the first turn that is not strictly counter-clockwise. Convex
-- boundary admissions share this one indexed obstruction descent.
firstNonCounterClockwiseTurn
  :: (value -> value -> value -> Ordering)
  -> NonEmpty value
  -> Maybe (Int, Ordering)
firstNonCounterClockwiseTurn orientation =
  List.find ((/= GT) . snd)
    . zip [0 ..]
    . fmap (\(previousValue, current, nextValue) -> orientation previousValue current nextValue)
    . cyclicTriples
    . NonEmpty.toList
{-# INLINE firstNonCounterClockwiseTurn #-}

-- | Whether one exact relation between two indexed cycle edges is compatible
-- with a simple closed boundary. Only adjacent edges may meet, and then only
-- at their shared endpoint. Producers remain free to obtain relation sections
-- through the sweep plan or direct pairwise descent; this is the single
-- admissibility policy glued over both representations.
admitsSimpleCycleEdgeRelation
  :: Int
  -> Int
  -> Int
  -> SegmentRelation
  -> Bool
admitsSimpleCycleEdgeRelation segmentCount leftIndex rightIndex relation =
  relation == SegmentsDisjoint
    || ( cycleEdgesAreAdjacent
           segmentCount leftIndex rightIndex
           && relation == SegmentsShareEndpoint
       )
{-# INLINE admitsSimpleCycleEdgeRelation #-}

cycleEdgesAreAdjacent :: Int -> Int -> Int -> Bool
cycleEdgesAreAdjacent segmentCount leftIndex rightIndex =
  upperIndex == lowerIndex + 1
    || (lowerIndex == 0 && upperIndex == segmentCount - 1)
 where
  lowerIndex = min leftIndex rightIndex
  upperIndex = max leftIndex rightIndex
{-# INLINE cycleEdgesAreAdjacent #-}

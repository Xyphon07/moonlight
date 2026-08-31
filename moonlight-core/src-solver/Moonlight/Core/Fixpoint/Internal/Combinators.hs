-- | Bounded fixpoint iteration, once-only integer worklists, and transitive
-- closure. Pure combinators over 'Moonlight.Core.Queue'; no mutation.
module Moonlight.Core.Fixpoint.Internal.Combinators
  ( FixpointDivergence (..),
    fixpointBounded,
    fixpointBoundedM,
    worklistFold,
    traverseOnceIntSet,
    reschedulingWorklistFoldIntSet,
    closureUnder,
    closureUnderInt,
    reachabilityFrom,
    reachabilityFromInt,
  )
where

import Data.Functor.Identity (Identity (..), runIdentity)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core.Queue (Queue, dequeue, enqueueAll, queueFromList)
import Numeric.Natural (Natural)
import Prelude

data FixpointDivergence a = FixpointDivergence
  { fixpointDivergenceBudget :: !Natural,
    fixpointDivergenceLast :: !a
  }
  deriving stock (Eq, Ord, Show, Read)

fixpointBounded :: Eq a => Natural -> (a -> a) -> a -> Either (FixpointDivergence a) a
fixpointBounded budget step =
  runIdentity . fixpointBoundedM budget (Identity . step)

fixpointBoundedM :: (Monad m, Eq a) => Natural -> (a -> m a) -> a -> m (Either (FixpointDivergence a) a)
fixpointBoundedM budget =
  fixpointBoundedWorker budget budget

fixpointBoundedWorker ::
  (Monad m, Eq a) =>
  Natural ->
  Natural ->
  (a -> m a) ->
  a ->
  m (Either (FixpointDivergence a) a)
fixpointBoundedWorker budget remaining step current
  | remaining == 0 = pure (Left (FixpointDivergence budget current))
  | otherwise = do
      next <- step current
      if next == current
        then pure (Right current)
        else fixpointBoundedWorker budget (remaining - 1) step next

worklistFold :: (state -> item -> (state, [item])) -> state -> Queue item -> state
worklistFold step state frontier =
  case dequeue frontier of
    Nothing -> state
    Just (item, remainingFrontier) ->
      case step state item of
        (nextState, queuedItems) ->
          nextState `seq` worklistFold step nextState (enqueueAll queuedItems remainingFrontier)

-- | Reachability primitive: each integer key is expanded at most once.
traverseOnceIntSet :: (state -> Int -> (state, IntSet)) -> state -> IntSet -> state
traverseOnceIntSet step =
  freshIntSetWorklist step IntSet.empty

-- | Fixpoint-style integer worklist: an item may run again after it has been
-- dequeued, but duplicate queued copies are collapsed while it is waiting.
reschedulingWorklistFoldIntSet :: (state -> Int -> (state, IntSet)) -> state -> IntSet -> state
reschedulingWorklistFoldIntSet step initialState initialFrontier =
  reschedulingIntSetWorklist
    step
    initialState
    (queueFromList (IntSet.toAscList initialFrontier))
    initialFrontier

reschedulingIntSetWorklist ::
  (state -> Int -> (state, IntSet)) ->
  state ->
  Queue Int ->
  IntSet ->
  state
reschedulingIntSetWorklist step state frontier queued =
  case dequeue frontier of
    Nothing ->
      state
    Just (item, remainingFrontier) ->
      let queuedAfterPop =
            IntSet.delete item queued
       in case step state item of
            (nextState, requestedItems) ->
              let freshItems =
                    IntSet.difference requestedItems queuedAfterPop
               in nextState `seq`
                    reschedulingIntSetWorklist
                      step
                      nextState
                      (enqueueAll (IntSet.toAscList freshItems) remainingFrontier)
                      (IntSet.union queuedAfterPop freshItems)

freshIntSetWorklist :: (state -> Int -> (state, IntSet)) -> IntSet -> state -> IntSet -> state
freshIntSetWorklist step processed state frontier =
  case IntSet.minView frontier of
    Nothing -> state
    Just (item, rest)
      | IntSet.member item processed ->
          freshIntSetWorklist step processed state rest
      | otherwise ->
          case step state item of
            (nextState, queued) ->
              let nextProcessed = IntSet.insert item processed
               in nextState `seq`
                    freshIntSetWorklist
                      step
                      nextProcessed
                      nextState
                      (IntSet.union rest (IntSet.difference queued nextProcessed))

closureUnder :: Ord value => (value -> Set value) -> Set value -> Set value
closureUnder =
  reachabilityFrom

closureUnderInt :: (Int -> IntSet) -> IntSet -> IntSet
closureUnderInt =
  reachabilityFromInt

reachabilityFrom :: Ord value => (value -> Set value) -> Set value -> Set value
reachabilityFrom expand seeds =
  worklistClosureSet expand seeds (queueFromList (Set.toAscList seeds))

reachabilityFromInt :: (Int -> IntSet) -> IntSet -> IntSet
reachabilityFromInt expand seeds =
  traverseOnceIntSet step seeds seeds
  where
    step visited value =
      let freshValues = IntSet.difference (expand value) visited
       in (IntSet.union visited freshValues, freshValues)

worklistClosureSet :: Ord value => (value -> Set value) -> Set value -> Queue value -> Set value
worklistClosureSet expand =
  worklistFold step
  where
    step visited value =
      let freshValues = Set.difference (expand value) visited
       in (Set.union visited freshValues, Set.toAscList freshValues)

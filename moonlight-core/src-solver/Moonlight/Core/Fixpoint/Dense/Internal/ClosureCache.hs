-- | A graph-bound memoized transitive-closure cache over the SCC condensation:
-- reachable-component closures are computed by depth-first descent, memoized per
-- descent, and materialized into the persistent cache once a component has been
-- queried often enough to earn its keep.
module Moonlight.Core.Fixpoint.Dense.Internal.ClosureCache
  ( SccClosureCache,
    sccClosureCacheFor,
    closureCacheGraph,
    closeComponents,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Moonlight.Core.Fixpoint.Dense.Internal.AdaptiveIntSet
  ( AdaptiveIntSet,
    adaptiveIntSetFromIntSet,
    adaptiveIntSetToIntSet,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Csr (csrTargetsSet)
import Moonlight.Core.Fixpoint.Dense.Internal.Scc
  ( FrozenDigraph (..),
    SccPlan (..),
  )
import Prelude

type SccClosureCache :: Type
data SccClosureCache = SccClosureCache
  { closureCacheGraph :: !FrozenDigraph,
    cachedClosures :: !(IntMap AdaptiveIntSet),
    queryCounts :: !(IntMap Int),
    materializeAfter :: !Int
  }
  deriving stock (Eq, Show)

type ClosureDescent :: Type
data ClosureDescent = ClosureDescent
  { cacheState :: !SccClosureCache,
    memo :: !(IntMap IntSet)
  }

sccClosureCacheFor :: FrozenDigraph -> SccClosureCache
sccClosureCacheFor graph =
  SccClosureCache
    { closureCacheGraph = graph,
      cachedClosures = IntMap.empty,
      queryCounts = IntMap.empty,
      materializeAfter = 2
    }

closeComponents :: IntSet -> SccClosureCache -> (IntSet, SccClosureCache)
closeComponents components cache =
  let (closed, descent) =
        IntSet.foldl'
          closeComponentInto
          ( IntSet.empty,
            ClosureDescent
              { cacheState = cache,
                memo = IntMap.empty
              }
          )
          components
   in (closed, cacheState descent)
  where
    plan =
      graphSccPlan (closureCacheGraph cache)
    closeComponentInto (closed, currentDescent) component =
      let (closedComponent, nextDescent) = cachedComponentClosure plan component currentDescent
       in (IntSet.union closed closedComponent, nextDescent)

cachedComponentClosure :: SccPlan -> Int -> ClosureDescent -> (IntSet, ClosureDescent)
cachedComponentClosure plan component descent =
  case lookupComponentClosure component descent of
    Just closure ->
      (closure, descent)
    Nothing ->
      let queriedDescent =
            descent
              { cacheState =
                  queryComponentClosure component (cacheState descent)
              }
          (closure, closedDescent) =
            componentClosure plan component queriedDescent
       in (closure, rememberComponentClosure component closure closedDescent)

componentClosure :: SccPlan -> Int -> ClosureDescent -> (IntSet, ClosureDescent)
componentClosure plan component descent =
  IntSet.foldl'
    closeSuccessor
    (IntSet.singleton component, descent)
    (csrTargetsSet (condensation plan) component)
  where
    closeSuccessor (closed, currentDescent) successor =
      let (successorClosure, nextDescent) =
            cachedComponentClosure plan successor currentDescent
       in (IntSet.union closed successorClosure, nextDescent)
{-# INLINE componentClosure #-}

lookupComponentClosure :: Int -> ClosureDescent -> Maybe IntSet
lookupComponentClosure component descent =
  case IntMap.lookup component (cachedClosures (cacheState descent)) of
    Just closure ->
      Just (adaptiveIntSetToIntSet closure)
    Nothing ->
      IntMap.lookup component (memo descent)
{-# INLINE lookupComponentClosure #-}

queryComponentClosure :: Int -> SccClosureCache -> SccClosureCache
queryComponentClosure component cache =
  cache
    { queryCounts =
        IntMap.insertWith (+) component 1 (queryCounts cache)
    }
{-# INLINE queryComponentClosure #-}

rememberComponentClosure :: Int -> IntSet -> ClosureDescent -> ClosureDescent
rememberComponentClosure component closure descent =
  descent
    { cacheState =
        cacheComponentClosure component closure (cacheState descent),
      memo =
        IntMap.insert component closure (memo descent)
    }
{-# INLINE rememberComponentClosure #-}

cacheComponentClosure :: Int -> IntSet -> SccClosureCache -> SccClosureCache
cacheComponentClosure component closure cache
  | queryCount >= max 1 (materializeAfter cache) =
      cache
        { cachedClosures =
            IntMap.insert component (adaptiveIntSetFromIntSet closure) (cachedClosures cache),
          queryCounts =
            IntMap.delete component (queryCounts cache)
        }
  | otherwise =
      cache
  where
    queryCount =
      IntMap.findWithDefault 0 component (queryCounts cache)
{-# INLINE cacheComponentClosure #-}

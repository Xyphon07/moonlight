-- | The adaptive sparse-push/dense-pull reachability engine and its @runST@
-- seals. Direction is chosen per frontier by work estimates; frozen digraphs
-- traverse the SCC condensation, snapshots traverse the live overlay. The four
-- public entry points are the only openings of 'runST' in the Dense kernel.
module Moonlight.Core.Fixpoint.Dense.Internal.Traverse
  ( frozenReachabilityFrom,
    frozenReachabilityWithPolicy,
    frozenReachabilityWithCache,
    snapshotReachabilityFrom,
  )
where

import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word32)
import Moonlight.Core.Fixpoint.Dense.Internal.ClosureCache
  ( SccClosureCache,
    closeComponents,
    closureCacheGraph,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Csr
  ( GraphCsr,
    csrOutDegree,
    csrTargetsForKey,
    csrVertexCount,
    inBounds,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Policy
  ( ReachabilityPolicy (..),
    defaultReachabilityPolicy,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Scc
  ( FrozenDigraph (..),
    SccPlan (..),
    expandComponents,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Scratch
  ( Frontier (..),
    ReachabilityScratch,
    ScratchSide (..),
    appendFreshTarget,
    appendSparseBuffer,
    flipScratchSide,
    frontierContainsAnyM,
    frontierEmpty,
    frontierFoldM',
    frontierMemberM,
    frontierSize,
    isMarked,
    markVectorFresh,
    markVisited,
    newReachabilityScratch,
    nextReachabilityGeneration,
    strictFoldM,
    visitedMarksToIntSet,
    writeDenseFrontierM,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Snapshot
  ( EdgeTombstones (..),
    GraphSnapshot (..),
    edgeLive,
  )
import Prelude

frozenReachabilityFrom :: FrozenDigraph -> IntSet -> IntSet
frozenReachabilityFrom =
  frozenReachabilityWithPolicy defaultReachabilityPolicy
{-# INLINE frozenReachabilityFrom #-}

frozenReachabilityWithPolicy :: ReachabilityPolicy -> FrozenDigraph -> IntSet -> IntSet
frozenReachabilityWithPolicy policy graph seeds =
  expandComponents plan reachedComponents
  where
    plan = graphSccPlan graph
    seedComponents =
      IntSet.fromList
        [ component
          | v <- IntSet.toAscList (IntSet.filter (inBounds (csrVertexCount (graphForward graph))) seeds),
            Just component <- [sccOfVertex plan U.!? v]
        ]
    reachedComponents =
      adaptiveReachability policy (condensation plan) (condensationBackward plan) seedComponents
{-# INLINE frozenReachabilityWithPolicy #-}

frozenReachabilityWithCache ::
  SccClosureCache ->
  IntSet ->
  (IntSet, SccClosureCache)
frozenReachabilityWithCache cache seeds =
  (expandComponents plan reachedComponents, nextCache)
  where
    plan =
      graphSccPlan graph
    graph =
      closureCacheGraph cache
    seedComponents =
      IntSet.fromList
        [ component
          | v <- IntSet.toAscList (IntSet.filter (inBounds (csrVertexCount (graphForward graph))) seeds),
            Just component <- [sccOfVertex plan U.!? v]
        ]
    (reachedComponents, nextCache) =
      closeComponents seedComponents cache

snapshotReachabilityFrom :: GraphSnapshot -> IntSet -> IntSet
snapshotReachabilityFrom snapshot seeds
  | IntMap.null (insertedEdgeOverlay snapshot) && Set.null tombstones =
      frozenReachabilityFrom (frozenBase snapshot) seeds
  | otherwise =
      snapshotReachabilityWithPolicy defaultReachabilityPolicy snapshot seeds
  where
    tombstones =
      unEdgeTombstones (deletedEdges snapshot)

adaptiveReachability :: ReachabilityPolicy -> GraphCsr -> GraphCsr -> IntSet -> IntSet
adaptiveReachability policy forward backward seeds =
  runST $ do
    scratch <- newReachabilityScratch (csrVertexCount forward)
    adaptiveReachabilityWithScratch policy forward backward scratch seeds
{-# INLINE adaptiveReachability #-}

snapshotReachabilityWithPolicy :: ReachabilityPolicy -> GraphSnapshot -> IntSet -> IntSet
snapshotReachabilityWithPolicy policy snapshot seeds =
  runST $ do
    scratch <- newReachabilityScratch vertexCount
    generation <- nextReachabilityGeneration scratch
    seedLength <- markVectorFresh scratch generation ScratchA boundedSeedVector
    frontier <- frontierFromSparseM policy scratch ScratchA ScratchA seedLength
    let advanceFrontier currentFrontier targetSide = do
          pull <- useSnapshotPullM policy snapshot scratch generation currentFrontier
          if pull
            then snapshotDensePullM snapshot scratch generation currentFrontier targetSide
            else snapshotSparsePushM snapshot scratch generation currentFrontier targetSide
    traverseFrontiersWith policy scratch advanceFrontier frontier ScratchB ScratchB
    visitedMarksToIntSet scratch generation
  where
    vertexCount =
      csrVertexCount (graphForward (frozenBase snapshot))
    boundedSeedVector =
      U.fromList (IntSet.toAscList (IntSet.filter (inBounds vertexCount) seeds))
{-# INLINE snapshotReachabilityWithPolicy #-}

adaptiveReachabilityWithScratch :: ReachabilityPolicy -> GraphCsr -> GraphCsr -> ReachabilityScratch state -> IntSet -> ST state IntSet
adaptiveReachabilityWithScratch policy forward backward scratch seeds = do
  generation <- nextReachabilityGeneration scratch
  seedLength <- markVectorFresh scratch generation ScratchA boundedSeedVector
  frontier <- frontierFromSparseM policy scratch ScratchA ScratchA seedLength
  let advanceFrontier currentFrontier targetSide = do
        pull <- usePullM policy forward backward scratch generation currentFrontier
        if pull
          then densePullM backward scratch generation currentFrontier targetSide
          else sparsePushM forward scratch generation currentFrontier targetSide
  traverseFrontiersWith policy scratch advanceFrontier frontier ScratchB ScratchB
  visitedMarksToIntSet scratch generation
  where
    boundedSeedVector =
      U.fromList (IntSet.toAscList (IntSet.filter (inBounds (csrVertexCount forward)) seeds))
{-# INLINE adaptiveReachabilityWithScratch #-}

traverseFrontiersWith ::
  ReachabilityPolicy ->
  ReachabilityScratch state ->
  (Frontier -> ScratchSide -> ST state Int) ->
  Frontier ->
  ScratchSide ->
  ScratchSide ->
  ST state ()
traverseFrontiersWith policy scratch advanceFrontier frontier nextSparseSide nextDenseSide
  | frontierEmpty frontier = pure ()
  | otherwise = do
      nextLength <- advanceFrontier frontier nextSparseSide
      nextFrontier <- frontierFromSparseM policy scratch nextSparseSide nextDenseSide nextLength
      traverseFrontiersWith
        policy
        scratch
        advanceFrontier
        nextFrontier
        (flipScratchSide nextSparseSide)
        (flipScratchSide nextDenseSide)
{-# INLINE traverseFrontiersWith #-}

usePullM :: ReachabilityPolicy -> GraphCsr -> GraphCsr -> ReachabilityScratch state -> Word32 -> Frontier -> ST state Bool
usePullM policy forward backward scratch generation frontier
  | frontierSize frontier <= smallFrontierLimit policy = pure False
  | otherwise = do
      pullWork <- max 1 <$> unvisitedIncomingWorkM backward scratch generation
      pushWork <- frontierWorkM forward scratch frontier
      pure (fromIntegral pushWork > frontierPullThreshold policy frontier * fromIntegral pullWork)
{-# INLINE usePullM #-}

useSnapshotPullM :: ReachabilityPolicy -> GraphSnapshot -> ReachabilityScratch state -> Word32 -> Frontier -> ST state Bool
useSnapshotPullM policy snapshot =
  usePullM policy (graphForward (frozenBase snapshot)) (graphBackward (frozenBase snapshot))
{-# INLINE useSnapshotPullM #-}

frontierPullThreshold :: ReachabilityPolicy -> Frontier -> Double
frontierPullThreshold policy frontier =
  case frontier of
    SparseFrontier _ _ ->
      pushToPullRatio policy
    DenseFrontier _ _ ->
      pullToPushRatio policy
{-# INLINE frontierPullThreshold #-}

frontierFromSparseM :: ReachabilityPolicy -> ReachabilityScratch state -> ScratchSide -> ScratchSide -> Int -> ST state Frontier
frontierFromSparseM policy scratch sparseSide denseSide frontierLength
  | frontierLength <= smallFrontierLimit policy = pure (SparseFrontier sparseSide frontierLength)
  | otherwise = do
      writeDenseFrontierM scratch sparseSide denseSide frontierLength
      pure (DenseFrontier denseSide frontierLength)
{-# INLINE frontierFromSparseM #-}

frontierWorkM :: GraphCsr -> ReachabilityScratch state -> Frontier -> ST state Int
frontierWorkM csr scratch =
  frontierFoldM' scratch (\acc v -> pure (acc + csrOutDegree csr v)) 0
{-# INLINE frontierWorkM #-}

unvisitedIncomingWorkM :: GraphCsr -> ReachabilityScratch state -> Word32 -> ST state Int
unvisitedIncomingWorkM backward scratch generation =
  strictFoldM step 0 [0 .. csrVertexCount backward - 1]
  where
    step acc vertex = do
      seen <- isMarked scratch generation vertex
      pure (if seen then acc else acc + csrOutDegree backward vertex)
{-# INLINE unvisitedIncomingWorkM #-}

sparsePushM :: GraphCsr -> ReachabilityScratch state -> Word32 -> Frontier -> ScratchSide -> ST state Int
sparsePushM forward scratch generation frontier targetSide =
  frontierFoldM'
    scratch
    ( \fresh source ->
        U.foldM'
          (appendFreshTarget scratch generation targetSide)
          fresh
          (csrTargetsForKey forward source)
    )
    0
    frontier
{-# INLINE sparsePushM #-}

snapshotSparsePushM :: GraphSnapshot -> ReachabilityScratch state -> Word32 -> Frontier -> ScratchSide -> ST state Int
snapshotSparsePushM snapshot scratch generation frontier targetSide =
  frontierFoldM'
    scratch
    (appendFreshSnapshotTargets snapshot scratch generation targetSide)
    0
    frontier
{-# INLINE snapshotSparsePushM #-}

snapshotDensePullM :: GraphSnapshot -> ReachabilityScratch state -> Word32 -> Frontier -> ScratchSide -> ST state Int
snapshotDensePullM snapshot scratch generation frontier targetSide = do
  baseFreshLength <- snapshotDenseBasePullM snapshot scratch generation frontier targetSide
  snapshotOverlayPushM snapshot scratch generation frontier targetSide baseFreshLength
{-# INLINE snapshotDensePullM #-}

snapshotDenseBasePullM :: GraphSnapshot -> ReachabilityScratch state -> Word32 -> Frontier -> ScratchSide -> ST state Int
snapshotDenseBasePullM snapshot scratch generation frontier targetSide =
  strictFoldM step 0 [0 .. csrVertexCount backward - 1]
  where
    backward =
      graphBackward (frozenBase snapshot)

    step freshLength target = do
      seen <- isMarked scratch generation target
      if seen
        then pure freshLength
        else do
          reached <- frontierContainsAnyLiveBaseIncomingM snapshot scratch frontier target (csrTargetsForKey backward target)
          if reached
            then markVisited scratch generation target *> appendSparseBuffer scratch targetSide freshLength target
            else pure freshLength
{-# INLINE snapshotDenseBasePullM #-}

frontierContainsAnyLiveBaseIncomingM ::
  GraphSnapshot ->
  ReachabilityScratch state ->
  Frontier ->
  Int ->
  Vector Int ->
  ST state Bool
frontierContainsAnyLiveBaseIncomingM snapshot scratch frontier target =
  U.foldr
    ( \source remaining ->
        if edgeLive snapshot source target
          then do
            found <- frontierMemberM scratch frontier source
            if found then pure True else remaining
          else remaining
    )
    (pure False)
{-# INLINE frontierContainsAnyLiveBaseIncomingM #-}

snapshotOverlayPushM :: GraphSnapshot -> ReachabilityScratch state -> Word32 -> Frontier -> ScratchSide -> Int -> ST state Int
snapshotOverlayPushM snapshot scratch generation frontier targetSide initialFreshLength =
  frontierFoldM'
    scratch
    (appendFreshSnapshotOverlayTargets snapshot scratch generation targetSide)
    initialFreshLength
    frontier
{-# INLINE snapshotOverlayPushM #-}

appendFreshSnapshotOverlayTargets ::
  GraphSnapshot ->
  ReachabilityScratch state ->
  Word32 ->
  ScratchSide ->
  Int ->
  Int ->
  ST state Int
appendFreshSnapshotOverlayTargets snapshot scratch generation targetSide freshLength source =
  strictFoldM
    (appendLiveSnapshotTarget snapshot scratch generation targetSide source)
    freshLength
    (IntSet.toAscList (IntMap.findWithDefault IntSet.empty source (insertedEdgeOverlay snapshot)))
{-# INLINE appendFreshSnapshotOverlayTargets #-}

appendFreshSnapshotTargets ::
  GraphSnapshot ->
  ReachabilityScratch state ->
  Word32 ->
  ScratchSide ->
  Int ->
  Int ->
  ST state Int
appendFreshSnapshotTargets snapshot scratch generation targetSide freshLength source = do
  baseFreshLength <-
    U.foldM'
      (appendLiveSnapshotTarget snapshot scratch generation targetSide source)
      freshLength
      (csrTargetsForKey (graphForward (frozenBase snapshot)) source)
  strictFoldM
    (appendLiveSnapshotTarget snapshot scratch generation targetSide source)
    baseFreshLength
    (IntSet.toAscList (IntMap.findWithDefault IntSet.empty source (insertedEdgeOverlay snapshot)))
{-# INLINE appendFreshSnapshotTargets #-}

appendLiveSnapshotTarget ::
  GraphSnapshot ->
  ReachabilityScratch state ->
  Word32 ->
  ScratchSide ->
  Int ->
  Int ->
  Int ->
  ST state Int
appendLiveSnapshotTarget snapshot scratch generation targetSide source freshLength target
  | edgeLive snapshot source target =
      appendFreshTarget scratch generation targetSide freshLength target
  | otherwise =
      pure freshLength
{-# INLINE appendLiveSnapshotTarget #-}

densePullM :: GraphCsr -> ReachabilityScratch state -> Word32 -> Frontier -> ScratchSide -> ST state Int
densePullM backward scratch generation frontier targetSide =
  strictFoldM step 0 [0 .. csrVertexCount backward - 1]
  where
    step freshLength vertex = do
      seen <- isMarked scratch generation vertex
      if seen
        then pure freshLength
        else do
          reached <- frontierContainsAnyM scratch frontier (csrTargetsForKey backward vertex)
          if reached
            then markVisited scratch generation vertex *> appendSparseBuffer scratch targetSide freshLength vertex
            else pure freshLength
{-# INLINE densePullM #-}

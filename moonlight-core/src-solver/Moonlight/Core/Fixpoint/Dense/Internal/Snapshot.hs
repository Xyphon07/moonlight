-- | A mutable-graph overlay over a frozen digraph: inserted-edge overlay,
-- deleted-edge tombstones, epoch-driven compaction, and the pure live-edge and
-- successor projections the traversal engine consumes. Entirely pure — no ST.
module Moonlight.Core.Fixpoint.Dense.Internal.Snapshot
  ( Edge (..),
    EdgeTombstones (..),
    GraphSnapshot (..),
    emptyEdgeTombstones,
    snapshotFromFrozen,
    insertSnapshotEdge,
    deleteSnapshotEdge,
    compactSnapshot,
    needsCompaction,
    edgeLive,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector.Unboxed qualified as U
import Moonlight.Core.Fixpoint.Dense.Internal.Csr
  ( csrTargets,
    csrTargetsForKey,
    csrTargetsSet,
    csrVertexCount,
    inBounds,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Scc
  ( FrozenDigraph (..),
    frozenDigraphFromSuccessors,
  )
import Prelude

type Edge :: Type
data Edge = Edge
  { edgeSource :: !Int,
    edgeTarget :: !Int
  }
  deriving stock (Eq, Ord, Show)

type EdgeTombstones :: Type
newtype EdgeTombstones = EdgeTombstones
  { unEdgeTombstones :: Set Edge
  }
  deriving stock (Eq, Show)

type GraphSnapshot :: Type
data GraphSnapshot = GraphSnapshot
  { frozenBase :: !FrozenDigraph,
    insertedEdgeOverlay :: !(IntMap IntSet),
    deletedEdges :: !EdgeTombstones,
    epoch :: !Int
  }
  deriving stock (Eq, Show)

emptyEdgeTombstones :: EdgeTombstones
emptyEdgeTombstones =
  EdgeTombstones Set.empty

snapshotFromFrozen :: FrozenDigraph -> GraphSnapshot
snapshotFromFrozen graph =
  GraphSnapshot
    { frozenBase = graph,
      insertedEdgeOverlay = IntMap.empty,
      deletedEdges = emptyEdgeTombstones,
      epoch = 0
    }

insertSnapshotEdge :: Edge -> GraphSnapshot -> GraphSnapshot
insertSnapshotEdge edge snapshot =
  if not (edgeInBounds snapshot edge) || snapshotContainsEdge snapshot edge
    then snapshot
    else
      applyEdgeEdit
        ( if baseContainsEdge snapshot edge
            then insertedEdgeOverlay snapshot
            else
              IntMap.insertWith
                IntSet.union
                (edgeSource edge)
                (IntSet.singleton (edgeTarget edge))
                (insertedEdgeOverlay snapshot)
        )
        (deleteTombstone edge (deletedEdges snapshot))
        snapshot

deleteSnapshotEdge :: Edge -> GraphSnapshot -> GraphSnapshot
deleteSnapshotEdge edge snapshot =
  if not (snapshotContainsEdge snapshot edge)
    then snapshot
    else
      applyEdgeEdit
        ( IntMap.update
            (keepNonEmptyIntSet . IntSet.delete (edgeTarget edge))
            (edgeSource edge)
            (insertedEdgeOverlay snapshot)
        )
        ( if baseContainsEdge snapshot edge
            then insertTombstone edge (deletedEdges snapshot)
            else deletedEdges snapshot
        )
        snapshot

applyEdgeEdit :: IntMap IntSet -> EdgeTombstones -> GraphSnapshot -> GraphSnapshot
applyEdgeEdit nextOverlay nextTombstones snapshot =
  compactIfNeeded
    snapshot
      { insertedEdgeOverlay = nextOverlay,
        deletedEdges = nextTombstones,
        epoch = epoch snapshot + 1
      }

edgeInBounds :: GraphSnapshot -> Edge -> Bool
edgeInBounds snapshot edge =
  inBounds vertexCount (edgeSource edge)
    && inBounds vertexCount (edgeTarget edge)
  where
    vertexCount =
      csrVertexCount (graphForward (frozenBase snapshot))

snapshotContainsEdge :: GraphSnapshot -> Edge -> Bool
snapshotContainsEdge snapshot edge =
  edgeInBounds snapshot edge
    && not (edgeTombstoned snapshot edge)
    && (baseContainsEdge snapshot edge || overlayContainsEdge snapshot edge)

baseContainsEdge :: GraphSnapshot -> Edge -> Bool
baseContainsEdge snapshot edge =
  U.elem
    (edgeTarget edge)
    (csrTargetsForKey (graphForward (frozenBase snapshot)) (edgeSource edge))

overlayContainsEdge :: GraphSnapshot -> Edge -> Bool
overlayContainsEdge snapshot edge =
  IntSet.member
    (edgeTarget edge)
    (IntMap.findWithDefault IntSet.empty (edgeSource edge) (insertedEdgeOverlay snapshot))

edgeTombstoned :: GraphSnapshot -> Edge -> Bool
edgeTombstoned snapshot edge =
  Set.member edge (unEdgeTombstones (deletedEdges snapshot))

compactSnapshot :: GraphSnapshot -> GraphSnapshot
compactSnapshot snapshot =
  compactAtEpoch (epoch snapshot + 1) snapshot

compactIfNeeded :: GraphSnapshot -> GraphSnapshot
compactIfNeeded snapshot
  | needsCompaction snapshot =
      compactAtEpoch (epoch snapshot) snapshot
  | otherwise =
      snapshot
{-# INLINE compactIfNeeded #-}

needsCompaction :: GraphSnapshot -> Bool
needsCompaction snapshot =
  overlayEdgeCount snapshot + tombstoneCount snapshot
    > max 1 (baseEdgeCount snapshot `quot` compactionDivisor)
{-# INLINE needsCompaction #-}

compactionDivisor :: Int
compactionDivisor =
  10
{-# INLINE compactionDivisor #-}

baseEdgeCount :: GraphSnapshot -> Int
baseEdgeCount =
  U.length . csrTargets . graphForward . frozenBase
{-# INLINE baseEdgeCount #-}

overlayEdgeCount :: GraphSnapshot -> Int
overlayEdgeCount =
  IntMap.foldl' (\count targets -> count + IntSet.size targets) 0 . insertedEdgeOverlay
{-# INLINE overlayEdgeCount #-}

tombstoneCount :: GraphSnapshot -> Int
tombstoneCount (GraphSnapshot {deletedEdges = EdgeTombstones tombstones}) =
  Set.size tombstones
{-# INLINE tombstoneCount #-}

compactAtEpoch :: Int -> GraphSnapshot -> GraphSnapshot
compactAtEpoch epochValue snapshot =
  GraphSnapshot
    { frozenBase = frozenDigraphFromSuccessors vertexCount (successors snapshot),
      insertedEdgeOverlay = IntMap.empty,
      deletedEdges = emptyEdgeTombstones,
      epoch = epochValue
    }
  where
    vertexCount =
      csrVertexCount (graphForward (frozenBase snapshot))
{-# INLINE compactAtEpoch #-}

edgeLive :: GraphSnapshot -> Int -> Int -> Bool
edgeLive snapshot source target =
  inBounds (csrVertexCount (graphForward (frozenBase snapshot))) target
    && not (Set.member (Edge source target) tombstones)
  where
    EdgeTombstones tombstones =
      deletedEdges snapshot
{-# INLINE edgeLive #-}

successors :: GraphSnapshot -> Int -> IntSet
successors snapshot source =
  IntSet.filter
    (\target -> inBounds vertexCount target && not (tombstoned target))
    ( IntSet.union
        (csrTargetsSet (graphForward (frozenBase snapshot)) source)
        (IntMap.findWithDefault IntSet.empty source (insertedEdgeOverlay snapshot))
    )
  where
    vertexCount =
      csrVertexCount (graphForward (frozenBase snapshot))
    EdgeTombstones tombstones =
      deletedEdges snapshot
    tombstoned target =
      Set.member (Edge source target) tombstones

insertTombstone :: Edge -> EdgeTombstones -> EdgeTombstones
insertTombstone edge (EdgeTombstones tombstones) =
  EdgeTombstones (Set.insert edge tombstones)

deleteTombstone :: Edge -> EdgeTombstones -> EdgeTombstones
deleteTombstone edge (EdgeTombstones tombstones) =
  EdgeTombstones (Set.delete edge tombstones)

keepNonEmptyIntSet :: IntSet -> Maybe IntSet
keepNonEmptyIntSet values
  | IntSet.null values = Nothing
  | otherwise = Just values

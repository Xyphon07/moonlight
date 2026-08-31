-- | The sole public face of the Dense reachability kernel. The sealed
-- @Dense.Internal.*@ owners are re-exported here as a curated surface:
-- 'Csr', 'FrozenDigraph', and 'SccPlan' stay abstract — their field accessors
-- are exposed as a lawful read-only projection (inspect and force, never forge a
-- malformed CSR or plan), while their constructors remain withheld.
module Moonlight.Core.Fixpoint.Dense
  ( Csr,
    GraphCsr,
    RowCsr,
    csrVertexCount,
    csrOffsets,
    csrTargets,
    csrFromRows,
    csrTargetsForKey,
    csrTargetsSet,
    csrOutDegree,
    csrTranspose,
    SccPlan,
    sccOfVertex,
    sccMembers,
    condensation,
    condensationBackward,
    FrozenDigraph,
    graphForward,
    graphBackward,
    graphSccPlan,
    AdaptiveIntSet,
    ChunkedBitmap,
    BitmapChunk,
    SccClosureCache,
    Edge (..),
    EdgeTombstones,
    GraphSnapshot,
    ReachabilityPolicy,
    ReachabilityPolicyValidationError (..),
    mkReachabilityPolicy,
    defaultReachabilityPolicy,
    sccClosureCacheFor,
    emptyEdgeTombstones,
    frozenDigraphFromSuccessors,
    frozenReachabilityFrom,
    frozenReachabilityWithPolicy,
    frozenReachabilityWithCache,
    snapshotFromFrozen,
    snapshotReachabilityFrom,
    needsCompaction,
    insertSnapshotEdge,
    deleteSnapshotEdge,
    compactSnapshot,
  )
where

import Moonlight.Core.Fixpoint.Dense.Internal.AdaptiveIntSet
  ( AdaptiveIntSet,
    BitmapChunk,
    ChunkedBitmap,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.ClosureCache
  ( SccClosureCache,
    sccClosureCacheFor,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Csr
  ( Csr,
    GraphCsr,
    RowCsr,
    csrFromRows,
    csrOffsets,
    csrOutDegree,
    csrTargets,
    csrTargetsForKey,
    csrTargetsSet,
    csrTranspose,
    csrVertexCount,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Policy
  ( ReachabilityPolicy,
    ReachabilityPolicyValidationError (..),
    defaultReachabilityPolicy,
    mkReachabilityPolicy,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Scc
  ( FrozenDigraph,
    SccPlan,
    condensation,
    condensationBackward,
    frozenDigraphFromSuccessors,
    graphBackward,
    graphForward,
    graphSccPlan,
    sccMembers,
    sccOfVertex,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Snapshot
  ( Edge (..),
    EdgeTombstones,
    GraphSnapshot,
    compactSnapshot,
    deleteSnapshotEdge,
    emptyEdgeTombstones,
    snapshotFromFrozen,
    needsCompaction,
    insertSnapshotEdge,
  )
import Moonlight.Core.Fixpoint.Dense.Internal.Traverse
  ( frozenReachabilityFrom,
    frozenReachabilityWithCache,
    frozenReachabilityWithPolicy,
    snapshotReachabilityFrom,
  )

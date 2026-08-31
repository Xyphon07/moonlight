-- | Strongly-connected-component condensation over CSR digraphs: the SCC plan
-- (vertex→component map, members, forward/backward condensation), frozen
-- digraph construction, and component expansion back to vertices.
module Moonlight.Core.Fixpoint.Dense.Internal.Scc
  ( SccPlan (..),
    FrozenDigraph (..),
    frozenDigraphFromSuccessors,
    expandComponents,
  )
where

import Data.Graph qualified as Graph
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as U
import Moonlight.Core.Fixpoint.Dense.Internal.Csr
  ( GraphCsr,
    RowCsr,
    csrFromBoundedRows,
    csrFromRows,
    csrTargetsForKey,
    csrTargetsSet,
    csrTranspose,
    csrVertexCount,
  )
import Prelude

type SccPlan :: Type
data SccPlan = SccPlan
  { sccOfVertex :: !(Vector Int),
    sccMembers :: !RowCsr,
    condensation :: !GraphCsr,
    condensationBackward :: !GraphCsr
  }
  deriving stock (Eq, Show)

type FrozenDigraph :: Type
data FrozenDigraph = FrozenDigraph
  { graphForward :: !GraphCsr,
    graphBackward :: !GraphCsr,
    graphSccPlan :: !SccPlan
  }
  deriving stock (Eq, Show)

frozenDigraphFromSuccessors :: Int -> (Int -> IntSet) -> FrozenDigraph
frozenDigraphFromSuccessors size successors =
  FrozenDigraph
    { graphForward = forward,
      graphBackward = backward,
      graphSccPlan = sccPlanFromCsr forward
    }
  where
    n = max 0 size
    forward = csrFromBoundedRows n [IntSet.toAscList (successors v) | v <- [0 .. n - 1]]
    backward = csrTranspose forward
{-# INLINE frozenDigraphFromSuccessors #-}

sccPlanFromCsr :: GraphCsr -> SccPlan
sccPlanFromCsr forward =
  SccPlan
    { sccOfVertex = vertexComponents,
      sccMembers = csrFromRows componentCount (fmap List.sort components),
      condensation = condensationForward,
      condensationBackward = csrTranspose condensationForward
    }
  where
    components =
      fmap sccVertices $
        Graph.stronglyConnComp
          [(v, v, U.toList (csrTargetsForKey forward v)) | v <- [0 .. csrVertexCount forward - 1]]
    componentCount = length components
    componentEntries = zip [0 ..] components
    vertexComponentMap =
      IntMap.fromList [(v, componentId) | (componentId, vs) <- componentEntries, v <- vs]
    vertexComponents =
      U.generate (csrVertexCount forward) (\v -> IntMap.findWithDefault v v vertexComponentMap)
    condensationForward =
      csrFromBoundedRows componentCount [IntSet.toAscList (successors componentId vs) | (componentId, vs) <- componentEntries]
    successors componentId vs =
      IntSet.delete componentId $
        IntSet.fromList
          [ targetComponent
            | v <- vs,
              target <- U.toList (csrTargetsForKey forward v),
              Just targetComponent <- [vertexComponents U.!? target]
          ]
    sccVertices :: Graph.SCC Int -> [Int]
    sccVertices scc =
      case scc of
        Graph.AcyclicSCC v -> [v]
        Graph.CyclicSCC vs -> List.sort vs
{-# INLINE sccPlanFromCsr #-}

expandComponents :: SccPlan -> IntSet -> IntSet
expandComponents plan =
  IntSet.foldl' (\acc component -> IntSet.union acc (csrTargetsSet (sccMembers plan) component)) IntSet.empty
{-# INLINE expandComponents #-}

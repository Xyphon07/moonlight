-- | The commit/abort boundary: thawing an immutable 'UnionFind' into a mutable
-- editor, materializing its dense prefix, and freezing a mutated editor back to
-- an immutable owner (returning the base unchanged when nothing was written).
module Moonlight.Core.UnionFind.Transaction.Internal.Lifecycle
  ( thawUnionFind,
    freezeUnionFind,
    materializeDenseBase,
  )
where

import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.STRef (newSTRef, readSTRef)
import Data.Vector.Unboxed.Mutable qualified as Mutable
import Moonlight.Core.Identifier.EGraph (ClassId (..))
import Moonlight.Core.UnionFind.Internal.Types (UnionFind (..))
import Moonlight.Core.UnionFind.Transaction.Internal.Policy
  ( chooseDenseLength,
    densePrefixMap,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Snapshot
  ( parentMap,
    rankMap,
  )
import Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( DenseStore (..),
    UnionFindEditor (..),
    denseFlagSet,
    denseFlagUnset,
  )
import Prelude

thawUnionFind :: UnionFind -> ST state (UnionFindEditor state)
thawUnionFind base = do
  let denseLength = chooseDenseLength (ufParent base)
      denseBaseParents = densePrefixMap denseLength (ufParent base)
  denseParents <- Mutable.replicate denseLength 0
  denseRanks <- Mutable.replicate denseLength 0
  densePresence <- Mutable.replicate denseLength denseFlagUnset
  dirtyParents <- Mutable.replicate denseLength denseFlagUnset
  dirtyRanks <- Mutable.replicate denseLength denseFlagUnset
  denseReference <-
    newSTRef
      DenseStore
        { parent = denseParents,
          rank = denseRanks,
          present = densePresence,
          parentDirty = dirtyParents,
          rankDirty = dirtyRanks
        }
  sparseParentWrites <- newSTRef IntMap.empty
  sparseRankWrites <- newSTRef IntMap.empty
  dirtyDenseParents <- newSTRef []
  dirtyDenseRanks <- newSTRef []
  dirtyDenseParentCount <- newSTRef 0
  dirtyDenseRankCount <- newSTRef 0
  denseMemberCount <- newSTRef (IntMap.size denseBaseParents)
  nextFresh <- newSTRef (ufNextFresh base)
  let editor =
        UnionFindEditor
          { base = base,
            dense = denseReference,
            sparseParentWrites = sparseParentWrites,
            sparseRankWrites = sparseRankWrites,
            dirtyDenseParents = dirtyDenseParents,
            dirtyDenseRanks = dirtyDenseRanks,
            dirtyDenseParentCount = dirtyDenseParentCount,
            dirtyDenseRankCount = dirtyDenseRankCount,
            denseMemberCount = denseMemberCount,
            nextFresh = nextFresh
          }
  materializeDenseBase editor denseBaseParents
  pure editor

freezeUnionFind :: UnionFindEditor state -> ST state UnionFind
freezeUnionFind editor = do
  sparseParentWrites <- readSTRef (sparseParentWrites editor)
  sparseRankWrites <- readSTRef (sparseRankWrites editor)
  dirtyDenseParents <- readSTRef (dirtyDenseParents editor)
  dirtyDenseRanks <- readSTRef (dirtyDenseRanks editor)
  nextFresh <- readSTRef (nextFresh editor)
  if IntMap.null sparseParentWrites
    && IntMap.null sparseRankWrites
    && null dirtyDenseParents
    && null dirtyDenseRanks
    && nextFresh == ufNextFresh (base editor)
    then pure (base editor)
    else do
      parents <- parentMap editor
      ranks <- rankMap editor
      pure
        UnionFind
          { ufParent = parents,
            ufRank = ranks,
            ufNextFresh = nextFresh
          }

materializeDenseBase ::
  UnionFindEditor state ->
  IntMap ClassId ->
  ST state ()
materializeDenseBase editor denseParents = do
  store <- readSTRef (dense editor)
  traverse_
    (\(key, ClassId parentKey) -> do
       Mutable.write (parent store) key parentKey
       Mutable.write (rank store) key (IntMap.findWithDefault 0 key (ufRank (base editor)))
       Mutable.write (present store) key denseFlagSet
    )
    (IntMap.toAscList denseParents)

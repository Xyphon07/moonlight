-- | Carrier types for the union-find transaction editor: the mutable dense
-- store, the sealed editor state token (with its nominal role), the union
-- outcome, and the dense sizing/flag constants.
module Moonlight.Core.UnionFind.Transaction.Internal.Types
  ( minimumDenseSlots,
    maximumDenseSlots,
    denseFlagUnset,
    denseFlagSet,
    DenseStore (..),
    UnionFindEditor (..),
    UnionOutcome (..),
  )
where

import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.STRef (STRef)
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Word (Word8)
import Moonlight.Core.Identifier.EGraph (ClassId)
import Moonlight.Core.UnionFind.Internal.Types (UnionFind)
import Prelude

minimumDenseSlots :: Int
minimumDenseSlots = 64

maximumDenseSlots :: Int
maximumDenseSlots = 262144

denseFlagUnset :: Word8
denseFlagUnset = 0

denseFlagSet :: Word8
denseFlagSet = 1

type DenseStore :: Type -> Type
data DenseStore state = DenseStore
  { parent :: !(MVector state Int),
    rank :: !(MVector state Int),
    present :: !(MVector state Word8),
    parentDirty :: !(MVector state Word8),
    rankDirty :: !(MVector state Word8)
  }

type UnionFindEditor :: Type -> Type
data UnionFindEditor state = UnionFindEditor
  { base :: !UnionFind,
    dense :: !(STRef state (DenseStore state)),
    sparseParentWrites :: !(STRef state (IntMap ClassId)),
    sparseRankWrites :: !(STRef state (IntMap Int)),
    dirtyDenseParents :: !(STRef state [Int]),
    dirtyDenseRanks :: !(STRef state [Int]),
    dirtyDenseParentCount :: !(STRef state Int),
    dirtyDenseRankCount :: !(STRef state Int),
    denseMemberCount :: !(STRef state Int),
    nextFresh :: !(STRef state Integer)
  }

type role UnionFindEditor nominal

data UnionOutcome
  = AlreadyEquivalent !ClassId
  | MergedClasses !ClassId !ClassId
  deriving stock (Eq, Show)

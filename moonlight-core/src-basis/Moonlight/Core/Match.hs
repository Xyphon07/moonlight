-- | Incremental-match bookkeeping: 'MatchFootprint' (the root, dependency, topo
-- and result node sets touched by a query) and 'QuerySnapshot'.
module Moonlight.Core.Match
  ( MatchFootprint (..),
    emptyFootprint,
    QuerySnapshot (..),
  )
where

import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Moonlight.Core.Relational (QueryId)
import Prelude

type MatchFootprint :: Type
data MatchFootprint = MatchFootprint
  { mfRoots :: !IntSet,
    mfDeps :: !IntSet,
    mfTopo :: !IntSet,
    mfResults :: !IntSet
  }
  deriving stock (Eq, Show)

emptyFootprint :: MatchFootprint
emptyFootprint =
  MatchFootprint
    { mfRoots = IntSet.empty,
      mfDeps = IntSet.empty,
      mfTopo = IntSet.empty,
      mfResults = IntSet.empty
    }

type QuerySnapshot :: Type -> Type -> Type
data QuerySnapshot projection relation = QuerySnapshot
  { baseRevision :: !Int,
    queryId :: !QueryId,
    liveEpoch :: !Int,
    liveRelations :: !(IntMap relation),
    projection :: !(IntMap projection),
    footprint :: !MatchFootprint
  }

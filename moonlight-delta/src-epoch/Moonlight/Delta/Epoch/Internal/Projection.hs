{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | The context projection join-semilattice: a pair of dirty-key sets under
-- componentwise union — a commutative idempotent monoid whose components are
-- canonical persistent sets read independently by consumers.  The carrier is
-- canonical by construction, so 'normalizeContextProjectionDelta' is the
-- identity: both components have no representational slack, they inhabit
-- different key spaces, and every eliminator reads one component alone, so
-- any non-identity normalization would alter observable content for some
-- input.  The exported witnesses answer to the 'DeltaNormalize' and
-- 'DeltaSupport' contracts and are enrolled in the package law harnesses.
module Moonlight.Delta.Epoch.Internal.Projection
  ( ContextProjectionDelta (..),
    emptyContextProjectionDelta,
    dirtyBaseDelta,
    dirtyResultDelta,
    normalizeContextProjectionDelta,
    nullContextProjectionDelta,
    mapContextProjectionDelta,
  )
where

import Data.Kind (Type)
import Moonlight.Core (OrdSet (..))
import Moonlight.Delta.Normalize (DeltaNormalize (..))
import Moonlight.Delta.Support (DeltaSupport (..))
import Prelude
  ( Bool,
    Eq,
    Functor,
    Monoid (..),
    Ord,
    Semigroup (..),
    Show,
    fmap,
    id,
    (&&),
    (.),
  )

type ContextProjectionDelta :: Type -> Type
data ContextProjectionDelta observed = ContextProjectionDelta
  { dirtyBaseKeys :: !observed,
    dirtyResultKeys :: !observed
  }
  deriving stock (Eq, Ord, Show, Functor)

instance OrdSet observed => Semigroup (ContextProjectionDelta observed) where
  leftDelta <> rightDelta =
    ContextProjectionDelta
      { dirtyBaseKeys =
          unionSet (dirtyBaseKeys leftDelta) (dirtyBaseKeys rightDelta),
        dirtyResultKeys =
          unionSet (dirtyResultKeys leftDelta) (dirtyResultKeys rightDelta)
      }

instance OrdSet observed => Monoid (ContextProjectionDelta observed) where
  mempty = emptyContextProjectionDelta

emptyContextProjectionDelta :: OrdSet observed => ContextProjectionDelta observed
emptyContextProjectionDelta =
  ContextProjectionDelta
    emptySet
    emptySet

dirtyBaseDelta :: OrdSet observed => SetKey observed -> ContextProjectionDelta observed
dirtyBaseDelta key =
  emptyContextProjectionDelta
    { dirtyBaseKeys = singletonSet key
    }

dirtyResultDelta :: OrdSet observed => SetKey observed -> ContextProjectionDelta observed
dirtyResultDelta key =
  emptyContextProjectionDelta
    { dirtyResultKeys = singletonSet key
    }

normalizeContextProjectionDelta :: ContextProjectionDelta observed -> ContextProjectionDelta observed
normalizeContextProjectionDelta =
  id
{-# INLINE normalizeContextProjectionDelta #-}

nullContextProjectionDelta :: OrdSet observed => ContextProjectionDelta observed -> Bool
nullContextProjectionDelta deltaValue =
  nullSet (dirtyBaseKeys deltaValue)
    && nullSet (dirtyResultKeys deltaValue)
{-# INLINE nullContextProjectionDelta #-}

instance OrdSet observed => DeltaNormalize (ContextProjectionDelta observed) where
  normalizeDelta =
    normalizeContextProjectionDelta

  deltaNull =
    nullContextProjectionDelta

instance OrdSet observed => DeltaSupport (ContextProjectionDelta observed) where
  type DeltaSupportSet (ContextProjectionDelta observed) = ContextProjectionDelta observed

  emptySupport =
    emptyContextProjectionDelta

  deltaSupport =
    normalizeContextProjectionDelta

mapContextProjectionDelta ::
  (OrdSet observed1, OrdSet observed2) =>
  (SetKey observed1 -> SetKey observed2) ->
  ContextProjectionDelta observed1 ->
  ContextProjectionDelta observed2
mapContextProjectionDelta rekey deltaValue =
  ContextProjectionDelta
    { dirtyBaseKeys = rekeySet rekey (dirtyBaseKeys deltaValue),
      dirtyResultKeys = rekeySet rekey (dirtyResultKeys deltaValue)
    }

rekeySet ::
  (OrdSet observed1, OrdSet observed2) =>
  (SetKey observed1 -> SetKey observed2) ->
  observed1 ->
  observed2
rekeySet rekey =
  fromListSet . fmap rekey . toAscListSet

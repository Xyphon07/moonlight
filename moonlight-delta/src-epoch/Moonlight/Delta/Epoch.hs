{-|
The epoch calculus: version-anchored key transport between endpoint snapshots.

@Version@ is an opaque, unbounded epoch counter. An @Endpoint@ pairs a version
with the result-key universe observed at that instant. An @EpochDelta@ is an
abstract partial transport: source keys either descend to target keys or retire,
and the carrier owns its target-side dirty set. @ContextProjectionDelta@ is the
bounded join-semilattice of dirty-key pairs; its @Semigroup@ and @Monoid@
structure (from "Data.Semigroup" and "Data.Monoid") is the seam consumed
upstream and is preserved verbatim.

A query against an @EpochDelta@ is partitioned into transported, retired, and
unknown keys.

> import Data.IntMap.Strict (IntMap)
> import Data.IntMap.Strict qualified as IntMap
> import Data.IntSet (IntSet)
> import Data.IntSet qualified as IntSet
> import Moonlight.Delta.Epoch qualified as Epoch
>
> source, target :: Epoch.Endpoint IntSet
> source = Epoch.Endpoint Epoch.initialVersion (IntSet.fromList [10, 20])
> target =
>   Epoch.Endpoint
>     (Epoch.nextVersion Epoch.initialVersion)
>     (IntSet.fromList [11, 30])
>
> advance
>   :: Either (Epoch.DeltaViolation Int) (Epoch.EpochDelta (IntMap Int) IntSet)
> advance =
>   Epoch.epochDelta source target
>     (IntMap.singleton 10 11) (IntSet.singleton 20) (IntSet.singleton 10)
>
> query
>   :: Either (Epoch.DeltaViolation Int) (Epoch.Transport (IntMap Int) IntSet)
> query =
>   fmap (\d -> Epoch.transportKeys d (IntSet.fromList [10, 20, 99])) advance

Invalid versions, universes, transports, retirements, or changed keys return a
@DeltaViolation@. Here @query@ partitions @10 -> 11@, retired @20@, and unknown
@99@; changed key @10@ and fresh key @30@ determine the dirty target set.
-}
module Moonlight.Delta.Epoch
  ( Version,
    initialVersion,
    nextVersion,
    versionKey,
    versionFromKey,
    ContextProjectionDelta (..),
    emptyContextProjectionDelta,
    dirtyBaseDelta,
    dirtyResultDelta,
    normalizeContextProjectionDelta,
    nullContextProjectionDelta,
    mapContextProjectionDelta,
    ContextView (..),
    viewAt,
    viewWithVersion,
    viewWithSupport,
    viewWithSection,
    mapContextViewKeys,
    contextViewIsCurrent,
    contextViewIsStale,
    EpochKeyed,
    Endpoint (..),
    EpochDelta,
    epochDelta,
    identityDelta,
    DeltaViolation (..),
    sourceEndpointOf,
    targetEndpointOf,
    sourceVersion,
    targetVersion,
    sourceKeys,
    targetKeys,
    transportOverrides,
    freshKeys,
    retiredKeys,
    changedKeysAcrossEpoch,
    transportKeys,
    Transport (..),
    ViewTransportError (..),
    transportView,
    ComposeError (..),
    composeDelta,
  )
where

import Moonlight.Delta.Epoch.Internal.Compose
  ( composeDelta,
  )
import Moonlight.Delta.Epoch.Internal.Construction
  ( epochDelta,
    identityDelta,
  )
import Moonlight.Delta.Epoch.Internal.Projection
  ( ContextProjectionDelta (..),
    dirtyBaseDelta,
    dirtyResultDelta,
    emptyContextProjectionDelta,
    mapContextProjectionDelta,
    normalizeContextProjectionDelta,
    nullContextProjectionDelta,
  )
import Moonlight.Delta.Epoch.Internal.Transport
  ( transportKeys,
    transportView,
  )
import Moonlight.Delta.Epoch.Internal.Types
  ( ComposeError (..),
    EpochDelta,
    DeltaViolation (..),
    Endpoint (..),
    EpochKeyed,
    Transport (..),
    ViewTransportError (..),
    changedKeysAcrossEpoch,
    freshKeys,
    retiredKeys,
    sourceEndpointOf,
    sourceKeys,
    sourceVersion,
    targetEndpointOf,
    targetKeys,
    targetVersion,
    transportOverrides,
  )
import Moonlight.Delta.Epoch.Internal.Version
  ( Version,
    versionFromKey,
    versionKey,
    initialVersion,
    nextVersion,
  )
import Moonlight.Delta.Epoch.Internal.View
  ( ContextView (..),
    contextViewIsCurrent,
    contextViewIsStale,
    mapContextViewKeys,
    viewAt,
    viewWithSection,
    viewWithSupport,
    viewWithVersion,
  )

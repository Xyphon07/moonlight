{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Carriers of the epoch calculus.
--
-- An 'Endpoint' is the complete object of the calculus: a version and the key
-- universe present at that version. An 'EpochDelta' stores a partial transport
-- from its source object to its target object. Source keys either survive via
-- the identity-almost-everywhere override or occur in the explicit retirement
-- set. Target dirtiness is stored directly because it is the compositional fact
-- consumed downstream.
module Moonlight.Delta.Epoch.Internal.Types
  ( EpochKeyed,
    Endpoint (..),
    EpochDelta (..),
    DeltaViolation (..),
    ViewTransportError (..),
    ComposeError (..),
    Transport (..),
    sourceEndpointOf,
    targetEndpointOf,
    sourceVersion,
    targetVersion,
    sourceKeys,
    targetKeys,
    transportOverrides,
    retiredKeys,
    freshKeys,
    changedKeysAcrossEpoch,
    transportKeyTotal,
    survivingTransportImage,
  )
where

import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Type.Equality (type (~))
import Moonlight.Core (OrdMap (..), OrdSet (..))
import Moonlight.Delta.Epoch.Internal.Version (Version)
import Moonlight.Delta.Normalize (DeltaNormalize (..))
import Moonlight.Delta.Support (DeltaSupport (..))
import Prelude
  ( Bool (..),
    Eq ((==)),
    Maybe (..),
    Show,
    fmap,
    fst,
    id,
    otherwise,
    snd,
    (&&),
  )

type EpochKeyed :: Type -> Type -> Constraint
type EpochKeyed keyMap observed =
  ( OrdMap keyMap,
    OrdSet observed,
    MapKey keyMap ~ SetKey observed,
    MapValue keyMap ~ SetKey observed,
    Eq (SetKey observed)
  )

type Endpoint :: Type -> Type
data Endpoint observed = Endpoint
  { endpointVersion :: !Version,
    endpointKeys :: !observed
  }
  deriving stock (Eq, Show)

type EpochDelta :: Type -> Type -> Type
data EpochDelta keyMap observed = EpochDelta
  { sourceEndpoint :: !(Endpoint observed),
    targetEndpoint :: !(Endpoint observed),
    transportOverride :: !keyMap,
    retiredSourceKeys :: !observed,
    dirtyTargetKeys :: !observed
  }
  deriving stock (Eq, Show)

type DeltaViolation :: Type -> Type
data DeltaViolation key
  = VersionDidNotAdvance !Version !Version
  | TransportDomainEscapesSource !key
  | TransportImageEscapesTarget !key !key
  | TransportDefinedForRetiredSource !key
  | RetiredKeyOutsideSource !key
  | SurvivingKeyOutsideTarget !key
  | ChangedKeyOutsideSource !key
  deriving stock (Eq, Show)

type ViewTransportError :: Type -> Type
data ViewTransportError key
  = ViewSourceVersionMismatch !Version !Version
  | ViewObservedKeyUnknown !key
  deriving stock (Eq, Show)

type ComposeError :: Type -> Type
data ComposeError key
  = ComposeVersionMismatch !Version !Version
  | ComposeUniverseMismatch
  deriving stock (Eq, Show)

-- | The descent result for an arbitrary query. The domain of
-- 'transportedKeys', 'transportRetiredKeys', and 'transportUnknownKeys'
-- partitions the queried source keys. Map values are always target keys.
type Transport :: Type -> Type -> Type
data Transport keyMap observed = Transport
  { transportedKeys :: !keyMap,
    transportRetiredKeys :: !observed,
    transportUnknownKeys :: !observed
  }
  deriving stock (Eq, Show)

sourceEndpointOf :: EpochDelta keyMap observed -> Endpoint observed
sourceEndpointOf =
  sourceEndpoint
{-# INLINE sourceEndpointOf #-}

targetEndpointOf :: EpochDelta keyMap observed -> Endpoint observed
targetEndpointOf =
  targetEndpoint
{-# INLINE targetEndpointOf #-}

sourceVersion :: EpochDelta keyMap observed -> Version
sourceVersion deltaValue =
  endpointVersion (sourceEndpoint deltaValue)
{-# INLINE sourceVersion #-}

targetVersion :: EpochDelta keyMap observed -> Version
targetVersion deltaValue =
  endpointVersion (targetEndpoint deltaValue)
{-# INLINE targetVersion #-}

sourceKeys :: EpochDelta keyMap observed -> observed
sourceKeys deltaValue =
  endpointKeys (sourceEndpoint deltaValue)
{-# INLINE sourceKeys #-}

targetKeys :: EpochDelta keyMap observed -> observed
targetKeys deltaValue =
  endpointKeys (targetEndpoint deltaValue)
{-# INLINE targetKeys #-}

transportOverrides :: EpochDelta keyMap observed -> keyMap
transportOverrides =
  transportOverride
{-# INLINE transportOverrides #-}

retiredKeys :: EpochDelta keyMap observed -> observed
retiredKeys =
  retiredSourceKeys
{-# INLINE retiredKeys #-}

transportKeyTotal ::
  EpochKeyed keyMap observed =>
  EpochDelta keyMap observed ->
  SetKey observed ->
  Maybe (SetKey observed)
transportKeyTotal deltaValue sourceKey
  | memberSet sourceKey (retiredSourceKeys deltaValue) =
      Nothing
  | otherwise =
      Just (fromMaybe sourceKey (lookupMap sourceKey (transportOverride deltaValue)))
{-# INLINABLE transportKeyTotal #-}

survivingTransportImage ::
  EpochKeyed keyMap observed =>
  observed ->
  keyMap ->
  observed ->
  observed
survivingTransportImage sourceKeySet transportMap retiredKeySet =
  case (nullMap transportMap, nullSet retiredKeySet) of
    (True, True) ->
      sourceKeySet
    (True, False) ->
      differenceSet sourceKeySet retiredKeySet
    (False, _) ->
      unionSet identitySurvivors transportedTargets
  where
    transportEntries =
      toAscListMap transportMap
    identitySurvivors =
      differenceSet
        (differenceSet sourceKeySet retiredKeySet)
        (fromListSet (fmap fst transportEntries))
    transportedTargets =
      fromListSet (fmap snd transportEntries)
{-# INLINABLE survivingTransportImage #-}

freshKeys ::
  EpochKeyed keyMap observed =>
  EpochDelta keyMap observed ->
  observed
freshKeys deltaValue =
  differenceSet
    (targetKeys deltaValue)
    ( survivingTransportImage
        (sourceKeys deltaValue)
        (transportOverride deltaValue)
        (retiredSourceKeys deltaValue)
    )
{-# INLINABLE freshKeys #-}

changedKeysAcrossEpoch :: EpochDelta keyMap observed -> observed
changedKeysAcrossEpoch =
  dirtyTargetKeys
{-# INLINE changedKeysAcrossEpoch #-}

instance
  (EpochKeyed keyMap observed, Eq observed) =>
  DeltaNormalize (EpochDelta keyMap observed)
  where
  normalizeDelta =
    id

  deltaNull deltaValue =
    nullMap (transportOverride deltaValue)
      && nullSet (retiredSourceKeys deltaValue)
      && nullSet (dirtyTargetKeys deltaValue)
      && sourceEndpoint deltaValue == targetEndpoint deltaValue

instance
  EpochKeyed keyMap observed =>
  DeltaSupport (EpochDelta keyMap observed)
  where
  type DeltaSupportSet (EpochDelta keyMap observed) = observed

  emptySupport =
    emptySet

  deltaSupport =
    changedKeysAcrossEpoch

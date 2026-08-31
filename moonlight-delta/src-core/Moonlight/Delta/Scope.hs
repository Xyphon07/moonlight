{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Dirty-scope tracking with one type-indexed notion of emptiness. A @Scope@
distinguishes no invalidation, a finite dirty set, and global invalidation.

> import Data.Set (Set)
> import Data.Set qualified as Set
> import Moonlight.Delta.Scope qualified as Scope
>
> dirtyPages :: Scope.Scope (Set Int)
> dirtyPages = Scope.dirtyScope (Set.fromList [2, 7])
>
> visibleDirtyPages :: Scope.Scope (Set Int)
> visibleDirtyPages = Scope.restrictScope (Set.singleton 7) dirtyPages

@scopeKeys@ returns @Nothing@ for @fullScope@.
-}
module Moonlight.Delta.Scope
  ( ScopeCarrier (..),
    Scope,
    Scoped (..),
    cleanScope,
    fullScope,
    dirtyScope,
    foldScope,
    cleanDelta,
    fullDelta,
    payloadDelta,
    scopedDelta,
    normalizeScope,
    normalizeScoped,
    scopeNull,
    scopedDeltaNull,
    scopedDeltaHasPayload,
    scopedDeltaSupport,
    scopedDeltaPayload,
    scopeKeys,
    unionScope,
    restrictScope,
    mapScope,
    mapScopedScope,
  )
where

import Data.Kind (Type)
import Data.Maybe (Maybe (..), isJust)
import Moonlight.Delta.Normalize
  ( DeltaNormalize (..),
  )
import Moonlight.Delta.Support
  ( DeltaSupport (..),
  )
import Moonlight.Core
  ( OrdSet (..),
  )
import Prelude
  ( Bool (..),
    Eq (..),
    Monoid (..),
    Ord (..),
    Semigroup (..),
    Show,
    id,
    not,
    otherwise,
    (&&),
    (.),
  )

type Scope :: Type -> Type
data Scope scope
  = CleanScope
  | DirtyScope !scope
  | FullScope
  deriving stock (Eq, Ord, Show)

type Scoped :: Type -> Type -> Type
data Scoped scope payload = Scoped
  { scope :: !(Scope scope),
    payload :: !(Maybe payload)
  }
  deriving stock (Eq, Ord, Show)

class ScopeCarrier scope where
  scopeCarrierNull :: scope -> Bool

instance {-# OVERLAPPABLE #-} OrdSet scope => ScopeCarrier scope where
  scopeCarrierNull =
    nullSet

instance (ScopeCarrier scope, Semigroup scope) => Semigroup (Scope scope) where
  (<>) =
    unionScope
  {-# INLINE (<>) #-}

instance (ScopeCarrier scope, Semigroup scope) => Monoid (Scope scope) where
  mempty =
    CleanScope
  {-# INLINE mempty #-}

instance (ScopeCarrier scope, Semigroup scope, Semigroup payload) => Semigroup (Scoped scope payload) where
  leftDelta <> rightDelta =
    normalizeScoped
      Scoped
        { scope =
            unionScope
              (scope leftDelta)
              (scope rightDelta),
          payload =
            payload leftDelta <> payload rightDelta
        }
  {-# INLINE (<>) #-}

instance (ScopeCarrier scope, Semigroup scope, Semigroup payload) => Monoid (Scoped scope payload) where
  mempty =
    cleanDelta
  {-# INLINE mempty #-}

instance DeltaNormalize (Scope scope) where
  normalizeDelta =
    normalizeScope
  {-# INLINE normalizeDelta #-}

  deltaNull =
    scopeNull
  {-# INLINE deltaNull #-}

instance DeltaSupport (Scope scope) where
  type DeltaSupportSet (Scope scope) = Scope scope

  emptySupport =
    CleanScope
  {-# INLINE emptySupport #-}

  deltaSupport =
    normalizeScope
  {-# INLINE deltaSupport #-}

instance DeltaNormalize (Scoped scope payload) where
  normalizeDelta =
    normalizeScoped
  {-# INLINE normalizeDelta #-}

  deltaNull =
    scopedDeltaNull
  {-# INLINE deltaNull #-}

instance DeltaSupport (Scoped scope payload) where
  type DeltaSupportSet (Scoped scope payload) = Scope scope

  emptySupport =
    CleanScope
  {-# INLINE emptySupport #-}

  deltaSupport =
    scopedDeltaSupport
  {-# INLINE deltaSupport #-}

cleanScope :: Scope scope
cleanScope =
  CleanScope
{-# INLINE cleanScope #-}

fullScope :: Scope scope
fullScope =
  FullScope
{-# INLINE fullScope #-}

dirtyScope ::
  ScopeCarrier scope =>
  scope ->
  Scope scope
dirtyScope keys
  | scopeCarrierNull keys =
      CleanScope
  | otherwise =
      DirtyScope keys
{-# INLINE dirtyScope #-}

cleanDelta :: Scoped scope payload
cleanDelta =
  Scoped
    { scope = cleanScope,
      payload = Nothing
    }
{-# INLINE cleanDelta #-}

fullDelta :: Scoped scope payload
fullDelta =
  Scoped
    { scope = fullScope,
      payload = Nothing
    }
{-# INLINE fullDelta #-}

payloadDelta :: payload -> Scoped scope payload
payloadDelta payload =
  Scoped
    { scope = cleanScope,
      payload = Just payload
    }
{-# INLINE payloadDelta #-}

scopedDelta ::
  Scope scope ->
  Maybe payload ->
  Scoped scope payload
scopedDelta scopeValue payloadValue =
  normalizeScoped
    Scoped
      { scope = scopeValue,
        payload = payloadValue
      }
{-# INLINE scopedDelta #-}

normalizeScope ::
  Scope scope ->
  Scope scope
normalizeScope =
  id
{-# INLINE normalizeScope #-}

foldScope ::
  result ->
  (scope -> result) ->
  result ->
  Scope scope ->
  result
foldScope cleanCase dirtyCase fullCase scopeValue =
  case scopeValue of
    CleanScope ->
      cleanCase
    DirtyScope keys ->
      dirtyCase keys
    FullScope ->
      fullCase
{-# INLINE foldScope #-}

normalizeScoped ::
  Scoped scope payload ->
  Scoped scope payload
normalizeScoped =
  id
{-# INLINE normalizeScoped #-}

scopeNull ::
  Scope scope ->
  Bool
scopeNull scopeValue =
  case scopeValue of
    CleanScope ->
      True
    DirtyScope _ ->
      False
    FullScope ->
      False
{-# INLINE scopeNull #-}

scopedDeltaHasPayload :: Scoped scope payload -> Bool
scopedDeltaHasPayload =
  isJust . payload
{-# INLINE scopedDeltaHasPayload #-}

scopedDeltaNull ::
  Scoped scope payload ->
  Bool
scopedDeltaNull delta =
  scopeNull (scope delta)
    && not (scopedDeltaHasPayload delta)
{-# INLINE scopedDeltaNull #-}

scopedDeltaSupport ::
  Scoped scope payload ->
  Scope scope
scopedDeltaSupport =
  scope
{-# INLINE scopedDeltaSupport #-}

scopedDeltaPayload :: Scoped scope payload -> Maybe payload
scopedDeltaPayload =
  payload
{-# INLINE scopedDeltaPayload #-}

scopeKeys ::
  OrdSet scope =>
  Scope scope ->
  Maybe scope
scopeKeys scopeValue =
  case scopeValue of
    CleanScope ->
      Just emptySet
    DirtyScope keys ->
      Just keys
    FullScope ->
      Nothing
{-# INLINE scopeKeys #-}

unionScope ::
  (ScopeCarrier scope, Semigroup scope) =>
  Scope scope ->
  Scope scope ->
  Scope scope
unionScope leftScope rightScope =
  case (leftScope, rightScope) of
    (FullScope, _) ->
      FullScope
    (_, FullScope) ->
      FullScope
    (CleanScope, scopeValue) ->
      scopeValue
    (scopeValue, CleanScope) ->
      scopeValue
    (DirtyScope leftKeys, DirtyScope rightKeys) ->
      dirtyScope (leftKeys <> rightKeys)
{-# INLINE unionScope #-}

restrictScope ::
  OrdSet scope =>
  scope ->
  Scope scope ->
  Scope scope
restrictScope restriction scopeValue =
  if nullSet restriction
    then cleanScope
    else
      case scopeValue of
        CleanScope ->
          cleanScope
        DirtyScope keys ->
          dirtyScope (intersectionSet restriction keys)
        FullScope ->
          dirtyScope restriction
{-# INLINE restrictScope #-}

mapScope ::
  ScopeCarrier targetScope =>
  (sourceScope -> targetScope) ->
  Scope sourceScope ->
  Scope targetScope
mapScope project scopeValue =
  case scopeValue of
    CleanScope ->
      cleanScope
    DirtyScope keys ->
      dirtyScope (project keys)
    FullScope ->
      fullScope
{-# INLINE mapScope #-}

mapScopedScope ::
  ScopeCarrier targetScope =>
  (sourceScope -> targetScope) ->
  Scoped sourceScope payload ->
  Scoped targetScope payload
mapScopedScope project delta =
  Scoped
    { scope = mapScope project (scope delta),
      payload = payload delta
    }
{-# INLINE mapScopedScope #-}

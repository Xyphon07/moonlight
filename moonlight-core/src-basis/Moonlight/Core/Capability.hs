{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A constraint captured as data: 'Capability' packs @requirement phase@ evidence with a payload; 'withCapability' releases it.
module Moonlight.Core.Capability
  ( Capability,
    mkCapability,
    withCapability,
    mapCapability,
  )
where

import Data.Kind (Constraint, Type)

type Capability :: forall kindValue. (kindValue -> Constraint) -> kindValue -> Type -> Type
data Capability (requirement :: kindValue -> Constraint) (phase :: kindValue) payload where
  Capability :: requirement phase => payload -> Capability requirement phase payload

mkCapability :: requirement phase => payload -> Capability requirement phase payload
mkCapability = Capability

withCapability :: Capability requirement phase payload -> (requirement phase => payload -> result) -> result
withCapability (Capability payload) use = use payload

mapCapability ::
  (payload -> nextPayload) ->
  Capability requirement phase payload ->
  Capability requirement phase nextPayload
mapCapability transform (Capability payload) =
  Capability (transform payload)

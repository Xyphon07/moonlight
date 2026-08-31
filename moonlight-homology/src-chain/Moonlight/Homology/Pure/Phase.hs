{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Homology.Pure.Phase
  ( HomologyPhase (..),
    RequirePhase2,
    RequirePhase4,
    requirePhase2Witness,
    requirePhase4Witness,
  )
where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

type HomologyPhase :: Type
data HomologyPhase
  = Phase1
  | Phase2
  | Phase4

type RequirePhase2 :: HomologyPhase -> Constraint
class RequirePhase2 (phase :: HomologyPhase) where
  requirePhase2Witness :: ()

instance RequirePhase2 'Phase2 where
  requirePhase2Witness = ()

instance RequirePhase2 'Phase4 where
  requirePhase2Witness = ()

instance
  TypeError
    ( 'Text "Moonlight.Homology phase gate violation."
        ':$$: 'Text "Phase 1 can expose interfaces and boundary materialization only."
        ':$$: 'Text "Betti reduction and Smith Normal Form require Phase 2 or later."
    ) =>
  RequirePhase2 'Phase1
  where
  requirePhase2Witness = ()

type RequirePhase4 :: HomologyPhase -> Constraint
class RequirePhase4 (phase :: HomologyPhase) where
  requirePhase4Witness :: ()

instance RequirePhase4 'Phase4 where
  requirePhase4Witness = ()

instance
  TypeError
    ( 'Text "Moonlight.Homology phase gate violation."
        ':$$: 'Text "Spectral sequence page advancement is Phase 4 infrastructure."
        ':$$: 'Text "Requested phase: Phase1"
    ) =>
  RequirePhase4 'Phase1
  where
  requirePhase4Witness = ()

instance
  TypeError
    ( 'Text "Moonlight.Homology phase gate violation."
        ':$$: 'Text "Spectral sequence page advancement is Phase 4 infrastructure."
        ':$$: 'Text "Requested phase: Phase2"
    ) =>
  RequirePhase4 'Phase2
  where
  requirePhase4Witness = ()

{-# LANGUAGE RankNTypes #-}

-- | The tree-automata carriers: deterministic ('DBTA'), nondeterministic ('NBTA'), and top-down, with 'Acceptance' as the observable.
module Moonlight.Automata.Pure.Core
  ( DBTA (..),
    NBTA (..),
    TopDownTA (..),
    Acceptance (..),
    mapAcceptance,
    zipAcceptanceWith,
    AcceptingDBTA (..),
    AcceptingNBTA (..),
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Prelude

type DBTA :: (Type -> Type) -> Type -> Type
newtype DBTA f state = DBTA
  { runDBTA :: f state -> state
  }

type NBTA :: (Type -> Type) -> Type -> Type
newtype NBTA f state = NBTA
  { runNBTA :: f (Set state) -> Set state
  }

type TopDownTA :: (Type -> Type) -> Type -> Type
newtype TopDownTA f state = TopDownTA
  { runTopDownTA :: forall child. state -> f child -> f (state, child)
  }

type Acceptance :: Type -> Type
newtype Acceptance state = Acceptance
  { accepts :: state -> Bool
  }

mapAcceptance :: (Bool -> Bool) -> Acceptance state -> Acceptance state
mapAcceptance morphism acceptance =
  Acceptance (morphism . accepts acceptance)

zipAcceptanceWith :: (Bool -> Bool -> Bool) -> Acceptance leftState -> Acceptance rightState -> Acceptance (leftState, rightState)
zipAcceptanceWith combine leftAcceptance rightAcceptance =
  Acceptance
    ( \(leftState, rightState) ->
        combine
    (accepts leftAcceptance leftState)
          (accepts rightAcceptance rightState)
    )

type AcceptingDBTA :: (Type -> Type) -> Type -> Type
data AcceptingDBTA f state = AcceptingDBTA
  { adbtaAlgebra :: DBTA f state,
    adbtaAcceptance :: Acceptance state
  }

type AcceptingNBTA :: (Type -> Type) -> Type -> Type
data AcceptingNBTA f state = AcceptingNBTA
  { anbtaAlgebra :: NBTA f state,
    anbtaAcceptance :: Acceptance state
  }

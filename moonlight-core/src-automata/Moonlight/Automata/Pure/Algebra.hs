{-# LANGUAGE ScopedTypeVariables #-}

-- | Bottom-up evaluation and the closure algebra (product, intersection, union, complement) over the automata carriers.
module Moonlight.Automata.Pure.Algebra
  ( evalDBTA,
    annotateBottomUp,
    evalNBTA,
    acceptsDBTA,
    acceptsNBTA,
    dependentDBTA,
    productDBTA,
    intersectionDBTA,
    unionDBTA,
    complementAcceptance,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Foldable (Base, Recursive, cata)
import Data.Set qualified as Set
import Moonlight.Automata.Pure.Core
  ( Acceptance,
    AcceptingDBTA (..),
    AcceptingNBTA (..),
    DBTA (..),
    NBTA (..),
    accepts,
    mapAcceptance,
    zipAcceptanceWith,
  )
import Prelude

evalDBTA :: (Recursive t, Base t ~ f) => DBTA f state -> t -> state
evalDBTA (DBTA algebra) = cata algebra

annotateBottomUp :: forall input f state. (Recursive input, Base input ~ f, Functor f) => DBTA f state -> input -> Cofree f state
annotateBottomUp (DBTA algebra) =
  cata annotateLayer
  where
    annotateLayer :: f (Cofree f state) -> Cofree f state
    annotateLayer layer =
      algebra (fmap rootState layer) :< layer

    rootState :: Cofree f state -> state
    rootState (state :< _) = state

evalNBTA :: (Recursive t, Base t ~ f) => NBTA f state -> t -> Set.Set state
evalNBTA (NBTA algebra) = cata algebra

acceptsDBTA :: (Recursive t, Base t ~ f) => AcceptingDBTA f state -> t -> Bool
acceptsDBTA automaton value =
  accepts (adbtaAcceptance automaton) (evalDBTA (adbtaAlgebra automaton) value)

acceptsNBTA :: (Recursive t, Base t ~ f) => AcceptingNBTA f state -> t -> Bool
acceptsNBTA automaton value =
  any
    (accepts (anbtaAcceptance automaton))
    (evalNBTA (anbtaAlgebra automaton) value)

dependentDBTA :: Functor f => DBTA f leftState -> (f (leftState, rightState) -> rightState) -> DBTA f (leftState, rightState)
dependentDBTA (DBTA leftAlgebra) rightAlgebra =
  DBTA
    ( \layer ->
        let leftState = leftAlgebra (fmap fst layer)
            rightState = rightAlgebra layer
         in (leftState, rightState)
    )

productDBTA :: Functor f => DBTA f leftState -> DBTA f rightState -> DBTA f (leftState, rightState)
productDBTA leftAutomaton rightAutomaton =
  dependentDBTA leftAutomaton (runDBTA rightAutomaton . fmap snd)

intersectionDBTA :: Functor f => AcceptingDBTA f leftState -> AcceptingDBTA f rightState -> AcceptingDBTA f (leftState, rightState)
intersectionDBTA =
  combineAcceptingDBTAWith (&&)

unionDBTA :: Functor f => AcceptingDBTA f leftState -> AcceptingDBTA f rightState -> AcceptingDBTA f (leftState, rightState)
unionDBTA =
  combineAcceptingDBTAWith (||)

complementAcceptance :: Acceptance state -> Acceptance state
complementAcceptance =
  mapAcceptance not

combineAcceptingDBTAWith :: Functor f => (Bool -> Bool -> Bool) -> AcceptingDBTA f leftState -> AcceptingDBTA f rightState -> AcceptingDBTA f (leftState, rightState)
combineAcceptingDBTAWith combine leftAutomaton rightAutomaton =
  AcceptingDBTA
    { adbtaAlgebra = productDBTA (adbtaAlgebra leftAutomaton) (adbtaAlgebra rightAutomaton),
      adbtaAcceptance = zipAcceptanceWith combine (adbtaAcceptance leftAutomaton) (adbtaAcceptance rightAutomaton)
    }

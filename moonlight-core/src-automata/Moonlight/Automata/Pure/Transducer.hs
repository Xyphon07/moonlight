{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tree transducers over contexts: bottom-up, top-down, macro, and lookahead-macro variants, with composition where it is lawful.
module Moonlight.Automata.Pure.Transducer
  ( TreeContext,
    contextHole,
    contextLayer,
    foldTreeContext,
    substituteTreeContext,
    lowerTreeContext,
    BottomUpTransducer (..),
    runBottomUpTransducer,
    composeBottomUp,
    TopDownTransducer (..),
    runTopDownTransducer,
    composeTopDown,
    MacroTreeTransducer (..),
    runMacroTreeTransducer,
    LookaheadMacroTreeTransducer (..),
    runLookaheadMacroTreeTransducer,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Control.Monad.Free (Free (..), iter)
import Data.Bifunctor (second)
import Data.Functor.Foldable (Base, Corecursive (embed), Recursive (project), cata)
import Data.Kind (Type)
import Moonlight.Automata.Pure.Algebra (annotateBottomUp)
import Moonlight.Automata.Pure.Core (DBTA)
import Prelude

type TreeContext :: (Type -> Type) -> Type -> Type
newtype TreeContext g hole = TreeContext
  { unTreeContext :: Free g hole
  }
  deriving newtype (Functor, Applicative, Monad)

contextHole :: hole -> TreeContext g hole
contextHole =
  TreeContext . Pure

contextLayer :: Functor g => g (TreeContext g hole) -> TreeContext g hole
contextLayer =
  TreeContext . Free . fmap unTreeContext

foldTreeContext :: forall g hole result. Functor g => (hole -> result) -> (g result -> result) -> TreeContext g hole -> result
foldTreeContext holeAlgebra layerAlgebra (TreeContext context) =
  iter layerAlgebra (fmap holeAlgebra context)

substituteTreeContext :: Functor g => (hole -> TreeContext g hole') -> TreeContext g hole -> TreeContext g hole'
substituteTreeContext =
  (=<<)

lowerTreeContext :: (Functor g, Corecursive output, Base output ~ g) => TreeContext g output -> output
lowerTreeContext =
  foldTreeContext id embed

joinTreeContext :: Functor g => TreeContext g (TreeContext g hole) -> TreeContext g hole
joinTreeContext =
  substituteTreeContext id

collapseNestedContext :: Functor g => (TreeContext g hole -> hole) -> TreeContext g (TreeContext g hole) -> hole
collapseNestedContext collapse =
  collapse . joinTreeContext

type BottomUpTransducer ::
  (Type -> Type) -> Type -> (Type -> Type) -> Type
newtype BottomUpTransducer f state g = BottomUpTransducer
  { runBottomUpStep :: forall hole. f (state, hole) -> (state, TreeContext g hole)
  }

runBottomUpTransducer :: (Recursive input, Base input ~ f, Corecursive output, Base output ~ g, Functor g) => BottomUpTransducer f state g -> input -> output
runBottomUpTransducer transducer =
  snd . cata (second lowerTreeContext . runBottomUpStep transducer)

composeBottomUp :: forall f g h innerState outerState. (Functor f, Functor g, Functor h) => BottomUpTransducer g innerState h -> BottomUpTransducer f outerState g -> BottomUpTransducer f (outerState, innerState) h
composeBottomUp innerTransducer outerTransducer =
  BottomUpTransducer composedStep
  where
    composedStep :: forall hole. f ((outerState, innerState), hole) -> ((outerState, innerState), TreeContext h hole)
    composedStep layer =
      let outerLayer =
            fmap
              (\((parentOuterState, parentInnerState), hole) -> (parentOuterState, (parentInnerState, hole)))
              layer
          (outerState, intermediateTerm) = runBottomUpStep outerTransducer outerLayer
          (innerState, outputTerm) = lowerIntermediate intermediateTerm
       in ((outerState, innerState), outputTerm)

    lowerIntermediate :: forall hole. TreeContext g (innerState, hole) -> (innerState, TreeContext h hole)
    lowerIntermediate =
      foldTreeContext
        (second contextHole)
        (second joinTreeContext . runBottomUpStep innerTransducer)

type TopDownTransducer ::
  (Type -> Type) -> Type -> (Type -> Type) -> Type
newtype TopDownTransducer f state g = TopDownTransducer
  { runTopDownStep :: forall child. state -> f child -> TreeContext g (state, child)
  }

runTopDownTransducer :: forall input f output g state. (Recursive input, Base input ~ f, Corecursive output, Base output ~ g, Functor g) => TopDownTransducer f state g -> state -> input -> output
runTopDownTransducer transducer =
  go
  where
    go :: state -> input -> output
    go state value =
      lowerTreeContext
        ( runTopDownStep transducer state (project value)
            >>= contextHole . uncurry go
        )

composeTopDown :: forall f g h innerState outerState. (Functor g, Functor h) => TopDownTransducer g innerState h -> TopDownTransducer f outerState g -> TopDownTransducer f (outerState, innerState) h
composeTopDown innerTransducer outerTransducer =
  TopDownTransducer composedStep
  where
    composedStep :: forall child. (outerState, innerState) -> f child -> TreeContext h ((outerState, innerState), child)
    composedStep (outerState, innerState) layer =
      lowerOuterContext (runTopDownStep outerTransducer outerState layer) innerState

    lowerOuterContext :: forall child. TreeContext g (outerState, child) -> innerState -> TreeContext h ((outerState, innerState), child)
    lowerOuterContext =
      foldTreeContext descendIntoHole descendIntoLayer

    descendIntoHole :: forall child. (outerState, child) -> innerState -> TreeContext h ((outerState, innerState), child)
    descendIntoHole (outerState, child) innerState =
      contextHole ((outerState, innerState), child)

    descendIntoLayer :: forall child. g (innerState -> TreeContext h ((outerState, innerState), child)) -> innerState -> TreeContext h ((outerState, innerState), child)
    descendIntoLayer childContinuations innerState =
      runTopDownStep innerTransducer innerState childContinuations
        >>= uncurry (flip ($))

type MacroTreeTransducer ::
  (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type
newtype MacroTreeTransducer f macroState g = MacroTreeTransducer
  { runMacroStep ::
      forall hole.
      macroState hole ->
      f (macroState (TreeContext g hole) -> hole) ->
      TreeContext g hole
  }

runMacroTreeTransducer :: (Recursive input, Base input ~ f, Corecursive output, Base output ~ g, Functor f, Functor g) => MacroTreeTransducer f macroState g -> macroState output -> input -> output
runMacroTreeTransducer transducer initialState input =
  lowerTreeContext (runMacroContext lowerTreeContext transducer initialState input)

runMacroContext ::
  forall input f g hole macroState.
  (Recursive input, Base input ~ f, Functor f, Functor g) =>
  (TreeContext g hole -> hole) ->
  MacroTreeTransducer f macroState g ->
  macroState hole ->
  input ->
  TreeContext g hole
runMacroContext collapse transducer state value =
  runMacroStep
    transducer
    state
    (fmap childContinuation (project value))
  where
    childContinuation :: input -> macroState (TreeContext g hole) -> hole
    childContinuation child childState =
      collapseNestedContext collapse (runMacroContext joinTreeContext transducer childState child)

type LookaheadMacroTreeTransducer ::
  (Type -> Type) -> Type -> (Type -> Type) -> (Type -> Type) -> Type
newtype LookaheadMacroTreeTransducer f lookahead macroState g = LookaheadMacroTreeTransducer
  { runLookaheadMacroStep ::
      forall hole.
      macroState hole ->
      lookahead ->
      f (macroState (TreeContext g hole) -> hole, lookahead) ->
      TreeContext g hole
  }

runLookaheadMacroTreeTransducer :: (Recursive input, Base input ~ f, Corecursive output, Base output ~ g, Functor f, Functor g) => DBTA f lookahead -> LookaheadMacroTreeTransducer f lookahead macroState g -> macroState output -> input -> output
runLookaheadMacroTreeTransducer lookaheadAutomaton transducer initialState input =
  lowerTreeContext (runLookaheadMacroContext lowerTreeContext transducer initialState annotatedInput)
  where
    annotatedInput = annotateBottomUp lookaheadAutomaton input

runLookaheadMacroContext ::
  forall f g hole lookahead macroState.
  (Functor f, Functor g) =>
  (TreeContext g hole -> hole) ->
  LookaheadMacroTreeTransducer f lookahead macroState g ->
  macroState hole ->
  Cofree f lookahead ->
  TreeContext g hole
runLookaheadMacroContext collapse transducer state (lookahead :< layer) =
  runLookaheadMacroStep
    transducer
    state
    lookahead
    (fmap childContinuation layer)
  where
    childContinuation :: Cofree f lookahead -> (macroState (TreeContext g hole) -> hole, lookahead)
    childContinuation child@(childLookahead :< _) =
      ( \childState -> collapseNestedContext collapse (runLookaheadMacroContext joinTreeContext transducer childState child),
        childLookahead
      )

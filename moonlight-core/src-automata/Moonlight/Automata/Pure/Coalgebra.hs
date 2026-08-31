-- | Top-down duals: folds and annotations flowing from the root, including inherited attributes.
module Moonlight.Automata.Pure.Coalgebra
  ( topDownFold,
    annotateTopDown,
    annotateTopDownWithAttribute,
    projectStateAnnotation,
    projectAttributeAnnotation,
    rootAttribute,
    inheritedAttribute,
  )
where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Foldable (Base, Recursive (project))
import Moonlight.Automata.Pure.Core (TopDownTA (..))
import Prelude

topDownFold :: (Functor f, Recursive t, Base t ~ f) => TopDownTA f state -> (state -> f result -> result) -> state -> t -> result
topDownFold automaton algebra = go
  where
    go state value =
      let distributedChildren = runTopDownTA automaton state (project value)
       in algebra state (fmap (uncurry go) distributedChildren)

annotateTopDown :: (Functor f, Recursive t, Base t ~ f) => TopDownTA f state -> state -> t -> Cofree f state
annotateTopDown automaton =
  topDownFold automaton (\state annotatedChildren -> state :< annotatedChildren)

annotateTopDownWithAttribute :: (Functor f, Recursive t, Base t ~ f) => TopDownTA f state -> (state -> f attribute -> attribute) -> state -> t -> Cofree f (state, attribute)
annotateTopDownWithAttribute automaton algebra =
  topDownFold
    automaton
    ( \state annotatedChildren ->
        let attribute = algebra state (fmap rootAttribute annotatedChildren)
         in (state, attribute) :< annotatedChildren
    )

projectStateAnnotation :: Functor f => Cofree f (state, attribute) -> Cofree f state
projectStateAnnotation = fmap fst

projectAttributeAnnotation :: Functor f => Cofree f (state, attribute) -> Cofree f attribute
projectAttributeAnnotation = fmap snd

rootAttribute :: Cofree f (state, attribute) -> attribute
rootAttribute ((_, attribute) :< _) = attribute

inheritedAttribute :: (Functor f, Recursive t, Base t ~ f) => TopDownTA f state -> (state -> f attribute -> attribute) -> state -> t -> attribute
inheritedAttribute = topDownFold

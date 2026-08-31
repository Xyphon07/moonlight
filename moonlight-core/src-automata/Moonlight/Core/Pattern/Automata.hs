{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bottom-up matcher for compiled pattern kernels.
-- It owns deterministic tree-automaton evaluation, conjunction by kernel
-- intersection, and binding compatibility checks against existing bindings.
module Moonlight.Core.Pattern.Automata
  ( PatternAutomaton,
    compilePatternAutomaton,
    compileConjunctivePatternAutomaton,
    intersectPatternAutomaton,
    matchesPatternAutomaton,
    matchPatternAutomaton,
  )
where

import Data.Foldable (foldlM, toList)
import Data.Fix (Fix (..))
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.These (These (..))
import Moonlight.Core.Fix.Order (OrderedFix (..))
import Moonlight.Core.Identifier.EGraph (PatternVar, patternVarKey)
import Moonlight.Core.Language (Language, ZipMatch (..))
import Moonlight.Core.Pattern (Pattern)
import Moonlight.Core.Pattern.Kernel
  ( CompiledPatternKernel (..),
    PatternKernelChildren (..),
    PatternKernelStateSpec (..),
    compilePatternKernel,
    intersectCompiledPatternKernel,
  )
import Moonlight.Automata.Pure.Algebra (evalDBTA)
import Moonlight.Automata.Pure.Core (DBTA (..))
import Prelude

type PatternState :: (Type -> Type) -> Type -> Type
data PatternState f state = PatternState
  { patternStateTerm :: Fix f,
    patternStateBindings :: Map state (IntMap (Fix f))
  }

type PatternAutomaton :: (Type -> Type) -> Type
data PatternAutomaton f where
  PatternAutomaton :: Ord state => CompiledPatternKernel f state -> DBTA f (PatternState f state) -> PatternAutomaton f

compilePatternAutomaton :: (Language f, ZipMatch f) => Pattern f -> PatternAutomaton f
compilePatternAutomaton =
  patternAutomatonFromKernel . compilePatternKernel

compileConjunctivePatternAutomaton :: (Language f, ZipMatch f) => NonEmpty (Pattern f) -> PatternAutomaton f
compileConjunctivePatternAutomaton (patternValue :| patternValues) =
  foldl'
    intersectPatternAutomaton
    (compilePatternAutomaton patternValue)
    (fmap compilePatternAutomaton patternValues)

intersectPatternAutomaton :: (Language f, ZipMatch f) => PatternAutomaton f -> PatternAutomaton f -> PatternAutomaton f
intersectPatternAutomaton (PatternAutomaton leftKernel _) (PatternAutomaton rightKernel _) =
  patternAutomatonFromKernel (intersectCompiledPatternKernel leftKernel rightKernel)

matchesPatternAutomaton :: Language f => PatternAutomaton f -> Fix f -> Bool
matchesPatternAutomaton automaton term =
  isJust (matchPatternAutomaton automaton term IntMap.empty)

matchPatternAutomaton :: Language f => PatternAutomaton f -> Fix f -> IntMap (Fix f) -> Maybe (IntMap (Fix f))
matchPatternAutomaton (PatternAutomaton kernel automaton) term initialBindings =
  mergeBindings initialBindings
    =<< rootBindingsFor kernel (evalDBTA automaton term)

patternAutomatonFromKernel :: (Language f, ZipMatch f, Ord state) => CompiledPatternKernel f state -> PatternAutomaton f
patternAutomatonFromKernel kernel =
  PatternAutomaton kernel (DBTA (stepPatternKernelAutomaton kernel))

stepPatternKernelAutomaton :: (Language f, ZipMatch f, Ord state) => CompiledPatternKernel f state -> f (PatternState f state) -> PatternState f state
stepPatternKernelAutomaton kernel childStates =
  let currentTerm = Fix (fmap patternStateTerm childStates)
      stateBindings =
        Map.fromList
          (mapMaybe
             (\state ->
                fmap
                  ((,) state)
                  (matchKernelState currentTerm childStates (cpkStateSpec kernel state))
             )
             (cpkOrderedStates kernel))
   in PatternState currentTerm stateBindings

matchKernelState :: (Language f, ZipMatch f, Ord state) => Fix f -> f (PatternState f state) -> PatternKernelStateSpec f state -> Maybe (IntMap (Fix f))
matchKernelState currentTerm childStates stateSpec =
  case runPatternKernelStateSpec stateSpec of
    (_, KernelMatchImpossible) ->
      Nothing
    (patternVars, KernelMatchAny) ->
      bindCurrentTerm patternVars currentTerm IntMap.empty
    (patternVars, KernelMatchNode patternNode) ->
      zipMatchedChildren patternNode childStates
        >>= foldlM mergeChildBindings IntMap.empty . toList
        >>= bindCurrentTerm patternVars currentTerm

zipMatchedChildren ::
  ZipMatch f =>
  f state ->
  f child ->
  Maybe (f (state, child))
zipMatchedChildren patternNode childValues =
  traverse matchedChild
    =<< zipMatch (fmap This patternNode) (fmap That childValues)
  where
    matchedChild ::
      (These state child, These state child) ->
      Maybe (state, child)
    matchedChild matchedValue =
      case matchedValue of
        (This stateValue, That childValue) ->
          Just (stateValue, childValue)
        _ ->
          Nothing

mergeChildBindings :: (Language f, Ord state) => IntMap (Fix f) -> (state, PatternState f state) -> Maybe (IntMap (Fix f))
mergeChildBindings bindings (state, childState) =
  Map.lookup state (patternStateBindings childState)
    >>= mergeBindings bindings

bindCurrentTerm :: Language f => [PatternVar] -> Fix f -> IntMap (Fix f) -> Maybe (IntMap (Fix f))
bindCurrentTerm patternVars boundTerm bindings =
  foldlM
    (\currentBindings patternVar -> bindPatternVar patternVar boundTerm currentBindings)
    bindings
    patternVars

bindPatternVar :: Language f => PatternVar -> Fix f -> IntMap (Fix f) -> Maybe (IntMap (Fix f))
bindPatternVar patternVar boundTerm bindings =
  case IntMap.lookup (patternVarKey patternVar) bindings of
    Nothing ->
      Just (IntMap.insert (patternVarKey patternVar) boundTerm bindings)
    Just existingTerm
      | OrderedFix existingTerm == OrderedFix boundTerm -> Just bindings
      | otherwise -> Nothing

mergeBindings :: forall f. Language f => IntMap (Fix f) -> IntMap (Fix f) -> Maybe (IntMap (Fix f))
mergeBindings initialBindings =
  foldlM mergeEntry initialBindings . IntMap.toAscList
  where
    mergeEntry :: IntMap (Fix f) -> (IntMap.Key, Fix f) -> Maybe (IntMap (Fix f))
    mergeEntry bindings (bindingKey, boundTerm) =
      case IntMap.lookup bindingKey bindings of
        Nothing -> Just (IntMap.insert bindingKey boundTerm bindings)
        Just existingTerm
          | OrderedFix existingTerm == OrderedFix boundTerm -> Just bindings
          | otherwise -> Nothing

rootBindingsFor :: Ord state => CompiledPatternKernel f state -> PatternState f state -> Maybe (IntMap (Fix f))
rootBindingsFor kernel stateValue =
  Map.lookup (cpkRootState kernel) (patternStateBindings stateValue)

module Moonlight.Core.Term.Database.Pattern where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Traversable (mapAccumL)
import Moonlight.Core.Pattern
  ( Pattern (..),
    patternVariables,
  )
import Moonlight.Core.Term.Database.Types
import Prelude

compilePatternFreeJoinPlan ::
  Traversable f =>
  Pattern f ->
  PatternFreeJoinPlan f key
compilePatternFreeJoinPlan patternValue =
  compilePatternsFreeJoinPlan (patternValue :| [])
{-# INLINE compilePatternFreeJoinPlan #-}

compilePatternsFreeJoinPlan ::
  Traversable f =>
  NonEmpty (Pattern f) ->
  PatternFreeJoinPlan f key
compilePatternsFreeJoinPlan patterns =
  PatternFreeJoinPlan
    { patternFreeJoinPlan =
        FreeJoinPlan (reverse (compileAtoms finalState)),
      patternFreeJoinRoots = rootTerms,
      patternFreeJoinVariables = queryVarsByPatternVar
    }
  where
    patternVars =
      foldMap patternVariables patterns

    queryVarsByPatternVar =
      Map.fromSet AuthoredPatternVar patternVars

    initialState :: PatternCompileState f key
    initialState =
      PatternCompileState
        { nextGeneratedPatternNodeVar = 0,
          compileAtoms = []
        }

    (finalState, rootTerms) =
      mapAccumL compilePatternTerm initialState patterns
{-# INLINE compilePatternsFreeJoinPlan #-}

compilePatternTerm ::
  Traversable f =>
  PatternCompileState f key ->
  Pattern f ->
  (PatternCompileState f key, QueryTerm key)
compilePatternTerm state patternValue =
  case patternValue of
    PatternVar patternVar ->
      (state, QueryVariable (AuthoredPatternVar patternVar))
    PatternNode node ->
      let resultTerm =
            QueryVariable (GeneratedPatternNodeVar (nextGeneratedPatternNodeVar state))
          stateAfterResult =
            state {nextGeneratedPatternNodeVar = nextGeneratedPatternNodeVar state + 1}
          (stateAfterChildren, childTerms) =
            mapAccumL compilePatternTerm stateAfterResult node
          atom =
            QueryAtom
              { atomOperator = extractOperator node,
                atomResult = resultTerm,
                atomChildren = toList childTerms
              }
       in ( stateAfterChildren
              { compileAtoms = atom : compileAtoms stateAfterChildren
              },
            resultTerm
          )
{-# INLINE compilePatternTerm #-}

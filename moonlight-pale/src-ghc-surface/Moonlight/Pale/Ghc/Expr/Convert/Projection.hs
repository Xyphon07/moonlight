{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Projection
  ( bindingExpr,
    projectBinding,
    projectClause,
    projectRhs,
    attachProjectedBindingGroup,
    projectBindingRow,
    mkProjectedExpr,
    attachBindingGroup
  )
where

import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Pale.Ghc.Expr.Convert.Dependencies qualified as Dependencies
import Moonlight.Pale.Ghc.Expr.Convert.FreeScopes (ScopeAlgebra (..))
import Moonlight.Pale.Ghc.Expr.Convert.FreeScopes qualified as FreeScopes
import Moonlight.Pale.Ghc.Expr.Convert.Row
import Moonlight.Pale.Ghc.Expr.Convert.State
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax

bindingExpr ::
  ScopeIndex ->
  ConvertedValueBinding ->
  Either ScopeLookupFailure Expr
bindingExpr scopeIndex topLevelBinding = do
  expressionValue <-
    projectBinding
      scopeIndex
      (tlbScope topLevelBinding)
      (tlbBinding topLevelBinding)
  pure
    expressionValue
      { exprRegion = tlbRegion topLevelBinding
      }

projectBinding ::
  ScopeIndex ->
  ScopeId ->
  Binding ->
  Either ScopeLookupFailure Expr
projectBinding scopeIndex bindingScope = \case
  PatternBinding _ rhsValue ->
    projectRhs scopeIndex bindingScope rhsValue
  FunctionBinding _ clauses ->
    case clauses of
      Clause patterns rhsValue :| []
        | Just binderAnns <- traverse simplePatternBinderAnn patterns -> do
            rhsScope <-
              clauseBodyScope scopeIndex bindingScope patterns
            rhsExpression <-
              projectRhs
                scopeIndex
                rhsScope
                rhsValue
            foldM
              ( \bodyExpression binderAnn -> do
                  lambdaScope <-
                    binderSiteScope scopeIndex (baId binderAnn)
                  mkProjectedExpr
                    scopeIndex
                    lambdaScope
                    (LamF binderAnn bodyExpression)
              )
              rhsExpression
              (reverse binderAnns)
      clauseValues -> do
        projectedClauses <-
          traverse
            (projectClause scopeIndex bindingScope)
            (NonEmpty.toList clauseValues)
        mkProjectedExpr
          scopeIndex
          bindingScope
          (ClausesF projectedClauses)

projectClause ::
  ScopeIndex ->
  ScopeId ->
  Clause ->
  Either ScopeLookupFailure ([HsPatF], Expr)
projectClause scopeIndex bindingScope clauseValue = do
  rhsScope <-
    clauseBodyScope
      scopeIndex
      bindingScope
      (clausePatterns clauseValue)
  rhsExpression <-
    projectRhs
      scopeIndex
      rhsScope
      (clauseRhs clauseValue)
  pure (clausePatterns clauseValue, rhsExpression)

projectRhs ::
  ScopeIndex ->
  ScopeId ->
  Rhs ->
  Either ScopeLookupFailure Expr
projectRhs scopeIndex rhsScope = \case
  UnguardedRhs bodyExpression maybeBindingGroup ->
    attachProjectedBindingGroup
      scopeIndex
      rhsScope
      maybeBindingGroup
      bodyExpression
  GuardedRhs guardedAlternatives maybeBindingGroup -> do
    guardedExpression <-
      mkProjectedExpr
        scopeIndex
        rhsScope
        (GuardedF (NonEmpty.toList guardedAlternatives))
    attachProjectedBindingGroup
      scopeIndex
      rhsScope
      maybeBindingGroup
      guardedExpression

attachProjectedBindingGroup ::
  ScopeIndex ->
  ScopeId ->
  Maybe BindingGroup ->
  Expr ->
  Either ScopeLookupFailure Expr
attachProjectedBindingGroup _ _ Nothing bodyExpression =
  Right bodyExpression
attachProjectedBindingGroup scopeIndex rhsScope (Just bindingGroup) bodyExpression = do
  bindingRows <-
    traverse
      (projectBindingRow scopeIndex (bindingGroupScope bindingGroup))
      (NonEmpty.toList (bindingGroupBindings bindingGroup))
  mkProjectedExpr
    scopeIndex
    rhsScope
    ( LetF
        (Dependencies.bindingComponentsRecursion (bindingGroupComponents bindingGroup))
        bindingRows
        bodyExpression
    )

projectBindingRow ::
  ScopeIndex ->
  ScopeId ->
  Binding ->
  Either ScopeLookupFailure (HsPatF, Expr)
projectBindingRow scopeIndex bindingScope bindingValue =
  (,)
    (bindingHeadPattern bindingValue)
    <$> projectBinding scopeIndex bindingScope bindingValue

mkProjectedExpr ::
  ScopeIndex ->
  ScopeId ->
  HsExprF Expr ->
  Either ScopeLookupFailure Expr
mkProjectedExpr scopeIndex occurrenceScope expressionNode = do
  freeScopes <-
    FreeScopes.freeScopesExpr
      ScopeAlgebra
        { saScopeDepth = scopeDepthOf scopeIndex,
          saBinderIntro = binderIntroScope scopeIndex . baId
        }
      expressionNode
  pure
    Expr
      { exprRegion = Nothing,
        exprScope = occurrenceScope,
        exprFreeScopes = freeScopes,
        exprNode = expressionNode
      }

attachBindingGroup ::
  Maybe ConvertedLocalBinds ->
  Expr ->
  ConvM Expr
attachBindingGroup = \case
  Nothing ->
    pure
  Just convertedBinds ->
    mkConvExpr
      Nothing
      . LetF (clbRecursion convertedBinds) (clbBindings convertedBinds)

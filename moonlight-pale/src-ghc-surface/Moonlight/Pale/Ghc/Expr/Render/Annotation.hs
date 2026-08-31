{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Annotation
  ( bindingRootExpressions,
    rhsRootExpressions,
    bindingGroupRootExpressions,
    bindingRenderAnnotations,
    localBindingRenderAnnotations,
    bindingHeadAnnotations,
    clauseRenderAnnotations,
    rhsRenderAnnotations,
    bindingGroupRenderAnnotations,
    nodeBindingAnnotations,
    statementBindingAnnotations,
    guardedAltBindingAnnotations,
    guardBindingAnnotations
  )
where

import Data.Foldable qualified as Foldable
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
  ( Binding (..),
    BindingGroup,
    bindingGroupBindings,
    Clause (..),
    Rhs (..),
  )
import Moonlight.Pale.Ghc.Expr.Syntax
bindingRootExpressions :: Binding -> [Expr]
bindingRootExpressions = \case
  FunctionBinding _ clauses ->
    foldMap (rhsRootExpressions . clauseRhs) clauses
  PatternBinding _ rhsValue ->
    rhsRootExpressions rhsValue

rhsRootExpressions :: Rhs -> [Expr]
rhsRootExpressions = \case
  UnguardedRhs bodyExpression maybeBindingGroup ->
    bodyExpression : foldMap bindingGroupRootExpressions maybeBindingGroup
  GuardedRhs guardedAlternatives maybeBindingGroup ->
    foldMap Foldable.toList guardedAlternatives
      <> foldMap bindingGroupRootExpressions maybeBindingGroup

bindingGroupRootExpressions :: BindingGroup -> [Expr]
bindingGroupRootExpressions =
  foldMap bindingRootExpressions . bindingGroupBindings

bindingRenderAnnotations :: Binding -> [BinderAnn]
bindingRenderAnnotations = \case
  FunctionBinding _ clauses ->
    foldMap clauseRenderAnnotations clauses
  PatternBinding _ rhsValue ->
    rhsRenderAnnotations rhsValue

localBindingRenderAnnotations :: Binding -> [BinderAnn]
localBindingRenderAnnotations bindingValue =
  bindingHeadAnnotations bindingValue <> bindingRenderAnnotations bindingValue

bindingHeadAnnotations :: Binding -> [BinderAnn]
bindingHeadAnnotations = \case
  FunctionBinding binderAnn _ ->
    [binderAnn]
  PatternBinding patternValue _ ->
    patBinders patternValue

clauseRenderAnnotations :: Clause -> [BinderAnn]
clauseRenderAnnotations clauseValue =
  foldMap patBinders (clausePatterns clauseValue)
    <> rhsRenderAnnotations (clauseRhs clauseValue)

rhsRenderAnnotations :: Rhs -> [BinderAnn]
rhsRenderAnnotations = \case
  UnguardedRhs _ maybeBindingGroup ->
    foldMap bindingGroupRenderAnnotations maybeBindingGroup
  GuardedRhs guardedAlternatives maybeBindingGroup ->
    foldMap guardedAltBindingAnnotations guardedAlternatives
      <> foldMap bindingGroupRenderAnnotations maybeBindingGroup

bindingGroupRenderAnnotations :: BindingGroup -> [BinderAnn]
bindingGroupRenderAnnotations =
  foldMap localBindingRenderAnnotations . bindingGroupBindings

nodeBindingAnnotations :: HsExprF recursive -> [BinderAnn]
nodeBindingAnnotations = \case
  LamF binderAnn _ ->
    [binderAnn]
  LetF _ bindingValues _ ->
    foldMap (patBinders . fst) bindingValues
  CaseF _ alternatives ->
    foldMap (patBinders . fst) alternatives
  DoF statementValues ->
    foldMap statementBindingAnnotations statementValues
  GuardedF guardedAlts ->
    foldMap guardedAltBindingAnnotations guardedAlts
  ClausesF clauseValues ->
    foldMap (foldMap patBinders . fst) clauseValues
  MultiIfF guardedAlts ->
    foldMap guardedAltBindingAnnotations guardedAlts
  _ ->
    []

statementBindingAnnotations :: HsStmtF recursive -> [BinderAnn]
statementBindingAnnotations = \case
  BindStmtF patternValue _ ->
    patBinders patternValue
  LetStmtF _ bindingValues ->
    foldMap (patBinders . fst) bindingValues
  BodyStmtF _ ->
    []

guardedAltBindingAnnotations :: GuardedAltF recursive -> [BinderAnn]
guardedAltBindingAnnotations =
  foldMap guardBindingAnnotations . gaGuards

guardBindingAnnotations :: HsGuardStmtF recursive -> [BinderAnn]
guardBindingAnnotations = \case
  GuardPatF patternValue _ ->
    patBinders patternValue
  GuardLetF _ bindingValues ->
    foldMap (patBinders . fst) bindingValues
  GuardBoolF _ ->
    []

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Binding
  ( renderBindingWith,
    renderBindingWithSource,
    renderTopLevelBindingWith,
    renderBindingLhs,
    renderBindingClause,
    renderBindingRhs,
    appendWhereGroup,
    renderWhereGroup,
    renderInlineWhereGroup,
    renderGuardedTopLevelAlts,
    renderGuardedTopLevelAlt
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
  ( Binding (..),
    BindingGroup,
    bindingGroupBindings,
    Clause (..),
    Rhs (..),
  )
import Moonlight.Pale.Ghc.Expr.Render.Carrier
import Moonlight.Pale.Ghc.Expr.Render.Document
import Moonlight.Pale.Ghc.Expr.Render.Expression
import Moonlight.Pale.Ghc.Expr.Render.Name
import Moonlight.Pale.Ghc.Expr.Render.Pattern
import Moonlight.Pale.Ghc.Expr.Render.Refusal
import Moonlight.Pale.Ghc.Expr.Syntax

renderBindingWith ::
  RenderDocument document =>
  RenderMode ->
  Binding ->
  Either RenderRefusal document
renderBindingWith renderMode bindingValue = do
  renderContext <- bindingRenderSource bindingValue
  renderBindingWithSource renderContext renderMode bindingValue

renderBindingWithSource ::
  RenderDocument document =>
  RenderSource Expr ->
  RenderMode ->
  Binding ->
  Either RenderRefusal document
renderBindingWithSource renderContext renderMode = \case
  FunctionBinding binderAnn clauses ->
    vcat
      <$> traverse
        (renderBindingClause renderContext renderMode (renderDefinitionName (renderBinderSpelling renderContext binderAnn)))
        (NonEmpty.toList clauses)
  PatternBinding patternValue rhsValue -> do
    patternDoc <- renderPat renderContext False patternValue
    renderBindingRhs renderContext renderMode patternDoc rhsValue

renderTopLevelBindingWith ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  String ->
  recursive ->
  Either RenderRefusal document
renderTopLevelBindingWith renderContext renderMode bindingName bindingTerm
  | null bindingName =
      Left RenderEmptyBindingName
  | otherwise = do
      bodyDoc <- renderExprWith renderContext renderMode 0 bindingTerm
      Right
        ( renderDelimitedExpression
            renderMode
            (renderDefinitionName bindingName)
            "="
            bodyDoc
        )

renderBindingLhs ::
  RenderDocument document =>
  RenderSource recursive ->
  document ->
  [HsPatF] ->
  Either RenderRefusal document
renderBindingLhs renderContext headDoc patternValues =
  case patternValues of
    [] ->
      Right headDoc
    _ ->
      renderClauseLhs renderContext headDoc patternValues

renderBindingClause ::
  RenderDocument document =>
  RenderSource Expr ->
  RenderMode ->
  document ->
  Clause ->
  Either RenderRefusal document
renderBindingClause renderContext renderMode bindingHead clauseValue = do
  lhsDoc <-
    renderBindingLhs renderContext bindingHead (clausePatterns clauseValue)
  renderBindingRhs renderContext renderMode lhsDoc (clauseRhs clauseValue)

renderBindingRhs ::
  RenderDocument document =>
  RenderSource Expr ->
  RenderMode ->
  document ->
  Rhs ->
  Either RenderRefusal document
renderBindingRhs renderContext renderMode lhsDoc = \case
  UnguardedRhs bodyExpression maybeWhereGroup -> do
    bodyDoc <-
      renderExprWith
        renderContext
        renderMode
        0
        bodyExpression
    appendWhereGroup
      renderContext
      renderMode
      maybeWhereGroup
      (renderDelimitedExpression renderMode lhsDoc "=" bodyDoc)
  GuardedRhs guardedAlternatives maybeWhereGroup -> do
    equationDoc <-
      renderGuardedTopLevelAlts
        renderContext
        renderMode
        lhsDoc
        (NonEmpty.toList guardedAlternatives)
    appendWhereGroup renderContext renderMode maybeWhereGroup equationDoc

appendWhereGroup ::
  RenderDocument document =>
  RenderSource Expr ->
  RenderMode ->
  Maybe BindingGroup ->
  document ->
  Either RenderRefusal document
appendWhereGroup renderContext renderMode maybeBindingGroup equationDoc =
  case maybeBindingGroup of
    Nothing ->
      Right equationDoc
    Just bindingGroup -> do
      case renderMode of
        CompactRender -> do
          whereSuffix <-
            renderInlineWhereGroup renderContext CompactRender bindingGroup
          Right (equationDoc <> whereSuffix)
        GeneratedRender -> do
          whereDoc <-
            renderWhereGroup renderContext GeneratedRender bindingGroup
          Right (vcat [equationDoc, whereDoc])

renderWhereGroup ::
  RenderDocument document =>
  RenderSource Expr ->
  RenderMode ->
  BindingGroup ->
  Either RenderRefusal document
renderWhereGroup renderContext renderMode bindingGroup = do
  bindingDocs <-
    traverse
      (renderBindingWithSource renderContext renderMode)
      (NonEmpty.toList (bindingGroupBindings bindingGroup))
  Right (vcat [nest 2 (text "where"), nest 4 (vcat bindingDocs)])

renderInlineWhereGroup ::
  RenderDocument document =>
  RenderSource Expr ->
  RenderMode ->
  BindingGroup ->
  Either RenderRefusal document
renderInlineWhereGroup renderContext renderMode bindingGroup = do
  bindingDocs <-
    traverse
      (renderBindingWithSource renderContext renderMode)
      (NonEmpty.toList (bindingGroupBindings bindingGroup))
  Right (renderBlock renderMode " where" bindingDocs)

renderGuardedTopLevelAlts ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  document ->
  [GuardedAltF recursive] ->
  Either RenderRefusal document
renderGuardedTopLevelAlts renderContext renderMode lhsDoc guardedAlts =
  case guardedAlts of
    [] ->
      Left RenderGuardedExpression
    [GuardedAltF [] bodyValue] -> do
      bodyDoc <- renderExprWith renderContext renderMode 0 bodyValue
      Right (lhsDoc <> text " = " <> bodyDoc)
    _ -> do
      altDocs <- traverse (renderGuardedTopLevelAlt renderContext renderMode) guardedAlts
      Right
        ( case renderMode of
            CompactRender ->
              lhsDoc <+> intercalateDoc (text " ") altDocs
            GeneratedRender ->
              vcat (lhsDoc : fmap (nest 2) altDocs)
        )

renderGuardedTopLevelAlt ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  GuardedAltF recursive ->
  Either RenderRefusal document
renderGuardedTopLevelAlt renderContext renderMode =
  renderGuardedAlt renderContext renderMode (text "| ") " = "

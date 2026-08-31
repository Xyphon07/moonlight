{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Module
  ( ModuleRenderContext (..),
    RenderTarget (..),
    renderSourceWith,
    renderRewriteModuleSource,
    renderModuleDeclaration,
    renderConvertedModuleWith
  )
where

import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Moonlight.Core (Pattern (..))
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
  ( Binding (..),
    ConvertedInstanceDeclaration (..),
    ConvertedModule (..),
    ModuleDeclaration (..),
    convertedBindingValue,
    convertedModuleBindingSites,
    tlbBinding,
  )
import Moonlight.Pale.Ghc.Expr.Render.Analysis
import Moonlight.Pale.Ghc.Expr.Render.Binding
import Moonlight.Pale.Ghc.Expr.Render.Document
import Moonlight.Pale.Ghc.Expr.Render.Expression
import Moonlight.Pale.Ghc.Expr.Render.Name
import Moonlight.Pale.Ghc.Expr.Render.Refusal
import Moonlight.Pale.Ghc.Expr.Syntax

type ModuleRenderContext :: Type
data ModuleRenderContext = ModuleRenderContext
  { moduleHeaderPrefix :: !String,
    moduleRenderedName :: !(Maybe String)
  }
  deriving stock (Eq, Ord, Show)

type RenderTarget :: Type
data RenderTarget
  = RenderAnnotatedExpression !Expr
  | RenderRewriteExpression !(Pattern HsExprF)
  | RenderNamedRewriteBinding !String !(Pattern HsExprF)
  | RenderSourceBinding !Binding
  | RenderRewriteModule !ModuleRenderContext ![(String, Pattern HsExprF)]
  | RenderConvertedModule !ModuleRenderContext !ConvertedModule

renderSourceWith ::
  RenderDocument document =>
  RenderMode ->
  RenderTarget ->
  Either RenderRefusal document
renderSourceWith renderMode = \case
  RenderAnnotatedExpression expressionValue -> do
    expressionRenderSource <-
      prepareRenderSource (Right . exprNode) [] [expressionValue]
    renderExprWith
      expressionRenderSource
      renderMode
      0
      expressionValue
  RenderRewriteExpression expressionValue -> do
    expressionRenderSource <- patternRenderSource expressionValue
    renderExprWith
      expressionRenderSource
      renderMode
      0
      expressionValue
  RenderNamedRewriteBinding bindingName bindingTerm -> do
    expressionRenderSource <- patternRenderSource bindingTerm
    renderTopLevelBindingWith
      expressionRenderSource
      renderMode
      bindingName
      bindingTerm
  RenderSourceBinding bindingValue ->
    renderBindingWith renderMode bindingValue
  RenderRewriteModule moduleContext renderedBindings ->
    renderRewriteModuleSource renderMode moduleContext renderedBindings
  RenderConvertedModule moduleContext convertedModule ->
    renderConvertedModuleWith renderMode moduleContext convertedModule

renderRewriteModuleSource ::
  RenderDocument document =>
  RenderMode ->
  ModuleRenderContext ->
  [(String, Pattern HsExprF)] ->
  Either RenderRefusal document
renderRewriteModuleSource renderMode moduleContext renderedBindings = do
  bindingDocuments <-
    traverse
      ( \(bindingName, bindingTerm) -> do
          expressionRenderSource <- patternRenderSource bindingTerm
          renderTopLevelBindingWith
            expressionRenderSource
            renderMode
            bindingName
            bindingTerm
      )
      renderedBindings
  let headerPrefix = moduleHeaderPrefix moduleContext
  let prefixValue =
        if null headerPrefix
          then
            requiredLanguageHeader (fmap snd renderedBindings)
              <> maybe
                ""
                (\moduleNameValue -> "module " <> moduleNameValue <> " where\n\n")
                (moduleRenderedName moduleContext)
          else headerPrefix
  Right
    ( text prefixValue
        <> intercalateDoc (text "\n\n") bindingDocuments
        <> text "\n"
    )

renderModuleDeclaration ::
  RenderDocument document =>
  RenderMode ->
  ModuleDeclaration ->
  Either RenderRefusal document
renderModuleDeclaration renderMode = \case
  ValueDeclaration bindingValue ->
    renderBindingWith renderMode (tlbBinding bindingValue)
  TypeSignatureDeclaration signature ->
    Right (text (renderTypeSignature signature))
  FixityDeclarationNode declaration ->
    Right (text (renderFixityDeclaration declaration))
  InstanceDeclarationNode instanceDeclaration ->
    Right (text (convertedInstanceSource instanceDeclaration))
  OpaqueDeclaration _ _ declarationSource ->
    Right (text declarationSource)

renderConvertedModuleWith ::
  RenderDocument document =>
  RenderMode ->
  ModuleRenderContext ->
  ConvertedModule ->
  Either RenderRefusal document
renderConvertedModuleWith renderMode moduleContext convertedModule = do
  declarationDocuments <-
    traverse
      (renderModuleDeclaration renderMode)
      (Foldable.toList (cmDeclarations convertedModule))
  let bindings =
        fmap convertedBindingValue (convertedModuleBindingSites convertedModule)
      headerPrefix =
        moduleHeaderPrefix moduleContext
  let prefixValue =
        if null headerPrefix
          then
            requiredConvertedLanguageHeader bindings
              <> maybe
                ""
                (\moduleNameValue -> "module " <> moduleNameValue <> " where\n\n")
                (moduleRenderedName moduleContext)
          else headerPrefix
  Right
    ( text prefixValue
        <> intercalateDoc (text "\n\n") declarationDocuments
        <> text "\n"
    )

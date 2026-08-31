{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Pattern
  ( renderClausePatterns,
    renderPat,
    renderRecordPattern,
    renderRecordPatternItem
  )
where

import GHC.Types.Name.Occurrence (isSymOcc)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import Moonlight.Pale.Ghc.Expr.NameRender (renderRdrName)
import Moonlight.Pale.Ghc.Expr.Render.Carrier
import Moonlight.Pale.Ghc.Expr.Render.Document
import Moonlight.Pale.Ghc.Expr.Render.Literal
import Moonlight.Pale.Ghc.Expr.Render.Name
import Moonlight.Pale.Ghc.Expr.Render.Refusal
import Moonlight.Pale.Ghc.Expr.Syntax

renderClausePatterns ::
  RenderDocument document =>
  RenderSource recursive ->
  [HsPatF] ->
  Either RenderRefusal document
renderClausePatterns renderContext patternValues = do
  patternDocs <- traverse (renderPat renderContext True) patternValues
  Right (renderPatternSpine patternDocs)

renderPatternSpine ::
  RenderDocument document =>
  [document] ->
  document
renderPatternSpine = \case
  [] ->
    mempty
  firstPattern : remainingPatterns ->
    foldl' renderBreakableApplication firstPattern remainingPatterns

renderPat ::
  RenderDocument document =>
  RenderSource recursive ->
  Bool ->
  HsPatF ->
  Either RenderRefusal document
renderPat renderContext atomicContext = \case
  PVarP binderAnn ->
    Right (renderBinderAnn renderContext binderAnn)
  PWildP ->
    Right (text "_")
  PConP conName subPatterns ->
    case subPatterns of
      [] ->
        Right (renderConName conName)
      [leftPattern, rightPattern]
        | isSymOcc (rdrNameOcc conName) -> do
            leftDoc <- renderPat renderContext True leftPattern
            rightDoc <- renderPat renderContext True rightPattern
            Right (wrapParen atomicContext (leftDoc <> text (" " <> renderRdrName conName <> " ") <> rightDoc))
      _ -> do
        argDocs <- traverse (renderPat renderContext True) subPatterns
        Right
          ( wrapParen
              atomicContext
              (renderPatternSpine (renderConName conName : argDocs))
          )
  PTupleP boxity subPatterns -> do
    componentDocs <- traverse (renderPat renderContext False) subPatterns
    let delimiters =
          case boxity of
            BoxedTuple -> ("(", ")")
            UnboxedTuple -> ("(#", "#)")
    Right
      ( renderLayoutDelimitedSequence
          (fst delimiters)
          (snd delimiters)
          componentDocs
      )
  PListP subPatterns -> do
    componentDocs <- traverse (renderPat renderContext False) subPatterns
    Right (renderLayoutDelimitedSequence "[" "]" componentDocs)
  PLitP literalValue ->
    Right (text (renderNormalizedLit literalValue))
  POverLitP literalValue ->
    Right (text (renderNormalizedOverLit literalValue))
  PAsP binderAnn subPattern -> do
    subDoc <- renderPat renderContext True subPattern
    Right (renderBinderAnn renderContext binderAnn <> text "@" <> subDoc)
  PBangP subPattern -> do
    subDoc <- renderPat renderContext True subPattern
    Right (text "!" <> subDoc)
  PLazyP subPattern -> do
    subDoc <- renderPat renderContext True subPattern
    Right (text "~" <> subDoc)
  PParP subPattern -> do
    subDoc <- renderPat renderContext False subPattern
    Right (parenthesizeDoc subDoc)
  PRecP conName fieldPatterns ->
    renderRecordPattern renderContext conName fieldPatterns

renderRecordPattern ::
  RenderDocument document =>
  RenderSource recursive ->
  RdrName ->
  [HsRecPatItem] ->
  Either RenderRefusal document
renderRecordPattern renderContext conName recordItems =
  case recordItems of
    [] ->
      Right (renderConName conName <> text " {}")
    _ -> do
      itemDocuments <-
        traverse (renderRecordPatternItem renderContext) recordItems
      Right
        ( renderConName conName
            <> text " {"
            <> intercalateDoc (text ", ") itemDocuments
            <> text "}"
        )

renderRecordPatternItem ::
  RenderDocument document =>
  RenderSource recursive ->
  HsRecPatItem ->
  Either RenderRefusal document
renderRecordPatternItem renderContext = \case
  HsRecPatField fieldName (HsRecPatExplicit fieldPattern) -> do
    fieldDocument <- renderPat renderContext False fieldPattern
    Right
      ( text (renderRdrName fieldName)
          <> text " = "
          <> fieldDocument
      )
  HsRecPatField fieldName (HsRecPatPun _) ->
    Right (text (renderRdrName fieldName))
  HsRecPatWildcard _ _ ->
    Right (text "..")

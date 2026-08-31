{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Expression
  ( renderExprWith,
    renderClausesExpression,
    renderPatternLambda,
    renderLambdaCase,
    renderLambdaCaseAlt,
    renderLambdaCases,
    renderLambdaCasesAlt,
    renderClauseLhs,
    renderClauseArrow,
    renderClauseArrowCore,
    renderGuardedAlt,
    renderOpChain,
    renderCaseBranch,
    renderCaseExpression,
    renderGuardedCaseAlts,
    renderGuardedCaseAlt,
    renderMultiIf,
    renderMultiIfAlt,
    renderDoExpression,
    renderDoStatement,
    renderLetStatement,
    renderLetExpression,
    renderLetBinding,
    renderGuardStatements,
    renderGuardStatement,
    renderRecordLike,
    renderField,
    renderRecordExpression,
    renderArithSeq
  )
where

import Data.List (dropWhileEnd)
import Data.List.NonEmpty qualified as NonEmpty
import Moonlight.Pale.Ghc.Expr.Render.Analysis
import Moonlight.Pale.Ghc.Expr.Render.Carrier
import Moonlight.Pale.Ghc.Expr.Render.Document
import Moonlight.Pale.Ghc.Expr.Render.Literal
import Moonlight.Pale.Ghc.Expr.Render.Name
import Moonlight.Pale.Ghc.Expr.Render.Pattern
import Moonlight.Pale.Ghc.Expr.Render.Refusal
import Moonlight.Pale.Ghc.Expr.Syntax

renderExprWith ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  recursive ->
  Either RenderRefusal document
renderExprWith renderContext renderMode parentPrecedence expressionTerm = do
  expressionValue <- rsProjectNode renderContext expressionTerm
  case expressionValue of
      VarF variableReference ->
        Right (renderVarRefAtom renderContext variableReference)
      AppF functionValue argumentValue -> do
        functionDoc <- renderExprWith renderContext renderMode 10 functionValue
        argumentDoc <- renderExprWith renderContext renderMode 11 argumentValue
        Right
          ( wrapParen
              (parentPrecedence > 10)
              (renderBreakableApplication functionDoc argumentDoc)
          )
      LamF binderAnn bodyValue -> do
        bodyDoc <- renderExprWith renderContext renderMode 0 bodyValue
        Right
          ( wrapParen
              (parentPrecedence > 0)
              ( renderDelimitedExpression
                  renderMode
                  (text "\\" <> renderBinderAnn renderContext binderAnn)
                  "->"
                  bodyDoc
              )
          )
      LetF _ bindingValues bodyValue ->
        renderLetExpression renderContext renderMode parentPrecedence bindingValues bodyValue
      OpChainF firstOperand chainTail ->
        renderOpChain renderContext renderMode parentPrecedence firstOperand (NonEmpty.toList chainTail)
      SectionLF leftValue operatorValue -> do
        leftDoc <- renderExprWith renderContext renderMode 0 leftValue
        operatorDoc <- renderOperator renderContext operatorValue
        Right (text "(" <> leftDoc <> text " " <> operatorDoc <> text ")")
      SectionRF operatorValue rightValue -> do
        operatorDoc <- renderOperator renderContext operatorValue
        rightDoc <- renderExprWith renderContext renderMode 0 rightValue
        Right (text "(" <> operatorDoc <> text " " <> rightDoc <> text ")")
      ParF innerValue -> do
        innerDoc <- renderExprWith renderContext renderMode 0 innerValue
        Right (parenthesizeDoc innerDoc)
      LitF literalValue ->
        Right (text (renderNormalizedLit literalValue))
      OverLitF literalValue ->
        Right (text (renderNormalizedOverLit literalValue))
      IfF conditionValue thenValue elseValue -> do
        conditionDoc <- renderExprWith renderContext renderMode 0 conditionValue
        thenDoc <- renderExprWith renderContext renderMode 0 thenValue
        elseDoc <- renderExprWith renderContext renderMode 0 elseValue
        Right
          ( wrapParen
              (parentPrecedence > 0)
              ( group
                  ( text "if "
                      <> conditionDoc
                      <> hangingIndent
                        2
                        ( softline
                            <> text "then "
                            <> thenDoc
                            <> softline
                            <> text "else "
                            <> elseDoc
                        )
                  )
              )
          )
      CaseF scrutineeValue branchValues ->
        renderCaseExpression renderContext renderMode parentPrecedence scrutineeValue branchValues
      DoF statementValues -> do
        renderDoExpression renderContext renderMode parentPrecedence statementValues
      NegF innerValue -> do
        innerDoc <- renderExprWith renderContext renderMode 10 innerValue
        Right (wrapParen (parentPrecedence > 9) (text "-" <> innerDoc))
      ExplicitListF valueList -> do
        elementDocs <- traverse (renderExprWith renderContext renderMode 0) valueList
        Right (renderDelimitedSequence "[" "]" elementDocs)
      ExplicitTupleF boxity slots -> do
        slotDocs <- traverse (traverse (renderExprWith renderContext renderMode 0)) slots
        let delimiters =
              case boxity of
                BoxedTuple -> ("(", ")")
                UnboxedTuple -> ("(#", "#)")
        Right
          ( renderDelimitedSequence
              (fst delimiters)
              (snd delimiters)
              (fmap (foldMap id) slotDocs)
          )
      RecordConF constructorValue fieldValues ->
        renderRecordLike renderContext renderMode constructorValue fieldValues
      RecordUpdF recordValue fieldValues ->
        renderRecordLike renderContext renderMode recordValue fieldValues
      ArithSeqF arithSeqValue ->
        renderArithSeq renderContext renderMode arithSeqValue
      GuardedF {} ->
        Left RenderGuardedExpression
      ClausesF clauseValues ->
        renderClausesExpression renderContext renderMode parentPrecedence clauseValues
      MultiIfF guardedAlts ->
        renderMultiIf renderContext renderMode parentPrecedence guardedAlts
      ExprWithTySigF bodyValue typeTextValue -> do
        bodyDoc <- renderExprWith renderContext renderMode 0 bodyValue
        Right (wrapParen (parentPrecedence > 0) (bodyDoc <> text " :: " <> renderTypeText typeTextValue))
      AppTypeF functionValue typeTextValue -> do
        functionDoc <- renderExprWith renderContext renderMode 10 functionValue
        Right (wrapParen (parentPrecedence > 10) (functionDoc <> text " @" <> renderTypeText typeTextValue))

renderClausesExpression ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  [([HsPatF], recursive)] ->
  Either RenderRefusal document
renderClausesExpression renderContext renderMode parentPrecedence clauseValues = do
  validClauses <- validateClauses clauseValues
  renderAsPatternLambda <-
    case validClauses of
      [(patternValues, bodyValue)]
        | not (all isLambdaBinderPattern patternValues) ->
            not <$> isGuardedBody renderContext bodyValue
      _ ->
        Right False
  if renderAsPatternLambda
    then
      case validClauses of
        [(patternValues, bodyValue)] ->
          renderPatternLambda renderContext renderMode parentPrecedence patternValues bodyValue
        _ ->
          Left RenderClausesShape
    else
      if all ((== 1) . length . fst) validClauses
        then renderLambdaCase renderContext renderMode parentPrecedence validClauses
        else renderLambdaCases renderContext renderMode parentPrecedence validClauses

renderPatternLambda ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  [HsPatF] ->
  recursive ->
  Either RenderRefusal document
renderPatternLambda renderContext renderMode parentPrecedence patternValues bodyValue = do
  lhsDoc <- renderClausePatterns renderContext patternValues
  bodyDoc <- renderExprWith renderContext renderMode 0 bodyValue
  Right
    ( wrapParen
        (parentPrecedence > 0)
        (renderDelimitedExpression renderMode (text "\\" <> lhsDoc) "->" bodyDoc)
    )

renderLambdaCase ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  [([HsPatF], recursive)] ->
  Either RenderRefusal document
renderLambdaCase renderContext renderMode parentPrecedence clauseValues = do
  altDocs <- traverse (renderLambdaCaseAlt renderContext renderMode) clauseValues
  Right (wrapParen (parentPrecedence > 0) (renderBlock renderMode "\\case" altDocs))

renderLambdaCaseAlt ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  ([HsPatF], recursive) ->
  Either RenderRefusal document
renderLambdaCaseAlt renderContext renderMode = \case
  ([patternValue], bodyValue) -> do
    patternDoc <- renderPat renderContext False patternValue
    renderClauseArrow renderContext renderMode patternDoc bodyValue
  _ ->
    Left RenderClausesShape

renderLambdaCases ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  [([HsPatF], recursive)] ->
  Either RenderRefusal document
renderLambdaCases renderContext renderMode parentPrecedence clauseValues = do
  altDocs <- traverse (renderLambdaCasesAlt renderContext renderMode) clauseValues
  Right (wrapParen (parentPrecedence > 0) (renderBlock renderMode "\\cases" altDocs))

renderLambdaCasesAlt ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  ([HsPatF], recursive) ->
  Either RenderRefusal document
renderLambdaCasesAlt renderContext renderMode (patternValues, bodyValue) = do
  lhsDoc <- renderClausePatterns renderContext patternValues
  renderClauseArrow renderContext renderMode lhsDoc bodyValue

renderClauseLhs ::
  RenderDocument document =>
  RenderSource recursive ->
  document ->
  [HsPatF] ->
  Either RenderRefusal document
renderClauseLhs renderContext headDoc patternValues =
  (headDoc <+>) <$> renderClausePatterns renderContext patternValues

renderClauseArrow ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  document ->
  recursive ->
  Either RenderRefusal document
renderClauseArrow =
  renderClauseArrowCore

renderClauseArrowCore ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  document ->
  recursive ->
  Either RenderRefusal document
renderClauseArrowCore renderContext renderMode lhsDoc bodyValue = do
  bodyNode <- rsProjectNode renderContext bodyValue
  case bodyNode of
    GuardedF guardedAlts ->
      renderGuardedCaseAlts renderContext renderMode lhsDoc guardedAlts
    _ -> do
      bodyDoc <- renderExprWith renderContext renderMode 0 bodyValue
      Right (renderDelimitedExpression renderMode lhsDoc "->" bodyDoc)

renderGuardedAlt ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  document ->
  String ->
  GuardedAltF recursive ->
  Either RenderRefusal document
renderGuardedAlt renderContext renderMode prefixDoc delimiter guardedAlt =
  case gaGuards guardedAlt of
    [] ->
      Left RenderGuardedExpression
    guardStatements -> do
      guardDoc <- renderGuardStatements renderContext renderMode guardStatements
      bodyDoc <- renderExprWith renderContext renderMode 0 (gaBody guardedAlt)
      Right
        ( renderDelimitedExpression
            renderMode
            (prefixDoc <> guardDoc)
            (trimDelimiter delimiter)
            bodyDoc
        )

trimDelimiter :: String -> String
trimDelimiter =
  dropWhileEnd (== ' ') . dropWhile (== ' ')

renderOpChain ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  recursive ->
  [(recursive, recursive)] ->
  Either RenderRefusal document
renderOpChain renderContext renderMode parentPrecedence firstOperand chainTail = do
  firstDoc <- renderExprWith renderContext renderMode 1 firstOperand
  tailDocs <-
    traverse
      ( \(operatorValue, operandValue) ->
          (,)
            <$> renderOperator renderContext operatorValue
            <*> renderExprWith renderContext renderMode 1 operandValue
      )
      chainTail
  let chainDoc =
        group
          ( firstDoc
              <> foldMap
                ( \(operatorDoc, operandDoc) ->
                    hangingIndent
                      2
                      (softline <> operatorDoc <> text " " <> operandDoc)
                )
                tailDocs
          )
  Right (wrapParen (parentPrecedence > 0) chainDoc)

renderCaseBranch ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  (HsPatF, recursive) ->
  Either RenderRefusal document
renderCaseBranch renderContext renderMode (casePattern, branchValue) = do
  patternDoc <- renderPat renderContext False casePattern
  renderClauseArrow renderContext renderMode patternDoc branchValue

renderCaseExpression ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  recursive ->
  [(HsPatF, recursive)] ->
  Either RenderRefusal document
renderCaseExpression renderContext renderMode parentPrecedence scrutineeValue branchValues = do
  scrutineeDoc <- renderExprWith renderContext renderMode 0 scrutineeValue
  branchDocs <- traverse (renderCaseBranch renderContext renderMode) branchValues
  Right
    ( wrapParen
        (parentPrecedence > 0)
        ( case renderMode of
            CompactRender ->
              text "case "
                <> scrutineeDoc
                <> text " of { "
                <> intercalateDoc (text "; ") branchDocs
                <> text " }"
            GeneratedRender ->
              vcat
                [ text "case " <> scrutineeDoc <> text " of",
                  nest 2 (vcat branchDocs)
                ]
        )
    )

renderGuardedCaseAlts ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  document ->
  [GuardedAltF recursive] ->
  Either RenderRefusal document
renderGuardedCaseAlts renderContext renderMode patternDoc guardedAlts =
  case guardedAlts of
    [] ->
      Left RenderGuardedExpression
    [GuardedAltF [] bodyValue] -> do
      bodyDoc <- renderExprWith renderContext renderMode 0 bodyValue
      Right (renderDelimitedExpression renderMode patternDoc "->" bodyDoc)
    _ -> do
      altDocs <- traverse (renderGuardedCaseAlt renderContext renderMode) guardedAlts
      Right (patternDoc <> hcat altDocs)

renderGuardedCaseAlt ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  GuardedAltF recursive ->
  Either RenderRefusal document
renderGuardedCaseAlt renderContext renderMode =
  renderGuardedAlt renderContext renderMode (text " | ") " -> "

renderMultiIf ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  [GuardedAltF recursive] ->
  Either RenderRefusal document
renderMultiIf renderContext renderMode parentPrecedence guardedAlts =
  case guardedAlts of
    [] ->
      Left RenderGuardedExpression
    firstAlt : restAlts -> do
      firstDoc <- renderMultiIfAlt renderContext renderMode (text "if ") firstAlt
      restDocs <- traverse (renderMultiIfAlt renderContext renderMode (text "   ")) restAlts
      Right
        ( wrapParen
            (parentPrecedence > 0)
            ( case renderMode of
                CompactRender -> hcat (firstDoc : restDocs)
                GeneratedRender -> vcat (firstDoc : restDocs)
            )
        )

renderMultiIfAlt ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  document ->
  GuardedAltF recursive ->
  Either RenderRefusal document
renderMultiIfAlt renderContext renderMode prefixDoc =
  renderGuardedAlt renderContext renderMode (prefixDoc <> text "| ") " -> "

renderDoExpression ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  [HsStmtF recursive] ->
  Either RenderRefusal document
renderDoExpression renderContext renderMode parentPrecedence statementValues = do
  statementDocs <- traverse (renderDoStatement renderContext renderMode) statementValues
  Right (wrapParen (parentPrecedence > 0) (renderBlock renderMode "do" statementDocs))

renderDoStatement ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  HsStmtF recursive ->
  Either RenderRefusal document
renderDoStatement renderContext renderMode = \case
  BindStmtF bindPattern rhsValue -> do
    patternDoc <- renderPat renderContext False bindPattern
    rhsDoc <- renderExprWith renderContext renderMode 0 rhsValue
    Right (renderDelimitedExpression renderMode patternDoc "<-" rhsDoc)
  BodyStmtF exprValue ->
    renderExprWith renderContext renderMode 0 exprValue
  LetStmtF _ bindingValues ->
    renderLetStatement renderContext renderMode bindingValues

renderLetStatement ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  LocalBindingRows recursive ->
  Either RenderRefusal document
renderLetStatement renderContext renderMode bindingValues = do
  bindingDocs <- traverse (renderLetBinding renderContext renderMode) bindingValues
  Right (renderBlock renderMode "let" bindingDocs)

renderLetExpression ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  Int ->
  LocalBindingRows recursive ->
  recursive ->
  Either RenderRefusal document
renderLetExpression renderContext renderMode parentPrecedence bindingValues bodyValue = do
  bindingDocs <- traverse (renderLetBinding renderContext renderMode) bindingValues
  bodyDoc <- renderExprWith renderContext renderMode 0 bodyValue
  Right $
    wrapParen (parentPrecedence > 0) $
      case renderMode of
        CompactRender ->
          text "let " <> intercalateDoc (text "; ") bindingDocs <> text " in " <> bodyDoc
        GeneratedRender ->
          vcat
            [ text "let",
              nest 2 (vcat bindingDocs),
              renderBreakableApplication (text "in") bodyDoc
            ]

renderLetBinding ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  (HsPatF, recursive) ->
  Either RenderRefusal document
renderLetBinding renderContext renderMode (bindingPattern, rhsValue) = do
  patternDoc <- renderPat renderContext False bindingPattern
  rhsDoc <- renderExprWith renderContext renderMode 0 rhsValue
  Right
    (renderDelimitedExpression renderMode patternDoc "=" rhsDoc)

renderGuardStatements ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  [HsGuardStmtF recursive] ->
  Either RenderRefusal document
renderGuardStatements renderContext renderMode guardStatements = do
  guardDocs <- traverse (renderGuardStatement renderContext renderMode) guardStatements
  Right (intercalateDoc (text ", ") guardDocs)

renderGuardStatement ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  HsGuardStmtF recursive ->
  Either RenderRefusal document
renderGuardStatement renderContext renderMode = \case
  GuardBoolF exprValue ->
    renderExprWith renderContext renderMode 0 exprValue
  GuardPatF patternValue rhsValue -> do
    patternDoc <- renderPat renderContext False patternValue
    rhsDoc <- renderExprWith renderContext renderMode 0 rhsValue
    Right (patternDoc <+> text "<-" <+> rhsDoc)
  GuardLetF _ bindingValues ->
    renderLetStatement renderContext renderMode bindingValues

renderRecordLike ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  recursive ->
  [(NormalizedFieldLabel, recursive)] ->
  Either RenderRefusal document
renderRecordLike renderContext renderMode headValue fieldValues = do
  headDoc <- renderExprWith renderContext renderMode 11 headValue
  fieldDocs <- traverse (renderField renderContext renderMode) fieldValues
  Right (renderRecordExpression renderMode headDoc fieldDocs)

renderField ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  (NormalizedFieldLabel, recursive) ->
  Either RenderRefusal document
renderField renderContext renderMode (fieldLabelValue, fieldValue) = do
  fieldDoc <- renderExprWith renderContext renderMode 0 fieldValue
  Right
    ( renderDelimitedExpression
        renderMode
        (text (nflSelector fieldLabelValue))
        "="
        fieldDoc
    )

renderRecordExpression ::
  RenderDocument document =>
  RenderMode ->
  document ->
  [document] ->
  document
renderRecordExpression renderMode headDoc fieldDocs =
  case (renderMode, fieldDocs) of
    (CompactRender, _) ->
      headDoc <> text " { " <> intercalateDoc (text ", ") fieldDocs <> text " }"
    (GeneratedRender, []) ->
      headDoc <> text " {}"
    (GeneratedRender, firstField : remainingFields) ->
      vcat
        [ headDoc,
          nest 2
            ( vcat
                ( (text "{ " <> firstField)
                    : fmap (text ", " <>) remainingFields
                    <> [text "}"]
                )
            )
        ]

renderArithSeq ::
  RenderDocument document =>
  RenderSource recursive ->
  RenderMode ->
  NormalizedArithSeq recursive ->
  Either RenderRefusal document
renderArithSeq renderContext renderMode = \case
  ArithSeqFrom fromValue -> do
    fromDoc <- renderExprWith renderContext renderMode 0 fromValue
    Right (text "[" <> fromDoc <> text " ..]")
  ArithSeqFromThen fromValue thenValue -> do
    fromDoc <- renderExprWith renderContext renderMode 0 fromValue
    thenDoc <- renderExprWith renderContext renderMode 0 thenValue
    Right (text "[" <> fromDoc <> text ", " <> thenDoc <> text " ..]")
  ArithSeqFromTo fromValue toValue -> do
    fromDoc <- renderExprWith renderContext renderMode 0 fromValue
    toDoc <- renderExprWith renderContext renderMode 0 toValue
    Right (text "[" <> fromDoc <> text " .. " <> toDoc <> text "]")
  ArithSeqFromThenTo fromValue thenValue toValue -> do
    fromDoc <- renderExprWith renderContext renderMode 0 fromValue
    thenDoc <- renderExprWith renderContext renderMode 0 thenValue
    toDoc <- renderExprWith renderContext renderMode 0 toValue
    Right (text "[" <> fromDoc <> text ", " <> thenDoc <> text " .. " <> toDoc <> text "]")

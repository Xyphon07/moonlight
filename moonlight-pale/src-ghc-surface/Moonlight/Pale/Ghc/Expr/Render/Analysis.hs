{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Analysis
  ( RequiredExtension (..),
    requiredLanguageHeader,
    requiredConvertedLanguageHeader,
    bindingRequiredExtensions,
    rhsRequiredExtensions,
    exprRequiredExtensions,
    patternRequiredExtensions,
    validateClauses,
    isLambdaBinderPattern,
    isGuardedBody
  )
where

import Data.Kind (Type)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core (Pattern (..))
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
  ( Binding (..),
    bindingGroupBindings,
    Clause (..),
    Rhs (..),
    ConvertedValueBinding,
    tlbBinding,
  )
import Moonlight.Pale.Ghc.Expr.Render.Carrier
import Moonlight.Pale.Ghc.Expr.Render.Refusal
import Moonlight.Pale.Ghc.Expr.Syntax

type RequiredExtension :: Type
data RequiredExtension
  = MultiWayIfExtension
  | NamedFieldPunsExtension
  | RecordWildCardsExtension
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type RequiredExtensions :: Type
newtype RequiredExtensions = RequiredExtensions
  { requiredExtensionSet :: Set RequiredExtension
  }

instance Semigroup RequiredExtensions where
  RequiredExtensions leftExtensions <> RequiredExtensions rightExtensions =
    RequiredExtensions (Set.union leftExtensions rightExtensions)

instance Monoid RequiredExtensions where
  mempty =
    RequiredExtensions Set.empty

requiredLanguageHeader :: [Pattern HsExprF] -> String
requiredLanguageHeader expressionValues =
  renderRequiredLanguageHeader
    (foldMap patternRequiredExtensions expressionValues)

requiredConvertedLanguageHeader :: [ConvertedValueBinding] -> String
requiredConvertedLanguageHeader bindings =
  renderRequiredLanguageHeader
    (foldMap (bindingRequiredExtensions . tlbBinding) bindings)

renderRequiredLanguageHeader :: RequiredExtensions -> String
renderRequiredLanguageHeader =
  foldMap
    ( \requiredExtension ->
        "{-# LANGUAGE "
          <> requiredExtensionName requiredExtension
          <> " #-}\n"
    )
    . Set.toAscList
    . requiredExtensionSet

requiredExtensionName :: RequiredExtension -> String
requiredExtensionName = \case
  MultiWayIfExtension ->
    "MultiWayIf"
  NamedFieldPunsExtension ->
    "NamedFieldPuns"
  RecordWildCardsExtension ->
    "RecordWildCards"

singletonRequiredExtension :: RequiredExtension -> RequiredExtensions
singletonRequiredExtension =
  RequiredExtensions . Set.singleton

bindingRequiredExtensions :: Binding -> RequiredExtensions
bindingRequiredExtensions = \case
  FunctionBinding _ clauses ->
    foldMap
      ( \clauseValue ->
          foldMap hsPatRequiredExtensions (clausePatterns clauseValue)
            <> rhsRequiredExtensions (clauseRhs clauseValue)
      )
      clauses
  PatternBinding patternValue rhsValue ->
    hsPatRequiredExtensions patternValue
      <> rhsRequiredExtensions rhsValue

rhsRequiredExtensions :: Rhs -> RequiredExtensions
rhsRequiredExtensions = \case
  UnguardedRhs bodyExpression maybeBindingGroup ->
    exprRequiredExtensions bodyExpression
      <> maybe
        mempty
        (foldMap bindingRequiredExtensions . bindingGroupBindings)
        maybeBindingGroup
  GuardedRhs alternatives maybeBindingGroup ->
    foldMap (guardedAltRequiredExtensions exprRequiredExtensions) alternatives
      <> maybe
        mempty
        (foldMap bindingRequiredExtensions . bindingGroupBindings)
        maybeBindingGroup

exprRequiredExtensions :: Expr -> RequiredExtensions
exprRequiredExtensions expressionValue =
  expressionNodeRequiredExtensions
    exprRequiredExtensions
    (exprNode expressionValue)

patternRequiredExtensions :: Pattern HsExprF -> RequiredExtensions
patternRequiredExtensions = \case
  PatternVar _ ->
    mempty
  PatternNode nodeValue ->
    expressionNodeRequiredExtensions
      patternRequiredExtensions
      nodeValue

expressionNodeRequiredExtensions ::
  (recursive -> RequiredExtensions) ->
  HsExprF recursive ->
  RequiredExtensions
expressionNodeRequiredExtensions recursiveRequiredExtensions nodeValue =
  constructorRequiredExtensions
    <> expressionNodePatternRequiredExtensions nodeValue
    <> foldMap recursiveRequiredExtensions nodeValue
  where
    constructorRequiredExtensions =
      case nodeValue of
        MultiIfF {} ->
          singletonRequiredExtension MultiWayIfExtension
        _ ->
          mempty

expressionNodePatternRequiredExtensions ::
  HsExprF recursive ->
  RequiredExtensions
expressionNodePatternRequiredExtensions = \case
  LetF _ bindingValues _ ->
    foldMap (hsPatRequiredExtensions . fst) bindingValues
  CaseF _ alternatives ->
    foldMap (hsPatRequiredExtensions . fst) alternatives
  DoF statements ->
    foldMap statementPatternRequiredExtensions statements
  GuardedF alternatives ->
    foldMap guardedAltPatternRequiredExtensions alternatives
  ClausesF clauses ->
    foldMap
      (foldMap hsPatRequiredExtensions . fst)
      clauses
  MultiIfF alternatives ->
    foldMap guardedAltPatternRequiredExtensions alternatives
  _ ->
    mempty

guardedAltRequiredExtensions ::
  (recursive -> RequiredExtensions) ->
  GuardedAltF recursive ->
  RequiredExtensions
guardedAltRequiredExtensions recursiveRequiredExtensions guardedAlternative =
  guardedAltPatternRequiredExtensions guardedAlternative
    <> foldMap recursiveRequiredExtensions guardedAlternative

guardedAltPatternRequiredExtensions ::
  GuardedAltF recursive ->
  RequiredExtensions
guardedAltPatternRequiredExtensions =
  foldMap guardPatternRequiredExtensions . gaGuards

guardPatternRequiredExtensions ::
  HsGuardStmtF recursive ->
  RequiredExtensions
guardPatternRequiredExtensions = \case
  GuardBoolF _ ->
    mempty
  GuardPatF patternValue _ ->
    hsPatRequiredExtensions patternValue
  GuardLetF _ bindingValues ->
    foldMap (hsPatRequiredExtensions . fst) bindingValues

statementPatternRequiredExtensions ::
  HsStmtF recursive ->
  RequiredExtensions
statementPatternRequiredExtensions = \case
  BindStmtF patternValue _ ->
    hsPatRequiredExtensions patternValue
  BodyStmtF _ ->
    mempty
  LetStmtF _ bindingValues ->
    foldMap (hsPatRequiredExtensions . fst) bindingValues

hsPatRequiredExtensions :: HsPatF -> RequiredExtensions
hsPatRequiredExtensions = \case
  PVarP _ ->
    mempty
  PWildP ->
    mempty
  PConP _ subPatterns ->
    foldMap hsPatRequiredExtensions subPatterns
  PTupleP _ subPatterns ->
    foldMap hsPatRequiredExtensions subPatterns
  PListP subPatterns ->
    foldMap hsPatRequiredExtensions subPatterns
  PLitP _ ->
    mempty
  POverLitP _ ->
    mempty
  PAsP _ subPattern ->
    hsPatRequiredExtensions subPattern
  PBangP subPattern ->
    hsPatRequiredExtensions subPattern
  PLazyP subPattern ->
    hsPatRequiredExtensions subPattern
  PParP subPattern ->
    hsPatRequiredExtensions subPattern
  PRecP _ recordItems ->
    foldMap recordItemRequiredExtensions recordItems

recordItemRequiredExtensions ::
  HsRecPatItem ->
  RequiredExtensions
recordItemRequiredExtensions = \case
  HsRecPatField _ (HsRecPatExplicit fieldPattern) ->
    hsPatRequiredExtensions fieldPattern
  HsRecPatField _ (HsRecPatPun _) ->
    singletonRequiredExtension NamedFieldPunsExtension
  HsRecPatWildcard _ _ ->
    singletonRequiredExtension RecordWildCardsExtension

validateClauses :: [([HsPatF], recursive)] -> Either RenderRefusal [([HsPatF], recursive)]
validateClauses clauseValues =
  case clauseValues of
    [] ->
      Left RenderClausesShape
    [(patternValues, _)]
      | null patternValues || all isLambdaBinderPattern patternValues ->
          Left RenderClausesShape
    (firstPatterns, _) : _ ->
      let arityValue = length firstPatterns
       in if arityValue == 0 || any ((/= arityValue) . length . fst) clauseValues
            then Left RenderClausesShape
            else Right clauseValues

isLambdaBinderPattern :: HsPatF -> Bool
isLambdaBinderPattern = \case
  PVarP {} ->
    True
  PParP innerPattern ->
    isLambdaBinderPattern innerPattern
  PBangP innerPattern ->
    isLambdaBinderPattern innerPattern
  PLazyP innerPattern ->
    isLambdaBinderPattern innerPattern
  _ ->
    False

isGuardedBody :: RenderSource recursive -> recursive -> Either RenderRefusal Bool
isGuardedBody renderContext bodyValue = do
  nodeValue <- rsProjectNode renderContext bodyValue
  Right
    ( case nodeValue of
        GuardedF {} ->
          True
        _ ->
          False
    )

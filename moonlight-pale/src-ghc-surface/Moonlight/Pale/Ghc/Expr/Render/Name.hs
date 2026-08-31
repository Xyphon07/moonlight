{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moonlight.Pale.Ghc.Expr.Render.Name
  ( renderFixityDeclaration,
    renderTypeSignature,
    renderSignatureName,
    renderOperator,
    renderConName,
    renderNameAtom,
    renderVarRefAtom,
    renderVarRefOperator,
    renderVarRefName,
    renderBinderAnn,
    renderBinderSpelling,
    renderBinderName,
    renderDefinitionName,
    renderTypeText,
    RenderNamePlan (..),
    patternRenderSource,
    bindingRenderSource,
    prepareRenderSource,
    allocateRenderName,
    allocateAvailableSpelling,
    collectGlobalSpellings,
    allocateTermRenderNames
  )
where

import Data.Char (isAlpha)
import Data.Foldable qualified as Foldable
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Types.Name.Occurrence (isSymOcc, mkVarOcc, occNameString)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual, rdrNameOcc)
import Moonlight.Core (Pattern (..), binderIdKey)
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra (Binding (..))
import Moonlight.Pale.Ghc.Expr.NameRender (renderRdrName)
import Moonlight.Pale.Ghc.Expr.Render.Annotation
import Moonlight.Pale.Ghc.Expr.Render.Carrier
import Moonlight.Pale.Ghc.Expr.Render.Document
import Moonlight.Pale.Ghc.Expr.Render.Refusal
import Moonlight.Pale.Ghc.Expr.Syntax

renderFixityDeclaration :: FixityDeclaration -> String
renderFixityDeclaration declaration =
  let keyword =
        case fixityAssociativity declaration of
          FixityLeft -> "infixl"
          FixityRight -> "infixr"
          FixityNone -> "infix"
   in keyword
        <> " "
        <> show (fixityPrecedence declaration)
        <> " "
        <> intercalate ", " (fmap renderRdrName (NonEmpty.toList (fixityOperators declaration)))

renderTypeSignature :: TypeSignature -> String
renderTypeSignature signature =
  intercalate ", " (fmap renderSignatureName (NonEmpty.toList (typeSignatureNames signature)))
    <> " :: "
    <> nttText (typeSignatureType signature)

renderSignatureName :: RdrName -> String
renderSignatureName nameValue =
  if isSymOcc (rdrNameOcc nameValue)
    then "(" <> renderRdrName nameValue <> ")"
    else renderRdrName nameValue

renderOperator ::
  RenderDocument document =>
  RenderSource recursive ->
  recursive ->
  Either RenderRefusal document
renderOperator renderContext operatorValue = do
  operatorNode <- rsProjectNode renderContext operatorValue
  case operatorNode of
    VarF variableReference ->
      Right (renderVarRefOperator renderContext variableReference)
    _ ->
      Left RenderNonVarOperator

renderConName ::
  RenderDocument document =>
  RdrName ->
  document
renderConName = renderNameAtom

renderNameAtom ::
  RenderDocument document =>
  RdrName ->
  document
renderNameAtom nameValue =
  if isSymOcc (rdrNameOcc nameValue)
    then text ("(" <> renderRdrName nameValue <> ")")
    else text (renderRdrName nameValue)

renderVarRefAtom ::
  RenderDocument document =>
  RenderSource recursive ->
  HsVarRef ->
  document
renderVarRefAtom renderContext =
  renderNameAtom . renderVarRefName renderContext

renderVarRefOperator ::
  RenderDocument document =>
  RenderSource recursive ->
  HsVarRef ->
  document
renderVarRefOperator renderContext variableReference =
  let nameValue = renderVarRefName renderContext variableReference
   in if isSymOcc (rdrNameOcc nameValue)
        then text (renderRdrName nameValue)
        else text ("`" <> renderRdrName nameValue <> "`")

renderVarRefName :: RenderSource recursive -> HsVarRef -> RdrName
renderVarRefName renderContext = \case
  GlobalName globalName ->
    globalName
  LocalName binderAnn ->
    renderBinderName renderContext binderAnn

renderBinderAnn ::
  RenderDocument document =>
  RenderSource recursive ->
  BinderAnn ->
  document
renderBinderAnn renderContext =
  renderNameAtom . renderBinderName renderContext

renderBinderSpelling :: RenderSource recursive -> BinderAnn -> String
renderBinderSpelling renderContext =
  renderRdrName . renderBinderName renderContext

renderBinderName :: RenderSource recursive -> BinderAnn -> RdrName
renderBinderName renderContext binderAnn =
  IntMap.findWithDefault
    (baName binderAnn)
    (binderIdKey (baId binderAnn))
    (rsRenderNames renderContext)

renderDefinitionName ::
  RenderDocument document =>
  String ->
  document
renderDefinitionName definitionName =
  case definitionName of
    headChar : _
      | isAlpha headChar || headChar == '_' ->
          text definitionName
    _ ->
      text ("(" <> definitionName <> ")")

renderTypeText ::
  RenderDocument document =>
  NormalizedTypeText ->
  document
renderTypeText =
  text . nttText

type RenderNamePlan :: Type
data RenderNamePlan = RenderNamePlan
  { rnpSeenBinders :: !IntSet,
    rnpNames :: !(IntMap RdrName),
    rnpUsedSpellings :: !(Map String ()),
    rnpNextSuffixes :: !(Map String Int)
  }

patternRenderSource :: Pattern HsExprF -> Either RenderRefusal (RenderSource (Pattern HsExprF))
patternRenderSource expressionValue =
  prepareRenderSource projectPatternNode [] [expressionValue]

bindingRenderSource :: Binding -> Either RenderRefusal (RenderSource Expr)
bindingRenderSource bindingValue =
  prepareRenderSource
    (Right . exprNode)
    (bindingRenderAnnotations bindingValue)
    (bindingRootExpressions bindingValue)

prepareRenderSource ::
  RenderNodeProjection recursive ->
  [BinderAnn] ->
  [recursive] ->
  Either RenderRefusal (RenderSource recursive)
prepareRenderSource projectNode outerBindingAnnotations rootTerms = do
  globalSpellings <-
    Foldable.foldlM
      (collectGlobalSpellings projectNode)
      Map.empty
      rootTerms
  let !outerNamePlan =
        Foldable.foldl'
          allocateRenderName
          RenderNamePlan
            { rnpSeenBinders = IntSet.empty,
              rnpNames = IntMap.empty,
              rnpUsedSpellings = globalSpellings,
              rnpNextSuffixes = Map.empty
            }
          outerBindingAnnotations
  namePlan <-
    Foldable.foldlM
      (allocateTermRenderNames projectNode)
      outerNamePlan
      rootTerms
  Right
    RenderSource
      { rsProjectNode = projectNode,
        rsRenderNames = rnpNames namePlan
      }

allocateRenderName :: RenderNamePlan -> BinderAnn -> RenderNamePlan
allocateRenderName namePlan binderAnn =
  let binderKey = binderIdKey (baId binderAnn)
   in if IntSet.member binderKey (rnpSeenBinders namePlan)
        then namePlan
        else
          let preferredName = occNameString (rdrNameOcc (baName binderAnn))
              currentSuffix =
                Map.findWithDefault 0 preferredName (rnpNextSuffixes namePlan)
              (chosenSpelling, nextSuffix) =
                allocateAvailableSpelling
                  (rnpUsedSpellings namePlan)
                  currentSuffix
                  preferredName
              chosenName = mkRdrUnqual (mkVarOcc chosenSpelling)
              renderNameOverrides =
                if chosenName == baName binderAnn
                  then rnpNames namePlan
                  else IntMap.insert binderKey chosenName (rnpNames namePlan)
              nextSuffixes =
                if nextSuffix == currentSuffix
                  then rnpNextSuffixes namePlan
                  else
                    Map.insert
                      preferredName
                      nextSuffix
                      (rnpNextSuffixes namePlan)
           in namePlan
                { rnpSeenBinders =
                    IntSet.insert binderKey (rnpSeenBinders namePlan),
                  rnpNames = renderNameOverrides,
                  rnpUsedSpellings = Map.insert chosenSpelling () (rnpUsedSpellings namePlan),
                  rnpNextSuffixes = nextSuffixes
                }

allocateAvailableSpelling :: Map String () -> Int -> String -> (String, Int)
allocateAvailableSpelling usedSpellings nextSuffix initialSpelling
  | Map.notMember initialSpelling usedSpellings =
      (initialSpelling, nextSuffix)
  | otherwise =
      firstUnusedSuffix nextSuffix
  where
    firstUnusedSuffix suffix =
      let candidateSpelling =
            initialSpelling <> "_" <> show suffix
       in if Map.member candidateSpelling usedSpellings
            then firstUnusedSuffix (suffix + 1)
            else (candidateSpelling, suffix + 1)

collectGlobalSpellings ::
  RenderNodeProjection recursive ->
  Map String () ->
  recursive ->
  Either RenderRefusal (Map String ())
collectGlobalSpellings projectNode accumulatedSpellings expressionValue = do
  nodeValue <- projectNode expressionValue
  let !nodeSpellings =
        case nodeValue of
          VarF (GlobalName globalName) ->
            Map.insert (renderRdrName globalName) () accumulatedSpellings
          _ ->
            accumulatedSpellings
  Foldable.foldlM
    (collectGlobalSpellings projectNode)
    nodeSpellings
    nodeValue

allocateTermRenderNames ::
  RenderNodeProjection recursive ->
  RenderNamePlan ->
  recursive ->
  Either RenderRefusal RenderNamePlan
allocateTermRenderNames projectNode namePlan expressionValue = do
  nodeValue <- projectNode expressionValue
  let !nodeNamePlan =
        Foldable.foldl'
          allocateRenderName
          namePlan
          (nodeBindingAnnotations nodeValue)
  Foldable.foldlM
    (allocateTermRenderNames projectNode)
    nodeNamePlan
    nodeValue

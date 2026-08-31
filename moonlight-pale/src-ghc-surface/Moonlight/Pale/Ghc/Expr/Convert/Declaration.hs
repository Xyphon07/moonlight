{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Declaration
  ( fixityAssociativityFromGhc,
    convertLocatedDecl,
    convertDecl,
    convertSignatureDeclaration,
    opaqueDeclaration,
    convertValueBinding
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import GHC.Hs
  ( ClsInstDecl (..),
    GhcPs,
    HsBind,
    HsDecl (..),
    InstDecl (..),
    LHsBind,
    LHsDecl,
    FixitySig (..),
    Sig (..),
  )
import GHC.Parser.Annotation (getLocA)
import Language.Haskell.Syntax.Basic (Fixity (..), FixityDirection (..))
import GHC.Types.SrcLoc (unLoc)
import Moonlight.Pale.Ghc.Expr.Convert.Expression
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction
import Moonlight.Pale.Ghc.Expr.Convert.Pattern
import Moonlight.Pale.Ghc.Expr.Convert.Row
import Moonlight.Pale.Ghc.Expr.Convert.Source
import Moonlight.Pale.Ghc.Expr.Convert.State
import Moonlight.Pale.Ghc.Expr.Syntax

fixityAssociativityFromGhc :: FixityDirection -> FixityAssociativity
fixityAssociativityFromGhc = \case
  InfixL -> FixityLeft
  InfixR -> FixityRight
  InfixN -> FixityNone

convertLocatedDecl :: SourceSliceIndex -> LHsDecl GhcPs -> ConvM ModuleDeclaration
convertLocatedDecl moduleSourceIndex locatedDecl =
  convertDecl
    moduleSourceIndex
    (sourceRegionFromSrcSpan (getLocA locatedDecl))
    (unLoc locatedDecl)

convertDecl :: SourceSliceIndex -> Maybe SourceRegion -> HsDecl GhcPs -> ConvM ModuleDeclaration
convertDecl moduleSourceIndex declRegion = \case
  ValD _ bindValue ->
    ValueDeclaration <$> convertValueBinding declRegion bindValue
  SigD _ signature ->
    convertSignatureDeclaration moduleSourceIndex declRegion signature
  TyClD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedTypeOrClassDeclaration
  InstD _ instanceDeclaration ->
    convertInstanceDeclaration moduleSourceIndex declRegion instanceDeclaration
  DerivD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedDerivingDeclaration
  KindSigD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedKindSignatureDeclaration
  DefD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedDefaultDeclaration
  ForD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedForeignDeclaration
  WarningD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedWarningDeclaration
  AnnD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedAnnotationDeclaration
  RuleD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedRuleDeclaration
  SpliceD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedSpliceDeclaration
  DocD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedDocumentationDeclaration
  RoleAnnotD {} ->
    opaqueDeclaration moduleSourceIndex declRegion UnsupportedRoleAnnotationDeclaration

convertSignatureDeclaration ::
  SourceSliceIndex ->
  Maybe SourceRegion ->
  Sig GhcPs ->
  ConvM ModuleDeclaration
convertSignatureDeclaration moduleSourceIndex declarationRegion = \case
  TypeSig _ names signatureType ->
    case NonEmpty.nonEmpty (fmap unLoc names) of
      Nothing ->
        throwConvert (ConvertEmptyTypeSignature declarationRegion)
      Just signatureNames ->
        pure
          ( TypeSignatureDeclaration
              TypeSignature
                { typeSignatureNames = signatureNames,
                  typeSignatureType = normalizedTypeText signatureType
                }
          )
  FixSig _ (FixitySig _ operatorNames (Fixity precedence direction)) ->
    case NonEmpty.nonEmpty (fmap unLoc operatorNames) of
      Nothing ->
        throwConvert (ConvertEmptyFixityDeclaration declarationRegion)
      Just fixityOperatorNames ->
        pure
          ( FixityDeclarationNode
              FixityDeclaration
                { fixityAssociativity = fixityAssociativityFromGhc direction,
                  fixityPrecedence = precedence,
                  fixityOperators = fixityOperatorNames
                }
          )
  PatSynSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedPatternSynonymSignature
  ClassOpSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedClassOperationSignature
  InlineSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedInlineSignature
  SpecSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedSpecializationSignature
  SpecSigE {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedExpressionSpecializationSignature
  SpecInstSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedInstanceSpecializationSignature
  MinimalSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedMinimalSignature
  SCCFunSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedCostCentreSignature
  CompleteMatchSig {} ->
    opaqueDeclaration moduleSourceIndex declarationRegion UnsupportedCompleteMatchSignature

opaqueDeclaration ::
  SourceSliceIndex ->
  Maybe SourceRegion ->
  UnsupportedDeclarationTag ->
  ConvM ModuleDeclaration
opaqueDeclaration moduleSourceIndex declarationRegion declarationTag =
  uncurry (OpaqueDeclaration declarationTag)
    <$> requireDeclarationSource
      moduleSourceIndex
      declarationRegion
      (ConvertDeclarationSourceUnavailable declarationRegion declarationTag)

convertInstanceDeclaration ::
  SourceSliceIndex ->
  Maybe SourceRegion ->
  InstDecl GhcPs ->
  ConvM ModuleDeclaration
convertInstanceDeclaration moduleSourceIndex declarationRegion = \case
  ClsInstD {cid_inst = ClsInstDecl {cid_binds = methodBindings}} -> do
    (instanceRegion, instanceSource) <-
      requireDeclarationSource
        moduleSourceIndex
        declarationRegion
        (ConvertInstanceDeclarationSourceUnavailable declarationRegion)
    methodSections <-
      traverse convertLocatedInstanceMethod (toList methodBindings)
    pure
      ( InstanceDeclarationNode
          ConvertedInstanceDeclaration
            { convertedInstanceRegion = instanceRegion,
              convertedInstanceSource = instanceSource,
              convertedInstanceMethods = methodSections
            }
      )
  DataFamInstD {} ->
    opaqueDeclaration
      moduleSourceIndex
      declarationRegion
      UnsupportedDataFamilyInstanceDeclaration
  TyFamInstD {} ->
    opaqueDeclaration
      moduleSourceIndex
      declarationRegion
      UnsupportedTypeFamilyInstanceDeclaration

convertLocatedInstanceMethod ::
  LHsBind GhcPs ->
  ConvM InstanceMethodSection
convertLocatedInstanceMethod locatedBinding = do
  let methodRegion =
        sourceRegionFromSrcSpan (getLocA locatedBinding)
  methodResult <-
    runInstanceMethodSection
      methodRegion
      (convertValueBinding methodRegion (unLoc locatedBinding))
  pure
    ( case methodResult of
        Left methodObstruction ->
          ObstructedInstanceMethod methodObstruction
        Right convertedBinding ->
          TraversableInstanceMethod convertedBinding
    )

requireDeclarationSource ::
  SourceSliceIndex ->
  Maybe SourceRegion ->
  ConvertObstruction ->
  ConvM (SourceRegion, String)
requireDeclarationSource moduleSourceIndex declarationRegion unavailableObstruction =
  case declarationRegion >>= sourceSliceForRegion moduleSourceIndex of
    Just declarationSection ->
      pure declarationSection
    Nothing ->
      throwConvert unavailableObstruction

convertValueBinding :: Maybe SourceRegion -> HsBind GhcPs -> ConvM ConvertedValueBinding
convertValueBinding declRegion bindValue = do
  bindingScope <- freshChildScope
  convertedBinding <-
    withScope bindingScope $ do
      headPattern <- bindingHeadPatternFromBind declRegion bindValue
      convertBindingWithPattern Map.empty headPattern bindValue
  let !bindingMetrics =
        convertedExpressionMetrics (cbExpression convertedBinding)
  pure
    ConvertedValueBinding
      { convertedValueBindingValue = cbBinding convertedBinding,
        convertedValueBindingScope = bindingScope,
        convertedValueBindingRegion = declRegion,
        convertedValueBindingMetricSection = bindingMetrics
      }

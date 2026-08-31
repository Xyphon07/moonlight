{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Pattern
  ( simpleLambdaBinderNames,
    localBindPattern,
    bindingHeadPatternFromBind,
    normalizeFieldOcc,
    simpleLambdaBinderName,
    collectPatternNames,
    collectResolvedPatternNames,
    convertPat,
    convertRecPatField,
    lossyPat,
    convertTupleBoxity
  )
where

import Control.Applicative ((<|>))
import Data.Set qualified as Set
import GHC.Hs
  ( FieldOcc (..),
    GhcPs,
    HsBind,
    HsBindLR (..),
    HsConDetails (..),
    HsFieldBind (..),
    HsRecField,
    HsRecFields (..),
    LHsBind,
    LHsExpr,
    LPat,
    Match (..),
    Pat (..),
    RecFieldsDotDot (..),
  )
import GHC.Hs.Utils (CollectFlag (CollNoDictBinders), collectPatBinders)
import GHC.Parser.Annotation (EpaLocation, getHasLoc, getLocA)
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated, unLoc)
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction
import Moonlight.Pale.Ghc.Expr.Convert.State
import Moonlight.Pale.Ghc.Expr.Opaque
import Moonlight.Pale.Ghc.Expr.Syntax

simpleLambdaBinderNames :: Match GhcPs (LHsExpr GhcPs) -> Maybe [RdrName]
simpleLambdaBinderNames matchValue =
  traverse (simpleLambdaBinderName . unLoc) (unLoc (m_pats matchValue))

localBindPattern :: LHsBind GhcPs -> ConvM HsPatF
localBindPattern locatedBinding =
  bindingHeadPatternFromBind
    (sourceRegionFromSrcSpan (getLocA locatedBinding))
    (unLoc locatedBinding)

bindingHeadPatternFromBind ::
  Maybe SourceRegion ->
  HsBind GhcPs ->
  ConvM HsPatF
bindingHeadPatternFromBind region = \case
  FunBind {fun_id = nameValue} ->
    PVarP <$> freshBinderAnn (unLoc nameValue)
  PatBind {pat_lhs = patternValue} ->
    convertPat patternValue
  VarBind {var_id = nameValue} ->
    PVarP <$> freshBinderAnn nameValue
  PatSynBind {} ->
    throwUnsupportedExpression region OpaquePatternSynonymBind

normalizeFieldOcc :: RdrName -> NormalizedFieldLabel
normalizeFieldOcc rdrName =
  NormalizedFieldLabel
    { nflSelector = occNameString (rdrNameOcc rdrName),
      nflAllowsDuplicateRecordFields = False,
      nflHasSelector = True
    }

simpleLambdaBinderName :: Pat GhcPs -> Maybe RdrName
simpleLambdaBinderName = \case
  VarPat _ nameValue -> Just (unLoc nameValue)
  ParPat _ patternValue -> simpleLambdaBinderName (unLoc patternValue)
  BangPat _ patternValue -> simpleLambdaBinderName (unLoc patternValue)
  LazyPat _ patternValue -> simpleLambdaBinderName (unLoc patternValue)
  SigPat _ patternValue _ -> simpleLambdaBinderName (unLoc patternValue)
  _ -> Nothing

collectPatternNames :: LPat GhcPs -> [RdrName]
collectPatternNames = collectPatBinders CollNoDictBinders

collectResolvedPatternNames :: LPat GhcPs -> ConvM [RdrName]
collectResolvedPatternNames patternValue =
  (collectPatternNames patternValue <>)
    <$> collectRecordWildcardBinderNames patternValue

collectRecordWildcardBinderNames :: LPat GhcPs -> ConvM [RdrName]
collectRecordWildcardBinderNames patternValue =
  case unLoc patternValue of
    ParPat _ innerValue ->
      collectRecordWildcardBinderNames innerValue
    BangPat _ innerValue ->
      collectRecordWildcardBinderNames innerValue
    LazyPat _ innerValue ->
      collectRecordWildcardBinderNames innerValue
    AsPat _ _ innerValue ->
      collectRecordWildcardBinderNames innerValue
    TuplePat _ componentValues _ ->
      concat <$> traverse collectRecordWildcardBinderNames componentValues
    ListPat _ componentValues ->
      concat <$> traverse collectRecordWildcardBinderNames componentValues
    ConPat {pat_con = constructorValue, pat_args = argumentsValue} ->
      case argumentsValue of
        PrefixCon argumentValues ->
          concat <$> traverse collectRecordWildcardBinderNames argumentValues
        InfixCon leftValue rightValue ->
          concat
            <$> traverse
              collectRecordWildcardBinderNames
              [leftValue, rightValue]
        RecCon recordFieldsValue -> do
          nestedWildcardBinders <-
            concat
              <$> traverse
                (collectRecordWildcardBinderNames . hfbRHS . unLoc)
                (rec_flds recordFieldsValue)
          currentWildcardBinders <-
            case rec_dotdot recordFieldsValue of
              Nothing ->
                pure []
              Just locatedDotDot -> do
                wildcardRegion <-
                  recordWildcardRegion patternValue locatedDotDot
                resolveRecordWildcardBinderNames
                  wildcardRegion
                  (unLoc constructorValue)
                  (fmap (recordFieldName . unLoc) (rec_flds recordFieldsValue))
          pure (currentWildcardBinders <> nestedWildcardBinders)
    _ ->
      pure []

convertPat :: LPat GhcPs -> ConvM HsPatF
convertPat patternValue =
  case unLoc patternValue of
    VarPat _ nameValue ->
      PVarP <$> freshBinderAnn (unLoc nameValue)
    WildPat {} ->
      pure PWildP
    ParPat _ innerValue ->
      PParP <$> convertPat innerValue
    BangPat _ innerValue ->
      PBangP <$> convertPat innerValue
    LazyPat _ innerValue ->
      PLazyP <$> convertPat innerValue
    AsPat _ nameValue innerValue ->
      PAsP <$> freshBinderAnn (unLoc nameValue) <*> convertPat innerValue
    TuplePat _ componentValues boxity ->
      PTupleP (convertTupleBoxity boxity) <$> traverse convertPat componentValues
    ListPat _ componentValues ->
      PListP <$> traverse convertPat componentValues
    LitPat _ literalValue ->
      pure (PLitP (normalizeHsLit literalValue))
    NPat _ overLitValue Nothing _ ->
      pure (POverLitP (normalizeHsOverLit (unLoc overLitValue)))
    NPat {} ->
      lossyPat PatOpaqueNegativeLit patternValue
    ConPat {pat_con = conValue, pat_args = argsValue} ->
      case argsValue of
        PrefixCon argValues ->
          PConP (unLoc conValue) <$> traverse convertPat argValues
        InfixCon leftValue rightValue ->
          PConP (unLoc conValue) <$> traverse convertPat [leftValue, rightValue]
        RecCon recordFieldsValue -> do
          convertedFields <-
            traverse (convertRecPatField . unLoc) (rec_flds recordFieldsValue)
          case rec_dotdot recordFieldsValue of
            Nothing ->
              pure (PRecP (unLoc conValue) convertedFields)
            Just locatedDotDot -> do
              wildcardRegion <-
                recordWildcardRegion patternValue locatedDotDot
              wildcardBinderNames <-
                resolveRecordWildcardBinderNames
                  wildcardRegion
                  (unLoc conValue)
                  (fmap (recordFieldName . unLoc) (rec_flds recordFieldsValue))
              wildcardBinders <-
                traverse freshBinderAnn wildcardBinderNames
              recordItems <-
                insertRecordWildcard
                  wildcardRegion
                  (unRecFieldsDotDot (unLoc locatedDotDot))
                  wildcardBinders
                  convertedFields
              pure (PRecP (unLoc conValue) recordItems)
    OrPat {} ->
      lossyPat PatOpaqueOr patternValue
    SumPat {} ->
      lossyPat PatOpaqueSum patternValue
    ViewPat {} ->
      lossyPat PatOpaqueView patternValue
    SplicePat {} ->
      lossyPat PatOpaqueSplice patternValue
    NPlusKPat {} ->
      lossyPat PatOpaqueNPlusK patternValue
    SigPat {} ->
      lossyPat PatOpaqueSig patternValue
    EmbTyPat {} ->
      lossyPat PatOpaqueEmbTy patternValue
    InvisPat {} ->
      lossyPat PatOpaqueInvis patternValue

convertRecPatField :: HsRecField GhcPs (LPat GhcPs) -> ConvM HsRecPatItem
convertRecPatField fieldBindValue =
  case unLoc (hfbLHS fieldBindValue) of
    FieldOcc {foLabel = labelValue} -> do
      let fieldName = unLoc labelValue
      fieldValue <-
        if hfbPun fieldBindValue
          then
            HsRecPatPun
              <$> freshBinderAnn
                (mkRdrUnqual (rdrNameOcc fieldName))
          else
            HsRecPatExplicit <$> convertPat (hfbRHS fieldBindValue)
      pure (HsRecPatField fieldName fieldValue)

recordFieldName :: HsRecField GhcPs argument -> RdrName
recordFieldName fieldBindValue =
  case unLoc (hfbLHS fieldBindValue) of
    FieldOcc {foLabel = labelValue} ->
      unLoc labelValue

resolveRecordWildcardBinderNames ::
  SourceRegion ->
  RdrName ->
  [RdrName] ->
  ConvM [RdrName]
resolveRecordWildcardBinderNames wildcardRegion constructorName explicitFieldNames = do
  constructorFieldNames <-
    resolveRecordWildcardFields wildcardRegion constructorName
  let explicitOccurrences =
        Set.fromList (fmap rdrNameOcc explicitFieldNames)
  pure
    ( fmap
        (mkRdrUnqual . rdrNameOcc)
        ( filter
            (\fieldName -> Set.notMember (rdrNameOcc fieldName) explicitOccurrences)
            constructorFieldNames
        )
    )

recordWildcardRegion ::
  LPat GhcPs ->
  GenLocated EpaLocation RecFieldsDotDot ->
  ConvM SourceRegion
recordWildcardRegion patternValue locatedDotDot =
  case
      sourceRegionFromSrcSpan (getHasLoc locatedDotDot)
        <|> sourceRegionFromSrcSpan (getLocA patternValue)
    of
      Just wildcardRegion ->
        pure wildcardRegion
      Nothing ->
        throwConvert
          ( ConvertRecordWildcardRegionUnavailable
              (sourceRegionFromSrcSpan (getLocA patternValue))
          )

insertRecordWildcard ::
  SourceRegion ->
  Int ->
  [BinderAnn] ->
  [HsRecPatItem] ->
  ConvM [HsRecPatItem]
insertRecordWildcard wildcardRegion wildcardPosition wildcardBinders recordItems
  | wildcardPosition < 0 || wildcardPosition > length recordItems =
      throwConvert
        ( ConvertRecordWildcardPositionInvalid
            wildcardRegion
            wildcardPosition
            (length recordItems)
        )
  | otherwise =
      let (beforeWildcard, afterWildcard) =
            splitAt wildcardPosition recordItems
       in pure
            ( beforeWildcard
                <> [HsRecPatWildcard wildcardRegion wildcardBinders]
                <> afterWildcard
            )

lossyPat :: HsPatOpaqueTag -> LPat GhcPs -> ConvM HsPatF
lossyPat tagValue patternValue =
  throwConvert
    ( ConvertUnsupportedPattern
        (sourceRegionFromSrcSpan (getLocA patternValue))
        tagValue
    )

convertTupleBoxity :: Boxity -> TupleBoxity
convertTupleBoxity = \case
  Boxed -> BoxedTuple
  Unboxed -> UnboxedTuple

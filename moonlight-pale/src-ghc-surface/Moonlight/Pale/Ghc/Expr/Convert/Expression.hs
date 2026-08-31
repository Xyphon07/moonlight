{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Expression
  ( convertExpr,
    convertLocatedExpr,
    convertBindingWithPattern,
    convertSourceMatchGroup,
    convertSimpleSourceClause,
    convertSourceClause,
    convertSourceRhs,
    convertLambdaLikeMatchGroup,
    convertClauses,
    convertClause,
    convertLambdaBinders,
    convertCaseAlternatives,
    convertCaseAlternative,
    convertGRHSs,
    convertGuardedAlt,
    convertGuardedAltBody,
    prependGuardStatement,
    convertStatements,
    convertLocalBinds,
    convertValBinds,
    convertTupleArg,
    flattenOpChain,
    flattenLocatedOpChain,
    convertRecordFields,
    convertRecordUpdFields,
    convertRecordField,
    convertArithSeq
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Hs
  ( ArithSeqInfo (..),
    ExprLStmt,
    FieldOcc (..),
    GRHS (..),
    GRHSs (..),
    GuardLStmt,
    GhcPs,
    HsBind,
    HsBindLR (..),
    HsExpr (..),
    HsFieldBind (..),
    HsLocalBinds,
    HsLocalBindsLR (..),
    HsPragE (HsPragSCC),
    HsRecField,
    HsRecFields (..),
    HsTupArg (..),
    HsValBindsLR (..),
    LGRHS,
    LHsExpr,
    LHsRecUpdFields (..),
    LMatch,
    Match (..),
    MatchGroup (..),
    StmtLR (..),
  )
import GHC.Parser.Annotation (getLocA)
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (unLoc)
import Moonlight.Pale.Ghc.Expr.Convert.Dependencies qualified as Dependencies
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction
import Moonlight.Pale.Ghc.Expr.Convert.Pattern
import Moonlight.Pale.Ghc.Expr.Convert.Projection
import Moonlight.Pale.Ghc.Expr.Convert.Row
import Moonlight.Pale.Ghc.Expr.Convert.Source
import Moonlight.Pale.Ghc.Expr.Convert.State
import Moonlight.Pale.Ghc.Expr.Opaque
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax

convertExpr :: Env -> Maybe SourceRegion -> HsExpr GhcPs -> ConvM ConvExpr
convertExpr env region = \case
  HsVar _ nameValue -> do
    variableReference <- resolveVarRef env (unLoc nameValue)
    mkConvExpr region (VarF variableReference)
  HsOverLabel {} ->
    throwUnsupportedExpression region OpaqueOverLabel
  HsIPVar {} ->
    throwUnsupportedExpression region OpaqueIPVar
  HsOverLit _ overLitValue ->
    mkConvExpr region (OverLitF (normalizeHsOverLit overLitValue))
  HsLit _ literalValue ->
    mkConvExpr region (LitF (normalizeHsLit literalValue))
  HsLam _ _ matchGroupValue ->
    convertLambdaLikeMatchGroup env region matchGroupValue
  HsApp _ functionValue argumentValue -> do
    functionExpr <- convertLocatedExpr env functionValue
    argumentExpr <- convertLocatedExpr env argumentValue
    mkConvExpr region (AppF functionExpr argumentExpr)
  HsAppType _ exprValue typeValue -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (AppTypeF innerExpr (normalizedTypeText typeValue))
  OpApp _ leftValue operatorValue rightValue -> do
    let (firstOperand, chainTail) =
          flattenOpChain leftValue operatorValue rightValue
    convertedFirst <- convertLocatedExpr env firstOperand
    convertedTail <-
      traverse
        ( \(operatorTerm, operandTerm) ->
            (,)
              <$> convertLocatedExpr env operatorTerm
              <*> convertLocatedExpr env operandTerm
        )
        chainTail
    mkConvExpr region (OpChainF convertedFirst convertedTail)
  NegApp _ exprValue _ -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (NegF innerExpr)
  HsPar _ exprValue -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (ParF innerExpr)
  SectionL _ exprValue operatorValue -> do
    leftExpr <- convertLocatedExpr env exprValue
    operatorExpr <- convertLocatedExpr env operatorValue
    mkConvExpr region (SectionLF leftExpr operatorExpr)
  SectionR _ operatorValue exprValue -> do
    operatorExpr <- convertLocatedExpr env operatorValue
    rightExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (SectionRF operatorExpr rightExpr)
  ExplicitTuple _ tupleArgs boxity -> do
    tupleExprs <- traverse (convertTupleArg env) tupleArgs
    mkConvExpr region (ExplicitTupleF (convertTupleBoxity boxity) tupleExprs)
  ExplicitSum {} ->
    throwUnsupportedExpression region OpaqueExplicitSum
  HsCase _ scrutineeValue matchGroupValue -> do
    scrutineeExpr <- convertLocatedExpr env scrutineeValue
    alternatives <- convertCaseAlternatives env matchGroupValue
    mkConvExpr region (CaseF scrutineeExpr alternatives)
  HsIf _ conditionValue thenValue elseValue -> do
    conditionExpr <- convertLocatedExpr env conditionValue
    thenExpr <- convertLocatedExpr env thenValue
    elseExpr <- convertLocatedExpr env elseValue
    mkConvExpr region (IfF conditionExpr thenExpr elseExpr)
  HsMultiIf _ grhsValues -> do
    guardedAlts <- traverse (convertGuardedAlt env) (NonEmpty.toList grhsValues)
    mkConvExpr region (MultiIfF guardedAlts)
  HsLet _ localBindsValue bodyValue ->
    convertLocalBinds region env localBindsValue >>= \case
      Nothing ->
        throwUnsupportedExpression region OpaqueEmptyLocalBinds
      Just convertedBinds -> do
        bodyExpr <-
          withScope
            (clbScope convertedBinds)
            (convertLocatedExpr (clbEnv convertedBinds) bodyValue)
        mkConvExpr region (LetF (clbRecursion convertedBinds) (clbBindings convertedBinds) bodyExpr)
  HsDo _ _ statementValues -> do
    statements <- convertStatements env (unLoc statementValues)
    mkConvExpr region (DoF statements)
  ExplicitList _ exprValues -> do
    listExprs <- traverse (convertLocatedExpr env) exprValues
    mkConvExpr region (ExplicitListF listExprs)
  RecordCon {rcon_con = constructorValue, rcon_flds = recordFieldsValue} -> do
    constructorExpr <- mkConvExpr Nothing (VarF (GlobalName (unLoc constructorValue)))
    fieldValues <- convertRecordFields env recordFieldsValue
    mkConvExpr region (RecordConF constructorExpr fieldValues)
  RecordUpd {rupd_expr = recordValue, rupd_flds = recordFieldsValue} -> do
    fieldValues <- convertRecordUpdFields region env recordFieldsValue
    recordExpr <- convertLocatedExpr env recordValue
    mkConvExpr region (RecordUpdF recordExpr fieldValues)
  HsGetField {} ->
    throwUnsupportedExpression region OpaqueGetField
  HsProjection {} ->
    throwUnsupportedExpression region OpaqueProjection
  ExprWithTySig _ exprValue sigValue -> do
    innerExpr <- convertLocatedExpr env exprValue
    mkConvExpr region (ExprWithTySigF innerExpr (normalizedTypeText sigValue))
  ArithSeq _ _ arithSeqValue -> do
    convertedSeq <- convertArithSeq env arithSeqValue
    mkConvExpr region (ArithSeqF convertedSeq)
  HsTypedBracket {} ->
    throwUnsupportedExpression region OpaqueTypedBracket
  HsUntypedBracket {} ->
    throwUnsupportedExpression region OpaqueUntypedBracket
  HsTypedSplice {} ->
    throwUnsupportedExpression region OpaqueTypedSplice
  HsUntypedSplice {} ->
    throwUnsupportedExpression region OpaqueUntypedSplice
  HsProc {} ->
    throwUnsupportedExpression region OpaqueProc
  HsStatic {} ->
    throwUnsupportedExpression region OpaqueStatic
  HsPragE _ (HsPragSCC {}) exprValue ->
    convertLocatedExpr env exprValue
  HsEmbTy {} ->
    throwUnsupportedExpression region OpaqueEmbTy
  HsHole {} ->
    throwUnsupportedExpression region OpaqueHole
  HsForAll {} ->
    throwUnsupportedExpression region OpaqueForAll
  HsQual {} ->
    throwUnsupportedExpression region OpaqueQual
  HsFunArr {} ->
    throwUnsupportedExpression region OpaqueFunArr

convertLocatedExpr :: Env -> LHsExpr GhcPs -> ConvM ConvExpr
convertLocatedExpr env locatedExpr =
  convertExpr env (sourceRegionFromSrcSpan (getLocA locatedExpr)) (unLoc locatedExpr)

convertBindingWithPattern :: Env -> HsPatF -> HsBind GhcPs -> ConvM ConvertedBinding
convertBindingWithPattern env headPattern = \case
  FunBind {fun_matches = matchGroupValue} ->
    case headPattern of
      PVarP binderAnn -> do
        (clauses, expressionValue) <-
          convertSourceMatchGroup env Nothing matchGroupValue
        pure
          ConvertedBinding
            { cbBinding = FunctionBinding binderAnn clauses,
              cbExpression = expressionValue
            }
      _ ->
        throwConvert
          (ConvertUnsupportedTopLevelBinding Nothing "non-variable function binding")
  PatBind {pat_rhs = grhssValue} -> do
    (rhsValue, expressionValue) <- convertSourceRhs env grhssValue
    pure
      ConvertedBinding
        { cbBinding = PatternBinding headPattern rhsValue,
          cbExpression = expressionValue
        }
  VarBind {var_rhs = rhsValue} -> do
    expressionValue <- convertLocatedExpr env rhsValue
    pure
      ConvertedBinding
        { cbBinding = PatternBinding headPattern (UnguardedRhs expressionValue Nothing),
          cbExpression = expressionValue
        }
  PatSynBind {} ->
    throwConvert (ConvertUnsupportedTopLevelBinding Nothing "PatSynBind")

convertSourceMatchGroup ::
  Env ->
  Maybe SourceRegion ->
  MatchGroup GhcPs (LHsExpr GhcPs) ->
  ConvM (NonEmpty Clause, Expr)
convertSourceMatchGroup env region = \case
  MG {mg_alts = alternativesValue} ->
    case unLoc alternativesValue of
      [matchValue]
        | Just _ <- simpleLambdaBinderNames (unLoc matchValue) -> do
            (clauseValue, expressionValue) <-
              convertSimpleSourceClause env region (unLoc matchValue)
            pure (clauseValue :| [], expressionValue)
      matchValues -> do
        convertedClauses <-
          traverse (convertSourceClause env . unLoc) matchValues
        case NonEmpty.nonEmpty convertedClauses of
          Nothing ->
            throwConvert
              (ConvertUnsupportedTopLevelBinding region "empty match group")
          Just clausePairs -> do
            expressionValue <-
              mkConvExpr
                region
                ( ClausesF
                    ( fmap
                        (\(clauseValue, rhsExpressionValue) -> (clausePatterns clauseValue, rhsExpressionValue))
                        (NonEmpty.toList clausePairs)
                    )
                )
            pure (fmap fst clausePairs, expressionValue)

convertSimpleSourceClause ::
  Env ->
  Maybe SourceRegion ->
  Match GhcPs (LHsExpr GhcPs) ->
  ConvM (Clause, Expr)
convertSimpleSourceClause env region matchValue =
  convertSimplePatterns env region (unLoc (m_pats matchValue))
  where
    convertSimplePatterns currentEnv currentRegion = \case
      [] -> do
        (rhsValue, expressionValue) <-
          convertSourceRhs currentEnv (m_grhss matchValue)
        pure (Clause [] rhsValue, expressionValue)
      patternValue : remainingPatterns -> do
        childScope <- freshChildScope
        (convertedPattern, binderAnn, remainingClause, bodyExpression) <-
          withScope childScope $ do
            convertedPattern <- convertPat patternValue
            case simplePatternBinderAnn convertedPattern of
              Nothing ->
                throwConvert
                  (ConvertUnsupportedPattern Nothing PatOpaqueExtension)
              Just binderAnn -> do
                recordLambdaSite binderAnn
                (remainingClause, bodyExpression) <-
                  convertSimplePatterns
                    (extendEnv currentEnv [binderAnn])
                    Nothing
                    remainingPatterns
                pure
                  ( convertedPattern,
                    binderAnn,
                    remainingClause,
                    bodyExpression
                  )
        lambdaExpression <-
          mkConvExpr currentRegion (LamF binderAnn bodyExpression)
        pure
          ( remainingClause
              { clausePatterns =
                  convertedPattern : clausePatterns remainingClause
              },
            lambdaExpression
          )

convertSourceClause ::
  Env ->
  Match GhcPs (LHsExpr GhcPs) ->
  ConvM (Clause, Expr)
convertSourceClause env matchValue = do
  let patternValues = unLoc (m_pats matchValue)
  binderNames <-
    concat <$> traverse collectResolvedPatternNames patternValues
  if null binderNames
    then do
      convertedPatterns <- traverse convertPat patternValues
      (rhsValue, expressionValue) <-
        convertSourceRhs env (m_grhss matchValue)
      pure (Clause convertedPatterns rhsValue, expressionValue)
    else do
      childScope <- freshChildScope
      withScope childScope $ do
        convertedPatterns <- traverse convertPat patternValues
        let extendedEnv =
              extendEnv env (concatMap patBinders convertedPatterns)
        (rhsValue, expressionValue) <-
          convertSourceRhs extendedEnv (m_grhss matchValue)
        pure (Clause convertedPatterns rhsValue, expressionValue)

convertSourceRhs ::
  Env ->
  GRHSs GhcPs (LHsExpr GhcPs) ->
  ConvM (Rhs, Expr)
convertSourceRhs env grhssValue = do
  let rhsRegion =
        sourceRegionFromSrcSpan
          (getLocA (NonEmpty.head (grhssGRHSs grhssValue)))
  maybeConvertedBinds <-
    convertLocalBinds rhsRegion env (grhssLocalBinds grhssValue)
  let rhsEnv = maybe env clbEnv maybeConvertedBinds
      maybeBindingGroup = clbGroup <$> maybeConvertedBinds
      convertAtBindingScope :: ConvM converted -> ConvM converted
      convertAtBindingScope =
        maybe id (withScope . clbScope) maybeConvertedBinds
  case grhssGRHSs grhssValue of
    locatedGrhs :| []
      | GRHS _ [] bodyValue <- unLoc locatedGrhs -> do
          bodyExpression <-
            convertAtBindingScope (convertLocatedExpr rhsEnv bodyValue)
          expressionValue <-
            attachBindingGroup maybeConvertedBinds bodyExpression
          pure
            ( UnguardedRhs bodyExpression maybeBindingGroup,
              expressionValue
            )
    grhsAlternatives -> do
      guardedAlternatives <-
        convertAtBindingScope
          (traverse (convertGuardedAlt rhsEnv) grhsAlternatives)
      case guardedAlternatives of
        GuardedAltF [] bodyExpression :| [] -> do
          expressionValue <-
            attachBindingGroup maybeConvertedBinds bodyExpression
          pure
            ( UnguardedRhs bodyExpression maybeBindingGroup,
              expressionValue
            )
        guardedValues -> do
          guardedExpression <-
            mkConvExpr rhsRegion (GuardedF (NonEmpty.toList guardedValues))
          expressionValue <-
            attachBindingGroup maybeConvertedBinds guardedExpression
          pure
            ( GuardedRhs guardedValues maybeBindingGroup,
              expressionValue
            )

convertLambdaLikeMatchGroup :: Env -> Maybe SourceRegion -> MatchGroup GhcPs (LHsExpr GhcPs) -> ConvM ConvExpr
convertLambdaLikeMatchGroup env region = \case
  MG {mg_alts = alternativesValue} ->
    case unLoc alternativesValue of
      [matchValue]
        | Just binderNames <- simpleLambdaBinderNames (unLoc matchValue) ->
            convertLambdaBinders env region binderNames (m_grhss (unLoc matchValue))
      matchValues ->
        convertClauses env region matchValues

convertClauses :: Env -> Maybe SourceRegion -> [LMatch GhcPs (LHsExpr GhcPs)] -> ConvM ConvExpr
convertClauses env region matchValues = do
  clauseValues <- traverse (convertClause env . unLoc) matchValues
  mkConvExpr region (ClausesF clauseValues)

convertClause :: Env -> Match GhcPs (LHsExpr GhcPs) -> ConvM ([HsPatF], ConvExpr)
convertClause env matchValue = do
  let patternValues = unLoc (m_pats matchValue)
  binderNames <-
    concat <$> traverse collectResolvedPatternNames patternValues
  if null binderNames
    then do
      clausePatterns <- traverse convertPat patternValues
      bodyExpr <- convertGRHSs env (m_grhss matchValue)
      pure (clausePatterns, bodyExpr)
    else do
      childScope <- freshChildScope
      withScope childScope $ do
        clausePatterns <- traverse convertPat patternValues
        let extendedEnv = extendEnv env (concatMap patBinders clausePatterns)
        bodyExpr <- convertGRHSs extendedEnv (m_grhss matchValue)
        pure (clausePatterns, bodyExpr)

convertLambdaBinders :: Env -> Maybe SourceRegion -> [RdrName] -> GRHSs GhcPs (LHsExpr GhcPs) -> ConvM ConvExpr
convertLambdaBinders env region binderNames grhssValue =
  case binderNames of
    [] ->
      convertGRHSs env grhssValue
    binderName : remainingNames -> do
      childScope <- freshChildScope
      (binderAnn, bodyExpr) <-
        withScope childScope $ do
          binderAnn <- freshBinderAnn binderName
          recordLambdaSite binderAnn
          bodyExpr <-
            convertLambdaBinders
              (extendEnv env [binderAnn])
              Nothing
              remainingNames
              grhssValue
          pure (binderAnn, bodyExpr)
      mkConvExpr region (LamF binderAnn bodyExpr)

convertCaseAlternatives :: Env -> MatchGroup GhcPs (LHsExpr GhcPs) -> ConvM [(HsPatF, ConvExpr)]
convertCaseAlternatives env = \case
  MG {mg_alts = alternativesValue} ->
    traverse (convertCaseAlternative env . unLoc) (unLoc alternativesValue)

convertCaseAlternative :: Env -> Match GhcPs (LHsExpr GhcPs) -> ConvM (HsPatF, ConvExpr)
convertCaseAlternative env matchValue =
  case unLoc (m_pats matchValue) of
    [patternValue] -> do
      childScope <- freshChildScope
      withScope childScope $ do
        casePattern <- convertPat patternValue
        let extendedEnv = extendEnv env (patBinders casePattern)
        rhsExpr <- convertGRHSs extendedEnv (m_grhss matchValue)
        pure (casePattern, rhsExpr)
    _ ->
      throwUnsupportedExpression Nothing OpaqueCaseAlternative

convertGRHSs :: Env -> GRHSs GhcPs (LHsExpr GhcPs) -> ConvM ConvExpr
convertGRHSs env =
  fmap snd . convertSourceRhs env

convertGuardedAlt :: Env -> LGRHS GhcPs (LHsExpr GhcPs) -> ConvM (GuardedAltF ConvExpr)
convertGuardedAlt env grhsValue =
  case unLoc grhsValue of
    GRHS _ guardValues bodyValue -> do
      (guardStatements, bodyExpr) <-
        convertGuardedAltBody env guardValues bodyValue
      pure
        GuardedAltF
          { gaGuards = guardStatements,
            gaBody = bodyExpr
          }

convertGuardedAltBody ::
  Env ->
  [GuardLStmt GhcPs] ->
  LHsExpr GhcPs ->
  ConvM ([HsGuardStmtF ConvExpr], ConvExpr)
convertGuardedAltBody env guardValues bodyValue =
  case guardValues of
    [] -> do
      bodyExpr <- convertLocatedExpr env bodyValue
      pure ([], bodyExpr)
    guardValue : remainingValues ->
      case unLoc guardValue of
        BodyStmt _ exprValue _ _ -> do
          guardExpr <- convertLocatedExpr env exprValue
          prependGuardStatement (GuardBoolF guardExpr)
            <$> convertGuardedAltBody env remainingValues bodyValue
        LastStmt _ exprValue _ _ -> do
          guardExpr <- convertLocatedExpr env exprValue
          prependGuardStatement (GuardBoolF guardExpr)
            <$> convertGuardedAltBody env remainingValues bodyValue
        BindStmt _ patternValue rhsValue -> do
          rhsExpr <- convertLocatedExpr env rhsValue
          binderNames <- collectResolvedPatternNames patternValue
          if null binderNames
            then do
              bindPattern <- convertPat patternValue
              prependGuardStatement (GuardPatF bindPattern rhsExpr)
                <$> convertGuardedAltBody env remainingValues bodyValue
            else do
              childScope <- freshChildScope
              withScope childScope $ do
                bindPattern <- convertPat patternValue
                let extendedEnv = extendEnv env (patBinders bindPattern)
                prependGuardStatement (GuardPatF bindPattern rhsExpr)
                  <$> convertGuardedAltBody extendedEnv remainingValues bodyValue
        LetStmt _ localBindsValue ->
          convertLocalBinds
            (sourceRegionFromSrcSpan (getLocA guardValue))
            env
            localBindsValue
            >>= \case
            Nothing ->
              throwUnsupportedExpression
                (sourceRegionFromSrcSpan (getLocA guardValue))
                OpaqueEmptyLocalBinds
            Just convertedBinds ->
              prependGuardStatement
                (GuardLetF (clbRecursion convertedBinds) (clbBindings convertedBinds))
                <$> withScope
                  (clbScope convertedBinds)
                  (convertGuardedAltBody (clbEnv convertedBinds) remainingValues bodyValue)
        ParStmt {} ->
          throwUnsupportedExpression
            (sourceRegionFromSrcSpan (getLocA guardValue))
            OpaqueParallelStatement
        TransStmt {} ->
          throwUnsupportedExpression
            (sourceRegionFromSrcSpan (getLocA guardValue))
            OpaqueTransformStatement
        RecStmt {} ->
          throwUnsupportedExpression
            (sourceRegionFromSrcSpan (getLocA guardValue))
            OpaqueRecursiveStatement

prependGuardStatement ::
  HsGuardStmtF ConvExpr ->
  ([HsGuardStmtF ConvExpr], ConvExpr) ->
  ([HsGuardStmtF ConvExpr], ConvExpr)
prependGuardStatement guardStatement (guardStatements, bodyExpr) =
  (guardStatement : guardStatements, bodyExpr)

convertStatements :: Env -> [ExprLStmt GhcPs] -> ConvM [HsStmtF ConvExpr]
convertStatements env = \case
  [] ->
    pure []
  statementValue : remainingValues ->
    case unLoc statementValue of
      BindStmt _ patternValue rhsValue -> do
        rhsExpr <- convertLocatedExpr env rhsValue
        binderNames <- collectResolvedPatternNames patternValue
        if null binderNames
          then do
            bindPattern <- convertPat patternValue
            (BindStmtF bindPattern rhsExpr :)
              <$> convertStatements env remainingValues
          else do
            childScope <- freshChildScope
            withScope childScope $ do
              bindPattern <- convertPat patternValue
              let extendedEnv = extendEnv env (patBinders bindPattern)
              (BindStmtF bindPattern rhsExpr :)
                <$> convertStatements extendedEnv remainingValues
      BodyStmt _ exprValue _ _ -> do
        bodyExpr <- convertLocatedExpr env exprValue
        (BodyStmtF bodyExpr :) <$> convertStatements env remainingValues
      LastStmt _ exprValue _ _ -> do
        bodyExpr <- convertLocatedExpr env exprValue
        pure [BodyStmtF bodyExpr]
      LetStmt _ localBindsValue ->
        convertLocalBinds
          (sourceRegionFromSrcSpan (getLocA statementValue))
          env
          localBindsValue
          >>= \case
          Nothing ->
            throwUnsupportedExpression
              (sourceRegionFromSrcSpan (getLocA statementValue))
              OpaqueEmptyLocalBinds
          Just convertedBinds ->
            (LetStmtF (clbRecursion convertedBinds) (clbBindings convertedBinds) :)
              <$> withScope
                (clbScope convertedBinds)
                (convertStatements (clbEnv convertedBinds) remainingValues)
      ParStmt {} ->
        throwUnsupportedExpression
          (sourceRegionFromSrcSpan (getLocA statementValue))
          OpaqueParallelStatement
      TransStmt {} ->
        throwUnsupportedExpression
          (sourceRegionFromSrcSpan (getLocA statementValue))
          OpaqueTransformStatement
      RecStmt {} ->
        throwUnsupportedExpression
          (sourceRegionFromSrcSpan (getLocA statementValue))
          OpaqueRecursiveStatement

convertLocalBinds ::
  Maybe SourceRegion ->
  Env ->
  HsLocalBinds GhcPs ->
  ConvM (Maybe ConvertedLocalBinds)
convertLocalBinds region env = \case
  EmptyLocalBinds _ ->
    pure Nothing
  HsValBinds _ valBindsValue ->
    convertValBinds region env valBindsValue
  HsIPBinds {} ->
    throwUnsupportedExpression region OpaqueImplicitParameterBinds

convertValBinds ::
  Maybe SourceRegion ->
  Env ->
  HsValBindsLR GhcPs GhcPs ->
  ConvM (Maybe ConvertedLocalBinds)
convertValBinds region env = \case
  ValBinds _ bindsValue _ -> do
    case NonEmpty.nonEmpty bindsValue of
      Nothing ->
        pure Nothing
      Just (locatedBindValue :| []) -> do
        childScope <- freshChildScope
        withScope childScope $ do
          bindingPatternValue <- localBindPattern locatedBindValue
          let binders = patBinders bindingPatternValue
              extendedEnv = extendEnv env binders
          convertedBinding <-
            convertBindingWithPattern extendedEnv bindingPatternValue (unLoc locatedBindValue)
          let referencesOwnBinder =
                freeScopeSummaryContains
                  childScope
                  (exprFreeScopes (cbExpression convertedBinding))
              bindingComponents =
                Dependencies.singletonBindingComponent bindingPatternValue referencesOwnBinder
              bindingGroup =
                BindingGroup childScope bindingComponents (cbBinding convertedBinding :| [])
              letRecursionValue =
                Dependencies.bindingComponentsRecursion bindingComponents
          case bindingPatternValue of
            PVarP binderAnn
              | letRecursionValue == NonRecursiveBinds ->
                  recordLetSite binderAnn
            _ ->
              pure ()
          pure
            ( Just
                ConvertedLocalBinds
                  { clbRecursion = letRecursionValue,
                    clbScope = childScope,
                    clbGroup = bindingGroup,
                    clbBindings = [(bindingPatternValue, cbExpression convertedBinding)],
                    clbBinders = binders,
                    clbEnv = extendedEnv
                  }
            )
      Just nonEmptyLocatedBindValues -> do
        childScope <- freshChildScope
        withScope childScope $ do
          bindingGroupId <- freshBindingGroupId
          bindPatterns <-
            traverse localBindPattern nonEmptyLocatedBindValues
          registerBindingOwners bindingGroupId bindPatterns
          let binders =
                foldMap patBinders bindPatterns
              extendedEnv =
                extendEnv env binders
              indexedBindings =
                NonEmpty.zip
                  (0 :| [1 ..])
                  (NonEmpty.zip bindPatterns (fmap unLoc nonEmptyLocatedBindValues))
          convertedBindings <-
            traverse
              ( \(rowIndex, (bindingPatternValue, bindingValue)) ->
                  withActiveBindingRow bindingGroupId rowIndex
                    (convertBindingWithPattern extendedEnv bindingPatternValue bindingValue)
              )
              indexedBindings
          dependenciesByRow <-
            takeBindingDependencies bindingGroupId binders
          let bindingRowsNonEmpty =
                convertedBindingRows convertedBindings
              bindingRows =
                NonEmpty.toList bindingRowsNonEmpty
          bindingGroup <-
            either
              (throwConvert . ConvertBindingDependencyFailure)
              pure
              (mkBindingGroup childScope convertedBindings dependenciesByRow)
          let letRecursionValue =
                Dependencies.bindingComponentsRecursion (bindingGroupComponents bindingGroup)
          case bindingRows of
            [(PVarP binderAnn, _)]
              | letRecursionValue == NonRecursiveBinds ->
                  recordLetSite binderAnn
            _ ->
              pure ()
          pure
            ( Just
                ConvertedLocalBinds
                  { clbRecursion = letRecursionValue,
                    clbScope = childScope,
                    clbGroup = bindingGroup,
                    clbBindings = bindingRows,
                    clbBinders = binders,
                    clbEnv = extendedEnv
                  }
            )
  XValBindsLR _ ->
    throwUnsupportedExpression region OpaqueExtensionValBinds

convertTupleArg :: Env -> HsTupArg GhcPs -> ConvM (TupleSlot ConvExpr)
convertTupleArg env = \case
  Present _ exprValue ->
    TuplePresent <$> convertLocatedExpr env exprValue
  Missing _ ->
    pure TupleMissing

flattenOpChain ::
  LHsExpr GhcPs ->
  LHsExpr GhcPs ->
  LHsExpr GhcPs ->
  (LHsExpr GhcPs, NonEmpty (LHsExpr GhcPs, LHsExpr GhcPs))
flattenOpChain leftValue operatorValue rightValue =
  let (firstOperand, leftTail) = flattenLocatedOpChain leftValue
      (rightHead, rightTail) = flattenLocatedOpChain rightValue
      finalPair = (operatorValue, rightHead)
   in case leftTail of
        [] ->
          (firstOperand, finalPair :| rightTail)
        firstPair : remainingPairs ->
          (firstOperand, firstPair :| (remainingPairs <> (finalPair : rightTail)))

flattenLocatedOpChain ::
  LHsExpr GhcPs ->
  (LHsExpr GhcPs, [(LHsExpr GhcPs, LHsExpr GhcPs)])
flattenLocatedOpChain locatedExpr =
  case unLoc locatedExpr of
    OpApp _ leftValue operatorValue rightValue ->
      let (firstOperand, leftTail) = flattenLocatedOpChain leftValue
          (rightHead, rightTail) = flattenLocatedOpChain rightValue
       in (firstOperand, leftTail <> ((operatorValue, rightHead) : rightTail))
    _ ->
      (locatedExpr, [])

convertRecordFields :: Env -> HsRecFields GhcPs (LHsExpr GhcPs) -> ConvM [(NormalizedFieldLabel, ConvExpr)]
convertRecordFields env recordFieldsValue =
  traverse
    (convertRecordField env . unLoc)
    (rec_flds recordFieldsValue)

convertRecordUpdFields ::
  Maybe SourceRegion ->
  Env ->
  LHsRecUpdFields GhcPs ->
  ConvM [(NormalizedFieldLabel, ConvExpr)]
convertRecordUpdFields region env = \case
  RegularRecUpdFields {recUpdFields = recordFieldsValue} ->
    traverse
      (convertRecordField env . unLoc)
      recordFieldsValue
  OverloadedRecUpdFields {} ->
    throwUnsupportedExpression region OpaqueOverloadedRecordUpdate

convertRecordField :: Env -> HsRecField GhcPs (LHsExpr GhcPs) -> ConvM (NormalizedFieldLabel, ConvExpr)
convertRecordField env fieldBindValue =
  case unLoc (hfbLHS fieldBindValue) of
    FieldOcc {foLabel = labelValue} -> do
      fieldExpr <-
        if hfbPun fieldBindValue
          then do
            variableReference <- resolveVarRef env (unLoc labelValue)
            mkConvExpr Nothing (VarF variableReference)
          else convertLocatedExpr env (hfbRHS fieldBindValue)
      pure
        ( normalizeFieldOcc (unLoc labelValue),
          fieldExpr
        )

convertArithSeq :: Env -> ArithSeqInfo GhcPs -> ConvM (NormalizedArithSeq ConvExpr)
convertArithSeq env = \case
  From fromValue ->
    ArithSeqFrom <$> convertLocatedExpr env fromValue
  FromThen fromValue thenValue ->
    ArithSeqFromThen
      <$> convertLocatedExpr env fromValue
      <*> convertLocatedExpr env thenValue
  FromTo fromValue toValue ->
    ArithSeqFromTo
      <$> convertLocatedExpr env fromValue
      <*> convertLocatedExpr env toValue
  FromThenTo fromValue thenValue toValue ->
    ArithSeqFromThenTo
      <$> convertLocatedExpr env fromValue
      <*> convertLocatedExpr env thenValue
      <*> convertLocatedExpr env toValue

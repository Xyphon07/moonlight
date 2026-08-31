{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.FreeScopes
  ( ScopeAlgebra (..),
    freeScopesExpr,
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax

type ScopeAlgebra :: Type -> Type
data ScopeAlgebra failure = ScopeAlgebra
  { saScopeDepth :: ScopeId -> Either failure Int,
    saBinderIntro :: BinderAnn -> Either failure ScopeId
  }

mergeScopeSummary :: ScopeAlgebra failure -> FreeScopeSummary -> FreeScopeSummary -> Either failure FreeScopeSummary
mergeScopeSummary scopeAlgebra =
  mergeFreeScopeSummaryByEither (saScopeDepth scopeAlgebra)

mergeScopeSummaries :: ScopeAlgebra failure -> [FreeScopeSummary] -> Either failure FreeScopeSummary
mergeScopeSummaries scopeAlgebra =
  foldM (mergeScopeSummary scopeAlgebra) emptyFreeScopeSummary

deleteBinderScope :: ScopeAlgebra failure -> BinderAnn -> FreeScopeSummary -> Either failure FreeScopeSummary
deleteBinderScope scopeAlgebra binderAnn summaryValue = do
  binderScope <- saBinderIntro scopeAlgebra binderAnn
  pure (deleteFreeScopeSummary binderScope summaryValue)

deletePatBinderScopes :: ScopeAlgebra failure -> HsPatF -> FreeScopeSummary -> Either failure FreeScopeSummary
deletePatBinderScopes scopeAlgebra patternValue summaryValue =
  foldM (\acc binderAnn -> deleteBinderScope scopeAlgebra binderAnn acc) summaryValue (patBinders patternValue)

freeScopesExpr :: ScopeAlgebra failure -> HsExprF Expr -> Either failure FreeScopeSummary
freeScopesExpr scopeAlgebra nodeValue =
  case nodeValue of
    VarF (GlobalName _) ->
      pure emptyFreeScopeSummary
    VarF (LocalName binderAnn) ->
      singletonFreeScopeSummary <$> saBinderIntro scopeAlgebra binderAnn
    LamF binderAnn bodyExpr ->
      deleteBinderScope scopeAlgebra binderAnn (exprFreeScopes bodyExpr)
    LetF letRecursion bindingValues bodyExpr ->
      freeScopesLet scopeAlgebra letRecursion bindingValues (exprFreeScopes bodyExpr)
    CaseF scrutineeExpr branchValues -> do
      branchFree <- traverse (freeScopesCaseAlternative scopeAlgebra) branchValues >>= mergeScopeSummaries scopeAlgebra
      mergeScopeSummary scopeAlgebra (exprFreeScopes scrutineeExpr) branchFree
    DoF statementValues ->
      freeScopesDo scopeAlgebra statementValues
    GuardedF guardedAlts ->
      traverse (freeScopesGuardedAlt scopeAlgebra) guardedAlts >>= mergeScopeSummaries scopeAlgebra
    MultiIfF guardedAlts ->
      traverse (freeScopesGuardedAlt scopeAlgebra) guardedAlts >>= mergeScopeSummaries scopeAlgebra
    ClausesF clauseValues ->
      traverse (freeScopesClause scopeAlgebra) clauseValues >>= mergeScopeSummaries scopeAlgebra
    _ ->
      foldM
        (\acc childExpr -> mergeScopeSummary scopeAlgebra acc (exprFreeScopes childExpr))
        emptyFreeScopeSummary
        nodeValue

freeScopesLet ::
  ScopeAlgebra failure ->
  LetRecursion ->
  [(HsPatF, Expr)] ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
freeScopesLet scopeAlgebra letRecursion bindingValues bodyFree0 = do
  bodyFree <-
    foldM
      (\acc (rowPattern, _) -> deletePatBinderScopes scopeAlgebra rowPattern acc)
      bodyFree0
      bindingValues
  rhsFree <-
    case letRecursion of
      NonRecursiveBinds ->
        foldM
          (\acc (_, rhsExpr) -> mergeScopeSummary scopeAlgebra acc (exprFreeScopes rhsExpr))
          emptyFreeScopeSummary
          bindingValues
      AcyclicDependentBinds ->
        freeScopesMutuallyVisibleBindings scopeAlgebra bindingValues
      RecursiveBinds ->
        freeScopesMutuallyVisibleBindings scopeAlgebra bindingValues
  mergeScopeSummary scopeAlgebra rhsFree bodyFree

freeScopesMutuallyVisibleBindings ::
  ScopeAlgebra failure ->
  [(HsPatF, Expr)] ->
  Either failure FreeScopeSummary
freeScopesMutuallyVisibleBindings scopeAlgebra bindingValues = do
  aggregateRhsFree <-
    mergeScopeSummaries
      scopeAlgebra
      (fmap (exprFreeScopes . snd) bindingValues)
  foldM
    (\acc (rowPattern, _) -> deletePatBinderScopes scopeAlgebra rowPattern acc)
    aggregateRhsFree
    bindingValues

freeScopesCaseAlternative :: ScopeAlgebra failure -> (HsPatF, Expr) -> Either failure FreeScopeSummary
freeScopesCaseAlternative scopeAlgebra (casePattern, branchExpr) =
  deletePatBinderScopes scopeAlgebra casePattern (exprFreeScopes branchExpr)

freeScopesClause :: ScopeAlgebra failure -> ([HsPatF], Expr) -> Either failure FreeScopeSummary
freeScopesClause scopeAlgebra (clausePatterns, bodyExpr) =
  foldM (flip (deletePatBinderScopes scopeAlgebra)) (exprFreeScopes bodyExpr) clausePatterns

freeScopesDo :: ScopeAlgebra failure -> [HsStmtF Expr] -> Either failure FreeScopeSummary
freeScopesDo scopeAlgebra = \case
  [] ->
    pure emptyFreeScopeSummary
  statementValue : remainingValues -> do
    laterFree <- freeScopesDo scopeAlgebra remainingValues
    freeScopesStmt scopeAlgebra statementValue laterFree

freeScopesStmt :: ScopeAlgebra failure -> HsStmtF Expr -> FreeScopeSummary -> Either failure FreeScopeSummary
freeScopesStmt scopeAlgebra statementValue laterFree =
  case statementValue of
    BindStmtF bindPattern rhsExpr -> do
      visibleLaterFree <- deletePatBinderScopes scopeAlgebra bindPattern laterFree
      mergeScopeSummary scopeAlgebra (exprFreeScopes rhsExpr) visibleLaterFree
    BodyStmtF exprValue ->
      mergeScopeSummary scopeAlgebra (exprFreeScopes exprValue) laterFree
    LetStmtF letRecursion bindingValues ->
      freeScopesLet scopeAlgebra letRecursion bindingValues laterFree

freeScopesGuardedAlt :: ScopeAlgebra failure -> GuardedAltF Expr -> Either failure FreeScopeSummary
freeScopesGuardedAlt scopeAlgebra guardedAlt =
  freeScopesGuardStmts scopeAlgebra (gaGuards guardedAlt) (exprFreeScopes (gaBody guardedAlt))

freeScopesGuardStmts ::
  ScopeAlgebra failure ->
  [HsGuardStmtF Expr] ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
freeScopesGuardStmts scopeAlgebra guardStatements bodyFree =
  case guardStatements of
    [] ->
      pure bodyFree
    guardStatement : remainingStatements -> do
      laterFree <- freeScopesGuardStmts scopeAlgebra remainingStatements bodyFree
      freeScopesGuardStmt scopeAlgebra guardStatement laterFree

freeScopesGuardStmt ::
  ScopeAlgebra failure ->
  HsGuardStmtF Expr ->
  FreeScopeSummary ->
  Either failure FreeScopeSummary
freeScopesGuardStmt scopeAlgebra guardStatement laterFree =
  case guardStatement of
    GuardBoolF exprValue ->
      mergeScopeSummary scopeAlgebra (exprFreeScopes exprValue) laterFree
    GuardPatF guardPattern rhsExpr -> do
      visibleLaterFree <- deletePatBinderScopes scopeAlgebra guardPattern laterFree
      mergeScopeSummary scopeAlgebra (exprFreeScopes rhsExpr) visibleLaterFree
    GuardLetF letRecursion bindingValues ->
      freeScopesLet scopeAlgebra letRecursion bindingValues laterFree

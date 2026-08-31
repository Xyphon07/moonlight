{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
  ( Binding (..),
    Clause (..),
    Rhs (..),
    BindingGroup,
    bindingGroupScope,
    bindingGroupComponents,
    bindingGroupBindings,
    BindingComponent,
    bindingComponentRows,
    bindingComponentBinders,
    bindingComponentDependencies,
    bindingComponentRecursion,
    BindingComponentRecursion (..),
    bindingExpr,
    bindingPattern,
    bindingNames,
    ConvertedBindingMetrics,
    convertedBindingScopedExprCount,
    convertedBindingGlobalVarRefCount,
    convertedBindingLocalVarRefCount,
    convertedBindingMaxFreeScopeCount,
    RecordFieldEnvironment,
    emptyRecordFieldEnvironment,
    recordFieldEnvironmentFromDefinitions,
    ConvertedValueBinding,
    tlbBinding,
    tlbScope,
    tlbRegion,
    convertedValueBindingMetrics,
    ConvertedInstanceDeclaration (..),
    InstanceMethodSection (..),
    ConvertedBindingOrigin (..),
    ConvertedBindingSite (..),
    ModuleDeclaration (..),
    ConvertedModule (..),
    convertedModuleBindings,
    convertedModuleBindingSites,
    convertedModuleInstanceMethodObstructions,
    convertedModuleTypeSignatures,
    convertedModuleFixityDeclarations,
    UnsupportedDeclarationTag (..),
    InstanceMethodObstructionCause (..),
    InstanceMethodObstruction (..),
    RecordWildcardResolutionFailure (..),
    ConvertObstruction (..),
    recoverableInstanceMethodObstruction,
    convertHsExpr,
    convertModule,
    convertModuleWithRecordFieldEnvironment,
    convertHaskellSource,
    convertHaskellSourceWithRecordFieldEnvironment,
  )
where

import Control.Monad.State.Strict (evalStateT, runStateT)
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import GHC.Hs
  ( GhcPs,
    HsExpr (..),
    HsModule (..),
  )
import Moonlight.Core (Pattern (..))
import Moonlight.Pale.Ghc.Expr.Convert.Declaration
import Moonlight.Pale.Ghc.Expr.Convert.Expression
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction
import Moonlight.Pale.Ghc.Expr.Convert.Projection
import Moonlight.Pale.Ghc.Expr.Convert.Row
import Moonlight.Pale.Ghc.Expr.Convert.Source
import Moonlight.Pale.Ghc.Expr.Convert.State
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax
import Moonlight.Pale.Ghc.ModuleSurface (parseHsModule)

convertHsExpr :: HsExpr GhcPs -> Either ConvertObstruction (Pattern HsExprF)
convertHsExpr exprValue =
  eraseExpr <$> evalStateT (convertExpr Map.empty Nothing exprValue) initialConvState

convertModule :: String -> HsModule GhcPs -> Either ConvertObstruction ConvertedModule
convertModule =
  convertModuleWithRecordFieldEnvironment emptyRecordFieldEnvironment

convertModuleWithRecordFieldEnvironment ::
  RecordFieldEnvironment ->
  String ->
  HsModule GhcPs ->
  Either ConvertObstruction ConvertedModule
convertModuleWithRecordFieldEnvironment recordFieldEnvironment moduleContents moduleValue = do
  let moduleSourceIndex = sourceSliceIndex moduleContents
  (declarations, finalState) <-
    runStateT
      (traverse (convertLocatedDecl moduleSourceIndex) (hsmodDecls moduleValue))
      (initialConvStateWithRecordFieldEnvironment recordFieldEnvironment)
  let scopeParents = V.fromList (reverse (csScopeParentsRev finalState))
      binderIntro = V.fromList (reverse (csBinderIntroRev finalState))
  scopeIndex <-
    either
      (Left . ConvertScopeIndexFailure)
      Right
      (mkScopeIndex scopeParents binderIntro)
  pure
    ConvertedModule
      { cmDeclarations = V.fromList declarations,
        cmScopeIndex = scopeIndex,
        cmLambdaSites = reverse (csLambdaSites finalState),
        cmLetSites = reverse (csLetSites finalState)
      }

convertHaskellSource :: FilePath -> String -> Either ConvertObstruction ConvertedModule
convertHaskellSource =
  convertHaskellSourceWithRecordFieldEnvironment emptyRecordFieldEnvironment

convertHaskellSourceWithRecordFieldEnvironment ::
  RecordFieldEnvironment ->
  FilePath ->
  String ->
  Either ConvertObstruction ConvertedModule
convertHaskellSourceWithRecordFieldEnvironment recordFieldEnvironment sourcePath moduleContents =
  either
    (Left . ConvertParseFailure)
    (convertModuleWithRecordFieldEnvironment recordFieldEnvironment moduleContents)
    (parseHsModule sourcePath moduleContents)

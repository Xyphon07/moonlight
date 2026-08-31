module Moonlight.Pale.Ghc.Expr.Convert.Metrics
  ( ConvertedModuleMetrics (..),
    convertedModuleMetrics,
  )
where

import Data.Kind (Type)
import Moonlight.Pale.Ghc.Expr.Convert.Coalgebra
  ( ConvertedBindingMetrics,
    ConvertedInstanceDeclaration (..),
    ConvertedModule (..),
    InstanceMethodSection (..),
    ModuleDeclaration (..),
    convertedBindingGlobalVarRefCount,
    convertedBindingLocalVarRefCount,
    convertedBindingMaxFreeScopeCount,
    convertedBindingScopedExprCount,
    convertedValueBindingMetrics,
  )
import Moonlight.Pale.Ghc.Expr.Scope
  ( scopeObservedCount,
  )

type ConvertedModuleMetrics :: Type
data ConvertedModuleMetrics = ConvertedModuleMetrics
  { cmmBindingCount :: !Int,
    cmmInstanceDeclarationCount :: !Int,
    cmmTraversableInstanceMethodCount :: !Int,
    cmmObstructedInstanceMethodCount :: !Int,
    cmmObservedContextCount :: !Int,
    cmmLambdaSiteCount :: !Int,
    cmmLetSiteCount :: !Int,
    cmmScopedExprCount :: !Int,
    cmmGlobalVarRefCount :: !Int,
    cmmLocalVarRefCount :: !Int,
    cmmMaxFreeScopeCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

type ModuleMetricSection :: Type
data ModuleMetricSection = ModuleMetricSection
  { moduleMetricBindingCount :: !Int,
    moduleMetricInstanceDeclarationCount :: !Int,
    moduleMetricTraversableInstanceMethodCount :: !Int,
    moduleMetricObstructedInstanceMethodCount :: !Int,
    moduleMetricExpressionSection :: !ConvertedBindingMetrics
  }

instance Semigroup ModuleMetricSection where
  leftSection <> rightSection =
    ModuleMetricSection
      { moduleMetricBindingCount =
          moduleMetricBindingCount leftSection
            + moduleMetricBindingCount rightSection,
        moduleMetricInstanceDeclarationCount =
          moduleMetricInstanceDeclarationCount leftSection
            + moduleMetricInstanceDeclarationCount rightSection,
        moduleMetricTraversableInstanceMethodCount =
          moduleMetricTraversableInstanceMethodCount leftSection
            + moduleMetricTraversableInstanceMethodCount rightSection,
        moduleMetricObstructedInstanceMethodCount =
          moduleMetricObstructedInstanceMethodCount leftSection
            + moduleMetricObstructedInstanceMethodCount rightSection,
        moduleMetricExpressionSection =
          moduleMetricExpressionSection leftSection
            <> moduleMetricExpressionSection rightSection
      }

instance Monoid ModuleMetricSection where
  mempty =
    ModuleMetricSection
      { moduleMetricBindingCount = 0,
        moduleMetricInstanceDeclarationCount = 0,
        moduleMetricTraversableInstanceMethodCount = 0,
        moduleMetricObstructedInstanceMethodCount = 0,
        moduleMetricExpressionSection = mempty
      }

convertedModuleMetrics ::
  ConvertedModule ->
  ConvertedModuleMetrics
convertedModuleMetrics convertedModule =
  let scopeIndex = cmScopeIndex convertedModule
      metricSection =
        foldMap declarationMetricSection (cmDeclarations convertedModule)
      expressionMetrics =
        moduleMetricExpressionSection metricSection
   in
    ConvertedModuleMetrics
      { cmmBindingCount = moduleMetricBindingCount metricSection,
        cmmInstanceDeclarationCount =
          moduleMetricInstanceDeclarationCount metricSection,
        cmmTraversableInstanceMethodCount =
          moduleMetricTraversableInstanceMethodCount metricSection,
        cmmObstructedInstanceMethodCount =
          moduleMetricObstructedInstanceMethodCount metricSection,
        cmmObservedContextCount = scopeObservedCount scopeIndex,
        cmmLambdaSiteCount = length (cmLambdaSites convertedModule),
        cmmLetSiteCount = length (cmLetSites convertedModule),
        cmmScopedExprCount =
          convertedBindingScopedExprCount expressionMetrics,
        cmmGlobalVarRefCount =
          convertedBindingGlobalVarRefCount expressionMetrics,
        cmmLocalVarRefCount =
          convertedBindingLocalVarRefCount expressionMetrics,
        cmmMaxFreeScopeCount =
          convertedBindingMaxFreeScopeCount expressionMetrics
      }

declarationMetricSection ::
  ModuleDeclaration ->
  ModuleMetricSection
declarationMetricSection = \case
  ValueDeclaration bindingValue ->
    mempty
      { moduleMetricBindingCount = 1,
        moduleMetricExpressionSection =
          convertedValueBindingMetrics bindingValue
      }
  InstanceDeclarationNode instanceDeclaration ->
    mempty
      { moduleMetricInstanceDeclarationCount = 1
      }
      <> foldMap
        instanceMethodMetricSection
        (convertedInstanceMethods instanceDeclaration)
  TypeSignatureDeclaration _ ->
    mempty
  FixityDeclarationNode _ ->
    mempty
  OpaqueDeclaration {} ->
    mempty

instanceMethodMetricSection ::
  InstanceMethodSection ->
  ModuleMetricSection
instanceMethodMetricSection = \case
  TraversableInstanceMethod bindingValue ->
    mempty
      { moduleMetricTraversableInstanceMethodCount = 1,
        moduleMetricExpressionSection =
          convertedValueBindingMetrics bindingValue
      }
  ObstructedInstanceMethod _ ->
    mempty
      { moduleMetricObstructedInstanceMethodCount = 1
      }

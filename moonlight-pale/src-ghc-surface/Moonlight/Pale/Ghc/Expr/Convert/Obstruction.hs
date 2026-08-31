module Moonlight.Pale.Ghc.Expr.Convert.Obstruction
  ( UnsupportedDeclarationTag (..),
    InstanceMethodObstructionCause (..),
    InstanceMethodObstruction (..),
    RecordWildcardResolutionFailure (..),
    ConvertObstruction (..),
    recoverableInstanceMethodObstruction,
  )
where

import Data.Kind (Type)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import Moonlight.Core (BinderId)
import Moonlight.Pale.Ghc.Expr.Convert.Dependencies
  ( BindingDependencyFailure,
  )
import Moonlight.Pale.Ghc.Expr.Scope
  ( ScopeId,
    ScopeIdFailure,
    ScopeIndexFailure,
  )
import Moonlight.Pale.Ghc.Expr.Syntax (SourceRegion)
import Moonlight.Pale.Ghc.Expr.Opaque (HsOpaqueTag, HsPatOpaqueTag)
import Moonlight.Pale.Ghc.ModuleSurface (GhcParseFailure)

type UnsupportedDeclarationTag :: Type
data UnsupportedDeclarationTag
  = UnsupportedTypeOrClassDeclaration
  | UnsupportedTypeFamilyInstanceDeclaration
  | UnsupportedDataFamilyInstanceDeclaration
  | UnsupportedDerivingDeclaration
  | UnsupportedKindSignatureDeclaration
  | UnsupportedDefaultDeclaration
  | UnsupportedForeignDeclaration
  | UnsupportedWarningDeclaration
  | UnsupportedAnnotationDeclaration
  | UnsupportedRuleDeclaration
  | UnsupportedSpliceDeclaration
  | UnsupportedDocumentationDeclaration
  | UnsupportedRoleAnnotationDeclaration
  | UnsupportedPatternSynonymSignature
  | UnsupportedClassOperationSignature
  | UnsupportedInlineSignature
  | UnsupportedSpecializationSignature
  | UnsupportedExpressionSpecializationSignature
  | UnsupportedInstanceSpecializationSignature
  | UnsupportedMinimalSignature
  | UnsupportedCostCentreSignature
  | UnsupportedCompleteMatchSignature
  deriving stock (Eq, Ord, Show, Enum, Bounded)

type InstanceMethodObstructionCause :: Type
data InstanceMethodObstructionCause
  = InstanceMethodUnsupportedBinding !(Maybe SourceRegion) !String
  | InstanceMethodUnsupportedExpression !(Maybe SourceRegion) !HsOpaqueTag
  | InstanceMethodUnsupportedPattern !(Maybe SourceRegion) !HsPatOpaqueTag
  deriving stock (Eq, Ord, Show)

type InstanceMethodObstruction :: Type
data InstanceMethodObstruction = InstanceMethodObstruction
  { instanceMethodObstructionRegion :: !(Maybe SourceRegion),
    instanceMethodObstructionCause :: !InstanceMethodObstructionCause
  }
  deriving stock (Eq, Ord, Show)

type RecordWildcardResolutionFailure :: Type
data RecordWildcardResolutionFailure
  = RecordWildcardConstructorUnavailable !RdrName
  | RecordWildcardConstructorAmbiguous !RdrName
  deriving stock (Eq, Ord)

instance Show RecordWildcardResolutionFailure where
  show = \case
    RecordWildcardConstructorUnavailable constructorName ->
      "RecordWildcardConstructorUnavailable "
        <> occNameString (rdrNameOcc constructorName)
    RecordWildcardConstructorAmbiguous constructorName ->
      "RecordWildcardConstructorAmbiguous "
        <> occNameString (rdrNameOcc constructorName)

type ConvertObstruction :: Type
data ConvertObstruction
  = ConvertParseFailure !GhcParseFailure
  | ConvertScopeIndexFailure !ScopeIndexFailure
  | ConvertFreshScopeIdFailure !Int !ScopeIdFailure
  | ConvertMissingScopeDepth !ScopeId
  | ConvertMissingBinderIntro !BinderId
  | ConvertMissingScopeSummaryDepth !ScopeId
  | ConvertBindingDependencyFailure !BindingDependencyFailure
  | ConvertUnsupportedTopLevelBinding !(Maybe SourceRegion) !String
  | ConvertDeclarationSourceUnavailable !(Maybe SourceRegion) !UnsupportedDeclarationTag
  | ConvertInstanceDeclarationSourceUnavailable !(Maybe SourceRegion)
  | ConvertRecordWildcardResolutionUnavailable !SourceRegion !RecordWildcardResolutionFailure
  | ConvertRecordWildcardPositionInvalid !SourceRegion !Int !Int
  | ConvertRecordWildcardRegionUnavailable !(Maybe SourceRegion)
  | ConvertEmptyTypeSignature !(Maybe SourceRegion)
  | ConvertEmptyFixityDeclaration !(Maybe SourceRegion)
  | ConvertUnsupportedExpression !(Maybe SourceRegion) !HsOpaqueTag
  | ConvertUnsupportedPattern !(Maybe SourceRegion) !HsPatOpaqueTag
  deriving stock (Eq, Ord, Show)

recoverableInstanceMethodObstruction ::
  Maybe SourceRegion ->
  ConvertObstruction ->
  Maybe InstanceMethodObstruction
recoverableInstanceMethodObstruction methodRegion = \case
  ConvertUnsupportedTopLevelBinding obstructionRegion bindingShape ->
    Just
      InstanceMethodObstruction
        { instanceMethodObstructionRegion = methodRegion,
          instanceMethodObstructionCause =
            InstanceMethodUnsupportedBinding obstructionRegion bindingShape
        }
  ConvertUnsupportedExpression obstructionRegion expressionTag ->
    Just
      InstanceMethodObstruction
        { instanceMethodObstructionRegion = methodRegion,
          instanceMethodObstructionCause =
            InstanceMethodUnsupportedExpression obstructionRegion expressionTag
        }
  ConvertUnsupportedPattern obstructionRegion patternTag ->
    Just
      InstanceMethodObstruction
        { instanceMethodObstructionRegion = methodRegion,
          instanceMethodObstructionCause =
            InstanceMethodUnsupportedPattern obstructionRegion patternTag
        }
  ConvertParseFailure {} ->
    Nothing
  ConvertScopeIndexFailure {} ->
    Nothing
  ConvertFreshScopeIdFailure {} ->
    Nothing
  ConvertMissingScopeDepth {} ->
    Nothing
  ConvertMissingBinderIntro {} ->
    Nothing
  ConvertMissingScopeSummaryDepth {} ->
    Nothing
  ConvertBindingDependencyFailure {} ->
    Nothing
  ConvertDeclarationSourceUnavailable {} ->
    Nothing
  ConvertInstanceDeclarationSourceUnavailable {} ->
    Nothing
  ConvertRecordWildcardResolutionUnavailable {} ->
    Nothing
  ConvertRecordWildcardPositionInvalid {} ->
    Nothing
  ConvertRecordWildcardRegionUnavailable {} ->
    Nothing
  ConvertEmptyTypeSignature {} ->
    Nothing
  ConvertEmptyFixityDeclaration {} ->
    Nothing

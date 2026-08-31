{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Moonlight.Pale.Ghc.Expr.Convert.Row
  ( RecordFieldEnvironment,
    emptyRecordFieldEnvironment,
    recordFieldEnvironmentFromDefinitions,
    resolveRecordFieldEnvironment,
    Env,
    ConvertedBindingMetrics (..),
    ConvertedValueBinding (..),
    tlbBinding,
    tlbScope,
    tlbRegion,
    convertedValueBindingMetrics,
    Binding (..),
    Clause (..),
    Rhs (..),
    BindingGroup (..),
    bindingNames,
    bindingPattern,
    clausePattern,
    rhsPattern,
    attachBindingGroupPattern,
    bindingRowPattern,
    clauseBodyScope,
    bindingHeadPattern,
    ConvertedModule (..),
    ConvertedInstanceDeclaration (..),
    InstanceMethodSection (..),
    ConvertedBindingOrigin (..),
    ConvertedBindingSite (..),
    ModuleDeclaration (..),
    convertedModuleBindings,
    convertedModuleBindingSites,
    convertedModuleInstanceMethodObstructions,
    convertedModuleTypeSignatures,
    convertedModuleFixityDeclarations,
    ConvertedLocalBinds (..),
    ConvExpr,
    ConvertedBinding (..),
    convertedBindingRows,
    mkBindingGroup,
    BindingGroupId (..),
    bindingGroupIdKey,
    simplePatternBinderAnn,
    extendEnv,
    convertedExpressionMetrics
  )
where

import Control.Monad (foldM)
import Data.Kind (Type)
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Types.Name.Reader (RdrName)
import Moonlight.Core (BinderId (..), Pattern (..))
import Moonlight.Pale.Ghc.Expr.Convert.Dependencies qualified as Dependencies
import Moonlight.Pale.Ghc.Expr.Convert.Obstruction
import Moonlight.Pale.Ghc.Expr.Scope
import Moonlight.Pale.Ghc.Expr.Syntax

type RecordFieldEnvironment :: Type
newtype RecordFieldEnvironment = RecordFieldEnvironment
  { recordFieldDefinitions :: Map RdrName (NonEmpty [RdrName])
  }
  deriving stock (Eq, Ord)

emptyRecordFieldEnvironment :: RecordFieldEnvironment
emptyRecordFieldEnvironment =
  RecordFieldEnvironment Map.empty

recordFieldEnvironmentFromDefinitions ::
  [(RdrName, [RdrName])] ->
  RecordFieldEnvironment
recordFieldEnvironmentFromDefinitions definitions =
  RecordFieldEnvironment
    ( Map.fromListWith
        (flip (<>))
        ( fmap
            (\(constructorName, fieldNames) -> (constructorName, fieldNames :| []))
            definitions
        )
    )

resolveRecordFieldEnvironment ::
  RecordFieldEnvironment ->
  RdrName ->
  Either RecordWildcardResolutionFailure [RdrName]
resolveRecordFieldEnvironment recordFieldEnvironment constructorName =
  case Map.lookup constructorName (recordFieldDefinitions recordFieldEnvironment) of
    Nothing ->
      Left (RecordWildcardConstructorUnavailable constructorName)
    Just (fieldNames :| []) ->
      Right fieldNames
    Just _ ->
      Left (RecordWildcardConstructorAmbiguous constructorName)

type Env :: Type
type Env = Map RdrName BinderAnn

type ConvertedBindingMetrics :: Type
data ConvertedBindingMetrics = ConvertedBindingMetrics
  { convertedBindingScopedExprCount :: !Int,
    convertedBindingGlobalVarRefCount :: !Int,
    convertedBindingLocalVarRefCount :: !Int,
    convertedBindingMaxFreeScopeCount :: !Int
  }
  deriving stock (Eq, Ord, Show)

instance Semigroup ConvertedBindingMetrics where
  leftMetrics <> rightMetrics =
    ConvertedBindingMetrics
      { convertedBindingScopedExprCount =
          convertedBindingScopedExprCount leftMetrics
            + convertedBindingScopedExprCount rightMetrics,
        convertedBindingGlobalVarRefCount =
          convertedBindingGlobalVarRefCount leftMetrics
            + convertedBindingGlobalVarRefCount rightMetrics,
        convertedBindingLocalVarRefCount =
          convertedBindingLocalVarRefCount leftMetrics
            + convertedBindingLocalVarRefCount rightMetrics,
        convertedBindingMaxFreeScopeCount =
          max
            (convertedBindingMaxFreeScopeCount leftMetrics)
            (convertedBindingMaxFreeScopeCount rightMetrics)
      }

instance Monoid ConvertedBindingMetrics where
  mempty =
    ConvertedBindingMetrics
      { convertedBindingScopedExprCount = 0,
        convertedBindingGlobalVarRefCount = 0,
        convertedBindingLocalVarRefCount = 0,
        convertedBindingMaxFreeScopeCount = 0
      }

type ConvertedValueBinding :: Type
data ConvertedValueBinding = ConvertedValueBinding
  { convertedValueBindingValue :: !Binding,
    convertedValueBindingScope :: !ScopeId,
    convertedValueBindingRegion :: !(Maybe SourceRegion),
    convertedValueBindingMetricSection :: !ConvertedBindingMetrics
  }
  deriving stock (Eq, Ord, Show)

tlbBinding :: ConvertedValueBinding -> Binding
tlbBinding =
  convertedValueBindingValue

tlbScope :: ConvertedValueBinding -> ScopeId
tlbScope =
  convertedValueBindingScope

tlbRegion :: ConvertedValueBinding -> Maybe SourceRegion
tlbRegion =
  convertedValueBindingRegion

convertedValueBindingMetrics :: ConvertedValueBinding -> ConvertedBindingMetrics
convertedValueBindingMetrics =
  convertedValueBindingMetricSection

type Binding :: Type
data Binding
  = FunctionBinding !BinderAnn !(NonEmpty Clause)
  | PatternBinding !HsPatF !Rhs
  deriving stock (Eq, Ord, Show)

type Clause :: Type
data Clause = Clause
  { clausePatterns :: ![HsPatF],
    clauseRhs :: !Rhs
  }
  deriving stock (Eq, Ord, Show)

type Rhs :: Type
data Rhs
  = UnguardedRhs !Expr !(Maybe BindingGroup)
  | GuardedRhs !(NonEmpty (GuardedAltF Expr)) !(Maybe BindingGroup)
  deriving stock (Eq, Ord, Show)

type BindingGroup :: Type
data BindingGroup = BindingGroup
  { bindingGroupScope :: !ScopeId,
    bindingGroupComponents :: !(NonEmpty BindingComponent),
    bindingGroupBindings :: !(NonEmpty Binding)
  }
  deriving stock (Eq, Ord, Show)

bindingNames :: Binding -> [RdrName]
bindingNames = \case
  FunctionBinding binderAnn _ -> [baName binderAnn]
  PatternBinding patternValue _ -> fmap baName (patBinders patternValue)

bindingPattern :: Binding -> Pattern HsExprF
bindingPattern = \case
  PatternBinding _ rhsValue ->
    rhsPattern rhsValue
  FunctionBinding _ clauses ->
    case clauses of
      Clause patterns rhsValue :| []
        | Just binderAnns <- traverse simplePatternBinderAnn patterns ->
            foldr
              (\binderAnn bodyPattern -> PatternNode (LamF binderAnn bodyPattern))
              (rhsPattern rhsValue)
              binderAnns
      clauseValues ->
        PatternNode
          ( ClausesF
              (fmap clausePattern (NonEmpty.toList clauseValues))
          )

clausePattern :: Clause -> ([HsPatF], Pattern HsExprF)
clausePattern clauseValue =
  ( clausePatterns clauseValue,
    rhsPattern (clauseRhs clauseValue)
  )

rhsPattern :: Rhs -> Pattern HsExprF
rhsPattern = \case
  UnguardedRhs bodyExpression maybeBindingGroup ->
    attachBindingGroupPattern maybeBindingGroup (eraseExpr bodyExpression)
  GuardedRhs guardedAlternatives maybeBindingGroup ->
    attachBindingGroupPattern
      maybeBindingGroup
      ( PatternNode
          (GuardedF (fmap (fmap eraseExpr) (NonEmpty.toList guardedAlternatives)))
      )

attachBindingGroupPattern ::
  Maybe BindingGroup ->
  Pattern HsExprF ->
  Pattern HsExprF
attachBindingGroupPattern Nothing bodyPattern =
  bodyPattern
attachBindingGroupPattern (Just bindingGroup) bodyPattern =
  PatternNode
    ( LetF
        (Dependencies.bindingComponentsRecursion (bindingGroupComponents bindingGroup))
        (fmap bindingRowPattern (NonEmpty.toList (bindingGroupBindings bindingGroup)))
        bodyPattern
    )

bindingRowPattern :: Binding -> (HsPatF, Pattern HsExprF)
bindingRowPattern bindingValue =
  (bindingHeadPattern bindingValue, bindingPattern bindingValue)

clauseBodyScope ::
  ScopeIndex ->
  ScopeId ->
  [HsPatF] ->
  Either ScopeLookupFailure ScopeId
clauseBodyScope scopeIndex bindingScope patternValues =
  foldM
    (\_ binderAnn -> binderIntroScope scopeIndex (baId binderAnn))
    bindingScope
    (foldMap patBinders patternValues)

bindingHeadPattern :: Binding -> HsPatF
bindingHeadPattern = \case
  FunctionBinding binderAnn _ ->
    PVarP binderAnn
  PatternBinding patternValue _ ->
    patternValue

type ConvertedModule :: Type
data ConvertedModule = ConvertedModule
  { cmDeclarations :: !(Vector ModuleDeclaration),
    cmScopeIndex :: !ScopeIndex,
    cmLambdaSites :: ![BinderAnn],
    cmLetSites :: ![BinderAnn]
  }

type ConvertedInstanceDeclaration :: Type
data ConvertedInstanceDeclaration = ConvertedInstanceDeclaration
  { convertedInstanceRegion :: !SourceRegion,
    convertedInstanceSource :: !String,
    convertedInstanceMethods :: ![InstanceMethodSection]
  }
  deriving stock (Eq, Ord, Show)

type InstanceMethodSection :: Type
data InstanceMethodSection
  = TraversableInstanceMethod !ConvertedValueBinding
  | ObstructedInstanceMethod !InstanceMethodObstruction
  deriving stock (Eq, Ord, Show)

type ConvertedBindingOrigin :: Type
data ConvertedBindingOrigin
  = TopLevelBindingOrigin
  | InstanceMethodBindingOrigin !SourceRegion
  deriving stock (Eq, Ord, Show)

type ConvertedBindingSite :: Type
data ConvertedBindingSite = ConvertedBindingSite
  { convertedBindingOrigin :: !ConvertedBindingOrigin,
    convertedBindingValue :: !ConvertedValueBinding
  }
  deriving stock (Eq, Ord, Show)

type ModuleDeclaration :: Type
data ModuleDeclaration
  = ValueDeclaration !ConvertedValueBinding
  | TypeSignatureDeclaration !TypeSignature
  | FixityDeclarationNode !FixityDeclaration
  | InstanceDeclarationNode !ConvertedInstanceDeclaration
  | OpaqueDeclaration !UnsupportedDeclarationTag !SourceRegion !String
  deriving stock (Eq, Ord, Show)

convertedModuleBindings :: ConvertedModule -> [ConvertedValueBinding]
convertedModuleBindings =
  foldMap
    ( \case
        ValueDeclaration bindingValue -> [bindingValue]
        TypeSignatureDeclaration _ -> []
        FixityDeclarationNode _ -> []
        InstanceDeclarationNode _ -> []
        OpaqueDeclaration {} -> []
    )
    . cmDeclarations

convertedModuleBindingSites :: ConvertedModule -> [ConvertedBindingSite]
convertedModuleBindingSites =
  foldMap
    ( \case
        ValueDeclaration bindingValue ->
          [ConvertedBindingSite TopLevelBindingOrigin bindingValue]
        TypeSignatureDeclaration _ ->
          []
        FixityDeclarationNode _ ->
          []
        InstanceDeclarationNode instanceDeclaration ->
          foldMap
            ( \case
                TraversableInstanceMethod bindingValue ->
                  [ ConvertedBindingSite
                      (InstanceMethodBindingOrigin (convertedInstanceRegion instanceDeclaration))
                      bindingValue
                  ]
                ObstructedInstanceMethod _ ->
                  []
            )
            (convertedInstanceMethods instanceDeclaration)
        OpaqueDeclaration {} ->
          []
    )
    . cmDeclarations

convertedModuleInstanceMethodObstructions ::
  ConvertedModule ->
  [InstanceMethodObstruction]
convertedModuleInstanceMethodObstructions =
  foldMap
    ( \case
        InstanceDeclarationNode instanceDeclaration ->
          foldMap
            ( \case
                TraversableInstanceMethod _ ->
                  []
                ObstructedInstanceMethod obstruction ->
                  [obstruction]
            )
            (convertedInstanceMethods instanceDeclaration)
        ValueDeclaration _ ->
          []
        TypeSignatureDeclaration _ ->
          []
        FixityDeclarationNode _ ->
          []
        OpaqueDeclaration {} ->
          []
    )
    . cmDeclarations

convertedModuleTypeSignatures :: ConvertedModule -> [TypeSignature]
convertedModuleTypeSignatures =
  foldMap
    ( \case
        TypeSignatureDeclaration signature -> [signature]
        ValueDeclaration _ -> []
        FixityDeclarationNode _ -> []
        InstanceDeclarationNode _ -> []
        OpaqueDeclaration {} -> []
    )
    . cmDeclarations

convertedModuleFixityDeclarations :: ConvertedModule -> [FixityDeclaration]
convertedModuleFixityDeclarations =
  foldMap
    ( \case
        FixityDeclarationNode declaration -> [declaration]
        ValueDeclaration _ -> []
        TypeSignatureDeclaration _ -> []
        InstanceDeclarationNode _ -> []
        OpaqueDeclaration {} -> []
    )
    . cmDeclarations

type ConvertedLocalBinds :: Type
data ConvertedLocalBinds = ConvertedLocalBinds
  { clbRecursion :: !LetRecursion,
    clbScope :: !ScopeId,
    clbGroup :: !BindingGroup,
    clbBindings :: ![(HsPatF, ConvExpr)],
    clbBinders :: ![BinderAnn],
    clbEnv :: !Env
  }

type ConvExpr :: Type
type ConvExpr = Expr

type ConvertedBinding :: Type
data ConvertedBinding = ConvertedBinding
  { cbBinding :: !Binding,
    cbExpression :: !Expr
  }

convertedBindingRows :: NonEmpty ConvertedBinding -> NonEmpty (HsPatF, Expr)
convertedBindingRows =
  fmap
    (\convertedBinding -> (bindingHeadPattern (cbBinding convertedBinding), cbExpression convertedBinding))

mkBindingGroup ::
  ScopeId ->
  NonEmpty ConvertedBinding ->
  IntMap (Set BinderId) ->
  Either Dependencies.BindingDependencyFailure BindingGroup
mkBindingGroup bindingScope convertedBindings dependenciesByRow =
  BindingGroup bindingScope
    <$> Dependencies.inferBindingComponents
      (fmap (bindingHeadPattern . cbBinding) convertedBindings)
      dependenciesByRow
    <*> pure (fmap cbBinding convertedBindings)

type BindingGroupId :: Type
newtype BindingGroupId = BindingGroupId Int
  deriving stock (Eq, Ord, Show)

bindingGroupIdKey :: BindingGroupId -> Int
bindingGroupIdKey (BindingGroupId groupKey) =
  groupKey

simplePatternBinderAnn :: HsPatF -> Maybe BinderAnn
simplePatternBinderAnn = \case
  PVarP binderAnn -> Just binderAnn
  PParP patternValue -> simplePatternBinderAnn patternValue
  PBangP patternValue -> simplePatternBinderAnn patternValue
  PLazyP patternValue -> simplePatternBinderAnn patternValue
  _ -> Nothing

extendEnv :: Env -> [BinderAnn] -> Env
extendEnv env binderAnns =
  foldr (\binderAnn -> Map.insert (baName binderAnn) binderAnn) env binderAnns

convertedExpressionMetrics :: Expr -> ConvertedBindingMetrics
convertedExpressionMetrics expressionValue =
  let nodeValue = exprNode expressionValue
      childMetrics = foldMap convertedExpressionMetrics nodeValue
      freeScopeCount = freeScopeSummarySize (exprFreeScopes expressionValue)
      (globalRefIncrement, localRefIncrement) =
        case nodeValue of
          VarF (GlobalName _) -> (1, 0)
          VarF (LocalName _) -> (0, 1)
          _ -> (0, 0)
   in ConvertedBindingMetrics
        { convertedBindingScopedExprCount =
            convertedBindingScopedExprCount childMetrics + 1,
          convertedBindingGlobalVarRefCount =
            convertedBindingGlobalVarRefCount childMetrics + globalRefIncrement,
          convertedBindingLocalVarRefCount =
            convertedBindingLocalVarRefCount childMetrics + localRefIncrement,
          convertedBindingMaxFreeScopeCount =
            max
              (convertedBindingMaxFreeScopeCount childMetrics)
              freeScopeCount
        }

{-# LANGUAGE LambdaCase #-}

module Expr.RenderRoundTripSpec
  ( tests,
  )
where

import Data.ByteString qualified as ByteString
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import GHC.Types.Name.Occurrence (mkDataOcc, mkVarOcc, occNameString)
import GHC.Types.Name.Reader (RdrName, mkRdrUnqual, rdrNameOcc)
import Moonlight.Core (BinderId (..), Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Pale.Ghc.Expr
import Moonlight.Pale.Test.Assertions (expectRightWithLabel)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "pale.expr"
    [ renderRoundTripTests,
      declarationOrderTests,
      typedUnsupportedSyntaxTests,
      spanLockstepTests,
      scopeIndexTests,
      caseAlternativeScopeTests
    ]

renderSourceString ::
  LayoutPolicy ->
  RenderTarget ->
  Either RenderRefusal String
renderSourceString layoutPolicy =
  fmap Text.unpack . renderSource layoutPolicy

renderFixtureModule ::
  String ->
  ConvertedModule ->
  Either RenderRefusal String
renderFixtureModule moduleName =
  renderSourceString
    CompactLayout
    . RenderConvertedModule (ModuleRenderContext "" (Just moduleName))

data DeclarationKind
  = ValueDeclarationKind
  | TypeSignatureDeclarationKind
  | FixityDeclarationKind
  | InstanceDeclarationKind
  | OpaqueDeclarationKind
  deriving stock (Eq, Show)

data ExpressionMetricOracle = ExpressionMetricOracle
  { oracleScopedExprCount :: !Int,
    oracleGlobalVarRefCount :: !Int,
    oracleLocalVarRefCount :: !Int,
    oracleMaxFreeScopeCount :: !Int
  }
  deriving stock (Eq, Show)

instance Semigroup ExpressionMetricOracle where
  leftMetrics <> rightMetrics =
    ExpressionMetricOracle
      { oracleScopedExprCount =
          oracleScopedExprCount leftMetrics + oracleScopedExprCount rightMetrics,
        oracleGlobalVarRefCount =
          oracleGlobalVarRefCount leftMetrics + oracleGlobalVarRefCount rightMetrics,
        oracleLocalVarRefCount =
          oracleLocalVarRefCount leftMetrics + oracleLocalVarRefCount rightMetrics,
        oracleMaxFreeScopeCount =
          max
            (oracleMaxFreeScopeCount leftMetrics)
            (oracleMaxFreeScopeCount rightMetrics)
      }

instance Monoid ExpressionMetricOracle where
  mempty =
    ExpressionMetricOracle
      { oracleScopedExprCount = 0,
        oracleGlobalVarRefCount = 0,
        oracleLocalVarRefCount = 0,
        oracleMaxFreeScopeCount = 0
      }

declarationKind :: ModuleDeclaration -> DeclarationKind
declarationKind = \case
  ValueDeclaration _ -> ValueDeclarationKind
  TypeSignatureDeclaration _ -> TypeSignatureDeclarationKind
  FixityDeclarationNode _ -> FixityDeclarationKind
  InstanceDeclarationNode _ -> InstanceDeclarationKind
  OpaqueDeclaration {} -> OpaqueDeclarationKind

declarationOrderTests :: TestTree
declarationOrderTests =
  testGroup
    "pale.declarations"
    [ testCase "conversion and rendering preserve supported declaration order" $ do
        let sourceText =
              unlines
                [ "module DeclarationOrder where",
                  "before :: Int -> Int",
                  "before value = value",
                  "infixr 5 <+>",
                  "(<+>) :: Int -> Int -> Int",
                  "left <+> right = left + right"
                ]
            expectedOrder =
              [ TypeSignatureDeclarationKind,
                ValueDeclarationKind,
                FixityDeclarationKind,
                TypeSignatureDeclarationKind,
                ValueDeclarationKind
              ]
        convertedModule <-
          expectRightWithLabel
            "ordered declaration conversion"
            (convertHaskellSource "DeclarationOrder.hs" sourceText)
        fmap declarationKind (Vector.toList (cmDeclarations convertedModule))
          @?= expectedOrder
        length (convertedModuleBindings convertedModule) @?= 2
        length (convertedModuleTypeSignatures convertedModule) @?= 2
        length (convertedModuleFixityDeclarations convertedModule) @?= 1
        renderedSource <-
          expectRightWithLabel
            "ordered declaration rendering"
            ( renderSourceString
                CompactLayout
                ( RenderConvertedModule
                    (ModuleRenderContext "" (Just "DeclarationOrder"))
                    convertedModule
                )
            )
        reparsedModule <-
          expectRightWithLabel
            ("ordered declaration re-parse:\n" <> renderedSource)
            (convertHaskellSource "DeclarationOrder.hs" renderedSource)
        fmap declarationKind (Vector.toList (cmDeclarations reparsedModule))
          @?= expectedOrder,
      testCase "class instances expose traversable methods while exact source remains authoritative" $ do
        let instanceSource =
              unlines
                [ "module OpaqueDeclarations where",
                  "type Alias = Int",
                  "data Box = Box { unBox :: Int }",
                  "instance Show Box where",
                  "  show box = box"
                ]
        convertedModule <-
          expectRightWithLabel
            "instance declaration conversion"
            (convertHaskellSource "OpaqueDeclarations.hs" instanceSource)
        fmap declarationKind (Vector.toList (cmDeclarations convertedModule))
          @?= [OpaqueDeclarationKind, OpaqueDeclarationKind, InstanceDeclarationKind]
        case Vector.toList (cmDeclarations convertedModule) of
          [ OpaqueDeclaration typeTag _ typeSource,
            OpaqueDeclaration dataTag _ dataSource,
            InstanceDeclarationNode convertedInstance
            ] -> do
              typeTag @?= UnsupportedTypeOrClassDeclaration
              dataTag @?= UnsupportedTypeOrClassDeclaration
              typeSource @?= "type Alias = Int"
              dataSource @?= "data Box = Box { unBox :: Int }"
              convertedInstanceSource convertedInstance
                @?= "instance Show Box where\n  show box = box"
              srStartLine (convertedInstanceRegion convertedInstance) @?= 4
              case convertedInstanceMethods convertedInstance of
                [TraversableInstanceMethod methodBinding] -> do
                  fmap srStartLine (tlbRegion methodBinding) @?= Just 5
                  fmap
                    (occNameString . rdrNameOcc)
                    (bindingNames (tlbBinding methodBinding))
                    @?= ["show"]
                methodSections ->
                  assertFailure
                    ("expected one traversable instance method, got " <> show methodSections)
          _ ->
            assertFailure "expected two opaque declarations and one instance node"
        convertedModuleBindings convertedModule @?= []
        convertedModuleInstanceMethodObstructions convertedModule @?= []
        case convertedModuleBindingSites convertedModule of
          [ConvertedBindingSite (InstanceMethodBindingOrigin originRegion) methodBinding] -> do
            srStartLine originRegion @?= 4
            fmap srStartLine (tlbRegion methodBinding) @?= Just 5
          bindingSites ->
            assertFailure
              ("expected one origin-tagged instance method site, got " <> show bindingSites)
        let metrics = convertedModuleMetrics convertedModule
        cmmBindingCount metrics @?= 0
        cmmInstanceDeclarationCount metrics @?= 1
        cmmTraversableInstanceMethodCount metrics @?= 1
        cmmObstructedInstanceMethodCount metrics @?= 0
        renderedModule <-
          expectRightWithLabel
            "instance declaration rendering"
            ( renderSourceString
                CompactLayout
                ( RenderConvertedModule
                    (ModuleRenderContext "" (Just "OpaqueDeclarations"))
                    convertedModule
                )
            )
        assertBool
          "type, data, and class instance declarations retain their exact source"
          ( "type Alias = Int" `isInfixOf` renderedModule
              && "data Box = Box { unBox :: Int }" `isInfixOf` renderedModule
              && "instance Show Box where\n  show box = box" `isInfixOf` renderedModule
          )
        reparsedModule <-
          expectRightWithLabel
            "rendered instance reparse"
            (convertHaskellSource "OpaqueDeclarations.hs" renderedModule)
        case Vector.toList (cmDeclarations reparsedModule) of
          [_, _, InstanceDeclarationNode reparsedInstance] ->
            convertedInstanceSource reparsedInstance
              @?= convertedInstanceSource
                ( case Vector.toList (cmDeclarations convertedModule) of
                    [_, _, InstanceDeclarationNode originalInstance] ->
                      originalInstance
                    _ ->
                      reparsedInstance
                )
          declarations ->
            assertFailure
              ("expected reparsed instance declaration, got " <> show declarations),
      testCase "binding sites preserve declaration and method source order without widening top-level bindings" $ do
        let sourceText =
              unlines
                [ "module MixedBindingSites where",
                  "before = 1",
                  "instance Example Item where",
                  "  method value = value",
                  "after = 2"
                ]
        convertedModule <-
          expectRightWithLabel
            "mixed binding-site conversion"
            (convertHaskellSource "MixedBindingSites.hs" sourceText)
        fmap declarationKind (Vector.toList (cmDeclarations convertedModule))
          @?= [ValueDeclarationKind, InstanceDeclarationKind, ValueDeclarationKind]
        fmap
          (fmap (occNameString . rdrNameOcc) . bindingNames . tlbBinding)
          (convertedModuleBindings convertedModule)
          @?= [["before"], ["after"]]
        case convertedModuleBindingSites convertedModule of
          [ ConvertedBindingSite TopLevelBindingOrigin beforeBinding,
            ConvertedBindingSite (InstanceMethodBindingOrigin instanceRegion) methodBinding,
            ConvertedBindingSite TopLevelBindingOrigin afterBinding
            ] -> do
              fmap srStartLine (tlbRegion beforeBinding) @?= Just 2
              srStartLine instanceRegion @?= 3
              fmap srStartLine (tlbRegion methodBinding) @?= Just 4
              fmap srStartLine (tlbRegion afterBinding) @?= Just 5
          bindingSites ->
            assertFailure
              ("expected ordered top-level and instance binding sites, got " <> show bindingSites),
      testCase "empty class instances are exact valid nodes" $ do
        convertedModule <-
          expectRightWithLabel
            "empty instance conversion"
            ( convertHaskellSource
                "EmptyInstance.hs"
                (unlines ["module EmptyInstance where", "instance Empty Item"])
            )
        case Vector.toList (cmDeclarations convertedModule) of
          [InstanceDeclarationNode convertedInstance] -> do
            convertedInstanceSource convertedInstance @?= "instance Empty Item"
            convertedInstanceMethods convertedInstance @?= []
          declarations ->
            assertFailure ("expected one empty instance node, got " <> show declarations),
      testCase "type-family and data-family instances retain exact source under precise tags" $ do
        convertedModule <-
          expectRightWithLabel
            "family instance conversion"
            ( convertHaskellSource
                "FamilyInstances.hs"
                ( unlines
                    [ "{-# LANGUAGE TypeFamilies #-}",
                      "module FamilyInstances where",
                      "type family Family value",
                      "type instance Family Int = Bool",
                      "data family FamilyData value",
                      "data instance FamilyData Int = FamilyDataInt"
                    ]
                )
            )
        case Vector.toList (cmDeclarations convertedModule) of
          [ OpaqueDeclaration typeFamilyTag _ typeFamilySource,
            OpaqueDeclaration typeInstanceTag _ typeInstanceSource,
            OpaqueDeclaration dataFamilyTag _ dataFamilySource,
            OpaqueDeclaration dataInstanceTag _ dataInstanceSource
            ] -> do
              typeFamilyTag @?= UnsupportedTypeOrClassDeclaration
              typeInstanceTag @?= UnsupportedTypeFamilyInstanceDeclaration
              dataFamilyTag @?= UnsupportedTypeOrClassDeclaration
              dataInstanceTag @?= UnsupportedDataFamilyInstanceDeclaration
              typeFamilySource @?= "type family Family value"
              typeInstanceSource @?= "type instance Family Int = Bool"
              dataFamilySource @?= "data family FamilyData value"
              dataInstanceSource @?= "data instance FamilyData Int = FamilyDataInt"
          declarations ->
            assertFailure
              ("expected four exact family declaration rows, got " <> show declarations),
      testCase "instance method conversion rolls back recoverable syntax without erasing siblings" $ do
        convertedModule <-
          expectRightWithLabel
            "recoverable instance method conversion"
            ( convertHaskellSource
                "RecoverableInstanceMethod.hs"
                ( unlines
                    [ "{-# LANGUAGE ViewPatterns #-}",
                      "module RecoverableInstanceMethod where",
                      "instance Example Item where",
                      "  first x = x",
                      "  bad (project -> y) = y",
                      "  third z = z"
                    ]
                )
            )
        case Vector.toList (cmDeclarations convertedModule) of
          [InstanceDeclarationNode convertedInstance] ->
            case convertedInstanceMethods convertedInstance of
              [ TraversableInstanceMethod firstBinding,
                ObstructedInstanceMethod methodObstruction,
                TraversableInstanceMethod thirdBinding
                ] -> do
                  fmap srStartLine (tlbRegion firstBinding) @?= Just 4
                  fmap srStartLine (instanceMethodObstructionRegion methodObstruction) @?= Just 5
                  fmap srStartLine (tlbRegion thirdBinding) @?= Just 6
                  case instanceMethodObstructionCause methodObstruction of
                    InstanceMethodUnsupportedPattern (Just obstructionRegion) PatOpaqueView ->
                      srStartLine obstructionRegion @?= 5
                    obstructionCause ->
                      assertFailure
                        ("expected a region-bearing view-pattern cause, got " <> show obstructionCause)
                  case (tlbBinding firstBinding, tlbBinding thirdBinding) of
                    ( FunctionBinding firstHead (Clause [PVarP firstArgument] _ :| []),
                      FunctionBinding thirdHead (Clause [PVarP thirdArgument] _ :| [])
                      ) ->
                        fmap baId [firstHead, firstArgument, thirdHead, thirdArgument]
                          @?= [BinderId 0, BinderId 1, BinderId 2, BinderId 3]
                    bindingPair ->
                      assertFailure
                        ("expected two single-argument function bindings, got " <> show bindingPair)
              methodSections ->
                assertFailure
                  ("expected traversable/obstructed/traversable method order, got " <> show methodSections)
          declarations ->
            assertFailure ("expected one instance node, got " <> show declarations)
        fmap baId (cmLambdaSites convertedModule) @?= [BinderId 1, BinderId 3]
        convertedModuleBindings convertedModule @?= []
        length (convertedModuleBindingSites convertedModule) @?= 2
        length (convertedModuleInstanceMethodObstructions convertedModule) @?= 1
        let metrics = convertedModuleMetrics convertedModule
        cmmBindingCount metrics @?= 0
        cmmInstanceDeclarationCount metrics @?= 1
        cmmTraversableInstanceMethodCount metrics @?= 2
        cmmObstructedInstanceMethodCount metrics @?= 1,
      testCase "instance recovery classifier rejects invariant failures" $ do
        recoverableInstanceMethodObstruction
          Nothing
          (ConvertMissingScopeDepth rootScopeId)
          @?= Nothing
        assertBool
          "unsupported expression syntax must remain recoverable inside one method"
          ( case
              recoverableInstanceMethodObstruction
                Nothing
                (ConvertUnsupportedExpression Nothing OpaqueStatic)
              of
                Just _ ->
                  True
                Nothing ->
                  False
          )
    ]

typedUnsupportedSyntaxTests :: TestTree
typedUnsupportedSyntaxTests =
  testGroup
    "pale.typed-unsupported-syntax"
    [ testCase "parallel statements retain their exact refusal" $
        assertUnsupportedExpressionTag
          "ParallelStatement.hs"
          ( unlines
              [ "{-# LANGUAGE ParallelListComp #-}",
                "module ParallelStatement where",
                "parallel left right = [x + y | x <- left | y <- right]"
              ]
          )
          OpaqueParallelStatement,
      testCase "transform statements retain their exact refusal" $
        assertUnsupportedExpressionTag
          "TransformStatement.hs"
          ( unlines
              [ "{-# LANGUAGE TransformListComp #-}",
                "module TransformStatement where",
                "import GHC.Exts (groupWith)",
                "grouped values = [value | value <- values, then group by value using groupWith]"
              ]
          )
          OpaqueTransformStatement,
      testCase "recursive statements retain their exact refusal" $
        assertUnsupportedExpressionTag
          "RecursiveStatement.hs"
          ( unlines
                [ "{-# LANGUAGE RecursiveDo #-}",
                  "module RecursiveStatement where",
                  "recursive action = mdo { rec { value <- action value }; pure value }"
                ]
          )
          OpaqueRecursiveStatement,
      testCase "implicit-parameter binds retain their exact refusal" $
        assertUnsupportedExpressionTag
          "ImplicitParameter.hs"
          ( unlines
              [ "{-# LANGUAGE ImplicitParams #-}",
                "module ImplicitParameter where",
                "parameter value = let ?parameter = value in ?parameter"
              ]
          )
          OpaqueImplicitParameterBinds,
      testCase "overloaded record updates retain their exact refusal" $
        assertUnsupportedExpressionTag
          "OverloadedRecordUpdate.hs"
          ( unlines
              [ "{-# LANGUAGE OverloadedRecordDot #-}",
                "{-# LANGUAGE OverloadedRecordUpdate #-}",
                "module OverloadedRecordUpdate where",
                "rename recordValue = recordValue { owner.name = \"Ada\" }"
              ]
          )
          OpaqueOverloadedRecordUpdate
    ]

assertUnsupportedExpressionTag ::
  FilePath ->
  String ->
  HsOpaqueTag ->
  IO ()
assertUnsupportedExpressionTag sourcePath sourceText expectedTag =
  case convertHaskellSource sourcePath sourceText of
    Left (ConvertUnsupportedExpression (Just _) actualTag) ->
      actualTag @?= expectedTag
    Left obstruction ->
      assertFailure
        ( "expected a region-bearing "
            <> show expectedTag
            <> " obstruction, got "
            <> show obstruction
        )
    Right _ ->
      assertFailure
        ("expected a region-bearing " <> show expectedTag <> " obstruction, got success")

scopeIndexTests :: TestTree
scopeIndexTests =
  testGroup
    "pale.scope-index"
    [ testCase "all valid preorder parent vectors through size eight agree with the parent-walk oracle" $ do
        let (rejectedParentVectors, validScopeIndexes) = scopeParentVectorPartition
        length scopeParentVectorCandidates @?= 5914
        length rejectedParentVectors @?= 5288
        length validScopeIndexes @?= 626
        case
            find
              ( \(parentVector, scopeIndex) ->
                  case verifyScopeIndexAgainstParentWalk parentVector scopeIndex of
                    Left _ ->
                      True
                    Right () ->
                      False
              )
              validScopeIndexes
          of
          Nothing ->
            pure ()
          Just (parentVector, scopeIndex) ->
            case verifyScopeIndexAgainstParentWalk parentVector scopeIndex of
              Left failure ->
                assertFailure
                  ( failure
                      <> "\nparent vector: "
                      <> show (Vector.toList parentVector)
                  )
              Right () ->
                assertFailure "scope differential reported a non-reproducible failure",
      testCase "a preorder chain has its deepest scope as O(1) top" $ do
        scopeIndex <-
          expectRightWithLabel
            "chain scope index"
            (mkScopeIndex (Vector.fromList [0, 0, 1, 2]) Vector.empty)
        scopeOne <- expectRightWithLabel "scope one" (mkScopeId 1)
        scopeTwo <- expectRightWithLabel "scope two" (mkScopeId 2)
        scopeThree <- expectRightWithLabel "scope three" (mkScopeId 3)
        scopeTopCtx scopeIndex @?= Right (ActualScope scopeThree)
        scopeIsAncestorOf scopeIndex scopeOne scopeThree @?= Right True
        scopeLca scopeIndex scopeTwo scopeThree @?= Right scopeTwo,
      testCase "preorder-chain construction stays linear before lift-table construction" $ do
        let chainSize = 32768
            parentVector =
              Vector.generate chainSize (\scopeKey -> max 0 (scopeKey - 1))
        scopeIndex <-
          expectRightWithLabel
            "deep chain scope index"
            (mkScopeIndex parentVector Vector.empty)
        deepestScope <- expectRightWithLabel "deepest chain scope" (mkScopeId (chainSize - 1))
        scopeTopCtx scopeIndex @?= Right (ActualScope deepestScope),
      testCase "a preorder branch has incompatible top and range-correct ancestry" $ do
        scopeIndex <-
          expectRightWithLabel
            "branch scope index"
            (mkScopeIndex (Vector.fromList [0, 0, 1, 0]) Vector.empty)
        scopeOne <- expectRightWithLabel "scope one" (mkScopeId 1)
        scopeThree <- expectRightWithLabel "scope three" (mkScopeId 3)
        scopeTopCtx scopeIndex @?= Right IncompatibleScope
        scopeIsAncestorOf scopeIndex scopeOne scopeThree @?= Right False
        scopeLca scopeIndex scopeOne scopeThree @?= Right rootScopeId,
      testCase "free-scope merge retains distinct scopes at equal depth" $ do
        scopeOne <- expectRightWithLabel "scope one" (mkScopeId 1)
        scopeTwo <- expectRightWithLabel "scope two" (mkScopeId 2)
        let leftSummary = singletonFreeScopeSummary scopeOne
            rightSummary = singletonFreeScopeSummary scopeTwo
            expectedScopes = [scopeOne, scopeTwo]
        freeScopeSummaryToList
          (mergeFreeScopeSummaryBy (const 1) leftSummary rightSummary)
          @?= expectedScopes
        freeScopeSummaryToList
          (mergeFreeScopeSummaryBy (const 1) rightSummary leftSummary)
          @?= expectedScopes,
      testCase "module metrics derive wrapper free scopes from the canonical expression" $ do
        convertedModule <-
          expectRightWithLabel
            "metrics wrapper fixture"
            ( convertHaskellSource
                "Counterexample.hs"
                ( unlines
                    [ "module Counterexample where",
                      "f x y = let { g True = x; g False = y } in g True"
                    ]
                )
            )
        let metrics = convertedModuleMetrics convertedModule
        cmmMaxFreeScopeCount metrics @?= 2,
      testCase "sealed binding metric sections equal checked structural projections" $ do
        convertedModule <-
          expectRightWithLabel
            "metric section fixture"
            ( convertHaskellSource
                "MetricSections.hs"
                ( unlines
                    [ "module MetricSections where",
                      "nested x y = let { choose True = x; choose False = y } in choose True",
                      "guarded value | value = external | otherwise = value",
                      "multi True value = value",
                      "multi False _ = external",
                      "plain = external"
                    ]
                )
            )
        projectedBindings <-
          expectRightWithLabel
            "checked metric projections"
            ( traverse
                (bindingExpr (cmScopeIndex convertedModule))
                (convertedModuleBindings convertedModule)
            )
        let metrics = convertedModuleMetrics convertedModule
            oracleMetrics =
              foldMap expressionMetricOracle projectedBindings
        ( cmmScopedExprCount metrics,
          cmmGlobalVarRefCount metrics,
          cmmLocalVarRefCount metrics,
          cmmMaxFreeScopeCount metrics
          )
          @?= ( oracleScopedExprCount oracleMetrics,
                oracleGlobalVarRefCount oracleMetrics,
                oracleLocalVarRefCount oracleMetrics,
                oracleMaxFreeScopeCount oracleMetrics
              ),
      testCase "binding dependencies distinguish independent, acyclic, and cyclic groups" $ do
        convertedModule <-
          expectRightWithLabel
            "dependency fixture"
            ( convertHaskellSource
                "Dependencies.hs"
                ( unlines
                    [ "module Dependencies where",
                      "independent = let { a = 1; b = 2 } in a + b",
                      "acyclic = let { a = 1; b = a } in b",
                      "recursive = let { a = b; b = a } in a",
                      "nested = let { a = 1; b = let { inner = a } in inner } in b"
                    ]
                )
            )
        foldMap
          (letRecursions . tlbTerm)
          (convertedModuleBindings convertedModule)
          @?= [ NonRecursiveBinds,
                AcyclicDependentBinds,
                RecursiveBinds,
                AcyclicDependentBinds,
                NonRecursiveBinds
              ],
      testCase "binderless dependent rows do not leak their binding-group scope" $ do
        convertedModule <-
          expectRightWithLabel
            "binderless dependency fixture"
            ( convertHaskellSource
                "BinderlessDependency.hs"
                ( unlines
                    [ "module BinderlessDependency where",
                      "closed = let { x = 1; _ = x } in 2"
                    ]
                )
            )
        topLevelBinding <- singleBinding convertedModule
        projectedBinding <-
          expectRightWithLabel
            "binderless dependency projection"
            (bindingExpr (cmScopeIndex convertedModule) topLevelBinding)
        freeScopeSummaryToList (exprFreeScopes projectedBinding) @?= [],
      testCase "singleton binding components preserve generic SCC evidence" $ do
        assertSingletonBindingComponent
          "one independent binder"
          ["one x = y where y = x"]
          1
          AcyclicBindingComponent
        assertSingletonBindingComponent
          "one recursive binder"
          ["self = y where y = y"]
          1
          RecursiveBindingComponent
        assertSingletonBindingComponent
          "multiple recursive pattern binders"
          ["pair x = right where (left, right) = (left, x)"]
          2
          RecursiveBindingComponent
        assertSingletonBindingComponent
          "no pattern binders"
          ["wild x = x where _ = x"]
          0
          AcyclicBindingComponent,
      testCase "specialized binding components agree with the generic SCC oracle" $ do
        traverse_
          assertBindingComponentsMatchGraphOracle
          [ ( "independent rows",
              ["subject = a + b + c + d where { a = 1; b = 2; (c, d) = (3, 4); _ = 5 }"]
            ),
            ( "acyclic rows",
              ["subject = c where { a = 1; b = a; c = b }"]
            ),
            ( "cyclic rows",
              ["subject = a where { a = b; b = c; c = a }"]
            ),
            ( "singleton general pattern",
              ["subject x = result where (left, right) = (x, left); result = right"]
            )
          ]
    ]

caseAlternativeScopeTests :: TestTree
caseAlternativeScopeTests =
  testGroup
    "pale.case-scopes"
    [ testCase "binderless case alternatives receive distinct child scopes" $ do
        (convertedModule, caseScope, nilPattern, nilScope, consPattern, consScope) <-
          binderlessCaseAlternativeScopes
        patBinders nilPattern @?= []
        patBinders consPattern @?= []
        assertBool "binderless alternatives collapsed into one scope" (nilScope /= consScope)
        scopeParentId (cmScopeIndex convertedModule) nilScope @?= Right caseScope
        scopeParentId (cmScopeIndex convertedModule) consScope @?= Right caseScope,
      testCase "scope restriction reaches binderless sibling alternatives independently" $ do
        (convertedModule, caseScope, _, nilScope, _, consScope) <-
          binderlessCaseAlternativeScopes
        let scopeIndex = cmScopeIndex convertedModule
            caseContext = ActualScope caseScope
            nilContext = ActualScope nilScope
            consContext = ActualScope consScope
        scopeCtxLeq scopeIndex caseContext nilContext @?= Right True
        scopeCtxLeq scopeIndex caseContext consContext @?= Right True
        scopeCtxLeq scopeIndex nilContext consContext @?= Right False
        scopeCtxLeq scopeIndex consContext nilContext @?= Right False
        scopeCtxMeet scopeIndex nilContext consContext @?= Right caseContext
        scopeCtxJoin scopeIndex nilContext consContext @?= Right IncompatibleScope
    ]

binderlessCaseAlternativeScopes :: IO (ConvertedModule, ScopeId, HsPatF, ScopeId, HsPatF, ScopeId)
binderlessCaseAlternativeScopes = do
  convertedModule <-
    expectRightWithLabel
      "binderless case fixture conversion"
      ( convertHaskellSource
          "BinderlessCase.hs"
          ( unlines
              [ "module BinderlessCase where",
                "",
                "classify values = case values of { [] -> empty; (_ : _) -> nonempty }"
              ]
          )
      )
  bindingValue <- singleBinding convertedModule
  case tlbBinding bindingValue of
    FunctionBinding _ (Clause _ (UnguardedRhs bodyExpr _) :| []) ->
      case exprNode bodyExpr of
        CaseF _ [(nilPattern, nilExpr), (consPattern, consExpr)] ->
          pure
            ( convertedModule,
              exprScope bodyExpr,
              nilPattern,
              exprScope nilExpr,
              consPattern,
              exprScope consExpr
            )
        otherNode ->
          assertFailure ("expected a two-alternative scoped case expression, got " <> show otherNode)
    otherBinding ->
      assertFailure ("expected one-clause function binding around the case expression, got " <> show otherBinding)

scopeParentVectorCandidates :: [Vector.Vector Int]
scopeParentVectorCandidates =
  [ Vector.fromList (0 : parentKeys)
  | scopeCount <- [1 .. 8],
    parentKeys <-
      sequence
        [ [0 .. scopeKey - 1]
        | scopeKey <- [1 .. scopeCount - 1]
        ]
  ]

scopeParentVectorPartition ::
  ( [(Vector.Vector Int, ScopeIndexFailure)],
    [(Vector.Vector Int, ScopeIndex)]
  )
scopeParentVectorPartition =
  partitionEithers
    ( fmap
        ( \parentVector ->
            case mkScopeIndex parentVector Vector.empty of
              Left failure ->
                Left (parentVector, failure)
              Right scopeIndex ->
                Right (parentVector, scopeIndex)
        )
        scopeParentVectorCandidates
    )

verifyScopeIndexAgainstParentWalk ::
  Vector.Vector Int ->
  ScopeIndex ->
  Either String ()
verifyScopeIndexAgainstParentWalk parentVector scopeIndex = do
  let scopeKeys = [0 .. Vector.length parentVector - 1]
      scopePairs = (,) <$> scopeKeys <*> scopeKeys
  scopeIds <- traverse checkedScopeId scopeKeys
  traverse_ (verifyScopePair parentVector scopeIndex) scopePairs
  pairComparabilities <-
    traverse
      ( \(leftKey, rightKey) ->
          (||)
            <$> parentWalkIsAncestor parentVector leftKey rightKey
            <*> parentWalkIsAncestor parentVector rightKey leftKey
      )
      scopePairs
  scopeDepths <-
    traverse
      ( \scopeKey ->
          fmap
            (\ancestors -> (scopeKey, length ancestors - 1))
            (parentWalkAncestors parentVector scopeKey)
      )
      scopeKeys
  let hasIncomparableScopes = any not pairComparabilities
      (deepestScopeKey, _deepestDepth) =
        foldl' preferDeeperScope (0, 0) scopeDepths
  deepestScope <- checkedScopeId deepestScopeKey
  let expectedTopContext =
        if hasIncomparableScopes
          then IncompatibleScope
          else ActualScope deepestScope
      expectedObservedContexts =
        fmap ActualScope scopeIds
          <> [IncompatibleScope | hasIncomparableScopes]
  requireEqual
    "scope top-context classification"
    (Right expectedTopContext)
    (scopeTopCtx scopeIndex)
  requireEqual
    "scope observed-context branch classification"
    (Right expectedObservedContexts)
    (scopeObservedContexts scopeIndex)
  where
    preferDeeperScope :: (Int, Int) -> (Int, Int) -> (Int, Int)
    preferDeeperScope deepest@(_deepestKey, deepestDepth) candidate@(_candidateKey, candidateDepth)
      | candidateDepth > deepestDepth =
          candidate
      | otherwise =
          deepest

verifyScopePair ::
  Vector.Vector Int ->
  ScopeIndex ->
  (Int, Int) ->
  Either String ()
verifyScopePair parentVector scopeIndex (leftKey, rightKey) = do
  leftScope <- checkedScopeId leftKey
  rightScope <- checkedScopeId rightKey
  expectedLeftAncestor <-
    parentWalkIsAncestor parentVector leftKey rightKey
  expectedRightAncestor <-
    parentWalkIsAncestor parentVector rightKey leftKey
  requireEqual
    ("scopeIsAncestorOf " <> show (leftKey, rightKey))
    (Right expectedLeftAncestor)
    (scopeIsAncestorOf scopeIndex leftScope rightScope)
  requireEqual
    ("scopeComparable " <> show (leftKey, rightKey))
    (Right (expectedLeftAncestor || expectedRightAncestor))
    (scopeComparable scopeIndex leftScope rightScope)

checkedScopeId :: Int -> Either String ScopeId
checkedScopeId scopeKey =
  case mkScopeId scopeKey of
    Left failure ->
      Left ("scope-id construction failed: " <> show failure)
    Right scopeId ->
      Right scopeId

parentWalkIsAncestor ::
  Vector.Vector Int ->
  Int ->
  Int ->
  Either String Bool
parentWalkIsAncestor parentVector candidateAncestor scopeKey =
  elem candidateAncestor
    <$> parentWalkAncestors parentVector scopeKey

parentWalkAncestors ::
  Vector.Vector Int ->
  Int ->
  Either String [Int]
parentWalkAncestors parentVector =
  descend (Vector.length parentVector + 1)
  where
    descend remainingSteps scopeKey
      | remainingSteps <= 0 =
          Left ("parent walk did not reach the root from scope " <> show scopeKey)
      | scopeKey == 0 =
          Right [0]
      | otherwise =
          case parentVector Vector.!? scopeKey of
            Nothing ->
              Left
                ( "parent walk left the vector at scope "
                    <> show scopeKey
                    <> " of "
                    <> show (Vector.length parentVector)
                )
            Just parentKey
              | parentKey < 0 || parentKey >= scopeKey ->
                  Left
                    ( "parent walk encountered invalid edge "
                        <> show (scopeKey, parentKey)
                    )
              | otherwise ->
                  (scopeKey :)
                    <$> descend (remainingSteps - 1) parentKey

requireEqual ::
  (Eq value, Show value) =>
  String ->
  value ->
  value ->
  Either String ()
requireEqual label expected actual
  | expected == actual =
      Right ()
  | otherwise =
      Left
        ( label
            <> "\nexpected: "
            <> show expected
            <> "\nactual: "
            <> show actual
        )

renderRoundTripTests :: TestTree
renderRoundTripTests =
  testGroup
    "render.roundtrip"
    [ roundTripCase
        "lambda and application round-trip"
        [ "module Fixture where",
          "",
          "handle = \\evt -> process evt evt"
        ],
      testCase "SCC expression pragmas transparently convert their wrapped expression" $ do
        let wrappedSource =
              unlines
                [ "module SccWrappedExpression where",
                  "instrumented value = {-# SCC \"nebula.ingest.parse\" #-} value + 1"
                ]
            plainSource =
              unlines
                [ "module SccWrappedExpression where",
                  "instrumented value = value + 1"
                ]
        wrappedModule <-
          expectRightWithLabel
            "SCC-wrapped expression conversion"
            (convertHaskellSource "SccWrappedExpression.hs" wrappedSource)
        plainModule <-
          expectRightWithLabel
            "plain expression conversion"
            (convertHaskellSource "SccWrappedExpression.hs" plainSource)
        wrappedBinding <- singleBinding wrappedModule
        plainBinding <- singleBinding plainModule
        assertBool
          "SCC wrapper changed the converted expression"
          (renderRoundTripEquivalent (tlbTerm wrappedBinding) (tlbTerm plainBinding)),
      roundTripCase
        "multi-argument function becomes a lambda chain"
        [ "module Fixture where",
          "",
          "apply2 f x = f x x"
        ],
      roundTripCase
        "plain where round-trips with layout"
        [ "module Fixture where",
          "",
          "scale x = base * x",
          "  where",
          "    base = 10"
        ],
      roundTripCase
        "multi-bind where round-trips"
        [ "module Fixture where",
          "",
          "f x = combine y z",
          "  where",
          "    y = deriveY x",
          "    z = deriveZ x"
        ],
      roundTripCase
        "where guarded local function round-trips"
        [ "module Fixture where",
          "",
          "clamp q = saturate q",
          "  where",
          "    saturate value",
          "      | value > upper = upper",
          "      | value < lower = lower",
          "      | otherwise = value"
        ],
      roundTripCase
        "where tuple pattern bind round-trips"
        [ "module Fixture where",
          "",
          "f x = combine a b where (a, b) = splitPair x"
        ],
      roundTripCase
        "let constructor pattern bind round-trips"
        [ "module Fixture where",
          "",
          "g m = let Just y = m in use y"
        ],
      roundTripCase
        "mixed var and pattern where binds round-trip"
        [ "module Fixture where",
          "",
          "mix x = combine seed a b",
          "  where",
          "    seed = deriveSeed x",
          "    (a, b) = splitPair x"
        ],
      roundTripCase
        "do-let tuple pattern bind round-trips"
        [ "module Fixture where",
          "",
          "run pair = do { let { (a, b) = pair }; pure (combine a b) }"
        ],
      roundTripCase
        "lazy where pattern bind round-trips"
        [ "module Fixture where",
          "",
          "lazyBind pair = combine a b",
          "  where",
          "    ~(a, b) = pair"
        ],
      roundTripCase
        "case with tuple and wildcard branches"
        [ "module Fixture where",
          "",
          "swap p = case p of { (a, b) -> (b, a); _ -> p }"
        ],
      roundTripCase
        "do block with bind, let, and body statements"
        [ "module Fixture where",
          "",
          "run action = do { x <- action; let { y = combine x x }; pure y }"
        ],
      testCase "generated top-level binding renders do and let as layout while compact rendering stays compact" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "run action = do { x <- action; let { y = combine x x }; pure y }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <-
          expectRightWithLabel
            "compact render"
            (renderSourceString CompactLayout (RenderSourceBinding (tlbBinding bindingValue)))
        assertBool
          ("compact top-level renderer changed its round-trip surface:\n" <> compactRendered)
          ("do {" `isInfixOf` compactRendered && "let {" `isInfixOf` compactRendered)
        generatedRendered <-
          expectRightWithLabel
            "generated render"
            ( renderSourceString
                (PrettyLayout defaultPageWidth)
                (RenderSourceBinding (tlbBinding bindingValue))
            )
        assertBool
          ("generated renderer still emitted compact do syntax:\n" <> generatedRendered)
          (not ("do {" `isInfixOf` generatedRendered))
        assertBool
          ("generated renderer still emitted compact let syntax:\n" <> generatedRendered)
          (not ("let {" `isInfixOf` generatedRendered))
        assertBool
          ("generated renderer did not emit layout do syntax:\n" <> generatedRendered)
          ("\n  do" `isInfixOf` generatedRendered || "= do" `isInfixOf` generatedRendered)
        reparsedModule <-
          expectRightWithLabel
            "re-parse of generated rendering"
            (convertHaskellSource "Generated.hs" ("module Generated where\n\n" <> generatedRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding generatedRendered (bindingValue, reparsedBinding),
      testCase "generated short breakable expressions remain on one line while compact output is unchanged" $ do
        let expectedRendered =
              "short source = if c source then f source [a, b] else (g, d)"
        convertedModule <-
          expectRightWithLabel
            "short breakable fixture conversion"
            ( convertHaskellSource
                "ShortBreakable.hs"
                ( unlines
                    [ "module ShortBreakable where",
                      "",
                      "short source = if c source then f source [a, b] else (g, d)"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <-
          expectRightWithLabel
            "short breakable compact render"
            (renderSourceString CompactLayout (RenderSourceBinding (tlbBinding bindingValue)))
        compactRendered @?= expectedRendered
        generatedRendered <-
          expectRightWithLabel
            "short breakable generated render"
            ( renderSourceString
                (PrettyLayout defaultPageWidth)
                (RenderSourceBinding (tlbBinding bindingValue))
            )
        generatedRendered @?= expectedRendered
        reparsedModule <-
          expectRightWithLabel
            "short breakable generated re-parse"
            (convertHaskellSource "ShortBreakableGenerated.hs" ("module ShortBreakableGenerated where\n\n" <> generatedRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding generatedRendered (bindingValue, reparsedBinding),
      testCase "generated long applications collections operators and patterns wrap within the page width" $ do
        convertedModule <-
          expectRightWithLabel
            "long breakable fixture conversion"
            ( convertHaskellSource
                "LongBreakable.hs"
                ( unlines
                    [ "module LongBreakable where",
                      "",
                      "longBreakable source =",
                      "  case reads source of",
                      "    [(rawIdentifierFromEncodedRiverPath, \"\"), (secondaryIdentifierFromEncodedRiverPath, \"\")]",
                      "      | rawIdentifierFromEncodedRiverPath >= 0 ->",
                      "          if rawIdentifierFromEncodedRiverPath > 0",
                      "            then buildExpandedRiverSummary",
                      "                   (deriveExpansionOrigin source rawIdentifierFromEncodedRiverPath)",
                      "                   [(firstExpansionIdentifier, firstExpansionPayload), (secondExpansionIdentifier, secondExpansionPayload), (thirdExpansionIdentifier, thirdExpansionPayload)]",
                      "            else \\continuationValue -> continuationValue + deriveExpansionScore source + normalizeExpansionScore rawIdentifierFromEncodedRiverPath",
                      "    _ -> fallbackExpansion source"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <-
          expectRightWithLabel
            "long breakable compact render"
            (renderSourceString CompactLayout (RenderSourceBinding (tlbBinding bindingValue)))
        assertBool
          ("compact rendering unexpectedly introduced a line break:\n" <> compactRendered)
          (not ("\n" `isInfixOf` compactRendered))
        generatedRendered <-
          expectRightWithLabel
            "long breakable generated render"
            ( renderSourceString
                (PrettyLayout defaultPageWidth)
                (RenderSourceBinding (tlbBinding bindingValue))
            )
        assertBool
          ("long generated expression did not wrap:\n" <> generatedRendered)
          ("\n" `isInfixOf` generatedRendered)
        assertBool
          ("long generated expression exceeded the page width:\n" <> generatedRendered)
          (all ((<= 80) . length) (lines generatedRendered))
        assertBool
          ("long generated collection remained inline:\n" <> generatedRendered)
          ("[\n" `isInfixOf` generatedRendered)
        reparsedModule <-
          expectRightWithLabel
            ("long breakable generated re-parse:\n" <> generatedRendered)
            (convertHaskellSource "LongBreakableGenerated.hs" ("module LongBreakableGenerated where\n\n" <> generatedRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding generatedRendered (bindingValue, reparsedBinding),
      testCase "generated long constructor patterns break inside case layout" $ do
        let narrowedPageWidth =
              shrinkPageWidth 16 defaultPageWidth
            inlineConstructorPattern =
              "TopologyRoute dischargeValue areaValue orderValue roleValue _ ->"
        convertedModule <-
          expectRightWithLabel
            "long constructor-pattern fixture conversion"
            ( convertHaskellSource
                "LongConstructorPattern.hs"
                ( unlines
                    [ "module LongConstructorPattern where",
                      "",
                      "longConstructorPattern route =",
                      "  case route of",
                      "    TopologyRoute dischargeValue areaValue orderValue roleValue _ ->",
                      "      makeRoute dischargeValue areaValue orderValue roleValue",
                      "    _ -> fallback route"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <-
          expectRightWithLabel
            "long constructor-pattern compact render"
            (renderSourceString CompactLayout (RenderSourceBinding (tlbBinding bindingValue)))
        assertBool
          ("compact constructor pattern changed:\n" <> compactRendered)
          (inlineConstructorPattern `isInfixOf` compactRendered)
        generatedRendered <-
          expectRightWithLabel
            "long constructor-pattern generated render"
            ( renderSourceString
                (PrettyLayout narrowedPageWidth)
                (RenderSourceBinding (tlbBinding bindingValue))
            )
        assertBool
          ("generated constructor pattern remained inline:\n" <> generatedRendered)
          (not (inlineConstructorPattern `isInfixOf` generatedRendered))
        assertBool
          ("generated constructor pattern exceeded the narrowed page width:\n" <> generatedRendered)
          (all ((<= 64) . length) (lines generatedRendered))
        reparsedModule <-
          expectRightWithLabel
            ("long constructor-pattern generated re-parse:\n" <> generatedRendered)
            (convertHaskellSource "LongConstructorPatternGenerated.hs" ("module LongConstructorPatternGenerated where\n\n" <> generatedRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding generatedRendered (bindingValue, reparsedBinding),
      testCase "readable rendering keeps lambda-case out of brace layout" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "choose input = consume (\\case { Just x -> pure x; Nothing -> empty }) input"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <-
          expectRightWithLabel
            "compact render"
            (renderSourceString CompactLayout (RenderSourceBinding (tlbBinding bindingValue)))
        assertBool
          ("compact lambda-case renderer changed its round-trip surface:\n" <> compactRendered)
          ("\\case {" `isInfixOf` compactRendered)
        readableRendered <-
          expectRightWithLabel
            "readable render"
            ( renderSourceString
                (PrettyLayout defaultPageWidth)
                (RenderSourceBinding (tlbBinding bindingValue))
            )
        assertBool
          ("readable renderer still emitted compact lambda-case syntax:\n" <> readableRendered)
          (not ("\\case {" `isInfixOf` readableRendered))
        assertBool
          ("readable renderer did not emit layout lambda-case syntax:\n" <> readableRendered)
          ("\\case\n" `isInfixOf` readableRendered)
        reparsedModule <-
          expectRightWithLabel
            "re-parse of readable rendering"
            (convertHaskellSource "Readable.hs" ("module Readable where\n\n" <> readableRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding readableRendered (bindingValue, reparsedBinding),
      testCase "readable rendering keeps record construction in layout form" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "record = Metrics { alpha = one, beta = two }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <-
          expectRightWithLabel
            "compact render"
            (renderSourceString CompactLayout (RenderSourceBinding (tlbBinding bindingValue)))
        assertBool
          ("compact record renderer changed its round-trip surface:\n" <> compactRendered)
          ("{ alpha = one, beta = two }" `isInfixOf` compactRendered)
        readableRendered <-
          expectRightWithLabel
            "readable render"
            ( renderSourceString
                (PrettyLayout defaultPageWidth)
                (RenderSourceBinding (tlbBinding bindingValue))
            )
        assertBool
          ("readable renderer still emitted one-line record syntax:\n" <> readableRendered)
          (not ("{ alpha = one, beta = two }" `isInfixOf` readableRendered))
        assertBool
          ("readable renderer did not emit layout record syntax:\n" <> readableRendered)
          ("\n    { alpha = one\n    , beta = two\n    }" `isInfixOf` readableRendered)
        reparsedModule <-
          expectRightWithLabel
            "re-parse of readable record rendering"
            (convertHaskellSource "ReadableRecord.hs" ("module ReadableRecord where\n\n" <> readableRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding readableRendered (bindingValue, reparsedBinding),
      testCase "generated long record fields break inside record layout" $ do
        let narrowedPageWidth =
              shrinkPageWidth 16 defaultPageWidth
            inlineField =
              "riverFaceDischargeNormalized = evidenceDischargeNorm hydrologyValue"
        convertedModule <-
          expectRightWithLabel
            "long record-field fixture conversion"
            ( convertHaskellSource
                "LongRecordField.hs"
                ( unlines
                    [ "module LongRecordField where",
                      "",
                      "longRecordField hydrologyValue =",
                      "  RiverFace { riverFaceDischargeNormalized = evidenceDischargeNorm hydrologyValue, riverFaceId = faceId }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        compactRendered <-
          expectRightWithLabel
            "long record-field compact render"
            (renderSourceString CompactLayout (RenderSourceBinding (tlbBinding bindingValue)))
        assertBool
          ("compact record field changed:\n" <> compactRendered)
          (inlineField `isInfixOf` compactRendered)
        generatedRendered <-
          expectRightWithLabel
            "long record-field generated render"
            ( renderSourceString
                (PrettyLayout narrowedPageWidth)
                (RenderSourceBinding (tlbBinding bindingValue))
            )
        assertBool
          ("generated record field remained inline:\n" <> generatedRendered)
          (not (inlineField `isInfixOf` generatedRendered))
        assertBool
          ("generated record field exceeded the narrowed page width:\n" <> generatedRendered)
          (all ((<= 64) . length) (lines generatedRendered))
        reparsedModule <-
          expectRightWithLabel
            ("long record-field generated re-parse:\n" <> generatedRendered)
            (convertHaskellSource "LongRecordFieldGenerated.hs" ("module LongRecordFieldGenerated where\n\n" <> generatedRendered <> "\n"))
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding generatedRendered (bindingValue, reparsedBinding),
      roundTripCase
        "operator applications with symbolic and alphanumeric operators"
        [ "module Fixture where",
          "",
          "total a b c = a + b * c",
          "",
          "halve a b = a `div` b",
          "",
          "summed = foldr (+) 0"
        ],
      testCase "mixed-precedence operator chains render without parser-tree parens" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "bounded value = value >= 0 && value <= 32"
                    ]
                )
            )
        renderedSource <-
          expectRightWithLabel
            "render"
            (renderFixtureModule "Fixture" convertedModule)
        assertBool
          ("mixed fixity chain was rendered through the parser tree:\n" <> renderedSource)
          (not ("((value >= 0) && value) <= 32" `isInfixOf` renderedSource))
        assertBool
          ("mixed fixity chain lost its surface order:\n" <> renderedSource)
          ("value >= 0 && value <= 32" `isInfixOf` renderedSource),
      testCase "custom fixity declarations and surface-order chains survive module rendering" $ do
        convertedModule <-
          expectRightWithLabel
            "custom-fixity conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "infixr 4 <~>",
                      "infixl 6 <+>",
                      "chain = a <~> b <+> c <~> d"
                    ]
                )
            )
        renderedSource <-
          expectRightWithLabel
            "custom-fixity module rendering"
            (renderFixtureModule "Fixture" convertedModule)
        assertBool "right-associative fixity declaration was lost" ("infixr 4 <~>" `isInfixOf` renderedSource)
        assertBool "left-associative fixity declaration was lost" ("infixl 6 <+>" `isInfixOf` renderedSource)
        assertBool "operator chain surface order was lost" ("a <~> b <+> c <~> d" `isInfixOf` renderedSource)
        reparsedModule <-
          expectRightWithLabel
            "custom-fixity rendered-source conversion"
            (convertHaskellSource "Fixture.hs" renderedSource)
        originalBinding <- singleBinding convertedModule
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (originalBinding, reparsedBinding),
      testCase "alpha equivalence follows binder identity under shadowing" $ do
        let leftOuter = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "x"))
            leftInner = BinderAnn (BinderId 1) (mkRdrUnqual (mkVarOcc "x"))
            rightOuter = BinderAnn (BinderId 10) (mkRdrUnqual (mkVarOcc "a"))
            rightInner = BinderAnn (BinderId 11) (mkRdrUnqual (mkVarOcc "b"))
            leftTerm =
              PatternNode
                (LamF leftOuter (PatternNode (LamF leftInner (PatternNode (VarF (LocalName leftOuter))))))
            alphaRenamed =
              PatternNode
                (LamF rightOuter (PatternNode (LamF rightInner (PatternNode (VarF (LocalName rightOuter))))))
            captured =
              PatternNode
                (LamF rightOuter (PatternNode (LamF rightInner (PatternNode (VarF (LocalName rightInner))))))
        assertBool "alpha-renamed outer reference should remain equivalent" (renderRoundTripEquivalent leftTerm alphaRenamed)
        assertBool "outer and inner shadowed references must not collapse" (not (renderRoundTripEquivalent leftTerm captured)),
      testCase "capture-avoiding rendering freshens binders against globals" $ do
        let binderAnn = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "x"))
            globalName = mkRdrUnqual (mkVarOcc "x")
            term =
              PatternNode
                ( LamF
                    binderAnn
                    (PatternNode (AppF (PatternNode (VarF (GlobalName globalName))) (PatternNode (VarF (LocalName binderAnn)))))
                )
        renderSourceString CompactLayout (RenderRewriteExpression term)
          @?= Right "\\x_0 -> x x_0",
      testCase "repeated binder spellings use one flat suffix frontier" $ do
        let outerBinder = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "value"))
            middleBinder = BinderAnn (BinderId 1) (mkRdrUnqual (mkVarOcc "value"))
            innerBinder = BinderAnn (BinderId 2) (mkRdrUnqual (mkVarOcc "value"))
            term =
              PatternNode
                ( LamF
                    outerBinder
                    ( PatternNode
                        ( LamF
                            middleBinder
                            ( PatternNode
                                ( LamF
                                    innerBinder
                                    (PatternNode (VarF (LocalName innerBinder)))
                                )
                            )
                        )
                    )
                )
        renderSourceString CompactLayout (RenderRewriteExpression term)
          @?= Right "\\value -> \\value_0 -> \\value_1 -> value_1",
      testCase "tuple sections preserve missing slots and boxity" $ do
        boxedModule <-
          expectRightWithLabel
            "boxed tuple-section conversion"
            (convertHaskellSource "Boxed.hs" (unlines ["{-# LANGUAGE TupleSections #-}", "module Boxed where", "section x = (, x)"]))
        unboxedModule <-
          expectRightWithLabel
            "unboxed tuple-section conversion"
            ( convertHaskellSource
                "Unboxed.hs"
                (unlines ["{-# LANGUAGE MagicHash #-}", "{-# LANGUAGE TupleSections #-}", "{-# LANGUAGE UnboxedTuples #-}", "module Unboxed where", "section x = (# x, #)"])
            )
        boxedBinding <- singleBinding boxedModule
        unboxedBinding <- singleBinding unboxedModule
        case stripBindingLambdas (tlbTerm boxedBinding) of
          PatternNode (ExplicitTupleF BoxedTuple [TupleMissing, TuplePresent _]) -> pure ()
          otherTerm -> assertFailure ("unexpected boxed tuple-section structure: " <> show otherTerm)
        case stripBindingLambdas (tlbTerm unboxedBinding) of
          PatternNode (ExplicitTupleF UnboxedTuple [TuplePresent _, TupleMissing]) -> pure ()
          otherTerm -> assertFailure ("unexpected unboxed tuple-section structure: " <> show otherTerm),
      testCase "top-level pattern bindings are represented rather than omitted" $ do
        convertedModule <-
          expectRightWithLabel
            "top-level pattern conversion"
            (convertHaskellSource "PatternBinding.hs" (unlines ["module PatternBinding where", "Just value = source"]))
        bindingValue <- singleBinding convertedModule
        case tlbBinding bindingValue of
          PatternBinding (PConP _ [PVarP _]) _ -> pure ()
          otherBinding -> assertFailure ("unexpected top-level binding structure: " <> show otherBinding),
      testCase "case operand in operator application uses byte-stable compact braces" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "globalReferenceNames nodeValue = (case nodeValue of { Just value -> pure value; Nothing -> mempty }) <> foldMap globalReferenceNames nodeValue"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        renderedSource <-
          expectRightWithLabel
            "render"
            (renderFixtureModule "Fixture" convertedModule)
        assertBool
          ("case expression did not use compact brace layout:\n" <> renderedSource)
          ("case nodeValue of { Just value -> pure value; Nothing -> mempty }" `isInfixOf` renderedSource)
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      roundTripCase
        "left and right sections"
        [ "module Fixture where",
          "",
          "increment = (1 +)",
          "",
          "halved = (`div` 2)"
        ],
      roundTripCase
        "if-then-else"
        [ "module Fixture where",
          "",
          "choose c = if c then trueBranch else falseBranch"
        ],
      roundTripCase
        "multi-way if with boolean guards round-trips"
        [ "module Fixture where",
          "",
          "choose x = if | isSmall x -> small",
          "              | isBig x -> big",
          "              | otherwise -> unknown"
        ],
      roundTripCase
        "multi-way if with pattern guard round-trips"
        [ "module Fixture where",
          "",
          "pick source = if | Just y <- lookupThing source -> y",
          "                 | otherwise -> fallback"
        ],
      roundTripCase
        "expression type signature in argument position round-trips"
        [ "module Fixture where",
          "",
          "typedArg x = apply (x :: Int)"
        ],
      roundTripCase
        "type applications round-trip"
        [ "module Fixture where",
          "",
          "typedApps f = pair (f @Int) (f @(Maybe a))"
        ],
      roundTripCase
        "lists and tuples"
        [ "module Fixture where",
          "",
          "trio = [1, 2, 3]",
          "",
          "pair = (1, \"two\")"
        ],
      roundTripCase
        "record construction"
        [ "module Fixture where",
          "",
          "settings = MkSettings { width = 3, label = \"wide\" }"
        ],
      roundTripCase
        "record update"
        [ "module Fixture where",
          "",
          "widen settings = settings { width = 4, label = \"wider\" }"
        ],
      roundTripCase
        "record patterns round-trip"
        [ "module Fixture where",
          "",
          "recordPat value = case value of { MkRec {left = Just a, right = (b, c)} -> combine a b c; EmptyRec {} -> empty; _ -> fallback }"
        ],
      roundTripCase
        "record pun pattern round-trips"
        [ "module Fixture where",
          "",
          "recordPun value = case value of { MkRec {field} -> field; _ -> fallback }"
        ],
      roundTripCase
        "arithmetic sequences"
        [ "module Fixture where",
          "",
          "open = [0 ..]",
          "",
          "steppedOpen = [0, 2 ..]",
          "",
          "closed = [0 .. 10]",
          "",
          "steppedClosed = [0, 2 .. 10]"
        ],
      roundTripCase
        "negation"
        [ "module Fixture where",
          "",
          "invert x = -x"
        ],
      roundTripCase
        "character, string, and numeric literals"
        [ "module Fixture where",
          "",
          "letter = 'c'",
          "",
          "greeting = \"hello\"",
          "",
          "answer = 42",
          "",
          "ratio = 2.5"
        ],
      roundTripCase
        "symbolic top-level definition"
        [ "module Fixture where",
          "",
          "(<+>) = \\x -> x"
        ],
      roundTripCase
        "nested let and shadow-style reuse"
        [ "module Fixture where",
          "",
          "shadow = let g = \\x -> use x x in g alpha"
        ],
      roundTripCase
        "constructor-pattern case alternatives round-trip"
        [ "module Fixture where",
          "",
          "unwrap m = case m of { Just x -> x; Nothing -> fallback }"
        ],
      roundTripCase
        "nested constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "nested x = case x of { Just (Left y) -> y; _ -> other }"
        ],
      roundTripCase
        "as-patterns round-trip"
        [ "module Fixture where",
          "",
          "asPat x = case x of { all@(Just y) -> use all y; Nothing -> base }"
        ],
      roundTripCase
        "list patterns round-trip"
        [ "module Fixture where",
          "",
          "listPat xs = case xs of { [a, b] -> combine a b; _ -> empty }"
        ],
      roundTripCase
        "tuple-inside-constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "tupCon x = case x of { Just (a, b) -> pair a b; Nothing -> base }"
        ],
      roundTripCase
        "integer literal alternatives round-trip"
        [ "module Fixture where",
          "",
          "classify n = case n of { 0 -> zero; _ -> other }"
        ],
      roundTripCase
        "character and string literal alternatives round-trip"
        [ "module Fixture where",
          "",
          "tag c = case c of { 'a' -> alpha; 'b' -> beta; _ -> other }",
          "",
          "named s = case s of { \"yes\" -> true; _ -> false }"
        ],
      roundTripCase
        "infix constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "headTail xs = case xs of { (h : t) -> use h t; [] -> base }"
        ],
      roundTripCase
        "bang patterns in case alternatives round-trip"
        [ "module Fixture where",
          "",
          "strict x = case x of { !y -> use y }"
        ],
      roundTripCase
        "wildcard alternatives round-trip"
        [ "module Fixture where",
          "",
          "ignore x = case x of { _ -> constant }"
        ],
      roundTripCase
        "do-bind with constructor pattern round-trips"
        [ "module Fixture where",
          "",
          "run action = do { Just v <- action; pure v }"
        ],
      roundTripCase
        "adversarial tuple-of-constructor-and-list pattern round-trips"
        [ "module Fixture where",
          "",
          "adversarial x = case x of { (Just a, [b, c]) -> combine a b c; _ -> base }"
        ],
      roundTripCase
        "guarded otherwise chain round-trips"
        [ "module Fixture where",
          "",
          "choose x",
          "  | isPrimary x = primary",
          "  | otherwise = secondary"
        ],
      roundTripCase
        "multi-alternative boolean guard chain round-trips"
        [ "module Fixture where",
          "",
          "traffic signal",
          "  | isRed signal = stop",
          "  | isYellow signal = caution",
          "  | isGreen signal = go",
          "  | otherwise = unknown"
        ],
      roundTripCase
        "pattern guard round-trips"
        [ "module Fixture where",
          "",
          "lookupValue x",
          "  | Just y <- lookupThing x = y",
          "  | otherwise = fallback"
        ],
      roundTripCase
        "let guard round-trips"
        [ "module Fixture where",
          "",
          "letGuard x",
          "  | let { y = normalize x } = y"
        ],
      roundTripCase
        "guarded case alternative round-trips"
        [ "module Fixture where",
          "",
          "select m = case m of { Just x | valid x -> x; _ -> fallback }"
        ],
      roundTripCase
        "case alternative where round-trips"
        [ "module Fixture where",
          "",
          "select m = case m of { Just x -> use x y where { y = derive x }; Nothing -> fallback }"
        ],
      roundTripCase
        "guarded binding with multiple arguments round-trips"
        [ "module Fixture where",
          "",
          "combineGuard a b",
          "  | ok a b = pair a b",
          "  | otherwise = fallback a b"
        ],
      roundTripCase
        "guarded where round-trips"
        [ "module Fixture where",
          "",
          "f x",
          "  | isBig x = large y",
          "  | otherwise = small y",
          "  where",
          "    y = derive x"
        ],
      roundTripCase
        "clauses multi-clause recursion round-trips"
        [ "module Fixture where",
          "",
          "factorial 0 = 1",
          "factorial n = times n (factorial (minus n one))"
        ],
      roundTripCase
        "clauses multi-clause constructor patterns round-trip"
        [ "module Fixture where",
          "",
          "unwrap (Just x) = x",
          "unwrap Nothing = fallback"
        ],
      roundTripCase
        "clauses pattern lambda in expression position round-trips"
        [ "module Fixture where",
          "",
          "mapper = apply (\\(Just x) -> use x)"
        ],
      roundTripCase
        "clauses lambda-case multi-alternative round-trips"
        [ "module Fixture where",
          "",
          "handler = \\case { Just x -> use x; Nothing -> fallback }"
        ],
      roundTripCase
        "clauses lambda-cases two-pattern round-trips"
        [ "module Fixture where",
          "",
          "combiner = \\cases { (Just x) (Just y) -> pair x y; _ _ -> fallback }"
        ],
      roundTripCase
        "clauses guarded multi-clause binding round-trips"
        [ "module Fixture where",
          "",
          "classify x",
          "  | isBig x = large",
          "classify y = small y"
        ],
      roundTripCase
        "clause where under multi-clause definition round-trips"
        [ "module Fixture where",
          "",
          "choose 0 = zero",
          "choose n = combine n y",
          "  where",
          "    y = derive n"
        ],
      testCase "clauses multi-clause definition converts without opaque lambda match group" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "factorial 0 = 1",
                      "factorial n = times n (factorial (minus n one))"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case tlbTerm bindingValue of
          PatternNode (ClausesF clauseValues) -> do
            length clauseValues @?= 2
            case fmap fst clauseValues of
              [[POverLitP (NormalizedIntegralOverLit zeroValue)], [PVarP _]]
                | exactIntegralValue zeroValue == 0 ->
                pure ()
              patternShapes ->
                assertFailure ("expected literal and variable clause patterns, got " <> show patternShapes)
          otherTerm ->
            assertFailure ("expected a ClausesF multi-clause binding, got " <> show otherTerm),
      testCase "clauses var-only single-clause top-level rendering refuses lam territory" $ do
        let binderAnn = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "x"))
            bodyValue :: Pattern HsExprF
            bodyValue = PatternNode (VarF (LocalName binderAnn))
        renderSourceString
          CompactLayout
          ( RenderNamedRewriteBinding
              "identity"
              (PatternNode (ClausesF [([PVarP binderAnn], bodyValue)]))
          )
          @?= Left RenderClausesShape,
      testCase "expression lambda remains on the rhs and reconverts" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "f = \\x -> use x"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        renderedSource <-
          expectRightWithLabel
            "render"
            (renderFixtureModule "Fixture" convertedModule)
        renderedSource @?= unlines ["module Fixture where", "", "f = \\x -> use x"]
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      testCase "guarded where renders compactly and reconverts" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "f x | isBig x = large y | otherwise = small y where y = derive x"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        renderedSource <-
          expectRightWithLabel
            "render"
            (renderFixtureModule "Fixture" convertedModule)
        renderedSource
          @?= unlines
            [ "module Fixture where",
              "",
              "f x | isBig x = large y | otherwise = small y where { y = derive x }"
            ]
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      testCase "guarded binding converts without opaque fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "lookupValue x",
                      "  | Just y <- lookupThing x = y"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (GuardedF [GuardedAltF [GuardPatF (PConP _ [PVarP guardBinder]) _] (PatternNode (VarF (LocalName bodyBinder)))]) ->
            occNameString (rdrNameOcc (baName guardBinder)) @?= occNameString (rdrNameOcc (baName bodyBinder))
          otherBody ->
            assertFailure ("expected a pattern-guarded body, got " <> show otherBody),
      testCase "multi-way if pattern guard converts without opaque fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "pick source = if | Just y <- lookupThing source -> y",
                      "                 | otherwise -> fallback"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (MultiIfF [GuardedAltF [GuardPatF (PConP _ [PVarP guardBinder]) _] (PatternNode (VarF (LocalName bodyBinder))), GuardedAltF [GuardBoolF _] _]) ->
            occNameString (rdrNameOcc (baName guardBinder)) @?= occNameString (rdrNameOcc (baName bodyBinder))
          otherBody ->
            assertFailure ("expected a pattern-guarded multi-way if, got " <> show otherBody),
      testCase "type syntax constructors convert without opaque fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "typed f x = pair (apply (x :: Int)) (pair (f @Int) (f @(Maybe a)))"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        assertBool
          "fixture must contain an expression type signature node"
          (patternContainsExprWithTySig (tlbTerm bindingValue))
        assertBool
          "fixture must contain visible type application nodes"
          (patternContainsAppType (tlbTerm bindingValue)),
      testCase "record patterns convert to field rows without lossy fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "recordPat value = case value of { MkRec {left = Just a, right = (b, c)} -> combine a b c; EmptyRec {} -> empty; _ -> fallback }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (CaseF _ branchValues) ->
            case fmap (stripTestPatParens . fst) branchValues of
              [ PRecP _
                  [ HsRecPatField leftName (HsRecPatExplicit (PConP _ [PVarP _])),
                    HsRecPatField rightName (HsRecPatExplicit (PTupleP BoxedTuple [PVarP _, PVarP _]))
                    ],
                PRecP _ [],
                PWildP
                ] -> do
                  fmap (occNameString . rdrNameOcc) [leftName, rightName]
                    @?= ["left", "right"]
              alternativePatterns ->
                assertFailure ("expected faithful record pattern rows, got " <> show alternativePatterns)
          otherBody ->
            assertFailure ("expected a case expression with record patterns, got " <> show otherBody),
      testCase "record pun syntax and binder identity are preserved" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "recordPun value = case value of { MkRec {field} -> field; _ -> fallback }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode
            ( CaseF
                _
                [ (PRecP _ [HsRecPatField fieldName (HsRecPatPun punBinder)], PatternNode (VarF (LocalName bodyBinder))),
                  (PWildP, _)
                  ]
              ) -> do
                occNameString (rdrNameOcc fieldName) @?= "field"
                baId punBinder @?= baId bodyBinder
          bodyPattern ->
            assertFailure
              ("expected a preserved record pun and local body reference, got " <> show bodyPattern)
        renderedSource <-
          expectRightWithLabel
            "render"
            (renderFixtureModule "Fixture" convertedModule)
        assertBool
          ("record pun must remain pun syntax:\n" <> renderedSource)
          ( "MkRec {field}" `isInfixOf` renderedSource
              && not ("field = field" `isInfixOf` renderedSource)
              && "{-# LANGUAGE NamedFieldPuns #-}" `isInfixOf` renderedSource
          )
        reparsedModule <- expectRightWithLabel "re-parse of rendered source" (convertHaskellSource "Fixture.hs" renderedSource)
        reparsedBinding <- singleBinding reparsedModule
        assertRoundTripBinding renderedSource (bindingValue, reparsedBinding),
      testCase "record wildcard patterns require region-bearing field evidence" $
        case
            convertHaskellSource
              "Fixture.hs"
              ( unlines
                  [ "module Fixture where",
                    "",
                    "recordWildcard value = case value of { MkRec {..} -> fallback }"
                  ]
              )
          of
            Left
              ( ConvertRecordWildcardResolutionUnavailable
                  wildcardRegion
                  (RecordWildcardConstructorUnavailable constructorName)
                ) -> do
                  srStartLine wildcardRegion @?= 3
                  occNameString (rdrNameOcc constructorName) @?= "MkRec"
            Left obstruction ->
              assertFailure
                ("expected missing record-wildcard field evidence, got " <> show obstruction)
            Right _ ->
              assertFailure "expected missing record-wildcard field evidence, got successful conversion",
      testCase "resolved wildcard-only patterns mint real local binders in definition order" $ do
        let recordEnvironment =
              recordFieldEnvironmentFromStrings
                [("MkRec", ["left", "right"])]
            sourceText =
              unlines
                [ "{-# LANGUAGE RecordWildCards #-}",
                  "module WildcardOnly where",
                  "recordWildcard value = case value of { MkRec {..} -> combine left right }"
                ]
        convertedModule <-
          expectRightWithLabel
            "resolved wildcard-only conversion"
            ( convertHaskellSourceWithRecordFieldEnvironment
                recordEnvironment
                "WildcardOnly.hs"
                sourceText
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode
            ( CaseF
                _
                [(recordPattern@(PRecP _ [HsRecPatWildcard wildcardRegion wildcardBinders]), branchBody)]
              ) -> do
                srStartLine wildcardRegion @?= 3
                fmap (occNameString . rdrNameOcc . baName) wildcardBinders
                  @?= ["left", "right"]
                fmap (occNameString . rdrNameOcc . baName) (patBinders recordPattern)
                  @?= ["left", "right"]
                Set.fromList (patternLocalReferences branchBody)
                  @?= Set.fromList (fmap baId wildcardBinders)
                Set.intersection
                  (Set.fromList ["left", "right"])
                  (Set.fromList (patternGlobalReferenceNames branchBody))
                  @?= Set.empty
          bodyPattern ->
            assertFailure
              ("expected a wildcard-only record pattern, got " <> show bodyPattern),
      testCase "mixed explicit, pun, and wildcard items preserve order, binders, headers, and round-trip syntax" $ do
        let recordEnvironment =
              recordFieldEnvironmentFromStrings
                [("MkRec", ["explicit", "pun", "implicit", "later"])]
            sourceText =
              unlines
                [ "{-# LANGUAGE NamedFieldPuns #-}",
                  "{-# LANGUAGE RecordWildCards #-}",
                  "{-# LANGUAGE MultiWayIf #-}",
                  "module MixedRecordWildcard where",
                  "record value = case value of { MkRec {explicit = renamed, pun, ..} -> if | condition -> combine renamed pun implicit later }"
                ]
        convertedModule <-
          expectRightWithLabel
            "mixed record-wildcard conversion"
            ( convertHaskellSourceWithRecordFieldEnvironment
                recordEnvironment
                "MixedRecordWildcard.hs"
                sourceText
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode
            ( CaseF
                _
                [ ( recordPattern@(PRecP
                          _
                          [ HsRecPatField explicitName (HsRecPatExplicit (PVarP explicitBinder)),
                            HsRecPatField punName (HsRecPatPun punBinder),
                            HsRecPatWildcard wildcardRegion wildcardBinders
                          ]
                      ),
                    branchBody
                    )
                  ]
              ) -> do
                fmap (occNameString . rdrNameOcc) [explicitName, punName]
                  @?= ["explicit", "pun"]
                srStartLine wildcardRegion @?= 5
                fmap (occNameString . rdrNameOcc . baName) wildcardBinders
                  @?= ["implicit", "later"]
                fmap (occNameString . rdrNameOcc . baName) (patBinders recordPattern)
                  @?= ["renamed", "pun", "implicit", "later"]
                Set.fromList (patternLocalReferences branchBody)
                  @?= Set.fromList
                    (fmap baId (explicitBinder : punBinder : wildcardBinders))
                Set.intersection
                  (Set.fromList ["renamed", "pun", "implicit", "later"])
                  (Set.fromList (patternGlobalReferenceNames branchBody))
                  @?= Set.empty
          bodyPattern ->
            assertFailure
              ("expected ordered mixed record items, got " <> show bodyPattern)
        assertSpannedBindingLockstep (cmScopeIndex convertedModule) bindingValue
        compactSource <-
          expectRightWithLabel
            "compact mixed wildcard render"
            (renderFixtureModule "MixedRecordWildcard" convertedModule)
        prettySource <-
          expectRightWithLabel
            "pretty mixed wildcard render"
            ( renderSourceString
                (PrettyLayout defaultPageWidth)
                ( RenderConvertedModule
                    (ModuleRenderContext "" (Just "MixedRecordWildcard"))
                    convertedModule
                )
            )
        traverse_
          ( \renderedSource -> do
              assertBool
                ("generated header must contain NamedFieldPuns exactly once:\n" <> renderedSource)
                (countSubstring "{-# LANGUAGE NamedFieldPuns #-}" renderedSource == 1)
              assertBool
                ("generated header must contain RecordWildCards exactly once:\n" <> renderedSource)
                (countSubstring "{-# LANGUAGE RecordWildCards #-}" renderedSource == 1)
              assertBool
                ("generated header must compose MultiWayIf exactly once:\n" <> renderedSource)
                (countSubstring "{-# LANGUAGE MultiWayIf #-}" renderedSource == 1)
              assertBool
                ("mixed record syntax must retain pun and wildcard items:\n" <> renderedSource)
                ("MkRec {explicit = renamed, pun, ..}" `isInfixOf` renderedSource)
              reparsedModule <-
                expectRightWithLabel
                  ("mixed wildcard reparse:\n" <> renderedSource)
                  ( convertHaskellSourceWithRecordFieldEnvironment
                      recordEnvironment
                      "MixedRecordWildcard.hs"
                      renderedSource
                  )
              reparsedBinding <- singleBinding reparsedModule
              assertRoundTripBinding renderedSource (bindingValue, reparsedBinding)
          )
          [compactSource, prettySource],
      testCase "ambiguous constructor evidence obstructs locally without poisoning unrelated constructors" $ do
        let ambiguousEnvironment =
              recordFieldEnvironmentFromStrings
                [ ("MkRec", ["left"]),
                  ("MkRec", ["right"])
                ]
            unrelatedAmbiguityEnvironment =
              recordFieldEnvironmentFromStrings
                [ ("Other", ["first"]),
                  ("Other", ["second"]),
                  ("MkRec", ["left"])
                ]
            sourceText =
              unlines
                [ "{-# LANGUAGE RecordWildCards #-}",
                  "module AmbiguousWildcard where",
                  "record value = case value of { MkRec {..} -> left }"
                ]
        case
            convertHaskellSourceWithRecordFieldEnvironment
              ambiguousEnvironment
              "AmbiguousWildcard.hs"
              sourceText
          of
            Left
              ( ConvertRecordWildcardResolutionUnavailable
                  wildcardRegion
                  (RecordWildcardConstructorAmbiguous constructorName)
                ) -> do
                  srStartLine wildcardRegion @?= 3
                  occNameString (rdrNameOcc constructorName) @?= "MkRec"
            Left obstruction ->
              assertFailure
                ("expected ambiguous record-wildcard evidence, got " <> show obstruction)
            Right _ ->
              assertFailure "expected ambiguous record-wildcard evidence, got successful conversion"
        convertedModule <-
          expectRightWithLabel
            "unrelated ambiguity conversion"
            ( convertHaskellSourceWithRecordFieldEnvironment
                unrelatedAmbiguityEnvironment
                "AmbiguousWildcard.hs"
                sourceText
            )
        bindingValue <- singleBinding convertedModule
        patternLocalReferences (tlbTerm bindingValue) @?= [BinderId 1, BinderId 2],
      testCase "qualified record puns preserve qualified field identity and an unqualified local binder" $ do
        convertedModule <-
          expectRightWithLabel
            "qualified record pun conversion"
            ( convertHaskellSource
                "QualifiedRecordPun.hs"
                ( unlines
                    [ "{-# LANGUAGE NamedFieldPuns #-}",
                      "module QualifiedRecordPun where",
                      "record value = case value of { MkRec { Qualified.field } -> field }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode
            ( CaseF
                _
                [ (PRecP _ [HsRecPatField fieldName (HsRecPatPun punBinder)], PatternNode (VarF (LocalName bodyBinder)))
                  ]
              ) -> do
                renderRdrName fieldName @?= "Qualified.field"
                occNameString (rdrNameOcc (baName punBinder)) @?= "field"
                baId punBinder @?= baId bodyBinder
          bodyPattern ->
            assertFailure
              ("expected a qualified field key with an unqualified pun binder, got " <> show bodyPattern)
        renderedSource <-
          expectRightWithLabel
            "qualified record pun render"
            (renderFixtureModule "QualifiedRecordPun" convertedModule)
        assertBool
          ("qualified record pun must remain qualified:\n" <> renderedSource)
          ("MkRec {Qualified.field}" `isInfixOf` renderedSource),
      testCase "render-round-trip record equivalence is ordered and syntax-structural" $ do
        let constructorName = mkRdrUnqual (mkDataOcc "MkRec")
            firstField = mkRdrUnqual (mkVarOcc "first")
            secondField = mkRdrUnqual (mkVarOcc "second")
            leftFirst = BinderAnn (BinderId 10) (mkRdrUnqual (mkVarOcc "leftFirst"))
            leftSecond = BinderAnn (BinderId 11) (mkRdrUnqual (mkVarOcc "leftSecond"))
            rightFirst = BinderAnn (BinderId 20) (mkRdrUnqual (mkVarOcc "rightFirst"))
            rightSecond = BinderAnn (BinderId 21) (mkRdrUnqual (mkVarOcc "rightSecond"))
            leftRegion = SourceRegion 1 1 1 3
            rightRegion = SourceRegion 9 4 9 6
            clauseTerm recordItems bodyBinder =
              PatternNode
                ( ClausesF
                    [ ( [PRecP constructorName recordItems],
                        PatternNode (VarF (LocalName bodyBinder))
                      )
                    ]
                )
            orderedLeft =
              [ HsRecPatField firstField (HsRecPatExplicit (PVarP leftFirst)),
                HsRecPatField secondField (HsRecPatExplicit (PVarP leftSecond))
              ]
            orderedRight =
              [ HsRecPatField firstField (HsRecPatExplicit (PVarP rightFirst)),
                HsRecPatField secondField (HsRecPatExplicit (PVarP rightSecond))
              ]
            reorderedRight =
              [ HsRecPatField secondField (HsRecPatExplicit (PVarP rightSecond)),
                HsRecPatField firstField (HsRecPatExplicit (PVarP rightFirst))
              ]
            punLeft =
              [HsRecPatField firstField (HsRecPatPun leftFirst)]
            explicitRight =
              [HsRecPatField firstField (HsRecPatExplicit (PVarP rightFirst))]
            wildcardLeft =
              [HsRecPatWildcard leftRegion [leftFirst]]
            wildcardRight =
              [HsRecPatWildcard rightRegion [rightFirst]]
        assertBool
          "ordered explicit record items must alpha-compare"
          (renderRoundTripEquivalent (clauseTerm orderedLeft leftFirst) (clauseTerm orderedRight rightFirst))
        assertBool
          "record field reordering is not render-round-trip syntax equivalence"
          (not (renderRoundTripEquivalent (clauseTerm orderedLeft leftFirst) (clauseTerm reorderedRight rightFirst)))
        assertBool
          "pun syntax does not collapse into explicit-self syntax"
          (not (renderRoundTripEquivalent (clauseTerm punLeft leftFirst) (clauseTerm explicitRight rightFirst)))
        assertBool
          "wildcard token regions are provenance, not syntax identity"
          (renderRoundTripEquivalent (clauseTerm wildcardLeft leftFirst) (clauseTerm wildcardRight rightFirst))
        assertBool
          "wildcard presence cannot disappear"
          (not (renderRoundTripEquivalent (clauseTerm wildcardLeft leftFirst) (clauseTerm [] rightFirst)))
        assertBool
          "wildcard position is syntax-structural"
          ( not
              ( renderRoundTripEquivalent
                  (clauseTerm (orderedLeft <> wildcardLeft) leftFirst)
                  (clauseTerm (wildcardRight <> orderedRight) rightFirst)
              )
          ),
      testCase "guard pattern equivalence uses the same ordered record relation" $ do
        let constructorName = mkRdrUnqual (mkDataOcc "MkRec")
            punField = mkRdrUnqual (mkVarOcc "punField")
            wildcardField = mkRdrUnqual (mkVarOcc "wildcardField")
            scrutineeName = mkRdrUnqual (mkVarOcc "scrutinee")
            leftPun = BinderAnn (BinderId 30) punField
            leftWildcard = BinderAnn (BinderId 31) wildcardField
            rightPun = BinderAnn (BinderId 40) punField
            rightWildcard = BinderAnn (BinderId 41) wildcardField
            leftPattern =
              PRecP
                constructorName
                [ HsRecPatField punField (HsRecPatPun leftPun),
                  HsRecPatWildcard (SourceRegion 2 5 2 7) [leftWildcard]
                ]
            rightPattern =
              PRecP
                constructorName
                [ HsRecPatField punField (HsRecPatPun rightPun),
                  HsRecPatWildcard (SourceRegion 8 3 8 5) [rightWildcard]
                ]
            scrutinee =
              PatternNode (VarF (GlobalName scrutineeName))
            localReference binderAnn =
              PatternNode (VarF (LocalName binderAnn))
            leftPatternGuards =
              [ GuardPatF leftPattern scrutinee,
                GuardBoolF (localReference leftWildcard)
              ]
            rightPatternGuards =
              [ GuardPatF rightPattern scrutinee,
                GuardBoolF (localReference rightWildcard)
              ]
            leftLetGuards =
              [ GuardLetF NonRecursiveBinds [(leftPattern, scrutinee)],
                GuardBoolF (localReference leftPun)
              ]
            rightLetGuards =
              [ GuardLetF NonRecursiveBinds [(rightPattern, scrutinee)],
                GuardBoolF (localReference rightPun)
              ]
        assertBool
          "GuardPatF threads pun and wildcard binders through the Pale relation"
          (renderRoundTripGuardStatementsEquivalent leftPatternGuards rightPatternGuards)
        assertBool
          "GuardLetF threads pun and wildcard binders through the Pale relation"
          (renderRoundTripGuardStatementsEquivalent leftLetGuards rightLetGuards),
      testCase "constructor-pattern case alternatives convert to PConP, not lossy shapes" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "unwrap m = case m of { Just x -> x; Nothing -> fallback }"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case stripBindingLambdas (tlbTerm bindingValue) of
          PatternNode (CaseF _ branchValues) ->
            case fmap fst branchValues of
              [PConP _ [PVarP _], PConP _ []] ->
                pure ()
              alternativePatterns ->
                assertFailure
                  ("expected faithful constructor patterns, got " <> show alternativePatterns)
          _ ->
            assertFailure "expected the fixture body to convert to a case expression",
      testCase "binder-bearing view patterns are typed conversion obstructions" $
        case
            convertHaskellSource
              "Fixture.hs"
              ( unlines
                  [ "module Fixture where",
                    "",
                    "viewed x = case x of { (project -> y) -> use y }"
                  ]
              )
          of
            Left (ConvertUnsupportedPattern (Just _) PatOpaqueView) ->
              pure ()
            _ ->
              assertFailure "expected a region-bearing view-pattern obstruction",
      testCase "unsupported pattern kinds remain distinct typed obstructions" $ do
        let viewResult =
              convertHaskellSource
                "Fixture.hs"
                (unlines ["module Fixture where", "lossy x = case x of { (project -> y) -> use y }"])
            plusKResult =
              convertHaskellSource
                "Fixture.hs"
                (unlines ["{-# LANGUAGE NPlusKPatterns #-}", "module Fixture where", "lossy x = case x of { (y + 1) -> use y }"])
        case (viewResult, plusKResult) of
          ( Left (ConvertUnsupportedPattern _ PatOpaqueView),
            Left (ConvertUnsupportedPattern _ PatOpaqueNPlusK)
            ) ->
              pure ()
          _ ->
            assertFailure "expected distinct view and n-plus-k obstructions",
      testCase "where pattern bindings convert without opaque local-binds fallback" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "clear x = combine a b where (a, b) = splitPair x"
                    ]
                )
            )
        bindingValue <- singleBinding convertedModule
        case tlbBinding bindingValue of
          FunctionBinding _ (Clause _ (UnguardedRhs _ (Just bindingGroup)) :| []) ->
            case bindingGroupBindings bindingGroup of
              PatternBinding headPattern _ :| [] ->
                case stripTestPatParens headPattern of
                  PTupleP BoxedTuple [PVarP _, PVarP _] ->
                    pure ()
                  otherPattern ->
                    assertFailure ("expected a tuple pattern where binding, got " <> show otherPattern)
              otherBindings ->
                assertFailure ("expected one pattern binding in the canonical where group, got " <> show otherBindings)
          otherBinding ->
            assertFailure ("expected a function binding with a canonical where group, got " <> show otherBinding),
      testCase "var let binding rows render byte-identically" $ do
        let binderAnn = BinderAnn (BinderId 0) (mkRdrUnqual (mkVarOcc "y"))
        renderSourceString
          CompactLayout
          ( RenderRewriteExpression
              ( PatternNode
                  ( LetF
                      NonRecursiveBinds
                      [(PVarP binderAnn, PatternNode (OverLitF (NormalizedIntegralOverLit (exactIntegralFromInteger 1))))]
                      (PatternNode (VarF (LocalName binderAnn)))
                  )
              )
          )
          @?= Right "let y = 1 in y",
      testCase "render refuses pattern variables and empty names" $ do
        renderSourceString
          CompactLayout
          (RenderRewriteExpression (PatternVar (EGraph.mkPatternVar 0)))
          @?= Left RenderPatternVariable
        renderSourceString
          CompactLayout
          ( RenderNamedRewriteBinding
              ""
              (PatternNode (OverLitF (NormalizedIntegralOverLit (exactIntegralFromInteger 1))))
          )
          @?= Left RenderEmptyBindingName,
      testCase "prim literals render with their hash-suffixed forms" $ do
        renderSourceString
          CompactLayout
          (RenderRewriteExpression (PatternNode (LitF (NormalizedIntPrim (exactIntegralFromInteger 5)))))
          @?= Right "5#"
        renderSourceString
          CompactLayout
          (RenderRewriteExpression (PatternNode (LitF (NormalizedWordPrim (exactIntegralFromInteger 5)))))
          @?= Right "5##"
        renderSourceString
          CompactLayout
          (RenderRewriteExpression (PatternNode (LitF (NormalizedDoublePrim (exactFractionalFromRational (5 / 2))))))
          @?= Right "2.5##"
        renderSourceString
          CompactLayout
          (RenderRewriteExpression (PatternNode (LitF (NormalizedStringPrim (ByteString.pack [102, 111, 111, 0, 255])))))
          @?= Right "\"foo\\x0\\&\\xff\\&\"#",
      testCase "top-level bindings carry ordered source regions" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "first = 1",
                      "",
                      "second x = x"
                    ]
                )
            )
        case fmap tlbRegion (convertedModuleBindings convertedModule) of
          [Just firstRegion, Just secondRegion] -> do
            srStartLine firstRegion @?= 3
            srStartLine secondRegion @?= 5
            projectedBindings <-
              expectRightWithLabel
                "annotated binding projections"
                (traverse (bindingExpr (cmScopeIndex convertedModule)) (convertedModuleBindings convertedModule))
            fmap exprRegion projectedBindings @?= [Just firstRegion, Just secondRegion]
            assertBool
              "regions must not overlap"
              (srEndLine firstRegion <= srStartLine secondRegion)
          regions ->
            assertFailure ("expected two located bindings, got " <> show regions)
    ]

spanLockstepTests :: TestTree
spanLockstepTests =
  testGroup
    "pale.spans.lockstep"
    [ testCase "the canonical annotated tree erases across renderable expression shapes" $ do
        convertedModule <-
          expectRightWithLabel
            "fixture conversion"
            ( convertHaskellSource
                "Fixture.hs"
                ( unlines
                    [ "module Fixture where",
                      "",
                      "lockstep flag action p = do { x <- action; let { y = case p of { (a, b) -> if flag then [a + b, -x] else [x]; _ -> [x] } }; pure (y, MkSettings { width = 3, label = \"wide\" }) }"
                    ]
                )
            )
        assertBool "fixture must contain at least one binding" (not (null (convertedModuleBindings convertedModule)))
        mapM_
          (assertSpannedBindingLockstep (cmScopeIndex convertedModule))
          (convertedModuleBindings convertedModule)
    ]

assertSpannedBindingLockstep :: ScopeIndex -> ConvertedValueBinding -> IO ()
assertSpannedBindingLockstep scopeIndex bindingValue = do
  projectedBinding <-
    expectRightWithLabel
      "annotated binding projection"
      (bindingExpr scopeIndex bindingValue)
  eraseExpr projectedBinding @?= tlbTerm bindingValue

expressionMetricOracle :: Expr -> ExpressionMetricOracle
expressionMetricOracle expressionValue =
  let nodeValue = exprNode expressionValue
      childMetrics = foldMap expressionMetricOracle nodeValue
      freeScopeCount = freeScopeSummarySize (exprFreeScopes expressionValue)
      (globalRefIncrement, localRefIncrement) =
        case nodeValue of
          VarF (GlobalName _) -> (1, 0)
          VarF (LocalName _) -> (0, 1)
          _ -> (0, 0)
   in ExpressionMetricOracle
        { oracleScopedExprCount =
            oracleScopedExprCount childMetrics + 1,
          oracleGlobalVarRefCount =
            oracleGlobalVarRefCount childMetrics + globalRefIncrement,
          oracleLocalVarRefCount =
            oracleLocalVarRefCount childMetrics + localRefIncrement,
          oracleMaxFreeScopeCount =
            max
              (oracleMaxFreeScopeCount childMetrics)
              freeScopeCount
        }

roundTripCase :: String -> [String] -> TestTree
roundTripCase caseName fixtureLines =
  testCase caseName $ do
    let sourceText = unlines fixtureLines
    convertedModule <- expectRightWithLabel "fixture conversion" (convertHaskellSource "Fixture.hs" sourceText)
    assertBool "fixture must contain at least one binding" (not (null (convertedModuleBindings convertedModule)))
    renderedSource <-
      expectRightWithLabel
        "render"
        (renderFixtureModule "Fixture" convertedModule)
    reparsedModule <-
      expectRightWithLabel
        ("re-parse of rendered source:\n" <> renderedSource)
        (convertHaskellSource "Fixture.hs" renderedSource)
    length (convertedModuleBindings reparsedModule) @?= length (convertedModuleBindings convertedModule)
    mapM_
      (assertRoundTripBinding renderedSource)
      (zip (convertedModuleBindings convertedModule) (convertedModuleBindings reparsedModule))

assertRoundTripBinding :: String -> (ConvertedValueBinding, ConvertedValueBinding) -> IO ()
assertRoundTripBinding renderedSource (originalBinding, reparsedBinding) = do
  bindingNameStrings originalBinding @?= bindingNameStrings reparsedBinding
  assertBool
    ( "binding "
        <> show (bindingNameStrings originalBinding)
        <> " is not round-trip equivalent; rendered source:\n"
        <> renderedSource
    )
    (renderRoundTripEquivalent (tlbTerm originalBinding) (tlbTerm reparsedBinding))

bindingNameStrings :: ConvertedValueBinding -> [String]
bindingNameStrings =
  fmap (occNameString . rdrNameOcc) . tlbNames

tlbNames :: ConvertedValueBinding -> [RdrName]
tlbNames =
  bindingNames . tlbBinding

tlbTerm :: ConvertedValueBinding -> Pattern HsExprF
tlbTerm =
  bindingPattern . tlbBinding

singleBinding :: ConvertedModule -> IO ConvertedValueBinding
singleBinding convertedModule =
  case convertedModuleBindings convertedModule of
    [bindingValue] -> pure bindingValue
    bindingValues -> assertFailure ("expected exactly one binding, got " <> show (length bindingValues))

assertSingletonBindingComponent ::
  String ->
  [String] ->
  Int ->
  BindingComponentRecursion ->
  IO ()
assertSingletonBindingComponent fixtureLabel bindingLines expectedBinderCount expectedRecursion = do
  convertedModule <-
    expectRightWithLabel
      fixtureLabel
      ( convertHaskellSource
          "SingletonBindingComponent.hs"
          (unlines ("module SingletonBindingComponent where" : bindingLines))
      )
  topLevelBinding <- singleBinding convertedModule
  bindingGroup <-
    case tlbBinding topLevelBinding of
      FunctionBinding _ (Clause _ rhsValue :| []) ->
        expectBindingGroup fixtureLabel rhsValue
      otherBinding ->
        assertFailure
          (fixtureLabel <> ": expected one-clause function binding, got " <> show otherBinding)
  case (bindingGroupBindings bindingGroup, bindingGroupComponents bindingGroup) of
    (localBinding :| [], componentValue :| []) -> do
      let expectedBinders =
            Set.toList (Set.fromList (localBindingBinderIds localBinding))
      length expectedBinders @?= expectedBinderCount
      bindingComponentRows componentValue @?= (0 :| [])
      bindingComponentBinders componentValue @?= expectedBinders
      bindingComponentDependencies componentValue @?= []
      bindingComponentRecursion componentValue @?= expectedRecursion
    (localBindings, componentValues) ->
      assertFailure
        ( fixtureLabel
            <> ": expected one local binding and one component, got "
            <> show (NonEmpty.length localBindings, NonEmpty.length componentValues)
        )

type BindingComponentEvidence =
  ( NonEmpty Int,
    [BinderId],
    [BinderId],
    BindingComponentRecursion
  )

assertBindingComponentsMatchGraphOracle ::
  (String, [String]) ->
  IO ()
assertBindingComponentsMatchGraphOracle (fixtureLabel, bindingLines) = do
  convertedModule <-
    expectRightWithLabel
      fixtureLabel
      ( convertHaskellSource
          "BindingComponentDifferential.hs"
          (unlines ("module BindingComponentDifferential where" : bindingLines))
      )
  topLevelBinding <- singleBinding convertedModule
  bindingGroup <-
    case tlbBinding topLevelBinding of
      FunctionBinding _ (Clause _ rhsValue :| []) ->
        expectBindingGroup fixtureLabel rhsValue
      PatternBinding _ rhsValue ->
        expectBindingGroup fixtureLabel rhsValue
      otherBinding ->
        assertFailure
          (fixtureLabel <> ": expected one-clause binding, got " <> show otherBinding)
  oracleComponents <-
    either
      (assertFailure . ((fixtureLabel <> ": ") <>))
      pure
      (genericBindingComponentOracle bindingGroup)
  fmap bindingComponentEvidence (NonEmpty.toList (bindingGroupComponents bindingGroup))
    @?= oracleComponents

genericBindingComponentOracle ::
  BindingGroup ->
  Either String [BindingComponentEvidence]
genericBindingComponentOracle bindingGroup =
  traverse
    componentEvidenceFromScc
    (stronglyConnComp dependencyNodes)
  where
    indexedBindings =
      zip
        [0 :: Int ..]
        (NonEmpty.toList (bindingGroupBindings bindingGroup))
    binderOwnerRows =
      Map.fromList
        [ (binderId, rowIndex)
        | (rowIndex, bindingValue) <- indexedBindings,
          binderId <- localBindingBinderIds bindingValue
        ]
    groupBinderIds =
      Map.keysSet binderOwnerRows
    dependencyNodes =
      fmap bindingDependencyNode indexedBindings
    bindingDependencyNode (rowIndex, bindingValue) =
      let bindingIds =
            localBindingBinderIds bindingValue
          dependencyIds =
            Set.toList
              ( Set.intersection
                  groupBinderIds
                  (Set.fromList (bindingLocalReferences bindingValue))
              )
          dependencyRows =
            Set.toList
              ( Set.fromList
                  ( foldMap
                      (\binderId -> maybe [] (: []) (Map.lookup binderId binderOwnerRows))
                      dependencyIds
                  )
              )
       in ( (rowIndex, bindingIds, dependencyIds),
            rowIndex,
            dependencyRows
          )

componentEvidenceFromScc ::
  SCC (Int, [BinderId], [BinderId]) ->
  Either String BindingComponentEvidence
componentEvidenceFromScc = \case
  AcyclicSCC rowPayload ->
    Right (componentEvidenceFromRows (rowPayload :| []) AcyclicBindingComponent)
  CyclicSCC rowPayloads ->
    maybe
      (Left "generic SCC oracle returned an empty cyclic component")
      (Right . (`componentEvidenceFromRows` RecursiveBindingComponent))
      (NonEmpty.nonEmpty rowPayloads)

componentEvidenceFromRows ::
  NonEmpty (Int, [BinderId], [BinderId]) ->
  BindingComponentRecursion ->
  BindingComponentEvidence
componentEvidenceFromRows rowPayloads recursionValue =
  let componentRows =
        fmap (\(rowIndex, _, _) -> rowIndex) rowPayloads
      componentBinders =
        Set.toList
          ( foldMap
              (Set.fromList . (\(_, binderIds, _) -> binderIds))
              rowPayloads
          )
      binderSet =
        Set.fromList componentBinders
      externalDependencies =
        foldMap
          (Set.fromList . (\(_, _, dependencyIds) -> dependencyIds))
          rowPayloads
          `Set.difference` binderSet
   in ( componentRows,
        componentBinders,
        Set.toList externalDependencies,
        recursionValue
      )

bindingComponentEvidence :: BindingComponent -> BindingComponentEvidence
bindingComponentEvidence componentValue =
  ( bindingComponentRows componentValue,
    bindingComponentBinders componentValue,
    bindingComponentDependencies componentValue,
    bindingComponentRecursion componentValue
  )

bindingLocalReferences :: Binding -> [BinderId]
bindingLocalReferences =
  patternLocalReferences . bindingPattern

patternLocalReferences :: Pattern HsExprF -> [BinderId]
patternLocalReferences = \case
  PatternVar _ ->
    []
  PatternNode nodeValue ->
    [baId binderAnn | VarF (LocalName binderAnn) <- [nodeValue]]
      <> foldMap patternLocalReferences nodeValue

patternGlobalReferenceNames :: Pattern HsExprF -> [String]
patternGlobalReferenceNames = \case
  PatternVar _ ->
    []
  PatternNode nodeValue ->
    [occNameString (rdrNameOcc globalName) | VarF (GlobalName globalName) <- [nodeValue]]
      <> foldMap patternGlobalReferenceNames nodeValue

recordFieldEnvironmentFromStrings ::
  [(String, [String])] ->
  RecordFieldEnvironment
recordFieldEnvironmentFromStrings =
  recordFieldEnvironmentFromDefinitions
    . fmap
      ( \(constructorName, fieldNames) ->
          ( mkRdrUnqual (mkDataOcc constructorName),
            fmap (mkRdrUnqual . mkVarOcc) fieldNames
          )
      )

countSubstring :: String -> String -> Int
countSubstring needle haystack =
  Text.count (Text.pack needle) (Text.pack haystack)

expectBindingGroup :: String -> Rhs -> IO BindingGroup
expectBindingGroup fixtureLabel = \case
  UnguardedRhs _ (Just bindingGroup) ->
    pure bindingGroup
  otherRhs ->
    assertFailure (fixtureLabel <> ": expected an unguarded RHS with local bindings, got " <> show otherRhs)

localBindingBinderIds :: Binding -> [BinderId]
localBindingBinderIds = \case
  FunctionBinding binderAnn _ ->
    [baId binderAnn]
  PatternBinding bindingPatternValue _ ->
    fmap baId (patBinders bindingPatternValue)

letRecursions :: Pattern HsExprF -> [LetRecursion]
letRecursions = \case
  PatternVar {} ->
    []
  PatternNode expressionNode ->
    case expressionNode of
    LetF letRecursion bindingRows bodyExpr ->
      letRecursion
        : foldMap (letRecursions . snd) bindingRows
          <> letRecursions bodyExpr
    otherExpressionNode ->
      foldMap letRecursions otherExpressionNode

stripBindingLambdas :: Pattern HsExprF -> Pattern HsExprF
stripBindingLambdas = \case
  PatternNode (LamF _ bodyValue) -> stripBindingLambdas bodyValue
  patternValue -> patternValue

stripTestPatParens :: HsPatF -> HsPatF
stripTestPatParens = \case
  PParP innerPattern -> stripTestPatParens innerPattern
  patternValue -> patternValue

patternContainsExprWithTySig :: Pattern HsExprF -> Bool
patternContainsExprWithTySig = \case
  PatternVar {} -> False
  PatternNode (ExprWithTySigF _ _) -> True
  PatternNode layer -> any patternContainsExprWithTySig layer

patternContainsAppType :: Pattern HsExprF -> Bool
patternContainsAppType = \case
  PatternVar {} -> False
  PatternNode (AppTypeF _ _) -> True
  PatternNode layer -> any patternContainsAppType layer

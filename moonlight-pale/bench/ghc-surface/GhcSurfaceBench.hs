-- Parse-and-convert workloads for @moonlight-pale:ghc-surface@.  The
-- common-subset corpus is deliberately restricted to syntax shared with the
-- historical converter; the full-fidelity corpus exercises current structural
-- ownership and is therefore not offered as a historical ratio gate.
module GhcSurfaceBench
  ( GhcSurfaceBenchmarkObstruction (..),
    SemanticConversionManifest (..),
    ConversionBenchmarkDigest,
    PreparedConversionCorpus,
    commonSubsetSemanticManifests,
    commonSubsetWorkload,
    prepareCommonSubsetCorpus,
    convertCommonCorpus,
    conversionBenchmarkDigestHash,
    ghcSurfaceBenchmarks,
  )
where

import BenchSupport (preparedBenchmarks)
import Control.DeepSeq (NFData (rnf))
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Text qualified as Text
import Moonlight.Core (binderIdKey)
import Moonlight.Pale.Ghc.Expr
  ( BinderAnn (..),
    Binding (..),
    BindingGroup,
    Clause (..),
    ConvertedModule (..),
    ConvertedModuleMetrics
      ( cmmBindingCount,
        cmmGlobalVarRefCount,
        cmmLambdaSiteCount,
        cmmLetSiteCount,
        cmmLocalVarRefCount,
        cmmMaxFreeScopeCount,
        cmmObservedContextCount,
        cmmScopedExprCount
      ),
    ConvertObstruction,
    Expr,
    LayoutPolicy (CompactLayout),
    ModuleRenderContext (..),
    RenderRefusal,
    RenderTarget (RenderConvertedModule),
    Rhs (..),
    ScopeCtx (..),
    ScopeLookupFailure,
    SourceRegion (..),
    ConvertedValueBinding,
    bindingGroupBindings,
    bindingGroupScope,
    bindingNames,
    convertHaskellSource,
    convertedModuleBindings,
    convertedModuleMetrics,
    exprFreeScopes,
    exprNode,
    exprRegion,
    exprScope,
    freeScopeSummaryToList,
    renderSource,
    renderRdrName,
    scopeIdKey,
    scopeObservedContexts,
    tlbBinding,
    tlbRegion,
    tlbScope,
  )
import Test.Tasty.Bench (Benchmark, bgroup)

data GhcSurfaceBenchmarkObstruction
  = InvalidCommonSubsetSize !Int
  | BenchmarkConversionRejected !String !ConvertObstruction
  | BenchmarkRenderingRefused !String !RenderRefusal
  | BenchmarkScopeMetadataRejected !String !ScopeLookupFailure
  | UnexpectedBindingCardinality !String !Int !Int
  | UnexpectedBindingNameCardinality !String ![String]
  | UnexpectedOrderedBinders !String ![String] ![String]
  deriving stock (Eq, Show)

instance NFData GhcSurfaceBenchmarkObstruction where
  rnf obstruction =
    rnf (show obstruction)

-- The overlap section used to glue current and historical conversion rows.
-- It is semantic source plus ordered top-level binder evidence, not internal
-- structural node counts whose owners deliberately changed in the rewrite.
data SemanticConversionManifest = SemanticConversionManifest
  { semanticManifestBindingCount :: !Int,
    semanticManifestRenderedModule :: !String,
    semanticManifestOrderedBinders :: ![String]
  }
  deriving stock (Eq, Show)

instance NFData SemanticConversionManifest where
  rnf manifest =
    rnf (semanticManifestBindingCount manifest)
      `seq` rnf (semanticManifestRenderedModule manifest)
      `seq` rnf (semanticManifestOrderedBinders manifest)

-- Current-only readiness evidence.  All eight metrics are forced and retained
-- for performance accounting, but are not cross-version equality evidence.
data RepresentationReadinessDigest = RepresentationReadinessDigest
  { readinessBindingCount :: !Int,
    readinessObservedContextCount :: !Int,
    readinessLambdaSiteCount :: !Int,
    readinessLetSiteCount :: !Int,
    readinessScopedExprCount :: !Int,
    readinessGlobalVarRefCount :: !Int,
    readinessLocalVarRefCount :: !Int,
    readinessMaxFreeScopeCount :: !Int,
    readinessAnnotationDigest :: !Int
  }
  deriving stock (Eq, Show)

instance NFData RepresentationReadinessDigest where
  rnf digest =
    rnf (readinessBindingCount digest)
      `seq` rnf (readinessObservedContextCount digest)
      `seq` rnf (readinessLambdaSiteCount digest)
      `seq` rnf (readinessLetSiteCount digest)
      `seq` rnf (readinessScopedExprCount digest)
      `seq` rnf (readinessGlobalVarRefCount digest)
      `seq` rnf (readinessLocalVarRefCount digest)
      `seq` rnf (readinessMaxFreeScopeCount digest)
      `seq` rnf (readinessAnnotationDigest digest)

data ConversionBenchmarkDigest = ConversionBenchmarkDigest
  { conversionSemanticManifest :: !SemanticConversionManifest,
    conversionRepresentationReadiness :: !RepresentationReadinessDigest
  }
  deriving stock (Eq, Show)

instance NFData ConversionBenchmarkDigest where
  rnf digest =
    rnf (conversionSemanticManifest digest)
      `seq` rnf (conversionRepresentationReadiness digest)

conversionBenchmarkDigestHash :: ConversionBenchmarkDigest -> Int
conversionBenchmarkDigestHash digest =
  let semanticManifest = conversionSemanticManifest digest
      readinessDigest = conversionRepresentationReadiness digest
      semanticHash =
        foldl'
          digestString
          (digestInt 2166136261 (semanticManifestBindingCount semanticManifest))
          ( semanticManifestRenderedModule semanticManifest
              : semanticManifestOrderedBinders semanticManifest
          )
   in foldl'
        digestInt
        semanticHash
        [ readinessBindingCount readinessDigest,
          readinessObservedContextCount readinessDigest,
          readinessLambdaSiteCount readinessDigest,
          readinessLetSiteCount readinessDigest,
          readinessScopedExprCount readinessDigest,
          readinessGlobalVarRefCount readinessDigest,
          readinessLocalVarRefCount readinessDigest,
          readinessMaxFreeScopeCount readinessDigest,
          readinessAnnotationDigest readinessDigest
        ]

data PreparedConversionCorpus = PreparedConversionCorpus
  { preparedCorpusLabel :: !String,
    preparedCorpusSource :: !String
  }
  deriving stock (Eq, Show)

instance NFData PreparedConversionCorpus where
  rnf corpus =
    rnf (preparedCorpusLabel corpus)
      `seq` rnf (preparedCorpusSource corpus)

ghcSurfaceBenchmarks :: Either GhcSurfaceBenchmarkObstruction Benchmark
ghcSurfaceBenchmarks = do
  commonCorpora <- traverse prepareCommonSubsetCorpus commonSubsetSizes
  fullFidelityCorpus <- prepareFullFidelityCorpus
  structuralBenchmarks <- traverse prepareStructuralBenchmark structuralCorpusFamilies
  pure
    ( bgroup
        "ghc-surface"
        [ bgroup
            "common-subset-convert-and-normalize"
            (preparedBenchmarks "bindings" commonCorpora convertCommonCorpus),
          bgroup
            "current-full-fidelity"
            (preparedBenchmarks "modules" [(1, fullFidelityCorpus)] convertFullFidelityCorpus),
          bgroup
            "current-structure-matrix"
            structuralBenchmarks
        ]
    )

prepareStructuralBenchmark ::
  (String, [Int], Int -> String) ->
  Either GhcSurfaceBenchmarkObstruction Benchmark
prepareStructuralBenchmark (familyLabel, sizes, sourceForSize) = do
  preparedCorpora <-
    traverse
      ( \size ->
          prepareStructuralCorpus
            familyLabel
            size
            (sourceForSize size)
      )
      sizes
  pure
    ( bgroup
        familyLabel
        (preparedBenchmarks "size" preparedCorpora convertFullFidelityCorpus)
    )

prepareStructuralCorpus ::
  String ->
  Int ->
  String ->
  Either GhcSurfaceBenchmarkObstruction (Int, PreparedConversionCorpus)
prepareStructuralCorpus familyLabel size sourceText = do
  let corpus =
        PreparedConversionCorpus
          { preparedCorpusLabel = familyLabel <> "/" <> show size,
            preparedCorpusSource = sourceText
          }
  _ <- convertFullFidelityCorpus corpus
  pure (size, corpus)

structuralCorpusFamilies :: [(String, [Int], Int -> String)]
structuralCorpusFamilies =
  [ ("scope-depth", [8, 32, 128], scopeDepthModule),
    ("scope-branch-count", [8, 32, 128], scopeBranchModule),
    ("shadow-depth", [8, 32, 128], shadowDepthModule),
    ("sparse-scc-cardinality", [8, 32, 128], sparseSccModule),
    ("dense-scc-cardinality", [4, 8, 16], denseSccModule),
    ("rendered-list-elements", [32, 256, 2048], renderedListModule),
    ("opaque-declaration-position", [0, 32, 128], opaqueDeclarationPositionModule)
  ]

commonSubsetSemanticManifests :: Either GhcSurfaceBenchmarkObstruction [(Int, SemanticConversionManifest)]
commonSubsetSemanticManifests =
  traverse
    ( \bindingCount ->
        fmap
          ((,) bindingCount . conversionSemanticManifest)
          (commonSubsetWorkload bindingCount)
    )
    commonSubsetSizes

commonSubsetWorkload :: Int -> Either GhcSurfaceBenchmarkObstruction ConversionBenchmarkDigest
commonSubsetWorkload bindingCount
  | bindingCount <= 0 =
      Left (InvalidCommonSubsetSize bindingCount)
  | otherwise = do
      let corpus = commonSubsetCorpus bindingCount
          expectedBinders = fmap (\index -> "f" <> show index) [1 .. bindingCount]
      digest <- convertCommonCorpus corpus
      validateCommonDigest
        (preparedCorpusLabel corpus)
        bindingCount
        expectedBinders
        digest
      pure digest

commonSubsetSizes :: [Int]
commonSubsetSizes =
  [8, 32, 128]

prepareCommonSubsetCorpus :: Int -> Either GhcSurfaceBenchmarkObstruction (Int, PreparedConversionCorpus)
prepareCommonSubsetCorpus bindingCount =
  (bindingCount, commonSubsetCorpus bindingCount)
    <$ commonSubsetWorkload bindingCount

commonSubsetCorpus :: Int -> PreparedConversionCorpus
commonSubsetCorpus bindingCount =
  PreparedConversionCorpus
    { preparedCorpusLabel = "common-subset/" <> show bindingCount,
      preparedCorpusSource = commonSubsetModule bindingCount
    }

prepareFullFidelityCorpus :: Either GhcSurfaceBenchmarkObstruction PreparedConversionCorpus
prepareFullFidelityCorpus = do
  let corpus =
        PreparedConversionCorpus
          { preparedCorpusLabel = "current-full-fidelity",
            preparedCorpusSource = fullFidelityModule
          }
  _ <- convertFullFidelityCorpus corpus
  pure corpus

convertCommonCorpus :: PreparedConversionCorpus -> Either GhcSurfaceBenchmarkObstruction ConversionBenchmarkDigest
convertCommonCorpus corpus = do
  convertedModule <- convertCorpus corpus
  semanticManifest <- commonSemanticManifest (preparedCorpusLabel corpus) convertedModule
  readinessDigest <- representationReadinessDigest (preparedCorpusLabel corpus) convertedModule
  pure
    ConversionBenchmarkDigest
      { conversionSemanticManifest = semanticManifest,
        conversionRepresentationReadiness = readinessDigest
      }

convertFullFidelityCorpus :: PreparedConversionCorpus -> Either GhcSurfaceBenchmarkObstruction ConversionBenchmarkDigest
convertFullFidelityCorpus corpus = do
  convertedModule <- convertCorpus corpus
  renderedModule <-
    first
      (BenchmarkRenderingRefused (preparedCorpusLabel corpus))
      ( Text.unpack
          <$> renderSource
            CompactLayout
            ( RenderConvertedModule
                (ModuleRenderContext "" (Just "Bench"))
                convertedModule
            )
      )
  let metrics = convertedModuleMetrics convertedModule
  let orderedBinders = orderedBindingNames convertedModule
  readinessDigest <- representationReadinessDigest (preparedCorpusLabel corpus) convertedModule
  pure
    ConversionBenchmarkDigest
      { conversionSemanticManifest =
          SemanticConversionManifest
            { semanticManifestBindingCount = cmmBindingCount metrics,
              semanticManifestRenderedModule = renderedModule,
              semanticManifestOrderedBinders = orderedBinders
            },
        conversionRepresentationReadiness = readinessDigest
      }

convertCorpus :: PreparedConversionCorpus -> Either GhcSurfaceBenchmarkObstruction ConvertedModule
convertCorpus corpus =
  case convertHaskellSource "Bench.hs" (preparedCorpusSource corpus) of
    Left obstruction ->
      Left (BenchmarkConversionRejected (preparedCorpusLabel corpus) obstruction)
    Right convertedModule ->
      Right convertedModule

commonSemanticManifest ::
  String ->
  ConvertedModule ->
  Either GhcSurfaceBenchmarkObstruction SemanticConversionManifest
commonSemanticManifest corpusLabel convertedModule = do
  orderedBinders <-
    traverse
      (commonBindingName corpusLabel)
      (convertedModuleBindings convertedModule)
  renderedModule <-
    first
      (BenchmarkRenderingRefused corpusLabel)
      ( Text.unpack
          <$> renderSource
            CompactLayout
            ( RenderConvertedModule
                (ModuleRenderContext "" (Just "Bench"))
                convertedModule
            )
      )
  pure
    SemanticConversionManifest
      { semanticManifestBindingCount = length orderedBinders,
        semanticManifestRenderedModule = renderedModule,
        semanticManifestOrderedBinders = orderedBinders
      }

commonBindingName ::
  String ->
  ConvertedValueBinding ->
  Either GhcSurfaceBenchmarkObstruction String
commonBindingName corpusLabel topLevelBinding =
  case fmap renderRdrName (bindingNames (tlbBinding topLevelBinding)) of
    [bindingName] ->
      Right bindingName
    names ->
      Left (UnexpectedBindingNameCardinality corpusLabel names)

validateCommonDigest ::
  String ->
  Int ->
  [String] ->
  ConversionBenchmarkDigest ->
  Either GhcSurfaceBenchmarkObstruction ()
validateCommonDigest corpusLabel expectedBindingCount expectedBinders digest
  | semanticManifestBindingCount semanticManifest /= expectedBindingCount =
      Left
        ( UnexpectedBindingCardinality
            corpusLabel
            expectedBindingCount
            (semanticManifestBindingCount semanticManifest)
        )
  | semanticManifestOrderedBinders semanticManifest /= expectedBinders =
      Left
        ( UnexpectedOrderedBinders
            corpusLabel
            expectedBinders
            (semanticManifestOrderedBinders semanticManifest)
        )
  | otherwise =
      Right ()
  where
    semanticManifest = conversionSemanticManifest digest

orderedBindingNames :: ConvertedModule -> [String]
orderedBindingNames =
  foldMap
    (fmap renderRdrName . bindingNames . tlbBinding)
    . convertedModuleBindings

representationReadinessDigest ::
  String ->
  ConvertedModule ->
  Either GhcSurfaceBenchmarkObstruction RepresentationReadinessDigest
representationReadinessDigest corpusLabel convertedModule = do
  scopeContexts <-
    first
      (BenchmarkScopeMetadataRejected corpusLabel)
      (scopeObservedContexts (cmScopeIndex convertedModule))
  let metrics = convertedModuleMetrics convertedModule
      annotationDigest =
        foldl'
          digestConvertedValueBindingAnnotations
          ( foldl'
              digestBinderAnn
              ( foldl'
                  digestBinderAnn
                  (foldl' digestScopeContext 2166136261 scopeContexts)
                  (cmLambdaSites convertedModule)
              )
              (cmLetSites convertedModule)
          )
          (convertedModuleBindings convertedModule)
  pure
    RepresentationReadinessDigest
      { readinessBindingCount = cmmBindingCount metrics,
        readinessObservedContextCount = cmmObservedContextCount metrics,
        readinessLambdaSiteCount = cmmLambdaSiteCount metrics,
        readinessLetSiteCount = cmmLetSiteCount metrics,
        readinessScopedExprCount = cmmScopedExprCount metrics,
        readinessGlobalVarRefCount = cmmGlobalVarRefCount metrics,
        readinessLocalVarRefCount = cmmLocalVarRefCount metrics,
        readinessMaxFreeScopeCount = cmmMaxFreeScopeCount metrics,
        readinessAnnotationDigest = annotationDigest
      }

digestConvertedValueBindingAnnotations :: Int -> ConvertedValueBinding -> Int
digestConvertedValueBindingAnnotations digest convertedValueBinding =
  digestBindingAnnotations
    ( digestSourceRegion
        (digestInt digest (scopeIdKey (tlbScope convertedValueBinding)))
        (tlbRegion convertedValueBinding)
    )
    (tlbBinding convertedValueBinding)

digestBindingAnnotations :: Int -> Binding -> Int
digestBindingAnnotations digest = \case
  FunctionBinding binderAnn clauses ->
    foldl'
      digestClauseAnnotations
      (digestBinderAnn digest binderAnn)
      clauses
  PatternBinding _ rhsValue ->
    digestRhsAnnotations digest rhsValue

digestClauseAnnotations :: Int -> Clause -> Int
digestClauseAnnotations digest clauseValue =
  digestRhsAnnotations digest (clauseRhs clauseValue)

digestRhsAnnotations :: Int -> Rhs -> Int
digestRhsAnnotations digest = \case
  UnguardedRhs bodyExpression maybeBindingGroup ->
    foldl'
      digestBindingGroupAnnotations
      (digestExprAnnotations digest bodyExpression)
      maybeBindingGroup
  GuardedRhs guardedAlternatives maybeBindingGroup ->
    foldl'
      digestBindingGroupAnnotations
      ( foldl'
          digestExprAnnotations
          digest
          (foldMap toList guardedAlternatives)
      )
      maybeBindingGroup

digestBindingGroupAnnotations :: Int -> BindingGroup -> Int
digestBindingGroupAnnotations digest bindingGroup =
  foldl'
    digestBindingAnnotations
    (digestInt digest (scopeIdKey (bindingGroupScope bindingGroup)))
    (bindingGroupBindings bindingGroup)

digestExprAnnotations :: Int -> Expr -> Int
digestExprAnnotations digest expressionValue =
  foldl'
    digestExprAnnotations
    ( foldl'
        (\nestedDigest scopeId -> digestInt nestedDigest (scopeIdKey scopeId))
        ( digestSourceRegion
            (digestInt digest (scopeIdKey (exprScope expressionValue)))
            (exprRegion expressionValue)
        )
        (freeScopeSummaryToList (exprFreeScopes expressionValue))
    )
    (exprNode expressionValue)

digestBinderAnn :: Int -> BinderAnn -> Int
digestBinderAnn digest binderAnn =
  foldl'
    (\nestedDigest character -> digestInt nestedDigest (fromEnum character))
    (digestInt digest (binderIdKey (baId binderAnn)))
    (renderRdrName (baName binderAnn))

digestSourceRegion :: Int -> Maybe SourceRegion -> Int
digestSourceRegion digest = \case
  Nothing ->
    digestInt digest 0
  Just region ->
    digestInt
      ( digestInt
          (digestInt (digestInt digest (srStartLine region)) (srStartCol region))
          (srEndLine region)
      )
      (srEndCol region)

digestScopeContext :: Int -> ScopeCtx -> Int
digestScopeContext digest = \case
  ActualScope scopeId ->
    digestInt digest (scopeIdKey scopeId)
  IncompatibleScope ->
    digestInt digest (-1)

digestInt :: Int -> Int -> Int
digestInt digest value =
  (digest * 16777619) + value

digestString :: Int -> String -> Int
digestString =
  foldl'
    (\digest character -> digestInt digest (fromEnum character))

commonSubsetModule :: Int -> String
commonSubsetModule bindingCount =
  unlines ("module Bench where" : "" : fmap binding [1 .. bindingCount])
  where
    binding :: Int -> String
    binding index =
      let name = show index
       in "f" <> name <> " x = let y = x + " <> name <> " in h" <> name <> " (y * y)"

fullFidelityModule :: String
fullFidelityModule =
  unlines
    [ "{-# LANGUAGE MagicHash #-}",
      "{-# LANGUAGE TupleSections #-}",
      "{-# LANGUAGE UnboxedTuples #-}",
      "module Bench where",
      "",
      "infixr 5 <+>",
      "(<+>) left right = left + right",
      "",
      "(answer, label) = (42, \"exact\")",
      "tupleSection value = (, value)",
      "unboxed value = (# value, value + 1 #)",
      "exactFraction = 1.25",
      "primitiveString = \"bytes\"#",
      "multi [] = 0",
      "multi (value : values) = local value + multi values",
      "  where",
      "    local nested = nested <+> 1"
    ]

scopeDepthModule :: Int -> String
scopeDepthModule depth =
  unlines
    [ "module Bench where",
      "",
      "deep = "
        <> foldr
          (\binderName bodySource -> "\\" <> binderName <> " -> " <> bodySource)
          ("level" <> show depth)
          binderNames
    ]
  where
    binderNames =
      fmap (\index -> "level" <> show index) [1 .. depth]

scopeBranchModule :: Int -> String
scopeBranchModule branchCount =
  unlines
    ( [ "module Bench where",
        "",
        "branch value = case value of"
      ]
        <> fmap branchRow [1 .. branchCount]
    )
  where
    branchRow :: Int -> String
    branchRow index =
      "  Branch" <> show index <> " branchValue -> branchValue"

shadowDepthModule :: Int -> String
shadowDepthModule depth =
  unlines
    [ "module Bench where",
      "",
      "shadow = "
        <> foldr
          (\_ bodySource -> "\\value -> " <> bodySource)
          "value"
          [1 .. depth]
    ]

sparseSccModule :: Int -> String
sparseSccModule cardinality =
  unlines
    ( [ "module Bench where",
        "",
        "sparse seed =",
        "  let"
      ]
        <> fmap sparseBindingRow [1 .. cardinality]
        <> ["  in node1"]
    )
  where
    sparseBindingRow index =
      "    node"
        <> show index
        <> " = node"
        <> show (if index == cardinality then 1 else index + 1)
        <> " + seed"

denseSccModule :: Int -> String
denseSccModule cardinality =
  unlines
    ( [ "module Bench where",
        "",
        "dense seed =",
        "  let"
      ]
        <> fmap denseBindingRow [1 .. cardinality]
        <> ["  in node1"]
    )
  where
    denseBindingRow index =
      "    node"
        <> show index
        <> " = "
        <> intercalate
          " + "
          ( "seed"
              : fmap
                (\referencedIndex -> "node" <> show referencedIndex)
                (filter (/= index) [1 .. cardinality])
          )

renderedListModule :: Int -> String
renderedListModule elementCount =
  unlines
    [ "module Bench where",
      "",
      "rendered = [" <> intercalate ", " (fmap show [1 .. elementCount]) <> "]"
    ]

opaqueDeclarationPositionModule :: Int -> String
opaqueDeclarationPositionModule declarationPosition =
  unlines
    ( ["module Bench where", ""]
        <> precedingBindings
        <> [ "class BenchClass value where",
             "  benchMethod :: value -> value"
           ]
        <> remainingBindings
    )
  where
    allBindings =
      fmap
        (\index -> "value" <> show index <> " = " <> show index)
        [1 .. opaquePositionBindingCount]
    (precedingBindings, remainingBindings) =
      splitAt declarationPosition allBindings

opaquePositionBindingCount :: Int
opaquePositionBindingCount =
  128

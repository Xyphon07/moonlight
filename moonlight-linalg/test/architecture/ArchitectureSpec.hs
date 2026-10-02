{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module ArchitectureSpec
  ( tests,
  )
where

import Control.Applicative ((<|>))
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Prelude

data SourceCheck = SourceCheck
  { sourcePath :: !FilePath,
    forbiddenFragments :: ![(String, String)]
  }

data MissingSource = MissingSource
  { missingPath :: !FilePath,
    missingLabel :: !String
  }

data SliceSourceRoot = SliceSourceRoot
  { sliceSourceRoot :: !FilePath,
    sliceForbiddenFragments :: ![(String, String)]
  }

tests :: TestTree
tests =
  testGroup
    "architecture"
    [ testCase "obsolete linalg owners stay deleted" assertObsoleteOwnersStayDeleted,
      testCase "public Krylov surface stays algorithm-only" assertPublicKrylovSurface,
      testCase "public operator surface keeps source constructors hidden" assertPublicOperatorSurface,
      testCase "Krylov hot coefficients stay vector-native" assertKrylovHotCoefficients,
      testCase "selected spectral values stay vector-native" assertSelectedSpectralValuesVectorNative,
      testCase "flat dense Double storage stays storable-native" assertDenseDoubleStorageStorable,
      testCase "spectral fallback seed stays vector-native" assertSpectralSeedBoundary,
      testCase "structured projected/block routes avoid dense row owners" assertStructuredRoutesAvoidDenseRowOwners,
      testCase "native LAPACK details stay behind the native effect boundary" assertNativeLapackBoundary,
      testCase "linalg sublibrary slices stay downward" assertSublibrarySliceDiscipline,
      testCase "canonical sparse storage exposes vector payloads only" assertCanonicalSparseStorageSurface,
      testCase "canonical CSR matvec does not revalidate in hot apply paths" assertCSRHotApplyBoundary,
      testCase "public sparse solver surface stays canonical" assertPublicSparseSolverSurface,
      testCase "sparse solvers stay off list orchestration" assertSparseSolverHotSurface,
      testCase "linalg docs do not advertise deleted spectral APIs" assertLinalgDocsAvoidDeletedSpectralAPIs
    ]

assertObsoleteOwnersStayDeleted :: Assertion
assertObsoleteOwnersStayDeleted = do
  existingSources <- catMaybes <$> traverse existingMissingSource obsoleteSources
  assertBool
    ("obsolete linalg owner files still exist:\n" <> unlines existingSources)
    (null existingSources)

obsoleteSources :: [MissingSource]
obsoleteSources =
  [ MissingSource "src-carrier/Moonlight/LinAlg/Internal/Continuous.hs" "continuous dense-list helper",
    MissingSource "src-dense/Moonlight/LinAlg/Pure/Dense/Classes.hs" "duplicate dense class owner",
    MissingSource "src-dense/Moonlight/LinAlg/Pure/Dense/Primitives.hs" "duplicate dense primitive owner",
    MissingSource "src-dense/Moonlight/LinAlg/Pure/Dense/VectorOps.hs" "duplicate dense vector ops",
    MissingSource "src-spectral/Moonlight/LinAlg/Pure/Krylov.hs" "old core Krylov barrel",
    MissingSource "src-spectral/Moonlight/LinAlg/Pure/Krylov/Restart.hs" "obsolete restarted solve front door",
    MissingSource "src-spectral/Moonlight/LinAlg/Pure/Krylov/Structure.hs" "duplicate projected structure owner",
    MissingSource "src-spectral/Moonlight/LinAlg/Pure/Krylov/TridiagonalSolve.hs" "tridiagonal wrapper solve owner",
    MissingSource "src-sparse/Moonlight/LinAlg/Pure/Sparse/Solver.hs" "old list sparse solver facade"
  ]

assertPublicKrylovSurface :: Assertion
assertPublicKrylovSurface =
  assertForbiddenFragments
    [ SourceCheck
        "src-public/Moonlight/LinAlg/Krylov.hs"
        [ ("public operator source tag", "LinearOperatorSource"),
          ("public operator source accessor", "linearOperatorSource"),
          ("public Ritz pair result", "RitzPair"),
          ("public Ritz vector conversion", "ritzVectorDyn"),
          ("public projected solve policy", "ProjectedSolvePolicy"),
          ("public projected solve backend", "ProjectedSolveBackend"),
          ("public projected policy runner", "projectedEigenpairsWithPolicy"),
          ("public projected policy default", "defaultProjectedSolvePolicy"),
          ("public projected backend accessor", "projectedSolveBackend"),
          ("public projected operator internals", "SymmetricProjectedOperator"),
          ("public projected subspace internals", "ProjectedSubspace"),
          ("obsolete restarted solve front door", "restartedEigenpairsSymmetric"),
          ("obsolete block solve front door", "blockEigenpairsSymmetric")
        ],
      SourceCheck
        "src-public/Moonlight/LinAlg/Spectral.hs"
        [ ("list fallback setter", "withEigenFallbackInitialList"),
          ("public list eigenvalue projection", "eigenvaluesToList"),
          ("public list eigenpair projection", "eigenpairsToListColumns"),
          ("obsolete selected eigenvalue alias", "selectedEigenvalues"),
          ("obsolete selected eigenpair alias", "selectedEigenpairs")
        ]
    ]

assertPublicOperatorSurface :: Assertion
assertPublicOperatorSurface =
  assertForbiddenFragments
    [ SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Operator.hs"
        [ ("public linear operator constructor export", "LinearOperator (..)"),
          ("public operator source type leak", "OperatorSource"),
          ("public operator source field leak", "operatorSource"),
          ("public source interpreter leak", "applyOperatorSource"),
          ("public source shape leak", "operatorSourceShape"),
          ("public list-valued operator constructor", "mkLinearOperator"),
          ("public list-valued self-adjoint constructor", "declaredSelfAdjointLinearOperator"),
          ("public list-valued operator application", "applyLinearOperator")
        ],
      SourceCheck
        "src-public/Moonlight/LinAlg/Operator.hs"
        [ ("public linear operator constructor export", "LinearOperator (..)"),
          ("public operator source type leak", "OperatorSource"),
          ("public operator source field leak", "operatorSource"),
          ("public source interpreter leak", "applyOperatorSource"),
          ("public source shape leak", "operatorSourceShape"),
          ("public list-valued operator constructor", "mkLinearOperator"),
          ("public list-valued self-adjoint constructor", "declaredSelfAdjointLinearOperator"),
          ("public list-valued operator application", "applyLinearOperator")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Operator/Internal.hs"
        [ ("list-valued operator constructor", "mkLinearOperator"),
          ("list-valued self-adjoint constructor", "declaredSelfAdjointLinearOperator"),
          ("list-valued operator application", "applyLinearOperator")
        ]
    ]

assertKrylovHotCoefficients :: Assertion
assertKrylovHotCoefficients =
  assertForbiddenFragments
    [ SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/Internal.hs"
        [ ("list-valued orthogonalization coefficients", "Either MoonlightError (U.Vector Double, [Double])"),
          ("seed list normalizer signature", "normalizeSeed :: String -> Int -> Double -> [Double]"),
          ("seed list materialization", "U.fromList seedValues"),
          ("basis list traversal", "Box.toList"),
          ("coefficient list zipper", "zipWithExact"),
          ("restart seed residue", "nextRestartSeed")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/Arnoldi.hs"
        [ ("Arnoldi list seed signature", "-> [Double] ->"),
          ("Arnoldi coefficient list materialization", "U.fromList (coefficients"),
          ("Arnoldi column list accumulator", "columnsRev")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/Lanczos.hs"
        [("Lanczos list seed signature", "-> [Double] ->")]
    ]

assertSelectedSpectralValuesVectorNative :: Assertion
assertSelectedSpectralValuesVectorNative =
  assertForbiddenFragments
    [ SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/SelectedTridiagonal.hs"
        [ ("list-valued selected tridiagonal direct values", "selectedSymmetricTridiagonalEigenvaluesDirect ::\n  SpectrumEnd ->\n  Int ->\n  SymmetricTridiagonal ->\n  Either MoonlightError [Double]"),
          ("list-valued selected tridiagonal checked values", "selectedSymmetricTridiagonalEigenvaluesChecked ::\n  SpectrumEnd ->\n  Int ->\n  SymmetricTridiagonal ->\n  Either MoonlightError [Double]"),
          ("CSR row-offset list roundtrip", "U.fromList (csrRowOffsets csrValue)"),
          ("CSR column-index list roundtrip", "U.fromList (csrColumnIndices csrValue)"),
          ("CSR value list roundtrip", "U.fromList (csrValues csrValue)"),
          ("path tridiagonal pair sort before dispatch", "fmap (sortForSpectrumBy spectrumEnd (\\(eigenvalue, _, _) -> eigenvalue) . take requestedCount) $\n    case pathLaplacianEigenpairs"),
          ("path tridiagonal largest pair ascending modes", "LargestEigenvalues -> [matrixSize - boundedCount .. matrixSize - 1]"),
          ("path tridiagonal pair residual via generic tridiagonal apply", "!residualNorm = tridiagonalResidualNorm tridiagonalValue eigenvalue eigenvector"),
          ("path tridiagonal pair residual vector allocation", "normU (U.generate matrixSize (pathLaplacianResidualEntry matrixSize eigenvalue eigenvector))"),
          ("reducible values through QL pair kernel", "traverse blockEigenvaluesViaQL (tridiagonalBlocks tridiagonalValue)"),
          ("values-only selected tridiagonal QL detour", "selectedTridiagonalEigenvaluesViaQL"),
          ("small values-only QL threshold", "smallTridiagonalSelectedQLThreshold")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Spectral/Solve.hs"
        [ ("selected value call-site list materialization", "U.fromList <$> selectedSymmetricTridiagonalEigenvaluesDirect"),
          ("path value list materialization", "U.fromList\n    . sortForSpectrumBy spectrumEnd id\n    . fmap (pathLaplacianEigenvalueAt dimension)"),
          ("path pair operator reconstruction", "pathLaplacianLinearOperator dimension"),
          ("path pair residual via full operator apply", "runOperatorU operatorValue eigenvector"),
          ("path pair residual vector allocation", "normU (U.generate dimension (pathLaplacianResidualEntry dimension eigenvalue eigenvector))"),
          ("path pair resorting after ordered mode selection", "sortBy\n      (spectrumPairOrdering spectrumEnd)\n      (pathLaplacianColumn dimension <$> selectedModeIndices spectrumEnd requestedCount dimension)"),
          ("diagonal values unconditional full sort", "Right . U.fromList . take requestedCount . fmap snd . sortIndexedValues spectrumEnd . U.toList $ U.indexed diagonalEntries")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Spectral/Result.hs"
        [ ("list eigenvalue projection", "eigenvaluesToList"),
          ("list eigenpair projection", "eigenpairsToListColumns"),
          ("list eigenpair column projection", "eigenpairsToColumns")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/Projected.hs"
        [ ("projected value call-site list materialization", "U.fromList <$> symmetricProjectedEigenvalues"),
          ("projected selected value call-site list materialization", "U.fromList <$> selectedSymmetricTridiagonalEigenvaluesDirect"),
          ("projected list-valued pair column request", "selectedSymmetricTridiagonalEigenpairColumnsDirect"),
          ("projected raw pair list intermediate", "rawPairs <-"),
          ("projected lifted column list intermediate", "liftedColumns <- traverse")
        ]
      ,
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Spectral/Request.hs"
        [ ("obsolete selected eigenvalue alias", "selectedEigenvalues"),
          ("obsolete selected eigenpair alias", "selectedEigenpairs")
        ]
    ]

assertDenseDoubleStorageStorable :: Assertion
assertDenseDoubleStorageStorable =
  assertForbiddenFragments
    [ SourceCheck
        "src-carrier/Moonlight/LinAlg/Pure/Dense/Flat.hs"
        [ ("flat dense Double unboxed storage import", "Data.Vector.Unboxed"),
          ("flat dense Double unboxed payload", "U.Vector Double")
        ]
    ]

assertSpectralSeedBoundary :: Assertion
assertSpectralSeedBoundary =
  assertForbiddenFragments
    [ SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Spectral/Solve.hs"
        [ ("list fallback setter", "withEigenFallbackInitialList"),
          ("fallback seed list conversion", "U.toList (fallbackSeed")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/Projected.hs"
        [("projected Lanczos seed list conversion", "U.toList seedVector")],
      SourceCheck
        "bench/spectral/ProjectedBlock.hs"
        [("projected benchmark list seed", "projectedSeedVector :: Int -> [Double]")],
      SourceCheck
        "bench/spectral/SpectralDispatch.hs"
        [ ("spectral benchmark list seed", "seedVector :: Int -> [Double]"),
          ("spectral benchmark list fallback setter", "withEigenFallbackInitialList")
        ]
    ]

assertStructuredRoutesAvoidDenseRowOwners :: Assertion
assertStructuredRoutesAvoidDenseRowOwners =
  assertForbiddenFragments
    [ SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/Projected.hs"
        [ ("projected dense row owner", "symmetricProjectedOperatorRows"),
          ("projected dense row payload", "[[Double]]"),
          ("fake block-projected values Lanczos fallback", "blockProjectedEigenvaluesViaLanczos"),
          ("fake block-projected pairs Lanczos fallback", "blockProjectedEigenpairsViaLanczos"),
          ("projected block fallback operator wrapper", "declaredSelfAdjointVectorLinearOperator")
        ],
      SourceCheck
        "src-structured/Moonlight/LinAlg/Pure/Structured/BlockTridiagonal.hs"
        [ ("block tridiagonal dense rows", "blockTridiagonalRows"),
          ("nested dense block row payload", "[[[Double]]]"),
          ("derived row-to-block map owner", "blockRowIndices"),
          ("derived row-to-block map builder", "rowsToBlockIndices"),
          ("block apply generated output vector", "Right\n        ( U.generate"),
          ("block apply generated row sum", "U.sum\n        ( U.generate"),
          ("block apply generated absolute row sum", "U.sum\n    ( U.generate")
        ],
      SourceCheck
        "src-structured/Moonlight/LinAlg/Pure/Structured/Tridiagonal.hs"
        [ ("tridiagonal dense row view", "symmetricTridiagonalRows"),
          ("tridiagonal dense row payload", "[[Double]]")
        ],
      SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Krylov/Decomposition.hs"
        [("stored projected dimension witness", "ProjectedDimension")],
      SourceCheck
        "src-native/Moonlight/LinAlg/Native.hs"
        [("native-owned block band payload builder", "symmetricBlockTridiagonalLowerBandPayload ::")]
    ]

assertNativeLapackBoundary :: Assertion
assertNativeLapackBoundary =
  assertForbiddenFragments
    [ SourceCheck
        "src-native/Moonlight/LinAlg/Native.hs"
        [ ("public native Fortran range type", "FortranIndexRange"),
          ("public native Fortran range constructor", "mkFortranIndexRange"),
          ("public native raw symmetric-band value driver", "selectedSymmetricBandEigenValuesLapack"),
          ("public native raw symmetric-band pair driver", "selectedSymmetricBandEigenPairsLapack"),
          ("public native block band payload import", "symmetricBlockTridiagonalLowerBandPayload")
        ],
      SourceCheck
        "src-structured/Moonlight/LinAlg/Pure/Structured/BlockTridiagonal.hs"
        [ ("structured carrier native band payload", "symmetricBlockTridiagonalLowerBandPayload") ]
    ]

assertCSRHotApplyBoundary :: Assertion
assertCSRHotApplyBoundary =
  assertForbiddenFragments
    [ SourceCheck
        "src-spectral/Moonlight/LinAlg/Pure/Operator/Internal.hs"
        [ ("canonical CSR operator revalidating kernel import", "import Moonlight.LinAlg.Internal.VectorOps (csrMatVecU)"),
          ("canonical CSR operator revalidating kernel call", "csrMatVecU ")
        ],
      SourceCheck
        "src-sparse/Moonlight/LinAlg/Pure/Sparse/Types.hs"
        [ ("canonical CSR public matvec revalidating kernel import", "import Moonlight.LinAlg.Internal.VectorOps (csrMatVecU)"),
          ("canonical CSR public matvec revalidating kernel call", "csrMatVecU ")
        ]
    ]

assertCanonicalSparseStorageSurface :: Assertion
assertCanonicalSparseStorageSurface =
  assertForbiddenFragments
    [ SourceCheck
        "src-sparse/Moonlight/LinAlg/Pure/Sparse/Types.hs"
        [ ("CSR row-offset list accessor", "csrRowOffsets ::"),
          ("CSR column-index list accessor", "csrColumnIndices ::"),
          ("CSR value list accessor", "csrValues ::"),
          ("CSC column-offset list accessor", "cscColumnOffsets ::"),
          ("CSC row-index list accessor", "cscRowIndices ::"),
          ("CSC value list accessor", "cscValues ::"),
          ("CSR list matvec wrapper", "csrMatVec ::")
        ],
      SourceCheck
        "src-public/Moonlight/LinAlg/Sparse.hs"
        [ ("public CSR row-offset list accessor", "csrRowOffsets,"),
          ("public CSR column-index list accessor", "csrColumnIndices,"),
          ("public CSR value list accessor", "csrValues,"),
          ("public CSC column-offset list accessor", "cscColumnOffsets,"),
          ("public CSC row-index list accessor", "cscRowIndices,"),
          ("public CSC value list accessor", "cscValues,"),
          ("public CSR list matvec wrapper", "csrMatVec,")
        ]
    ]

assertSparseSolverHotSurface :: Assertion
assertSparseSolverHotSurface =
  assertForbiddenFragments
    ( sparseSolverCheck
        <$> [ "src-sparse/Moonlight/LinAlg/Pure/Sparse/Solver/CG.hs",
              "src-sparse/Moonlight/LinAlg/Pure/Sparse/Solver/GMRES.hs",
              "src-sparse/Moonlight/LinAlg/Pure/Sparse/Solver/Stationary.hs",
              "src-sparse/Moonlight/LinAlg/Pure/Sparse/Solver/Preconditioner.hs",
              "src-sparse/Moonlight/LinAlg/Pure/Sparse/Solver/Mutable.hs"
            ]
    )

sparseSolverCheck :: FilePath -> SourceCheck
sparseSolverCheck pathValue =
  SourceCheck
    pathValue
    [ ("list vector payload", "[Double]"),
      ("list drop in solver hot path", "drop "),
      ("list replacement helper", "replaceAt"),
      ("list concatenation in solver hot path", "++"),
      ("unboxed vector to-list conversion", "U.toList"),
      ("unboxed vector from-list conversion", "U.fromList"),
      ("boxed vector to-list conversion", "Box.toList")
    ]

assertPublicSparseSolverSurface :: Assertion
assertPublicSparseSolverSurface =
  assertForbiddenFragments
    [ SourceCheck
        "src-public/Moonlight/LinAlg/Sparse.hs"
        [ ("public compiled sparse preconditioner type", "SparsePreconditioner,"),
          ("public sparse preconditioner applier", "applySparsePreconditioner"),
          ("public sparse preconditioner compiler", "compileSparsePreconditioner"),
          ("public direct diagonal preconditioner compiler", "diagonalPreconditioner"),
          ("public direct SSOR preconditioner compiler", "ssorPreconditioner"),
          ("public direct shifted preconditioner compiler", "shiftedDiagonalPreconditioner"),
          ("public PCG entry point", "solveSparsePCG"),
          ("public GMRES family wrapper", "solveSparseGMRESWithFamily")
        ]
    ]

assertLinalgDocsAvoidDeletedSpectralAPIs :: Assertion
assertLinalgDocsAvoidDeletedSpectralAPIs =
  assertForbiddenFragments
    [ SourceCheck
        "README.md"
        deletedSpectralDocFragments,
      SourceCheck
        "docs/CONSTRUCTION.md"
        deletedSpectralDocFragments,
      SourceCheck
        "CHANGELOG.md"
        deletedSpectralDocFragments
    ]

deletedSpectralDocFragments :: [(String, String)]
deletedSpectralDocFragments =
  [ ("deleted list-valued self-adjoint constructor", "declaredSelfAdjointLinearOperator"),
    ("deleted list-valued fallback seed setter", "withEigenFallbackInitialList"),
    ("deleted public operator source tag", "LinearOperatorSource"),
    ("deleted Ritz pair result", "RitzPair"),
    ("deleted projected solve policy", "ProjectedSolvePolicy"),
    ("deleted path tridiagonal helper name", "pathLaplacianTridiagonal")
  ]

assertSublibrarySliceDiscipline :: Assertion
assertSublibrarySliceDiscipline = do
  discoveredSlices <- traverse discoverSliceSources sliceSourceRoots
  let discoveredSources = concatMap snd discoveredSlices
      missingNestedDomainSources =
        filter
          (\requiredSuffix -> not (any (requiredSuffix `isSuffixOf`) discoveredSources))
          [ "src-domain/Moonlight/LinAlg/Pure/Domain/Smith/Multimodular.hs",
            "src-domain/Moonlight/LinAlg/Pure/Domain/Smith/Witnessed.hs"
          ]
      sourceChecks =
        concatMap
          (\(sourceRoot, sourcePaths) -> sourceCheckFor (sliceForbiddenFragments sourceRoot) <$> sourcePaths)
          discoveredSlices
  assertBool
    ("recursive slice discovery missed nested domain modules: " <> show missingNestedDomainSources)
    (null missingNestedDomainSources)
  assertForbiddenFragments sourceChecks

discoverSliceSources :: SliceSourceRoot -> IO (SliceSourceRoot, [FilePath])
discoverSliceSources sourceRoot = do
  resolvedRoot <- resolveSourceDirectory (sliceSourceRoot sourceRoot)
  sourcePaths <-
    case resolvedRoot of
      Nothing ->
        assertFailure ("architecture source root is not reachable from test cwd: " <> sliceSourceRoot sourceRoot)
          *> pure []
      Just rootPath -> discoverHaskellSources rootPath
  assertBool
    ("architecture source root contains no Haskell modules: " <> sliceSourceRoot sourceRoot)
    (not (null sourcePaths))
  pure (sourceRoot, sourcePaths)

discoverHaskellSources :: FilePath -> IO [FilePath]
discoverHaskellSources rootPath = do
  childNames <- sort <$> listDirectory rootPath
  concat
    <$> traverse
      (\childName ->
        let childPath = rootPath </> childName
         in doesDirectoryExist childPath >>= \case
              True -> discoverHaskellSources childPath
              False -> pure [childPath | takeExtension childPath == ".hs"]
      )
      childNames

sourceCheckFor :: [(String, String)] -> FilePath -> SourceCheck
sourceCheckFor fragments pathValue =
  SourceCheck pathValue fragments

noInPackageImports :: [(String, String)]
noInPackageImports =
  forbiddenSliceImports
    [ ("in-package", "Moonlight.LinAlg.")
    ]

carrierForbiddenImports :: [(String, String)]
carrierForbiddenImports =
  forbiddenSliceImports
    [ ("dense slice", "Moonlight.LinAlg.Internal.Backend"),
      ("dense slice", "Moonlight.LinAlg.Internal.Dense."),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Basic"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Block"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Decomposition"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Dynamic"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Exterior"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Field"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.GF2"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Solver"),
      ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("eigen slice", "Moonlight.LinAlg.Internal.Eigen"),
      ("geometry slice", "Moonlight.LinAlg.Pure.Geometry"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("sparse slice", "Moonlight.LinAlg.Pure.Sparse"),
      ("spectral Krylov slice", "Moonlight.LinAlg.Pure.Krylov"),
      ("spectral operator slice", "Moonlight.LinAlg.Pure.Operator"),
      ("spectral solve slice", "Moonlight.LinAlg.Pure.Spectral"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics"),
      ("structured slice", "Moonlight.LinAlg.Pure.Structured")
    ]

eigenForbiddenImports :: [(String, String)]
eigenForbiddenImports =
  forbiddenSliceImports
    [ ("dense slice", "Moonlight.LinAlg.Internal.Backend"),
      ("dense slice", "Moonlight.LinAlg.Internal.Dense."),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Basic"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Block"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Decomposition"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Dynamic"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Exterior"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Field"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.GF2"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Solver"),
      ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("geometry slice", "Moonlight.LinAlg.Pure.Geometry"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("sparse slice", "Moonlight.LinAlg.Pure.Sparse"),
      ("spectral Krylov slice", "Moonlight.LinAlg.Pure.Krylov"),
      ("spectral operator slice", "Moonlight.LinAlg.Pure.Operator"),
      ("spectral solve slice", "Moonlight.LinAlg.Pure.Spectral"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics"),
      ("structured slice", "Moonlight.LinAlg.Pure.Structured")
    ]

geometryForbiddenImports :: [(String, String)]
geometryForbiddenImports =
  forbiddenSliceImports
    [ ("dense slice", "Moonlight.LinAlg.Internal.Backend"),
      ("dense slice", "Moonlight.LinAlg.Internal.Dense."),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Basic"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Block"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Decomposition"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Dynamic"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Exterior"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Field"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.GF2"),
      ("dense slice", "Moonlight.LinAlg.Pure.Dense.Solver"),
      ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("eigen slice", "Moonlight.LinAlg.Internal.Eigen"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("sparse slice", "Moonlight.LinAlg.Pure.Sparse"),
      ("spectral Krylov slice", "Moonlight.LinAlg.Pure.Krylov"),
      ("spectral operator slice", "Moonlight.LinAlg.Pure.Operator"),
      ("spectral solve slice", "Moonlight.LinAlg.Pure.Spectral"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics"),
      ("structured slice", "Moonlight.LinAlg.Pure.Structured")
    ]

denseForbiddenImports :: [(String, String)]
denseForbiddenImports =
  forbiddenSliceImports
    [ ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("geometry slice", "Moonlight.LinAlg.Pure.Geometry"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("sparse slice", "Moonlight.LinAlg.Pure.Sparse"),
      ("spectral Krylov slice", "Moonlight.LinAlg.Pure.Krylov"),
      ("spectral operator slice", "Moonlight.LinAlg.Pure.Operator"),
      ("spectral solve slice", "Moonlight.LinAlg.Pure.Spectral"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics"),
      ("structured slice", "Moonlight.LinAlg.Pure.Structured")
    ]

domainForbiddenImports :: [(String, String)]
domainForbiddenImports =
  forbiddenSliceImports
    [ ("eigen slice", "Moonlight.LinAlg.Internal.Eigen"),
      ("geometry slice", "Moonlight.LinAlg.Pure.Geometry"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("sparse slice", "Moonlight.LinAlg.Pure.Sparse"),
      ("spectral Krylov slice", "Moonlight.LinAlg.Pure.Krylov"),
      ("spectral operator slice", "Moonlight.LinAlg.Pure.Operator"),
      ("spectral solve slice", "Moonlight.LinAlg.Pure.Spectral"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics"),
      ("structured slice", "Moonlight.LinAlg.Pure.Structured")
    ]

sparseForbiddenImports :: [(String, String)]
sparseForbiddenImports =
  forbiddenSliceImports
    [ ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("eigen slice", "Moonlight.LinAlg.Internal.Eigen"),
      ("geometry slice", "Moonlight.LinAlg.Pure.Geometry"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("spectral Krylov slice", "Moonlight.LinAlg.Pure.Krylov"),
      ("spectral operator slice", "Moonlight.LinAlg.Pure.Operator"),
      ("spectral solve slice", "Moonlight.LinAlg.Pure.Spectral"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics")
    ]

staticsForbiddenImports :: [(String, String)]
staticsForbiddenImports =
  forbiddenSliceImports
    [ ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("eigen slice", "Moonlight.LinAlg.Internal.Eigen"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("sparse slice", "Moonlight.LinAlg.Pure.Sparse"),
      ("spectral Krylov slice", "Moonlight.LinAlg.Pure.Krylov"),
      ("spectral operator slice", "Moonlight.LinAlg.Pure.Operator"),
      ("spectral solve slice", "Moonlight.LinAlg.Pure.Spectral"),
      ("structured slice", "Moonlight.LinAlg.Pure.Structured")
    ]

spectralForbiddenImports :: [(String, String)]
spectralForbiddenImports =
  forbiddenSliceImports
    [ ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("geometry slice", "Moonlight.LinAlg.Pure.Geometry"),
      ("native slice", "Moonlight.LinAlg.Effect.Native"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics")
    ]

nativeForbiddenImports :: [(String, String)]
nativeForbiddenImports =
  forbiddenSliceImports
    [ ("domain slice", "Moonlight.LinAlg.Pure.Domain"),
      ("geometry slice", "Moonlight.LinAlg.Pure.Geometry"),
      ("sparse slice", "Moonlight.LinAlg.Pure.Sparse"),
      ("statics slice", "Moonlight.LinAlg.Pure.Statics")
    ]

forbiddenSliceImports :: [(String, String)] -> [(String, String)]
forbiddenSliceImports slicePrefixes =
  concatMap importPrefixFragments slicePrefixes

importPrefixFragments :: (String, String) -> [(String, String)]
importPrefixFragments (labelValue, prefixValue) =
  [ (labelValue <> " import", "import " <> prefixValue),
    (labelValue <> " qualified import", "import qualified " <> prefixValue)
  ]

sliceSourceRoots :: [SliceSourceRoot]
sliceSourceRoots =
  [ SliceSourceRoot "src-carrier" carrierForbiddenImports,
    SliceSourceRoot "src-structured" noInPackageImports,
    SliceSourceRoot "src-eigen" eigenForbiddenImports,
    SliceSourceRoot "src-geometry" geometryForbiddenImports,
    SliceSourceRoot "src-dense" denseForbiddenImports,
    SliceSourceRoot "src-domain" domainForbiddenImports,
    SliceSourceRoot "src-sparse" sparseForbiddenImports,
    SliceSourceRoot "src-statics" staticsForbiddenImports,
    SliceSourceRoot "src-spectral" spectralForbiddenImports,
    SliceSourceRoot "src-native" nativeForbiddenImports
  ]

assertForbiddenFragments :: [SourceCheck] -> Assertion
assertForbiddenFragments sourceChecks = do
  violations <- concat <$> traverse checkSource sourceChecks
  assertBool
    ("forbidden architecture fragments remain:\n" <> unlines violations)
    (null violations)

checkSource :: SourceCheck -> IO [String]
checkSource sourceCheck = do
  contents <- readSourceFile (sourcePath sourceCheck)
  pure
    ( violationMessage (sourcePath sourceCheck)
        <$> filter
          (\(_, forbiddenFragment) -> forbiddenFragment `isInfixOf` contents)
          (forbiddenFragments sourceCheck)
    )

violationMessage :: FilePath -> (String, String) -> String
violationMessage pathValue (labelValue, fragmentValue) =
  pathValue <> " contains " <> labelValue <> ": " <> show fragmentValue

readSourceFile :: FilePath -> IO String
readSourceFile relativePath =
  resolveSourcePath relativePath >>= \case
    Just resolvedPath -> readFile resolvedPath
    Nothing -> assertFailure ("architecture source file is not reachable from test cwd: " <> relativePath) *> pure ""

existingMissingSource :: MissingSource -> IO (Maybe String)
existingMissingSource missingSource = do
  exists <- sourceExists (missingPath missingSource)
  pure
    ( if exists
        then Just (missingPath missingSource <> " (" <> missingLabel missingSource <> ")")
        else Nothing
    )

sourceExists :: FilePath -> IO Bool
sourceExists relativePath =
  maybe False (const True) <$> resolveSourcePath relativePath

resolveSourcePath :: FilePath -> IO (Maybe FilePath)
resolveSourcePath relativePath =
  firstJust <$> traverse existingCandidate (sourcePathCandidates relativePath)

resolveSourceDirectory :: FilePath -> IO (Maybe FilePath)
resolveSourceDirectory relativePath =
  firstJust <$> traverse existingDirectoryCandidate (sourcePathCandidates relativePath)

sourcePathCandidates :: FilePath -> [FilePath]
sourcePathCandidates relativePath =
  [ relativePath,
    "foundation/moonlight-linalg/" <> relativePath,
    "compiler/foundation/moonlight-linalg/" <> relativePath
  ]

existingCandidate :: FilePath -> IO (Maybe FilePath)
existingCandidate candidatePath = do
  exists <- doesFileExist candidatePath
  pure (if exists then Just candidatePath else Nothing)

existingDirectoryCandidate :: FilePath -> IO (Maybe FilePath)
existingDirectoryCandidate candidatePath = do
  exists <- doesDirectoryExist candidatePath
  pure (if exists then Just candidatePath else Nothing)

firstJust :: [Maybe value] -> Maybe value
firstJust =
  foldr (<|>) Nothing

module SparseSolverSpec
  ( tests,
  )
where

import Moonlight.Core (MoonlightError)
import Moonlight.LinAlg.Sparse
  ( IC0Config (..),
    SparseConjugateGradientConfig (..),
    SparseGMRESConfig (..),
    SparsePreconditionerFamily (..),
    SparseIterativeFailure (..),
    SparseIterativeResult,
    SparseStationaryIterationConfig (..),
    sparseIterations,
    sparseResidualNorm,
    sparseSolution,
    solveSparseCG,
    solveSparseGMRES,
    solveSparseJacobi,
    solveSparseRichardson,
  )
import Moonlight.LinAlg.Sparse
  ( SparseCSR,
    cooToCSR,
    mkSparseCOO,
  )
import Moonlight.LinAlg.Pure.Sparse.Solver.Preconditioner
  ( compileSparsePreconditioner,
  )
import qualified Data.Vector.Unboxed as U
import Data.Foldable (traverse_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "SparseSolver"
    [ testCase "diagonal preconditioner rejects zero diagonal" testDiagonalPreconditionerRejectsZeroDiagonal,
      testCase "identity preconditioner family compiles without matrix assumptions" testIdentityPreconditionerFamilyCompiles,
      testCase "shifted diagonal preconditioner regularizes zero diagonals" testShiftedDiagonalPreconditionerRegularizesZeroDiagonal,
      testCase "SSOR preconditioner rejects invalid relaxation" testSsorPreconditionerRejectsInvalidRelaxation,
      testCase "IC(0) preconditioner rejects nonpositive pivot" testIC0PreconditionerRejectsNonpositivePivot,
      testCase "sparse CG solves a small SPD system" testSparseCGSolvesSpd,
      testCase "sparse CG with diagonal family solves a small SPD system" testSparseCGWithFamilySolvesSpd,
      testCase "sparse CG with shifted diagonal family solves a small SPD system" testSparseCGWithShiftedFamilySolvesSpd,
      testCase "sparse CG with SSOR family solves a small SPD system" testSparseCGWithSsorFamilySolvesSpd,
      testCase "sparse CG with IC(0) family solves an anchored SPD Laplacian" testSparseCGWithIC0FamilySolvesAnchoredLaplacian,
      testCase "restarted sparse GMRES accumulates Arnoldi work across cycles" testRestartedSparseGmresAccumulatesArnoldiWork,
      testCase "sparse Jacobi solves a diagonal system" testSparseJacobiSolvesDiagonal,
      testCase "sparse Richardson solves a diagonal system" testSparseRichardsonSolvesDiagonal,
      testCase "Richardson converges on the 6x6 SPD equicorrelation counterexample" testRichardsonEquicorrelationRegression,
      testCase "all iterative solvers reject non-finite matrix entries" testSolversRejectNonFiniteMatrix,
      testCase "all iterative solvers reject non-finite right-hand sides" testSolversRejectNonFiniteRhs,
      testCase "all iterative solvers reject non-finite initial guesses" testSolversRejectNonFiniteGuess,
      testCase "all iterative solvers reject negative and NaN tolerances" testSolversRejectInvalidTolerance,
      testCase "stationary solvers reject method-invalid damping" testStationarySolversRejectInvalidDamping,
      testCase "GMRES reports zero-operator projected breakdown before correction" testGmresRejectsZeroProjectedDiagonal,
      testCase "GMRES reports scale-negligible projected breakdown" testGmresRejectsNearProjectedBreakdown,
      testCase "GMRES reports non-finite residual arithmetic" testGmresRejectsNonFiniteResidualArithmetic,
      testCase "GMRES handles very large finite Hessenberg entries" testGmresHandlesLargeFiniteHessenberg,
      testCase "GMRES accepts certified happy breakdown" testGmresHappyBreakdown,
      testCase "GMRES rejects overflowing workspace cardinality before allocation" testGmresRejectsOverflowingWorkspace
    ]

testDiagonalPreconditionerRejectsZeroDiagonal :: Assertion
testDiagonalPreconditionerRejectsZeroDiagonal =
  withSparseCSRFixture (csrFixture 2 2 [(0, 0, 1.0), (1, 0, 2.0)]) $ \matrixValue ->
    case compileSparsePreconditioner DiagonalJacobiSparsePreconditionerFamily matrixValue of
      Left (SparseInvalidInput _) ->
        pure ()
      Left failureValue ->
        assertFailure ("unexpected sparse preconditioner failure: " <> show failureValue)
      Right _ ->
        assertFailure "expected diagonal preconditioner to reject a zero diagonal"

testIdentityPreconditionerFamilyCompiles :: Assertion
testIdentityPreconditionerFamilyCompiles =
  withSparseCSRFixture (csrFixture 2 2 [(0, 1, 2.0)]) $ \matrixValue ->
    case compileSparsePreconditioner IdentitySparsePreconditionerFamily matrixValue of
      Left failureValue ->
        assertFailure ("identity preconditioner family failed: " <> show failureValue)
      Right _ ->
        pure ()

testShiftedDiagonalPreconditionerRegularizesZeroDiagonal :: Assertion
testShiftedDiagonalPreconditionerRegularizesZeroDiagonal =
  withSparseCSRFixture (csrFixture 2 2 [(0, 1, 1.0), (1, 1, 3.0)]) $ \matrixValue ->
    case compileSparsePreconditioner (ShiftedDiagonalJacobiSparsePreconditionerFamily 0.5) matrixValue of
      Left failureValue ->
        assertFailure ("shifted diagonal preconditioner failed: " <> show failureValue)
      Right _ ->
        pure ()

testSsorPreconditionerRejectsInvalidRelaxation :: Assertion
testSsorPreconditionerRejectsInvalidRelaxation =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case compileSparsePreconditioner (SsorSparsePreconditionerFamily 2.0) matrixValue of
      Left (SparseInvalidInput _) ->
        pure ()
      Left failureValue ->
        assertFailure ("unexpected SSOR preconditioner failure: " <> show failureValue)
      Right _ ->
        assertFailure "expected SSOR preconditioner to reject relaxation outside (0, 2)"

testIC0PreconditionerRejectsNonpositivePivot :: Assertion
testIC0PreconditionerRejectsNonpositivePivot =
  withSparseCSRFixture indefiniteSymmetricMatrix $ \matrixValue ->
    case compileSparsePreconditioner (IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)) matrixValue of
      Left (SparseNonpositivePivot 1 _) ->
        pure ()
      Left failureValue ->
        assertFailure ("unexpected IC(0) preconditioner failure: " <> show failureValue)
      Right _ ->
        assertFailure "expected IC(0) preconditioner to reject a nonpositive pivot"

testSparseCGSolvesSpd :: Assertion
testSparseCGSolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith IdentitySparsePreconditionerFamily) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithFamilySolvesSpd :: Assertion
testSparseCGWithFamilySolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith DiagonalJacobiSparsePreconditionerFamily) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG with family failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithShiftedFamilySolvesSpd :: Assertion
testSparseCGWithShiftedFamilySolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith (ShiftedDiagonalJacobiSparsePreconditionerFamily 0.25)) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG with shifted family failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithSsorFamilySolvesSpd :: Assertion
testSparseCGWithSsorFamilySolvesSpd =
  withSparseCSRFixture smallSpdMatrix $ \matrixValue ->
    case solveSparseCG (cgConfigWith (SsorSparsePreconditionerFamily 1.0)) matrixValue (U.fromList [1.0, 2.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse CG with SSOR family failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0 / 11.0, 7.0 / 11.0] (sparseSolution resultValue)

testSparseCGWithIC0FamilySolvesAnchoredLaplacian :: Assertion
testSparseCGWithIC0FamilySolvesAnchoredLaplacian =
  withSparseCSRFixture matrixValue $ \csrValue ->
    case solveSparseCG ic0AnchoredLaplacianConfig csrValue rhsValues initialGuess of
      Left failureValue ->
        assertFailure ("sparse CG with IC(0) family failed: " <> show failureValue)
      Right resultValue ->
        assertBool
          ("IC(0) PCG residual too large: " <> show (sparseResidualNorm resultValue))
          (sparseResidualNorm resultValue <= scgcTolerance ic0AnchoredLaplacianConfig)
  where
    dimension = 32
    matrixValue = anchoredPathLaplacian dimension
    rhsValues = anchoredPathRightHandSide dimension
    initialGuess = U.replicate dimension 0.0

testRestartedSparseGmresAccumulatesArnoldiWork :: Assertion
testRestartedSparseGmresAccumulatesArnoldiWork =
  let dimension = 64
      restartDimension = 8
      matrixValue = anchoredPathLaplacian dimension
      rhsValues = anchoredPathRightHandSide dimension
      initialGuess = U.replicate dimension 0.0
   in withSparseCSRFixture matrixValue $ \csrValue ->
        let cgResult = solveSparseCG cgRestartComparisonConfig csrValue rhsValues initialGuess
            gmresResult = solveSparseGMRES (gmresConfigWith restartDimension) csrValue rhsValues initialGuess
         in case (cgResult, gmresResult) of
              (Left failureValue, _) ->
                assertFailure ("sparse CG comparison failed: " <> show failureValue)
              (_, Left failureValue) ->
                assertFailure ("restarted sparse GMRES failed: " <> show failureValue)
              (Right cgValue, Right gmresValue) -> do
                assertBool
                  ("GMRES iterations did not cross a restart boundary: " <> show (sparseIterations gmresValue))
                  (sparseIterations gmresValue > restartDimension)
                assertBool
                  ("GMRES true residual too large: " <> show (sparseResidualNorm gmresValue))
                  (sparseResidualNorm gmresValue <= 1.0e-8)
                assertVectorApproxWith 1.0e-4 (sparseSolution cgValue) (sparseSolution gmresValue)

testSparseJacobiSolvesDiagonal :: Assertion
testSparseJacobiSolvesDiagonal =
  withSparseCSRFixture diagonalMatrix $ \matrixValue ->
    case solveSparseJacobi stationaryConfig matrixValue (U.fromList [4.0, 9.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse Jacobi failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [2.0, 3.0] (sparseSolution resultValue)

testSparseRichardsonSolvesDiagonal :: Assertion
testSparseRichardsonSolvesDiagonal =
  withSparseCSRFixture diagonalMatrix $ \matrixValue ->
    case solveSparseRichardson stationaryConfig matrixValue (U.fromList [4.0, 9.0]) (U.fromList [0.0, 0.0]) of
      Left failureValue ->
        assertFailure ("sparse Richardson failed: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [2.0, 3.0] (sparseSolution resultValue)

testRichardsonEquicorrelationRegression :: Assertion
testRichardsonEquicorrelationRegression =
  withSparseCSRFixture equicorrelationMatrix $ \matrixValue ->
    case solveSparseRichardson stationaryConfig matrixValue (U.replicate 6 5.5) (U.replicate 6 0.0) of
      Left failureValue ->
        assertFailure ("Richardson failed on SPD equicorrelation matrix: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox (replicate 6 1.0) (sparseSolution resultValue)

testSolversRejectNonFiniteMatrix :: Assertion
testSolversRejectNonFiniteMatrix =
  traverse_
    (\invalidValue ->
      withSparseCSRFixture (csrFixture 1 1 [(0, 0, invalidValue)]) $ \matrixValue ->
        assertSparseInvalidInvocations
          (solverInvocations matrixValue (U.singleton 1.0) (U.singleton 0.0))
    )
    nonFiniteValues

testSolversRejectNonFiniteRhs :: Assertion
testSolversRejectNonFiniteRhs =
  withSparseCSRFixture identityMatrix $ \matrixValue ->
    traverse_
      (\invalidValue ->
        assertSparseInvalidInvocations
          (solverInvocations matrixValue (U.singleton invalidValue) (U.singleton 0.0))
      )
      nonFiniteValues

testSolversRejectNonFiniteGuess :: Assertion
testSolversRejectNonFiniteGuess =
  withSparseCSRFixture identityMatrix $ \matrixValue ->
    traverse_
      (\invalidValue ->
        assertSparseInvalidInvocations
          (solverInvocations matrixValue (U.singleton 1.0) (U.singleton invalidValue))
      )
      nonFiniteValues

testSolversRejectInvalidTolerance :: Assertion
testSolversRejectInvalidTolerance =
  withSparseCSRFixture identityMatrix $ \matrixValue ->
    traverse_
      (\invalidTolerance ->
        assertSparseInvalidInvocations
          (invalidToleranceInvocations invalidTolerance matrixValue)
      )
      [-1.0, nanValue]

testStationarySolversRejectInvalidDamping :: Assertion
testStationarySolversRejectInvalidDamping =
  withSparseCSRFixture identityMatrix $ \matrixValue ->
    traverse_
      assertSparseInvalidInvocation
      [ ( "Jacobi damping above one",
          solveSparseJacobi (stationaryConfig {ssicDamping = 1.01}) matrixValue (U.singleton 1.0) (U.singleton 0.0)
        ),
        ( "Jacobi NaN damping",
          solveSparseJacobi (stationaryConfig {ssicDamping = nanValue}) matrixValue (U.singleton 1.0) (U.singleton 0.0)
        ),
        ( "Richardson damping at two",
          solveSparseRichardson (stationaryConfig {ssicDamping = 2.0}) matrixValue (U.singleton 1.0) (U.singleton 0.0)
        ),
        ( "Richardson infinite damping",
          solveSparseRichardson (stationaryConfig {ssicDamping = infinityValue}) matrixValue (U.singleton 1.0) (U.singleton 0.0)
        )
      ]

testGmresRejectsZeroProjectedDiagonal :: Assertion
testGmresRejectsZeroProjectedDiagonal =
  withSparseCSRFixture (csrFixture 1 1 []) $ \matrixValue ->
    assertSparseInvalidInvocation
      ( "zero operator",
        solveSparseGMRES (identityGmresConfig 1) matrixValue (U.singleton 1.0) (U.singleton 0.0)
      )

testGmresRejectsNearProjectedBreakdown :: Assertion
testGmresRejectsNearProjectedBreakdown =
  withSparseCSRFixture nearlySingularMatrix $ \matrixValue ->
    assertSparseInvalidInvocation
      ( "near projected breakdown",
        solveSparseGMRES (identityGmresConfig 2) matrixValue (U.fromList [1.0, 0.0]) (U.fromList [0.0, 0.0])
      )

testGmresRejectsNonFiniteResidualArithmetic :: Assertion
testGmresRejectsNonFiniteResidualArithmetic =
  withSparseCSRFixture overflowingResidualMatrix $ \matrixValue ->
    assertSparseInvalidInvocation
      ( "non-finite residual arithmetic",
        solveSparseGMRES
          (identityGmresConfig 2)
          matrixValue
          (U.fromList [0.0, 0.0])
          (U.fromList [1.0e308, 1.0e308])
      )

testGmresHandlesLargeFiniteHessenberg :: Assertion
testGmresHandlesLargeFiniteHessenberg =
  withSparseCSRFixture (csrFixture 1 1 [(0, 0, 1.0e300)]) $ \matrixValue ->
    case solveSparseGMRES (identityGmresConfig 1) matrixValue (U.singleton 1.0e300) (U.singleton 0.0) of
      Left failureValue ->
        assertFailure ("GMRES rejected representable scaled Givens arithmetic: " <> show failureValue)
      Right resultValue ->
        assertVectorApprox [1.0] (sparseSolution resultValue)

testGmresHappyBreakdown :: Assertion
testGmresHappyBreakdown =
  withSparseCSRFixture identityMatrix $ \matrixValue ->
    case solveSparseGMRES (identityGmresConfig 1) matrixValue (U.singleton 3.0) (U.singleton 0.0) of
      Left failureValue ->
        assertFailure ("GMRES happy breakdown failed certification: " <> show failureValue)
      Right resultValue -> do
        assertBool "happy breakdown should complete one Arnoldi step" (sparseIterations resultValue == 1)
        assertVectorApprox [3.0] (sparseSolution resultValue)

testGmresRejectsOverflowingWorkspace :: Assertion
testGmresRejectsOverflowingWorkspace =
  withSparseCSRFixture (csrFixture 0 0 []) $ \matrixValue ->
    assertSparseInvalidInvocation
      ( "overflowing workspace",
        solveSparseGMRES
          ( (identityGmresConfig maxBound)
              { sgcIterationLimit = 0
              }
          )
          matrixValue
          U.empty
          U.empty
      )

smallSpdMatrix :: Either MoonlightError (SparseCSR Double)
smallSpdMatrix =
  csrFixture
    2
    2
    [ (0, 0, 4.0),
      (0, 1, 1.0),
      (1, 0, 1.0),
      (1, 1, 3.0)
    ]

indefiniteSymmetricMatrix :: Either MoonlightError (SparseCSR Double)
indefiniteSymmetricMatrix =
  csrFixture
    2
    2
    [ (0, 0, 1.0),
      (0, 1, 2.0),
      (1, 0, 2.0),
      (1, 1, 1.0)
    ]

diagonalMatrix :: Either MoonlightError (SparseCSR Double)
diagonalMatrix =
  csrFixture
    2
    2
    [ (0, 0, 2.0),
      (1, 1, 3.0)
    ]

identityMatrix :: Either MoonlightError (SparseCSR Double)
identityMatrix =
  csrFixture 1 1 [(0, 0, 1.0)]

nearlySingularMatrix :: Either MoonlightError (SparseCSR Double)
nearlySingularMatrix =
  csrFixture
    2
    2
    [ (0, 0, 1.0),
      (0, 1, 1.0),
      (1, 0, 1.0),
      (1, 1, 1.0 + 1.0e-14)
    ]

overflowingResidualMatrix :: Either MoonlightError (SparseCSR Double)
overflowingResidualMatrix =
  csrFixture
    2
    2
    [ (0, 0, 1.0e308),
      (0, 1, -1.0e308)
    ]

equicorrelationMatrix :: Either MoonlightError (SparseCSR Double)
equicorrelationMatrix =
  csrFixture
    6
    6
    ( concatMap
        (\rowIndex ->
          (\columnIndex -> (rowIndex, columnIndex, if rowIndex == columnIndex then 1.0 else 0.9))
            <$> [0 .. 5]
        )
        [0 .. 5]
    )

anchoredPathLaplacian :: Int -> Either MoonlightError (SparseCSR Double)
anchoredPathLaplacian dimension =
  csrFixture
    dimension
    dimension
    ((0, 0, 1.0) : concatMap anchoredPathEdgeEntries [0 .. dimension - 2])

anchoredPathEdgeEntries :: Int -> [(Int, Int, Double)]
anchoredPathEdgeEntries leftIndex =
  let rightIndex = leftIndex + 1
   in [ (leftIndex, leftIndex, 1.0),
        (leftIndex, rightIndex, -1.0),
        (rightIndex, leftIndex, -1.0),
        (rightIndex, rightIndex, 1.0)
      ]

anchoredPathRightHandSide :: Int -> U.Vector Double
anchoredPathRightHandSide dimension =
  U.generate
    dimension
    ( \indexValue ->
        let entryPhase = fromIntegral (indexValue + 1)
            entrySkew = fromIntegral ((indexValue * 7) `mod` 11)
         in 1.0 + sin entryPhase + 0.125 * entrySkew
    )

csrFixture :: Int -> Int -> [(Int, Int, Double)] -> Either MoonlightError (SparseCSR Double)
csrFixture rowCount columnCount entries =
  mkSparseCOO rowCount columnCount entries >>= cooToCSR

withSparseCSRFixture :: Either MoonlightError (SparseCSR Double) -> (SparseCSR Double -> Assertion) -> Assertion
withSparseCSRFixture fixtureValue onFixture =
  case fixtureValue of
    Left err -> assertFailure ("invalid sparse solver fixture: " <> show err)
    Right csrValue -> onFixture csrValue

cgConfigWith :: SparsePreconditionerFamily -> SparseConjugateGradientConfig
cgConfigWith preconditionerFamily =
  SparseConjugateGradientConfig
    { scgcTolerance = 1.0e-10,
      scgcIterationLimit = 32,
      scgcPreconditionerFamily = preconditionerFamily
    }

gmresConfigWith :: Int -> SparseGMRESConfig
gmresConfigWith restartDimension =
  SparseGMRESConfig
    { sgcTolerance = 1.0e-8,
      sgcIterationLimit = 4096,
      sgcRestartDimension = restartDimension,
      sgcPreconditionerFamily = DiagonalJacobiSparsePreconditionerFamily
    }

identityGmresConfig :: Int -> SparseGMRESConfig
identityGmresConfig restartDimension =
  (gmresConfigWith restartDimension)
    { sgcPreconditionerFamily = IdentitySparsePreconditionerFamily
    }

cgRestartComparisonConfig :: SparseConjugateGradientConfig
cgRestartComparisonConfig =
  SparseConjugateGradientConfig
    { scgcTolerance = 1.0e-10,
      scgcIterationLimit = 256,
      scgcPreconditionerFamily = IdentitySparsePreconditionerFamily
    }

ic0AnchoredLaplacianConfig :: SparseConjugateGradientConfig
ic0AnchoredLaplacianConfig =
  SparseConjugateGradientConfig
    { scgcTolerance = 1.0e-8,
      scgcIterationLimit = 128,
      scgcPreconditionerFamily = IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)
    }

stationaryConfig :: SparseStationaryIterationConfig
stationaryConfig =
  SparseStationaryIterationConfig
    { ssicTolerance = 1.0e-8,
      ssicIterationLimit = 128,
      ssicDamping = 1.0
    }

type SolverInvocation = (String, Either SparseIterativeFailure SparseIterativeResult)

solverInvocations :: SparseCSR Double -> U.Vector Double -> U.Vector Double -> [SolverInvocation]
solverInvocations matrixValue rhsValues initialGuess =
  [ ( "CG",
      solveSparseCG (cgConfigWith IdentitySparsePreconditionerFamily) matrixValue rhsValues initialGuess
    ),
    ( "GMRES",
      solveSparseGMRES (identityGmresConfig 1) matrixValue rhsValues initialGuess
    ),
    ( "Jacobi",
      solveSparseJacobi stationaryConfig matrixValue rhsValues initialGuess
    ),
    ( "Richardson",
      solveSparseRichardson stationaryConfig matrixValue rhsValues initialGuess
    )
  ]

invalidToleranceInvocations :: Double -> SparseCSR Double -> [SolverInvocation]
invalidToleranceInvocations invalidTolerance matrixValue =
  [ ( "CG invalid tolerance",
      solveSparseCG
        ((cgConfigWith IdentitySparsePreconditionerFamily) {scgcTolerance = invalidTolerance})
        matrixValue
        (U.singleton 1.0)
        (U.singleton 0.0)
    ),
    ( "GMRES invalid tolerance",
      solveSparseGMRES
        ((identityGmresConfig 1) {sgcTolerance = invalidTolerance})
        matrixValue
        (U.singleton 1.0)
        (U.singleton 0.0)
    ),
    ( "Jacobi invalid tolerance",
      solveSparseJacobi
        (stationaryConfig {ssicTolerance = invalidTolerance})
        matrixValue
        (U.singleton 1.0)
        (U.singleton 0.0)
    ),
    ( "Richardson invalid tolerance",
      solveSparseRichardson
        (stationaryConfig {ssicTolerance = invalidTolerance})
        matrixValue
        (U.singleton 1.0)
        (U.singleton 0.0)
    )
  ]

assertSparseInvalidInvocations :: [SolverInvocation] -> Assertion
assertSparseInvalidInvocations =
  traverse_ assertSparseInvalidInvocation

assertSparseInvalidInvocation :: SolverInvocation -> Assertion
assertSparseInvalidInvocation (solverName, solverResult) =
  case solverResult of
    Left (SparseInvalidInput _) -> pure ()
    Left failureValue ->
      assertFailure (solverName <> " returned the wrong typed obstruction: " <> show failureValue)
    Right resultValue ->
      assertFailure (solverName <> " fabricated success: " <> show resultValue)

nonFiniteValues :: [Double]
nonFiniteValues = [nanValue, infinityValue, negate infinityValue]

nanValue :: Double
nanValue = 0.0 / 0.0

infinityValue :: Double
infinityValue = 1.0 / 0.0

assertVectorApprox :: [Double] -> U.Vector Double -> Assertion
assertVectorApprox expectedValues actualValues =
  assertBool
    ("expected " <> show expectedValues <> " but received " <> show actualValues)
    ( and
        ( zipWith
            (\expectedValue actualValue -> abs (expectedValue - actualValue) <= 1.0e-5)
            expectedValues
            (U.toList actualValues)
        )
    )

assertVectorApproxWith :: Double -> U.Vector Double -> U.Vector Double -> Assertion
assertVectorApproxWith tolerance expectedValues actualValues =
  assertBool
    ("expected " <> show expectedValues <> " but received " <> show actualValues)
    ( U.length expectedValues == U.length actualValues
        && U.and (U.zipWith (\expectedValue actualValue -> abs (expectedValue - actualValue) <= tolerance) expectedValues actualValues)
    )

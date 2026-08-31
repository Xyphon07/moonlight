{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module KrylovSpec
  ( tests,
  )
where

import qualified Data.Vector as Box
import qualified Data.Vector.Unboxed as U
import Data.List (sort)
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Dense (mkDynMatrix)
import Moonlight.LinAlg.Internal.Eigen.Kernels (epsDouble)
import Moonlight.LinAlg.Krylov
  ( SpectrumEnd (..),
    PositiveCount,
    NonNegativeConfigTolerance,
    blockLanczosBasisCount,
    blockLanczosProjectedBlockTridiagonal,
    blockLanczosSymmetric,
    defaultBlockLanczosConfig,
    defaultLanczosConfig,
    LanczosConfig,
    lanczosBasisColumns,
    lanczosProjectedTridiagonal,
    lanczosRestartProjectionBasisColumns,
    lanczosRestartProjectionProjectedPairs,
    lanczosRestartedProjection,
    lanczosStepsCompleted,
    lanczosSymmetric,
    mkNonNegativeConfigTolerance,
    mkPositiveCount,
    withBlockLanczosBlockSize,
    withBlockLanczosIterations,
    withLanczosIterations,
    withLanczosTolerance,
  )
import Moonlight.LinAlg.Operator
  ( addScaledIdentity,
    csrLinearOperator,
    declaredSelfAdjointVectorLinearOperator,
    diagonalLinearOperator,
    graphLaplacianLinearOperator,
    LinearOperator,
    OperatorSymmetry (SelfAdjointOperator),
    operatorShape,
    pathLaplacianLinearOperator,
    runOperatorU,
    scaleLinearOperator,
    selfAdjointCSRLinearOperator,
    sigmaIdentityMinus,
  )
import Moonlight.LinAlg.Pure.Krylov.Projected
  ( SymmetricProjectedOperator (..),
    projectedEigenpairs,
    projectedEigenpairsFromRestartedLanczos,
    projectedEigenvalues,
    projectedSubspaceDimension,
    projectedSubspaceFromBlockLanczos,
    projectedSubspaceFromLanczos,
    projectedSubspaceOperator,
    symmetricProjectedOperatorDimension,
  )
import Moonlight.LinAlg.Pure.Krylov.SelectedTridiagonal
  ( TridiagonalRejection (..),
    selectedSymmetricTridiagonalEigenpairsDirect,
    selectedSymmetricTridiagonalEigenvaluesDirect,
    symmetricTridiagonalFromCSR,
  )
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal
  ( applySymmetricBlockTridiagonalU,
    mkRowMajorBlock,
    mkSymmetricBlockTridiagonal,
    SymmetricBlockTridiagonal,
    symmetrizeRowMajorBlockLower,
    symmetricBlockTridiagonalBandwidth,
    symmetricBlockTridiagonalDimension,
    symmetricBlockTridiagonalEntry,
    symmetricBlockTridiagonalFrobeniusNorm,
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal
  ( SymmetricTridiagonal,
    mkSymmetricTridiagonal,
    symmetricTridiagonalDiagonalEntries,
    symmetricTridiagonalDimension,
    symmetricTridiagonalOffDiagonalEntries,
  )
import Moonlight.LinAlg.Sparse
  ( GraphEdge (..),
    SparseCSR,
    cooToCSR,
    mkSparseCOO,
    pathLaplacianCSR,
    tridiagonalCSR,
  )
import Moonlight.LinAlg.Spectral
  ( CertifiedSelectedEigenpairResult (..),
    Eigenpairs,
    EigenRequest (..),
    SelectedEigenpairCertificationFailure (..),
    SelectedEigenpairOrthonormalityEvidence (..),
    SelectedEigenpairRequestOrderingEvidence (..),
    SelectedEigenpairResidualEvidence (..),
    certifySelectedEigenpairResult,
    defaultEigenSolveConfig,
    eigenpairCount,
    eigenpairDimension,
    eigenpairResidualNorms,
    eigenpairValues,
    eigenpairVectorAt,
    solveEigenRequest,
    withEigenFallbackInitialVector,
    withEigenFallbackLanczosConfig,
  )
import Moonlight.LinAlg.Pure.Spectral.Solve (denseSpectralFallbackDimensionThreshold)
import Moonlight.LinAlg.Native
  ( selectedSymmetricBlockTridiagonalEigenRequestLapack,
    selectedSymmetricTridiagonalEigenRequestLapack,
    symmetricEigenRequestLapack,
  )
import Helpers (extractRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC
import Prelude

tests :: TestTree
tests =
  testGroup
    "Krylov"
    [ testGroup
        "operator"
        [ testCase "operator shape" testOperatorShape,
          testCase "CSR operator matvec" testCSROperatorMatvec,
          testCase "affine normalization applies sigma I minus A" testSigmaIdentityMinusApplies,
          testCase "zero affine scale has deterministic coordinate eigenpairs" testZeroScaleEigenpairs,
          testCase "negative affine scale reverses the requested spectrum end" testNegativeScaleEigenvalues
        ],
      testGroup
        "spectral dispatch"
        [ testCase "diagonal values and pairs are selected directly" testDiagonalSpectralDispatch,
          testCase "path Laplacian values and pairs are closed-form" testPathSpectralDispatch,
          testCase "checked graph Laplacian construction preserves spectral semantics" testGraphLaplacianSpectralDispatch,
          testCase "large cycle graph descent selects the analytic smallest spectrum" testLargeCycleGraphCascadicSpectrum,
          testCase "large graph Laplacian smallest modes descend with certified residuals" testLargeGraphLaplacianCascadicDispatch,
          testCase "generic low-demand fallback densifies through threshold and restarts above it" testGenericFallbackThresholdDispatch,
          testCase "generic high-demand fallback stays on the bounded dense route" testGenericHighDemandFallbackDispatch,
          testCase "requested count above dimension is rejected" testEigenCountRejectsOversubscription
        ],
      testGroup
        "exact tridiagonal structure"
        [ testCase "zero-dimensional tridiagonal requests are rejected" testZeroDimensionTridiagonalRejected,
          testCase "one-dimensional tridiagonal values and pairs are exact" testOneDimensionalTridiagonal,
          testCase "repeated diagonal eigenvalues are checked through the projector" testRepeatedDiagonalProjector,
          testCase "CSR tridiagonal classification preserves tiny non-zero couplings" testTinyCouplingIsStructural,
          testCase "CSR tridiagonal classification rejects out-of-band entries exactly" testOutOfBandCSRRejected,
          testCase "CSR tridiagonal classification rejects asymmetric off-diagonals exactly" testAsymmetricCSRRejected,
          testCase "reducible tridiagonal values split and interleave blocks" testReducibleTridiagonalValues,
          testCase "reducible tridiagonal pairs split and interleave blocks" testReducibleTridiagonalPairs,
          testCase "generic tridiagonal pairs are selected inverse-iteration residual-checked columns" testGenericTridiagonalPairs,
          testCase "selected tridiagonal pairs certify residuals orthonormality count and order" testSelectedTridiagonalPairCertification,
          testCase "generic selected tridiagonal pairs agree with all-pairs on small n" testGenericSelectedTridiagonalPairsAgreeWithAllPairs,
          testCase "clustered irreducible tridiagonal values agree with LAPACK" testClusteredIrreducibleTridiagonalValues,
          testCase "generic irreducible largest values agree with LAPACK order" testGenericIrreducibleLargestValues,
          testCase "extreme scaled irreducible tridiagonal values agree with LAPACK" testExtremeScaledIrreducibleTridiagonalValues,
          testCase "perturbed path is not silently solved as the exact path" testPerturbedPathNotExactPath,
          testCase "extreme affine scale transports eigenvalues without losing order" testExtremeAffineScale
        ],
      testGroup
        "projected block"
        [ testCase "packed symmetric block tridiagonal applies without row materialization" testPackedBlockTridiagonalApply,
          testCase "row-major block rejects wrapped cardinality" testRowMajorBlockCardinalityOverflow,
          testCase "packed block Frobenius norm preserves overflow evidence" testBlockFrobeniusNormPreservesOverflow,
          QC.testProperty "packed block Frobenius norm equals dense reconstruction on mixed block sizes" propBlockFrobeniusNormMatchesDense,
          testCase "block Lanczos builds a structured projected operator" testBlockLanczosStructuredProjection,
          testCase "projected tridiagonal pairs use selected tridiagonal columns" testProjectedTridiagonalSelectedPairs,
          testCase "projected eigensolve rejects oversubscribed requests" testProjectedCountRejectsOversubscription
        ],
      testGroup
        "native selected eigensolve"
        [ testCase "dense selected values agree with selected pairs" testNativeDenseSelectedValuesAgreeWithPairs,
          testCase "generic tridiagonal selected values and pairs use native selected path" testNativeGenericTridiagonalSelected,
          testCase "block-band selected values and pairs agree with dense reference" testNativeBlockBandSelectedAgreesWithDense
        ],
      testGroup
        "Lanczos"
        [ testCase "Lanczos decomposition owns a SymmetricTridiagonal projection" testLanczosProjection,
          testCase "thick restart fallback converges across multiple Krylov cycles" testLanczosThickRestartMultiCycle,
          testCase "thick restart locked Ritz pairs satisfy residual bounds" testLanczosThickRestartLockedResiduals,
          testCase "thick restart projected pairs carry residual evidence" testLanczosThickRestartProjectedResidualEvidence,
          testCase "thick restart basis stays orthogonal across locked and active vectors" testLanczosThickRestartBasisOrthogonality
        ]
    ]

tolerance :: Double
tolerance = 1.0e-7

approxEqual :: Double -> Double -> Bool
approxEqual expected actual =
  abs (expected - actual) <= tolerance * max 1.0 (max (abs expected) (abs actual))

assertApproxList :: [Double] -> [Double] -> Assertion
assertApproxList expected actual =
  assertBool
    ("expected " <> show expected <> " but received " <> show actual)
    (length expected == length actual && and (zipWith approxEqual expected actual))

assertApproxVector :: [Double] -> U.Vector Double -> Assertion
assertApproxVector expected actual =
  assertApproxList expected (U.toList actual)

withPositiveCount :: Int -> (PositiveCount -> Assertion) -> Assertion
withPositiveCount value onCount =
  extractRight (mkPositiveCount value) onCount

assertScaledEigenpairResiduals :: String -> Double -> Eigenpairs -> Assertion
assertScaledEigenpairResiduals context matrixNorm pairs = do
  _ <- traverse (assertScaledEigenpairResidualAt context matrixNorm pairs) [0 .. eigenpairCount pairs - 1]
  pure ()

assertScaledEigenpairResidualAt :: String -> Double -> Eigenpairs -> Int -> Assertion
assertScaledEigenpairResidualAt context matrixNorm pairs columnIndex = do
  eigenvector <- extractEither (eigenpairVectorAt columnIndex pairs)
  let eigenvalue = eigenpairValues pairs `U.unsafeIndex` columnIndex
      residualNorm = eigenpairResidualNorms pairs `U.unsafeIndex` columnIndex
      residualRatio = scaledEigenpairResidualRatio matrixNorm eigenvalue eigenvector residualNorm
      residualLimit = scaledEigenpairResidualLimit pairs
  assertBool
    ( context
        <> " residual ratio exceeded scaled machine bound at column "
        <> show columnIndex
        <> ": ratio="
        <> show residualRatio
        <> ", limit="
        <> show residualLimit
        <> ", residual="
        <> show residualNorm
    )
    (isFiniteDouble residualRatio && residualRatio <= residualLimit)

scaledEigenpairResidualRatio :: Double -> Double -> U.Vector Double -> Double -> Double
scaledEigenpairResidualRatio matrixNorm eigenvalue eigenvector residualNorm =
  residualNorm / max 1.0 ((matrixNorm + abs eigenvalue) * vectorNormU eigenvector)

scaledEigenpairResidualLimit :: Eigenpairs -> Double
scaledEigenpairResidualLimit pairs =
  1.0e7 * max 1.0 (fromIntegral (eigenpairDimension pairs)) * epsDouble

vectorNormU :: U.Vector Double -> Double
vectorNormU values =
  sqrt (U.sum (U.map (\entryValue -> entryValue * entryValue) values))

isFiniteDouble :: Double -> Bool
isFiniteDouble value =
  not (isNaN value || isInfinite value)

denseFrobeniusNorm :: [Double] -> Double
denseFrobeniusNorm values =
  sqrt (sum ((\entryValue -> entryValue * entryValue) <$> values))

tridiagonalFrobeniusNorm :: [Double] -> [Double] -> Double
tridiagonalFrobeniusNorm diagonalValues offDiagonalValues =
  sqrt
    ( sum ((\entryValue -> entryValue * entryValue) <$> diagonalValues)
        + 2.0 * sum ((\entryValue -> entryValue * entryValue) <$> offDiagonalValues)
    )

tridiagonalMatrixNorm :: SymmetricTridiagonal -> Double
tridiagonalMatrixNorm tridiagonalValue =
  tridiagonalFrobeniusNorm
    (symmetricTridiagonalDiagonalEntries tridiagonalValue)
    (symmetricTridiagonalOffDiagonalEntries tridiagonalValue)

pathLaplacianFrobeniusNorm :: Int -> Double
pathLaplacianFrobeniusNorm dimension
  | dimension <= 0 = 0.0
  | dimension == 1 = 0.0
  | otherwise =
      tridiagonalFrobeniusNorm
        (1.0 : (replicate (dimension - 2) 2.0 <> [1.0]))
        (replicate (dimension - 1) (-1.0))

eigenpairProjectorDiagonal :: Eigenpairs -> Either MoonlightError (U.Vector Double)
eigenpairProjectorDiagonal pairs = do
  columns <- traverse (`eigenpairVectorAt` pairs) [0 .. eigenpairCount pairs - 1]
  pure
    ( U.generate
        (eigenpairDimension pairs)
        ( \rowIndex ->
            sum
              ( (\columnVector ->
                   let !entryValue = columnVector `U.unsafeIndex` rowIndex
                    in entryValue * entryValue
                )
                  <$> columns
              )
        )
    )

assertEigenpairColumnsOrthonormal :: String -> Eigenpairs -> Assertion
assertEigenpairColumnsOrthonormal context pairs =
  case traverse (`eigenpairVectorAt` pairs) [0 .. eigenpairCount pairs - 1] of
    Left err -> assertFailure (context <> ": eigenpair column extraction failed: " <> show err)
    Right columns -> do
      _ <-
        traverse
          (assertEigenpairColumnInnerProduct context)
          [ (leftIndex, rightIndex, leftColumn, rightColumn)
            | (leftIndex, leftColumn) <- zip [0 ..] columns,
              (rightIndex, rightColumn) <- zip [0 ..] columns,
              leftIndex <= rightIndex
          ]
      pure ()

assertEigenpairColumnInnerProduct :: String -> (Int, Int, U.Vector Double, U.Vector Double) -> Assertion
assertEigenpairColumnInnerProduct context (leftIndex, rightIndex, leftColumn, rightColumn) =
  let !actual = vectorDotU leftColumn rightColumn
      !expected =
        if leftIndex == rightIndex
          then 1.0
          else 0.0
      !limit = 1.0e-6
   in assertBool
        ( context
            <> " columns are not orthonormal at ("
            <> show leftIndex
            <> ", "
            <> show rightIndex
            <> "): "
            <> show actual
        )
        (abs (actual - expected) <= limit)

assertBasisColumnsOrthonormal :: String -> Box.Vector (U.Vector Double) -> Assertion
assertBasisColumnsOrthonormal context basisColumns = do
  _ <-
    traverse
      (assertEigenpairColumnInnerProduct context)
      [ (leftIndex, rightIndex, leftColumn, rightColumn)
        | (leftIndex, leftColumn) <- zip [0 ..] (Box.toList basisColumns),
          (rightIndex, rightColumn) <- zip [0 ..] (Box.toList basisColumns),
          leftIndex <= rightIndex
      ]
  pure ()

assertEigenpairResidualsBelow :: String -> Double -> Eigenpairs -> Assertion
assertEigenpairResidualsBelow context residualLimit pairs =
  assertBool
    (context <> " residuals exceeded " <> show residualLimit <> ": " <> show (U.toList (eigenpairResidualNorms pairs)))
    (U.all (\residualNorm -> isFiniteDouble residualNorm && residualNorm <= residualLimit) (eigenpairResidualNorms pairs))

vectorDotU :: U.Vector Double -> U.Vector Double -> Double
vectorDotU leftVector rightVector =
  U.sum (U.zipWith (*) leftVector rightVector)

testOperatorShape :: Assertion
testOperatorShape =
  extractRight (pathLaplacianLinearOperator 4) $ \operatorValue ->
    assertEqual "path operator shape" (4, 4) (operatorShape operatorValue)

testCSROperatorMatvec :: Assertion
testCSROperatorMatvec =
  extractRight (tridiagonalCSR [2.0, 3.0, 4.0] [-1.0, -2.0]) $ \csrValue ->
    case runOperatorU (csrLinearOperator csrValue) (U.fromList [1.0, 2.0, 3.0]) of
      Left err -> assertFailure ("CSR operator failed: " <> show err)
      Right actual -> assertApproxVector [0.0, -1.0, 8.0] actual

testSigmaIdentityMinusApplies :: Assertion
testSigmaIdentityMinusApplies =
  extractRight (diagonalLinearOperator (U.fromList [2.0, 5.0])) $ \operatorValue ->
    case runOperatorU (sigmaIdentityMinus 7.0 operatorValue) (U.fromList [3.0, 11.0]) of
      Left err -> assertFailure ("sigma identity minus failed: " <> show err)
      Right actual -> assertApproxVector [15.0, 22.0] actual

testZeroScaleEigenpairs :: Assertion
testZeroScaleEigenpairs =
  withPositiveCount 2 $ \countValue ->
    extractRight (diagonalLinearOperator (U.fromList [2.0, 5.0, 9.0])) $ \operatorValue ->
      case solveEigenRequest defaultEigenSolveConfig (addScaledIdentity 4.0 (scaleLinearOperator 0.0 operatorValue)) (EigenpairsRequest SmallestEigenvalues countValue) of
        Left err -> assertFailure ("zero-scale eigensolve failed: " <> show err)
        Right pairs -> do
          assertApproxVector [4.0, 4.0] (eigenpairValues pairs)
          assertEqual "ambient dimension" 3 (eigenpairDimension pairs)
          assertEqual "pair count" 2 (eigenpairCount pairs)
          assertApproxVector [0.0, 0.0] (eigenpairResidualNorms pairs)

testNegativeScaleEigenvalues :: Assertion
testNegativeScaleEigenvalues =
  withPositiveCount 2 $ \countValue ->
    extractRight (diagonalLinearOperator (U.fromList [1.0, 3.0, 9.0])) $ \operatorValue ->
      case solveEigenRequest defaultEigenSolveConfig (scaleLinearOperator (-2.0) operatorValue) (EigenvaluesRequest SmallestEigenvalues countValue) of
        Left err -> assertFailure ("negative-scale eigensolve failed: " <> show err)
        Right values -> assertApproxVector [-18.0, -6.0] values

testDiagonalSpectralDispatch :: Assertion
testDiagonalSpectralDispatch =
  withPositiveCount 2 $ \countValue ->
    extractRight (diagonalLinearOperator (U.fromList [3.0, -2.0, 7.0, 1.0])) $ \operatorValue -> do
      case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue) of
        Left err -> assertFailure ("diagonal values failed: " <> show err)
        Right values -> assertApproxVector [-2.0, 1.0] values
      case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest LargestEigenvalues countValue) of
        Left err -> assertFailure ("diagonal pairs failed: " <> show err)
        Right pairs -> do
          assertApproxVector [7.0, 3.0] (eigenpairValues pairs)
          assertApproxVector [0.0, 0.0] (eigenpairResidualNorms pairs)

testPathSpectralDispatch :: Assertion
testPathSpectralDispatch =
  withPositiveCount 3 $ \countValue ->
    extractRight (pathLaplacianLinearOperator 5) $ \operatorValue -> do
      case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue) of
        Left err -> assertFailure ("path values failed: " <> show err)
        Right values -> assertApproxVector (pathLaplacianValues 5 [0, 1, 2]) values
      case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue) of
        Left err -> assertFailure ("path pairs failed: " <> show err)
        Right pairs -> do
          assertEqual "path pair count" 3 (eigenpairCount pairs)
          assertScaledEigenpairResiduals "path Laplacian pairs" (pathLaplacianFrobeniusNorm 5) pairs

testGraphLaplacianSpectralDispatch :: Assertion
testGraphLaplacianSpectralDispatch =
  withPositiveCount 3 $ \countValue ->
    extractRight
      ( graphLaplacianLinearOperator
          [0 :: Int, 1, 2]
          [ GraphEdge 0 1 1.0,
            GraphEdge 1 2 1.0,
            GraphEdge 0 2 1.0
          ]
      )
      $ \operatorValue -> do
        case runOperatorU operatorValue (U.fromList [1.0, 2.0, 4.0]) of
          Left err -> assertFailure ("graph Laplacian operator failed: " <> show err)
          Right actual -> assertApproxVector [-4.0, -1.0, 5.0] actual
        case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue) of
          Left err -> assertFailure ("graph Laplacian eigensolve failed: " <> show err)
          Right pairs -> do
            assertApproxVector [0.0, 3.0, 3.0] (eigenpairValues pairs)
            assertEigenpairResidualsBelow "checked graph Laplacian" 1.0e-10 pairs
            assertEigenpairColumnsOrthonormal "checked graph Laplacian" pairs

testLargeGraphLaplacianCascadicDispatch :: Assertion
testLargeGraphLaplacianCascadicDispatch =
  let dimension = 4096
   in withPositiveCount 3 $ \countValue ->
        extractRight
          ( graphLaplacianLinearOperator
              [0 .. dimension - 1]
              (successorLikeGraphEdges dimension)
          )
          $ \operatorValue ->
            case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue) of
              Left err -> assertFailure ("large graph Laplacian eigensolve failed: " <> show err)
              Right pairs -> do
                let values = U.toList (eigenpairValues pairs)
                assertEqual "large graph pair count" 3 (eigenpairCount pairs)
                assertEqual "large graph ambient dimension" dimension (eigenpairDimension pairs)
                assertEqual "large graph eigenvalue order" (sort values) values
                assertEigenpairResidualsBelow "large graph cascadic pairs" 1.0e-5 pairs
                assertEigenpairColumnsOrthonormal "large graph cascadic pairs" pairs

testLargeCycleGraphCascadicSpectrum :: Assertion
testLargeCycleGraphCascadicSpectrum =
  let dimension = 4096 :: Int
      firstNonzeroEigenvalue = 2.0 - 2.0 * cos (2.0 * pi / fromIntegral dimension)
      expectedValues = [0.0, firstNonzeroEigenvalue, firstNonzeroEigenvalue]
      cycleEdges =
        GraphEdge (dimension - 1) 0 1.0
          : fmap
            (\sourceVertex -> GraphEdge sourceVertex (sourceVertex + 1) 1.0)
            [0 .. dimension - 2]
   in withPositiveCount 3 $ \countValue ->
        extractRight
          (graphLaplacianLinearOperator [0 .. dimension - 1] cycleEdges)
          $ \operatorValue ->
            case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue) of
              Left err -> assertFailure ("large cycle graph eigensolve failed: " <> show err)
              Right pairs -> do
                assertApproxList expectedValues (U.toList (eigenpairValues pairs))
                assertEigenpairResidualsBelow "large cycle cascadic pairs" 1.0e-5 pairs
                assertEigenpairColumnsOrthonormal "large cycle cascadic pairs" pairs

successorLikeGraphEdges :: Int -> [GraphEdge Int]
successorLikeGraphEdges dimension =
  localEdges
    <> strideEdges 8 0.25
    <> strideEdges 31 0.125
  where
    localEdges =
      fmap
        (\sourceVertex -> GraphEdge sourceVertex (sourceVertex + 1) 1.0)
        [0 .. dimension - 2]
    strideEdges strideValue edgeWeight =
      fmap
        (\sourceVertex -> GraphEdge sourceVertex (sourceVertex + strideValue) edgeWeight)
        [0, strideValue .. dimension - strideValue - 1]

testGenericFallbackThresholdDispatch :: Assertion
testGenericFallbackThresholdDispatch =
  withPositiveCount 3 $ \countValue ->
    withPositiveCount 8 $ \iterationCount ->
      extractRight (mkNonNegativeConfigTolerance 1.0e-12) $ \toleranceValue -> do
        _ <-
          traverse
            (assertGenericFallbackThresholdDimension countValue iterationCount toleranceValue)
            [ denseSpectralFallbackDimensionThreshold,
              denseSpectralFallbackDimensionThreshold + 1
            ]
        pure ()

testGenericHighDemandFallbackDispatch :: Assertion
testGenericHighDemandFallbackDispatch =
  let dimension = denseSpectralFallbackDimensionThreshold + 1
   in withPositiveCount dimension $ \countValue ->
        withPositiveCount 8 $ \iterationCount ->
          extractRight (genericThresholdOperator dimension) $ \operatorValue -> do
            let solveConfig =
                  withEigenFallbackLanczosConfig
                    (withLanczosIterations iterationCount defaultLanczosConfig)
                    defaultEigenSolveConfig
            case solveEigenRequest solveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue) of
              Left err -> assertFailure ("generic high-demand values failed: " <> show err)
              Right values -> do
                assertEqual "generic high-demand value count" dimension (U.length values)
                assertApproxVector [1.0, 1.5, 2.25] (U.take 3 values)

assertGenericFallbackThresholdDimension :: PositiveCount -> PositiveCount -> NonNegativeConfigTolerance -> Int -> Assertion
assertGenericFallbackThresholdDimension countValue iterationCount toleranceValue dimension =
  extractRight (genericThresholdOperator dimension) $ \operatorValue -> do
    let solveConfig =
          withEigenFallbackInitialVector (restartSeedVector dimension)
            ( withEigenFallbackLanczosConfig
                (withLanczosTolerance toleranceValue (withLanczosIterations iterationCount defaultLanczosConfig))
                defaultEigenSolveConfig
            )
        expectedValues = [1.0, 1.5, 2.25]
        dispatchContext = "generic fallback n=" <> show dimension
    case solveEigenRequest solveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue) of
      Left err -> assertFailure (dispatchContext <> " values failed: " <> show err)
      Right values -> assertApproxVector expectedValues values
    case solveEigenRequest solveConfig operatorValue (EigenpairsRequest SmallestEigenvalues countValue) of
      Left err -> assertFailure (dispatchContext <> " pairs failed: " <> show err)
      Right pairs -> do
        assertApproxVector expectedValues (eigenpairValues pairs)
        assertEigenpairResidualsBelow dispatchContext 1.0e-5 pairs
        assertEigenpairColumnsOrthonormal dispatchContext pairs

genericThresholdOperator :: Int -> Either MoonlightError (LinearOperator 'SelfAdjointOperator)
genericThresholdOperator dimension =
  declaredSelfAdjointVectorLinearOperator
    dimension
    (Right . U.imap (\entryIndex entryValue -> genericThresholdEigenvalue entryIndex * entryValue))

genericThresholdEigenvalue :: Int -> Double
genericThresholdEigenvalue entryIndex
  | entryIndex == 0 = 1.0
  | entryIndex == 1 = 1.5
  | entryIndex == 2 = 2.25
  | otherwise = 10.0

testEigenCountRejectsOversubscription :: Assertion
testEigenCountRejectsOversubscription =
  withPositiveCount 4 $ \countValue ->
    extractRight (diagonalLinearOperator (U.fromList [1.0, 2.0, 3.0])) $ \operatorValue ->
      case solveEigenRequest defaultEigenSolveConfig operatorValue (EigenvaluesRequest SmallestEigenvalues countValue) of
        Left (InvariantViolation _) -> pure ()
        Left err -> assertFailure ("expected InvariantViolation, got " <> show err)
        Right values -> assertFailure ("expected oversubscription rejection, got " <> show values)

testZeroDimensionTridiagonalRejected :: Assertion
testZeroDimensionTridiagonalRejected =
  extractRight (mkSymmetricTridiagonal [] []) $ \tridiagonalValue -> do
    case selectedSymmetricTridiagonalEigenvaluesDirect SmallestEigenvalues 1 tridiagonalValue of
      Left (InvariantViolation _) -> pure ()
      Left err -> assertFailure ("expected eigenvalue InvariantViolation, got " <> show err)
      Right values -> assertFailure ("expected zero-dimensional eigenvalue rejection, got " <> show values)
    case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 1 tridiagonalValue of
      Left (InvariantViolation _) -> pure ()
      Left err -> assertFailure ("expected eigenpair InvariantViolation, got " <> show err)
      Right pairs -> assertFailure ("expected zero-dimensional eigenpair rejection, got " <> show pairs)

testOneDimensionalTridiagonal :: Assertion
testOneDimensionalTridiagonal =
  extractRight (mkSymmetricTridiagonal [-3.0] []) $ \tridiagonalValue -> do
    case selectedSymmetricTridiagonalEigenvaluesDirect SmallestEigenvalues 1 tridiagonalValue of
      Left err -> assertFailure ("one-dimensional values failed: " <> show err)
      Right values -> assertApproxVector [-3.0] values
    case selectedSymmetricTridiagonalEigenpairsDirect LargestEigenvalues 1 tridiagonalValue of
      Left err -> assertFailure ("one-dimensional pairs failed: " <> show err)
      Right pairs -> do
        assertApproxVector [-3.0] (eigenpairValues pairs)
        assertScaledEigenpairResiduals "one-dimensional tridiagonal" 3.0 pairs

testRepeatedDiagonalProjector :: Assertion
testRepeatedDiagonalProjector =
  extractRight (mkSymmetricTridiagonal [2.0, 2.0, 3.0] [0.0, 0.0]) $ \tridiagonalValue ->
    case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 2 tridiagonalValue of
      Left err -> assertFailure ("repeated diagonal pairs failed: " <> show err)
      Right pairs -> do
        assertApproxVector [2.0, 2.0] (eigenpairValues pairs)
        assertScaledEigenpairResiduals "repeated diagonal tridiagonal" (tridiagonalMatrixNorm tridiagonalValue) pairs
        projectorDiagonal <- extractEither (eigenpairProjectorDiagonal pairs)
        assertApproxVector [1.0, 1.0, 0.0] projectorDiagonal

testTinyCouplingIsStructural :: Assertion
testTinyCouplingIsStructural =
  extractRight (tridiagonalCSR [1.0, 2.0] [1.0e-300]) $ \csrValue ->
    case symmetricTridiagonalFromCSR csrValue of
      Left err -> assertFailure ("classification failed: " <> show err)
      Right (Left rejection) -> assertFailure ("expected accepted tridiagonal, got " <> show rejection)
      Right (Right tridiagonalValue) ->
        assertEqual "tiny coupling must not be collapsed to structural zero" [1.0e-300] (symmetricTridiagonalOffDiagonalEntries tridiagonalValue)

testOutOfBandCSRRejected :: Assertion
testOutOfBandCSRRejected =
  withCSRFixture 3 3 [(0, 0, 1.0), (0, 2, 1.0e-300), (1, 1, 2.0), (2, 0, 1.0e-300), (2, 2, 3.0)] $ \csrValue ->
    case symmetricTridiagonalFromCSR csrValue of
      Left err -> assertFailure ("classification failed before rejection: " <> show err)
      Right (Left (TridiagonalOutOfBandEntry 0 2)) -> pure ()
      Right (Left rejection) -> assertFailure ("unexpected rejection: " <> show rejection)
      Right (Right tridiagonalValue) -> assertFailure ("expected out-of-band rejection, got " <> show tridiagonalValue)

testAsymmetricCSRRejected :: Assertion
testAsymmetricCSRRejected =
  withCSRFixture 2 2 [(0, 0, 1.0), (0, 1, 1.0), (1, 0, 1.0 + 1.0e-12), (1, 1, 2.0)] $ \csrValue ->
    case symmetricTridiagonalFromCSR csrValue of
      Left err -> assertFailure ("classification failed before rejection: " <> show err)
      Right (Left TridiagonalAsymmetricOffDiagonal) -> pure ()
      Right (Left rejection) -> assertFailure ("unexpected rejection: " <> show rejection)
      Right (Right tridiagonalValue) -> assertFailure ("expected asymmetric rejection, got " <> show tridiagonalValue)

testReducibleTridiagonalValues :: Assertion
testReducibleTridiagonalValues =
  extractRight (mkSymmetricTridiagonal [1.0, 3.0, 2.0, 4.0] [0.5, 0.0, 0.25]) $ \tridiagonalValue ->
    case selectedSymmetricTridiagonalEigenvaluesDirect SmallestEigenvalues 3 tridiagonalValue of
      Left err -> assertFailure ("reducible tridiagonal solve failed: " <> show err)
      Right values -> assertApproxVector (take 3 (sortedValues (twoByTwoSymmetricEigenvalues 1.0 0.5 3.0 <> twoByTwoSymmetricEigenvalues 2.0 0.25 4.0))) values

testReducibleTridiagonalPairs :: Assertion
testReducibleTridiagonalPairs =
  extractRight (mkSymmetricTridiagonal [1.0, 3.0, 2.0, 4.0] [0.5, 0.0, 0.25]) $ \tridiagonalValue ->
    case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 3 tridiagonalValue of
      Left err -> assertFailure ("reducible tridiagonal pairs failed: " <> show err)
      Right pairs -> do
        assertApproxVector
          (take 3 (sortedValues (twoByTwoSymmetricEigenvalues 1.0 0.5 3.0 <> twoByTwoSymmetricEigenvalues 2.0 0.25 4.0)))
          (eigenpairValues pairs)
        assertScaledEigenpairResiduals "reducible tridiagonal pairs" (tridiagonalMatrixNorm tridiagonalValue) pairs

testGenericTridiagonalPairs :: Assertion
testGenericTridiagonalPairs =
  extractRight (mkSymmetricTridiagonal [2.0, 2.0, 2.0, 2.0] [-0.75, -0.5, -0.25]) $ \tridiagonalValue ->
    case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 2 tridiagonalValue of
      Left err -> assertFailure ("generic tridiagonal pairs failed: " <> show err)
      Right pairs -> do
        assertEqual "pair dimension" 4 (eigenpairDimension pairs)
        assertEqual "pair count" 2 (eigenpairCount pairs)
        assertScaledEigenpairResiduals "generic tridiagonal selected inverse-iteration pairs" (tridiagonalMatrixNorm tridiagonalValue) pairs
        assertEigenpairColumnsOrthonormal "generic tridiagonal selected inverse-iteration pairs" pairs

testSelectedTridiagonalPairCertification :: Assertion
testSelectedTridiagonalPairCertification =
  extractRight (mkSymmetricTridiagonal [1.0, 2.0, 4.0] [0.0, 0.0]) $ \tridiagonalValue ->
    case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 2 tridiagonalValue of
      Left err -> assertFailure ("selected tridiagonal pairs failed: " <> show err)
      Right pairs ->
        case certifySelectedEigenpairResult SmallestEigenvalues 2 1.0e-12 1.0e-12 pairs of
          Left failureValue ->
            assertFailure ("selected tridiagonal pair certification failed: " <> show failureValue)
          Right
            CertifiedSelectedEigenpairResult
              { certifiedSelectedEigenpairResidualEvidence = residualEvidence,
                certifiedSelectedEigenpairOrthonormalityEvidence = orthonormalityEvidence,
                certifiedSelectedEigenpairRequestOrderingEvidence = requestOrderingEvidence
              } -> do
            assertEqual
              "residual bound evidence"
              1.0e-12
              (selectedEigenpairResidualBound residualEvidence)
            assertEqual
              "orthonormality bound evidence"
              1.0e-12
              (selectedEigenpairOrthonormalityBound orthonormalityEvidence)
            assertEqual
              "requested count evidence"
              2
              (selectedEigenpairRequestedCount requestOrderingEvidence)
            assertEqual
              "certified count evidence"
              2
              (selectedEigenpairCertifiedCount requestOrderingEvidence)
            assertEqual
              "ordering evidence"
              SmallestEigenvalues
              (selectedEigenpairCertifiedOrdering requestOrderingEvidence)
            case certifySelectedEigenpairResult LargestEigenvalues 2 1.0e-12 1.0e-12 pairs of
              Left (SelectedEigenpairCertificationOrderingViolation LargestEigenvalues 0 _ _) ->
                pure ()
              Left failureValue ->
                assertFailure ("expected ordering violation, received " <> show failureValue)
              Right _ ->
                assertFailure "expected largest-order selected certification to reject ascending pairs"

testGenericSelectedTridiagonalPairsAgreeWithAllPairs :: Assertion
testGenericSelectedTridiagonalPairsAgreeWithAllPairs =
  withPositiveCount 5 $ \fullCount ->
    extractRight (mkSymmetricTridiagonal [2.0, 2.5, 3.0, 3.5, 4.0] [-0.31, -0.27, -0.23, -0.19]) $ \tridiagonalValue -> do
      allPairs <-
        selectedSymmetricTridiagonalEigenRequestLapack (EigenpairsRequest SmallestEigenvalues fullCount) tridiagonalValue
          >>= extractEither
      case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 3 tridiagonalValue of
        Left err -> assertFailure ("generic selected tridiagonal pairs failed: " <> show err)
        Right pairs -> do
          assertApproxVector (take 3 (U.toList (eigenpairValues allPairs))) (eigenpairValues pairs)
          assertScaledEigenpairResiduals "generic selected tridiagonal all-pairs agreement" (tridiagonalMatrixNorm tridiagonalValue) pairs
          assertEigenpairColumnsOrthonormal "generic selected tridiagonal all-pairs agreement" pairs

testClusteredIrreducibleTridiagonalValues :: Assertion
testClusteredIrreducibleTridiagonalValues =
  withPositiveCount 3 $ \countValue ->
    extractRight (mkSymmetricTridiagonal [1.0, 1.0 + 1.0e-12, 1.0 + 2.0e-12, 1.0 + 3.0e-12] [1.0e-8, 1.0e-8, 1.0e-8]) $ \tridiagonalValue -> do
      nativeValues <-
        selectedSymmetricTridiagonalEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues countValue) tridiagonalValue
          >>= extractEither
      nativePairs <-
        selectedSymmetricTridiagonalEigenRequestLapack (EigenpairsRequest SmallestEigenvalues countValue) tridiagonalValue
          >>= extractEither
      case selectedSymmetricTridiagonalEigenvaluesDirect SmallestEigenvalues 3 tridiagonalValue of
        Left err -> assertFailure ("clustered tridiagonal values failed: " <> show err)
        Right values -> assertApproxVector (U.toList nativeValues) values
      case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 3 tridiagonalValue of
        Left err -> assertFailure ("clustered tridiagonal pairs failed: " <> show err)
        Right pairs -> do
          assertApproxVector (U.toList nativeValues) (eigenpairValues pairs)
          assertScaledEigenpairResiduals "clustered pure tridiagonal pairs" (tridiagonalMatrixNorm tridiagonalValue) pairs
          assertEigenpairColumnsOrthonormal "clustered pure tridiagonal pairs" pairs
      assertScaledEigenpairResiduals "clustered native tridiagonal pairs" (tridiagonalMatrixNorm tridiagonalValue) nativePairs

testGenericIrreducibleLargestValues :: Assertion
testGenericIrreducibleLargestValues =
  withPositiveCount 4 $ \countValue ->
    extractRight (genericTestTridiagonal 16) $ \tridiagonalValue -> do
      nativeValues <-
        selectedSymmetricTridiagonalEigenRequestLapack (EigenvaluesRequest LargestEigenvalues countValue) tridiagonalValue
          >>= extractEither
      case selectedSymmetricTridiagonalEigenvaluesDirect LargestEigenvalues 4 tridiagonalValue of
        Left err -> assertFailure ("generic largest tridiagonal values failed: " <> show err)
        Right values -> assertApproxVector (U.toList nativeValues) values

testExtremeScaledIrreducibleTridiagonalValues :: Assertion
testExtremeScaledIrreducibleTridiagonalValues =
  withPositiveCount 2 $ \countValue ->
    extractRight (mkSymmetricTridiagonal [1.0e100, 2.0e100, 4.0e100] [1.0e90, -1.0e90]) $ \tridiagonalValue -> do
      nativeValues <-
        selectedSymmetricTridiagonalEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues countValue) tridiagonalValue
          >>= extractEither
      case selectedSymmetricTridiagonalEigenvaluesDirect SmallestEigenvalues 2 tridiagonalValue of
        Left err -> assertFailure ("extreme scaled tridiagonal values failed: " <> show err)
        Right values -> assertApproxVector (U.toList nativeValues) values

testPerturbedPathNotExactPath :: Assertion
testPerturbedPathNotExactPath =
  extractRight (mkSymmetricTridiagonal [1.0, 2.0, 1.0] [-0.95, -1.0]) $ \tridiagonalValue ->
    case selectedSymmetricTridiagonalEigenvaluesDirect SmallestEigenvalues 3 tridiagonalValue of
      Left err -> assertFailure ("perturbed path solve failed: " <> show err)
      Right values ->
        assertBool
          "perturbed path must not reuse exact path spectrum"
          (not (and (zipWith approxEqual [0.0, 1.0, 3.0] (U.toList values))))

testExtremeAffineScale :: Assertion
testExtremeAffineScale =
  withPositiveCount 2 $ \countValue ->
    extractRight (diagonalLinearOperator (U.fromList [-1.0e100, 2.0e100, 3.0e100])) $ \operatorValue ->
      case solveEigenRequest defaultEigenSolveConfig (addScaledIdentity 5.0 (scaleLinearOperator (-0.5) operatorValue)) (EigenvaluesRequest LargestEigenvalues countValue) of
        Left err -> assertFailure ("extreme affine solve failed: " <> show err)
        Right values -> assertApproxVector [5.0e99 + 5.0, -1.0e100 + 5.0] values

testPackedBlockTridiagonalApply :: Assertion
testPackedBlockTridiagonalApply = do
  diagonal0 <- extractEither (mkRowMajorBlock 2 2 (U.fromList [2.0, 0.5, 0.5, 3.0]))
  diagonal1 <- extractEither (mkRowMajorBlock 1 1 (U.singleton 4.0))
  coupling0 <- extractEither (mkRowMajorBlock 1 2 (U.fromList [1.0, -1.0]))
  blockValue <- extractEither (mkSymmetricBlockTridiagonal (Box.fromList [diagonal0, diagonal1]) (Box.singleton coupling0))
  assertEqual "packed block dimension" 3 (symmetricBlockTridiagonalDimension blockValue)
  assertEqual "packed block bandwidth" 2 (symmetricBlockTridiagonalBandwidth blockValue)
  extractRight (symmetricBlockTridiagonalEntry blockValue 0 2) (assertApproxList [1.0] . pure)
  extractRight (symmetricBlockTridiagonalEntry blockValue 1 2) (assertApproxList [-1.0] . pure)
  case applySymmetricBlockTridiagonalU blockValue (U.fromList [1.0, 2.0, 3.0]) of
    Left err -> assertFailure ("block tridiagonal apply failed: " <> show err)
    Right actual -> assertApproxVector [6.0, 3.5, 11.0] actual

testRowMajorBlockCardinalityOverflow :: Assertion
testRowMajorBlockCardinalityOverflow =
  let wrappedDimension = 2 ^ (32 :: Int)
   in assertEqual
        "oversized block cardinality"
        (Left (InvariantViolation "row-major block dimensions exceed Int cardinality"))
        (mkRowMajorBlock wrappedDimension wrappedDimension U.empty)

data GeneratedBlockTridiagonal = GeneratedBlockTridiagonal
  { generatedBlockSizes :: [Int],
    generatedDiagonalPayloads :: [[Double]],
    generatedCouplingPayloads :: [[Double]]
  }
  deriving stock (Show)

instance QC.Arbitrary GeneratedBlockTridiagonal where
  arbitrary = do
    blockCount <- QC.chooseInt (2, 4)
    firstSize <- QC.chooseInt (1, 4)
    secondSize <- QC.elements (filter (/= firstSize) [1 .. 4])
    remainingSizes <- QC.vectorOf (blockCount - 2) (QC.chooseInt (1, 4))
    let blockSizes = firstSize : secondSize : remainingSizes
    diagonalPayloads <- traverse generatedPayload ((\blockSize -> blockSize * blockSize) <$> blockSizes)
    couplingPayloads <- traverse generatedPayload (zipWith (*) (drop 1 blockSizes) blockSizes)
    pure
      GeneratedBlockTridiagonal
        { generatedBlockSizes = blockSizes,
          generatedDiagonalPayloads = diagonalPayloads,
          generatedCouplingPayloads = couplingPayloads
        }
    where
      generatedPayload :: Int -> QC.Gen [Double]
      generatedPayload entryCount =
        QC.vectorOf entryCount (QC.choose (-8.0, 8.0))

propBlockFrobeniusNormMatchesDense :: GeneratedBlockTridiagonal -> QC.Property
propBlockFrobeniusNormMatchesDense GeneratedBlockTridiagonal {..} =
  case generatedBlockValue of
    Left err ->
      QC.counterexample ("generated block construction failed: " <> show err) False
    Right blockValue ->
      case traverse (uncurry (symmetricBlockTridiagonalEntry blockValue)) denseCoordinates of
        Left err ->
          QC.counterexample ("generated dense reconstruction failed: " <> show err) False
        Right denseEntries ->
          QC.counterexample
            ("packed norm differs from dense reconstruction for block sizes " <> show generatedBlockSizes)
            (approxEqual (denseFrobeniusNorm denseEntries) (symmetricBlockTridiagonalFrobeniusNorm blockValue))
  where
    generatedBlockValue = do
      diagonalBlocks <-
        traverse
          (\(blockSize, payload) -> mkRowMajorBlock blockSize blockSize (U.fromList payload) >>= symmetrizeRowMajorBlockLower)
          (zip generatedBlockSizes generatedDiagonalPayloads)
      couplingBlocks <-
        traverse
          (\((previousSize, nextSize), payload) -> mkRowMajorBlock nextSize previousSize (U.fromList payload))
          (zip (zip generatedBlockSizes (drop 1 generatedBlockSizes)) generatedCouplingPayloads)
      mkSymmetricBlockTridiagonal (Box.fromList diagonalBlocks) (Box.fromList couplingBlocks)
    matrixDimension = sum generatedBlockSizes
    denseCoordinates =
      [ (rowIndex, columnIndex)
        | rowIndex <- [0 .. matrixDimension - 1],
          columnIndex <- [0 .. matrixDimension - 1]
      ]

testBlockFrobeniusNormPreservesOverflow :: Assertion
testBlockFrobeniusNormPreservesOverflow =
  extractRight
    ( do
        diagonalBlock <- mkRowMajorBlock 1 1 (U.singleton 1.0e308)
        mkSymmetricBlockTridiagonal (Box.singleton diagonalBlock) Box.empty
    )
    (\blockValue ->
      assertBool
        "finite entries with an unrepresentable squared norm must not collapse to zero"
        (isInfinite (symmetricBlockTridiagonalFrobeniusNorm blockValue))
    )

testBlockLanczosStructuredProjection :: Assertion
testBlockLanczosStructuredProjection =
  withPositiveCount 3 $ \iterationCount ->
    withPositiveCount 2 $ \blockSize ->
      extractRight (pathLaplacianCSR 4 >>= selfAdjointCSRLinearOperator) $ \operatorValue -> do
        let config = withBlockLanczosBlockSize blockSize (withBlockLanczosIterations iterationCount defaultBlockLanczosConfig)
            seedBlock = Box.fromList [U.fromList [1.0, 0.0, 0.0, 0.0], U.fromList [0.0, 1.0, 0.0, 0.0]]
        case blockLanczosSymmetric config operatorValue seedBlock of
          Left err -> assertFailure ("block Lanczos failed: " <> show err)
          Right decomposition -> do
            assertEqual "projected block dimension" (blockLanczosBasisCount decomposition) (symmetricBlockTridiagonalDimension (blockLanczosProjectedBlockTridiagonal decomposition))
            assertEqual "projected subspace dimension" (blockLanczosBasisCount decomposition) (symmetricProjectedOperatorDimension (projectedSubspaceOperator (projectedSubspaceFromBlockLanczos decomposition)))

testProjectedTridiagonalSelectedPairs :: Assertion
testProjectedTridiagonalSelectedPairs =
  withPositiveCount 4 $ \iterationCount ->
    extractRight (pathLaplacianLinearOperator 8) $ \operatorValue ->
      case lanczosSymmetric (withLanczosIterations iterationCount defaultLanczosConfig) operatorValue (U.fromList [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]) of
        Left err -> assertFailure ("Lanczos failed: " <> show err)
        Right decomposition -> do
          let subspace = projectedSubspaceFromLanczos decomposition
          case projectedSubspaceOperator subspace of
            TridiagonalProjectedOperator tridiagonalValue ->
              case selectedSymmetricTridiagonalEigenpairsDirect SmallestEigenvalues 2 tridiagonalValue of
                Left err -> assertFailure ("selected projected tridiagonal reference failed: " <> show err)
                Right selectedPairs ->
                  case projectedEigenpairs SmallestEigenvalues 2 operatorValue subspace of
                    Left err -> assertFailure ("projected selected pairs failed: " <> show err)
                    Right projectedPairs ->
                      assertApproxVector (U.toList (eigenpairValues selectedPairs)) (eigenpairValues projectedPairs)
            BlockTridiagonalProjectedOperator blockValue ->
              assertFailure ("expected tridiagonal projection, got block projection " <> show blockValue)

testProjectedCountRejectsOversubscription :: Assertion
testProjectedCountRejectsOversubscription =
  withPositiveCount 1 $ \iterationCount ->
    extractRight (pathLaplacianLinearOperator 4) $ \operatorValue ->
      case lanczosSymmetric (withLanczosIterations iterationCount defaultLanczosConfig) operatorValue (U.fromList [1.0, 0.0, 0.0, 0.0]) of
        Left err -> assertFailure ("Lanczos failed: " <> show err)
        Right decomposition ->
          let subspace = projectedSubspaceFromLanczos decomposition
              oversubscribedCount = projectedSubspaceDimension subspace + 1
           in case projectedEigenvalues SmallestEigenvalues oversubscribedCount operatorValue subspace of
                Left (InvariantViolation _) -> pure ()
                Left err -> assertFailure ("expected InvariantViolation, got " <> show err)
                Right values -> assertFailure ("expected projected oversubscription rejection, got " <> show values)

testLanczosProjection :: Assertion
testLanczosProjection =
  withPositiveCount 3 $ \iterationCount ->
    extractRight (pathLaplacianLinearOperator 4) $ \operatorValue ->
      case lanczosSymmetric (withLanczosIterations iterationCount defaultLanczosConfig) operatorValue (U.fromList [1.0, 0.0, 0.0, 0.0]) of
        Left err -> assertFailure ("Lanczos failed: " <> show err)
        Right decomposition -> do
          assertEqual "Lanczos projected dimension" (Box.length (lanczosBasisColumns decomposition)) (symmetricTridiagonalDimension (lanczosProjectedTridiagonal decomposition))
          assertBool "Lanczos completed at least one step" (lanczosStepsCompleted decomposition > 0)

testLanczosThickRestartMultiCycle :: Assertion
testLanczosThickRestartMultiCycle =
  withPositiveCount 3 $ \requestedCount ->
    withRestartedLanczosFixture 18 5 $ \operatorValue lanczosConfig seedVector -> do
      let solveConfig =
            withEigenFallbackInitialVector seedVector
              (withEigenFallbackLanczosConfig lanczosConfig defaultEigenSolveConfig)
      case solveEigenRequest solveConfig operatorValue (EigenpairsRequest SmallestEigenvalues requestedCount) of
        Left err -> assertFailure ("thick restart spectral fallback failed: " <> show err)
        Right pairs -> do
          assertEqual "multi-cycle pair count" 3 (eigenpairCount pairs)
          assertEigenpairResidualsBelow "multi-cycle restarted Lanczos" 1.0e-5 pairs
          assertEigenpairColumnsOrthonormal "multi-cycle restarted Lanczos" pairs

testLanczosThickRestartLockedResiduals :: Assertion
testLanczosThickRestartLockedResiduals =
  withRestartedLanczosFixture 20 6 $ \operatorValue lanczosConfig seedVector ->
    case projectedEigenpairsFromRestartedLanczos lanczosConfig SmallestEigenvalues 4 operatorValue seedVector of
      Left err -> assertFailure ("restarted projected eigenpairs failed: " <> show err)
      Right pairs -> do
        assertEqual "locked residual pair count" 4 (eigenpairCount pairs)
        assertEigenpairResidualsBelow "locked restarted Lanczos" 1.0e-5 pairs

testLanczosThickRestartProjectedResidualEvidence :: Assertion
testLanczosThickRestartProjectedResidualEvidence =
  withRestartedLanczosFixture 18 5 $ \operatorValue lanczosConfig seedVector ->
    case lanczosRestartedProjection lanczosConfig SmallestEigenvalues 3 operatorValue seedVector of
      Left err -> assertFailure ("restarted Lanczos projection failed: " <> show err)
      Right restartProjection -> do
        let projectedResiduals = eigenpairResidualNorms (lanczosRestartProjectionProjectedPairs restartProjection)
        assertEqual "projected residual evidence count" 3 (U.length projectedResiduals)
        assertBool
          ("projected residual evidence must be finite: " <> show (U.toList projectedResiduals))
          (U.all isFiniteDouble projectedResiduals)
        assertBool
          ("projected residual evidence must not be stamped as zero: " <> show (U.toList projectedResiduals))
          (U.any (> 0.0) projectedResiduals)

testLanczosThickRestartBasisOrthogonality :: Assertion
testLanczosThickRestartBasisOrthogonality =
  withRestartedLanczosFixture 16 5 $ \operatorValue lanczosConfig seedVector ->
    case lanczosRestartedProjection lanczosConfig SmallestEigenvalues 3 operatorValue seedVector of
      Left err -> assertFailure ("restarted Lanczos projection failed: " <> show err)
      Right restartProjection ->
        assertBasisColumnsOrthonormal
          "restarted Lanczos locked/active basis"
          (lanczosRestartProjectionBasisColumns restartProjection)

testNativeDenseSelectedValuesAgreeWithPairs :: Assertion
testNativeDenseSelectedValuesAgreeWithPairs =
  withPositiveCount 2 $ \countValue -> do
    matrixValue <- extractEither (mkDynMatrix 3 3 [2.0, 1.0, 0.0, 1.0, 2.0, 0.0, 0.0, 0.0, 5.0])
    values <-
      symmetricEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues countValue) matrixValue
        >>= extractEither
    pairs <-
      symmetricEigenRequestLapack (EigenpairsRequest SmallestEigenvalues countValue) matrixValue
        >>= extractEither
    assertApproxVector (U.toList (eigenpairValues pairs)) values
    assertScaledEigenpairResiduals "native dense selected pairs" (denseFrobeniusNorm [2.0, 1.0, 0.0, 1.0, 2.0, 0.0, 0.0, 0.0, 5.0]) pairs

testNativeGenericTridiagonalSelected :: Assertion
testNativeGenericTridiagonalSelected =
  withPositiveCount 4 $ \countValue ->
    extractRight (genericTestTridiagonal 16) $ \tridiagonalValue -> do
      pureValues <-
        extractEither
          (selectedSymmetricTridiagonalEigenvaluesDirect SmallestEigenvalues 4 tridiagonalValue)
      nativeValues <-
        selectedSymmetricTridiagonalEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues countValue) tridiagonalValue
          >>= extractEither
      nativePairs <-
        selectedSymmetricTridiagonalEigenRequestLapack (EigenpairsRequest SmallestEigenvalues countValue) tridiagonalValue
          >>= extractEither
      assertApproxVector (U.toList pureValues) nativeValues
      assertApproxVector (U.toList nativeValues) (eigenpairValues nativePairs)
      assertScaledEigenpairResiduals "native generic tridiagonal selected pairs" (tridiagonalMatrixNorm tridiagonalValue) nativePairs

testNativeBlockBandSelectedAgreesWithDense :: Assertion
testNativeBlockBandSelectedAgreesWithDense =
  withPositiveCount 2 $ \countValue -> do
    blockValue <- nativeBlockFixture
    denseMatrix <- extractEither (mkDynMatrix 3 3 [2.0, 0.5, 1.0, 0.5, 3.0, -1.0, 1.0, -1.0, 4.0])
    denseValues <-
      symmetricEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues countValue) denseMatrix
        >>= extractEither
    blockValues <-
      selectedSymmetricBlockTridiagonalEigenRequestLapack (EigenvaluesRequest SmallestEigenvalues countValue) blockValue
        >>= extractEither
    blockPairs <-
      selectedSymmetricBlockTridiagonalEigenRequestLapack (EigenpairsRequest SmallestEigenvalues countValue) blockValue
        >>= extractEither
    assertApproxVector (U.toList denseValues) blockValues
    assertApproxVector (U.toList denseValues) (eigenpairValues blockPairs)
    assertScaledEigenpairResiduals "native block-band selected pairs" (denseFrobeniusNorm [2.0, 0.5, 1.0, 0.5, 3.0, -1.0, 1.0, -1.0, 4.0]) blockPairs

pathLaplacianValues :: Int -> [Int] -> [Double]
pathLaplacianValues dimension =
  fmap (\modeIndex -> 2.0 - 2.0 * cos (pi * fromIntegral modeIndex / fromIntegral dimension))

genericTestTridiagonal :: Int -> Either MoonlightError SymmetricTridiagonal
genericTestTridiagonal dimension =
  mkSymmetricTridiagonal
    (genericTestTridiagonalDiagonalEntry <$> [0 .. dimension - 1])
    (genericTestTridiagonalOffDiagonalEntry <$> [0 .. dimension - 2])

genericTestTridiagonalDiagonalEntry :: Int -> Double
genericTestTridiagonalDiagonalEntry indexValue =
  2.0 + fromIntegral (indexValue `mod` 17) / 17.0

genericTestTridiagonalOffDiagonalEntry :: Int -> Double
genericTestTridiagonalOffDiagonalEntry indexValue =
  -0.35 - 0.01 * fromIntegral (indexValue `mod` 5)

withRestartedLanczosFixture ::
  Int ->
  Int ->
  (LinearOperator 'SelfAdjointOperator -> LanczosConfig -> U.Vector Double -> Assertion) ->
  Assertion
withRestartedLanczosFixture dimension iterationLimit onFixture =
  withPositiveCount iterationLimit $ \iterationCount ->
    extractRight (mkNonNegativeConfigTolerance 1.0e-8) $ \toleranceValue ->
      extractRight (genericPentadiagonalCSR dimension >>= selfAdjointCSRLinearOperator) $ \operatorValue ->
        onFixture
          operatorValue
          (withLanczosTolerance toleranceValue (withLanczosIterations iterationCount defaultLanczosConfig))
          (restartSeedVector dimension)

genericPentadiagonalCSR :: Int -> Either MoonlightError (SparseCSR Double)
genericPentadiagonalCSR dimension =
  mkSparseCOO dimension dimension (genericPentadiagonalEntries dimension) >>= cooToCSR

genericPentadiagonalEntries :: Int -> [(Int, Int, Double)]
genericPentadiagonalEntries dimension =
  diagonalEntries <> firstOffDiagonalEntries <> secondOffDiagonalEntries
  where
    diagonalEntries =
      (\rowIndex -> (rowIndex, rowIndex, 4.0 + 0.03 * fromIntegral (rowIndex `mod` 7)))
        <$> [0 .. dimension - 1]
    firstOffDiagonalEntries =
      symmetricBandEntries dimension 1 (\rowIndex -> -1.0 - 0.01 * fromIntegral (rowIndex `mod` 5))
    secondOffDiagonalEntries =
      symmetricBandEntries dimension 2 (\rowIndex -> -0.2 - 0.005 * fromIntegral (rowIndex `mod` 3))

symmetricBandEntries :: Int -> Int -> (Int -> Double) -> [(Int, Int, Double)]
symmetricBandEntries dimension offset entryAt =
  concatMap
    ( \rowIndex ->
        let columnIndex = rowIndex + offset
            entryValue = entryAt rowIndex
         in [(rowIndex, columnIndex, entryValue), (columnIndex, rowIndex, entryValue)]
    )
    [0 .. dimension - offset - 1]

restartSeedVector :: Int -> U.Vector Double
restartSeedVector dimension =
  U.generate dimension (\indexValue -> 1.0 / fromIntegral (indexValue + 1))

twoByTwoSymmetricEigenvalues :: Double -> Double -> Double -> [Double]
twoByTwoSymmetricEigenvalues a b d =
  let traceHalf = 0.5 * (a + d)
      radius = sqrt (((a - d) * 0.5) * ((a - d) * 0.5) + b * b)
   in [traceHalf - radius, traceHalf + radius]

sortedValues :: [Double] -> [Double]
sortedValues = sort

withCSRFixture :: Int -> Int -> [(Int, Int, Double)] -> (SparseCSR Double -> Assertion) -> Assertion
withCSRFixture rowCount columnCount entries onCSR =
  case mkSparseCOO rowCount columnCount entries >>= cooToCSR of
    Left err -> assertFailure ("invalid sparse fixture: " <> show err)
    Right csrValue -> onCSR csrValue

extractEither :: Either MoonlightError value -> IO value
extractEither value =
  case value of
    Left err -> assertFailure ("expected Right, got " <> show err)
    Right resultValue -> pure resultValue

nativeBlockFixture :: IO SymmetricBlockTridiagonal
nativeBlockFixture = do
  diagonal0 <- extractEither (mkRowMajorBlock 2 2 (U.fromList [2.0, 0.5, 0.5, 3.0]))
  diagonal1 <- extractEither (mkRowMajorBlock 1 1 (U.singleton 4.0))
  coupling0 <- extractEither (mkRowMajorBlock 1 2 (U.fromList [1.0, -1.0]))
  extractEither (mkSymmetricBlockTridiagonal (Box.fromList [diagonal0, diagonal1]) (Box.singleton coupling0))

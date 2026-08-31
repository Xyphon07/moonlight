{-# LANGUAGE DataKinds #-}

module SpectralDispatch
  ( spectralDispatchBenchmarks,
    spectralDispatchOnceBenchmarks,
  )
where

import Data.Bifunctor (first)
import Control.DeepSeq (NFData (..))
import qualified Data.Vector.Unboxed as U
import Env (BenchmarkSelection (..))
import Fixtures
  ( bandedSpdCSR,
    diagonalBenchmarkValues,
    genericBenchmarkTridiagonal,
    reducibleBenchmarkTridiagonal,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight,
    OnceBenchmark (..),
    benchmarkWeightEither,
    eigenpairsChecksum,
    eigenpairsResidualValidationChecksum,
    eitherBenchmarkWeight,
    prepareBenchmarkSetup,
  )
import Moonlight.LinAlg.Krylov
  ( SpectrumEnd (..),
    defaultLanczosConfig,
    mkPositiveCount,
    withLanczosIterations,
  )
import Moonlight.LinAlg.Operator
  ( LinearOperator,
    OperatorSymmetry (..),
    diagonalLinearOperator,
    operatorDimension,
    pathLaplacianLinearOperator,
    runOperatorU,
    selfAdjointCSRLinearOperator,
    symmetricTridiagonalLinearOperator,
  )
import Moonlight.LinAlg.Pure.Krylov.SelectedTridiagonal (symmetricTridiagonalFromCSR)
import Moonlight.LinAlg.Pure.Structured.Tridiagonal (symmetricTridiagonalDimension)
import Moonlight.LinAlg.Spectral
  ( Eigenpairs,
    EigenRequest (..),
    EigenSolveConfig,
    defaultEigenSolveConfig,
    eigenpairCount,
    solveEigenRequest,
    withEigenFallbackLanczosConfig,
    withEigenFallbackInitialVector,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)
import Prelude

data SpectralDispatchCase = SpectralDispatchCase
  { spectralCaseLabel :: !String,
    spectralCaseDimension :: !Int,
    spectralCaseRequestedModes :: !Int,
    spectralCaseKind :: !SpectralDispatchKind
  }

data SpectralDispatchKind
  = PathDispatch
  | DiagonalDispatch
  | GenericTridiagonalDispatch
  | ReducibleTridiagonalDispatch
  | GenericCSRDispatch
  deriving stock (Eq, Show)

data SpectralPreparedCase = SpectralPreparedCase
  { spectralPreparedLabel :: !String,
    spectralPreparedRequestedModes :: !Int,
    spectralPreparedOperator :: !(LinearOperator 'SelfAdjointOperator),
    spectralPreparedConfig :: !EigenSolveConfig
  }

data SpectralResidualPreparedCase = SpectralResidualPreparedCase
  { spectralResidualPreparedLabel :: !String,
    spectralResidualPreparedOperator :: !(LinearOperator 'SelfAdjointOperator),
    spectralResidualPreparedPairs :: !Eigenpairs
  }

instance NFData SpectralPreparedCase where
  rnf preparedCase =
    spectralPreparedLabel preparedCase
      `seq` spectralPreparedRequestedModes preparedCase
      `seq` spectralPreparedOperator preparedCase
      `seq` spectralPreparedConfig preparedCase
      `seq` ()

instance NFData SpectralResidualPreparedCase where
  rnf preparedCase =
    spectralResidualPreparedLabel preparedCase
      `seq` spectralResidualPreparedOperator preparedCase
      `seq` spectralResidualPreparedPairs preparedCase
      `seq` ()

spectralDispatchBenchmarks :: BenchmarkSelection -> Benchmark
spectralDispatchBenchmarks benchmarkSelection =
  bgroup
    "spectral demand dispatch"
    (spectralDispatchBenchmark <$> spectralDispatchCases benchmarkSelection)

spectralDispatchOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
spectralDispatchOnceBenchmarks benchmarkSelection =
  spectralDispatchOnceBenchmark =<< spectralDispatchCases benchmarkSelection

spectralDispatchCases :: BenchmarkSelection -> [SpectralDispatchCase]
spectralDispatchCases benchmarkSelection =
  [ SpectralDispatchCase "path-values-pairs-1024" 1024 4 PathDispatch,
    SpectralDispatchCase "diagonal-values-pairs-4096" 4096 4 DiagonalDispatch,
    SpectralDispatchCase "generic-tridiagonal-values-pairs-512" 512 4 GenericTridiagonalDispatch,
    SpectralDispatchCase "reducible-tridiagonal-values-pairs-512" 512 4 ReducibleTridiagonalDispatch,
    SpectralDispatchCase "generic-csr-fallback-values-pairs-96" 96 4 GenericCSRDispatch
  ]
    <> [SpectralDispatchCase "generic-csr-fallback-values-pairs-192" 192 6 GenericCSRDispatch | includeBroadMedium benchmarkSelection || includeBroadLarge benchmarkSelection]
    <> ( if includeBroadLarge benchmarkSelection
           then
             [ SpectralDispatchCase "generic-csr-dense-fallback-values-pairs-384" 384 8 GenericCSRDispatch,
               SpectralDispatchCase "generic-csr-dense-fallback-values-pairs-512" 512 8 GenericCSRDispatch,
               SpectralDispatchCase "generic-csr-high-demand-dense-values-pairs-513" 513 513 GenericCSRDispatch,
               SpectralDispatchCase "generic-csr-high-demand-dense-values-pairs-1024" 1024 1024 GenericCSRDispatch
             ]
           else []
       )

spectralDispatchBenchmark :: SpectralDispatchCase -> Benchmark
spectralDispatchBenchmark benchmarkCase =
  env (prepareBenchmarkSetup (prepareSpectralDispatchCase benchmarkCase)) $ \preparedCase ->
    bgroup
      (spectralCaseLabel benchmarkCase)
      [ bench "construction/classification" (nf spectralConstructionClassificationWeight benchmarkCase),
        bench "values" (nf spectralValuesWeight preparedCase),
        bench "pairs" (nf spectralPairsWeight preparedCase),
        env (prepareBenchmarkSetup (prepareSpectralResidualCase benchmarkCase)) $
          \residualCase ->
            bench "residual validation" (nf spectralResidualValidationWeight residualCase)
      ]

spectralDispatchOnceBenchmark :: SpectralDispatchCase -> [OnceBenchmark]
spectralDispatchOnceBenchmark benchmarkCase =
  [ spectralDispatchOnceBenchmarkRow benchmarkCase "values" spectralValuesWeight,
    spectralDispatchOnceBenchmarkRow benchmarkCase "pairs" spectralPairsWeight
  ]

spectralDispatchOnceBenchmarkRow :: SpectralDispatchCase -> String -> (SpectralPreparedCase -> BenchmarkWeight) -> OnceBenchmark
spectralDispatchOnceBenchmarkRow benchmarkCase rowLabel measure =
  OnceBenchmark
    { onceBenchmarkLabel = "spectral demand dispatch." <> spectralCaseLabel benchmarkCase <> "." <> rowLabel,
      onceBenchmarkAction =
        pure
          (runBenchmarkSetup (prepareSpectralDispatchCase benchmarkCase) >>= benchmarkWeightEither . measure)
    }

prepareSpectralDispatchCase :: SpectralDispatchCase -> BenchmarkSetup SpectralPreparedCase
prepareSpectralDispatchCase benchmarkCase =
  BenchmarkSetup $ do
    operatorValue <- spectralOperator benchmarkCase
    pure
      SpectralPreparedCase
        { spectralPreparedLabel = spectralCaseLabel benchmarkCase,
          spectralPreparedRequestedModes = spectralCaseRequestedModes benchmarkCase,
          spectralPreparedOperator = operatorValue,
          spectralPreparedConfig = spectralConfig benchmarkCase
        }

spectralOperator :: SpectralDispatchCase -> Either String (LinearOperator 'SelfAdjointOperator)
spectralOperator benchmarkCase =
  case spectralCaseKind benchmarkCase of
    PathDispatch ->
      first show (pathLaplacianLinearOperator (spectralCaseDimension benchmarkCase))
    DiagonalDispatch ->
      first show (diagonalLinearOperator (U.fromList (diagonalBenchmarkValues (spectralCaseDimension benchmarkCase))))
    GenericTridiagonalDispatch ->
      symmetricTridiagonalLinearOperator <$> genericBenchmarkTridiagonal (spectralCaseDimension benchmarkCase)
    ReducibleTridiagonalDispatch ->
      symmetricTridiagonalLinearOperator <$> reducibleBenchmarkTridiagonal (spectralCaseDimension benchmarkCase)
    GenericCSRDispatch ->
      bandedSpdCSR (spectralCaseDimension benchmarkCase) >>= first show . selfAdjointCSRLinearOperator

spectralConfig :: SpectralDispatchCase -> EigenSolveConfig
spectralConfig benchmarkCase =
  let fallbackIterations = max 8 (min (spectralCaseDimension benchmarkCase) 32)
   in case mkPositiveCount fallbackIterations of
        Left _ -> defaultEigenSolveConfig
        Right iterationCount ->
          withEigenFallbackInitialVector (seedVector (spectralCaseDimension benchmarkCase))
            ( withEigenFallbackLanczosConfig
                (withLanczosIterations iterationCount defaultLanczosConfig)
                defaultEigenSolveConfig
            )

spectralValuesWeight :: SpectralPreparedCase -> BenchmarkWeight
spectralValuesWeight preparedCase =
  eitherBenchmarkWeight
    (spectralPreparedLabel preparedCase <> " values")
    U.sum
    ( do
        requestedCount <- first show (mkPositiveCount (spectralPreparedRequestedModes preparedCase))
        first
          show
          ( solveEigenRequest
              (spectralPreparedConfig preparedCase)
              (spectralPreparedOperator preparedCase)
              (EigenvaluesRequest SmallestEigenvalues requestedCount)
          )
    )

spectralPairsWeight :: SpectralPreparedCase -> BenchmarkWeight
spectralPairsWeight preparedCase =
  eitherBenchmarkWeight
    (spectralPreparedLabel preparedCase <> " pairs")
    eigenpairsChecksum
    (spectralPairsResult preparedCase)

spectralPairsResult :: SpectralPreparedCase -> Either String Eigenpairs
spectralPairsResult preparedCase = do
  requestedCount <- first show (mkPositiveCount (spectralPreparedRequestedModes preparedCase))
  first
    show
    ( solveEigenRequest
        (spectralPreparedConfig preparedCase)
        (spectralPreparedOperator preparedCase)
        (EigenpairsRequest SmallestEigenvalues requestedCount)
    )

prepareSpectralResidualCase :: SpectralDispatchCase -> BenchmarkSetup SpectralResidualPreparedCase
prepareSpectralResidualCase benchmarkCase =
  BenchmarkSetup $ do
    preparedCase <- runBenchmarkSetup (prepareSpectralDispatchCase benchmarkCase)
    pairs <- spectralPairsResult preparedCase
    eigenpairsChecksum pairs `seq`
      pure
        SpectralResidualPreparedCase
          { spectralResidualPreparedLabel = spectralPreparedLabel preparedCase,
            spectralResidualPreparedOperator = spectralPreparedOperator preparedCase,
            spectralResidualPreparedPairs = pairs
          }

spectralConstructionClassificationWeight :: SpectralDispatchCase -> BenchmarkWeight
spectralConstructionClassificationWeight benchmarkCase =
  eitherBenchmarkWeight
    (spectralCaseLabel benchmarkCase <> " construction/classification")
    id
    ( do
        operatorValue <- spectralOperator benchmarkCase
        classificationChecksum <- spectralClassificationChecksum benchmarkCase
        pure (fromIntegral (operatorDimension operatorValue) + classificationChecksum)
    )

spectralClassificationChecksum :: SpectralDispatchCase -> Either String Double
spectralClassificationChecksum benchmarkCase =
  case spectralCaseKind benchmarkCase of
    GenericCSRDispatch -> do
      csrValue <- bandedSpdCSR (spectralCaseDimension benchmarkCase)
      case symmetricTridiagonalFromCSR csrValue of
        Left err -> Left (show err)
        Right (Left _) -> Right 0.0
        Right (Right tridiagonalValue) -> Right (fromIntegral (symmetricTridiagonalDimension tridiagonalValue))
    _ -> Right 0.0

spectralResidualValidationWeight :: SpectralResidualPreparedCase -> BenchmarkWeight
spectralResidualValidationWeight residualCase =
  eitherBenchmarkWeight
    (spectralResidualPreparedLabel residualCase <> " residual validation")
    id
    ( ( + fromIntegral (eigenpairCount (spectralResidualPreparedPairs residualCase))
      )
        <$> eigenpairsResidualValidationChecksum
          (runOperatorU (spectralResidualPreparedOperator residualCase))
          (spectralResidualPreparedPairs residualCase)
    )

seedVector :: Int -> U.Vector Double
seedVector dimension =
  U.generate dimension (\indexValue -> if indexValue == 0 then 1.0 else 1.0 / fromIntegral (indexValue + 1))

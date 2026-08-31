module SparseKrylov
  ( sparseKrylovBenchmarks,
    sparseKrylovOnceBenchmarks,
  )
where

import Data.Bifunctor (first)
import qualified Data.Vector.Unboxed as U
import Fixtures
  ( pathLaplacianTridiagonal,
    sparseKrylovBenchmarkCases,
  )
import Env (BenchmarkSelection)
import Types
  ( BenchmarkSetup (..),
    OnceBenchmark (..),
    SparseKrylovBenchmarkCase (..),
    SparseKrylovPreparedCase (..),
    BenchmarkWeight (..),
    prepareBenchmarkSetup,
    benchmarkWeightEither,
  )
import Moonlight.LinAlg.Krylov
  ( mkPositiveCount,
    SpectrumEnd (..),
  )
import Moonlight.LinAlg.Operator (symmetricTridiagonalLinearOperator)
import Moonlight.LinAlg.Spectral
  ( defaultEigenSolveConfig,
    EigenRequest (..),
    solveEigenRequest,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)
import Prelude

sparseKrylovBenchmarks :: BenchmarkSelection -> Benchmark
sparseKrylovBenchmarks benchmarkSelection =
  bgroup
    "selected tridiagonal eigenvalue solve"
    (sparseKrylovBenchmark <$> sparseKrylovBenchmarkCases benchmarkSelection)

sparseKrylovOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
sparseKrylovOnceBenchmarks benchmarkSelection =
  sparseKrylovOnceBenchmark <$> sparseKrylovBenchmarkCases benchmarkSelection

sparseKrylovBenchmark :: SparseKrylovBenchmarkCase -> Benchmark
sparseKrylovBenchmark benchmarkCase =
  env (prepareBenchmarkSetup (prepareSparseKrylovCase benchmarkCase)) $ \preparedCase ->
    bench (sparseBenchmarkLabel benchmarkCase) (nf sparseKrylovWeight preparedCase)

sparseKrylovOnceBenchmark :: SparseKrylovBenchmarkCase -> OnceBenchmark
sparseKrylovOnceBenchmark benchmarkCase =
  OnceBenchmark
    { onceBenchmarkLabel = "selected tridiagonal eigenvalue solve." <> sparseBenchmarkLabel benchmarkCase,
      onceBenchmarkAction =
        pure
          (runBenchmarkSetup (prepareSparseKrylovCase benchmarkCase) >>= benchmarkWeightEither . sparseKrylovWeight)
    }

prepareSparseKrylovCase :: SparseKrylovBenchmarkCase -> BenchmarkSetup SparseKrylovPreparedCase
prepareSparseKrylovCase benchmarkCase =
  BenchmarkSetup $ do
    tridiagonalOperator <-
      first
        (\err -> "benchmark tridiagonal fixture failed for " <> sparseBenchmarkLabel benchmarkCase <> ": " <> err)
        (pathLaplacianTridiagonal (sparseBenchmarkDimension benchmarkCase))
    pure
      SparseKrylovPreparedCase
        { sparsePreparedLabel = sparseBenchmarkLabel benchmarkCase,
          sparsePreparedRequestedModes = sparseBenchmarkRequestedModes benchmarkCase,
          sparsePreparedTridiagonal = tridiagonalOperator
        }

sparseKrylovWeight :: SparseKrylovPreparedCase -> BenchmarkWeight
sparseKrylovWeight preparedCase =
  case
    do
      requestedCount <- first show (mkPositiveCount (sparsePreparedRequestedModes preparedCase))
      first
        show
        ( solveEigenRequest
            defaultEigenSolveConfig
            (symmetricTridiagonalLinearOperator (sparsePreparedTridiagonal preparedCase))
            (EigenvaluesRequest SmallestEigenvalues requestedCount)
        ) of
    Left err -> BenchmarkMeasurementFailure (sparsePreparedLabel preparedCase <> ": " <> err)
    Right eigenvalues -> BenchmarkWeight (sum (U.toList eigenvalues))

module SparseSolvers
  ( sparseSolverBenchmarks,
    sparseSolverOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import qualified Data.Vector.Unboxed as U
import Env (BenchmarkSelection (..))
import Fixtures
  ( bandedSpdCSR,
    denseBenchmarkVector,
    diagonalBenchmarkValues,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight,
    OnceBenchmark,
    PreparedBenchmarkRow (..),
    eitherBenchmarkWeight,
    renderPreparedBenchmark,
    renderPreparedOnceBenchmark,
  )
import Moonlight.LinAlg.Sparse
  ( IC0Config (..),
    SparseCSR,
    SparseConjugateGradientConfig (..),
    SparseGMRESConfig (..),
    SparseIterativeResult (..),
    SparsePreconditionerFamily (..),
    SparseStationaryIterationConfig (..),
    csrColumnIndicesVector,
    csrRowOffsetsVector,
    csrValuesVector,
    diagonalCSR,
    solveSparseCG,
    solveSparseGMRES,
    solveSparseJacobi,
    solveSparseRichardson,
  )
import Test.Tasty.Bench (Benchmark, bgroup)
import Prelude

data SparseSolverCase = SparseSolverCase
  { sparseSolverLabel :: !String,
    sparseSolverMatrix :: !(SparseCSR Double),
    sparseSolverDiagonalMatrix :: !(SparseCSR Double),
    sparseSolverRhs :: !(U.Vector Double),
    sparseSolverInitialGuess :: !(U.Vector Double),
    sparseSolverCGConfig :: !SparseConjugateGradientConfig,
    sparseSolverGMRESConfig :: !SparseGMRESConfig,
    sparseSolverStationaryConfig :: !SparseStationaryIterationConfig
  }

instance NFData SparseSolverCase where
  rnf benchmarkCase =
    rnf (sparseSolverLabel benchmarkCase)
      `seq` rnf (csrRowOffsetsVector (sparseSolverMatrix benchmarkCase))
      `seq` rnf (csrColumnIndicesVector (sparseSolverMatrix benchmarkCase))
      `seq` rnf (csrValuesVector (sparseSolverMatrix benchmarkCase))
      `seq` rnf (csrRowOffsetsVector (sparseSolverDiagonalMatrix benchmarkCase))
      `seq` rnf (csrColumnIndicesVector (sparseSolverDiagonalMatrix benchmarkCase))
      `seq` rnf (csrValuesVector (sparseSolverDiagonalMatrix benchmarkCase))
      `seq` rnf (sparseSolverRhs benchmarkCase)
      `seq` rnf (sparseSolverInitialGuess benchmarkCase)
      `seq` sparseSolverCGConfig benchmarkCase
      `seq` sparseSolverGMRESConfig benchmarkCase
      `seq` sparseSolverStationaryConfig benchmarkCase
      `seq` ()

sparseSolverBenchmarks :: BenchmarkSelection -> Benchmark
sparseSolverBenchmarks benchmarkSelection =
  bgroup
    "sparse iterative solvers"
    (sparseSolverBenchmark <$> sparseSolverDimensions benchmarkSelection)

sparseSolverOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
sparseSolverOnceBenchmarks benchmarkSelection =
  sparseSolverOnceBenchmark =<< sparseSolverDimensions benchmarkSelection

sparseSolverDimensions :: BenchmarkSelection -> [Int]
sparseSolverDimensions benchmarkSelection =
  [64]
    <> [128 | includeBroadMedium benchmarkSelection || includeBroadLarge benchmarkSelection]
    <> [256 | includeBroadLarge benchmarkSelection]

sparseSolverBenchmark :: Int -> Benchmark
sparseSolverBenchmark dimension =
  bgroup
    ("n=" <> show dimension)
    (renderPreparedBenchmark (prepareSparseSolverCase dimension) <$> sparseSolverRows)

sparseSolverOnceBenchmark :: Int -> [OnceBenchmark]
sparseSolverOnceBenchmark dimension =
  renderPreparedOnceBenchmark ("sparse iterative solvers.n=" <> show dimension <> ".") (prepareSparseSolverCase dimension)
    <$> sparseSolverRows

sparseSolverRows :: [PreparedBenchmarkRow SparseSolverCase]
sparseSolverRows =
  [ PurePreparedBenchmarkRow "CG" sparseCgWeight,
    PurePreparedBenchmarkRow "PCG diagonal" sparsePcgDiagonalWeight,
    PurePreparedBenchmarkRow "PCG SSOR" sparsePcgSsorWeight,
    PurePreparedBenchmarkRow "PCG IC0" sparsePcgIC0Weight,
    PurePreparedBenchmarkRow "GMRES" sparseGmresWeight,
    PurePreparedBenchmarkRow "Jacobi diagonal" sparseJacobiWeight,
    PurePreparedBenchmarkRow "Richardson diagonal" sparseRichardsonWeight
  ]

prepareSparseSolverCase :: Int -> BenchmarkSetup SparseSolverCase
prepareSparseSolverCase dimension =
  BenchmarkSetup $ do
    sparseMatrix <- first (("sparse solver matrix fixture failed: " <>)) (bandedSpdCSR dimension)
    diagonalMatrix <- first (("sparse solver diagonal fixture failed: " <>) . show) (diagonalCSR (diagonalBenchmarkValues dimension))
    let rhsValues = U.fromList (denseBenchmarkVector dimension)
    pure
      SparseSolverCase
        { sparseSolverLabel = "n=" <> show dimension,
          sparseSolverMatrix = sparseMatrix,
          sparseSolverDiagonalMatrix = diagonalMatrix,
          sparseSolverRhs = rhsValues,
          sparseSolverInitialGuess = U.replicate dimension 0.0,
          sparseSolverCGConfig =
            SparseConjugateGradientConfig
              { scgcTolerance = 1.0e-8,
                scgcIterationLimit = max 64 (dimension * 4),
                scgcPreconditionerFamily = IdentitySparsePreconditionerFamily
              },
          sparseSolverGMRESConfig =
            SparseGMRESConfig
              { sgcTolerance = 1.0e-8,
                sgcIterationLimit = max 64 (dimension * 2),
                sgcRestartDimension = min 24 (max 4 dimension),
                sgcPreconditionerFamily = IdentitySparsePreconditionerFamily
              },
          sparseSolverStationaryConfig =
            SparseStationaryIterationConfig
              { ssicTolerance = 1.0e-8,
                ssicIterationLimit = max 64 (dimension * 4),
                ssicDamping = 0.9
              }
        }

sparseCgWeight :: SparseSolverCase -> BenchmarkWeight
sparseCgWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseSolverLabel benchmarkCase <> " CG")
    sparseResultChecksum
    ( solveSparseCG
        (sparseSolverCGConfig benchmarkCase)
        (sparseSolverMatrix benchmarkCase)
        (sparseSolverRhs benchmarkCase)
        (sparseSolverInitialGuess benchmarkCase)
    )

sparsePcgDiagonalWeight :: SparseSolverCase -> BenchmarkWeight
sparsePcgDiagonalWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseSolverLabel benchmarkCase <> " PCG diagonal")
    sparseResultChecksum
    ( solveSparseCG
        ((sparseSolverCGConfig benchmarkCase) {scgcPreconditionerFamily = DiagonalJacobiSparsePreconditionerFamily})
        (sparseSolverMatrix benchmarkCase)
        (sparseSolverRhs benchmarkCase)
        (sparseSolverInitialGuess benchmarkCase)
    )

sparsePcgSsorWeight :: SparseSolverCase -> BenchmarkWeight
sparsePcgSsorWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseSolverLabel benchmarkCase <> " PCG SSOR")
    sparseResultChecksum
    ( solveSparseCG
        ((sparseSolverCGConfig benchmarkCase) {scgcPreconditionerFamily = SsorSparsePreconditionerFamily 1.0})
        (sparseSolverMatrix benchmarkCase)
        (sparseSolverRhs benchmarkCase)
        (sparseSolverInitialGuess benchmarkCase)
    )

sparsePcgIC0Weight :: SparseSolverCase -> BenchmarkWeight
sparsePcgIC0Weight benchmarkCase =
  eitherBenchmarkWeight
    (sparseSolverLabel benchmarkCase <> " PCG IC0")
    sparseResultChecksum
    ( solveSparseCG
        ((sparseSolverCGConfig benchmarkCase) {scgcPreconditionerFamily = IncompleteCholesky0SparsePreconditionerFamily (IC0Config Nothing)})
        (sparseSolverMatrix benchmarkCase)
        (sparseSolverRhs benchmarkCase)
        (sparseSolverInitialGuess benchmarkCase)
    )

sparseGmresWeight :: SparseSolverCase -> BenchmarkWeight
sparseGmresWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseSolverLabel benchmarkCase <> " GMRES")
    sparseResultChecksum
    ( solveSparseGMRES
        (sparseSolverGMRESConfig benchmarkCase)
        (sparseSolverMatrix benchmarkCase)
        (sparseSolverRhs benchmarkCase)
        (sparseSolverInitialGuess benchmarkCase)
    )

sparseJacobiWeight :: SparseSolverCase -> BenchmarkWeight
sparseJacobiWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseSolverLabel benchmarkCase <> " Jacobi diagonal")
    sparseResultChecksum
    ( solveSparseJacobi
        (sparseSolverStationaryConfig benchmarkCase)
        (sparseSolverDiagonalMatrix benchmarkCase)
        (sparseSolverRhs benchmarkCase)
        (sparseSolverInitialGuess benchmarkCase)
    )

sparseRichardsonWeight :: SparseSolverCase -> BenchmarkWeight
sparseRichardsonWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseSolverLabel benchmarkCase <> " Richardson diagonal")
    sparseResultChecksum
    ( solveSparseRichardson
        (sparseSolverStationaryConfig benchmarkCase)
        (sparseSolverDiagonalMatrix benchmarkCase)
        (sparseSolverRhs benchmarkCase)
        (sparseSolverInitialGuess benchmarkCase)
    )

sparseResultChecksum :: SparseIterativeResult -> Double
sparseResultChecksum resultValue =
  fromIntegral (sparseIterations resultValue)
    + sparseResidualNorm resultValue
    + U.sum (U.map abs (sparseSolution resultValue))

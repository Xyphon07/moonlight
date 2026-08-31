{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module DenseDecomposition
  ( denseDecompositionBenchmarks,
    denseDecompositionOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Vector.Storable qualified as S
import Fixtures
  ( denseBenchmarkRows,
    denseBenchmarkVector,
    denseSpdRows,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight,
    OnceBenchmark,
    PreparedBenchmarkRow (..),
    eigenpairsChecksum,
    eitherBenchmarkWeight,
    renderPreparedBenchmark,
    renderPreparedOnceBenchmark,
  )
import Moonlight.LinAlg.Dense
  ( DenseDoubleMatrix,
    Matrix,
    Vector,
    denseDoubleMatrixToRowMajorVector,
    fromListMatrix,
    fromListVector,
    mkDenseDoubleMatrixRowMajor,
    toListMatrix,
    toListVector,
  )
import Moonlight.LinAlg.Dense.Decomposition
  ( choleskyDecomp,
    qrDecompFullColumnRank,
    symmetricEigen,
    thinSvdFullColumnRank,
  )
import Moonlight.LinAlg.Dense.Solver
  ( solveCG,
    solveDirect,
    solveGMRES,
  )
import Moonlight.LinAlg.Native
  ( denseDoubleLinearSolveLapack,
    denseDoubleSymmetricEigenpairsLapack,
  )
import Test.Tasty.Bench (Benchmark, bgroup)
import Prelude

data DenseDecompositionCase = DenseDecompositionCase
  { denseDecompositionGeneral :: !(Matrix 12 12 Double),
    denseDecompositionSpd :: !(Matrix 12 12 Double),
    denseDecompositionRhs :: !(Vector 12 Double),
    denseDecompositionFlatGeneral :: !DenseDoubleMatrix,
    denseDecompositionFlatSpd :: !DenseDoubleMatrix,
    denseDecompositionFlatRhs :: !(S.Vector Double)
  }

instance NFData DenseDecompositionCase where
  rnf benchmarkCase =
    rnf (toListMatrix (denseDecompositionGeneral benchmarkCase))
      `seq` rnf (toListMatrix (denseDecompositionSpd benchmarkCase))
      `seq` rnf (toListVector (denseDecompositionRhs benchmarkCase))
      `seq` rnf (denseDoubleMatrixToRowMajorVector (denseDecompositionFlatGeneral benchmarkCase))
      `seq` rnf (denseDoubleMatrixToRowMajorVector (denseDecompositionFlatSpd benchmarkCase))
      `seq` rnf (denseDecompositionFlatRhs benchmarkCase)

denseDecompositionBenchmarks :: Benchmark
denseDecompositionBenchmarks =
  bgroup
    "dense decomposition and solvers"
    (renderPreparedBenchmark prepareDenseDecompositionCase <$> denseDecompositionRows)

denseDecompositionOnceBenchmarks :: [OnceBenchmark]
denseDecompositionOnceBenchmarks =
  renderPreparedOnceBenchmark "dense decomposition and solvers." prepareDenseDecompositionCase <$> denseDecompositionRows

denseDecompositionRows :: [PreparedBenchmarkRow DenseDecompositionCase]
denseDecompositionRows =
  [ PurePreparedBenchmarkRow "qr 12x12" qrWeight,
    PurePreparedBenchmarkRow "cholesky 12x12" choleskyWeight,
    PurePreparedBenchmarkRow "symmetric eigen pure 12x12" symmetricEigenWeight,
    EffectfulPreparedBenchmarkRow "native LAPACK symmetric eigen certified 12x12" nativeSymmetricEigenWeight,
    PurePreparedBenchmarkRow "svd 12x12" svdWeight,
    PurePreparedBenchmarkRow "direct solve 12x12" directSolveWeight,
    EffectfulPreparedBenchmarkRow "native LAPACK direct solve 12x12" nativeDirectSolveWeight,
    PurePreparedBenchmarkRow "dense CG 12x12" denseCgWeight,
    PurePreparedBenchmarkRow "dense GMRES 12x12" denseGmresWeight
  ]

prepareDenseDecompositionCase :: BenchmarkSetup DenseDecompositionCase
prepareDenseDecompositionCase =
  BenchmarkSetup $ do
    let generalRows = denseBenchmarkRows 12
        spdRows = denseSpdRows 12
        rhsValues = denseBenchmarkVector 12
    generalMatrix <- first show (fromListMatrix @12 @12 @Double (concat generalRows))
    spdMatrix <- first show (fromListMatrix @12 @12 @Double (concat spdRows))
    rhsVector <- first show (fromListVector @12 @Double rhsValues)
    flatGeneral <- first show (mkDenseDoubleMatrixRowMajor 12 12 (S.fromList (concat generalRows)))
    flatSpd <- first show (mkDenseDoubleMatrixRowMajor 12 12 (S.fromList (concat spdRows)))
    pure
      DenseDecompositionCase
        { denseDecompositionGeneral = generalMatrix,
          denseDecompositionSpd = spdMatrix,
          denseDecompositionRhs = rhsVector,
          denseDecompositionFlatGeneral = flatGeneral,
          denseDecompositionFlatSpd = flatSpd,
          denseDecompositionFlatRhs = S.fromList rhsValues
        }

qrWeight :: DenseDecompositionCase -> BenchmarkWeight
qrWeight benchmarkCase =
  eitherBenchmarkWeight
    "qr 12x12"
    (\(qMatrix, rMatrix) -> matrixChecksum (toListMatrix qMatrix) + matrixChecksum (toListMatrix rMatrix))
    (qrDecompFullColumnRank (denseDecompositionGeneral benchmarkCase))

choleskyWeight :: DenseDecompositionCase -> BenchmarkWeight
choleskyWeight benchmarkCase =
  eitherBenchmarkWeight
    "cholesky 12x12"
    (matrixChecksum . toListMatrix)
    (choleskyDecomp (denseDecompositionSpd benchmarkCase))

symmetricEigenWeight :: DenseDecompositionCase -> BenchmarkWeight
symmetricEigenWeight benchmarkCase =
  eitherBenchmarkWeight
    "symmetric eigen pure 12x12"
    (\(values, vectors) -> vectorChecksum (toListVector values) + matrixChecksum (toListMatrix vectors))
    (symmetricEigen (denseDecompositionSpd benchmarkCase))

nativeSymmetricEigenWeight :: DenseDecompositionCase -> IO BenchmarkWeight
nativeSymmetricEigenWeight benchmarkCase =
  eitherBenchmarkWeight
    "native LAPACK symmetric eigen certified 12x12"
    eigenpairsChecksum
    <$> denseDoubleSymmetricEigenpairsLapack (denseDecompositionFlatSpd benchmarkCase)

svdWeight :: DenseDecompositionCase -> BenchmarkWeight
svdWeight benchmarkCase =
  eitherBenchmarkWeight
    "svd 12x12"
    (\(uMatrix, sMatrix, vtMatrix) -> matrixChecksum (toListMatrix uMatrix) + matrixChecksum (toListMatrix sMatrix) + matrixChecksum (toListMatrix vtMatrix))
    (thinSvdFullColumnRank (denseDecompositionGeneral benchmarkCase))

directSolveWeight :: DenseDecompositionCase -> BenchmarkWeight
directSolveWeight benchmarkCase =
  eitherBenchmarkWeight
    "direct solve 12x12"
    (vectorChecksum . toListVector)
    (solveDirect (denseDecompositionGeneral benchmarkCase) (denseDecompositionRhs benchmarkCase))

nativeDirectSolveWeight :: DenseDecompositionCase -> IO BenchmarkWeight
nativeDirectSolveWeight benchmarkCase =
  eitherBenchmarkWeight
    "native LAPACK direct solve 12x12"
    storableVectorChecksum
    <$> denseDoubleLinearSolveLapack
      (denseDecompositionFlatGeneral benchmarkCase)
      (denseDecompositionFlatRhs benchmarkCase)

denseCgWeight :: DenseDecompositionCase -> BenchmarkWeight
denseCgWeight benchmarkCase =
  eitherBenchmarkWeight
    "dense CG 12x12"
    (vectorChecksum . toListVector)
    (solveCG (denseDecompositionSpd benchmarkCase) (denseDecompositionRhs benchmarkCase))

denseGmresWeight :: DenseDecompositionCase -> BenchmarkWeight
denseGmresWeight benchmarkCase =
  eitherBenchmarkWeight
    "dense GMRES 12x12"
    (vectorChecksum . toListVector)
    (solveGMRES (denseDecompositionGeneral benchmarkCase) (denseDecompositionRhs benchmarkCase))

vectorChecksum :: [Double] -> Double
vectorChecksum values =
  sum (abs <$> values)

storableVectorChecksum :: S.Vector Double -> Double
storableVectorChecksum values =
  S.foldl' (\accumulator value -> accumulator + abs value) 0.0 values

matrixChecksum :: [Double] -> Double
matrixChecksum values =
  sum (abs <$> values)

module DenseCore
  ( denseCoreBenchmarks,
    denseCoreOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Vector.Storable qualified as S
import Env (BenchmarkSelection (..))
import Fixtures
  ( denseBenchmarkRows,
    denseBenchmarkVector,
  )
import Types
  ( BenchmarkWeight (..),
    OnceBenchmark (..),
    benchmarkWeightEither,
    eitherBenchmarkWeight,
  )
import Moonlight.LinAlg.Dense
  ( DenseDoubleMatrix,
    denseDoubleMatrixToRowMajorVector,
    denseDoubleMatrixVectorProduct,
    mkDenseDoubleMatrixRowMajor,
  )
import Moonlight.LinAlg.Dense.Primitives
  ( matrixVectorProduct,
  )
import Moonlight.LinAlg.Dense.Rows
  ( hcatRowsExact,
    matrixProductRowsWith,
    transposeRowsExact,
    vcatRowsExact,
  )
import Moonlight.LinAlg.Native
  ( denseDoubleMatrixProductBlas,
  )
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf, nfIO)
import Prelude

data DenseCoreCase = DenseCoreCase
  { denseCoreLabel :: !String,
    denseCoreRows :: ![[Double]],
    denseCoreVector :: ![Double],
    denseCoreFlatVector :: !(S.Vector Double),
    denseCoreFlatMatrix :: !(Either String DenseDoubleMatrix)
  }

instance NFData DenseCoreCase where
  rnf benchmarkCase =
    rnf (denseCoreLabel benchmarkCase)
      `seq` rnf (denseCoreRows benchmarkCase)
      `seq` rnf (denseCoreVector benchmarkCase)
      `seq` rnf (denseCoreFlatVector benchmarkCase)
      `seq` forceFlatMatrix (denseCoreFlatMatrix benchmarkCase)
      `seq` ()

denseCoreBenchmarks :: BenchmarkSelection -> Benchmark
denseCoreBenchmarks benchmarkSelection =
  bgroup
    "dense row validation surface"
    (denseCoreCaseBenchmarks =<< denseCoreCases benchmarkSelection)

denseCoreOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
denseCoreOnceBenchmarks benchmarkSelection =
  denseCoreCaseOnceBenchmarks =<< denseCoreCases benchmarkSelection

denseCoreCases :: BenchmarkSelection -> [DenseCoreCase]
denseCoreCases benchmarkSelection =
  fmap
    denseCoreCase
    ( [32]
        <> [96 | includeBroadMedium benchmarkSelection || includeBroadLarge benchmarkSelection]
        <> [192 | includeBroadLarge benchmarkSelection]
    )

denseCoreCase :: Int -> DenseCoreCase
denseCoreCase dimension =
  let rowValues = denseBenchmarkRows dimension
      vectorValues = denseBenchmarkVector dimension
      flatPayload = S.fromList (concat rowValues)
   in DenseCoreCase
        { denseCoreLabel = "n=" <> show dimension,
          denseCoreRows = rowValues,
          denseCoreVector = vectorValues,
          denseCoreFlatVector = S.fromList vectorValues,
          denseCoreFlatMatrix =
            first show
              ( mkDenseDoubleMatrixRowMajor
                  dimension
                  dimension
                  flatPayload
              )
        }

denseCoreCaseBenchmarks :: DenseCoreCase -> [Benchmark]
denseCoreCaseBenchmarks benchmarkCase =
  [ bench (denseCoreLabel benchmarkCase <> " matrix-vector reference rows") (nf denseMatvecWeight benchmarkCase),
    bench (denseCoreLabel benchmarkCase <> " flat matrix-vector") (nf denseFlatMatvecWeight benchmarkCase),
    bench (denseCoreLabel benchmarkCase <> " matrix-product reference rows") (nf denseMatmulWeight benchmarkCase),
    bench (denseCoreLabel benchmarkCase <> " native BLAS matrix-product") (nfIO (denseNativeMatmulWeight benchmarkCase)),
    bench (denseCoreLabel benchmarkCase <> " transpose") (nf denseTransposeWeight benchmarkCase),
    bench (denseCoreLabel benchmarkCase <> " hcat/vcat") (nf denseConcatWeight benchmarkCase)
  ]

denseCoreCaseOnceBenchmarks :: DenseCoreCase -> [OnceBenchmark]
denseCoreCaseOnceBenchmarks benchmarkCase =
  [ denseCoreOnceBenchmark benchmarkCase "matrix-vector reference rows" denseMatvecWeight,
    denseCoreOnceBenchmark benchmarkCase "flat matrix-vector" denseFlatMatvecWeight,
    denseCoreOnceBenchmark benchmarkCase "matrix-product reference rows" denseMatmulWeight,
    denseCoreOnceBenchmarkIO benchmarkCase "native BLAS matrix-product" denseNativeMatmulWeight,
    denseCoreOnceBenchmark benchmarkCase "transpose" denseTransposeWeight,
    denseCoreOnceBenchmark benchmarkCase "hcat/vcat" denseConcatWeight
  ]

denseCoreOnceBenchmark :: DenseCoreCase -> String -> (DenseCoreCase -> BenchmarkWeight) -> OnceBenchmark
denseCoreOnceBenchmark benchmarkCase label measure =
  OnceBenchmark
    { onceBenchmarkLabel = "dense row validation surface." <> denseCoreLabel benchmarkCase <> " " <> label,
      onceBenchmarkAction = pure (benchmarkWeightEither (measure benchmarkCase))
    }

denseCoreOnceBenchmarkIO :: DenseCoreCase -> String -> (DenseCoreCase -> IO BenchmarkWeight) -> OnceBenchmark
denseCoreOnceBenchmarkIO benchmarkCase label measure =
  OnceBenchmark
    { onceBenchmarkLabel = "dense row validation surface." <> denseCoreLabel benchmarkCase <> " " <> label,
      onceBenchmarkAction = benchmarkWeightEither <$> measure benchmarkCase
    }

denseMatvecWeight :: DenseCoreCase -> BenchmarkWeight
denseMatvecWeight benchmarkCase =
  eitherBenchmarkWeight
    (denseCoreLabel benchmarkCase <> " matrix-vector reference rows")
    vectorChecksum
    (matrixVectorProduct (denseCoreRows benchmarkCase) (denseCoreVector benchmarkCase))

denseFlatMatvecWeight :: DenseCoreCase -> BenchmarkWeight
denseFlatMatvecWeight benchmarkCase =
  eitherBenchmarkWeight
    (denseCoreLabel benchmarkCase <> " flat matrix-vector")
    storableVectorChecksum
    ( do
        matrixValue <- denseCoreFlatMatrix benchmarkCase
        first show (denseDoubleMatrixVectorProduct matrixValue (denseCoreFlatVector benchmarkCase))
    )

forceFlatMatrix :: Either String DenseDoubleMatrix -> ()
forceFlatMatrix matrixResult =
  case matrixResult of
    Left failureText -> rnf failureText
    Right matrixValue -> rnf (denseDoubleMatrixToRowMajorVector matrixValue)

denseMatmulWeight :: DenseCoreCase -> BenchmarkWeight
denseMatmulWeight benchmarkCase =
  eitherBenchmarkWeight
    (denseCoreLabel benchmarkCase <> " matrix-product reference rows")
    matrixChecksum
    (matrixProductRowsWith (*) (+) 0.0 (denseCoreRows benchmarkCase) (denseCoreRows benchmarkCase))

denseNativeMatmulWeight :: DenseCoreCase -> IO BenchmarkWeight
denseNativeMatmulWeight benchmarkCase =
  case denseCoreFlatMatrix benchmarkCase of
    Left failureText ->
      pure (BenchmarkMeasurementFailure (denseCoreLabel benchmarkCase <> " native BLAS matrix-product: " <> failureText))
    Right matrixValue ->
      eitherBenchmarkWeight
        (denseCoreLabel benchmarkCase <> " native BLAS matrix-product")
        (storableVectorChecksum . denseDoubleMatrixToRowMajorVector)
        <$> denseDoubleMatrixProductBlas matrixValue matrixValue

denseTransposeWeight :: DenseCoreCase -> BenchmarkWeight
denseTransposeWeight benchmarkCase =
  eitherBenchmarkWeight
    (denseCoreLabel benchmarkCase <> " transpose")
    matrixChecksum
    (transposeRowsExact (denseCoreRows benchmarkCase))

denseConcatWeight :: DenseCoreCase -> BenchmarkWeight
denseConcatWeight benchmarkCase =
  eitherBenchmarkWeight
    (denseCoreLabel benchmarkCase <> " hcat/vcat")
    matrixChecksum
    ( do
        horizontal <- hcatRowsExact [denseCoreRows benchmarkCase, denseCoreRows benchmarkCase]
        vertical <- vcatRowsExact [denseCoreRows benchmarkCase, denseCoreRows benchmarkCase]
        pure (horizontal <> vertical)
    )

vectorChecksum :: [Double] -> Double
vectorChecksum values =
  sum (abs <$> values)

storableVectorChecksum :: S.Vector Double -> Double
storableVectorChecksum values =
  S.foldl' (\accumulator value -> accumulator + abs value) 0.0 values

matrixChecksum :: [[Double]] -> Double
matrixChecksum rows =
  sum (vectorChecksum <$> rows)

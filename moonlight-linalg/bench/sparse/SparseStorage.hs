{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module SparseStorage
  ( sparseStorageBenchmarks,
    sparseStorageOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.Vector.Unboxed qualified as Unboxed
import Env (BenchmarkSelection (..))
import Fixtures
  ( bandedDenseRows,
    bandedSpdCSR,
    denseBenchmarkVector,
    packedSparseBenchmarkOperator,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight (..),
    OnceBenchmark,
    PreparedBenchmarkRow (..),
    eitherBenchmarkWeight,
    renderPreparedBenchmark,
    renderPreparedOnceBenchmark,
  )
import Moonlight.LinAlg.Dense
  ( Matrix,
    fromListMatrix,
  )
import Moonlight.LinAlg.Sparse
  ( GraphEdge (..),
    PackedSparseOperator,
    SparseCOO,
    SparseCSC,
    SparseCSR,
    applyPackedSparseOperatorDense,
    cooEntries,
    csrColumnIndicesVector,
    csrMatVecVector,
    csrRowOffsetsVector,
    csrToCOO,
    csrToCSC,
    csrValuesVector,
    cscColumnOffsetsVector,
    cscRowIndicesVector,
    cscValuesVector,
    denseToCOO,
    denseToCSC,
    denseToCSR,
    graphLaplacianCSR,
  )
import Test.Tasty.Bench (Benchmark, bgroup)
import Prelude

data SparseStorageCase = SparseStorageCase
  { sparseStorageLabel :: !String,
    sparseStorageDense32 :: !(Matrix 32 32 Double),
    sparseStorageCSR :: !(SparseCSR Double),
    sparseStoragePackedOperator :: !(PackedSparseOperator Double),
    sparseStoragePackedVector :: !(Unboxed.Vector Double),
    sparseStorageGraphVertices :: ![Int],
    sparseStorageGraphEdges :: ![GraphEdge Int]
  }

instance NFData SparseStorageCase where
  rnf benchmarkCase =
    rnf (sparseStorageLabel benchmarkCase)
      `seq` sparseStorageDense32 benchmarkCase
      `seq` rnf (csrRowOffsetsVector (sparseStorageCSR benchmarkCase))
      `seq` rnf (csrColumnIndicesVector (sparseStorageCSR benchmarkCase))
      `seq` rnf (csrValuesVector (sparseStorageCSR benchmarkCase))
      `seq` sparseStoragePackedOperator benchmarkCase
      `seq` rnf (sparseStoragePackedVector benchmarkCase)
      `seq` rnf (sparseStorageGraphVertices benchmarkCase)
      `seq` rnf
        ( fmap
            ( \edgeValue ->
                ( graphEdgeLeft edgeValue,
                  graphEdgeRight edgeValue,
                  graphEdgeWeight edgeValue
                )
            )
            (sparseStorageGraphEdges benchmarkCase)
        )
      `seq` ()

sparseStorageBenchmarks :: BenchmarkSelection -> Benchmark
sparseStorageBenchmarks benchmarkSelection =
  bgroup
    "sparse storage and packed kernels"
    (sparseStorageBenchmark <$> sparseStorageDimensions benchmarkSelection)

sparseStorageOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
sparseStorageOnceBenchmarks benchmarkSelection =
  sparseStorageOnceBenchmark =<< sparseStorageDimensions benchmarkSelection

sparseStorageDimensions :: BenchmarkSelection -> [Int]
sparseStorageDimensions benchmarkSelection =
  [512]
    <> [2048 | includeBroadMedium benchmarkSelection || includeBroadLarge benchmarkSelection]
    <> [8192 | includeBroadLarge benchmarkSelection]

sparseStorageBenchmark :: Int -> Benchmark
sparseStorageBenchmark dimension =
  bgroup
    ("n=" <> show dimension)
    (renderPreparedBenchmark (prepareSparseStorageCase dimension) <$> sparseStorageRows)

sparseStorageOnceBenchmark :: Int -> [OnceBenchmark]
sparseStorageOnceBenchmark dimension =
  renderPreparedOnceBenchmark ("sparse storage and packed kernels.n=" <> show dimension <> ".") (prepareSparseStorageCase dimension)
    <$> sparseStorageRows

sparseStorageRows :: [PreparedBenchmarkRow SparseStorageCase]
sparseStorageRows =
  [ PurePreparedBenchmarkRow "dense 32x32 -> COO" denseToCooWeight,
    PurePreparedBenchmarkRow "dense 32x32 -> CSR" denseToCsrWeight,
    PurePreparedBenchmarkRow "dense 32x32 -> CSC" denseToCscWeight,
    PurePreparedBenchmarkRow "CSR -> COO" csrToCooWeight,
    PurePreparedBenchmarkRow "CSR -> CSC" csrToCscWeight,
    PurePreparedBenchmarkRow "CSR matvec" csrMatvecWeight,
    PurePreparedBenchmarkRow "packed sparse apply" packedSparseWeight,
    PurePreparedBenchmarkRow "graph Laplacian construction" graphLaplacianWeight
  ]

prepareSparseStorageCase :: Int -> BenchmarkSetup SparseStorageCase
prepareSparseStorageCase dimension =
  BenchmarkSetup $ do
    dense32 <- first show (fromListMatrix @32 @32 @Double (concat (bandedDenseRows 32)))
    csrValue <- first (("banded sparse fixture failed: " <>)) (bandedSpdCSR dimension)
    packedOperator <- first (("packed sparse fixture failed: " <>)) (packedSparseBenchmarkOperator dimension)
    let vectorValues = denseBenchmarkVector dimension
    pure
      SparseStorageCase
        { sparseStorageLabel = "n=" <> show dimension,
          sparseStorageDense32 = dense32,
          sparseStorageCSR = csrValue,
          sparseStoragePackedOperator = packedOperator,
          sparseStoragePackedVector = Unboxed.fromList vectorValues,
          sparseStorageGraphVertices = [0 .. dimension - 1],
          sparseStorageGraphEdges = duplicatePathEdges dimension
        }

denseToCooWeight :: SparseStorageCase -> BenchmarkWeight
denseToCooWeight benchmarkCase =
  BenchmarkWeight (cooChecksum (denseToCOO (sparseStorageDense32 benchmarkCase)))

denseToCsrWeight :: SparseStorageCase -> BenchmarkWeight
denseToCsrWeight benchmarkCase =
  BenchmarkWeight (csrChecksum (denseToCSR (sparseStorageDense32 benchmarkCase)))

denseToCscWeight :: SparseStorageCase -> BenchmarkWeight
denseToCscWeight benchmarkCase =
  BenchmarkWeight (cscChecksum (denseToCSC (sparseStorageDense32 benchmarkCase)))

csrToCooWeight :: SparseStorageCase -> BenchmarkWeight
csrToCooWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseStorageLabel benchmarkCase <> " CSR -> COO")
    cooChecksum
    (csrToCOO (sparseStorageCSR benchmarkCase))

csrToCscWeight :: SparseStorageCase -> BenchmarkWeight
csrToCscWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseStorageLabel benchmarkCase <> " CSR -> CSC")
    cscChecksum
    (csrToCSC (sparseStorageCSR benchmarkCase))

csrMatvecWeight :: SparseStorageCase -> BenchmarkWeight
csrMatvecWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseStorageLabel benchmarkCase <> " CSR matvec")
    unboxedVectorChecksum
    (csrMatVecVector (sparseStorageCSR benchmarkCase) (sparseStoragePackedVector benchmarkCase))

packedSparseWeight :: SparseStorageCase -> BenchmarkWeight
packedSparseWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseStorageLabel benchmarkCase <> " packed sparse apply")
    unboxedVectorChecksum
    (applyPackedSparseOperatorDense (sparseStoragePackedOperator benchmarkCase) (sparseStoragePackedVector benchmarkCase))

graphLaplacianWeight :: SparseStorageCase -> BenchmarkWeight
graphLaplacianWeight benchmarkCase =
  eitherBenchmarkWeight
    (sparseStorageLabel benchmarkCase <> " graph Laplacian construction")
    csrChecksum
    ( graphLaplacianCSR
        (sparseStorageGraphVertices benchmarkCase)
        (sparseStorageGraphEdges benchmarkCase)
    )

duplicatePathEdges :: Int -> [GraphEdge Int]
duplicatePathEdges dimension =
  concatMap
    ( \vertexIndex ->
        [ GraphEdge vertexIndex (vertexIndex + 1) 0.75,
          GraphEdge (vertexIndex + 1) vertexIndex 0.25
        ]
    )
    [0 .. dimension - 2]

cooChecksum :: SparseCOO Double -> Double
cooChecksum cooValue =
  fromIntegral (length (cooEntries cooValue))
    + sum ((\(rowIndex, columnIndex, entryValue) -> fromIntegral rowIndex + fromIntegral columnIndex + abs entryValue) <$> cooEntries cooValue)

csrChecksum :: SparseCSR Double -> Double
csrChecksum csrValue =
  fromIntegral (Unboxed.length (csrValuesVector csrValue))
    + fromIntegral (Unboxed.sum (csrRowOffsetsVector csrValue))
    + fromIntegral (Unboxed.sum (csrColumnIndicesVector csrValue))
    + unboxedVectorChecksum (csrValuesVector csrValue)

cscChecksum :: SparseCSC Double -> Double
cscChecksum cscValue =
  fromIntegral (Unboxed.length (cscValuesVector cscValue))
    + fromIntegral (Unboxed.sum (cscColumnOffsetsVector cscValue))
    + fromIntegral (Unboxed.sum (cscRowIndicesVector cscValue))
    + unboxedVectorChecksum (cscValuesVector cscValue)

unboxedVectorChecksum :: Unboxed.Vector Double -> Double
unboxedVectorChecksum values =
  Unboxed.foldl' (\acc value -> acc + abs value) 0.0 values

{-# LANGUAGE DataKinds #-}

module ProjectedBlock
  ( projectedBlockBenchmarks,
    projectedBlockOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import qualified Data.Vector.Unboxed as U
import Env (BenchmarkSelection)
import Fixtures
  ( benchmarkSeedBlock,
    denseOperator,
    projectedBenchmarkDimension,
    projectedBenchmarkRows,
    projectedBlockBenchmarkCases,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight (..),
    OnceBenchmark,
    PreparedBenchmarkRow (..),
    ProjectedBlockBenchmarkCase (..),
    ProjectedBlockPreparedCase (..),
    eitherBenchmarkWeight,
    eigenpairsChecksum,
    renderPreparedBenchmark,
    renderPreparedOnceBenchmark,
  )
import Moonlight.LinAlg.Krylov
  ( SpectrumEnd (..),
    blockLanczosSymmetric,
    defaultBlockLanczosConfig,
    defaultLanczosConfig,
    lanczosSymmetric,
    mkPositiveCount,
    withBlockLanczosBlockSize,
    withBlockLanczosIterations,
    withLanczosIterations,
  )
import Moonlight.LinAlg.Operator
  ( LinearOperator,
    OperatorSymmetry (..),
    pathLaplacianLinearOperator,
  )
import Moonlight.LinAlg.Pure.Dense.Decomposition (symmetricEigenPairs)
import Moonlight.LinAlg.Pure.Krylov.Projected
  ( ProjectedSubspace,
    SymmetricProjectedOperator (..),
    applySymmetricProjectedOperatorU,
    projectedEigenpairs,
    projectedEigenvalues,
    projectedSubspaceDimension,
    projectedSubspaceFromBlockLanczos,
    projectedSubspaceFromLanczos,
    projectedSubspaceOperator,
    symmetricProjectedOperatorDimension,
  )
import Moonlight.LinAlg.Pure.Krylov.Selection (sortRawPairsForSpectrum)
import Moonlight.LinAlg.Native (selectedSymmetricBlockTridiagonalEigenRequestLapack)
import Moonlight.LinAlg.Spectral (EigenRequest (..))
import Moonlight.LinAlg.Pure.Structured.BlockTridiagonal (SymmetricBlockTridiagonal)
import Test.Tasty.Bench (Benchmark, bgroup)
import Prelude

data ProjectedTridiagonalBenchmarkCase = ProjectedTridiagonalBenchmarkCase
  { projectedTridiagonalLabel :: !String,
    projectedTridiagonalDimension :: !Int,
    projectedTridiagonalIterations :: !Int,
    projectedTridiagonalRequestedModes :: !Int
  }

data ProjectedTridiagonalPreparedCase = ProjectedTridiagonalPreparedCase
  { projectedTridiagonalPreparedCase :: !ProjectedTridiagonalBenchmarkCase,
    projectedTridiagonalPreparedOperator :: !(LinearOperator 'SelfAdjointOperator),
    projectedTridiagonalPreparedSubspace :: !ProjectedSubspace
  }

instance NFData ProjectedTridiagonalPreparedCase where
  rnf preparedCase =
    projectedTridiagonalPreparedCase preparedCase
      `seq` projectedTridiagonalPreparedOperator preparedCase
      `seq` projectedTridiagonalPreparedSubspace preparedCase
      `seq` ()

projectedBlockBenchmarks :: BenchmarkSelection -> Benchmark
projectedBlockBenchmarks benchmarkSelection =
  bgroup
    "projected structured eigensolve"
    ( concatMap
        (\benchmarkCase -> renderPreparedBenchmark (prepareProjectedTridiagonalCase benchmarkCase) <$> projectedTridiagonalRows benchmarkCase)
        projectedTridiagonalBenchmarkCases
        <> concatMap
          (\benchmarkCase -> renderPreparedBenchmark (prepareProjectedBlockCase benchmarkCase) <$> projectedBlockRows benchmarkCase)
          (projectedBlockBenchmarkCases benchmarkSelection)
    )

projectedBlockOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
projectedBlockOnceBenchmarks benchmarkSelection =
  concatMap
    (\benchmarkCase -> renderPreparedOnceBenchmark "projected structured eigensolve." (prepareProjectedTridiagonalCase benchmarkCase) <$> projectedTridiagonalRows benchmarkCase)
    projectedTridiagonalBenchmarkCases
    <> concatMap
      (\benchmarkCase -> renderPreparedOnceBenchmark "projected structured eigensolve." (prepareProjectedBlockCase benchmarkCase) <$> projectedBlockRows benchmarkCase)
      (projectedBlockBenchmarkCases benchmarkSelection)

projectedTridiagonalBenchmarkCases :: [ProjectedTridiagonalBenchmarkCase]
projectedTridiagonalBenchmarkCases =
  [ProjectedTridiagonalBenchmarkCase "tridiagonal-path-512" 512 16 4]

projectedTridiagonalRows :: ProjectedTridiagonalBenchmarkCase -> [PreparedBenchmarkRow ProjectedTridiagonalPreparedCase]
projectedTridiagonalRows benchmarkCase =
  [ PurePreparedBenchmarkRow (projectedTridiagonalValuesBenchmarkLabel benchmarkCase) projectedTridiagonalValuesWeight,
    PurePreparedBenchmarkRow (projectedTridiagonalPairsBenchmarkLabel benchmarkCase) projectedTridiagonalPairsWeight
  ]

projectedTridiagonalValuesBenchmarkLabel :: ProjectedTridiagonalBenchmarkCase -> String
projectedTridiagonalValuesBenchmarkLabel =
  projectedTridiagonalBenchmarkLabel "values"

projectedTridiagonalPairsBenchmarkLabel :: ProjectedTridiagonalBenchmarkCase -> String
projectedTridiagonalPairsBenchmarkLabel =
  projectedTridiagonalBenchmarkLabel "pairs"

projectedTridiagonalBenchmarkLabel :: String -> ProjectedTridiagonalBenchmarkCase -> String
projectedTridiagonalBenchmarkLabel requestLabel benchmarkCase =
  projectedTridiagonalLabel benchmarkCase
    <> " "
    <> requestLabel
    <> " n="
    <> show (projectedTridiagonalDimension benchmarkCase)
    <> " m="
    <> show (projectedTridiagonalIterations benchmarkCase)

prepareProjectedTridiagonalCase :: ProjectedTridiagonalBenchmarkCase -> BenchmarkSetup ProjectedTridiagonalPreparedCase
prepareProjectedTridiagonalCase benchmarkCase =
  BenchmarkSetup $ do
    iterationCount <-
      first
        (\err -> "invalid projected tridiagonal iteration count for " <> projectedTridiagonalLabel benchmarkCase <> ": " <> show err)
        (mkPositiveCount (projectedTridiagonalIterations benchmarkCase))
    operatorValue <-
      first
        (\err -> "projected tridiagonal operator construction failed for " <> projectedTridiagonalLabel benchmarkCase <> ": " <> show err)
        (pathLaplacianLinearOperator (projectedTridiagonalDimension benchmarkCase))
    subspace <-
      first
        (\err -> "projected tridiagonal decomposition failed for " <> projectedTridiagonalLabel benchmarkCase <> ": " <> show err)
        ( projectedSubspaceFromLanczos
            <$> lanczosSymmetric
              (withLanczosIterations iterationCount defaultLanczosConfig)
              operatorValue
              (projectedSeedVector (projectedTridiagonalDimension benchmarkCase))
        )
    pure
      ProjectedTridiagonalPreparedCase
        { projectedTridiagonalPreparedCase = benchmarkCase,
          projectedTridiagonalPreparedOperator = operatorValue,
          projectedTridiagonalPreparedSubspace = subspace
        }

projectedTridiagonalValuesWeight :: ProjectedTridiagonalPreparedCase -> BenchmarkWeight
projectedTridiagonalValuesWeight preparedCase =
  case
    projectedEigenvalues
      SmallestEigenvalues
      (projectedTridiagonalRequestedModes (projectedTridiagonalPreparedCase preparedCase))
      (projectedTridiagonalPreparedOperator preparedCase)
      (projectedTridiagonalPreparedSubspace preparedCase) of
    Left err -> BenchmarkMeasurementFailure (projectedTridiagonalLabel (projectedTridiagonalPreparedCase preparedCase) <> " values: " <> show err)
    Right values -> BenchmarkWeight (U.sum values)

projectedTridiagonalPairsWeight :: ProjectedTridiagonalPreparedCase -> BenchmarkWeight
projectedTridiagonalPairsWeight preparedCase =
  case
    projectedEigenpairs
      SmallestEigenvalues
      (projectedTridiagonalRequestedModes (projectedTridiagonalPreparedCase preparedCase))
      (projectedTridiagonalPreparedOperator preparedCase)
      (projectedTridiagonalPreparedSubspace preparedCase) of
    Left err -> BenchmarkMeasurementFailure (projectedTridiagonalLabel (projectedTridiagonalPreparedCase preparedCase) <> " pairs: " <> show err)
    Right pairs -> BenchmarkWeight (eigenpairsChecksum pairs)

projectedBlockRows :: ProjectedBlockBenchmarkCase -> [PreparedBenchmarkRow ProjectedBlockPreparedCase]
projectedBlockRows benchmarkCase =
  [ EffectfulPreparedBenchmarkRow (projectedBlockValuesBenchmarkLabel benchmarkCase) projectedBlockValuesWeight,
    EffectfulPreparedBenchmarkRow (projectedBlockPairsBenchmarkLabel benchmarkCase) projectedBlockPairsWeight,
    PurePreparedBenchmarkRow (projectedDenseOracleBenchmarkLabel benchmarkCase) projectedDenseOracleWeight
  ]

projectedBlockValuesBenchmarkLabel :: ProjectedBlockBenchmarkCase -> String
projectedBlockValuesBenchmarkLabel benchmarkCase =
  projectedBlockBenchmarkLabel "values" benchmarkCase

projectedBlockPairsBenchmarkLabel :: ProjectedBlockBenchmarkCase -> String
projectedBlockPairsBenchmarkLabel benchmarkCase =
  projectedBlockBenchmarkLabel "pairs" benchmarkCase

projectedDenseOracleBenchmarkLabel :: ProjectedBlockBenchmarkCase -> String
projectedDenseOracleBenchmarkLabel benchmarkCase =
  projectedBlockBenchmarkLabel "generic dense oracle" benchmarkCase

projectedBlockBenchmarkLabel :: String -> ProjectedBlockBenchmarkCase -> String
projectedBlockBenchmarkLabel requestLabel benchmarkCase =
  projectedBenchmarkLabel benchmarkCase
    <> " "
    <> requestLabel
    <> " profile="
    <> show (projectedBenchmarkSpectrumProfile benchmarkCase)
    <> " n="
    <> show (projectedBenchmarkDimension benchmarkCase)

prepareProjectedBlockCase :: ProjectedBlockBenchmarkCase -> BenchmarkSetup ProjectedBlockPreparedCase
prepareProjectedBlockCase benchmarkCase =
  BenchmarkSetup $ do
    iterationCount <-
      first
        (\err -> "invalid projected benchmark iteration count for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> show err)
        (mkPositiveCount (projectedBenchmarkIterations benchmarkCase))
    blockSize <-
      first
        (\err -> "invalid projected benchmark block size for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> show err)
        (mkPositiveCount (projectedBenchmarkBlockSize benchmarkCase))
    operatorValue <-
      first
        (\err -> "projected benchmark operator construction failed for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> err)
        (denseOperator (projectedBenchmarkRows benchmarkCase))
    let operatorDimension = projectedBenchmarkDimension benchmarkCase
        seedBlock = benchmarkSeedBlock operatorDimension (projectedBenchmarkBlockSize benchmarkCase)
        blockConfig =
          withBlockLanczosBlockSize
            blockSize
            (withBlockLanczosIterations iterationCount defaultBlockLanczosConfig)
    subspace <-
      first
        (\err -> "projected benchmark decomposition failed for " <> projectedBenchmarkLabel benchmarkCase <> ": " <> show err)
        (projectedSubspaceFromBlockLanczos <$> blockLanczosSymmetric blockConfig operatorValue seedBlock)
    pure
      ProjectedBlockPreparedCase
        { projectedPreparedCase = benchmarkCase,
          projectedPreparedOperator = operatorValue,
          projectedPreparedSubspace = subspace,
          projectedPreparedDimension = projectedSubspaceDimension subspace
        }

projectedBlockValuesWeight :: ProjectedBlockPreparedCase -> IO BenchmarkWeight
projectedBlockValuesWeight preparedCase =
  case nativeProjectedBlockOperator preparedCase of
    Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel (projectedPreparedCase preparedCase) <> " values: " <> err))
    Right blockValue ->
      case mkPositiveCount (projectedBenchmarkRequestedModes (projectedPreparedCase preparedCase)) of
        Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel (projectedPreparedCase preparedCase) <> " values: " <> show err))
        Right countValue ->
          eitherBenchmarkWeight
            (projectedBenchmarkLabel (projectedPreparedCase preparedCase) <> " values")
            U.sum
            <$> selectedSymmetricBlockTridiagonalEigenRequestLapack
              (EigenvaluesRequest SmallestEigenvalues countValue)
              blockValue

projectedBlockPairsWeight :: ProjectedBlockPreparedCase -> IO BenchmarkWeight
projectedBlockPairsWeight preparedCase =
  case nativeProjectedBlockOperator preparedCase of
    Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel (projectedPreparedCase preparedCase) <> " pairs: " <> err))
    Right blockValue ->
      case mkPositiveCount (projectedBenchmarkRequestedModes (projectedPreparedCase preparedCase)) of
        Left err -> pure (BenchmarkMeasurementFailure (projectedBenchmarkLabel (projectedPreparedCase preparedCase) <> " pairs: " <> show err))
        Right countValue ->
          eitherBenchmarkWeight
            (projectedBenchmarkLabel (projectedPreparedCase preparedCase) <> " pairs")
            eigenpairsChecksum
            <$> selectedSymmetricBlockTridiagonalEigenRequestLapack
              (EigenpairsRequest SmallestEigenvalues countValue)
              blockValue

nativeProjectedBlockOperator :: ProjectedBlockPreparedCase -> Either String SymmetricBlockTridiagonal
nativeProjectedBlockOperator preparedCase =
  case projectedSubspaceOperator (projectedPreparedSubspace preparedCase) of
    BlockTridiagonalProjectedOperator blockValue -> Right blockValue
    TridiagonalProjectedOperator _ -> Left "expected block-tridiagonal projected operator"

projectedDenseOracleWeight :: ProjectedBlockPreparedCase -> BenchmarkWeight
projectedDenseOracleWeight preparedCase =
  case projectedDenseOraclePairs SmallestEigenvalues (projectedBenchmarkRequestedModes (projectedPreparedCase preparedCase)) (projectedSubspaceOperator (projectedPreparedSubspace preparedCase)) of
    Left err -> BenchmarkMeasurementFailure (projectedBenchmarkLabel (projectedPreparedCase preparedCase) <> " dense oracle: " <> show err)
    Right pairs -> BenchmarkWeight (projectedDenseOracleChecksum pairs)

projectedDenseOraclePairs ::
  SpectrumEnd ->
  Int ->
  SymmetricProjectedOperator ->
  Either String [(Double, [Double])]
projectedDenseOraclePairs spectrumEnd requestedModes projectedOperator =
  let projectedDimension = symmetricProjectedOperatorDimension projectedOperator
   in if requestedModes <= 0
        then Left "projected dense oracle requested count must be positive"
        else
          if requestedModes > projectedDimension
            then Left "projected dense oracle requested count exceeds projected dimension"
            else do
              projectedRows <- projectedOperatorDenseRows projectedOperator
              rawPairs <- first show (symmetricEigenPairs projectedDimension projectedRows)
              Right (take requestedModes (sortRawPairsForSpectrum spectrumEnd rawPairs))

projectedOperatorDenseRows :: SymmetricProjectedOperator -> Either String [[Double]]
projectedOperatorDenseRows projectedOperator =
  let projectedDimension = symmetricProjectedOperatorDimension projectedOperator
   in do
        imageColumns <-
          traverse
            (\coordinateIndex -> first show (applySymmetricProjectedOperatorU projectedOperator (coordinateBasisVector projectedDimension coordinateIndex)))
            [0 .. projectedDimension - 1]
        traverse (projectedDenseRow imageColumns) [0 .. projectedDimension - 1]

projectedDenseRow :: [U.Vector Double] -> Int -> Either String [Double]
projectedDenseRow imageColumns rowIndex =
  traverse (projectedColumnEntry rowIndex) imageColumns

projectedColumnEntry :: Int -> U.Vector Double -> Either String Double
projectedColumnEntry rowIndex columnValue =
  case columnValue U.!? rowIndex of
    Just entryValue -> Right entryValue
    Nothing -> Left "projected dense oracle column dimension mismatch"

coordinateBasisVector :: Int -> Int -> U.Vector Double
coordinateBasisVector dimension columnIndex =
  U.generate dimension (\rowIndex -> if rowIndex == columnIndex then 1.0 else 0.0)

projectedSeedVector :: Int -> U.Vector Double
projectedSeedVector dimension =
  U.generate dimension (\indexValue -> if indexValue == 0 then 1.0 else 1.0 / fromIntegral (indexValue + 1))

projectedDenseOracleChecksum :: [(Double, [Double])] -> Double
projectedDenseOracleChecksum pairs =
  sum ((\(eigenvalue, eigenvector) -> eigenvalue + sum (abs <$> eigenvector)) <$> pairs)

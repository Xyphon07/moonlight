module SparseSpectral
  ( benchmarkNotice,
    shouldIncludeLarge,
    shouldInclude100k,
    sparseSpectralBenchmarks,
  )
where

import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)
import Data.Function ((&))
import Moonlight.Homology
  ( GraphSpectralMode (..),
    HomologyFailure,
  )
import Moonlight.Homology.Sequence
  ( defaultSparseSpectralConfig,
    weightedGraphSparseSpectralModes,
  )
import System.Environment (lookupEnv)

data SparseSpectralBenchmarkCase = SparseSpectralBenchmarkCase
  { ssbcLabel :: !String,
    ssbcVertexCount :: !Int,
    ssbcRequestedModeCount :: !Int
  }

data MeasuredSpectralWeight
  = MeasuredSpectralWeight !Double
  | SpectralMeasurementFailure !String
  deriving stock (Show)

sparseSpectralBenchmarks :: Bool -> Bool -> Benchmark
sparseSpectralBenchmarks includeLarge include100k =
  bgroup
    "successor-like-sparse-spectral"
    (fmap benchmarkSparseSpectralCase (sparseSpectralBenchmarkCases includeLarge include100k))

sparseSpectralBenchmarkCases :: Bool -> Bool -> [SparseSpectralBenchmarkCase]
sparseSpectralBenchmarkCases includeLarge include100k =
  [SparseSpectralBenchmarkCase "successor-carrier-1k" 1024 3]
    <> ( if includeLarge
           then
             [ SparseSpectralBenchmarkCase "successor-carrier-10k" 10000 3,
               SparseSpectralBenchmarkCase "successor-carrier-50k" 50000 3
             ]
           else []
       )
    <> [SparseSpectralBenchmarkCase "successor-carrier-100k" 100000 3 | include100k]

benchmarkNotice :: Bool -> Bool -> String
benchmarkNotice includeLarge include100k =
  "large sparse spectral benchmarks "
    <> gateNotice "MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_LARGE" includeLarge
    <> "; 100k sparse spectral benchmark "
    <> gateNotice "MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_100K" include100k
    <> "."

gateNotice :: String -> Bool -> String
gateNotice variableName enabled =
  if enabled
    then "enabled via " <> variableName
    else "skipped by default; set " <> variableName <> "=1 to opt in"

shouldIncludeLarge :: IO Bool
shouldIncludeLarge =
  fmap parseTruthy (lookupEnv "MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_LARGE")

shouldInclude100k :: IO Bool
shouldInclude100k =
  fmap parseTruthy (lookupEnv "MOONLIGHT_HOMOLOGY_SPARSE_SPECTRAL_BENCH_ENABLE_100K")

parseTruthy :: Maybe String -> Bool
parseTruthy maybeValue =
  case maybeValue of
    Just "1" -> True
    Just "true" -> True
    Just "TRUE" -> True
    Just "yes" -> True
    Just "YES" -> True
    _ -> False

benchmarkSparseSpectralCase :: SparseSpectralBenchmarkCase -> Benchmark
benchmarkSparseSpectralCase benchmarkCase =
  bench (ssbcLabel benchmarkCase) (whnf sparseSpectralWeight benchmarkCase)

sparseSpectralWeight :: SparseSpectralBenchmarkCase -> MeasuredSpectralWeight
sparseSpectralWeight =
  either
    (SpectralMeasurementFailure . show)
    MeasuredSpectralWeight
    . sparseSpectralWeightResult

sparseSpectralWeightResult :: SparseSpectralBenchmarkCase -> Either HomologyFailure Double
sparseSpectralWeightResult benchmarkCase =
  weightedGraphSparseSpectralModes
    defaultSparseSpectralConfig
    (ssbcRequestedModeCount benchmarkCase)
    (ssbcVertexCount benchmarkCase)
    (successorLikeSparseSupports (ssbcVertexCount benchmarkCase))
    & fmap spectralModeChecksum

successorLikeSparseSupports :: Int -> [(Int, Int, Double)]
successorLikeSparseSupports vertexCount =
  localSuccessorSupports vertexCount
    <> strideSuccessorSupports vertexCount 8 0.25
    <> strideSuccessorSupports vertexCount 31 0.125

localSuccessorSupports :: Int -> [(Int, Int, Double)]
localSuccessorSupports vertexCount
  | vertexCount <= 1 = []
  | otherwise =
      [0 .. max 0 (vertexCount - 2)]
        & fmap (\sourceVertex -> (sourceVertex, sourceVertex + 1, 1.0))

strideSuccessorSupports :: Int -> Int -> Double -> [(Int, Int, Double)]
strideSuccessorSupports vertexCount strideValue edgeWeight
  | vertexCount <= strideValue = []
  | otherwise =
      [0, strideValue .. max 0 (vertexCount - strideValue - 1)]
        & fmap (\sourceVertex -> (sourceVertex, sourceVertex + strideValue, edgeWeight))

spectralModeChecksum :: [GraphSpectralMode] -> Double
spectralModeChecksum =
  foldl' (\checksumValue modeValue -> checksumValue + spectralModeWeight modeValue) 0.0

spectralModeWeight :: GraphSpectralMode -> Double
spectralModeWeight modeValue =
  spectralEigenvalue modeValue
    + spectralSupportCriticality modeValue
    + coefficientChecksum (spectralCoefficients modeValue)
    + fromIntegral (length (spectralPositiveSupport modeValue))
    - fromIntegral (length (spectralNegativeSupport modeValue))

coefficientChecksum :: [(Int, Double)] -> Double
coefficientChecksum =
  foldl'
    ( \checksumValue (cellIndex, coefficientValue) ->
        checksumValue + coefficientValue * fromIntegral (cellIndex + 1)
    )
    0.0

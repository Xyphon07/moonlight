{-# LANGUAGE DataKinds #-}

module Types
  ( BenchmarkSetup (..),
    PreparedBenchmarkRow (..),
    OnceBenchmark (..),
    OnceBenchmarkResult (..),
    OnceBenchmarkStats (..),
    BenchmarkWeight (..),
    ProjectedBlockBenchmarkCase (..),
    ProjectedBlockPreparedCase (..),
    SparseKrylovBenchmarkCase (..),
    SparseKrylovPreparedCase (..),
    SpectrumProfile (..),
    eigenpairsChecksum,
    eigenpairsResidualValidationChecksum,
    benchmarkWeightEither,
    eitherBenchmarkWeight,
    prepareBenchmarkSetup,
    renderPreparedBenchmark,
    renderPreparedOnceBenchmark,
    runOnceBenchmark,
  )
where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.Vector.Unboxed as U
import GHC.Stats
  ( RTSStats,
    allocated_bytes,
    gc,
    gcdetails_live_bytes,
    getRTSStats,
    getRTSStatsEnabled,
    max_live_bytes,
  )
import Moonlight.LinAlg.Pure.Krylov.Projected
  ( ProjectedSubspace,
  )
import Moonlight.LinAlg.Operator
  ( LinearOperator,
    OperatorSymmetry (..),
  )
import Moonlight.LinAlg.Pure.Structured.Tridiagonal (SymmetricTridiagonal)
import Moonlight.LinAlg.Spectral
  ( Eigenpairs,
    eigenpairCount,
    eigenpairDimension,
    eigenpairResidualNorms,
    eigenpairVectorAt,
    eigenpairValues,
    eigenpairVectorsColumnMajor,
  )
import System.CPUTime (getCPUTime)
import System.Mem (performMajorGC)
import Test.Tasty.Bench (Benchmark, bench, env, nf, nfIO)
import Prelude

newtype BenchmarkSetup value = BenchmarkSetup
  { runBenchmarkSetup :: Either String value
  }

data OnceBenchmark = OnceBenchmark
  { onceBenchmarkLabel :: !String,
    onceBenchmarkAction :: IO (Either String Double)
  }

data OnceBenchmarkResult = OnceBenchmarkResult
  { onceResultLabel :: !String,
    onceResultElapsedSeconds :: !Double,
    onceResultChecksum :: !Double,
    onceResultStats :: !(Maybe OnceBenchmarkStats)
  }

data OnceBenchmarkStats = OnceBenchmarkStats
  { onceAllocatedBytes :: !Integer,
    onceLiveBytesAfterMajorGC :: !Integer,
    onceProcessMaximumLiveBytes :: !Integer
  }

data BenchmarkWeight
  = BenchmarkWeight !Double
  | BenchmarkMeasurementFailure !String
  deriving stock (Show)

data PreparedBenchmarkRow preparedCase
  = PurePreparedBenchmarkRow
      !String
      (preparedCase -> BenchmarkWeight)
  | EffectfulPreparedBenchmarkRow
      !String
      (preparedCase -> IO BenchmarkWeight)

instance NFData BenchmarkWeight where
  rnf weightValue =
    case weightValue of
      BenchmarkWeight checksumValue -> rnf checksumValue
      BenchmarkMeasurementFailure failureText -> failBenchmarkMeasurement failureText

data SparseKrylovBenchmarkCase = SparseKrylovBenchmarkCase
  { sparseBenchmarkLabel :: !String,
    sparseBenchmarkDimension :: !Int,
    sparseBenchmarkRequestedModes :: !Int
  }

data SparseKrylovPreparedCase = SparseKrylovPreparedCase
  { sparsePreparedLabel :: !String,
    sparsePreparedRequestedModes :: !Int,
    sparsePreparedTridiagonal :: !SymmetricTridiagonal
  }

instance NFData SparseKrylovPreparedCase where
  rnf preparedCase =
    sparsePreparedLabel preparedCase
      `seq` sparsePreparedRequestedModes preparedCase
      `seq` sparsePreparedTridiagonal preparedCase
      `seq` ()

data ProjectedBlockBenchmarkCase = ProjectedBlockBenchmarkCase
  { projectedBenchmarkLabel :: !String,
    projectedBenchmarkBlockCount :: !Int,
    projectedBenchmarkBlockSize :: !Int,
    projectedBenchmarkIterations :: !Int,
    projectedBenchmarkRequestedModes :: !Int,
    projectedBenchmarkSpectrumProfile :: !SpectrumProfile
  }

data SpectrumProfile = ClusteredSpectrum | SeparatedSpectrum
  deriving stock (Eq, Show)

data ProjectedBlockPreparedCase = ProjectedBlockPreparedCase
  { projectedPreparedCase :: !ProjectedBlockBenchmarkCase,
    projectedPreparedOperator :: !(LinearOperator 'SelfAdjointOperator),
    projectedPreparedSubspace :: !ProjectedSubspace,
    projectedPreparedDimension :: !Int
  }

instance NFData ProjectedBlockPreparedCase where
  rnf preparedCase =
    projectedPreparedCase preparedCase
      `seq` projectedPreparedOperator preparedCase
      `seq` projectedPreparedSubspace preparedCase
      `seq` projectedPreparedDimension preparedCase
      `seq` ()

prepareBenchmarkSetup :: BenchmarkSetup value -> IO value
prepareBenchmarkSetup =
  either failBenchmarkSetup pure . runBenchmarkSetup

renderPreparedBenchmark ::
  NFData preparedCase =>
  BenchmarkSetup preparedCase ->
  PreparedBenchmarkRow preparedCase ->
  Benchmark
renderPreparedBenchmark benchmarkSetup benchmarkRow =
  case benchmarkRow of
    PurePreparedBenchmarkRow benchmarkLabel benchmarkWeight ->
      env (prepareBenchmarkSetup benchmarkSetup) $ \preparedCase ->
        bench benchmarkLabel (nf benchmarkWeight preparedCase)
    EffectfulPreparedBenchmarkRow benchmarkLabel benchmarkWeight ->
      env (prepareBenchmarkSetup benchmarkSetup) $ \preparedCase ->
        bench benchmarkLabel (nfIO (benchmarkWeight preparedCase))

renderPreparedOnceBenchmark ::
  String ->
  BenchmarkSetup preparedCase ->
  PreparedBenchmarkRow preparedCase ->
  OnceBenchmark
renderPreparedOnceBenchmark benchmarkGroup benchmarkSetup benchmarkRow =
  case benchmarkRow of
    PurePreparedBenchmarkRow benchmarkLabel benchmarkWeight ->
      OnceBenchmark
        { onceBenchmarkLabel = benchmarkGroup <> benchmarkLabel,
          onceBenchmarkAction =
            pure
              (runBenchmarkSetup benchmarkSetup >>= benchmarkWeightEither . benchmarkWeight)
        }
    EffectfulPreparedBenchmarkRow benchmarkLabel benchmarkWeight ->
      OnceBenchmark
        { onceBenchmarkLabel = benchmarkGroup <> benchmarkLabel,
          onceBenchmarkAction =
            case runBenchmarkSetup benchmarkSetup of
              Left err -> pure (Left err)
              Right preparedCase -> benchmarkWeightEither <$> benchmarkWeight preparedCase
        }

runOnceBenchmark :: OnceBenchmark -> IO (Either String OnceBenchmarkResult)
runOnceBenchmark benchmarkValue = do
  statsEnabled <- getRTSStatsEnabled
  beforeStats <- beforeOnceStats statsEnabled
  startTime <- getCPUTime
  resultValue <- onceBenchmarkAction benchmarkValue >>= evaluate . force
  endTime <- getCPUTime
  afterStats <- afterOnceStats statsEnabled
  pure
    ( fmap
        ( \checksumValue ->
            OnceBenchmarkResult
              { onceResultLabel = onceBenchmarkLabel benchmarkValue,
                onceResultElapsedSeconds = fromIntegral (endTime - startTime) / 1.0e12,
                onceResultChecksum = checksumValue,
                onceResultStats = onceStatsDelta <$> beforeStats <*> afterStats
              }
        )
        resultValue
    )

beforeOnceStats :: Bool -> IO (Maybe RTSStats)
beforeOnceStats statsEnabled =
  if statsEnabled
    then performMajorGC *> (Just <$> getRTSStats)
    else pure Nothing

afterOnceStats :: Bool -> IO (Maybe RTSStats)
afterOnceStats statsEnabled =
  if statsEnabled
    then performMajorGC *> (Just <$> getRTSStats)
    else pure Nothing

onceStatsDelta :: RTSStats -> RTSStats -> OnceBenchmarkStats
onceStatsDelta beforeStats afterStats =
  OnceBenchmarkStats
    { onceAllocatedBytes =
        toInteger (allocated_bytes afterStats - allocated_bytes beforeStats),
      onceLiveBytesAfterMajorGC =
        toInteger (gcdetails_live_bytes (gc afterStats)),
      onceProcessMaximumLiveBytes =
        toInteger (max_live_bytes afterStats)
    }

failBenchmarkSetup :: String -> IO value
failBenchmarkSetup failureText = do
  putStrLn ("moonlight-linalg benchmark setup failed: " <> failureText)
  ioError (userError failureText)

failBenchmarkMeasurement :: String -> value
failBenchmarkMeasurement failureText =
  error ("moonlight-linalg benchmark measurement failed: " <> failureText)

eigenpairsChecksum :: Eigenpairs -> Double
eigenpairsChecksum pairs =
  U.sum (eigenpairValues pairs)
    + U.sum (eigenpairResidualNorms pairs)
    + U.sum (U.map abs (eigenpairVectorsColumnMajor pairs))

eigenpairsResidualValidationChecksum :: Show err => (U.Vector Double -> Either err (U.Vector Double)) -> Eigenpairs -> Either String Double
eigenpairsResidualValidationChecksum applyVector pairs =
  foldM accumulateResidualChecksum 0.0 [0 .. eigenpairCount pairs - 1]
  where
    accumulateResidualChecksum accumulatedChecksum columnIndex = do
      eigenvalue <- eigenvalueAt columnIndex
      eigenvector <- first show (eigenpairVectorAt columnIndex pairs)
      imageVector <- first show (applyVector eigenvector)
      if U.length imageVector == eigenpairDimension pairs
        then
          pure
            ( accumulatedChecksum
                + normU
                  ( U.zipWith
                      (-)
                      imageVector
                      (U.map (* eigenvalue) eigenvector)
                  )
            )
        else Left "eigenpair residual validation apply dimension mismatch"

    eigenvalueAt columnIndex =
      case eigenpairValues pairs U.!? columnIndex of
        Just eigenvalue -> Right eigenvalue
        Nothing -> Left "eigenpair residual validation value index out of bounds"

normU :: U.Vector Double -> Double
normU vectorValue =
  sqrt (U.sum (U.map (\entryValue -> entryValue * entryValue) vectorValue))

eitherBenchmarkWeight :: Show err => String -> (value -> Double) -> Either err value -> BenchmarkWeight
eitherBenchmarkWeight label checksum =
  either
    (\err -> BenchmarkMeasurementFailure (label <> ": " <> show err))
    (BenchmarkWeight . checksum)

benchmarkWeightEither :: BenchmarkWeight -> Either String Double
benchmarkWeightEither weightValue =
  case weightValue of
    BenchmarkWeight checksumValue -> Right checksumValue
    BenchmarkMeasurementFailure failureText -> Left failureText

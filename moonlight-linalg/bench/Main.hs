module Main
  ( main,
  )
where

import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..))
import Env
  ( BenchmarkSelection,
    benchmarkNotice,
    readBenchmarkSelection,
  )
import DenseCore (denseCoreBenchmarks, denseCoreOnceBenchmarks)
import DenseDecomposition (denseDecompositionBenchmarks, denseDecompositionOnceBenchmarks)
import DomainAlgebra (domainAlgebraBenchmarks, domainAlgebraOnceBenchmarks)
import GeometryStatics (geometryStaticsBenchmarks, geometryStaticsOnceBenchmarks)
import NativeLapack (nativeLapackBenchmarks, nativeLapackOnceBenchmarks)
import ProjectedBlock (projectedBlockBenchmarks, projectedBlockOnceBenchmarks)
import SparseKrylov (sparseKrylovBenchmarks, sparseKrylovOnceBenchmarks)
import SparseSolvers (sparseSolverBenchmarks, sparseSolverOnceBenchmarks)
import SpectralDispatch (spectralDispatchBenchmarks, spectralDispatchOnceBenchmarks)
import SparseStorage (sparseStorageBenchmarks, sparseStorageOnceBenchmarks)
import Types
  ( OnceBenchmark,
    OnceBenchmarkResult (..),
    OnceBenchmarkStats (..),
    runOnceBenchmark,
  )
import System.Environment (getArgs)
import Test.Tasty.Bench (defaultMain)
import Text.Printf (printf)
import Prelude

main :: IO ()
main = do
  benchmarkSelection <- readBenchmarkSelection
  putStrLn (benchmarkNotice benchmarkSelection)
  args <- getArgs
  case args of
    ["--once"] ->
      runOnceBenchmarks (linalgOnceBenchmarks benchmarkSelection)
    _ ->
      defaultMain
        [ denseCoreBenchmarks benchmarkSelection,
          denseDecompositionBenchmarks,
          sparseStorageBenchmarks benchmarkSelection,
          sparseSolverBenchmarks benchmarkSelection,
          spectralDispatchBenchmarks benchmarkSelection,
          domainAlgebraBenchmarks,
          geometryStaticsBenchmarks benchmarkSelection,
          sparseKrylovBenchmarks benchmarkSelection,
          nativeLapackBenchmarks benchmarkSelection,
          projectedBlockBenchmarks benchmarkSelection
        ]

linalgOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
linalgOnceBenchmarks benchmarkSelection =
  denseCoreOnceBenchmarks benchmarkSelection
    <> denseDecompositionOnceBenchmarks
    <> sparseStorageOnceBenchmarks benchmarkSelection
    <> sparseSolverOnceBenchmarks benchmarkSelection
    <> spectralDispatchOnceBenchmarks benchmarkSelection
    <> domainAlgebraOnceBenchmarks
    <> geometryStaticsOnceBenchmarks benchmarkSelection
    <> sparseKrylovOnceBenchmarks benchmarkSelection
    <> nativeLapackOnceBenchmarks benchmarkSelection
    <> projectedBlockOnceBenchmarks benchmarkSelection

runOnceBenchmarks :: [OnceBenchmark] -> IO ()
runOnceBenchmarks benchmarks = do
  results <- traverse runOnceBenchmark benchmarks
  case sequence results of
    Left failureText ->
      ioError (userError ("moonlight-linalg once benchmark failed: " <> failureText))
    Right successfulResults ->
      reportOnceResults successfulResults

reportOnceResults :: [OnceBenchmarkResult] -> IO ()
reportOnceResults results = do
  putStrLn
    ( "All "
        <> show (length results)
        <> " benchmark rows executed once (measured CPU "
        <> secondsText totalSeconds
        <> "; checksum "
        <> printf "%.6f" checksumTotal
        <> onceStatsSummaryText results
        <> ")."
    )
  putStrLn "Slowest once rows:"
  traverse_ (putStrLn . renderOnceResult) (take 10 (sortOn (Down . onceResultElapsedSeconds) results))
  case onceResultsWithStats results of
    [] -> putStrLn "RTS per-row allocation stats disabled; rerun with +RTS -T."
    statsRows -> do
      putStrLn "Highest allocation once rows:"
      traverse_ (putStrLn . renderAllocatedOnceResult) (take 10 (sortOn (Down . onceAllocatedBytes . snd) statsRows))
  where
    totalSeconds =
      sum (onceResultElapsedSeconds <$> results)
    checksumTotal =
      sum (onceResultChecksum <$> results)

renderOnceResult :: OnceBenchmarkResult -> String
renderOnceResult result =
  "  "
    <> secondsText (onceResultElapsedSeconds result)
    <> maybe "" renderInlineOnceStats (onceResultStats result)
    <> "  "
    <> onceResultLabel result

renderAllocatedOnceResult :: (OnceBenchmarkResult, OnceBenchmarkStats) -> String
renderAllocatedOnceResult (result, statsValue) =
  "  "
    <> bytesText (onceAllocatedBytes statsValue)
    <> " allocated; "
    <> bytesText (onceLiveBytesAfterMajorGC statsValue)
    <> " live after major GC; "
    <> bytesText (onceProcessMaximumLiveBytes statsValue)
    <> " process maximum residency  "
    <> onceResultLabel result

onceStatsSummaryText :: [OnceBenchmarkResult] -> String
onceStatsSummaryText results =
  case onceResultsWithStats results of
    [] -> "; RTS allocation stats disabled"
    statsRows ->
      "; allocated "
        <> bytesText (sum (onceAllocatedBytes . snd <$> statsRows))
        <> "; max retained live after row GC "
        <> bytesText (maximum (0 : (onceLiveBytesAfterMajorGC . snd <$> statsRows)))
        <> "; process maximum residency "
        <> bytesText (maximum (0 : (onceProcessMaximumLiveBytes . snd <$> statsRows)))

onceResultsWithStats :: [OnceBenchmarkResult] -> [(OnceBenchmarkResult, OnceBenchmarkStats)]
onceResultsWithStats =
  mapMaybe withStats
  where
    withStats result =
      case onceResultStats result of
        Just statsValue -> Just (result, statsValue)
        Nothing -> Nothing

renderInlineOnceStats :: OnceBenchmarkStats -> String
renderInlineOnceStats statsValue =
  " / alloc "
    <> bytesText (onceAllocatedBytes statsValue)
    <> " / live "
    <> bytesText (onceLiveBytesAfterMajorGC statsValue)
    <> " / max "
    <> bytesText (onceProcessMaximumLiveBytes statsValue)

secondsText :: Double -> String
secondsText secondsValue
  | secondsValue < 1.0e-6 = printf "%.3f ns" (secondsValue * 1.0e9)
  | secondsValue < 1.0e-3 = printf "%.3f us" (secondsValue * 1.0e6)
  | secondsValue < 1.0 = printf "%.3f ms" (secondsValue * 1.0e3)
  | otherwise = printf "%.3f s" secondsValue

bytesText :: Integer -> String
bytesText byteCount
  | byteCount < 1024 = show byteCount <> " B"
  | byteCount < 1024 * 1024 = printf "%.3f KiB" (fromIntegral byteCount / 1024 :: Double)
  | byteCount < 1024 * 1024 * 1024 = printf "%.3f MiB" (fromIntegral byteCount / (1024 * 1024) :: Double)
  | otherwise = printf "%.3f GiB" (fromIntegral byteCount / (1024 * 1024 * 1024) :: Double)

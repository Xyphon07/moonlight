module Env
  ( BenchmarkSelection (..),
    benchmarkNotice,
    readBenchmarkSelection,
  )
where

import System.Environment (lookupEnv)
import Prelude

data BenchmarkSelection = BenchmarkSelection
  { includeSparseLarge :: !Bool,
    includeSparse100k :: !Bool,
    includeBroadMedium :: !Bool,
    includeBroadLarge :: !Bool,
    includeProjectedMedium :: !Bool,
    includeProjectedLarge :: !Bool,
    includeNativeLarge :: !Bool
  }

readBenchmarkSelection :: IO BenchmarkSelection
readBenchmarkSelection =
  BenchmarkSelection
    <$> gateEnabled SparseLargeGate
    <*> gateEnabled Sparse100kGate
    <*> gateEnabled BroadMediumGate
    <*> gateEnabled BroadLargeGate
    <*> gateEnabled ProjectedMediumGate
    <*> gateEnabled ProjectedLargeGate
    <*> gateEnabled NativeLargeGate

benchmarkNotice :: BenchmarkSelection -> String
benchmarkNotice selection =
  mconcat
    [ "sparse-large benchmark ",
      gateNotice SparseLargeGate (includeSparseLarge selection || includeSparse100k selection),
      "; 100k sparse Krylov benchmark ",
      gateNotice Sparse100kGate (includeSparse100k selection),
      "; medium broad linalg benchmarks ",
      gateNotice BroadMediumGate (includeBroadMedium selection || includeBroadLarge selection),
      "; large broad linalg benchmarks ",
      gateNotice BroadLargeGate (includeBroadLarge selection),
      "; medium projected structured-block benchmarks ",
      gateNotice ProjectedMediumGate (includeProjectedMedium selection || includeProjectedLarge selection),
      "; large projected structured-block benchmarks ",
      gateNotice ProjectedLargeGate (includeProjectedLarge selection),
      "; large native LAPACK benchmarks ",
      gateNotice NativeLargeGate (includeNativeLarge selection),
      "."
    ]

data BenchmarkGate
  = SparseLargeGate
  | Sparse100kGate
  | BroadMediumGate
  | BroadLargeGate
  | ProjectedMediumGate
  | ProjectedLargeGate
  | NativeLargeGate

benchmarkGateEnvName :: BenchmarkGate -> String
benchmarkGateEnvName benchmarkGate =
  case benchmarkGate of
    SparseLargeGate -> "MOONLIGHT_LINALG_BENCH_ENABLE_SPARSE_LARGE"
    Sparse100kGate -> "MOONLIGHT_LINALG_BENCH_ENABLE_100K"
    BroadMediumGate -> "MOONLIGHT_LINALG_BENCH_ENABLE_BROAD_MEDIUM"
    BroadLargeGate -> "MOONLIGHT_LINALG_BENCH_ENABLE_BROAD_LARGE"
    ProjectedMediumGate -> "MOONLIGHT_LINALG_BENCH_ENABLE_PROJECTED_MEDIUM"
    ProjectedLargeGate -> "MOONLIGHT_LINALG_BENCH_ENABLE_PROJECTED_LARGE"
    NativeLargeGate -> "MOONLIGHT_LINALG_BENCH_ENABLE_NATIVE_LARGE"

gateEnabled :: BenchmarkGate -> IO Bool
gateEnabled =
  fmap parseTruthy . lookupEnv . benchmarkGateEnvName

parseTruthy :: Maybe String -> Bool
parseTruthy maybeValue =
  case maybeValue of
    Just "1" -> True
    Just "true" -> True
    Just "TRUE" -> True
    Just "yes" -> True
    Just "YES" -> True
    _ -> False

gateNotice :: BenchmarkGate -> Bool -> String
gateNotice benchmarkGate enabled =
  if enabled
    then "enabled via " <> benchmarkGateEnvName benchmarkGate
    else "skipped by default; set " <> benchmarkGateEnvName benchmarkGate <> "=1 to opt in"

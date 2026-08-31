module Tableless
  ( tablelessFallbackBenchmarks,
  )
where

import Fixtures
  ( Shape (..),
    caseLabel,
    compileTablelessLatticeEnv,
    compileTablelessLatticeWeight,
    compileTablelessTallGridEnv,
    compileTablelessTallGridWeight,
    compileSizes,
    querySizes,
    tablelessTallGridCompileHeights,
    tablelessTallGridQueryHeights,
    tallGridElementCount,
  )
import Kernels
  ( implicationKeySweepWeight,
    implicationSweepWeight,
    joinMeetSweepWeight,
    residentJoinMeetKeySweepWeight,
    residentLeqSweepWeight,
  )
import Moonlight.FiniteLattice.Core
  ( ContextLattice,
  )
import Moonlight.FiniteLattice.Resident
  ( residentContextKeys,
    withResidentContext,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

tablelessFallbackBenchmarks :: Benchmark
tablelessFallbackBenchmarks =
  bgroup
    "finite-lattice-tableless"
    [ bgroup
        "dense-grid"
        (tablelessDenseGridCompileBenchmarks <> fmap tablelessDenseGridQueryBenchmarks querySizes),
      bgroup
        "tall-grid"
        (tablelessTallGridCompileBenchmarks <> fmap tablelessTallGridQueryBenchmarks tablelessTallGridQueryHeights)
    ]

tablelessDenseGridCompileBenchmarks :: [Benchmark]
tablelessDenseGridCompileBenchmarks =
  [ bench (caseLabel "compile/binary-table-budget" size) (nf (compileTablelessLatticeWeight DenseGrid) size)
  | size <- compileSizes
  ]

tablelessDenseGridQueryBenchmarks :: Int -> Benchmark
tablelessDenseGridQueryBenchmarks size =
  env (compileTablelessLatticeEnv DenseGrid size) $ \lattice ->
    bgroup
      (caseLabel "compiled query fixture" size)
      (tablelessQueryBenchmarkCases size lattice)

tablelessTallGridCompileBenchmarks :: [Benchmark]
tablelessTallGridCompileBenchmarks =
  [ bench (caseLabel "compile/binary-table-budget" (tallGridElementCount height)) (nf compileTablelessTallGridWeight height)
  | height <- tablelessTallGridCompileHeights
  ]

tablelessTallGridQueryBenchmarks :: Int -> Benchmark
tablelessTallGridQueryBenchmarks height =
  env (compileTablelessTallGridEnv height) $ \lattice ->
    bgroup
      (caseLabel "compiled query fixture" (tallGridElementCount height))
      (tablelessQueryBenchmarkCases (tallGridElementCount height) lattice)

tablelessQueryBenchmarkCases :: Int -> ContextLattice Int -> [Benchmark]
tablelessQueryBenchmarkCases size lattice =
  withResidentContext lattice $ \contextValue ->
    let contextKeys = residentContextKeys contextValue
     in [ bench (caseLabel "key <= sweep" size) (nf (residentLeqSweepWeight contextValue) contextKeys),
          bench (caseLabel "key join/meet sweep" size) (nf (residentJoinMeetKeySweepWeight contextValue) contextKeys),
          bench (caseLabel "join/meet sweep" size) (nf (joinMeetSweepWeight size) lattice),
          bench (caseLabel "key heyting implication sweep" size) (nf implicationKeySweepWeight lattice),
          bench (caseLabel "heyting implication sweep" size) (nf (implicationSweepWeight size) lattice)
        ]

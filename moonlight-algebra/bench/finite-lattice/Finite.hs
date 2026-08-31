module Finite
  ( finiteLatticeBenchmarks,
  )
where

import Fixtures
  ( Shape (..),
    caseLabel,
    compileBoundedPresentationWeight,
    compileLatticeEnv,
    compileLatticeWeight,
    compilePresentationWeight,
    compileSizes,
    querySizes,
    shapeLabel,
    shapes,
  )
import Kernels
  ( fixpointPairWeight,
    implicationKeySweepWeight,
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

finiteLatticeBenchmarks :: Benchmark
finiteLatticeBenchmarks =
  bgroup
    "finite-lattice"
    [ bgroup
        (shapeLabel shape)
        (finiteLatticeBenchmarksForShape shape)
    | shape <- shapes
    ]

finiteLatticeBenchmarksForShape :: Shape -> [Benchmark]
finiteLatticeBenchmarksForShape shape =
  compileBenchmarks shape
    <> fmap (queryBenchmarks shape) querySizes

compileBenchmarks :: Shape -> [Benchmark]
compileBenchmarks shape =
  compileSizes >>= \size ->
    [ bench (caseLabel "compile/order+operations" size) (nf (compileLatticeWeight shape) size),
      bench (caseLabel "presentation/order+operations" size) (nf (compilePresentationWeight shape) size),
      bench (caseLabel "bounded-presentation/order+operations" size) (nf (compileBoundedPresentationWeight shape) size)
    ]

queryBenchmarks :: Shape -> Int -> Benchmark
queryBenchmarks shape size =
  env (compileLatticeEnv shape size) $ \lattice ->
    bgroup
      (caseLabel "compiled query fixture" size)
      (queryBenchmarkCases shape size lattice)

queryBenchmarkCases :: Shape -> Int -> ContextLattice Int -> [Benchmark]
queryBenchmarkCases shape size lattice =
  withResidentContext lattice $ \contextValue ->
    let contextKeys = residentContextKeys contextValue
     in [ bench (caseLabel "key <= sweep" size) (nf (residentLeqSweepWeight contextValue) contextKeys),
          bench (caseLabel "key join/meet sweep" size) (nf (residentJoinMeetKeySweepWeight contextValue) contextKeys),
          bench (caseLabel "join/meet sweep" size) (nf (joinMeetSweepWeight size) lattice),
          bench (caseLabel "least/greatest fixpoint" size) (nf (fixpointPairWeight shape size) lattice)
        ]
          <> heytingQueryBenchmarkCases shape size lattice

heytingQueryBenchmarkCases :: Shape -> Int -> ContextLattice Int -> [Benchmark]
heytingQueryBenchmarkCases shape size lattice =
  case shape of
    Fan -> []
    _ ->
      [ bench (caseLabel "key heyting implication sweep" size) (nf implicationKeySweepWeight lattice),
        bench (caseLabel "heyting implication sweep" size) (nf (implicationSweepWeight size) lattice)
      ]

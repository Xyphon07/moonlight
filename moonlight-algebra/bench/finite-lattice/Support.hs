module Support
  ( supportBenchmarks,
  )
where

import Data.Bifunctor
  ( first,
  )
import Fixtures
  ( Shape,
    caseLabel,
    compileLatticeEnv,
    keys,
    querySizes,
    shapeLabel,
    shapes,
    shiftedWideSupportSeeds,
    supportSeeds,
    topKey,
    wideSupportSeeds,
  )
import Moonlight.FiniteLattice.Core
  ( ContextLattice,
  )
import Moonlight.FiniteLattice.Resident
  ( residentContextElements,
    withResidentContext,
  )
import Moonlight.FiniteLattice.Support
  ( principalSupport,
    residentSupportContainsElement,
    residentSupportFromElements,
    residentSupportKeys,
    residentSupportMeet,
    residentSupportReachableElements,
    residentSupportWithClosure,
    supportBasis,
    supportContains,
    supportGenerators,
    supportMeet,
    supportReachableLatticeContexts,
    supportUnion,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

supportBenchmarks :: Benchmark
supportBenchmarks =
  bgroup
    "support-basis"
    [ bgroup
        (shapeLabel shape)
        (supportBenchmarksForShape shape)
    | shape <- shapes
    ]

supportBenchmarksForShape :: Shape -> [Benchmark]
supportBenchmarksForShape shape =
  fmap (supportBenchmarksForSize shape) querySizes

supportBenchmarksForSize :: Shape -> Int -> Benchmark
supportBenchmarksForSize shape size =
  env (compileLatticeEnv shape size) $ \lattice ->
    bgroup
      (caseLabel "compiled support fixture" size)
      [ bench (caseLabel "normalize generators" size) (nf (supportNormalizeWeight shape size) lattice),
        bench (caseLabel "contains sweep" size) (nf (supportContainsSweepWeight shape size) lattice),
        bench (caseLabel "resident wide contains cached" size) (nf (residentSupportContainsWeight shape size) lattice),
        bench (caseLabel "union/meet" size) (nf (supportUnionMeetWeight shape size) lattice),
        bench (caseLabel "resident wide meet" size) (nf (residentSupportMeetWeight shape size) lattice),
        bench (caseLabel "reachable contexts" size) (nf (supportReachableWeight shape size) lattice),
        bench (caseLabel "resident wide reachable" size) (nf (residentSupportReachableWeight shape size) lattice)
      ]

supportNormalizeWeight :: Shape -> Int -> ContextLattice Int -> Either String Int
supportNormalizeWeight shape size lattice =
  length . supportGenerators <$> first show (supportBasis lattice (supportSeeds shape size))

supportContainsSweepWeight :: Shape -> Int -> ContextLattice Int -> Either String Int
supportContainsSweepWeight shape size lattice = do
  support <- first show (supportBasis lattice (supportSeeds shape size))
  sum . fmap fromEnum
    <$> traverse
      (\candidate -> first show (supportContains lattice support candidate))
      (keys size)

supportUnionMeetWeight :: Shape -> Int -> ContextLattice Int -> Either String Int
supportUnionMeetWeight shape size lattice = do
  leftSupport <- first show (supportBasis lattice (supportSeeds shape size))
  let rightSupport = principalSupport (topKey size)
  unionSupport <- first show (supportUnion lattice leftSupport rightSupport)
  meetSupport <- first show (supportMeet lattice leftSupport rightSupport)
  pure (length (supportGenerators unionSupport) + length (supportGenerators meetSupport))

supportReachableWeight :: Shape -> Int -> ContextLattice Int -> Either String Int
supportReachableWeight shape size lattice = do
  support <- first show (supportBasis lattice (supportSeeds shape size))
  length <$> first show (supportReachableLatticeContexts lattice support)

residentSupportContainsWeight :: Shape -> Int -> ContextLattice Int -> Either String Int
residentSupportContainsWeight shape size lattice =
  withResidentContext lattice $ \contextValue -> do
    support <- first show (residentSupportFromElements contextValue (wideSupportSeeds shape size))
    let cachedSupport = residentSupportWithClosure contextValue support
    pure
      ( length
          [ ()
          | contextElement <- residentContextElements contextValue,
            residentSupportContainsElement contextValue cachedSupport contextElement
          ]
      )

residentSupportReachableWeight :: Shape -> Int -> ContextLattice Int -> Either String Int
residentSupportReachableWeight shape size lattice =
  withResidentContext lattice $ \contextValue -> do
    support <- first show (residentSupportFromElements contextValue (wideSupportSeeds shape size))
    pure (length (residentSupportReachableElements contextValue support))

residentSupportMeetWeight :: Shape -> Int -> ContextLattice Int -> Either String Int
residentSupportMeetWeight shape size lattice =
  withResidentContext lattice $ \contextValue -> do
    leftSupport <- first show (residentSupportFromElements contextValue (wideSupportSeeds shape size))
    rightSupport <- first show (residentSupportFromElements contextValue (shiftedWideSupportSeeds shape size))
    pure
      ( length
          ( residentSupportKeys
              contextValue
              (residentSupportMeet contextValue leftSupport rightSupport)
          )
      )

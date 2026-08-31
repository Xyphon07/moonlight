module SimplicialDelta
  ( deltaBenchmarks,
  )
where

import Data.Function ((&))
import Moonlight.Category.Simplicial
  ( DeltaMorphism,
    allDeltaMorphisms,
    deltaCodomainDimension,
    deltaDomainDimension,
    deltaIdentity,
    deltaMapValues,
    normalInjection,
    normalSurjection,
    normalizeDeltaMorphism,
  )
import Numeric.Natural (Natural)
import SimplicialWeight (naturalListWeight, naturalWeight)
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

deltaBenchmarks :: Benchmark
deltaBenchmarks =
  bgroup
    "operational Delta API"
    [ bgroup
        "allDeltaMorphisms"
        (dimensionPairs & fmap (\dimensions -> bench (dimensionPairLabel dimensions) (nf deltaEnumerationWeight dimensions))),
      bgroup
        "normalizeDeltaMorphism over allDeltaMorphisms"
        (dimensionPairs & fmap (\dimensions -> bench (dimensionPairLabel dimensions) (nf deltaNormalizationWeight dimensions))),
      bgroup
        "normalize identity"
        (identityDimensions & fmap (\dimension -> bench ("dimension=" <> show dimension) (nf normalizeIdentityWeight dimension)))
    ]

type DimensionPair = (Natural, Natural)

dimensionPairs :: [DimensionPair]
dimensionPairs =
  [ (3, 3),
    (4, 4),
    (5, 5),
    (6, 4)
  ]

identityDimensions :: [Natural]
identityDimensions =
  [ 128,
    512,
    2048
  ]

dimensionPairLabel :: DimensionPair -> String
dimensionPairLabel (domainDimension, codomainDimension) =
  "domain=" <> show domainDimension <> " codomain=" <> show codomainDimension

deltaEnumerationWeight :: DimensionPair -> Int
deltaEnumerationWeight (domainDimension, codomainDimension) =
  allDeltaMorphisms domainDimension codomainDimension
    & fmap deltaMorphismWeight
    & sum

deltaNormalizationWeight :: DimensionPair -> Int
deltaNormalizationWeight (domainDimension, codomainDimension) =
  allDeltaMorphisms domainDimension codomainDimension
    & fmap normalizedDeltaWeight
    & sum

normalizeIdentityWeight :: Natural -> Int
normalizeIdentityWeight =
  normalizedDeltaWeight . deltaIdentity

deltaMorphismWeight :: DeltaMorphism -> Int
deltaMorphismWeight morphism =
  naturalWeight (deltaDomainDimension morphism)
    + naturalWeight (deltaCodomainDimension morphism)
    + naturalListWeight (deltaMapValues morphism)

normalizedDeltaWeight :: DeltaMorphism -> Int
normalizedDeltaWeight morphism =
  maybe 0
    (\normalForm -> naturalListWeight (normalSurjection normalForm) + naturalListWeight (normalInjection normalForm))
    (normalizeDeltaMorphism morphism)

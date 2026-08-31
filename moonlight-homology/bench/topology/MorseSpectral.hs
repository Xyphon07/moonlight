{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module MorseSpectral
  ( morseSpectralBenchmarks,
  )
where

import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Homology
  ( BasisCellRef (..),
    BoundaryEntry,
    BoundaryIncidence,
    FiltrationFunction,
    FiniteChainComplex,
    FormalMap (..),
    HomologicalDegree (..),
    HomologyFailure,
    RationalSpectralPage,
    SpectralEntry (..),
    computeRationalSpectralPages,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    entryGroupValue,
    filteredRefinedMorseComplex,
    filteredReducedFiltration,
    frmcRefinedMorseComplex,
    formalCodomainBasis,
    formalDomainBasis,
    formalMatrix,
    freeRank,
    mkBoundaryEntry,
    mkBoundaryIncidence,
    pageDifferentialMap,
    pageEntryMap,
    pageIndex,
    rationalizeFiniteChainComplex,
    rmcReducedComplex,
    torsionInvariants,
  )
import Moonlight.Homology.Boundary.Finite (mkFiniteChainComplex)

type BenchComplex :: Type
data BenchComplex = BenchComplex
  { bcPathComplex :: PathBenchComplex,
    bcReducedRational :: FiniteChainComplex Rational
  }

type PathBenchComplex :: Type
data PathBenchComplex = PathBenchComplex
  { pbcRawIntegral :: FiniteChainComplex Integer,
    pbcRawRational :: FiniteChainComplex Rational
  }

type MeasuredWeight :: Type
data MeasuredWeight
  = MeasuredWeight !Int
  | MeasurementObstruction !String
  deriving stock (Show)

morseSpectralBenchmarks :: Bool -> Benchmark
morseSpectralBenchmarks includeLarge =
  case morseSpectralBenchmarkSuites includeLarge of
    Left failureMessage -> bench "invalid-morse-spectral-fixture" (whnf id failureMessage)
    Right benchmarks -> bgroup "morse-spectral" benchmarks

morseSpectralBenchmarkSuites :: Bool -> Either String [Benchmark]
morseSpectralBenchmarkSuites includeLarge =
  (<>)
    <$> traverse (uncurry unfilteredBenchSuite) (unfilteredPathBenchmarkCases includeLarge)
    <*> traverse (uncurry filteredBenchSuite) (filteredPathBenchmarkCases includeLarge)

unfilteredPathBenchmarkCases :: Bool -> [(String, Int)]
unfilteredPathBenchmarkCases includeLarge =
  [ ("path-16", 16),
    ("path-32", 32),
    ("path-64", 64),
    ("path-256", 256),
    ("path-512", 512)
  ]
    <> [("path-1024", 1024) | includeLarge]

filteredPathBenchmarkCases :: Bool -> [(String, Int)]
filteredPathBenchmarkCases includeLarge =
  [ ("filtered-path-16", 16),
    ("filtered-path-32", 32),
    ("filtered-path-64", 64)
  ]
    <> [("filtered-path-128", 128) | includeLarge]

unfilteredBenchSuite :: String -> Int -> Either String Benchmark
unfilteredBenchSuite label edgeCount =
  fmap
    ( \complexValue ->
        bgroup
          label
          [ bench "raw-unreduced-rational-spectral" (whnf (rawSpectralWeight trivialFiltration) (pbcRawRational (bcPathComplex complexValue))),
            bench "refined-morse-plus-spectral" (whnf (refinedMorseSpectralWeight trivialFiltration) (pbcRawIntegral (bcPathComplex complexValue))),
            bench "reduced-rational-spectral-only" (whnf (rationalSpectralWeight trivialFiltration) (bcReducedRational complexValue))
          ]
    )
    (unfilteredBenchComplex edgeCount)

filteredBenchSuite :: String -> Int -> Either String Benchmark
filteredBenchSuite label edgeCount =
  fmap
    ( \complexValue ->
        bgroup
          label
          [ bench "raw-unreduced-rational-spectral" (whnf (rawSpectralWeight pathFiltration) (pbcRawRational complexValue)),
            bench "refined-morse-plus-spectral" (whnf (refinedMorseSpectralWeight pathFiltration) (pbcRawIntegral complexValue))
          ]
    )
    (filteredBenchComplex edgeCount)

unfilteredBenchComplex :: Int -> Either String BenchComplex
unfilteredBenchComplex edgeCount = do
  pathComplexValue <- pathBenchComplex edgeCount
  reducedComplexValue <- reducedComplex (pbcRawIntegral pathComplexValue)
  _ <- rawSpectralWeightResult trivialFiltration (pbcRawRational pathComplexValue)
  _ <- refinedMorseSpectralWeightResult trivialFiltration (pbcRawIntegral pathComplexValue)
  _ <- rationalSpectralWeightResult trivialFiltration reducedComplexValue
  pure
    BenchComplex
      { bcPathComplex = pathComplexValue,
        bcReducedRational = reducedComplexValue
      }

filteredBenchComplex :: Int -> Either String PathBenchComplex
filteredBenchComplex edgeCount = do
  pathComplexValue <- pathBenchComplex edgeCount
  _ <- rawSpectralWeightResult pathFiltration (pbcRawRational pathComplexValue)
  _ <- refinedMorseSpectralWeightResult pathFiltration (pbcRawIntegral pathComplexValue)
  pure pathComplexValue

pathBenchComplex :: Int -> Either String PathBenchComplex
pathBenchComplex edgeCount = do
  rawComplex <- pathComplex edgeCount
  pure
    PathBenchComplex
      { pbcRawIntegral = rawComplex,
        pbcRawRational = rationalizeFiniteChainComplex rawComplex
      }

pathComplex :: Int -> Either String (FiniteChainComplex Integer)
pathComplex edgeCount
  | edgeCount <= 0 = Left ("edge count must be positive: " <> show edgeCount)
  | otherwise = do
      edgeBoundary <- pathBoundary edgeCount
      pure
        ( mkFiniteChainComplex (HomologicalDegree 1) $ \degreeValue ->
            case degreeValue of
              HomologicalDegree 1 -> edgeBoundary
              HomologicalDegree 0 -> emptyBoundaryIncidenceOf (fromIntegral (edgeCount + 1)) 0
              _ -> emptyBoundaryIncidence
        )

pathBoundary :: Int -> Either String (BoundaryIncidence Integer)
pathBoundary edgeCount =
  case mkBoundaryIncidence (fromIntegral edgeCount) (fromIntegral (edgeCount + 1)) (pathBoundaryEntries edgeCount) of
    Left shapeError -> Left ("invalid path boundary: " <> show shapeError)
    Right incidenceValue -> Right incidenceValue

pathBoundaryEntries :: Int -> [BoundaryEntry Integer]
pathBoundaryEntries edgeCount =
  foldMap edgeBoundaryEntries [0 .. edgeCount - 1]

edgeBoundaryEntries :: Int -> [BoundaryEntry Integer]
edgeBoundaryEntries edgeIndexValue =
  [ boundaryEntry edgeIndexValue edgeIndexValue (-1),
    boundaryEntry edgeIndexValue (edgeIndexValue + 1) 1
  ]

boundaryEntry :: Int -> Int -> coefficient -> BoundaryEntry coefficient
boundaryEntry sourceIndexValue targetIndexValue =
  mkBoundaryEntry (fromIntegral sourceIndexValue) (fromIntegral targetIndexValue)

reducedComplex :: FiniteChainComplex Integer -> Either String (FiniteChainComplex Rational)
reducedComplex rawComplex =
  case filteredRefinedMorseComplex rawComplex trivialFiltration (const 0) of
    Left failureValue -> Left ("filtered refined Morse reduction failed: " <> show failureValue)
    Right filteredValue -> Right (rmcReducedComplex (frmcRefinedMorseComplex filteredValue))

rawSpectralWeight :: FiltrationFunction -> FiniteChainComplex Rational -> MeasuredWeight
rawSpectralWeight filtration finiteComplex =
  measuredWeight (rawSpectralWeightResult filtration finiteComplex)

refinedMorseSpectralWeight :: FiltrationFunction -> FiniteChainComplex Integer -> MeasuredWeight
refinedMorseSpectralWeight filtration finiteComplex =
  measuredWeight (refinedMorseSpectralWeightResult filtration finiteComplex)

rationalSpectralWeight :: FiltrationFunction -> FiniteChainComplex Rational -> MeasuredWeight
rationalSpectralWeight filtration finiteComplex =
  measuredWeight (rationalSpectralWeightResult filtration finiteComplex)

rawSpectralWeightResult :: FiltrationFunction -> FiniteChainComplex Rational -> Either String Int
rawSpectralWeightResult filtration finiteComplex =
  firstHomologyFailure "raw unreduced rational spectral computation failed" $
    fmap spectralPagesWeight (computeRationalSpectralPages finiteComplex filtration)

refinedMorseSpectralWeightResult :: FiltrationFunction -> FiniteChainComplex Integer -> Either String Int
refinedMorseSpectralWeightResult filtration finiteComplex =
  case filteredRefinedMorseComplex finiteComplex filtration (const 0) of
    Left failureValue -> Left ("filtered refined Morse reduction failed: " <> show failureValue)
    Right filteredValue ->
      rationalSpectralWeightResult
        (filteredReducedFiltration filteredValue)
        (rmcReducedComplex (frmcRefinedMorseComplex filteredValue))

rationalSpectralWeightResult :: FiltrationFunction -> FiniteChainComplex Rational -> Either String Int
rationalSpectralWeightResult filtration finiteComplex =
  firstHomologyFailure "rational spectral computation failed" $
    fmap spectralPagesWeight (computeRationalSpectralPages finiteComplex filtration)

firstHomologyFailure :: String -> Either HomologyFailure value -> Either String value
firstHomologyFailure contextMessage =
  either (\failureValue -> Left (contextMessage <> ": " <> show failureValue)) Right

measuredWeight :: Either String Int -> MeasuredWeight
measuredWeight =
  either MeasurementObstruction MeasuredWeight

trivialFiltration :: BasisCellRef -> Int
trivialFiltration = const 0

pathFiltration :: BasisCellRef -> Int
pathFiltration basisCellRef =
  case cellDegree basisCellRef of
    HomologicalDegree 0 -> cellIndex basisCellRef
    HomologicalDegree 1 -> cellIndex basisCellRef + 1
    _ -> 0

spectralPagesWeight :: [RationalSpectralPage] -> Int
spectralPagesWeight =
  foldl' (\weightValue pageValue -> weightValue + spectralPageWeight pageValue) 0

spectralPageWeight :: RationalSpectralPage -> Int
spectralPageWeight pageValue =
  pageIndex pageValue
    + foldl' (\weightValue entryValue -> weightValue + spectralEntryWeight entryValue) 0 (Map.elems (pageEntryMap pageValue))
    + foldl' (\weightValue formalMapValue -> weightValue + formalMapWeight formalMapValue) 0 (Map.elems (pageDifferentialMap pageValue))

spectralEntryWeight :: SpectralEntry Rational -> Int
spectralEntryWeight entryValue =
  let groupValue = entryGroupValue entryValue
   in freeRank groupValue + length (torsionInvariants groupValue)

formalMapWeight :: FormalMap Rational -> Int
formalMapWeight formalMapValue =
  sumMatrixWeight (formalMatrix formalMapValue)
    + length (formalDomainBasis formalMapValue)
    + length (formalCodomainBasis formalMapValue)

sumMatrixWeight :: [[Rational]] -> Int
sumMatrixWeight =
  foldl' (\weightValue rowValue -> weightValue + rowWeight rowValue) 0

rowWeight :: [Rational] -> Int
rowWeight =
  foldl' (\weightValue coefficientValue -> weightValue + coefficientWeight coefficientValue) 0

coefficientWeight :: Rational -> Int
coefficientWeight coefficientValue =
  if coefficientValue == 0
    then 0
    else 1

{-# LANGUAGE DataKinds #-}

module GeometryStatics
  ( geometryStaticsBenchmarks,
    geometryStaticsOnceBenchmarks,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor (first)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Env (BenchmarkSelection (..))
import Fixtures
  ( staticsBenchmarkNetwork,
  )
import Types
  ( BenchmarkSetup (..),
    BenchmarkWeight (..),
    OnceBenchmark,
    PreparedBenchmarkRow (..),
    eitherBenchmarkWeight,
    renderPreparedBenchmark,
    renderPreparedOnceBenchmark,
  )
import Moonlight.LinAlg.Dense
  ( Matrix,
    Vector,
    dynMatrixToList,
    dynVectorToList,
    toListMatrix,
    toListVector,
  )
import Moonlight.LinAlg.Geometry
  ( AABB,
    Symmetric3 (..),
    Vec3 (..),
    aabbRadius,
    crossVec3,
    dotVec3,
    eigendecomposeSymmetric3,
    expandAabb,
    magnitudeVec3,
    normalizeVec3,
    symmetricAabb,
    symmetric3Entries,
    translateAabb,
    unionAabb,
    vec3ToList,
  )
import Moonlight.LinAlg.Statics
  ( CompiledEquilibrium,
    EquilibriumResult (..),
    EquilibriumSolution (..),
    EquilibriumViolation (..),
    ForceNetwork,
    assembleEquilibriumEquations,
    checkEquilibrium,
    compiledCoefficientMatrix,
    compiledNodeOrder,
    compiledRightHandSide,
    compiledUnknownOrder,
  )
import Test.Tasty.Bench (Benchmark, bgroup)
import Prelude

data GeometryStaticsCase = GeometryStaticsCase
  { geometryStaticsLabel :: !String,
    geometryVectors :: ![Vec3],
    geometryBoxes :: ![AABB],
    geometrySymmetricTensors :: ![Symmetric3 Double],
    geometryStaticsNetwork :: !ForceNetwork
  }

instance NFData GeometryStaticsCase where
  rnf benchmarkCase =
    rnf (geometryStaticsLabel benchmarkCase)
      `seq` rnf (vec3ToList =<< geometryVectors benchmarkCase)
      `seq` rnf (aabbRadius <$> geometryBoxes benchmarkCase)
      `seq` rnf (symmetric3Entries =<< geometrySymmetricTensors benchmarkCase)
      `seq` geometryStaticsNetwork benchmarkCase
      `seq` ()

geometryStaticsBenchmarks :: BenchmarkSelection -> Benchmark
geometryStaticsBenchmarks benchmarkSelection =
  bgroup
    "geometry and statics"
    (geometryStaticsBenchmark <$> geometryStaticsSpans benchmarkSelection)

geometryStaticsOnceBenchmarks :: BenchmarkSelection -> [OnceBenchmark]
geometryStaticsOnceBenchmarks benchmarkSelection =
  geometryStaticsOnceBenchmark =<< geometryStaticsSpans benchmarkSelection

geometryStaticsSpans :: BenchmarkSelection -> [Int]
geometryStaticsSpans benchmarkSelection =
  [8]
    <> [24 | includeBroadMedium benchmarkSelection || includeBroadLarge benchmarkSelection]
    <> [64 | includeBroadLarge benchmarkSelection]

geometryStaticsBenchmark :: Int -> Benchmark
geometryStaticsBenchmark spanCount =
  bgroup
    ("spans=" <> show spanCount)
    (renderPreparedBenchmark (prepareGeometryStaticsCase spanCount) <$> geometryStaticsRows)

geometryStaticsOnceBenchmark :: Int -> [OnceBenchmark]
geometryStaticsOnceBenchmark spanCount =
  renderPreparedOnceBenchmark ("geometry and statics.spans=" <> show spanCount <> ".") (prepareGeometryStaticsCase spanCount)
    <$> geometryStaticsRows

geometryStaticsRows :: [PreparedBenchmarkRow GeometryStaticsCase]
geometryStaticsRows =
  [ PurePreparedBenchmarkRow "Vec3 normalize/cross batch" vec3BatchWeight,
    PurePreparedBenchmarkRow "AABB union/translate batch" aabbBatchWeight,
    PurePreparedBenchmarkRow "Symmetric3 eigendecompose batch" symmetric3EigenBatchWeight,
    PurePreparedBenchmarkRow "assemble equilibrium" staticsAssembleWeight,
    PurePreparedBenchmarkRow "check equilibrium" staticsSolveWeight
  ]

prepareGeometryStaticsCase :: Int -> BenchmarkSetup GeometryStaticsCase
prepareGeometryStaticsCase spanCount =
  BenchmarkSetup $ do
    networkValue <- first (("statics fixture failed: " <>)) (staticsBenchmarkNetwork spanCount)
    boxes <- boxFixture (spanCount * 16)
    pure
      GeometryStaticsCase
        { geometryStaticsLabel = "spans=" <> show spanCount,
          geometryVectors = vectorFixture (spanCount * 32),
          geometryBoxes = boxes,
          geometrySymmetricTensors = symmetric3Fixture (spanCount * 16),
          geometryStaticsNetwork = networkValue
        }

vectorFixture :: Int -> [Vec3]
vectorFixture count =
  [ Vec3
      (1.0 + fromIntegral (indexValue `mod` 13))
      (2.0 + fromIntegral (indexValue `mod` 17) / 2.0)
      (3.0 + fromIntegral (indexValue `mod` 19) / 3.0)
    | indexValue <- [0 .. count - 1]
  ]

boxFixture :: Int -> Either String [AABB]
boxFixture count =
  traverse boxAt [0 .. count - 1]
  where
    boxAt :: Int -> Either String AABB
    boxAt indexValue =
      let translationVector =
            Vec3
              (fromIntegral indexValue * 0.01)
              (fromIntegral (indexValue `mod` 11) * 0.02)
              0.0
          halfY = 1.0 + fromIntegral (indexValue `mod` 5) * 0.1
       in maybe
            (Left "AABB fixture half-extents violated constructor contract")
            (Right . translateAabb translationVector)
            (symmetricAabb 1.0 halfY 1.5)

symmetric3Fixture :: Int -> [Symmetric3 Double]
symmetric3Fixture count =
  tensorAt <$> [0 .. count - 1]
  where
    tensorAt :: Int -> Symmetric3 Double
    tensorAt indexValue =
      let xValue = 1.0 + fromIntegral (indexValue `mod` 7) * 0.125
          yValue = 2.0 + fromIntegral (indexValue `mod` 11) * 0.0625
          zValue = 3.0 + fromIntegral (indexValue `mod` 13) * 0.03125
          couplingScale = fromIntegral (indexValue `mod` 5) * 0.01
       in Symmetric3
            { sym3XX = xValue,
              sym3XY = couplingScale,
              sym3XZ = -0.5 * couplingScale,
              sym3YY = yValue,
              sym3YZ = 0.25 * couplingScale,
              sym3ZZ = zValue
            }

vec3BatchWeight :: GeometryStaticsCase -> BenchmarkWeight
vec3BatchWeight benchmarkCase =
  eitherBenchmarkWeight
    (geometryStaticsLabel benchmarkCase <> " Vec3 normalize/cross batch")
    vectorBatchChecksum
    ( do
        normalized <- traverse normalizeVec3 (geometryVectors benchmarkCase)
        pure (zipWith crossVec3 normalized (drop 1 normalized), normalized)
    )

aabbBatchWeight :: GeometryStaticsCase -> BenchmarkWeight
aabbBatchWeight benchmarkCase =
  eitherBenchmarkWeight
    (geometryStaticsLabel benchmarkCase <> " AABB union/translate batch")
    aabbRadius
    ( do
        seedBox <- maybeToEither "seed AABB half-extents violated constructor contract" (symmetricAabb 1.0 1.0 1.0)
        expandedBoxes <- traverse (maybeToEither "AABB expansion inverted a box" . expandAabb 0.05) (geometryBoxes benchmarkCase)
        pure (foldr unionAabb seedBox expandedBoxes)
    )

symmetric3EigenBatchWeight :: GeometryStaticsCase -> BenchmarkWeight
symmetric3EigenBatchWeight benchmarkCase =
  eitherBenchmarkWeight
    (geometryStaticsLabel benchmarkCase <> " Symmetric3 eigendecompose batch")
    symmetric3EigenBatchChecksum
    (traverse eigendecomposeSymmetric3 (geometrySymmetricTensors benchmarkCase))

staticsAssembleWeight :: GeometryStaticsCase -> BenchmarkWeight
staticsAssembleWeight benchmarkCase =
  eitherBenchmarkWeight
    (geometryStaticsLabel benchmarkCase <> " assemble equilibrium")
    compiledEquilibriumChecksum
    (assembleEquilibriumEquations (geometryStaticsNetwork benchmarkCase))

staticsSolveWeight :: GeometryStaticsCase -> BenchmarkWeight
staticsSolveWeight benchmarkCase =
  eitherBenchmarkWeight
    (geometryStaticsLabel benchmarkCase <> " check equilibrium")
    equilibriumResultChecksum
    (checkEquilibrium (geometryStaticsNetwork benchmarkCase))

vectorBatchChecksum :: ([Vec3], [Vec3]) -> Double
vectorBatchChecksum (crossVectors, normalizedVectors) =
  sum (magnitudeVec3 <$> crossVectors)
    + sum (zipWith dotVec3 normalizedVectors (drop 1 normalizedVectors))

symmetric3EigenBatchChecksum :: [(Vector 3 Double, Matrix 3 3 Double)] -> Double
symmetric3EigenBatchChecksum =
  sum . fmap symmetric3EigenChecksum

symmetric3EigenChecksum :: (Vector 3 Double, Matrix 3 3 Double) -> Double
symmetric3EigenChecksum (eigenvalues, eigenvectors) =
  sum (abs <$> toListVector eigenvalues) + sum (abs <$> toListMatrix eigenvectors)

compiledEquilibriumChecksum :: CompiledEquilibrium -> Double
compiledEquilibriumChecksum compiledValue =
  fromIntegral (length (compiledNodeOrder compiledValue))
    + fromIntegral (length (compiledUnknownOrder compiledValue))
    + sum (abs <$> dynMatrixToList (compiledCoefficientMatrix compiledValue))
    + sum (abs <$> dynVectorToList (compiledRightHandSide compiledValue))

equilibriumResultChecksum :: EquilibriumResult -> Double
equilibriumResultChecksum resultValue =
  case resultValue of
    InEquilibrium solutionValue -> equilibriumSolutionChecksum solutionValue
    Disequilibrium violations -> sum (violationResidualMagnitude <$> NonEmpty.toList violations)

equilibriumSolutionChecksum :: EquilibriumSolution -> Double
equilibriumSolutionChecksum solutionValue =
  fromIntegral (Map.size (equilibriumMemberForces solutionValue))
    + fromIntegral (Map.size (equilibriumReactionForces solutionValue))
    + fromIntegral (Map.size (equilibriumResidualForces solutionValue))
    + sum (abs <$> Map.elems (equilibriumMemberForces solutionValue))

maybeToEither :: err -> Maybe value -> Either err value
maybeToEither failureValue =
  maybe (Left failureValue) Right

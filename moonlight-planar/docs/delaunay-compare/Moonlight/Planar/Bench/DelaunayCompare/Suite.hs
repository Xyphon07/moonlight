{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Fixture descent, compatibility gluing, and the authoritative benchmark view.
module Moonlight.Planar.Bench.DelaunayCompare.Suite
  ( PreparedSuite
  , withPreparedSuite
  , preflightSuite
  , suiteBenchmarks
  , suiteAgreementMessage
  ) where

import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Data.Bifunctor (first)
import Data.Either (lefts, rights)
import Data.Foldable (toList, traverse_)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import Moonlight.Planar.Bench.DelaunayCompare.Domain
import Moonlight.Planar.Bench.DelaunayCompare.Native
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.Dcel (numInnerFaces, numVertices)
import Moonlight.Planar.Types (BuildError)
import Moonlight.Planar.Point (Point (Point))
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf, nfIO)

data NativeSections value = NativeSections
  { nativeSpadeSection :: !value
  , nativeSpadeHierarchySection :: !value
  , nativeCdtSection :: !value
  , nativeDelaunatorSection :: !value
  }
  deriving stock (Functor, Foldable, Traversable)

nativeImplementationSections :: NativeSections NativeImplementation
nativeImplementationSections =
  NativeSections
    { nativeSpadeSection = NativeSpade
    , nativeSpadeHierarchySection = NativeSpadeHierarchy
    , nativeCdtSection = NativeCdt
    , nativeDelaunatorSection = NativeDelaunator
    }

nativeSectionAt :: NativeImplementation -> NativeSections value -> value
nativeSectionAt implementation sections =
  case implementation of
    NativeSpade -> nativeSpadeSection sections
    NativeSpadeHierarchy -> nativeSpadeHierarchySection sections
    NativeCdt -> nativeCdtSection sections
    NativeDelaunator -> nativeDelaunatorSection sections

data PreparedFixture = PreparedFixture
  { preparedSpec :: !FixtureSpec
  , preparedMoonlightPoints :: !(Vector.Vector Point)
  , preparedNativeFixtures :: !(NativeSections NativeFixture)
  }

newtype PreparedSuite = PreparedSuite [PreparedFixture]

withPreparedSuite
  :: NativeApi
  -> (PreparedSuite -> IO (Either CompareObstruction result))
  -> IO (Either CompareObstruction result)
withPreparedSuite api action = do
  prepared <- prepareSuite api
  case prepared of
    Left obstruction -> pure (Left obstruction)
    Right suite -> bracket (pure suite) (releaseSuite api) action

prepareSuite :: NativeApi -> IO (Either CompareObstruction PreparedSuite)
prepareSuite api = do
  outcomes <- traverse (prepareFixture api) allFixtureSpecs
  case NonEmpty.nonEmpty (lefts outcomes) of
    Just obstructions -> do
      traverse_ (releasePreparedFixture api) (rights outcomes)
      pure (Left (ComparisonObstructions obstructions))
    Nothing -> pure (Right (PreparedSuite (rights outcomes)))

prepareFixture :: NativeApi -> FixtureSpec -> IO (Either CompareObstruction PreparedFixture)
prepareFixture api fixture = do
  generated <- generateCoordinates api fixture
  case generated of
    Left status -> pure (Left (FixtureGenerationFailed fixture status))
    Right coordinates -> do
      nativeFixtures <- prepareNativeFixtures api fixture coordinates
      pure $
        PreparedFixture
          fixture
          (Vector.map (uncurry Point) (generatedCoordinatePairs coordinates))
          <$> nativeFixtures

prepareNativeFixtures
  :: NativeApi
  -> FixtureSpec
  -> GeneratedCoordinates
  -> IO (Either CompareObstruction (NativeSections NativeFixture))
prepareNativeFixtures api fixture coordinates = do
  outcomes <- traverse prepareSection nativeImplementationSections
  case sequenceA outcomes of
    Right nativeFixtures -> pure (Right nativeFixtures)
    Left obstruction -> do
      traverse_ (releaseNativeFixture api) (rights (toList outcomes))
      pure (Left obstruction)
 where
  prepareSection implementation = do
    prepared <- prepareNativeFixture api implementation coordinates
    pure $
      first
        (preparationObstruction implementation)
        prepared

  preparationObstruction implementation failure =
    case failure of
      NativePreparationStatus status -> FixturePreparationFailed fixture implementation status
      NativePreparationReturnedNull -> NativePreparedNull fixture implementation

releaseSuite :: NativeApi -> PreparedSuite -> IO ()
releaseSuite api (PreparedSuite fixtures) =
  traverse_ (releasePreparedFixture api) fixtures

releasePreparedFixture :: NativeApi -> PreparedFixture -> IO ()
releasePreparedFixture api = releasePreparedNativeFixtures api . preparedNativeFixtures

releasePreparedNativeFixtures :: NativeApi -> NativeSections NativeFixture -> IO ()
releasePreparedNativeFixtures api = traverse_ (releaseNativeFixture api)

preflightSuite :: NativeApi -> PreparedSuite -> IO (Either CompareObstruction ())
preflightSuite api (PreparedSuite fixtures) = do
  outcomes <- traverse (preflightFixture api) fixtures
  pure $
    case NonEmpty.nonEmpty (lefts outcomes) of
      Just obstructions -> Left (ComparisonObstructions obstructions)
      Nothing -> Right ()

preflightFixture :: NativeApi -> PreparedFixture -> IO (Either CompareObstruction ())
preflightFixture api fixture = do
  moonlightResult <-
    evaluate
      ( force
          (moonlightConstructionSummary (preparedMoonlightPoints fixture))
      )
  nativeOutcomes <- traverse runSection nativeImplementationSections
  pure $ do
    moonlightSummary <-
      first
        (MoonlightConstructionFailed spec)
        moonlightResult
    nativeSummaries <- sequenceA nativeOutcomes
    traverse_
      ( \implementation ->
          requireSummaryAgreement
            spec
            moonlightSummary
            ( implementationOfNative implementation
            , nativeSectionAt implementation nativeSummaries
            )
      )
      nativeImplementationSections
 where
  spec = preparedSpec fixture
  nativeFixtures = preparedNativeFixtures fixture
  runSection implementation =
    first (NativeConstructionFailed spec implementation)
      <$> runNativeFixture api (nativeSectionAt implementation nativeFixtures)

requireSummaryAgreement
  :: FixtureSpec
  -> DelaunaySummary
  -> (Implementation, DelaunaySummary)
  -> Either CompareObstruction ()
requireSummaryAgreement fixture expected (implementation, observed) =
  if observed == expected
    then Right ()
    else Left (SummaryDisagreement fixture implementation expected observed)

moonlightConstructionSummary
  :: Vector.Vector Point
  -> Either BuildError DelaunaySummary
moonlightConstructionSummary points = do
  triangulation <- delaunayGeometry points
  pure
    DelaunaySummary
      { summaryVertexCount = numVertices triangulation
      , summaryTriangleCount = numInnerFaces triangulation
      }

suiteBenchmarks :: NativeApi -> PreparedSuite -> [Benchmark]
suiteBenchmarks api (PreparedSuite fixtures) =
  fmap (sizeBandBenchmarks api fixtures) allSizeBands

sizeBandBenchmarks
  :: NativeApi
  -> [PreparedFixture]
  -> SizeBand
  -> Benchmark
sizeBandBenchmarks api fixtures sizeBand =
  bgroup
    ("comparison: creation benchmark (" <> sizeBandLabel sizeBand <> ")")
    (fmap (implementationBenchmarks api fixtures sizeBand) allImplementations)

implementationBenchmarks
  :: NativeApi
  -> [PreparedFixture]
  -> SizeBand
  -> Implementation
  -> Benchmark
implementationBenchmarks api fixtures sizeBand implementation =
  bgroup
    (implementationLabel implementation)
    ( fmap
        (distributionBenchmarks api fixtures sizeBand implementation)
        allPointDistributions
    )

distributionBenchmarks
  :: NativeApi
  -> [PreparedFixture]
  -> SizeBand
  -> Implementation
  -> PointDistribution
  -> Benchmark
distributionBenchmarks api fixtures sizeBand implementation distribution =
  bgroup
    (pointDistributionLabel distribution)
    ( fmap
        (fixtureBenchmark api implementation)
        ( filter
            (matchesFixture sizeBand distribution . preparedSpec)
            fixtures
        )
    )

matchesFixture :: SizeBand -> PointDistribution -> FixtureSpec -> Bool
matchesFixture sizeBand distribution fixture =
  fixtureSizeBand fixture == sizeBand
    && fixturePointDistribution fixture == distribution

fixtureBenchmark :: NativeApi -> Implementation -> PreparedFixture -> Benchmark
fixtureBenchmark api implementation fixture =
  bench (show (pointCountValue (fixturePointCount (preparedSpec fixture)))) $
    case implementation of
      Moonlight -> nf moonlightConstructionSummary (preparedMoonlightPoints fixture)
      Spade -> nativeBenchmark NativeSpade
      SpadeHierarchy -> nativeBenchmark NativeSpadeHierarchy
      Cdt -> nativeBenchmark NativeCdt
      Delaunator -> nativeBenchmark NativeDelaunator
 where
  nativeFixtures = preparedNativeFixtures fixture
  nativeBenchmark nativeImplementation =
    nfIO (runNativeFixture api (nativeSectionAt nativeImplementation nativeFixtures))

suiteAgreementMessage :: String
suiteAgreementMessage =
  "delaunay-compare agreement: "
    <> show (length allFixtureSpecs)
    <> " fixtures agree on vertex and triangle counts across "
    <> show (length allImplementations)
    <> " implementations"

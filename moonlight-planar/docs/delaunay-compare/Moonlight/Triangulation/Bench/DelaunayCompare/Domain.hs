{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

-- | The closed case matrix and typed obstruction surface for the comparison.
module Moonlight.Triangulation.Bench.DelaunayCompare.Domain
  ( SizeBand (..)
  , allSizeBands
  , sizeBandLabel
  , PointDistribution (..)
  , allPointDistributions
  , pointDistributionLabel
  , Implementation (..)
  , allImplementations
  , implementationLabel
  , NativeImplementation (..)
  , nativeImplementationLabel
  , implementationOfNative
  , PointCount
  , pointCountValue
  , pointCountsFor
  , FixtureSpec (..)
  , allFixtureSpecs
  , DelaunaySummary (..)
  , NativeStatus (..)
  , NativePreparationFailure (..)
  , NativeRunFailure (..)
  , CompareObstruction (..)
  , renderCompareObstruction
  ) where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Generics (Generic)
import Moonlight.Triangulation.Types (BuildError)

data SizeBand
  = Small
  | Big
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

allSizeBands :: [SizeBand]
allSizeBands = [Small, Big]

sizeBandLabel :: SizeBand -> String
sizeBandLabel = \case
  Small -> "small"
  Big -> "big"

data PointDistribution
  = LocalInsertion
  | Uniform
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

allPointDistributions :: [PointDistribution]
allPointDistributions = [LocalInsertion, Uniform]

pointDistributionLabel :: PointDistribution -> String
pointDistributionLabel = \case
  LocalInsertion -> "local insertion"
  Uniform -> "uniform"

data Implementation
  = Spade
  | SpadeHierarchy
  | Cdt
  | Delaunator
  | Moonlight
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Upstream order, with Moonlight appended rather than interposed.
allImplementations :: [Implementation]
allImplementations = [Spade, SpadeHierarchy, Cdt, Delaunator, Moonlight]

implementationLabel :: Implementation -> String
implementationLabel = \case
  Spade -> "spade 2"
  SpadeHierarchy -> "spade 2 hierarchy"
  Cdt -> "cdt"
  Delaunator -> "delaunator"
  Moonlight -> "moonlight-triangulation"

data NativeImplementation
  = NativeSpade
  | NativeSpadeHierarchy
  | NativeCdt
  | NativeDelaunator
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

nativeImplementationLabel :: NativeImplementation -> String
nativeImplementationLabel = implementationLabel . implementationOfNative

implementationOfNative :: NativeImplementation -> Implementation
implementationOfNative = \case
  NativeSpade -> Spade
  NativeSpadeHierarchy -> SpadeHierarchy
  NativeCdt -> Cdt
  NativeDelaunator -> Delaunator

newtype PointCount = PointCount Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

pointCountValue :: PointCount -> Int
pointCountValue (PointCount count) = count

data FixtureSpec = FixtureSpec
  { fixtureSizeBand :: !SizeBand
  , fixturePointDistribution :: !PointDistribution
  , fixturePointCount :: !PointCount
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

allFixtureSpecs :: [FixtureSpec]
allFixtureSpecs =
  concatMap
    ( \sizeBand ->
        liftA2
          (FixtureSpec sizeBand)
          allPointDistributions
          (pointCountsFor sizeBand)
    )
    allSizeBands

pointCountsFor :: SizeBand -> [PointCount]
pointCountsFor = \case
  Small -> PointCount <$> [2_000, 4_000, 6_000, 8_000, 10_000, 12_000, 14_000]
  Big -> PointCount <$> [50_000, 100_000, 150_000, 200_000, 250_000]

data DelaunaySummary = DelaunaySummary
  { summaryVertexCount :: !Int
  , summaryTriangleCount :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data NativeStatus
  = NativeNullPointer
  | NativeUnknownTag
  | NativeCoordinateCountMismatch
  | NativeDistributionConstructionFailed
  | NativeTriangulationFailed
  | NativePanicked
  | NativeUnknownStatus !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data NativePreparationFailure
  = NativePreparationStatus !NativeStatus
  | NativePreparationReturnedNull
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data NativeRunFailure
  = NativeRunStatus !NativeStatus
  | NativeCountExceedsHaskellInt !Integer
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data CompareObstruction
  = RustManifestNotFound !(NonEmpty FilePath)
  | CargoInvocationFailed !FilePath !String
  | CargoBuildFailed !FilePath !Int !String
  | UnsupportedDynamicLibraryHost !String
  | DynamicLibraryOpenFailed !FilePath !String
  | DynamicSymbolLoadFailed !FilePath !String !String
  | FixtureGenerationFailed !FixtureSpec !NativeStatus
  | FixturePreparationFailed !FixtureSpec !NativeImplementation !NativeStatus
  | NativePreparedNull !FixtureSpec !NativeImplementation
  | MoonlightConstructionFailed !FixtureSpec !BuildError
  | NativeConstructionFailed !FixtureSpec !NativeImplementation !NativeRunFailure
  | SummaryDisagreement !FixtureSpec !Implementation !DelaunaySummary !DelaunaySummary
  | ComparisonObstructions !(NonEmpty CompareObstruction)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

renderCompareObstruction :: CompareObstruction -> String
renderCompareObstruction = \case
  RustManifestNotFound candidates ->
    "Rust referent manifest not found; checked " <> show (NonEmpty.toList candidates)
  CargoInvocationFailed manifest details ->
    "could not invoke Cargo for " <> manifest <> ": " <> details
  CargoBuildFailed manifest exitCode details ->
    "Cargo failed for "
      <> manifest
      <> " with exit code "
      <> show exitCode
      <> if null details then "" else ":\n" <> details
  UnsupportedDynamicLibraryHost host ->
    "delaunay-compare does not know the dynamic-library suffix for host " <> show host
  DynamicLibraryOpenFailed path details ->
    "could not load Rust referent library " <> path <> ": " <> details
  DynamicSymbolLoadFailed path symbol details ->
    "could not load symbol " <> symbol <> " from " <> path <> ": " <> details
  FixtureGenerationFailed fixture status ->
    "upstream fixture generation failed for " <> renderFixture fixture <> ": " <> show status
  FixturePreparationFailed fixture implementation status ->
    "native preparation failed for "
      <> nativeImplementationLabel implementation
      <> " on "
      <> renderFixture fixture
      <> ": "
      <> show status
  NativePreparedNull fixture implementation ->
    "native preparation returned a null handle for "
      <> nativeImplementationLabel implementation
      <> " on "
      <> renderFixture fixture
  MoonlightConstructionFailed fixture buildError ->
    "Moonlight construction failed for " <> renderFixture fixture <> ": " <> show buildError
  NativeConstructionFailed fixture implementation failure ->
    "native construction failed for "
      <> nativeImplementationLabel implementation
      <> " on "
      <> renderFixture fixture
      <> ": "
      <> show failure
  SummaryDisagreement fixture implementation expected observed ->
    "construction summary disagreement for "
      <> implementationLabel implementation
      <> " on "
      <> renderFixture fixture
      <> "; Moonlight="
      <> show expected
      <> ", observed="
      <> show observed
  ComparisonObstructions obstructions ->
    unlines ("delaunay comparison obstructed:" : fmap (("  - " <>) . renderCompareObstruction) (NonEmpty.toList obstructions))

renderFixture :: FixtureSpec -> String
renderFixture fixture =
  sizeBandLabel (fixtureSizeBand fixture)
    <> "/"
    <> pointDistributionLabel (fixturePointDistribution fixture)
    <> "/"
    <> show (pointCountValue (fixturePointCount fixture))

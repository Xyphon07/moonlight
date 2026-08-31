{-# LANGUAGE DerivingStrategies #-}

{-| Validated counters, durations, rates, and replay diagnostics. -}
module Moonlight.Pale.Diagnostic.Local.Replay
  ( RateNonFiniteValue (..),
    ReplayDiagnosticsValidationError (..),
    NonNegativeCount,
    nonNegativeCountFromNatural,
    mkNonNegativeCount,
    nonNegativeCountValue,
    zeroNonNegativeCount,
    addNonNegativeCount,
    diffNonNegativeCount,
    Nanoseconds,
    nanosecondsFromNatural,
    mkNanoseconds,
    nanosecondsValue,
    zeroNanoseconds,
    addNanoseconds,
    diffNanoseconds,
    Rate,
    mkRate,
    rateValue,
    rateFromCounts,
    ReplayDiagnostics (..),
    liftReplayDiagnostics2,
    diffReplayDiagnostics,
    replayTotalRequests,
    replayCacheHitRate,
    replayIncrementalRate,
    replayFallbackRate,
    replayExactCoverageRate,
  )
where

import Data.Kind (Type)
import Numeric.Natural (Natural)
import Prelude
  ( Applicative ((<*>)),
    Double,
    Either (Left, Right),
    Eq ((==)),
    Int,
    Monoid (mempty),
    Ord ((<), (>)),
    Semigroup ((<>)),
    Show,
    fromIntegral,
    fromRational,
    isInfinite,
    isNaN,
    otherwise,
    toRational,
    (+),
    (-),
    (/),
    (<$>),
  )

type RateNonFiniteValue :: Type
data RateNonFiniteValue
  = RateNaN
  | RateInfinite
  deriving stock (Eq, Show)

type ReplayDiagnosticsValidationError :: Type
data ReplayDiagnosticsValidationError
  = NegativeCount Int
  | CountDifferenceUnderflow NonNegativeCount NonNegativeCount
  | NegativeNanoseconds Int
  | NanosecondsDifferenceUnderflow Nanoseconds Nanoseconds
  | NonFiniteRate RateNonFiniteValue
  | RateOutOfBounds Double
  | RateNumeratorExceedsDenominator NonNegativeCount NonNegativeCount
  | RateDenominatorZero
  deriving stock (Eq, Show)

type NonNegativeCount :: Type
newtype NonNegativeCount = NonNegativeCount Natural
  deriving stock (Eq, Ord, Show)

nonNegativeCountFromNatural :: Natural -> NonNegativeCount
nonNegativeCountFromNatural =
  NonNegativeCount

mkNonNegativeCount :: Int -> Either ReplayDiagnosticsValidationError NonNegativeCount
mkNonNegativeCount value
  | value < 0 = Left (NegativeCount value)
  | otherwise = Right (NonNegativeCount (fromIntegral value))

nonNegativeCountValue :: NonNegativeCount -> Natural
nonNegativeCountValue (NonNegativeCount value) =
  value

zeroNonNegativeCount :: NonNegativeCount
zeroNonNegativeCount =
  NonNegativeCount 0

addNonNegativeCount :: NonNegativeCount -> NonNegativeCount -> NonNegativeCount
addNonNegativeCount (NonNegativeCount leftValue) (NonNegativeCount rightValue) =
  NonNegativeCount (leftValue + rightValue)

diffNonNegativeCount ::
  NonNegativeCount ->
  NonNegativeCount ->
  Either ReplayDiagnosticsValidationError NonNegativeCount
diffNonNegativeCount leftCount@(NonNegativeCount leftValue) rightCount@(NonNegativeCount rightValue)
  | leftValue < rightValue = Left (CountDifferenceUnderflow leftCount rightCount)
  | otherwise = Right (NonNegativeCount (leftValue - rightValue))

type Nanoseconds :: Type
newtype Nanoseconds = Nanoseconds Natural
  deriving stock (Eq, Ord, Show)

nanosecondsFromNatural :: Natural -> Nanoseconds
nanosecondsFromNatural =
  Nanoseconds

mkNanoseconds :: Int -> Either ReplayDiagnosticsValidationError Nanoseconds
mkNanoseconds value
  | value < 0 = Left (NegativeNanoseconds value)
  | otherwise = Right (Nanoseconds (fromIntegral value))

nanosecondsValue :: Nanoseconds -> Natural
nanosecondsValue (Nanoseconds value) =
  value

zeroNanoseconds :: Nanoseconds
zeroNanoseconds =
  Nanoseconds 0

addNanoseconds :: Nanoseconds -> Nanoseconds -> Nanoseconds
addNanoseconds (Nanoseconds leftValue) (Nanoseconds rightValue) =
  Nanoseconds (leftValue + rightValue)

diffNanoseconds ::
  Nanoseconds ->
  Nanoseconds ->
  Either ReplayDiagnosticsValidationError Nanoseconds
diffNanoseconds leftNanoseconds@(Nanoseconds leftValue) rightNanoseconds@(Nanoseconds rightValue)
  | leftValue < rightValue = Left (NanosecondsDifferenceUnderflow leftNanoseconds rightNanoseconds)
  | otherwise = Right (Nanoseconds (leftValue - rightValue))

type Rate :: Type
newtype Rate = Rate Double
  deriving stock (Eq, Ord, Show)

mkRate :: Double -> Either ReplayDiagnosticsValidationError Rate
mkRate value
  | isNaN value = Left (NonFiniteRate RateNaN)
  | isInfinite value = Left (NonFiniteRate RateInfinite)
  | value < 0 = Left (RateOutOfBounds value)
  | value > 1 = Left (RateOutOfBounds value)
  | value == 0 = Right (Rate 0)
  | otherwise = Right (Rate value)

rateValue :: Rate -> Double
rateValue (Rate value) =
  value

rateFromCounts ::
  NonNegativeCount ->
  NonNegativeCount ->
  Either ReplayDiagnosticsValidationError Rate
rateFromCounts numeratorCount denominatorCount =
  case denominatorCount of
    NonNegativeCount 0 ->
      Left RateDenominatorZero
    NonNegativeCount denominatorValue ->
      case numeratorCount of
        NonNegativeCount numeratorValue
          | numeratorValue > denominatorValue ->
              Left (RateNumeratorExceedsDenominator numeratorCount denominatorCount)
          | otherwise ->
              mkRate (fromRational (toRational numeratorValue / toRational denominatorValue))

type ReplayDiagnostics :: Type
data ReplayDiagnostics = ReplayDiagnostics
  { rdRequestCacheHits :: !NonNegativeCount,
    rdRequestCacheMisses :: !NonNegativeCount,
    rdFullReplayQueries :: !NonNegativeCount,
    rdIncrementalReplayQueries :: !NonNegativeCount,
    rdFrontierSeedCount :: !NonNegativeCount,
    rdMaterializedRegionCount :: !NonNegativeCount,
    rdAffectedRootCount :: !NonNegativeCount,
    rdReusedRootCount :: !NonNegativeCount,
    rdExactFeasibleRootCount :: !NonNegativeCount,
    rdExactInfeasibleRootCount :: !NonNegativeCount,
    rdObstructedRootCount :: !NonNegativeCount,
    rdFallbackAttemptedRootCount :: !NonNegativeCount,
    rdFallbackHitRootCount :: !NonNegativeCount,
    rdRegionEnumerationNanoseconds :: !Nanoseconds,
    rdRegionAnalysisNanoseconds :: !Nanoseconds,
    rdFallbackMatchingNanoseconds :: !Nanoseconds,
    rdDatabaseConstructionNanoseconds :: !Nanoseconds,
    rdSeedsAfterPruningGates :: !NonNegativeCount,
    rdSeedsAfterFrontierFilter :: !NonNegativeCount,
    rdSeedsAfterMaterialization :: !NonNegativeCount,
    rdSeedsPassingMicrosupport :: !NonNegativeCount,
    rdSeedsPassingContext :: !NonNegativeCount,
    rdSeedsPassingSpectral :: !NonNegativeCount,
    rdSeedsPassingLaplacian :: !NonNegativeCount
  }
  deriving stock (Eq, Show)

liftReplayDiagnostics2 ::
  (NonNegativeCount -> NonNegativeCount -> NonNegativeCount) ->
  (Nanoseconds -> Nanoseconds -> Nanoseconds) ->
  ReplayDiagnostics ->
  ReplayDiagnostics ->
  ReplayDiagnostics
liftReplayDiagnostics2 countFunction nanosecondsFunction a b =
  ReplayDiagnostics
    { rdRequestCacheHits = countFunction (rdRequestCacheHits a) (rdRequestCacheHits b),
      rdRequestCacheMisses = countFunction (rdRequestCacheMisses a) (rdRequestCacheMisses b),
      rdFullReplayQueries = countFunction (rdFullReplayQueries a) (rdFullReplayQueries b),
      rdIncrementalReplayQueries = countFunction (rdIncrementalReplayQueries a) (rdIncrementalReplayQueries b),
      rdFrontierSeedCount = countFunction (rdFrontierSeedCount a) (rdFrontierSeedCount b),
      rdMaterializedRegionCount = countFunction (rdMaterializedRegionCount a) (rdMaterializedRegionCount b),
      rdAffectedRootCount = countFunction (rdAffectedRootCount a) (rdAffectedRootCount b),
      rdReusedRootCount = countFunction (rdReusedRootCount a) (rdReusedRootCount b),
      rdExactFeasibleRootCount = countFunction (rdExactFeasibleRootCount a) (rdExactFeasibleRootCount b),
      rdExactInfeasibleRootCount = countFunction (rdExactInfeasibleRootCount a) (rdExactInfeasibleRootCount b),
      rdObstructedRootCount = countFunction (rdObstructedRootCount a) (rdObstructedRootCount b),
      rdFallbackAttemptedRootCount = countFunction (rdFallbackAttemptedRootCount a) (rdFallbackAttemptedRootCount b),
      rdFallbackHitRootCount = countFunction (rdFallbackHitRootCount a) (rdFallbackHitRootCount b),
      rdRegionEnumerationNanoseconds = nanosecondsFunction (rdRegionEnumerationNanoseconds a) (rdRegionEnumerationNanoseconds b),
      rdRegionAnalysisNanoseconds = nanosecondsFunction (rdRegionAnalysisNanoseconds a) (rdRegionAnalysisNanoseconds b),
      rdFallbackMatchingNanoseconds = nanosecondsFunction (rdFallbackMatchingNanoseconds a) (rdFallbackMatchingNanoseconds b),
      rdDatabaseConstructionNanoseconds = nanosecondsFunction (rdDatabaseConstructionNanoseconds a) (rdDatabaseConstructionNanoseconds b),
      rdSeedsAfterPruningGates = countFunction (rdSeedsAfterPruningGates a) (rdSeedsAfterPruningGates b),
      rdSeedsAfterFrontierFilter = countFunction (rdSeedsAfterFrontierFilter a) (rdSeedsAfterFrontierFilter b),
      rdSeedsAfterMaterialization = countFunction (rdSeedsAfterMaterialization a) (rdSeedsAfterMaterialization b),
      rdSeedsPassingMicrosupport = countFunction (rdSeedsPassingMicrosupport a) (rdSeedsPassingMicrosupport b),
      rdSeedsPassingContext = countFunction (rdSeedsPassingContext a) (rdSeedsPassingContext b),
      rdSeedsPassingSpectral = countFunction (rdSeedsPassingSpectral a) (rdSeedsPassingSpectral b),
      rdSeedsPassingLaplacian = countFunction (rdSeedsPassingLaplacian a) (rdSeedsPassingLaplacian b)
    }

instance Semigroup ReplayDiagnostics where
  (<>) = liftReplayDiagnostics2 addNonNegativeCount addNanoseconds

instance Monoid ReplayDiagnostics where
  mempty =
    ReplayDiagnostics
      { rdRequestCacheHits = zeroNonNegativeCount,
        rdRequestCacheMisses = zeroNonNegativeCount,
        rdFullReplayQueries = zeroNonNegativeCount,
        rdIncrementalReplayQueries = zeroNonNegativeCount,
        rdFrontierSeedCount = zeroNonNegativeCount,
        rdMaterializedRegionCount = zeroNonNegativeCount,
        rdAffectedRootCount = zeroNonNegativeCount,
        rdReusedRootCount = zeroNonNegativeCount,
        rdExactFeasibleRootCount = zeroNonNegativeCount,
        rdExactInfeasibleRootCount = zeroNonNegativeCount,
        rdObstructedRootCount = zeroNonNegativeCount,
        rdFallbackAttemptedRootCount = zeroNonNegativeCount,
        rdFallbackHitRootCount = zeroNonNegativeCount,
        rdRegionEnumerationNanoseconds = zeroNanoseconds,
        rdRegionAnalysisNanoseconds = zeroNanoseconds,
        rdFallbackMatchingNanoseconds = zeroNanoseconds,
        rdDatabaseConstructionNanoseconds = zeroNanoseconds,
        rdSeedsAfterPruningGates = zeroNonNegativeCount,
        rdSeedsAfterFrontierFilter = zeroNonNegativeCount,
        rdSeedsAfterMaterialization = zeroNonNegativeCount,
        rdSeedsPassingMicrosupport = zeroNonNegativeCount,
        rdSeedsPassingContext = zeroNonNegativeCount,
        rdSeedsPassingSpectral = zeroNonNegativeCount,
        rdSeedsPassingLaplacian = zeroNonNegativeCount
      }

diffReplayDiagnostics ::
  ReplayDiagnostics ->
  ReplayDiagnostics ->
  Either ReplayDiagnosticsValidationError ReplayDiagnostics
diffReplayDiagnostics a b =
  ReplayDiagnostics
    <$> diffNonNegativeCount (rdRequestCacheHits a) (rdRequestCacheHits b)
    <*> diffNonNegativeCount (rdRequestCacheMisses a) (rdRequestCacheMisses b)
    <*> diffNonNegativeCount (rdFullReplayQueries a) (rdFullReplayQueries b)
    <*> diffNonNegativeCount (rdIncrementalReplayQueries a) (rdIncrementalReplayQueries b)
    <*> diffNonNegativeCount (rdFrontierSeedCount a) (rdFrontierSeedCount b)
    <*> diffNonNegativeCount (rdMaterializedRegionCount a) (rdMaterializedRegionCount b)
    <*> diffNonNegativeCount (rdAffectedRootCount a) (rdAffectedRootCount b)
    <*> diffNonNegativeCount (rdReusedRootCount a) (rdReusedRootCount b)
    <*> diffNonNegativeCount (rdExactFeasibleRootCount a) (rdExactFeasibleRootCount b)
    <*> diffNonNegativeCount (rdExactInfeasibleRootCount a) (rdExactInfeasibleRootCount b)
    <*> diffNonNegativeCount (rdObstructedRootCount a) (rdObstructedRootCount b)
    <*> diffNonNegativeCount (rdFallbackAttemptedRootCount a) (rdFallbackAttemptedRootCount b)
    <*> diffNonNegativeCount (rdFallbackHitRootCount a) (rdFallbackHitRootCount b)
    <*> diffNanoseconds (rdRegionEnumerationNanoseconds a) (rdRegionEnumerationNanoseconds b)
    <*> diffNanoseconds (rdRegionAnalysisNanoseconds a) (rdRegionAnalysisNanoseconds b)
    <*> diffNanoseconds (rdFallbackMatchingNanoseconds a) (rdFallbackMatchingNanoseconds b)
    <*> diffNanoseconds (rdDatabaseConstructionNanoseconds a) (rdDatabaseConstructionNanoseconds b)
    <*> diffNonNegativeCount (rdSeedsAfterPruningGates a) (rdSeedsAfterPruningGates b)
    <*> diffNonNegativeCount (rdSeedsAfterFrontierFilter a) (rdSeedsAfterFrontierFilter b)
    <*> diffNonNegativeCount (rdSeedsAfterMaterialization a) (rdSeedsAfterMaterialization b)
    <*> diffNonNegativeCount (rdSeedsPassingMicrosupport a) (rdSeedsPassingMicrosupport b)
    <*> diffNonNegativeCount (rdSeedsPassingContext a) (rdSeedsPassingContext b)
    <*> diffNonNegativeCount (rdSeedsPassingSpectral a) (rdSeedsPassingSpectral b)
    <*> diffNonNegativeCount (rdSeedsPassingLaplacian a) (rdSeedsPassingLaplacian b)

replayTotalRequests :: ReplayDiagnostics -> NonNegativeCount
replayTotalRequests d =
  addNonNegativeCount (rdRequestCacheHits d) (rdRequestCacheMisses d)

replayCacheHitRate :: ReplayDiagnostics -> Either ReplayDiagnosticsValidationError Rate
replayCacheHitRate d =
  rateFromCounts (rdRequestCacheHits d) (replayTotalRequests d)

replayIncrementalRate :: ReplayDiagnostics -> Either ReplayDiagnosticsValidationError Rate
replayIncrementalRate d =
  let totalQueries =
        addNonNegativeCount
          (rdFullReplayQueries d)
          (rdIncrementalReplayQueries d)
   in rateFromCounts (rdIncrementalReplayQueries d) totalQueries

replayFallbackRate :: ReplayDiagnostics -> Either ReplayDiagnosticsValidationError Rate
replayFallbackRate d =
  rateFromCounts (rdFallbackAttemptedRootCount d) (rdAffectedRootCount d)

replayExactCoverageRate :: ReplayDiagnostics -> Either ReplayDiagnosticsValidationError Rate
replayExactCoverageRate d =
  rateFromCounts (rdExactFeasibleRootCount d) (rdAffectedRootCount d)

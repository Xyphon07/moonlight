{-| Checked wall-clock and RTS-resource measurements for benchmark actions. -}
module Moonlight.Pale.Bench.Measure
  ( TimedSample (..),
    timeSample,
    RtsCounter (..),
    RtsSnapshot (..),
    RtsDeltaObstruction (..),
    RtsDelta (..),
    checkedRtsDelta,
    RtsPhaseResourceObstruction (..),
    RtsPhaseMeasurement,
    rtsPhaseElapsedNanoseconds,
    measuredRtsPhaseResourceBytes,
    RtsPhaseBoundaryObservation (..),
    finalizeRtsPhaseMeasurement,
    unmeasuredRtsPhaseMeasurement,
    combineRtsPhaseMeasurements,
    observeRtsPhaseEither,
    RtsMeasurementFailure (..),
    RtsMeasurement (..),
    finalizeRtsMeasurement,
    measureSample,
  )
where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
  ( RTSStats,
    allocated_bytes,
    copied_bytes,
    cpu_ns,
    elapsed_ns,
    gc,
    gc_cpu_ns,
    gc_elapsed_ns,
    gcdetails_live_bytes,
    gcs,
    getRTSStats,
    getRTSStatsEnabled,
    major_gcs,
    max_live_bytes,
    mutator_cpu_ns,
    mutator_elapsed_ns,
  )
import System.Mem (performMajorGC)

data TimedSample value = TimedSample
  { timedSampleElapsedNanoseconds :: !Word64,
    timedSampleValue :: !value,
    timedSampleDigest :: !Int
  }

timeSample ::
  Int -> (Int -> IO input) -> (input -> Either errorValue value) -> (value -> Int) ->
  IO (Either errorValue (TimedSample value))
timeSample sampleOrdinal prepareInput runSample digest = do
  input <- prepareInput sampleOrdinal
  start <- getMonotonicTimeNSec
  sampleResult <- evaluate (runSample input)
  traverse
    ( \sampleValue -> do
        sampleDigest <- evaluate (force (digest sampleValue))
        end <- getMonotonicTimeNSec
        pure (TimedSample (end - start) sampleValue sampleDigest)
    )
    sampleResult

-- The closed set of monotone RTS counters used by process measurements.
data RtsCounter
  = RtsCounterGcs
  | RtsCounterMajorGcs
  | RtsCounterAllocatedBytes
  | RtsCounterCopiedBytes
  | RtsCounterMutatorCpuNanoseconds
  | RtsCounterMutatorElapsedNanoseconds
  | RtsCounterGcCpuNanoseconds
  | RtsCounterGcElapsedNanoseconds
  | RtsCounterCpuNanoseconds
  | RtsCounterElapsedNanoseconds
  deriving stock (Eq, Show, Read)

-- Strict action-boundary projection of the cumulative RTS counters.
data RtsSnapshot = RtsSnapshot
  { rtsSnapshotGcs :: !Word32,
    rtsSnapshotMajorGcs :: !Word32,
    rtsSnapshotAllocatedBytes :: !Word64,
    rtsSnapshotCopiedBytes :: !Word64,
    rtsSnapshotMutatorCpuNanoseconds :: !Int64,
    rtsSnapshotMutatorElapsedNanoseconds :: !Int64,
    rtsSnapshotGcCpuNanoseconds :: !Int64,
    rtsSnapshotGcElapsedNanoseconds :: !Int64,
    rtsSnapshotCpuNanoseconds :: !Int64,
    rtsSnapshotElapsedNanoseconds :: !Int64,
    rtsSnapshotLiveBytes :: !Word64,
    rtsSnapshotMaxLiveBytes :: !Word64
  }
  deriving stock (Eq, Show)

data RtsDeltaObstruction
  = RtsCounterRegression !RtsCounter !Integer !Integer
  deriving stock (Eq, Show, Read)

-- Checked action-local differences of every governed cumulative RTS counter.
data RtsDelta = RtsDelta
  { rtsDeltaGcs :: !Word64,
    rtsDeltaMajorGcs :: !Word64,
    rtsDeltaAllocatedBytes :: !Word64,
    rtsDeltaCopiedBytes :: !Word64,
    rtsDeltaMutatorCpuNanoseconds :: !Word64,
    rtsDeltaMutatorElapsedNanoseconds :: !Word64,
    rtsDeltaGcCpuNanoseconds :: !Word64,
    rtsDeltaGcElapsedNanoseconds :: !Word64,
    rtsDeltaCpuNanoseconds :: !Word64,
    rtsDeltaElapsedNanoseconds :: !Word64
  }
  deriving stock (Eq, Show, Read)

-- Why a profiled phase cannot publish checked allocation/copy evidence.
-- The semantic action may still succeed: this obstruction belongs to the
-- measurement boundary, not to the domain interpreter being observed.
data RtsPhaseResourceObstruction
  = RtsPhaseResourcesUnmeasured
  | RtsPhaseStatsUnavailable
  | RtsPhaseDeltaRefused !(NonEmpty RtsDeltaObstruction)
  deriving stock (Eq, Show, Read)

data RtsPhaseResources
  = RtsPhaseResourcesMeasured
      !Integer
      !Integer
  | RtsPhaseResourcesNotMeasured
  | RtsPhaseResourcesStatsUnavailable
  | RtsPhaseResourcesDeltaRefused !(NonEmpty RtsDeltaObstruction)
  deriving stock (Eq, Show, Read)

-- One semantic phase observed at its existing IO interpreter boundary.
-- Allocation and copied bytes are checked monotone deltas.  They use
-- 'Integer' after differencing so composing adjacent sub-phases is exact and
-- cannot overflow a machine counter type.
data RtsPhaseMeasurement = RtsPhaseMeasurement
  { rtsPhaseElapsedNanoseconds :: !Integer,
    rtsPhaseResources :: !RtsPhaseResources
  }
  deriving stock (Eq, Show, Read)

-- The exhaustive boundary evidence from which a phase measurement is
-- finalized.  Snapshot construction remains owned by the RTS interpreter;
-- the pure finalizer makes precedence and checked differencing testable.
data RtsPhaseBoundaryObservation
  = RtsPhaseBoundaryNotMeasured
  | RtsPhaseBoundaryStatsUnavailable
  | RtsPhaseBoundarySnapshots !RtsSnapshot !RtsSnapshot !RtsSnapshot
  deriving stock (Eq, Show)

finalizeRtsPhaseMeasurement ::
  Integer ->
  RtsPhaseBoundaryObservation ->
  RtsPhaseMeasurement
finalizeRtsPhaseMeasurement elapsedNanoseconds boundaryObservation =
  RtsPhaseMeasurement
    { rtsPhaseElapsedNanoseconds = elapsedNanoseconds,
      rtsPhaseResources =
        case boundaryObservation of
          RtsPhaseBoundaryNotMeasured -> RtsPhaseResourcesNotMeasured
          RtsPhaseBoundaryStatsUnavailable -> RtsPhaseResourcesStatsUnavailable
          RtsPhaseBoundarySnapshots beforeAction afterAction afterPostGc ->
            either
              (RtsPhaseResourcesDeltaRefused . (:| []))
              ( \deltaValue ->
                  RtsPhaseResourcesMeasured
                    (toInteger (rtsDeltaAllocatedBytes deltaValue))
                    (toInteger (rtsDeltaCopiedBytes deltaValue))
              )
              (checkedRtsPhaseDelta beforeAction afterAction afterPostGc)
    }

measuredRtsPhaseResourceBytes ::
  RtsPhaseMeasurement ->
  Either RtsPhaseResourceObstruction (Integer, Integer)
measuredRtsPhaseResourceBytes measurementValue =
  case rtsPhaseResources measurementValue of
    RtsPhaseResourcesMeasured allocatedBytes copiedBytes ->
      Right (allocatedBytes, copiedBytes)
    RtsPhaseResourcesNotMeasured -> Left RtsPhaseResourcesUnmeasured
    RtsPhaseResourcesStatsUnavailable -> Left RtsPhaseStatsUnavailable
    RtsPhaseResourcesDeltaRefused obstructions ->
      Left (RtsPhaseDeltaRefused obstructions)

unmeasuredRtsPhaseMeasurement :: RtsPhaseMeasurement
unmeasuredRtsPhaseMeasurement =
  finalizeRtsPhaseMeasurement 0 RtsPhaseBoundaryNotMeasured

combineRtsPhaseMeasurements ::
  RtsPhaseMeasurement ->
  RtsPhaseMeasurement ->
  RtsPhaseMeasurement
combineRtsPhaseMeasurements leftMeasurement rightMeasurement =
  RtsPhaseMeasurement
    { rtsPhaseElapsedNanoseconds =
        rtsPhaseElapsedNanoseconds leftMeasurement
          + rtsPhaseElapsedNanoseconds rightMeasurement,
      rtsPhaseResources =
        combineRtsPhaseResources
          (rtsPhaseResources leftMeasurement)
          (rtsPhaseResources rightMeasurement)
    }

combineRtsPhaseResources ::
  RtsPhaseResources ->
  RtsPhaseResources ->
  RtsPhaseResources
combineRtsPhaseResources leftResources rightResources =
  case (leftResources, rightResources) of
    (RtsPhaseResourcesDeltaRefused leftObstructions, RtsPhaseResourcesDeltaRefused rightObstructions) ->
      RtsPhaseResourcesDeltaRefused (leftObstructions <> rightObstructions)
    (RtsPhaseResourcesDeltaRefused obstructions, _) ->
      RtsPhaseResourcesDeltaRefused obstructions
    (_, RtsPhaseResourcesDeltaRefused obstructions) ->
      RtsPhaseResourcesDeltaRefused obstructions
    (RtsPhaseResourcesStatsUnavailable, _) -> RtsPhaseResourcesStatsUnavailable
    (_, RtsPhaseResourcesStatsUnavailable) -> RtsPhaseResourcesStatsUnavailable
    (RtsPhaseResourcesNotMeasured, _) -> RtsPhaseResourcesNotMeasured
    (_, RtsPhaseResourcesNotMeasured) -> RtsPhaseResourcesNotMeasured
    (RtsPhaseResourcesMeasured leftAllocated leftCopied, RtsPhaseResourcesMeasured rightAllocated rightCopied) ->
      RtsPhaseResourcesMeasured
        (leftAllocated + rightAllocated)
        (leftCopied + rightCopied)

-- Observe one already-owned pure phase without introducing a second
-- pipeline.  A major collection before the phase establishes the allocation
-- boundary; the post-phase collection closes the nursery for allocation only.
-- Copied bytes and elapsed time stop before that collection, matching
-- 'finalizeRtsMeasurement'.  Neither boundary collection is charged to phase
-- elapsed time.
observeRtsPhaseEither ::
  (value -> witness) ->
  Either errorValue value ->
  IO (Either errorValue (value, RtsPhaseMeasurement))
observeRtsPhaseEither timingReadiness phaseResult =
  getRTSStatsEnabled >>= \statsEnabled ->
    if statsEnabled
      then observeWithStats
      else observeWithoutStats
  where
    observeWithStats = do
      beforeActionStats <- majorGcStats
      start <- getMonotonicTimeNSec
      case phaseResult of
        Left phaseFailure -> pure (Left phaseFailure)
        Right phaseValue -> do
          _ <- evaluate (timingReadiness phaseValue)
          end <- getMonotonicTimeNSec
          afterActionStats <- getRTSStats
          afterPostGcStats <- majorGcStats
          pure
            ( Right
                ( phaseValue,
                  finalizeRtsPhaseMeasurement
                    (toInteger (end - start))
                    ( RtsPhaseBoundarySnapshots
                        (rtsSnapshotFromStats beforeActionStats)
                        (rtsSnapshotFromStats afterActionStats)
                        (rtsSnapshotFromStats afterPostGcStats)
                    )
                )
            )

    observeWithoutStats = do
      start <- getMonotonicTimeNSec
      case phaseResult of
        Left phaseFailure -> pure (Left phaseFailure)
        Right phaseValue -> do
          _ <- evaluate (timingReadiness phaseValue)
          end <- getMonotonicTimeNSec
          pure
            ( Right
                ( phaseValue,
                  finalizeRtsPhaseMeasurement
                    (toInteger (end - start))
                    RtsPhaseBoundaryStatsUnavailable
                )
            )

checkedRtsPhaseDelta ::
  RtsSnapshot ->
  RtsSnapshot ->
  RtsSnapshot ->
  Either RtsDeltaObstruction RtsDelta
checkedRtsPhaseDelta beforeAction afterAction afterPostGc =
  (\actionDelta allocatedBytes -> actionDelta {rtsDeltaAllocatedBytes = allocatedBytes})
    <$> checkedRtsDelta beforeAction afterAction
    <*> checkedCounterDifference
      RtsCounterAllocatedBytes
      (rtsSnapshotAllocatedBytes beforeAction)
      (rtsSnapshotAllocatedBytes afterPostGc)

checkedRtsDelta ::
  RtsSnapshot ->
  RtsSnapshot ->
  Either RtsDeltaObstruction RtsDelta
checkedRtsDelta beforeSnapshot afterSnapshot =
  RtsDelta
    <$> counterDelta RtsCounterGcs rtsSnapshotGcs
    <*> counterDelta RtsCounterMajorGcs rtsSnapshotMajorGcs
    <*> counterDelta RtsCounterAllocatedBytes rtsSnapshotAllocatedBytes
    <*> counterDelta RtsCounterCopiedBytes rtsSnapshotCopiedBytes
    <*> counterDelta RtsCounterMutatorCpuNanoseconds rtsSnapshotMutatorCpuNanoseconds
    <*> counterDelta RtsCounterMutatorElapsedNanoseconds rtsSnapshotMutatorElapsedNanoseconds
    <*> counterDelta RtsCounterGcCpuNanoseconds rtsSnapshotGcCpuNanoseconds
    <*> counterDelta RtsCounterGcElapsedNanoseconds rtsSnapshotGcElapsedNanoseconds
    <*> counterDelta RtsCounterCpuNanoseconds rtsSnapshotCpuNanoseconds
    <*> counterDelta RtsCounterElapsedNanoseconds rtsSnapshotElapsedNanoseconds
  where
    counterDelta ::
      (Integral counter) =>
      RtsCounter ->
      (RtsSnapshot -> counter) ->
      Either RtsDeltaObstruction Word64
    counterDelta counter project =
      checkedCounterDifference counter (project beforeSnapshot) (project afterSnapshot)

checkedCounterDifference ::
  (Integral counter) =>
  RtsCounter ->
  counter ->
  counter ->
  Either RtsDeltaObstruction Word64
checkedCounterDifference counter beforeValue afterValue
  | afterValue < beforeValue =
      Left
        ( RtsCounterRegression
            counter
            (toInteger beforeValue)
            (toInteger afterValue)
        )
  | otherwise =
      Right (fromIntegral (afterValue - beforeValue))

data RtsMeasurementFailure errorValue
  = RtsMeasurementStatsDisabled
  | RtsMeasurementActionFailed !errorValue
  | RtsMeasurementDeltaFailed !RtsDeltaObstruction
  deriving stock (Eq, Show)

data RtsMeasurement value = RtsMeasurement
  { rtsMeasurementElapsedNanoseconds :: !Word64,
    rtsMeasurementDelta :: !RtsDelta,
    rtsMeasurementProcessLiveBytesAfterGc :: !Word64,
    rtsMeasurementProcessMaxLiveBytes :: !Word64,
    rtsMeasurementValue :: !value,
    rtsMeasurementDigest :: !Int
  }

-- Pure checked gluing of the three action-boundary RTS observations.
-- The live and maximum fields are explicitly process-wide observations. GHC's
-- counters do not expose action-local retained or peak residency.
--
-- Allocation is taken across the post-GC boundary while every other counter is
-- taken across the action boundary, and the asymmetry is deliberate. GHC
-- refreshes @allocated_bytes@ at garbage collections, so a plain
-- before-to-after difference counts only the allocation that a collection
-- inside the region happened to close out: measured 2026-08-06, four of five
-- atomic probes reported exactly zero allocated bytes beside a nonzero wall,
-- each with a zero GC count, while the one probe that triggered four
-- collections reported 17.7 MB. Reading allocation after a major collection
-- accounts the outstanding nursery. It is sound for this counter alone because
-- a collection performs no mutator allocation; the timing counters and
-- @copied_bytes@ must not cross that boundary, since the collection's own cost
-- would be attributed to the measured action.
finalizeRtsMeasurement ::
  Word64 ->
  RtsSnapshot ->
  RtsSnapshot ->
  RtsSnapshot ->
  value ->
  Int ->
  Either RtsDeltaObstruction (RtsMeasurement value)
finalizeRtsMeasurement elapsedNanoseconds beforeAction afterAction afterPostGc sampleValue sampleDigest =
  (\actionDelta allocatedBytes ->
      RtsMeasurement
        { rtsMeasurementElapsedNanoseconds = elapsedNanoseconds,
          rtsMeasurementDelta = actionDelta {rtsDeltaAllocatedBytes = allocatedBytes},
          rtsMeasurementProcessLiveBytesAfterGc = rtsSnapshotLiveBytes afterPostGc,
          rtsMeasurementProcessMaxLiveBytes = rtsSnapshotMaxLiveBytes afterPostGc,
          rtsMeasurementValue = sampleValue,
          rtsMeasurementDigest = sampleDigest
        }
  )
    <$> checkedRtsDelta beforeAction afterAction
    <*> checkedCounterDifference
      RtsCounterAllocatedBytes
      (rtsSnapshotAllocatedBytes beforeAction)
      (rtsSnapshotAllocatedBytes afterPostGc)

measureSample ::
  Int ->
  (Int -> IO input) ->
  (input -> IO (Either errorValue value)) ->
  (value -> ()) ->
  (value -> Int) ->
  IO (Either (RtsMeasurementFailure errorValue) (RtsMeasurement value))
measureSample sampleOrdinal prepareInput runSample timingReadiness digest =
  getRTSStatsEnabled >>= \statsEnabled ->
    if statsEnabled
      then measureWithStats
      else pure (Left RtsMeasurementStatsDisabled)
  where
    measureWithStats = do
      input <- prepareInput sampleOrdinal
      beforeActionStats <- majorGcStats
      start <- getMonotonicTimeNSec
      sampleResult <- runSample input
      fmap (>>= id) $
        traverse
          (finishMeasurement beforeActionStats start)
          (first RtsMeasurementActionFailed sampleResult)

    finishMeasurement beforeActionStats start sampleValue = do
      sampleDigest <-
        snd
          <$> evaluate
            (force (timingReadiness sampleValue, digest sampleValue))
      end <- getMonotonicTimeNSec
      afterActionStats <- getRTSStats
      afterPostGcStats <- majorGcStats
      pure
        ( first RtsMeasurementDeltaFailed
            ( finalizeRtsMeasurement
                (end - start)
                (rtsSnapshotFromStats beforeActionStats)
                (rtsSnapshotFromStats afterActionStats)
                (rtsSnapshotFromStats afterPostGcStats)
                sampleValue
                sampleDigest
            )
        )

rtsSnapshotFromStats :: RTSStats -> RtsSnapshot
rtsSnapshotFromStats stats =
  RtsSnapshot
    { rtsSnapshotGcs = gcs stats,
      rtsSnapshotMajorGcs = major_gcs stats,
      rtsSnapshotAllocatedBytes = allocated_bytes stats,
      rtsSnapshotCopiedBytes = copied_bytes stats,
      rtsSnapshotMutatorCpuNanoseconds = mutator_cpu_ns stats,
      rtsSnapshotMutatorElapsedNanoseconds = mutator_elapsed_ns stats,
      rtsSnapshotGcCpuNanoseconds = gc_cpu_ns stats,
      rtsSnapshotGcElapsedNanoseconds = gc_elapsed_ns stats,
      rtsSnapshotCpuNanoseconds = cpu_ns stats,
      rtsSnapshotElapsedNanoseconds = elapsed_ns stats,
      rtsSnapshotLiveBytes = gcdetails_live_bytes (gc stats),
      rtsSnapshotMaxLiveBytes = max_live_bytes stats
    }

majorGcStats :: IO RTSStats
majorGcStats =
  performMajorGC *> getRTSStats

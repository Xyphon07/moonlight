module Main (main) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word64)
import Moonlight.Pale.Bench.Measure
  ( RtsMeasurement (..),
    RtsCounter (..),
    RtsDelta (..),
    RtsDeltaObstruction (..),
    RtsSnapshot (..),
    checkedRtsDelta,
    RtsPhaseBoundaryObservation(..),
    RtsPhaseResourceObstruction(..),
    combineRtsPhaseMeasurements,
    finalizeRtsPhaseMeasurement,
    finalizeRtsMeasurement,
    measuredRtsPhaseResourceBytes,
    measureSample,
    rtsPhaseElapsedNanoseconds,
    unmeasuredRtsPhaseMeasurement,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "RTS measurement"
        [ testCase "all monotone counters produce their exact differences" monotoneDeltaLaw,
          testCase "equal snapshots produce the zero delta" zeroDeltaLaw,
          testGroup "counter regression obstructions" (fmap counterRegressionLaw allRtsCounters),
          testGroup
            "extreme monotone signed counters"
            (fmap extremeSignedCounterLaw allSignedRtsCounters),
          testCase
            "pure finalization labels process-wide live and maximum observations"
            finalizationSnapshotLaw,
          testCase "phase composition is exact" phaseCompositionLaw,
          testCase "phase resource obstruction precedence is exhaustive" phaseObstructionPrecedenceLaw,
          testCase "phase allocation closes post-GC while copy stops at action" phaseBoundaryDeltaLaw,
          testCase "RTS-backed measurement integration succeeds" measurementIntegrationSmokeLaw
        ]
    )

monotoneDeltaLaw :: IO ()
monotoneDeltaLaw =
  checkedRtsDelta monotoneBeforeSnapshot monotoneAfterSnapshot
    @?= Right expectedMonotoneDelta

zeroDeltaLaw :: IO ()
zeroDeltaLaw =
  checkedRtsDelta monotoneBeforeSnapshot monotoneBeforeSnapshot
    @?= Right zeroRtsDelta

counterRegressionLaw :: RtsCounter -> TestTree
counterRegressionLaw counter =
  testCase (show counter) $ do
    let (beforeSnapshot, afterSnapshot) = regressionSnapshots counter
    checkedRtsDelta beforeSnapshot afterSnapshot
      @?= Left (RtsCounterRegression counter 2 1)

data SignedRtsCounter
  = SignedRtsCounterMutatorCpuNanoseconds
  | SignedRtsCounterMutatorElapsedNanoseconds
  | SignedRtsCounterGcCpuNanoseconds
  | SignedRtsCounterGcElapsedNanoseconds
  | SignedRtsCounterCpuNanoseconds
  | SignedRtsCounterElapsedNanoseconds
  deriving stock (Eq, Show)

extremeSignedCounterLaw :: SignedRtsCounter -> TestTree
extremeSignedCounterLaw counter =
  testCase (show counter) $
    case checkedRtsDelta beforeSnapshot afterSnapshot of
      Left obstructionValue ->
        assertFailure ("extreme monotone counter was rejected: " <> show obstructionValue)
      Right deltaValue ->
        signedCounterDelta counter deltaValue @?= maxBound
  where
    (beforeSnapshot, afterSnapshot) = extremeSignedSnapshots counter

finalizationSnapshotLaw :: IO ()
finalizationSnapshotLaw =
  case finalizeRtsMeasurement 17 beforeSnapshot afterActionSnapshot afterPostGcSnapshot "value" 29 of
    Left obstructionValue ->
      assertFailure ("valid finalization was rejected: " <> show obstructionValue)
    Right measurement -> do
      rtsMeasurementElapsedNanoseconds measurement @?= 17
      rtsDeltaAllocatedBytes (rtsMeasurementDelta measurement) @?= 23
      rtsMeasurementProcessLiveBytesAfterGc measurement @?= 31
      rtsMeasurementProcessMaxLiveBytes measurement @?= 43
      rtsMeasurementValue measurement @?= "value"
      rtsMeasurementDigest measurement @?= 29
  where
    beforeSnapshot = zeroRtsSnapshot
    afterActionSnapshot =
      zeroRtsSnapshot
        { rtsSnapshotAllocatedBytes = 23
        , rtsSnapshotLiveBytes = 41
        , rtsSnapshotMaxLiveBytes = 37
        }
    afterPostGcSnapshot =
      afterActionSnapshot
        { rtsSnapshotLiveBytes = 31
        , rtsSnapshotMaxLiveBytes = 43
        }

phaseCompositionLaw :: IO ()
phaseCompositionLaw = do
  rtsPhaseElapsedNanoseconds combinedMeasurement @?= 12
  measuredRtsPhaseResourceBytes combinedMeasurement @?= Right (18, 9)
  where
    combinedMeasurement =
      combineRtsPhaseMeasurements
        (finalizeRtsPhaseMeasurement 5 (phaseSnapshots 13 7))
        (finalizeRtsPhaseMeasurement 7 (phaseSnapshots 5 2))

phaseObstructionPrecedenceLaw :: IO ()
phaseObstructionPrecedenceLaw = do
  measuredRtsPhaseResourceBytes
    ( combineRtsPhaseMeasurements
        (finalizeRtsPhaseMeasurement 2 (phaseSnapshots 1 1))
        unmeasuredRtsPhaseMeasurement
    )
    @?= Left RtsPhaseResourcesUnmeasured
  measuredRtsPhaseResourceBytes
    (combineRtsPhaseMeasurements unmeasuredRtsPhaseMeasurement unavailableMeasurement)
    @?= Left RtsPhaseStatsUnavailable
  measuredRtsPhaseResourceBytes
    (combineRtsPhaseMeasurements refusedMeasurement unavailableMeasurement)
    @?= Left
      ( RtsPhaseDeltaRefused
          (RtsCounterRegression RtsCounterAllocatedBytes 2 1 :| [])
      )
  where
    unavailableMeasurement =
      finalizeRtsPhaseMeasurement 3 RtsPhaseBoundaryStatsUnavailable
    refusedMeasurement =
      finalizeRtsPhaseMeasurement
        4
        ( RtsPhaseBoundarySnapshots
            (zeroRtsSnapshot {rtsSnapshotAllocatedBytes = 2})
            (zeroRtsSnapshot {rtsSnapshotAllocatedBytes = 1})
            (zeroRtsSnapshot {rtsSnapshotAllocatedBytes = 1})
        )

phaseBoundaryDeltaLaw :: IO ()
phaseBoundaryDeltaLaw =
  measuredRtsPhaseResourceBytes
    ( finalizeRtsPhaseMeasurement
        11
        ( RtsPhaseBoundarySnapshots
            zeroRtsSnapshot
            ( zeroRtsSnapshot
                { rtsSnapshotAllocatedBytes = 10
                , rtsSnapshotCopiedBytes = 7
                }
            )
            ( zeroRtsSnapshot
                { rtsSnapshotAllocatedBytes = 13
                , rtsSnapshotCopiedBytes = 19
                }
            )
        )
    )
    @?= Right (13, 7)

phaseSnapshots :: Word64 -> Word64 -> RtsPhaseBoundaryObservation
phaseSnapshots allocatedBytes copiedBytes =
  RtsPhaseBoundarySnapshots
    zeroRtsSnapshot
    ( zeroRtsSnapshot
        { rtsSnapshotAllocatedBytes = allocatedBytes
        , rtsSnapshotCopiedBytes = copiedBytes
        }
    )
    ( zeroRtsSnapshot
        { rtsSnapshotAllocatedBytes = allocatedBytes
        , rtsSnapshotCopiedBytes = copiedBytes
        }
    )

measurementIntegrationSmokeLaw :: IO ()
measurementIntegrationSmokeLaw = do
  measurementResult <-
    measureSample
      1
      (const (pure 41))
      (\input -> pure (Right (input + 1) :: Either String Int))
      (\value -> value `seq` ())
      id
  case measurementResult of
    Left failure ->
      assertFailure ("RTS-backed measurement failed: " <> show failure)
    Right measurement -> do
      rtsMeasurementValue measurement @?= 42
      rtsMeasurementDigest measurement @?= 42

allRtsCounters :: [RtsCounter]
allRtsCounters =
  [ RtsCounterGcs,
    RtsCounterMajorGcs,
    RtsCounterAllocatedBytes,
    RtsCounterCopiedBytes,
    RtsCounterMutatorCpuNanoseconds,
    RtsCounterMutatorElapsedNanoseconds,
    RtsCounterGcCpuNanoseconds,
    RtsCounterGcElapsedNanoseconds,
    RtsCounterCpuNanoseconds,
    RtsCounterElapsedNanoseconds
  ]

allSignedRtsCounters :: [SignedRtsCounter]
allSignedRtsCounters =
  [ SignedRtsCounterMutatorCpuNanoseconds
  , SignedRtsCounterMutatorElapsedNanoseconds
  , SignedRtsCounterGcCpuNanoseconds
  , SignedRtsCounterGcElapsedNanoseconds
  , SignedRtsCounterCpuNanoseconds
  , SignedRtsCounterElapsedNanoseconds
  ]

extremeSignedSnapshots :: SignedRtsCounter -> (RtsSnapshot, RtsSnapshot)
extremeSignedSnapshots counter =
  case counter of
    SignedRtsCounterMutatorCpuNanoseconds ->
      ( zeroRtsSnapshot {rtsSnapshotMutatorCpuNanoseconds = minBound}
      , zeroRtsSnapshot {rtsSnapshotMutatorCpuNanoseconds = maxBound}
      )
    SignedRtsCounterMutatorElapsedNanoseconds ->
      ( zeroRtsSnapshot {rtsSnapshotMutatorElapsedNanoseconds = minBound}
      , zeroRtsSnapshot {rtsSnapshotMutatorElapsedNanoseconds = maxBound}
      )
    SignedRtsCounterGcCpuNanoseconds ->
      ( zeroRtsSnapshot {rtsSnapshotGcCpuNanoseconds = minBound}
      , zeroRtsSnapshot {rtsSnapshotGcCpuNanoseconds = maxBound}
      )
    SignedRtsCounterGcElapsedNanoseconds ->
      ( zeroRtsSnapshot {rtsSnapshotGcElapsedNanoseconds = minBound}
      , zeroRtsSnapshot {rtsSnapshotGcElapsedNanoseconds = maxBound}
      )
    SignedRtsCounterCpuNanoseconds ->
      ( zeroRtsSnapshot {rtsSnapshotCpuNanoseconds = minBound}
      , zeroRtsSnapshot {rtsSnapshotCpuNanoseconds = maxBound}
      )
    SignedRtsCounterElapsedNanoseconds ->
      ( zeroRtsSnapshot {rtsSnapshotElapsedNanoseconds = minBound}
      , zeroRtsSnapshot {rtsSnapshotElapsedNanoseconds = maxBound}
      )

signedCounterDelta :: SignedRtsCounter -> RtsDelta -> Word64
signedCounterDelta counter deltaValue =
  case counter of
    SignedRtsCounterMutatorCpuNanoseconds -> rtsDeltaMutatorCpuNanoseconds deltaValue
    SignedRtsCounterMutatorElapsedNanoseconds -> rtsDeltaMutatorElapsedNanoseconds deltaValue
    SignedRtsCounterGcCpuNanoseconds -> rtsDeltaGcCpuNanoseconds deltaValue
    SignedRtsCounterGcElapsedNanoseconds -> rtsDeltaGcElapsedNanoseconds deltaValue
    SignedRtsCounterCpuNanoseconds -> rtsDeltaCpuNanoseconds deltaValue
    SignedRtsCounterElapsedNanoseconds -> rtsDeltaElapsedNanoseconds deltaValue

regressionSnapshots :: RtsCounter -> (RtsSnapshot, RtsSnapshot)
regressionSnapshots = \case
  RtsCounterGcs ->
    ( zeroRtsSnapshot {rtsSnapshotGcs = 2},
      zeroRtsSnapshot {rtsSnapshotGcs = 1}
    )
  RtsCounterMajorGcs ->
    ( zeroRtsSnapshot {rtsSnapshotMajorGcs = 2},
      zeroRtsSnapshot {rtsSnapshotMajorGcs = 1}
    )
  RtsCounterAllocatedBytes ->
    ( zeroRtsSnapshot {rtsSnapshotAllocatedBytes = 2},
      zeroRtsSnapshot {rtsSnapshotAllocatedBytes = 1}
    )
  RtsCounterCopiedBytes ->
    ( zeroRtsSnapshot {rtsSnapshotCopiedBytes = 2},
      zeroRtsSnapshot {rtsSnapshotCopiedBytes = 1}
    )
  RtsCounterMutatorCpuNanoseconds ->
    ( zeroRtsSnapshot {rtsSnapshotMutatorCpuNanoseconds = 2},
      zeroRtsSnapshot {rtsSnapshotMutatorCpuNanoseconds = 1}
    )
  RtsCounterMutatorElapsedNanoseconds ->
    ( zeroRtsSnapshot {rtsSnapshotMutatorElapsedNanoseconds = 2},
      zeroRtsSnapshot {rtsSnapshotMutatorElapsedNanoseconds = 1}
    )
  RtsCounterGcCpuNanoseconds ->
    ( zeroRtsSnapshot {rtsSnapshotGcCpuNanoseconds = 2},
      zeroRtsSnapshot {rtsSnapshotGcCpuNanoseconds = 1}
    )
  RtsCounterGcElapsedNanoseconds ->
    ( zeroRtsSnapshot {rtsSnapshotGcElapsedNanoseconds = 2},
      zeroRtsSnapshot {rtsSnapshotGcElapsedNanoseconds = 1}
    )
  RtsCounterCpuNanoseconds ->
    ( zeroRtsSnapshot {rtsSnapshotCpuNanoseconds = 2},
      zeroRtsSnapshot {rtsSnapshotCpuNanoseconds = 1}
    )
  RtsCounterElapsedNanoseconds ->
    ( zeroRtsSnapshot {rtsSnapshotElapsedNanoseconds = 2},
      zeroRtsSnapshot {rtsSnapshotElapsedNanoseconds = 1}
    )

zeroRtsSnapshot :: RtsSnapshot
zeroRtsSnapshot =
  RtsSnapshot
    { rtsSnapshotGcs = 0,
      rtsSnapshotMajorGcs = 0,
      rtsSnapshotAllocatedBytes = 0,
      rtsSnapshotCopiedBytes = 0,
      rtsSnapshotMutatorCpuNanoseconds = 0,
      rtsSnapshotMutatorElapsedNanoseconds = 0,
      rtsSnapshotGcCpuNanoseconds = 0,
      rtsSnapshotGcElapsedNanoseconds = 0,
      rtsSnapshotCpuNanoseconds = 0,
      rtsSnapshotElapsedNanoseconds = 0,
      rtsSnapshotLiveBytes = 0,
      rtsSnapshotMaxLiveBytes = 0
    }

monotoneBeforeSnapshot :: RtsSnapshot
monotoneBeforeSnapshot =
  RtsSnapshot
    { rtsSnapshotGcs = 2,
      rtsSnapshotMajorGcs = 4,
      rtsSnapshotAllocatedBytes = 10,
      rtsSnapshotCopiedBytes = 40,
      rtsSnapshotMutatorCpuNanoseconds = 50,
      rtsSnapshotMutatorElapsedNanoseconds = 60,
      rtsSnapshotGcCpuNanoseconds = 70,
      rtsSnapshotGcElapsedNanoseconds = 80,
      rtsSnapshotCpuNanoseconds = 90,
      rtsSnapshotElapsedNanoseconds = 100,
      rtsSnapshotLiveBytes = 11,
      rtsSnapshotMaxLiveBytes = 12
    }

monotoneAfterSnapshot :: RtsSnapshot
monotoneAfterSnapshot =
  RtsSnapshot
    { rtsSnapshotGcs = 5,
      rtsSnapshotMajorGcs = 7,
      rtsSnapshotAllocatedBytes = 110,
      rtsSnapshotCopiedBytes = 240,
      rtsSnapshotMutatorCpuNanoseconds = 350,
      rtsSnapshotMutatorElapsedNanoseconds = 460,
      rtsSnapshotGcCpuNanoseconds = 570,
      rtsSnapshotGcElapsedNanoseconds = 680,
      rtsSnapshotCpuNanoseconds = 790,
      rtsSnapshotElapsedNanoseconds = 900,
      rtsSnapshotLiveBytes = 13,
      rtsSnapshotMaxLiveBytes = 14
    }

expectedMonotoneDelta :: RtsDelta
expectedMonotoneDelta =
  RtsDelta
    { rtsDeltaGcs = 3,
      rtsDeltaMajorGcs = 3,
      rtsDeltaAllocatedBytes = 100,
      rtsDeltaCopiedBytes = 200,
      rtsDeltaMutatorCpuNanoseconds = 300,
      rtsDeltaMutatorElapsedNanoseconds = 400,
      rtsDeltaGcCpuNanoseconds = 500,
      rtsDeltaGcElapsedNanoseconds = 600,
      rtsDeltaCpuNanoseconds = 700,
      rtsDeltaElapsedNanoseconds = 800
    }

zeroRtsDelta :: RtsDelta
zeroRtsDelta =
  RtsDelta
    { rtsDeltaGcs = 0,
      rtsDeltaMajorGcs = 0,
      rtsDeltaAllocatedBytes = 0,
      rtsDeltaCopiedBytes = 0,
      rtsDeltaMutatorCpuNanoseconds = 0,
      rtsDeltaMutatorElapsedNanoseconds = 0,
      rtsDeltaGcCpuNanoseconds = 0,
      rtsDeltaGcElapsedNanoseconds = 0,
      rtsDeltaCpuNanoseconds = 0,
      rtsDeltaElapsedNanoseconds = 0
    }

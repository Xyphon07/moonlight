{-# LANGUAGE DerivingStrategies #-}

module Recursion.RecursionSpec
  ( tests,
  )
where

import Hedgehog qualified as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Moonlight.Pale.Test.Recursion
  ( cataAfterAnaIdentity,
    interpreterCoherence,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog qualified as TH
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck qualified as QC

newtype RecursionBound = RecursionBound
  { recursionBoundValue :: Int
  }
  deriving stock (Eq, Show)

data RecursionTrace = RecursionTrace
  { recursionTraceConfiguredBound :: RecursionBound,
    recursionTraceVisitedFrames :: [Int]
  }
  deriving stock (Eq, Show)

data RecursionReport = RecursionReport
  { recursionReportSteps :: Int,
    recursionReportStoppedAtBound :: Bool
  }
  deriving stock (Eq, Show)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Recursion"
    [ testCase "cata-after-ana distinguishes coherent and incoherent inverses" $ do
        cataAfterAnaIdentity boundedAna traceConfiguredBound configuredBound @?= True
        cataAfterAnaIdentity boundedAna underreportedTraceBound configuredBound @?= False,
      testCase "interpreter coherence distinguishes matching and mismatched reports" $ do
        interpreterCoherence boundedAna boundedCata boundedHylo configuredBound @?= True
        interpreterCoherence boundedAna mismatchedCata boundedHylo configuredBound @?= False,
      QC.testProperty "QuickCheck: bounded recursion reports its configured limit" $
        QC.property boundedReportMatchesNonNegative,
      TH.testProperty "Hedgehog: bounded recursion reports its configured limit" $
        HH.property (HH.forAll boundedGenerator >>= HH.assert . boundedReportMatchesBound)
    ]

configuredBound :: RecursionBound
configuredBound =
  RecursionBound 4

boundedGenerator :: HH.Gen RecursionBound
boundedGenerator =
  RecursionBound <$> Gen.int (Range.linear 0 16)

boundedReportMatchesNonNegative :: QC.NonNegative Int -> Bool
boundedReportMatchesNonNegative rawBound =
  boundedReportMatchesBound (smallRecursionBound rawBound)

boundedReportMatchesBound :: RecursionBound -> Bool
boundedReportMatchesBound bound =
  boundedHylo bound
    == RecursionReport
      { recursionReportSteps = recursionBoundValue bound,
        recursionReportStoppedAtBound = True
      }

smallRecursionBound :: QC.NonNegative Int -> RecursionBound
smallRecursionBound (QC.NonNegative rawBound) =
  RecursionBound (rawBound `mod` 17)

boundedAna :: RecursionBound -> RecursionTrace
boundedAna bound =
  RecursionTrace
    { recursionTraceConfiguredBound = bound,
      recursionTraceVisitedFrames = [0 .. recursionBoundValue bound - 1]
    }

boundedCata :: RecursionTrace -> RecursionReport
boundedCata trace =
  RecursionReport
    { recursionReportSteps = length (recursionTraceVisitedFrames trace),
      recursionReportStoppedAtBound = length (recursionTraceVisitedFrames trace) == recursionBoundValue (recursionTraceConfiguredBound trace)
    }

boundedHylo :: RecursionBound -> RecursionReport
boundedHylo =
  boundedCata . boundedAna

traceConfiguredBound :: RecursionTrace -> RecursionBound
traceConfiguredBound =
  recursionTraceConfiguredBound

underreportedTraceBound :: RecursionTrace -> RecursionBound
underreportedTraceBound trace =
  RecursionBound (recursionBoundValue (recursionTraceConfiguredBound trace) - 1)

mismatchedCata :: RecursionTrace -> RecursionReport
mismatchedCata trace =
  RecursionReport
    { recursionReportSteps = recursionReportSteps (boundedCata trace) + 1,
      recursionReportStoppedAtBound = False
    }

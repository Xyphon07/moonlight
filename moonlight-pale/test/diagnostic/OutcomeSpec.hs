module OutcomeSpec
  ( tests,
  )
where

import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Moonlight.Pale.Diagnostic.Aggregation.Algebra
  ( OutcomeSummary,
    outcomeSummaryDiagnostics,
    outcomeSummaryFromProjectionOutcome,
    outcomeSummaryFromRestrictionOutcome,
    outcomeSummaryRestrictionOutcomes,
    restrictionIndexByCell,
    restrictionIndexByMismatch,
    restrictionIndexFromOutcomes,
    restrictionIndexStats,
    restrictionIndexTotal,
    topRestrictionHotspots,
  )
import Moonlight.Pale.Diagnostic.Local.Propagation
  ( ProjectionRunOutcome (..),
    RestrictionOutcomeStat (..),
    RestrictionRunOutcome (..),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

type Cell = String

type ProjectionKey = String

type ProjectionValue = String

type ProjectionFailure = String

type Diagnostic = String

data Mismatch
  = ContextMismatch
  | PhaseMismatch
  | ShapeMismatch
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "pale.diagnostic.outcome"
    [ testCase "OutcomeSummary has mempty as a left identity" $
        assertEqual
          "left identity"
          outcomeSummaryA
          (mempty <> outcomeSummaryA),
      testCase "OutcomeSummary has mempty as a right identity" $
        assertEqual
          "right identity"
          outcomeSummaryA
          (outcomeSummaryA <> mempty),
      testCase "OutcomeSummary composition is associative" $
        assertEqual
          "associativity"
          ((outcomeSummaryA <> outcomeSummaryB) <> outcomeSummaryC)
          (outcomeSummaryA <> (outcomeSummaryB <> outcomeSummaryC)),
      testCase "OutcomeSummary preserves diagnostic and restriction order" $
        let summary = outcomeSummaryA <> outcomeSummaryC <> outcomeSummaryC
         in do
              assertEqual
                "diagnostics"
                (Seq.singleton "alpha adjusted")
                (outcomeSummaryDiagnostics summary)
              assertEqual
                "restrictions"
                (Seq.fromList [restrictionC, restrictionC])
                (outcomeSummaryRestrictionOutcomes summary),
      testCase "one restriction index derives every aggregate view" $
        let restrictionIndex = restrictionIndexFromOutcomes knownRestrictionOutcomes
         in do
              assertEqual "total" 6 (restrictionIndexTotal restrictionIndex)
              assertEqual
                "mismatch counts"
                [(ContextMismatch, 1), (PhaseMismatch, 3), (ShapeMismatch, 2)]
                (Map.toAscList (restrictionIndexByMismatch restrictionIndex))
              assertEqual
                "cell counts"
                [("alpha", 3), ("beta", 2), ("gamma", 1), ("omega", 6)]
                (Map.toAscList (restrictionIndexByCell restrictionIndex))
              assertEqual
                "ranked hotspot structure"
                [ ("alpha", "omega", PhaseMismatch),
                  ("beta", "omega", ShapeMismatch)
                ]
                (hotspotKey <$> topRestrictionHotspots 2 restrictionIndex),
      testCase "bounded hotspots equal the stable full-sort reference for every limit" $
        let restrictionIndex = restrictionIndexFromOutcomes differentialRestrictionOutcomes
            stableFullSort =
              sortOn
                (Down . rosOccurrences)
                (restrictionIndexStats restrictionIndex)
            limits = [-2 .. length stableFullSort + 2]
         in traverse_
              ( \limitValue ->
                  assertEqual
                    ("limit " <> show limitValue)
                    (take (max 0 limitValue) stableFullSort)
                    (topRestrictionHotspots limitValue restrictionIndex)
              )
              limits,
      testCase "hotspot benchmark matrix matches the stable full-sort reference" $
        traverse_
          assertHotspotMatrixAgreement
          hotspotBenchmarkScales
    ]

outcomeSummaryA :: OutcomeSummary Cell Mismatch ProjectionKey ProjectionValue ProjectionFailure Diagnostic
outcomeSummaryA =
  outcomeSummaryFromProjectionOutcome projectionAppliedA

outcomeSummaryB :: OutcomeSummary Cell Mismatch ProjectionKey ProjectionValue ProjectionFailure Diagnostic
outcomeSummaryB =
  outcomeSummaryFromProjectionOutcome projectionSkippedB

outcomeSummaryC :: OutcomeSummary Cell Mismatch ProjectionKey ProjectionValue ProjectionFailure Diagnostic
outcomeSummaryC =
  outcomeSummaryFromRestrictionOutcome restrictionC

projectionAppliedA :: ProjectionRunOutcome Cell ProjectionKey ProjectionValue ProjectionFailure Diagnostic
projectionAppliedA =
  ProjectionApplied
    "project-alpha"
    (Set.fromList ["alpha", "beta"])
    "projected"
    0.25
    (Seq.singleton "alpha adjusted")

projectionSkippedB :: ProjectionRunOutcome Cell ProjectionKey ProjectionValue ProjectionFailure Diagnostic
projectionSkippedB =
  ProjectionSkipped "project-beta" "already stable"

restrictionC :: RestrictionRunOutcome Cell Mismatch
restrictionC =
  RestrictionMismatch "beta" "omega" [ShapeMismatch]

knownRestrictionOutcomes :: [RestrictionRunOutcome Cell Mismatch]
knownRestrictionOutcomes =
  [ RestrictionMismatch "alpha" "omega" [PhaseMismatch, PhaseMismatch, PhaseMismatch],
    RestrictionMismatch "beta" "omega" [ShapeMismatch, ShapeMismatch],
    RestrictionMismatch "gamma" "omega" [ContextMismatch]
  ]

differentialRestrictionOutcomes :: [RestrictionRunOutcome Cell Mismatch]
differentialRestrictionOutcomes =
  [ RestrictionMismatch "alpha" "omega" [PhaseMismatch, PhaseMismatch],
    RestrictionMismatch "beta" "omega" [ShapeMismatch, ShapeMismatch, ShapeMismatch],
    RestrictionMismatch "gamma" "omega" [ContextMismatch, ContextMismatch, ContextMismatch],
    RestrictionMismatch "delta" "omega" [PhaseMismatch],
    RestrictionMismatch "epsilon" "omega" [ShapeMismatch, ShapeMismatch]
  ]

hotspotBenchmarkScales :: [(Int, [Int])]
hotspotBenchmarkScales =
  [ (2048, [1, 16, 45, 1023, 1024, 2048]),
    (16384, [1, 16, 128, 8191, 8192, 16384]),
    (65536, [1, 16, 256, 32767, 32768, 65536])
  ]

assertHotspotMatrixAgreement :: (Int, [Int]) -> IO ()
assertHotspotMatrixAgreement (uniqueAtomCount, hotspotCounts) =
  let restrictionIndex =
        restrictionIndexFromOutcomes
          (rankedRestrictionOutcomes uniqueAtomCount)
      stableFullSort =
        sortOn
          (Down . rosOccurrences)
          (restrictionIndexStats restrictionIndex)
   in traverse_
        ( \hotspotCount ->
            assertEqual
              ("K=" <> show uniqueAtomCount <> ", k=" <> show hotspotCount)
              (take hotspotCount stableFullSort)
              (topRestrictionHotspots hotspotCount restrictionIndex)
        )
        hotspotCounts

rankedRestrictionOutcomes :: Int -> [RestrictionRunOutcome Int Int]
rankedRestrictionOutcomes count =
  [ RestrictionMismatch
      index
      (index + 1)
      (replicate (1 + (index `mod` 7)) index)
    | index <- [1 .. count]
  ]

hotspotKey :: RestrictionOutcomeStat Cell Mismatch -> (Cell, Cell, Mismatch)
hotspotKey stat =
  (rosSourceCell stat, rosTargetCell stat, rosMismatch stat)

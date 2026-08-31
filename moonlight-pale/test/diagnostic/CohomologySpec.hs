module CohomologySpec
  ( tests,
  )
where

import Moonlight.Pale.Diagnostic.Views.Rewrite
  ( RewriteOutcomeSummary (..),
    summarizeSaturationTrace,
  )
import Moonlight.Pale.Diagnostic.Summary.Structural
  ( GrothendieckStructuralSummary (..),
    StructuralSummary (..),
  )
import Moonlight.Pale.Diagnostic.Local.Rewrite
  ( RewriteOutcomeStat (..),
    RuleTrace (..),
  )
import Moonlight.Pale.Diagnostic.Local.Saturation
  ( SaturationIterationTrace (..),
    SaturationTrace (..),
  )
import Moonlight.Pale.Diagnostic.Topology.Cohomology
  ( CoboundaryNilpotenceEvidence (..),
    evidenceNilpotent,
  )
import Moonlight.Pale.Diagnostic.Topology.Homotopy (NerveHomotopyProfile (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

data RuleId
  = RuleFold
  | RuleInline
  | RuleSimplify
  deriving stock (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "pale.diagnostic.cohomology"
    [ testCase "cohomology evidence distinguishes single-context nilpotence from multi-context obstruction" $ do
        assertEqual
          "single-context nilpotent constructor"
          SingleContextNilpotent
          knownSingleContextEvidence
        assertEqual
          "multi-context non-nilpotent constructor"
          MultiContextNonNilpotent
          knownMultiContextEvidence
        assertEqual
          "single-context nilpotence predicate"
          True
          (evidenceNilpotent knownSingleContextEvidence)
        assertEqual
          "multi-context obstruction predicate"
          False
          (evidenceNilpotent knownMultiContextEvidence),
      testCase "global structural summary folds cohomology and homotopy evidence into record shape" $
        assertEqual
          "structural summary shape"
          expectedStructuralSummary
          (structuralSummaryFromGrothendieck knownGrothendieckSummary),
      testCase "derived rewrite summary ranks worked trace structure" $
        assertEqual
          "rewrite rule rank"
          [RuleInline, RuleFold, RuleSimplify]
          (rosRuleId <$> rosRuleStats workedRewriteSummary)
    ]

knownSingleContextEvidence :: CoboundaryNilpotenceEvidence
knownSingleContextEvidence =
  SingleContextNilpotent

knownMultiContextEvidence :: CoboundaryNilpotenceEvidence
knownMultiContextEvidence =
  MultiContextNonNilpotent

knownHomotopyProfile :: NerveHomotopyProfile
knownHomotopyProfile =
  NerveHomotopyProfile
    { nhpConnectedComponents = 1,
      nhpBettiVector = [1, 0]
    }

knownGrothendieckSummary :: GrothendieckStructuralSummary
knownGrothendieckSummary =
  GrothendieckStructuralSummary
    { gssHomotopyProfile = knownHomotopyProfile,
      gssCellCount = 4,
      gssFaceCount = 3,
      gssObjectCount = 2,
      gssMorphismCount = 5,
      gssCrossContextMorphismCount = 2,
      gssVerticalMorphismCount = 2,
      gssDiagonalMorphismCount = 1,
      gssCoboundaryNilpotenceEvidence = knownSingleContextEvidence
    }

expectedStructuralSummary :: StructuralSummary
expectedStructuralSummary =
  StructuralSummary
    { ssConnectedComponents = 1,
      ssBettiNumbers = [1, 0],
      ssCellCount = 4,
      ssRestrictionCount = 5,
      ssCoboundaryNilpotent = True,
      ssMicrosupportSize = Just 2,
      ssCriticalCellCount = Just 2,
      ssNoncriticalFraction = Nothing
    }

structuralSummaryFromGrothendieck :: GrothendieckStructuralSummary -> StructuralSummary
structuralSummaryFromGrothendieck summary =
  StructuralSummary
    { ssConnectedComponents = nhpConnectedComponents (gssHomotopyProfile summary),
      ssBettiNumbers = nhpBettiVector (gssHomotopyProfile summary),
      ssCellCount = gssCellCount summary,
      ssRestrictionCount = gssMorphismCount summary,
      ssCoboundaryNilpotent = evidenceNilpotent (gssCoboundaryNilpotenceEvidence summary),
      ssMicrosupportSize = Just (gssObjectCount summary),
      ssCriticalCellCount = Just (gssCrossContextMorphismCount summary),
      ssNoncriticalFraction = Nothing
    }

workedTrace :: SaturationTrace RuleId
workedTrace =
  SaturationTrace
    { stIterations =
        [ firstIterationTrace,
          secondIterationTrace
        ]
    }

workedRewriteSummary :: RewriteOutcomeSummary RuleId
workedRewriteSummary =
  summarizeSaturationTrace workedTrace

firstIterationTrace :: SaturationIterationTrace RuleId
firstIterationTrace =
  SaturationIterationTrace
    { sitIteration = 0,
      sitNodeCountBefore = 2,
      sitNodeCountAfter = 4,
      sitBaseEligibleCount = 3,
      sitContextEligibleCount = 2,
      sitAggregatedEligibleCount = 3,
      sitGuidedCount = 2,
      sitScheduledCount = 4,
      sitFactsChanged = True,
      sitFactRoundCount = 1,
      sitContextRevision = 0,
      sitRuleTraces =
        [ foldTraceInitial,
          inlineTraceInitial
        ]
    }

secondIterationTrace :: SaturationIterationTrace RuleId
secondIterationTrace =
  SaturationIterationTrace
    { sitIteration = 1,
      sitNodeCountBefore = 4,
      sitNodeCountAfter = 5,
      sitBaseEligibleCount = 2,
      sitContextEligibleCount = 2,
      sitAggregatedEligibleCount = 2,
      sitGuidedCount = 1,
      sitScheduledCount = 4,
      sitFactsChanged = False,
      sitFactRoundCount = 2,
      sitContextRevision = 1,
      sitRuleTraces =
        [ inlineTraceFollowup,
          simplifyTraceFiltered,
          foldTraceBanned
        ]
    }

foldTraceInitial :: RuleTrace RuleId
foldTraceInitial =
  RuleTrace
    { rtRuleId = RuleFold,
      rtMatchedCount = 5,
      rtFilteredCount = 1,
      rtScheduledCount = 3,
      rtSkippedByScheduler = False,
      rtBannedUntil = Nothing
    }

inlineTraceInitial :: RuleTrace RuleId
inlineTraceInitial =
  RuleTrace
    { rtRuleId = RuleInline,
      rtMatchedCount = 2,
      rtFilteredCount = 1,
      rtScheduledCount = 1,
      rtSkippedByScheduler = False,
      rtBannedUntil = Nothing
    }

inlineTraceFollowup :: RuleTrace RuleId
inlineTraceFollowup =
  RuleTrace
    { rtRuleId = RuleInline,
      rtMatchedCount = 4,
      rtFilteredCount = 0,
      rtScheduledCount = 4,
      rtSkippedByScheduler = False,
      rtBannedUntil = Nothing
    }

simplifyTraceFiltered :: RuleTrace RuleId
simplifyTraceFiltered =
  RuleTrace
    { rtRuleId = RuleSimplify,
      rtMatchedCount = 3,
      rtFilteredCount = 3,
      rtScheduledCount = 0,
      rtSkippedByScheduler = False,
      rtBannedUntil = Nothing
    }

foldTraceBanned :: RuleTrace RuleId
foldTraceBanned =
  RuleTrace
    { rtRuleId = RuleFold,
      rtMatchedCount = 1,
      rtFilteredCount = 1,
      rtScheduledCount = 0,
      rtSkippedByScheduler = True,
      rtBannedUntil = Just 3
    }

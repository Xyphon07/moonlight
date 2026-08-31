-- @moonlight-pale:diagnostic@ workloads over deterministic repeated- and
-- distinct-cardinality mismatch corpora.  Every result is reduced through the
-- full ordered payload; a cheap length masquerading as semantic work is not a
-- benchmark.
module DiagnosticBench
  ( RestrictionCorpus,
    RestrictionDigest (..),
    repeatedRestrictionCorpus,
    distinctRestrictionCorpus,
    outcomeSummaryMconcat,
    outcomeSummaryLeftFold,
    outcomeSummaryBalanced,
    restrictionIndexStatsDigest,
    restrictionHotspotDigest,
    diagnosticBenchmarks,
  )
where

import BenchSupport (preparedBenchmarks)
import Control.DeepSeq (NFData (rnf))
import Data.List (sortOn)
import Data.Ord (Down (..))
import Moonlight.Pale.Diagnostic.Aggregation.Algebra
  ( OutcomeSummary,
    RestrictionIndex,
    outcomeSummaryFromRestrictionOutcome,
    outcomeSummaryRestrictionOutcomes,
    restrictionIndexFromOutcomes,
    restrictionIndexStats,
    restrictionIndexTotal,
    topRestrictionHotspots,
  )
import Moonlight.Pale.Diagnostic.Local.Propagation
  ( RestrictionOutcomeStat
      ( rosMismatch,
        rosOccurrences,
        rosSourceCell,
        rosTargetCell
      ),
    RestrictionRunOutcome (RestrictionMismatch),
  )
import Test.Tasty.Bench (Benchmark, bgroup)

diagnosticBenchmarks :: Benchmark
diagnosticBenchmarks =
  bgroup
    "diagnostic"
    ( fmap
        (\(regimeLabel, corpusFromSize) -> regimeBenchmarks regimeLabel corpusFromSize)
        restrictionRegimes
        <> [ bgroup
               "cardinality-matrix"
               [ bgroup
                   "cell-cardinality"
                   ( preparedBenchmarks
                       "cells"
                       cellCardinalityCorpora
                       restrictionIndexStatsDigest
                   ),
                 bgroup
                   "mismatch-cardinality"
                   ( preparedBenchmarks
                       "mismatches"
                       mismatchCardinalityCorpora
                       restrictionIndexStatsDigest
                   )
               ],
             bgroup
               "hotspot-k"
               (fmap (uncurry hotspotCardinalityBenchmarks) hotspotIndexScales)
           ]
    )

regimeBenchmarks :: String -> (Int -> RestrictionCorpus) -> Benchmark
regimeBenchmarks regimeLabel corpusFromSize =
  bgroup
    regimeLabel
    [ bgroup
        "outcome-summary-mconcat"
        (preparedBenchmarks "restrictions" preparedCorpora outcomeSummaryMconcat),
      bgroup
        "outcome-summary-left-fold"
        (preparedBenchmarks "restrictions" preparedCorpora outcomeSummaryLeftFold),
      bgroup
        "outcome-summary-balanced"
        (preparedBenchmarks "restrictions" preparedCorpora outcomeSummaryBalanced),
      bgroup
        "restriction-index-stats"
        (preparedBenchmarks "restrictions" preparedCorpora restrictionIndexStatsDigest),
      bgroup
        "restriction-hotspots-top-16"
        (preparedBenchmarks "restrictions" preparedCorpora restrictionHotspotDigest)
    ]
  where
    preparedCorpora =
      fmap (\size -> (size, corpusFromSize size)) restrictionSizes

restrictionSizes :: [Int]
restrictionSizes =
  [256, 2048, 16384]

restrictionRegimes :: [(String, Int -> RestrictionCorpus)]
restrictionRegimes =
  [ ("repeated-cardinality", repeatedRestrictionCorpus),
    ("distinct-cardinality", distinctRestrictionCorpus)
  ]

fixedCardinalityAtomCount :: Int
fixedCardinalityAtomCount =
  16384

cellCardinalityCorpora :: [(Int, RestrictionCorpus)]
cellCardinalityCorpora =
  fmap
    ( \cellCardinality ->
        ( cellCardinality,
          cardinalityRestrictionCorpus
            fixedCardinalityAtomCount
            cellCardinality
            8
        )
    )
    [16, 64, 256]

mismatchCardinalityCorpora :: [(Int, RestrictionCorpus)]
mismatchCardinalityCorpora =
  fmap
    ( \mismatchCardinality ->
        ( mismatchCardinality,
          cardinalityRestrictionCorpus
            fixedCardinalityAtomCount
            64
            mismatchCardinality
        )
    )
    [2, 16, 128]

hotspotIndexScales :: [(Int, [Int])]
hotspotIndexScales =
  [ (2048, [1, 16, 45, 1023, 1024, 2048]),
    (16384, [1, 16, 128, 8191, 8192, 16384]),
    (65536, [1, 16, 256, 32767, 32768, 65536])
  ]

hotspotCardinalityBenchmarks :: Int -> [Int] -> Benchmark
hotspotCardinalityBenchmarks uniqueAtomCount hotspotCounts =
  let preparedCorpora = hotspotKCorpora uniqueAtomCount hotspotCounts
   in bgroup
        ("unique-atoms/" <> show uniqueAtomCount)
        [ bgroup
            "production-hybrid"
            ( preparedBenchmarks
                "k"
                preparedCorpora
                restrictionPreparedHotspotDigest
            ),
          bgroup
            "full-sort-reference"
            ( preparedBenchmarks
                "k"
                preparedCorpora
                restrictionPreparedFullSortDigest
            )
        ]

hotspotKCorpora :: Int -> [Int] -> [(Int, RestrictionHotspotCase)]
hotspotKCorpora uniqueAtomCount hotspotCounts =
  let restrictionIndex =
        restrictionIndexFromOutcomes
          (restrictionCorpusOutcomes (rankedRestrictionCorpus uniqueAtomCount))
   in fmap
        ( \hotspotCount ->
            ( hotspotCount,
              RestrictionHotspotCase
                { restrictionHotspotCount = hotspotCount,
                  restrictionHotspotIndex = restrictionIndex
                }
            )
        )
        hotspotCounts

newtype RestrictionCorpus = RestrictionCorpus
  { restrictionCorpusOutcomes :: [RestrictionRunOutcome Int Int]
  }

instance NFData RestrictionCorpus where
  rnf =
    foldr
      (\outcome forcedTail -> forceRestrictionOutcome outcome `seq` forcedTail)
      ()
      . restrictionCorpusOutcomes

data RestrictionHotspotCase = RestrictionHotspotCase
  { restrictionHotspotCount :: !Int,
    restrictionHotspotIndex :: !(RestrictionIndex Int Int)
  }

instance NFData RestrictionHotspotCase where
  rnf hotspotCase =
    rnf (restrictionHotspotCount hotspotCase)
      `seq` forceRestrictionIndex (restrictionHotspotIndex hotspotCase)

data RestrictionDigest = RestrictionDigest
  { restrictionDigestValues :: !Int,
    restrictionDigestHash :: !Int
  }
  deriving stock (Eq, Show)

instance NFData RestrictionDigest where
  rnf (RestrictionDigest valueCount hashValue) =
    rnf valueCount `seq` rnf hashValue

forceRestrictionOutcome :: RestrictionRunOutcome Int Int -> ()
forceRestrictionOutcome (RestrictionMismatch sourceCell targetCell mismatches) =
  rnf sourceCell `seq` rnf targetCell `seq` rnf mismatches

forceRestrictionIndex :: RestrictionIndex Int Int -> ()
forceRestrictionIndex restrictionIndex =
  foldr
    (\statValue forcedTail -> forceRestrictionStat statValue `seq` forcedTail)
    (rnf (restrictionIndexTotal restrictionIndex))
    (restrictionIndexStats restrictionIndex)

forceRestrictionStat :: RestrictionOutcomeStat Int Int -> ()
forceRestrictionStat statValue =
  rnf (rosSourceCell statValue)
    `seq` rnf (rosTargetCell statValue)
    `seq` rnf (rosMismatch statValue)
    `seq` rnf (rosOccurrences statValue)

repeatedRestrictionCorpus :: Int -> RestrictionCorpus
repeatedRestrictionCorpus count =
  RestrictionCorpus
    [ RestrictionMismatch (index `mod` 64) ((index + 1) `mod` 64) [index `mod` 8]
      | index <- [1 .. count]
    ]

distinctRestrictionCorpus :: Int -> RestrictionCorpus
distinctRestrictionCorpus count =
  RestrictionCorpus
    [ RestrictionMismatch index (index + 1) [index]
      | index <- [1 .. count]
    ]

rankedRestrictionCorpus :: Int -> RestrictionCorpus
rankedRestrictionCorpus count =
  RestrictionCorpus
    [ RestrictionMismatch
        index
        (index + 1)
        (replicate (1 + (index `mod` 7)) index)
      | index <- [1 .. count]
    ]

cardinalityRestrictionCorpus :: Int -> Int -> Int -> RestrictionCorpus
cardinalityRestrictionCorpus atomCount cellCardinality mismatchCardinality =
  RestrictionCorpus
    [ RestrictionMismatch
        (index `mod` cellCardinality)
        ((index + 1) `mod` cellCardinality)
        [index `mod` mismatchCardinality]
      | index <- [1 .. atomCount]
    ]

outcomeSummaryMconcat :: RestrictionCorpus -> RestrictionDigest
outcomeSummaryMconcat corpus =
  outcomeSummaryDigest
    (mconcat (fmap liftRestrictionOutcome (restrictionCorpusOutcomes corpus)))

outcomeSummaryLeftFold :: RestrictionCorpus -> RestrictionDigest
outcomeSummaryLeftFold corpus =
  outcomeSummaryDigest
    (foldl' (<>) mempty (fmap liftRestrictionOutcome (restrictionCorpusOutcomes corpus)))

outcomeSummaryBalanced :: RestrictionCorpus -> RestrictionDigest
outcomeSummaryBalanced corpus =
  outcomeSummaryDigest
    ( balancedSummary
        (fmap liftRestrictionOutcome (restrictionCorpusOutcomes corpus))
    )

balancedSummary :: [OutcomeSummary Int Int () () () ()] -> OutcomeSummary Int Int () () () ()
balancedSummary = \case
  [] ->
    mempty
  [summaryValue] ->
    summaryValue
  summaryValues ->
    balancedSummary (pairAdjacentSummaries summaryValues)

pairAdjacentSummaries ::
  [OutcomeSummary Int Int () () () ()] ->
  [OutcomeSummary Int Int () () () ()]
pairAdjacentSummaries = \case
  [] ->
    []
  [summaryValue] ->
    [summaryValue]
  leftSummary : rightSummary : remainingSummaries ->
    (leftSummary <> rightSummary) : pairAdjacentSummaries remainingSummaries

liftRestrictionOutcome ::
  RestrictionRunOutcome Int Int ->
  OutcomeSummary Int Int () () () ()
liftRestrictionOutcome =
  outcomeSummaryFromRestrictionOutcome

outcomeSummaryDigest :: OutcomeSummary Int Int () () () () -> RestrictionDigest
outcomeSummaryDigest =
  foldl' restrictionOutcomeDigest emptyRestrictionDigest
    . outcomeSummaryRestrictionOutcomes

restrictionIndexStatsDigest :: RestrictionCorpus -> RestrictionDigest
restrictionIndexStatsDigest corpus =
  let restrictionIndex =
        restrictionIndexFromOutcomes (restrictionCorpusOutcomes corpus)
   in digestInt
        (foldl' restrictionStatDigest emptyRestrictionDigest (restrictionIndexStats restrictionIndex))
        (restrictionIndexTotal restrictionIndex)

restrictionHotspotDigest :: RestrictionCorpus -> RestrictionDigest
restrictionHotspotDigest corpus =
  restrictionHotspotDigestFor 16 corpus

restrictionHotspotDigestFor :: Int -> RestrictionCorpus -> RestrictionDigest
restrictionHotspotDigestFor hotspotCount corpus =
  foldl'
    restrictionStatDigest
    emptyRestrictionDigest
    ( topRestrictionHotspots
        hotspotCount
        (restrictionIndexFromOutcomes (restrictionCorpusOutcomes corpus))
    )

restrictionPreparedHotspotDigest :: RestrictionHotspotCase -> RestrictionDigest
restrictionPreparedHotspotDigest hotspotCase =
  foldl'
    restrictionStatDigest
    emptyRestrictionDigest
    ( topRestrictionHotspots
        (restrictionHotspotCount hotspotCase)
        (restrictionHotspotIndex hotspotCase)
    )

restrictionPreparedFullSortDigest :: RestrictionHotspotCase -> RestrictionDigest
restrictionPreparedFullSortDigest hotspotCase =
  foldl'
    restrictionStatDigest
    emptyRestrictionDigest
    ( take
        (restrictionHotspotCount hotspotCase)
        ( sortOn
            (Down . rosOccurrences)
            (restrictionIndexStats (restrictionHotspotIndex hotspotCase))
        )
    )

restrictionOutcomeDigest :: RestrictionDigest -> RestrictionRunOutcome Int Int -> RestrictionDigest
restrictionOutcomeDigest digest (RestrictionMismatch sourceCell targetCell mismatches) =
  foldl'
    digestInt
    (digestInt (digestInt digest sourceCell) targetCell)
    mismatches

restrictionStatDigest :: RestrictionDigest -> RestrictionOutcomeStat Int Int -> RestrictionDigest
restrictionStatDigest digest stat =
  digestInt
    ( digestInt
        (digestInt (digestInt digest (rosSourceCell stat)) (rosTargetCell stat))
        (rosMismatch stat)
    )
    (rosOccurrences stat)

emptyRestrictionDigest :: RestrictionDigest
emptyRestrictionDigest =
  RestrictionDigest
    { restrictionDigestValues = 0,
      restrictionDigestHash = 2166136261
    }

digestInt :: RestrictionDigest -> Int -> RestrictionDigest
digestInt digest value =
  RestrictionDigest
    { restrictionDigestValues = restrictionDigestValues digest + 1,
      restrictionDigestHash =
        (restrictionDigestHash digest * 16777619) + value
    }

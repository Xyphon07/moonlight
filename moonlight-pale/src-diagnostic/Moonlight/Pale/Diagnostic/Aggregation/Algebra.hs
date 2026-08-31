{-# LANGUAGE DerivingStrategies #-}

{-| Monoidal summaries and hotspot indexes over local outcomes. -}
module Moonlight.Pale.Diagnostic.Aggregation.Algebra
  ( OutcomeSummary,
    outcomeSummaryDiagnostics,
    outcomeSummaryProjectionOutcomes,
    outcomeSummaryRestrictionOutcomes,
    RestrictionIndex,
    projectionOutcomeChangedCells,
    projectionOutcomeResidual,
    projectionOutcomeDiagnostics,
    outcomeSummaryFromProjectionOutcome,
    outcomeSummaryFromRestrictionOutcome,
    outcomeSummaryChangedCells,
    outcomeSummaryResidual,
    restrictionIndexFromOutcomes,
    restrictionIndexStats,
    restrictionIndexByMismatch,
    restrictionIndexByCell,
    restrictionIndexTotal,
    topRestrictionHotspots,
  )
where

import Data.Function ((&))
import Data.Foldable (Foldable, foldl')
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Pale.Diagnostic.Local.Propagation
  ( ProjectionRunOutcome,
    RestrictionOutcomeStat (..),
    RestrictionRunOutcome (..),
    foldProjectionOutcome,
  )
import Prelude
  ( Bool (True),
    Double,
    Eq,
    Int,
    Maybe (Just, Nothing),
    Monoid (mempty),
    Ord,
    Semigroup ((<>)),
    Show,
    all,
    fmap,
    foldMap,
    length,
    max,
    maybe,
    otherwise,
    take,
    zip,
    (+),
    (-),
    (.),
    (<),
    (>=),
    (==),
    (||),
  )

type OutcomeSummary :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data OutcomeSummary cell mismatch key outcome failure diagnostic = OutcomeSummary
  { outcomeSummaryDiagnostics :: !(Seq diagnostic),
    outcomeSummaryProjectionOutcomes :: !(Seq (ProjectionRunOutcome cell key outcome failure diagnostic)),
    outcomeSummaryRestrictionOutcomes :: !(Seq (RestrictionRunOutcome cell mismatch))
  }
  deriving stock (Eq, Show)

instance Semigroup (OutcomeSummary cell mismatch key outcome failure diagnostic) where
  leftSummary <> rightSummary =
    OutcomeSummary
      { outcomeSummaryDiagnostics =
          outcomeSummaryDiagnostics leftSummary
            <> outcomeSummaryDiagnostics rightSummary,
        outcomeSummaryProjectionOutcomes =
          outcomeSummaryProjectionOutcomes leftSummary
            <> outcomeSummaryProjectionOutcomes rightSummary,
        outcomeSummaryRestrictionOutcomes =
          outcomeSummaryRestrictionOutcomes leftSummary
            <> outcomeSummaryRestrictionOutcomes rightSummary
      }

instance Monoid (OutcomeSummary cell mismatch key outcome failure diagnostic) where
  mempty =
    OutcomeSummary
      { outcomeSummaryDiagnostics = Seq.empty,
        outcomeSummaryProjectionOutcomes = Seq.empty,
        outcomeSummaryRestrictionOutcomes = Seq.empty
      }

type RestrictionIndex :: Type -> Type -> Type
newtype RestrictionIndex cell mismatch = RestrictionIndex
  { restrictionAtomCounts :: Map (cell, cell, mismatch) Int
  }
  deriving stock (Eq, Show)

instance (Ord cell, Ord mismatch) => Semigroup (RestrictionIndex cell mismatch) where
  leftIndex <> rightIndex =
    RestrictionIndex
      ( Map.unionWith
          (+)
          (restrictionAtomCounts leftIndex)
          (restrictionAtomCounts rightIndex)
      )

instance (Ord cell, Ord mismatch) => Monoid (RestrictionIndex cell mismatch) where
  mempty = RestrictionIndex Map.empty

projectionOutcomeChangedCells :: ProjectionRunOutcome cell key outcome failure diagnostic -> Set cell
projectionOutcomeChangedCells =
  foldProjectionOutcome (\_ cells _ _ _ -> cells) (\_ _ -> Set.empty) (\_ _ -> Set.empty)

projectionOutcomeResidual :: ProjectionRunOutcome cell key outcome failure diagnostic -> Maybe Double
projectionOutcomeResidual =
  foldProjectionOutcome (\_ _ _ residual _ -> Just residual) (\_ _ -> Nothing) (\_ _ -> Nothing)

projectionOutcomeDiagnostics :: ProjectionRunOutcome cell key outcome failure diagnostic -> Seq diagnostic
projectionOutcomeDiagnostics =
  foldProjectionOutcome (\_ _ _ _ diagnostics -> diagnostics) (\_ _ -> Seq.empty) (\_ _ -> Seq.empty)

outcomeSummaryFromProjectionOutcome :: ProjectionRunOutcome cell key outcome failure diagnostic -> OutcomeSummary cell mismatch key outcome failure diagnostic
outcomeSummaryFromProjectionOutcome outcome =
  OutcomeSummary
    { outcomeSummaryDiagnostics = projectionOutcomeDiagnostics outcome,
      outcomeSummaryProjectionOutcomes = Seq.singleton outcome,
      outcomeSummaryRestrictionOutcomes = Seq.empty
    }

outcomeSummaryFromRestrictionOutcome :: RestrictionRunOutcome cell mismatch -> OutcomeSummary cell mismatch key outcome failure diagnostic
outcomeSummaryFromRestrictionOutcome outcome =
  OutcomeSummary
    { outcomeSummaryDiagnostics = Seq.empty,
      outcomeSummaryProjectionOutcomes = Seq.empty,
      outcomeSummaryRestrictionOutcomes = Seq.singleton outcome
    }

outcomeSummaryChangedCells :: Ord cell => OutcomeSummary cell mismatch key outcome failure diagnostic -> Set cell
outcomeSummaryChangedCells summary =
  outcomeSummaryProjectionOutcomes summary
    & foldMap projectionOutcomeChangedCells

outcomeSummaryResidual :: OutcomeSummary cell mismatch key outcome failure diagnostic -> Double
outcomeSummaryResidual summary =
  foldl'
    (\residualTotal outcome -> maybe residualTotal (residualTotal +) (projectionOutcomeResidual outcome))
    0
    (outcomeSummaryProjectionOutcomes summary)

restrictionIndexFromOutcomes ::
  (Foldable collection, Ord cell, Ord mismatch) =>
  collection (RestrictionRunOutcome cell mismatch) ->
  RestrictionIndex cell mismatch
restrictionIndexFromOutcomes outcomes =
  RestrictionIndex
    (foldl' insertRestrictionOutcomeAtoms Map.empty outcomes)
{-# INLINE restrictionIndexFromOutcomes #-}

insertRestrictionOutcomeAtoms ::
  (Ord cell, Ord mismatch) =>
  Map (cell, cell, mismatch) Int ->
  RestrictionRunOutcome cell mismatch ->
  Map (cell, cell, mismatch) Int
insertRestrictionOutcomeAtoms atomCounts (RestrictionMismatch sourceCell targetCell mismatches) =
  foldl'
    (\accumulatedCounts mismatch -> Map.insertWith (+) (sourceCell, targetCell, mismatch) 1 accumulatedCounts)
    atomCounts
    mismatches

restrictionIndexStats :: RestrictionIndex cell mismatch -> [RestrictionOutcomeStat cell mismatch]
restrictionIndexStats indexValue =
  Map.foldrWithKey
    ( \(sourceCell, targetCell, mismatch) occurrences remainingStats ->
        RestrictionOutcomeStat
          { rosSourceCell = sourceCell,
            rosTargetCell = targetCell,
            rosMismatch = mismatch,
            rosOccurrences = occurrences
          }
          : remainingStats
    )
    []
    (restrictionAtomCounts indexValue)
{-# INLINE restrictionIndexStats #-}

restrictionIndexByMismatch :: Ord mismatch => RestrictionIndex cell mismatch -> Map mismatch Int
restrictionIndexByMismatch indexValue =
  Map.foldlWithKey'
    (\counts (_, _, mismatch) occurrences -> Map.insertWith (+) mismatch occurrences counts)
    Map.empty
    (restrictionAtomCounts indexValue)

restrictionIndexByCell :: Ord cell => RestrictionIndex cell mismatch -> Map cell Int
restrictionIndexByCell indexValue =
  Map.foldlWithKey'
    insertCellCounts
    Map.empty
    (restrictionAtomCounts indexValue)

insertCellCounts ::
  Ord cell =>
  Map cell Int ->
  (cell, cell, mismatch) ->
  Int ->
  Map cell Int
insertCellCounts counts (sourceCell, targetCell, _) occurrences =
  let withSource = Map.insertWith (+) sourceCell occurrences counts
   in if sourceCell == targetCell
        then withSource
        else Map.insertWith (+) targetCell occurrences withSource

restrictionIndexTotal :: RestrictionIndex cell mismatch -> Int
restrictionIndexTotal =
  Map.foldl' (+) 0 . restrictionAtomCounts

topRestrictionHotspots :: Int -> RestrictionIndex cell mismatch -> [RestrictionOutcomeStat cell mismatch]
topRestrictionHotspots limitValue indexValue
  | boundedLimit == 0 =
      []
  | restrictionStatsHaveUniformOccurrences stats =
      take boundedLimit stats
  | boundedLimit >= statCount
      || boundedLimit >= statCount - boundedLimit =
      stats
        & sortOn (Down . rosOccurrences)
        & take boundedLimit
  | otherwise =
      foldl'
        (retainHotspot boundedLimit)
        Map.empty
        (zip [0 ..] stats)
        & Map.toDescList
        & fmap (\(_, statValue) -> statValue)
  where
    stats = restrictionIndexStats indexValue
    boundedLimit = max 0 limitValue
    statCount = length stats

restrictionStatsHaveUniformOccurrences :: [RestrictionOutcomeStat cell mismatch] -> Bool
restrictionStatsHaveUniformOccurrences stats =
  case stats of
    [] ->
      True
    firstStat : remainingStats ->
      all
        ((== rosOccurrences firstStat) . rosOccurrences)
        remainingStats

retainHotspot ::
  Int ->
  Map (Int, Down Int) (RestrictionOutcomeStat cell mismatch) ->
  (Int, RestrictionOutcomeStat cell mismatch) ->
  Map (Int, Down Int) (RestrictionOutcomeStat cell mismatch)
retainHotspot retainedLimit retainedStats (ordinal, statValue)
  | retainedLimit == 0 =
      Map.empty
  | otherwise =
      let rank = (rosOccurrences statValue, Down ordinal)
       in if Map.size retainedStats < retainedLimit
            then Map.insert rank statValue retainedStats
            else
              case Map.lookupMin retainedStats of
                Just (lowestRetainedRank, _)
                  | lowestRetainedRank < rank ->
                      Map.insert rank statValue (Map.deleteMin retainedStats)
                _ ->
                  retainedStats

{-# LANGUAGE DerivingStrategies #-}

{-| Whole-run propagation traces, summaries, and reports. -}
module Moonlight.Pale.Diagnostic.Aggregation.Propagation
  ( PropagationFailure (..),
    PropagationTrace (..),
    PropagationSummary (..),
    PropagationReport (..),
    traceProjectionOutcomes,
    traceRestrictionOutcomes,
    filterReportDiagnostics,
    reportTotalMismatches,
  )
where

import Data.Foldable (foldMap)
import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Moonlight.Pale.Diagnostic.Aggregation.Algebra
  ( RestrictionIndex,
    restrictionIndexTotal,
  )
import Moonlight.Pale.Diagnostic.Local.Propagation
  ( IterationTrace (..),
    ProjectionRunOutcome,
    RestrictionRunOutcome,
  )
import Prelude (Bool, Double, Eq, Int, Show, String, (.))

type PropagationFailure :: Type -> Type -> Type
data PropagationFailure key failure
  = PropagationIterationExceeded Int
  | PropagationInvariantViolation String
  | PropagationProjectionFailure key failure
  deriving stock (Eq, Show)

type PropagationTrace :: Type -> Type -> Type -> Type -> Type -> Type -> Type
newtype PropagationTrace cell mismatch key outcome failure diagnostic = PropagationTrace
  { traceIterations :: Seq (IterationTrace cell mismatch key outcome failure diagnostic)
  }
  deriving stock (Eq, Show)

type PropagationSummary :: Type -> Type -> Type -> Type
data PropagationSummary cell mismatch diagnostic = PropagationSummary
  { summaryChangedCells :: !(Set cell),
    summaryIterationCount :: !Int,
    summaryConverged :: !Bool,
    summaryTotalCellsProcessed :: !Int,
    summaryResidualEnergy :: !Double,
    summaryDiagnostics :: !(Seq diagnostic),
    summaryRestrictionIndex :: !(RestrictionIndex cell mismatch)
  }
  deriving stock (Eq, Show)

type PropagationReport :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data PropagationReport cell mismatch key outcome failure diagnostic = PropagationReport
  { propagationSummary :: !(PropagationSummary cell mismatch diagnostic),
    propagationTrace :: !(PropagationTrace cell mismatch key outcome failure diagnostic)
  }
  deriving stock (Eq, Show)

traceProjectionOutcomes ::
  PropagationTrace cell mismatch key outcome failure diagnostic ->
  Seq (ProjectionRunOutcome cell key outcome failure diagnostic)
traceProjectionOutcomes =
  foldMap itProjectionOutcomes . traceIterations

traceRestrictionOutcomes ::
  PropagationTrace cell mismatch key outcome failure diagnostic ->
  Seq (RestrictionRunOutcome cell mismatch)
traceRestrictionOutcomes =
  foldMap itRestrictionOutcomes . traceIterations

filterReportDiagnostics ::
  (diagnostic -> Bool) ->
  PropagationReport cell mismatch key outcome failure diagnostic ->
  PropagationReport cell mismatch key outcome failure diagnostic
filterReportDiagnostics predicate report =
  report
    { propagationSummary =
        (propagationSummary report)
          { summaryDiagnostics =
              Seq.filter predicate (summaryDiagnostics (propagationSummary report))
          }
    }

reportTotalMismatches :: PropagationReport cell mismatch key outcome failure diagnostic -> Int
reportTotalMismatches =
  restrictionIndexTotal . summaryRestrictionIndex . propagationSummary

{-# LANGUAGE DerivingStrategies #-}

{-| Per-projection and per-restriction propagation outcomes. -}
module Moonlight.Pale.Diagnostic.Local.Propagation
  ( ProjectionRunOutcome (..),
    foldProjectionOutcome,
    RestrictionRunOutcome (..),
    RestrictionOutcomeStat (..),
    IterationTrace (..),
  )
where

import Data.Kind (Type)
import Data.Sequence (Seq)
import Data.Set (Set)
import Prelude (Double, Eq, Int, Show, String)

type ProjectionRunOutcome :: Type -> Type -> Type -> Type -> Type -> Type
data ProjectionRunOutcome cell key outcome failure diagnostic
  = ProjectionApplied key (Set cell) outcome Double (Seq diagnostic)
  | ProjectionSkipped key String
  | ProjectionFailed key failure
  deriving stock (Eq, Show)

foldProjectionOutcome ::
  (key -> Set cell -> outcome -> Double -> Seq diagnostic -> r) ->
  (key -> String -> r) ->
  (key -> failure -> r) ->
  ProjectionRunOutcome cell key outcome failure diagnostic ->
  r
foldProjectionOutcome applied skipped failed outcome =
  case outcome of
    ProjectionApplied key changedCells result residual diagnostics ->
      applied key changedCells result residual diagnostics
    ProjectionSkipped key reason ->
      skipped key reason
    ProjectionFailed key failure ->
      failed key failure

type RestrictionRunOutcome :: Type -> Type -> Type
data RestrictionRunOutcome cell mismatch
  = RestrictionMismatch cell cell [mismatch]
  deriving stock (Eq, Show)

type RestrictionOutcomeStat :: Type -> Type -> Type
data RestrictionOutcomeStat cell mismatch = RestrictionOutcomeStat
  { rosSourceCell :: cell,
    rosTargetCell :: cell,
    rosMismatch :: mismatch,
    rosOccurrences :: Int
  }
  deriving stock (Eq, Show)

type IterationTrace :: Type -> Type -> Type -> Type -> Type -> Type -> Type
data IterationTrace cell mismatch key outcome failure diagnostic = IterationTrace
  { itIterationIndex :: !Int,
    itFrontierSize :: !Int,
    itChangedCells :: !(Set cell),
    itResidualEnergy :: !Double,
    itProjectionOutcomes :: !(Seq (ProjectionRunOutcome cell key outcome failure diagnostic)),
    itRestrictionOutcomes :: !(Seq (RestrictionRunOutcome cell mismatch))
  }
  deriving stock (Eq, Show)

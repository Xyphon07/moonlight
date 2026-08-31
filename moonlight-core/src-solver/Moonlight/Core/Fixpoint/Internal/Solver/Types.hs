-- | Solver carrier types: equation ids, evaluation programs, delta domains, plans,
-- obstructions, snapshots, and results. Pure data; no mutation.
module Moonlight.Core.Fixpoint.Internal.Solver.Types
  ( EquationId (..),
    equationIdKey,
    Evaluation (..),
    EquationRead (..),
    readEquationValue,
    evaluationInputs,
    DeltaDomain (..),
    Equation (..),
    ConvergencePlan (..),
    WideningPolicy (..),
    Plan (..),
    Obstruction (..),
    Snapshot (..),
    Result (..),
    Component (..),
    OutputUpdate,
  )
where

import Control.Applicative.Free (Ap, liftAp, runAp_)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector (Vector)
import Prelude

newtype EquationId = EquationId {unEquationId :: Int}
  deriving stock (Eq, Ord, Show, Read)

equationIdKey :: EquationId -> Int
equationIdKey (EquationId key) =
  key

data EquationRead value result where
  ReadEquationValue :: !EquationId -> EquationRead value value

newtype Evaluation value result = Evaluation
  { evaluationProgram :: Ap (EquationRead value) result
  }

instance Functor (Evaluation value) where
  fmap project (Evaluation program) =
    Evaluation (fmap project program)

instance Applicative (Evaluation value) where
  pure =
    Evaluation . pure
  Evaluation function <*> Evaluation argument =
    Evaluation (function <*> argument)

readEquationValue :: EquationId -> Evaluation value value
readEquationValue =
  Evaluation . liftAp . ReadEquationValue

evaluationInputs :: Evaluation value result -> IntSet
evaluationInputs (Evaluation program) =
  runAp_ equationReadInput program

equationReadInput :: EquationRead value result -> IntSet
equationReadInput (ReadEquationValue equationId) =
  IntSet.singleton (equationIdKey equationId)

data DeltaDomain value delta = DeltaDomain
  { deltaEmpty :: !delta,
    deltaNull :: delta -> Bool,
    deltaMerge :: delta -> delta -> delta,
    deltaApply :: delta -> value -> value,
    deltaBetween :: value -> value -> delta
  }

data Equation value delta = Equation
  { equationOutput :: !EquationId,
    evaluateFull :: !(Evaluation value value),
    evaluateDelta :: !(Maybe (EquationId -> delta -> delta))
  }

data ConvergencePlan value
  = FiniteHeightScc
  | Widening !(WideningPolicy value)

data WideningPolicy value = WideningPolicy
  { wideningHeads :: !IntSet,
    widenAt :: !(Int -> value -> value -> value),
    narrowAt :: !(Int -> value -> value -> value)
  }

data Plan value delta = Plan
  { valueCount :: !Int,
    convergencePlan :: !(ConvergencePlan value),
    components :: ![Component],
    equationsByOutput :: !(IntMap [Equation value delta]),
    usersByInput :: !(IntMap [Equation value delta])
  }

data Obstruction
  = NegativeEquationId !EquationId
  | EquationIdExceedsCapacity !EquationId !Int
  | SnapshotSizeMismatch !Int !Int
  | DeltaOutOfBounds !EquationId !Int
  deriving stock (Eq, Show)

data Snapshot value delta = Snapshot
  { snapshotValues :: !(Vector value)
  }

data Result value delta = Result
  { resultValues :: !(Vector value),
    resultSnapshot :: !(Snapshot value delta)
  }

data Component
  = AcyclicOutput !EquationId
  | CyclicOutputs !IntSet

type OutputUpdate value = Int -> value -> value -> value

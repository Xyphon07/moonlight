-- | Bounded fixpoint iteration, worklist scheduling, transitive closure, and a
-- monotone dataflow solver over an abstract delta domain.
--
-- This module is a curated re-export surface. The pure iteration combinators
-- live in "Moonlight.Core.Fixpoint.Internal.Combinators"; the solver's carrier
-- types, plan construction, mutable arena/queue machinery, and evaluation
-- engine live under "Moonlight.Core.Fixpoint.Internal.Solver". 'Evaluation'
-- and 'Plan' are exported abstractly: equation reads are declared by the same
-- applicative program that the arena interprets.
module Moonlight.Core.Fixpoint
  ( FixpointDivergence (..),
    fixpointBounded,
    fixpointBoundedM,
    worklistFold,
    traverseOnceIntSet,
    reschedulingWorklistFoldIntSet,
    EquationId (..),
    Evaluation,
    readEquationValue,
    DeltaDomain (..),
    Equation (..),
    ConvergencePlan (..),
    WideningPolicy (..),
    Plan,
    Obstruction (..),
    Snapshot (..),
    Result (..),
    planFromEquations,
    planWithConvergenceFromEquations,
    solveMonotone,
    solveDenseMonotone,
    solveIncremental,
    closureUnder,
    closureUnderInt,
    reachabilityFrom,
    reachabilityFromInt,
  )
where

import Moonlight.Core.Fixpoint.Internal.Combinators
  ( FixpointDivergence (..),
    closureUnder,
    closureUnderInt,
    fixpointBounded,
    fixpointBoundedM,
    reachabilityFrom,
    reachabilityFromInt,
    reschedulingWorklistFoldIntSet,
    traverseOnceIntSet,
    worklistFold,
  )
import Moonlight.Core.Fixpoint.Internal.Solver.Engine
  ( solveDenseMonotone,
    solveIncremental,
    solveMonotone,
  )
import Moonlight.Core.Fixpoint.Internal.Solver.Plan
  ( planFromEquations,
    planWithConvergenceFromEquations,
  )
import Moonlight.Core.Fixpoint.Internal.Solver.Types
  ( ConvergencePlan (..),
    DeltaDomain (..),
    Equation (..),
    EquationId (..),
    Evaluation,
    Obstruction (..),
    Plan,
    Result (..),
    Snapshot (..),
    WideningPolicy (..),
    readEquationValue,
  )

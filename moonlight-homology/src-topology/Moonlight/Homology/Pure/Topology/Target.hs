module Moonlight.Homology.Pure.Topology.Target
  ( TopologyTarget (..),
    TargetViolation (..),
    topologyTargetConstraints,
    validateTarget,
    validateTargets,
    validateTopologyTarget,
    validateTopologyTargets,
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Chain (TopologyWitness)
import Moonlight.Homology.Pure.Constraint.Algebra (evaluateTopologicalConstraints)
import Moonlight.Homology.Pure.Constraint.Core
  ( EulerBound,
    HarmonicLoopBudget,
    PersistenceBudget,
    SingularityBudget,
    SkeletonAdherence,
    TargetBetti,
    TopologicalConstraint
      ( EulerBoundConstraint,
        HarmonicLoopBudgetConstraint,
        PersistenceBudgetConstraint,
        SingularityBudgetConstraint,
        SkeletonAdherenceConstraint,
        TargetBettiConstraint
      ),
    TopologicalViolation,
  )
import Moonlight.Homology.Pure.Filtration (FiltrationValue)
import Moonlight.Homology.Pure.Topology.MacroScaffold (MacroScaffoldIR)

type TopologyTarget :: Type
data TopologyTarget
  = EulerTarget EulerBound
  | BettiTarget TargetBetti
  | PersistenceTarget PersistenceBudget
  | SingularityTarget SingularityBudget
  | HarmonicLoopTarget HarmonicLoopBudget
  | SkeletonTarget SkeletonAdherence
  deriving stock (Eq, Show)

type TargetViolation :: Type
data TargetViolation = TargetViolation
  { violatedTarget :: TopologyTarget,
    targetViolationCause :: TopologicalViolation
  }
  deriving stock (Eq, Show)

topologyTargetConstraints :: TopologyTarget -> [TopologicalConstraint]
topologyTargetConstraints targetValue =
  case targetValue of
    EulerTarget boundValue -> [EulerBoundConstraint boundValue]
    BettiTarget bettiValue -> [TargetBettiConstraint bettiValue]
    PersistenceTarget budgetValue -> [PersistenceBudgetConstraint budgetValue]
    SingularityTarget budgetValue -> [SingularityBudgetConstraint budgetValue]
    HarmonicLoopTarget budgetValue -> [HarmonicLoopBudgetConstraint budgetValue]
    SkeletonTarget adherenceValue -> [SkeletonAdherenceConstraint adherenceValue]

validateTarget ::
  TopologyTarget ->
  TopologyWitness MacroScaffoldIR spectral FiltrationValue coefficient basis ->
  Either TargetViolation ()
validateTarget targetValue witnessValue =
  validateTopologyTarget witnessValue targetValue

validateTargets ::
  [TopologyTarget] ->
  TopologyWitness MacroScaffoldIR spectral FiltrationValue coefficient basis ->
  Either [TargetViolation] ()
validateTargets targetValues witnessValue =
  validateTopologyTargets witnessValue targetValues

validateTopologyTarget ::
  TopologyWitness MacroScaffoldIR spectral FiltrationValue coefficient basis ->
  TopologyTarget ->
  Either TargetViolation ()
validateTopologyTarget witnessValue targetValue =
  case violationsForTarget witnessValue targetValue of
    [] -> Right ()
    violationValue : _ -> Left violationValue

validateTopologyTargets ::
  TopologyWitness MacroScaffoldIR spectral FiltrationValue coefficient basis ->
  [TopologyTarget] ->
  Either [TargetViolation] ()
validateTopologyTargets witnessValue targetValues =
  case targetValues >>= violationsForTarget witnessValue of
    [] -> Right ()
    violationValues -> Left violationValues

violationsForTarget ::
  TopologyWitness MacroScaffoldIR spectral FiltrationValue coefficient basis ->
  TopologyTarget ->
  [TargetViolation]
violationsForTarget witnessValue targetValue =
  evaluateTopologicalConstraints witnessValue (topologyTargetConstraints targetValue)
    >>= (\violationValue -> [TargetViolation targetValue violationValue])

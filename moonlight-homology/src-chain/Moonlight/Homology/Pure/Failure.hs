module Moonlight.Homology.Pure.Failure
  ( HomologyLaw (..),
    NonEffectiveCause (..),
    TopologyInputObstruction (..),
    HomologyFailure (..),
  )
where

import Data.Kind (Type)
import Moonlight.Homology.Pure.Carrier (BasisCellRef)

type HomologyLaw :: Type
data HomologyLaw
  = ChainNilpotenceLaw
  | ReductionLeftInverseLaw
  | ReductionHomotopyLaw
  | ReductionProjectionChainMapLaw
  | ReductionInclusionChainMapLaw
  | IncidenceScopeLaw
  | DeterminismLaw
  deriving stock (Eq, Show)

type NonEffectiveCause :: Type
data NonEffectiveCause
  = MissingFiniteReduction
  | UnsupportedInfiniteCarrier
  | MissingConvergenceWitness
  deriving stock (Eq, Show)

type TopologyInputObstruction :: Type
data TopologyInputObstruction
  = TopologyObjectAbsent
  | TopologyStrongComponentAbsent !Int
  | TopologyUpperSetAbsent !Int
  | TopologyRankAbsent !Int
  | TopologyGeneratedDegreeAbsent !Int
  | TopologyGeneratedFaceAbsent !Int ![Int]
  | TopologyGeneratedChainIndexCollision !Int
  | TopologyGeneratedEmptyChain !Int
  | TopologyDuplicateCells !Int !Int
  | TopologyBasisCardinalityMismatch !Int !Int
  deriving stock (Eq, Show)

type HomologyFailure :: Type
data HomologyFailure
  = NonConvergent Int
  | BudgetExceeded Int Int
  | NonEffective NonEffectiveCause
  | LawViolation HomologyLaw
  | InvalidBoundaryIncidence String
  | InvalidMatrixShape String
  | InvalidTopologyInput String
  | TopologyInputRejected TopologyInputObstruction
  | ChainComplexShapeMismatch Int Int Int
  | ChainComplexNilpotenceViolation Int
  | MissingCriticalBasisProvenance BasisCellRef
  | FiltrationIncompatibleMorsePair BasisCellRef BasisCellRef Int Int
  | FiltrationNotPreserved BasisCellRef BasisCellRef Int Int
  | SpectralQuotientDenominatorNotSubspace (Int, Int) Int [Rational]
  | BackendFailure String
  deriving stock (Eq, Show)

module Moonlight.Homology.Pure.Constraint.Core
  ( Bound (..),
    TargetBetti (..),
    PersistenceBudget (..),
    EulerBound (..),
    LoopSemanticRole (..),
    LoopRole (..),
    RequireTorsionInvariant (..),
    RequireElementOrder (..),
    RequireOrderSupport (..),
    PrimaryOrderSupportBudget (..),
    RequirePrimaryOrderSupport (..),
    TorsionBudgetMeasure (..),
    TorsionBudget (..),
    RequireCyclicOrder (..),
    SingularityBudget (..),
    HarmonicLoopBudget (..),
    SkeletonAdherence (..),
    TopologicalConstraint (..),
    TopologicalViolation (..),
    boundSatisfied,
    checkBound,
  )
where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Moonlight.Homology.Pure.Chain
  ( EulerCharacteristic (..),
    HomologicalDegree (..),
  )
import Moonlight.Homology.Pure.Filtration
  ( CriticalKind (..),
    FiltrationValue (..),
  )
import Moonlight.Homology.Pure.Skeleton (SkeletonSignature (..))

type Bound :: Type -> Type
data Bound a
  = Exactly a
  | AtLeast a
  | AtMost a
  | Between a a
  deriving stock (Eq, Ord, Show, Read)

type TargetBetti :: Type
newtype TargetBetti = TargetBetti
  { targetBettiVector :: [Int]
  }
  deriving stock (Eq, Show, Read)

type PersistenceBudget :: Type
data PersistenceBudget = PersistenceBudget
  { persistenceBudgetDegree :: Maybe HomologicalDegree,
    persistenceBudgetMinimumLifetime :: FiltrationValue,
    persistenceBudgetCountBound :: Bound Int
  }
  deriving stock (Eq, Show, Read)

type EulerBound :: Type
newtype EulerBound = EulerBound
  { requiredEulerBound :: Bound Int
  }
  deriving stock (Eq, Show, Read)

type LoopSemanticRole :: Type
data LoopSemanticRole
  = CirculationLoop
  | StructuralLoop
  | OrnamentLoop
  | AccessLoop
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

type LoopRole :: Type
data LoopRole = LoopRole
  { loopSemanticRole :: LoopSemanticRole,
    loopTargetDegree :: HomologicalDegree,
    loopCountBound :: Bound Int
  }
  deriving stock (Eq, Show, Read)

type RequireTorsionInvariant :: Type
data RequireTorsionInvariant = RequireTorsionInvariant
  { requiredTorsionDegree :: HomologicalDegree,
    requiredTorsionInvariant :: Integer,
    requiredTorsionMultiplicity :: Bound Int
  }
  deriving stock (Eq, Show, Read)

type RequireElementOrder :: Type
data RequireElementOrder = RequireElementOrder
  { requiredElementDegree :: HomologicalDegree,
    requiredElementOrder :: Integer,
    requiredElementMultiplicity :: Bound Integer
  }
  deriving stock (Eq, Show, Read)

type RequireOrderSupport :: Type
data RequireOrderSupport = RequireOrderSupport
  { requiredOrderSupportDegree :: Maybe HomologicalDegree,
    requiredSupportedOrders :: [Integer],
    requiredForbiddenOrders :: [Integer]
  }
  deriving stock (Eq, Show, Read)

type PrimaryOrderSupportBudget :: Type
data PrimaryOrderSupportBudget = PrimaryOrderSupportBudget
  { primarySupportBudgetDegree :: Maybe HomologicalDegree,
    primarySupportBudgetPrime :: Integer,
    primarySupportBudgetBound :: Bound Integer
  }
  deriving stock (Eq, Show, Read)

type RequirePrimaryOrderSupport :: Type
data RequirePrimaryOrderSupport = RequirePrimaryOrderSupport
  { requiredPrimarySupportDegree :: Maybe HomologicalDegree,
    requiredPrimarySupportPrime :: Integer,
    requiredPrimarySupportedOrders :: [Integer],
    requiredPrimaryForbiddenOrders :: [Integer]
  }
  deriving stock (Eq, Show, Read)

type TorsionBudget :: Type
data TorsionBudget = TorsionBudget
  { torsionBudgetDegree :: Maybe HomologicalDegree,
    torsionBudgetOrder :: Maybe Integer,
    torsionBudgetMeasure :: TorsionBudgetMeasure,
    torsionBudgetBound :: Bound Integer
  }
  deriving stock (Eq, Show, Read)

type TorsionBudgetMeasure :: Type
data TorsionBudgetMeasure
  = TorsionSummandCount
  | TorsionTotalCardinality
  | TorsionElementOrderCount
  | TorsionOrderSupportCount
  deriving stock (Eq, Show, Read)

type RequireCyclicOrder :: Type
data RequireCyclicOrder = RequireCyclicOrder
  { requiredCyclicDegree :: HomologicalDegree,
    requiredCyclicOrder :: Integer,
    requiredCyclicMultiplicity :: Bound Int
  }
  deriving stock (Eq, Show, Read)

type SingularityBudget :: Type
newtype SingularityBudget = SingularityBudget
  { singularityBounds :: Map.Map CriticalKind (Bound Int)
  }
  deriving stock (Eq, Show, Read)

type HarmonicLoopBudget :: Type
data HarmonicLoopBudget = HarmonicLoopBudget
  { harmonicLoopDegree :: HomologicalDegree,
    harmonicLoopCountBound :: Bound Int
  }
  deriving stock (Eq, Show, Read)

type SkeletonAdherence :: Type
data SkeletonAdherence = SkeletonAdherence
  { skeletonTargetSignature :: SkeletonSignature,
    skeletonTolerance :: Int
  }
  deriving stock (Eq, Show, Read)

type TopologicalConstraint :: Type
data TopologicalConstraint
  = TargetBettiConstraint TargetBetti
  | PersistenceBudgetConstraint PersistenceBudget
  | EulerBoundConstraint EulerBound
  | LoopRoleConstraint LoopRole
  | RequireTorsionInvariantConstraint RequireTorsionInvariant
  | RequireElementOrderConstraint RequireElementOrder
  | RequireOrderSupportConstraint RequireOrderSupport
  | PrimaryOrderSupportBudgetConstraint PrimaryOrderSupportBudget
  | RequirePrimaryOrderSupportConstraint RequirePrimaryOrderSupport
  | TorsionBudgetConstraint TorsionBudget
  | RequireCyclicOrderConstraint RequireCyclicOrder
  | SingularityBudgetConstraint SingularityBudget
  | HarmonicLoopBudgetConstraint HarmonicLoopBudget
  | SkeletonAdherenceConstraint SkeletonAdherence
  deriving stock (Eq, Show, Read)

type TopologicalViolation :: Type
data TopologicalViolation
  = BettiViolation TargetBetti [Int]
  | EulerWitnessMissing EulerBound
  | EulerBoundViolation EulerBound EulerCharacteristic
  | PersistenceBudgetViolation PersistenceBudget Int
  | LoopRoleViolation LoopRole Int
  | IntegralHomologyWitnessMissing TopologicalConstraint
  | RequireTorsionInvariantViolation RequireTorsionInvariant Int
  | RequireElementOrderViolation RequireElementOrder Integer
  | RequireOrderSupportViolation RequireOrderSupport [Integer] [Integer]
  | PrimaryOrderSupportBudgetViolation PrimaryOrderSupportBudget Integer
  | RequirePrimaryOrderSupportViolation RequirePrimaryOrderSupport [Integer] [Integer]
  | InvalidPrimaryPrime TopologicalConstraint Integer
  | TorsionBudgetViolation TorsionBudget Integer
  | RequireCyclicOrderViolation RequireCyclicOrder Int
  | MacroScaffoldMissing TopologicalConstraint
  | SingularityBudgetViolation CriticalKind (Bound Int) Int
  | HarmonicLoopBudgetViolation HarmonicLoopBudget Int
  | SkeletonAdherenceViolation SkeletonAdherence SkeletonSignature
  deriving stock (Eq, Show, Read)

boundSatisfied :: Ord a => Bound a -> a -> Bool
boundSatisfied boundValue observedValue =
  case boundValue of
    Exactly targetValue -> observedValue == targetValue
    AtLeast minimumValue -> observedValue >= minimumValue
    AtMost maximumValue -> observedValue <= maximumValue
    Between minimumValue maximumValue -> observedValue >= minimumValue && observedValue <= maximumValue

checkBound :: Ord a => Bound a -> a -> violation -> [violation]
checkBound bound observed violation =
  if boundSatisfied bound observed then [] else [violation]

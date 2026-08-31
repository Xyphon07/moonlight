module Moonlight.LinAlg.Pure.Statics.Types
  ( NodeRef (..),
    Axis (..),
    Vec3 (..),
    MemberRef,
    mkMemberRef,
    memberEndpoints,
    memberTouchesNode,
    SupportAxes,
    mkSupportAxes,
    supportAxesList,
    freeSupportAxes,
    fixedSupportAxes,
    ForceNode,
    freeForceNode,
    supportedForceNode,
    fixedForceNode,
    forceNodePosition,
    forceNodeLoad,
    forceNodeSupportAxes,
    forceNodeReactionAxes,
    ForceNetwork (..),
    UnknownForce (..),
    EquationRef (..),
    CompiledEquilibrium,
    mkCompiledEquilibrium,
    compiledNodeOrder,
    compiledFoundationOrder,
    compiledMemberOrder,
    compiledMemberDirections,
    compiledUnknownOrder,
    compiledEquationOrder,
    compiledCoefficientMatrix,
    compiledRightHandSide,
    EquilibriumSolution (..),
    ForceSign (..),
    EquilibriumViolation (..),
    EquilibriumResult (..),
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Pure.Dense.Dynamic
  ( DynMatrix,
    DynVector,
    dynMatrixShape,
    dynVectorLength,
  )
import Moonlight.LinAlg.Pure.Geometry.Vec3 (Axis (..), Vec3 (..))
import Prelude

type NodeRef :: Type
newtype NodeRef = NodeRef
  { unNodeRef :: String
  }
  deriving stock (Eq, Ord, Show)

type MemberRef :: Type
data MemberRef = MemberRef NodeRef NodeRef
  deriving stock (Eq, Ord, Show)

mkMemberRef :: NodeRef -> NodeRef -> Either MoonlightError MemberRef
mkMemberRef leftRef rightRef
  | leftRef == rightRef = Left (InvariantViolation "member endpoints must be distinct")
  | leftRef < rightRef = Right (MemberRef leftRef rightRef)
  | otherwise = Right (MemberRef rightRef leftRef)

memberEndpoints :: MemberRef -> (NodeRef, NodeRef)
memberEndpoints (MemberRef leftRef rightRef) =
  (leftRef, rightRef)

memberTouchesNode :: NodeRef -> MemberRef -> Bool
memberTouchesNode nodeRefValue memberRefValue =
  case memberEndpoints memberRefValue of
    (leftRef, rightRef) -> nodeRefValue == leftRef || nodeRefValue == rightRef

type SupportAxes :: Type
newtype SupportAxes = SupportAxes
  { supportAxesSet :: Set Axis
  }
  deriving stock (Eq, Ord, Show)

mkSupportAxes :: [Axis] -> SupportAxes
mkSupportAxes =
  SupportAxes . Set.fromList

supportAxesList :: SupportAxes -> [Axis]
supportAxesList =
  Set.toAscList . supportAxesSet

freeSupportAxes :: SupportAxes
freeSupportAxes =
  SupportAxes Set.empty

fixedSupportAxes :: SupportAxes
fixedSupportAxes =
  mkSupportAxes [AxisX, AxisY, AxisZ]

type ForceNode :: Type
data ForceNode = ForceNode
  { forceNodePosition :: Vec3,
    forceNodeLoad :: Vec3,
    forceNodeSupportAxesValue :: SupportAxes
  }
  deriving stock (Eq, Show)

freeForceNode :: Vec3 -> Vec3 -> ForceNode
freeForceNode position load =
  ForceNode position load freeSupportAxes

supportedForceNode :: Vec3 -> Vec3 -> SupportAxes -> ForceNode
supportedForceNode =
  ForceNode

fixedForceNode :: Vec3 -> Vec3 -> ForceNode
fixedForceNode position load =
  ForceNode position load fixedSupportAxes

forceNodeSupportAxes :: ForceNode -> SupportAxes
forceNodeSupportAxes =
  forceNodeSupportAxesValue

forceNodeReactionAxes :: ForceNode -> [Axis]
forceNodeReactionAxes =
  supportAxesList . forceNodeSupportAxes

type ForceNetwork :: Type
data ForceNetwork = ForceNetwork
  { forceNodes :: Map NodeRef ForceNode,
    forceMembers :: Set MemberRef
  }
  deriving stock (Eq, Show)

type UnknownForce :: Type
data UnknownForce
  = MemberUnknown MemberRef
  | ReactionUnknown NodeRef Axis
  deriving stock (Eq, Ord, Show)

type EquationRef :: Type
data EquationRef = EquationRef
  { equationNodeRef :: NodeRef,
    equationAxis :: Axis
  }
  deriving stock (Eq, Ord, Show)

type CompiledEquilibrium :: Type
data CompiledEquilibrium = CompiledEquilibrium
  { compiledNodeOrder :: [NodeRef],
    compiledFoundationOrder :: [NodeRef],
    compiledMemberOrder :: [MemberRef],
    compiledMemberDirections :: Map MemberRef Vec3,
    compiledUnknownOrder :: [UnknownForce],
    compiledEquationOrder :: [EquationRef],
    compiledCoefficientMatrix :: DynMatrix Double,
    compiledRightHandSide :: DynVector Double
  }

mkCompiledEquilibrium ::
  [NodeRef] ->
  [NodeRef] ->
  [MemberRef] ->
  Map MemberRef Vec3 ->
  [UnknownForce] ->
  [EquationRef] ->
  DynMatrix Double ->
  DynVector Double ->
  Either MoonlightError CompiledEquilibrium
mkCompiledEquilibrium nodeOrder foundationOrder memberOrder memberDirections unknownOrder equationOrder coefficientMatrix rightHandSide =
  let (matrixRowCount, matrixColumnCount) = dynMatrixShape coefficientMatrix
      equationCount = length equationOrder
      unknownCount = length unknownOrder
   in if matrixRowCount /= equationCount
        then
          Left
            ( InvariantViolation
                ( "compiled equilibrium coefficient row count mismatch: expected "
                    <> show equationCount
                    <> " rows but received "
                    <> show matrixRowCount
                )
            )
        else
          if matrixColumnCount /= unknownCount
            then
              Left
                ( InvariantViolation
                    ( "compiled equilibrium coefficient column count mismatch: expected "
                        <> show unknownCount
                        <> " columns but received "
                        <> show matrixColumnCount
                    )
                )
            else
              if dynVectorLength rightHandSide /= equationCount
                then
                  Left
                    ( InvariantViolation
                        ( "compiled equilibrium RHS length mismatch: expected "
                            <> show equationCount
                            <> " entries but received "
                            <> show (dynVectorLength rightHandSide)
                        )
                    )
                else
                  Right
                    CompiledEquilibrium
                      { compiledNodeOrder = nodeOrder,
                        compiledFoundationOrder = foundationOrder,
                        compiledMemberOrder = memberOrder,
                        compiledMemberDirections = memberDirections,
                        compiledUnknownOrder = unknownOrder,
                        compiledEquationOrder = equationOrder,
                        compiledCoefficientMatrix = coefficientMatrix,
                        compiledRightHandSide = rightHandSide
                      }

type EquilibriumSolution :: Type
data EquilibriumSolution = EquilibriumSolution
  { equilibriumMemberForces :: Map MemberRef Double,
    equilibriumReactionForces :: Map NodeRef Vec3,
    equilibriumResidualForces :: Map NodeRef Vec3
  }
  deriving stock (Eq, Show)

type ForceSign :: Type
data ForceSign
  = Compression
  | Tension
  deriving stock (Eq, Ord, Show, Read)

type EquilibriumViolation :: Type
data EquilibriumViolation = EquilibriumViolation
  { violationNode :: NodeRef,
    violationResidualForce :: Vec3,
    violationResidualMagnitude :: Double,
    violationWorstMember :: Maybe MemberRef,
    violationMemberForceSign :: Maybe ForceSign
  }
  deriving stock (Eq, Show)

type EquilibriumResult :: Type
data EquilibriumResult
  = InEquilibrium EquilibriumSolution
  | Disequilibrium (NonEmpty EquilibriumViolation)
  deriving stock (Eq, Show)

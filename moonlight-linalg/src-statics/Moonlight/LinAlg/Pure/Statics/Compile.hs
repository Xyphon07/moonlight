module Moonlight.LinAlg.Pure.Statics.Compile
  ( assembleEquilibriumEquations,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Core (MoonlightError (..))
import Moonlight.LinAlg.Pure.Dense.Dynamic (mkDynMatrix, mkDynVector)
import Moonlight.LinAlg.Pure.Statics.Algebra
  ( allAxes,
    axisComponent,
    memberEndpoints,
    mkMemberRef,
    normalizeVec3,
    scaleVec3,
    subVec3,
  )
import Moonlight.LinAlg.Pure.Statics.Types
  ( CompiledEquilibrium,
    EquationRef (..),
    ForceNetwork,
    ForceNode,
    MemberRef,
    NodeRef,
    UnknownForce (..),
    Vec3,
    forceMembers,
    forceNodeLoad,
    forceNodePosition,
    forceNodeReactionAxes,
    forceNodes,
    mkCompiledEquilibrium,
  )
import Prelude

assembleEquilibriumEquations :: ForceNetwork -> Either MoonlightError CompiledEquilibrium
assembleEquilibriumEquations networkValue = do
  memberOrder <- canonicalMemberOrder (forceMembers networkValue)
  let nodeOrder = Map.keys (forceNodes networkValue)
      foundationEntries = supportedNodeEntries (forceNodes networkValue)
      foundationOrder = fst <$> foundationEntries
      reactionUnknowns =
        concatMap
          ( \(nodeRefValue, nodeValue) ->
              fmap
                (ReactionUnknown nodeRefValue)
                (forceNodeReactionAxes nodeValue)
          )
          foundationEntries
      equationOrder =
        concatMap
          (\nodeRefValue -> fmap (EquationRef nodeRefValue) allAxes)
          nodeOrder
      unknownOrder =
        fmap MemberUnknown memberOrder
          <> reactionUnknowns
  memberDirections <- Map.fromList <$> traverse (directionEntry networkValue) memberOrder
  coefficientRows <- traverse (equationCoefficients networkValue memberDirections unknownOrder) equationOrder
  rightHandSideValues <- traverse (equationRightHandSide networkValue) equationOrder
  coefficientMatrix <- mkDynMatrix (length equationOrder) (length unknownOrder) (concat coefficientRows)
  rightHandSideVector <- mkDynVector (length equationOrder) rightHandSideValues
  mkCompiledEquilibrium
    nodeOrder
    foundationOrder
    memberOrder
    memberDirections
    unknownOrder
    equationOrder
    coefficientMatrix
    rightHandSideVector

canonicalMemberOrder :: Set MemberRef -> Either MoonlightError [MemberRef]
canonicalMemberOrder memberRefs =
  fmap Set.toAscList
    ( Set.fromList
        <$> traverse
          ( \memberRefValue ->
              case memberEndpoints memberRefValue of
                (leftRef, rightRef) -> mkMemberRef leftRef rightRef
          )
          (Set.toAscList memberRefs)
    )

directionEntry :: ForceNetwork -> MemberRef -> Either MoonlightError (MemberRef, Vec3)
directionEntry networkValue memberRefValue =
  fmap ((,) memberRefValue) (memberDirection networkValue memberRefValue)

memberDirection :: ForceNetwork -> MemberRef -> Either MoonlightError Vec3
memberDirection networkValue memberRefValue =
  case memberEndpoints memberRefValue of
    (leftRef, rightRef) -> do
      leftNode <- lookupNode networkValue leftRef
      rightNode <- lookupNode networkValue rightRef
      normalizeVec3
        ( subVec3
            (forceNodePosition rightNode)
            (forceNodePosition leftNode)
        )

equationCoefficients ::
  ForceNetwork ->
  Map MemberRef Vec3 ->
  [UnknownForce] ->
  EquationRef ->
  Either MoonlightError [Double]
equationCoefficients networkValue memberDirections unknownOrder equationRefValue = do
  _ <- lookupNode networkValue (equationNodeRef equationRefValue)
  pure
    ( fmap
        (unknownCoefficient memberDirections equationRefValue)
        unknownOrder
    )

equationRightHandSide :: ForceNetwork -> EquationRef -> Either MoonlightError Double
equationRightHandSide networkValue equationRefValue = do
  nodeValue <- lookupNode networkValue (equationNodeRef equationRefValue)
  pure
    ( negate
        (axisComponent (equationAxis equationRefValue) (forceNodeLoad nodeValue))
    )

unknownCoefficient :: Map MemberRef Vec3 -> EquationRef -> UnknownForce -> Double
unknownCoefficient memberDirections equationRefValue unknownValue =
  case unknownValue of
    MemberUnknown memberRefValue ->
      maybe 0.0
        ( \directionValue ->
            maybe
              0.0
              (axisComponent (equationAxis equationRefValue))
              (memberContribution (equationNodeRef equationRefValue) memberRefValue directionValue)
        )
        (Map.lookup memberRefValue memberDirections)
    ReactionUnknown reactionNode reactionAxisValue ->
      if reactionNode == equationNodeRef equationRefValue && reactionAxisValue == equationAxis equationRefValue
        then 1.0
        else 0.0

memberContribution :: NodeRef -> MemberRef -> Vec3 -> Maybe Vec3
memberContribution nodeRefValue memberRefValue directionValue =
  case memberEndpoints memberRefValue of
    (leftRef, rightRef)
      | nodeRefValue == leftRef -> Just (scaleVec3 (-1.0) directionValue)
      | nodeRefValue == rightRef -> Just directionValue
      | otherwise -> Nothing

lookupNode :: ForceNetwork -> NodeRef -> Either MoonlightError ForceNode
lookupNode networkValue nodeRefValue =
  case Map.lookup nodeRefValue (forceNodes networkValue) of
    Nothing ->
      Left (InvariantViolation ("force network member references unknown node " <> show nodeRefValue))
    Just nodeValue -> Right nodeValue

supportedNodeEntries :: Map NodeRef ForceNode -> [(NodeRef, ForceNode)]
supportedNodeEntries =
  filter (not . null . forceNodeReactionAxes . snd) . Map.toAscList

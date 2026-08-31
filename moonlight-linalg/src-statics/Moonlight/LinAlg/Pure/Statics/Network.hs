{-# LANGUAGE StrictData #-}

module Moonlight.LinAlg.Pure.Statics.Network
  ( NetworkDeclaration,
    NetworkBuildError (..),
    nodeRef,
    nodeRefLabel,
    joint,
    support,
    supportOn,
    load,
    member,
    network,
    nodePosition,
    nodeLoad,
    nodeSupportAxes,
    nodeReactionAxes,
    networkNodeMap,
    networkMemberSet,
  )
where

import Control.Monad (foldM)
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Core (fieldValueValid)
import Moonlight.LinAlg.Pure.Geometry.Vec3
  ( Axis,
    Vec3 (..),
    magnitudeVec3,
    normalizeVec3,
    subVec3,
  )
import Moonlight.LinAlg.Pure.Statics.Types
  ( ForceNetwork (..),
    ForceNode,
    MemberRef,
    NodeRef (..),
    SupportAxes,
    fixedSupportAxes,
    forceMembers,
    forceNodeLoad,
    forceNodePosition,
    forceNodeReactionAxes,
    forceNodeSupportAxes,
    forceNodes,
    memberEndpoints,
    mkMemberRef,
    mkSupportAxes,
    supportedForceNode,
    supportAxesList,
  )
import Prelude

type NetworkDeclaration :: Type
data NetworkDeclaration
  = JointDeclaration !String !Vec3
  | SupportDeclaration !String !Vec3 !SupportAxes
  | LoadDeclaration !String !Vec3 !Vec3
  | MemberDeclaration !String !String
  deriving stock (Eq, Show)

type NetworkBuildError :: Type
data NetworkBuildError
  = EmptyNodeLabel
  | NonFiniteNodePosition !String !Vec3
  | NonFiniteNodeLoad !String !Vec3
  | MissingNodePosition !String
  | ConflictingNodePosition !String !Vec3 !Vec3
  | NonFiniteAccumulatedLoad !String !Vec3
  | UnknownMemberEndpoint !String
  | SelfMember !String
  | NonFiniteMemberGeometry !String !String
  | DegenerateMember !String !String
  deriving stock (Eq, Show)

nodeRef :: String -> Either NetworkBuildError NodeRef
nodeRef labelValue
  | null labelValue = Left EmptyNodeLabel
  | otherwise = Right (NodeRef labelValue)

nodeRefLabel :: NodeRef -> String
nodeRefLabel (NodeRef labelValue) =
  labelValue

joint :: String -> Vec3 -> NetworkDeclaration
joint =
  JointDeclaration

support :: String -> Vec3 -> NetworkDeclaration
support labelValue positionValue =
  SupportDeclaration labelValue positionValue fixedSupportAxes

supportOn :: String -> Vec3 -> SupportAxes -> NetworkDeclaration
supportOn =
  SupportDeclaration

load :: String -> Vec3 -> Vec3 -> NetworkDeclaration
load =
  LoadDeclaration

member :: String -> String -> NetworkDeclaration
member =
  MemberDeclaration

network :: [NetworkDeclaration] -> Either NetworkBuildError ForceNetwork
network declarations = do
  accumulatedNetwork <-
    foldM
      collectDeclaration
      emptyNetworkAccumulator
      declarations
  finalizedNodes <-
    Map.traverseWithKey
      finalizePartialNode
      (networkAccumulatorNodes accumulatedNetwork)
  traverse_
    (validateMemberGeometry finalizedNodes)
    (Set.toAscList (networkAccumulatorMembers accumulatedNetwork))
  Right
    ForceNetwork
      { forceNodes = finalizedNodes,
        forceMembers = networkAccumulatorMembers accumulatedNetwork
      }

nodePosition :: ForceNode -> Vec3
nodePosition =
  forceNodePosition

nodeLoad :: ForceNode -> Vec3
nodeLoad =
  forceNodeLoad

nodeSupportAxes :: ForceNode -> SupportAxes
nodeSupportAxes =
  forceNodeSupportAxes

nodeReactionAxes :: ForceNode -> [Axis]
nodeReactionAxes =
  forceNodeReactionAxes

networkNodeMap :: ForceNetwork -> Map NodeRef ForceNode
networkNodeMap =
  forceNodes

networkMemberSet :: ForceNetwork -> Set MemberRef
networkMemberSet =
  forceMembers

type ExactVec3 :: Type
data ExactVec3 = ExactVec3
  { exactVec3X :: !Rational,
    exactVec3Y :: !Rational,
    exactVec3Z :: !Rational
  }
  deriving stock (Eq, Show)

zeroExactVec3 :: ExactVec3
zeroExactVec3 =
  ExactVec3 0 0 0

exactVec3FromVec3 :: Vec3 -> ExactVec3
exactVec3FromVec3 (Vec3 xValue yValue zValue) =
  ExactVec3
    (toRational xValue)
    (toRational yValue)
    (toRational zValue)

addExactVec3 :: ExactVec3 -> ExactVec3 -> ExactVec3
addExactVec3 leftValue rightValue =
  ExactVec3
    { exactVec3X = exactVec3X leftValue + exactVec3X rightValue,
      exactVec3Y = exactVec3Y leftValue + exactVec3Y rightValue,
      exactVec3Z = exactVec3Z leftValue + exactVec3Z rightValue
    }

type PartialNode :: Type
data PartialNode = PartialNode
  { partialNodePositions :: !(Set Vec3),
    partialNodeLoadExact :: !ExactVec3,
    partialNodeSupportAxes :: !(Set Axis)
  }
  deriving stock (Eq, Show)

emptyPartialNode :: PartialNode
emptyPartialNode =
  PartialNode
    { partialNodePositions = Set.empty,
      partialNodeLoadExact = zeroExactVec3,
      partialNodeSupportAxes = Set.empty
    }

type NetworkAccumulator :: Type
data NetworkAccumulator = NetworkAccumulator
  { networkAccumulatorNodes :: !(Map NodeRef PartialNode),
    networkAccumulatorMembers :: !(Set MemberRef)
  }
  deriving stock (Eq, Show)

emptyNetworkAccumulator :: NetworkAccumulator
emptyNetworkAccumulator =
  NetworkAccumulator
    { networkAccumulatorNodes = Map.empty,
      networkAccumulatorMembers = Set.empty
    }

collectDeclaration ::
  NetworkAccumulator ->
  NetworkDeclaration ->
  Either NetworkBuildError NetworkAccumulator
collectDeclaration accumulator declarationValue =
  case declarationValue of
    JointDeclaration labelValue positionValue ->
      collectNodeDeclaration
        labelValue
        positionValue
        Nothing
        Set.empty
        accumulator
    SupportDeclaration labelValue positionValue supportAxes ->
      collectNodeDeclaration
        labelValue
        positionValue
        Nothing
        (Set.fromList (supportAxesList supportAxes))
        accumulator
    LoadDeclaration labelValue positionValue loadValue ->
      collectNodeDeclaration
        labelValue
        positionValue
        (Just loadValue)
        Set.empty
        accumulator
    MemberDeclaration leftLabel rightLabel ->
      collectMemberDeclaration
        leftLabel
        rightLabel
        accumulator

collectNodeDeclaration ::
  String ->
  Vec3 ->
  Maybe Vec3 ->
  Set Axis ->
  NetworkAccumulator ->
  Either NetworkBuildError NetworkAccumulator
collectNodeDeclaration labelValue positionValue maybeLoadValue supportAxes accumulator = do
  nodeReference <- nodeRef labelValue
  if finiteVec3 positionValue
    then Right ()
    else Left (NonFiniteNodePosition labelValue positionValue)
  traverse_ validateLoad maybeLoadValue
  let currentPartialNode =
        Map.findWithDefault
          emptyPartialNode
          nodeReference
          (networkAccumulatorNodes accumulator)
      loadContribution =
        maybe zeroExactVec3 exactVec3FromVec3 maybeLoadValue
      updatedPartialNode =
        currentPartialNode
          { partialNodePositions =
              Set.insert positionValue (partialNodePositions currentPartialNode),
            partialNodeLoadExact =
              addExactVec3 (partialNodeLoadExact currentPartialNode) loadContribution,
            partialNodeSupportAxes =
              Set.union (partialNodeSupportAxes currentPartialNode) supportAxes
          }
  Right
    accumulator
      { networkAccumulatorNodes =
          Map.insert
            nodeReference
            updatedPartialNode
            (networkAccumulatorNodes accumulator)
      }
  where
    validateLoad loadValue
      | finiteVec3 loadValue = Right ()
      | otherwise = Left (NonFiniteNodeLoad labelValue loadValue)

collectMemberDeclaration ::
  String ->
  String ->
  NetworkAccumulator ->
  Either NetworkBuildError NetworkAccumulator
collectMemberDeclaration leftLabel rightLabel accumulator = do
  leftReference <- nodeRef leftLabel
  rightReference <- nodeRef rightLabel
  if leftReference == rightReference
    then Left (SelfMember leftLabel)
    else
      case mkMemberRef leftReference rightReference of
        Left _ -> Left (SelfMember leftLabel)
        Right memberReference ->
          Right
            accumulator
              { networkAccumulatorMembers =
                  Set.insert
                    memberReference
                    (networkAccumulatorMembers accumulator)
              }

finalizePartialNode ::
  NodeRef ->
  PartialNode ->
  Either NetworkBuildError ForceNode
finalizePartialNode nodeReference partialNode = do
  positionValue <-
    case Set.toAscList (partialNodePositions partialNode) of
      [] -> Left (MissingNodePosition (nodeRefLabel nodeReference))
      [singlePosition] -> Right singlePosition
      firstPosition : secondPosition : _ ->
        Left
          ( ConflictingNodePosition
              (nodeRefLabel nodeReference)
              firstPosition
              secondPosition
          )
  loadValue <-
    finalizeExactLoad
      (nodeRefLabel nodeReference)
      (partialNodeLoadExact partialNode)
  let supportAxes = mkSupportAxes (Set.toAscList (partialNodeSupportAxes partialNode))
  Right (supportedForceNode positionValue loadValue supportAxes)

finalizeExactLoad :: String -> ExactVec3 -> Either NetworkBuildError Vec3
finalizeExactLoad labelValue exactLoad =
  let loadValue =
        Vec3
          (fromRational (exactVec3X exactLoad))
          (fromRational (exactVec3Y exactLoad))
          (fromRational (exactVec3Z exactLoad))
   in if finiteVec3 loadValue
        then Right loadValue
        else Left (NonFiniteAccumulatedLoad labelValue loadValue)

validateMemberGeometry ::
  Map NodeRef ForceNode ->
  MemberRef ->
  Either NetworkBuildError ()
validateMemberGeometry nodeValues memberReference = do
  let (leftReference, rightReference) = memberEndpoints memberReference
      leftLabel = nodeRefLabel leftReference
      rightLabel = nodeRefLabel rightReference
  leftNode <- requireNode leftLabel leftReference nodeValues
  rightNode <- requireNode rightLabel rightReference nodeValues
  let displacement =
        subVec3
          (forceNodePosition rightNode)
          (forceNodePosition leftNode)
      displacementMagnitude = magnitudeVec3 displacement
  if not (finiteVec3 displacement) || not (fieldValueValid displacementMagnitude)
    then Left (NonFiniteMemberGeometry leftLabel rightLabel)
    else
      case normalizeVec3 displacement of
        Left _ -> Left (DegenerateMember leftLabel rightLabel)
        Right _ -> Right ()

requireNode ::
  String ->
  NodeRef ->
  Map NodeRef ForceNode ->
  Either NetworkBuildError ForceNode
requireNode labelValue nodeReference nodeValues =
  case Map.lookup nodeReference nodeValues of
    Nothing -> Left (UnknownMemberEndpoint labelValue)
    Just nodeValue -> Right nodeValue

finiteVec3 :: Vec3 -> Bool
finiteVec3 (Vec3 xValue yValue zValue) =
  fieldValueValid xValue && fieldValueValid yValue && fieldValueValid zValue

module Moonlight.Homology.Pure.Topology.MacroScaffold.Compose.Stitch
  ( StitchBoundarySide (..),
    StitchSupportSelection (..),
    StitchSupportRefinement (..),
    StitchSemantics (..),
    StitchRoute (..),
    StitchRouteKey (..),
    MacroScaffoldStitchError (..),
    stitchRoutePair,
    uniqueRoutePairs,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Moonlight.Core (note)
import Moonlight.Homology.Pure.Carrier
  ( BasisCellRef,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( MacroScaffoldIR (..),
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Potential
  ( PotentialValue,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold.Reeb
  ( Monotonicity (..),
    MorseReebArc (..),
    MorseReebNode (..),
    MorseReebScaffold (..),
    ReebArcId (..),
  )

type StitchBoundarySide :: Type
data StitchBoundarySide
  = LowerBoundary
  | UpperBoundary
  deriving stock (Eq, Ord, Show)

type StitchSupportSelection :: Type
data StitchSupportSelection
  = BoundarySupport
  | AnchorSupport
  deriving stock (Eq, Ord, Show)

type StitchSupportRefinement :: Type
data StitchSupportRefinement
  = KernelSupportRefinement
  | BoundaryEnvelopeRefinement
  | RegionalEnvelopeRefinement
  deriving stock (Eq, Ord, Show)

type StitchSemantics :: Type
data StitchSemantics = StitchSemantics
  { ssSourceBoundary :: StitchBoundarySide,
    ssTargetBoundary :: StitchBoundarySide,
    ssSupportSelection :: StitchSupportSelection,
    ssSupportRefinement :: StitchSupportRefinement
  }
  deriving stock (Eq, Ord, Show)

type StitchRoute :: Type -> Type -> Type
data StitchRoute route label = StitchRoute
  { stitchRouteKind :: route,
    stitchRouteRegions :: NonEmpty label
  }
  deriving stock (Eq, Show)

type StitchRouteKey :: Type -> Type -> Type
data StitchRouteKey route label = StitchRouteKey
  { srkRouteKind :: route,
    srkSourceRegion :: label,
    srkTargetRegion :: label
  }
  deriving stock (Eq, Ord, Show)

type MacroScaffoldStitchError :: Type -> Type
data MacroScaffoldStitchError label
  = MissingStitchRegion label
  | MissingBoundaryNode label StitchBoundarySide
  deriving stock (Eq, Show)

stitchRoutePair ::
  (Ord route, Ord label) =>
  (route -> StitchSemantics) ->
  Map label (Set BasisCellRef) ->
  MacroScaffoldIR ->
  (Int, [MorseReebArc], Map (StitchRouteKey route label) (Set BasisCellRef)) ->
  StitchRouteKey route label ->
  Either (MacroScaffoldStitchError label) (Int, [MorseReebArc], Map (StitchRouteKey route label) (Set BasisCellRef))
stitchRoutePair semanticsFor regionScopeMap scaffoldValue (nextArcId, stitchedArcs, stitchScopes) stitchKey
  | sourceLabel == targetLabel =
      Right (nextArcId, stitchedArcs, stitchScopes)
  | otherwise = do
      let semantics = semanticsFor routeKind
      (sourceRegionBasisRefs, sourceNode, sourceSupport) <-
        boundaryNodeAndSupport regionScopeMap scaffoldValue sourceLabel (ssSourceBoundary semantics)
      (targetRegionBasisRefs, targetNode, targetSupport) <-
        boundaryNodeAndSupport regionScopeMap scaffoldValue targetLabel (ssTargetBoundary semantics)
      let supportRefs =
            combineSupportRefs
              (ssSupportSelection semantics)
              (ssSupportRefinement semantics)
              sourceRegionBasisRefs
              targetRegionBasisRefs
              sourceSupport
              targetSupport
              sourceNode
              targetNode
          stitchedArc =
            MorseReebArc
              { morseReebArcId = ReebArcId nextArcId,
                morseReebArcSource = morseReebNodeId sourceNode,
                morseReebArcTarget = morseReebNodeId targetNode,
                morseReebArcMonotonicity = monotonicityBetween sourceNode targetNode,
                morseReebArcSupport = Set.toAscList supportRefs
              }
      Right
        ( nextArcId + 1,
          stitchedArc : stitchedArcs,
          Map.insertWith Set.union stitchKey supportRefs stitchScopes
        )
  where
    routeKind = srkRouteKind stitchKey
    sourceLabel = srkSourceRegion stitchKey
    targetLabel = srkTargetRegion stitchKey

uniqueRoutePairs :: (Ord route, Ord label) => [StitchRoute route label] -> [StitchRouteKey route label]
uniqueRoutePairs = nubOrd . concatMap routeTransitions

routeTransitions :: StitchRoute route label -> [StitchRouteKey route label]
routeTransitions routeValue =
  go firstLabel remainingLabels
  where
    routeKind = stitchRouteKind routeValue
    firstLabel :| remainingLabels = stitchRouteRegions routeValue
    go _ [] = []
    go previousLabel (nextLabel : rest) =
      StitchRouteKey routeKind previousLabel nextLabel : go nextLabel rest

boundaryNodeAndSupport ::
  Ord label =>
  Map label (Set BasisCellRef) ->
  MacroScaffoldIR ->
  label ->
  StitchBoundarySide ->
  Either (MacroScaffoldStitchError label) (Set BasisCellRef, MorseReebNode, Set BasisCellRef)
boundaryNodeAndSupport regionScopeMap scaffoldValue labelValue boundarySide = do
  regionBasisRefs <-
    note (MissingStitchRegion labelValue) (Map.lookup labelValue regionScopeMap)
  boundaryNodes <-
    boundaryNodesForRegion regionBasisRefs scaffoldValue boundarySide
      & note (MissingBoundaryNode labelValue boundarySide)
  let supportRefs =
        boundarySupportRefs regionBasisRefs scaffoldValue boundaryNodes
      representativeNode = selectRepresentativeNode boundaryNodes
  Right (regionBasisRefs, representativeNode, supportRefs)

boundaryNodesForRegion ::
  Set BasisCellRef ->
  MacroScaffoldIR ->
  StitchBoundarySide ->
  Maybe (NonEmpty MorseReebNode)
boundaryNodesForRegion regionBasisRefs scaffoldValue boundarySide =
  let regionNodes =
        filter
          (\nodeValue -> Set.member (morseReebNodeAnchor nodeValue) regionBasisRefs)
          (morseReebNodes (macroScaffoldReeb scaffoldValue))
   in case regionNodes of
        [] -> Nothing
        firstNode : restNodes ->
          let boundaryPotential =
                foldr
                  (selectPotential boundarySide . morseReebNodePotential)
                  (morseReebNodePotential firstNode)
                  restNodes
              matchingNodes =
                filter
                  ((== boundaryPotential) . morseReebNodePotential)
                  regionNodes
           in NonEmpty.nonEmpty matchingNodes

selectPotential :: StitchBoundarySide -> PotentialValue -> PotentialValue -> PotentialValue
selectPotential boundarySide candidate current =
  case boundarySide of
    LowerBoundary ->
      min candidate current
    UpperBoundary ->
      max candidate current

boundarySupportRefs ::
  Set BasisCellRef ->
  MacroScaffoldIR ->
  NonEmpty MorseReebNode ->
  Set BasisCellRef
boundarySupportRefs regionBasisRefs scaffoldValue boundaryNodes =
  let boundaryNodeIds =
        Set.fromList (fmap morseReebNodeId (NonEmpty.toList boundaryNodes))
      incidentArcSupport =
        morseReebArcs (macroScaffoldReeb scaffoldValue)
          & foldr
            ( \arcValue supportRefs ->
                if Set.member (morseReebArcSource arcValue) boundaryNodeIds
                  || Set.member (morseReebArcTarget arcValue) boundaryNodeIds
                  then
                    Set.union
                      supportRefs
                      ( Set.fromList
                          ( filter (`Set.member` regionBasisRefs) (morseReebArcSupport arcValue) )
                      )
                  else supportRefs
            )
            Set.empty
      anchorSupport =
        Set.fromList (fmap morseReebNodeAnchor (NonEmpty.toList boundaryNodes))
   in Set.union anchorSupport incidentArcSupport

selectRepresentativeNode :: NonEmpty MorseReebNode -> MorseReebNode
selectRepresentativeNode (firstNode :| remainingNodes) =
  foldr
    (\nodeValue selectedNode -> if morseReebNodeId nodeValue < morseReebNodeId selectedNode then nodeValue else selectedNode)
    firstNode
    remainingNodes

combineSupportRefs ::
  StitchSupportSelection ->
  StitchSupportRefinement ->
  Set BasisCellRef ->
  Set BasisCellRef ->
  Set BasisCellRef ->
  Set BasisCellRef ->
  MorseReebNode ->
  MorseReebNode ->
  Set BasisCellRef
combineSupportRefs supportSelection supportRefinement sourceRegionBasisRefs targetRegionBasisRefs sourceSupport targetSupport sourceNode targetNode =
  case supportRefinement of
    KernelSupportRefinement ->
      kernelSupport
    BoundaryEnvelopeRefinement ->
      Set.union kernelSupport boundarySupport
    RegionalEnvelopeRefinement ->
      Set.unions [kernelSupport, boundarySupport, sourceRegionBasisRefs, targetRegionBasisRefs]
  where
    kernelSupport =
      case supportSelection of
        BoundarySupport ->
          boundarySupport
        AnchorSupport ->
          Set.fromList [morseReebNodeAnchor sourceNode, morseReebNodeAnchor targetNode]
    boundarySupport =
      Set.union sourceSupport targetSupport

monotonicityBetween :: MorseReebNode -> MorseReebNode -> Monotonicity
monotonicityBetween sourceNode targetNode =
  if morseReebNodePotential sourceNode <= morseReebNodePotential targetNode
    then Ascending
    else Descending

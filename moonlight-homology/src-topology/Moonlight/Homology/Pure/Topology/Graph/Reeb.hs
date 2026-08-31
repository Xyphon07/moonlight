module Moonlight.Homology.Pure.Topology.Graph.Reeb
  ( graphReebArcs,
    graphSingularities,
    traceArc,
    followGradient,
    arcSupportCells,
    edgeCellsBetween,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Set qualified as Set
import Moonlight.Homology.Pure.Topology.Core
import Moonlight.Homology.Pure.Topology.Graph.Critical (higherNeighborEdges)
import Moonlight.Homology.Pure.Topology.Graph.Skeleton (edgeBasisCellRef, vertexBasisCellRef)
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( Monotonicity (..),
    MorseReebArc (..),
    MorseReebNode (..),
    ReebArcId (..),
    ReebNodeId,
    Singularity (..),
    SingularityId (..),
    SingularityIndex (..),
  )

graphReebArcs :: Map.Map Int Double -> Graph1Skeleton -> [MorseReebNode] -> [MorseReebArc]
graphReebArcs potentials skeleton nodes =
  let nodeIdsByVertex =
        nodes
          & fmap (\node -> (cellIndex (morseReebNodeAnchor node), morseReebNodeId node))
          & Map.fromList
      criticalKindsByVertex =
        nodes
          & fmap (\node -> (cellIndex (morseReebNodeAnchor node), morseReebNodeKind node))
          & Map.fromList
      arcSeeds =
        nodes
          >>= ( \node ->
                  let sourceVertex = cellIndex (morseReebNodeAnchor node)
                   in higherNeighborEdges potentials skeleton sourceVertex
                        & mapMaybe
                          ( \(seedEdge, seedVertex) ->
                              traceArc potentials nodeIdsByVertex criticalKindsByVertex skeleton (morseReebNodeId node) sourceVertex seedEdge seedVertex
                          )
             )
          & nubOrd
   in zip (fmap ReebArcId (enumerateFromZero (length arcSeeds))) arcSeeds
        & fmap
          ( \(arcId, (sourceNodeId, targetNodeId, monotonicityValue, supportCells)) ->
              MorseReebArc
                { morseReebArcId = arcId,
                  morseReebArcSource = sourceNodeId,
                  morseReebArcTarget = targetNodeId,
                  morseReebArcMonotonicity = monotonicityValue,
                  morseReebArcSupport = supportCells
                }
          )

graphSingularities :: [MorseReebNode] -> [MorseReebArc] -> [Singularity]
graphSingularities nodes arcs =
  let incidentArcs =
        Map.fromListWith (<>)
          ( arcs >>= \arcValue ->
              [ (morseReebArcSource arcValue, [morseReebArcId arcValue]),
                (morseReebArcTarget arcValue, [morseReebArcId arcValue])
              ]
          )
   in zip (fmap SingularityId (enumerateFromZero (length nodes))) nodes
        & fmap
          ( \(singularityIdValue, nodeValue) ->
              Singularity
                { singularityId = singularityIdValue,
                  singularityAnchor = morseReebNodeAnchor nodeValue,
                  singularityKind = morseReebNodeKind nodeValue,
                  singularityPotential = Just (morseReebNodePotential nodeValue),
                  singularityIndex = criticalKindSingularityIndex (morseReebNodeKind nodeValue),
                  singularityReebNode = Just (morseReebNodeId nodeValue),
                  singularityIncidentArcs =
                    Map.findWithDefault [] (morseReebNodeId nodeValue) incidentArcs
                }
          )

criticalKindSingularityIndex :: CriticalKind -> SingularityIndex
criticalKindSingularityIndex criticalKindValue =
  SingularityIndex
    ( case criticalKindValue of
        Basin -> 1 % 1
        Peak -> 1 % 1
        Isolated -> 1 % 1
        Merge -> (-1) % 1
        Split -> (-1) % 1
        Pass -> (-1) % 1
    )

traceArc ::
  Map.Map Int Double ->
  Map.Map Int ReebNodeId ->
  Map.Map Int CriticalKind ->
  Graph1Skeleton ->
  ReebNodeId ->
  Int ->
  GraphEdge ->
  Int ->
  Maybe (ReebNodeId, ReebNodeId, Monotonicity, [BasisCellRef])
traceArc potentials nodeIdsByVertex criticalKindsByVertex skeleton sourceNodeId sourceVertex seedEdge seedVertex =
  let pathTail = followGradientEdges potentials criticalKindsByVertex skeleton Set.empty seedVertex
      supportCells = arcSupportCellsFromEdges sourceVertex ((seedEdge, seedVertex) : pathTail)
      targetVertex =
        case reverse pathTail of
          (_, targetValue) : _ -> targetValue
          [] -> seedVertex
   in fmap
        ( \targetNodeId ->
            ( sourceNodeId,
              targetNodeId,
              Ascending,
              supportCells
            )
        )
        (Map.lookup targetVertex nodeIdsByVertex)

followGradient :: Map.Map Int Double -> Map.Map Int CriticalKind -> Graph1Skeleton -> Set.Set Int -> Int -> [Int]
followGradient potentials criticalKindsByVertex skeleton visited currentVertex =
  currentVertex : fmap snd (followGradientEdges potentials criticalKindsByVertex skeleton visited currentVertex)

followGradientEdges :: Map.Map Int Double -> Map.Map Int CriticalKind -> Graph1Skeleton -> Set.Set Int -> Int -> [(GraphEdge, Int)]
followGradientEdges potentials criticalKindsByVertex skeleton visited currentVertex =
  if Set.member currentVertex visited
    then []
    else
      case Map.lookup currentVertex criticalKindsByVertex of
        Just _ -> []
        Nothing ->
          case higherNeighborEdges potentials skeleton currentVertex of
            [] -> []
            higherOnly ->
              let nextVertex =
                    higherOnly
                      & List.find (\(_, neighborValue) -> not (Set.member neighborValue visited))
               in case nextVertex of
                    Nothing -> []
                    Just (edgeValue, nextVertexValue) ->
                      (edgeValue, nextVertexValue) : followGradientEdges potentials criticalKindsByVertex skeleton (Set.insert currentVertex visited) nextVertexValue

arcSupportCellsFromEdges :: Int -> [(GraphEdge, Int)] -> [BasisCellRef]
arcSupportCellsFromEdges sourceVertex edgePath =
  vertexBasisCellRef sourceVertex
    : ( edgePath
          >>= ( \(edgeValue, targetVertex) ->
                  [ edgeBasisCellRef (graphEdgeIndex edgeValue),
                    vertexBasisCellRef targetVertex
                  ]
              )
      )

arcSupportCells :: Graph1Skeleton -> [Int] -> [BasisCellRef]
arcSupportCells skeleton vertexPath =
  case vertexPath of
    [] -> []
    [vertexValue] -> [vertexBasisCellRef vertexValue]
    sourceVertex : targetVertex : remainingVertices ->
      [vertexBasisCellRef sourceVertex]
        <> edgeCellsBetween skeleton sourceVertex targetVertex
        <> arcSupportCells skeleton (targetVertex : remainingVertices)

edgeCellsBetween :: Graph1Skeleton -> Int -> Int -> [BasisCellRef]
edgeCellsBetween skeleton sourceVertex targetVertex =
  graphEdges skeleton
    & filter
      ( \edgeValue ->
          let edgePair = List.sort [graphEdgeSource edgeValue, graphEdgeTarget edgeValue]
              targetPair = List.sort [sourceVertex, targetVertex]
           in edgePair == targetPair
      )
    & fmap (edgeBasisCellRef . graphEdgeIndex)

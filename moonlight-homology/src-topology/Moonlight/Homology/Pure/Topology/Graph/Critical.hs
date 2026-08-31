module Moonlight.Homology.Pure.Topology.Graph.Critical
  ( criticalNodes,
    criticalKindAt,
    lowerNeighborEdges,
    lowerNeighbors,
    higherNeighborEdges,
    higherNeighbors,
    compareVertex,
    vertexPotential,
    graphDirectionField,
    scalarPotentialByVertex,
    potentialValueAtVertex,
    edgeOrientationCoefficient,
  )
where

import Data.Function ((&))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Carrier (carrierCells)
import Moonlight.Homology.Pure.Topology.Core
import Moonlight.Homology.Pure.Topology.Graph.Skeleton
  ( edgeBasisCellRef,
    graphEdgeCarrier,
    graphVertexCarrier,
    vertexBasisCellRef,
  )
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( DirectionField,
    MorseReebNode (..),
    ReebNodeId (..),
    ScalarPotentialField,
    mkDirectionCoefficient,
    mkDirectionCochainField,
    mkDirectionSymmetryOrder,
    scalarPotentialCarrier,
    scalarPotentialSamples,
    unPotentialValue,
    PotentialValue,
  )

criticalNodes :: ScalarPotentialField -> Map.Map Int Double -> Graph1Skeleton -> Either HomologyFailure [MorseReebNode]
criticalNodes scalarPotential potentials skeleton =
  let criticalVertices =
        enumerateFromZero (graphVertexCount skeleton)
          & mapMaybe
            ( \vertexValue ->
                criticalKindAt potentials skeleton vertexValue
                  & fmap (\criticalKindValue -> (vertexValue, criticalKindValue))
            )
   in zip (fmap ReebNodeId (enumerateFromZero (length criticalVertices))) criticalVertices
        & traverse
          ( \(nodeId, (vertexValue, criticalKindValue)) ->
              potentialValueAtVertex scalarPotential vertexValue
                & maybe
                  (Left (InvalidTopologyInput "critical node construction requires scalar samples at every graph vertex"))
                  ( \potentialValue ->
                      Right
                        MorseReebNode
                          { morseReebNodeId = nodeId,
                            morseReebNodeAnchor = vertexBasisCellRef vertexValue,
                            morseReebNodeKind = criticalKindValue,
                            morseReebNodePotential = potentialValue
                          }
                  )
          )

criticalKindAt :: Map.Map Int Double -> Graph1Skeleton -> Int -> Maybe CriticalKind
criticalKindAt potentials skeleton vertexValue =
  let lowerCount = length (lowerNeighbors potentials skeleton vertexValue)
      higherCount = length (higherNeighbors potentials skeleton vertexValue)
   in case (lowerCount, higherCount) of
        (0, 0) -> Just Isolated
        (0, _) -> Just Basin
        (_, 0) -> Just Peak
        (1, 1) -> Nothing
        (lowerValue, higherValue)
          | lowerValue > 1 && higherValue > 1 -> Just Pass
          | lowerValue > 1 -> Just Merge
          | higherValue > 1 -> Just Split
          | otherwise -> Nothing

lowerNeighbors :: Map.Map Int Double -> Graph1Skeleton -> Int -> [Int]
lowerNeighbors potentials skeleton vertexValue =
  lowerNeighborEdges potentials skeleton vertexValue
    & fmap snd

higherNeighbors :: Map.Map Int Double -> Graph1Skeleton -> Int -> [Int]
higherNeighbors potentials skeleton vertexValue =
  higherNeighborEdges potentials skeleton vertexValue
    & fmap snd

lowerNeighborEdges :: Map.Map Int Double -> Graph1Skeleton -> Int -> [(GraphEdge, Int)]
lowerNeighborEdges potentials skeleton vertexValue =
  neighborEdgesWithOrdering LT potentials skeleton vertexValue

higherNeighborEdges :: Map.Map Int Double -> Graph1Skeleton -> Int -> [(GraphEdge, Int)]
higherNeighborEdges potentials skeleton vertexValue =
  neighborEdgesWithOrdering GT potentials skeleton vertexValue

neighborEdgesWithOrdering :: Ordering -> Map.Map Int Double -> Graph1Skeleton -> Int -> [(GraphEdge, Int)]
neighborEdgesWithOrdering expectedOrdering potentials skeleton vertexValue =
  graphEdgeAdjacency skeleton
    & Map.findWithDefault [] vertexValue
    & List.sortOn graphEdgeIndex
    & mapMaybe
      ( \edgeValue ->
          fmap
            (\neighborValue -> (edgeValue, neighborValue))
            (oppositeGraphVertex vertexValue edgeValue)
      )
    & filter (\(_, neighborValue) -> compareVertex potentials neighborValue vertexValue == expectedOrdering)

oppositeGraphVertex :: Int -> GraphEdge -> Maybe Int
oppositeGraphVertex vertexValue edgeValue =
  if graphEdgeSource edgeValue == vertexValue
    then Just (graphEdgeTarget edgeValue)
    else
      if graphEdgeTarget edgeValue == vertexValue
        then Just (graphEdgeSource edgeValue)
        else Nothing

compareVertex :: Map.Map Int Double -> Int -> Int -> Ordering
compareVertex potentials leftVertex rightVertex =
  compare (vertexPotential potentials leftVertex, leftVertex) (vertexPotential potentials rightVertex, rightVertex)

vertexPotential :: Map.Map Int Double -> Int -> Double
vertexPotential potentials vertexValue = Map.findWithDefault 0.0 vertexValue potentials

graphDirectionField :: Map.Map Int Double -> Graph1Skeleton -> Either HomologyFailure DirectionField
graphDirectionField potentials skeleton =
  let edgeCells =
        graphEdges skeleton
          & fmap (edgeBasisCellRef . graphEdgeIndex)
   in do
        edgeCarrier <- graphEdgeCarrier edgeCells & either (Left . InvalidTopologyInput . show) Right
        coefficients <-
          graphEdges skeleton
            & traverse
              ( \edgeValue -> do
                  coefficientValue <-
                    mkDirectionCoefficient (edgeOrientationCoefficient potentials edgeValue)
                      & either
                        (Left . (InvalidTopologyInput . ("graph direction field requires finite coefficients: " <>) . show))
                        Right
                  pure
                    ( edgeBasisCellRef (graphEdgeIndex edgeValue),
                      coefficientValue
                    )
              )
        symmetryOrder <-
          mkDirectionSymmetryOrder 1
            & either
              (Left . (InvalidTopologyInput . ("graph direction field requires a positive symmetry order: " <>) . show))
              Right
        mkDirectionCochainField edgeCarrier symmetryOrder (Map.fromList coefficients)
          & either
            (Left . (InvalidTopologyInput . ("graph direction field requires exact carrier coverage: " <>) . show))
            Right

scalarPotentialByVertex :: Graph1Skeleton -> ScalarPotentialField -> Either HomologyFailure (Map.Map Int Double)
scalarPotentialByVertex skeleton scalarPotential = do
  expectedCarrier <- graphVertexCarrier skeleton & either (Left . InvalidTopologyInput . show) Right
  let expectedCells = carrierCells expectedCarrier
      sampleMap = scalarPotentialSamples scalarPotential
      sampleDomain = Map.keysSet sampleMap
      expectedDomain = Set.fromList expectedCells
  if scalarPotentialCarrier scalarPotential /= expectedCarrier
    then Left (InvalidTopologyInput "graph macro scaffold requires a scalar potential carried exactly by the graph 0-cells")
    else
      if sampleDomain /= expectedDomain
        then Left (InvalidTopologyInput "scalar potential samples must cover the graph 0-cell carrier exactly")
        else
          expectedCells
            & traverse
              ( \cellRefValue ->
                  fmap
                    (\potentialValue -> (cellIndex cellRefValue, unPotentialValue potentialValue))
                    (Map.lookup cellRefValue sampleMap)
              )
            & maybe
              (Left (InvalidTopologyInput "scalar potential lookup failed after carrier validation"))
              (Right . Map.fromList)

potentialValueAtVertex :: ScalarPotentialField -> Int -> Maybe PotentialValue
potentialValueAtVertex scalarPotential vertexValue =
  Map.lookup (vertexBasisCellRef vertexValue) (scalarPotentialSamples scalarPotential)

edgeOrientationCoefficient :: Map.Map Int Double -> GraphEdge -> Double
edgeOrientationCoefficient potentials edgeValue =
  case compareVertex potentials (graphEdgeSource edgeValue) (graphEdgeTarget edgeValue) of
    LT -> 1.0
    GT -> -1.0
    EQ -> 0.0

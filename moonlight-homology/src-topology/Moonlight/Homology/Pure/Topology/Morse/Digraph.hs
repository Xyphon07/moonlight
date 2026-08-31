-- | Directed-graph machinery for algebraic discrete Morse theory: edge maps
-- over basis cells, adjacency projections, DFS topological order, three-color
-- acyclicity certification, and gradient path-weight propagation.
--
-- This layer is deliberately theory-free: it knows nothing about matchings,
-- reductions, or chain complexes — only 'BasisCellRef' vertices, weighted
-- directed edges, and the DAG algorithms 'Moonlight.Homology.Pure.Topology.Morse'
-- runs on top of them.
module Moonlight.Homology.Pure.Topology.Morse.Digraph
  ( DirectedEdgeMap,
    DirectedAdjacencyMap,
    WeightedAdjacencyMap,
    PathWeightOracle (..),
    adjacencyMap,
    deleteAdjacencyEdge,
    insertAdjacencyEdge,
    topologicalOrderWithVertices,
    pathWeightOracle,
    pathWeightsFromOracle,
    graphIsAcyclic,
    basisCellDimension,
    basisCellKey,
  )
where

import Data.Kind (Type)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Homology.Pure.Carrier (BasisCellRef (..))
import Moonlight.Homology.Pure.Degree (HomologicalDegree (..))

type DirectedEdgeMap :: Type -> Type
type DirectedEdgeMap r = Map (BasisCellRef, BasisCellRef) r

type DirectedAdjacencyMap :: Type
type DirectedAdjacencyMap = Map BasisCellRef [BasisCellRef]

type WeightedAdjacencyMap :: Type -> Type
type WeightedAdjacencyMap r = Map BasisCellRef [(r, BasisCellRef)]

type PathWeightOracle :: Type -> Type
newtype PathWeightOracle r = PathWeightOracle
  { pwoPathWeightsFrom :: BasisCellRef -> Map BasisCellRef r
  }

rankOf :: Map BasisCellRef Int -> BasisCellRef -> Int
rankOf rankValue cell =
  Map.findWithDefault maxBound cell rankValue

topologicalRank :: [BasisCellRef] -> Map BasisCellRef Int
topologicalRank orderValue =
  Map.fromList (zip orderValue [0 :: Int ..])

adjacencyMap :: DirectedEdgeMap r -> DirectedAdjacencyMap
adjacencyMap edgeMap =
  Map.map
    (sortOn basisCellKey)
    ( Map.fromListWith
        (<>)
        [ (fromCell, [toCell])
        | ((fromCell, toCell), _) <- Map.toAscList edgeMap
        ]
    )

deleteAdjacencyEdge ::
  BasisCellRef ->
  BasisCellRef ->
  DirectedAdjacencyMap ->
  DirectedAdjacencyMap
deleteAdjacencyEdge fromCell toCell =
  Map.update prunedSuccessors fromCell
  where
    prunedSuccessors successorCells =
      case filter (/= toCell) successorCells of
        [] -> Nothing
        remainingSuccessors -> Just remainingSuccessors

insertAdjacencyEdge ::
  BasisCellRef ->
  BasisCellRef ->
  DirectedAdjacencyMap ->
  DirectedAdjacencyMap
insertAdjacencyEdge fromCell toCell =
  Map.insertWith mergeSuccessors fromCell [toCell]
  where
    mergeSuccessors insertedSuccessors existingSuccessors =
      sortOn basisCellKey (insertedSuccessors <> filter (/= toCell) existingSuccessors)

topologicalOrderWithVertices :: [BasisCellRef] -> DirectedAdjacencyMap -> [BasisCellRef]
topologicalOrderWithVertices vertices adjacency =
  snd (foldl' visitVertex (Set.empty, []) graphVertices)
  where
    graphVertices =
      Set.toAscList
        (Set.fromList vertices <> Map.keysSet adjacency <> foldMap Set.fromList (Map.elems adjacency))
    visitVertex (visited, orderedCells) vertex
      | Set.member vertex visited = (visited, orderedCells)
      | otherwise = visitFrom visited orderedCells vertex
    visitFrom visited orderedCells vertex
      | Set.member vertex visited = (visited, orderedCells)
      | otherwise =
          let (visitedAfterSuccessors, orderedAfterSuccessors) =
                foldl'
                  ( \(visitedState, orderedState) successorCell ->
                      visitFrom visitedState orderedState successorCell
                  )
                  (Set.insert vertex visited, orderedCells)
                  (Map.findWithDefault [] vertex adjacency)
           in (visitedAfterSuccessors, vertex : orderedAfterSuccessors)

pathWeightOracle ::
  (Eq r, Num r) =>
  DirectedEdgeMap r ->
  [BasisCellRef] ->
  PathWeightOracle r
pathWeightOracle edgeMap topologicalVertexOrder =
  PathWeightOracle
    { pwoPathWeightsFrom =
        pathWeightsFromSource weightedAdjacency topologicalVertexOrder (topologicalRank topologicalVertexOrder)
    }
  where
    weightedAdjacency = weightedAdjacencyMap edgeMap

weightedAdjacencyMap :: DirectedEdgeMap r -> WeightedAdjacencyMap r
weightedAdjacencyMap edgeMap =
  Map.map
    (sortOn (basisCellKey . snd))
    ( Map.fromListWith
        (<>)
        [ (fromCell, [(coefficientValue, toCell)])
        | ((fromCell, toCell), coefficientValue) <- Map.toAscList edgeMap
        ]
    )

pathWeightsFromOracle :: PathWeightOracle r -> BasisCellRef -> Map BasisCellRef r
pathWeightsFromOracle pathWeightSums =
  pwoPathWeightsFrom pathWeightSums

pathWeightsFromSource ::
  (Eq r, Num r) =>
  WeightedAdjacencyMap r ->
  [BasisCellRef] ->
  Map BasisCellRef Int ->
  BasisCellRef ->
  Map BasisCellRef r
pathWeightsFromSource weightedAdjacency topologicalVertexOrder rankValue startCell =
  Map.filter (/= 0) $
    foldl'
      propagatePathWeights
      (Map.singleton startCell 1)
      (topologicalSuffix rankValue startCell topologicalVertexOrder)
  where
    propagatePathWeights pathWeights currentCell =
      case Map.lookup currentCell pathWeights of
        Nothing -> pathWeights
        Just currentWeight ->
          foldl'
            (accumulateWeightedSuccessor currentWeight)
            pathWeights
            (Map.findWithDefault [] currentCell weightedAdjacency)

accumulateWeightedSuccessor ::
  (Eq r, Num r) =>
  r ->
  Map BasisCellRef r ->
  (r, BasisCellRef) ->
  Map BasisCellRef r
accumulateWeightedSuccessor currentWeight pathWeights (edgeWeight, successorCell) =
  let contribution = currentWeight * edgeWeight
   in if contribution == 0
        then pathWeights
        else Map.alter (addPathContribution contribution) successorCell pathWeights

addPathContribution :: (Eq r, Num r) => r -> Maybe r -> Maybe r
addPathContribution contribution existingValue =
  let nextValue = maybe contribution (+ contribution) existingValue
   in if nextValue == 0
        then Nothing
        else Just nextValue

topologicalSuffix ::
  Map BasisCellRef Int ->
  BasisCellRef ->
  [BasisCellRef] ->
  [BasisCellRef]
topologicalSuffix rankValue startCell =
  filter (\cell -> rankOf rankValue cell >= rankOf rankValue startCell)

graphIsAcyclic :: DirectedEdgeMap r -> Bool
graphIsAcyclic edgeMap =
  visitAll Set.empty graphVertices
  where
    adjacency = adjacencyMap edgeMap
    graphVertices =
      Set.toAscList
        ( Set.fromList
            [ cell
            | ((fromCell, toCell), _) <- Map.toList edgeMap,
              cell <- [fromCell, toCell]
            ]
        )
    visitAll permanentlyVisited remainingVertices =
      case remainingVertices of
        [] -> True
        vertex : restVertices
          | Set.member vertex permanentlyVisited -> visitAll permanentlyVisited restVertices
          | otherwise ->
              case visit permanentlyVisited Set.empty vertex of
                Nothing -> False
                Just permanentlyVisited' -> visitAll permanentlyVisited' restVertices
    visit permanentlyVisited temporarilyVisited vertex
      | Set.member vertex temporarilyVisited = Nothing
      | Set.member vertex permanentlyVisited = Just permanentlyVisited
      | otherwise =
          let temporarilyVisited' = Set.insert vertex temporarilyVisited
           in case visitSuccessors permanentlyVisited temporarilyVisited' (Map.findWithDefault [] vertex adjacency) of
                Nothing -> Nothing
                Just permanentlyVisited' -> Just (Set.insert vertex permanentlyVisited')
    visitSuccessors permanentlyVisited temporarilyVisited successorCells =
      case successorCells of
        [] -> Just permanentlyVisited
        successorCell : restCells ->
          case visit permanentlyVisited temporarilyVisited successorCell of
            Nothing -> Nothing
            Just permanentlyVisited' -> visitSuccessors permanentlyVisited' temporarilyVisited restCells

basisCellDimension :: BasisCellRef -> Int
basisCellDimension BasisCellRef {cellDegree = HomologicalDegree degreeValue} = degreeValue

basisCellKey :: BasisCellRef -> (Int, Int)
basisCellKey BasisCellRef {cellDegree = HomologicalDegree degreeValue, cellIndex = indexValue} =
  (degreeValue, indexValue)

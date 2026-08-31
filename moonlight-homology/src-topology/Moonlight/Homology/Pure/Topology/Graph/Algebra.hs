module Moonlight.Homology.Pure.Topology.Graph.Algebra
  ( addUndirectedAdjacency,
    connectedComponentsFromAdjacency,
  )
where

import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Data.Bifunctor (second)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

addUndirectedAdjacency ::
  Ord label =>
  label ->
  label ->
  Map label [label] ->
  Map label [label]
addUndirectedAdjacency sourceLabel targetLabel =
  Map.insertWith (<>) sourceLabel [targetLabel]
    . Map.insertWith (<>) targetLabel [sourceLabel]

connectedComponentsFromAdjacency :: (Ord label, Foldable adjacency) => Map label (adjacency label) -> Int
connectedComponentsFromAdjacency =
  length
    . strongComponentSets
    . AdjacencyMap.symmetricClosure
    . AdjacencyMap.fromAdjacencySets
    . fmap (second (Set.fromList . Foldable.toList))
    . Map.toAscList

strongComponentSets :: Ord vertex => AdjacencyMap.AdjacencyMap vertex -> [Set.Set vertex]
strongComponentSets graph =
  List.sortOn
    Set.lookupMin
    ( fmap
        (Set.fromList . NonEmpty.toList . NonEmptyAdjacencyMap.vertexList1)
        (AdjacencyMap.vertexList (AdjacencyMapAlgorithm.scc graph))
    )

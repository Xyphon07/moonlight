-- | Homotopy-flavoured queries over nerves: connected components and
-- core\/automorphism groupoids.
module Moonlight.Category.Pure.Simplicial.Homotopy
  ( pi0Nerve,
    CoreGroupoid,
    CoreGroupoidObject,
    CoreGroupoidMorphism,
    AutomorphismGroupoid,
    AutomorphismGroupoidObject,
    AutomorphismGroupoidMorphism,
    forgetCoreGroupoidObject,
    forgetCoreGroupoidMorphism,
    forgetAutomorphismGroupoidObject,
    forgetAutomorphismGroupoidMorphism,
    coreGroupoidOfNerve,
    coreGroupoidObjects,
    coreGroupoidMorphisms,
    coreGroupoidMorphismsBetween,
    automorphismGroupoidOfNerve,
    automorphismGroupoidObjects,
    automorphismGroupAt,
  )
where

import Algebra.Graph.AdjacencyMap qualified as AdjacencyMap
import Algebra.Graph.AdjacencyMap.Algorithm qualified as AdjacencyMapAlgorithm
import Algebra.Graph.NonEmpty.AdjacencyMap qualified as NonEmptyAdjacencyMap
import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.FiniteComposable (FiniteComposableCategory (..))
import Moonlight.Category.Pure.Invertibility
  ( AutomorphismGroupoid,
    AutomorphismGroupoidMorphism,
    AutomorphismGroupoidObject,
    CoreGroupoid,
    CoreGroupoidMorphism,
    CoreGroupoidObject,
    automorphismGroupoid,
    automorphismGroupAt,
    automorphismGroupoidObjects,
    coreGroupoid,
    coreGroupoidObjects,
    coreGroupoidMorphisms,
    coreGroupoidMorphismsBetween,
    forgetAutomorphismGroupoidMorphism,
    forgetAutomorphismGroupoidObject,
    forgetCoreGroupoidMorphism,
    forgetCoreGroupoidObject,
  )

adjacencyFromEdges :: Ord a => [a] -> [(a, a)] -> Map a (Set a)
adjacencyFromEdges vertices edges =
  let vertexAdjacency =
        foldr
          (\vertex -> Map.insertWith Set.union vertex Set.empty)
          Map.empty
          vertices
      edgeAdjacency =
        foldr
          (\(sourceVertex, targetVertex) ->
             Map.insertWith Set.union sourceVertex (Set.singleton targetVertex)
               . Map.insertWith Set.union targetVertex (Set.singleton sourceVertex)
          )
          vertexAdjacency
          edges
   in edgeAdjacency

pi0Nerve :: (FiniteComposableCategory c, Ord (Ob c)) => c -> [[Ob c]]
pi0Nerve categoryValue =
  let objects = enumerateObjects categoryValue
      undirectedEdges =
        enumerateMorphisms categoryValue
          & mapMaybe
            ( \morphism ->
                case (source categoryValue morphism, target categoryValue morphism) of
                  (Right sourceObject, Right targetObject) -> Just (sourceObject, targetObject)
                  _ -> Nothing
            )
   in componentsFromAdjacency (adjacencyFromEdges objects undirectedEdges)
        & fmap Set.toAscList

coreGroupoidOfNerve ::
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  CoreGroupoid c
coreGroupoidOfNerve =
  coreGroupoid

automorphismGroupoidOfNerve :: 
  (FiniteComposableCategory c, Ord (Ob c), Ord (Mor c)) =>
  c ->
  AutomorphismGroupoid c
automorphismGroupoidOfNerve =
  automorphismGroupoid

componentsFromAdjacency :: Ord vertex => Map vertex (Set vertex) -> [Set vertex]
componentsFromAdjacency =
  strongComponentSets
    . AdjacencyMap.symmetricClosure
    . AdjacencyMap.fromAdjacencySets
    . Map.toAscList

strongComponentSets :: Ord vertex => AdjacencyMap.AdjacencyMap vertex -> [Set vertex]
strongComponentSets graph =
  List.sortOn
    Set.lookupMin
    ( fmap
        (Set.fromList . NonEmpty.toList . NonEmptyAdjacencyMap.vertexList1)
        (AdjacencyMap.vertexList (AdjacencyMapAlgorithm.scc graph))
    )

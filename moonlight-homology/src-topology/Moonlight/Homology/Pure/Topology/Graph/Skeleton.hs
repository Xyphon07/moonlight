module Moonlight.Homology.Pure.Topology.Graph.Skeleton
  ( graph1SkeletonFromComplex,
    GraphSkeletonExtractionFailure (..),
    GraphOneComplex (..),
    graphOneComplexFromComplex,
    graphFiniteChainComplex,
    edgeTargetsByIndex,
    endpointPair,
    graphFromEdgeSupports,
    addUndirectedEdge,
    vertexBasisCellRef,
    edgeBasisCellRef,
    graphVertexCarrier,
    graphEdgeCarrier,
  )
where

import Data.Function ((&))
import Data.Graph qualified as Graph
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Tree qualified as Tree
import Data.Vector qualified as Vector
import Moonlight.Core (Semiring)
import Moonlight.Homology.Boundary.Finite
  ( FiniteChainComplex,
    incidenceMatrixAt,
    mkFiniteChainComplex,
  )
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryEntry,
    BoundaryIncidence,
    boundaryCoefficient,
    boundaryEntries,
    boundaryEntriesBySource,
    emptyBoundaryIncidence,
    emptyBoundaryIncidenceOf,
    materializeIncidenceBoundary,
    sourceCardinality,
    sourceIndex,
    targetCardinality,
    targetIndex,
  )
import Moonlight.Homology.Pure.Chain (HomologicalDegree (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure)
import Moonlight.Homology.Pure.Carrier (CellCarrier, CellCarrierError, mkCellCarrier)
import Moonlight.Homology.Pure.Topology.Core

data GraphSkeletonExtractionFailure
  = UnsupportedGraphSkeletonDimensions [HomologicalDegree]
  | NonEmptyGraphZeroBoundary
  | GraphEdgeBoundaryTargetMismatch Int Int
  | InvalidOrientedUnitGraphEdgeBoundary Int
  deriving stock (Eq, Show)

data GraphOneComplex = GraphOneComplex
  { graphOneVertexCount :: !Int,
    graphOneEdgeCount :: !Int,
    graphOneComponents :: ![Set.Set Int]
  }
  deriving stock (Eq, Show)

graphOneComplexFromComplex :: Integral r => FiniteChainComplex r -> Maybe GraphOneComplex
graphOneComplexFromComplex finite =
  case graphOneComplexShape finite of
    Right (vertexCount, edgeCount, edgeSupports) ->
      Just (mkGraphOneComplex vertexCount edgeCount edgeSupports)
    Left _ ->
      Nothing

graphOneComplexShape :: Integral r => FiniteChainComplex r -> Either GraphSkeletonExtractionFailure (Int, Int, [(Int, Int)])
graphOneComplexShape finite =
  case dimensionsOf finite of
    [HomologicalDegree 0] ->
      graph1SkeletonShape finite
    [HomologicalDegree 0, HomologicalDegree 1] ->
      graph1SkeletonShape finite
    dimensionValues ->
      Left (UnsupportedGraphSkeletonDimensions dimensionValues)

graph1SkeletonShape :: Integral r => FiniteChainComplex r -> Either GraphSkeletonExtractionFailure (Int, Int, [(Int, Int)])
graph1SkeletonShape finite =
  case dimensionsOf finite of
    [HomologicalDegree 0] ->
      let zeroBoundary = incidenceMatrixAt finite (HomologicalDegree 0)
       in fmap
            (\vertexCount -> (vertexCount, 0, []))
            (validateZeroBoundary zeroBoundary)
    HomologicalDegree 0 : HomologicalDegree 1 : _ ->
      let zeroBoundary = incidenceMatrixAt finite (HomologicalDegree 0)
          edgeBoundary = incidenceMatrixAt finite (HomologicalDegree 1)
       in do
            vertexCount <- validateZeroBoundary zeroBoundary
            validateEdgeBoundaryTarget vertexCount edgeBoundary
            edgeSupports <- orientedUnitEdgeSupports edgeBoundary
            pure (vertexCount, sourceCardinality edgeBoundary, edgeSupports)
    dimensionValues ->
      Left (UnsupportedGraphSkeletonDimensions dimensionValues)

validateZeroBoundary :: BoundaryIncidence r -> Either GraphSkeletonExtractionFailure Int
validateZeroBoundary zeroBoundary =
  if targetCardinality zeroBoundary == 0 && null (boundaryEntries zeroBoundary)
    then Right (sourceCardinality zeroBoundary)
    else Left NonEmptyGraphZeroBoundary

validateEdgeBoundaryTarget :: Int -> BoundaryIncidence r -> Either GraphSkeletonExtractionFailure ()
validateEdgeBoundaryTarget vertexCount edgeBoundary =
  if targetCardinality edgeBoundary == vertexCount
    then Right ()
    else Left (GraphEdgeBoundaryTargetMismatch vertexCount (targetCardinality edgeBoundary))

mkGraphOneComplex :: Int -> Int -> [(Int, Int)] -> GraphOneComplex
mkGraphOneComplex vertexCount edgeCount edgeSupports =
  GraphOneComplex
    { graphOneVertexCount = vertexCount,
      graphOneEdgeCount = edgeCount,
      graphOneComponents = graphComponentsFromSupports vertexCount edgeSupports
    }

graphComponentsFromSupports :: Int -> [(Int, Int)] -> [Set.Set Int]
graphComponentsFromSupports vertexCount edgeSupports =
  if vertexCount <= 0
    then []
    else
      edgeSupports
        & foldMap (\(leftVertex, rightVertex) -> [(leftVertex, rightVertex), (rightVertex, leftVertex)])
        & Graph.buildG (0, vertexCount - 1)
        & Graph.components
        & fmap (Set.fromList . Tree.flatten)

orientedUnitEdgeSupports :: Integral r => BoundaryIncidence r -> Either GraphSkeletonExtractionFailure [(Int, Int)]
orientedUnitEdgeSupports edgeBoundary =
  enumerateFromZero (sourceCardinality edgeBoundary)
    & traverse (unitEdgeSupport (boundaryEntriesBySource edgeBoundary))

unitEdgeSupport :: Integral r => Vector.Vector [BoundaryEntry r] -> Int -> Either GraphSkeletonExtractionFailure (Int, Int)
unitEdgeSupport groupedEntries edgeIndex =
  case traverse signedUnitTarget (filter ((/= 0) . boundaryCoefficient) (maybe [] id (groupedEntries Vector.!? edgeIndex))) of
    Just [(leftTarget, leftSign), (rightTarget, rightSign)]
      | leftTarget /= rightTarget && leftSign + rightSign == 0 ->
          Right (leftTarget, rightTarget)
    _ ->
      Left (InvalidOrientedUnitGraphEdgeBoundary edgeIndex)

signedUnitTarget :: Integral r => BoundaryEntry r -> Maybe (Int, Integer)
signedUnitTarget entryValue =
  let coefficientValue = fromIntegral (boundaryCoefficient entryValue) :: Integer
   in if abs coefficientValue == 1
        then Just (targetIndex entryValue, coefficientValue)
        else Nothing

graph1SkeletonFromComplex :: Integral r => FiniteChainComplex r -> Either GraphSkeletonExtractionFailure Graph1Skeleton
graph1SkeletonFromComplex finite =
  fmap
    (\(vertexCount, _, edgeSupports) -> graphFromEdgeSupports vertexCount edgeSupports)
    (graph1SkeletonShape finite)

graphFiniteChainComplex :: (Eq r, Num r, Semiring r) => Graph1Skeleton -> Either HomologyFailure (FiniteChainComplex r)
graphFiniteChainComplex skeleton = do
  edgeBoundary <-
    materializeIncidenceBoundary
      graphEdgeBoundary
      (graphEdges skeleton)
      (enumerateFromZero (graphVertexCount skeleton))
  pure
    ( mkFiniteChainComplex
        (HomologicalDegree 1)
        ( \degreeValue ->
            case degreeValue of
              HomologicalDegree 0 -> emptyBoundaryIncidenceOf (fromIntegral (graphVertexCount skeleton)) 0
              HomologicalDegree 1 -> edgeBoundary
              _ -> emptyBoundaryIncidence
        )
    )

edgeTargetsByIndex :: Integral r => BoundaryIncidence r -> Map.Map Int (Set.Set Int)
edgeTargetsByIndex incidence =
  boundaryEntries incidence
    & filter (\entry -> boundaryCoefficient entry /= 0)
    & fmap (\entry -> (sourceIndex entry, Set.singleton (targetIndex entry)))
    & Map.fromListWith Set.union

endpointPair :: Int -> Set.Set Int -> Maybe (Int, Int)
endpointPair _ endpointSet =
  case Set.toAscList endpointSet of
    [sourceVertex, targetVertex] -> Just (sourceVertex, targetVertex)
    [] -> Nothing
    [_] -> Nothing
    _ -> Nothing

addUndirectedEdge :: Map.Map Int [GraphEdge] -> GraphEdge -> Map.Map Int [GraphEdge]
addUndirectedEdge adjacency edgeValue =
  if graphEdgeSource edgeValue == graphEdgeTarget edgeValue
    then Map.insertWith (<>) (graphEdgeSource edgeValue) [edgeValue] adjacency
    else
      Map.insertWith (<>) (graphEdgeSource edgeValue) [edgeValue]
        (Map.insertWith (<>) (graphEdgeTarget edgeValue) [edgeValue] adjacency)

graphFromEdgeSupports :: Int -> [(Int, Int)] -> Graph1Skeleton
graphFromEdgeSupports vertexCount edgeSupports =
  let edges =
        edgeSupports
          & zip [0 :: Int ..]
          & fmap
            ( \(edgeIndexValue, (sourceVertex, targetVertex)) ->
                GraphEdge
                  { graphEdgeIndex = edgeIndexValue,
                    graphEdgeSource = sourceVertex,
                    graphEdgeTarget = targetVertex
                  }
            )
      edgeAdjacency =
        foldr
          (flip addUndirectedEdge)
          (Map.fromList (enumerateFromZero vertexCount & fmap (\vertex -> (vertex, []))))
          edges
   in Graph1Skeleton
        { graphVertexCount = vertexCount,
          graphEdges = edges,
          graphEdgeAdjacency = edgeAdjacency
        }

graphVertexCarrier :: Graph1Skeleton -> Either CellCarrierError CellCarrier
graphVertexCarrier skeleton =
  mkCellCarrier (HomologicalDegree 0) (vertexBasisCellRef <$> enumerateFromZero (graphVertexCount skeleton))

graphEdgeCarrier :: [BasisCellRef] -> Either CellCarrierError CellCarrier
graphEdgeCarrier edgeCells =
  mkCellCarrier (HomologicalDegree 1) edgeCells

vertexBasisCellRef :: Int -> BasisCellRef
vertexBasisCellRef vertexIndex =
  BasisCellRef
    { cellDegree = HomologicalDegree 0,
      cellIndex = vertexIndex
    }

edgeBasisCellRef :: Int -> BasisCellRef
edgeBasisCellRef edgeIndex =
  BasisCellRef
    { cellDegree = HomologicalDegree 1,
      cellIndex = edgeIndex
    }

graphEdgeBoundary :: Num r => GraphEdge -> [(r, Int)]
graphEdgeBoundary graphEdgeValue =
  [ (negate 1, graphEdgeSource graphEdgeValue),
    (1, graphEdgeTarget graphEdgeValue)
  ]

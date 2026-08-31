{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Moonlight.Homology.Boundary.GraphGF2
  ( GraphBoundaryGF2,
    gbgf2VertexIndex,
    gbgf2Edges,
    gbgf2Boundary,
    GraphBoundaryGF2Failure (..),
    prepareGraphBoundaryGF2,
    graphBoundaryRankDefectGF2,
  )
where

import Data.Bifunctor
  ( first,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.Kind
  ( Type,
  )
import Data.Map.Strict
  ( Map,
  )
import Data.Map.Strict qualified as Map
import Data.Set
  ( Set,
  )
import Data.Set qualified as Set
import Moonlight.Homology.Boundary.LinAlg
  ( BoundaryEntry,
    BoundaryIncidenceShapeError,
    mkBoundaryEntry,
    mkBoundaryIncidenceFromOrderedEntries,
  )
import Moonlight.Homology.Rank.GF2
  ( GF2RankFailure,
    PreparedGF2Boundary,
    prepareGF2Boundary,
    rankPreparedGF2Boundary,
  )
import Moonlight.LinAlg.Pure.Dense.GF2
  ( GF2 (..),
  )

type GraphBoundaryGF2 :: Type -> Type
data GraphBoundaryGF2 cell = GraphBoundaryGF2
  { graphBoundaryVertexIndex :: !(Map cell Int),
    graphBoundaryEdges :: !(IntMap (cell, cell)),
    graphBoundaryPrepared :: !PreparedGF2Boundary
  }
  deriving stock (Eq, Show)

gbgf2VertexIndex :: GraphBoundaryGF2 cell -> Map cell Int
gbgf2VertexIndex =
  graphBoundaryVertexIndex

gbgf2Edges :: GraphBoundaryGF2 cell -> IntMap (cell, cell)
gbgf2Edges =
  graphBoundaryEdges

gbgf2Boundary :: GraphBoundaryGF2 cell -> PreparedGF2Boundary
gbgf2Boundary =
  graphBoundaryPrepared

type GraphBoundaryGF2Failure :: Type -> Type
data GraphBoundaryGF2Failure cell
  = GraphBoundaryGF2EndpointMissing !Int !cell !(cell, cell)
  | GraphBoundaryGF2BoundaryShapeFailed !BoundaryIncidenceShapeError
  | GraphBoundaryGF2RankFailed !GF2RankFailure
  deriving stock (Eq, Show)

prepareGraphBoundaryGF2 ::
  Ord cell =>
  Set cell ->
  [(cell, cell)] ->
  Either (GraphBoundaryGF2Failure cell) (GraphBoundaryGF2 cell)
prepareGraphBoundaryGF2 vertices edges = do
  edgeTerms <- traverse edgeBoundaryTerms indexedEdges
  incidence <-
    first GraphBoundaryGF2BoundaryShapeFailed $
      mkBoundaryIncidenceFromOrderedEntries
        (fromIntegral edgeCount)
        (fromIntegral vertexCount)
        (concat edgeTerms)
  preparedBoundary <-
    first GraphBoundaryGF2RankFailed $
      prepareGF2Boundary incidence
  Right
    GraphBoundaryGF2
      { graphBoundaryVertexIndex = vertexIndex,
        graphBoundaryEdges = IntMap.fromAscList indexedEdges,
        graphBoundaryPrepared = preparedBoundary
      }
  where
    vertexIndex =
      Map.fromList (zip (Set.toAscList vertices) [0 :: Int ..])

    vertexCount =
      Set.size vertices

    indexedEdges =
      zip [0 :: Int ..] edges

    edgeCount =
      length edges

    edgeBoundaryTerms (edgeIndex, edgeValue@(sourceCell, targetCell)) = do
      sourceOffset <- endpointOffset edgeIndex sourceCell edgeValue
      targetOffset <- endpointOffset edgeIndex targetCell edgeValue
      Right (boundaryTermsForEdge edgeIndex sourceOffset targetOffset)

    endpointOffset edgeIndex endpointCell edgeValue =
      maybe
        (Left (GraphBoundaryGF2EndpointMissing edgeIndex endpointCell edgeValue))
        Right
        (Map.lookup endpointCell vertexIndex)

graphBoundaryRankDefectGF2 :: GraphBoundaryGF2 cell -> Int
graphBoundaryRankDefectGF2 boundaryValue =
  IntMap.size (graphBoundaryEdges boundaryValue)
    - rankPreparedGF2Boundary (graphBoundaryPrepared boundaryValue)
{-# INLINE graphBoundaryRankDefectGF2 #-}

boundaryTermsForEdge :: Int -> Int -> Int -> [BoundaryEntry GF2]
boundaryTermsForEdge edgeIndex sourceOffset targetOffset =
  fmap
    (\targetOffsetValue -> mkBoundaryEntry (fromIntegral edgeIndex) (fromIntegral targetOffsetValue) GF2One)
    [min sourceOffset targetOffset, max sourceOffset targetOffset]

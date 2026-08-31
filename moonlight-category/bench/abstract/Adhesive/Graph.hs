{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Adhesive.Graph
  ( finiteGraphDPOBenchmarks,
  )
where

import BenchSupport (boolWeight)
import Control.DeepSeq (NFData (..))
import Control.Monad (guard)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Moonlight.Category.Pure.Adhesive
  ( AdhesiveCategory (..),
    DenseIntSet,
    MonicMatchComponents (..),
    PBPOAdhesiveCategory (..),
    PBPOComplementComponents (..),
    PBPOComplementWitness,
    PushoutComplementWitness,
    PushoutComplementComponents (..),
    denseIntSetDifference,
    denseIntSetFoldl',
    denseIntSetFromAscList,
    denseIntSetFull,
    denseIntSetIntersection,
    denseIntSetIntersects,
    denseIntSetInterval,
    denseIntSetIsSubsetOf,
    denseIntSetMember,
    denseIntSetSize,
    denseIntSetUnion,
    denseIntSetUniverseSize,
    denseIntSetWeight,
    monicMatchArrow,
    pbpoComplement,
    pbpoComplementBorrowedLeg,
    pbpoComplementPullbackObject,
    pbpoComplementPullbackToBorrowed,
    pbpoComplementPullbackToMatch,
    pbpoComplementPushoutFromComplement,
    pbpoComplementPushoutFromMatch,
    pbpoComplementPushoutObject,
    pbpoComplementResidualLeg,
    pbpoPullbackSquareCommutes,
    pbpoPushoutSquareCommutes,
    pushoutComplement,
    pushoutComplementBorrowedLeg,
    pushoutComplementObject,
    pushoutComplementResidualLeg,
    pushoutComplementSquareCommutes,
    witnessMonic,
  )
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.Limits (HasPullbacks (..), HasPushouts (..), pullback, pushout)
import Test.Tasty.Bench (Benchmark, bench, bgroup, env, nf)

newtype GraphId = GraphId {unGraphId :: Int}
  deriving stock (Eq, Ord, Show)

data GraphCategory = GraphCategory
  { graphCategoryCarrier :: !GraphCarrier
  }
  deriving stock (Show)

data GraphTwoMor

data GraphCompositor = GraphCompositor

data GraphCategoryError
  = GraphBoundaryMismatch
  | GraphCompositeInvalid
  deriving stock (Eq, Show)

data GraphEdge = GraphEdge
  { graphEdgeSource :: !Int,
    graphEdgeTarget :: !Int
  }
  deriving stock (Eq, Show)

data GraphCarrier = GraphCarrier
  { graphCarrierId :: !GraphId,
    graphCarrierVertices :: !DenseIntSet,
    graphCarrierEdgeIds :: !DenseIntSet,
    graphCarrierEdges :: !(IntMap GraphEdge),
    graphCarrierIncidentEdges :: !(Vector DenseIntSet)
  }
  deriving stock (Show)

data GraphObject = GraphObject
  { graphObjectCarrierId :: !GraphId,
    graphObjectVertices :: !DenseIntSet,
    graphObjectEdges :: !DenseIntSet,
    graphObjectVertexCount :: !Int,
    graphObjectEdgeCount :: !Int
  }
  deriving stock (Show)

data GraphDeletionDelta = GraphDeletionDelta
  { graphDeletionVertices :: !DenseIntSet,
    graphDeletionEdges :: !DenseIntSet
  }
  deriving stock (Eq, Show)

data GraphMorphism = GraphMorphism
  { graphMorphismSource :: !GraphObject,
    graphMorphismTarget :: !GraphObject,
    graphMorphismKnownComplement :: !(Maybe GraphDeletionDelta)
  }
  deriving stock (Show)

data GraphRewriteCase = GraphRewriteCase
  { graphRewriteCategory :: !GraphCategory,
    graphRewriteRuleLeg :: !GraphMorphism,
    graphRewriteMatch :: !GraphMorphism
  }
  deriving stock (Show)

data PreparedGraphRewriteCase = PreparedGraphRewriteCase
  { preparedGraphRewrite :: !GraphRewriteCase,
    preparedGraphComplement :: !(PushoutComplementWitness GraphCategory),
    preparedGraphPBPO :: !(PBPOComplementWitness GraphCategory)
  }

data PreparedGraphRewriteBatch = PreparedGraphRewriteBatch
  { preparedGraphAmbientSize :: !Int,
    preparedGraphCases :: ![PreparedGraphRewriteCase]
  }

instance Eq GraphObject where
  left == right =
    graphObjectCarrierId left == graphObjectCarrierId right
      && graphObjectVertexCount left == graphObjectVertexCount right
      && graphObjectEdgeCount left == graphObjectEdgeCount right
      && graphObjectVertices left == graphObjectVertices right
      && graphObjectEdges left == graphObjectEdges right

instance Eq GraphMorphism where
  left == right =
    graphMorphismSource left == graphMorphismSource right
      && graphMorphismTarget left == graphMorphismTarget right

instance NFData GraphId where
  rnf graphId =
    unGraphId graphId `seq` ()

instance NFData GraphEdge where
  rnf edge =
    graphEdgeSource edge
      `seq` graphEdgeTarget edge
      `seq` ()

instance NFData GraphCarrier where
  rnf carrier =
    rnf (graphCarrierId carrier)
      `seq` denseIntSetSize (graphCarrierVertices carrier)
      `seq` denseIntSetSize (graphCarrierEdgeIds carrier)
      `seq` rnf (graphCarrierEdges carrier)
      `seq` Vector.foldl' (\forced incidentEdges -> denseIntSetSize incidentEdges `seq` forced) () (graphCarrierIncidentEdges carrier)

instance NFData GraphCategory where
  rnf categoryValue =
    rnf (graphCategoryCarrier categoryValue)

instance NFData GraphObject where
  rnf graph =
    rnf (graphObjectCarrierId graph)
      `seq` denseIntSetSize (graphObjectVertices graph)
      `seq` denseIntSetSize (graphObjectEdges graph)
      `seq` graphObjectVertexCount graph
      `seq` graphObjectEdgeCount graph
      `seq` ()

instance NFData GraphDeletionDelta where
  rnf delta =
    denseIntSetSize (graphDeletionVertices delta)
      `seq` denseIntSetSize (graphDeletionEdges delta)
      `seq` ()

instance NFData GraphMorphism where
  rnf morphism =
    rnf (graphMorphismSource morphism)
      `seq` rnf (graphMorphismTarget morphism)
      `seq` rnf (graphMorphismKnownComplement morphism)

instance NFData GraphRewriteCase where
  rnf rewriteCase =
    rnf (graphRewriteCategory rewriteCase)
      `seq` rnf (graphRewriteRuleLeg rewriteCase)
      `seq` rnf (graphRewriteMatch rewriteCase)

instance NFData (PushoutComplementWitness GraphCategory) where
  rnf witness =
    graphPushoutComplementWitnessWeight witness `seq` ()

instance NFData (PBPOComplementWitness GraphCategory) where
  rnf witness =
    graphPBPOComplementWitnessWeight witness `seq` ()

instance NFData PreparedGraphRewriteCase where
  rnf prepared =
    rnf (preparedGraphRewrite prepared)
      `seq` rnf (preparedGraphComplement prepared)
      `seq` rnf (preparedGraphPBPO prepared)

instance NFData PreparedGraphRewriteBatch where
  rnf prepared =
    preparedGraphAmbientSize prepared
      `seq` rnf (preparedGraphCases prepared)

instance Category GraphCategory where
  type Ob GraphCategory = GraphObject
  type Mor GraphCategory = GraphMorphism
  type TwoMor GraphCategory = GraphTwoMor
  type Compositor GraphCategory = GraphCompositor
  type CategoryError GraphCategory = GraphCategoryError

  identity categoryValue graph
    | graphObjectCarrierId graph == graphCarrierId (graphCategoryCarrier categoryValue) =
        Right (graphTrustedInclusion graph graph)
    | otherwise =
        Left GraphBoundaryMismatch

  compose categoryValue leftMorphism rightMorphism
    | not (graphMorphismValidIn categoryValue leftMorphism)
        || not (graphMorphismValidIn categoryValue rightMorphism) =
        Left GraphBoundaryMismatch
    | graphMorphismTarget rightMorphism /= graphMorphismSource leftMorphism =
        Left GraphBoundaryMismatch
    | otherwise =
        Right (graphTrustedInclusion (graphMorphismSource rightMorphism) (graphMorphismTarget leftMorphism), GraphCompositor)

  source categoryValue morphism
    | graphMorphismValidIn categoryValue morphism =
        Right (graphMorphismSource morphism)
    | otherwise =
        Left GraphBoundaryMismatch

  target categoryValue morphism
    | graphMorphismValidIn categoryValue morphism =
        Right (graphMorphismTarget morphism)
    | otherwise =
        Left GraphBoundaryMismatch

instance HasPullbacks GraphCategory where
  pullback categoryValue leftMorphism rightMorphism
    | graphMorphismValidIn categoryValue leftMorphism
        && graphMorphismValidIn categoryValue rightMorphism
        && graphMorphismTarget leftMorphism == graphMorphismTarget rightMorphism = do
        pullbackObjectValue <-
          graphIntersectionObject
            (graphMorphismSource leftMorphism)
            (graphMorphismSource rightMorphism)
        pure
          ( pullbackObjectValue,
            graphTrustedInclusion pullbackObjectValue (graphMorphismSource leftMorphism),
            graphTrustedInclusion pullbackObjectValue (graphMorphismSource rightMorphism)
          )
    | otherwise =
        Nothing

  pullbackMediator categoryValue leftMorphism rightMorphism coneLeft coneRight
    | graphMorphismValidIn categoryValue leftMorphism
        && graphMorphismValidIn categoryValue rightMorphism
        && graphMorphismValidIn categoryValue coneLeft
        && graphMorphismValidIn categoryValue coneRight
        && graphMorphismTarget leftMorphism == graphMorphismTarget rightMorphism
        && graphMorphismTarget coneLeft == graphMorphismSource leftMorphism
        && graphMorphismTarget coneRight == graphMorphismSource rightMorphism
        && graphMorphismSource coneLeft == graphMorphismSource coneRight = do
        pullbackObjectValue <-
          graphIntersectionObject
            (graphMorphismSource leftMorphism)
            (graphMorphismSource rightMorphism)
        pure (graphTrustedInclusion (graphMorphismSource coneLeft) pullbackObjectValue)
    | otherwise =
        Nothing

instance HasPushouts GraphCategory where
  pushout categoryValue leftMorphism rightMorphism
    | graphMorphismValidIn categoryValue leftMorphism
        && graphMorphismValidIn categoryValue rightMorphism
        && graphMorphismSource leftMorphism == graphMorphismSource rightMorphism = do
        pushoutObjectValue <-
          graphCompatibleUnion
            (graphMorphismTarget leftMorphism)
            (graphMorphismTarget rightMorphism)
        pure
          ( pushoutObjectValue,
            graphTrustedInclusion (graphMorphismTarget leftMorphism) pushoutObjectValue,
            graphTrustedInclusion (graphMorphismTarget rightMorphism) pushoutObjectValue
          )
    | otherwise =
        Nothing

instance AdhesiveCategory GraphCategory where
  monicMatchComponents categoryValue morphism
    | graphMorphismValidIn categoryValue morphism =
        Just (MonicMatchComponents morphism)
    | otherwise =
        Nothing

  pushoutComplementComponents categoryValue ruleLeg monicMatch = do
    let carrier = graphCategoryCarrier categoryValue
        matchArrow = monicMatchArrow monicMatch
        kernelObject = graphMorphismSource ruleLeg
        hostObject = graphMorphismTarget matchArrow
    guard (graphMorphismValidIn categoryValue ruleLeg)
    guard (graphMorphismValidIn categoryValue matchArrow)
    guard (graphMorphismTarget ruleLeg == graphMorphismSource matchArrow)
    guard (graphObjectCarrierId hostObject == graphCarrierId carrier)
    deletionDelta <- graphMorphismDeletionDelta ruleLeg
    let deletedGraphVertices = graphDeletionVertices deletionDelta
        deletedGraphEdges = graphDeletionEdges deletionDelta
    danglingEdges <- graphHasDanglingEdges carrier hostObject deletedGraphVertices deletedGraphEdges
    guard (not danglingEdges)
    complementObjectValue <-
      graphObjectRemoveAfterDanglingCheck deletedGraphVertices deletedGraphEdges hostObject
    pure
      PushoutComplementComponents
        { pushoutComplementComponentObject = complementObjectValue,
          pushoutComplementComponentBorrowedLeg = graphTrustedInclusion complementObjectValue hostObject,
          pushoutComplementComponentResidualLeg = graphTrustedInclusion kernelObject complementObjectValue
        }

instance PBPOAdhesiveCategory GraphCategory where
  pbpoComplementComponents categoryValue ruleLeg monicMatch = do
    pushoutComplementComponentsValue <- pushoutComplementComponents categoryValue ruleLeg monicMatch
    let matchArrow = monicMatchArrow monicMatch
        pullbackObjectValue = graphMorphismSource ruleLeg
        pullbackToBorrowed = pushoutComplementComponentResidualLeg pushoutComplementComponentsValue
        pullbackToMatch = ruleLeg
        pushoutObjectValue = graphMorphismTarget matchArrow
        pushoutFromComplement = pushoutComplementComponentBorrowedLeg pushoutComplementComponentsValue
        pushoutFromMatch = matchArrow
    pure
      PBPOComplementComponents
        { pbpoComplementComponentPullbackObject = pullbackObjectValue,
          pbpoComplementComponentPullbackToBorrowed = pullbackToBorrowed,
          pbpoComplementComponentPullbackToMatch = pullbackToMatch,
          pbpoComplementComponentPushoutObject = pushoutObjectValue,
          pbpoComplementComponentPushoutFromComplement = pushoutFromComplement,
          pbpoComplementComponentPushoutFromMatch = pushoutFromMatch,
          pbpoComplementComponentBorrowedLeg = pushoutComplementComponentBorrowedLeg pushoutComplementComponentsValue,
          pbpoComplementComponentResidualLeg = pushoutComplementComponentResidualLeg pushoutComplementComponentsValue
        }

finiteGraphDPOBenchmarks :: Benchmark
finiteGraphDPOBenchmarks =
  bgroup
    "pre-matched indexed-subgraph DPO/PBPO workload"
    (fmap finiteGraphDPOBenchmark [32, 128, 512])

finiteGraphDPOBenchmark :: Int -> Benchmark
finiteGraphDPOBenchmark ambientSize =
  env (prepareGraphRewriteBatch ambientSize) $ \prepared ->
    bgroup
      ("ambient vertices=" <> show ambientSize <> ", cases=64")
      [ bench "pullback graph intersections" (nf graphPullbackBatchWeight prepared),
        bench "pushout graph unions" (nf graphPushoutBatchWeight prepared),
        bench "monic match validation" (nf graphMonicBatchWeight prepared),
        bench "DPO indexed witness construction shape" (nf graphPushoutComplementShapeBatchWeight prepared),
        bench "DPO indexed witness full projection" (nf graphPushoutComplementBatchWeight prepared),
        bench "DPO square commute checks" (nf graphPushoutComplementCommuteBatchWeight prepared),
        bench "PBPO specialized witness construction shape" (nf graphPBPOComplementShapeBatchWeight prepared),
        bench "PBPO specialized witness full projection" (nf graphPBPOComplementBatchWeight prepared),
        bench "PBPO pullback+pushout commute checks" (nf graphPBPOCommuteBatchWeight prepared)
      ]

prepareGraphRewriteBatch :: Int -> IO PreparedGraphRewriteBatch
prepareGraphRewriteBatch ambientSize =
  case traverse (preparedGraphRewriteCase ambientSize) [0 .. 63] of
    Nothing ->
      ioError (userError ("failed to prepare finite graph DPO benchmark for ambient size " <> show ambientSize))
    Just rewriteCases ->
      let prepared =
            PreparedGraphRewriteBatch
              { preparedGraphAmbientSize = ambientSize,
                preparedGraphCases = rewriteCases
              }
       in rnf prepared `seq` pure prepared

preparedGraphRewriteCase :: Int -> Int -> Maybe PreparedGraphRewriteCase
preparedGraphRewriteCase ambientSize seed = do
  rewriteCase <- graphRewriteCase ambientSize seed
  complement <- graphComplementWitness rewriteCase
  pbpo <- graphPBPOWitness rewriteCase
  pure
    PreparedGraphRewriteCase
      { preparedGraphRewrite = rewriteCase,
        preparedGraphComplement = complement,
        preparedGraphPBPO = pbpo
      }

graphRewriteCase :: Int -> Int -> Maybe GraphRewriteCase
graphRewriteCase ambientSize seed = do
  kernelVertices <- graphRangeSet normalizedSize 0 kernelCount
  deletedVertices <- graphRangeSet normalizedSize kernelCount deletedCount
  ambientVertices <- denseIntSetFull normalizedSize
  let kernelEdgePairs =
        graphPathEdgePairs (graphRangeList 0 kernelCount)
      deletedEdgePairs =
        graphPathEdgePairs (graphRangeList kernelCount deletedCount)
      contextEdgePairs =
        graphPathEdgePairs (graphRangeList (kernelCount + deletedCount) contextCount)
      kernelEdgeCount =
        length kernelEdgePairs
      deletedEdgeCount =
        length deletedEdgePairs
      contextEdgeCount =
        length contextEdgePairs
      edgeUniverse =
        kernelEdgeCount + deletedEdgeCount + contextEdgeCount
      ambientEdges =
        graphEdgesFromPairs (kernelEdgePairs <> deletedEdgePairs <> contextEdgePairs)
  kernelEdges <- graphRangeSet edgeUniverse 0 kernelEdgeCount
  deletedEdges <- graphRangeSet edgeUniverse kernelEdgeCount deletedEdgeCount
  ruleVertices <- denseIntSetUnion kernelVertices deletedVertices
  ruleEdges <- denseIntSetUnion kernelEdges deletedEdges
  ambientEdgeIds <- denseIntSetFull edgeUniverse
  ambientCarrier <- graphCarrierFromEdges graphId ambientVertices ambientEdges
  kernelGraph <- graphObjectFromParts ambientCarrier kernelVertices kernelEdges
  ruleGraph <- graphObjectFromParts ambientCarrier ruleVertices ruleEdges
  ambientGraph <- graphObjectFromParts ambientCarrier ambientVertices ambientEdgeIds
  ruleLeg <- graphSubobjectInclusion kernelGraph ruleGraph
  matchArrow <- graphSubobjectInclusion ruleGraph ambientGraph
  pure
    GraphRewriteCase
      { graphRewriteCategory = GraphCategory ambientCarrier,
        graphRewriteRuleLeg = ruleLeg,
        graphRewriteMatch = matchArrow
      }
  where
    normalizedSize =
      max 8 ambientSize
    kernelCount =
      max 2 (normalizedSize `div` 4)
    deletedCount =
      max 2 (normalizedSize `div` 4)
    contextCount =
      max 2 (normalizedSize - kernelCount - deletedCount)
    graphId =
      GraphId (ambientSize * 1024 + seed)

graphCarrierFromEdges :: GraphId -> DenseIntSet -> IntMap GraphEdge -> Maybe GraphCarrier
graphCarrierFromEdges graphId vertices edges = do
  let edgeUniverseSize = IntMap.size edges
  edgeIds <- denseIntSetFull edgeUniverseSize
  guard (IntMap.keys edges == [0 .. edgeUniverseSize - 1])
  guard (graphEdgesClosedOver vertices edges)
  incidentEdges <- graphIncidentIndex (denseIntSetUniverseSize vertices) edgeUniverseSize edges
  pure
    GraphCarrier
      { graphCarrierId = graphId,
        graphCarrierVertices = vertices,
        graphCarrierEdgeIds = edgeIds,
        graphCarrierEdges = edges,
        graphCarrierIncidentEdges = incidentEdges
      }

graphObjectFromParts :: GraphCarrier -> DenseIntSet -> DenseIntSet -> Maybe GraphObject
graphObjectFromParts carrier vertices edgeIds = do
  let objectValue =
        GraphObject
          { graphObjectCarrierId = graphCarrierId carrier,
            graphObjectVertices = vertices,
            graphObjectEdges = edgeIds,
            graphObjectVertexCount = denseIntSetSize vertices,
            graphObjectEdgeCount = denseIntSetSize edgeIds
          }
  guard (denseIntSetIsSubsetOf vertices (graphCarrierVertices carrier) == Just True)
  guard (denseIntSetIsSubsetOf edgeIds (graphCarrierEdgeIds carrier) == Just True)
  guard (graphObjectClosed carrier objectValue)
  pure objectValue

graphRangeSet :: Int -> Int -> Int -> Maybe DenseIntSet
graphRangeSet =
  denseIntSetInterval

graphRangeList :: Int -> Int -> [Int]
graphRangeList start count =
  [start .. start + count - 1]

graphPathEdgePairs :: [Int] -> [(Int, Int)]
graphPathEdgePairs vertices =
  zip vertices (drop 1 vertices)

graphEdgesFromPairs :: [(Int, Int)] -> IntMap GraphEdge
graphEdgesFromPairs pairs =
  zip [0 ..] pairs
    & fmap (\(edgeId, (sourceVertex, targetVertex)) -> (edgeId, GraphEdge sourceVertex targetVertex))
    & IntMap.fromAscList

graphIncidentIndex :: Int -> Int -> IntMap GraphEdge -> Maybe (Vector DenseIntSet)
graphIncidentIndex vertexUniverseSize edgeUniverseSize edges =
  traverse
    (denseIntSetFromAscList edgeUniverseSize . graphIncidentEdgeIds edges)
    (Vector.generate vertexUniverseSize id)

graphIncidentEdgeIds :: IntMap GraphEdge -> Int -> [Int]
graphIncidentEdgeIds edges vertex =
  [ edgeId
    | (edgeId, edge) <- IntMap.toAscList edges,
      graphEdgeSource edge == vertex || graphEdgeTarget edge == vertex
  ]

graphObjectVertexSet :: GraphObject -> DenseIntSet
graphObjectVertexSet =
  graphObjectVertices

graphObjectEdgeSet :: GraphObject -> DenseIntSet
graphObjectEdgeSet =
  graphObjectEdges

graphEdgesClosedOver :: DenseIntSet -> IntMap GraphEdge -> Bool
graphEdgesClosedOver vertices edges =
  edges
    & IntMap.elems
    & all
      ( \edge ->
          denseIntSetMember (graphEdgeSource edge) vertices
            && denseIntSetMember (graphEdgeTarget edge) vertices
      )

graphObjectClosed :: GraphCarrier -> GraphObject -> Bool
graphObjectClosed carrier graph =
  denseIntSetFoldl'
    (\closed edgeId -> closed && graphObjectContainsEdgeEndpoints carrier graph edgeId)
    True
    (graphObjectEdgeSet graph)

graphObjectContainsEdgeEndpoints :: GraphCarrier -> GraphObject -> Int -> Bool
graphObjectContainsEdgeEndpoints carrier graph edgeId =
  case IntMap.lookup edgeId (graphCarrierEdges carrier) of
    Just edge ->
      denseIntSetMember (graphEdgeSource edge) (graphObjectVertices graph)
        && denseIntSetMember (graphEdgeTarget edge) (graphObjectVertices graph)
    Nothing ->
      False

graphObjectInCategory :: GraphCategory -> GraphObject -> Bool
graphObjectInCategory categoryValue graph =
  graphObjectCarrierId graph == graphCarrierId (graphCategoryCarrier categoryValue)

graphMorphismValidIn :: GraphCategory -> GraphMorphism -> Bool
graphMorphismValidIn categoryValue morphism =
  graphObjectInCategory categoryValue (graphMorphismSource morphism)
    && graphObjectInCategory categoryValue (graphMorphismTarget morphism)
    && graphMorphismIsInclusion morphism

graphMorphismIsInclusion :: GraphMorphism -> Bool
graphMorphismIsInclusion morphism =
  graphObjectCarrierId (graphMorphismSource morphism) == graphObjectCarrierId (graphMorphismTarget morphism)

graphObjectIsSubobjectOf :: GraphObject -> GraphObject -> Bool
graphObjectIsSubobjectOf sourceGraph targetGraph =
  graphObjectCarrierId sourceGraph == graphObjectCarrierId targetGraph
    && denseIntSetIsSubsetOf (graphObjectVertices sourceGraph) (graphObjectVertices targetGraph) == Just True
    && denseIntSetIsSubsetOf (graphObjectEdges sourceGraph) (graphObjectEdges targetGraph) == Just True

graphSubobjectInclusion :: GraphObject -> GraphObject -> Maybe GraphMorphism
graphSubobjectInclusion sourceGraph targetGraph = do
  guard (graphObjectIsSubobjectOf sourceGraph targetGraph)
  deletionDelta <- graphDeletionDelta sourceGraph targetGraph
  pure (graphTrustedInclusionWithDelta sourceGraph targetGraph (Just deletionDelta))

graphTrustedInclusion :: GraphObject -> GraphObject -> GraphMorphism
graphTrustedInclusion sourceGraph targetGraph =
  graphTrustedInclusionWithDelta sourceGraph targetGraph Nothing

graphTrustedInclusionWithDelta :: GraphObject -> GraphObject -> Maybe GraphDeletionDelta -> GraphMorphism
graphTrustedInclusionWithDelta sourceGraph targetGraph complementDelta =
  GraphMorphism
    { graphMorphismSource = sourceGraph,
      graphMorphismTarget = targetGraph,
      graphMorphismKnownComplement = complementDelta
    }

graphMorphismDeletionDelta :: GraphMorphism -> Maybe GraphDeletionDelta
graphMorphismDeletionDelta morphism =
  case graphMorphismKnownComplement morphism of
    Just deletionDelta ->
      Just deletionDelta
    Nothing ->
      graphDeletionDelta (graphMorphismSource morphism) (graphMorphismTarget morphism)

graphDeletionDelta :: GraphObject -> GraphObject -> Maybe GraphDeletionDelta
graphDeletionDelta sourceGraph targetGraph = do
  deletedVertices <- denseIntSetDifference (graphObjectVertexSet targetGraph) (graphObjectVertexSet sourceGraph)
  deletedEdges <- denseIntSetDifference (graphObjectEdgeSet targetGraph) (graphObjectEdgeSet sourceGraph)
  pure
    GraphDeletionDelta
      { graphDeletionVertices = deletedVertices,
        graphDeletionEdges = deletedEdges
      }

graphIntersectionObject :: GraphObject -> GraphObject -> Maybe GraphObject
graphIntersectionObject leftGraph rightGraph = do
  guard (graphObjectCarrierId leftGraph == graphObjectCarrierId rightGraph)
  intersectionVertices <- denseIntSetIntersection (graphObjectVertexSet leftGraph) (graphObjectVertexSet rightGraph)
  intersectionEdges <- denseIntSetIntersection (graphObjectEdgeSet leftGraph) (graphObjectEdgeSet rightGraph)
  pure
    GraphObject
      { graphObjectCarrierId = graphObjectCarrierId leftGraph,
        graphObjectVertices = intersectionVertices,
        graphObjectEdges = intersectionEdges,
        graphObjectVertexCount = denseIntSetSize intersectionVertices,
        graphObjectEdgeCount = denseIntSetSize intersectionEdges
      }

graphCompatibleUnion :: GraphObject -> GraphObject -> Maybe GraphObject
graphCompatibleUnion leftGraph rightGraph = do
  guard (graphObjectCarrierId leftGraph == graphObjectCarrierId rightGraph)
  unionVertices <- denseIntSetUnion (graphObjectVertexSet leftGraph) (graphObjectVertexSet rightGraph)
  unionEdges <- denseIntSetUnion (graphObjectEdgeSet leftGraph) (graphObjectEdgeSet rightGraph)
  pure
    GraphObject
      { graphObjectCarrierId = graphObjectCarrierId leftGraph,
        graphObjectVertices = unionVertices,
        graphObjectEdges = unionEdges,
        graphObjectVertexCount = denseIntSetSize unionVertices,
        graphObjectEdgeCount = denseIntSetSize unionEdges
      }

graphObjectRemoveAfterDanglingCheck :: DenseIntSet -> DenseIntSet -> GraphObject -> Maybe GraphObject
graphObjectRemoveAfterDanglingCheck deletedVertices deletedEdges graph =
  do
    remainingVertices <- denseIntSetDifference (graphObjectVertices graph) deletedVertices
    remainingEdges <- denseIntSetDifference (graphObjectEdges graph) deletedEdges
    pure
      GraphObject
        { graphObjectCarrierId = graphObjectCarrierId graph,
          graphObjectVertices = remainingVertices,
          graphObjectEdges = remainingEdges,
          graphObjectVertexCount = denseIntSetSize remainingVertices,
          graphObjectEdgeCount = denseIntSetSize remainingEdges
        }

graphHasDanglingEdges :: GraphCarrier -> GraphObject -> DenseIntSet -> DenseIntSet -> Maybe Bool
graphHasDanglingEdges carrier hostGraph deletedVertices deletedEdges =
  do
    hostEdgesAfterDeletion <- denseIntSetDifference (graphObjectEdges hostGraph) deletedEdges
    denseIntSetFoldl' (detectDanglingEdge hostEdgesAfterDeletion) (Just False) deletedVertices
  where
    detectDanglingEdge hostEdgesAfterDeletion danglingFound vertex =
      case danglingFound of
        Nothing ->
          Nothing
        Just True ->
          Just True
        Just False ->
          vertexHasDanglingEdge hostEdgesAfterDeletion vertex

    vertexHasDanglingEdge hostEdgesAfterDeletion vertex =
      case graphCarrierIncidentEdges carrier Vector.!? vertex of
        Nothing ->
          Nothing
        Just incidentEdges ->
          denseIntSetIntersects hostEdgesAfterDeletion incidentEdges

graphPullbackBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPullbackBatchWeight =
  graphBatchWeight graphPullbackWeight

graphPushoutBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPushoutBatchWeight =
  graphBatchWeight graphPushoutWeight

graphMonicBatchWeight :: PreparedGraphRewriteBatch -> Int
graphMonicBatchWeight =
  graphBatchWeight graphMonicWeight

graphPushoutComplementBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPushoutComplementBatchWeight =
  graphBatchWeight graphPushoutComplementWeight

graphPushoutComplementShapeBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPushoutComplementShapeBatchWeight =
  graphBatchWeight graphPushoutComplementShapeWeight

graphPushoutComplementCommuteBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPushoutComplementCommuteBatchWeight =
  graphBatchWeight graphPushoutComplementCommuteWeight

graphPBPOComplementBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPBPOComplementBatchWeight =
  graphBatchWeight graphPBPOComplementWeight

graphPBPOComplementShapeBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPBPOComplementShapeBatchWeight =
  graphBatchWeight graphPBPOComplementShapeWeight

graphPBPOCommuteBatchWeight :: PreparedGraphRewriteBatch -> Int
graphPBPOCommuteBatchWeight =
  graphBatchWeight graphPBPOCommuteWeight

graphBatchWeight :: (PreparedGraphRewriteCase -> Int) -> PreparedGraphRewriteBatch -> Int
graphBatchWeight weight prepared =
  preparedGraphCases prepared
    & fmap weight
    & sum

graphPullbackWeight :: PreparedGraphRewriteCase -> Int
graphPullbackWeight prepared =
  maybe
    0
    graphPullbackTripleWeight
    (pullback (graphPreparedCategory prepared) (pushoutComplementBorrowedLeg (preparedGraphComplement prepared)) (graphRewriteMatch (preparedGraphRewrite prepared)))

graphPushoutWeight :: PreparedGraphRewriteCase -> Int
graphPushoutWeight prepared =
  maybe
    0
    graphPushoutTripleWeight
    (pushout (graphPreparedCategory prepared) (pushoutComplementResidualLeg (preparedGraphComplement prepared)) (graphRewriteRuleLeg (preparedGraphRewrite prepared)))

graphMonicWeight :: PreparedGraphRewriteCase -> Int
graphMonicWeight prepared =
  let rewriteCase = preparedGraphRewrite prepared
   in maybe 0 (graphMorphismWeight . monicMatchArrow) (witnessMonic (graphRewriteCategory rewriteCase) (graphRewriteMatch rewriteCase))

graphPushoutComplementWeight :: PreparedGraphRewriteCase -> Int
graphPushoutComplementWeight prepared =
  let rewriteCase = preparedGraphRewrite prepared
   in maybe 0 graphPushoutComplementWitnessWeight (graphComplementWitness rewriteCase)

graphPushoutComplementShapeWeight :: PreparedGraphRewriteCase -> Int
graphPushoutComplementShapeWeight prepared =
  let rewriteCase = preparedGraphRewrite prepared
   in maybe 0 graphPushoutComplementWitnessShapeWeight (graphComplementWitness rewriteCase)

graphPushoutComplementCommuteWeight :: PreparedGraphRewriteCase -> Int
graphPushoutComplementCommuteWeight prepared =
  boolWeight (pushoutComplementSquareCommutes (graphPreparedCategory prepared) (preparedGraphComplement prepared))

graphPBPOComplementWeight :: PreparedGraphRewriteCase -> Int
graphPBPOComplementWeight prepared =
  let rewriteCase = preparedGraphRewrite prepared
   in maybe 0 graphPBPOComplementWitnessWeight (graphPBPOWitness rewriteCase)

graphPBPOComplementShapeWeight :: PreparedGraphRewriteCase -> Int
graphPBPOComplementShapeWeight prepared =
  let rewriteCase = preparedGraphRewrite prepared
   in maybe 0 graphPBPOComplementWitnessShapeWeight (graphPBPOWitness rewriteCase)

graphPBPOCommuteWeight :: PreparedGraphRewriteCase -> Int
graphPBPOCommuteWeight prepared =
  let witness = preparedGraphPBPO prepared
      categoryValue = graphPreparedCategory prepared
   in boolWeight (pbpoPullbackSquareCommutes categoryValue witness)
        + boolWeight (pbpoPushoutSquareCommutes categoryValue witness)

graphPreparedCategory :: PreparedGraphRewriteCase -> GraphCategory
graphPreparedCategory =
  graphRewriteCategory . preparedGraphRewrite

graphComplementWitness :: GraphRewriteCase -> Maybe (PushoutComplementWitness GraphCategory)
graphComplementWitness rewriteCase = do
  monicWitness <- witnessMonic (graphRewriteCategory rewriteCase) (graphRewriteMatch rewriteCase)
  pushoutComplement (graphRewriteCategory rewriteCase) (graphRewriteRuleLeg rewriteCase) monicWitness

graphPBPOWitness :: GraphRewriteCase -> Maybe (PBPOComplementWitness GraphCategory)
graphPBPOWitness rewriteCase = do
  monicWitness <- witnessMonic (graphRewriteCategory rewriteCase) (graphRewriteMatch rewriteCase)
  pbpoComplement (graphRewriteCategory rewriteCase) (graphRewriteRuleLeg rewriteCase) monicWitness

graphPullbackTripleWeight :: (GraphObject, GraphMorphism, GraphMorphism) -> Int
graphPullbackTripleWeight (objectValue, leftLeg, rightLeg) =
  graphObjectWeight objectValue
    + graphMorphismWeight leftLeg
    + graphMorphismWeight rightLeg

graphPushoutTripleWeight :: (GraphObject, GraphMorphism, GraphMorphism) -> Int
graphPushoutTripleWeight =
  graphPullbackTripleWeight

graphPushoutComplementWitnessWeight :: PushoutComplementWitness GraphCategory -> Int
graphPushoutComplementWitnessWeight witness =
  graphObjectWeight (pushoutComplementObject witness)
    + graphMorphismWeight (pushoutComplementBorrowedLeg witness)
    + graphMorphismWeight (pushoutComplementResidualLeg witness)

graphPushoutComplementWitnessShapeWeight :: PushoutComplementWitness GraphCategory -> Int
graphPushoutComplementWitnessShapeWeight witness =
  graphObjectShapeWeight (pushoutComplementObject witness)
    + graphMorphismShapeWeight (pushoutComplementBorrowedLeg witness)
    + graphMorphismShapeWeight (pushoutComplementResidualLeg witness)

graphPBPOComplementWitnessWeight :: PBPOComplementWitness GraphCategory -> Int
graphPBPOComplementWitnessWeight witness =
  graphObjectWeight (pbpoComplementPullbackObject witness)
    + graphMorphismWeight (pbpoComplementPullbackToBorrowed witness)
    + graphMorphismWeight (pbpoComplementPullbackToMatch witness)
    + graphObjectWeight (pbpoComplementPushoutObject witness)
    + graphMorphismWeight (pbpoComplementPushoutFromComplement witness)
    + graphMorphismWeight (pbpoComplementPushoutFromMatch witness)
    + graphMorphismWeight (pbpoComplementBorrowedLeg witness)
    + graphMorphismWeight (pbpoComplementResidualLeg witness)

graphPBPOComplementWitnessShapeWeight :: PBPOComplementWitness GraphCategory -> Int
graphPBPOComplementWitnessShapeWeight witness =
  graphObjectShapeWeight (pbpoComplementPullbackObject witness)
    + graphMorphismShapeWeight (pbpoComplementPullbackToBorrowed witness)
    + graphMorphismShapeWeight (pbpoComplementPullbackToMatch witness)
    + graphObjectShapeWeight (pbpoComplementPushoutObject witness)
    + graphMorphismShapeWeight (pbpoComplementPushoutFromComplement witness)
    + graphMorphismShapeWeight (pbpoComplementPushoutFromMatch witness)
    + graphMorphismShapeWeight (pbpoComplementBorrowedLeg witness)
    + graphMorphismShapeWeight (pbpoComplementResidualLeg witness)

graphObjectShapeWeight :: GraphObject -> Int
graphObjectShapeWeight graph =
  graphIdWeight (graphObjectCarrierId graph)
    + graphObjectVertexCount graph
    + graphObjectEdgeCount graph

graphMorphismShapeWeight :: GraphMorphism -> Int
graphMorphismShapeWeight morphism =
  graphObjectShapeWeight (graphMorphismSource morphism)
    + graphObjectShapeWeight (graphMorphismTarget morphism)

graphObjectWeight :: GraphObject -> Int
graphObjectWeight graph =
  graphIdWeight (graphObjectCarrierId graph)
    + denseIntSetWeight (graphObjectVertexSet graph)
    + denseIntSetWeight (graphObjectEdgeSet graph)

graphMorphismWeight :: GraphMorphism -> Int
graphMorphismWeight morphism =
  graphObjectWeight (graphMorphismSource morphism)
    + graphObjectWeight (graphMorphismTarget morphism)

graphIdWeight :: GraphId -> Int
graphIdWeight =
  unGraphId

module Moonlight.Homology.Pure.Topology.Realize
  ( Orientation (..),
    RawCellData (..),
    RawCellScopes (..),
    RealizationBudget (..),
    realizeScaffoldRaw,
    realizeScaffoldRawWithScopes,
  )
where

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Algebra.Pure.Orientation (Orientation (..))
import Moonlight.Homology.Pure.Failure (HomologyFailure (..))
import Moonlight.Homology.Pure.Topology.Graph.Algebra (connectedComponentsFromAdjacency)
import Moonlight.Homology.Pure.Topology.MacroScaffold
  ( BasisCellRef,
    HarmonicLoop (..),
    MacroScaffoldIR (..),
    MorseReebArc (..),
    MorseReebNode (..),
    MorseReebScaffold (..),
    PotentialValue,
    ReebArcId,
    ReebNodeId (..),
    unPotentialValue,
  )

type RawCellData :: Type
data RawCellData = RawCellData
  { rawVertices :: [Int],
    rawEdges :: [(Int, Int, Int)],
    rawFaces :: [(Int, [(Int, Orientation)])],
    rawVertexCount :: !Int,
    rawEdgeCount :: !Int,
    rawFaceCount :: !Int
  }
  deriving stock (Eq, Show)

instance Semigroup RawCellData where
  a <> b =
    RawCellData
      { rawVertices = rawVertices a <> rawVertices b,
        rawEdges = rawEdges a <> rawEdges b,
        rawFaces = rawFaces a <> rawFaces b,
        rawVertexCount = rawVertexCount a + rawVertexCount b,
        rawEdgeCount = rawEdgeCount a + rawEdgeCount b,
        rawFaceCount = rawFaceCount a + rawFaceCount b
      }

instance Monoid RawCellData where
  mempty = RawCellData [] [] [] 0 0 0

type RawCellScopes :: Type -> Type
data RawCellScopes tag = RawCellScopes
  { rawVertexScopes :: Map.Map tag (Set Int),
    rawEdgeScopes :: Map.Map tag (Set Int),
    rawFaceScopes :: Map.Map tag (Set Int)
  }
  deriving stock (Eq, Show)

instance Ord tag => Semigroup (RawCellScopes tag) where
  left <> right =
    RawCellScopes
      { rawVertexScopes = Map.unionWith Set.union (rawVertexScopes left) (rawVertexScopes right),
        rawEdgeScopes = Map.unionWith Set.union (rawEdgeScopes left) (rawEdgeScopes right),
        rawFaceScopes = Map.unionWith Set.union (rawFaceScopes left) (rawFaceScopes right)
      }

instance Ord tag => Monoid (RawCellScopes tag) where
  mempty =
    RawCellScopes
      { rawVertexScopes = Map.empty,
        rawEdgeScopes = Map.empty,
        rawFaceScopes = Map.empty
      }

type RealizationBudget :: Type
newtype RealizationBudget = RealizationBudget
  { unRealizationBudget :: Int
  }
  deriving stock (Eq, Show)

type IdState :: Type
data IdState = IdState
  { nextVertexId :: !Int,
    nextEdgeId :: !Int,
    nextFaceId :: !Int
  }

realizeScaffoldRaw :: MacroScaffoldIR -> RealizationBudget -> Either HomologyFailure RawCellData
realizeScaffoldRaw scaffold budget = do
  let reebScaffold = macroScaffoldReeb scaffold
      nodes = morseReebNodes reebScaffold
      arcs = morseReebArcs reebScaffold
      loops = macroScaffoldHarmonicLoops scaffold
      nodePotentialMap = buildNodePotentialMap nodes
  validateLoopClosure reebScaffold loops
  let (nodeData, nodeVertexMap, initialIdState) = realizeNodes reebScaffold
      arcBudgets = distributeBudget budget arcs nodePotentialMap
  arcDataList <- foldArcs nodeVertexMap arcBudgets initialIdState arcs
  Right (nodeData <> mconcat arcDataList)

realizeScaffoldRawWithScopes ::
  Ord tag =>
  (BasisCellRef -> Set tag) ->
  MacroScaffoldIR ->
  RealizationBudget ->
  Either HomologyFailure (RawCellData, RawCellScopes tag)
realizeScaffoldRawWithScopes labelsAt scaffold budget = do
  let reebScaffold = macroScaffoldReeb scaffold
      nodes = morseReebNodes reebScaffold
      arcs = morseReebArcs reebScaffold
      loops = macroScaffoldHarmonicLoops scaffold
      nodePotentialMap = buildNodePotentialMap nodes
  validateLoopClosure reebScaffold loops
  let (nodeData, nodeVertexMap, initialIdState, nodeScopes) =
        realizeNodesWithScopes labelsAt reebScaffold
      arcBudgets = distributeBudget budget arcs nodePotentialMap
  (arcData, arcScopes) <-
    foldArcsWithScopes labelsAt nodeVertexMap arcBudgets initialIdState arcs
  pure (nodeData <> arcData, nodeScopes <> arcScopes)

buildNodePotentialMap :: [MorseReebNode] -> Map.Map ReebNodeId PotentialValue
buildNodePotentialMap =
  Map.fromList
    . fmap (\node -> (morseReebNodeId node, morseReebNodePotential node))

realizeNodes :: MorseReebScaffold -> (RawCellData, Map.Map ReebNodeId Int, IdState)
realizeNodes reebScaffold =
  let nodes = morseReebNodes reebScaffold
      nodeCount = length nodes
      nodeVertexMap =
        Map.fromList
          ( zip
              (fmap morseReebNodeId nodes)
              [0 .. nodeCount - 1]
          )
      nodeData =
        RawCellData
          { rawVertices = [0 .. nodeCount - 1],
            rawEdges = [],
            rawFaces = [],
            rawVertexCount = nodeCount,
            rawEdgeCount = 0,
            rawFaceCount = 0
          }
      initialIdState =
        IdState
          { nextVertexId = nodeCount,
            nextEdgeId = 0,
            nextFaceId = 0
          }
   in (nodeData, nodeVertexMap, initialIdState)

realizeNodesWithScopes ::
  Ord tag =>
  (BasisCellRef -> Set tag) ->
  MorseReebScaffold ->
  (RawCellData, Map.Map ReebNodeId Int, IdState, RawCellScopes tag)
realizeNodesWithScopes labelsAt reebScaffold =
  let (nodeData, nodeVertexMap, initialIdState) = realizeNodes reebScaffold
      nodeScopes =
        morseReebNodes reebScaffold
          & zip [0 ..]
          & foldr
            ( \(vertexId, nodeValue) ->
                (<>)
                  (scopeForVertices (labelsAt (morseReebNodeAnchor nodeValue)) [vertexId])
            )
            mempty
   in (nodeData, nodeVertexMap, initialIdState, nodeScopes)

distributeBudget ::
  RealizationBudget ->
  [MorseReebArc] ->
  Map.Map ReebNodeId PotentialValue ->
  Map.Map ReebArcId Int
-- | Distribute a face budget across arcs proportionally to potential delta,
-- with a floor of one face per arc. The floor dominates the budget: when
-- @totalBudget < length arcs@ the distribution totals @length arcs@, i.e.
-- the budget is treated as a target, not a hard cap.
distributeBudget (RealizationBudget totalBudget) arcs nodePotentialMap =
  let arcDeltas =
        fmap (arcPotentialDelta nodePotentialMap) arcs
      totalDelta = sum arcDeltas
      arcIdsWithDeltas = zip (fmap morseReebArcId arcs) arcDeltas
      rawAllocations =
        fmap
          ( \(arcId, delta) ->
              let proportion =
                    if totalDelta > 0
                      then delta / totalDelta
                      else 1.0 / fromIntegral (max 1 (length arcs))
                  rawAlloc = fromIntegral totalBudget * proportion :: Double
                  floorAlloc = max 1 (floor rawAlloc :: Int)
                  fractionalRemainder = rawAlloc - fromIntegral floorAlloc
               in (arcId, floorAlloc, fractionalRemainder)
          )
          arcIdsWithDeltas
      floorTotal = sum (fmap (\(_, alloc, _) -> alloc) rawAllocations)
      deficit = totalBudget - floorTotal
      sortedByRemainder =
        sortBy (comparing (\(_, _, remainder) -> Down remainder)) rawAllocations
      distributed = distributeRemainder deficit sortedByRemainder
   in Map.fromList distributed

distributeRemainder :: Int -> [(ReebArcId, Int, Double)] -> [(ReebArcId, Int)]
distributeRemainder remaining allocations =
  case allocations of
    [] -> []
    (arcId, alloc, _remainder) : rest
      | remaining > 0 ->
          (arcId, alloc + 1) : distributeRemainder (remaining - 1) rest
      | otherwise ->
          (arcId, alloc) : fmap (\(aid, a, _) -> (aid, a)) rest

arcPotentialDelta :: Map.Map ReebNodeId PotentialValue -> MorseReebArc -> Double
arcPotentialDelta nodePotentialMap arc =
  let sourcePotential =
        maybe 0.0 unPotentialValue
          (Map.lookup (morseReebArcSource arc) nodePotentialMap)
      targetPotential =
        maybe 0.0 unPotentialValue
          (Map.lookup (morseReebArcTarget arc) nodePotentialMap)
   in abs (targetPotential - sourcePotential)

foldArcs ::
  Map.Map ReebNodeId Int ->
  Map.Map ReebArcId Int ->
  IdState ->
  [MorseReebArc] ->
  Either HomologyFailure [RawCellData]
foldArcs nodeVertexMap arcBudgets initialState arcs =
  let step (currentState, revAccumulated) arc = do
        (nextState, cellData) <- realizeArcWithState nodeVertexMap arcBudgets currentState arc
        Right (nextState, cellData : revAccumulated)
   in fmap (reverse . snd) (foldl' (\acc arc -> acc >>= \s -> step s arc) (Right (initialState, [])) arcs)

foldArcsWithScopes ::
  Ord tag =>
  (BasisCellRef -> Set tag) ->
  Map.Map ReebNodeId Int ->
  Map.Map ReebArcId Int ->
  IdState ->
  [MorseReebArc] ->
  Either HomologyFailure (RawCellData, RawCellScopes tag)
foldArcsWithScopes labelsAt nodeVertexMap arcBudgets initialState arcs =
  let step (currentState, revDataList, revScopesList) arc = do
        (nextState, cellData, cellScopes) <-
          realizeArcWithStateAndScopes labelsAt nodeVertexMap arcBudgets currentState arc
        Right (nextState, cellData : revDataList, cellScopes : revScopesList)
   in fmap
        (\(_, revDataList, revScopesList) -> (mconcat (reverse revDataList), mconcat (reverse revScopesList)))
        (foldl' (\acc arc -> acc >>= \stateValue -> step stateValue arc) (Right (initialState, [], [])) arcs)

realizeArcWithState ::
  Map.Map ReebNodeId Int ->
  Map.Map ReebArcId Int ->
  IdState ->
  MorseReebArc ->
  Either HomologyFailure (IdState, RawCellData)
realizeArcWithState nodeVertexMap arcBudgets idState arc = do
  let arcId = morseReebArcId arc
  faceBudget <-
    maybe
      (Left (InvalidTopologyInput "arc budget not found during realization"))
      Right
      (Map.lookup arcId arcBudgets)
  sourceVertex <-
    maybe
      (Left (InvalidTopologyInput "arc source node not found in vertex map"))
      Right
      (Map.lookup (morseReebArcSource arc) nodeVertexMap)
  targetVertex <-
    maybe
      (Left (InvalidTopologyInput "arc target node not found in vertex map"))
      Right
      (Map.lookup (morseReebArcTarget arc) nodeVertexMap)
  Right (realizeArc idState faceBudget sourceVertex targetVertex)

realizeArcWithStateAndScopes ::
  Ord tag =>
  (BasisCellRef -> Set tag) ->
  Map.Map ReebNodeId Int ->
  Map.Map ReebArcId Int ->
  IdState ->
  MorseReebArc ->
  Either HomologyFailure (IdState, RawCellData, RawCellScopes tag)
realizeArcWithStateAndScopes labelsAt nodeVertexMap arcBudgets idState arc = do
  (nextState, cellData) <- realizeArcWithState nodeVertexMap arcBudgets idState arc
  let arcLabels =
        morseReebArcSupport arc
          & fmap labelsAt
          & foldr Set.union Set.empty
  pure
    ( nextState,
      cellData,
      scopeForRawCellData arcLabels cellData
    )

realizeArc :: IdState -> Int -> Int -> Int -> (IdState, RawCellData)
realizeArc idState faceBudget sourceVertex targetVertex =
  let intermediateCount = faceBudget - 1
      intermediateVertexIds = [nextVertexId idState .. nextVertexId idState + intermediateCount - 1]
      lateralVertexId = nextVertexId idState + intermediateCount
      newVertexIds = intermediateVertexIds <> [lateralVertexId]
      newVertexCount = intermediateCount + 1
      spineVertices = [sourceVertex] <> intermediateVertexIds <> [targetVertex]
      spineEdges =
        zipWith
          (\i (src, tgt) -> (nextEdgeId idState + i, src, tgt))
          [0 ..]
          (zip (take faceBudget spineVertices) (drop 1 spineVertices))
      lateralEdgeStart = nextEdgeId idState + faceBudget
      lateralEdgeCount = faceBudget + 1
      lateralEdges =
        zipWith
          (\i sv -> (lateralEdgeStart + i, sv, lateralVertexId))
          [0 ..]
          spineVertices
      totalNewEdges = faceBudget + lateralEdgeCount
      faceStart = nextFaceId idState
      faces =
        zipWith
          ( \i faceIdx ->
              let spineEdgeId = nextEdgeId idState + i
                  lateralEdgeIdNext = lateralEdgeStart + i + 1
                  lateralEdgeIdCur = lateralEdgeStart + i
               in ( faceIdx,
                    [ (spineEdgeId, Positive),
                      (lateralEdgeIdNext, Positive),
                      (lateralEdgeIdCur, Negative)
                    ]
                  )
          )
          [0 .. faceBudget - 1]
          [faceStart .. faceStart + faceBudget - 1]
      nextState =
        IdState
          { nextVertexId = nextVertexId idState + newVertexCount,
            nextEdgeId = nextEdgeId idState + totalNewEdges,
            nextFaceId = nextFaceId idState + faceBudget
          }
      cellData =
        RawCellData
          { rawVertices = newVertexIds,
            rawEdges = spineEdges <> lateralEdges,
            rawFaces = faces,
            rawVertexCount = newVertexCount,
            rawEdgeCount = totalNewEdges,
            rawFaceCount = faceBudget
         }
   in (nextState, cellData)

scopeForRawCellData :: Ord tag => Set tag -> RawCellData -> RawCellScopes tag
scopeForRawCellData labelsValue rawCellData =
  scopeForVertices labelsValue (rawVertices rawCellData)
    <> scopeForEdges labelsValue (fmap (\(edgeId, _, _) -> edgeId) (rawEdges rawCellData))
    <> scopeForFaces labelsValue (fmap fst (rawFaces rawCellData))

scopeForVertices :: Ord tag => Set tag -> [Int] -> RawCellScopes tag
scopeForVertices labelsValue vertexIds =
  mempty
    { rawVertexScopes = labelsToCells labelsValue vertexIds
    }

scopeForEdges :: Ord tag => Set tag -> [Int] -> RawCellScopes tag
scopeForEdges labelsValue edgeIds =
  mempty
    { rawEdgeScopes = labelsToCells labelsValue edgeIds
    }

scopeForFaces :: Ord tag => Set tag -> [Int] -> RawCellScopes tag
scopeForFaces labelsValue faceIds =
  mempty
    { rawFaceScopes = labelsToCells labelsValue faceIds
    }

labelsToCells :: Ord tag => Set tag -> [Int] -> Map.Map tag (Set Int)
labelsToCells labelsValue cellIds =
  labelsValue
    & Set.toList
    & fmap (\labelValue -> (labelValue, Set.fromList cellIds))
    & Map.fromListWith Set.union

validateLoopClosure :: MorseReebScaffold -> [HarmonicLoop] -> Either HomologyFailure ()
validateLoopClosure reebScaffold loops =
  let arcs = morseReebArcs reebScaffold
      arcEndpointMap =
        Map.fromList
          ( fmap
              (\arc -> (morseReebArcId arc, (morseReebArcSource arc, morseReebArcTarget arc)))
              arcs
          )
   in traverse_ (validateSingleLoop arcEndpointMap) loops

validateSingleLoop ::
  Map.Map ReebArcId (ReebNodeId, ReebNodeId) ->
  HarmonicLoop ->
  Either HomologyFailure ()
validateSingleLoop arcEndpointMap loop =
  let supportArcs = harmonicLoopSupport loop
   in case supportArcs of
        [] -> Right ()
        _ ->
          let endpoints =
                fmap
                  (\arcId -> Map.lookup arcId arcEndpointMap)
                  supportArcs
           in case sequence endpoints of
                Nothing ->
                  Left
                    ( InvalidTopologyInput
                        "harmonic loop references arc not present in scaffold"
                    )
                Just pairs ->
                  validateArcChainClosure pairs

validateArcChainClosure :: [(ReebNodeId, ReebNodeId)] -> Either HomologyFailure ()
validateArcChainClosure pairs =
  let allEndpoints = concatMap (\(src, tgt) -> [src, tgt]) pairs
      occurrences =
        foldl'
          (\acc nodeId -> Map.insertWith (+) nodeId (1 :: Int) acc)
          Map.empty
          allEndpoints
      oddDegreeNodes =
        Map.filter odd occurrences
   in if not (Map.null oddDegreeNodes)
        then
          Left
            ( InvalidTopologyInput
                "harmonic loop arc support has nodes with odd degree"
            )
        else case Map.keys occurrences of
          [] -> Right ()
          (_ : _) ->
            let adjacency =
                  foldl'
                    ( \acc (src, tgt) ->
                        Map.insertWith Set.union src (Set.singleton tgt) (Map.insertWith Set.union tgt (Set.singleton src) acc)
                    )
                    Map.empty
                    pairs
                componentCount = connectedComponentsFromAdjacency adjacency
             in if componentCount == 1
                  then Right ()
                  else
                    Left
                      ( InvalidTopologyInput
                          "harmonic loop arc support is not connected"
                      )

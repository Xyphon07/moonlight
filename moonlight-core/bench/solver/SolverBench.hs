{-# LANGUAGE RankNTypes #-}

module SolverBench
  ( solverBenchmarks,
  )
where

import Control.DeepSeq
  ( NFData (..),
  )
import Control.Monad.ST
  ( ST,
  )
import Data.Equivalence.Monad qualified as Equivalence
import Data.Foldable
  ( traverse_,
  )
import Data.Graph qualified as Graph
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet
  ( IntSet,
  )
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector
import BenchSupport
  ( caseLabel,
    foundationSizes,
    keys,
    sampleKeys,
    showLength,
    unionFindSizes,
  )
import Moonlight.Core
  ( DeltaDomain (..),
    EquationId (..),
    Evaluation,
    closureUnderInt,
    fixpointBounded,
    readEquationValue,
    resultValues,
    solveDenseMonotone,
    traverseOnceIntSet,
  )
import Moonlight.Core.Fixpoint.Dense
  ( Csr,
    csrOffsets,
    csrTargets,
    csrTranspose,
    csrVertexCount,
    FrozenDigraph,
    graphBackward,
    graphForward,
    graphSccPlan,
    SccClosureCache,
    SccPlan,
    condensation,
    condensationBackward,
    sccMembers,
    sccOfVertex,
    sccClosureCacheFor,
    Edge (..),
    frozenDigraphFromSuccessors,
    frozenReachabilityFrom,
    frozenReachabilityWithCache,
    snapshotFromFrozen,
    snapshotReachabilityFrom,
    insertSnapshotEdge,
  )
import Moonlight.Core
  ( ClassId (..),
    classIdKey,
  )
import Moonlight.Core qualified as UF
import Moonlight.Core qualified as UFT
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )
import Prelude

newtype ContainersUnionFind = ContainersUnionFind (IntMap Int)

newtype ForcedFrozenDigraph = ForcedFrozenDigraph FrozenDigraph

instance NFData ForcedFrozenDigraph where
  rnf (ForcedFrozenDigraph graph) =
    rnfFrozenDigraph graph

newtype ForcedGraph = ForcedGraph Graph.Graph

instance NFData ForcedGraph where
  rnf (ForcedGraph graph) =
    rnf (Graph.vertices graph, Graph.edges graph)

data ForcedSccClosureCache = ForcedSccClosureCache
  { forcedCacheReceipt :: !Int,
    forcedCache :: !SccClosureCache
  }

instance NFData ForcedSccClosureCache where
  rnf cache =
    rnf (forcedCacheReceipt cache)

solverBenchmarks :: Benchmark
solverBenchmarks =
  bgroup
    "solver"
    [ unionFindBenchmarks,
      fixpointBenchmarks
    ]

unionFindBenchmarks :: Benchmark
unionFindBenchmarks =
  bgroup
    "union-find"
    ( (unionFindSizes >>= unionFindBenchmarksForSize)
        <> (unionFindSizes >>= unionFindKeyDistributionBenchmarksForSize)
    )

unionFindBenchmarksForSize :: Int -> [Benchmark]
unionFindBenchmarksForSize size =
  [ bench (caseLabel "insert+canonical compress" size) (nf unionFindBuildWeight size),
    env (pure (keys size)) $ \ids ->
      bench (caseLabel "union chain/compress" size) (nf unionFindChainWeight ids),
    bench (caseLabel "balanced union/compress" size) (nf balancedUnionFindCompressSizeWeight size),
    bench (caseLabel "transaction balanced union/compress" size) (nf transactionBalancedUnionCompressWeight size),
    bench (caseLabel "transaction prefix+sparse-outlier" size) (nf transactionSparseOutlierWeight size),
    bench (caseLabel "balanced build+cold find sweep" size) (nf balancedColdFindSweepSizeWeight size),
    bench (caseLabel "transaction build+cold find sweep" size) (nf transactionColdFindSweepSizeWeight size),
    bench (caseLabel "balanced build+compressed find sweep" size) (nf balancedCompressedFindSweepSizeWeight size),
    bench (caseLabel "paired build/equivalence sweep" size) (nf pairedUnionFindEquivalenceWeight size),
    bench (caseLabel "world: containers parent-map balanced union/canonical" size) (nf containersUnionFindBalancedWeight size),
    bench (caseLabel "hackage: equivalence balanced union/equivalence sweep" size) (nf hackageEquivalenceUnionFindWeight size)
  ]

unionFindKeyDistributionBenchmarksForSize :: Int -> [Benchmark]
unionFindKeyDistributionBenchmarksForSize size =
  [ env (pure (holeyKeys size)) $ \ids ->
      bench (caseLabel "canonical sweep/50pct holes" size) (nf unionFindCanonicalSweepWeight ids),
    env (pure (negativeKeys size)) $ \ids ->
      bench (caseLabel "canonical sweep/negative keys" size) (nf unionFindCanonicalSweepWeight ids),
    env (pure (densePrefixWithGiantOutlier size)) $ \ids ->
      bench (caseLabel "canonical sweep/dense prefix + giant outlier" size) (nf unionFindCanonicalSweepWeight ids),
    env (pure (mixedDenseSparseKeys size)) $ \ids ->
      bench (caseLabel "canonical sweep/mixed dense sparse" size) (nf unionFindCanonicalSweepWeight ids)
  ]

fixpointBenchmarks :: Benchmark
fixpointBenchmarks =
  bgroup
    "fixpoint-worklists"
    ( (foundationSizes >>= fixpointBenchmarksForSize)
        <> [bench (caseLabel "solveDenseMonotone one-read chain" 16000) (nf denseMonotoneSolverWeight 16000)]
        <> [ env (pure (hotSingletonCache 100000)) $ \cache ->
               bench (caseLabel "frozenReachabilityWithCache hot singleton isolated" 100000) (nf hotSingletonCacheWeight cache)
           ]
        <> (foundationSizes >>= variableDegreeReachabilityBenchmarksForSize)
    )

fixpointBenchmarksForSize :: Int -> [Benchmark]
fixpointBenchmarksForSize size =
  [ bench (caseLabel "solveDenseMonotone one-read chain" size) (nf denseMonotoneSolverWeight size),
    bench (caseLabel "closureUnderInt fanout=2" size) (nf closureWeight size),
    bench (caseLabel "traverseOnceIntSet fanout=2" size) (nf worklistWeight size),
    bench (caseLabel "frozenDigraph build+reach fanout=2" size) (nf denseBuildClosureWeight size),
    env (pure (forcedFrozenDigraphForSize size)) $ \denseRelation ->
      bench (caseLabel "csrTranspose/prebuilt fanout=2" size) (nf denseTransposeWeight denseRelation),
    env (pure (forcedFrozenDigraphForSize size)) $ \denseRelation ->
      bench (caseLabel "frozenReachabilityFrom/prebuilt fanout=2" size) (nf denseClosureWeight denseRelation),
    env (pure (forcedFrozenDigraphForSize size)) $ \denseRelation ->
      bench (caseLabel "frozenReachabilityFrom/prebuilt repeated fanout=2" size) (nf denseRepeatedClosureWeight denseRelation),
    bench (caseLabel "frozenDigraph build+reach cyclic fanout=2" size) (nf denseCyclicBuildClosureWeight size),
    env (pure (forcedFrozenCyclicDigraphForSize size)) $ \denseRelation ->
      bench (caseLabel "frozenReachabilityFrom/prebuilt cyclic fanout=2" size) (nf denseClosureWeight denseRelation),
    bench (caseLabel "frozenDigraph build+reach scc-heavy" size) (nf denseSccHeavyBuildClosureWeight size),
    env (pure (forcedFrozenSccHeavyDigraphForSize size)) $ \denseRelation ->
      bench (caseLabel "frozenReachabilityFrom/prebuilt scc-heavy" size) (nf denseClosureWeight denseRelation),
    env (pure (forcedFrozenSccHeavyDigraphForSize size)) $ \denseRelation ->
      bench (caseLabel "frozenReachabilityWithCache cold/hot scc-heavy" size) (nf denseCachedClosureWeight denseRelation),
    bench (caseLabel "graphSnapshot overlay reachability" size) (nf graphSnapshotOverlayReachabilityWeight size),
    bench (caseLabel "world: containers IntSet reachability fanout=2" size) (nf containersReachabilityWeight size),
    bench (caseLabel "hackage: containers Data.Graph build+reachable fanout=2" size) (nf hackageDataGraphBuildReachabilityWeight size),
    env (pure (forcedHackageDataGraphForSize size)) $ \graph ->
      bench (caseLabel "hackage: containers Data.Graph reachable/prebuilt fanout=2" size) (nf hackageDataGraphReachabilityWeight graph),
    bench (caseLabel "fixpointBounded decrement" size) (nf boundedFixpointWeight size)
  ]

variableDegreeReachabilityBenchmarksForSize :: Int -> [Benchmark]
variableDegreeReachabilityBenchmarksForSize size =
  [ bench (caseLabel "frozenDigraph build+reach var-degree" size) (nf (denseBuildClosureWith variableDegreeSuccessors) size),
    env (pure (forcedFrozenDigraphWith variableDegreeSuccessors size)) $ \denseRelation ->
      bench (caseLabel "csrTranspose/prebuilt var-degree" size) (nf denseTransposeWeight denseRelation),
    env (pure (forcedFrozenDigraphWith variableDegreeSuccessors size)) $ \denseRelation ->
      bench (caseLabel "frozenReachabilityFrom/prebuilt var-degree" size) (nf denseClosureWeight denseRelation),
    bench (caseLabel "world: containers IntSet reachability var-degree" size) (nf (containersReachabilityWith variableDegreeSuccessors) size),
    bench (caseLabel "hackage: containers Data.Graph build+reachable var-degree" size) (nf (hackageDataGraphBuildReachabilityWith variableDegreeSuccessors) size),
    env (pure (forcedHackageDataGraphWith variableDegreeSuccessors size)) $ \graph ->
      bench (caseLabel "hackage: containers Data.Graph reachable/prebuilt var-degree" size) (nf hackageDataGraphReachabilityWeight graph)
  ]

denseMonotoneSolverWeight :: Int -> Int
denseMonotoneSolverWeight size =
  either
    (const (-1))
    (Vector.sum . resultValues)
    (solveDenseMonotone intGrowthDeltaDomain size denseEvaluation (const 0))

denseEvaluation :: Int -> Evaluation Int Int
denseEvaluation key
  | key <= 0 =
      pure 1
  | otherwise =
      readEquationValue (EquationId (key - 1))

intGrowthDeltaDomain :: DeltaDomain Int Int
intGrowthDeltaDomain =
  DeltaDomain
    { deltaEmpty = 0,
      deltaNull = (== 0),
      deltaMerge = max,
      deltaApply = (+),
      deltaBetween = \oldValue newValue -> max 0 (newValue - oldValue)
    }

unionFindBuildWeight :: Int -> Int
unionFindBuildWeight =
  canonicalMapDigest . fst . UF.canonicalMapAndCompress . UF.fromClassIds . classIds

unionFindChainWeight :: [Int] -> Int
unionFindChainWeight ids =
  canonicalMapDigest (fst (UF.canonicalMapAndCompress (unionChain (fmap ClassId ids))))

unionFindCanonicalSweepWeight :: [Int] -> Int
unionFindCanonicalSweepWeight =
  canonicalMapDigest . fst . UF.canonicalMapAndCompress . UF.fromClassIds . fmap ClassId

balancedUnionFindCompressSizeWeight :: Int -> Int
balancedUnionFindCompressSizeWeight =
  canonicalMapDigest . fst . UF.canonicalMapAndCompress . balancedUnionFind

transactionBalancedUnionCompressWeight :: Int -> Int
transactionBalancedUnionCompressWeight size =
  transactionCanonicalDigest UF.emptyUnionFind $ \editor ->
    traverse_
      (\(leftClassId, rightClassId) -> UFT.transactionUnion editor leftClassId rightClassId)
      (balancedUnionPairs size)

transactionSparseOutlierWeight :: Int -> Int
transactionSparseOutlierWeight size =
  let ids = classIds size
      outlier = ClassId 1000000000
   in transactionCanonicalDigest UF.emptyUnionFind $ \editor -> do
        traverse_ (UFT.transactionInsertClassId editor) ids
        UFT.transactionInsertClassId editor outlier
        case ids of
          firstClassId : _ -> do
            _ <- UFT.transactionUnion editor outlier firstClassId
            pure ()
          [] ->
            pure ()

transactionColdFindSweepSizeWeight :: Int -> Int
transactionColdFindSweepSizeWeight size =
  transactionCanonicalDigest (balancedUnionFind size) $ \editor ->
    traverse_ (UFT.transactionFind editor) (classIds size)

transactionCanonicalDigest :: UF.UnionFind -> (forall state. UFT.UnionFindEditor state -> ST state ()) -> Int
transactionCanonicalDigest unionFind action =
  let (digest, _) =
        UFT.runUnionFindTransaction unionFind $ \editor -> do
          action editor
          canonicalParents <- UFT.transactionCanonicalMapAndCompress editor
          pure (canonicalMapDigest canonicalParents)
   in digest

canonicalMapDigest :: IntMap ClassId -> Int
canonicalMapDigest =
  IntMap.foldlWithKey'
    (\digest key classId -> digest * 16777619 + key * 31 + classIdKey classId)
    146959810

balancedColdFindSweepSizeWeight :: Int -> Int
balancedColdFindSweepSizeWeight size =
  unionFindFindSweepWeight size (balancedUnionFind size)

balancedCompressedFindSweepSizeWeight :: Int -> Int
balancedCompressedFindSweepSizeWeight size =
  unionFindFindSweepWeight size (snd (UF.canonicalMapAndCompress (balancedUnionFind size)))

unionFindFindSweepWeight :: Int -> UF.UnionFind -> Int
unionFindFindSweepWeight size unionFind =
  canonicalMapDigest (snd (foldl' findSweepStep (unionFind, IntMap.empty) (classIds size)))

findSweepStep :: (UF.UnionFind, IntMap ClassId) -> ClassId -> (UF.UnionFind, IntMap ClassId)
findSweepStep (unionFind, roots) classId =
  let (rootClassId, compressedUnionFind) = UF.find classId unionFind
   in (compressedUnionFind, IntMap.insert (classIdKey classId) rootClassId roots)

pairedUnionFindEquivalenceWeight :: Int -> Int
pairedUnionFindEquivalenceWeight size =
  let unionFind = pairedUnionFind size
   in length
        [ ()
        | (leftKey, rightKey) <- equivalenceQueryPairs size,
          UF.equivalent (ClassId leftKey) (ClassId rightKey) unionFind
        ]

containersUnionFindBalancedWeight :: Int -> Int
containersUnionFindBalancedWeight =
  containersParentDigest . containersCanonicalParents . containersBalancedUnionFind

hackageEquivalenceUnionFindWeight :: Int -> Int
hackageEquivalenceUnionFindWeight size =
  Equivalence.runEquivM' $ do
    traverse_ Equivalence.getClass (keys size)
    traverse_
      ( \(ClassId leftKey, ClassId rightKey) ->
          Equivalence.equate leftKey rightKey
      )
      (balancedUnionPairs size)
    equivalenceResults <-
      traverse
        (uncurry Equivalence.equivalent)
        (equivalenceQueryPairs size)
    pure (length (filter id equivalenceResults))

equivalenceQueryPairs :: Int -> [(Int, Int)]
equivalenceQueryPairs size =
  [ (leftKey, (leftKey + max 1 (size `div` 2)) `mod` max 1 size)
  | leftKey <- keys size
  ]

containersBalancedUnionFind :: Int -> ContainersUnionFind
containersBalancedUnionFind size =
  containersUnionPairsFrom (containersFromKeys (keys size)) (balancedUnionPairs size)

containersFromKeys :: [Int] -> ContainersUnionFind
containersFromKeys =
  ContainersUnionFind . IntMap.fromAscList . fmap (\key -> (key, key))

containersUnionPairsFrom :: ContainersUnionFind -> [(ClassId, ClassId)] -> ContainersUnionFind
containersUnionPairsFrom =
  foldl'
    ( \unionFind (ClassId leftKey, ClassId rightKey) ->
        containersUnion leftKey rightKey unionFind
    )

containersUnion :: Int -> Int -> ContainersUnionFind -> ContainersUnionFind
containersUnion leftKey rightKey unionFind@(ContainersUnionFind parents)
  | leftRoot == rightRoot = unionFind
  | otherwise = ContainersUnionFind (IntMap.insert rightRoot leftRoot parents)
  where
    leftRoot =
      containersFindRoot leftKey unionFind
    rightRoot =
      containersFindRoot rightKey unionFind

containersCanonicalParents :: ContainersUnionFind -> IntMap Int
containersCanonicalParents unionFind@(ContainersUnionFind parents) =
  IntMap.mapWithKey (\key _parent -> containersFindRoot key unionFind) parents

containersFindRoot :: Int -> ContainersUnionFind -> Int
containersFindRoot key (ContainersUnionFind parents) =
  case IntMap.lookup key parents of
    Just parentKey
      | parentKey /= key -> containersFindRoot parentKey (ContainersUnionFind parents)
    _ -> key

containersParentDigest :: IntMap Int -> Int
containersParentDigest =
  IntMap.foldlWithKey'
    (\digest key rootKey -> digest * 16777619 + key * 31 + rootKey)
    146959810

unionChain :: [ClassId] -> UF.UnionFind
unionChain ids =
  foldl'
    (\unionFind (leftClassId, rightClassId) -> UF.union leftClassId rightClassId unionFind)
    (UF.fromClassIds ids)
    (zip ids (drop 1 ids))

balancedUnionFind :: Int -> UF.UnionFind
balancedUnionFind =
  unionPairsFrom UF.emptyUnionFind . balancedUnionPairs

unionPairsFrom :: UF.UnionFind -> [(ClassId, ClassId)] -> UF.UnionFind
unionPairsFrom =
  foldl' (\unionFind (leftClassId, rightClassId) -> UF.union leftClassId rightClassId unionFind)

balancedUnionPairs :: Int -> [(ClassId, ClassId)]
balancedUnionPairs size =
  foldMap (balancedPairsForStride size) (takeWhile (< size) (iterate (* 2) 1))

balancedPairsForStride :: Int -> Int -> [(ClassId, ClassId)]
balancedPairsForStride size stride =
  fmap
    (\leftKey -> (ClassId leftKey, ClassId (leftKey + stride)))
    [0, 2 * stride .. size - stride - 1]

pairedUnionFind :: Int -> UF.UnionFind
pairedUnionFind size =
  unionPairsFrom (UF.fromClassIds (classIds size)) (pairedClassIds size)

pairedClassIds :: Int -> [(ClassId, ClassId)]
pairedClassIds size =
  fmap
    (\key -> (ClassId key, ClassId (key + 1)))
    [0, 2 .. size - 2]

closureWeight :: Int -> Int
closureWeight size =
  IntSet.size (closureUnderInt (successors size) (IntSet.singleton 0))

worklistWeight :: Int -> Int
worklistWeight size =
  IntSet.size
    ( traverseOnceIntSet
        (\visited item -> (IntSet.insert item visited, IntSet.difference (successors size item) visited))
        IntSet.empty
        (IntSet.singleton 0)
    )

denseClosureWeight :: ForcedFrozenDigraph -> Int
denseClosureWeight (ForcedFrozenDigraph relation) =
  IntSet.size (frozenReachabilityFrom relation (IntSet.singleton 0))

denseTransposeWeight :: ForcedFrozenDigraph -> Int
denseTransposeWeight (ForcedFrozenDigraph relation) =
  let transposed = csrTranspose (graphForward relation)
   in rnfCsr transposed `seq` csrVertexCount transposed

denseRepeatedClosureWeight :: ForcedFrozenDigraph -> Int
denseRepeatedClosureWeight (ForcedFrozenDigraph relation) =
  sum
    [ IntSet.size (frozenReachabilityFrom relation (IntSet.singleton rootKey))
    | rootKey <- repeatedQueryRoots (csrVertexCount (graphForward relation))
    ]

repeatedQueryRoots :: Int -> [Int]
repeatedQueryRoots size =
  [0, 4 .. size - 1]

denseBuildClosureWeight :: Int -> Int
denseBuildClosureWeight =
  denseBuildClosureWith successors

denseBuildClosureWith :: (Int -> Int -> IntSet) -> Int -> Int
denseBuildClosureWith successorsOf size =
  denseClosureWeight (forcedFrozenDigraphWith successorsOf size)

forcedFrozenDigraphForSize :: Int -> ForcedFrozenDigraph
forcedFrozenDigraphForSize =
  forcedFrozenDigraphWith successors

forcedFrozenDigraphWith :: (Int -> Int -> IntSet) -> Int -> ForcedFrozenDigraph
forcedFrozenDigraphWith successorsOf size =
  ForcedFrozenDigraph (frozenDigraphFromSuccessors size (successorsOf size))

denseCyclicBuildClosureWeight :: Int -> Int
denseCyclicBuildClosureWeight size =
  denseClosureWeight (forcedFrozenCyclicDigraphForSize size)

forcedFrozenCyclicDigraphForSize :: Int -> ForcedFrozenDigraph
forcedFrozenCyclicDigraphForSize size =
  ForcedFrozenDigraph (frozenDigraphFromSuccessors size (cyclicSuccessors size))

denseSccHeavyBuildClosureWeight :: Int -> Int
denseSccHeavyBuildClosureWeight size =
  denseClosureWeight (forcedFrozenSccHeavyDigraphForSize size)

forcedFrozenSccHeavyDigraphForSize :: Int -> ForcedFrozenDigraph
forcedFrozenSccHeavyDigraphForSize size =
  ForcedFrozenDigraph (frozenDigraphFromSuccessors size (sccHeavySuccessors size))

denseCachedClosureWeight :: ForcedFrozenDigraph -> Int
denseCachedClosureWeight (ForcedFrozenDigraph relation) =
  let seeds = IntSet.singleton 0
      coldCache = sccClosureCacheFor relation
      (coldReachable, warmedCache) =
        frozenReachabilityWithCache coldCache seeds
      (hotReachable, _hotCache) =
        frozenReachabilityWithCache warmedCache seeds
   in IntSet.size coldReachable + IntSet.size hotReachable

hotSingletonCache :: Int -> ForcedSccClosureCache
hotSingletonCache size =
  let coldCache =
        sccClosureCacheFor (frozenDigraphFromSuccessors size (const IntSet.empty))
      seeds =
        IntSet.singleton 0
      (_firstReachable, onceQueriedCache) =
        frozenReachabilityWithCache coldCache seeds
      (secondReachable, hotCache) =
        frozenReachabilityWithCache onceQueriedCache seeds
   in ForcedSccClosureCache
        { forcedCacheReceipt = IntSet.size secondReachable,
          forcedCache = hotCache
        }

hotSingletonCacheWeight :: ForcedSccClosureCache -> Int
hotSingletonCacheWeight cache =
  IntSet.size (fst (frozenReachabilityWithCache (forcedCache cache) (IntSet.singleton 0)))

graphSnapshotOverlayReachabilityWeight :: Int -> Int
graphSnapshotOverlayReachabilityWeight size =
  IntSet.size (snapshotReachabilityFrom snapshot (IntSet.singleton 0))
  where
    snapshot =
      insertSnapshotEdge
        (Edge (max 0 (size - 1)) 0)
        (snapshotFromFrozen (frozenDigraphFromSuccessors size (successors size)))

rnfFrozenDigraph :: FrozenDigraph -> ()
rnfFrozenDigraph graph =
  rnfCsr (graphForward graph)
    `seq` rnfCsr (graphBackward graph)
    `seq` rnfSccPlan (graphSccPlan graph)

rnfSccPlan :: SccPlan -> ()
rnfSccPlan plan =
  rnf (sccOfVertex plan)
    `seq` rnfCsr (sccMembers plan)
    `seq` rnfCsr (condensation plan)
    `seq` rnfCsr (condensationBackward plan)

rnfCsr :: Csr shape -> ()
rnfCsr csr =
  rnf (csrVertexCount csr, csrOffsets csr, csrTargets csr)

boundedFixpointWeight :: Int -> Either Int Int
boundedFixpointWeight size =
  case fixpointBounded (fromIntegral size) decrementToZero size of
    Left divergence -> Left (showLength (show divergence))
    Right result -> Right result

containersReachabilityWeight :: Int -> Int
containersReachabilityWeight =
  containersReachabilityWith successors

containersReachabilityWith :: (Int -> Int -> IntSet) -> Int -> Int
containersReachabilityWith successorsOf size =
  IntSet.size (containersReachability (successorsOf size) (IntSet.singleton 0))

hackageDataGraphBuildReachabilityWeight :: Int -> Int
hackageDataGraphBuildReachabilityWeight =
  hackageDataGraphBuildReachabilityWith successors

hackageDataGraphBuildReachabilityWith :: (Int -> Int -> IntSet) -> Int -> Int
hackageDataGraphBuildReachabilityWith successorsOf =
  hackageDataGraphReachabilityWeight . forcedHackageDataGraphWith successorsOf

hackageDataGraphReachabilityWeight :: ForcedGraph -> Int
hackageDataGraphReachabilityWeight (ForcedGraph graph) =
  length (Graph.reachable graph 0)

forcedHackageDataGraphForSize :: Int -> ForcedGraph
forcedHackageDataGraphForSize =
  forcedHackageDataGraphWith successors

forcedHackageDataGraphWith :: (Int -> Int -> IntSet) -> Int -> ForcedGraph
forcedHackageDataGraphWith successorsOf size =
  ForcedGraph
    ( Graph.buildG
        (0, max 0 (size - 1))
        [ (key, target)
        | key <- keys size,
          target <- IntSet.toAscList (successorsOf size key)
        ]
    )

containersReachability :: (Int -> IntSet) -> IntSet -> IntSet
containersReachability nextItems =
  containersReachabilityStep nextItems IntSet.empty

containersReachabilityStep :: (Int -> IntSet) -> IntSet -> IntSet -> IntSet
containersReachabilityStep nextItems visited pending =
  case IntSet.minView pending of
    Nothing -> visited
    Just (item, remainingPending)
      | IntSet.member item visited ->
          containersReachabilityStep nextItems visited remainingPending
      | otherwise ->
          let nextPending =
                IntSet.union
                  remainingPending
                  (IntSet.difference (nextItems item) visited)
           in containersReachabilityStep nextItems (IntSet.insert item visited) nextPending

decrementToZero :: Int -> Int
decrementToZero value
  | value <= 0 = 0
  | otherwise = value - 1

successors :: Int -> Int -> IntSet
successors size key =
  IntSet.fromAscList
    (filter (< size) [key + 1, key + 2])

variableDegreeSuccessors :: Int -> Int -> IntSet
variableDegreeSuccessors size key
  | size <= 0 = IntSet.empty
  | otherwise =
      IntSet.fromList
        (filter (< size) [key + step | step <- [1 .. 1 + (key `mod` 6)]])

sccHeavySuccessors :: Int -> Int -> IntSet
sccHeavySuccessors size key
  | size <= 0 = IntSet.empty
  | otherwise =
      IntSet.fromList
        [ (key + 1) `mod` size,
          (key + 2) `mod` size,
          (key - 1) `mod` size
        ]

cyclicSuccessors :: Int -> Int -> IntSet
cyclicSuccessors size key
  | size <= 0 = IntSet.empty
  | otherwise = IntSet.fromList [(key + 1) `mod` size, (key + 2) `mod` size]

classIds :: Int -> [ClassId]
classIds size =
  fmap ClassId (keys size)

holeyKeys :: Int -> [Int]
holeyKeys =
  fmap (* 2) . keys

negativeKeys :: Int -> [Int]
negativeKeys =
  fmap negate . keys

densePrefixWithGiantOutlier :: Int -> [Int]
densePrefixWithGiantOutlier size =
  keys size <> [1_000_000_000]

mixedDenseSparseKeys :: Int -> [Int]
mixedDenseSparseKeys size =
  keys size <> fmap (+ 1_000_000_000) (sampleKeys size)

{-# LANGUAGE DeriveTraversable #-}

module TermBench
  ( termBenchmarks,
  )
where

import Data.Bifunctor (first)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector.Unboxed qualified as U
import BenchSupport
  ( caseLabel,
    keys,
    termSizes,
  )
import Moonlight.Core
  ( Column (..),
    Pattern (..),
    PatternFreeJoinPlan (..),
    RelationStats (..),
    ArrangementKey,
    Database,
    TermCommand (..),
    arrangementKeyForOperator,
    arrangementPrefixForKey,
    canonicalizeDirtyRows,
    compact,
    commitTermCommands,
    relationStats,
    committedDatabase,
    compilePatternFreeJoinPlan,
    arrangementRowsForPrefix,
    deleteTuple,
    freeJoin,
    rowsDeleted,
    rowsInserted,
    operatorRows,
    resultKeysUsingAnyChildKey,
    emptyDatabase,
    extractOperator,
    insertTuple,
    insertTuples,
    lookupLeastTuple,
    mkPatternVar,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )
import Prelude

data TestTerm key
  = TNull
  | TUnary !key
  | TBinary !key !key
  | TNary [key]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

type BenchDatabase = Database TestTerm Int

termBenchmarks :: Benchmark
termBenchmarks =
  bgroup
    "term"
    [ termTupleSetBenchmarks,
      termDatabaseBenchmarks
    ]

termTupleSetBenchmarks :: Benchmark
termTupleSetBenchmarks =
  bgroup
    "term-tuple-set"
    (termSizes >>= termTupleSetBenchmarksForSize)

termTupleSetBenchmarksForSize :: Int -> [Benchmark]
termTupleSetBenchmarksForSize size =
  [ bench (caseLabel "containers Set tuple populate/extract" size) (nf tupleSetBuildExtractWeight size),
    env (pure (tupleSetPair size)) $ \tuples ->
      bench (caseLabel "containers Set tuple merge/extract" size) (nf tupleSetMergeWeight tuples)
  ]

termDatabaseBenchmarks :: Benchmark
termDatabaseBenchmarks =
  bgroup
    "term-database"
    (termSizes >>= termDatabaseBenchmarksForSize)

termDatabaseBenchmarksForSize :: Int -> [Benchmark]
termDatabaseBenchmarksForSize size =
  [ bench (caseLabel "insert/build row-count" size) (nf databaseBuildWeight size),
    bench (caseLabel "bulk insert/build row-count" size) (nf databaseBulkBuildWeight size),
    prebuiltDatabaseLookupBenchmark size,
    bench (caseLabel "insert/lookup" size) (nf databaseBuildLookupWeight size),
    bench (caseLabel "alternating insert/query" size) (nf databaseAlternatingInsertLookupWeight size),
    bench (caseLabel "bulk insert/lookup" size) (nf databaseBulkBuildLookupWeight size),
    bench (caseLabel "command batch insert/lookup" size) (nf databaseCommandBatchBuildLookupWeight size),
    bench (caseLabel "child-user reverse lookup" size) (nf databaseChildUserLookupWeight size),
    bench (caseLabel "arrangement prefix materialization" size) (nf databaseArrangementPrefixLookupWeight size),
    bench (caseLabel "free join two-atom" size) (nf databasePatternFreeJoinWeight size),
    bench (caseLabel "canonicalize dirty rows" size) (nf databaseCanonicalizeDirtyRowsWeight size),
    bench (caseLabel "delete half+compact row-count" size) (nf databaseDeleteCompactWeight size),
    bench (caseLabel "delete indexed+unindexed rows" size) (nf databaseIndexedUnindexedDeleteWeight size),
    bench (caseLabel "relation stats result/child prefixes" size) (nf databaseRelationStatsWeight size),
    bench (caseLabel "hackage: containers Map term insert/lookup" size) (nf hackageContainersTermMapLookupWeight size),
    bench (caseLabel "hackage: containers Map term->IntSet insert/lookup" size) (nf hackageContainersTermMultiMapLookupWeight size),
    env (pure (keys size)) $ \lookupKeys ->
      bench (caseLabel "build/lookup sweep" size) (nf databaseBuildLookupKeysWeight lookupKeys)
  ]

tupleSetBuildExtractWeight :: Int -> Int
tupleSetBuildExtractWeight =
  length . Set.toAscList . Set.fromList . tupleSetTuples

tupleSetMergeWeight :: ([[Int]], [[Int]]) -> Int
tupleSetMergeWeight (leftTuples, rightTuples) =
  length (Set.toAscList (Set.union (Set.fromList leftTuples) (Set.fromList rightTuples)))

tupleSetPair :: Int -> ([[Int]], [[Int]])
tupleSetPair size =
  (tupleSetTuples size, shiftedTupleSetTuples size)

tupleSetTuples :: Int -> [[Int]]
tupleSetTuples size =
  fmap (\key -> [key, key + 1, key + 2]) (keys size)

shiftedTupleSetTuples :: Int -> [[Int]]
shiftedTupleSetTuples size =
  fmap (\key -> [key, key + 2, key + 4]) (keys size)

databaseBuildLookupWeight :: Int -> Int
databaseBuildLookupWeight size =
  databaseLookupWeight size (databaseForSize size)

databaseBuildWeight :: Int -> Int
databaseBuildWeight =
  databaseRowCount . databaseForSize

databaseAlternatingInsertLookupWeight :: Int -> Int
databaseAlternatingInsertLookupWeight size =
  snd $
    foldl'
      insertAndQuery
      (emptyDatabase, 0)
      (keys size)
  where
    insertAndQuery (database, checksum) key =
      ( insertedDatabase,
        checksum + maybe 0 id (lookupLeastTuple (termForKey key) insertedDatabase)
      )
      where
        insertedDatabase =
          insertTuple key (termForKey key) database

databaseBulkBuildLookupWeight :: Int -> Int
databaseBulkBuildLookupWeight size =
  databaseLookupWeight size (bulkDatabaseForSize size)

databaseBulkBuildWeight :: Int -> Int
databaseBulkBuildWeight =
  databaseRowCount . bulkDatabaseForSize

databaseCommandBatchBuildLookupWeight :: Int -> Int
databaseCommandBatchBuildLookupWeight size =
  databaseLookupWeight size (databaseCommandBatchForSize size)

prebuiltDatabaseLookupBenchmark :: Int -> Benchmark
prebuiltDatabaseLookupBenchmark size =
  databaseRowCount database `seq`
    bench (caseLabel "lookup/prebuilt" size) (nf (databaseLookupKeysWeight lookupKeys) database)
  where
    database =
      databaseForSize size
    lookupKeys =
      keys size

hackageContainersTermMapLookupWeight :: Int -> Int
hackageContainersTermMapLookupWeight size =
  sum
    [ maybe 0 id (Map.lookup (termForKey key) table)
    | key <- keys size
    ]
  where
    table =
      Map.fromList
        [ (termForKey key, key)
        | key <- keys size
        ]

hackageContainersTermMultiMapLookupWeight :: Int -> Int
hackageContainersTermMultiMapLookupWeight size =
  sum
    [ maybe 0 leastIntSetValue (Map.lookup (termForKey key) table)
    | key <- keys size
    ]
  where
    table =
      Map.fromListWith
        IntSet.union
        [ (termForKey key, IntSet.singleton key)
        | key <- keys size
        ]

databaseBuildLookupKeysWeight :: [Int] -> Int
databaseBuildLookupKeysWeight lookupKeys =
  databaseLookupKeysWeight lookupKeys (databaseForSize (length lookupKeys))

databaseLookupWeight :: Int -> BenchDatabase -> Int
databaseLookupWeight size database =
  databaseLookupKeysWeight (keys size) database

databaseLookupKeysWeight :: [Int] -> BenchDatabase -> Int
databaseLookupKeysWeight lookupKeys database =
  sum
    [ maybe 0 id (lookupLeastTuple (termForKey key) database)
    | key <- lookupKeys
    ]

databaseChildUserLookupWeight :: Int -> Int
databaseChildUserLookupWeight size =
  IntSet.size (resultKeysUsingAnyChildKey (IntSet.fromList (sampleChildKeys size)) (databaseForSize size))

databaseArrangementPrefixLookupWeight :: Int -> Either String Int
databaseArrangementPrefixLookupWeight size =
  first show $ do
    let operator =
          extractOperator (TBinary () ())
    arrangementKey <-
      arrangementKeyForOperator operator [ChildColumn 0]
    arrangementPrefix <-
      arrangementPrefixForKey arrangementKey [size + 1]
    (rows, arrangedDatabase) <-
      arrangementRowsForPrefix operator arrangementKey arrangementPrefix (databaseForSize size)
    pure (length rows + databaseRowCount arrangedDatabase)

databasePatternFreeJoinWeight :: Int -> Either String Int
databasePatternFreeJoinWeight size =
  first show $ do
    (bindings, _database) <-
      freeJoin plan (databaseForSize size)
    pure (length bindings + length roots)
  where
    PatternFreeJoinPlan
      { patternFreeJoinPlan = plan,
        patternFreeJoinRoots = roots
      } =
        compileBinaryPatternFreeJoinPlan


databaseCanonicalizeDirtyRowsWeight :: Int -> Int
databaseCanonicalizeDirtyRowsWeight size =
  databaseRowCount canonicalDatabase + length commands + insertedCount - deletedCount
  where
    dirtyKeys =
      IntSet.fromList (sampleChildKeys size)
    delta =
      canonicalizeDirtyRows dirtyKeys id (databaseForSize size)
    (rowDelta, commands, canonicalDatabase) =
      delta
    insertedCount =
      sum (fmap length (Map.elems (rowsInserted rowDelta)))
    deletedCount =
      sum (fmap length (Map.elems (rowsDeleted rowDelta)))

databaseDeleteCompactWeight :: Int -> Int
databaseDeleteCompactWeight size =
  databaseRowCount compactedDatabase
    + databaseLookupWeight size compactedDatabase
  where
    compactedDatabase =
      compact (deleteHalfDatabaseForSize size)

deleteHalfDatabaseForSize :: Int -> BenchDatabase
deleteHalfDatabaseForSize size =
  foldl'
    (\database key -> deleteTuple key (termForKey key) database)
    (databaseForSize size)
    (filter even (keys size))

databaseIndexedUnindexedDeleteWeight :: Int -> Either String Int
databaseIndexedUnindexedDeleteWeight size = do
  arrangementKey <-
    first show (arrangementKeyForOperator binaryOperator [ResultColumn])
  emptyPrefix <-
    first show (arrangementPrefixForKey arrangementKey [])
  (_indexedRows, indexedDatabase) <-
    first show $
      arrangementRowsForPrefix
        binaryOperator
        arrangementKey
        emptyPrefix
        firstBurstDatabase
  let completedDatabase =
        foldl'
          (\database key -> insertTuple key (termForKey key) database)
          indexedDatabase
          secondBurstKeys
      deletedDatabase =
        foldl'
          (\database key -> deleteTuple key (termForKey key) database)
          completedDatabase
          boundaryDeleteKeys
  pure (databaseRowCount deletedDatabase + databaseLookupWeight size deletedDatabase)
  where
    allKeys =
      keys size
    (firstBurstKeys, secondBurstKeys) =
      splitAt (size `div` 2) allKeys
    firstBurstDatabase =
      foldl'
        (\database key -> insertTuple key (termForKey key) database)
        emptyDatabase
        firstBurstKeys
    boundaryDeleteKeys =
      take 1 (filter isBinaryKey firstBurstKeys)
        <> take 1 (filter isBinaryKey secondBurstKeys)
    isBinaryKey :: Int -> Bool
    isBinaryKey key =
      key `mod` 4 == 2
    binaryOperator =
      extractOperator (TBinary () ())

databaseRelationStatsWeight :: Int -> Either String Int
databaseRelationStatsWeight size = do
  arrangementKeys <-
    relationStatsArrangementKeys
  stats <-
    first show $
      relationStats
        arrangementKeys
        (extractOperator (TBinary () ()))
        (databaseForSize size)
  pure (maybe 0 relationStatsWeight stats)

relationStatsArrangementKeys :: Either String [ArrangementKey]
relationStatsArrangementKeys =
  first show $
    traverse
      (arrangementKeyForOperator (extractOperator (TBinary () ())))
      [ [ResultColumn],
        [ChildColumn 0],
        [ChildColumn 1],
        [ResultColumn, ChildColumn 0],
        [ChildColumn 0, ChildColumn 1]
      ]

relationStatsWeight :: RelationStats -> Int
relationStatsWeight stats =
  rowCount stats
    + liveRowCount stats
    + U.sum (distinctPerColumn stats)
    + sum (Map.elems (distinctPerPrefix stats))
    + maximumBucketSize stats

compileBinaryPatternFreeJoinPlan :: PatternFreeJoinPlan TestTerm Int
compileBinaryPatternFreeJoinPlan =
  compilePatternFreeJoinPlan
    ( PatternNode
        ( TBinary
            (PatternVar (mkPatternVar 0))
            (PatternVar (mkPatternVar 1))
        )
    )

sampleChildKeys :: Int -> [Int]
sampleChildKeys size =
  fmap (+ 1) (keys size)

databaseForSize :: Int -> BenchDatabase
databaseForSize size =
  foldl'
    (\database key -> insertTuple key (termForKey key) database)
    emptyDatabase
    (keys size)

bulkDatabaseForSize :: Int -> BenchDatabase
bulkDatabaseForSize size =
  insertTuples
    (fmap (\key -> (key, termForKey key)) (keys size))
    emptyDatabase

databaseCommandBatchForSize :: Int -> BenchDatabase
databaseCommandBatchForSize size =
  committedDatabase (commitTermCommands commands emptyDatabase)
  where
    commands =
      fmap (\key -> InsertTerm key (termForKey key)) (keys size)

databaseRowCount :: BenchDatabase -> Int
databaseRowCount =
  sum . fmap length . Map.elems . operatorRows

termForKey :: Int -> TestTerm Int
termForKey key =
  case key `mod` 4 of
    0 -> TNull
    1 -> TUnary (key + 1)
    2 -> TBinary (key + 1) (key + 2)
    _ -> TNary [key + 1, key + 2, key + 3]


leastIntSetValue :: IntSet -> Int
leastIntSetValue =
  maybe 0 id . IntSet.lookupMin

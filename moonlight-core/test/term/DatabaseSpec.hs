{-# LANGUAGE DeriveTraversable #-}

module DatabaseSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Moonlight.Core (Pattern (..))
import Moonlight.Core qualified as EGraph
import Moonlight.Core
  ( Database,
    DatabaseRowDelta (..),
    ArrangementValidationError (..),
    Column (..),
    FreeJoinPlan (..),
    Operator (..),
    PatternFreeJoinPlan (..),
    QueryAtom (..),
    QueryBinding (..),
    QueryTerm (..),
    QueryVar (..),
    RelationStats (..),
    TermCommitResult (..),
    TermCommand (..),
    TupleLookup (..),
    arrangementKeyForOperator,
    arrangementPrefixForKey,
    compact,
    compilePatternFreeJoinPlan,
    compilePatternsFreeJoinPlan,
    databaseEntries,
    entriesForResultKey,
    arrangementRowsForPrefix,
    editorCompact,
    editorDeleteTuple,
    editorInsertTuple,
    freeJoin,
    relationStats,
    rowChildren,
    rowResult,
    rowsForOperator,
    deleteRow,
    deleteTuple,
    emptyDatabase,
    insertTuples,
    insertTuple,
    lookupTupleAll,
    lookupLeastTuple,
    rehydrateTuple,
    commitTermCommands,
    abortTransaction,
    runCommandTransaction,
  )
import Moonlight.Core
  ( canonicalizeDatabase,
    canonicalizeDirtyRowsInEditor,
  )
import DatabaseLawSpec qualified as DatabaseLawSpec
import Prelude
import System.Mem.StableName (makeStableName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

data TestTerm key
  = TNull
  | TUnary !key
  | TBinary !key !key
  | TNary [key]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Core.Term.Database"
    [ databaseEntriesRoundTrip,
      databaseChunkBoundaryTombstones,
      databaseResultFilter,
      databaseChildLookup,
      databaseTupleDeleteRemovesOnlyExactOwner,
      databaseArrangementPrefixLookup,
      databaseAbsentDeletePreservesArrangementCache,
      databaseArrangementValidationRejectsInvalidShapes,
      databaseRelationStatsSummarizeLiveRows,
      databaseFreeJoinUsesBoundPrefixAndEquality,
      databaseFreeJoinGluesMultiAtomBindings,
      databasePatternCompilationMatchesNestedPattern,
      databasePatternCompilationSeparatesVariableNamespaces,
      databasePatternCompilationGluesMultiplePatterns,
      databaseBulkInsertMatchesRepeatedInsert,
      databaseCommandBatchUnionsAmbiguousInserts,
      databaseCommandBatchUnionsExistingAndBatchOwners,
      databaseTransactionCommitsDeferredEdits,
      databaseTransactionAbortPreservesSnapshot,
      databaseTransactionQueuesDirtyCanonicalization,
      databaseCanonicalizationDeltaReportsCommittedInsertions,
      databaseCompactionDropsTombstoneSlots,
      rehydrateTupleArity,
      canonicalizeDatabaseRewritesKeys,
      canonicalizeDatabaseSurfacesResidualUnions,
      DatabaseLawSpec.tests
    ]

databaseEntriesRoundTrip :: TestTree
databaseEntriesRoundTrip =
  testCase "entries rehydrates nullary, unary, binary, and n-ary entries" $
    entrySet db
      @?= Set.fromList
        [ (1, TNull),
          (2, TUnary 10),
          (3, TBinary 11 12),
          (4, TNary [13, 14, 15])
        ]
  where
    db =
        insertNode (TNary [13, 14, 15]) 4 $
        insertNode (TBinary 11 12) 3 $
          insertNode (TUnary 10) 2 $
            insertNode TNull 1 emptyDatabase

databaseChunkBoundaryTombstones :: TestTree
databaseChunkBoundaryTombstones =
  testCase "column chunks, pending rows, tombstones, and compaction glue to one row set" $ do
    entrySet deletedDatabase @?= expectedEntries
    entrySet compactedDatabase @?= expectedEntries
    length (rowsForOperator naryOperator compactedDatabase) @?= Set.size expectedEntries
  where
    entries =
      fmap chunkBoundaryEntry [0 .. 1030]
    fullDatabase =
      foldl'
        (\database (resultKey, term) -> insertTuple resultKey term database)
        emptyDatabase
        entries
    selectedRowIds =
      fmap fst $
        take 1 operatorTableRows
          <> take 1 (drop 511 operatorTableRows)
          <> take 1 (drop 1024 operatorTableRows)
    operatorTableRows =
      rowsForOperator naryOperator fullDatabase
    deletedDatabase =
      foldl'
        (\database rowId -> deleteRow naryOperator rowId database)
        fullDatabase
        selectedRowIds
    compactedDatabase =
      compact deletedDatabase
    expectedEntries =
      Set.difference
        (Set.fromList entries)
        (Set.fromList (fmap chunkBoundaryEntry [0, 511, 1024]))
    naryOperator =
      Operator (TNary [(), (), ()])
    chunkBoundaryEntry :: Int -> (Int, TestTerm Int)
    chunkBoundaryEntry resultKey =
      (resultKey, TNary [resultKey + 1, resultKey + 2, resultKey + 3])

databaseResultFilter :: TestTree
databaseResultFilter =
  testCase "entriesForResultKey filters by encoded result key" $
    Set.fromList (entriesForResultKey 3 db)
      @?= Set.fromList
        [ (3, TBinary 11 12)
        ]
  where
    db =
      insertNode (TBinary 11 12) 3 $
        insertNode (TBinary 11 13) 4 emptyDatabase

databaseChildLookup :: TestTree
databaseChildLookup =
  testCase "database tuple lookup exposes ambiguous and least-result policy" $ do
    lookupTupleAll (TBinary 11 12) db @?= TupleAmbiguous (3 :| [5])
    lookupLeastTuple (TBinary 11 12) db @?= Just 3
    lookupTupleAll (TBinary 9 9) db @?= TupleMissing
    lookupLeastTuple (TBinary 9 9) db @?= Nothing
  where
    db =
      insertNode (TBinary 11 12) 5 $
        insertNode (TBinary 11 12) 3 $
          insertNode (TBinary 12 11) 7 emptyDatabase

databaseTupleDeleteRemovesOnlyExactOwner :: TestTree
databaseTupleDeleteRemovesOnlyExactOwner =
  testCase "deleteTuple removes only the exact result and children row" $ do
    entrySet deletedDb
      @?= Set.fromList
        [ (5, TBinary 11 12),
          (7, TBinary 12 11)
        ]
    lookupTupleAll (TBinary 11 12) deletedDb @?= TupleUnique 5
  where
    deletedDb =
      deleteTuple 3 (TBinary 11 12) db

    db =
      insertNode (TBinary 11 12) 5 $
        insertNode (TBinary 11 12) 3 $
          insertNode (TBinary 12 11) 7 emptyDatabase

databaseArrangementPrefixLookup :: TestTree
databaseArrangementPrefixLookup =
  testCase "lazy arrangement prefix lookup materializes requested prefixes on demand" $ do
    exactBinaryKey <-
      expectRight (arrangementKeyForOperator binaryOperator [ChildColumn 0, ChildColumn 1, ResultColumn])
    firstPrefix <-
      expectRight (arrangementPrefixForKey exactBinaryKey [11])
    (firstChildRows, arrangedDb) <-
      expectRight (arrangementRowsForPrefix binaryOperator exactBinaryKey firstPrefix db)
    siblingPrefix <-
      expectRight (arrangementPrefixForKey exactBinaryKey [12])
    (siblingRows, _cachedDb) <-
      expectRight (arrangementRowsForPrefix binaryOperator exactBinaryKey siblingPrefix arrangedDb)
    fmap (rowResult . snd) firstChildRows @?= [3, 5]
    fmap (rowResult . snd) siblingRows @?= [7]
  where
    db =
      insertNode (TBinary 11 12) 5 $
        insertNode (TBinary 11 12) 3 $
          insertNode (TBinary 12 11) 7 emptyDatabase

databaseAbsentDeletePreservesArrangementCache :: TestTree
databaseAbsentDeletePreservesArrangementCache =
  testCase "absent tuple deletion returns the arrangement-bearing database unchanged" $ do
    exactBinaryKey <-
      expectRight (arrangementKeyForOperator binaryOperator [ChildColumn 0, ChildColumn 1, ResultColumn])
    prefix <-
      expectRight (arrangementPrefixForKey exactBinaryKey [11])
    (_rows, arrangedDb) <-
      expectRight (arrangementRowsForPrefix binaryOperator exactBinaryKey prefix db)
    let unchangedDb =
          deleteTuple 99 (TBinary 90 91) arrangedDb
    arrangedDb `seq` unchangedDb `seq` pure ()
    arrangedName <- makeStableName arrangedDb
    unchangedName <- makeStableName unchangedDb
    assertBool "absent deletion rebuilt the database and discarded cached arrangements" (arrangedName == unchangedName)
  where
    db =
      insertNode (TBinary 11 12) 3 emptyDatabase

databaseArrangementValidationRejectsInvalidShapes :: TestTree
databaseArrangementValidationRejectsInvalidShapes =
  testCase "arrangement construction rejects invalid columns, prefixes, and operator reuse" $ do
    arrangementKeyForOperator binaryOperator [ChildColumn (-1)]
      @?= Left (NegativeArrangementChildColumn (-1))
    arrangementKeyForOperator binaryOperator [ChildColumn 2]
      @?= Left (ArrangementChildColumnOutOfBounds 2 2)
    unaryKey <-
      expectRight (arrangementKeyForOperator unaryOperator [ChildColumn 0])
    arrangementPrefixForKey unaryKey [10, 11]
      @?= Left (ArrangementPrefixTooDeep 2 1)
    emptyPrefix <-
      expectRight (arrangementPrefixForKey unaryKey [])
    case arrangementRowsForPrefix binaryOperator unaryKey emptyPrefix emptyDatabase of
      Left mismatch ->
        mismatch @?= ArrangementOperatorArityMismatch 1 2
      Right _fabricatedRows ->
        assertFailure "arrangement key for a unary operator was accepted by a binary operator"

databaseRelationStatsSummarizeLiveRows :: TestTree
databaseRelationStatsSummarizeLiveRows =
  testCase "relationStats reports live prefix and bucket cardinalities" $ do
    exactBinaryKey <-
      expectRight (arrangementKeyForOperator binaryOperator [ChildColumn 0, ChildColumn 1])
    statsResult <-
      expectRight (relationStats [exactBinaryKey] binaryOperator db)
    case statsResult of
      Just stats -> do
        rowCount stats @?= 3
        liveRowCount stats @?= 3
        distinctPerPrefix stats @?= Map.singleton exactBinaryKey 2
        maximumBucketSize stats @?= 2
      Nothing ->
        assertFailure "expected relation stats for binary operator"
  where
    db =
      insertNode (TBinary 11 12) 5 $
        insertNode (TBinary 11 12) 3 $
          insertNode (TBinary 12 11) 7 emptyDatabase

databaseFreeJoinUsesBoundPrefixAndEquality :: TestTree
databaseFreeJoinUsesBoundPrefixAndEquality =
  testCase "freeJoin uses bound-prefix probes and repeated-variable equality" $ do
    (bindings, _arrangedDb) <- expectRight joinResult
    bindingSet bindings
      @?= Set.fromList
        [ Map.fromList
            [ (ExplicitQueryVar 0, 1),
              (ExplicitQueryVar 1, 10)
            ]
        ]
  where
    joinResult =
      freeJoin
        ( FreeJoinPlan
            [ QueryAtom
                { atomOperator = binaryOperator,
                  atomResult = QueryVariable (ExplicitQueryVar 0),
                  atomChildren =
                    [ QueryVariable (ExplicitQueryVar 1),
                      QueryVariable (ExplicitQueryVar 1)
                    ]
                }
            ]
        )
        db

    db =
      insertNode (TBinary 10 10) 1 $
        insertNode (TBinary 10 11) 2 emptyDatabase

databaseFreeJoinGluesMultiAtomBindings :: TestTree
databaseFreeJoinGluesMultiAtomBindings =
  testCase "freeJoin glues compatible bindings across atoms" $ do
    (bindings, _arrangedDb) <- expectRight joinResult
    bindingSet bindings
      @?= Set.fromList
        [ Map.fromList
            [ (ExplicitQueryVar 0, 100),
              (ExplicitQueryVar 1, 1),
              (ExplicitQueryVar 2, 2),
              (ExplicitQueryVar 3, 101),
              (ExplicitQueryVar 4, 3)
            ]
        ]
  where
    joinResult =
      freeJoin
        ( FreeJoinPlan
            [ QueryAtom
                { atomOperator = binaryOperator,
                  atomResult = QueryVariable (ExplicitQueryVar 0),
                  atomChildren =
                    [ QueryVariable (ExplicitQueryVar 1),
                      QueryVariable (ExplicitQueryVar 2)
                    ]
                },
              QueryAtom
                { atomOperator = binaryOperator,
                  atomResult = QueryVariable (ExplicitQueryVar 3),
                  atomChildren =
                    [ QueryVariable (ExplicitQueryVar 2),
                      QueryVariable (ExplicitQueryVar 4)
                    ]
                }
            ]
        )
        db

    db =
      insertNode (TBinary 1 2) 100 $
        insertNode (TBinary 2 3) 101 $
          insertNode (TBinary 8 9) 102 emptyDatabase

databasePatternCompilationMatchesNestedPattern :: TestTree
databasePatternCompilationMatchesNestedPattern =
  testCase "compilePatternFreeJoinPlan lowers nested repeated variables into relation atoms" $ do
    (bindings, _arrangedDb) <- expectRight joinResult
    patternFreeJoinRoots compiled
      @?= QueryVariable (GeneratedPatternNodeVar 0) :| []
    patternFreeJoinVariables compiled
      @?= Map.singleton (EGraph.mkPatternVar 0) (AuthoredPatternVar (EGraph.mkPatternVar 0))
    bindingSet bindings
      @?= Set.fromList
        [ Map.fromList
            [ (AuthoredPatternVar (EGraph.mkPatternVar 0), 10),
              (GeneratedPatternNodeVar 0, 100),
              (GeneratedPatternNodeVar 1, 11)
            ]
        ]
  where
    compiled :: PatternFreeJoinPlan TestTerm Int
    compiled =
      compilePatternFreeJoinPlan
        ( PatternNode
            ( TBinary
                (PatternVar (EGraph.mkPatternVar 0))
                (PatternNode (TUnary (PatternVar (EGraph.mkPatternVar 0))))
            )
        )

    joinResult =
      freeJoin (patternFreeJoinPlan compiled) db

    db =
      insertNode (TBinary 10 11) 100 $
        insertNode (TUnary 10) 11 $
          insertNode (TBinary 10 12) 101 $
            insertNode (TUnary 20) 12 emptyDatabase

databasePatternCompilationSeparatesVariableNamespaces :: TestTree
databasePatternCompilationSeparatesVariableNamespaces =
  testCase "pattern compilation keeps authored and generated query variables disjoint" $ do
    patternFreeJoinVariables compiled
      @?= Map.fromList
        [ (EGraph.mkPatternVar 0, AuthoredPatternVar (EGraph.mkPatternVar 0)),
          (EGraph.mkPatternVar maxBound, AuthoredPatternVar (EGraph.mkPatternVar maxBound))
        ]
    patternFreeJoinRoots compiled
      @?= QueryVariable (GeneratedPatternNodeVar 0) :| []
    (bindings, _arrangedDatabase) <- expectRight (freeJoin (patternFreeJoinPlan compiled) database)
    bindingSet bindings
      @?= Set.singleton
        ( Map.fromList
            [ (AuthoredPatternVar (EGraph.mkPatternVar 0), 10),
              (AuthoredPatternVar (EGraph.mkPatternVar maxBound), 20),
              (GeneratedPatternNodeVar 0, 100)
            ]
        )
  where
    compiled :: PatternFreeJoinPlan TestTerm Int
    compiled =
      compilePatternFreeJoinPlan
        ( PatternNode
            ( TBinary
                (PatternVar (EGraph.mkPatternVar 0))
                (PatternVar (EGraph.mkPatternVar maxBound))
            )
        )

    database =
      insertNode (TBinary 10 20) 100 emptyDatabase

databasePatternCompilationGluesMultiplePatterns :: TestTree
databasePatternCompilationGluesMultiplePatterns =
  testCase "compilePatternsFreeJoinPlan shares pattern variables across conjunctive roots" $ do
    (bindings, _arrangedDb) <- expectRight joinResult
    bindingSet bindings
      @?= Set.fromList
        [ Map.fromList
            [ (AuthoredPatternVar (EGraph.mkPatternVar 0), 10),
              (AuthoredPatternVar (EGraph.mkPatternVar 1), 20),
              (GeneratedPatternNodeVar 0, 100),
              (GeneratedPatternNodeVar 1, 200)
            ]
        ]
  where
    compiled :: PatternFreeJoinPlan TestTerm Int
    compiled =
      compilePatternsFreeJoinPlan
        ( PatternNode (TBinary (PatternVar (EGraph.mkPatternVar 0)) (PatternVar (EGraph.mkPatternVar 1)))
            :| [PatternNode (TUnary (PatternVar (EGraph.mkPatternVar 1)))]
        )

    joinResult =
      freeJoin (patternFreeJoinPlan compiled) db

    db =
      insertNode (TBinary 10 20) 100 $
        insertNode (TBinary 20 10) 101 $
          insertNode (TUnary 20) 200 emptyDatabase

databaseBulkInsertMatchesRepeatedInsert :: TestTree
databaseBulkInsertMatchesRepeatedInsert =
  testCase "insertTuples preserves repeated insertTuple semantics" $
    entrySet bulkDb @?= entrySet repeatedDb
  where
    entries =
      [ (1, TNull),
        (2, TUnary 10),
        (3, TBinary 11 12),
        (5, TBinary 11 12),
        (4, TNary [13, 14, 15])
      ]

    bulkDb =
      insertTuples entries emptyDatabase

    repeatedDb =
      foldl'
        (\database (resultKey, node) -> insertTuple resultKey node database)
        emptyDatabase
        entries

databaseCommandBatchUnionsAmbiguousInserts :: TestTree
databaseCommandBatchUnionsAmbiguousInserts =
  testCase "commitTermCommands coalesces batched inserts without dropping ambiguity unions" $ do
    unionResultSet (residualCommands commandResult) @?= Set.singleton (3, 5)
    fmap (rowResult . snd) (Map.findWithDefault [] binaryOperator (insertedRows commandResult))
      @?= [3, 5]
    lookupTupleAll (TBinary 11 12) (committedDatabase commandResult) @?= TupleAmbiguous (3 :| [5])
  where
    commandResult :: TermCommitResult TestTerm Int
    commandResult =
      commitTermCommands
        [ InsertTerm 5 (TBinary 11 12),
          InsertTerm 3 (TBinary 11 12)
        ]
        emptyDatabase

databaseCommandBatchUnionsExistingAndBatchOwners :: TestTree
databaseCommandBatchUnionsExistingAndBatchOwners =
  testCase "commitTermCommands preserves existing owners while coalescing a batch" $ do
    unionResultSet (residualCommands commandResult) @?= Set.fromList [(3, 5), (3, 7), (5, 7)]
    fmap (rowResult . snd) (Map.findWithDefault [] binaryOperator (insertedRows commandResult))
      @?= [3, 5]
    lookupTupleAll (TBinary 11 12) (committedDatabase commandResult) @?= TupleAmbiguous (3 :| [5, 7])
  where
    baseDb =
      insertNode (TBinary 11 12) 7 emptyDatabase

    commandResult :: TermCommitResult TestTerm Int
    commandResult =
      commitTermCommands
        [ InsertTerm 5 (TBinary 11 12),
          InsertTerm 3 (TBinary 11 12)
        ]
        baseDb

databaseTransactionCommitsDeferredEdits :: TestTree
databaseTransactionCommitsDeferredEdits =
  testCase "runCommandTransaction commits deferred row edits at the sealed boundary" $
    entrySet committedDb
      @?= Set.fromList [(2, TNull)]
  where
    baseDb =
      insertNode (TBinary 1 2) 1 emptyDatabase

    (_result, _residualCommands, committedDb) =
      runCommandTransaction baseDb $ \editor -> do
        editorDeleteTuple 1 (TBinary 1 2) editor
        editorInsertTuple 2 TNull editor
        editorCompact editor

databaseTransactionAbortPreservesSnapshot :: TestTree
databaseTransactionAbortPreservesSnapshot =
  testCase "abortTransaction discards deferred edits" $
    entrySet abortedDb @?= Set.empty
  where
    (_result, _residualCommands, abortedDb) =
      runCommandTransaction (emptyDatabase :: Database TestTerm Int) $ \editor -> do
        editorInsertTuple 1 TNull editor
        abortTransaction editor

databaseTransactionQueuesDirtyCanonicalization :: TestTree
databaseTransactionQueuesDirtyCanonicalization =
  testCase "canonicalizeDirtyRowsInEditor queues canonical row commands" $
    entrySet canonicalDb
      @?= Set.fromList
        [ (1, TBinary 2 3),
          (4, TUnary 2)
        ]
  where
    db =
      insertNode (TBinary 20 3) 10 $
        insertNode (TUnary 20) 40 emptyDatabase

    (_deltaAndCommands, _residualCommands, canonicalDb) =
      runCommandTransaction db $
        canonicalizeDirtyRowsInEditor (IntSet.fromList [10, 20, 40]) canonicalize

    canonicalize :: Int -> Int
    canonicalize key =
      case key of
        10 -> 1
        20 -> 2
        40 -> 4
        other -> other

databaseCanonicalizationDeltaReportsCommittedInsertions :: TestTree
databaseCanonicalizationDeltaReportsCommittedInsertions =
  testCase "canonicalization delta reports committed inserted rows, not collapsed candidates" $ do
    rowsInserted collapsedDelta @?= Map.empty
    case Map.elems (rowsInserted insertedDelta) of
      [[(_rowId, insertedRow)]] -> do
        rowResult insertedRow @?= 3
        rowChildren insertedRow @?= [2]
      insertedRows ->
        assertFailure ("expected one committed inserted row, got " <> show insertedRows)
  where
    collapsedDb =
      insertNode (TUnary 2) 3 $
        insertNode (TUnary 2) 1 emptyDatabase

    ((collapsedDelta, _collapsedCommands), _collapsedResidualCommands, _collapsedCanonicalDb) =
      runCommandTransaction collapsedDb $
        canonicalizeDirtyRowsInEditor (IntSet.singleton 3) collapseToExisting

    collapseToExisting :: Int -> Int
    collapseToExisting key =
      case key of
        3 -> 1
        other -> other

    insertedDb =
      insertNode (TUnary 20) 3 emptyDatabase

    ((insertedDelta, _insertedCommands), _insertedResidualCommands, _insertedCanonicalDb) =
      runCommandTransaction insertedDb $
        canonicalizeDirtyRowsInEditor (IntSet.fromList [20]) rewriteChild

    rewriteChild :: Int -> Int
    rewriteChild key =
      case key of
        20 -> 2
        other -> other

databaseCompactionDropsTombstoneSlots :: TestTree
databaseCompactionDropsTombstoneSlots =
  testCase "compact preserves entries and rekeys live rows after deletion" $
    case rowsForOperator binaryOperator fullDb of
      (deletedRowId, _) : _ -> do
        let deletedDb =
              deleteRow binaryOperator deletedRowId fullDb
            compactedDb =
              compact deletedDb
            deletedRowIds =
              fmap fst (rowsForOperator binaryOperator deletedDb)
            compactedRowIds =
              fmap fst (rowsForOperator binaryOperator compactedDb)
        entrySet compactedDb @?= entrySet deletedDb
        assertBool
          "compaction should rebuild live rows into fresh contiguous row ids"
          (deletedRowIds /= compactedRowIds)
      [] ->
        assertFailure "expected rows before compaction"
  where
    fullDb =
      insertNode (TBinary 3 4) 2 $
        insertNode (TBinary 1 2) 1 emptyDatabase

rehydrateTupleArity :: TestTree
rehydrateTupleArity =
  testCase "rehydrateTuple accepts exactly one result plus template arity children" $ do
    rehydrateBinaryTuple [7, 1, 2]
      @?= Just (7, TBinary 1 2)
    rehydrateBinaryTuple [7, 1]
      @?= Nothing
    rehydrateBinaryTuple [7, 1, 2, 3]
      @?= Nothing
    rehydrateBinaryTuple []
      @?= Nothing
  where
    rehydrateBinaryTuple :: [Int] -> Maybe (Int, TestTerm Int)
    rehydrateBinaryTuple =
      rehydrateTuple binaryOperator

binaryOperator :: Operator TestTerm
binaryOperator =
  Operator (TBinary () ())

unaryOperator :: Operator TestTerm
unaryOperator =
  Operator (TUnary ())

expectRight :: Either ArrangementValidationError value -> IO value
expectRight eitherValue =
  case eitherValue of
    Left validationError ->
      assertFailure ("unexpected arrangement validation failure: " <> show validationError)
    Right value ->
      pure value

canonicalizeDatabaseRewritesKeys :: TestTree
canonicalizeDatabaseRewritesKeys =
  testCase "canonicalizeDatabase rewrites every encoded key through the canonicalizer" $
    case canonicalDb of
      Left _residualCommands ->
        assertFailure "congruence-closed canonicalization returned residual commands"
      Right rewrittenDb ->
        entrySet rewrittenDb
          @?= Set.fromList
            [ (1, TBinary 2 3),
              (4, TUnary 2)
            ]
  where
    db =
      insertNode (TBinary 20 3) 10 $
        insertNode (TUnary 20) 40 emptyDatabase

    canonicalize :: Int -> Int
    canonicalize key =
      case key of
        10 -> 1
        20 -> 2
        40 -> 4
        other -> other

    canonicalDb =
      canonicalizeDatabase canonicalize db

canonicalizeDatabaseSurfacesResidualUnions :: TestTree
canonicalizeDatabaseSurfacesResidualUnions =
  testCase "canonicalizeDatabase refuses to discard residual congruence unions" $
    case canonicalizeDatabase collapseToExisting db of
      Left [UnionResults left right] ->
        Set.fromList [left, right] @?= Set.fromList [1, 3]
      Left _otherCommands ->
        assertFailure "expected exactly one residual union command"
      Right _database ->
        assertFailure "non-congruence-closed canonicalization fabricated success"
  where
    db =
      insertNode (TUnary 2) 3 $
        insertNode (TUnary 4) 1 emptyDatabase

    collapseToExisting :: Int -> Int
    collapseToExisting key =
      case key of
        4 -> 2
        other -> other

insertNode ::
  TestTerm Int ->
  Int ->
  Database TestTerm Int ->
  Database TestTerm Int
insertNode node resultKey =
  insertTuple resultKey node

entrySet ::
  Database TestTerm Int ->
  Set.Set (Int, TestTerm Int)
entrySet =
  Set.fromList . databaseEntries

bindingSet :: [QueryBinding Int] -> Set.Set (Map.Map QueryVar Int)
bindingSet =
  Set.fromList . fmap queryBindingAssignments

unionResultSet :: Ord key => [TermCommand f key] -> Set.Set (key, key)
unionResultSet =
  foldr collectUnionResult Set.empty
  where
    collectUnionResult :: Ord key => TermCommand f key -> Set.Set (key, key) -> Set.Set (key, key)
    collectUnionResult command pairs =
      case command of
        UnionResults left right ->
          Set.insert (left, right) pairs
        _ ->
          pairs

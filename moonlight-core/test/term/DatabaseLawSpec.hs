{-# LANGUAGE DeriveTraversable #-}

module DatabaseLawSpec
  ( tests,
  )
where

import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Core
  ( IsLawName (..),
    constructorLawName,
  )
import Moonlight.Core
  ( ArrangementValidationError,
    Database,
    DatabaseRow,
    DatabaseRowDelta (..),
    FreeJoinPlan (..),
    Operator,
    QueryAtom (..),
    QueryBinding (..),
    QueryTerm (..),
    QueryVar (..),
    TermCommitResult (..),
    TermCommand (..),
    TupleLookup (..),
    commitTermCommands,
    compact,
    databaseEntries,
    entriesForResultKey,
    operatorRows,
    rowChildren,
    rowEntry,
    rowResult,
    rowsForOperator,
    deleteTuple,
    emptyDatabase,
    extractOperator,
    insertTuple,
    insertTuples,
    lookupTupleAll,
    rehydrateTuple,
  )
import Moonlight.Core qualified as Database
import Moonlight.Core
  ( canonicalizeDatabase,
    canonicalizeDirtyRows,
  )
import Prelude
import Test.Tasty (TestTree, localOption, testGroup)
import Test.QuickCheck (Testable)
import Test.Tasty.QuickCheck qualified as QuickCheck

data TestTerm key
  = TNull
  | TUnary !key
  | TBinary !key !key
  | TNary [key]
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data TermDatabaseLawName
  = TermDatabaseRoundTrip
  | TermDatabaseInsertIdempotenceUnionClosure
  | TermDatabaseCommitInsertFrontier
  | TermDatabaseBulkEqualsRepeated
  | TermDatabaseCompactionFixpoint
  | TermDatabaseCanonicalizationMorphism
  | TermDatabaseFreeJoinNaiveEnumeration
  | TermDatabaseWatermarkDeferredIndexInterleaving
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName TermDatabaseLawName where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  localOption (QuickCheck.QuickCheckTests 80) $
    testGroup
      "Moonlight.Core.Term.Database laws"
      [ lawProperty TermDatabaseRoundTrip propRoundTrip,
        lawProperty TermDatabaseInsertIdempotenceUnionClosure propInsertIdempotenceUnionClosure,
        lawProperty TermDatabaseCommitInsertFrontier propCommitInsertFrontier,
        lawProperty TermDatabaseBulkEqualsRepeated propBulkEqualsRepeated,
        lawProperty TermDatabaseCompactionFixpoint propCompactionFixpoint,
        lawProperty TermDatabaseCanonicalizationMorphism propCanonicalizationMorphism,
        lawProperty TermDatabaseFreeJoinNaiveEnumeration propFreeJoinNaiveEnumeration,
        lawProperty TermDatabaseWatermarkDeferredIndexInterleaving propWatermarkDeferredIndexInterleaving
      ]

lawProperty :: Testable property => TermDatabaseLawName -> property -> TestTree
lawProperty lawName =
  QuickCheck.testProperty (lawNameText lawName)

newtype RoundTripCase = RoundTripCase (Int, TestTerm Int)
  deriving stock (Eq, Show)

data CommitClosureCase = CommitClosureCase [(Int, TestTerm Int)] [(Int, TestTerm Int)]
  deriving stock (Eq, Show)

data BulkInsertCase = BulkInsertCase [(Int, TestTerm Int)] [(Int, TestTerm Int)]
  deriving stock (Eq, Show)

data CompactionCase = CompactionCase [(Int, TestTerm Int)] [(Int, TestTerm Int)]
  deriving stock (Eq, Show)

data KeyCanonicalizer = KeyCanonicalizer !Int
  deriving stock (Eq, Show)

data CanonicalizationCase = CanonicalizationCase [(Int, TestTerm Int)] !IntSet !KeyCanonicalizer
  deriving stock (Eq, Show)

data FreeJoinCase = FreeJoinCase [(Int, TestTerm Int)] (FreeJoinPlan TestTerm Int)

instance Show FreeJoinCase where
  show (FreeJoinCase entries plan) =
    "FreeJoinCase " <> show entries <> " " <> showFreeJoinPlan plan

newtype WatermarkInterleavingCase = WatermarkInterleavingCase [TermDatabaseAction]
  deriving stock (Eq, Show)

data TermDatabaseAction
  = InsertSingle !(Int, TestTerm Int)
  | InsertBatch ![(Int, TestTerm Int)]
  | DeleteExact !(Int, TestTerm Int)
  | CompactDatabase
  | QueryEntriesForResultKey !Int
  | QueryTupleAll !(TestTerm Int)
  | QueryFixedFreeJoin
  deriving stock (Eq, Show)

newtype ReferenceTermTable = ReferenceTermTable (Map (TestTerm Int) (Set Int))

data InterleavingState = InterleavingState
  { interleavingDatabase :: !(Database TestTerm Int),
    interleavingReference :: !ReferenceTermTable
  }

data InterleavingMismatch
  = FinalEntriesMismatch !(Set (Int, TestTerm Int)) !(Set (Int, TestTerm Int))
  | ResultEntriesMismatch !Int !(Set (Int, TestTerm Int)) !(Set (Int, TestTerm Int))
  | TupleOwnersMismatch !(TestTerm Int) !(Set Int) !(Set Int)
  | FixedFreeJoinValidationFailure !ArrangementValidationError
  | FixedFreeJoinBindingsMismatch !(Set (Map QueryVar Int)) !(Set (Map QueryVar Int))
  deriving stock (Show)

instance QuickCheck.Arbitrary RoundTripCase where
  arbitrary =
    RoundTripCase <$> entryGen

instance QuickCheck.Arbitrary CommitClosureCase where
  arbitrary =
    CommitClosureCase <$> entryListGen 14 <*> entryListGen 14

instance QuickCheck.Arbitrary BulkInsertCase where
  arbitrary =
    BulkInsertCase <$> entryListGen 18 <*> entryListGen 18

instance QuickCheck.Arbitrary CompactionCase where
  arbitrary =
    CompactionCase <$> entryListGen 18 <*> entryListGen 10

instance QuickCheck.Arbitrary KeyCanonicalizer where
  arbitrary =
    KeyCanonicalizer <$> QuickCheck.chooseInt (1, 8)

instance QuickCheck.Arbitrary CanonicalizationCase where
  arbitrary = do
    entries <- entryListGen 18
    canonicalizer <- QuickCheck.arbitrary
    dirtyKeys <- dirtyKeySetGen entries
    pure (CanonicalizationCase entries dirtyKeys canonicalizer)

instance QuickCheck.Arbitrary FreeJoinCase where
  arbitrary =
    FreeJoinCase <$> entryListGen 12 <*> freeJoinPlanGen

instance QuickCheck.Arbitrary WatermarkInterleavingCase where
  arbitrary = do
    sharedEntry <- interleavingEntryGen
    batchEntries <- interleavingEntryListGen 5
    resultProbe <- interleavingKeyGen
    tupleProbe <- interleavingTermGen
    additionalActionCount <- QuickCheck.chooseInt (10, 22)
    additionalActions <- QuickCheck.vectorOf additionalActionCount interleavingActionGen
    WatermarkInterleavingCase
      <$> QuickCheck.shuffle
        ( [ InsertSingle sharedEntry,
            InsertBatch batchEntries,
            DeleteExact sharedEntry,
            CompactDatabase,
            QueryEntriesForResultKey resultProbe,
            QueryTupleAll tupleProbe,
            QueryFixedFreeJoin
          ]
            <> additionalActions
        )

propRoundTrip :: RoundTripCase -> QuickCheck.Property
propRoundTrip (RoundTripCase (resultKey, term)) =
  case rowsForOperator operator db of
    [(_rowId, row)] ->
      QuickCheck.conjoin
        [ rowEntry operator row QuickCheck.=== Just (resultKey, term),
          rehydrateTuple operator (rowResult row : rowChildren row) QuickCheck.=== Just (resultKey, term)
        ]
    rows ->
      QuickCheck.counterexample ("expected one encoded row, got " <> show rows) False
  where
    operator =
      extractOperator term
    db =
      insertTuple resultKey term (emptyDatabase :: Database TestTerm Int)

propInsertIdempotenceUnionClosure :: CommitClosureCase -> QuickCheck.Property
propInsertIdempotenceUnionClosure (CommitClosureCase baseEntries insertedEntries) =
  QuickCheck.counterexample counterexampleText $
    QuickCheck.conjoin
      [ entrySet committedDb QuickCheck.=== expectedCommittedEntries,
        unionResultSet residualCommandList QuickCheck.=== expectedResidualPairs,
        entrySet duplicateCommittedDb QuickCheck.=== entrySet committedDb
      ]
  where
    baseDb =
      databaseFromEntries baseEntries
    commands =
      fmap (uncurry InsertTerm) insertedEntries
    duplicateCommands =
      commands <> commands
    commitResult =
      commitTermCommands commands baseDb
    duplicateCommitResult =
      commitTermCommands duplicateCommands baseDb
    residualCommandList =
      residualCommands commitResult
    committedDb =
      committedDatabase commitResult
    duplicateCommittedDb =
      committedDatabase duplicateCommitResult
    expectedCommittedEntries =
      Set.union (entrySet baseDb) (Set.fromList insertedEntries)
    expectedResidualPairs =
      expectedInsertClosurePairs baseDb insertedEntries
    counterexampleText =
      "base: "
        <> show (entryList baseDb)
        <> "\ninserted: "
        <> show insertedEntries
        <> "\nresidual: "
        <> show (termCommandsText residualCommandList)
        <> "\nexpected residual pairs: "
        <> show expectedResidualPairs

propCommitInsertFrontier :: CommitClosureCase -> QuickCheck.Property
propCommitInsertFrontier (CommitClosureCase baseEntries candidateEntries) =
  QuickCheck.counterexample counterexampleText $
    QuickCheck.conjoin
      [ QuickCheck.counterexample "frontier row ids must be unique and ascending per operator" $
          QuickCheck.property (all frontierIdsUniqueAndAscending frontierByOperator),
        QuickCheck.counterexample "every frontier row must occur at the same id in the committed snapshot" $
          QuickCheck.property (all (frontierRowsPresent committedRowsByOperator) (Map.toAscList frontierByOperator)),
        QuickCheck.counterexample "frontier rows must all rehydrate" $
          QuickCheck.property (maybe False (const True) frontierEntries),
        frontierEntries QuickCheck.=== Just semanticEntryIncrease
      ]
  where
    baseDatabase =
      databaseFromEntries baseEntries
    commitResult =
      commitTermCommands (fmap (uncurry InsertTerm) candidateEntries) baseDatabase
    committedSnapshot =
      committedDatabase commitResult
    frontierByOperator =
      insertedRows commitResult
    committedRowsByOperator =
      operatorRows committedSnapshot
    frontierEntries =
      fmap (Set.fromList . concat) $
        traverse
          (\(operator, rows) -> traverse (rowEntry operator . snd) rows)
          (Map.toAscList frontierByOperator)
    semanticEntryIncrease =
      Set.difference (entrySet committedSnapshot) (entrySet baseDatabase)
    counterexampleText =
      "base entries: "
        <> show (entryList baseDatabase)
        <> "\ncandidate entries: "
        <> show candidateEntries
        <> "\nfrontier: "
        <> show (fmap (fmap fst) frontierByOperator)
        <> "\nsemantic increase: "
        <> show semanticEntryIncrease

frontierIdsUniqueAndAscending :: Ord rowId => [(rowId, row)] -> Bool
frontierIdsUniqueAndAscending rows =
  rowIds == sort rowIds
    && Set.size (Set.fromList rowIds) == length rowIds
  where
    rowIds =
      fmap fst rows

frontierRowsPresent ::
  (Ord operator, Ord rowId, Eq row) =>
  Map operator [(rowId, row)] ->
  (operator, [(rowId, row)]) ->
  Bool
frontierRowsPresent committedRowsByOperator (operator, frontierRows) =
  all
    (\(rowId, row) -> Map.lookup rowId committedRows == Just row)
    frontierRows
  where
    committedRows =
      Map.fromList (Map.findWithDefault [] operator committedRowsByOperator)

propBulkEqualsRepeated :: BulkInsertCase -> QuickCheck.Property
propBulkEqualsRepeated (BulkInsertCase baseEntries insertedEntries) =
  operatorEntrySets bulkDb QuickCheck.=== operatorEntrySets repeatedDb
  where
    baseDb =
      databaseFromEntries baseEntries
    bulkDb =
      insertTuples insertedEntries baseDb
    repeatedDb =
      foldl'
        (\database (resultKey, term) -> insertTuple resultKey term database)
        baseDb
        insertedEntries

propCompactionFixpoint :: CompactionCase -> QuickCheck.Property
propCompactionFixpoint (CompactionCase baseEntries deletedEntries) =
  QuickCheck.conjoin
    [ entrySet compactedDb QuickCheck.=== entrySet deletedDb,
      operatorRows compactedAgainDb QuickCheck.=== operatorRows compactedDb
    ]
  where
    baseDb =
      databaseFromEntries baseEntries
    deletedDb =
      foldl'
        (\database (resultKey, term) -> deleteTuple resultKey term database)
        baseDb
        deletedEntries
    compactedDb =
      compact deletedDb
    compactedAgainDb =
      compact compactedDb

propCanonicalizationMorphism :: CanonicalizationCase -> QuickCheck.Property
propCanonicalizationMorphism (CanonicalizationCase entries dirtyKeys canonicalizer) =
  QuickCheck.counterexample counterexampleText $
    QuickCheck.conjoin
      [ canonicalizationOutcome,
        deltaInsertedEntries delta QuickCheck.=== committedNewEntries
      ]
  where
    canonicalize =
      applyCanonicalizer canonicalizer
    db =
      databaseFromEntries entries
    canonicalOperatorEntrySets =
      fmap (Set.map (canonicalizeEntry canonicalize)) (operatorEntrySets db)
    canonicalizationOutcome =
      case canonicalizeDatabase canonicalize db of
        Left _residualCommands ->
          QuickCheck.property True
        Right fullyCanonicalDb ->
          operatorEntrySets fullyCanonicalDb QuickCheck.=== canonicalOperatorEntrySets
    (delta, _commands, committedDb) =
      canonicalizeDirtyRows dirtyKeys canonicalize db
    entriesAfterDeletion =
      Set.difference (entrySet db) (Set.fromList (deltaDeletedEntries delta))
    committedNewEntries =
      sort (Set.toList (Set.difference (entrySet committedDb) entriesAfterDeletion))
    counterexampleText =
      "entries: "
        <> show (entryList db)
        <> "\ndirty keys: "
        <> show dirtyKeys
        <> "\ncanonicalizer: "
        <> show canonicalizer
        <> "\ndelta inserted: "
        <> show (deltaInsertedEntries delta)
        <> "\ncommitted entries: "
        <> show (entryList committedDb)

propFreeJoinNaiveEnumeration :: FreeJoinCase -> QuickCheck.Property
propFreeJoinNaiveEnumeration (FreeJoinCase entries plan) =
  case Database.freeJoin plan db of
    Left validationError ->
      QuickCheck.counterexample (counterexampleText Set.empty <> "\nvalidation error: " <> show validationError) False
    Right (bindings, _arrangedDb) ->
      let producedBindings =
            bindingSet bindings
       in QuickCheck.counterexample (counterexampleText producedBindings) $
            QuickCheck.conjoin
              [ QuickCheck.counterexample "freeJoin produced a binding outside naive enumeration" $
                  producedBindings `Set.isSubsetOf` expectedBindings,
                producedBindings QuickCheck.=== expectedBindings
              ]
  where
    db =
      databaseFromEntries entries
    expectedBindings =
      bindingSet (naiveFreeJoinEntries plan entries)
    counterexampleText producedBindings =
      "entries: "
        <> show (entryList db)
        <> "\nplan: "
        <> showFreeJoinPlan plan
        <> "\nproduced: "
        <> show producedBindings
        <> "\nexpected: "
        <> show expectedBindings

propWatermarkDeferredIndexInterleaving :: WatermarkInterleavingCase -> QuickCheck.Property
propWatermarkDeferredIndexInterleaving (WatermarkInterleavingCase actions) =
  case foldM applyTermDatabaseAction emptyInterleavingState actions >>= validateFinalInterleavingState of
    Left mismatch ->
      QuickCheck.counterexample
        ( "actions: "
            <> show actions
            <> "\nmismatch: "
            <> show mismatch
        )
        False
    Right () ->
      QuickCheck.property True

emptyInterleavingState :: InterleavingState
emptyInterleavingState =
  InterleavingState
    { interleavingDatabase = emptyDatabase,
      interleavingReference = ReferenceTermTable Map.empty
    }

applyTermDatabaseAction ::
  InterleavingState ->
  TermDatabaseAction ->
  Either InterleavingMismatch InterleavingState
applyTermDatabaseAction state action =
  case action of
    InsertSingle entry@(resultKey, term) ->
      Right
        state
          { interleavingDatabase = insertTuple resultKey term (interleavingDatabase state),
            interleavingReference = insertReferenceEntry entry (interleavingReference state)
          }
    InsertBatch entries ->
      Right
        state
          { interleavingDatabase = insertTuples entries (interleavingDatabase state),
            interleavingReference = insertReferenceEntries entries (interleavingReference state)
          }
    DeleteExact entry@(resultKey, term) ->
      Right
        state
          { interleavingDatabase = deleteTuple resultKey term (interleavingDatabase state),
            interleavingReference = deleteReferenceEntry entry (interleavingReference state)
          }
    CompactDatabase ->
      Right
        state
          { interleavingDatabase = compact (interleavingDatabase state)
          }
    QueryEntriesForResultKey resultKey ->
      let expectedEntries =
            referenceEntriesForResultKey resultKey (interleavingReference state)
          actualEntries =
            Set.fromList (entriesForResultKey resultKey (interleavingDatabase state))
       in if actualEntries == expectedEntries
            then Right state
            else Left (ResultEntriesMismatch resultKey expectedEntries actualEntries)
    QueryTupleAll term ->
      let expectedOwners =
            referenceTupleOwners term (interleavingReference state)
          actualOwners =
            tupleLookupOwners (lookupTupleAll term (interleavingDatabase state))
       in if actualOwners == expectedOwners
            then Right state
            else Left (TupleOwnersMismatch term expectedOwners actualOwners)
    QueryFixedFreeJoin ->
      case Database.freeJoin fixedTwoAtomFreeJoinPlan (interleavingDatabase state) of
        Left validationError ->
          Left (FixedFreeJoinValidationFailure validationError)
        Right (bindings, arrangedDatabase) ->
          let expectedBindings =
                bindingSet
                  (naiveFreeJoinEntries fixedTwoAtomFreeJoinPlan (referenceEntries (interleavingReference state)))
              actualBindings =
                bindingSet bindings
           in if actualBindings == expectedBindings
                then Right state {interleavingDatabase = arrangedDatabase}
                else Left (FixedFreeJoinBindingsMismatch expectedBindings actualBindings)

validateFinalInterleavingState :: InterleavingState -> Either InterleavingMismatch ()
validateFinalInterleavingState state =
  if actualEntries == expectedEntries
    then Right ()
    else Left (FinalEntriesMismatch expectedEntries actualEntries)
  where
    expectedEntries =
      Set.fromList (referenceEntries (interleavingReference state))
    actualEntries =
      entrySet (interleavingDatabase state)

insertReferenceEntry :: (Int, TestTerm Int) -> ReferenceTermTable -> ReferenceTermTable
insertReferenceEntry (resultKey, term) (ReferenceTermTable ownersByTerm) =
  ReferenceTermTable
    (Map.insertWith Set.union term (Set.singleton resultKey) ownersByTerm)

insertReferenceEntries :: [(Int, TestTerm Int)] -> ReferenceTermTable -> ReferenceTermTable
insertReferenceEntries entries reference =
  foldl' (flip insertReferenceEntry) reference entries

deleteReferenceEntry :: (Int, TestTerm Int) -> ReferenceTermTable -> ReferenceTermTable
deleteReferenceEntry (resultKey, term) (ReferenceTermTable ownersByTerm) =
  ReferenceTermTable (Map.update deleteOwner term ownersByTerm)
  where
    deleteOwner owners =
      case Set.delete resultKey owners of
        remainingOwners
          | Set.null remainingOwners -> Nothing
          | otherwise -> Just remainingOwners

referenceEntries :: ReferenceTermTable -> [(Int, TestTerm Int)]
referenceEntries (ReferenceTermTable ownersByTerm) =
  Map.foldMapWithKey
    (\term owners -> fmap (\resultKey -> (resultKey, term)) (Set.toAscList owners))
    ownersByTerm

referenceEntriesForResultKey :: Int -> ReferenceTermTable -> Set (Int, TestTerm Int)
referenceEntriesForResultKey resultKey =
  Set.fromList . filter ((== resultKey) . fst) . referenceEntries

referenceTupleOwners :: TestTerm Int -> ReferenceTermTable -> Set Int
referenceTupleOwners term (ReferenceTermTable ownersByTerm) =
  Map.findWithDefault Set.empty term ownersByTerm

tupleLookupOwners :: TupleLookup Int -> Set Int
tupleLookupOwners tupleLookup =
  case tupleLookup of
    TupleMissing -> Set.empty
    TupleUnique resultKey -> Set.singleton resultKey
    TupleAmbiguous resultKeys -> Set.fromList (toList resultKeys)

fixedTwoAtomFreeJoinPlan :: FreeJoinPlan TestTerm Int
fixedTwoAtomFreeJoinPlan =
  FreeJoinPlan
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
  where
    binaryOperator =
      operatorFromArity 2

interleavingActionGen :: QuickCheck.Gen TermDatabaseAction
interleavingActionGen =
  QuickCheck.frequency
    [ (4, InsertSingle <$> interleavingEntryGen),
      (2, InsertBatch <$> interleavingEntryListGen 5),
      (3, DeleteExact <$> interleavingEntryGen),
      (1, pure CompactDatabase),
      (2, QueryEntriesForResultKey <$> interleavingKeyGen),
      (2, QueryTupleAll <$> interleavingTermGen),
      (2, pure QueryFixedFreeJoin)
    ]

interleavingEntryGen :: QuickCheck.Gen (Int, TestTerm Int)
interleavingEntryGen =
  (,) <$> interleavingKeyGen <*> interleavingTermGen

interleavingEntryListGen :: Int -> QuickCheck.Gen [(Int, TestTerm Int)]
interleavingEntryListGen upperBound = do
  entryCount <- QuickCheck.chooseInt (1, upperBound)
  QuickCheck.vectorOf entryCount interleavingEntryGen

interleavingKeyGen :: QuickCheck.Gen Int
interleavingKeyGen =
  QuickCheck.chooseInt (0, 7)

interleavingTermGen :: QuickCheck.Gen (TestTerm Int)
interleavingTermGen = do
  arity <- QuickCheck.chooseInt (0, 4)
  termFromArity arity <$> QuickCheck.vectorOf arity interleavingKeyGen

entryGen :: QuickCheck.Gen (Int, TestTerm Int)
entryGen =
  (,) <$> keyGen <*> termGen 4

entryListGen :: Int -> QuickCheck.Gen [(Int, TestTerm Int)]
entryListGen upperBound = do
  entryCount <- QuickCheck.chooseInt (0, upperBound)
  QuickCheck.vectorOf entryCount entryGen

keyGen :: QuickCheck.Gen Int
keyGen =
  QuickCheck.chooseInt (0, 15)

termGen :: Int -> QuickCheck.Gen (TestTerm Int)
termGen maximumArity = do
  arity <- QuickCheck.chooseInt (0, maximumArity)
  termFromArity arity <$> QuickCheck.vectorOf arity keyGen

termFromArity :: Int -> [Int] -> TestTerm Int
termFromArity arity children =
  case (arity, children) of
    (0, []) ->
      TNull
    (1, [child]) ->
      TUnary child
    (2, [leftChild, rightChild]) ->
      TBinary leftChild rightChild
    _ ->
      TNary children

operatorFromArity :: Int -> Operator TestTerm
operatorFromArity arity =
  extractOperator (termFromArity arity (replicate arity 0))

freeJoinPlanGen :: QuickCheck.Gen (FreeJoinPlan TestTerm Int)
freeJoinPlanGen = do
  atomCount <- QuickCheck.chooseInt (0, 3)
  FreeJoinPlan <$> QuickCheck.vectorOf atomCount queryAtomGen

queryAtomGen :: QuickCheck.Gen (QueryAtom TestTerm Int)
queryAtomGen = do
  arity <- QuickCheck.chooseInt (0, 3)
  QueryAtom
    <$> pure (operatorFromArity arity)
    <*> queryTermGen
    <*> QuickCheck.vectorOf arity queryTermGen

queryTermGen :: QuickCheck.Gen (QueryTerm Int)
queryTermGen =
  QuickCheck.frequency
    [ (3, QueryVariable . ExplicitQueryVar <$> QuickCheck.chooseInt (0, 4)),
      (2, QueryBound <$> keyGen)
    ]

dirtyKeySetGen :: [(Int, TestTerm Int)] -> QuickCheck.Gen IntSet
dirtyKeySetGen entries =
  case Set.toList (entryKeys entries) of
    [] ->
      pure IntSet.empty
    keys ->
      IntSet.fromList <$> QuickCheck.sublistOf keys

entryKeys :: [(Int, TestTerm Int)] -> Set Int
entryKeys =
  foldMap (\(resultKey, term) -> Set.fromList (resultKey : toList term))

applyCanonicalizer :: KeyCanonicalizer -> Int -> Int
applyCanonicalizer (KeyCanonicalizer modulus) key =
  key `mod` modulus

canonicalizeEntry :: (Int -> Int) -> (Int, TestTerm Int) -> (Int, TestTerm Int)
canonicalizeEntry canonicalize (resultKey, term) =
  (canonicalize resultKey, fmap canonicalize term)

databaseFromEntries :: [(Int, TestTerm Int)] -> Database TestTerm Int
databaseFromEntries entries =
  insertTuples entries emptyDatabase

entrySet :: Database TestTerm Int -> Set (Int, TestTerm Int)
entrySet =
  Set.fromList . databaseEntries

entryList :: Database TestTerm Int -> [(Int, TestTerm Int)]
entryList =
  sort . databaseEntries

operatorEntrySets :: Database TestTerm Int -> Map (Operator TestTerm) (Set (Int, TestTerm Int))
operatorEntrySets =
  Map.mapWithKey (\operator rows -> Set.fromList (rowEntries operator rows)) . operatorRows

rowEntries :: Operator TestTerm -> [(rowId, DatabaseRow)] -> [(Int, TestTerm Int)]
rowEntries operator =
  mapMaybe (rowEntry operator . snd)

deltaDeletedEntries :: DatabaseRowDelta TestTerm -> [(Int, TestTerm Int)]
deltaDeletedEntries =
  sort . deltaEntries . rowsDeleted

deltaInsertedEntries :: DatabaseRowDelta TestTerm -> [(Int, TestTerm Int)]
deltaInsertedEntries =
  sort . deltaEntries . rowsInserted

deltaEntries :: Map (Operator TestTerm) [(rowId, DatabaseRow)] -> [(Int, TestTerm Int)]
deltaEntries =
  Map.foldMapWithKey rowEntries

expectedInsertClosurePairs ::
  Database TestTerm Int ->
  [(Int, TestTerm Int)] ->
  Set (Int, Int)
expectedInsertClosurePairs baseDb insertedEntries =
  Map.foldlWithKey' collectTuplePairs Set.empty insertedOwnersByTuple
  where
    baseOwnersByTuple =
      tupleOwners (databaseEntries baseDb)
    insertedOwnersByTuple =
      tupleOwners insertedEntries
    collectTuplePairs pairs term insertedOwners =
      Set.union pairs $
        Set.filter
          (pairTouches insertedOwners)
          (ownerPairs (Set.union insertedOwners (Map.findWithDefault Set.empty term baseOwnersByTuple)))

tupleOwners :: [(Int, TestTerm Int)] -> Map (TestTerm Int) (Set Int)
tupleOwners =
  foldl'
    (\owners (resultKey, term) -> Map.insertWith Set.union term (Set.singleton resultKey) owners)
    Map.empty

ownerPairs :: Set Int -> Set (Int, Int)
ownerPairs owners =
  Set.fromList
    [ (leftOwner, rightOwner)
      | leftOwner <- ownerList,
        rightOwner <- ownerList,
        leftOwner < rightOwner
    ]
  where
    ownerList =
      Set.toList owners

pairTouches :: Set Int -> (Int, Int) -> Bool
pairTouches owners (leftOwner, rightOwner) =
  Set.member leftOwner owners || Set.member rightOwner owners

unionResultSet :: [TermCommand TestTerm Int] -> Set (Int, Int)
unionResultSet =
  foldMap unionPair

unionPair :: TermCommand TestTerm Int -> Set (Int, Int)
unionPair command =
  case command of
    UnionResults left right ->
      maybe Set.empty Set.singleton (orderedDistinctPair left right)
    _ ->
      Set.empty

orderedDistinctPair :: Ord key => key -> key -> Maybe (key, key)
orderedDistinctPair left right
  | left == right =
      Nothing
  | left < right =
      Just (left, right)
  | otherwise =
      Just (right, left)

termCommandsText :: [TermCommand TestTerm Int] -> [String]
termCommandsText =
  fmap termCommandText

termCommandText :: TermCommand TestTerm Int -> String
termCommandText command =
  case command of
    DeleteRow operator rowId ->
      "DeleteRow " <> show operator <> " " <> show rowId
    InsertTerm resultKey term ->
      "InsertTerm " <> show resultKey <> " " <> show term
    UnionResults left right ->
      "UnionResults " <> show left <> " " <> show right

bindingSet :: [QueryBinding Int] -> Set (Map QueryVar Int)
bindingSet =
  Set.fromList . fmap queryBindingAssignments

naiveFreeJoinEntries :: FreeJoinPlan TestTerm Int -> [(Int, TestTerm Int)] -> [QueryBinding Int]
naiveFreeJoinEntries (FreeJoinPlan atoms) entries =
  foldl'
    (\bindings atom -> foldMap (bindNaiveAtom atom) bindings)
    [QueryBinding Map.empty]
    atoms
  where
    rowsByOperator =
      entriesByOperator entries
    bindNaiveAtom atom binding =
      mapMaybe (bindNaiveEntry atom binding) (Map.findWithDefault [] (atomOperator atom) rowsByOperator)

entriesByOperator :: [(Int, TestTerm Int)] -> Map (Operator TestTerm) [(Int, TestTerm Int)]
entriesByOperator =
  Map.fromListWith (<>)
    . fmap (\entry@(_resultKey, term) -> (extractOperator term, [entry]))

bindNaiveEntry :: QueryAtom TestTerm Int -> QueryBinding Int -> (Int, TestTerm Int) -> Maybe (QueryBinding Int)
bindNaiveEntry atom binding (resultKey, term)
  | extractOperator term == atomOperator atom =
      bindNaiveValues atom binding (resultKey : toList term)
  | otherwise =
      Nothing

bindNaiveValues :: QueryAtom TestTerm Int -> QueryBinding Int -> [Int] -> Maybe (QueryBinding Int)
bindNaiveValues atom binding values
  | length terms == length values =
      foldM bindQueryTerm binding (zip terms values)
  | otherwise =
      Nothing
  where
    terms =
      atomResult atom : atomChildren atom

bindQueryTerm :: QueryBinding Int -> (QueryTerm Int, Int) -> Maybe (QueryBinding Int)
bindQueryTerm binding (term, value) =
  case term of
    QueryBound expectedValue
      | expectedValue == value ->
          Just binding
      | otherwise ->
          Nothing
    QueryVariable variable ->
      bindQueryVariable variable value binding

bindQueryVariable :: QueryVar -> Int -> QueryBinding Int -> Maybe (QueryBinding Int)
bindQueryVariable variable value binding =
  case Map.lookup variable (queryBindingAssignments binding) of
    Nothing ->
      Just binding {queryBindingAssignments = Map.insert variable value (queryBindingAssignments binding)}
    Just existingValue
      | existingValue == value ->
          Just binding
      | otherwise ->
          Nothing

showFreeJoinPlan :: FreeJoinPlan TestTerm Int -> String
showFreeJoinPlan (FreeJoinPlan atoms) =
  "FreeJoinPlan " <> show (fmap showQueryAtom atoms)

showQueryAtom :: QueryAtom TestTerm Int -> String
showQueryAtom atom =
  "{operator="
    <> show (atomOperator atom)
    <> ", result="
    <> show (atomResult atom)
    <> ", children="
    <> show (atomChildren atom)
    <> "}"

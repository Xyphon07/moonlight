module UnionFindSpec
  ( tests,
  )
where

import Control.Monad
  ( foldM,
  )
import Control.Monad.ST
  ( ST,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.IntMap.Strict
  ( IntMap,
  )
import Data.IntMap.Strict qualified as IntMap
import Moonlight.Core
  ( ClassId (..),
  )
import Moonlight.Core
  ( UnionFind,
    UnionFindAllocationError (..),
  )
import Moonlight.Core qualified as UnionFind
import Moonlight.Core qualified as Transaction
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    assertEqual,
    assertFailure,
    testCase,
  )
import Test.Tasty.QuickCheck qualified as QuickCheck
import Prelude

data Operation
  = InsertClass !Int
  | FindClass !Int
  | FindExistingClass !Int
  | CanonicalClass !Int
  | ClassesEquivalent !Int !Int
  | UnionClasses !Int !Int
  | MakeSet
  | CompressCanonicalMap
  deriving stock (Eq, Show)

newtype OperationTrace = OperationTrace
  { unOperationTrace :: [Operation]
  }
  deriving stock (Eq, Show)

data TraceSeed
  = EmptySeed
  | DensePrefixSeed
  | HoleyPrefixSeed
  | NegativeSparseSeed
  | GiantOutlierSeed
  | PrelinkedDenseSparseSeed
  deriving stock (Bounded, Enum, Eq, Show)

data Observation
  = ObservedUnit
  | ObservedClass !ClassId
  | ObservedMaybeClass !(Maybe ClassId)
  | ObservedAllocation !(Either UnionFindAllocationError ClassId)
  | ObservedBool !Bool
  | ObservedCanonicalMap ![(Int, ClassId)]
  deriving stock (Eq, Show)

instance QuickCheck.Arbitrary Operation where
  arbitrary =
    QuickCheck.frequency
      [ (4, InsertClass <$> arbitraryClassKey),
        (8, FindClass <$> arbitraryClassKey),
        (4, FindExistingClass <$> arbitraryClassKey),
        (4, CanonicalClass <$> arbitraryClassKey),
        (4, ClassesEquivalent <$> arbitraryClassKey <*> arbitraryClassKey),
        (8, UnionClasses <$> arbitraryClassKey <*> arbitraryClassKey),
        (1, pure MakeSet),
        (1, pure CompressCanonicalMap)
      ]

  shrink operation =
    case operation of
      InsertClass key ->
        InsertClass <$> QuickCheck.shrink key
      FindClass key ->
        FindClass <$> QuickCheck.shrink key
      FindExistingClass key ->
        FindExistingClass <$> QuickCheck.shrink key
      CanonicalClass key ->
        CanonicalClass <$> QuickCheck.shrink key
      ClassesEquivalent leftKey rightKey ->
        uncurry ClassesEquivalent <$> QuickCheck.shrink (leftKey, rightKey)
      UnionClasses leftKey rightKey ->
        uncurry UnionClasses <$> QuickCheck.shrink (leftKey, rightKey)
      MakeSet ->
        []
      CompressCanonicalMap ->
        []

instance QuickCheck.Arbitrary OperationTrace where
  arbitrary =
    QuickCheck.sized $ \size -> do
      operationCount <-
        QuickCheck.chooseInt
          (0, min 200 (2 * size + 1))
      OperationTrace
        <$> QuickCheck.vectorOf
          operationCount
          QuickCheck.arbitrary

  shrink (OperationTrace operations) =
    OperationTrace <$> QuickCheck.shrinkList QuickCheck.shrink operations

instance QuickCheck.Arbitrary TraceSeed where
  arbitrary =
    QuickCheck.elements [minBound .. maxBound]

tests :: TestTree
tests =
  testGroup
    "union-find"
    [ QuickCheck.testProperty
        "transaction exactly matches persistent operation traces"
        transactionMatchesPersistent,
      testCase
        "equal-rank union chooses the smaller representative"
        equalRankRepresentativeIsDeterministic,
      testCase
        "transactional union reports named merge roots"
        transactionalUnionReportsMergeRoots,
      testCase
        "prefix and giant sparse outlier interoperate"
        prefixOutlierCrossLink,
      testCase
        "canonical compression is exact and idempotent"
        canonicalCompressionIsIdempotent,
      testCase
        "samePartition ignores representative history"
        samePartitionIgnoresRepresentativeHistory,
      testCase
        "an aborted transaction publishes no snapshot"
        abortedTransactionDoesNotCommit,
      testCase
        "union inserts absent class identifiers"
        unionInsertsAbsentClassIdentifiers,
      testCase
        "makeSet does not overwrite relationships introduced by union"
        makeSetPreservesUnionIntroducedRelationships,
      testCase
        "persistent allocation exhaustion preserves the partition"
        persistentAllocationExhaustionPreservesPartition,
      testCase
        "transactional allocation exhaustion preserves the partition"
        transactionalAllocationExhaustionPreservesPartition,
      testCase
        "allocation starts at zero and ignores negative sparse ids"
        ordinaryAllocationSequenceIsStable
    ]

transactionMatchesPersistent ::
  TraceSeed ->
  OperationTrace ->
  QuickCheck.Property
transactionMatchesPersistent seed trace =
  let persistentResult =
        runPersistentTraceFrom (unionFindSeed seed) trace
      transactionResult =
        runTransactionTraceFrom (unionFindSeed seed) trace
   in QuickCheck.counterexample
        ( "seed: "
            <> show seed
            <> "\npersistent: "
            <> show persistentResult
            <> "\ntransaction: "
            <> show transactionResult
        )
        (sameTraceResult persistentResult transactionResult)

runPersistentTraceFrom ::
  UnionFind ->
  OperationTrace ->
  ([Observation], UnionFind)
runPersistentTraceFrom base (OperationTrace operations) =
  let (reverseObservations, finalUnionFind) =
        foldl'
          persistentStep
          ([], base)
          operations
   in (reverse reverseObservations, finalUnionFind)

persistentStep ::
  ([Observation], UnionFind) ->
  Operation ->
  ([Observation], UnionFind)
persistentStep (observations, unionFind) operation =
  case operation of
    InsertClass key ->
      ( ObservedUnit : observations,
        UnionFind.insertClassId (ClassId key) unionFind
      )
    FindClass key ->
      let (rootClass, unionFind') =
            UnionFind.find (ClassId key) unionFind
       in ( ObservedClass rootClass : observations,
            unionFind'
          )
    FindExistingClass key ->
      case UnionFind.findExisting (ClassId key) unionFind of
        Nothing ->
          (ObservedMaybeClass Nothing : observations, unionFind)
        Just (rootClass, unionFind') ->
          (ObservedMaybeClass (Just rootClass) : observations, unionFind')
    CanonicalClass key ->
      ( ObservedMaybeClass (UnionFind.canonicalClass (ClassId key) unionFind) : observations,
        unionFind
      )
    ClassesEquivalent leftKey rightKey ->
      ( ObservedBool (UnionFind.equivalent (ClassId leftKey) (ClassId rightKey) unionFind) : observations,
        unionFind
      )
    UnionClasses leftKey rightKey ->
      ( ObservedUnit : observations,
        UnionFind.union (ClassId leftKey) (ClassId rightKey) unionFind
      )
    MakeSet ->
      case UnionFind.makeSet unionFind of
        Left allocationError ->
          (ObservedAllocation (Left allocationError) : observations, unionFind)
        Right (classId, unionFind') ->
          (ObservedAllocation (Right classId) : observations, unionFind')
    CompressCanonicalMap ->
      let (parents, unionFind') =
            UnionFind.canonicalMapAndCompress unionFind
       in (observeCanonicalMap parents : observations, unionFind')

runTransactionTraceFrom ::
  UnionFind ->
  OperationTrace ->
  ([Observation], UnionFind)
runTransactionTraceFrom base (OperationTrace operations) =
  let (reverseObservations, finalUnionFind) =
        Transaction.runUnionFindTransaction
          base
          (\editor -> foldM (transactionStep editor) [] operations)
   in (reverse reverseObservations, finalUnionFind)

transactionStep ::
  Transaction.UnionFindEditor state ->
  [Observation] ->
  Operation ->
  ST state [Observation]
transactionStep editor observations operation =
  case operation of
    InsertClass key -> do
      Transaction.transactionInsertClassId editor (ClassId key)
      pure (ObservedUnit : observations)
    FindClass key -> do
      rootClass <- Transaction.transactionFind editor (ClassId key)
      pure (ObservedClass rootClass : observations)
    FindExistingClass key -> do
      maybeRoot <- Transaction.transactionFindExisting editor (ClassId key)
      pure (ObservedMaybeClass maybeRoot : observations)
    CanonicalClass key -> do
      maybeRoot <- Transaction.transactionCanonicalClass editor (ClassId key)
      pure (ObservedMaybeClass maybeRoot : observations)
    ClassesEquivalent leftKey rightKey -> do
      result <- Transaction.transactionEquivalent editor (ClassId leftKey) (ClassId rightKey)
      pure (ObservedBool result : observations)
    UnionClasses leftKey rightKey -> do
      _ <- Transaction.transactionUnion editor (ClassId leftKey) (ClassId rightKey)
      pure (ObservedUnit : observations)
    MakeSet -> do
      allocation <- Transaction.transactionMakeSet editor
      pure (ObservedAllocation allocation : observations)
    CompressCanonicalMap -> do
      parents <- Transaction.transactionCanonicalMapAndCompress editor
      pure (observeCanonicalMap parents : observations)

observeCanonicalMap ::
  IntMap ClassId ->
  Observation
observeCanonicalMap =
  ObservedCanonicalMap . IntMap.toAscList

sameTraceResult ::
  ([Observation], UnionFind) ->
  ([Observation], UnionFind) ->
  Bool
sameTraceResult (leftObservations, leftUnionFind) (rightObservations, rightUnionFind) =
  leftObservations == rightObservations
    && UnionFind.samePartition leftUnionFind rightUnionFind

assertSameTraceResult ::
  String ->
  ([Observation], UnionFind) ->
  ([Observation], UnionFind) ->
  Assertion
assertSameTraceResult label (expectedObservations, expectedUnionFind) (actualObservations, actualUnionFind) = do
  assertEqual
    (label <> " observations")
    expectedObservations
    actualObservations
  assertBool
    (label <> " partition")
    (UnionFind.samePartition expectedUnionFind actualUnionFind)

arbitraryClassKey :: QuickCheck.Gen Int
arbitraryClassKey =
  QuickCheck.frequency
    [ (12, QuickCheck.chooseInt (-128, 1024)),
      (3, (* 17) <$> QuickCheck.chooseInt (-4096, 4096)),
      ( 1,
        QuickCheck.elements
          [ minBound,
            -1000000000,
            1000000000,
            maxBound
          ]
      )
    ]

unionFindSeed :: TraceSeed -> UnionFind
unionFindSeed seed =
  case seed of
    EmptySeed ->
      UnionFind.emptyUnionFind
    DensePrefixSeed ->
      UnionFind.fromClassIds (ClassId <$> [0 .. 127])
    HoleyPrefixSeed ->
      UnionFind.fromClassIds (ClassId <$> [0, 2 .. 254])
    NegativeSparseSeed ->
      UnionFind.fromClassIds (ClassId . negate <$> [1 .. 128])
    GiantOutlierSeed ->
      UnionFind.fromClassIds (ClassId <$> ([0 .. 63] <> [1000000000]))
    PrelinkedDenseSparseSeed ->
      UnionFind.union
        (ClassId 1000000000)
        (ClassId 0)
        (UnionFind.fromClassIds (ClassId <$> ([0 .. 127] <> [-7])))

equalRankRepresentativeIsDeterministic :: Assertion
equalRankRepresentativeIsDeterministic = do
  let unionFind =
        UnionFind.union
          (ClassId 9)
          (ClassId 3)
          UnionFind.emptyUnionFind
  assertEqual
    "canonical roots"
    (Just (ClassId 3), Just (ClassId 3))
    ( UnionFind.canonicalClass (ClassId 9) unionFind,
      UnionFind.canonicalClass (ClassId 3) unionFind
    )

transactionalUnionReportsMergeRoots :: Assertion
transactionalUnionReportsMergeRoots = do
  let (outcomes, committedUnionFind) =
        Transaction.runUnionFindTransaction UnionFind.emptyUnionFind $ \editor -> do
          firstUnion <- Transaction.transactionUnion editor (ClassId 9) (ClassId 4)
          secondUnion <- Transaction.transactionUnion editor (ClassId 9) (ClassId 4)
          pure (firstUnion, secondUnion)
  assertEqual
    "union outcome"
    (Transaction.MergedClasses (ClassId 4) (ClassId 9), Transaction.AlreadyEquivalent (ClassId 4))
    outcomes
  assertBool
    "committed partition"
    ( UnionFind.samePartition
        (UnionFind.union (ClassId 9) (ClassId 4) UnionFind.emptyUnionFind)
        committedUnionFind
    )

prefixOutlierCrossLink :: Assertion
prefixOutlierCrossLink = do
  let outlier =
        1000000000
      operations =
        OperationTrace
          ( fmap InsertClass [0 .. 255]
              <> [ InsertClass outlier,
                   UnionClasses outlier 0,
                   FindClass outlier,
                   FindClass 0,
                   CompressCanonicalMap
                 ]
          )
  assertSameTraceResult
    "transaction preserves observations and partition"
    (runPersistentTraceFrom UnionFind.emptyUnionFind operations)
    (runTransactionTraceFrom UnionFind.emptyUnionFind operations)

canonicalCompressionIsIdempotent :: Assertion
canonicalCompressionIsIdempotent = do
  let pairs =
        [ (ClassId leftKey, ClassId (leftKey + stride))
        | stride <- takeWhile (< 256) (iterate (* 2) 1),
          leftKey <- [0, 2 * stride .. 255 - stride]
        ]
      unionFind =
        foldl'
          (\current (leftClass, rightClass) -> UnionFind.union leftClass rightClass current)
          UnionFind.emptyUnionFind
          pairs
      (parents, compressed) =
        UnionFind.canonicalMapAndCompress unionFind
      (parentsAgain, compressedAgain) =
        UnionFind.canonicalMapAndCompress compressed
  assertEqual
    "second canonical map"
    parents
    parentsAgain
  assertBool
    "second compression is a semantic fixed point"
    (UnionFind.samePartition compressed compressedAgain)
  traverse_
    (\(key, rootClass) ->
       assertEqual
        ("compressed root for key " <> show key)
        (Just rootClass)
        (UnionFind.canonicalClass (ClassId key) compressed)
    )
    (IntMap.toAscList parents)

samePartitionIgnoresRepresentativeHistory :: Assertion
samePartitionIgnoresRepresentativeHistory = do
  let leftUnionFind =
        UnionFind.union (ClassId 0) (ClassId 10) $
          UnionFind.union (ClassId 10) (ClassId 11) UnionFind.emptyUnionFind
      rightUnionFind =
        UnionFind.union (ClassId 10) (ClassId 11) $
          UnionFind.union (ClassId 0) (ClassId 10) UnionFind.emptyUnionFind
  assertBool
    "representative histories differ"
    ( UnionFind.canonicalClass (ClassId 0) leftUnionFind
        /= UnionFind.canonicalClass (ClassId 0) rightUnionFind
    )
  assertBool
    "partitions agree"
    (UnionFind.samePartition leftUnionFind rightUnionFind)

abortedTransactionDoesNotCommit :: Assertion
abortedTransactionDoesNotCommit = do
  let base =
        UnionFind.fromClassIds
          [ClassId 0, ClassId 1]
      outcome =
        Transaction.runUnionFindTransactionEither base $ \editor -> do
          _ <- Transaction.transactionUnion editor (ClassId 0) (ClassId 1)
          pure (Left "abort" :: Either String ())
  case outcome of
    Left "abort" ->
      pure ()
    _ ->
      assertFailure ("transaction outcome changed: " <> show outcome)
  assertBool
    "base snapshot remains unchanged"
    (not (UnionFind.equivalent (ClassId 0) (ClassId 1) base))

unionInsertsAbsentClassIdentifiers :: Assertion
unionInsertsAbsentClassIdentifiers = do
  let leftClass = ClassId 4
      rightClass = ClassId 9
      unionFind = UnionFind.union leftClass rightClass UnionFind.emptyUnionFind
  assertEqual
    "absent union is total"
    ( True,
      True,
      True,
      Just leftClass,
      Just leftClass
    )
    ( UnionFind.member leftClass unionFind,
      UnionFind.member rightClass unionFind,
      UnionFind.equivalent leftClass rightClass unionFind,
      UnionFind.canonicalClass leftClass unionFind,
      UnionFind.canonicalClass rightClass unionFind
    )

makeSetPreservesUnionIntroducedRelationships :: Assertion
makeSetPreservesUnionIntroducedRelationships = do
  let unionFind = UnionFind.union (ClassId 2) (ClassId 3) UnionFind.emptyUnionFind
  (freshClass, grownUnionFind) <- expectRight (UnionFind.makeSet unionFind)
  assertEqual
    "high-water makeSet is above inserted ids"
    ( ClassId 4,
      True,
      Just (ClassId 2)
    )
    ( freshClass,
      UnionFind.equivalent (ClassId 2) (ClassId 3) grownUnionFind,
      IntMap.lookup 3 (UnionFind.canonicalMap grownUnionFind)
    )

persistentAllocationExhaustionPreservesPartition :: Assertion
persistentAllocationExhaustionPreservesPartition = do
  let unionFind = exhaustedUnionFind
      before = UnionFind.canonicalMap unionFind
  case UnionFind.makeSet unionFind of
    Left allocationError ->
      assertEqual "persistent allocation obstruction" ClassIdSpaceExhausted allocationError
    Right unexpectedAllocation ->
      assertFailure ("persistent exhaustion allocated: " <> show (fst unexpectedAllocation))
  assertEqual "persistent canonical map" before (UnionFind.canonicalMap unionFind)
  assertBool
    "persistent equivalence survives exhaustion"
    (UnionFind.equivalent (ClassId (maxBound - 1)) (ClassId maxBound) unionFind)

transactionalAllocationExhaustionPreservesPartition :: Assertion
transactionalAllocationExhaustionPreservesPartition = do
  let before = UnionFind.canonicalMap exhaustedUnionFind
      ((allocation, observedMap), committedUnionFind) =
        Transaction.runUnionFindTransaction exhaustedUnionFind $ \editor -> do
          editorAllocation <- Transaction.transactionMakeSet editor
          editorObservedMap <- Transaction.transactionCanonicalMapAndCompress editor
          pure (editorAllocation, editorObservedMap)
  assertEqual "transaction allocation obstruction" (Left ClassIdSpaceExhausted) allocation
  assertEqual "transaction canonical map" before observedMap
  assertEqual "committed canonical map" before (UnionFind.canonicalMap committedUnionFind)
  assertBool
    "transaction equivalence survives exhaustion"
    (UnionFind.equivalent (ClassId (maxBound - 1)) (ClassId maxBound) committedUnionFind)

ordinaryAllocationSequenceIsStable :: Assertion
ordinaryAllocationSequenceIsStable = do
  let negativeOnly = UnionFind.insertClassId (ClassId (-1)) UnionFind.emptyUnionFind
  (class0, unionFind1) <- expectRight (UnionFind.makeSet negativeOnly)
  (class1, unionFind2) <- expectRight (UnionFind.makeSet unionFind1)
  (class2, _unionFind3) <- expectRight (UnionFind.makeSet unionFind2)
  assertEqual "ordinary allocation sequence" [ClassId 0, ClassId 1, ClassId 2] [class0, class1, class2]

exhaustedUnionFind :: UnionFind
exhaustedUnionFind =
  UnionFind.union
    (ClassId (maxBound - 1))
    (ClassId maxBound)
    UnionFind.emptyUnionFind

expectRight :: Show errorValue => Either errorValue value -> IO value
expectRight result =
  case result of
    Left failure ->
      assertFailure ("unexpected Left: " <> show failure)
    Right value ->
      pure value

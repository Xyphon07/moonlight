{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module DeltaHashSpec
  ( deltaHashTests,
  )
where

import Control.Exception (TypeError, evaluate, try)
import Control.Monad (foldM, void)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import DeltaHashGoldenOracle
  ( OracleDigest (..),
    oracleEmptyNodeDigest,
    oracleFlatDigest,
    oracleMultisetDigest,
    oracleSipHashReferenceVectors,
    oracleTrieDigest,
  )
import DeltaHashTypeErrors
  ( crossStrategyDigestComparison,
  )
import Moonlight.Algebra.Pure.LaneVector
  ( laneCount,
    laneVectorFromLanes,
    laneVectorLanes,
    laneVectorZero,
  )
import Moonlight.Core
  ( AdditiveGroup (sub),
    AdditiveMonoid (add),
    StableHashEncoding,
    stableHashEncodingWord8,
    stableHashEncodingWord64LE,
  )
import Moonlight.Delta.Patch
  ( ApplyError (..),
    DeltaHashApplyError (..),
    DeltaHashBuildError (..),
    DeltaHashComposeError (..),
    DeltaHashDigestDecodeError (..),
    DeltaHashEncoderVersion (..),
    MerkleDeltaHash,
    MerkleDeltaHashDigest,
    MultisetDeltaHash,
    MultisetDeltaHashDigest,
  )
import Moonlight.Delta.Patch qualified as Patch
import Moonlight.Delta.Patch.Internal.IncrementalDigest
  ( DeltaHashDigestLanes (..),
    DigestStrategy (..),
    RawDigest128 (..),
    deltaHashDigestLanes,
  )
import Moonlight.Delta.Patch.Internal.NodeCommitment qualified as Node
import Moonlight.Delta.Patch.Internal.Types (CodecStats (..), debugCodecStats)
import Prelude
import Test.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    counterexample,
    forAll,
    listOf,
    vectorOf,
    withNumTests,
    (.&&.),
    (===),
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

deltaHashTests :: TestTree
deltaHashTests =
  testGroup
    "delta hash"
    [ testProperty "Merkle flat patch agrees with rebuild" merklePatchAgreesWithRebuild,
      testProperty "Merkle edit history descends to one root" merkleHistoryDescendsToCanonicalRoot,
      testProperty "Merkle trie patch agrees with rebuild above the flat boundary" merkleTriePatchAgreesWithRebuild,
      testCase "Merkle large multi-row path reassignment agrees with rebuild" largeMultiRowRebuildCase,
      testProperty "Merkle composed action equals sequential action" merkleComposedEqualsSequential,
      testProperty "contextual composition associates on values, definedness, and underlying failure" contextualCompositionAssociativity,
      testCase "reassociation exposes the documented binary-local stage divergence" contextualCompositionStageDivergenceCase,
      testProperty "multiset patch agrees with rebuild" multisetPatchAgreesWithRebuild,
      testProperty "multiset composed action equals sequential action" multisetComposedEqualsSequential,
      testProperty "multiset disjoint union composes through the patch action" multisetDisjointUnionHomomorphism,
      testProperty "multiset insertion and deletion are additive inverses" multisetAdditiveInverse,
      testProperty "multiset disjoint edit order is history-independent" multisetOrderIndependence,
      testCase "Merkle crosses the adaptive boundary in both directions" adaptiveBoundaryCase,
      testCase "Merkle digest is stable across the 63/64/65 and 255/256/257 boundaries" boundaryDigestCase,
      testCase "Merkle admits insertion into a path freed by a later deletion" pathFreedByDeletionCase,
      testCase "Merkle contextual composition rejects the insert-delete mirror at the first obstruction" mirrorPathCollisionCase,
      testCase "contextual composition preserves exact failure stage and cause under both strategies" contextualCompositionFailureRefinementCase,
      testCase "Merkle admits a multi-key path cycle in one patch" pathCycleCase,
      testCase "contextual composition preserves a same-path key representative under both strategies" samePathRepresentativeCompositionCase,
      testCase "empty patch is a digest identity in flat, trie, and multiset modes" emptyPatchIdentityCase,
      testCase "inverse composition returns to the original digest on both strategies" inverseCompositionCase,
      testCase "digesting replay's final map agrees with sequential digest replay" replayDigestAgreesCase,
      testCase "all four cell edits update both strategies" cellEditsCase,
      testCase "stale patch rejection is preserved on both strategies" stalePatchCase,
      testCase "equal key representatives rebuild on both strategies" representativeRewriteCase,
      testCase "equal value representatives rebuild on both strategies" valueRepresentativeRewriteCase,
      testCase "flat mode admits localized path collisions" flatModeCollisionCase,
      testCase "Merkle construction reports one localized path collision" constructionCollisionCase,
      testCase "Merkle insertion reports a localized path collision atomically" updateCollisionCase,
      testCase "digest action agrees with rebuild across mixed page encodings" mixedPageEncodingCase,
      testCase "non-injective value encoder collapses distinct states to one digest" multisetNonInjectiveCollapseCase,
      testCase "constructed additive cancellation matches the reduced state" multisetCancellationCase,
      testCase "modular additive inverse restores a trie-scale digest" multisetModularInverseCase,
      testCase "the multiset accumulator wraps modulo 2^64 per lane" laneVectorModularWraparoundCase,
      testCase "digests carry self-identifying strategy and encoder identity" crossIdentityCase,
      testProperty "complete digest serialization round-trips" digestSerializationRoundTrip,
      testCase "cross-strategy digest comparison is rejected by the type checker" crossStrategyCompileNegativeCase,
      testCase "oracle SipHash-2-4 matches all 64 canonical vectors" sipHashReferenceVectorCase,
      testCase "128-bit digest construction matches protocol goldens" digestGoldenCase
    ]

merklePatchAgreesWithRebuild :: Property
merklePatchAgreesWithRebuild =
  forAll smallMapGen $ \beforeState ->
    forAll smallMapGen $ \afterState ->
      case (buildTestMerkleDeltaHash beforeState, buildTestMerkleDeltaHash afterState) of
        (Right beforeDeltaHash, Right rebuiltDeltaHash) ->
          fmap
            merkleObservation
            (Patch.applyMerkleDeltaHash (Patch.diff beforeState afterState) beforeDeltaHash)
            === Right (merkleObservation rebuiltDeltaHash)
        (Left obstruction, _) ->
          counterexample ("unexpected initial key collision: " <> show obstruction) False
        (_, Left obstruction) ->
          counterexample ("unexpected rebuilt key collision: " <> show obstruction) False

merkleHistoryDescendsToCanonicalRoot :: Property
merkleHistoryDescendsToCanonicalRoot =
  forAll smallMapGen $ \middleState ->
    forAll smallMapGen $ \finalState ->
      case (buildTestMerkleDeltaHash Map.empty, buildTestMerkleDeltaHash finalState) of
        (Right emptyDeltaHash, Right rebuiltDeltaHash) ->
          let descended = do
                middleDeltaHash <-
                  Patch.applyMerkleDeltaHash
                    (Patch.diff Map.empty middleState)
                    emptyDeltaHash
                Patch.applyMerkleDeltaHash
                  (Patch.diff middleState finalState)
                  middleDeltaHash
           in fmap merkleObservation descended
                === Right (merkleObservation rebuiltDeltaHash)
        (Left obstruction, _) ->
          counterexample ("unexpected empty Merkle failure: " <> show obstruction) False
        (_, Left obstruction) ->
          counterexample ("unexpected final key collision: " <> show obstruction) False

-- | The two states straddle the trie mode (>256 entries), so this law exercises
-- genuine multi-row Patricia insertion and deletion, not the flat hash.
merkleTriePatchAgreesWithRebuild :: Property
merkleTriePatchAgreesWithRebuild =
  withNumTests 40 $
    forAll largeMapGen $ \beforeState ->
      forAll largeMapGen $ \afterState ->
        case (buildTestMerkleDeltaHash beforeState, buildTestMerkleDeltaHash afterState) of
          (Right beforeDeltaHash, Right rebuiltDeltaHash) ->
            fmap
              merkleObservation
              (Patch.applyMerkleDeltaHash (Patch.diff beforeState afterState) beforeDeltaHash)
              === Right (merkleObservation rebuiltDeltaHash)
          (Left obstruction, _) ->
            counterexample ("unexpected initial key collision: " <> show obstruction) False
          (_, Left obstruction) ->
            counterexample ("unexpected rebuilt key collision: " <> show obstruction) False

-- | @apply (compose p2 p1) == apply p2 <=< apply p1@ over trie-mode states.
merkleComposedEqualsSequential :: Property
merkleComposedEqualsSequential =
  withNumTests 25 $
    forAll largeMapGen $ \state0 ->
      forAll largeMapGen $ \state1 ->
        forAll largeMapGen $ \state2 ->
          composedEqualsSequential
            buildTestMerkleDeltaHash
            Patch.composeMerkleDeltaHash
            Patch.applyMerkleDeltaHash
            merkleObservation
            state0
            state1
            state2

multisetComposedEqualsSequential :: Property
multisetComposedEqualsSequential =
  withNumTests 25 $
    forAll largeMapGen $ \state0 ->
      forAll largeMapGen $ \state1 ->
        forAll largeMapGen $ \state2 ->
          composedEqualsSequential
            buildTestMultisetForComposition
            Patch.composeMultisetDeltaHash
            Patch.applyMultisetDeltaHash
            multisetObservation
            state0
            state1
            state2

composedEqualsSequential ::
  (Show obstruction, Show composeError, Eq applyError, Show applyError, Eq observation, Show observation) =>
  (Map Int Int -> Either obstruction carrier) ->
  (Patch.Patch Int Int -> Patch.Patch Int Int -> carrier -> Either composeError (Patch.Patch Int Int)) ->
  (Patch.Patch Int Int -> carrier -> Either applyError carrier) ->
  (carrier -> observation) ->
  Map Int Int ->
  Map Int Int ->
  Map Int Int ->
  Property
composedEqualsSequential build composeFor apply observe state0 state1 state2 =
  case (build state0, build state2) of
    (Right base, Right rebuilt) ->
      let patch1 = Patch.diff state0 state1
          patch2 = Patch.diff state1 state2
       in case composeFor patch2 patch1 base of
            Left composeError ->
              counterexample ("compose refused a valid chain: " <> show composeError) False
            Right composed ->
              let sequentialResult = apply patch1 base >>= apply patch2
                  composedResult = apply composed base
               in ( fmap observe sequentialResult,
                    fmap observe composedResult
                  )
                    === (Right (observe rebuilt), Right (observe rebuilt))
    (Left obstruction, _) ->
      counterexample ("unexpected initial obstruction: " <> show obstruction) False
    (_, Left obstruction) ->
      counterexample ("unexpected final obstruction: " <> show obstruction) False

contextualCompositionAssociativity :: Property
contextualCompositionAssociativity =
  forAll smallMapGen $ \initialState ->
    forAll arbitraryPatchGen $ \patch1 ->
      forAll arbitraryPatchGen $ \patch2 ->
        forAll arbitraryPatchGen $ \patch3 ->
          case buildTestMerkleDeltaHash initialState of
            Left obstruction ->
              counterexample ("unexpected Merkle construction obstruction: " <> show obstruction) False
            Right initialMerkleDeltaHash ->
              contextualAssociativityFor
                "Merkle"
                Patch.composeMerkleDeltaHash
                Patch.applyMerkleDeltaHash
                initialMerkleDeltaHash
                patch1
                patch2
                patch3
                .&&. contextualAssociativityFor
                  "multiset"
                  Patch.composeMultisetDeltaHash
                  Patch.applyMultisetDeltaHash
                  (buildTestMultisetDeltaHash initialState)
                  patch1
                  patch2
                  patch3

arbitraryPatchGen :: Gen (Patch.Patch Int Int)
arbitraryPatchGen =
  Patch.diff <$> smallMapGen <*> smallMapGen

contextualAssociativityFor ::
  String ->
  (Patch.Patch Int Int -> Patch.Patch Int Int -> carrier -> Either (DeltaHashComposeError Int Int) (Patch.Patch Int Int)) ->
  (Patch.Patch Int Int -> carrier -> Either (DeltaHashApplyError Int Int) carrier) ->
  carrier ->
  Patch.Patch Int Int ->
  Patch.Patch Int Int ->
  Patch.Patch Int Int ->
  Property
contextualAssociativityFor strategyName composeFor applyFor initial patch1 patch2 patch3 =
  let leftAssociated =
        composeFor patch2 patch1 initial
          >>= \patch21 -> composeFor patch3 patch21 initial
      rightAssociated =
        rightAssociatedComposition composeFor applyFor patch3 patch2 patch1 initial
      sequential =
        applyFor patch1 initial
          >>= applyFor patch2
          >>= applyFor patch3
      leftProjection = projectCompositionOutcome leftAssociated
      rightProjection = projectCompositionOutcome rightAssociated
      sequentialDefinedness = void sequential
   in counterexample
        (strategyName <> " contextual composition reassociation")
        ( leftProjection === rightProjection
            .&&. void leftProjection === sequentialDefinedness
            .&&. void rightProjection === sequentialDefinedness
        )

rightAssociatedComposition ::
  (Patch.Patch Int Int -> Patch.Patch Int Int -> carrier -> Either (DeltaHashComposeError Int Int) (Patch.Patch Int Int)) ->
  (Patch.Patch Int Int -> carrier -> Either (DeltaHashApplyError Int Int) carrier) ->
  Patch.Patch Int Int ->
  Patch.Patch Int Int ->
  Patch.Patch Int Int ->
  carrier ->
  Either (DeltaHashComposeError Int Int) (Patch.Patch Int Int)
rightAssociatedComposition composeFor applyFor patch3 patch2 patch1 initial =
  case applyFor patch1 initial of
    Left obstruction ->
      Left (DeltaHashOlderPatchRejected obstruction)
    Right afterPatch1 -> do
      patch32 <- composeFor patch3 patch2 afterPatch1
      composeFor patch32 patch1 initial

projectCompositionOutcome ::
  Either (DeltaHashComposeError key value) result ->
  Either (DeltaHashApplyError key value) result
projectCompositionOutcome =
  either (Left . compositionUnderlyingError) Right

compositionUnderlyingError ::
  DeltaHashComposeError key value ->
  DeltaHashApplyError key value
compositionUnderlyingError composeError =
  case composeError of
    DeltaHashOlderPatchRejected obstruction -> obstruction
    DeltaHashNewerPatchRejected obstruction -> obstruction

contextualCompositionStageDivergenceCase :: IO ()
contextualCompositionStageDivergenceCase = do
  let initialState = Map.singleton 1 10
      patch1 = Patch.singleton 1 (Patch.replace 10 11)
      patch2 = Patch.singleton 1 (Patch.replace 10 12)
      patch3 :: Patch.Patch Int Int
      patch3 = Patch.empty
      underlyingFailure =
        DeltaHashPatchRejected
          ApplyBeforeMismatch
            { mismatchKey = 1,
              expectedBefore = Just 10,
              actualBefore = Just 11
            }
  initialMerkleDeltaHash <- requireRight (buildTestMerkleDeltaHash initialState)
  assertBinaryLocalStageDivergence
    Patch.composeMerkleDeltaHash
    Patch.applyMerkleDeltaHash
    initialMerkleDeltaHash
    patch1
    patch2
    patch3
    underlyingFailure
  assertBinaryLocalStageDivergence
    Patch.composeMultisetDeltaHash
    Patch.applyMultisetDeltaHash
    (buildTestMultisetDeltaHash initialState)
    patch1
    patch2
    patch3
    underlyingFailure

assertBinaryLocalStageDivergence ::
  (Patch.Patch Int Int -> Patch.Patch Int Int -> carrier -> Either (DeltaHashComposeError Int Int) (Patch.Patch Int Int)) ->
  (Patch.Patch Int Int -> carrier -> Either (DeltaHashApplyError Int Int) carrier) ->
  carrier ->
  Patch.Patch Int Int ->
  Patch.Patch Int Int ->
  Patch.Patch Int Int ->
  DeltaHashApplyError Int Int ->
  IO ()
assertBinaryLocalStageDivergence composeFor applyFor initial patch1 patch2 patch3 underlyingFailure = do
  let leftAssociated =
        composeFor patch2 patch1 initial
          >>= \patch21 -> composeFor patch3 patch21 initial
      rightAssociated =
        rightAssociatedComposition composeFor applyFor patch3 patch2 patch1 initial
  leftAssociated @?= Left (DeltaHashNewerPatchRejected underlyingFailure)
  rightAssociated @?= Left (DeltaHashOlderPatchRejected underlyingFailure)
  projectCompositionOutcome leftAssociated
    @?= projectCompositionOutcome rightAssociated

multisetPatchAgreesWithRebuild :: Property
multisetPatchAgreesWithRebuild =
  forAll smallMapGen $ \beforeState ->
    forAll smallMapGen $ \afterState ->
      let beforeDeltaHash = buildTestMultisetDeltaHash beforeState
          rebuiltDeltaHash = buildTestMultisetDeltaHash afterState
       in fmap
            multisetObservation
            (Patch.applyMultisetDeltaHash (Patch.diff beforeState afterState) beforeDeltaHash)
            === Right (multisetObservation rebuiltDeltaHash)

multisetDisjointUnionHomomorphism :: Property
multisetDisjointUnionHomomorphism =
  forAll smallMapGen $ \authoritativeState ->
    let (leftState, rightState) =
          Map.partitionWithKey (\key _value -> even key) authoritativeState
        combinedState = Map.union leftState rightState
        combinedByAction =
          Patch.applyMultisetDeltaHash
            (Patch.diff Map.empty rightState)
            (buildTestMultisetDeltaHash leftState)
     in fmap multisetObservation combinedByAction
          === Right (multisetObservation (buildTestMultisetDeltaHash combinedState))

multisetAdditiveInverse :: Property
multisetAdditiveInverse =
  forAll smallMapGen $ \authoritativeState ->
    let freshKey = 4096
        freshValue = 17
        initialDeltaHash = buildTestMultisetDeltaHash authoritativeState
        roundTrip = do
          insertedDeltaHash <-
            Patch.applyMultisetDeltaHash
              (Patch.singleton freshKey (Patch.insert freshValue))
              initialDeltaHash
          Patch.applyMultisetDeltaHash
            (Patch.singleton freshKey (Patch.delete freshValue))
            insertedDeltaHash
     in fmap multisetObservation roundTrip
          === Right (multisetObservation initialDeltaHash)

multisetOrderIndependence :: Property
multisetOrderIndependence =
  forAll smallMapGen $ \authoritativeState ->
    let (leftState, rightState) =
          Map.partitionWithKey (\key _value -> even key) authoritativeState
        leftPatch = Patch.diff Map.empty leftState
        rightPatch = Patch.diff Map.empty rightState
        emptyDeltaHash = buildTestMultisetDeltaHash Map.empty
        leftThenRight = do
          leftDeltaHash <- Patch.applyMultisetDeltaHash leftPatch emptyDeltaHash
          Patch.applyMultisetDeltaHash rightPatch leftDeltaHash
        rightThenLeft = do
          rightDeltaHash <- Patch.applyMultisetDeltaHash rightPatch emptyDeltaHash
          Patch.applyMultisetDeltaHash leftPatch rightDeltaHash
        expectedObservation =
          multisetObservation (buildTestMultisetDeltaHash authoritativeState)
     in (fmap multisetObservation leftThenRight, fmap multisetObservation rightThenLeft)
          === (Right expectedObservation, Right expectedObservation)

adaptiveBoundaryCase :: IO ()
adaptiveBoundaryCase = do
  let boundaryState = sequentialState deltaHashFlatBoundarySize
      insertedKey = deltaHashFlatBoundarySize
      expandedState = Map.insert insertedKey insertedKey boundaryState
  boundaryDeltaHash <- requireRight (buildTestMerkleDeltaHash boundaryState)
  expandedDeltaHash <-
    requireRight
      ( Patch.applyMerkleDeltaHash
          (Patch.singleton insertedKey (Patch.insert insertedKey))
          boundaryDeltaHash
      )
  rebuiltExpandedDeltaHash <- requireRight (buildTestMerkleDeltaHash expandedState)
  merkleObservation expandedDeltaHash
    @?= merkleObservation rebuiltExpandedDeltaHash
  contractedDeltaHash <-
    requireRight
      ( Patch.applyMerkleDeltaHash
          (Patch.singleton insertedKey (Patch.delete insertedKey))
          expandedDeltaHash
      )
  merkleObservation contractedDeltaHash
    @?= merkleObservation boundaryDeltaHash

-- | Forced digest cases at the 64-row page capacity (63/64/65) and at the flat
-- adaptive threshold (255/256/257): a single insert crossing each boundary must
-- agree with a fresh rebuild under both strategies.
boundaryDigestCase :: IO ()
boundaryDigestCase =
  traverse_ checkBoundary [63, 64, 65, 255, 256, 257]
  where
    checkBoundary stateSize = do
      let baseState = sequentialState stateSize
          insertedKey = stateSize
          expandedState = Map.insert insertedKey insertedKey baseState
      baseMerkle <- requireRight (buildTestMerkleDeltaHash baseState)
      expandedMerkle <-
        requireRight
          ( Patch.applyMerkleDeltaHash
              (Patch.singleton insertedKey (Patch.insert insertedKey))
              baseMerkle
          )
      rebuiltMerkle <- requireRight (buildTestMerkleDeltaHash expandedState)
      merkleObservation expandedMerkle @?= merkleObservation rebuiltMerkle
      assertMerkleIdentity (Patch.merkleDeltaHashDigest expandedMerkle)
      let baseMultiset = buildTestMultisetDeltaHash baseState
      expandedMultiset <-
        requireRight
          ( Patch.applyMultisetDeltaHash
              (Patch.singleton insertedKey (Patch.insert insertedKey))
              baseMultiset
          )
      multisetObservation expandedMultiset
        @?= multisetObservation (buildTestMultisetDeltaHash expandedState)
      assertMultisetIdentity (Patch.multisetDeltaHashDigest expandedMultiset)

-- | In a trie-mode state, delete one key and then insert a distinct key on the
-- freed localized path. Contextual composition, sequential action, and rebuild
-- must agree.
pathFreedByDeletionCase :: IO ()
pathFreedByDeletionCase = do
  let beforeState = Map.fromList [(key, key * 10) | key <- [1 .. deltaHashFlatBoundarySize + 1]]
      collidingKey = deltaHashFlatBoundarySize
      freedKey = 0
      afterState = Map.insert freedKey 5 (Map.delete collidingKey beforeState)
      deletePatch = Patch.singleton collidingKey (Patch.delete (collidingKey * 10))
      insertPatch = Patch.singleton freedKey (Patch.insert 5)
  beforeDeltaHash <-
    requireRight (Patch.buildMerkleDeltaHash testEncoderVersion boundaryCollisionEncoding intEncoding beforeState)
  composed <-
    requireRight (Patch.composeMerkleDeltaHash insertPatch deletePatch beforeDeltaHash)
  rebuiltDeltaHash <-
    requireRight (Patch.buildMerkleDeltaHash testEncoderVersion boundaryCollisionEncoding intEncoding afterState)
  composedResult <-
    requireRight (Patch.applyMerkleDeltaHash composed beforeDeltaHash)
  sequentialResult <-
    requireRight
      (Patch.applyMerkleDeltaHash deletePatch beforeDeltaHash >>= Patch.applyMerkleDeltaHash insertPatch)
  merkleObservationOn composedResult @?= merkleObservationOn rebuiltDeltaHash
  merkleObservationOn sequentialResult @?= merkleObservationOn rebuiltDeltaHash

mirrorPathCollisionCase :: IO ()
mirrorPathCollisionCase = do
  let beforeState = Map.fromList [(key, key * 10) | key <- [1 .. deltaHashFlatBoundarySize + 1]]
      collidingKey = deltaHashFlatBoundarySize
      incomingKey = 0
      insertPatch = Patch.singleton incomingKey (Patch.insert 5)
      deletePatch = Patch.singleton collidingKey (Patch.delete (collidingKey * 10))
      afterState = Map.insert incomingKey 5 (Map.delete collidingKey beforeState)
  beforeDeltaHash <-
    requireRight (Patch.buildMerkleDeltaHash testEncoderVersion boundaryCollisionEncoding intEncoding beforeState)
  case Patch.applyMerkleDeltaHash insertPatch beforeDeltaHash of
    Left sequentialFailure -> do
      Patch.composeMerkleDeltaHash deletePatch insertPatch beforeDeltaHash
        @?= Left (DeltaHashOlderPatchRejected sequentialFailure)
      Patch.composeMerkleDeltaHash insertPatch Patch.empty beforeDeltaHash
        @?= Left (DeltaHashNewerPatchRejected sequentialFailure)
    Right _intermediate ->
      assertFailure "expected the older insertion to reject its occupied path"
  extensionalPatch <- requireRight (Patch.compose deletePatch insertPatch)
  extensionalResult <- requireRight (Patch.applyMerkleDeltaHash extensionalPatch beforeDeltaHash)
  Patch.merkleDeltaHashState extensionalResult @?= afterState

contextualCompositionFailureRefinementCase :: IO ()
contextualCompositionFailureRefinementCase = do
  let authoritativeState = Map.singleton 1 10
      rejectedOlder = Patch.singleton 1 (Patch.replace 9 11)
      admittedOlder = Patch.singleton 1 (Patch.replace 10 11)
      rejectedNewer = Patch.singleton 1 (Patch.replace 10 12)
      olderFailure =
        DeltaHashPatchRejected
          ApplyBeforeMismatch
            { mismatchKey = 1,
              expectedBefore = Just 9,
              actualBefore = Just 10
            }
      newerFailure =
        DeltaHashPatchRejected
          ApplyBeforeMismatch
            { mismatchKey = 1,
              expectedBefore = Just 10,
              actualBefore = Just 11
            }
  merkleDeltaHash <- requireRight (buildTestMerkleDeltaHash authoritativeState)
  Patch.composeMerkleDeltaHash Patch.empty rejectedOlder merkleDeltaHash
    @?= Left (DeltaHashOlderPatchRejected olderFailure)
  Patch.composeMerkleDeltaHash rejectedNewer admittedOlder merkleDeltaHash
    @?= Left (DeltaHashNewerPatchRejected newerFailure)
  let multisetDeltaHash = buildTestMultisetDeltaHash authoritativeState
  Patch.composeMultisetDeltaHash Patch.empty rejectedOlder multisetDeltaHash
    @?= Left (DeltaHashOlderPatchRejected olderFailure)
  Patch.composeMultisetDeltaHash rejectedNewer admittedOlder multisetDeltaHash
    @?= Left (DeltaHashNewerPatchRejected newerFailure)

largeMultiRowRebuildCase :: IO ()
largeMultiRowRebuildCase = do
  let buckets = [0 .. 64]
      paddingEntries =
        [ (PathBucketKey ident ident, ident)
          | ident <- [1000 .. 1000 + deltaHashFlatBoundarySize]
        ]
      oldEntries =
        [ (PathBucketKey (100 + bucket) bucket, bucket)
          | bucket <- buckets
        ]
      newEntries =
        [ (PathBucketKey bucket bucket, bucket + 1000)
          | bucket <- buckets
        ]
      beforeState = Map.fromList (paddingEntries <> oldEntries)
      afterState = Map.fromList (paddingEntries <> newEntries)
      reassignmentPatch = Patch.diff beforeState afterState
  beforeDeltaHash <-
    requireRight (Patch.buildMerkleDeltaHash testEncoderVersion pathBucketEncoding intEncoding beforeState)
  updatedDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash reassignmentPatch beforeDeltaHash)
  rebuiltDeltaHash <-
    requireRight (Patch.buildMerkleDeltaHash testEncoderVersion pathBucketEncoding intEncoding afterState)
  merkleObservationOn updatedDeltaHash @?= merkleObservationOn rebuiltDeltaHash

-- | A three-key localized-path cycle inside one trie-mode patch must agree with
-- rebuilding the final map.
pathCycleCase :: IO ()
pathCycleCase = do
  let paddingKeys = [1000 .. 1000 + deltaHashFlatBoundarySize + 16]
      paddingEntries = [(PathBucketKey ident ident, ident) | ident <- paddingKeys]
      oldEntries =
        [ (PathBucketKey 100 bucketX, 100),
          (PathBucketKey 101 bucketY, 101),
          (PathBucketKey 102 bucketZ, 102)
        ]
      beforeState = Map.fromList (paddingEntries <> oldEntries)
      cyclePatch =
        Patch.fromList
          [ (PathBucketKey 1 bucketY, Patch.insert 1),
            (PathBucketKey 2 bucketZ, Patch.insert 2),
            (PathBucketKey 3 bucketX, Patch.insert 3),
            (PathBucketKey 100 bucketX, Patch.delete 100),
            (PathBucketKey 101 bucketY, Patch.delete 101),
            (PathBucketKey 102 bucketZ, Patch.delete 102)
          ]
      afterState =
        Map.fromList
          ( paddingEntries
              <> [ (PathBucketKey 1 bucketY, 1),
                   (PathBucketKey 2 bucketZ, 2),
                   (PathBucketKey 3 bucketX, 3)
                 ]
          )
      bucketX = 10
      bucketY = 20
      bucketZ = 30
  beforeDeltaHash <-
    requireRight (Patch.buildMerkleDeltaHash testEncoderVersion pathBucketEncoding intEncoding beforeState)
  rebuiltDeltaHash <-
    requireRight (Patch.buildMerkleDeltaHash testEncoderVersion pathBucketEncoding intEncoding afterState)
  cycledDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash cyclePatch beforeDeltaHash)
  ( Patch.merkleDeltaHashState cycledDeltaHash,
    Patch.merkleDeltaHashDigest cycledDeltaHash
    )
    @?= ( Patch.merkleDeltaHashState rebuiltDeltaHash,
          Patch.merkleDeltaHashDigest rebuiltDeltaHash
        )

samePathRepresentativeCompositionCase :: IO ()
samePathRepresentativeCompositionCase = do
  let stateEntry key =
        (RepresentativeKey key StateRepresentative, key * 10)
      finalEntry key
        | key == 1 = (RepresentativeKey key FinalRepresentative, key * 10)
        | otherwise = stateEntry key
      keys = [1 .. deltaHashFlatBoundarySize + 1]
      initialState = Map.fromDistinctAscList (fmap stateEntry keys)
      finalState = Map.fromDistinctAscList (fmap finalEntry keys)
      olderPatch =
        Patch.singleton
          (RepresentativeKey 1 PatchRepresentative)
          (Patch.replace 10 10)
      newerPatch =
        Patch.singleton
          (RepresentativeKey 1 FinalRepresentative)
          (Patch.replace 10 10)
  initialDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion representativeSamePathEncoding intEncoding initialState)
  admittedPatch <-
    requireRight (Patch.composeMerkleDeltaHash newerPatch olderPatch initialDeltaHash)
  sequentialResult <-
    requireRight
      ( Patch.applyMerkleDeltaHash olderPatch initialDeltaHash
          >>= Patch.applyMerkleDeltaHash newerPatch
      )
  admittedResult <- requireRight (Patch.applyMerkleDeltaHash admittedPatch initialDeltaHash)
  rebuiltResult <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion representativeSamePathEncoding intEncoding finalState)
  merkleObservationOn sequentialResult @?= merkleObservationOn rebuiltResult
  merkleObservationOn admittedResult @?= merkleObservationOn rebuiltResult
  fmap (representativeKind . fst) (Map.lookupGE (RepresentativeKey 1 FinalRepresentative) (Patch.merkleDeltaHashState sequentialResult))
    @?= Just FinalRepresentative
  fmap (representativeKind . fst) (Map.lookupGE (RepresentativeKey 1 FinalRepresentative) (Patch.merkleDeltaHashState admittedResult))
    @?= Just FinalRepresentative
  let initialMultiset =
        Patch.buildMultisetDeltaHash
          testEncoderVersion
          representativeSamePathEncoding
          intEncoding
          initialState
  admittedMultisetPatch <-
    requireRight (Patch.composeMultisetDeltaHash newerPatch olderPatch initialMultiset)
  sequentialMultiset <-
    requireRight
      ( Patch.applyMultisetDeltaHash olderPatch initialMultiset
          >>= Patch.applyMultisetDeltaHash newerPatch
      )
  admittedMultiset <-
    requireRight (Patch.applyMultisetDeltaHash admittedMultisetPatch initialMultiset)
  fmap (representativeKind . fst) (Map.lookupGE (RepresentativeKey 1 FinalRepresentative) (Patch.multisetDeltaHashState sequentialMultiset))
    @?= Just FinalRepresentative
  fmap (representativeKind . fst) (Map.lookupGE (RepresentativeKey 1 FinalRepresentative) (Patch.multisetDeltaHashState admittedMultiset))
    @?= Just FinalRepresentative

emptyPatchIdentityCase :: IO ()
emptyPatchIdentityCase = do
  let flatState = Map.fromList [(1, 10), (2, 20)]
      trieState = sequentialState (deltaHashFlatBoundarySize + 32)
  flatMerkle <- requireRight (buildTestMerkleDeltaHash flatState)
  flatIdentity <- requireRight (Patch.applyMerkleDeltaHash Patch.empty flatMerkle)
  merkleObservation flatIdentity @?= merkleObservation flatMerkle
  assertMerkleIdentity (Patch.merkleDeltaHashDigest flatIdentity)
  trieMerkle <- requireRight (buildTestMerkleDeltaHash trieState)
  trieIdentity <- requireRight (Patch.applyMerkleDeltaHash Patch.empty trieMerkle)
  merkleObservation trieIdentity @?= merkleObservation trieMerkle
  assertMerkleIdentity (Patch.merkleDeltaHashDigest trieIdentity)
  let multiset = buildTestMultisetDeltaHash trieState
  multisetIdentity <- requireRight (Patch.applyMultisetDeltaHash Patch.empty multiset)
  multisetObservation multisetIdentity @?= multisetObservation multiset
  assertMultisetIdentity (Patch.multisetDeltaHashDigest multisetIdentity)

-- | @compose (invert p) p@ is the identity on @p@'s source and @compose p
-- (invert p)@ is the identity on its target, under both strategies and in trie
-- mode.
inverseCompositionCase :: IO ()
inverseCompositionCase = do
  let sourceState = trieStateA
      targetState = trieStateB
      forward = Patch.diff sourceState targetState
      backward = Patch.invert forward
  sourceMerkle <- requireRight (buildTestMerkleDeltaHash sourceState)
  targetMerkle <- requireRight (buildTestMerkleDeltaHash targetState)
  forwardThenBack <-
    requireRight (Patch.composeMerkleDeltaHash backward forward sourceMerkle)
  backThenForward <-
    requireRight (Patch.composeMerkleDeltaHash forward backward targetMerkle)
  restoredSource <- requireRight (Patch.applyMerkleDeltaHash forwardThenBack sourceMerkle)
  merkleObservation restoredSource @?= merkleObservation sourceMerkle
  restoredTarget <- requireRight (Patch.applyMerkleDeltaHash backThenForward targetMerkle)
  merkleObservation restoredTarget @?= merkleObservation targetMerkle
  let sourceMultiset = buildTestMultisetDeltaHash sourceState
      targetMultiset = buildTestMultisetDeltaHash targetState
  multisetForwardThenBack <-
    requireRight (Patch.composeMultisetDeltaHash backward forward sourceMultiset)
  multisetBackThenForward <-
    requireRight (Patch.composeMultisetDeltaHash forward backward targetMultiset)
  restoredSourceMultiset <- requireRight (Patch.applyMultisetDeltaHash multisetForwardThenBack sourceMultiset)
  multisetObservation restoredSourceMultiset @?= multisetObservation sourceMultiset
  restoredTargetMultiset <- requireRight (Patch.applyMultisetDeltaHash multisetBackThenForward targetMultiset)
  multisetObservation restoredTargetMultiset @?= multisetObservation targetMultiset

-- | Replay reduces a patch chain to a final @Map@; digesting that map must equal
-- folding the same patches through the incremental digest. This glues the
-- replay decoder to the digest action across strategies.
replayDigestAgreesCase :: IO ()
replayDigestAgreesCase = do
  let state0 = trieStateA
      state1 = trieStateB
      state2 = trieStateC
      patches =
        [ Patch.diff state0 state1,
          Patch.diff state1 state2
        ]
  replayedState <- requireRight (Patch.replay patches state0)
  merkleBase <- requireRight (buildTestMerkleDeltaHash state0)
  sequentialMerkle <-
    requireRight (foldM (flip Patch.applyMerkleDeltaHash) merkleBase patches)
  rebuiltMerkle <- requireRight (buildTestMerkleDeltaHash replayedState)
  merkleObservation sequentialMerkle @?= merkleObservation rebuiltMerkle
  assertMerkleIdentity (Patch.merkleDeltaHashDigest sequentialMerkle)
  let multisetBase = buildTestMultisetDeltaHash state0
  sequentialMultiset <-
    requireRight (foldM (flip Patch.applyMultisetDeltaHash) multisetBase patches)
  multisetObservation sequentialMultiset
    @?= multisetObservation (buildTestMultisetDeltaHash replayedState)
  assertMultisetIdentity (Patch.multisetDeltaHashDigest sequentialMultiset)

cellEditsCase :: IO ()
cellEditsCase = do
  let initialState = Map.fromList [(1, 10), (2, 20)]
      expectedState = Map.fromList [(1, 11), (3, 30)]
      patchValue =
        Patch.fromList
          [ (1, Patch.replace 10 11),
            (2, Patch.delete 20),
            (3, Patch.insert 30),
            (4, Patch.assertAbsent)
          ]
  initialMerkleDeltaHash <- requireRight (buildTestMerkleDeltaHash initialState)
  updatedMerkleDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash)
  rebuiltMerkleDeltaHash <- requireRight (buildTestMerkleDeltaHash expectedState)
  merkleObservation updatedMerkleDeltaHash
    @?= merkleObservation rebuiltMerkleDeltaHash
  updatedMultisetDeltaHash <-
    requireRight
      ( Patch.applyMultisetDeltaHash
          patchValue
          (buildTestMultisetDeltaHash initialState)
      )
  multisetObservation updatedMultisetDeltaHash
    @?= multisetObservation (buildTestMultisetDeltaHash expectedState)

stalePatchCase :: IO ()
stalePatchCase = do
  let initialState = Map.singleton 1 10
      patchValue = Patch.singleton 1 (Patch.replace 9 11)
      expectedObstruction =
        DeltaHashPatchRejected
          ApplyBeforeMismatch
            { mismatchKey = 1,
              expectedBefore = Just 9,
              actualBefore = Just 10
            }
  initialMerkleDeltaHash <- requireRight (buildTestMerkleDeltaHash initialState)
  case Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash of
    Left obstruction ->
      obstruction @?= expectedObstruction
    Right _deltaHash ->
      assertFailure "expected stale Merkle patch to be rejected"
  case Patch.applyMultisetDeltaHash patchValue (buildTestMultisetDeltaHash initialState) of
    Left obstruction ->
      obstruction @?= expectedObstruction
    Right _deltaHash ->
      assertFailure "expected stale multiset patch to be rejected"

representativeRewriteCase :: IO ()
representativeRewriteCase = do
  let stateEntry key =
        (RepresentativeKey key StateRepresentative, key * 10)
      expectedEntry key
        | key == 1 = (RepresentativeKey key PatchRepresentative, 11)
        | otherwise = stateEntry key
      keys = [1 .. deltaHashFlatBoundarySize + 1]
      initialState = Map.fromDistinctAscList (fmap stateEntry keys)
      expectedState = Map.fromDistinctAscList (fmap expectedEntry keys)
      patchKey = RepresentativeKey 1 PatchRepresentative
      patchValue = Patch.singleton patchKey (Patch.replace 10 11)
  initialMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion representativeKeyEncoding intEncoding initialState)
  updatedMerkleDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash)
  rebuiltMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion representativeKeyEncoding intEncoding expectedState)
  Patch.merkleDeltaHashDigest updatedMerkleDeltaHash
    @?= Patch.merkleDeltaHashDigest rebuiltMerkleDeltaHash
  fmap (representativeKind . fst) (Map.lookupGE patchKey (Patch.merkleDeltaHashState updatedMerkleDeltaHash))
    @?= Just PatchRepresentative
  let initialMultisetDeltaHash =
        Patch.buildMultisetDeltaHash testEncoderVersion representativeKeyEncoding intEncoding initialState
      rebuiltMultisetDeltaHash =
        Patch.buildMultisetDeltaHash testEncoderVersion representativeKeyEncoding intEncoding expectedState
  updatedMultisetDeltaHash <-
    requireRight (Patch.applyMultisetDeltaHash patchValue initialMultisetDeltaHash)
  Patch.multisetDeltaHashDigest updatedMultisetDeltaHash
    @?= Patch.multisetDeltaHashDigest rebuiltMultisetDeltaHash
  fmap (representativeKind . fst) (Map.lookupGE patchKey (Patch.multisetDeltaHashState updatedMultisetDeltaHash))
    @?= Just PatchRepresentative

valueRepresentativeRewriteCase :: IO ()
valueRepresentativeRewriteCase = do
  let stateValue = RepresentativeValue 10 StateRepresentative
      patchBefore = RepresentativeValue 10 PatchRepresentative
      patchAfter = RepresentativeValue 11 PatchRepresentative
      initialState = Map.singleton 1 stateValue
      expectedState = Map.singleton 1 patchAfter
      patchValue = Patch.singleton 1 (Patch.replace patchBefore patchAfter)
  initialMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion intEncoding representativeValueEncoding initialState)
  updatedMerkleDeltaHash <-
    requireRight (Patch.applyMerkleDeltaHash patchValue initialMerkleDeltaHash)
  rebuiltMerkleDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion intEncoding representativeValueEncoding expectedState)
  Patch.merkleDeltaHashDigest updatedMerkleDeltaHash
    @?= Patch.merkleDeltaHashDigest rebuiltMerkleDeltaHash
  let initialMultisetDeltaHash =
        Patch.buildMultisetDeltaHash testEncoderVersion intEncoding representativeValueEncoding initialState
      rebuiltMultisetDeltaHash =
        Patch.buildMultisetDeltaHash testEncoderVersion intEncoding representativeValueEncoding expectedState
  updatedMultisetDeltaHash <-
    requireRight (Patch.applyMultisetDeltaHash patchValue initialMultisetDeltaHash)
  Patch.multisetDeltaHashDigest updatedMultisetDeltaHash
    @?= Patch.multisetDeltaHashDigest rebuiltMultisetDeltaHash

flatModeCollisionCase :: IO ()
flatModeCollisionCase = do
  let authoritativeState = Map.fromList [(1, 10), (2, 20)]
  deltaHashValue <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion constantEncoding intEncoding authoritativeState)
  Patch.merkleDeltaHashState deltaHashValue @?= authoritativeState

constructionCollisionCase :: IO ()
constructionCollisionCase =
  case
      Patch.buildMerkleDeltaHash
        testEncoderVersion
        boundaryCollisionEncoding
        intEncoding
        (sequentialState (deltaHashFlatBoundarySize + 1))
    of
      Left DeltaHashKeyCollision {deltaHashExistingKey, deltaHashIncomingKey} ->
        (deltaHashExistingKey, deltaHashIncomingKey)
          @?= (0, deltaHashFlatBoundarySize)
      Right _deltaHash ->
        assertFailure "expected distinct keys with one digest path to be rejected"

updateCollisionCase :: IO ()
updateCollisionCase = do
  let authoritativeState = sequentialState deltaHashFlatBoundarySize
      incomingKey = deltaHashFlatBoundarySize
  initialDeltaHash <-
    requireRight
      (Patch.buildMerkleDeltaHash testEncoderVersion boundaryCollisionEncoding intEncoding authoritativeState)
  case
      Patch.applyMerkleDeltaHash
        (Patch.singleton incomingKey (Patch.insert incomingKey))
        initialDeltaHash
    of
      Left (DeltaHashUpdateRejected DeltaHashKeyCollision {}) ->
        Patch.merkleDeltaHashState initialDeltaHash @?= authoritativeState
      Left obstruction ->
        assertFailure ("expected collision obstruction, received: " <> show obstruction)
      Right _deltaHash ->
        assertFailure "expected insertion collision to be rejected"

-- | A paged patch that exercises the constant, run, affine, and dense value
-- column encodings plus absent presence columns. The digest action decodes it
-- through the same page fold as construction, so its result must equal the
-- rebuild of the final map.
mixedPageEncodingCase :: IO ()
mixedPageEncodingCase = do
  let stats = debugCodecStats mixedFormPatch
      exercisesEveryForm =
        codecConstantColumns stats >= 1
          && codecRunColumns stats >= 1
          && codecAffineValueColumns stats >= 1
          && codecDenseColumns stats >= 1
          && codecAbsentColumns stats >= 1
  exercisesEveryForm @?= True
  replayedState <- requireRight (Patch.replay [mixedFormPatch] mixedFormInitialMap)
  replayedState @?= mixedFormMap
  initialMerkle <- requireRight (buildTestMerkleDeltaHash mixedFormInitialMap)
  updatedMerkle <-
    requireRight (Patch.applyMerkleDeltaHash mixedFormPatch initialMerkle)
  rebuiltMerkle <- requireRight (buildTestMerkleDeltaHash replayedState)
  merkleObservation updatedMerkle @?= merkleObservation rebuiltMerkle
  assertMerkleIdentity (Patch.merkleDeltaHashDigest updatedMerkle)
  updatedMultiset <-
    requireRight (Patch.applyMultisetDeltaHash mixedFormPatch (buildTestMultisetDeltaHash mixedFormInitialMap))
  multisetObservation updatedMultiset
    @?= multisetObservation (buildTestMultisetDeltaHash replayedState)
  assertMultisetIdentity (Patch.multisetDeltaHashDigest updatedMultiset)

-- | With a constant value encoder, a valid replacement subtracts and re-adds the
-- same lane vector: the authoritative state changes while the digest does not.
-- This is the multiset scheme's documented non-injectivity, made explicit.
multisetNonInjectiveCollapseCase :: IO ()
multisetNonInjectiveCollapseCase = do
  let initialState = Map.singleton 1 (10 :: Int)
      collapsed = Patch.buildMultisetDeltaHash testEncoderVersion intEncoding constantEncoding initialState
      distinctEncoderIdentity = Patch.buildMultisetDeltaHash shiftedEncoderVersion intEncoding constantEncoding initialState
      patchValue = Patch.singleton 1 (Patch.replace 10 99)
  updated <- requireRight (Patch.applyMultisetDeltaHash patchValue collapsed)
  Patch.multisetDeltaHashState updated @?= Map.singleton 1 99
  Patch.multisetDeltaHashDigest updated @?= Patch.multisetDeltaHashDigest collapsed
  (Patch.multisetDeltaHashDigest distinctEncoderIdentity == Patch.multisetDeltaHashDigest collapsed)
    @?= False

-- | Inserting A, inserting B, then deleting A cancels A's contribution exactly:
-- the digest equals a fresh build of the reduced state.
multisetCancellationCase :: IO ()
multisetCancellationCase = do
  let program = do
        withA <-
          Patch.applyMultisetDeltaHash
            (Patch.singleton 1 (Patch.insert 100))
            (buildTestMultisetDeltaHash Map.empty)
        withAB <-
          Patch.applyMultisetDeltaHash (Patch.singleton 2 (Patch.insert 200)) withA
        Patch.applyMultisetDeltaHash (Patch.singleton 1 (Patch.delete 100)) withAB
  result <- requireRight program
  multisetObservation result
    @?= multisetObservation (buildTestMultisetDeltaHash (Map.singleton 2 200))

-- | Wrapping addition and subtraction are exact inverses on the 16-lane group,
-- so inserting then deleting a fresh key restores a trie-scale digest bit for
-- bit — the modular-group law the accumulator relies on at any scale.
multisetModularInverseCase :: IO ()
multisetModularInverseCase = do
  let baseState = sequentialState (deltaHashFlatBoundarySize + 64)
      freshKey = deltaHashFlatBoundarySize + 4096
      base = buildTestMultisetDeltaHash baseState
  roundTrip <-
    requireRight
      ( Patch.applyMultisetDeltaHash (Patch.singleton freshKey (Patch.insert 7)) base
          >>= Patch.applyMultisetDeltaHash (Patch.singleton freshKey (Patch.delete 7))
      )
  multisetObservation roundTrip @?= multisetObservation base

-- | The accumulator lives in @(Z / 2^64 Z)^16@: each lane is a 'Word64' with
-- wrapping addition. Adding one past the maximum returns to zero and
-- subtracting below zero returns to the maximum, so the digest's finite-group
-- collision structure is exactly modular, not saturating.
laneVectorModularWraparoundCase :: IO ()
laneVectorModularWraparoundCase = do
  let laneAt index value =
        laneVectorFromLanes
          (UVector.generate laneCount (\lane -> if lane == index then value else 0))
      wrappedForward = add (laneAt 0 maxBound) (laneAt 0 1)
      wrappedBackward = sub laneVectorZero (laneAt 0 1)
  (laneVectorLanes wrappedForward UVector.! 0) @?= (0 :: Word64)
  (laneVectorLanes wrappedBackward UVector.! 0) @?= (maxBound :: Word64)

-- | Digests self-identify: each carries its runtime strategy and encoding
-- version, and equal maps under different encoders produce different digests, so
-- verifying under the wrong scheme or encoder is a detectable mismatch. The
-- cross-strategy comparison @merkleDigest == multisetDigest@ is rejected by the
-- type checker and cannot be written here.
crossIdentityCase :: IO ()
crossIdentityCase = do
  let state = sequentialState (deltaHashFlatBoundarySize + 8)
  merkleDeltaHash <- requireRight (buildTestMerkleDeltaHash state)
  let merkleDigest = Patch.merkleDeltaHashDigest merkleDeltaHash
      multisetDigest = Patch.multisetDeltaHashDigest (buildTestMultisetDeltaHash state)
      merkleIdentity = Patch.deltaHashDigestIdentity merkleDigest
      multisetIdentity = Patch.deltaHashDigestIdentity multisetDigest
      encodedMerkleDigest = Patch.encodeDeltaHashDigest merkleDigest
      encodedMultisetDigest = Patch.encodeDeltaHashDigest multisetDigest
  Patch.decodeMerkleDeltaHashDigest testEncoderVersion encodedMerkleDigest
    @?= Right merkleDigest
  Patch.decodeMultisetDeltaHashDigest testEncoderVersion encodedMultisetDigest
    @?= Right multisetDigest
  Patch.decodeMultisetDeltaHashDigest testEncoderVersion encodedMerkleDigest
    @?= Left
      ( DeltaHashDigestIdentityMismatch
          multisetIdentity
          merkleIdentity
      )
  Patch.decodeMerkleDeltaHashDigest testEncoderVersion encodedMultisetDigest
    @?= Left
      ( DeltaHashDigestIdentityMismatch
          merkleIdentity
          multisetIdentity
      )
  Patch.decodeMerkleDeltaHashDigest testEncoderVersion (encodedMerkleDigest <> ByteString.singleton 0)
    @?= Left (DeltaHashDigestTrailingBytes 1)
  versionedMerkle <- requireRight (Patch.buildMerkleDeltaHash shiftedEncoderVersion intEncoding intEncoding state)
  let versionedMerkleDigest = Patch.merkleDeltaHashDigest versionedMerkle
      versionedMultisetDigest =
        Patch.multisetDeltaHashDigest
          (Patch.buildMultisetDeltaHash shiftedEncoderVersion intEncoding intEncoding state)
      versionedMerkleIdentity = Patch.deltaHashDigestIdentity versionedMerkleDigest
  Patch.decodeMerkleDeltaHashDigest shiftedEncoderVersion encodedMerkleDigest
    @?= Left
      ( DeltaHashDigestIdentityMismatch
          versionedMerkleIdentity
          merkleIdentity
      )
  (versionedMerkleDigest == merkleDigest) @?= False
  (versionedMultisetDigest == multisetDigest) @?= False
  shiftedMerkle <- requireRight (Patch.buildMerkleDeltaHash shiftedEncoderVersion shiftedEncoding intEncoding state)
  (merkleDigest == Patch.merkleDeltaHashDigest shiftedMerkle) @?= False
  let shiftedMultiset = Patch.buildMultisetDeltaHash shiftedEncoderVersion shiftedEncoding intEncoding state
  (multisetDigest == Patch.multisetDeltaHashDigest shiftedMultiset) @?= False

digestSerializationRoundTrip :: Property
digestSerializationRoundTrip =
  forAll smallMapGen $ \state ->
    case buildTestMerkleDeltaHash state of
      Left obstruction ->
        counterexample ("unexpected Merkle construction obstruction: " <> show obstruction) False
      Right merkleDeltaHash ->
        let merkleDigest = Patch.merkleDeltaHashDigest merkleDeltaHash
            multisetDigest = Patch.multisetDeltaHashDigest (buildTestMultisetDeltaHash state)
         in ( Patch.decodeMerkleDeltaHashDigest
                testEncoderVersion
                (Patch.encodeDeltaHashDigest merkleDigest),
              Patch.decodeMultisetDeltaHashDigest
                testEncoderVersion
                (Patch.encodeDeltaHashDigest multisetDigest)
            )
              === (Right merkleDigest, Right multisetDigest)

crossStrategyCompileNegativeCase :: IO ()
crossStrategyCompileNegativeCase = do
  merkleDeltaHash <- requireRight (buildTestMerkleDeltaHash Map.empty)
  let merkleDigest = Patch.merkleDeltaHashDigest merkleDeltaHash
      multisetDigest = Patch.multisetDeltaHashDigest (buildTestMultisetDeltaHash Map.empty)
  comparison <-
    try (evaluate (crossStrategyDigestComparison merkleDigest multisetDigest)) :: IO (Either TypeError Bool)
  case comparison of
    Left typeError -> do
      let typeErrorMessage = show typeError
      assertBool
        ("deferred type error did not name the digest-strategy mismatch: " <> typeErrorMessage)
        ( "Couldn't match type" `isInfixOf` typeErrorMessage
            && "MerkleDigestStrategy" `isInfixOf` typeErrorMessage
            && "MultisetDigestStrategy" `isInfixOf` typeErrorMessage
        )
    Right _comparisonResult ->
      assertFailure "complete digest cross-strategy comparison unexpectedly typechecked"

sipHashReferenceVectorCase :: IO ()
sipHashReferenceVectorCase =
  oracleSipHashReferenceVectors @?= canonicalSipHash24ReferenceVectors

-- SipHash reference implementation vectors.h for messages of length 0 through
-- 63 under key bytes 00..0f: https://github.com/veorq/SipHash/blob/master/vectors.h
canonicalSipHash24ReferenceVectors :: [Word64]
canonicalSipHash24ReferenceVectors =
  [ 0x726fdb47dd0e0e31,
    0x74f839c593dc67fd,
    0x0d6c8009d9a94f5a,
    0x85676696d7fb7e2d,
    0xcf2794e0277187b7,
    0x18765564cd99a68d,
    0xcbc9466e58fee3ce,
    0xab0200f58b01d137,
    0x93f5f5799a932462,
    0x9e0082df0ba9e4b0,
    0x7a5dbbc594ddb9f3,
    0xf4b32f46226bada7,
    0x751e8fbc860ee5fb,
    0x14ea5627c0843d90,
    0xf723ca908e7af2ee,
    0xa129ca6149be45e5,
    0x3f2acc7f57c29bdb,
    0x699ae9f52cbe4794,
    0x4bc1b3f0968dd39c,
    0xbb6dc91da77961bd,
    0xbed65cf21aa2ee98,
    0xd0f2cbb02e3b67c7,
    0x93536795e3a33e88,
    0xa80c038ccd5ccec8,
    0xb8ad50c6f649af94,
    0xbce192de8a85b8ea,
    0x17d835b85bbb15f3,
    0x2f2e6163076bcfad,
    0xde4daaaca71dc9a5,
    0xa6a2506687956571,
    0xad87a3535c49ef28,
    0x32d892fad841c342,
    0x7127512f72f27cce,
    0xa7f32346f95978e3,
    0x12e0b01abb051238,
    0x15e034d40fa197ae,
    0x314dffbe0815a3b4,
    0x027990f029623981,
    0xcadcd4e59ef40c4d,
    0x9abfd8766a33735c,
    0x0e3ea96b5304a7d0,
    0xad0c42d6fc585992,
    0x187306c89bc215a9,
    0xd4a60abcf3792b95,
    0xf935451de4f21df2,
    0xa9538f0419755787,
    0xdb9acddff56ca510,
    0xd06c98cd5c0975eb,
    0xe612a3cb9ecba951,
    0xc766e62cfcadaf96,
    0xee64435a9752fe72,
    0xa192d576b245165a,
    0x0a8787bf8ecb74b2,
    0x81b3e73d20b49b6f,
    0x7fa8220ba3b2ecea,
    0x245731c13ca42499,
    0xb78dbfaf3a8d83bd,
    0xea1ad565322a1a0b,
    0x60e61c23a3795013,
    0x6606d7e446282b93,
    0x6ca4ecb15c5f91e1,
    0x9f626da15c9625f3,
    0xe51b38608ef25f57,
    0x958a324ceb064572
  ]

-- | Protocol vectors for encoding version 3. These are the DeltaHash wire image,
-- not behavioral assertions; they change only on a deliberate version bump.
digestGoldenCase :: IO ()
digestGoldenCase = do
  let emptyNodeGolden = OracleDigest 0x86f54b9300a90465 0x63a57136d0551b36
      flatGolden :: DeltaHashDigestLanes 'MerkleDigestStrategy
      flatGolden = DeltaHashDigestLanes 0xa655a16f5418af01 0xcd90c77a2dfa851c
      trieGolden :: DeltaHashDigestLanes 'MerkleDigestStrategy
      trieGolden = DeltaHashDigestLanes 0xb33b70f78e117412 0x613fb19ab70e2378
      multisetGolden :: DeltaHashDigestLanes 'MultisetDigestStrategy
      multisetGolden = DeltaHashDigestLanes 0x8cfd047cf5fb3aca 0x27f3f7852d13d309
  oracleEmptyNodeDigest @?= emptyNodeGolden
  Node.emptyDigest Node.sipHashNodeCommitment
    @?= RawDigest128 0x86f54b9300a90465 0x63a57136d0551b36
  flatDeltaHash <-
    requireRight (buildTestMerkleDeltaHash (Map.fromList [(1, 10), (2, 20)]))
  let flatDigest = Patch.merkleDeltaHashDigest flatDeltaHash
  oracleDigestLanes oracleFlatDigest @?= flatGolden
  internalDigestLanes flatDigest @?= flatGolden
  assertMerkleIdentity flatDigest
  merkleDeltaHash <-
    requireRight
      (buildTestMerkleDeltaHash (sequentialState (deltaHashFlatBoundarySize + 1)))
  let trieDigest = Patch.merkleDeltaHashDigest merkleDeltaHash
  fmap oracleDigestLanes oracleTrieDigest @?= Just trieGolden
  internalDigestLanes trieDigest @?= trieGolden
  assertMerkleIdentity trieDigest
  let multisetDigest =
        Patch.multisetDeltaHashDigest
          (buildTestMultisetDeltaHash (Map.fromList [(1, 10), (2, 20)]))
  oracleDigestLanes oracleMultisetDigest @?= multisetGolden
  internalDigestLanes multisetDigest @?= multisetGolden
  assertMultisetIdentity multisetDigest

testEncoderVersion :: DeltaHashEncoderVersion
testEncoderVersion = DeltaHashEncoderVersion 1

shiftedEncoderVersion :: DeltaHashEncoderVersion
shiftedEncoderVersion = DeltaHashEncoderVersion 2

internalDigestLanes :: Patch.DeltaHashDigest strategy -> DeltaHashDigestLanes strategy
internalDigestLanes =
  deltaHashDigestLanes

oracleDigestLanes :: OracleDigest -> DeltaHashDigestLanes strategy
oracleDigestLanes (OracleDigest lane0 lane1) =
  DeltaHashDigestLanes lane0 lane1

assertMerkleIdentity :: MerkleDeltaHashDigest -> IO ()
assertMerkleIdentity digestValue =
  Patch.decodeMerkleDeltaHashDigest testEncoderVersion (Patch.encodeDeltaHashDigest digestValue)
    @?= Right digestValue

assertMultisetIdentity :: MultisetDeltaHashDigest -> IO ()
assertMultisetIdentity digestValue =
  Patch.decodeMultisetDeltaHashDigest testEncoderVersion (Patch.encodeDeltaHashDigest digestValue)
    @?= Right digestValue

merkleObservation :: MerkleDeltaHash Int Int -> (Map Int Int, MerkleDeltaHashDigest)
merkleObservation = merkleObservationOn

merkleObservationOn ::
  Ord key =>
  MerkleDeltaHash key value ->
  (Map key value, MerkleDeltaHashDigest)
merkleObservationOn deltaHashValue =
  (Patch.merkleDeltaHashState deltaHashValue, Patch.merkleDeltaHashDigest deltaHashValue)

multisetObservation :: MultisetDeltaHash Int Int -> (Map Int Int, MultisetDeltaHashDigest)
multisetObservation deltaHashValue =
  (Patch.multisetDeltaHashState deltaHashValue, Patch.multisetDeltaHashDigest deltaHashValue)

buildTestMerkleDeltaHash ::
  Map Int Int ->
  Either (DeltaHashBuildError Int) (MerkleDeltaHash Int Int)
buildTestMerkleDeltaHash =
  Patch.buildMerkleDeltaHash testEncoderVersion intEncoding intEncoding

buildTestMultisetDeltaHash :: Map Int Int -> MultisetDeltaHash Int Int
buildTestMultisetDeltaHash =
  Patch.buildMultisetDeltaHash testEncoderVersion intEncoding intEncoding

buildTestMultisetForComposition ::
  Map Int Int ->
  Either (DeltaHashBuildError Int) (MultisetDeltaHash Int Int)
buildTestMultisetForComposition =
  Right . buildTestMultisetDeltaHash

sequentialState :: Int -> Map Int Int
sequentialState stateSize =
  Map.fromDistinctAscList (fmap (\key -> (key, key)) [0 .. stateSize - 1])

intEncoding :: Int -> StableHashEncoding
intEncoding =
  stableHashEncodingWord64LE . fromIntegral

shiftedEncoding :: Int -> StableHashEncoding
shiftedEncoding key =
  stableHashEncodingWord8 7 <> intEncoding key

boundaryCollisionEncoding :: Int -> StableHashEncoding
boundaryCollisionEncoding key
  | key == deltaHashFlatBoundarySize = intEncoding 0
  | otherwise = intEncoding key

constantEncoding :: Int -> StableHashEncoding
constantEncoding _value =
  stableHashEncodingWord64LE 0

data RepresentativeKind
  = StateRepresentative
  | PatchRepresentative
  | FinalRepresentative
  deriving stock (Eq, Ord, Show)

data RepresentativeKey = RepresentativeKey
  { representativeId :: !Int,
    representativeKind :: !RepresentativeKind
  }
  deriving stock (Show)

instance Eq RepresentativeKey where
  left == right =
    representativeId left == representativeId right

instance Ord RepresentativeKey where
  compare left right =
    compare (representativeId left) (representativeId right)

representativeKeyEncoding :: RepresentativeKey -> StableHashEncoding
representativeKeyEncoding representative =
  intEncoding (representativeId representative)
    <> representativeKindEncoding (representativeKind representative)

representativeSamePathEncoding :: RepresentativeKey -> StableHashEncoding
representativeSamePathEncoding =
  intEncoding . representativeId

data RepresentativeValue = RepresentativeValue
  { representativeValueId :: !Int,
    representativeValueKind :: !RepresentativeKind
  }
  deriving stock (Show)

instance Eq RepresentativeValue where
  left == right =
    representativeValueId left == representativeValueId right

representativeValueEncoding :: RepresentativeValue -> StableHashEncoding
representativeValueEncoding representative =
  intEncoding (representativeValueId representative)
    <> representativeKindEncoding (representativeValueKind representative)

representativeKindEncoding :: RepresentativeKind -> StableHashEncoding
representativeKindEncoding representative =
  stableHashEncodingWord64LE
    ( case representative of
        StateRepresentative -> 0
        PatchRepresentative -> 1
        FinalRepresentative -> 2
    )

-- | A key whose Patricia path is fixed by an explicit bucket while its identity
-- is a separate ordinal, letting a test route several distinct keys onto a
-- controlled cycle of shared paths.
data PathBucketKey = PathBucketKey
  { pathBucketIdent :: !Int,
    pathBucketBucket :: !Int
  }
  deriving stock (Show)

instance Eq PathBucketKey where
  left == right =
    pathBucketIdent left == pathBucketIdent right

instance Ord PathBucketKey where
  compare left right =
    compare (pathBucketIdent left) (pathBucketIdent right)

pathBucketEncoding :: PathBucketKey -> StableHashEncoding
pathBucketEncoding =
  intEncoding . pathBucketBucket

trieStateA :: Map Int Int
trieStateA =
  Map.fromList [(key, key) | key <- [0 .. deltaHashFlatBoundarySize + 64]]

trieStateB :: Map Int Int
trieStateB =
  Map.insert 9001 42 (Map.delete 5 (Map.map (+ 1) trieStateA))

trieStateC :: Map Int Int
trieStateC =
  Map.insert 9002 7 (Map.delete 10 trieStateB)

mixedFormPatch :: Patch.Patch Int Int
mixedFormPatch =
  Patch.fromAscList [(key, mixedFormCell key) | key <- [0 .. 319]]

mixedFormInitialMap :: Map Int Int
mixedFormInitialMap =
  Map.fromList
    ( [(key, mixedFormBeforeValue key) | key <- [64 .. 255]]
        <> [(key, key) | key <- [1000 .. 1000 + deltaHashFlatBoundarySize]]
    )

mixedFormMap :: Map Int Int
mixedFormMap =
  Map.fromList
    ( [(key, mixedFormAfterValue key) | key <- [0 .. 63] <> [128 .. 255]]
        <> [(key, key) | key <- [1000 .. 1000 + deltaHashFlatBoundarySize]]
    )

mixedFormCell :: Int -> Patch.CellPatch Int
mixedFormCell key
  | key < 64 = Patch.insert (mixedFormAfterValue key)
  | key < 128 = Patch.delete (mixedFormBeforeValue key)
  | key < 256 = Patch.replace (mixedFormBeforeValue key) (mixedFormAfterValue key)
  | otherwise = Patch.assertAbsent

mixedFormBeforeValue :: Int -> Int
mixedFormBeforeValue key
  | key < 128 = if key < 96 then 100 else 200
  | key < 192 = key
  | otherwise = (key * 2654435761) `mod` 4096

mixedFormAfterValue :: Int -> Int
mixedFormAfterValue key
  | key < 64 = 777
  | key < 192 = key + 1
  | otherwise = (key * 40503 + 17) `mod` 8192

smallMapGen :: Gen (Map Int Int)
smallMapGen =
  Map.fromList <$> listOf ((,) <$> chooseInt (-32, 32) <*> chooseInt (-1000, 1000))

-- | Generates maps strictly larger than the flat boundary, so every property
-- using it exercises trie mode. Strictly increasing keys guarantee the size.
largeMapGen :: Gen (Map Int Int)
largeMapGen = do
  entryCount <- chooseInt (deltaHashFlatBoundarySize + 1, deltaHashFlatBoundarySize + 128)
  gaps <- vectorOf entryCount (chooseInt (1, 7))
  values <- vectorOf entryCount (chooseInt (-4096, 4096))
  let keys = scanl1 (+) gaps
  pure (Map.fromList (zip keys values))

deltaHashFlatBoundarySize :: Int
deltaHashFlatBoundarySize =
  256

requireRight :: Show obstruction => Either obstruction value -> IO value
requireRight result =
  case result of
    Left obstruction ->
      assertFailure (show obstruction)
    Right value ->
      pure value

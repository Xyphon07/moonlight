{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DeltaLaws
  ( deltaNormalizeLaws,
    deltaSupportLaws,
    frontierLaws,
    signedLaws,
    functorLaws,
    monotoneLaws,
  )
where

import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Core
  ( PartialOrder (..),
  )
import LawManifest
  ( lawManifestCase,
    lawProperty,
  )
import Moonlight.Delta.Frontier
import Moonlight.Delta.Monotone
import Moonlight.Delta.Normalize
import Moonlight.Delta.Signed
import Moonlight.Delta.Support
import Test.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    forAll,
    (===),
  )
import Test.Tasty (TestTree, testGroup)

data DeltaNormalizeLaw
  = DeltaNormalizeIdempotent
  | DeltaNormalizeNullStable
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName DeltaNormalizeLaw where
  lawNameText =
    constructorLawName . show

data DeltaSupportLaw
  = DeltaSupportNormalizeStable
  | DeltaSupportNullDeltaEmpty
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName DeltaSupportLaw where
  lawNameText =
    constructorLawName . show

data FrontierLaw
  = FrontierLowerAntichain
  | FrontierLowerCoversInput
  | FrontierLowerCanonical
  | FrontierLowerInputOrderInvariant
  | FrontierLowerObservationOrderInvariant
  | FrontierUpperAntichain
  | FrontierUpperCoversInput
  | FrontierUpperCanonical
  | FrontierUpperInputOrderInvariant
  | FrontierUpperObservationOrderInvariant
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName FrontierLaw where
  lawNameText =
    constructorLawName . show

data SignedLaw
  = SignedCanonicalByConstruction
  | SignedIdentityCombination
  | SignedCombinationAssociative
  | SignedCombinationApplicationSequential
  | SignedInverseCancellation
  | SignedApplySuccessUpdatesStateAndDeletesZero
  | SignedUnderflowRejected
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName SignedLaw where
  lawNameText =
    constructorLawName . show

data FunctorLaw
  = FunctorIdentity
  | FunctorComposition
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName FunctorLaw where
  lawNameText =
    constructorLawName . show

data MonotoneLaw
  = MonotoneCompositionSequentialApplication
  | MonotoneResetLeftAbsorption
  | MonotoneResetApplication
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName MonotoneLaw where
  lawNameText =
    constructorLawName . show

deltaNormalizeLaws ::
  forall delta.
  (DeltaNormalize delta, Eq delta, Show delta) =>
  String ->
  Gen delta ->
  TestTree
deltaNormalizeLaws label deltaGen =
  testGroup
    label
    [ lawManifestCase label ([minBound .. maxBound] :: [DeltaNormalizeLaw]),
      lawProperty DeltaNormalizeIdempotent $ forAll deltaGen $ \deltaValue ->
        normalizeDelta (normalizeDelta deltaValue) === normalizeDelta deltaValue,
      lawProperty DeltaNormalizeNullStable $ forAll deltaGen $ \deltaValue ->
        deltaNull (normalizeDelta deltaValue) === deltaNull deltaValue
    ]

deltaSupportLaws ::
  forall delta.
  ( DeltaNormalize delta,
    DeltaSupport delta,
    Eq (DeltaSupportSet delta),
    Show (DeltaSupportSet delta),
    Show delta
  ) =>
  String ->
  Gen delta ->
  Gen delta ->
  TestTree
deltaSupportLaws label deltaGen nullDeltaGen =
  testGroup
    label
    [ lawManifestCase label ([minBound .. maxBound] :: [DeltaSupportLaw]),
      lawProperty DeltaSupportNormalizeStable $ forAll deltaGen $ \deltaValue ->
        deltaSupport (normalizeDelta deltaValue) === deltaSupport deltaValue,
      lawProperty DeltaSupportNullDeltaEmpty $ forAll nullDeltaGen $ \deltaValue ->
        let normalized = normalizeDelta deltaValue
         in (deltaNull normalized, deltaSupport normalized)
              === (True, emptySupport @delta)
    ]

frontierLaws ::
  forall time.
  (Ord time, PartialOrder time, Eq time, Show time) =>
  String ->
  Gen [time] ->
  TestTree
frontierLaws label timesGen =
  testGroup
    label
    [ lawManifestCase label ([minBound .. maxBound] :: [FrontierLaw]),
      lawProperty FrontierLowerAntichain $ forAll timesGen $ \times ->
        frontierIsAntichain (frontierPoints (mkFrontier times)) === True,
      lawProperty FrontierLowerCoversInput $ forAll timesGen $ \times ->
        lowerFrontierCovers times (mkFrontier times) === True,
      lawProperty FrontierLowerCanonical $ forAll timesGen $ \times ->
        mkFrontier (frontierPoints (mkFrontier times)) === mkFrontier times,
      lawProperty FrontierLowerInputOrderInvariant $ forAll timesGen $ \times ->
        mkFrontier (reverse times) === mkFrontier times,
      lawProperty FrontierLowerObservationOrderInvariant $ forAll timesGen $ \times ->
        let forward = mkFrontier times
            backward = mkFrontier (reverse times)
         in (frontierPoints forward, show forward) === (frontierPoints backward, show backward),
      lawProperty FrontierUpperAntichain $ forAll timesGen $ \times ->
        frontierIsAntichain (upperFrontierPoints (mkUpperFrontier times)) === True,
      lawProperty FrontierUpperCoversInput $ forAll timesGen $ \times ->
        upperFrontierCovers times (mkUpperFrontier times) === True,
      lawProperty FrontierUpperCanonical $ forAll timesGen $ \times ->
        mkUpperFrontier (upperFrontierPoints (mkUpperFrontier times)) === mkUpperFrontier times,
      lawProperty FrontierUpperInputOrderInvariant $ forAll timesGen $ \times ->
        mkUpperFrontier (reverse times) === mkUpperFrontier times,
      lawProperty FrontierUpperObservationOrderInvariant $ forAll timesGen $ \times ->
        let forward = mkUpperFrontier times
            backward = mkUpperFrontier (reverse times)
         in (upperFrontierPoints forward, show forward) === (upperFrontierPoints backward, show backward)
    ]

signedLaws ::
  forall key.
  (Ord key, Show key) =>
  String ->
  Gen [(key, Int)] ->
  Gen key ->
  TestTree
signedLaws label entriesGen keyGen =
  testGroup
    label
    [ lawManifestCase label ([minBound .. maxBound] :: [SignedLaw]),
      deltaNormalizeLaws "normalize" (signedFromList <$> entriesGen),
      lawProperty SignedCanonicalByConstruction $ forAll signedCanonicalGen signedCanonicalByConstruction,
      lawProperty SignedIdentityCombination $ forAll entriesGen signedIdentity,
      lawProperty SignedCombinationAssociative $ forAll signedTripleGen signedAssociativity,
      lawProperty SignedCombinationApplicationSequential $ forAll signedSequentialApplicationGen signedCombinationApplicationSequential,
      lawProperty SignedInverseCancellation $ forAll entriesGen signedInverse,
      lawProperty SignedApplySuccessUpdatesStateAndDeletesZero $ forAll signedApplySuccessGen signedApplySuccess,
      lawProperty SignedUnderflowRejected $ forAll signedUnderflowGen signedUnderflow
    ]
  where
    signedTripleGen :: Gen ([(key, Int)], [(key, Int)], [(key, Int)])
    signedTripleGen =
      (,,) <$> entriesGen <*> entriesGen <*> entriesGen

    signedCanonicalGen :: Gen (key, [(key, Int)], [(key, Int)], [(key, Int)])
    signedCanonicalGen =
      (,,,) <$> keyGen <*> entriesGen <*> entriesGen <*> entriesGen

    signedUnderflowGen :: Gen (key, Int)
    signedUnderflowGen =
      (,) <$> keyGen <*> chooseInt (1, 64)

    signedCanonicalByConstruction :: (key, [(key, Int)], [(key, Int)], [(key, Int)]) -> Property
    signedCanonicalByConstruction (canonicalKey, olderEntries, middleEntries, newerEntries) =
      let older = signedFromList olderEntries
          middle = signedFromList middleEntries
          newer = signedFromList newerEntries
          combined = combineSigned newer (combineSigned middle older)
          negated = negateSigned combined
          fromChangeMap =
            signedFromChangeMap
              ( Map.fromList
                  (fmap (\(key, amount) -> (key, MultiplicityChange (fromIntegral amount))) olderEntries)
              )
          mapped = mapSignedKeys (const canonicalKey) combined
          traversed =
            traverseSignedKeysWith
              (\_ -> Right canonicalKey :: Either () key)
              combined
       in conjoin
            [ signedCanonicalRows (emptySigned :: Signed key),
              signedCanonicalRows (singletonSigned canonicalKey 0),
              signedCanonicalRows (singletonSigned canonicalKey 1),
              signedCanonicalRows older,
              signedCanonicalRows fromChangeMap,
              signedCanonicalRows combined,
              signedCanonicalRows negated,
              signedCanonicalRows mapped,
              fmap signedToAscList traversed === Right (signedToAscList mapped)
            ]

    signedCanonicalRows :: Signed key -> Property
    signedCanonicalRows signedValue =
      let rows = signedToAscList signedValue
          canonicalRows =
            Map.toAscList
              ( Map.filter
                  (/= zeroMultiplicityChange)
                  (Map.fromListWith addMultiplicityChange rows)
              )
       in rows === canonicalRows

    signedSequentialApplicationGen :: Gen ([(key, Int)], [(key, Int)], [(key, Int)])
    signedSequentialApplicationGen =
      (,,) <$> entriesGen <*> entriesGen <*> entriesGen

    signedApplySuccessGen :: Gen (key, Int, Int, Int, Int)
    signedApplySuccessGen =
      (,,,,)
        <$> keyGen
        <*> chooseInt (1, 64)
        <*> chooseInt (1, 64)
        <*> chooseInt (1, 64)
        <*> chooseInt (1, 64)

    signedIdentity :: [(key, Int)] -> Property
    signedIdentity entries =
      let deltaValue = signedFromList entries
       in ( combineSigned emptySigned deltaValue,
            combineSigned deltaValue emptySigned
          )
            === (deltaValue, deltaValue)

    signedAssociativity :: ([(key, Int)], [(key, Int)], [(key, Int)]) -> Property
    signedAssociativity (olderEntries, middleEntries, newerEntries) =
      let older = signedFromList olderEntries
          middle = signedFromList middleEntries
          newer = signedFromList newerEntries
       in combineSigned newer (combineSigned middle older)
            === combineSigned (combineSigned newer middle) older

    signedCombinationApplicationSequential :: ([(key, Int)], [(key, Int)], [(key, Int)]) -> Property
    signedCombinationApplicationSequential (olderEntries, newerEntries, stateEntries) =
      let older = signedFromList olderEntries
          newer = signedFromList newerEntries
          state = signedSequentialState olderEntries newerEntries stateEntries
       in applySignedToMap (combineSigned newer older) state
            === (applySignedToMap older state >>= applySignedToMap newer)

    signedInverse :: [(key, Int)] -> Property
    signedInverse entries =
      let deltaValue = signedFromList entries
       in combineSigned (negateSigned deltaValue) deltaValue
            === emptySigned

    signedApplySuccess :: (key, Int, Int, Int, Int) -> Property
    signedApplySuccess (key, insertAmount, incrementStart, incrementAmount, deleteStart) =
      let observed =
            ( fmap (fmap multiplicityValue) $
                applySignedToMap
                  (signedFromList [(key, insertAmount)])
                  Map.empty,
              fmap (fmap multiplicityValue) $
                applySignedToMap
                  (signedFromList [(key, incrementAmount)])
                  (Map.singleton key (Multiplicity (fromIntegral incrementStart))),
              fmap (fmap multiplicityValue) $
                applySignedToMap
                  (signedFromList [(key, negate deleteStart)])
                  (Map.singleton key (Multiplicity (fromIntegral deleteStart)))
            )
          expected =
            ( Right (Map.singleton key (fromIntegral insertAmount)),
              Right (Map.singleton key (fromIntegral (incrementStart + incrementAmount))),
              Right Map.empty
            )
       in observed === expected

    signedSequentialState :: [(key, Int)] -> [(key, Int)] -> [(key, Int)] -> Map key Multiplicity
    signedSequentialState olderEntries newerEntries stateEntries =
      let baseState = signedStateFromEntries stateEntries
          olderTotals = signedEntryTotals olderEntries
          newerTotals = signedEntryTotals newerEntries
          deltaKeys = Map.keys (Map.union olderTotals newerTotals)
          requirements =
            Map.fromList
              [ (key, Multiplicity (fromIntegral required))
                | key <- deltaKeys,
                  let olderChange = Map.findWithDefault 0 key olderTotals,
                  let newerChange = Map.findWithDefault 0 key newerTotals,
                  let required = max 0 (max (negate olderChange) (negate (olderChange + newerChange))),
                  required > 0
              ]
       in Map.unionWith max baseState requirements

    signedStateFromEntries :: [(key, Int)] -> Map key Multiplicity
    signedStateFromEntries entries =
      Map.fromList
        [ (key, Multiplicity (fromIntegral amount))
          | (key, rawAmount) <- Map.toList (signedEntryTotals entries),
            let amount = abs rawAmount,
            amount > 0
        ]

    signedEntryTotals :: [(key, Int)] -> Map key Int
    signedEntryTotals =
      Map.fromListWith (+)

    signedUnderflow :: (key, Int) -> Property
    signedUnderflow (key, amount) =
      applySignedToMap
        (singletonSigned key (negate amount))
        (Map.singleton key (Multiplicity (fromIntegral (amount - 1))))
        === Left
          SignedMultiplicityUnderflow
            { saeKey = key,
              saeOldMultiplicity = Multiplicity (fromIntegral (amount - 1)),
              saeDeltaMultiplicity = MultiplicityChange (fromIntegral (negate amount))
            }

functorLaws ::
  forall f.
  (Functor f, Eq (f Int), Show (f Int)) =>
  String ->
  Gen (f Int) ->
  TestTree
functorLaws label valueGen =
  testGroup
    label
    [ lawManifestCase label ([minBound .. maxBound] :: [FunctorLaw]),
      lawProperty FunctorIdentity $ forAll valueGen $ \value ->
        fmap identityInt value === value,
      lawProperty FunctorComposition $ forAll valueGen $ \value ->
        fmap (incrementInt . doubleInt) value === fmapSequential value
    ]

identityInt :: Int -> Int
identityInt value =
  value

doubleInt :: Int -> Int
doubleInt value =
  value * 2

incrementInt :: Int -> Int
incrementInt value =
  value + 1

fmapSequential :: Functor f => f Int -> f Int
fmapSequential value =
  let doubled = fmap doubleInt value
   in fmap incrementInt doubled

monotoneLaws ::
  forall value.
  (Eq value, Show value) =>
  String ->
  (value -> value -> value) ->
  Gen (Monotone value, Monotone value, value) ->
  TestTree
monotoneLaws label joinValue sectionGen =
  testGroup
    label
    [ lawManifestCase label ([minBound .. maxBound] :: [MonotoneLaw]),
      lawProperty MonotoneCompositionSequentialApplication $ forAll sectionGen $ \(newer, older, stateValue) ->
        applyDelta joinValue (composeDelta joinValue newer older) stateValue
          === applyDelta joinValue newer (applyDelta joinValue older stateValue),
      lawProperty MonotoneResetLeftAbsorption $ forAll sectionGen $ \(newer, older, _stateValue) ->
        let target =
              monotoneDeltaValue newer
         in composeDelta joinValue (ResetDelta target) older
              === ResetDelta target,
      lawProperty MonotoneResetApplication $ forAll sectionGen $ \(newer, _older, stateValue) ->
        let target =
              monotoneDeltaValue newer
         in applyDelta joinValue (ResetDelta target) stateValue
              === target
    ]

monotoneDeltaValue :: Monotone value -> value
monotoneDeltaValue delta =
  case delta of
    JoinDelta value ->
      value
    ResetDelta value ->
      value

frontierIsAntichain :: (PartialOrder time, Eq time) => [time] -> Bool
frontierIsAntichain points =
  Foldable.all
    (\left ->
       Foldable.all
        (\right -> left == right || (not (left `leq` right) && not (right `leq` left)))
        points)
    points

lowerFrontierCovers :: PartialOrder time => [time] -> Frontier time -> Bool
lowerFrontierCovers times frontier =
  Foldable.all
    (\time -> Foldable.any (`leq` time) (frontierPoints frontier))
    times

upperFrontierCovers :: PartialOrder time => [time] -> UpperFrontier time -> Bool
upperFrontierCovers times frontier =
  Foldable.all
    (\time -> Foldable.any (time `leq`) (upperFrontierPoints frontier))
    times

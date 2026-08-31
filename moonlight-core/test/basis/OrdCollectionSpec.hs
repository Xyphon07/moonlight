{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module OrdCollectionSpec (tests) where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Core (OrdMap (..), OrdSet (..))
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, Property, (===), (.&&.))

data OrdCollectionLaw
  = OrdSetUnionIdentity
  | OrdSetUnionAssociative
  | OrdSetIntersectionAbsorption
  | OrdSetDifferencePartition
  | OrdSetMembershipCoherent
  | OrdSetUnionsCoherent
  | OrdSetProjectionSizeCoherent
  | OrdMapProjectionRoundTrip
  | OrdMapInsertLookup
  | OrdMapInsertWithAccumulates
  | OrdMapDeleteRemoves
  | OrdMapUnionWithAssociative
  | OrdMapUnionWithCommutativeForCommutativeCombine
  | OrdMapFromListWithAccumulates
  | OrdMapProjectionSizeCoherent
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName OrdCollectionLaw where
  lawNameText =
    constructorLawName . show

tests :: TestTree
tests =
  testGroup
    "OrdCollection"
    [ testGroup "IntSet" (ordSetLaws @IntSet),
      testGroup "Set Int" (ordSetLaws @(Set Int)),
      testGroup "IntMap Int" (ordMapLaws @(IntMap Int)),
      testGroup "Map Int Int" (ordMapLaws @(Map Int Int))
    ]

ordSetLaws ::
  forall set.
  ( Arbitrary (SetKey set),
    Eq set,
    Eq (SetKey set),
    OrdSet set,
    Show set,
    Show (SetKey set)
  ) =>
  [TestTree]
ordSetLaws =
  [ lawProperty OrdSetUnionIdentity (propOrdSetUnionIdentity @set),
    lawProperty OrdSetUnionAssociative (propOrdSetUnionAssociative @set),
    lawProperty OrdSetIntersectionAbsorption (propOrdSetIntersectionAbsorption @set),
    lawProperty OrdSetDifferencePartition (propOrdSetDifferencePartition @set),
    lawProperty OrdSetMembershipCoherent (propOrdSetMembershipCoherent @set),
    lawProperty OrdSetUnionsCoherent (propOrdSetUnionsCoherent @set),
    lawProperty OrdSetProjectionSizeCoherent (propOrdSetProjectionSizeCoherent @set)
  ]

ordMapLaws ::
  forall map.
  ( Eq map,
    Eq (MapKey map),
    Eq (MapValue map),
    Arbitrary (MapKey map),
    Arbitrary (MapValue map),
    Num (MapValue map),
    OrdMap map,
    Show map,
    Show (MapKey map),
    Show (MapValue map)
  ) =>
  [TestTree]
ordMapLaws =
  [ lawProperty OrdMapProjectionRoundTrip (propOrdMapProjectionRoundTrip @map),
    lawProperty OrdMapInsertLookup (propOrdMapInsertLookup @map),
    lawProperty OrdMapInsertWithAccumulates (propOrdMapInsertWithAccumulates @map),
    lawProperty OrdMapDeleteRemoves (propOrdMapDeleteRemoves @map),
    lawProperty OrdMapUnionWithAssociative (propOrdMapUnionWithAssociative @map),
    lawProperty OrdMapUnionWithCommutativeForCommutativeCombine (propOrdMapUnionWithCommutative @map),
    lawProperty OrdMapFromListWithAccumulates (propOrdMapFromListWithAccumulates @map),
    lawProperty OrdMapProjectionSizeCoherent (propOrdMapProjectionSizeCoherent @map)
  ]

propOrdSetUnionIdentity ::
  forall set.
  (Eq set, OrdSet set, Show set) =>
  [SetKey set] ->
  Property
propOrdSetUnionIdentity values =
  unionSet emptySet setValue === setValue
    .&&. unionSet setValue emptySet === setValue
  where
    setValue =
      fromListSet values :: set

propOrdSetUnionAssociative ::
  forall set.
  (Eq set, OrdSet set, Show set) =>
  [SetKey set] ->
  [SetKey set] ->
  [SetKey set] ->
  Property
propOrdSetUnionAssociative leftValues middleValues rightValues =
  unionSet leftValue (unionSet middleValue rightValue)
    === unionSet (unionSet leftValue middleValue) rightValue
  where
    leftValue =
      fromListSet leftValues :: set
    middleValue =
      fromListSet middleValues :: set
    rightValue =
      fromListSet rightValues :: set

propOrdSetIntersectionAbsorption ::
  forall set.
  (Eq set, OrdSet set, Show set) =>
  [SetKey set] ->
  [SetKey set] ->
  Property
propOrdSetIntersectionAbsorption leftValues rightValues =
  intersectionSet leftValue (unionSet leftValue rightValue) === leftValue
  where
    leftValue =
      fromListSet leftValues :: set
    rightValue =
      fromListSet rightValues :: set

propOrdSetDifferencePartition ::
  forall set.
  (Eq set, OrdSet set, Show set) =>
  [SetKey set] ->
  [SetKey set] ->
  Property
propOrdSetDifferencePartition leftValues rightValues =
  intersectionSet (differenceSet leftValue rightValue) rightValue === emptySet
    .&&. unionSet (differenceSet leftValue rightValue) (intersectionSet leftValue rightValue) === leftValue
  where
    leftValue =
      fromListSet leftValues :: set
    rightValue =
      fromListSet rightValues :: set

propOrdSetMembershipCoherent ::
  forall set.
  (Eq (SetKey set), OrdSet set) =>
  SetKey set ->
  [SetKey set] ->
  Property
propOrdSetMembershipCoherent key values =
  memberSet key setValue === elem key (toAscListSet setValue)
  where
    setValue =
      fromListSet values :: set

propOrdSetUnionsCoherent ::
  forall set.
  (Eq set, OrdSet set, Show set) =>
  [[SetKey set]] ->
  Property
propOrdSetUnionsCoherent values =
  unionsSet setValues === foldr unionSet emptySet setValues
  where
    setValues =
      fromListSet <$> values :: [set]

propOrdSetProjectionSizeCoherent ::
  forall set.
  OrdSet set =>
  [SetKey set] ->
  Property
propOrdSetProjectionSizeCoherent values =
  sizeSet setValue === length ascendingValues
    .&&. nullSet setValue === null ascendingValues
  where
    setValue =
      fromListSet values :: set
    ascendingValues =
      toAscListSet setValue

propOrdMapProjectionRoundTrip ::
  forall map.
  (Eq map, OrdMap map, Show map) =>
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapProjectionRoundTrip entries =
  fromListMap (toAscListMap mapValue) === mapValue
  where
    mapValue =
      fromListMap entries :: map

propOrdMapInsertLookup ::
  forall map.
  (Eq (MapValue map), OrdMap map, Show (MapValue map)) =>
  MapKey map ->
  MapValue map ->
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapInsertLookup key value entries =
  lookupMap key (insertMap key value mapValue) === Just value
  where
    mapValue =
      fromListMap entries :: map

propOrdMapInsertWithAccumulates ::
  forall map.
  (Eq (MapValue map), Num (MapValue map), OrdMap map, Show (MapValue map)) =>
  MapKey map ->
  MapValue map ->
  MapValue map ->
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapInsertWithAccumulates key newValue oldValue entries =
  lookupMap key (insertWithMap (+) key newValue seededMap) === Just (newValue + oldValue)
  where
    seededMap =
      insertMap key oldValue (fromListMap entries :: map)

propOrdMapDeleteRemoves ::
  forall map.
  (Eq (MapValue map), OrdMap map, Show (MapValue map)) =>
  MapKey map ->
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapDeleteRemoves key entries =
  lookupMap key (deleteMap key mapValue) === Nothing
  where
    mapValue =
      fromListMap entries :: map

propOrdMapUnionWithAssociative ::
  forall map.
  (Eq map, Num (MapValue map), OrdMap map, Show map) =>
  [(MapKey map, MapValue map)] ->
  [(MapKey map, MapValue map)] ->
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapUnionWithAssociative leftEntries middleEntries rightEntries =
  unionWithMap (+) leftValue (unionWithMap (+) middleValue rightValue)
    === unionWithMap (+) (unionWithMap (+) leftValue middleValue) rightValue
  where
    leftValue =
      fromListMap leftEntries :: map
    middleValue =
      fromListMap middleEntries :: map
    rightValue =
      fromListMap rightEntries :: map

propOrdMapUnionWithCommutative ::
  forall map.
  (Eq map, Num (MapValue map), OrdMap map, Show map) =>
  [(MapKey map, MapValue map)] ->
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapUnionWithCommutative leftEntries rightEntries =
  unionWithMap (+) leftValue rightValue === unionWithMap (+) rightValue leftValue
  where
    leftValue =
      fromListMap leftEntries :: map
    rightValue =
      fromListMap rightEntries :: map

propOrdMapFromListWithAccumulates ::
  forall map.
  (Eq (MapKey map), Eq (MapValue map), Num (MapValue map), OrdMap map, Show (MapValue map)) =>
  MapKey map ->
  MapValue map ->
  MapValue map ->
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapFromListWithAccumulates key leftValue rightValue entries =
  lookupMap key (fromListWithMap (+) seededEntries :: map) === Just expectedValue
  where
    seededEntries =
      (key, leftValue) : (key, rightValue) : entries
    expectedValue =
      sum (matchingValue <$> seededEntries)
    matchingValue (candidateKey, value)
      | candidateKey == key = value
      | otherwise = 0

propOrdMapProjectionSizeCoherent ::
  forall map.
  OrdMap map =>
  [(MapKey map, MapValue map)] ->
  Property
propOrdMapProjectionSizeCoherent entries =
  sizeMap mapValue === length ascendingEntries
    .&&. nullMap mapValue === null ascendingEntries
  where
    mapValue =
      fromListMap entries :: map
    ascendingEntries =
      toAscListMap mapValue

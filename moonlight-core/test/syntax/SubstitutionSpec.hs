{-# LANGUAGE DerivingStrategies #-}

module SubstitutionSpec (tests) where

import Control.Monad (foldM)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust, mapMaybe)
import Moonlight.Core (ClassId (..), PatternVar, mkPatternVar)
import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Core
  ( Substitution (..),
    extendSubst,
    intersectRootedMatches,
    insertSubst,
    lookupSubst,
    mergeSubstitutions,
  )
import LawProperty (lawProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    counterexample,
    listOf,
    property,
    resize,
    vectorOf,
    (===),
    (.&&.),
  )

data SubstitutionLaw
  = SubstitutionExtendAgreement
  | SubstitutionMergeIdempotent
  | SubstitutionMergeCommutative
  | SubstitutionMergeAssociative
  | SubstitutionMergeAgreement
  | SubstitutionRootedMatchIntersectionFiberProduct
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName SubstitutionLaw where
  lawNameText =
    constructorLawName . show

newtype SmallPatternVar = SmallPatternVar PatternVar
  deriving stock (Eq, Ord, Show)

newtype SmallClassId = SmallClassId ClassId
  deriving stock (Eq, Ord, Show)

newtype SmallSubstitution = SmallSubstitution Substitution
  deriving stock (Eq, Ord, Show)

newtype RootedMatchSets = RootedMatchSets (NonEmpty [(ClassId, Substitution)])
  deriving stock (Eq, Show)

instance Arbitrary SmallPatternVar where
  arbitrary =
    SmallPatternVar <$> genPatternVar

instance Arbitrary SmallClassId where
  arbitrary =
    SmallClassId <$> genClassId

instance Arbitrary SmallSubstitution where
  arbitrary =
    SmallSubstitution <$> genSubstitution

instance Arbitrary RootedMatchSets where
  arbitrary =
    RootedMatchSets <$> genRootedMatchSets

tests :: TestTree
tests =
  testGroup
    "Substitution"
    [ lawProperty SubstitutionExtendAgreement propExtendAgreement,
      lawProperty SubstitutionMergeIdempotent propMergeIdempotent,
      lawProperty SubstitutionMergeCommutative propMergeCommutative,
      lawProperty SubstitutionMergeAssociative propMergeAssociative,
      lawProperty SubstitutionMergeAgreement propMergeAgreement,
      lawProperty SubstitutionRootedMatchIntersectionFiberProduct propRootedMatchIntersectionFiberProduct,
      testCase "rooted match intersection preserves left-major multiplicity inside class fibers" testRootedMatchIntersectionFiberOrder
    ]

propExtendAgreement :: SmallPatternVar -> SmallClassId -> SmallSubstitution -> Property
propExtendAgreement (SmallPatternVar patternVar) (SmallClassId classId) (SmallSubstitution substitution) =
  counterexample "extension success does not match agreement with the existing binding" (isJust extended === agrees)
    .&&. counterexample "successful extension does not contain the requested binding" (property resultContainsBinding)
  where
    extended =
      extendSubst patternVar classId substitution
    agrees =
      maybe True (== classId) (lookupSubst patternVar substitution)
    resultContainsBinding =
      maybe True ((== Just classId) . lookupSubst patternVar) extended

propMergeIdempotent :: SmallSubstitution -> Property
propMergeIdempotent (SmallSubstitution substitution) =
  mergeSubstitutions substitution substitution === Just substitution

propMergeCommutative :: SmallSubstitution -> SmallSubstitution -> Property
propMergeCommutative (SmallSubstitution left) (SmallSubstitution right) =
  mergeSubstitutions left right === mergeSubstitutions right left

propMergeAssociative :: SmallSubstitution -> SmallSubstitution -> SmallSubstitution -> Property
propMergeAssociative (SmallSubstitution left) (SmallSubstitution middle) (SmallSubstitution right) =
  mergeLeftFirst === mergeRightFirst
  where
    mergeLeftFirst =
      mergeSubstitutions left middle >>= \mergedLeft ->
        mergeSubstitutions mergedLeft right
    mergeRightFirst =
      mergeSubstitutions middle right >>= \mergedRight ->
        mergeSubstitutions left mergedRight

propMergeAgreement :: SmallSubstitution -> SmallSubstitution -> Property
propMergeAgreement (SmallSubstitution left) (SmallSubstitution right) =
  mergeSubstitutions left right === expectedMerge left right

propRootedMatchIntersectionFiberProduct :: RootedMatchSets -> Property
propRootedMatchIntersectionFiberProduct (RootedMatchSets rootedMatches) =
  intersectRootedMatches rootedMatches === expectedIntersectRootedMatches rootedMatches

testRootedMatchIntersectionFiberOrder :: IO ()
testRootedMatchIntersectionFiberOrder =
  intersectRootedMatches (leftMatches :| [rightMatches])
    @?= [ (ClassId 1, insertSubst (mkPatternVar 1) (ClassId 10) emptyLeft),
          (ClassId 1, insertSubst (mkPatternVar 2) (ClassId 20) emptyLeft),
          (ClassId 2, insertSubst (mkPatternVar 3) (ClassId 30) emptyLeft),
          (ClassId 1, insertSubst (mkPatternVar 1) (ClassId 10) emptyLeft),
          (ClassId 1, insertSubst (mkPatternVar 2) (ClassId 20) emptyLeft)
        ]
  where
    emptyLeft =
      Substitution IntMap.empty
    leftMatches =
      [(ClassId 1, emptyLeft), (ClassId 2, emptyLeft), (ClassId 1, emptyLeft)]
    rightMatches =
      [ (ClassId 1, insertSubst (mkPatternVar 1) (ClassId 10) emptyLeft),
        (ClassId 2, insertSubst (mkPatternVar 3) (ClassId 30) emptyLeft),
        (ClassId 1, insertSubst (mkPatternVar 2) (ClassId 20) emptyLeft)
      ]

expectedMerge :: Substitution -> Substitution -> Maybe Substitution
expectedMerge left right =
  if substitutionsAgree left right
    then Just (unionSubstitutions left right)
    else Nothing

substitutionsAgree :: Substitution -> Substitution -> Bool
substitutionsAgree (Substitution left) (Substitution right) =
  and (IntMap.intersectionWith (==) left right)

unionSubstitutions :: Substitution -> Substitution -> Substitution
unionSubstitutions (Substitution left) (Substitution right) =
  Substitution (IntMap.union left right)

expectedIntersectRootedMatches :: NonEmpty [(ClassId, Substitution)] -> [(ClassId, Substitution)]
expectedIntersectRootedMatches rootedMatches =
  mapMaybe rootedCombination (sequenceA (NonEmpty.toList rootedMatches))

rootedCombination :: [(ClassId, Substitution)] -> Maybe (ClassId, Substitution)
rootedCombination matches =
  case matches of
    [] ->
      Nothing
    (rootClassId, rootSubstitution) : remainingMatches
      | all ((== rootClassId) . fst) remainingMatches ->
          fmap
            (\mergedSubstitution -> (rootClassId, mergedSubstitution))
            (foldM mergeSubstitutions rootSubstitution (snd <$> remainingMatches))
      | otherwise ->
          Nothing

genPatternVar :: Gen PatternVar
genPatternVar =
  mkPatternVar <$> chooseInt (0, 5)

genClassId :: Gen ClassId
genClassId =
  ClassId <$> chooseInt (0, 5)

genSubstitution :: Gen Substitution
genSubstitution =
  Substitution . IntMap.fromList <$> resize 6 (listOf genEntry)

genEntry :: Gen (Int, ClassId)
genEntry =
  (,) <$> chooseInt (0, 5) <*> genClassId

genRootedMatchSets :: Gen (NonEmpty [(ClassId, Substitution)])
genRootedMatchSets =
  (:|)
    <$> genRootedMatchList
    <*> (chooseInt (0, 3) >>= \count -> vectorOf count genRootedMatchList)

genRootedMatchList :: Gen [(ClassId, Substitution)]
genRootedMatchList =
  chooseInt (0, 4) >>= \count ->
    vectorOf count genRootedMatch

genRootedMatch :: Gen (ClassId, Substitution)
genRootedMatch =
  (,) <$> genClassId <*> genSubstitution

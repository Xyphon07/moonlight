{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}

module TheorySpec (tests) where

import Data.Set qualified as Set
import Moonlight.Core qualified as EGraph
import Moonlight.Core (IsLawName (..), constructorLawName)
import Moonlight.Core (Pattern (..))
import LawProperty (lawProperty)
import Moonlight.Core
  ( StructuralLaw (..),
    TheorySpec (..),
    canonicalizeLayerByTheory,
    canonicalizePatternByTheory,
    commutativeBinary,
    expandPatternByTheory,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    chooseInt,
    counterexample,
    frequency,
    oneof,
    property,
    sized,
    (===),
  )

data TheoryLaw
  = TheoryCanonicalizationIdempotent
  | TheoryCanonicalChoiceDeterministicByTermOrder
  | TheoryExpansionExactCommutativeOrbit
  | TheoryExpansionDuplicateFree
  | TheoryExpansionContainsCanonicalForm
  deriving stock (Bounded, Enum, Eq, Ord, Show)

instance IsLawName TheoryLaw where
  lawNameText =
    constructorLawName . show

data TheoryNode a
  = TheoryLeaf !Int
  | TheoryUnary !a
  | TheoryAdd !a !a
  | TheoryPair !a !a
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype SmallTheoryNode = SmallTheoryNode (TheoryNode Int)
  deriving stock (Eq, Ord, Show)

newtype SmallTheoryPattern = SmallTheoryPattern (Pattern TheoryNode)
  deriving stock (Eq, Ord, Show)

instance Arbitrary SmallTheoryNode where
  arbitrary =
    SmallTheoryNode <$> genTheoryNode genChild

instance Arbitrary SmallTheoryPattern where
  arbitrary =
    SmallTheoryPattern <$> sized (genPattern . min 5)

tests :: TestTree
tests =
  testGroup
    "Theory"
    [ lawProperty TheoryCanonicalizationIdempotent propCanonicalizationIdempotent,
      lawProperty TheoryCanonicalChoiceDeterministicByTermOrder propCanonicalChoiceDeterministicByTermOrder,
      lawProperty TheoryExpansionExactCommutativeOrbit propExpansionExactCommutativeOrbit,
      lawProperty TheoryExpansionDuplicateFree propExpansionDuplicateFree,
      lawProperty TheoryExpansionContainsCanonicalForm propExpansionContainsCanonicalForm,
      testCase "recursive canonicalization orients pattern variables before pattern nodes" testRecursiveCanonicalizationDeclarationOrder
    ]

propCanonicalizationIdempotent :: SmallTheoryNode -> Property
propCanonicalizationIdempotent (SmallTheoryNode node) =
  canonicalizeLayerByTheory commutativeTheory (canonicalizeLayerByTheory commutativeTheory node)
    === canonicalizeLayerByTheory commutativeTheory node

propCanonicalChoiceDeterministicByTermOrder :: SmallTheoryNode -> Property
propCanonicalChoiceDeterministicByTermOrder (SmallTheoryNode node) =
  canonicalizeLayerByTheory commutativeTheory node === expectedCanonicalNode node

propExpansionExactCommutativeOrbit :: SmallTheoryPattern -> Property
propExpansionExactCommutativeOrbit (SmallTheoryPattern patternValue) =
  Set.fromList (expandPatternByTheory commutativeTheory patternValue) === commutativeOrbit patternValue

propExpansionDuplicateFree :: SmallTheoryPattern -> Property
propExpansionDuplicateFree (SmallTheoryPattern patternValue) =
  Set.size expandedSet === length expandedPatterns
  where
    expandedPatterns =
      expandPatternByTheory commutativeTheory patternValue
    expandedSet =
      Set.fromList expandedPatterns

propExpansionContainsCanonicalForm :: SmallTheoryPattern -> Property
propExpansionContainsCanonicalForm (SmallTheoryPattern patternValue) =
  counterexample "expanded orbit does not contain the recursive canonical form" $
    property (Set.member (canonicalizePatternByTheory commutativeTheory patternValue) (Set.fromList (expandPatternByTheory commutativeTheory patternValue)))

testRecursiveCanonicalizationDeclarationOrder :: IO ()
testRecursiveCanonicalizationDeclarationOrder =
  canonicalizePatternByTheory commutativeTheory unorderedPattern
    @?= orderedPattern
  where
    unorderedPattern =
      PatternNode
        ( TheoryAdd
            (PatternNode (TheoryLeaf 1))
            (PatternVar (EGraph.mkPatternVar 0))
        )
    orderedPattern =
      PatternNode
        ( TheoryAdd
            (PatternVar (EGraph.mkPatternVar 0))
            (PatternNode (TheoryLeaf 1))
        )

commutativeTheory :: TheorySpec TheoryNode
commutativeTheory =
  TheorySpec
    { tsClassify = \node ->
        case node of
          TheoryAdd _left _right ->
            commutativeBinary TheoryAdd
          _ ->
            Ordinary
    }

expectedCanonicalNode :: Ord a => TheoryNode a -> TheoryNode a
expectedCanonicalNode node =
  case node of
    TheoryAdd left right ->
      TheoryAdd (min left right) (max left right)
    _ ->
      node

commutativeOrbit :: Pattern TheoryNode -> Set.Set (Pattern TheoryNode)
commutativeOrbit patternValue =
  case patternValue of
    PatternVar patternVar ->
      Set.singleton (PatternVar patternVar)
    PatternNode node ->
      Set.fromList (PatternNode <$> (expandedChildNodes >>= localNodeOrbit))
      where
        expandedChildNodes =
          traverse (Set.toList . commutativeOrbit) node

localNodeOrbit :: TheoryNode (Pattern TheoryNode) -> [TheoryNode (Pattern TheoryNode)]
localNodeOrbit node =
  case node of
    TheoryAdd left right
      | left == right ->
          [node]
      | otherwise ->
          [node, TheoryAdd right left]
    _ ->
      [node]

genChild :: Gen Int
genChild =
  chooseInt (0, 5)

genPatternVar :: Gen EGraph.PatternVar
genPatternVar =
  EGraph.mkPatternVar <$> chooseInt (0, 5)

genTheoryNode :: Gen a -> Gen (TheoryNode a)
genTheoryNode genValue =
  frequency
    [ (1, TheoryLeaf <$> chooseInt (0, 3)),
      (2, TheoryUnary <$> genValue),
      (3, TheoryAdd <$> genValue <*> genValue),
      (2, TheoryPair <$> genValue <*> genValue)
    ]

genPattern :: Int -> Gen (Pattern TheoryNode)
genPattern size =
  case compare size 0 of
    GT ->
      frequency
        [ (2, PatternVar <$> genPatternVar),
          (2, PatternNode . TheoryLeaf <$> chooseInt (0, 3)),
          (2, PatternNode . TheoryUnary <$> genSmallerPattern),
          (3, PatternNode <$> (TheoryAdd <$> genSmallerPattern <*> genSmallerPattern)),
          (2, PatternNode <$> (TheoryPair <$> genSmallerPattern <*> genSmallerPattern))
        ]
    _ ->
      oneof
        [ PatternVar <$> genPatternVar,
          PatternNode . TheoryLeaf <$> chooseInt (0, 3)
        ]
  where
    genSmallerPattern =
      genPattern (size `div` 2)

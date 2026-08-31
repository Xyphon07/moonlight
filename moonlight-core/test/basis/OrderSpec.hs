{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module OrderSpec
  ( tests,
  )
where

import Control.Monad
  ( when,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.Set qualified as Set
import Moonlight.Core
  ( FiniteUniverse (..),
    boundedEnumUniverse,
    finiteUniverseList,
    finiteUniverseSet,
  )
import Moonlight.Core
  ( PartialOrder (..),
    comparable,
    finitePointwiseLeq,
    incomparable,
    pointwiseLeqOver,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( Assertion,
    assertBool,
    testCase,
    (@?=),
  )

data TinyDomain
  = TinyA
  | TinyB
  | TinyC
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FiniteUniverse TinyDomain where
  finiteUniverse =
    boundedEnumUniverse

tests :: TestTree
tests =
  testGroup
    "partial order"
    [ testCase "Bool satisfies the partial-order laws" $
        assertPartialOrderLaws "Bool" [False, True],
      testCase "product order satisfies the partial-order laws" $
        assertPartialOrderLaws "Bool product" boolProductSamples,
      testCase "set inclusion satisfies the partial-order laws" $
        assertPartialOrderLaws "Set Int" setSamples,
      testCase "strict order is non-equal less-or-equal" testStrictOrder,
      testCase "product order is pointwise and admits incomparable points" testProductOrder,
      testCase "finite pointwise order quantifies over the finite universe" testFinitePointwiseOrder
    ]

assertPartialOrderLaws ::
  (PartialOrder order, Show order) =>
  String ->
  [order] ->
  Assertion
assertPartialOrderLaws label values = do
  traverse_ (assertReflexive label) values
  traverse_ (uncurry (assertAntisymmetric label)) (pairs values)
  traverse_ (assertTransitive label) (triples values)

assertReflexive ::
  (PartialOrder order, Show order) =>
  String ->
  order ->
  Assertion
assertReflexive label value =
  assertBool
    (label <> " is not reflexive at " <> show value)
    (leq value value)

assertAntisymmetric ::
  (PartialOrder order, Show order) =>
  String ->
  order ->
  order ->
  Assertion
assertAntisymmetric label left right =
  when (leq left right && leq right left) $
    assertBool
      ( label
          <> " violates antisymmetry at "
          <> show (left, right)
      )
      (left == right)

assertTransitive ::
  (PartialOrder order, Show order) =>
  String ->
  (order, order, order) ->
  Assertion
assertTransitive label (left, middle, right) =
  when (leq left middle && leq middle right) $
    assertBool
      ( label
          <> " is not transitive at "
          <> show (left, middle, right)
      )
      (leq left right)

testStrictOrder :: Assertion
testStrictOrder = do
  lt False True @?= True
  lt True True @?= False
  lt True False @?= False

testProductOrder :: Assertion
testProductOrder = do
  leq (False, True) (True, True) @?= True
  leq (False, True) (True, False) @?= False
  comparable (False, True) (True, True) @?= True
  incomparable (False, True) (True, False) @?= True

testFinitePointwiseOrder :: Assertion
testFinitePointwiseOrder = do
  finiteUniverseList @TinyDomain @?= [TinyA, TinyB, TinyC]
  finiteUniverseSet @TinyDomain @?= Set.fromList [TinyA, TinyB, TinyC]
  pointwiseLeqOver [TinyA, TinyC] leftProjection rightProjection @?= True
  finitePointwiseLeq leftProjection rightProjection @?= False
  finitePointwiseLeq (const False) rightProjection @?= True

leftProjection :: TinyDomain -> Bool
leftProjection TinyA =
  True
leftProjection TinyB =
  True
leftProjection TinyC =
  False

rightProjection :: TinyDomain -> Bool
rightProjection TinyA =
  True
rightProjection TinyB =
  False
rightProjection TinyC =
  False

boolProductSamples :: [(Bool, Bool)]
boolProductSamples =
  (,) <$> [False, True] <*> [False, True]

setSamples :: [Set.Set Int]
setSamples =
  Set.fromList
    <$> [ [],
          [1],
          [2],
          [1, 2],
          [1, 2, 3]
        ]

pairs :: [a] -> [(a, a)]
pairs values =
  (,) <$> values <*> values

triples :: [a] -> [(a, a, a)]
triples values =
  (,,) <$> values <*> values <*> values

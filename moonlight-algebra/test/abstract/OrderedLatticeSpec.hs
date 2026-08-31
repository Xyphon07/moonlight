{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module OrderedLatticeSpec
  ( tests,
  )
where

import Control.Monad
  ( when,
  )
import Data.Foldable
  ( traverse_,
  )
import Data.List
  ( subsequences,
  )
import Data.Set qualified as Set
import Moonlight.Algebra
  ( JoinSemilattice (..),
    MeetSemilattice (..),
    OrderedLattice,
  )
import Moonlight.Algebra
  ( PowerSet,
    fromList,
  )
import Moonlight.Algebra
  ( mkProductAlgebra,
    toProductList,
  )
import Moonlight.Core
  ( FiniteUniverse (..),
    boundedEnumUniverse,
  )
import Moonlight.Core
  ( PartialOrder (..),
    comparable,
    incomparable,
  )
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
    (@?=),
  )

data Atom
  = AtomA
  | AtomB
  | AtomC
  deriving stock (Eq, Ord, Show, Enum, Bounded)

instance FiniteUniverse Atom where
  finiteUniverse =
    boundedEnumUniverse

tests :: TestTree
tests =
  testGroup
    "ordered lattice"
    [ testCase "Bool join and meet are least upper and greatest lower bounds" $
        assertOrderedLatticeLaws "Bool" [False, True],
      testCase "Set Int join and meet are least upper and greatest lower bounds" $
        assertOrderedLatticeLaws "Set Int" setSamples,
      testCase "PowerSet Atom obeys ordered lattice laws" $
        assertOrderedLatticeLaws "PowerSet Atom" powerSetSamples,
      testCase "ProductAlgebra 2 Bool matches DBSP product-time join and meet shape" testProductLattice
    ]

assertOrderedLatticeLaws ::
  (OrderedLattice lattice, Show lattice) =>
  String ->
  [lattice] ->
  Assertion
assertOrderedLatticeLaws label values = do
  traverse_ (uncurry (assertJoinMeetOrderCoherence label)) (pairs values)
  traverse_ (uncurry (assertLeastUpperBound label values)) (pairs values)
  traverse_ (uncurry (assertGreatestLowerBound label values)) (pairs values)
  traverse_ (assertIdempotence label) values
  traverse_ (uncurry (assertCommutativity label)) (pairs values)
  traverse_ (assertAssociativity label) (triples values)
  traverse_ (uncurry (assertAbsorption label)) (pairs values)

assertJoinMeetOrderCoherence ::
  (OrderedLattice lattice, Show lattice) =>
  String ->
  lattice ->
  lattice ->
  Assertion
assertJoinMeetOrderCoherence label left right = do
  leq left right @?= (join left right == right)
  leq left right @?= (meet left right == left)
  assertBool
    (label <> " join is not above left at " <> show (left, right))
    (leq left (join left right))
  assertBool
    (label <> " join is not above right at " <> show (left, right))
    (leq right (join left right))
  assertBool
    (label <> " meet is not below left at " <> show (left, right))
    (leq (meet left right) left)
  assertBool
    (label <> " meet is not below right at " <> show (left, right))
    (leq (meet left right) right)

assertLeastUpperBound ::
  (OrderedLattice lattice, Show lattice) =>
  String ->
  [lattice] ->
  lattice ->
  lattice ->
  Assertion
assertLeastUpperBound label values left right =
  traverse_ checkCandidate values
  where
    leastUpperBound =
      join left right

    checkCandidate candidate =
      when (leq left candidate && leq right candidate) $
        assertBool
          ( label
              <> " join is not least at "
              <> show (left, right, candidate)
          )
          (leq leastUpperBound candidate)

assertGreatestLowerBound ::
  (OrderedLattice lattice, Show lattice) =>
  String ->
  [lattice] ->
  lattice ->
  lattice ->
  Assertion
assertGreatestLowerBound label values left right =
  traverse_ checkCandidate values
  where
    greatestLowerBound =
      meet left right

    checkCandidate candidate =
      when (leq candidate left && leq candidate right) $
        assertBool
          ( label
              <> " meet is not greatest at "
              <> show (left, right, candidate)
          )
          (leq candidate greatestLowerBound)

assertIdempotence ::
  (Eq lattice, Show lattice, JoinSemilattice lattice, MeetSemilattice lattice) =>
  String ->
  lattice ->
  Assertion
assertIdempotence label value = do
  assertEqual (label <> " join idempotence at " <> show value) value (join value value)
  assertEqual (label <> " meet idempotence at " <> show value) value (meet value value)

assertCommutativity ::
  (Eq lattice, Show lattice, JoinSemilattice lattice, MeetSemilattice lattice) =>
  String ->
  lattice ->
  lattice ->
  Assertion
assertCommutativity label left right = do
  assertEqual
    (label <> " join commutativity at " <> show (left, right))
    (join left right)
    (join right left)
  assertEqual
    (label <> " meet commutativity at " <> show (left, right))
    (meet left right)
    (meet right left)

assertAssociativity ::
  (Eq lattice, Show lattice, JoinSemilattice lattice, MeetSemilattice lattice) =>
  String ->
  (lattice, lattice, lattice) ->
  Assertion
assertAssociativity label (left, middle, right) = do
  assertEqual
    (label <> " join associativity at " <> show (left, middle, right))
    (join (join left middle) right)
    (join left (join middle right))
  assertEqual
    (label <> " meet associativity at " <> show (left, middle, right))
    (meet (meet left middle) right)
    (meet left (meet middle right))

assertAbsorption ::
  (Eq lattice, Show lattice, JoinSemilattice lattice, MeetSemilattice lattice) =>
  String ->
  lattice ->
  lattice ->
  Assertion
assertAbsorption label left right = do
  assertEqual
    (label <> " join absorption at " <> show (left, right))
    left
    (join left (meet left right))
  assertEqual
    (label <> " meet absorption at " <> show (left, right))
    left
    (meet left (join left right))

testProductLattice :: Assertion
testProductLattice =
  case
    ( traverse (mkProductAlgebra @2) [[False, True], [True, False], [True, True], [False, False]],
      traverse (mkProductAlgebra @2) productBoolCoordinateLists
    ) of
    (Just [left, right, expectedJoin, expectedMeet], Just productBoolSamples) -> do
      toProductList (join left right) @?= toProductList expectedJoin
      toProductList (meet left right) @?= toProductList expectedMeet
      leq left expectedJoin @?= True
      leq right expectedJoin @?= True
      comparable left expectedJoin @?= True
      incomparable left right @?= True
      assertOrderedLatticeLaws "ProductAlgebra 2 Bool" productBoolSamples
    _ ->
      assertFailure "test fixture declared an invalid ProductAlgebra arity"

setSamples :: [Set.Set Int]
setSamples =
  Set.fromList <$> subsequences [1, 2, 3]

powerSetSamples :: [PowerSet Atom]
powerSetSamples =
  fromList <$> subsequences [AtomA, AtomB, AtomC]

productBoolCoordinateLists :: [[Bool]]
productBoolCoordinateLists =
  traverse (const [False, True]) [(), ()]

pairs :: [a] -> [(a, a)]
pairs values =
  (,) <$> values <*> values

triples :: [a] -> [(a, a, a)]
triples values =
  (,,) <$> values <*> values <*> values

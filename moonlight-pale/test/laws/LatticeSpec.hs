{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wmissing-local-signatures #-}

module LatticeSpec
  ( tests,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Pale.Test.Laws.Lattice
  ( FiniteLattice,
    FiniteLatticeError (..),
    LatticeBounds (..),
    compileFiniteLattice,
    finiteLatticeJoin,
    finiteLatticeLaws,
    finiteLatticeMeet,
  )
import Moonlight.Pale.Test.Laws.Suite (lawGroup, renderLawSuite)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck
  ( Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    forAll,
    testProperty,
    vectorOf,
    (===),
  )

data Diamond
  = DiamondBottom
  | DiamondLeft
  | DiamondRight
  | DiamondTop
  deriving stock (Eq, Ord, Show)

data OpenValue
  = OpenBottom
  | OpenTop
  | EscapedValue
  deriving stock (Eq, Ord, Show)

data TableValue
  = TableValue !Int
  | MissingTableEntry !Int !Int
  deriving stock (Eq, Ord, Show)

data ClosedOperationTables = ClosedOperationTables
  { closedTableUniverse :: !(NonEmpty TableValue),
    closedJoinTable :: !(Map (TableValue, TableValue) TableValue),
    closedMeetTable :: !(Map (TableValue, TableValue) TableValue)
  }
  deriving stock (Show)

tests :: TestTree
tests =
  testGroup
    "Moonlight.Pale.Test.Laws.Lattice"
    [ renderFiniteLattice "Bool bounded lattice" boolLattice,
      renderFiniteLattice "diamond bounded lattice" diamondLattice,
      testCase "bounds outside the universe are typed construction errors" $
        assertLatticeErrors
          "top is absent"
          (TopOutsideUniverse True :| [])
          invalidBoundedLattice,
      testCase "duplicate universe values retain both positions" $
        assertLatticeErrors
          "the second False duplicates the first"
          (DuplicateUniverseElement False 0 2 :| [])
          duplicateUniverseLattice,
      testCase "join closure failures name operands and escaped results" $
        case closureFailureLattice of
          Left errors ->
            assertEqual
              "every ordered pair was evaluated once and rejected"
              ( fmap
                  (\(leftValue, rightValue) ->
                     JoinOutsideUniverse leftValue rightValue EscapedValue
                  )
                  openPairs
              )
              (toList errors)
          Right _ -> assertFailure "expected escaped join results to reject compilation",
      testCase "dense lookup returns the original operation results" $
        case diamondLattice of
          Left errors -> assertFailure ("expected a compiled diamond: " <> show errors)
          Right lattice -> do
            assertEqual
              "join table"
              (Right DiamondTop)
              (finiteLatticeJoin lattice DiamondLeft DiamondRight)
            assertEqual
              "meet table"
              (Right DiamondBottom)
              (finiteLatticeMeet lattice DiamondLeft DiamondRight),
      testProperty
        "compiled dense tables agree with a simple list oracle"
        finiteLatticeDifferentialProperty
    ]

boolLattice :: Either (NonEmpty (FiniteLatticeError Bool)) (FiniteLattice Bool)
boolLattice =
  compileFiniteLattice
    "Bool"
    (False :| [True])
    (||)
    (&&)
    (Just (LatticeBounds False True))

diamondLattice :: Either (NonEmpty (FiniteLatticeError Diamond)) (FiniteLattice Diamond)
diamondLattice =
  compileFiniteLattice
    "diamond"
    diamondUniverse
    diamondJoin
    diamondMeet
    (Just (LatticeBounds DiamondBottom DiamondTop))

invalidBoundedLattice :: Either (NonEmpty (FiniteLatticeError Bool)) (FiniteLattice Bool)
invalidBoundedLattice =
  compileFiniteLattice
    "invalid Bool"
    (False :| [])
    (||)
    (&&)
    (Just (LatticeBounds False True))

duplicateUniverseLattice :: Either (NonEmpty (FiniteLatticeError Bool)) (FiniteLattice Bool)
duplicateUniverseLattice =
  compileFiniteLattice
    "duplicate Bool"
    (False :| [True, False])
    (||)
    (&&)
    Nothing

closureFailureLattice :: Either (NonEmpty (FiniteLatticeError OpenValue)) (FiniteLattice OpenValue)
closureFailureLattice =
  compileFiniteLattice
    "open operation"
    openUniverse
    (\_ _ -> EscapedValue)
    openMeet
    Nothing

openUniverse :: NonEmpty OpenValue
openUniverse = OpenBottom :| [OpenTop]

openPairs :: [(OpenValue, OpenValue)]
openPairs =
  (,) <$> toList openUniverse <*> toList openUniverse

openMeet :: OpenValue -> OpenValue -> OpenValue
openMeet leftValue rightValue
  | leftValue == OpenBottom = OpenBottom
  | rightValue == OpenBottom = OpenBottom
  | otherwise = OpenTop

diamondUniverse :: NonEmpty Diamond
diamondUniverse = DiamondBottom :| [DiamondLeft, DiamondRight, DiamondTop]

diamondJoin :: Diamond -> Diamond -> Diamond
diamondJoin leftValue rightValue
  | diamondLeq leftValue rightValue = rightValue
  | diamondLeq rightValue leftValue = leftValue
  | otherwise = DiamondTop

diamondMeet :: Diamond -> Diamond -> Diamond
diamondMeet leftValue rightValue
  | diamondLeq leftValue rightValue = leftValue
  | diamondLeq rightValue leftValue = rightValue
  | otherwise = DiamondBottom

diamondLeq :: Diamond -> Diamond -> Bool
diamondLeq leftValue rightValue =
  leftValue == rightValue || leftValue == DiamondBottom || rightValue == DiamondTop

renderFiniteLattice ::
  Show a =>
  String ->
  Either (NonEmpty (FiniteLatticeError a)) (FiniteLattice a) ->
  TestTree
renderFiniteLattice label latticeResult =
  case latticeResult of
    Left errors ->
      testCase (label <> " compiles") $
        assertFailure ("expected valid finite lattice: " <> show errors)
    Right lattice ->
      renderLawSuite (lawGroup label (finiteLatticeLaws lattice))

assertLatticeErrors ::
  (Eq a, Show a) =>
  String ->
  NonEmpty (FiniteLatticeError a) ->
  Either (NonEmpty (FiniteLatticeError a)) (FiniteLattice a) ->
  IO ()
assertLatticeErrors label expectedErrors latticeResult =
  case latticeResult of
    Left actualErrors -> assertEqual label expectedErrors actualErrors
    Right _ -> assertFailure (label <> ": expected finite lattice compilation to fail")

finiteLatticeDifferentialProperty :: Property
finiteLatticeDifferentialProperty =
  forAll closedOperationTablesGenerator $ \tables ->
    case
        compileFiniteLattice
          "generated"
          (closedTableUniverse tables)
          (operationFromTable (closedJoinTable tables))
          (operationFromTable (closedMeetTable tables))
          Nothing
      of
        Left errors ->
          counterexample ("closed operation table was rejected: " <> show errors) False
        Right lattice ->
          conjoin $
            fmap
              (\(leftValue, rightValue) ->
                 conjoin
                   [ finiteLatticeJoin lattice leftValue rightValue
                       === Right (operationFromTable (closedJoinTable tables) leftValue rightValue),
                     finiteLatticeMeet lattice leftValue rightValue
                       === Right (operationFromTable (closedMeetTable tables) leftValue rightValue)
                   ]
              )
              (simplePairs (toList (closedTableUniverse tables)))

closedOperationTablesGenerator :: Gen ClosedOperationTables
closedOperationTablesGenerator = do
  cardinality <- chooseInt (1, 4)
  let universe = TableValue 0 :| fmap TableValue [1 .. cardinality - 1]
      pairs = simplePairs (toList universe)
  joinResults <- vectorOf (cardinality * cardinality) (TableValue <$> chooseInt (0, cardinality - 1))
  meetResults <- vectorOf (cardinality * cardinality) (TableValue <$> chooseInt (0, cardinality - 1))
  pure
    ClosedOperationTables
      { closedTableUniverse = universe,
        closedJoinTable = Map.fromList (zip pairs joinResults),
        closedMeetTable = Map.fromList (zip pairs meetResults)
      }

simplePairs :: [a] -> [(a, a)]
simplePairs values =
  (,) <$> values <*> values

operationFromTable ::
  Map (TableValue, TableValue) TableValue ->
  TableValue ->
  TableValue ->
  TableValue
operationFromTable table leftValue rightValue =
  case (leftValue, rightValue) of
    (TableValue leftIndex, TableValue rightIndex) ->
      Map.findWithDefault
        (MissingTableEntry leftIndex rightIndex)
        (leftValue, rightValue)
        table
    (MissingTableEntry leftIndex rightIndex, _) ->
      MissingTableEntry leftIndex rightIndex
    (_, MissingTableEntry leftIndex rightIndex) ->
      MissingTableEntry leftIndex rightIndex

toList :: NonEmpty a -> [a]
toList (firstValue :| remainingValues) =
  firstValue : remainingValues

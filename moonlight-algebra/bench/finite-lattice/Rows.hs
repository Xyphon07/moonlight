module Rows
  ( contextRowComparisonBenchmarks,
  )
where

import Control.DeepSeq
  ( NFData (..),
  )
import Data.Bifunctor
  ( first,
  )
import Data.IntSet qualified as IntSet
import Data.Vector qualified as Vector
import Fixtures
  ( Shape,
    assertFiniteFixture,
    caseLabel,
    compileLatticeEnv,
    keys,
    querySizes,
    rawRelationRows,
    shapeLabel,
    shapes,
  )
import Kernels
  ( leqSweepWeight,
  )
import Moonlight.FiniteLattice.Core
  ( ContextLattice,
    leqContext,
  )
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    env,
    nf,
  )

data RowOwner
  = PackedRows
  | IntSetRows
  deriving stock (Eq, Ord, Show)

data PreparedRows
  = PreparedPackedRows !Int !(ContextLattice Int)
  | PreparedIntSetRows !Int !(Vector.Vector IntSet.IntSet)

instance NFData PreparedRows where
  rnf prepared =
    case prepared of
      PreparedPackedRows size lattice ->
        rnf (size, lattice)
      PreparedIntSetRows size rows ->
        rnf (size, rows)

contextRowComparisonBenchmarks :: Benchmark
contextRowComparisonBenchmarks =
  bgroup
    "context-row-membership-baseline"
    [ bgroup
        (shapeLabel shape)
        [ bgroup
            (rowOwnerLabel rowOwner)
            (fmap (membershipBenchmark shape rowOwner) querySizes)
        | rowOwner <- rowOwners
        ]
    | shape <- shapes
    ]

membershipBenchmark :: Shape -> RowOwner -> Int -> Benchmark
membershipBenchmark shape rowOwner size =
  env (prepareRows shape rowOwner size) $ \rows ->
    bench (caseLabel "membership sweep" size) (nf membershipCount rows)

prepareRows :: Shape -> RowOwner -> Int -> IO PreparedRows
prepareRows shape rowOwner size =
  case rowOwner of
    PackedRows ->
      PreparedPackedRows size <$> compileLatticeEnv shape size
    IntSetRows -> do
      lattice <- compileLatticeEnv shape size
      let !rows = rawRelationRows shape size
      assertFiniteFixture "IntSet membership rows" (assertMembershipRowsAgree size lattice rows)
      pure (PreparedIntSetRows size rows)

assertMembershipRowsAgree :: Int -> ContextLattice Int -> Vector.Vector IntSet.IntSet -> Either String ()
assertMembershipRowsAgree size lattice rows
  | Vector.length rows /= size =
      Left ("membership row count " <> show (Vector.length rows) <> " /= " <> show size)
  | otherwise =
      fmap (const ()) (traverse checkMembership memberships)
  where
    memberships =
      [ (leftValue, rightValue, IntSet.member rightValue row)
      | (leftValue, row) <- zip (keys size) (Vector.toList rows),
        rightValue <- keys size
      ]

    checkMembership (leftValue, rightValue, baseline) = do
      actual <- first show (leqContext lattice leftValue rightValue)
      if actual == baseline
        then Right ()
        else
          Left
            ( "membership row mismatch for "
                <> show (leftValue, rightValue)
                <> ": baseline "
                <> show baseline
                <> ", lattice "
                <> show actual
            )

membershipCount :: PreparedRows -> Int
membershipCount prepared =
  case prepared of
    PreparedPackedRows size lattice ->
      leqSweepWeight size lattice
    PreparedIntSetRows size rows ->
      length
        [ ()
        | row <- Vector.toList rows,
          rightKey <- keys size,
          IntSet.member rightKey row
        ]

rowOwners :: [RowOwner]
rowOwners = [PackedRows, IntSetRows]

rowOwnerLabel :: RowOwner -> String
rowOwnerLabel rowOwner =
  case rowOwner of
    PackedRows -> "moonlight: packed context-key rows"
    IntSetRows -> "world: containers IntSet rows"

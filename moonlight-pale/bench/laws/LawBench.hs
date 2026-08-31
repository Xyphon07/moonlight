-- Current finite-law construction workloads.  Dense lattice rows certify the
-- complete join/meet witness.  Restriction rows time checked quadratic relation
-- construction; historical unchecked seeds are deliberately excluded.
module LawBench
  ( LawBenchmarkObstruction (..),
    lawBenchmarks,
  )
where

import BenchSupport (preparedBenchmarks)
import Control.DeepSeq (NFData (rnf))
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Pale.Test.Laws.Lattice
  ( FiniteLattice,
    FiniteLatticeError,
    FiniteLatticeLookupError,
    LatticeBounds (..),
    compileFiniteLattice,
    finiteLatticeJoin,
    finiteLatticeMeet,
  )
import Moonlight.Pale.Test.Laws.Restriction
  ( FiniteRestrictionError (..),
    compileFiniteRestrictionLaw,
  )
import Test.Tasty.Bench (Benchmark, bgroup)

data LawBenchmarkObstruction
  = LatticeCompilationRejected !Int !(NonEmpty (FiniteLatticeError Int))
  | LatticeLookupRejected !Int !(FiniteLatticeLookupError Int)
  | UnexpectedLatticeWitness !Int !Integer !Integer
  | RestrictionCompilationRejected !Int !(NonEmpty (FiniteRestrictionError Int))
  | InvalidRestrictionCorpusAccepted !String
  | UnexpectedRestrictionObstruction !String !(NonEmpty (FiniteRestrictionError Int))
  deriving stock (Eq, Show)

instance NFData LawBenchmarkObstruction where
  rnf obstruction =
    rnf (show obstruction)

data ChainCorpus = ChainCorpus
  { chainCardinality :: !Int,
    chainUniverse :: !(NonEmpty Int)
  }

instance NFData ChainCorpus where
  rnf corpus =
    rnf (chainCardinality corpus)
      `seq` rnf (chainUniverse corpus)

data LatticeWitnessDigest = LatticeWitnessDigest
  { latticeWitnessPairs :: !Int,
    latticeWitnessOperationSum :: !Integer,
    latticeWitnessHash :: !Int
  }
  deriving stock (Eq, Show)

instance NFData LatticeWitnessDigest where
  rnf digest =
    rnf (latticeWitnessPairs digest)
      `seq` rnf (latticeWitnessOperationSum digest)
      `seq` rnf (latticeWitnessHash digest)

data RestrictionCompilationDigest = RestrictionCompilationDigest
  { restrictionCellCount :: !Int,
    restrictionSectionCount :: !Int,
    restrictionActionCount :: !Integer
  }
  deriving stock (Eq, Show)

instance NFData RestrictionCompilationDigest where
  rnf digest =
    rnf (restrictionCellCount digest)
      `seq` rnf (restrictionSectionCount digest)
      `seq` rnf (restrictionActionCount digest)

lawBenchmarks :: Either LawBenchmarkObstruction Benchmark
lawBenchmarks = do
  _ <- validateInvalidRestrictionCorpora
  preparedLattices <- traverse prepareLatticeCorpus lawSizes
  preparedRestrictions <- traverse prepareRestrictionCorpus lawSizes
  pure
    ( bgroup
        "finite-laws"
        [ bgroup
            "dense-chain-lattice-compile-and-witness"
            (preparedBenchmarks "cardinality" preparedLattices compileLatticeWitness),
          bgroup
            "chain-restriction-compile"
            (preparedBenchmarks "cardinality" preparedRestrictions compileRestrictionDigest)
        ]
    )

lawSizes :: [Int]
lawSizes =
  [32, 64, 128]

prepareLatticeCorpus :: Int -> Either LawBenchmarkObstruction (Int, ChainCorpus)
prepareLatticeCorpus cardinality = do
  let corpus = chainCorpus cardinality
  _ <- compileLatticeWitness corpus
  pure (cardinality, corpus)

prepareRestrictionCorpus :: Int -> Either LawBenchmarkObstruction (Int, ChainCorpus)
prepareRestrictionCorpus cardinality = do
  let corpus = chainCorpus cardinality
  _ <- compileRestrictionDigest corpus
  pure (cardinality, corpus)

chainCorpus :: Int -> ChainCorpus
chainCorpus cardinality =
  ChainCorpus
    { chainCardinality = cardinality,
      chainUniverse = 0 :| [1 .. cardinality - 1]
    }

compileLatticeWitness :: ChainCorpus -> Either LawBenchmarkObstruction LatticeWitnessDigest
compileLatticeWitness corpus = do
  lattice <-
    first
      (LatticeCompilationRejected (chainCardinality corpus))
      ( compileFiniteLattice
          "benchmark chain"
          (chainUniverse corpus)
          max
          min
          (Just (LatticeBounds 0 (chainCardinality corpus - 1)))
      )
  witness <-
    foldM
      (digestLatticeRow lattice (chainUniverse corpus) (chainCardinality corpus))
      emptyLatticeWitness
      (chainUniverse corpus)
  let expectedSum =
        toInteger (chainCardinality corpus)
          * toInteger (chainCardinality corpus)
          * toInteger (chainCardinality corpus - 1)
  if latticeWitnessOperationSum witness == expectedSum
    then Right witness
    else
      Left
        ( UnexpectedLatticeWitness
            (chainCardinality corpus)
            expectedSum
            (latticeWitnessOperationSum witness)
        )

digestLatticeRow ::
  FiniteLattice Int ->
  NonEmpty Int ->
  Int ->
  LatticeWitnessDigest ->
  Int ->
  Either LawBenchmarkObstruction LatticeWitnessDigest
digestLatticeRow lattice universe cardinality digest leftValue =
  foldM
    (digestLatticePair lattice cardinality leftValue)
    digest
    universe

digestLatticePair ::
  FiniteLattice Int ->
  Int ->
  Int ->
  LatticeWitnessDigest ->
  Int ->
  Either LawBenchmarkObstruction LatticeWitnessDigest
digestLatticePair lattice cardinality leftValue digest rightValue = do
  joinValue <-
    first
      (LatticeLookupRejected cardinality)
      (finiteLatticeJoin lattice leftValue rightValue)
  meetValue <-
    first
      (LatticeLookupRejected cardinality)
      (finiteLatticeMeet lattice leftValue rightValue)
  pure
    LatticeWitnessDigest
      { latticeWitnessPairs = latticeWitnessPairs digest + 1,
        latticeWitnessOperationSum =
          latticeWitnessOperationSum digest
            + toInteger joinValue
            + toInteger meetValue,
        latticeWitnessHash =
          (((latticeWitnessHash digest * 16777619) + joinValue) * 16777619)
            + meetValue
      }

emptyLatticeWitness :: LatticeWitnessDigest
emptyLatticeWitness =
  LatticeWitnessDigest
    { latticeWitnessPairs = 0,
      latticeWitnessOperationSum = 0,
      latticeWitnessHash = 2166136261
    }

compileRestrictionDigest :: ChainCorpus -> Either LawBenchmarkObstruction RestrictionCompilationDigest
compileRestrictionDigest corpus =
  case
      compileFiniteRestrictionLaw
        "benchmark chain"
        (chainUniverse corpus)
        (<=)
        (fmap (\cell -> (cell, cell)) (toList (chainUniverse corpus)))
        (\_ targetCell value -> min value targetCell)
    of
    Left errors ->
      Left (RestrictionCompilationRejected (chainCardinality corpus) errors)
    Right restrictionLaw ->
      restrictionLaw
        `seq` Right
          RestrictionCompilationDigest
            { restrictionCellCount = chainCardinality corpus,
              restrictionSectionCount = chainCardinality corpus,
              restrictionActionCount =
                let cardinality = toInteger (chainCardinality corpus)
                 in (cardinality * (cardinality + 1)) `div` 2
            }

validateInvalidRestrictionCorpora :: Either LawBenchmarkObstruction ()
validateInvalidRestrictionCorpora = do
  expectRestrictionObstruction
    "duplicate cells"
    (\case DuplicateRestrictionCell {} -> True; _ -> False)
    ( compileFiniteRestrictionLaw
        "duplicate"
        (0 :| [0])
        (<=)
        [(0, 0 :: Int)]
        (\_ target value -> min value target)
    )
  expectRestrictionObstruction
    "unknown section cell"
    (\case SectionCellOutsideUniverse 2 -> True; _ -> False)
    ( compileFiniteRestrictionLaw
        "unknown section"
        (0 :| [1])
        (<=)
        [(2, 2 :: Int)]
        (\_ target value -> min value target)
    )
  expectRestrictionObstruction
    "non-poset"
    (\case RestrictionRelationNotReflexive {} -> True; _ -> False)
    ( compileFiniteRestrictionLaw
        "non-poset"
        (0 :| [1])
        (\_ _ -> False)
        [(0, 0 :: Int), (1, 1)]
        (\_ target value -> min value target)
    )

expectRestrictionObstruction ::
  String ->
  (FiniteRestrictionError Int -> Bool) ->
  Either (NonEmpty (FiniteRestrictionError Int)) restrictionLaw ->
  Either LawBenchmarkObstruction ()
expectRestrictionObstruction label matches = \case
  Right _ ->
    Left (InvalidRestrictionCorpusAccepted label)
  Left errors
    | any matches errors -> Right ()
    | otherwise -> Left (UnexpectedRestrictionObstruction label errors)

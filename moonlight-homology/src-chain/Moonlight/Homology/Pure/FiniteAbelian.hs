module Moonlight.Homology.Pure.FiniteAbelian
  ( FiniteAbelianTorsion,
    mkFiniteAbelianTorsion,
    torsionFromHomologyGroup,
    finiteAbelianInvariants,
    normalizeTorsionOrders,
    finiteAbelianSummandCount,
    finiteAbelianCyclicSummandMultiplicity,
    finiteAbelianFilteredCardinality,
    finiteAbelianCardinality,
    finiteAbelianExponent,
    finiteAbelianOrderSupport,
    finiteAbelianPrimaryOrderSupport,
    finiteAbelianExactOrderElementCount,
    isPrime,
    matchesOptional,
  )
where

import Data.Kind (Type)
import Data.Function ((&))
import qualified Data.List as List
import Moonlight.Algebra.Pure.NumberTheory
  ( countExactOrderElements,
    divisorsOf,
    isPrime,
    primePowerPart,
  )
import Moonlight.Homology.Pure.Group (HomologyGroup (..))

type FiniteAbelianTorsion :: Type
newtype FiniteAbelianTorsion = FiniteAbelianTorsion
  { finiteAbelianInvariants :: [Integer]
  }
  deriving stock (Eq, Show)

mkFiniteAbelianTorsion :: [Integer] -> FiniteAbelianTorsion
mkFiniteAbelianTorsion =
  FiniteAbelianTorsion . normalizeInvariantFactors

torsionFromHomologyGroup :: HomologyGroup Integer -> FiniteAbelianTorsion
torsionFromHomologyGroup =
  mkFiniteAbelianTorsion . torsionInvariants

finiteAbelianSummandCount :: Maybe Integer -> FiniteAbelianTorsion -> Integer
finiteAbelianSummandCount orderConstraint =
  toInteger . length . finiteAbelianMatchingInvariants orderConstraint

finiteAbelianCyclicSummandMultiplicity :: Integer -> FiniteAbelianTorsion -> Int
finiteAbelianCyclicSummandMultiplicity orderValue =
  length . finiteAbelianMatchingInvariants (Just orderValue)

finiteAbelianFilteredCardinality :: Maybe Integer -> FiniteAbelianTorsion -> Integer
finiteAbelianFilteredCardinality orderConstraint =
  product . finiteAbelianMatchingInvariants orderConstraint

finiteAbelianCardinality :: FiniteAbelianTorsion -> Integer
finiteAbelianCardinality =
  finiteAbelianFilteredCardinality Nothing

finiteAbelianExponent :: FiniteAbelianTorsion -> Integer
finiteAbelianExponent =
  foldr lcm 1 . finiteAbelianInvariants

finiteAbelianOrderSupport :: FiniteAbelianTorsion -> [Integer]
finiteAbelianOrderSupport torsionValue =
  finiteAbelianExponent torsionValue
    & divisorsOf
    & filter (> 1)
    & normalizeTorsionOrders

finiteAbelianPrimaryOrderSupport :: Integer -> FiniteAbelianTorsion -> Maybe [Integer]
finiteAbelianPrimaryOrderSupport primeValue torsionValue =
  if not (isPrime primeValue)
    then Nothing
    else
      let primaryExponent =
            finiteAbelianInvariants torsionValue
              & fmap (primePowerPart primeValue)
              & foldr lcm 1
       in Just (primePowerOrdersUpToExponent primeValue primaryExponent)

finiteAbelianExactOrderElementCount :: Integer -> FiniteAbelianTorsion -> Integer
finiteAbelianExactOrderElementCount orderValue =
  countExactOrderElements orderValue . finiteAbelianInvariants

finiteAbelianMatchingInvariants :: Maybe Integer -> FiniteAbelianTorsion -> [Integer]
finiteAbelianMatchingInvariants orderConstraint torsionValue =
  finiteAbelianInvariants torsionValue
    & filter (matchesOptional (fmap abs orderConstraint) . abs)

matchesOptional :: Eq a => Maybe a -> a -> Bool
matchesOptional = maybe (const True) (==)

normalizeInvariantFactors :: [Integer] -> [Integer]
normalizeInvariantFactors =
  List.sort . filter (> 1) . fmap abs

normalizeTorsionOrders :: [Integer] -> [Integer]
normalizeTorsionOrders =
  deduplicateAdjacentValues . List.sort . filter (> 1) . fmap abs

deduplicateAdjacentValues :: Eq value => [value] -> [value]
deduplicateAdjacentValues =
  foldr
    ( \value deduplicatedValues ->
        case deduplicatedValues of
          nextValue : _
            | value == nextValue -> deduplicatedValues
          _ -> value : deduplicatedValues
    )
    []

primePowerOrdersUpToExponent :: Integer -> Integer -> [Integer]
primePowerOrdersUpToExponent primeValue exponentValue =
  let normalizedPrime = abs primeValue
      normalizedExponent = abs exponentValue
   in if not (isPrime normalizedPrime) || normalizedExponent <= 1
        then []
        else
          iterate (* normalizedPrime) normalizedPrime
            & takeWhile (<= normalizedExponent)

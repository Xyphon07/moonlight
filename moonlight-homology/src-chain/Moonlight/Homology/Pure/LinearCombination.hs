module Moonlight.Homology.Pure.LinearCombination
  ( LinearCombination,
    LinearCombinationArithmetic (..),
    numArithmetic,
    ringArithmetic,
    normalizeWith,
    composeWith,
    addWith,
    subtractWith,
    identityWith,
    checkLawWith,
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Moonlight.Core (AdditiveGroup (..), AdditiveMonoid (..), MultiplicativeMonoid (..), Ring)
import Moonlight.Homology.Pure.Failure (HomologyFailure (..), HomologyLaw)

type LinearCombination :: Type -> Type -> Type
type LinearCombination coefficient basis = [(coefficient, basis)]

type LinearCombinationArithmetic :: Type -> Type
data LinearCombinationArithmetic coefficient = LinearCombinationArithmetic
  { lcaZero :: !coefficient,
    lcaOne :: !coefficient,
    lcaAdd :: !(coefficient -> coefficient -> coefficient),
    lcaNegate :: !(coefficient -> coefficient),
    lcaMultiply :: !(coefficient -> coefficient -> coefficient)
  }

numArithmetic :: Num coefficient => LinearCombinationArithmetic coefficient
numArithmetic =
  LinearCombinationArithmetic
    { lcaZero = 0,
      lcaOne = 1,
      lcaAdd = (+),
      lcaNegate = negate,
      lcaMultiply = (*)
    }

ringArithmetic :: Ring coefficient => LinearCombinationArithmetic coefficient
ringArithmetic =
  LinearCombinationArithmetic
    { lcaZero = zero,
      lcaOne = one,
      lcaAdd = add,
      lcaNegate = neg,
      lcaMultiply = mul
    }

normalizeWith ::
  (Eq coefficient, Ord basis) =>
  LinearCombinationArithmetic coefficient ->
  LinearCombination coefficient basis ->
  LinearCombination coefficient basis
normalizeWith arithmetic =
  fmap (\(basisValue, coefficientValue) -> (coefficientValue, basisValue))
    . Map.toAscList
    . Map.filter (/= lcaZero arithmetic)
    . Map.fromListWith (lcaAdd arithmetic)
    . fmap (\(coefficientValue, basisValue) -> (basisValue, coefficientValue))

composeWith ::
  (Eq coefficient, Ord targetBasis) =>
  LinearCombinationArithmetic coefficient ->
  (sourceBasis -> LinearCombination coefficient targetBasis) ->
  LinearCombination coefficient sourceBasis ->
  LinearCombination coefficient targetBasis
composeWith arithmetic mapping combination =
  normalizeWith arithmetic
    [ (lcaMultiply arithmetic sourceCoefficient targetCoefficient, targetBasis)
    | (sourceCoefficient, sourceBasis) <- combination,
      (targetCoefficient, targetBasis) <- mapping sourceBasis
    ]

addWith ::
  (Eq coefficient, Ord basis) =>
  LinearCombinationArithmetic coefficient ->
  LinearCombination coefficient basis ->
  LinearCombination coefficient basis ->
  LinearCombination coefficient basis
addWith arithmetic leftCombination rightCombination =
  normalizeWith arithmetic (leftCombination <> rightCombination)

subtractWith ::
  (Eq coefficient, Ord basis) =>
  LinearCombinationArithmetic coefficient ->
  LinearCombination coefficient basis ->
  LinearCombination coefficient basis ->
  LinearCombination coefficient basis
subtractWith arithmetic leftCombination rightCombination =
  normalizeWith arithmetic
    (leftCombination <> fmap (negateTermWith arithmetic) rightCombination)

identityWith ::
  LinearCombinationArithmetic coefficient ->
  basis ->
  LinearCombination coefficient basis
identityWith arithmetic basisValue =
  [(lcaOne arithmetic, basisValue)]

checkLawWith ::
  (Eq coefficient, Ord targetBasis) =>
  LinearCombinationArithmetic coefficient ->
  HomologyLaw ->
  [sourceBasis] ->
  (sourceBasis -> LinearCombination coefficient targetBasis) ->
  (sourceBasis -> LinearCombination coefficient targetBasis) ->
  Either HomologyFailure ()
checkLawWith arithmetic law basisElements leftSide rightSide =
  if all (lawHoldsWith arithmetic leftSide rightSide) basisElements
    then Right ()
    else Left (LawViolation law)

lawHoldsWith ::
  (Eq coefficient, Ord targetBasis) =>
  LinearCombinationArithmetic coefficient ->
  (sourceBasis -> LinearCombination coefficient targetBasis) ->
  (sourceBasis -> LinearCombination coefficient targetBasis) ->
  sourceBasis ->
  Bool
lawHoldsWith arithmetic leftSide rightSide basisValue =
  normalizeWith arithmetic (leftSide basisValue)
    == normalizeWith arithmetic (rightSide basisValue)

negateTermWith ::
  LinearCombinationArithmetic coefficient ->
  (coefficient, basis) ->
  (coefficient, basis)
negateTermWith arithmetic (coefficientValue, basisValue) =
  (lcaNegate arithmetic coefficientValue, basisValue)

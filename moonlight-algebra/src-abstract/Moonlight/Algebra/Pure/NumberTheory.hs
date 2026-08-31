-- | Elementary number theory: primality, divisors, prime-power factorisation,
-- the Moebius function, and counts of group elements by multiplicative order.
module Moonlight.Algebra.Pure.NumberTheory
  ( countElementsWithOrderDividing,
    countExactOrderElements,
    divisorsFromPrimePowers,
    divisorsOf,
    isPrime,
    mobiusValue,
    primePowerFactors,
    primePowerPart,
  )
where

import Data.Function ((&))
import Moonlight.Algebra.Pure.GCD (gcd)
import Prelude hiding (gcd)

countExactOrderElements :: Integer -> [Integer] -> Integer
countExactOrderElements orderValue cyclicOrders
  | normalizedOrder <= 0 = 0
  | otherwise =
      divisorsOf normalizedOrder
        & fmap
          ( \divisorValue ->
              mobiusValue (normalizedOrder `div` divisorValue)
                * countElementsWithOrderDividing divisorValue cyclicOrders
          )
        & sum
  where normalizedOrder = abs orderValue

countElementsWithOrderDividing :: Integer -> [Integer] -> Integer
countElementsWithOrderDividing divisorValue =
  product . fmap (\cyclicOrder -> gcd (abs cyclicOrder) divisorValue)

mobiusValue :: Integer -> Integer
mobiusValue value
  | normalizedValue == 0 = 0
  | primePowers & any ((> 1) . snd) = 0
  | even (length primePowers) = 1
  | otherwise = -1
  where
    normalizedValue = abs value
    primePowers = primePowerFactors normalizedValue

divisorsOf :: Integer -> [Integer]
divisorsOf value
  | normalizedValue <= 0 = []
  | otherwise = divisorsFromPrimePowers (primePowerFactors normalizedValue)
  where normalizedValue = abs value

divisorsFromPrimePowers :: [(Integer, Int)] -> [Integer]
divisorsFromPrimePowers primePowers =
  case primePowers of
    [] -> [1]
    (primeValue, exponentValue) : remainingPrimePowers ->
      let remainingDivisors = divisorsFromPrimePowers remainingPrimePowers
          primePowersAtFactor = take (exponentValue + 1) (iterate (* primeValue) 1)
       in primePowersAtFactor >>= (\powerValue -> fmap (powerValue *) remainingDivisors)

primePowerFactors :: Integer -> [(Integer, Int)]
primePowerFactors value =
  factorFrom 2 (abs value)

primePowerPart :: Integer -> Integer -> Integer
primePowerPart primeValue value =
  let normalizedPrime = abs primeValue
      normalizedValue = abs value
   in if not (isPrime normalizedPrime)
        then 1
        else
          case lookup normalizedPrime (primePowerFactors normalizedValue) of
            Nothing -> 1
            Just exponentValue -> normalizedPrime ^ exponentValue

isPrime :: Integer -> Bool
isPrime value =
  let normalizedValue = abs value
   in normalizedValue > 1
        && primePowerFactors normalizedValue == [(normalizedValue, 1)]

factorFrom :: Integer -> Integer -> [(Integer, Int)]
factorFrom candidateValue remainingValue
  | remainingValue <= 1 = []
  | candidateValue * candidateValue > remainingValue = [(remainingValue, 1)]
  | remainingValue `mod` candidateValue == 0 =
      let (multiplicityValue, reducedValue) = factorMultiplicity candidateValue remainingValue 0
       in (candidateValue, multiplicityValue) : factorFrom (candidateValue + 1) reducedValue
  | otherwise = factorFrom (candidateValue + 1) remainingValue

factorMultiplicity :: Integer -> Integer -> Int -> (Int, Integer)
factorMultiplicity primeValue remainingValue multiplicityValue
  | remainingValue `mod` primeValue == 0 =
      factorMultiplicity primeValue (remainingValue `div` primeValue) (multiplicityValue + 1)
  | otherwise = (multiplicityValue, remainingValue)

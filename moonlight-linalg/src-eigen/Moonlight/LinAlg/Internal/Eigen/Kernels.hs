{-# LANGUAGE BangPatterns #-}

module Moonlight.LinAlg.Internal.Eigen.Kernels
  ( copySignMagnitude,
    epsDouble,
    finiteDouble,
    forDescendingIndex,
    forIndex,
    hypotStable,
    maxFiniteDouble,
    safeMinimumDouble,
  )
where

import Control.Monad.ST (ST)
import Moonlight.Core (fieldValueValid)
import Prelude

-- | IEEE-754 binary64 machine epsilon. The eigensolver's convergence tests need
-- the floating-point unit scale, not Moonlight's broader domain tolerance.
epsDouble :: Double
epsDouble = encodeFloat 1 (-52)
{-# NOINLINE epsDouble #-}

safeMinimumDouble :: Double
safeMinimumDouble = encodeFloat 1 (-1022)
{-# NOINLINE safeMinimumDouble #-}

maxFiniteDouble :: Double
maxFiniteDouble = encodeFloat 0x1fffffffffffff (971)
{-# NOINLINE maxFiniteDouble #-}

finiteDouble :: Double -> Bool
finiteDouble = fieldValueValid
{-# INLINE finiteDouble #-}

copySignMagnitude :: Double -> Double -> Double
copySignMagnitude !magnitudeValue !signReference =
  if signReference < 0.0 || isNegativeZero signReference
    then negate (abs magnitudeValue)
    else abs magnitudeValue
{-# INLINE copySignMagnitude #-}

hypotStable :: Double -> Double -> Double
hypotStable !leftValue !rightValue =
  let !leftAbs = abs leftValue
      !rightAbs = abs rightValue
   in if leftAbs < rightAbs
        then
          if rightAbs == 0.0
            then 0.0
            else
              let !scaled = leftAbs / rightAbs
               in rightAbs * sqrt (1.0 + scaled * scaled)
        else
          if leftAbs == 0.0
            then 0.0
            else
              let !scaled = rightAbs / leftAbs
               in leftAbs * sqrt (1.0 + scaled * scaled)
{-# INLINE hypotStable #-}

forIndex :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forIndex !startIndex !stopIndex action = go startIndex
  where
    go !indexValue
      | indexValue >= stopIndex = pure ()
      | otherwise = action indexValue >> go (indexValue + 1)
{-# INLINE forIndex #-}

forDescendingIndex :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forDescendingIndex !startIndex !stopIndex action = go startIndex
  where
    go !indexValue
      | indexValue < stopIndex = pure ()
      | otherwise = action indexValue >> go (indexValue - 1)
{-# INLINE forDescendingIndex #-}

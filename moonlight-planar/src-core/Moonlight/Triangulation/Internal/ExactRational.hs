{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}

-- | Normalized exact rational arithmetic without geometric dependencies.
module Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , ExactArithmeticError (..)
  , exactRational
  , exactRationalFromDouble
  , exactRationalFromFiniteDouble
  , exactRationalFromDyadic
  , exactRationalFromDyadicRatio
  , exactRationalFromNormalizedRatio
  , exactRationalNumerator
  , exactRationalDenominator
  , exactRationalBitWidth
  , exactRationalDenominatorBitWidth
  , exactRationalIsZero
  , exactDivide
  , exactSignum
  ) where

import Control.DeepSeq (NFData)
import Data.Bits ((.&.), shiftL, shiftR)
import Data.Ratio (Ratio, (%))
import qualified Data.Ratio as Ratio
import GHC.Generics (Generic)
import GHC.Exts (Int (I#))
import GHC.Integer.Logarithms (integerLog2#)
import GHC.Real (Ratio ((:%)))

-- | A checked wrapper around a reduced ratio with a strictly positive
-- denominator. 'Ratio' owns normalization, including the unique zero
-- representation @0 / 1@.
newtype ExactRational = ExactRational (Ratio Integer)
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num)
  deriving anyclass (NFData)

-- | Typed refusals from exact rational construction and division.
data ExactArithmeticError
  = ExactZeroDenominator
  | ExactZeroDivisor
  | ExactNaNInput
  | ExactInfiniteInput
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Construct a reduced rational, moving any sign onto the numerator and
-- refusing a zero denominator.
exactRational :: Integer -> Integer -> Either ExactArithmeticError ExactRational
exactRational _ 0 = Left ExactZeroDenominator
exactRational numerator denominator = Right (ExactRational (numerator % denominator))

-- | Convert a binary64 value without loss, refusing NaN and infinities.
exactRationalFromDouble :: Double -> Either ExactArithmeticError ExactRational
exactRationalFromDouble value
  | isNaN value = Left ExactNaNInput
  | isInfinite value = Left ExactInfiniteInput
  | otherwise = Right (exactRationalFromFiniteDouble value)

-- | Convert an already-admitted finite coordinate without loss. The caller
-- supplies a finite value, such as a coordinate inside a validated
-- @QueryPoint@, so this worker does not repeat the admission refusal.
exactRationalFromFiniteDouble :: Double -> ExactRational
exactRationalFromFiniteDouble value =
  uncurry exactRationalFromDyadic (decodeFloat value)
{-# INLINE exactRationalFromFiniteDouble #-}

-- | Construct @numerator * 2^power@ without a partial denominator path.
exactRationalFromDyadic :: Integer -> Int -> ExactRational
exactRationalFromDyadic numerator power
  | numerator == 0 = ExactRational (0 :% 1)
  | power >= 0 = ExactRational ((numerator `shiftL` power) :% 1)
  | otherwise =
      let denominatorPower = negate power
          removableFactor =
            min denominatorPower (integerTrailingZeroBits numerator)
          reducedNumerator = numerator `shiftR` removableFactor
          reducedDenominator =
            1 `shiftL` (denominatorPower - removableFactor)
       in ExactRational (reducedNumerator :% reducedDenominator)
{-# INLINE exactRationalFromDyadic #-}

-- | The denominator of a dyadic rational is a power of two, so its complete
-- normalization requires only the numerator's least set bit. Calling generic
-- rational GCD here merely rediscovers that closed arithmetic fact.
integerTrailingZeroBits :: Integer -> Int
integerTrailingZeroBits value =
  let magnitude = abs value
   in I# (integerLog2# (magnitude .&. negate magnitude))
{-# INLINE integerTrailingZeroBits #-}

-- | Construct @(numerator / denominator) * 2^power@ with one final rational
-- normalization. This is the publication boundary for exact dyadic kernels:
-- their integer arithmetic must not pay a greatest-common-divisor reduction
-- after every intermediate operation.
exactRationalFromDyadicRatio
  :: Integer
  -> Integer
  -> Int
  -> Either ExactArithmeticError ExactRational
exactRationalFromDyadicRatio _ 0 _ = Left ExactZeroDivisor
exactRationalFromDyadicRatio 0 _ _ = Right (ExactRational (0 :% 1))
exactRationalFromDyadicRatio numerator denominator power =
  let numeratorFactor = integerTrailingZeroBits numerator
      denominatorFactor = integerTrailingZeroBits denominator
      oddNumerator = numerator `shiftR` numeratorFactor
      oddDenominator = denominator `shiftR` denominatorFactor
      residualPower = power + numeratorFactor - denominatorFactor
   in if residualPower >= 0
        then exactRational (oddNumerator `shiftL` residualPower) oddDenominator
        else exactRational oddNumerator (oddDenominator `shiftL` negate residualPower)
{-# INLINE exactRationalFromDyadicRatio #-}

-- | Internal bridge from the normalized carrier owned by @Data.Ratio@. This
-- exists for statically nonzero rational constants in exact kernels; public
-- callers continue through 'exactRational'.
exactRationalFromNormalizedRatio :: Ratio Integer -> ExactRational
exactRationalFromNormalizedRatio = ExactRational
{-# INLINE exactRationalFromNormalizedRatio #-}

-- | Read the reduced numerator.
exactRationalNumerator :: ExactRational -> Integer
exactRationalNumerator (ExactRational value) = Ratio.numerator value
{-# INLINE exactRationalNumerator #-}

-- | Read the strictly positive reduced denominator.
exactRationalDenominator :: ExactRational -> Integer
exactRationalDenominator (ExactRational value) = Ratio.denominator value
{-# INLINE exactRationalDenominator #-}

-- | Maximum bit width of the reduced numerator magnitude and strictly
-- positive denominator. This is an observation, not an arithmetic bound.
exactRationalBitWidth :: ExactRational -> Int
exactRationalBitWidth value =
  max
    (integerBitWidth (abs (exactRationalNumerator value)))
    (exactRationalDenominatorBitWidth value)
{-# INLINE exactRationalBitWidth #-}

-- | Bit width of the reduced, strictly positive denominator.
exactRationalDenominatorBitWidth :: ExactRational -> Int
exactRationalDenominatorBitWidth =
  integerBitWidth . exactRationalDenominator
{-# INLINE exactRationalDenominatorBitWidth #-}

integerBitWidth :: Integer -> Int
integerBitWidth value
  | value <= 0 = 0
  | otherwise = I# (integerLog2# value) + 1
{-# INLINE integerBitWidth #-}

-- | Test whether the exact value is zero.
exactRationalIsZero :: ExactRational -> Bool
exactRationalIsZero (ExactRational value) = Ratio.numerator value == 0
{-# INLINE exactRationalIsZero #-}

-- | Divide by a nonzero exact rational, refusing a zero divisor explicitly.
exactDivide
  :: ExactRational
  -> ExactRational
  -> Either ExactArithmeticError ExactRational
exactDivide (ExactRational left) (ExactRational right)
  | Ratio.numerator right == 0 = Left ExactZeroDivisor
  | otherwise = Right (ExactRational (left / right))

-- | Compare an exact rational with zero through its canonical numerator.
exactSignum :: ExactRational -> Ordering
exactSignum (ExactRational value) = compare (Ratio.numerator value) 0
{-# INLINE exactSignum #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}

-- | Normalized exact rational arithmetic without geometric dependencies.
module Moonlight.Planar.Internal.ExactRational
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
  , PositiveExact
  , UnitInterval
  , ScalarRefinementError (..)
  , positiveExact
  , positiveExactValue
  , unitInterval
  , unitIntervalValue
  , unitZero
  , unitHalf
  , unitOne
  , positiveOne
  , positiveTwo
  , exactHalf
  , exactThird
  , blendPositive
  , unitMidpoint
  , unitBetween
  , unitClamp
  , unitIntervalRun
  , divideByPositive
  , ratioPositive
  , multiplyPositive
  , positiveSumSquares
  ) where

import Control.DeepSeq (NFData (..))
import Data.Bits ((.&.), shiftL, shiftR)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio (Ratio, (%))
import qualified Data.Ratio as Ratio
import GHC.Generics (Generic)
import GHC.Exts (Int (I#))
import GHC.Integer.Logarithms (integerLog2#)
import GHC.Real (Ratio ((:%)))
import Numeric.Natural (Natural)

-- | A checked wrapper around a reduced ratio with a strictly positive
-- denominator. 'Ratio' owns normalization, including the unique zero
-- representation @0 / 1@.
newtype ExactRational = ExactRational (Ratio Integer)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

instance NFData ExactRational where
  rnf (ExactRational value) = rnf value
  {-# INLINE rnf #-}

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

-- | A strictly positive scalar. Only admission and positivity-preserving
-- arithmetic in this scalar owner can construct the witness.
newtype PositiveExact = PositiveExact ExactRational
  deriving stock (Eq, Ord, Show)
  deriving newtype (NFData)

-- | A rational parameter in the closed unit interval.
newtype UnitInterval = UnitInterval ExactRational
  deriving stock (Eq, Ord, Show)
  deriving newtype (NFData)

data ScalarRefinementError
  = ExactNotPositive !ExactRational
  | ExactOutsideUnitInterval !ExactRational
  deriving stock (Eq, Ord, Show)

positiveExact :: ExactRational -> Either ScalarRefinementError PositiveExact
positiveExact value
  | value > 0 = Right (PositiveExact value)
  | otherwise = Left (ExactNotPositive value)

positiveExactValue :: PositiveExact -> ExactRational
positiveExactValue (PositiveExact value) = value

unitInterval :: ExactRational -> Either ScalarRefinementError UnitInterval
unitInterval value
  | value >= 0 && value <= 1 = Right (UnitInterval value)
  | otherwise = Left (ExactOutsideUnitInterval value)

unitIntervalValue :: UnitInterval -> ExactRational
unitIntervalValue (UnitInterval value) = value

unitZero, unitHalf, unitOne :: UnitInterval
unitZero = UnitInterval 0
unitHalf = UnitInterval exactHalf
unitOne = UnitInterval 1

positiveOne, positiveTwo :: PositiveExact
positiveOne = PositiveExact 1
positiveTwo = PositiveExact 2

exactHalf, exactThird :: ExactRational
exactHalf = ExactRational (1 :% 2)
exactThird = ExactRational (1 :% 3)

-- | Closed-interval convex blending preserves strict positivity, including
-- either endpoint. No repeated admission is required by its consumers.
blendPositive :: UnitInterval -> PositiveExact -> PositiveExact -> PositiveExact
blendPositive (UnitInterval t) (PositiveExact a) (PositiveExact b) =
  PositiveExact ((1 - t) * a + t * b)

-- | The exact midpoint of two unit parameters. A convex combination of two
-- admitted parameters stays in the closed unit interval without readmission.
unitMidpoint :: UnitInterval -> UnitInterval -> UnitInterval
unitMidpoint (UnitInterval a) (UnitInterval b) = UnitInterval ((a + b) * exactHalf)

-- | The parameter a unit fraction of the way from the first to the second, a
-- convex combination like 'unitMidpoint'.
unitBetween :: UnitInterval -> UnitInterval -> UnitInterval -> UnitInterval
unitBetween (UnitInterval a) (UnitInterval b) (UnitInterval t) = UnitInterval (a + (b - a) * t)

-- | The nearest point of the closed unit interval.
unitClamp :: ExactRational -> UnitInterval
unitClamp value = UnitInterval (max 0 (min 1 value))

-- | @intervals + 1@ evenly spaced points, both endpoints included: @first@,
-- then @first + k (final - first) / intervals@ for @k@ from one to
-- @intervals@. Zero intervals is @first@ alone, whose division is never
-- formed. Each point is a convex combination of the two, so none needs
-- readmission.
unitIntervalRun :: Natural -> UnitInterval -> UnitInterval -> NonEmpty UnitInterval
unitIntervalRun intervals (UnitInterval first) (UnitInterval final) =
  UnitInterval first :| fmap at [1 .. intervals]
 where
  at :: Natural -> UnitInterval
  at k = UnitInterval (first + (final - first) * ExactRational (toInteger k % toInteger intervals))

-- | The same Ratio division as 'exactDivide', with its nonzero precondition
-- already discharged by the opaque positive divisor.
divideByPositive :: ExactRational -> PositiveExact -> ExactRational
divideByPositive (ExactRational a) (PositiveExact (ExactRational b)) =
  ExactRational (a / b)

ratioPositive :: PositiveExact -> PositiveExact -> PositiveExact
ratioPositive (PositiveExact a) b = PositiveExact (divideByPositive a b)

-- | Products of admitted positive scalars remain positive without readmission.
multiplyPositive :: PositiveExact -> PositiveExact -> PositiveExact
multiplyPositive (PositiveExact a) (PositiveExact b) = PositiveExact (a * b)

-- | A squared Euclidean norm is zero exactly when both coordinates vanish.
-- The nonzero branch constructs its positivity witness without re-admission.
positiveSumSquares :: ExactRational -> ExactRational -> Maybe PositiveExact
positiveSumSquares x y
  | x == 0 && y == 0 = Nothing
  | otherwise = Just (PositiveExact (x * x + y * y))

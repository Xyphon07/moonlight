{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Certified Euclidean length arithmetic, independent of geometry: finite
-- sums of rational multiples of square roots in radical normal form, their
-- outward rational enclosures at an admitted precision, and the directed
-- binary64 projection of those enclosures. Region valuation and curve
-- measurement share this one owner.
module Moonlight.Planar.Internal.Length
  ( ExactLengthTerm
  , lengthCoefficient
  , lengthRadicand
  , ExactLengthExpression
  , exactLengthTerms
  , normalizeLengthContributions
  , scaleLengthExpression
  , RadicalPrecision
  , RadicalPrecisionError (..)
  , radicalPrecision
  , radicalPrecisionBits
  , publicationPrecision
  , LengthEnclosure
  , lengthEnclosureLower
  , lengthEnclosureUpper
  , lengthEnclosureWidth
  , enclosureBetween
  , LengthError (..)
  , squareRootEnclosure
  , expressionEnclosure
  , euclideanLengthEnclosure
  , CertifiedInterval (..)
  , certifiedInterval
  , ExactLengthMeasurement
  , exactLengthExpression
  , exactLengthBounds
  , measureLengthExpression
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL)
import Data.Foldable (foldlM)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Ratio as Ratio
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)
import GHC.Generics (Generic)
import Moonlight.Planar.Internal.Dyadic (integerBitLength)
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational
  , exactRationalDenominator
  , exactRationalFromDyadic
  , exactRationalFromFiniteDouble
  , exactRationalFromNormalizedRatio
  , exactRationalNumerator
  , exactSignum
  , PositiveExact
  , positiveExactValue
  , positiveOne
  )

data ExactLengthTerm = ExactLengthTerm
  { lengthCoefficient :: !ExactRational
  , lengthRadicand :: !Integer
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A normalized sum of rational coefficients times square roots of integer
-- radicands. No two radicands differ by a rational square, so equal lengths
-- are one term and rational length is the radicand-1 term; each radicand is
-- reduced by the square factors of the primes below 64 and by a perfect-square
-- cofactor. A squared prime above that bound inside a non-square cofactor is
-- beyond factoring-free reach, so the presentation is canonical within an
-- expression and across expressions only up to such factors; there is no
-- 'Eq' instance for that reason.
newtype ExactLengthExpression = ExactLengthExpression [ExactLengthTerm]
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

exactLengthTerms :: ExactLengthExpression -> [ExactLengthTerm]
exactLengthTerms (ExactLengthExpression terms) = terms

-- | Binary64 endpoints rounded outward from a rational enclosure.
data CertifiedInterval = CertifiedInterval
  { intervalLower :: !Double
  , intervalUpper :: !Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data ExactLengthMeasurement = ExactLengthMeasurement
  { exactLengthExpression :: !ExactLengthExpression
  , exactLengthBounds :: !CertifiedInterval
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-- | Fractional bits of a square-root enclosure: each root is bracketed by
-- consecutive multiples of @2^-bits@, so its width is at most @2^-bits@.
newtype RadicalPrecision = RadicalPrecision Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A root is bracketed by shifting its radicand left by twice the precision,
-- so the doubled count must itself be a representable shift.
data RadicalPrecisionError
  = NonPositiveRadicalPrecision !Int
  | UnrepresentableRadicalPrecision !Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

radicalPrecision :: Int -> Either RadicalPrecisionError RadicalPrecision
radicalPrecision bits
  | bits <= 0 = Left (NonPositiveRadicalPrecision bits)
  | bits > maxBound `quot` 2 = Left (UnrepresentableRadicalPrecision bits)
  | otherwise = Right (RadicalPrecision bits)

radicalPrecisionBits :: RadicalPrecision -> Int
radicalPrecisionBits (RadicalPrecision bits) = bits

-- | 128 bits: far below binary64 resolution for any length a double can
-- represent, so directed publication, not the rational enclosure, dominates
-- the width of a 'CertifiedInterval'.
publicationPrecision :: RadicalPrecision
publicationPrecision = RadicalPrecision 128

-- | An ordered pair of nonnegative rationals, @0 <= lower <= upper@. The sum
-- of enclosures encloses the sum of their values.
data LengthEnclosure = LengthEnclosure !ExactRational !ExactRational
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

instance Semigroup LengthEnclosure where
  LengthEnclosure a b <> LengthEnclosure c d = LengthEnclosure (a + c) (b + d)

instance Monoid LengthEnclosure where
  mempty = LengthEnclosure 0 0

lengthEnclosureLower :: LengthEnclosure -> ExactRational
lengthEnclosureLower (LengthEnclosure lower _) = lower

lengthEnclosureUpper :: LengthEnclosure -> ExactRational
lengthEnclosureUpper (LengthEnclosure _ upper) = upper

lengthEnclosureWidth :: LengthEnclosure -> ExactRational
lengthEnclosureWidth (LengthEnclosure lower upper) = upper - lower

-- | The first's lower bound to the second's upper bound: it encloses every
-- value lying between the first's value and the second's whenever the first
-- is at most the second, as a chord is at most its control polygon. The
-- endpoints are ordered for any pair.
enclosureBetween :: LengthEnclosure -> LengthEnclosure -> LengthEnclosure
enclosureBetween (LengthEnclosure a _) (LengthEnclosure _ d) = LengthEnclosure (min a d) (max a d)

-- | A negative square has no real root.
newtype LengthError = LengthNegativeSquare ExactRational
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Positive coefficients stay positive through merging, whose scales are
-- positive integers, so every normalized term has a positive coefficient.
-- This and 'scaleLengthExpression' are the only expression builders, which is
-- the invariant 'expressionEnclosure' relies on.
normalizeLengthContributions
  :: Foldable collection
  => (value -> (PositiveExact, ExactRational))
  -> collection value
  -> ExactLengthExpression
normalizeLengthContributions contribution =
  normalizeSquareCoefficients . List.foldl' accumulateContribution Map.empty
 where
  accumulateContribution coefficients value =
    let (coefficient, square) = contribution value
     in Map.insertWith (+) square (positiveExactValue coefficient) coefficients
-- Only the per-contribution fold is inlined, so each caller's contribution
-- fuses into it and no pair is built per element; the class merging below is
-- shared out of line.
{-# INLINE normalizeLengthContributions #-}

-- | Merge per-square coefficients into radical classes.
normalizeSquareCoefficients :: Map.Map ExactRational ExactRational -> ExactLengthExpression
normalizeSquareCoefficients coefficientsBySquare =
  ExactLengthExpression
    [ ExactLengthTerm coefficient radicand
    | (radicand, coefficient) <- List.sortOn fst (concat (Map.elems classes))
    ]
 where
  classes =
    Map.foldlWithKey' accumulateRadical Map.empty coefficientsBySquare
  accumulateRadical buckets square coefficient =
    let reduced = reduceRadicand square
     in if radicalRadicand reduced == 0
          then buckets
          else
            Map.alter
              ( Just
                  . mergeRadical (coefficient * radicalScale reduced) (radicalRadicand reduced)
                  . fromMaybe []
              )
              (radicalClassKey reduced)
              buckets

data ReducedRadical = ReducedRadical
  { radicalScale :: !ExactRational
  , radicalRadicand :: !Integer
  , radicalClassKey :: !Word64
  }

-- | Write √(n/d) as (1/d)·√(n·d), fold the even part of each trial prime's
-- multiplicity and a perfect-square cofactor into the scale, and key the
-- radicand by what those primes observe of its square class: the parity of
-- the multiplicity and the quadratic character of the prime-free part, both
-- invariant under multiplication by rational squares.
reduceRadicand :: ExactRational -> ReducedRadical
reduceRadicand square
  | radicand <= 0 = ReducedRadical inverseDenominator radicand 0
  | otherwise =
      let (cofactor, reduced) = List.foldl' stripPrime (radicand, initial) radicalPrimes
          root = integerSquareRoot cofactor
       in if root * root == cofactor
            then reduced {radicalScale = radicalScale reduced * fromInteger root}
            else reduced {radicalRadicand = radicalRadicand reduced * cofactor}
 where
  denominator = exactRationalDenominator square
  radicand = exactRationalNumerator square * denominator
  inverseDenominator = exactRationalFromNormalizedRatio (1 Ratio.% denominator)
  initial = ReducedRadical inverseDenominator 1 0
  stripPrime (remaining, reduced) (prime, residues) =
    let (multiplicity, rest) = primeMultiplicity prime remaining
        (halfPower, parity) = multiplicity `divMod` 2
        character = squareClassCharacter prime residues rest
     in ( rest
        , ReducedRadical
            { radicalScale = radicalScale reduced * fromInteger (prime ^ halfPower)
            , radicalRadicand = radicalRadicand reduced * prime ^ parity
            , radicalClassKey = radicalClassKey reduced * 8 + fromIntegral (parity * 4 + character)
            }
        )

radicalPrimes :: [(Integer, IntSet.IntSet)]
radicalPrimes =
  [ (toInteger prime, IntSet.fromList [(x * x) `mod` prime | x <- [1 .. prime - 1]])
  | prime <- [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61 :: Int]
  ]

primeMultiplicity :: Integer -> Integer -> (Int, Integer)
primeMultiplicity prime = go 0
 where
  go :: Int -> Integer -> (Int, Integer)
  go !count value =
    case value `quotRem` prime of
      (quotient, 0) -> go (count + 1) quotient
      _ -> (count, value)

squareClassCharacter :: Integer -> IntSet.IntSet -> Integer -> Int
squareClassCharacter 2 _ rest = fromInteger ((rest `mod` 8) `div` 2)
squareClassCharacter prime residues rest =
  if IntSet.member (fromInteger (rest `mod` prime)) residues then 1 else 0

-- | Fold a term into its square class: the product of two radicands of one
-- class is a perfect square, whose root exposes the rational square relating
-- them, so both fold onto their common divisor with integer scales.
mergeRadical
  :: ExactRational
  -> Integer
  -> [(Integer, ExactRational)]
  -> [(Integer, ExactRational)]
mergeRadical coefficient radicand = go
 where
  go [] = [(radicand, coefficient)]
  go ((kernel, total) : rest)
    | pairRoot * pairRoot == pairProduct =
        let common = gcd pairRoot kernel
            newScale = pairRoot `div` common
            oldScale = kernel `div` common
         in ( kernel `div` (oldScale * oldScale)
            , total * fromInteger oldScale + coefficient * fromInteger newScale
            )
              : rest
    | otherwise = (kernel, total) : go rest
   where
    pairProduct = radicand * kernel
    pairRoot = integerSquareRoot pairProduct

scaleLengthExpression
  :: PositiveExact
  -> ExactLengthExpression
  -> ExactLengthExpression
scaleLengthExpression scalar (ExactLengthExpression terms) =
  let exactScalar = positiveExactValue scalar
   in ExactLengthExpression
        [ term
            { lengthCoefficient =
                exactScalar * lengthCoefficient term
            }
        | term <- terms
        ]

-- | The root of a nonnegative rational between consecutive multiples of
-- @2^-bits@; an exact dyadic root has zero width.
squareRootEnclosure
  :: RadicalPrecision
  -> ExactRational
  -> Either LengthError LengthEnclosure
squareRootEnclosure (RadicalPrecision bits) value =
  case exactSignum value of
    LT -> Left (LengthNegativeSquare value)
    _ -> Right (uncurry LengthEnclosure (rootBounds bits value))

-- | Consecutive multiples of @2^-bits@ around the root of a nonnegative value.
rootBounds :: Int -> ExactRational -> (ExactRational, ExactRational)
rootBounds bits value =
  let numerator = exactRationalNumerator value
      denominator = exactRationalDenominator value
      scaledNumerator = numerator `shiftL` (2 * bits)
      root = integerSquareRoot (scaledNumerator `div` denominator)
      exact = root * root * denominator == scaledNumerator
      dyadicPower = negate bits
   in ( exactRationalFromDyadic root dyadicPower
      , exactRationalFromDyadic (if exact then root else root + 1) dyadicPower
      )

-- | Outward enclosure of the expression's value. Coefficients are positive by
-- construction, so each term scales its root enclosure without reordering; a
-- negative radicand, from a negative squared length, is refused.
expressionEnclosure
  :: RadicalPrecision
  -> ExactLengthExpression
  -> Either LengthError LengthEnclosure
expressionEnclosure precision (ExactLengthExpression terms) =
  foldlM addTerm mempty terms
 where
  addTerm (LengthEnclosure lower upper) (ExactLengthTerm coefficient radicand) = do
    LengthEnclosure lowerRoot upperRoot <-
      squareRootEnclosure precision (fromInteger radicand)
    pure (LengthEnclosure (lower + coefficient * lowerRoot) (upper + coefficient * upperRoot))

-- | Outward enclosure of the summed Euclidean norms of rational displacements,
-- through the radical normal form. Every contribution has coefficient one and
-- a nonnegative square, so every normalized term has a positive coefficient
-- and a positive radicand, and the enclosure needs no refusal. Each term's
-- endpoints are rounded outward onto multiples of @2^-bits@, so sums of these
-- enclosures stay on that grid instead of accumulating the coefficients'
-- denominators.
euclideanLengthEnclosure
  :: RadicalPrecision
  -> [(ExactRational, ExactRational)]
  -> LengthEnclosure
euclideanLengthEnclosure (RadicalPrecision bits) displacements =
  foldMap termEnclosure
    (exactLengthTerms (normalizeLengthContributions (\(x, y) -> (positiveOne, x * x + y * y)) displacements))
 where
  termEnclosure (ExactLengthTerm coefficient radicand) =
    let (lower, upper) = rootBounds bits (fromInteger radicand)
     in LengthEnclosure (gridFloor (coefficient * lower)) (negate (gridFloor (negate (coefficient * upper))))
  gridFloor value = exactRationalFromDyadic
    ((exactRationalNumerator value `shiftL` bits) `div` exactRationalDenominator value) (negate bits)

-- | Directed binary64 publication of a rational enclosure.
certifiedInterval :: LengthEnclosure -> CertifiedInterval
certifiedInterval (LengthEnclosure lower upper) =
  CertifiedInterval
    { intervalLower = directedLowerDouble lower
    , intervalUpper = directedUpperDouble upper
    }

measureLengthExpression
  :: RadicalPrecision
  -> ExactLengthExpression
  -> Either LengthError ExactLengthMeasurement
measureLengthExpression precision expression =
  ExactLengthMeasurement expression . certifiedInterval
    <$> expressionEnclosure precision expression

integerSquareRoot :: Integer -> Integer
integerSquareRoot value
  | value < 2 = value
  | otherwise = descend initial
 where
  initial = 1 `shiftL` ((integerBitLength value + 1) `div` 2)
  descend estimate =
    let refined = (estimate + value `div` estimate) `div` 2
     in if refined >= estimate then estimate else descend refined

directedLowerDouble :: ExactRational -> Double
directedLowerDouble value =
  let candidate = rationalToDouble value
   in if isInfinite candidate
        then maximumFiniteDouble
        else
          if exactRationalFromFiniteDouble candidate <= value
            then candidate
            else previousPositiveDouble candidate

directedUpperDouble :: ExactRational -> Double
directedUpperDouble value =
  let candidate = rationalToDouble value
   in if isInfinite candidate
        || exactRationalFromFiniteDouble candidate >= value
        then candidate
        else nextPositiveDouble candidate

rationalToDouble :: ExactRational -> Double
rationalToDouble value =
  fromRational
    ( exactRationalNumerator value
        Ratio.% exactRationalDenominator value
    )

previousPositiveDouble :: Double -> Double
previousPositiveDouble value
  | value <= 0 = 0
  | otherwise = castWord64ToDouble (castDoubleToWord64 value - 1)

nextPositiveDouble :: Double -> Double
nextPositiveDouble value
  | value == 0 = castWord64ToDouble 1
  | otherwise = castWord64ToDouble (castDoubleToWord64 value + 1)

maximumFiniteDouble :: Double
maximumFiniteDouble = castWord64ToDouble 0x7fefffffffffffff

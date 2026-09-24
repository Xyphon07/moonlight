{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

-- | The binary64 coordinate kernel and exact predicate boundary.
module Moonlight.Planar.Scalar
  ( scalarName
  , scalarByteSize
  , scalarBinaryFormat
  , scalarEpsilon
  , scalarUnitRoundoff
  , scalarCcwErrorBound
  , scalarInCircleErrorBound
  , circumradiusSquaredWithinCoordinates
  , BinaryFormat
  , formatRadix
  , formatMantissaDigits
  , formatExponentRange
  , minimumAllowedCoordinate
  , maximumAllowedCoordinate
  , isFinite
  , canonicalScalarZero
  , CoordinateError (..)
  , validateCoordinate
  , NonFiniteValue (..)
  , classifyNonFinite
  , RadiusSquared
  , RadiusSquaredError (..)
  , mkRadiusSquared
  , radiusSquaredValue
  ) where

import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic)
import Moonlight.Planar.Internal.Dyadic
  ( exactCircumradiusSquaredWithin
  )

-- | The coordinate component of canonical point identity. IEEE signed zeros
-- compare equal but hash differently by bits; every coordinate-keyed owner
-- therefore normalizes them before storage or hashing.
canonicalScalarZero :: Double -> Double
canonicalScalarZero value
  | value == 0 = 0
  | otherwise = value
{-# INLINE canonicalScalarZero #-}

-- | Machine format of the coordinate scalar.
data BinaryFormat = BinaryFormat
  { formatRadix :: !Integer
    -- ^ Numeric base of the significand.
  , formatMantissaDigits :: !Int
    -- ^ Number of base-'formatRadix' digits in the significand.
  , formatExponentRange :: !(Int, Int)
    -- ^ Inclusive minimum and exclusive maximum exponent bounds.
  }
  deriving stock (Eq, Show)

-- | Stable name of the coordinate scalar.
scalarName :: String
scalarName = "binary64"

-- | Bytes occupied by one coordinate component.
scalarByteSize :: Int
scalarByteSize = 8

-- | Runtime-confirmed binary format of 'Double'.
scalarBinaryFormat :: BinaryFormat
scalarBinaryFormat =
  BinaryFormat
    { formatRadix = floatRadix (0 :: Double)
    , formatMantissaDigits = floatDigits (0 :: Double)
    , formatExponentRange = floatRange (0 :: Double)
    }

-- | Difference between one and the next representable value above one.
scalarEpsilon :: Double
scalarEpsilon = 2.220446049250313e-16

-- | Maximum relative rounding error of one binary64 operation.
scalarUnitRoundoff :: Double
scalarUnitRoundoff = 1.1102230246251565e-16

-- | Error coefficient for the filtered orientation predicate.
scalarCcwErrorBound :: Double
scalarCcwErrorBound = 3.3306690738754716e-16

-- | Error coefficient for the filtered in-circle predicate.
scalarInCircleErrorBound :: Double
scalarInCircleErrorBound = 1.1102230246251577e-15

-- | Closed exact circumradius membership at a finite, non-negative binary64
-- threshold. Invalid thresholds and collinear triples are outside.
circumradiusSquaredWithinCoordinates
  :: Double
  -> Double -> Double -> Double -> Double -> Double -> Double
  -> Bool
circumradiusSquaredWithinCoordinates threshold ax ay bx by cx cy
    | threshold < 0 || not (isFinite threshold) = False
    | not (isFinite ax && isFinite ay && isFinite bx
        && isFinite by && isFinite cx && isFinite cy) = False
    | isFinite difference && isFinite tolerance
        && tolerance > 0 && abs difference > tolerance = difference <= 0
    | otherwise = exactCircumradiusSquaredWithin threshold ax ay bx by cx cy
   where
    !abx = bx - ax
    !aby = by - ay
    !acx = cx - ax
    !acy = cy - ay
    !bcx = cx - bx
    !bcy = cy - by
    !abSquared = abx * abx + aby * aby
    !acSquared = acx * acx + acy * acy
    !bcSquared = bcx * bcx + bcy * bcy
    !determinant = abx * acy - aby * acx
    !radiusNumerator = abSquared * acSquared * bcSquared
    !thresholdDenominator = 4 * determinant * determinant * threshold
    !difference = radiusNumerator - thresholdDenominator
    !permanent = abs radiusNumerator + abs thresholdDenominator
    !tolerance = 128 * scalarUnitRoundoff * permanent

{-# INLINE circumradiusSquaredWithinCoordinates #-}

-- | Whether a scalar is neither infinite nor NaN. Pure subtraction avoids the
-- FFI calls used by base's predicates in the supported GHC.
isFinite :: Double -> Bool
isFinite value = value - value == 0
{-# INLINE isFinite #-}

-- | The smallest coordinate magnitude the exact predicates accept.
minimumAllowedCoordinate :: Double
minimumAllowedCoordinate = 1.793662034335766e-43

-- | The largest coordinate magnitude the exact predicates accept.
maximumAllowedCoordinate :: Double
maximumAllowedCoordinate = 3.2138760885179806e60

-- | Reason a floating-point coordinate cannot enter the exact-predicate domain.
data CoordinateError
  = CoordinateNaN
  | CoordinateInfinite
  | CoordinateTooSmall
  | CoordinateTooLarge
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Classification retained when a numeric parameter is not finite.
data NonFiniteValue
  = ValueNaN
  | ValuePositiveInfinity
  | ValueNegativeInfinity
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Classify NaN and signed infinity, leaving finite values unclassified.
classifyNonFinite :: Double -> Maybe NonFiniteValue
classifyNonFinite value
  | isNaN value = Just ValueNaN
  | isInfinite value && value < 0 = Just ValueNegativeInfinity
  | isInfinite value = Just ValuePositiveInfinity
  | otherwise = Nothing

-- | An admitted finite, non-negative squared radius. This belongs to the
-- shared geometric vocabulary because both circle queries and alpha
-- filtration consume it.
newtype RadiusSquared = RadiusSquared Double
  deriving stock (Eq, Ord, Show)

instance NFData RadiusSquared where
  rnf (RadiusSquared value) = rnf value

-- | Typed refusal shared by every squared-radius consumer.
data RadiusSquaredError
  = NonFiniteRadiusSquared !NonFiniteValue
  | NegativeRadiusSquared !Double
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Admit a finite, non-negative squared radius.
mkRadiusSquared :: Double -> Either RadiusSquaredError RadiusSquared
mkRadiusSquared value =
  case classifyNonFinite value of
    Just nonFinite -> Left (NonFiniteRadiusSquared nonFinite)
    Nothing
      | value < 0 -> Left (NegativeRadiusSquared value)
      | otherwise -> Right (RadiusSquared value)

-- | The admitted binary64 threshold.
radiusSquaredValue :: RadiusSquared -> Double
radiusSquaredValue (RadiusSquared value) = value

validateCoordinate :: Double -> Maybe CoordinateError
validateCoordinate value
  | isNaN value = Just CoordinateNaN
  | isInfinite value = Just CoordinateInfinite
  | value /= 0 && abs value < minimumAllowedCoordinate = Just CoordinateTooSmall
  | abs value > maximumAllowedCoordinate = Just CoordinateTooLarge
  | otherwise = Nothing

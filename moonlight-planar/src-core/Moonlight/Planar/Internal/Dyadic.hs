{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

module Moonlight.Planar.Internal.Dyadic
  ( exactOrientDet
  , exactOrientSignDouble
  , exactInCircleDet
  , exactInCircleSignDouble
  , exactInCircleSignFixedWidth
  , exactSquaredDistanceOrdering
  , exactQuarterSquaredDistanceRational
  , exactCircumradiusSquaredRational
  , exactCircumradiusSquaredWithin
  , exactBarycentricDeterminants
  , exactDiametralDot
  , integerRatioToDouble
  , integerBitLength
  ) where

import Data.Bits
  ( countLeadingZeros
  , countTrailingZeros
  , finiteBitSize
  , shiftL
  , shiftR
  )
import Data.Word (Word64)
import GHC.Exts (Int (I#), (+#))
import GHC.Integer.Logarithms (integerLog2#)
import Moonlight.Planar.Internal.ExactRational
  ( ExactArithmeticError
  , ExactRational
  , exactRationalFromDyadic
  , exactRationalFromDyadicRatio
  )
#if WORD_SIZE_IN_BITS == 64
import GHC.Exts
  ( Double (D#)
  , Double#
  , Int#
  , Word#
  , and#
  , castDoubleToWord64#
  , eqWord#
  , gtWord#
  , int2Word#
  , isTrue#
  , or#
  , plusWord#
  , plusWord2#
  , subWordC#
  , timesWord2#
  , uncheckedShiftL#
  , uncheckedShiftRL#
  , word2Int#
  , word64ToWord#
  , (*#)
  , (-#)
  , (<=#)
  , (==#)
  , (>#)
  , (>=#)
  )
#endif

-- Every finite binary64 value is a dyadic rational. Aligning all mantissas to
-- one exponent gives exact integer predicates without constructing Rational
-- expression trees.
type Decoded = (Integer, Int)

alignDecoded :: Int -> Decoded -> Integer
alignDecoded !power (!mantissa, !sourcePower)
  | mantissa == 0 = 0
  | otherwise = mantissa `shiftL` (sourcePower - power)
{-# INLINE alignDecoded #-}

decodedExponentFloor :: Decoded -> Int
decodedExponentFloor (mantissa, power)
  | mantissa == 0 = 0
  | otherwise = min 0 power
{-# INLINE decodedExponentFloor #-}

commonExponent4 :: Decoded -> Decoded -> Decoded -> Decoded -> Int
commonExponent4 firstValue secondValue thirdValue fourthValue =
  min
    (min (decodedExponentFloor firstValue) (decodedExponentFloor secondValue))
    (min (decodedExponentFloor thirdValue) (decodedExponentFloor fourthValue))
{-# INLINE commonExponent4 #-}

commonExponent6
  :: Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Int
commonExponent6 firstValue secondValue thirdValue fourthValue fifthValue sixthValue =
  min
    ( commonExponent4
        firstValue
        secondValue
        thirdValue
        fourthValue
    )
    (min (decodedExponentFloor fifthValue) (decodedExponentFloor sixthValue))
{-# INLINE commonExponent6 #-}

commonExponent8
  :: Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Decoded
  -> Int
commonExponent8 firstValue secondValue thirdValue fourthValue fifthValue sixthValue seventhValue eighthValue =
  min
    (commonExponent4 firstValue secondValue thirdValue fourthValue)
    (commonExponent4 fifthValue sixthValue seventhValue eighthValue)
{-# INLINE commonExponent8 #-}

aligned4
  :: Double -> Double -> Double -> Double
  -> (Int, Integer, Integer, Integer, Integer)
aligned4 a b c d =
  let !da = decodeFloat a
      !db = decodeFloat b
      !dc = decodeFloat c
      !dd = decodeFloat d
      !power = commonExponent4 da db dc dd
   in ( power
      , alignDecoded power da
      , alignDecoded power db
      , alignDecoded power dc
      , alignDecoded power dd
      )
{-# INLINE aligned4 #-}

aligned6
  :: Double -> Double -> Double -> Double -> Double -> Double
  -> (Int, Integer, Integer, Integer, Integer, Integer, Integer)
aligned6 a b c d e f =
  let !da = decodeFloat a
      !db = decodeFloat b
      !dc = decodeFloat c
      !dd = decodeFloat d
      !de = decodeFloat e
      !df = decodeFloat f
      !power = commonExponent6 da db dc dd de df
   in ( power
      , alignDecoded power da
      , alignDecoded power db
      , alignDecoded power dc
      , alignDecoded power dd
      , alignDecoded power de
      , alignDecoded power df
      )

aligned8
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
aligned8 a b c d e f g h =
  let !da = decodeFloat a
      !db = decodeFloat b
      !dc = decodeFloat c
      !dd = decodeFloat d
      !de = decodeFloat e
      !df = decodeFloat f
      !dg = decodeFloat g
      !dh = decodeFloat h
      !power = commonExponent8 da db dc dd de df dg dh
   in ( alignDecoded power da
      , alignDecoded power db
      , alignDecoded power dc
      , alignDecoded power dd
      , alignDecoded power de
      , alignDecoded power df
      , alignDecoded power dg
      , alignDecoded power dh
      )

exactOrientDet
  :: Double -> Double -> Double -> Double -> Double -> Double -> Integer
exactOrientDet ax ay bx by cx cy =
  let (!_, !iax, !iay, !ibx, !iby, !icx, !icy) = aligned6 ax ay bx by cx cy
      !acx = iax - icx
      !acy = iay - icy
      !bcx = ibx - icx
      !bcy = iby - icy
   in acx * bcy - acy * bcx

-- | Compare exact squared distances from one finite binary64 query. All six
-- coordinates share a dyadic exponent, whose positive square cancels from the
-- ordering. No rounded distance or rational normalization participates.
exactSquaredDistanceOrdering
  :: Double -> Double -> Double -> Double -> Double -> Double -> Ordering
exactSquaredDistanceOrdering qx qy ax ay bx by =
  let (!_, !iqx, !iqy, !iax, !iay, !ibx, !iby) = aligned6 qx qy ax ay bx by
      !difference =
        (iax - ibx) * (iax + ibx - 2 * iqx)
          + (iay - iby) * (iay + iby - 2 * iqy)
   in compare difference 0
{-# INLINE exactSquaredDistanceOrdering #-}

-- | One quarter of the exact squared distance between two finite binary64
-- points. Alpha's Gabriel-edge birth needs this value directly, so the dyadic
-- exponent absorbs the quarter before the single rational normalization.
exactQuarterSquaredDistanceRational
  :: Double -> Double -> Double -> Double -> ExactRational
exactQuarterSquaredDistanceRational ax ay bx by =
  let (!coordinatePower, !iax, !iay, !ibx, !iby) = aligned4 ax ay bx by
      !deltaX = ibx - iax
      !deltaY = iby - iay
      !squaredDistance = deltaX * deltaX + deltaY * deltaY
   in exactRationalFromDyadic squaredDistance (2 * coordinatePower - 2)
{-# INLINE exactQuarterSquaredDistanceRational #-}

-- | Exact squared circumradius of three finite binary64 points. The local
-- dyadic section computes the integer numerator and denominator without
-- constructing intermediate ratios; descent normalizes the authoritative
-- result exactly once. A degenerate triangle is the typed zero-divisor
-- obstruction from 'ExactRational'.
exactCircumradiusSquaredRational
  :: Double -> Double -> Double -> Double -> Double -> Double
  -> Either ExactArithmeticError ExactRational
exactCircumradiusSquaredRational ax ay bx by cx cy =
  let (!coordinatePower, !iax, !iay, !ibx, !iby, !icx, !icy) =
        aligned6 ax ay bx by cx cy
      !abx = ibx - iax
      !aby = iby - iay
      !acx = icx - iax
      !acy = icy - iay
      !bcx = icx - ibx
      !bcy = icy - iby
      !abSquared = abx * abx + aby * aby
      !acSquared = acx * acx + acy * acy
      !bcSquared = bcx * bcx + bcy * bcy
      !determinant = abx * acy - aby * acx
      !radiusNumerator = abSquared * acSquared * bcSquared
      !radiusDenominator = 4 * determinant * determinant
   in exactRationalFromDyadicRatio
        radiusNumerator
        radiusDenominator
        (2 * coordinatePower)
{-# INLINE exactCircumradiusSquaredRational #-}

-- | Exact closed comparison of squared circumradius with a finite,
-- non-negative binary64 threshold. No constructed circumcenter participates.
exactCircumradiusSquaredWithin
  :: Double
  -> Double -> Double -> Double -> Double -> Double -> Double
  -> Bool
exactCircumradiusSquaredWithin
  threshold
  ax ay bx by cx cy =
    let (!thresholdMantissa, !thresholdPower) = decodeFloat threshold
     in smallIntegralCircumradiusSquaredWithin
          (fromInteger thresholdMantissa)
          thresholdPower
          ax ay bx by cx cy
{-# INLINE exactCircumradiusSquaredWithin #-}

arbitraryCircumradiusSquaredWithin
  :: Integer
  -> Int
  -> Double -> Double -> Double -> Double -> Double -> Double
  -> Bool
arbitraryCircumradiusSquaredWithin
  thresholdMantissa
  thresholdPower
  ax ay bx by cx cy =
  let (!coordinatePower, !iax, !iay, !ibx, !iby, !icx, !icy) =
        aligned6 ax ay bx by cx cy
      !abx = ibx - iax
      !aby = iby - iay
      !acx = icx - iax
      !acy = icy - iay
      !bcx = icx - ibx
      !bcy = icy - iby
      !abSquared = abx * abx + aby * aby
      !acSquared = acx * acx + acy * acy
      !bcSquared = bcx * bcx + bcy * bcy
      !determinant = abx * acy - aby * acx
      !radiusNumerator = abSquared * acSquared * bcSquared
      !thresholdDenominator =
        4 * determinant * determinant * thresholdMantissa
   in determinant /= 0
        && compareDyadic
          radiusNumerator
          (6 * coordinatePower)
          thresholdDenominator
          (4 * coordinatePower + thresholdPower)
          /= GT

-- Integer-sized edges cover grid, pixel, and indexed-world faces without
-- allocating arbitrary-precision mantissas. Larger or fractional edges
-- descend to the general dyadic comparison above.
smallIntegralCircumradiusSquaredWithin
  :: Word64
  -> Int
  -> Double -> Double -> Double -> Double -> Double -> Double
  -> Bool
smallIntegralCircumradiusSquaredWithin
  rawThresholdMantissa
  rawThresholdPower
  ax ay bx by cx cy =
    if not admittedDifferences
      then
        arbitraryCircumradiusSquaredWithin
          (toInteger rawThresholdMantissa)
          rawThresholdPower
          ax ay bx by cx cy
      else
        let !abSquared = squaredIntegralLength abx aby
            !acSquared = squaredIntegralLength acx acy
            !bcSquared = squaredIntegralLength bcx bcy
            !radiusNumerator = abSquared * acSquared * bcSquared
            !determinant = abx * acy - aby * acx
            !determinantMagnitude = fromIntegral (abs determinant)
         in if determinant == 0 || rawThresholdMantissa == 0
              then False
              else
                let !trailingZeros = countTrailingZeros rawThresholdMantissa
                    !thresholdMantissa = rawThresholdMantissa `shiftR` trailingZeros
                    !thresholdPower = rawThresholdPower + trailingZeros
                    !determinantSquared = determinantMagnitude * determinantMagnitude
                    !scaledDeterminant = 4 * determinantSquared
                    !productsFit =
                      determinantMagnitude <= maxBound `quot` determinantMagnitude
                        && determinantSquared <= maxBound `quot` 4
                        && thresholdMantissa <= maxBound `quot` scaledDeterminant
                 in if productsFit
                      then
                        compareWordDyadic
                          radiusNumerator
                          0
                          (scaledDeterminant * thresholdMantissa)
                          thresholdPower
                          /= GT
                      else
                        arbitraryCircumradiusSquaredWithin
                          (toInteger rawThresholdMantissa)
                          rawThresholdPower
                          ax ay bx by cx cy
 where
  !abxValue = bx - ax
  !abyValue = by - ay
  !acxValue = cx - ax
  !acyValue = cy - ay
  !abx = truncate abxValue
  !aby = truncate abyValue
  !acx = truncate acxValue
  !acy = truncate acyValue
  !bcx = acx - abx
  !bcy = acy - aby
  admittedDifferences =
    integralDifference abxValue abx
      && integralDifference abyValue aby
      && integralDifference acxValue acx
      && integralDifference acyValue acy
{-# INLINE smallIntegralCircumradiusSquaredWithin #-}

integralDifference :: Double -> Int -> Bool
integralDifference value integral =
  abs value <= 512 && fromIntegral integral == value
{-# INLINE integralDifference #-}

squaredIntegralLength :: Int -> Int -> Word64
squaredIntegralLength x y =
  let !xMagnitude = fromIntegral (abs x)
      !yMagnitude = fromIntegral (abs y)
   in xMagnitude * xMagnitude + yMagnitude * yMagnitude
{-# INLINE squaredIntegralLength #-}

compareWordDyadic :: Word64 -> Int -> Word64 -> Int -> Ordering
compareWordDyadic left leftPower right rightPower
  | left == 0 = compare left right
  | right == 0 = GT
  | leftMagnitude /= rightMagnitude = compare leftMagnitude rightMagnitude
  | leftPower < rightPower = compare left (right `shiftL` (rightPower - leftPower))
  | otherwise = compare (left `shiftL` (leftPower - rightPower)) right
 where
  !leftMagnitude = wordBitLength left + leftPower
  !rightMagnitude = wordBitLength right + rightPower

  wordBitLength :: Word64 -> Int
  wordBitLength value = finiteBitSize value - countLeadingZeros value
{-# INLINE compareWordDyadic #-}

compareDyadic :: Integer -> Int -> Integer -> Int -> Ordering
compareDyadic left leftPower right rightPower =
  case compare leftPower rightPower of
    LT -> compare left (right `shiftL` (rightPower - leftPower))
    EQ -> compare left right
    GT -> compare (left `shiftL` (leftPower - rightPower)) right
{-# INLINE compareDyadic #-}

exactInCircleDet
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> Integer
exactInCircleDet ax ay bx by cx cy dx dy =
  let (!iax, !iay, !ibx, !iby, !icx, !icy, !idx, !idy) =
        aligned8 ax ay bx by cx cy dx dy
      !adx = iax - idx
      !ady = iay - idy
      !bdx = ibx - idx
      !bdy = iby - idy
      !cdx = icx - idx
      !cdy = icy - idy
      !abdet = adx * bdy - bdx * ady
      !bcdet = bdx * cdy - cdx * bdy
      !cadet = cdx * ady - adx * cdy
      !alift = adx * adx + ady * ady
      !blift = bdx * bdx + bdy * bdy
      !clift = cdx * cdx + cdy * cdy
   in alift * bcdet + blift * cadet + clift * abdet

exactDiametralDot
  :: Double -> Double -> Double -> Double -> Double -> Double -> Integer
exactDiametralDot ax ay bx by px py =
  let (!_, !iax, !iay, !ibx, !iby, !ipx, !ipy) = aligned6 ax ay bx by px py
      !pax = ipx - iax
      !pay = ipy - iay
      !pbx = ipx - ibx
      !pby = ipy - iby
   in pax * pbx + pay * pby

exactBarycentricDeterminants
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> (Integer, Integer, Integer, Integer)
exactBarycentricDeterminants ax ay bx by cx cy qx qy =
  let (!iax, !iay, !ibx, !iby, !icx, !icy, !iqx, !iqy) =
        aligned8 ax ay bx by cx cy qx qy
      determinant :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
      determinant px py rx ry sx sy =
        let !psx = px - sx
            !psy = py - sy
            !rsx = rx - sx
            !rsy = ry - sy
         in psx * rsy - psy * rsx
      !denominator = determinant iax iay ibx iby icx icy
      !weightA = determinant iqx iqy ibx iby icx icy
      !weightB = determinant iax iay iqx iqy icx icy
      !weightC = determinant iax iay ibx iby iqx iqy
   in (denominator, weightA, weightB, weightC)

integerRatioToDouble :: Integer -> Integer -> Double
integerRatioToDouble numerator denominator
  | denominator == 0 = 0 / 0
  | numerator == 0 = 0
  | otherwise =
      let !precision = floatDigits (0 :: Double)
          !numeratorMagnitude = abs numerator
          !denominatorMagnitude = abs denominator
          !numeratorBits = integerBitLength numeratorMagnitude
          !denominatorBits = integerBitLength denominatorMagnitude
          !numeratorShift = max 0 (numeratorBits - precision)
          !denominatorShift = max 0 (denominatorBits - precision)
          !scaledNumerator = fromInteger (numeratorMagnitude `shiftR` numeratorShift)
          !scaledDenominator = fromInteger (denominatorMagnitude `shiftR` denominatorShift)
          !magnitude = scaleFloat (numeratorShift - denominatorShift) (scaledNumerator / scaledDenominator)
          !sameSign = (numerator < 0) == (denominator < 0)
       in if sameSign then magnitude else negate magnitude

integerBitLength :: Integer -> Int
integerBitLength value
  | value <= 0 = 0
  | otherwise = I# (integerLog2# value +# 1#)
{-# INLINE integerBitLength #-}

-- ---------------------------------------------------------------------------
-- Fixed-precision exact orient sign for Double.
--
-- The generic dyadic path answers every exact query with arbitrary-precision
-- Integers: six decodes, one alignment, and two multiplies, each allocating.
-- Practical inputs have an exponent spread small enough that the determinant's
-- exact sign is decided by 128-bit differences and 256-bit products in machine
-- words, without a single heap object. 'exactOrientSignDouble' takes that path
-- and falls back to 'exactOrientDet' the moment an operand is non-finite or an
-- alignment shift would outgrow the fixed width. The two agree by
-- construction: both compute the sign of the same integer determinant.
--
-- The fixed-width worker reads a Double as one machine word and aligns
-- mantissas across a 128-bit pair, so it is only meaningful where a machine
-- word is 64 bits wide. On a narrower target the same sign is taken from the
-- arbitrary-precision determinant directly, which is the branch this path
-- already falls back to whenever an alignment shift would outgrow the width.

#if WORD_SIZE_IN_BITS == 64

exactOrientSignDouble
  :: Double -> Double -> Double -> Double -> Double -> Double -> Ordering
exactOrientSignDouble ax ay bx by cx cy =
  case ax of
    D# axw ->
      case ay of
        D# ayw ->
          case bx of
            D# bxw ->
              case by of
                D# byw ->
                  case cx of
                    D# cxw ->
                      case cy of
                        D# cyw ->
                          case orientSignWorker axw ayw bxw byw cxw cyw of
                            2# -> compare (exactOrientDet ax ay bx by cx cy) 0
                            0# -> EQ
                            sign ->
                              case sign ># 0# of
                                1# -> GT
                                _ -> LT
{-# NOINLINE exactOrientSignDouble #-}

-- Decode a Double into sign bit (0/1), mantissa, and power-of-two exponent
-- with value = (-1)^sign * mantissa * 2^exponent. Zero decodes to a zero
-- mantissa; subnormals decode without a hidden bit. The fourth component is 1
-- when the value is finite and 0 when it is not.
decodeExact :: Double# -> (# Int#, Word#, Int#, Int# #)
decodeExact d =
  case word64ToWord# (castDoubleToWord64# d) of
    bits ->
      let neg = word2Int# (uncheckedShiftRL# bits 63#)
          exponentField = word2Int# (and# (uncheckedShiftRL# bits 52#) 2047##)
          mantissaField = and# bits 4503599627370495##
       in case exponentField of
            0# -> (# neg, mantissaField, -1074#, 1# #)
            2047# -> (# neg, mantissaField, 0#, 0# #)
            raw -> (# neg, or# mantissaField 4503599627370496##, raw -# 1075#, 1# #)

-- A mantissa of at most 53 bits shifted left by at most 73 bits: the pair
-- (high, low) of a value below 2^126.
shiftMantissa :: Word# -> Int# -> (# Word#, Word# #)
shiftMantissa mantissa k =
  case k >=# 64# of
    1# -> (# uncheckedShiftL# mantissa (k -# 64#), 0## #)
    _ ->
      case k ==# 0# of
        1# -> (# 0##, mantissa #)
        _ ->
          (#
            uncheckedShiftRL# mantissa (64# -# k),
            uncheckedShiftL# mantissa k
          #)

-- The exact sign and 128-bit magnitude of sa*ma*2^ea - sc*mc*2^ec, aligned
-- to the caller-supplied floor exponent, as (sign, high, low, status) with
-- sign in {-1, 0, 1} and status 1 when an alignment shift outgrows the fixed
-- width. The floor never exceeds the exponent of a nonzero operand, so every
-- shift is non-negative; one shared floor is what makes the four differences
-- of one determinant comparable after multiplication.
differenceExact
  :: Int# -> Word# -> Int# -> Int# -> Word# -> Int# -> Int# -> (# Int#, Word#, Word#, Int# #)
differenceExact nega ma ea negc mc ec emin =
  case ma of
    0## ->
      case mc of
        0## -> (# 0#, 0##, 0##, 0# #)
        _ -> aligned (negateSign (positiveSign negc)) mc (ec -# emin)
    _ ->
      case mc of
        0## -> aligned (positiveSign nega) ma (ea -# emin)
        _ ->
          case ea -# emin of
            da ->
              case da ># 73# of
                1# -> (# 0#, 0##, 0##, 1# #)
                _ ->
                  case ec -# emin of
                    dc ->
                      case dc ># 73# of
                        1# -> (# 0#, 0##, 0##, 1# #)
                        _ ->
                          case shiftMantissa ma da of
                            (# ahi, alo #) ->
                              case shiftMantissa mc dc of
                                (# chi, clo #) ->
                                  case nega ==# negc of
                                    1# ->
                                      -- Same operand signs: subtract magnitudes.
                                      case compareWord2 ahi alo chi clo of
                                        0# -> (# 0#, 0##, 0##, 0# #)
                                        1# ->
                                          case subtractWord2 ahi alo chi clo of
                                            (# hi, lo #) -> (# positiveSign nega, hi, lo, 0# #)
                                        _ ->
                                          case subtractWord2 chi clo ahi alo of
                                            (# hi, lo #) -> (# negateSign (positiveSign nega), hi, lo, 0# #)
                                    _ ->
                                      -- Opposite operand signs: add magnitudes.
                                      case addWord2 ahi alo chi clo of
                                        (# hi, lo #) -> (# positiveSign nega, hi, lo, 0# #)
  where
    aligned sign mantissa k =
      case k ># 73# of
        1# -> (# 0#, 0##, 0##, 1# #)
        _ ->
          case shiftMantissa mantissa k of
            (# hi, lo #) -> (# sign, hi, lo, 0# #)
    positiveSign neg = case neg of
      1# -> -1#
      _ -> 1#
    negateSign sign = case sign of
      1# -> -1#
      _ -> 1#

-- Lexicographic comparison of 128-bit magnitudes: 1, 0, or -1.
compareWord2 :: Word# -> Word# -> Word# -> Word# -> Int#
compareWord2 ahi alo chi clo =
  case eqWord# ahi chi of
    1# ->
      case eqWord# alo clo of
        1# -> 0#
        _ ->
          case gtWord# alo clo of
            1# -> 1#
            _ -> -1#
    _ ->
      case gtWord# ahi chi of
        1# -> 1#
        _ -> -1#

-- 128-bit difference of magnitudes, first operand at least the second.
subtractWord2 :: Word# -> Word# -> Word# -> Word# -> (# Word#, Word# #)
subtractWord2 ahi alo chi clo =
  case subWordC# alo clo of
    (# low, borrow #) ->
      case subWordC# ahi chi of
        (# high0, _ #) ->
          case subWordC# high0 (int2Word# borrow) of
            (# high, _ #) -> (# high, low #)

-- 128-bit sum of magnitudes each below 2^126: the total stays below 2^127 and
-- the final carry is empty by the shift bound.
addWord2 :: Word# -> Word# -> Word# -> Word# -> (# Word#, Word# #)
addWord2 ahi alo chi clo =
  case plusWord2# alo clo of
    (# carry0, low #) ->
      case plusWord2# ahi chi of
        (# _, high0 #) ->
          case plusWord2# high0 carry0 of
            (# _, high #) -> (# high, low #)

-- 128-bit by 128-bit exact product, (r3, r2, r1, r0), most significant first.
-- Each factor stays below 2^127, so the product stays below 2^254 and the top
-- accumulation cannot overflow.
multiplyWord2 :: Word# -> Word# -> Word# -> Word# -> (# Word#, Word#, Word#, Word# #)
multiplyWord2 ahi alo bhi blo =
  case timesWord2# alo blo of
    (# h00, l00 #) ->
      case timesWord2# alo bhi of
        (# h01, l01 #) ->
          case timesWord2# ahi blo of
            (# h10, l10 #) ->
              case timesWord2# ahi bhi of
                (# h11, l11 #) ->
                  case plusWord2# h00 l01 of
                    (# carryA, sumA #) ->
                      case plusWord2# sumA l10 of
                        (# carryB, r1 #) ->
                          case plusWord# carryA carryB of
                            carry2 ->
                              case plusWord2# h01 h10 of
                                (# carryC, sumC #) ->
                                  case plusWord2# sumC l11 of
                                    (# carryD, sumD #) ->
                                      case plusWord2# sumD carry2 of
                                        (# carryE, r2 #) ->
                                          case plusWord# (plusWord# carryC carryD) carryE of
                                            carry3 ->
                                              case plusWord# h11 carry3 of
                                                r3 -> (# r3, r2, r1, l00 #)

-- Lexicographic comparison of 256-bit magnitudes: 1, 0, or -1.
compareWord4
  :: Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Int#
compareWord4 a3 a2 a1 a0 b3 b2 b1 b0 =
  case compareWord2 a3 a2 b3 b2 of
    0# -> compareWord2 a1 a0 b1 b0
    answer -> answer

-- The exponent a mantissa contributes to the alignment floor: a zero
-- mantissa is exact at any floor and votes for the impossibly high sentinel.
floorExp :: Word# -> Int# -> Int#
floorExp mantissa power =
  case mantissa of
    0## -> 2000000#
    _ -> power

minExp :: Int# -> Int# -> Int#
minExp a b = if isTrue# (a <=# b) then a else b

orientSignWorker :: Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Int#
orientSignWorker ax ay bx by cx cy =
  case decodeExact ax of
    (# negax, max_, eax, okax #) ->
      case decodeExact ay of
        (# negay, may, eay, okay #) ->
          case decodeExact bx of
            (# negbx, mbx, ebx, okbx #) ->
              case decodeExact by of
                (# negby, mby, eby, okby #) ->
                  case decodeExact cx of
                    (# negcx, mcx, ecx, okcx #) ->
                      case decodeExact cy of
                        (# negcy, mcy, ecy, okcy #) ->
                          case okax +# okay +# okbx +# okby +# okcx +# okcy of
                            6# ->
                              -- The alignment floor is the least exponent
                              -- among nonzero mantissas; a zero mantissa is
                              -- exact at any floor and must not drag it down.
                              case floorExp max_ eax `minExp` floorExp may eay `minExp` floorExp mbx ebx `minExp` floorExp mby eby `minExp` floorExp mcx ecx `minExp` floorExp mcy ecy of
                                emin ->
                                  case differenceExact negax max_ eax negcx mcx ecx emin of
                                    (# s1, d1hi, d1lo, f1 #) ->
                                      case differenceExact negby mby eby negcy mcy ecy emin of
                                        (# s2, d2hi, d2lo, f2 #) ->
                                          case differenceExact negay may eay negcy mcy ecy emin of
                                            (# s3, d3hi, d3lo, f3 #) ->
                                              case differenceExact negbx mbx ebx negcx mcx ecx emin of
                                                (# s4, d4hi, d4lo, f4 #) ->
                                                  case f1 +# f2 +# f3 +# f4 of
                                                    0# ->
                                                      combineSigns
                                                        (s1 *# s2) d1hi d1lo d2hi d2lo
                                                        (s3 *# s4) d3hi d3lo d4hi d4lo
                                                    _ -> 2#
                            _ -> 2#
  where
    -- det = leftSign * leftProduct - rightSign * rightProduct
    combineSigns leftSign d1hi d1lo d2hi d2lo rightSign d3hi d3lo d4hi d4lo =
      case leftSign of
        0# ->
          case rightSign of
            0# -> 0#
            _ -> 0# -# rightSign
        _ ->
          case rightSign of
            0# -> leftSign
            _ ->
              case leftSign ==# rightSign of
                1# ->
                  case multiplyWord2 d1hi d1lo d2hi d2lo of
                    (# p3, p2, p1, p0 #) ->
                      case multiplyWord2 d3hi d3lo d4hi d4lo of
                        (# q3, q2, q1, q0 #) ->
                          case compareWord4 p3 p2 p1 p0 q3 q2 q1 q0 of
                            0# -> 0#
                            1# -> leftSign
                            _ -> 0# -# leftSign
                _ -> leftSign

-- | The sign of the in-circle determinant of four finite binary64 points,
-- resolved in fixed width where the eight coordinates align within it and
-- read from the arbitrary-precision determinant otherwise. The two agree by
-- construction: the worker evaluates the same integer determinant as
-- 'exactInCircleDet', over the same alignment floor, and only declines.
exactInCircleSignDouble
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> Ordering
exactInCircleSignDouble ax ay bx by cx cy dx dy =
  case inCircleSignDouble ax ay bx by cx cy dx dy of
    2# -> compare (exactInCircleDet ax ay bx by cx cy dx dy) 0
    0# -> EQ
    sign ->
      case sign ># 0# of
        1# -> GT
        _ -> LT
{-# NOINLINE exactInCircleSignDouble #-}

-- | The fixed-width verdict alone: 'Nothing' where the worker declined and
-- the sign would come from the determinant. A test that cross-checks the
-- worker against 'exactInCircleDet' counts how many cases it resolved
-- through this, rather than crediting the fallback with the worker's work.
exactInCircleSignFixedWidth
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> Maybe Ordering
exactInCircleSignFixedWidth ax ay bx by cx cy dx dy =
  case inCircleSignDouble ax ay bx by cx cy dx dy of
    2# -> Nothing
    0# -> Just EQ
    sign ->
      case sign ># 0# of
        1# -> Just GT
        _ -> Just LT

inCircleSignDouble
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> Int#
inCircleSignDouble (D# ax) (D# ay) (D# bx) (D# by) (D# cx) (D# cy) (D# dx) (D# dy) =
  inCircleSignWorker ax ay bx by cx cy dx dy
{-# INLINE inCircleSignDouble #-}

-- The degree-four analogue of 'orientSignWorker': the six differences
-- against the fourth point are the same 128-bit aligned magnitudes, each
-- minor and lift is a signed sum of two 256-bit products of them, each term
-- is a 512-bit product of a lift and a minor, and the determinant is the
-- signed sum of the three terms. Every product of two magnitudes below 2^127
-- stays below 2^254, so a minor or lift stays below 2^255, a term below
-- 2^510, and the sum of three terms below 2^512: no accumulation overflows
-- its width, and the only way out is an alignment shift beyond 73 bits.
inCircleSignWorker
  :: Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double#
  -> Int#
inCircleSignWorker ax ay bx by cx cy dx dy =
  case decodeExact ax of { (# negax, max_, eax, okax #) ->
  case decodeExact ay of { (# negay, may, eay, okay #) ->
  case decodeExact bx of { (# negbx, mbx, ebx, okbx #) ->
  case decodeExact by of { (# negby, mby, eby, okby #) ->
  case decodeExact cx of { (# negcx, mcx, ecx, okcx #) ->
  case decodeExact cy of { (# negcy, mcy, ecy, okcy #) ->
  case decodeExact dx of { (# negdx, mdx, edx, okdx #) ->
  case decodeExact dy of { (# negdy, mdy, edy, okdy #) ->
  case okax +# okay +# okbx +# okby +# okcx +# okcy +# okdx +# okdy of
    8# ->
      case floorExp max_ eax `minExp` floorExp may eay `minExp` floorExp mbx ebx `minExp` floorExp mby eby `minExp` floorExp mcx ecx `minExp` floorExp mcy ecy `minExp` floorExp mdx edx `minExp` floorExp mdy edy of
        emin ->
          case differenceExact negax max_ eax negdx mdx edx emin of { (# sadx, adxh, adxl, f1 #) ->
          case differenceExact negay may eay negdy mdy edy emin of { (# sady, adyh, adyl, f2 #) ->
          case differenceExact negbx mbx ebx negdx mdx edx emin of { (# sbdx, bdxh, bdxl, f3 #) ->
          case differenceExact negby mby eby negdy mdy edy emin of { (# sbdy, bdyh, bdyl, f4 #) ->
          case differenceExact negcx mcx ecx negdx mdx edx emin of { (# scdx, cdxh, cdxl, f5 #) ->
          case differenceExact negcy mcy ecy negdy mdy edy emin of { (# scdy, cdyh, cdyl, f6 #) ->
          case f1 +# f2 +# f3 +# f4 +# f5 +# f6 of
            0# ->
              inCircleFromDifferences
                sadx adxh adxl sady adyh adyl
                sbdx bdxh bdxl sbdy bdyh bdyl
                scdx cdxh cdxl scdy cdyh cdyl
            _ -> 2# }}}}}}
    _ -> 2# }}}}}}}}

inCircleFromDifferences
  :: Int# -> Word# -> Word# -> Int# -> Word# -> Word#
  -> Int# -> Word# -> Word# -> Int# -> Word# -> Word#
  -> Int# -> Word# -> Word# -> Int# -> Word# -> Word#
  -> Int#
inCircleFromDifferences
  sadx adxh adxl sady adyh adyl
  sbdx bdxh bdxl sbdy bdyh bdyl
  scdx cdxh cdxl scdy cdyh cdyl =
    case minor sadx adxh adxl sbdy bdyh bdyl sbdx bdxh bdxl sady adyh adyl of { (# sab, ab3, ab2, ab1, ab0 #) ->
    case minor sbdx bdxh bdxl scdy cdyh cdyl scdx cdxh cdxl sbdy bdyh bdyl of { (# sbc, bc3, bc2, bc1, bc0 #) ->
    case minor scdx cdxh cdxl sady adyh adyl sadx adxh adxl scdy cdyh cdyl of { (# sca, ca3, ca2, ca1, ca0 #) ->
    case lift sadx adxh adxl sady adyh adyl of { (# sal, al3, al2, al1, al0 #) ->
    case lift sbdx bdxh bdxl sbdy bdyh bdyl of { (# sbl, bl3, bl2, bl1, bl0 #) ->
    case lift scdx cdxh cdxl scdy cdyh cdyl of { (# scl, cl3, cl2, cl1, cl0 #) ->
    case product8 sal al3 al2 al1 al0 sbc bc3 bc2 bc1 bc0 of { (# s1, a7, a6, a5, a4, a3, a2, a1, a0 #) ->
    case product8 sbl bl3 bl2 bl1 bl0 sca ca3 ca2 ca1 ca0 of { (# s2, b7, b6, b5, b4, b3, b2, b1, b0 #) ->
    case product8 scl cl3 cl2 cl1 cl0 sab ab3 ab2 ab1 ab0 of { (# s3, c7, c6, c5, c4, c3, c2, c1, c0 #) ->
    case sum8 s1 a7 a6 a5 a4 a3 a2 a1 a0 s2 b7 b6 b5 b4 b3 b2 b1 b0 of { (# s12, d7, d6, d5, d4, d3, d2, d1, d0 #) ->
    case sum8 s12 d7 d6 d5 d4 d3 d2 d1 d0 s3 c7 c6 c5 c4 c3 c2 c1 c0 of { (# sign, _, _, _, _, _, _, _, _ #) ->
    sign }}}}}}}}}}}
  where
    -- p*q - r*s over signed 128-bit magnitudes.
    minor sp ph pl sq qh ql sr rh rl ss sh sl =
      case product4 sp ph pl sq qh ql of
        (# s1, p3, p2, p1, p0 #) ->
          case product4 sr rh rl ss sh sl of
            (# s2, q3, q2, q1, q0 #) ->
              sum4 s1 p3 p2 p1 p0 (0# -# s2) q3 q2 q1 q0
    -- x*x + y*y over signed 128-bit magnitudes.
    lift sx xh xl sy yh yl =
      case product4 sx xh xl sx xh xl of
        (# s1, p3, p2, p1, p0 #) ->
          case product4 sy yh yl sy yh yl of
            (# s2, q3, q2, q1, q0 #) ->
              sum4 s1 p3 p2 p1 p0 s2 q3 q2 q1 q0

-- Signed 256-bit product of two signed 128-bit magnitudes.
product4
  :: Int# -> Word# -> Word# -> Int# -> Word# -> Word#
  -> (# Int#, Word#, Word#, Word#, Word# #)
product4 sa ahi alo sb bhi blo =
  case multiplyWord2 ahi alo bhi blo of
    (# p3, p2, p1, p0 #) -> (# sa *# sb, p3, p2, p1, p0 #)

-- Signed 512-bit product of two signed 256-bit magnitudes.
product8
  :: Int# -> Word# -> Word# -> Word# -> Word# -> Int# -> Word# -> Word# -> Word# -> Word#
  -> (# Int#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
product8 sa a3 a2 a1 a0 sb b3 b2 b1 b0 =
  case multiplyWord4 a3 a2 a1 a0 b3 b2 b1 b0 of
    (# r7, r6, r5, r4, r3, r2, r1, r0 #) -> (# sa *# sb, r7, r6, r5, r4, r3, r2, r1, r0 #)

-- Signed sum of two signed 256-bit values whose magnitudes' sum fits.
sum4
  :: Int# -> Word# -> Word# -> Word# -> Word# -> Int# -> Word# -> Word# -> Word# -> Word#
  -> (# Int#, Word#, Word#, Word#, Word# #)
sum4 sa a3 a2 a1 a0 sb b3 b2 b1 b0 =
  case sa of
    0# -> (# sb, b3, b2, b1, b0 #)
    _ ->
      case sb of
        0# -> (# sa, a3, a2, a1, a0 #)
        _ ->
          case sa ==# sb of
            1# ->
              case addWord4 a3 a2 a1 a0 b3 b2 b1 b0 of
                (# _, r3, r2, r1, r0 #) -> (# sa, r3, r2, r1, r0 #)
            _ ->
              case compareWord4 a3 a2 a1 a0 b3 b2 b1 b0 of
                0# -> (# 0#, 0##, 0##, 0##, 0## #)
                1# ->
                  case subtractWord4 a3 a2 a1 a0 b3 b2 b1 b0 of
                    (# r3, r2, r1, r0 #) -> (# sa, r3, r2, r1, r0 #)
                _ ->
                  case subtractWord4 b3 b2 b1 b0 a3 a2 a1 a0 of
                    (# r3, r2, r1, r0 #) -> (# sb, r3, r2, r1, r0 #)

-- Signed sum of two signed 512-bit values whose magnitudes' sum fits.
sum8
  :: Int# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> Int# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> (# Int#, Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
sum8 sa a7 a6 a5 a4 a3 a2 a1 a0 sb b7 b6 b5 b4 b3 b2 b1 b0 =
  case sa of
    0# -> (# sb, b7, b6, b5, b4, b3, b2, b1, b0 #)
    _ ->
      case sb of
        0# -> (# sa, a7, a6, a5, a4, a3, a2, a1, a0 #)
        _ ->
          case sa ==# sb of
            1# ->
              case addWord8 a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 of
                (# r7, r6, r5, r4, r3, r2, r1, r0 #) -> (# sa, r7, r6, r5, r4, r3, r2, r1, r0 #)
            _ ->
              case compareWord8 a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 of
                0# -> (# 0#, 0##, 0##, 0##, 0##, 0##, 0##, 0##, 0## #)
                1# ->
                  case subtractWord8 a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 of
                    (# r7, r6, r5, r4, r3, r2, r1, r0 #) -> (# sa, r7, r6, r5, r4, r3, r2, r1, r0 #)
                _ ->
                  case subtractWord8 b7 b6 b5 b4 b3 b2 b1 b0 a7 a6 a5 a4 a3 a2 a1 a0 of
                    (# r7, r6, r5, r4, r3, r2, r1, r0 #) -> (# sb, r7, r6, r5, r4, r3, r2, r1, r0 #)

-- a + b + carry with carry in {0, 1}, as (carry out, sum).
addCarry :: Word# -> Word# -> Word# -> (# Word#, Word# #)
addCarry a b carry =
  case plusWord2# a b of
    (# carry1, partial #) ->
      case plusWord2# partial carry of
        (# carry2, total #) -> (# plusWord# carry1 carry2, total #)

-- a - b - borrow with borrow in {0, 1}, as (borrow out, difference).
subtractBorrow :: Word# -> Word# -> Word# -> (# Word#, Word# #)
subtractBorrow a b borrow =
  case subWordC# a b of
    (# partial, borrow1 #) ->
      case subWordC# partial borrow of
        (# total, borrow2 #) -> (# int2Word# (borrow1 +# borrow2), total #)

-- 256-bit sum with its carry out.
addWord4
  :: Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> (# Word#, Word#, Word#, Word#, Word# #)
addWord4 a3 a2 a1 a0 b3 b2 b1 b0 =
  case addCarry a0 b0 0## of { (# c0, r0 #) ->
  case addCarry a1 b1 c0 of { (# c1, r1 #) ->
  case addCarry a2 b2 c1 of { (# c2, r2 #) ->
  case addCarry a3 b3 c2 of { (# c3, r3 #) ->
  (# c3, r3, r2, r1, r0 #) }}}}

-- 256-bit difference of magnitudes, first operand at least the second.
subtractWord4
  :: Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> (# Word#, Word#, Word#, Word# #)
subtractWord4 a3 a2 a1 a0 b3 b2 b1 b0 =
  case subtractBorrow a0 b0 0## of { (# c0, r0 #) ->
  case subtractBorrow a1 b1 c0 of { (# c1, r1 #) ->
  case subtractBorrow a2 b2 c1 of { (# c2, r2 #) ->
  case subtractBorrow a3 b3 c2 of { (# _, r3 #) ->
  (# r3, r2, r1, r0 #) }}}}

-- 512-bit sum of magnitudes whose total fits, so the carry out is empty.
addWord8
  :: Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
addWord8 a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 =
  case addWord4 a3 a2 a1 a0 b3 b2 b1 b0 of { (# c3, r3, r2, r1, r0 #) ->
  case addCarry a4 b4 c3 of { (# c4, r4 #) ->
  case addCarry a5 b5 c4 of { (# c5, r5 #) ->
  case addCarry a6 b6 c5 of { (# c6, r6 #) ->
  case addCarry a7 b7 c6 of { (# _, r7 #) ->
  (# r7, r6, r5, r4, r3, r2, r1, r0 #) }}}}}

-- 512-bit difference of magnitudes, first operand at least the second.
subtractWord8
  :: Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
subtractWord8 a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 =
  case subtractBorrow a0 b0 0## of { (# c0, r0 #) ->
  case subtractBorrow a1 b1 c0 of { (# c1, r1 #) ->
  case subtractBorrow a2 b2 c1 of { (# c2, r2 #) ->
  case subtractBorrow a3 b3 c2 of { (# c3, r3 #) ->
  case subtractBorrow a4 b4 c3 of { (# c4, r4 #) ->
  case subtractBorrow a5 b5 c4 of { (# c5, r5 #) ->
  case subtractBorrow a6 b6 c5 of { (# c6, r6 #) ->
  case subtractBorrow a7 b7 c6 of { (# _, r7 #) ->
  (# r7, r6, r5, r4, r3, r2, r1, r0 #) }}}}}}}}

-- Lexicographic comparison of 512-bit magnitudes: 1, 0, or -1.
compareWord8
  :: Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> Int#
compareWord8 a7 a6 a5 a4 a3 a2 a1 a0 b7 b6 b5 b4 b3 b2 b1 b0 =
  case compareWord4 a7 a6 a5 a4 b7 b6 b5 b4 of
    0# -> compareWord4 a3 a2 a1 a0 b3 b2 b1 b0
    answer -> answer

-- 256-bit by 256-bit exact product as eight words, most significant first:
-- schoolbook over 128-bit halves, each half product taken exactly by
-- 'multiplyWord2', the two middle products summed to 257 bits before the
-- three bands are added at their offsets.
multiplyWord4
  :: Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word# -> Word#
  -> (# Word#, Word#, Word#, Word#, Word#, Word#, Word#, Word# #)
multiplyWord4 a3 a2 a1 a0 b3 b2 b1 b0 =
  case multiplyWord2 a1 a0 b1 b0 of { (# p3, p2, p1, p0 #) ->
  case multiplyWord2 a1 a0 b3 b2 of { (# q3, q2, q1, q0 #) ->
  case multiplyWord2 a3 a2 b1 b0 of { (# r3, r2, r1, r0 #) ->
  case multiplyWord2 a3 a2 b3 b2 of { (# t3, t2, t1, t0 #) ->
  case addWord4 q3 q2 q1 q0 r3 r2 r1 r0 of { (# m4, m3, m2, m1, m0 #) ->
  case addCarry p2 m0 0## of { (# c2, w2 #) ->
  case addCarry p3 m1 c2 of { (# c3, w3 #) ->
  case addCarry t0 m2 c3 of { (# c4, w4 #) ->
  case addCarry t1 m3 c4 of { (# c5, w5 #) ->
  case addCarry t2 m4 c5 of { (# c6, w6 #) ->
  case plusWord# t3 c6 of { w7 ->
  (# w7, w6, w5, w4, w3, w2, p1, p0 #) }}}}}}}}}}}

#else

-- The narrow-word answer to the same question. 'exactOrientDet' is the
-- determinant the fixed-width worker exists to avoid allocating, not a
-- different quantity, so the two branches agree by construction.
exactOrientSignDouble
  :: Double -> Double -> Double -> Double -> Double -> Double -> Ordering
exactOrientSignDouble ax ay bx by cx cy =
  compare (exactOrientDet ax ay bx by cx cy) 0
{-# NOINLINE exactOrientSignDouble #-}

exactInCircleSignDouble
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> Ordering
exactInCircleSignDouble ax ay bx by cx cy dx dy =
  compare (exactInCircleDet ax ay bx by cx cy dx dy) 0
{-# NOINLINE exactInCircleSignDouble #-}

exactInCircleSignFixedWidth
  :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
  -> Maybe Ordering
exactInCircleSignFixedWidth _ _ _ _ _ _ _ _ = Nothing

#endif

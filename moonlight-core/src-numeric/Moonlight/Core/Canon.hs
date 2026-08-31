-- | Checked constructors for canonical floating point ('mkFiniteDouble' and friends) and the hash-stable quantization they share.
module Moonlight.Core.Canon
  ( canonicalize
  , isCanonical
  , quantizeForHash
  , mkFiniteDouble
  , mkFiniteWith
  , mkPositiveInt
  , mkPositiveIntWith
  , mkPositiveFiniteDouble
  , mkPositiveFiniteWith
  , mkNonNegativeFiniteDouble
  , mkNonNegativeFiniteWith
  ) where

import Data.Int (Int64)
import Data.Word (Word32)
import Moonlight.Core.Error (MoonlightError (..), MoonlightErrorContext (..), NonFiniteInput (..))
import Moonlight.Internal.FloatMath (isNegativeZero, normalizeNegativeZero)
import Prelude
  ( Bool(..), Bounded(..), Double, Either(..), Int, Num(..), Ord(..)
  , Integer, String, fromIntegral, id, isInfinite, isNaN, not, otherwise
  , round, toInteger, (&&), (^), (>>=)
  )

canonicalize :: Double -> Either MoonlightError Double
canonicalize x
  | isNaN x = Left (NonFiniteValue CanonicalizeContext NaNInput)
  | isInfinite x = Left (NonFiniteValue CanonicalizeContext InfiniteInput)
  | otherwise = Right (normalizeNegativeZero x)

isCanonical :: Double -> Bool
isCanonical x = not (isNaN x) && not (isInfinite x) && not (isNegativeZero x)

quantizeForHash :: Word32 -> Double -> Either MoonlightError Int64
quantizeForHash precision value
  | precision > 9     = Left (QuantizePrecisionTooLarge precision)
  | isNaN value       = Left (NonFiniteValue QuantizeContext NaNInput)
  | isInfinite value  = Left (NonFiniteValue QuantizeContext InfiniteInput)
  | otherwise =
      let scale = (10 :: Double) ^ (fromIntegral precision :: Int)
          scaled = value * scale
      in if scaled >= fromIntegral (maxBound :: Int64)
           then Right maxBound
           else
             if scaled <= fromIntegral (minBound :: Int64)
               then Right minBound
               else Right (saturate (round scaled :: Integer))
  where
    saturate :: Integer -> Int64
    saturate q
      | q > toInteger (maxBound :: Int64) = maxBound
      | q < toInteger (minBound :: Int64) = minBound
      | otherwise = fromIntegral q

mkFiniteDouble :: String -> Double -> Either MoonlightError Double
mkFiniteDouble domainLabel value
  | isNaN value = Left (NonFiniteValue (DomainContext domainLabel) NaNInput)
  | isInfinite value = Left (NonFiniteValue (DomainContext domainLabel) InfiniteInput)
  | otherwise = Right (normalizeNegativeZero value)

mkFiniteWith :: (Double -> errorValue) -> (Double -> value) -> Double -> Either errorValue value
mkFiniteWith errorValue wrapValue value =
  case canonicalize value of
    Left _ -> Left (errorValue value)
    Right canonicalValue -> Right (wrapValue canonicalValue)

mkPositiveInt :: String -> Int -> Either MoonlightError Int
mkPositiveInt domainLabel =
  mkPositiveIntWith
    (\_ -> NonPositiveValue (DomainContext domainLabel))
    id

mkPositiveIntWith :: (Int -> errorValue) -> (Int -> value) -> Int -> Either errorValue value
mkPositiveIntWith errorValue wrapValue value
  | value <= 0 = Left (errorValue value)
  | otherwise = Right (wrapValue value)

mkPositiveFiniteDouble :: String -> Double -> Either MoonlightError Double
mkPositiveFiniteDouble domainLabel value =
  mkFiniteDouble domainLabel value >>= \finiteValue ->
    if finiteValue <= 0
      then Left (NonPositiveValue (DomainContext domainLabel))
      else Right finiteValue

mkPositiveFiniteWith :: (Double -> errorValue) -> (Double -> value) -> Double -> Either errorValue value
mkPositiveFiniteWith errorValue wrapValue value =
  case mkFiniteWith errorValue id value of
    Left validationError -> Left validationError
    Right finiteValue ->
      if finiteValue <= 0
        then Left (errorValue value)
        else Right (wrapValue finiteValue)

mkNonNegativeFiniteDouble :: String -> Double -> Either MoonlightError Double
mkNonNegativeFiniteDouble domainLabel value =
  mkFiniteDouble domainLabel value >>= \finiteValue ->
    if finiteValue < 0
      then Left (NegativeValue (DomainContext domainLabel))
      else Right finiteValue

mkNonNegativeFiniteWith :: (Double -> errorValue) -> (Double -> value) -> Double -> Either errorValue value
mkNonNegativeFiniteWith errorValue wrapValue value =
  case mkFiniteWith errorValue id value of
    Left validationError -> Left validationError
    Right finiteValue ->
      if finiteValue < 0
        then Left (errorValue value)
        else Right (wrapValue finiteValue)

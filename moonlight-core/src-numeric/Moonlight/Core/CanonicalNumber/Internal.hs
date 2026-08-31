{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Moonlight.Core.CanonicalNumber.Internal
  ( CanonicalNumber (..),
    CanonicalFiniteValue,
    unsafeCanonicalFiniteAssumeCanonical,
    unsafeCanonicalFiniteLiteral,
    mkCanonicalFiniteValue,
    mkCanonicalFiniteNumber,
    canonicalFiniteValue,
    canonicalNumberFromDouble,
    canonicalNumberToMaybeDouble,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Moonlight.Core.Canon (canonicalize, isCanonical)
import Moonlight.Core.Error (MoonlightError (..))
import Moonlight.Core.Refinement
  ( Refined,
    RefinementPredicate (..),
    refineEither,
    refinedValue,
  )
import Moonlight.Internal.FloatMath (normalizeNegativeZero)
import Moonlight.Internal.Unsound (TrustJustification (..), unsafelyTrustRefined)
import Prelude
  ( Double,
    Either (..),
    Eq,
    Int,
    Maybe (..),
    Ord (..),
    Show,
    compare,
    fmap,
    isInfinite,
    isNaN,
    otherwise,
    (&&),
    (<),
    ($),
    (.),
    (>>=),
  )

type CanonicalFiniteTag :: Type
data CanonicalFiniteTag

instance RefinementPredicate CanonicalFiniteTag Double where
  refinementPredicate Proxy =
    isCanonical

type CanonicalFiniteValue :: Type
-- | Finite canonical 'Double' value.
--
-- Invariant: the contained value is finite, is accepted by 'isCanonical',
-- has passed 'canonicalize', and therefore contains no NaN, no infinity, and
-- no negative zero.
newtype CanonicalFiniteValue = CanonicalFiniteValue (Refined CanonicalFiniteTag Double)
  deriving stock (Eq, Ord, Show, Generic)

type CanonicalNumber :: Type
-- | Canonical numeric domain with finite canonical values and explicit infinities
-- plus an explicit NaN inhabitant.
data CanonicalNumber
  = CanonicalFinite CanonicalFiniteValue
  | NegInf
  | PosInf
  | NaN
  deriving stock (Eq, Show, Generic)

instance Ord CanonicalNumber where
  compare leftValue rightValue =
    compare (canonicalNumberOrderKey leftValue) (canonicalNumberOrderKey rightValue)

unsafeCanonicalFiniteLiteral :: Double -> CanonicalFiniteValue
unsafeCanonicalFiniteLiteral =
  unsafeCanonicalFiniteWith CarrierContractCanonicalLiteral

unsafeCanonicalFiniteAssumeCanonical :: Double -> CanonicalFiniteValue
unsafeCanonicalFiniteAssumeCanonical =
  unsafeCanonicalFiniteWith CanonicalObservationBoundary

unsafeCanonicalFiniteWith :: TrustJustification -> Double -> CanonicalFiniteValue
unsafeCanonicalFiniteWith justification rawValue =
  CanonicalFiniteValue
    (unsafelyTrustRefined justification (normalizeNegativeZero rawValue))

mkCanonicalFiniteValue :: Double -> Either MoonlightError CanonicalFiniteValue
mkCanonicalFiniteValue rawValue =
  fmap CanonicalFiniteValue $
    canonicalize rawValue
      >>= refineEither NonCanonicalFiniteValue

mkCanonicalFiniteNumber :: Double -> Either MoonlightError CanonicalNumber
mkCanonicalFiniteNumber =
  fmap CanonicalFinite . mkCanonicalFiniteValue

canonicalFiniteValue :: CanonicalFiniteValue -> Double
canonicalFiniteValue (CanonicalFiniteValue refinedCanonical) =
  refinedValue refinedCanonical

canonicalNumberFromDouble :: Double -> CanonicalNumber
canonicalNumberFromDouble rawValue
  | isNaN rawValue = NaN
  | isInfinite rawValue && rawValue < 0 = NegInf
  | isInfinite rawValue = PosInf
  | otherwise =
      case mkCanonicalFiniteValue rawValue of
        Right finiteValue -> CanonicalFinite finiteValue
        Left _ -> NaN

canonicalNumberToMaybeDouble :: CanonicalNumber -> Maybe Double
canonicalNumberToMaybeDouble canonicalNumber =
  case canonicalNumber of
    CanonicalFinite finiteValue -> Just (canonicalFiniteValue finiteValue)
    PosInf -> Nothing
    NegInf -> Nothing
    NaN -> Nothing

canonicalNumberOrderKey :: CanonicalNumber -> (Int, Maybe Double)
canonicalNumberOrderKey canonicalNumber =
  case canonicalNumber of
    NegInf -> (0, Nothing)
    CanonicalFinite finiteValue -> (1, Just (canonicalFiniteValue finiteValue))
    PosInf -> (2, Nothing)
    NaN -> (3, Nothing)

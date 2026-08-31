{-# LANGUAGE DeriveAnyClass #-}

-- | Shared error vocabulary for checked numeric construction; each constructor names the violated precondition.
module Moonlight.Core.Error
  ( MoonlightError (..),
    MoonlightErrorContext (..),
    NonFiniteInput (..),
    renderMoonlightError,
  )
where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Prelude (Eq, Show, String, show, (<>))

type MoonlightErrorContext :: Type
data MoonlightErrorContext
  = CanonicalizeContext
  | QuantizeContext
  | DomainContext !String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

type NonFiniteInput :: Type
data NonFiniteInput
  = NaNInput
  | InfiniteInput
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

type MoonlightError :: Type
data MoonlightError
  = NonFiniteValue !MoonlightErrorContext !NonFiniteInput
  | QuantizePrecisionTooLarge !Word32
  | NonPositiveValue !MoonlightErrorContext
  | NegativeValue !MoonlightErrorContext
  | NonCanonicalFiniteValue
  | InvariantViolation !String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

renderMoonlightError :: MoonlightError -> String
renderMoonlightError errorValue =
  case errorValue of
    NonFiniteValue CanonicalizeContext NaNInput -> "NaN"
    NonFiniteValue CanonicalizeContext InfiniteInput -> "Infinite"
    QuantizePrecisionTooLarge precision -> "quantize precision exceeds 9: " <> show precision
    NonFiniteValue QuantizeContext NaNInput -> "NaN in quantization"
    NonFiniteValue QuantizeContext InfiniteInput -> "Infinite in quantization"
    NonFiniteValue (DomainContext label) NaNInput -> label <> " must not be NaN"
    NonFiniteValue (DomainContext label) InfiniteInput -> label <> " must be finite"
    NonPositiveValue (DomainContext label) -> label <> " must be positive"
    NegativeValue (DomainContext label) -> label <> " must be non-negative"
    NonCanonicalFiniteValue -> "Non-canonical finite value"
    InvariantViolation label -> label
    NonPositiveValue context -> "NonPositiveValue " <> show context
    NegativeValue context -> "NegativeValue " <> show context

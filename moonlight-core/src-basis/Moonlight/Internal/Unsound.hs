{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Internal.Unsound
  ( TrustJustification (..),
    Refined (..),
    unsafelyTrustRefined,
    IdentifierToken (..),
    unsafelyTrustIdentifierToken,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude (Eq, Ord, Read, Show, seq)

type TrustJustification :: Type
data TrustJustification
  = CarrierContractCanonicalLiteral
  | CanonicalObservationBoundary
  deriving stock (Eq, Ord, Show, Read)

type Refined :: forall kindValue. kindValue -> Type -> Type
newtype Refined tag value = Refined value
  deriving stock (Eq, Ord, Show)

type role Refined nominal nominal

unsafelyTrustRefined :: TrustJustification -> value -> Refined tag value
unsafelyTrustRefined justification value =
  justification `seq` Refined value

type IdentifierToken :: forall namespaceKind. namespaceKind -> Type
newtype IdentifierToken namespace = IdentifierToken Text
  deriving stock (Eq, Ord, Show)

type role IdentifierToken nominal

unsafelyTrustIdentifierToken :: TrustJustification -> Text -> IdentifierToken namespace
unsafelyTrustIdentifierToken trustJustification rawInput =
  trustJustification `seq` IdentifierToken (Text.strip rawInput)

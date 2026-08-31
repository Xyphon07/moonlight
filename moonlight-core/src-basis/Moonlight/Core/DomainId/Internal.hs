{-# LANGUAGE DerivingStrategies #-}

module Moonlight.Core.DomainId.Internal
  ( DomainId,
    mkDomainId,
    unsafeTrustDomainId,
    renderDomainId,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Moonlight.Core.Identifier
  ( IdentifierToken,
    mkScopedIdentifier,
    renderScopedIdentifier,
  )
import Moonlight.Internal.Unsound (TrustJustification, unsafelyTrustIdentifierToken)
import Prelude (Eq, Maybe, Ord, Show)

type DomainIdNamespace :: Type
data DomainIdNamespace

type DomainId :: Type
newtype DomainId = DomainId (IdentifierToken DomainIdNamespace)
  deriving stock (Eq, Ord, Show)

mkDomainId :: Text -> Maybe DomainId
mkDomainId =
  mkScopedIdentifier DomainId

unsafeTrustDomainId :: TrustJustification -> Text -> DomainId
unsafeTrustDomainId justification rawInput =
  DomainId (unsafelyTrustIdentifierToken justification rawInput)

renderDomainId :: DomainId -> Text
renderDomainId =
  renderScopedIdentifier (\(DomainId identifierToken) -> identifierToken)

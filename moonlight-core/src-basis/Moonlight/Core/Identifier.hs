{-# LANGUAGE RoleAnnotations #-}

-- | Validated, namespace-tagged identifier tokens ('IdentifierToken') with
-- checked and scoped constructors.
module Moonlight.Core.Identifier
  ( IdentifierToken,
    mkIdentifierToken,
    mkIdentifierTokenWith,
    mkScopedIdentifier,
    renderIdentifierToken,
    renderScopedIdentifier,
    isValidIdentifier,
  )
where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Moonlight.Internal.Unsound (IdentifierToken (..))
import Prelude (Bool, Char, Maybe (..), fmap, not, (&&), (.), (==), (||))

mkIdentifierToken :: Text -> Maybe (IdentifierToken namespace)
mkIdentifierToken =
  mkIdentifierTokenWith isValidIdentifier

mkIdentifierTokenWith :: (Text -> Bool) -> Text -> Maybe (IdentifierToken namespace)
mkIdentifierTokenWith predicate rawInput =
  let candidate = Text.strip rawInput
   in if predicate candidate
        then Just (IdentifierToken candidate)
        else Nothing

mkScopedIdentifier :: (IdentifierToken namespace -> identifier) -> Text -> Maybe identifier
mkScopedIdentifier wrapIdentifier =
  fmap wrapIdentifier . mkIdentifierToken

renderIdentifierToken :: IdentifierToken namespace -> Text
renderIdentifierToken (IdentifierToken identifier) =
  identifier

renderScopedIdentifier :: (identifier -> IdentifierToken namespace) -> identifier -> Text
renderScopedIdentifier unwrapIdentifier =
  renderIdentifierToken . unwrapIdentifier

isValidIdentifier :: Text -> Bool
isValidIdentifier candidate =
  not (Text.null candidate) && Text.all isIdentifierCharacter candidate

isIdentifierCharacter :: Char -> Bool
isIdentifierCharacter character =
  isAlphaNum character || character == '-' || character == '_'

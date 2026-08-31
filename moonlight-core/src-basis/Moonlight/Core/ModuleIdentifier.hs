-- | Predicates and splitting for qualified module names (dot-separated,
-- upper-initial segments).
module Moonlight.Core.ModuleIdentifier
  ( isQualifiedModuleName,
    isCompactName,
    isModuleSegment,
    isModuleSegmentCharacter,
    splitModuleName,
  )
where

import Data.Char (isAlphaNum, isSpace, isUpper)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude

isQualifiedModuleName :: Text -> Bool
isQualifiedModuleName candidate =
  isCompactName candidate
    && all isModuleSegment (splitModuleName candidate)

isCompactName :: Text -> Bool
isCompactName candidate =
  not (Text.null candidate) && not (Text.any isSpace candidate)

isModuleSegment :: Text -> Bool
isModuleSegment segment =
  case Text.uncons segment of
    Nothing -> False
    Just (firstCharacter, remainingCharacters) ->
      isUpper firstCharacter && Text.all isModuleSegmentCharacter remainingCharacters

isModuleSegmentCharacter :: Char -> Bool
isModuleSegmentCharacter character =
  isAlphaNum character || character == '_' || character == '\''

splitModuleName :: Text -> [Text]
splitModuleName =
  Text.splitOn (Text.pack ".")

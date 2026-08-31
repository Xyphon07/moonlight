-- | Pure JSON rendering and parsing for theorem manifests.
module Moonlight.Core.ProofManifest
  ( ProofManifestError (..),
    renderTheoremManifestJson,
    parseTheoremManifestNames,
    canonicalTheoremManifestNames,
  )
where

import Data.Char (chr, digitToInt, isHexDigit, isSpace, ord)
import Data.Kind (Type)
import Data.List (find, intercalate)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Moonlight.Core.Dedup (firstDuplicate)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    many,
    pfail,
    readP_to_S,
    satisfy,
    sepBy,
    skipSpaces,
    string,
    (+++),
  )
import Prelude

type ProofManifestError :: Type
data ProofManifestError
  = ProofManifestParseFailure
  | EmptyTheoremManifestName
  | WhitespacePaddedTheoremManifestName !String
  | DuplicateTheoremManifestName !String
  deriving stock (Eq, Show)

renderTheoremManifestJson :: [String] -> String
renderTheoremManifestJson theoremIdentifiers =
  "{\"theorems\":["
    <> intercalate "," (map quoteJsonString (canonicalTheoremManifestNames theoremIdentifiers))
    <> "]}"

parseTheoremManifestNames :: String -> Either ProofManifestError [String]
parseTheoremManifestNames source =
  parseManifestJson source >>= validateTheoremManifestNames

canonicalTheoremManifestNames :: [String] -> [String]
canonicalTheoremManifestNames =
  Set.toAscList . Set.fromList

trimWhitespace :: String -> String
trimWhitespace =
  dropWhile isSpace
    . reverse
    . dropWhile isSpace
    . reverse

validateTheoremManifestNames :: [String] -> Either ProofManifestError [String]
validateTheoremManifestNames theoremNames =
  case find null theoremNames of
    Just _ ->
      Left EmptyTheoremManifestName
    Nothing ->
      case find hasPadding theoremNames of
        Just theoremName ->
          Left (WhitespacePaddedTheoremManifestName theoremName)
        Nothing ->
          case duplicateTheoremName theoremNames of
            Just theoremName ->
              Left (DuplicateTheoremManifestName theoremName)
            Nothing ->
              Right theoremNames
  where
    hasPadding theoremName = trimWhitespace theoremName /= theoremName

duplicateTheoremName :: [String] -> Maybe String
duplicateTheoremName =
  firstDuplicate

parseManifestJson :: String -> Either ProofManifestError [String]
parseManifestJson source =
  case listToMaybe (reverse parses) of
    Nothing -> Left ProofManifestParseFailure
    Just (theoremNames, _) -> Right theoremNames
  where
    parses = readP_to_S (theoremManifestParser <* skipSpaces <* eof) source

theoremManifestParser :: ReadP [String]
theoremManifestParser = do
  skipSpaces
  _ <- char '{'
  skipSpaces
  _ <- string "\"theorems\""
  skipSpaces
  _ <- char ':'
  theoremNames <- jsonStringArrayParser
  skipSpaces
  _ <- char '}'
  pure theoremNames

jsonStringArrayParser :: ReadP [String]
jsonStringArrayParser = do
  skipSpaces
  _ <- char '['
  skipSpaces
  theoremNames <- sepBy jsonStringParser (skipSpaces *> char ',' <* skipSpaces)
  skipSpaces
  _ <- char ']'
  pure theoremNames

jsonStringParser :: ReadP String
jsonStringParser = do
  _ <- char '"'
  stringValue <- many jsonStringCharacterParser
  _ <- char '"'
  pure stringValue

jsonStringCharacterParser :: ReadP Char
jsonStringCharacterParser =
  escapedCharacterParser +++ satisfy jsonStringUnescapedCharacter

escapedCharacterParser :: ReadP Char
escapedCharacterParser = do
  _ <- char '\\'
  char '"'
    +++ char '\\'
    +++ char '/'
    +++ (char 'n' >> pure '\n')
    +++ (char 't' >> pure '\t')
    +++ (char 'r' >> pure '\r')
    +++ (char 'b' >> pure '\b')
    +++ (char 'f' >> pure '\f')
    +++ unicodeEscapeParser

unicodeEscapeParser :: ReadP Char
unicodeEscapeParser = do
  _ <- char 'u'
  h3 <- satisfy isHexDigit
  h2 <- satisfy isHexDigit
  h1 <- satisfy isHexDigit
  h0 <- satisfy isHexDigit
  let codePoint = digitToInt h3 * 4096 + digitToInt h2 * 256 + digitToInt h1 * 16 + digitToInt h0
  if jsonStringScalarEscape codePoint
    then pure (chr codePoint)
    else pfail

jsonStringUnescapedCharacter :: Char -> Bool
jsonStringUnescapedCharacter character =
  character /= '"' && character /= '\\' && ord character >= 0x20

jsonStringScalarEscape :: Int -> Bool
jsonStringScalarEscape codePoint =
  codePoint < 0xD800 || codePoint > 0xDFFF

quoteJsonString :: String -> String
quoteJsonString value =
  "\"" <> concatMap escapeJsonCharacter value <> "\""

escapeJsonCharacter :: Char -> String
escapeJsonCharacter '"' = "\\\""
escapeJsonCharacter '\\' = "\\\\"
escapeJsonCharacter c
  | ord c < 0x20 = "\\u" <> padHex4 (ord c)
  | otherwise = [c]

padHex4 :: Int -> String
padHex4 n =
  let d3 = n `div` 4096
      d2 = (n `mod` 4096) `div` 256
      d1 = (n `mod` 256) `div` 16
      d0 = n `mod` 16
   in fmap hexDigit [d3, d2, d1, d0]
  where
    hexDigit digit
      | digit < 10 = chr (ord '0' + digit)
      | digit < 16 = chr (ord 'a' + digit - 10)
      | otherwise = '?'

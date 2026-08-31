{-# LANGUAGE DerivingStrategies #-}

-- | The 'IsLawName' class and the shared 'CommonLawName' vocabulary, with
-- helpers to derive law names from constructor names.
module Moonlight.Core.LawName
  ( IsLawName (..),
    CommonLawName (..),
    constructorLawName,
    constructorLawNameWithOverrides,
  )
where

import Data.Char (isDigit, isLower, isUpper, toLower)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Prelude

type IsLawName :: Type -> Constraint
class (Eq a, Ord a, Show a) => IsLawName a where
  lawNameText :: a -> String

type CommonLawName :: Type
data CommonLawName
  = LatticeAbsorptionJoin
  | LatticeAbsorptionMeet
  | NormalizeIdempotent
  deriving stock (Eq, Ord, Show)

instance IsLawName CommonLawName where
  lawNameText = constructorLawName . show

constructorLawName :: String -> String
constructorLawName = constructorLawNameWithOverrides []

constructorLawNameWithOverrides :: [(String, String)] -> String -> String
constructorLawNameWithOverrides overrides constructorName =
  fromMaybe (snakeCaseConstructorName constructorName) (lookup constructorName overrides)

snakeCaseConstructorName :: String -> String
snakeCaseConstructorName constructorName =
  case constructorName of
    [] -> []
    firstCharacter : remainingCharacters ->
      toLower firstCharacter : go firstCharacter remainingCharacters
  where
    go previousCharacter remainingCharacters =
      case remainingCharacters of
        [] -> []
        currentCharacter : nextCharacters ->
          separator previousCharacter currentCharacter nextCharacters
            ++ [toLower currentCharacter]
            ++ go currentCharacter nextCharacters

separator :: Char -> Char -> String -> String
separator previousCharacter currentCharacter nextCharacters
  | startsUpperBoundary previousCharacter currentCharacter nextCharacters = "_"
  | isDigit currentCharacter && not (isDigit previousCharacter) = "_"
  | otherwise = ""

startsUpperBoundary :: Char -> Char -> String -> Bool
startsUpperBoundary previousCharacter currentCharacter nextCharacters =
  isUpper currentCharacter
    && ((isLower previousCharacter || isDigit previousCharacter) || continuesAcronym nextCharacters)

continuesAcronym :: String -> Bool
continuesAcronym nextCharacters =
  case nextCharacters of
    nextCharacter : _ -> isLower nextCharacter
    [] -> False

{-# LANGUAGE StandaloneKindSignatures #-}

{-| Package-origin and occurrence-resolution oracle values. -}
module Moonlight.Pale.Ghc.Hie.Oracle
  ( PackageName,
    PackageVersion,
    PackageUnit,
    PackageUnitParseFailure (..),
    mkPackageUnit,
    packageUnitText,
    mkResolvedOrigin,
    ResolvedOrigin (..),
    ModuleNameOracle (..),
    occResolvesUniquely,
    originAcceptedBy,
  )
where

import Data.Char (isDigit)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Moonlight.Pale.Ghc.Expr (SourceRegion)
import Moonlight.Pale.Ghc.Hie.TypeWords (TypeWords)

type PackageName :: Type
newtype PackageName = PackageName String
  deriving stock (Eq, Ord, Show)

type PackageVersion :: Type
newtype PackageVersion = PackageVersion String
  deriving stock (Eq, Ord, Show)

type PackageUnit :: Type
data PackageUnit = PackageUnit
  { puName :: !PackageName,
    puVersion :: !(Maybe PackageVersion),
    puText :: !String
  }
  deriving stock (Eq, Ord, Show)

type PackageUnitParseFailure :: Type
data PackageUnitParseFailure
  = EmptyPackageUnit
  | EmptyPackageName !String
  deriving stock (Eq, Ord, Show)

type ResolvedOrigin :: Type
data ResolvedOrigin = ResolvedOrigin
  { roUnit :: !PackageUnit,
    roModule :: !String,
    roOcc :: !String
  }
  deriving stock (Eq, Ord, Show)

type ModuleNameOracle :: Type
data ModuleNameOracle = ModuleNameOracle
  { mnoSourcePath :: !FilePath,
    mnoGlobalUsesAtSpan :: !(Map SourceRegion (Map String (Set ResolvedOrigin))),
    mnoGlobalUses :: !(Map String (Set ResolvedOrigin)),
    mnoEvidenceAtSpan :: !(Map SourceRegion (Set ResolvedOrigin)),
    mnoTypeAtSpan :: !(Map SourceRegion (Set TypeWords))
  }
  deriving stock (Eq, Show)

occResolvesUniquely :: ModuleNameOracle -> String -> Set ResolvedOrigin -> Bool
occResolvesUniquely oracle occName acceptedOrigins =
  case Map.lookup occName (mnoGlobalUses oracle) of
    Nothing ->
      False
    Just resolvedOrigins ->
      Set.size resolvedOrigins == 1
        && Set.isSubsetOf resolvedOrigins acceptedOrigins

originAcceptedBy :: ResolvedOrigin -> Set ResolvedOrigin -> Bool
originAcceptedBy =
  Set.member

mkResolvedOrigin :: String -> String -> String -> Either PackageUnitParseFailure ResolvedOrigin
mkResolvedOrigin unitText moduleText occText =
  (\unitValue -> ResolvedOrigin unitValue moduleText occText) <$> mkPackageUnit unitText

mkPackageUnit :: String -> Either PackageUnitParseFailure PackageUnit
mkPackageUnit unitText
  | null unitText =
      Left EmptyPackageUnit
  | otherwise =
      case packageUnitParts unitText of
        ("", _) ->
          Left (EmptyPackageName unitText)
        (nameText, versionText) ->
          Right
            PackageUnit
              { puName = PackageName nameText,
                puVersion = fmap PackageVersion versionText,
                puText = unitText
              }

packageUnitText :: PackageUnit -> String
packageUnitText =
  puText

packageUnitParts :: String -> (String, Maybe String)
packageUnitParts unitText =
  case break (== '-') (reverse unitText) of
    (reversedSuffix, '-' : reversedName)
      | let suffixText = reverse reversedSuffix,
        versionLike suffixText ->
          (reverse reversedName, Just suffixText)
    _ ->
      (unitText, Nothing)

versionLike :: String -> Bool
versionLike textValue =
  case textValue of
    [] ->
      False
    firstChar : _ ->
      isDigit firstChar && all versionChar textValue

versionChar :: Char -> Bool
versionChar charValue =
  isDigit charValue || charValue == '.'

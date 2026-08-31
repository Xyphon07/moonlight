{-# LANGUAGE DataKinds #-}

-- | The path-thin quotient of a fully validated site path category: quotient
-- objects and morphisms, and the quotient maps from the path category.
module Moonlight.Category.Pure.Site.Quotient
  ( PathThinCat (..),
    PathThinObject (..),
    PathThinMorphism (..),
    SitePathQuotient,
    sitePathQuotientDomain,
    sitePathQuotientCodomain,
    SitePathQuotientError (..),
    pathThinCat,
    mkPathThinObject,
    mkPathThinMorphism,
    quotientPathThinObject,
    quotientPathThinMorphism,
    pathThinCodomainObject,
    pathThinCodomainMorphism,
    sitePathQuotient,
    quotientMapObject,
    quotientMapMorphism,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NonEmpty
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinMor,
    FinMorphismId,
    FinObjectId,
    FinObj,
  )
import Moonlight.Category.Pure.Site.Category
  ( SitePathCategory,
    SitePathMorphism,
    SitePathObject,
    mkSitePathObject,
    sitePathCategoryCodomain,
    sitePathCategoryKernel,
    sitePathManifest,
    sitePathMorphismCategory,
    sitePathMorphismCodomain,
    sitePathMorphismNodes,
    sitePathObjectCategory,
    sitePathObjectCodomain,
    sitePathObjectValue,
  )
import Moonlight.Category.Pure.Site.Compile
  ( ThinSiteKernel,
    ThinSiteValidation (SiteValidated),
    ThinSiteLookupError (..),
    thinSiteFinMorphismByEndpoints,
    thinSiteFinObject,
  )
import Moonlight.Category.Pure.Site.Core (SiteManifest (..))

type PathThinCat :: Type -> Type
newtype PathThinCat obj = PathThinCat
  { pathThinDomain :: SitePathCategory obj
  }
  deriving stock (Eq, Show)

type PathThinObject :: Type -> Type
data PathThinObject obj = PathThinObject
  { pathThinObjectCategory :: PathThinCat obj,
    pathThinObjectValue :: obj,
    pathThinObjectCodomain :: FinObj
  }
  deriving stock (Eq, Show)

type PathThinMorphism :: Type -> Type
data PathThinMorphism obj = PathThinMorphism
  { pathThinMorphismCategory :: PathThinCat obj,
    pathThinMorphismSourceValue :: obj,
    pathThinMorphismTargetValue :: obj,
    pathThinMorphismWitness :: SitePathMorphism obj,
    pathThinMorphismCodomain :: FinMor
  }
  deriving stock (Show)

instance Eq obj => Eq (PathThinMorphism obj) where
  left == right =
    pathThinMorphismCategory left == pathThinMorphismCategory right
      && pathThinMorphismSourceValue left == pathThinMorphismSourceValue right
      && pathThinMorphismTargetValue left == pathThinMorphismTargetValue right

type PathThinCompositor :: Type -> Type
data PathThinCompositor obj
  = PathThinCompositor
  deriving stock (Eq, Show)

type PathThinTwoMor :: Type -> Type
data PathThinTwoMor obj
  = PathThinTwoMor
  deriving stock (Eq, Show)

type PathThinCategoryError :: Type -> Type
data PathThinCategoryError obj
  = PathThinObjectWrongCategory
  | PathThinMorphismWrongCategory
  | PathThinMorphismNotComposable
  | PathThinInvalidIdentity
  | PathThinInvalidComposite
  | PathThinInvalidSourceTarget
  deriving stock (Eq, Show)

type SitePathQuotient :: Type -> Type
newtype SitePathQuotient obj = SitePathQuotient
  { sitePathQuotientDomain :: SitePathCategory obj
  }

sitePathQuotientCodomain :: SitePathQuotient obj -> FinCat
sitePathQuotientCodomain = sitePathCategoryCodomain . sitePathQuotientDomain

type SitePathQuotientError :: Type -> Type
data SitePathQuotientError obj
  = QuotientUnknownObject obj
  | QuotientCodomainObjectMissing FinObjectId
  | QuotientUnknownMorphismPair obj obj
  | QuotientCodomainMorphismMissing FinMorphismId
  | QuotientCodomainMorphismInvalid
  | QuotientObjectWrongDomain
  | QuotientMorphismWrongDomain
  deriving stock (Eq, Show)

pathThinCat :: SitePathCategory obj -> PathThinCat obj
pathThinCat = PathThinCat

mkPathThinObject :: Ord obj => PathThinCat obj -> obj -> Maybe (PathThinObject obj)
mkPathThinObject category objectValue = do
  siteObject <- mkSitePathObject (pathThinDomain category) objectValue
  pure
    PathThinObject
      { pathThinObjectCategory = category,
        pathThinObjectValue = objectValue,
        pathThinObjectCodomain = sitePathObjectCodomain siteObject
      }

mkPathThinMorphism ::
  Ord obj =>
  PathThinCat obj ->
  SitePathMorphism obj ->
  Maybe (PathThinMorphism obj)
mkPathThinMorphism category witness =
  if sitePathMorphismCategory witness == pathThinDomain category
    then
      let sourceValue = NonEmpty.head (sitePathMorphismNodes witness)
          targetValue = NonEmpty.last (sitePathMorphismNodes witness)
       in Just
            PathThinMorphism
              { pathThinMorphismCategory = category,
                pathThinMorphismSourceValue = sourceValue,
                pathThinMorphismTargetValue = targetValue,
                pathThinMorphismWitness = witness,
                pathThinMorphismCodomain = sitePathMorphismCodomain witness
              }
    else Nothing

quotientPathThinObject :: SitePathObject obj -> PathThinObject obj
quotientPathThinObject objectValue =
  PathThinObject
    { pathThinObjectCategory = pathThinCat (sitePathObjectCategory objectValue),
      pathThinObjectValue = sitePathObjectValue objectValue,
      pathThinObjectCodomain = sitePathObjectCodomain objectValue
    }

quotientPathThinMorphism :: SitePathMorphism obj -> PathThinMorphism obj
quotientPathThinMorphism morphism =
  PathThinMorphism
    { pathThinMorphismCategory = pathThinCat (sitePathMorphismCategory morphism),
      pathThinMorphismSourceValue = NonEmpty.head (sitePathMorphismNodes morphism),
      pathThinMorphismTargetValue = NonEmpty.last (sitePathMorphismNodes morphism),
      pathThinMorphismWitness = morphism,
      pathThinMorphismCodomain = sitePathMorphismCodomain morphism
    }

pathThinCodomainObject :: PathThinObject obj -> FinObj
pathThinCodomainObject =
  pathThinObjectCodomain

pathThinCodomainMorphism :: PathThinMorphism obj -> FinMor
pathThinCodomainMorphism =
  pathThinMorphismCodomain

sitePathQuotient :: SitePathCategory obj -> SitePathQuotient obj
sitePathQuotient = SitePathQuotient

sameSitePathDomain :: Ord obj => SitePathCategory obj -> SitePathQuotient obj -> Bool
sameSitePathDomain category quotient =
  let categoryManifest = sitePathManifest category
      quotientManifest = sitePathManifest (sitePathQuotientDomain quotient)
   in siteObjects categoryManifest == siteObjects quotientManifest
        && siteImports categoryManifest == siteImports quotientManifest

quotientMapObject ::
  Ord obj =>
  SitePathQuotient obj ->
  SitePathObject obj ->
  Either (SitePathQuotientError obj) FinObj
quotientMapObject quotient objectValue
  | not (sameSitePathDomain (sitePathObjectCategory objectValue) quotient) =
      Left QuotientObjectWrongDomain
  | otherwise =
      first fromThinSiteLookupError
        ( thinSiteFinObject
            (sitePathQuotientKernel quotient)
            (sitePathObjectValue objectValue)
        )

quotientMapMorphism ::
  Ord obj =>
  SitePathQuotient obj ->
  SitePathMorphism obj ->
  Either (SitePathQuotientError obj) FinMor
quotientMapMorphism quotient morphism
  | not (sameSitePathDomain (sitePathMorphismCategory morphism) quotient) =
      Left QuotientMorphismWrongDomain
  | otherwise =
      first fromThinSiteLookupError
        ( thinSiteFinMorphismByEndpoints
            (sitePathQuotientKernel quotient)
            (NonEmpty.head (sitePathMorphismNodes morphism))
            (NonEmpty.last (sitePathMorphismNodes morphism))
        )

instance Ord obj => Category (PathThinCat obj) where
  type Ob (PathThinCat obj) = PathThinObject obj
  type Mor (PathThinCat obj) = PathThinMorphism obj
  type TwoMor (PathThinCat obj) = PathThinTwoMor obj
  type Compositor (PathThinCat obj) = PathThinCompositor obj
  type CategoryError (PathThinCat obj) = PathThinCategoryError obj

  identity category objectValue =
    if pathThinObjectCategory objectValue /= category
      then Left PathThinObjectWrongCategory
      else
        case mkSitePathObject (pathThinDomain category) (pathThinObjectValue objectValue) of
          Nothing -> Left PathThinInvalidIdentity
          Just siteObject -> do
            witness <- first (const PathThinInvalidIdentity) (identity (pathThinDomain category) siteObject)
            codomain <- first (const PathThinInvalidIdentity) (identity (sitePathCategoryCodomain (pathThinDomain category)) (pathThinObjectCodomain objectValue))
            Right
              PathThinMorphism
                { pathThinMorphismCategory = category,
                  pathThinMorphismSourceValue = pathThinObjectValue objectValue,
                  pathThinMorphismTargetValue = pathThinObjectValue objectValue,
                  pathThinMorphismWitness = witness,
                  pathThinMorphismCodomain = codomain
                }

  compose category left right
    | pathThinMorphismCategory left /= category = Left PathThinMorphismWrongCategory
    | pathThinMorphismCategory right /= category = Left PathThinMorphismWrongCategory
    | pathThinMorphismTargetValue right /= pathThinMorphismSourceValue left = Left PathThinMorphismNotComposable
    | otherwise = do
        (witnessComposed, _) <-
          first
            (const PathThinInvalidComposite)
            (compose (pathThinDomain category) (pathThinMorphismWitness left) (pathThinMorphismWitness right))
        case mkPathThinMorphism category witnessComposed of
          Nothing -> Left PathThinInvalidComposite
          Just morphism -> Right (morphism, PathThinCompositor)

  source category morphism =
    if pathThinMorphismCategory morphism /= category
      then Left PathThinMorphismWrongCategory
      else do
        codomainObject <-
          first
            (const PathThinInvalidSourceTarget)
            (source (sitePathCategoryCodomain (pathThinDomain category)) (pathThinMorphismCodomain morphism))
        Right
          PathThinObject
            { pathThinObjectCategory = category,
              pathThinObjectValue = pathThinMorphismSourceValue morphism,
              pathThinObjectCodomain = codomainObject
            }

  target category morphism =
    if pathThinMorphismCategory morphism /= category
      then Left PathThinMorphismWrongCategory
      else do
        codomainObject <-
          first
            (const PathThinInvalidSourceTarget)
            (target (sitePathCategoryCodomain (pathThinDomain category)) (pathThinMorphismCodomain morphism))
        Right
          PathThinObject
            { pathThinObjectCategory = category,
              pathThinObjectValue = pathThinMorphismTargetValue morphism,
              pathThinObjectCodomain = codomainObject
            }

sitePathQuotientKernel :: SitePathQuotient obj -> ThinSiteKernel 'SiteValidated obj
sitePathQuotientKernel = sitePathCategoryKernel . sitePathQuotientDomain

fromThinSiteLookupError :: ThinSiteLookupError obj -> SitePathQuotientError obj
fromThinSiteLookupError lookupError =
  case lookupError of
    ThinSiteUnknownObject objectValue ->
      QuotientUnknownObject objectValue
    ThinSiteCodomainObjectMissing objectId ->
      QuotientCodomainObjectMissing objectId
    ThinSiteUnknownMorphismPair sourceValue targetValue ->
      QuotientUnknownMorphismPair sourceValue targetValue
    ThinSiteCodomainMorphismMissing morId ->
      QuotientCodomainMorphismMissing morId
    ThinSiteCodomainMorphismInvalid _ ->
      QuotientCodomainMorphismInvalid

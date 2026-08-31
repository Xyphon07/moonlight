{-# LANGUAGE DataKinds #-}

-- | The path category of a fully validated site: objects, morphisms-as-paths,
-- and enumeration of the morphisms between two objects.
module Moonlight.Category.Pure.Site.Category
  ( SitePathCategory,
    SitePathObject,
    SitePathMorphism,
    sitePathCategory,
    sitePathCategoryKernel,
    sitePathCategoryCodomain,
    sitePathManifest,
    sitePathObjectCategory,
    sitePathObjectValue,
    sitePathObjectCodomain,
    sitePathMorphismCategory,
    sitePathMorphismNodes,
    sitePathMorphismCodomain,
    mkSitePathObject,
    mkSitePathMorphism,
    sitePathMorphismsBetween,
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Tree (foldTree, unfoldTree)
import Moonlight.Category.Pure.Category (Category (..))
import Moonlight.Category.Pure.FinCat
  ( FinCat,
    FinCatError,
    FinMor,
    FinObj,
  )
import Moonlight.Core qualified as Aggregate
import Moonlight.Category.Pure.Site.Compile
  ( ThinSiteKernel,
    ThinSiteValidation (SiteValidated),
    thinSiteFinMorphism,
    thinSiteFinObject,
    thinSiteKernelCodomain,
    thinSiteKernelManifest,
  )
import Moonlight.Category.Pure.Site.Core (SiteManifest (..))
import Moonlight.Category.Pure.Site.Graph (siteImportEdges)

type SitePathCategory :: Type -> Type
newtype SitePathCategory obj = SitePathCategory
  { sitePathCategoryKernel :: ThinSiteKernel 'SiteValidated obj
  }
  deriving stock (Eq, Show)

type SitePathObject :: Type -> Type
data SitePathObject obj = SitePathObject
  { sitePathObjectCategory :: SitePathCategory obj,
    sitePathObjectValue :: obj,
    sitePathObjectCodomain :: FinObj
  }
  deriving stock (Eq, Show)

type SitePathMorphism :: Type -> Type
data SitePathMorphism obj = SitePathMorphism
  { sitePathMorphismCategory :: SitePathCategory obj,
    sitePathMorphismNodes :: NonEmpty obj,
    sitePathMorphismCodomain :: FinMor
  }
  deriving stock (Eq, Show)

type SitePathCompositor :: Type -> Type
data SitePathCompositor obj
  = SitePathCompositor
  deriving stock (Eq, Show)

type SitePathTwoMor :: Type -> Type
data SitePathTwoMor obj
  = SitePathTwoMor
  deriving stock (Eq, Show)

type SitePathCategoryError :: Type -> Type
data SitePathCategoryError obj
  = SitePathObjectWrongCategory
  | SitePathMorphismWrongCategory
  | SitePathCodomainError FinCatError
  deriving stock (Eq, Show)

sitePathCategory :: ThinSiteKernel 'SiteValidated obj -> SitePathCategory obj
sitePathCategory = SitePathCategory

sitePathCategoryCodomain :: SitePathCategory obj -> FinCat
sitePathCategoryCodomain = thinSiteKernelCodomain . sitePathCategoryKernel

sitePathManifest :: SitePathCategory obj -> SiteManifest obj
sitePathManifest = thinSiteKernelManifest . sitePathCategoryKernel

mkSitePathObject :: Ord obj => SitePathCategory obj -> obj -> Maybe (SitePathObject obj)
mkSitePathObject category objectValue =
  case thinSiteFinObject (sitePathCategoryKernel category) objectValue of
    Left _ ->
      Nothing
    Right codomainObject ->
      Just
        SitePathObject
          { sitePathObjectCategory = category,
            sitePathObjectValue = objectValue,
            sitePathObjectCodomain = codomainObject
          }

mkSitePathMorphism :: Ord obj => SitePathCategory obj -> NonEmpty obj -> Maybe (SitePathMorphism obj)
mkSitePathMorphism category nodes =
  if validPath category nodes
    then sitePathMorphismFromValidatedNodes category nodes
    else Nothing

sitePathMorphismFromValidatedNodes :: Ord obj => SitePathCategory obj -> NonEmpty obj -> Maybe (SitePathMorphism obj)
sitePathMorphismFromValidatedNodes category nodes = do
  codomainMorphism <- either (const Nothing) Just (thinSiteFinMorphism (sitePathCategoryKernel category) nodes)
  pure
    SitePathMorphism
      { sitePathMorphismCategory = category,
        sitePathMorphismNodes = nodes,
        sitePathMorphismCodomain = codomainMorphism
      }

sitePathMorphismsBetween ::
  Ord obj =>
  SitePathCategory obj ->
  obj ->
  obj ->
  [SitePathMorphism obj]
sitePathMorphismsBetween category sourceValue targetValue =
  allPathNodes category sourceValue targetValue
    & mapMaybe (sitePathMorphismFromValidatedNodes category)

instance Ord obj => Category (SitePathCategory obj) where
  type Ob (SitePathCategory obj) = SitePathObject obj
  type Mor (SitePathCategory obj) = SitePathMorphism obj
  type TwoMor (SitePathCategory obj) = SitePathTwoMor obj
  type Compositor (SitePathCategory obj) = SitePathCompositor obj
  type CategoryError (SitePathCategory obj) = SitePathCategoryError obj

  identity category objectValue =
    if sitePathObjectCategory objectValue /= category
      then Left SitePathObjectWrongCategory
      else do
        codomainMorphism <-
          first SitePathCodomainError
            (identity (sitePathCategoryCodomain category) (sitePathObjectCodomain objectValue))
        Right
          SitePathMorphism
            { sitePathMorphismCategory = category,
              sitePathMorphismNodes = sitePathObjectValue objectValue :| [],
              sitePathMorphismCodomain = codomainMorphism
            }

  compose category left right
    | sitePathMorphismCategory left /= category = Left SitePathMorphismWrongCategory
    | sitePathMorphismCategory right /= category = Left SitePathMorphismWrongCategory
    | otherwise = do
        (codomainMorphism, _) <-
          first SitePathCodomainError
            (compose (sitePathCategoryCodomain category) (sitePathMorphismCodomain left) (sitePathMorphismCodomain right))
        let leftNodes = sitePathMorphismNodes left
            rightNodes = sitePathMorphismNodes right
            mergedPath =
              NonEmpty.head rightNodes
                :| (NonEmpty.tail rightNodes <> drop 1 (NonEmpty.toList leftNodes))
        Right
          ( SitePathMorphism
              { sitePathMorphismCategory = category,
                sitePathMorphismNodes = mergedPath,
                sitePathMorphismCodomain = codomainMorphism
              },
            SitePathCompositor
          )

  source category morphism =
    if sitePathMorphismCategory morphism /= category
      then Left SitePathMorphismWrongCategory
      else do
        codomainObject <-
          first SitePathCodomainError
            (source (sitePathCategoryCodomain category) (sitePathMorphismCodomain morphism))
        Right
          SitePathObject
            { sitePathObjectCategory = category,
              sitePathObjectValue = NonEmpty.head (sitePathMorphismNodes morphism),
              sitePathObjectCodomain = codomainObject
            }

  target category morphism =
    if sitePathMorphismCategory morphism /= category
      then Left SitePathMorphismWrongCategory
      else do
        codomainObject <-
          first SitePathCodomainError
            (target (sitePathCategoryCodomain category) (sitePathMorphismCodomain morphism))
        Right
          SitePathObject
            { sitePathObjectCategory = category,
              sitePathObjectValue = NonEmpty.last (sitePathMorphismNodes morphism),
              sitePathObjectCodomain = codomainObject
            }

validPath :: Ord obj => SitePathCategory obj -> NonEmpty obj -> Bool
validPath category nodes =
  let manifest = sitePathManifest category
      objects = siteObjects manifest
      allNodesPresent =
        nodes
          & NonEmpty.toList
          & all (`Set.member` objects)
      importEdges = siteImportEdges manifest
      consecutive =
        Aggregate.adjacentPairs (NonEmpty.toList nodes)
          & all (`Set.member` importEdges)
   in allNodesPresent && consecutive

allPathNodes :: Ord obj => SitePathCategory obj -> obj -> obj -> [NonEmpty obj]
allPathNodes category sourceValue targetValue =
  let manifest = sitePathManifest category
      imports = siteImports manifest
      unfoldPath (current, visited) =
        ( current,
          if current == targetValue
            then []
            else
              Map.findWithDefault Set.empty current imports
                & Set.toAscList
                & filter (`Set.notMember` visited)
                & fmap (\next -> (next, Set.insert next visited))
        )
      collectPaths current childPaths
        | current == targetValue = [current :| []]
        | otherwise = foldMap (fmap (NonEmpty.cons current)) childPaths
   in foldTree collectPaths (unfoldTree unfoldPath (sourceValue, Set.singleton sourceValue))

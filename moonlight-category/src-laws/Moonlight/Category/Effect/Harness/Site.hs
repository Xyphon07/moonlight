{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Builds the t'SiteLaws' record for a site presentation.
module Moonlight.Category.Effect.Harness.Site
  ( mkSiteLaws,
  )
where

import Data.Function ((&))
import qualified Data.Set as Set
import Moonlight.Category.Effect.Harness.Category (mkCategoryLaws)
import Moonlight.Category.Effect.Harness.Core (CategoryLaws (..), SiteLaws (..))
import Moonlight.Category.Pure.FinCat (FinCat, allMorphisms)
import Moonlight.Category.Pure.Site
  ( SiteManifest,
    SiteViolation (..),
    siteImportEdges,
    validateSiteManifest,
  )
import Moonlight.Category.Pure.Site.Compile
  ( thinSiteImportKernel,
    thinSiteKernelCodomain,
  )
import Prelude hiding (Functor)

mkSiteLaws :: forall obj layer. Ord obj => SiteLaws obj layer
mkSiteLaws =
  SiteLaws
    { siteCoverageClosure = siteCoverageClosureLaw @obj,
      siteCategoryIdentity = siteCategoryIdentityLaw @obj,
      siteCategoryAssociativity = siteCategoryAssociativityLaw @obj,
      siteLayerPolicyConformance = siteLayerPolicyConformanceLaw @obj @layer
    }

siteCoverageClosureLaw :: forall obj. Ord obj => SiteManifest obj -> Bool
siteCoverageClosureLaw manifest =
  validateSiteManifest manifest
    & all
      ( \violation ->
          case violation of
            CoverOutsideReachable {} -> False
            CoverNotClosed {} -> False
            MissingCover {} -> False
            _ -> True
      )

siteCategoryIdentityLaw :: forall obj. Ord obj => SiteManifest obj -> Bool
siteCategoryIdentityLaw manifest =
  case thinSiteImportKernel manifest of
    Left _ -> False
    Right importKernel ->
      let finCategory = thinSiteKernelCodomain importKernel
          morphisms = allMorphisms finCategory
          laws = mkCategoryLaws @FinCat finCategory
       in all (categoryLeftIdentity laws) morphisms
            && all (categoryRightIdentity laws) morphisms

siteCategoryAssociativityLaw :: forall obj. Ord obj => SiteManifest obj -> Bool
siteCategoryAssociativityLaw manifest =
  case thinSiteImportKernel manifest of
    Left _ -> False
    Right importKernel ->
      let finCategory = thinSiteKernelCodomain importKernel
          morphisms = allMorphisms finCategory
          laws = mkCategoryLaws @FinCat finCategory
       in [ (firstValue, secondValue, thirdValue)
            | firstValue <- morphisms,
              secondValue <- morphisms,
              thirdValue <- morphisms
          ]
            & all
              ( \(firstValue, secondValue, thirdValue) ->
                  categoryAssociativity laws firstValue secondValue thirdValue
              )

siteLayerPolicyConformanceLaw :: forall obj layer. Ord obj => (obj -> layer) -> (layer -> layer -> Bool) -> SiteManifest obj -> Bool
siteLayerPolicyConformanceLaw layerOf isAllowed manifest =
  siteImportEdges manifest
    & Set.toList
    & all
      ( \(importer, imported) ->
          isAllowed (layerOf importer) (layerOf imported)
      )

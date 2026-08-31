module Moonlight.Category.Effect.SitePathEnumeration
  ( allEqual,
    sitePathObjectValues,
    sitePathObjects,
    sitePathMorphisms,
  )
where

import Data.Function ((&))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Moonlight.Category.Pure.Site
  ( SitePathCategory,
    SitePathMorphism,
    SitePathObject,
    mkSitePathObject,
    siteObjects,
    sitePathManifest,
    sitePathMorphismsBetween,
  )

allEqual :: Eq a => [a] -> Bool
allEqual values =
  case values of
    [] -> True
    firstValue : restValues -> all (== firstValue) restValues

sitePathObjectValues :: SitePathCategory obj -> [obj]
sitePathObjectValues category =
  siteObjects (sitePathManifest category)
    & Set.toList

sitePathObjects :: Ord obj => SitePathCategory obj -> [SitePathObject obj]
sitePathObjects category =
  sitePathObjectValues category
    & mapMaybe (mkSitePathObject category)

sitePathMorphisms :: Ord obj => SitePathCategory obj -> [SitePathMorphism obj]
sitePathMorphisms category =
  let objectValues = sitePathObjectValues category
   in objectValues
        >>= (\sourceValue -> objectValues >>= sitePathMorphismsBetween category sourceValue)

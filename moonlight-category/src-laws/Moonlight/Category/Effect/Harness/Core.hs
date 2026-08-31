{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Shared plumbing for the law harnesses: law records and total accessors over
-- 'Category' operations.
module Moonlight.Category.Effect.Harness.Core
  ( CategoryLaws (..),
    SiteLaws (..),
    identityC,
    sourceC,
    targetC,
    composeC,
  )
where

import Data.Kind (Type)
import Moonlight.Category.Pure.Category (Category (..), composeMor)
import Moonlight.Category.Pure.Site (SiteManifest)

type CategoryLaws :: Type -> Type
data CategoryLaws c = CategoryLaws
  { categoryLeftIdentity :: Mor c -> Bool,
    categoryRightIdentity :: Mor c -> Bool,
    categoryAssociativity :: Mor c -> Mor c -> Mor c -> Bool
  }

type SiteLaws :: Type -> Type -> Type
data SiteLaws obj layer = SiteLaws
  { siteCoverageClosure :: SiteManifest obj -> Bool,
    siteCategoryIdentity :: SiteManifest obj -> Bool,
    siteCategoryAssociativity :: SiteManifest obj -> Bool,
    siteLayerPolicyConformance :: (obj -> layer) -> (layer -> layer -> Bool) -> SiteManifest obj -> Bool
  }

identityC :: forall c. Category c => c -> Ob c -> Either (CategoryError c) (Mor c)
identityC = identity @c

sourceC :: forall c. Category c => c -> Mor c -> Either (CategoryError c) (Ob c)
sourceC = source @c

targetC :: forall c. Category c => c -> Mor c -> Either (CategoryError c) (Ob c)
targetC = target @c

composeC :: forall c. Category c => c -> Mor c -> Mor c -> Either (CategoryError c) (Mor c)
composeC = composeMor @c

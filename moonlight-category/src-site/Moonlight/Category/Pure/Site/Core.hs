-- | Core site types: the t'SiteManifest' (objects, imports, covers) and the
-- 'SiteViolation'/'SiteFinCatError' diagnostics.
module Moonlight.Category.Pure.Site.Core
  ( SiteManifest (..),
    SiteViolation (..),
    SiteFinCatError (..),
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)

type SiteManifest :: Type -> Type
data SiteManifest obj = SiteManifest
  { siteObjects :: Set obj,
    siteImports :: Map obj (Set obj),
    siteCovers :: Map obj (Set obj)
  }
  deriving stock (Eq, Show)

type SiteViolation :: Type -> Type
data SiteViolation obj
  = MissingCover obj
  | UnknownImportTarget obj
  | UnknownImportedObject obj obj
  | UnknownCoverTarget obj
  | UnknownCoveredObject obj obj
  | CoverOutsideReachable obj (Set obj)
  | CoverNotClosed obj obj (Set obj)
  | ImportCycleDetected (NonEmpty obj)
  deriving stock (Eq, Show)

type SiteFinCatError :: Type -> Type
data SiteFinCatError obj
  = SiteManifestInvalid (NonEmpty (SiteViolation obj))
  deriving stock (Eq, Show)

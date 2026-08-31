-- | Shared site-manifest fixtures for the law suite.
module Moonlight.Category.Effect.SiteGen
  ( diamondManifest,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Moonlight.Category.Pure.Site (SiteManifest (..))

diamondManifest :: SiteManifest Int
diamondManifest =
  SiteManifest
    { siteObjects = Set.fromList [0, 1, 2, 3],
      siteImports =
        Map.fromList
          [ (0, Set.fromList [1, 2]),
            (1, Set.singleton 3),
            (2, Set.singleton 3),
            (3, Set.empty)
          ],
      siteCovers =
        Map.fromList
          [ (0, Set.fromList [1, 2, 3]),
            (1, Set.singleton 3),
            (2, Set.singleton 3),
            (3, Set.empty)
          ]
    }

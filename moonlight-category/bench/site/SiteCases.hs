module SiteCases
  ( SiteCase (..),
    pathSiteCases,
    siteCaseLabel,
    siteCases,
    siteEndpointObjectIds,
    siteEndpoints,
    siteManifestFromCase,
  )
where

import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import FinCat (objectKeys)
import Moonlight.Category.Pure.FinCat (FinObjectId, finObjId)
import Moonlight.Category.Pure.Site.Compile
  ( ThinSiteKernel,
    thinSiteFinObject,
  )
import Moonlight.Category.Pure.Site.Core (SiteManifest (..))
import Moonlight.Category.Pure.Site.Graph (reachableClosure)

data SiteCase
  = LinearSite !Int
  | LayeredSite !Int !Int
  deriving stock (Eq, Ord, Show)

siteCases :: [SiteCase]
siteCases =
  [ LinearSite 16,
    LinearSite 64,
    LayeredSite 3 5,
    LayeredSite 4 5
  ]

pathSiteCases :: [SiteCase]
pathSiteCases =
  [ LinearSite 16,
    LayeredSite 2 8,
    LayeredSite 3 6
  ]

siteCaseLabel :: SiteCase -> String
siteCaseLabel siteCase =
  case siteCase of
    LinearSite objectCount -> "linear objects=" <> show objectCount
    LayeredSite width depth -> "layered width=" <> show width <> " depth=" <> show depth

siteManifestFromCase :: SiteCase -> SiteManifest Int
siteManifestFromCase siteCase =
  case siteCase of
    LinearSite objectCount -> linearSiteManifest objectCount
    LayeredSite width depth -> layeredSiteManifest width depth

siteEndpoints :: SiteCase -> (Int, Int)
siteEndpoints siteCase =
  case siteCase of
    LinearSite objectCount -> (0, objectCount - 1)
    LayeredSite width depth -> (layeredNode width 0 0, layeredNode width depth 0)

siteEndpointObjectIds :: SiteCase -> ThinSiteKernel validation Int -> Either String (FinObjectId, FinObjectId)
siteEndpointObjectIds siteCase kernel = do
  sourceObject <- first show (thinSiteFinObject kernel sourceValue)
  targetObject <- first show (thinSiteFinObject kernel targetValue)
  pure (finObjId sourceObject, finObjId targetObject)
  where
    (sourceValue, targetValue) = siteEndpoints siteCase

linearSiteManifest :: Int -> SiteManifest Int
linearSiteManifest objectCount =
  validSiteManifest objects imports
  where
    objects = Set.fromAscList (objectKeys objectCount)
    imports =
      objectKeys objectCount
        & fmap
          ( \objectKey ->
              ( objectKey,
                if objectKey + 1 < objectCount
                  then Set.singleton (objectKey + 1)
                  else Set.empty
              )
          )
        & Map.fromAscList

layeredSiteManifest :: Int -> Int -> SiteManifest Int
layeredSiteManifest width depth =
  validSiteManifest objects imports
  where
    layers = [0 .. depth]
    objects =
      layers
        >>= (\layer -> layeredSlots width depth layer & fmap (layeredNode width layer))
        & Set.fromList
    imports =
      layers
        >>= (\layer -> layeredSlots width depth layer & fmap (layerImports layer))
        & Map.fromList
    layerImports layer slot =
      let sourceNode = layeredNode width layer slot
          importedNodes =
            if layer < depth
              then layeredSlots width depth (layer + 1) & fmap (layeredNode width (layer + 1)) & Set.fromList
              else Set.empty
       in (sourceNode, importedNodes)

layeredSlots :: Int -> Int -> Int -> [Int]
layeredSlots width depth layer
  | layer == 0 = [0]
  | layer == depth = [0]
  | otherwise = [0 .. width - 1]

layeredNode :: Int -> Int -> Int -> Int
layeredNode width layer slot =
  layer * width + slot

validSiteManifest :: Set Int -> Map Int (Set Int) -> SiteManifest Int
validSiteManifest objects imports =
  SiteManifest
    { siteObjects = objects,
      siteImports = imports,
      siteCovers = closureCovers objects imports
    }

closureCovers :: Set Int -> Map Int (Set Int) -> Map Int (Set Int)
closureCovers objects imports =
  let closureMap = reachableClosure imports
   in Map.fromSet
        (\objectValue -> Map.findWithDefault Set.empty objectValue closureMap)
        objects

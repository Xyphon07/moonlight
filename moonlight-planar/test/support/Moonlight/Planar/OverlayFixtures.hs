-- | Labelled-region fixtures shared by native and public algebra laws.
module Moonlight.Planar.OverlayFixtures
  ( singletonLayer
  , overlayRegions
  ) where

import qualified Data.Map.Strict as Map
import Moonlight.Planar.Overlay (OverlayResult, overlayLayers)
import Moonlight.Planar.Region (PlanarLayer, PlanarRegion, planarLayer)
import Support (requireRight)

singletonLayer :: Ord label => label -> label -> PlanarRegion -> IO (PlanarLayer label)
singletonLayer outside inside region =
  requireRight "singleton layer" (planarLayer outside (Map.singleton inside region))

overlayRegions
  :: PlanarRegion
  -> PlanarRegion
  -> IO (OverlayResult (Bool, Bool))
overlayRegions left right = do
  leftLayer <- singletonLayer False True left
  rightLayer <- singletonLayer False True right
  requireRight "region algebra overlay" (overlayLayers leftLayer rightLayer)

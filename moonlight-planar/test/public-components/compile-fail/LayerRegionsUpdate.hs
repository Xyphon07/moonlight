-- Expected: GHC-22385, record field planarLayerRegions not in scope.
module Client (observe, attempt) where

import Data.Map.Strict (Map)
import Moonlight.Planar.Region (PlanarLayer, PlanarRegion, planarLayerRegions)

observe :: PlanarLayer Bool -> Map Bool PlanarRegion
observe = planarLayerRegions

attempt :: PlanarLayer Bool -> Map Bool PlanarRegion -> PlanarLayer Bool
attempt layer regions = layer { planarLayerRegions = regions }

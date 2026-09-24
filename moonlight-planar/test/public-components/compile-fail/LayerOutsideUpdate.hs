-- Expected: GHC-22385, record field planarLayerOutsideLabel not in scope.
module Client (observe, attempt) where

import Moonlight.Planar.Region (PlanarLayer, planarLayerOutsideLabel)

observe :: PlanarLayer Bool -> Bool
observe = planarLayerOutsideLabel

attempt :: PlanarLayer Bool -> Bool -> PlanarLayer Bool
attempt layer outside = layer { planarLayerOutsideLabel = outside }

-- Expected: GHC-22385, record field polygonHoleLoops not in scope.
module Client (observe, attempt) where

import Moonlight.Planar.Region (ExactLoop, PolygonComponent, polygonHoleLoops)

observe :: PolygonComponent -> [ExactLoop]
observe = polygonHoleLoops

attempt :: PolygonComponent -> [ExactLoop] -> PolygonComponent
attempt component holes = component { polygonHoleLoops = holes }

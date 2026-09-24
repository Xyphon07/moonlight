-- Expected: GHC-22385, record field polygonOuterLoop not in scope.
module Client (observe, attempt) where

import Moonlight.Planar.Region (ExactLoop, PolygonComponent, polygonOuterLoop)

observe :: PolygonComponent -> ExactLoop
observe = polygonOuterLoop

attempt :: PolygonComponent -> ExactLoop -> PolygonComponent
attempt component outer = component { polygonOuterLoop = outer }

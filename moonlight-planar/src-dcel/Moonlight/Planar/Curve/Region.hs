{-# LANGUAGE DerivingStrategies #-}

-- | Explicit simple outer/hole assembly of approximated curves. Admission
-- proves polygon topology only, never topology equivalence with the curves.
module Moonlight.Planar.Curve.Region
  ( CurveComponent (..)
  , CurveRegionError (..)
  , lowerSimpleRegion
  ) where

import Data.Bifunctor (first)
import Moonlight.Planar.Curve (ClosedTrail, Located)
import Moonlight.Planar.Curve.Lowering
  ( LoweringPolicy, LoweringError, LoweredPath, lowerClosedTrail, loweredPoints )
import Moonlight.Planar.Region
  ( PlanarRegion, RegionValidationError, exactLoop, polygonComponent, planarRegion )

-- | The submitted winding is significant: outer CCW, holes CW. An explicitly
-- located closed trail does not by itself claim simplicity or containment.
data CurveComponent = CurveComponent
  { curveOuter :: !(Located ClosedTrail)
  , curveHoles :: ![Located ClosedTrail]
  }

data CurveRegionError
  = CurveLoweringRefused !LoweringError
  | CurvePolygonRefused !RegionValidationError
  deriving stock (Eq, Show)

-- | The lowering policy's leaf budget applies per contour. Receipts remain in
-- submitted component/outer/hole order, independently of polygon canonicalization.
-- Coordinates remain source-local; the policy's affine map only measures error.
lowerSimpleRegion
  :: LoweringPolicy
  -> [CurveComponent]
  -> Either CurveRegionError (PlanarRegion, [LoweredPath])
lowerSimpleRegion policy components = do
  admitted <- traverse lowerComponent components
  region <- first CurvePolygonRefused (planarRegion (map fst admitted))
  pure (region, concatMap snd admitted)
 where
  lowerComponent (CurveComponent outer holes) = do
    outerPath <- first CurveLoweringRefused (lowerClosedTrail policy outer)
    holePaths <- traverse (first CurveLoweringRefused . lowerClosedTrail policy) holes
    outerLoop <- first CurvePolygonRefused (exactLoop (loweredPoints outerPath))
    holeLoops <- traverse (first CurvePolygonRefused . exactLoop . loweredPoints) holePaths
    component <- first CurvePolygonRefused (polygonComponent outerLoop holeLoops)
    pure (component, outerPath : holePaths)

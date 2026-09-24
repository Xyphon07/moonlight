{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}

-- | These public-client attempts must be rejected at the admission boundary.
-- The test driver catches only deferred type errors, never arbitrary exceptions.
module Moonlight.Planar.AdmissionTypeErrors where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import GHC.Generics (K1 (..), M1 (..), (:*:) (..), from, to)
import GHC.Records (getField)
import GHC.Real (Ratio ((:%)))
import Moonlight.Planar.Exact
  ( ExactPoint, ExactRational, ExactAffineLine, ExactRetainedPolygon, ExactSegment, ExactRay
  , ExactVector (..)
  )
import Moonlight.Planar.Point (Point, QueryPoint, queryPointValue)
import Moonlight.Planar.Types (DelaunayTriangulation)
import Moonlight.Planar.BuildStats (BuildStats)
import Moonlight.Planar.Scalar (RadiusSquared)
import Moonlight.Planar.Alpha (AlphaBirth)
import Moonlight.Planar.Simplex (PlanarSimplex, PlanarComplex)
import Moonlight.Planar.PowerDiagram
  ( BoundedPowerDiagram, PowerCellDisposition, RegularTriangulation, PowerSite )
import Moonlight.Planar.Region
  ( ExactLoop, PolygonComponent, PlanarRegion, PlanarLayer
  , polygonOuterLoop, polygonHoleLoops, planarLayerOutsideLabel, planarLayerRegions
  )

forgeExactLoop :: NonEmpty ExactPoint -> ExactLoop
forgeExactLoop points = to (M1 (M1 (M1 (K1 points))))

forgePolygonComponent :: ExactLoop -> PolygonComponent
forgePolygonComponent clockwise =
  to (M1 (M1 (M1 (K1 clockwise) :*: M1 (K1 ([] :: [ExactLoop])))))

forgePlanarRegion :: PolygonComponent -> PlanarRegion
forgePlanarRegion component = to (M1 (M1 (M1 (K1 [component, component]))))

forgePlanarLayer :: PlanarRegion -> PlanarLayer Bool
forgePlanarLayer region =
  to (M1 (M1 (M1 (K1 False) :*: M1 (K1 (Map.singleton False region)))))

forgeBoundedPowerDiagram :: Map.Map Bool (PowerCellDisposition Bool) -> BoundedPowerDiagram Bool
forgeBoundedPowerDiagram cells = to (M1 (M1 (M1 (K1 cells))))

forgeRegularTriangulation :: PowerSite Bool -> RegularTriangulation Bool
forgeRegularTriangulation site =
  to (M1 (M1 (M1 (K1 (Map.singleton False site)) :*: M1 (K1 Nothing))))

forgeExactRational :: ExactRational
forgeExactRational = to (M1 (M1 (M1 (K1 (1 :% 0 :: Ratio Integer)))))

forgeExactAffineLine :: ExactAffineLine
forgeExactAffineLine =
  to (M1 (M1 (M1 (K1 (0 :: ExactRational)) :*: (M1 (K1 (0 :: ExactRational)) :*: M1 (K1 (0 :: ExactRational))))))

forgeRetainedPolygon :: ExactRetainedPolygon -> ExactRetainedPolygon
forgeRetainedPolygon polygon =
  case from polygon of
    M1 (M1 (M1 (K1 (vertex :| _)))) -> to (M1 (M1 (M1 (K1 (vertex :| [])))))

forgeExactSegment :: ExactPoint -> ExactSegment
forgeExactSegment point = to (M1 (M1 (M1 (K1 point) :*: M1 (K1 point))))

forgeExactRay :: ExactPoint -> ExactRay
forgeExactRay point = to (M1 (M1 (M1 (K1 point) :*: M1 (K1 (ExactVector 0 0)))))

forgeQueryPoint :: Point -> QueryPoint
forgeQueryPoint point = to (M1 (M1 (M1 (K1 point))))

forgeBuildStats :: BuildStats
forgeBuildStats = to (M1 (M1 (M1 (K1 (U.empty :: U.Vector Word64)))))


-- The left observations are lawful functions. The right attempted HasField
-- dictionaries would restore the record-selector capability and must fail.
recordPolygonOuter :: PolygonComponent -> (ExactLoop, ExactLoop)
recordPolygonOuter component = (polygonOuterLoop component, getField @"polygonOuterLoop" component)

recordPolygonHoles :: PolygonComponent -> ([ExactLoop], [ExactLoop])
recordPolygonHoles component = (polygonHoleLoops component, getField @"polygonHoleLoops" component)

recordLayerOutside :: PlanarLayer Bool -> (Bool, Bool)
recordLayerOutside layer = (planarLayerOutsideLabel layer, getField @"planarLayerOutsideLabel" layer)

recordLayerRegions :: PlanarLayer Bool -> (Map.Map Bool PlanarRegion, Map.Map Bool PlanarRegion)
recordLayerRegions layer = (planarLayerRegions layer, getField @"planarLayerRegions" layer)

recordQueryPoint :: QueryPoint -> (Point, Point)
recordQueryPoint query = (queryPointValue query, getField @"queryPointValue" query)

reconstructTriangulation :: DelaunayTriangulation () -> DelaunayTriangulation ()
reconstructTriangulation triangulation = to (from triangulation)

forgeRadiusSquared :: RadiusSquared
forgeRadiusSquared = to (M1 (M1 (M1 (K1 (-1 :: Double)))))

reconstructAlphaBirth :: AlphaBirth -> AlphaBirth
reconstructAlphaBirth birth = to (from birth)

reconstructPlanarSimplex :: PlanarSimplex Int -> PlanarSimplex Int
reconstructPlanarSimplex simplex = to (from simplex)

reconstructPlanarComplex :: PlanarComplex Int -> PlanarComplex Int
reconstructPlanarComplex complex = to (from complex)

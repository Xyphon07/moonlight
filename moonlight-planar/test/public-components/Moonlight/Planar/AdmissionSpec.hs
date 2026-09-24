{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | Public admission controls and attempted reconstruction of invalid carriers.
module Moonlight.Planar.AdmissionSpec (tests) where

import Control.DeepSeq (rnf)
import Control.Exception (TypeError, displayException, evaluate, try)
import Data.Foldable (traverse_)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import GHC.Generics (from, to)
import GHC.Records (getField)
import qualified Moonlight.Planar.AdmissionTypeErrors as Rejected
import Moonlight.Planar.BulkLoad (delaunayGeometry)
import Moonlight.Planar.Convex (convexPolygon)
import Moonlight.Planar.Exact
  ( ExactPoint, ExactClosedHalfPlane, exactPointFromPoint, exactRetainedPolygon, exactRetainedPolygonPoints
  , exactRational, exactAffineLine, exactClosedHalfPlane, exactSegment, exactRay
  , ExactVector (..), ExactArithmeticError (..), ExactHalfPlaneError (..), ExactGeometryError (..)
  )
import Moonlight.Planar.Point (mkQueryPoint)
import Moonlight.Planar.PowerDiagram
  ( PowerCellDisposition (..), RegularTriangulation, boundedPowerDiagramFromRegular
  , emptyRegularTriangulation, powerCellDispositions, regularSiteCount
  , powerSite, powerWeightFromExact
  )
import Moonlight.Planar.Region
  ( exactLoop, exactLoopPoints, polygonComponent, polygonOuterLoop, polygonHoleLoops
  , planarRegion, planarRegionComponents, planarLayer, planarLayerOutsideLabel
  , planarLayerRegions, RegionValidationError (..)
  )
import Moonlight.Planar.Point (Point (..), queryPointValue)
import Moonlight.Planar.Scalar (mkRadiusSquared, radiusSquaredValue)
import Moonlight.Planar.Alpha (alphaBirthFromRadiusSquared, alphaBirthToDouble)
import Moonlight.Planar.Simplex (planarVertex, planarComplex, planarSimplexVertices, planarComplexCells)
import System.Exit (die)

tests :: IO ()
tests = do
  triangulation <- requireRight (delaunayGeometry (Vector.fromList [Point 0 0, Point 2 0, Point 0 2]))
  points <- requireRight (traverse exactPointFromPoint (Point 0 0 :| [Point 2 0, Point 0 2]))
  clockwisePoints <- requireRight (traverse exactPointFromPoint (Point 0 0 :| [Point 0 2, Point 2 0]))
  loop <- requireRight (exactLoop points)
  clockwise <- requireRight (exactLoop clockwisePoints)
  component <- requireRight (polygonComponent loop [])
  region <- requireRight (planarRegion [component])
  layer <- requireRight (planarLayer False (Map.singleton True region))
  domain <- requireRight (convexPolygon points)
  retained <- requireRight (exactRetainedPolygon points)
  rational <- requireRight (exactRational 1 2)
  line <- requireRight (exactAffineLine 1 0 0)
  segment <- requireRight (exactSegment (firstPoint points) (secondPoint points))
  ray <- requireRight (exactRay (firstPoint points) (ExactVector 1 0))
  query <- requireRight (mkQueryPoint (Point 0 0))
  radius <- requireRight (mkRadiusSquared 4)
  let birth = alphaBirthFromRadiusSquared radius
      simplex = planarVertex (7 :: Int)
  complex <- requireRight (planarComplex (Set.singleton simplex))
  site <- requireRight (powerSite False (Point 0 0) (powerWeightFromExact 0))
  let regular = emptyRegularTriangulation :: RegularTriangulation Bool
  (diagram, _) <- requireRight (boundedPowerDiagramFromRegular domain regular)
  traverse_ evaluate
    [ rnf triangulation, rnf rational, rnf line, rnf (exactClosedHalfPlane line), rnf retained
    , rnf segment, rnf ray, rnf query, rnf loop, rnf component, rnf region
    , rnf layer, rnf regular, rnf diagram
    , rnf radius, rnf birth, rnf simplex, rnf complex
    ]
  assert "read-only observations preserve admitted values"
    ( exactLoopPoints loop == points
        && polygonOuterLoop component == loop
        && null (polygonHoleLoops component)
        && planarRegionComponents region == [component]
        && not (planarLayerOutsideLabel layer)
        && planarLayerRegions layer == Map.singleton True region
        && regularSiteCount regular == 0
        && null (powerCellDispositions diagram)
        && exactLoop (exactRetainedPolygonPoints retained) == Right loop
        && queryPointValue query == Point 0 0
        && radiusSquaredValue radius == 4
        && alphaBirthToDouble birth == 4
        && planarSimplexVertices simplex == (7 :| [])
        && planarComplexCells complex == Set.singleton simplex
    )
  assert "unconstrained Point retains its lawful HasField selector"
    (getField @"pointX" (Point 3 4) == 3)
  assert "unconstrained Generic carriers remain lawful"
    ( (to (from (firstPoint points)) :: ExactPoint) == firstPoint points
        && (to (from (exactClosedHalfPlane line)) :: ExactClosedHalfPlane) == exactClosedHalfPlane line
    )
  assert "checked loop constructor rejects one point"
    (case exactLoop (firstPoint points :| []) of Left (RegionLoopDegenerate _) -> True; _ -> False)
  assert "checked component constructor rejects clockwise outer"
    (case polygonComponent clockwise [] of Left (RegionOuterLoopWinding LT) -> True; _ -> False)
  assert "checked region constructor rejects duplicate interiors"
    (case planarRegion [component, component] of Left (RegionComponentInteriorOverlap 0 1) -> True; _ -> False)
  assert "checked layer constructor rejects outside label ownership"
    (case planarLayer False (Map.singleton False region) of Left RegionOutsideLabelUsed -> True; _ -> False)
  assert "checked rational constructor rejects zero denominator"
    (case exactRational 1 0 of Left ExactZeroDenominator -> True; _ -> False)
  assert "checked affine line constructor rejects zero normal"
    (case exactAffineLine 0 0 0 of Left (ExactAffineLineZeroNormal 0 0 0) -> True; _ -> False)
  assert "checked retained polygon rejects singleton"
    (case exactRetainedPolygon (firstPoint points :| []) of Left (ExactRetainedPolygonTooFewVertices 1) -> True; _ -> False)
  assert "checked segment rejects coincident endpoints"
    (case exactSegment (firstPoint points) (firstPoint points) of Left (ExactSegmentEndpointsCoincide _) -> True; _ -> False)
  assert "checked ray rejects zero direction"
    (case exactRay (firstPoint points) (ExactVector 0 0) of Left (ExactRayZeroDirection _) -> True; _ -> False)
  assert "checked query point rejects NaN"
    (case mkQueryPoint (Point (0 / 0) 0) of Left _ -> True; _ -> False)
  assert "checked radius rejects negatives"
    (case mkRadiusSquared (-1) of Left _ -> True; _ -> False)
  traverse_ (uncurry assertGenericRejected)
    [ ("Triangulation", Rejected.reconstructTriangulation triangulation `seq` ())
    , ("ExactLoop", Rejected.forgeExactLoop (fmap (const (firstPoint points)) points) `seq` ())
    , ("PolygonComponent", Rejected.forgePolygonComponent clockwise `seq` ())
    , ("PlanarRegion", Rejected.forgePlanarRegion component `seq` ())
    , ("PlanarLayer", Rejected.forgePlanarLayer region `seq` ())
    , ("BoundedPowerDiagram", Rejected.forgeBoundedPowerDiagram
        (Map.fromList [(False, PublishedPowerCell domain), (True, PublishedPowerCell domain)]) `seq` ())
    , ("RegularTriangulation", Rejected.forgeRegularTriangulation site `seq` ())
    , ("ExactRational", Rejected.forgeExactRational `seq` ())
    , ("ExactAffineLine", Rejected.forgeExactAffineLine `seq` ())
    , ("ExactRetainedPolygon", Rejected.forgeRetainedPolygon retained `seq` ())
    , ("ExactSegment", Rejected.forgeExactSegment (firstPoint points) `seq` ())
    , ("ExactRay", Rejected.forgeExactRay (firstPoint points) `seq` ())
    , ("QueryPoint", Rejected.forgeQueryPoint (Point (0 / 0) 0) `seq` ())
    , ("BuildStats", Rejected.forgeBuildStats `seq` ())
    , ("RadiusSquared", Rejected.forgeRadiusSquared `seq` ())
    , ("AlphaBirth", Rejected.reconstructAlphaBirth birth `seq` ())
    , ("PlanarSimplex", Rejected.reconstructPlanarSimplex simplex `seq` ())
    , ("PlanarComplex", Rejected.reconstructPlanarComplex complex `seq` ())
    ]
  traverse_ (uncurry assertRecordSelectorRejected)
    [ ("polygonOuterLoop", snd (Rejected.recordPolygonOuter component) `seq` ())
    , ("polygonHoleLoops", snd (Rejected.recordPolygonHoles component) `seq` ())
    , ("planarLayerOutsideLabel", snd (Rejected.recordLayerOutside layer) `seq` ())
    , ("planarLayerRegions", snd (Rejected.recordLayerRegions layer) `seq` ())
    , ("queryPointValue", snd (Rejected.recordQueryPoint query) `seq` ())
    ]
  assert "ordinary observers remain callable beside rejected HasField dictionaries"
    ( fst (Rejected.recordPolygonOuter component) == loop
        && null (fst (Rejected.recordPolygonHoles component))
        && not (fst (Rejected.recordLayerOutside layer))
        && fst (Rejected.recordLayerRegions layer) == Map.singleton True region
        && fst (Rejected.recordQueryPoint query) == Point 0 0
    )
  putStrLn "public admission: Generic and HasField reconstruction routes rejected"
 where
  firstPoint :: NonEmpty ExactPoint -> ExactPoint
  firstPoint (point :| _) = point
  secondPoint :: NonEmpty ExactPoint -> ExactPoint
  secondPoint (_ :| (point : _)) = point
  secondPoint (point :| []) = point

requireRight :: Show obstruction => Either obstruction value -> IO value
requireRight = either (die . ("admission positive control failed: " <>) . show) pure

assert :: String -> Bool -> IO ()
assert label condition = if condition then pure () else die label

assertGenericRejected :: String -> () -> IO ()
assertGenericRejected carrier =
  assertTypeRejected carrier
    (\diagnostic -> carrier `isInfixOf` diagnostic
      && ("Generic" `isInfixOf` diagnostic || "Rep" `isInfixOf` diagnostic))

assertRecordSelectorRejected :: String -> () -> IO ()
assertRecordSelectorRejected field =
  assertTypeRejected field
    (\diagnostic -> field `isInfixOf` diagnostic && "HasField" `isInfixOf` diagnostic)

assertTypeRejected :: String -> (String -> Bool) -> () -> IO ()
assertTypeRejected label expectedDiagnostic attempted = do
  result <- try (evaluate attempted) :: IO (Either TypeError ())
  case result of
    Left failure ->
      assert (label <> " failed for the wrong reason: " <> displayException failure)
        (expectedDiagnostic (displayException failure))
    Right () -> die (label <> " bypassed checked admission")

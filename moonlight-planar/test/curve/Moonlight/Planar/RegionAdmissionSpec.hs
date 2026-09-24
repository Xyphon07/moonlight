-- | Pin the polygon admission semantics consumed by curve-region lowering.
module Moonlight.Planar.RegionAdmissionSpec (tests) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Moonlight.Planar.Region
  ( RegionPointLocation (..)
  , RegionValidationError (..)
  , exactLoop
  , planarLayer
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , regionPointLocation
  )
import Support (assertEqual, integerPoint, rectangleComponent, rectangleLoop, requireRight)

tests :: IO ()
tests = sequence_
  [ testBatchRefusalOrder
  , testBoundaryContactAndHoles
  , putStrLn "region admission: ok"
  ]

testBatchRefusalOrder :: IO ()
testBatchRefusalOrder = do
  first <- rectangleComponent 20 0 24 4
  second <- rectangleComponent 21 1 25 5
  third <- rectangleComponent 0 0 4 4
  fourth <- rectangleComponent 1 1 5 5
  assertEqual "component refusal uses input order rather than x sweep order"
    (Left (RegionComponentInteriorOverlap 0 1))
    (planarRegion [first, second, third, fourth])
  regions <- traverse (requireRight "singleton region" . planarRegion . pure)
    [first, second, third, fourth]
  assertEqual "layer refusal uses label order rather than x sweep order"
    (Left (RegionLayerInteriorOverlap 0 1))
    (planarLayer (-1 :: Int) (Map.fromList (zip [0 ..] regions)))
  outer <- rectangleLoop 0 0 8 8
  outsideHole <- requireRight "clockwise outside hole" (exactLoop
    (integerPoint 20 0 :| [integerPoint 20 2, integerPoint 22 2, integerPoint 22 0]))
  wrongWindingHole <- rectangleLoop 2 2 3 3
  assertEqual "all hole windings precede containment admission"
    (Left (RegionHoleLoopWinding 1 GT))
    (polygonComponent outer [outsideHole, wrongWindingHole])
  assertEqual "disjoint bounds still report outside hole location"
    (Left (RegionHoleLocation 0 RegionExterior))
    (polygonComponent outer [outsideHole])

testBoundaryContactAndHoles :: IO ()
testBoundaryContactAndHoles = do
  left <- rectangleComponent 0 0 2 2
  right <- rectangleComponent 2 0 4 2
  touching <- requireRight "oppositely directed shared boundary" (planarRegion [left, right])
  assertEqual "boundary-only contact retains both components"
    2 (length (planarRegionComponents touching))
  assertEqual "shared edge remains boundary" RegionOnBoundary
    (regionPointLocation touching (integerPoint 2 1))
  assertEqual "identically directed duplicate cycles overlap"
    (Left (RegionComponentInteriorOverlap 0 1)) (planarRegion [left, left])
  outer <- rectangleLoop 0 0 10 10
  hole <- requireRight "clockwise central hole" (exactLoop
    (integerPoint 2 2 :| [integerPoint 2 8, integerPoint 8 8, integerPoint 8 2]))
  annulus <- requireRight "annulus" (polygonComponent outer [hole])
  island <- rectangleComponent 4 4 6 6
  nested <- requireRight "island within another component's hole" (planarRegion [annulus, island])
  assertEqual "annulus interior" RegionInterior (regionPointLocation nested (integerPoint 1 1))
  assertEqual "unfilled hole" RegionExterior (regionPointLocation nested (integerPoint 3 3))
  assertEqual "separate island interior" RegionInterior (regionPointLocation nested (integerPoint 5 5))
  assertEqual "hole boundary" RegionOnBoundary (regionPointLocation nested (integerPoint 2 5))

-- | Facade-only laws for exact planar Boolean composition.
module Moonlight.Triangulation.RegionAlgebraSpec (tests) where

import Moonlight.Triangulation
import Moonlight.Triangulation.AlgebraFixtures
  ( overlayRegions
  , rectangleComponent
  , rectangleRegion
  )
import Support (assertEqual, requireRight)

tests :: IO ()
tests = do
  testRegionBooleanLaws
  testFacadeComposition
  putStrLn "region algebra: ok"

testRegionBooleanLaws :: IO ()
testRegionBooleanLaws = do
  left <- rectangleRegion 0 0 2 2
  middle <- rectangleRegion 1 0 3 2
  right <- rectangleRegion 2 0 4 2
  expectedUnion <- rectangleRegion 0 0 3 2
  expectedIntersection <- rectangleRegion 1 0 2 2
  expectedDifference <- rectangleRegion 0 0 1 2

  leftUnionMiddle <- regionUnion left middle
  middleUnionLeft <- regionUnion middle left
  assertEqual "region union result" expectedUnion leftUnionMiddle
  assertEqual "region union commutativity" leftUnionMiddle middleUnionLeft
  assertEqual "region union idempotence" left =<< regionUnion left left
  assertEqual
    "region intersection result"
    expectedIntersection
    =<< regionIntersection left middle
  assertEqual
    "regularized region difference"
    expectedDifference
    =<< regionDifference left middle

  leftAssociated <- regionUnion leftUnionMiddle right
  middleUnionRight <- regionUnion middle right
  rightAssociated <- regionUnion left middleUnionRight
  assertEqual "region union associativity" leftAssociated rightAssociated

testFacadeComposition :: IO ()
testFacadeComposition = do
  left <- rectangleRegion 0 0 2 2
  right <- rectangleRegion 1 0 3 2
  overlay <- overlayRegions left right
  selectedUnion <-
    requireRight
      "facade selected region"
      (overlaySelectedRegion (uncurry (||)) overlay)
  valuations <- requireRight "facade region valuations" (regionValuations selectedUnion)
  assertEqual
    "facade valuation digest"
    (1, 6)
    ( eulerCharacteristicValue (valuationEuler valuations)
    , exactAreaValue (valuationArea valuations)
    )

  kernelComponent <- rectangleComponent 0 0 1 1
  kernel <-
    requireRight
      "facade convex kernel"
      (convexPolygon (exactLoopPoints (polygonOuterLoop kernelComponent)))
  element <- requireRight "facade structuring element" (structuringElement kernel)
  (expanded, receipt) <- requireRight "facade polygon offset" (polygonOffset element selectedUnion)
  expectedExpanded <- rectangleRegion 0 0 4 3
  assertEqual "facade morphology result" expectedExpanded expanded
  assertEqual "facade morphology receipt" MinkowskiAddition (minkowskiOperation receipt)

regionUnion :: PlanarRegion -> PlanarRegion -> IO PlanarRegion
regionUnion = combineRegions (uncurry (||))

regionIntersection :: PlanarRegion -> PlanarRegion -> IO PlanarRegion
regionIntersection = combineRegions (uncurry (&&))

regionDifference :: PlanarRegion -> PlanarRegion -> IO PlanarRegion
regionDifference = combineRegions (\(insideLeft, insideRight) -> insideLeft && not insideRight)

combineRegions
  :: ((Bool, Bool) -> Bool)
  -> PlanarRegion
  -> PlanarRegion
  -> IO PlanarRegion
combineRegions selected left right = do
  overlay <- overlayRegions left right
  requireRight "region Boolean publication" (overlaySelectedRegion selected overlay)

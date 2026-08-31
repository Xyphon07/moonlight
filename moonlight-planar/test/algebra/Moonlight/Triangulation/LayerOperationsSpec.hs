-- | Exact affine-envelope and compositional layer laws through the public
-- facade. Every comparison is against an independently composed public view.
module Moonlight.Triangulation.LayerOperationsSpec (tests) where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Moonlight.Triangulation
  ( AffineForm (..)
  , CoverGap
  , ExactPoint
  , ExactRational
  , LayerCoverageError (..)
  , PlanarLayer
  , PlanarRegion
  , PolygonComponent
  , RegionPublicationError (RegionUnboundedSelection)
  , UpperEnvelopeError (UpperEnvelopeEmptyForms)
  , coverGapRegion
  , exactAreaValue
  , exactLoop
  , exactLoopPoints
  , exactPoint
  , exactPointCoordinates
  , layerCovers
  , overlayAll
  , overlayConfusion
  , overlayLayers
  , overlayMass
  , overlaySelectedRegion
  , planarLayer
  , planarLayerLabelAt
  , planarLayerRegions
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  , regionValuations
  , upperEnvelope
  , valuationArea
  )
import Moonlight.Triangulation.AlgebraFixtures
  ( insideLayer
  , rectangleComponent
  , rectangleRegion
  )
import Support (assertEqual, integerPoint, requireRight)

tests :: IO ()
tests = do
  testConvexUpperEnvelope
  testNonconvexAndHoledUpperEnvelope
  testEmptyUpperEnvelope
  testLayerCoverage
  testNaryOverlayLabels
  testOverlayMassAndConfusion
  putStrLn "layer operations: ok"

testConvexUpperEnvelope :: IO ()
testConvexUpperEnvelope = do
  window <- rectangleComponent 0 0 10 10
  let forms =
        Map.fromList
          [ (0 :: Int, AffineForm 0 (-1) 0)
          , (1, AffineForm 0 (-1) 0)
          , (2, AffineForm (-10) 1 0)
          , (3, AffineForm (-11) 1 0)
          ]
  layer <- requireRight "convex exact upper envelope" (upperEnvelope window forms)
  reordered <-
    requireRight
      "upper envelope is independent of map construction order"
      (upperEnvelope window (Map.fromList (reverse (Map.toList forms))))
  assertEqual "upper envelope map-order invariance" layer reordered
  assertEqual "identical and dominated forms own no cell" [Just 0, Just 2] (Map.keys (planarLayerRegions layer))
  assertEnvelopeDominance forms layer
  assertEqual "left affine winner" (Just 0) (planarLayerLabelAt layer (exactPoint 2 5))
  assertEqual "right affine winner" (Just 2) (planarLayerLabelAt layer (exactPoint 8 5))
  assertEqual "outside finite window" Nothing (planarLayerLabelAt layer (exactPoint (-1) 5))
  requireCoverage "upper envelope covers its convex window" layer window

testNonconvexAndHoledUpperEnvelope :: IO ()
testNonconvexAndHoledUpperEnvelope = do
  nonconvex <- lShapedComponent
  holed <- holedComponent
  let constantForm = Map.singleton (7 :: Int) (AffineForm 1 0 0)
  assertEnvelopeEqualsWindow "nonconvex envelope restriction" nonconvex constantForm
  assertEnvelopeEqualsWindow "holed envelope restriction" holed constantForm

testEmptyUpperEnvelope :: IO ()
testEmptyUpperEnvelope = do
  window <- rectangleComponent 0 0 1 1
  case upperEnvelope window (Map.empty :: Map.Map Int AffineForm) of
    Left UpperEnvelopeEmptyForms -> pure ()
    result -> fail ("empty upper envelope: unexpected result " <> show result)

testLayerCoverage :: IO ()
testLayerCoverage = do
  window <- rectangleComponent 0 0 10 10
  half <- rectangleRegion 0 0 5 10
  incomplete <- insideLayer half
  case layerCovers incomplete window of
    Left (LayerCoverageGap gap) -> assertGapArea 50 gap
    result -> fail ("incomplete layer coverage: unexpected result " <> show result)
  completeRegion <- requireRight "complete coverage region" (planarRegion [window])
  complete <- requireRight "complete coverage layer" (planarLayer False (Map.singleton True completeRegion))
  requireCoverage "complete layer" complete window

testNaryOverlayLabels :: IO ()
testNaryOverlayLabels = do
  vertical <- rectangleRegion 0 0 6 10 >>= insideLayer
  horizontal <- rectangleRegion 0 0 10 6 >>= insideLayer
  central <- rectangleRegion 2 2 8 8 >>= insideLayer
  let sourceLayers = vertical :| [horizontal, central]
  refined <- requireRight "balanced n-ary overlay" (overlayAll sourceLayers)
  assertNaryLabel sourceLayers refined (exactPoint 3 3)
  assertNaryLabel sourceLayers refined (exactPoint 7 3)
  assertNaryLabel sourceLayers refined (exactPoint 9 9)
  assertNaryLabel sourceLayers refined (exactPoint (-1) (-1))
  singleton <- requireRight "singleton n-ary overlay" (overlayAll (vertical :| []))
  assertNaryLabel (vertical :| []) singleton (exactPoint 3 3)

testOverlayMassAndConfusion :: IO ()
testOverlayMassAndConfusion = do
  left <- rectangleRegion 0 0 6 10 >>= insideLayer
  right <- rectangleRegion 4 0 10 10 >>= insideLayer
  result <- requireRight "mass overlay" (overlayLayers left right)
  direct <- requireRight "direct overlap mass" (overlayMass (== (True, True)) result)
  published <-
    requireRight
      "published overlap region"
      (overlaySelectedRegion (== (True, True)) result)
  publishedValues <- requireRight "published overlap valuation" (regionValuations published)
  assertEqual
    "direct mass equals published-region valuation"
    (exactAreaValue (valuationArea publishedValues))
    (exactAreaValue direct)
  assertEqual "overlap mass" 20 (exactAreaValue direct)
  case overlayMass (== (False, False)) result of
    Left RegionUnboundedSelection -> pure ()
    value -> fail ("unbounded overlay mass: unexpected result " <> show value)
  assertEqual
    "one-pass finite confusion masses"
    ( Map.fromList
        [ ((False, True), 40)
        , ((True, False), 40)
        , ((True, True), 20)
        ]
    )
    (fmap exactAreaValue (overlayConfusion result))
  traverse_
    (\(labels, expected) -> do
       measured <- requireRight "direct confusion-cell mass" (overlayMass (== labels) result)
       assertEqual "confusion entry equals direct mass" expected (exactAreaValue measured))
    (Map.toAscList (fmap exactAreaValue (overlayConfusion result)))

assertEnvelopeDominance
  :: Map.Map Int AffineForm
  -> PlanarLayer (Maybe Int)
  -> IO ()
assertEnvelopeDominance forms layer =
  traverse_ checkRegion (Map.toAscList (planarLayerRegions layer))
 where
  checkRegion :: (Maybe Int, PlanarRegion) -> IO ()
  checkRegion (Nothing, _) = fail "bounded upper-envelope region has the outside label"
  checkRegion (Just winner, region) =
    case Map.lookup winner forms of
      Nothing -> fail ("upper-envelope winner is absent from its input map: " <> show winner)
      Just winnerForm ->
        traverse_
          (checkWinnerAtPoint winner winnerForm)
          (concatMap componentBoundaryPoints (planarRegionComponents region))

  checkWinnerAtPoint
    :: Int
    -> AffineForm
    -> ExactPoint
    -> IO ()
  checkWinnerAtPoint winner winnerForm point =
    let winnerValue = affineValue winnerForm point
     in traverse_
          (\(candidate, candidateForm) ->
             unless (winnerValue >= affineValue candidateForm point) $
               fail
                 ( "upper-envelope dominance failed: winner "
                     <> show winner
                     <> ", candidate "
                     <> show candidate
                 ))
          (Map.toAscList forms)

componentBoundaryPoints :: PolygonComponent -> [ExactPoint]
componentBoundaryPoints component =
  NonEmpty.toList (exactLoopPoints (polygonOuterLoop component))
    <> concatMap (NonEmpty.toList . exactLoopPoints) (polygonHoleLoops component)

affineValue
  :: AffineForm
  -> ExactPoint
  -> ExactRational
affineValue form point =
  let (coordinateX, coordinateY) = exactPointCoordinates point
   in affineFormConstant form
        + affineFormXCoefficient form * coordinateX
        + affineFormYCoefficient form * coordinateY

assertEnvelopeEqualsWindow
  :: String
  -> PolygonComponent
  -> Map.Map Int AffineForm
  -> IO ()
assertEnvelopeEqualsWindow label window forms = do
  expected <- requireRight (label <> " expected region") (planarRegion [window])
  layer <- requireRight label (upperEnvelope window forms)
  assertEqual label (Just expected) (Map.lookup (Just 7) (planarLayerRegions layer))
  requireCoverage (label <> " coverage") layer window

assertGapArea :: Integer -> CoverGap -> IO ()
assertGapArea expected gap = do
  values <- requireRight "gap valuation" (regionValuations (coverGapRegion gap))
  assertEqual "coverage obstruction retains exact gap" (fromInteger expected) (exactAreaValue (valuationArea values))

assertNaryLabel
  :: NonEmpty (PlanarLayer Bool)
  -> PlanarLayer (NonEmpty Bool)
  -> ExactPoint
  -> IO ()
assertNaryLabel sourceLayers refined point =
  assertEqual
    "n-ary label tuple follows source order"
    (fmap (`planarLayerLabelAt` point) sourceLayers)
    (planarLayerLabelAt refined point)

requireCoverage
  :: (Ord label, Show label)
  => String
  -> PlanarLayer label
  -> PolygonComponent
  -> IO ()
requireCoverage label layer window =
  case layerCovers layer window of
    Right () -> pure ()
    Left failure -> fail (label <> ": " <> show failure)

lShapedComponent :: IO PolygonComponent
lShapedComponent = do
  outer <-
    requireRight
      "L-shaped outer loop"
      ( exactLoop
          ( integerPoint 0 0
              :| [ integerPoint 4 0
                 , integerPoint 4 1
                 , integerPoint 1 1
                 , integerPoint 1 4
                 , integerPoint 0 4
                 ]
          )
      )
  requireRight "L-shaped component" (polygonComponent outer [])

holedComponent :: IO PolygonComponent
holedComponent = do
  outer <-
    requireRight
      "holed outer loop"
      (exactLoop (integerPoint 0 0 :| [integerPoint 6 0, integerPoint 6 6, integerPoint 0 6]))
  hole <-
    requireRight
      "clockwise hole loop"
      (exactLoop (integerPoint 2 2 :| [integerPoint 2 4, integerPoint 4 4, integerPoint 4 2]))
  requireRight "holed component" (polygonComponent outer [hole])

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Timing and allocation evidence for exact affine envelopes and layer
-- composition. Manual lanes use only the pre-existing public operations a
-- caller otherwise has to compose and discard.
module Moonlight.Planar.LayerBench
  ( LayerBenchFixtures
  , prepareFixtures
  , runBenchmarks
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Ratio (numerator, denominator)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Moonlight.Planar.Exact (ExactPoint, exactPoint, exactRational)
import Moonlight.Planar.Internal.ExactRational (ExactRational)
import Moonlight.Planar.Overlay (OverlayResult, layerCovers, overlayAll, overlayConfusion, overlayLayers, overlayMass, overlayPlanarLayer, overlaySelectedRegion, foldBoundedOverlayCells)
import Moonlight.Planar.PowerDiagram (AffineForm (..), upperEnvelope)
import Moonlight.Planar.Region (PlanarLayer, PlanarRegion, PolygonComponent, exactLoop, planarLayer, planarLayerOutsideLabel, planarLayerRegions, planarRegion, planarRegionComponents, polygonComponent)
import Moonlight.Planar.Valuation (ExactArea, exactAreaValue, regionValuations, valuationArea, ExactPlanarMoments, orientedBoundaryMoments, polygonComponentMoments)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)

data LayerBenchFixtures = LayerBenchFixtures
  { envelopeWindow :: !PolygonComponent
  , envelopeForms16 :: !(Map.Map Int AffineForm)
  , envelopeForms64 :: !(Map.Map Int AffineForm)
  , envelopeForms169 :: !(Map.Map Int AffineForm)
  , overlayLayers8 :: !(NonEmpty (PlanarLayer Bool))
  , overlayLayers16 :: !(NonEmpty (PlanarLayer Bool))
  , measuredOverlay :: !(OverlayResult (Bool, Bool))
  , coverageLayer :: !(PlanarLayer Bool)
  , momentBoundaries :: ![(String, [(ExactPoint, ExactPoint)])]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

prepareFixtures :: IO LayerBenchFixtures
prepareFixtures = do
  window <- rectangleComponent (-20) (-20) 20 20
  layers8 <- prepareLayerFamily 8
  layers16 <- prepareLayerFamily 16
  left <- rectangleLayer 0 0 60 100
  right <- rectangleLayer 40 0 100 100
  massOverlay <- requireRight "mass benchmark overlay" (overlayLayers left right)
  windowRegion <- requireRight "coverage benchmark region" (planarRegion [window])
  completeCoverage <-
    requireRight
      "coverage benchmark layer"
      (planarLayer False (Map.singleton True windowRegion))
  boundaries <- traverse (traverse (traverse admitMomentEdge)) rationalMomentBoundaries
  pure
    LayerBenchFixtures
      { envelopeWindow = window
      , envelopeForms16 = affineForms 16
      , envelopeForms64 = affineForms 64
      , envelopeForms169 = affineForms 169
      , overlayLayers8 = layers8
      , overlayLayers16 = layers16
      , measuredOverlay = massOverlay
      , coverageLayer = completeCoverage
      , momentBoundaries = boundaries
      }

runBenchmarks :: LayerBenchFixtures -> IO ()
runBenchmarks fixtures =
  defaultMain
    [ bgroup
        "exact affine upper envelope"
        [ bench "n=16" (nf (upperEnvelope (envelopeWindow fixtures)) (envelopeForms16 fixtures))
        , bench "n=64" (nf (upperEnvelope (envelopeWindow fixtures)) (envelopeForms64 fixtures))
        , bench "n=169" (nf (upperEnvelope (envelopeWindow fixtures)) (envelopeForms169 fixtures))
        ]
    , bgroup
        "n-ary overlay"
        [ bench "fused/publish/n=8" (nf fusedPublishedOverlay (overlayLayers8 fixtures))
        , bench "left-associated/publish/n=8" (nf leftAssociatedOverlay (overlayLayers8 fixtures))
        , bench "fused/publish/n=16" (nf fusedPublishedOverlay (overlayLayers16 fixtures))
        , bench "left-associated/publish/n=16" (nf leftAssociatedOverlay (overlayLayers16 fixtures))
        ]
    , bgroup "oriented edge moments"
        [ bench label (nf orientedBoundaryMoments boundary)
        | (label, boundary) <- momentBoundaries fixtures
        ]
    , bgroup
        "overlay analytics"
        [ bench "mass/direct" (nf directMass (measuredOverlay fixtures))
        , bench "mass/publish-and-value" (nf publishedMass (measuredOverlay fixtures))
        , bench "confusion/direct" (nf directConfusion (measuredOverlay fixtures))
        , bench "confusion/repeated-publication" (nf publishedConfusion (measuredOverlay fixtures))
        , bench "moments/direct" (nf directMoments (measuredOverlay fixtures))
        , bench "moments/publish-and-value" (nf publishedMoments (measuredOverlay fixtures))
        , bench
            "coverage/complete"
            (nf (coverageResult (coverageLayer fixtures)) (envelopeWindow fixtures))
        ]
    ]

fusedPublishedOverlay
  :: NonEmpty (PlanarLayer Bool)
  -> Either String (PlanarLayer (NonEmpty Bool))
fusedPublishedOverlay sources = do
  result <- first show (overlayAll sources)
  first show (overlayPlanarLayer result)

leftAssociatedOverlay
  :: NonEmpty (PlanarLayer Bool)
  -> Either String (PlanarLayer (NonEmpty Bool))
leftAssociatedOverlay (firstLayer :| remainingLayers) = do
  initial <- singletonLabelLayer firstLayer
  liftedRemaining <- traverse singletonLabelLayer remainingLayers
  foldM overlayNext initial liftedRemaining
 where
  overlayNext left right = do
    result <- first show (overlayLayers left right)
    published <- first show (overlayPlanarLayer result)
    flattenPublicLayer published

singletonLabelLayer
  :: PlanarLayer Bool
  -> Either String (PlanarLayer (NonEmpty Bool))
singletonLabelLayer layer =
  first show
    ( planarLayer
        (planarLayerOutsideLabel layer :| [])
        ( Map.fromList
            [ (label :| [], region)
            | (label, region) <- Map.toAscList (planarLayerRegions layer)
            ]
        )
    )

flattenPublicLayer
  :: PlanarLayer (NonEmpty Bool, NonEmpty Bool)
  -> Either String (PlanarLayer (NonEmpty Bool))
flattenPublicLayer layer =
  first show
    ( planarLayer
        (uncurry (<>) (planarLayerOutsideLabel layer))
        ( Map.fromList
            [ (leftLabels <> rightLabels, region)
            | ((leftLabels, rightLabels), region) <- Map.toAscList (planarLayerRegions layer)
            ]
        )
    )

directMass :: OverlayResult (Bool, Bool) -> Either String ExactArea
directMass = first show . overlayMass (== (True, True))

publishedMass :: OverlayResult (Bool, Bool) -> Either String ExactArea
publishedMass result = do
  region <- first show (overlaySelectedRegion (== (True, True)) result)
  valuationArea <$> first show (regionValuations region)

directConfusion :: OverlayResult (Bool, Bool) -> Either String (Map.Map (Bool, Bool) ExactRational)
directConfusion = fmap (fmap exactAreaValue) . first show . overlayConfusion

publishedConfusion
  :: OverlayResult (Bool, Bool)
  -> Either String (Map.Map (Bool, Bool) ExactRational)
publishedConfusion result =
  Map.fromList
    <$> traverse
      (\labels -> (labels,) . exactAreaValue <$> publishedPairMass labels)
      [(False, True), (True, False), (True, True)]
 where
  publishedPairMass labels = do
    region <- first show (overlaySelectedRegion (== labels) result)
    valuationArea <$> first show (regionValuations region)

directMoments :: OverlayResult (Bool, Bool) -> Either String ExactPlanarMoments
directMoments = first show . foldBoundedOverlayCells
  (\moments labels boundary -> if labels == (True, True) then moments <> orientedBoundaryMoments boundary else moments)
  mempty

publishedMoments :: OverlayResult (Bool, Bool) -> Either String ExactPlanarMoments
publishedMoments result = do
  region <- first show (overlaySelectedRegion (== (True, True)) result)
  pure (foldMap polygonComponentMoments (planarRegionComponents region))

coverageResult
  :: PlanarLayer Bool
  -> PolygonComponent
  -> Either String ()
coverageResult layer = first show . layerCovers layer

prepareLayerFamily :: Int -> IO (NonEmpty (PlanarLayer Bool))
prepareLayerFamily count = do
  layers <- traverse prepareLayer [0 .. count - 1]
  case layers of
    firstLayer : remaining -> pure (firstLayer :| remaining)
    [] -> fail "layer benchmark requires a positive family size"
 where
  prepareLayer :: Int -> IO (PlanarLayer Bool)
  prepareLayer index =
    rectangleLayer
      (-64 + fromIntegral ((7 * index) `mod` 31))
      (-64 + fromIntegral ((11 * index) `mod` 29))
      (32 + fromIntegral ((13 * index) `mod` 31))
      (32 + fromIntegral ((17 * index) `mod` 29))

rectangleLayer :: Integer -> Integer -> Integer -> Integer -> IO (PlanarLayer Bool)
rectangleLayer minimumX minimumY maximumX maximumY = do
  region <- rectangleRegion minimumX minimumY maximumX maximumY
  requireRight "benchmark rectangle layer" (planarLayer False (Map.singleton True region))

rectangleRegion :: Integer -> Integer -> Integer -> Integer -> IO PlanarRegion
rectangleRegion minimumX minimumY maximumX maximumY = do
  component <- rectangleComponent minimumX minimumY maximumX maximumY
  requireRight "benchmark rectangle region" (planarRegion [component])

rectangleComponent :: Integer -> Integer -> Integer -> Integer -> IO PolygonComponent
rectangleComponent minimumX minimumY maximumX maximumY = do
  loop <-
    requireRight
      "benchmark rectangle loop"
      ( exactLoop
          ( exactPoint (fromInteger minimumX) (fromInteger minimumY)
              :| [ exactPoint (fromInteger maximumX) (fromInteger minimumY)
                 , exactPoint (fromInteger maximumX) (fromInteger maximumY)
                 , exactPoint (fromInteger minimumX) (fromInteger maximumY)
                 ]
          )
      )
  requireRight "benchmark rectangle component" (polygonComponent loop [])

affineForms :: Int -> Map.Map Int AffineForm
affineForms count =
  Map.fromAscList
    [ (index, tangentForm index)
    | index <- [0 .. count - 1]
    ]
 where
  tangentForm :: Int -> AffineForm
  tangentForm index =
    let coordinateX = fromIntegral (index `mod` 17 - 8)
        coordinateY = fromIntegral (index `div` 17 - 5)
     in AffineForm
          (negate (coordinateX * coordinateX + coordinateY * coordinateY))
          (2 * coordinateX)
          (2 * coordinateY)

requireRight :: Show failure => String -> Either failure value -> IO value
requireRight label = either (fail . ((label <> ": ") <>) . show) pure

-- Identical fixture covers are prepared before both copied benchmark binaries.
-- These lanes time the existing public six-moment kernel, not input admission.
-- The independent Rational formula oracle lives in the existing ValuationSpec.
rationalMomentBoundaries :: [(String, [((Rational, Rational), (Rational, Rational))])]
rationalMomentBoundaries =
  [ ("integer/n=64", [((n, 2*n+1), (n+3, 1-n)) | n <- [-32 .. 31]])
  , ("common-denominator/n=64", [((n/64, (2*n+1)/64), ((n+3)/64, (1-n)/64)) | n <- [-32 .. 31]])
  , ("mixed-coprime/n=64", [((n/101, (2*n+1)/103), ((n+3)/107, (1-n)/109)) | n <- [-32 .. 31]])
  , ("high-bitwidth/n=64", [(((huge+n)/p, (huge-n)/q), ((huge+2*n+1)/r, (n-huge)/s)) | n <- [-32 .. 31]])
  , ("zero-cross/n=64", [((n/101, 2*n/101), ((n+3)/107, 2*(n+3)/107)) | n <- [-32 .. 31]])
  ]
 where
  huge = fromInteger (2 ^ (191 :: Int))
  p = fromInteger (2 ^ (127 :: Int) - 1)
  q = fromInteger (2 ^ (107 :: Int) - 1)
  r = fromInteger (2 ^ (89 :: Int) - 1)
  s = fromInteger (2 ^ (61 :: Int) - 1)

admitMomentEdge :: ((Rational, Rational), (Rational, Rational)) -> IO (ExactPoint, ExactPoint)
admitMomentEdge (from, to) = (,) <$> admitMomentPoint from <*> admitMomentPoint to

admitMomentPoint :: (Rational, Rational) -> IO ExactPoint
admitMomentPoint (x, y) =
  exactPoint <$> admit x <*> admit y
 where
  admit :: Rational -> IO ExactRational
  admit value = requireRight "moment benchmark coordinate"
    (exactRational (numerator value) (denominator value))

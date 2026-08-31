{-# LANGUAGE NumericUnderscores #-}

-- | Exact segment-event, overlay, and grouped-region publication receipts.
-- The source families are closed data; measurement is the only effect.
module Moonlight.Triangulation.RegionBench (benchmarks) where

import BenchSupport
  ( latticeFaceBand
  , latticePoints
  )
import BenchMeasure (requireRight, timedValue)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty (..))
import Moonlight.Triangulation
  ( buildTriangulation
  , delaunay
  , unitElementDefaults
  )
import Moonlight.Triangulation.CellSet
  ( exactCellSet
  , exactCellSetEdgeCount
  , exactCellSetFaceCount
  , exactCellSetVertexCount
  )
import Moonlight.Triangulation.Dcel (numInnerFaces)
import Moonlight.Triangulation.Exact (ExactPoint, exactPoint)
import Moonlight.Triangulation.Internal.HandleDefs (VertexId (..))
import Moonlight.Triangulation.Overlay
  ( OverlayReceipt (..)
  , overlayClosedIntersection
  , overlayLayers
  , overlayReceipt
  )
import Moonlight.Triangulation.Minkowski
  ( MinkowskiReceipt
  , convexMinkowskiSum
  , convexPolygon
  , erodeBy
  , minkowskiExactCoordinateBitGrowth
  , minkowskiExactCrossings
  , minkowskiGeneratedConvolutionEdges
  , minkowskiGeneratedPieces
  , minkowskiOutputCells
  , minkowskiOverlayPasses
  , minkowskiSum
  , structuringElement
  )
import Moonlight.Triangulation.Region
  ( PlanarLayer
  , PolygonComponent
  , RegionValidationError
  , exactLoop
  , exactLoopPoints
  , labelledPlanarLayer
  , planarLayer
  , planarLayerRegions
  , planarRegion
  , planarRegionComponents
  , polygonComponent
  , polygonHoleLoops
  , polygonOuterLoop
  )
import Moonlight.Triangulation.Valuation
  ( cellValuations
  , eulerCharacteristicValue
  , exactLengthTerms
  , regionValuations
  , valuationEuler
  , valuationIntrinsic1
  , exactLengthExpression
  )

data OverlayFamily
  = DisjointFamily
  | GridCrossingFamily
  | CollinearOverlapFamily
  deriving stock (Eq, Ord, Show)

benchmarks :: IO ()
benchmarks = do
  traverse_
    (\family -> traverse_ (benchmarkOverlayFamily family) [2, 4, 8])
    [DisjointFamily, GridCrossingFamily, CollinearOverlapFamily]
  benchmarkPublicationReceipt
  traverse_ benchmarkRegionAuthoring [64, 256, 1_024]
  traverse_ benchmarkRegionValuation [64, 256, 1_024]
  traverse_ benchmarkConvexMinkowski [8, 32, 128, 512]
  traverse_
    (\family -> traverse_ (benchmarkOverlaySelectorFamily family) [2, 4, 8])
    [DisjointFamily, GridCrossingFamily, CollinearOverlapFamily]
  benchmarkGeneralMorphology

benchmarkOverlayFamily :: OverlayFamily -> Int -> IO ()
benchmarkOverlayFamily family size = do
  layers <- requireRight (familyLayers family size)
  result <-
    timedValue
      (familyName family <> "-n" <> show size)
      (requireRight (uncurry overlayLayers layers))
  let receipt = overlayReceipt result
      inputAndOutput = overlayInputSegments receipt + overlayRelationEvents receipt
      logarithmicScale = max 1 (ceilingLog2 (overlayInputSegments receipt + 1))
      totalLimit = 128 * inputAndOutput * logarithmicScale
  putStrLn
    ( familyName family
        <> "-receipt: n="
        <> show size
        <> " source-segments="
        <> show (overlayInputSegments receipt)
        <> " relation-events-k="
        <> show (overlayRelationEvents receipt)
        <> " exact-relation-checks="
        <> show (overlayTotalRelationChecks receipt)
        <> " atomic-edges="
        <> show (overlayAtomicEdges receipt)
        <> " arrangement-cells="
        <> show (overlayArrangementCells receipt)
        <> " resident-faces="
        <> show (overlayResidentFaces receipt)
        <> " avl-height="
        <> show (overlaySweepMaximumHeight receipt)
    )
  if overlayTotalRelationChecks receipt <= totalLimit
    then pure ()
    else
      fail
        ( familyName family
            <> " retained superlinear total relation work: "
            <> show (overlayTotalRelationChecks receipt, totalLimit)
        )

benchmarkOverlaySelectorFamily :: OverlayFamily -> Int -> IO ()
benchmarkOverlaySelectorFamily family size = do
  layers <- requireRight (familyLayers family size)
  result <- requireRight (uncurry overlayLayers layers)
  selectedReceipt <-
    timedValue
      (familyName family <> "-selector-n" <> show size)
      (do
         selected <-
           requireRight
             (overlayClosedIntersection (== 1) (== 1) result)
         valuations <- requireRight (cellValuations selected)
         pure
           ( exactCellSetVertexCount selected
           , exactCellSetEdgeCount selected
           , exactCellSetFaceCount selected
           , eulerCharacteristicValue (valuationEuler valuations)
           , length
               ( exactLengthTerms
                   (exactLengthExpression (valuationIntrinsic1 valuations))
               )
           ))
  putStrLn
    ( familyName family
        <> "-selector-receipt: n="
        <> show size
        <> " cells="
        <> show selectedReceipt
    )

familyName :: OverlayFamily -> String
familyName DisjointFamily = "overlay-disjoint"
familyName GridCrossingFamily = "overlay-grid-crossing"
familyName CollinearOverlapFamily = "overlay-collinear-overlap"

benchmarkRegionAuthoring :: Int -> IO ()
benchmarkRegionAuthoring size = do
  componentCount <-
    timedValue
      ("planar-region-disjoint-authoring-n" <> show size)
      (requireRight $ do
         components <-
           traverse
             rectangleComponent
             (disjointRectangles size)
         length . planarRegionComponents <$> planarRegion components)
  if componentCount == size
    then
      putStrLn
        ( "planar-region-disjoint-authoring-receipt: n="
            <> show size
            <> " components="
            <> show componentCount
        )
    else fail ("planar region authoring lost components: " <> show componentCount)

benchmarkRegionValuation :: Int -> IO ()
benchmarkRegionValuation size = do
  components <-
    requireRight
      ( traverse
          rectangleComponent
          (disjointRectangles size)
      )
  region <- requireRight (planarRegion components)
  valuations <-
    timedValue
      ("planar-region-valuations-n" <> show size)
      (requireRight (regionValuations region))
  let receipt =
        ( eulerCharacteristicValue (valuationEuler valuations)
        , length
            ( exactLengthTerms
                (exactLengthExpression (valuationIntrinsic1 valuations))
            )
        )
  if receipt == (size, 1)
    then
      putStrLn
        ( "planar-region-valuations-receipt: n="
            <> show size
            <> " euler="
            <> show size
            <> " radical-terms=1"
        )
    else fail ("planar region valuation receipt mismatch: " <> show receipt)

benchmarkConvexMinkowski :: Int -> IO ()
benchmarkConvexMinkowski halfSize = do
  left <- requireRight (convexPolygon (convexLens halfSize))
  right <- requireRight (convexPolygon (convexLens halfSize))
  result <-
    timedValue
      ("convex-minkowski-n" <> show (2 * halfSize))
      (evaluate (convexMinkowskiSum left right))
  let outputVertices =
        sum
          [ length (exactLoopPoints (polygonOuterLoop component))
          | component <- planarRegionComponents result
          ]
      inputVertices = 4 * halfSize
  if outputVertices <= inputVertices
    then
      putStrLn
        ( "convex-minkowski-receipt: input-vertices="
            <> show inputVertices
            <> " output-vertices="
            <> show outputVertices
        )
    else fail ("convex Minkowski output exceeded n+m: " <> show (outputVertices, inputVertices))

benchmarkGeneralMorphology :: IO ()
benchmarkGeneralMorphology = do
  concaveLoop <-
    requireRight
      ( exactLoop
          ( integerPoint 0 0
              :| [ integerPoint 6 0
                 , integerPoint 6 2
                 , integerPoint 2 2
                 , integerPoint 2 6
                 , integerPoint 0 6
                 ]
          )
      )
  concaveComponent <- requireRight (polygonComponent concaveLoop [])
  concaveRegion <- requireRight (planarRegion [concaveComponent])
  kernelComponent <- requireRight (rectangleComponent (-1, -1, 1, 1))
  kernelRegion <- requireRight (planarRegion [kernelComponent])
  kernelPolygon <-
    requireRight
      (convexPolygon (exactLoopPoints (polygonOuterLoop kernelComponent)))
  element <- requireRight (structuringElement kernelPolygon)
  (sumComponents, sumReceipt) <-
    timedValue
      "general-minkowski-concave"
      (do
         (result, receipt) <- requireRight (minkowskiSum concaveRegion kernelRegion)
         pure (length (planarRegionComponents result), receipt))
  printMorphologyReceipt "general-minkowski-concave" sumComponents sumReceipt
  erosionSourceComponents <-
    requireRight
      ( traverse
          rectangleComponent
          [(0, 0, 6, 6), (9, 0, 15, 6)]
      )
  erosionSource <- requireRight (planarRegion erosionSourceComponents)
  (erosionComponents, erosionReceipt) <-
    timedValue
      "general-erosion-disconnected"
      (do
         (result, receipt) <- requireRight (erodeBy element erosionSource)
         pure (length (planarRegionComponents result), receipt))
  printMorphologyReceipt
    "general-erosion-disconnected"
    erosionComponents
    erosionReceipt
  if minkowskiOverlayPasses sumReceipt > 0
      && minkowskiGeneratedPieces sumReceipt > 0
      && minkowskiOverlayPasses erosionReceipt > 0
    then pure ()
    else fail "general morphology bypassed its declared decomposition/overlay work"

printMorphologyReceipt :: String -> Int -> MinkowskiReceipt -> IO ()
printMorphologyReceipt label outputComponents receipt =
  putStrLn
    ( label
        <> "-receipt: output-components="
        <> show outputComponents
        <> " generated-pieces="
        <> show (minkowskiGeneratedPieces receipt)
        <> " convolution-edges="
        <> show (minkowskiGeneratedConvolutionEdges receipt)
        <> " overlay-passes="
        <> show (minkowskiOverlayPasses receipt)
        <> " exact-crossings="
        <> show (minkowskiExactCrossings receipt)
        <> " output-cells="
        <> show (minkowskiOutputCells receipt)
        <> " coordinate-bit-growth="
        <> show (minkowskiExactCoordinateBitGrowth receipt)
    )

convexLens :: Int -> NonEmpty ExactPoint
convexLens halfSize =
  let maximumIndex = max 1 (halfSize - 1)
      height = 2 * maximumIndex * maximumIndex + 1
      lower =
        [ integerPoint index (index * index)
        | index <- [1 .. maximumIndex]
        ]
      upper =
        [ integerPoint index (height - index * index)
        | index <- reverse [0 .. maximumIndex]
        ]
   in integerPoint 0 0 :| (lower <> upper)

familyLayers
  :: OverlayFamily
  -> Int
  -> Either RegionValidationError (PlanarLayer Int, PlanarLayer Int)
familyLayers family size =
  case family of
    DisjointFamily ->
      (,)
        <$> layerFromRectangles (disjointRectangles size)
        <*> planarLayer 0 Map.empty
    GridCrossingFamily ->
      (,)
        <$> layerFromRectangles
          [ (3 * index, 0, 3 * index + 1, 3 * size - 1)
          | index <- [0 .. size - 1]
          ]
        <*> layerFromRectangles
          [ (0, 3 * index, 3 * size - 1, 3 * index + 1)
          | index <- [0 .. size - 1]
          ]
    CollinearOverlapFamily ->
      (,)
        <$> layerFromRectangles
          [ (3 * index, 0, 3 * index + 2, 2)
          | index <- [0 .. size - 1]
          ]
        <*> layerFromRectangles
          [ (3 * index + 1, 0, 3 * index + 3, 1)
          | index <- [0 .. size - 1]
          ]

disjointRectangles :: Int -> [(Int, Int, Int, Int)]
disjointRectangles size =
  [ (3 * index, 0, 3 * index + 1, 1)
  | index <- [0 .. size - 1]
  ]
{-# INLINE disjointRectangles #-}

layerFromRectangles
  :: [(Int, Int, Int, Int)]
  -> Either RegionValidationError (PlanarLayer Int)
layerFromRectangles rectangles = do
  components <- traverse rectangleComponent rectangles
  region <- planarRegion components
  planarLayer 0 (Map.singleton 1 region)

rectangleComponent
  :: (Int, Int, Int, Int)
  -> Either RegionValidationError PolygonComponent
rectangleComponent (minimumX, minimumY, maximumX, maximumY) = do
  loop <-
    exactLoop
      ( integerPoint minimumX minimumY
          :| [ integerPoint maximumX minimumY
             , integerPoint maximumX maximumY
             , integerPoint minimumX maximumY
             ]
      )
  polygonComponent loop []

integerPoint :: Int -> Int -> ExactPoint
integerPoint x y =
  exactPoint
    (fromIntegral x)
    (fromIntegral y)

benchmarkPublicationReceipt :: IO ()
benchmarkPublicationReceipt = do
  built <-
    requireRight (delaunay unitElementDefaults (latticePoints 440 272))
  triangulation <- evaluate (force (buildTriangulation built))
  published <-
    timedValue
      "labelled-planar-layer-publication"
      (requireRight (labelledPlanarLayer (-1) triangulation (latticeFaceBand triangulation)))
  let components =
        concatMap planarRegionComponents (Map.elems (planarLayerRegions published))
      holeCount = sum (map (length . polygonHoleLoops) components)
      exactCoordinateCount =
        sum
          [ length (exactLoopPoints (polygonOuterLoop component))
              + sum (map (length . exactLoopPoints) (polygonHoleLoops component))
          | component <- components
          ]
      receipt =
        ( numInnerFaces triangulation
        , length components
        , length components
        , holeCount
        , exactCoordinateCount
        )
  if receipt == (239_360, 22, 22, 0, 88)
    then
      putStrLn
        "labelled-planar-layer-publication-receipt: faces=239360 components=22 outer-loops=22 holes=0 exact-coordinates=88"
    else fail ("labelled planar layer receipt mismatch: " <> show receipt)
  sparseReceipt <-
    timedValue
      "exact-cell-set-sparse-selection"
      (do
         selected <- requireRight (exactCellSet triangulation [VertexId 0] [] [])
         pure
           ( exactCellSetVertexCount selected
           , exactCellSetEdgeCount selected
           , exactCellSetFaceCount selected
           ))
  if sparseReceipt == (1, 0, 0)
    then putStrLn "exact-cell-set-sparse-selection-receipt: vertices=1 edges=0 faces=0"
    else fail ("sparse exact cell selection receipt mismatch: " <> show sparseReceipt)

ceilingLog2 :: Int -> Int
ceilingLog2 target = length (takeWhile (< target) (iterate (* 2) 1))

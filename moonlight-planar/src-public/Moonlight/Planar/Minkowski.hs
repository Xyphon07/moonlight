-- | Exact polygonal Minkowski addition and regularized two-dimensional
-- morphology. Convex convolution is direct; nonconvex construction descends
-- through exact vertical slabs, the exact overlay, and grouped publication
-- owners. Lower-dimensional erosion residuals cannot inhabit 'PlanarRegion'
-- and therefore publish as empty rather than being forged as polygons.
module Moonlight.Planar.Minkowski
  ( StructuringElement
  , structuringElement
  , MinkowskiOperation (..)
  , MinkowskiError (..)
  , MinkowskiReceipt (..)
  , convexMinkowskiSum
  , minkowskiSum
  , erodeBy
  , openWith
  , closeWith
  , polygonOffset
  ) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Bifunctor (first)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Planar.Convex
  ( ConvexPolygon
  , convexPolygonFromLoop
  , convexHullPolygon
  , convexPolygonCentroid
  , convexPolygonPoints
  , convexPolygonRegion
  , reflectConvexPolygon
  , decomposeRegionIntoConvexSlabs
  )
import Moonlight.Planar.Exact
  ( ExactPoint
  , exactClipExactIntersections
  , exactPointCoordinates
  )
import Moonlight.Planar.Internal.HandleDefs
  ( FaceId (..)
  , vertexIdIndex
  )
import Moonlight.Planar.Internal.ExactRational
  ( exactRationalBitWidth
  )
import Moonlight.Planar.Internal.Minkowski.Operations
  ( addExactPoints
  , convexMinkowskiPolygon
  , convexMinkowskiSum
  , erodeConvexBy
  , structuringElement
  , structuringElementPolygon
  )
import Moonlight.Planar.Internal.Minkowski.Types
import Moonlight.Planar.Internal.Overlay.Arrangement (overlayFaceInteriorPoint)
import Moonlight.Planar.Internal.Overlay.Types
  ( OverlayResult (..)
  , OverlayVertex (..)
  )
import Moonlight.Planar.Overlay
  ( OverlayReceipt (..)
  , overlayAll
  , overlayLayers
  , overlayReceipt
  , overlaySelectedRegion
  )
import Moonlight.Planar.Region
  ( PlanarLayer
  , PlanarRegion
  , PolygonComponent
  , RegionPublicationError (..)
  , RegionPointLocation (..)
  , emptyPlanarRegion
  , exactLoopPoints
  , planarRegionComponents
  , polygonHoleLoops
  , polygonOuterLoop
  , regionPointLocation
  , regionBoundaryEdges
  )
import Moonlight.Planar.Internal.Region.Publication
  ( planarRegionFromSelectedIncidence
  , planarLayerFromAdmittedComponents
  )

data MorphologyMetrics = MorphologyMetrics
  { metricOverlayPasses :: !Int
  , metricExactCrossings :: !Int
  , metricOutputCells :: !(Maybe Int)
  }

emptyMetrics :: MorphologyMetrics
emptyMetrics = MorphologyMetrics 0 0 Nothing

appendMetrics :: MorphologyMetrics -> MorphologyMetrics -> MorphologyMetrics
appendMetrics left right =
  MorphologyMetrics
    { metricOverlayPasses = metricOverlayPasses left + metricOverlayPasses right
    , metricExactCrossings = metricExactCrossings left + metricExactCrossings right
    , metricOutputCells = metricOutputCells right <|> metricOutputCells left
    }

minkowskiSum
  :: PlanarRegion
  -> PlanarRegion
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
minkowskiSum left right = do
  (leftPieces, leftMetrics) <- decomposeRegion left
  (rightPieces, rightMetrics) <- decomposeRegion right
  let generated =
        [ convexMinkowskiPolygon leftPiece rightPiece
        | leftPiece <- leftPieces
        , rightPiece <- rightPieces
        ]
      generatedRegions = map convexPolygonRegion generated
      convolutionEdges = sum (map (NonEmpty.length . convexPolygonPoints) generated)
  (result, unionMetrics) <- unionRegions generatedRegions
  let metrics = leftMetrics `appendMetrics` rightMetrics `appendMetrics` unionMetrics
  pure
    ( result
    , MinkowskiReceipt
        { minkowskiOperation = MinkowskiAddition
        , minkowskiInputComponents =
            length (planarRegionComponents left)
              + length (planarRegionComponents right)
        , minkowskiConvexPieces = length leftPieces + length rightPieces
        , minkowskiGeneratedPieces = length generated
        , minkowskiGeneratedConvolutionEdges = convolutionEdges
        , minkowskiOverlayPasses = metricOverlayPasses metrics
        , minkowskiExactCrossings = metricExactCrossings metrics
        , minkowskiOutputCells = fromMaybe 0 (metricOutputCells metrics)
        , minkowskiExactCoordinateBitGrowth =
            coordinateBitGrowth [left, right] result
        }
    )

-- | Erode a polygonal region by an origin-anchored convex kernel and publish
-- the regularized full-dimensional result. A residual consisting only of
-- points or segments is represented by 'emptyPlanarRegion'.
erodeBy
  :: StructuringElement
  -> PlanarRegion
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
erodeBy element source =
  case singleConvexRegion source of
    Just sourcePolygon -> convexErosion element source sourcePolygon
    Nothing -> generalErosion element source

openWith
  :: StructuringElement
  -> PlanarRegion
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
openWith element source = do
  (eroded, erosionReceipt) <- erodeBy element source
  (opened, additionReceipt) <- polygonOffset element eroded
  pure
    ( opened
    , composeReceipts
        MinkowskiOpening
        (length (planarRegionComponents source))
        erosionReceipt
        additionReceipt
    )

closeWith
  :: StructuringElement
  -> PlanarRegion
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
closeWith element source = do
  (expanded, additionReceipt) <- polygonOffset element source
  (closed, erosionReceipt) <- erodeBy element expanded
  pure
    ( closed
    , composeReceipts
        MinkowskiClosing
        (length (planarRegionComponents source))
        additionReceipt
        erosionReceipt
    )

polygonOffset
  :: StructuringElement
  -> PlanarRegion
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
polygonOffset element source =
  minkowskiSum source (convexPolygonRegion (structuringElementPolygon element))

decomposeRegion
  :: PlanarRegion
  -> Either MinkowskiError ([ConvexPolygon], MorphologyMetrics)
decomposeRegion region =
  case traverse convexComponent (planarRegionComponents region) of
    Just convexPieces -> Right (convexPieces, emptyMetrics)
    Nothing -> (\pieces -> (pieces, emptyMetrics)) <$> first MinkowskiConvexFailed (decomposeRegionIntoConvexSlabs region)

convexComponent :: PolygonComponent -> Maybe ConvexPolygon
convexComponent component =
  case polygonHoleLoops component of
    [] -> convexPolygonFromLoop (polygonOuterLoop component)
    _ -> Nothing

singleConvexRegion :: PlanarRegion -> Maybe ConvexPolygon
singleConvexRegion region =
  case planarRegionComponents region of
    [component] -> convexComponent component
    _ -> Nothing

unionRegions
  :: [PlanarRegion]
  -> Either MinkowskiError (PlanarRegion, MorphologyMetrics)
unionRegions [] = Right (emptyPlanarRegion, emptyMetrics)
unionRegions [region] =
  Right
    ( region
    , emptyMetrics{metricOutputCells = Just (length (planarRegionComponents region))}
    )
unionRegions (initial : remaining) = do
  result <- first MinkowskiOverlayFailed
    (overlayAll (fmap morphologyLayer (initial :| remaining)))
  published <- first MinkowskiPublicationFailed (overlaySelectedRegion or result)
  let selectedCells = V.ifoldl'
        (\count index labels -> if index > 0 && or labels then count + 1 else count)
        0 (overlayResultLabels result)
  pure (published, metricsFromOverlay result selectedCells)

convexErosion
  :: StructuringElement
  -> PlanarRegion
  -> ConvexPolygon
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
convexErosion element source sourcePolygon = do
  (eroded, clipReceipt) <-
    erodeConvexBy sourcePolygon (structuringElementPolygon element)
  let result = maybe emptyPlanarRegion convexPolygonRegion eroded
      outputCells = maybe 0 (const 1) eroded
      generatedEdges = maybe 0 (NonEmpty.length . convexPolygonPoints) eroded
  pure
    ( result
    , MinkowskiReceipt
        { minkowskiOperation = MinkowskiErosion
        , minkowskiInputComponents = 1
        , minkowskiConvexPieces = 2
        , minkowskiGeneratedPieces = outputCells
        , minkowskiGeneratedConvolutionEdges = generatedEdges
        , minkowskiOverlayPasses = 0
        , minkowskiExactCrossings =
            exactClipExactIntersections clipReceipt
        , minkowskiOutputCells = outputCells
        , minkowskiExactCoordinateBitGrowth =
            coordinateBitGrowth
              [ source
              , convexPolygonRegion (structuringElementPolygon element)
              ]
              result
        }
    )

generalErosion
  :: StructuringElement
  -> PlanarRegion
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
generalErosion element source
  | null sourceEdges = Right (emptyPlanarRegion, emptyErosionReceipt)
  | otherwise = do
      sweptPolygons <- traverse (sweepBoundaryEdge reflectedKernel) sourceEdges
      let sweptRegions = map convexPolygonRegion sweptPolygons
          generatedEdges =
            sum (map (NonEmpty.length . convexPolygonPoints) sweptPolygons)
      (contactRegion, unionMetrics) <- unionRegions sweptRegions
      candidateOverlay <-
        first MinkowskiOverlayFailed
          (overlayLayers (morphologyLayer contactRegion) emptyMorphologyLayer)
      kernelWitness <- first MinkowskiConvexFailed (convexPolygonCentroid kernel)
      selectedCellIds <-
        Set.fromList
          <$> filterM
            ( classifyCandidateCell
                source
                kernelWitness
                candidateOverlay
            )
            (boundedOutsideCellIds candidateOverlay)
      published <- publishCellSelection selectedCellIds candidateOverlay
      let candidateMetrics =
            metricsFromOverlay candidateOverlay (Set.size selectedCellIds)
          metrics = unionMetrics `appendMetrics` candidateMetrics
      pure
        ( published
        , MinkowskiReceipt
            { minkowskiOperation = MinkowskiErosion
            , minkowskiInputComponents = length (planarRegionComponents source)
            , minkowskiConvexPieces = 1
            , minkowskiGeneratedPieces = length sweptPolygons
            , minkowskiGeneratedConvolutionEdges = generatedEdges
            , minkowskiOverlayPasses = metricOverlayPasses metrics
            , minkowskiExactCrossings = metricExactCrossings metrics
            , minkowskiOutputCells = Set.size selectedCellIds
            , minkowskiExactCoordinateBitGrowth =
                coordinateBitGrowth
                  [source, convexPolygonRegion kernel]
                  published
            }
        )
 where
  kernel = structuringElementPolygon element
  reflectedKernel = reflectConvexPolygon kernel
  sourceEdges = regionBoundaryEdges source
  emptyErosionReceipt =
    MinkowskiReceipt
      { minkowskiOperation = MinkowskiErosion
      , minkowskiInputComponents = 0
      , minkowskiConvexPieces = 1
      , minkowskiGeneratedPieces = 0
      , minkowskiGeneratedConvolutionEdges = 0
      , minkowskiOverlayPasses = 0
      , minkowskiExactCrossings = 0
      , minkowskiOutputCells = 0
      , minkowskiExactCoordinateBitGrowth = 0
      }

sweepBoundaryEdge
  :: ConvexPolygon
  -> (ExactPoint, ExactPoint)
  -> Either MinkowskiError ConvexPolygon
sweepBoundaryEdge reflectedKernel (from, to) =
  case convexPolygonPoints reflectedKernel of
    firstKernelPoint :| remainingKernelPoints ->
      first MinkowskiConvexFailed $ convexHullPolygon
        ( addExactPoints from firstKernelPoint
            :| ( map (addExactPoints from) remainingKernelPoints
                   <> map (addExactPoints to) kernelPoints
               )
        )
 where
  kernelPoints = NonEmpty.toList (convexPolygonPoints reflectedKernel)

boundedOutsideCellIds
  :: OverlayResult (Bool, Bool)
  -> [FaceId]
boundedOutsideCellIds result =
  V.ifoldr
    (\index (leftLabel, rightLabel) selected ->
       if index > 0 && not leftLabel && not rightLabel
         then FaceId (fromIntegral index) : selected
         else selected)
    []
    (overlayResultLabels result)

classifyCandidateCell
  :: PlanarRegion
  -> ExactPoint
  -> OverlayResult (Bool, Bool)
  -> FaceId
  -> Either MinkowskiError Bool
classifyCandidateCell source kernelWitness result cellId = do
  candidate <- first MinkowskiOverlayFailed (overlayFaceInteriorPoint result cellId)
  let inclusionWitness = addExactPoints candidate kernelWitness
  case regionPointLocation source inclusionWitness of
    RegionInterior -> Right True
    RegionExterior -> Right False
    RegionOnBoundary -> Left (MinkowskiInclusionAmbiguous cellId inclusionWitness)

publishCellSelection
  :: Set.Set FaceId
  -> OverlayResult labels
  -> Either MinkowskiError PlanarRegion
publishCellSelection selected result =
  first MinkowskiPublicationFailed
    (planarRegionFromSelectedIncidence (overlayResultIncidence result) exactPointAt (`Set.member` selected))
 where
  exactPointAt vertex = maybe
    (Left (RegionCoordinateMissing vertex))
    (Right . overlayExactPoint)
    (overlayResultVertices result V.!? vertexIdIndex vertex)

morphologyLayer
  :: PlanarRegion
  -> PlanarLayer Bool
morphologyLayer region =
  planarLayerFromAdmittedComponents
    False
    [(True, component) | component <- planarRegionComponents region]

emptyMorphologyLayer :: PlanarLayer Bool
emptyMorphologyLayer = morphologyLayer emptyPlanarRegion

metricsFromOverlay
  :: OverlayResult labels
  -> Int
  -> MorphologyMetrics
metricsFromOverlay result outputCells =
  MorphologyMetrics
    { metricOverlayPasses = 1
    , metricExactCrossings = overlayExactCrossings (overlayReceipt result)
    , metricOutputCells = Just outputCells
    }

composeReceipts
  :: MinkowskiOperation
  -> Int
  -> MinkowskiReceipt
  -> MinkowskiReceipt
  -> MinkowskiReceipt
composeReceipts operation inputComponents firstReceipt secondReceipt =
  MinkowskiReceipt
    { minkowskiOperation = operation
    , minkowskiInputComponents = inputComponents
    , minkowskiConvexPieces =
        minkowskiConvexPieces firstReceipt
          + minkowskiConvexPieces secondReceipt
    , minkowskiGeneratedPieces =
        minkowskiGeneratedPieces firstReceipt
          + minkowskiGeneratedPieces secondReceipt
    , minkowskiGeneratedConvolutionEdges =
        minkowskiGeneratedConvolutionEdges firstReceipt
          + minkowskiGeneratedConvolutionEdges secondReceipt
    , minkowskiOverlayPasses =
        minkowskiOverlayPasses firstReceipt
          + minkowskiOverlayPasses secondReceipt
    , minkowskiExactCrossings =
        minkowskiExactCrossings firstReceipt
          + minkowskiExactCrossings secondReceipt
    , minkowskiOutputCells = minkowskiOutputCells secondReceipt
    , minkowskiExactCoordinateBitGrowth =
        max
          (minkowskiExactCoordinateBitGrowth firstReceipt)
          (minkowskiExactCoordinateBitGrowth secondReceipt)
    }

coordinateBitGrowth :: [PlanarRegion] -> PlanarRegion -> Int
coordinateBitGrowth inputs output =
  max 0
    ( regionCoordinateBits output
        - List.foldl' (\maximumBits -> max maximumBits . regionCoordinateBits) 0 inputs
    )

regionCoordinateBits :: PlanarRegion -> Int
regionCoordinateBits = List.foldl' componentBits 0 . planarRegionComponents
 where
  componentBits maximumBits component =
    List.foldl'
      loopBits
      maximumBits
      (polygonOuterLoop component : polygonHoleLoops component)
  loopBits maximumBits =
    List.foldl' pointBits maximumBits . exactLoopPoints
  pointBits maximumBits point =
    let (x, y) = exactPointCoordinates point
     in max
          maximumBits
          (max (exactRationalBitWidth x) (exactRationalBitWidth y))

{-# LANGUAGE DataKinds #-}

-- | Exact polygonal Minkowski addition and regularized two-dimensional
-- morphology. Convex convolution is direct; nonconvex construction descends
-- through the existing exact overlay, resident CDT, and grouped publication
-- owners. Lower-dimensional erosion residuals cannot inhabit 'PlanarRegion'
-- and therefore publish as empty rather than being forged as polygons.
module Moonlight.Triangulation.Minkowski
  ( ConvexPolygon
  , convexPolygon
  , convexPolygonPoints
  , StructuringElement
  , structuringElement
  , MinkowskiOperation (..)
  , MinkowskiError (..)
  , MinkowskiReceipt (..)
  , convexMinkowskiSum
  , reflectConvexPolygon
  , minkowskiSum
  , erodeBy
  , openWith
  , closeWith
  , polygonOffset
  , polygonInset
  ) where

import Control.Applicative ((<|>))
import Control.Monad (filterM)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Triangulation.Dcel
  ( faceData
  , faceVertices
  , vertexData
  )
import Moonlight.Triangulation.Exact
  ( ExactPoint
  , exactClipExactIntersections
  , exactPointCoordinates
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators (innerFaces)
import Moonlight.Triangulation.Internal.ExactRational
  ( exactRationalBitWidth
  )
import Moonlight.Triangulation.Internal.BoundaryCycle (cyclePairs)
import Moonlight.Triangulation.Internal.Minkowski.Convex
  ( addExactPoints
  , admittedConvexLoop
  , convexHullPolygon
  , convexMinkowskiPolygon
  , convexMinkowskiSum
  , convexPolygon
  , convexPolygonCentroid
  , convexPolygonPoints
  , convexPolygonRegion
  , erodeConvexBy
  , reflectConvexPolygon
  , structuringElement
  , structuringElementPolygon
  )
import Moonlight.Triangulation.Internal.Minkowski.Types
import Moonlight.Triangulation.Internal.Overlay.Resident
  ( faceCarriesExactArea
  , faceLabels
  )
import Moonlight.Triangulation.Internal.Overlay.Types
  ( OverlayCell (..)
  , OverlayCellGeometry (..)
  , OverlayCellId (..)
  , OverlayFace (..)
  , OverlayResult (..)
  , OverlayVertex (..)
  )
import Moonlight.Triangulation.Overlay
  ( OverlayReceipt (..)
  , overlayLayers
  , overlayReceipt
  , overlaySelectedRegion
  )
import Moonlight.Triangulation.Region
  ( PlanarLayer
  , PlanarRegion
  , PolygonComponent
  , RegionPointLocation (..)
  , emptyPlanarRegion
  , exactLoopPoints
  , planarLayerRegions
  , planarRegionComponents
  , polygonHoleLoops
  , polygonOuterLoop
  , regionPointLocation
  )
import Moonlight.Triangulation.Internal.Region.Publication
  ( labelledPlanarLayerFromExactCoordinates
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

polygonInset
  :: StructuringElement
  -> PlanarRegion
  -> Either MinkowskiError (PlanarRegion, MinkowskiReceipt)
polygonInset = erodeBy

decomposeRegion
  :: PlanarRegion
  -> Either MinkowskiError ([ConvexPolygon], MorphologyMetrics)
decomposeRegion region =
  case traverse convexComponent (planarRegionComponents region) of
    Just convexPieces -> Right (convexPieces, emptyMetrics)
    Nothing -> triangulatedPieces region

convexComponent :: PolygonComponent -> Maybe ConvexPolygon
convexComponent component =
  case polygonHoleLoops component of
    [] -> admittedConvexLoop (polygonOuterLoop component)
    _ -> Nothing

singleConvexRegion :: PlanarRegion -> Maybe ConvexPolygon
singleConvexRegion region =
  case planarRegionComponents region of
    [component] -> convexComponent component
    _ -> Nothing

triangulatedPieces
  :: PlanarRegion
  -> Either MinkowskiError ([ConvexPolygon], MorphologyMetrics)
triangulatedPieces region = do
  let sourceLayer = morphologyLayer region
  result <- first MinkowskiOverlayFailed (overlayLayers sourceLayer emptyMorphologyLayer)
  selectedFaces <-
    filterM
      (\face ->
         if faceCarriesExactArea result face
           then
             fmap fst
               ( first MinkowskiOverlayCellWitness
                   (faceLabels result face)
               )
           else Right False)
      (innerFaces (overlayResultTriangulation result))
  pieces <- traverse (faceConvexPolygon result) selectedFaces
  pure (pieces, metricsFromOverlay result (length pieces))

faceConvexPolygon
  :: OverlayResult leftLabel rightLabel
  -> FaceId
  -> Either MinkowskiError ConvexPolygon
faceConvexPolygon result face =
  case
    map
      (overlayExactPoint . vertexData triangulation)
      (faceVertices triangulation face) of
    [firstPoint, secondPoint, thirdPoint] ->
      convexHullPolygon (firstPoint :| [secondPoint, thirdPoint])
    vertices -> Left (MinkowskiFaceArity face (length vertices))
 where
  triangulation = overlayResultTriangulation result

unionRegions
  :: [PlanarRegion]
  -> Either MinkowskiError (PlanarRegion, MorphologyMetrics)
unionRegions [] = Right (emptyPlanarRegion, emptyMetrics)
unionRegions [region] =
  Right
    ( region
    , emptyMetrics{metricOutputCells = Just (length (planarRegionComponents region))}
    )
unionRegions regions = do
  let (leftRegions, rightRegions) = splitAt (length regions `div` 2) regions
  left <- unionRegions leftRegions
  right <- unionRegions rightRegions
  glueRegionUnion left right

glueRegionUnion
  :: (PlanarRegion, MorphologyMetrics)
  -> (PlanarRegion, MorphologyMetrics)
  -> Either MinkowskiError (PlanarRegion, MorphologyMetrics)
glueRegionUnion (left, leftMetrics) (right, rightMetrics)
  | null (planarRegionComponents left) = Right (right, leftMetrics `appendMetrics` rightMetrics)
  | null (planarRegionComponents right) = Right (left, leftMetrics `appendMetrics` rightMetrics)
  | otherwise = do
      result <-
        first MinkowskiOverlayFailed
          (overlayLayers (morphologyLayer left) (morphologyLayer right))
      published <-
        first MinkowskiPublicationFailed
          ( overlaySelectedRegion
              (uncurry (||))
              result
          )
      let selectedCells =
            V.foldl'
              (\count cell ->
                 case overlayCellGeometry cell of
                   BoundedOverlayCell _
                     | overlayCellLeft cell || overlayCellRight cell ->
                         count + 1
                   _ -> count)
              0
              (overlayResultCells result)
      pure
        ( published
        , leftMetrics
            `appendMetrics` rightMetrics
            `appendMetrics` metricsFromOverlay result selectedCells
        )

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
      kernelWitness <- convexPolygonCentroid kernel
      let representativeFaceByCell = representativeFaces candidateOverlay
      selectedCellIds <-
        Set.fromList
          <$> filterM
            ( classifyCandidateCell
                source
                kernelWitness
                representativeFaceByCell
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
      convexHullPolygon
        ( addExactPoints from firstKernelPoint
            :| ( map (addExactPoints from) remainingKernelPoints
                   <> map (addExactPoints to) kernelPoints
               )
        )
 where
  kernelPoints = NonEmpty.toList (convexPolygonPoints reflectedKernel)

boundedOutsideCellIds
  :: OverlayResult Bool Bool
  -> [OverlayCellId]
boundedOutsideCellIds result =
  V.ifoldr
    (\index cell selected ->
       case overlayCellGeometry cell of
         BoundedOverlayCell _
           | not (overlayCellLeft cell)
               && not (overlayCellRight cell) ->
               OverlayCellId index : selected
         _ -> selected)
    []
    (overlayResultCells result)

classifyCandidateCell
  :: PlanarRegion
  -> ExactPoint
  -> IntMap.IntMap FaceId
  -> OverlayResult Bool Bool
  -> OverlayCellId
  -> Either MinkowskiError Bool
classifyCandidateCell source kernelWitness representativeFaceByCell result cellId = do
  face <-
    maybe
      (Left (MinkowskiCandidateCellMissing cellId))
      Right
      (IntMap.lookup (overlayCellIndex cellId) representativeFaceByCell)
  candidate <- convexPolygonCentroid =<< faceConvexPolygon result face
  let inclusionWitness = addExactPoints candidate kernelWitness
  case regionPointLocation source inclusionWitness of
    RegionInterior -> Right True
    RegionExterior -> Right False
    RegionOnBoundary -> Left (MinkowskiInclusionAmbiguous cellId inclusionWitness)

representativeFaces
  :: OverlayResult leftLabel rightLabel
  -> IntMap.IntMap FaceId
representativeFaces result =
  IntMap.fromListWith min
    [ (overlayCellIndex (overlayFaceCellId (faceData triangulation face)), face)
    | face <- innerFaces triangulation
    , faceCarriesExactArea result face
    ]
 where
  triangulation = overlayResultTriangulation result

overlayCellIndex :: OverlayCellId -> Int
overlayCellIndex (OverlayCellId index) = index

publishCellSelection
  :: Set.Set OverlayCellId
  -> OverlayResult leftLabel rightLabel
  -> Either MinkowskiError PlanarRegion
publishCellSelection selected result = do
  layer <-
    first MinkowskiPublicationFailed
      ( labelledPlanarLayerFromExactCoordinates
          False
          triangulation
          (\vertex -> Right (overlayExactPoint (vertexData triangulation vertex)))
          (\face ->
             if faceCarriesExactArea result face
               then
                 Right
                   ( Set.member
                       (overlayFaceCellId (faceData triangulation face))
                       selected
                   )
               else Right False)
      )
  pure (Map.findWithDefault emptyPlanarRegion True (planarLayerRegions layer))
 where
  triangulation = overlayResultTriangulation result

morphologyLayer
  :: PlanarRegion
  -> PlanarLayer Bool
morphologyLayer region =
  planarLayerFromAdmittedComponents
    False
    [(True, component) | component <- planarRegionComponents region]

emptyMorphologyLayer :: PlanarLayer Bool
emptyMorphologyLayer = morphologyLayer emptyPlanarRegion

regionBoundaryEdges :: PlanarRegion -> [(ExactPoint, ExactPoint)]
regionBoundaryEdges region =
  concatMap
    (\component ->
       concatMap
         (cyclePairs . exactLoopPoints)
         (polygonOuterLoop component : polygonHoleLoops component))
    (planarRegionComponents region)

metricsFromOverlay
  :: OverlayResult leftLabel rightLabel
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

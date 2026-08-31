{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact labelled common refinement. Source boundaries descend through one
-- exact segment-event plan, glue into canonical atomic constraints, and are
-- admitted only when their binary64 DCEL realization preserves every exact
-- relation.
module Moonlight.Triangulation.Overlay
  ( BoundaryLoopRef (..)
  , OverlayOperand
  , BoundaryRef
  , BoundaryVertexRef
  , BoundaryEdgeRef
  , boundaryRefComponent
  , boundaryRefLoop
  , boundaryRefLocalIndex
  , OverlayVertexOrigin
  , overlayOriginLeftVertices
  , overlayOriginRightVertices
  , overlayOriginLeftEdges
  , overlayOriginRightEdges
  , OverlayEdgeOrigin
  , overlayEdgeLeftSources
  , overlayEdgeRightSources
  , OverlaySupport
  , overlaySupportLabels
  , OverlayCellSupport (..)
  , OverlayCellId (..)
  , OverlayCellGeometry (..)
  , OverlayCell (..)
  , OverlayFace (..)
  , OverlayVertex (..)
  , OverlayEdge (..)
  , OverlayReceipt (..)
  , OverlayArrangementObstruction (..)
  , OverlayCellWitness (..)
  , OverlayError (..)
  , OverlayResult
  , OverlaySelectionKind (..)
  , OverlaySelectionError (..)
  , CoverGap
  , coverGapRegion
  , LayerCoverageError (..)
  , overlayLayers
  , overlayAll
  , overlayEmbeddedTriangulation
  , overlayReceipt
  , overlayCells
  , foldBoundedOverlayCells
  , overlayArrangementVertices
  , overlayArrangementEdges
  , overlayPlanarLayer
  , overlaySelectedRegion
  , overlayMass
  , overlayConfusion
  , layerCovers
  , overlayClosedUnion
  , overlayClosedIntersection
  , overlayRegularizedDifference
  ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Control.Monad (filterM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Moonlight.Triangulation.CellSet
  ( CellSelectionError
  , ExactCellSet
  )
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.Dcel (vertexData)
import Moonlight.Triangulation.Exact (ExactPoint)
import Moonlight.Triangulation.Internal.HandleDefs
  ( UndirectedEdgeId
  , VertexId
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( innerFaces
  , undirectedEdges
  , vertices
  )
import Moonlight.Triangulation.Internal.CellSet (closeExactCellSetWith)
import Moonlight.Triangulation.Internal.Overlay.Arrangement
  ( certifyArrangement
  )
import Moonlight.Triangulation.Internal.Overlay.Resident
  ( OverlayDiagonalSchedule (CanonicalOverlayDiagonals)
  , edgeSupport
  , faceCarriesExactArea
  , faceLabels
  , regionFaceLabels
  , residentOverlay
  , vertexSupport
  )
import Moonlight.Triangulation.Internal.Overlay.Types
import Moonlight.Triangulation.Internal.Representation (Triangulation)
import Moonlight.Triangulation.Internal.Types
  ( ConstraintMode (Constrained)
  )
import Moonlight.Triangulation.Internal.Tournament
  ( interpretTournament
  , planTournament
  )
import Moonlight.Triangulation.Region
  ( PlanarLayer
  , PlanarRegion
  , PolygonComponent
  , RegionPublicationError (..)
  , emptyPlanarRegion
  , planarLayerOutsideLabel
  , planarLayerRegions
  , planarRegionComponents
  )
import Moonlight.Triangulation.Internal.Region.Publication
  ( labelledPlanarLayerFromExactCoordinates
  , planarLayerFromAdmittedComponents
  )
import Moonlight.Triangulation.Valuation
  ( ExactArea
  , polygonComponentArea
  )

-- | Exact full-dimensional part of a requested window not owned by any
-- bounded label region in the candidate layer.
newtype CoverGap = CoverGap PlanarRegion
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

coverGapRegion :: CoverGap -> PlanarRegion
coverGapRegion (CoverGap region) = region

data LayerCoverageError label
  = LayerCoverageOverlayFailed !(OverlayError Bool label)
  | LayerCoverageGapPublicationFailed !RegionPublicationError
  | LayerCoverageGap !CoverGap
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Construct the exact common refinement and its one faithful resident DCEL.
overlayLayers
  :: (Ord leftLabel, Ord rightLabel)
  => PlanarLayer leftLabel
  -> PlanarLayer rightLabel
  -> Either
      (OverlayError leftLabel rightLabel)
      (OverlayResult leftLabel rightLabel)
overlayLayers leftLayer rightLayer = do
  certified <- certifyArrangement leftLayer rightLayer
  residentOverlay
    CanonicalOverlayDiagonals
    (planarLayerOutsideLabel leftLayer, planarLayerOutsideLabel rightLayer)
    certified

-- | Construct the common refinement of a nonempty family in input order.
-- Adjacent layers glue in a balanced tree, so an early boundary participates
-- in logarithmically many intermediate arrangements rather than every later
-- left-associated step. Each intermediate layer is projected directly from
-- its certified binary result.
overlayAll
  :: Ord label
  => NonEmpty (PlanarLayer label)
  -> Either
      (OverlayError (NonEmpty label) (NonEmpty label))
      (PlanarLayer (NonEmpty label))
overlayAll =
  interpretTournament overlayLayerPair
    . planTournament
    . fmap singletonLabelLayer

overlayLayerPair
  :: Ord label
  => PlanarLayer (NonEmpty label)
  -> PlanarLayer (NonEmpty label)
  -> Either
      (OverlayError (NonEmpty label) (NonEmpty label))
      (PlanarLayer (NonEmpty label))
overlayLayerPair leftLayer rightLayer =
  flattenNaryOverlay <$> overlayLayers leftLayer rightLayer

singletonLabelLayer :: Ord label => PlanarLayer label -> PlanarLayer (NonEmpty label)
singletonLabelLayer layer =
  planarLayerFromAdmittedComponents
    (planarLayerOutsideLabel layer :| [])
    [ (label :| [], component)
    | (label, region) <- Map.toAscList (planarLayerRegions layer)
    , component <- planarRegionComponents region
    ]

flattenNaryOverlay
  :: Ord label
  => OverlayResult (NonEmpty label) (NonEmpty label)
  -> PlanarLayer (NonEmpty label)
flattenNaryOverlay = projectOverlayLayerWith (uncurry (<>))

projectOverlayLayerWith
  :: (Eq leftLabel, Eq rightLabel, Ord projectedLabel)
  => ((leftLabel, rightLabel) -> projectedLabel)
  -> OverlayResult leftLabel rightLabel
  -> PlanarLayer projectedLabel
projectOverlayLayerWith projectLabel result =
  planarLayerFromAdmittedComponents
    (projectLabel (overlayResultOutsideLabels result))
    [ (projectLabel (overlayCellLabels cell), component)
    | cell <- V.toList (overlayResultCells result)
    , overlayCellLabels cell /= overlayResultOutsideLabels result
    , BoundedOverlayCell component <- [overlayCellGeometry cell]
    ]

overlayCellLabels :: OverlayCell leftLabel rightLabel -> (leftLabel, rightLabel)
overlayCellLabels cell = (overlayCellLeft cell, overlayCellRight cell)
{-# INLINE overlayCellLabels #-}

-- | The binary64 realization used by existing DCEL observations. Exact overlay
-- operations accept 'OverlayResult', never this projection.
overlayEmbeddedTriangulation
  :: OverlayResult leftLabel rightLabel
  -> Triangulation
      'Constrained
      OverlayVertex
      ()
      OverlayEdge
      OverlayFace
overlayEmbeddedTriangulation = overlayResultTriangulation

overlayReceipt :: OverlayResult leftLabel rightLabel -> OverlayReceipt
overlayReceipt = overlayResultReceipt

overlayCells
  :: OverlayResult leftLabel rightLabel
  -> [(OverlayCellId, OverlayCell leftLabel rightLabel)]
overlayCells result =
  V.toList
    (V.imap (\index cell -> (OverlayCellId index, cell)) (overlayResultCells result))

overlayArrangementVertices
  :: OverlayResult leftLabel rightLabel
  -> [(VertexId, OverlayVertex)]
overlayArrangementVertices result =
  let triangulation = overlayResultTriangulation result
   in [(vertex, vertexData triangulation vertex) | vertex <- vertices triangulation]

overlayArrangementEdges
  :: OverlayResult leftLabel rightLabel
  -> [(UndirectedEdgeId, OverlayEdgeOrigin)]
overlayArrangementEdges result =
  let triangulation = overlayResultTriangulation result
   in [ (edge, origin)
      | edge <- undirectedEdges triangulation
      , OverlayBoundary origin <- [Dcel.undirectedEdgeData triangulation edge]
      ]

-- | Publish the already-admitted bounded cell geometry. The resident DCEL is
-- a realization of these exact cells, not a second authoring source.
overlayPlanarLayer
  :: (Ord leftLabel, Ord rightLabel)
  => OverlayResult leftLabel rightLabel
  -> PlanarLayer (leftLabel, rightLabel)
overlayPlanarLayer = projectOverlayLayerWith id

-- | Publish the selected two-dimensional cells. Internal arrangement edges
-- between differently labelled but jointly selected cells dissolve because
-- selection precedes component descent.
overlaySelectedRegion
  :: ((leftLabel, rightLabel) -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either RegionPublicationError PlanarRegion
overlaySelectedRegion selected result
  | selected (overlayResultOutsideLabels result) = Left RegionUnboundedSelection
  | otherwise = do
      published <-
        labelledPlanarLayerFromExactCoordinates
          False
          triangulation
          exactPointAt
          labelFace
      pure (Map.findWithDefault emptyPlanarRegion True (planarLayerRegions published))
 where
 triangulation = overlayResultTriangulation result
 exactPointAt :: VertexId -> Either RegionPublicationError ExactPoint
 exactPointAt vertex = Right (overlayExactPoint (vertexData triangulation vertex))
 labelFace face
   | faceCarriesExactArea result face =
       selected <$> regionFaceLabels result face
   | otherwise = Right False

-- | Sum selected exact bounded cells without publishing a polygonal union or
-- computing its perimeter. Selecting the unbounded outside pair is refused by
-- the same obstruction used by 'overlaySelectedRegion'.
overlayMass
  :: ((leftLabel, rightLabel) -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either RegionPublicationError ExactArea
overlayMass selected result
  | selected (overlayResultOutsideLabels result) = Left RegionUnboundedSelection
  | otherwise =
      Right (foldBoundedOverlayCells selectedCellArea mempty result)
 where
  selectedCellArea area labels component
    | selected labels = area <> polygonComponentArea component
    | otherwise = area

-- | Exact finite confusion masses, grouped by label pair in one traversal.
-- The outside pair is absent because its complete planar cell is unbounded;
-- bounded cavities carrying that same pair are not misreported as its total.
overlayConfusion
  :: (Ord leftLabel, Ord rightLabel)
  => OverlayResult leftLabel rightLabel
  -> Map.Map (leftLabel, rightLabel) ExactArea
overlayConfusion result =
  foldBoundedOverlayCells accumulateCell Map.empty result
 where
  outsideLabels = overlayResultOutsideLabels result
  accumulateCell masses labels component
    | labels == outsideLabels = masses
    | otherwise = Map.insertWith (<>) labels (polygonComponentArea component) masses

foldBoundedOverlayCells
  :: (accumulator -> (leftLabel, rightLabel) -> PolygonComponent -> accumulator)
  -> accumulator
  -> OverlayResult leftLabel rightLabel
  -> accumulator
foldBoundedOverlayCells accumulate initial result =
  V.foldl' descend initial (overlayResultCells result)
 where
  descend accumulator cell =
    case overlayCellGeometry cell of
      BoundedOverlayCell component ->
        accumulate accumulator (overlayCellLabels cell) component
      UnboundedOverlayCell _ -> accumulator
{-# INLINE foldBoundedOverlayCells #-}

-- | Certify full-dimensional coverage of an admitted polygonal window. The
-- success path inspects certified cells only; an exact gap region is glued
-- solely when the obstruction actually exists.
layerCovers
  :: Ord label
  => PlanarLayer label
  -> PolygonComponent
  -> Either (LayerCoverageError label) ()
layerCovers layer window = do
  let outside = planarLayerOutsideLabel layer
      windowLayer = planarLayerFromAdmittedComponents False [(True, window)]
  result <- first LayerCoverageOverlayFailed (overlayLayers windowLayer layer)
  if V.any (isGapCell outside) (overlayResultCells result)
    then do
      gap <-
        first LayerCoverageGapPublicationFailed
          ( overlaySelectedRegion
              (\(insideWindow, label) ->
                 insideWindow && label == outside)
              result
          )
      Left (LayerCoverageGap (CoverGap gap))
    else Right ()

isGapCell :: Eq label => label -> OverlayCell Bool label -> Bool
isGapCell outside cell =
  overlayCellLeft cell
    && overlayCellRight cell == outside
    && case overlayCellGeometry cell of
      BoundedOverlayCell _ -> True
      UnboundedOverlayCell _ -> False

overlayClosedUnion
  :: (Ord leftLabel, Ord rightLabel)
  => (leftLabel -> Bool)
  -> (rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
overlayClosedUnion selectLeft selectRight =
  selectClosedCells
    ClosedUnionSelection
    (\support -> supportAny selectLeft (overlaySupportLeft support) || supportAny selectRight (overlaySupportRight support))
    (\leftLabel rightLabel -> selectLeft leftLabel || selectRight rightLabel)

overlayClosedIntersection
  :: (Ord leftLabel, Ord rightLabel)
  => (leftLabel -> Bool)
  -> (rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
overlayClosedIntersection selectLeft selectRight =
  selectClosedCells
    ClosedIntersectionSelection
    (\support -> supportAny selectLeft (overlaySupportLeft support) && supportAny selectRight (overlaySupportRight support))
    (\leftLabel rightLabel -> selectLeft leftLabel && selectRight rightLabel)

overlayRegularizedDifference
  :: (leftLabel -> Bool)
  -> (rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
overlayRegularizedDifference selectLeft selectRight result =
  let outsidePair = overlayResultOutsideLabels result
      selectFace leftLabel rightLabel = selectLeft leftLabel && not (selectRight rightLabel)
   in if uncurry selectFace outsidePair
        then Left (OverlaySelectionContainsUnboundedCell RegularizedDifferenceSelection)
        else closeSelectedCells [] [] selectFace result

selectClosedCells
  :: (Ord leftLabel, Ord rightLabel)
  => OverlaySelectionKind
  -> (OverlayCellSupport leftLabel rightLabel -> Bool)
  -> (leftLabel -> rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
selectClosedCells selectionKind selectSupport selectFace result =
  let outsidePair = overlayResultOutsideLabels result
      triangulation = overlayResultTriangulation result
   in if uncurry selectFace outsidePair
        then Left (OverlaySelectionContainsUnboundedCell selectionKind)
        else do
          selectedVertices <-
            filterM
              ( fmap selectSupport
                  . first OverlaySelectionProvenance
                  . vertexSupport result
              )
              (vertices triangulation)
          selectedEdges <-
            filterM
              (\edge ->
                 case Dcel.undirectedEdgeData triangulation edge of
                   OverlayDiagonal -> Right False
                   OverlayBoundary _ ->
                     selectSupport
                       <$> first OverlaySelectionProvenance (edgeSupport result edge))
              (undirectedEdges triangulation)
          closeSelectedCells selectedVertices selectedEdges selectFace result

closeSelectedCells
  :: [VertexId]
  -> [UndirectedEdgeId]
  -> (leftLabel -> rightLabel -> Bool)
  -> OverlayResult leftLabel rightLabel
  -> Either OverlaySelectionError ExactCellSet
closeSelectedCells selectedVertices selectedEdges selectFace result =
  let triangulation = overlayResultTriangulation result
      exactPointAt :: VertexId -> Either CellSelectionError ExactPoint
      exactPointAt vertex = Right (overlayExactPoint (vertexData triangulation vertex))
   in do
        selectedFaces <-
          filterM
            (\face ->
               if faceCarriesExactArea result face
                 then
                   fmap (uncurry selectFace)
                     ( first OverlaySelectionProvenance
                         (faceLabels result face)
                     )
                 else Right False)
            (innerFaces triangulation)
        first OverlaySelectionInvalid
          ( closeExactCellSetWith
              exactPointAt
              triangulation
              selectedVertices
              selectedEdges
              selectedFaces
          )

supportAny :: (label -> Bool) -> OverlaySupport label -> Bool
supportAny predicate = any predicate . overlaySupportLabels

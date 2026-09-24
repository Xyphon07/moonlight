{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Exact labelled common refinement. Source sections descend through one
-- segment-event arrangement; incidence, coordinates and provenance glue before
-- any polygon publication. Binary64 is not an admission prerequisite.
module Moonlight.Planar.Overlay
  ( BoundaryLoopRef (..)
  , OverlaySourceId (..)
  , BoundaryRef
  , BoundaryVertexRef
  , BoundaryEdgeRef
  , boundaryRefSource
  , boundaryRefComponent
  , boundaryRefLoop
  , boundaryRefLocalIndex
  , OverlayVertexOrigin
  , overlayOriginVertices
  , overlayOriginEdges
  , OverlayEdgeOrigin
  , overlayEdgeSources
  , OverlayVertex (..)
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
import Control.Monad (filterM)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Moonlight.Planar.CellSet (CellSelectionError (..), ExactCellSet)
import Moonlight.Planar.Exact (ExactPoint)
import Moonlight.Planar.Internal.HandleDefs
  ( FaceId (..), UndirectedEdgeId (..), VertexId (..)
  , faceIdIndex, normalizedDirected, reverseEdge, vertexIdIndex
  )
import Moonlight.Planar.Internal.CellSet (closeExactCellSetWith)
import Moonlight.Planar.Internal.Incidence
  ( faceBoundaryComponents, faceIsolatedVertices, incidenceDestination
  , incidenceFaceCount, incidenceIncidentFace, incidenceOrigin
  , incidenceVertexOutgoingEdges
  )
import Moonlight.Planar.Internal.Overlay.Arrangement (certifyArrangement)
import Moonlight.Planar.Internal.Overlay.Types
import Moonlight.Planar.Region
  ( PlanarLayer, PlanarRegion, PolygonComponent, RegionPublicationError (..)
  , planarLayerOutsideLabel, planarLayerRegions, planarRegionComponents
  )
import Moonlight.Planar.Internal.Region.Publication
  ( planarLayerFromAdmittedComponents, planarRegionFromSelectedIncidence )
import Moonlight.Planar.Valuation (ExactArea, orientedBoundaryArea)

newtype CoverGap = CoverGap PlanarRegion
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

coverGapRegion :: CoverGap -> PlanarRegion
coverGapRegion (CoverGap region) = region

data LayerCoverageError
  = LayerCoverageOverlayFailed !OverlayError
  | LayerCoverageGapPublicationFailed !RegionPublicationError
  | LayerCoverageGap !CoverGap
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The binary entrance is a typed two-source view of the same fused owner.
overlayLayers
  :: PlanarLayer left -> PlanarLayer right
  -> Either OverlayError (OverlayResult (left, right))
overlayLayers left right = do
  let (leftSource, leftLabels) = encodeSource left
      (rightSource, rightLabels) = encodeSource right
  result <- certifyArrangement (leftSource :| [rightSource])
  traverse
    (\state -> (,)
      <$> decodeSource (OverlaySourceId 0) leftLabels state
      <*> decodeSource (OverlaySourceId 1) rightLabels state)
    result

-- | Submit every source once. Labels are decoded in input order only after
-- sparse source-local transitions agree around the complete arrangement.
overlayAll
  :: NonEmpty (PlanarLayer label)
  -> Either OverlayError (OverlayResult (NonEmpty label))
overlayAll layers = do
  let sources = fmap encodeSource layers
      tables = NonEmpty.zip (OverlaySourceId 0 :| fmap OverlaySourceId [1 ..]) (fmap snd sources)
  result <- certifyArrangement (fmap fst sources)
  traverse
    (\state -> traverse (\(source, table) -> decodeSource source table state) tables)
    result

encodeSource :: PlanarLayer label -> (PlanarLayer OverlayLabelId, V.Vector label)
encodeSource layer =
  let regions = Map.toAscList (planarLayerRegions layer)
   in ( planarLayerFromAdmittedComponents (OverlayLabelId 0)
          [ (OverlayLabelId index, component)
          | (index, (_, region)) <- zip [1 ..] regions
          , component <- planarRegionComponents region
          ]
      , V.fromList (planarLayerOutsideLabel layer : fmap fst regions)
      )

decodeSource
  :: OverlaySourceId -> V.Vector label -> IntMap.IntMap OverlayLabelId
  -> Either OverlayError label
decodeSource source table state =
  let ordinal = IntMap.findWithDefault (OverlayLabelId 0) (overlaySourceIndex source) state
   in maybe
        (Left (OverlayProvenanceIncomplete (OverlaySourceLabelMissing source ordinal)))
        Right (table V.!? overlayLabelIndex ordinal)

overlayReceipt :: OverlayResult labels -> OverlayReceipt
overlayReceipt = overlayResultReceipt

overlayCells :: OverlayResult labels -> [(FaceId, labels)]
overlayCells = V.toList . V.imap (\index labels -> (FaceId (fromIntegral index), labels)) . overlayResultLabels

overlayArrangementVertices :: OverlayResult labels -> [(VertexId, OverlayVertex)]
overlayArrangementVertices = V.toList . V.imap (\index vertex -> (VertexId (fromIntegral index), vertex)) . overlayResultVertices

overlayArrangementEdges :: OverlayResult labels -> [(UndirectedEdgeId, OverlayEdgeOrigin)]
overlayArrangementEdges = V.toList . V.imap (\index origin -> (UndirectedEdgeId (fromIntegral index), origin)) . overlayResultEdges

-- | Polygon publication is a requested derived view, not eager cell storage.
overlayPlanarLayer
  :: Ord labels
  => OverlayResult labels -> Either RegionPublicationError (PlanarLayer labels)
overlayPlanarLayer result = do
  outside <- outsideLabels result
  components <- traverse
    (\labels -> fmap (fmap (labels,) . planarRegionComponents)
      (overlaySelectedRegion (== labels) result))
    (Set.toAscList (Set.delete outside (Set.fromList (V.toList (overlayResultLabels result)))))
  pure (planarLayerFromAdmittedComponents outside (concat components))

overlaySelectedRegion
  :: (labels -> Bool) -> OverlayResult labels
  -> Either RegionPublicationError PlanarRegion
overlaySelectedRegion selected result = do
  outside <- outsideLabels result
  if selected outside
    then Left RegionUnboundedSelection
    else planarRegionFromSelectedIncidence
      (overlayResultIncidence result)
      (exactPointAt RegionCoordinateMissing result)
      (\face -> IntSet.member (faceIdIndex face) selectedFaces)
 where
  selectedFaces = selectedFaceIndices selected result

overlayMass
  :: (labels -> Bool) -> OverlayResult labels
  -> Either RegionPublicationError ExactArea
overlayMass selected result = do
  outside <- outsideLabels result
  if selected outside
    then Left RegionUnboundedSelection
    else foldOverlayBoundariesWith (exactPointAt RegionCoordinateMissing result)
      selected
      (\area _ boundary -> area <> orientedBoundaryArea boundary)
      mempty result

overlayConfusion
  :: Ord labels
  => OverlayResult labels -> Either OverlayCellWitness (Map.Map labels ExactArea)
overlayConfusion result = do
  outside <- faceLabels result (FaceId 0)
  foldOverlayBoundariesWith (exactPointAt OverlayVertexMissing result) (/= outside)
    (\masses labels boundary -> Map.insertWith (<>) labels (orientedBoundaryArea boundary) masses)
    Map.empty result

-- | Direct exact oriented-boundary observation. Every CCB participates; bridge
-- darts cancel algebraically. No polygon or resident triangulation is created.
foldBoundedOverlayCells
  :: (accumulator -> labels -> [(ExactPoint, ExactPoint)] -> accumulator)
  -> accumulator -> OverlayResult labels
  -> Either OverlayCellWitness accumulator
foldBoundedOverlayCells step initial result =
  foldOverlayBoundariesWith (exactPointAt OverlayVertexMissing result) (const True) step initial result
{-# INLINE foldBoundedOverlayCells #-}

foldOverlayBoundariesWith
  :: forall obstruction accumulator labels. (VertexId -> Either obstruction ExactPoint)
  -> (labels -> Bool)
  -> (accumulator -> labels -> [(ExactPoint, ExactPoint)] -> accumulator)
  -> accumulator -> OverlayResult labels -> Either obstruction accumulator
foldOverlayBoundariesWith coordinate selected step initial result =
  V.ifoldM descend initial (overlayResultLabels result)
 where
  incidence = overlayResultIncidence result
  descend :: accumulator -> Int -> labels -> Either obstruction accumulator
  descend accumulator index labels
    | index == 0 || not (selected labels) = Right accumulator
    | otherwise = do
        boundary <- traverse
          (\edge -> (,) <$> coordinate (incidenceOrigin incidence edge)
            <*> coordinate (incidenceDestination incidence edge))
          (concat (faceBoundaryComponents incidence (FaceId (fromIntegral index))))
        pure $! step accumulator labels boundary
{-# INLINE foldOverlayBoundariesWith #-}

outsideLabels :: OverlayResult labels -> Either RegionPublicationError labels
outsideLabels result = maybe (Left (RegionFaceLabelMissing (FaceId 0))) Right
  (overlayResultLabels result V.!? 0)

faceLabels :: OverlayResult labels -> FaceId -> Either OverlayCellWitness labels
faceLabels result face = maybe (Left (OverlayFaceMissing face)) Right
  (overlayResultLabels result V.!? faceIdIndex face)

exactPointAt
  :: (VertexId -> obstruction) -> OverlayResult labels -> VertexId
  -> Either obstruction ExactPoint
exactPointAt obstruction result vertex = maybe (Left (obstruction vertex)) (Right . overlayExactPoint)
  (overlayResultVertices result V.!? vertexIdIndex vertex)

selectedFaceIndices :: (labels -> Bool) -> OverlayResult labels -> IntSet.IntSet
selectedFaceIndices selected = IntSet.fromDistinctAscList
  . V.ifoldr (\index labels rest -> if index /= 0 && selected labels then index : rest else rest) []
  . overlayResultLabels

layerCovers :: Eq label => PlanarLayer label -> PolygonComponent -> Either LayerCoverageError ()
layerCovers layer window = do
  let outside = planarLayerOutsideLabel layer
      windowLayer = planarLayerFromAdmittedComponents False [(True, window)]
      isGap (inside, label) = inside && label == outside
  result <- first LayerCoverageOverlayFailed (overlayLayers windowLayer layer)
  if IntSet.null (selectedFaceIndices isGap result)
    then Right ()
    else do
      gap <- first LayerCoverageGapPublicationFailed (overlaySelectedRegion isGap result)
      Left (LayerCoverageGap (CoverGap gap))

overlayClosedUnion
  :: (left -> Bool) -> (right -> Bool) -> OverlayResult (left, right)
  -> Either OverlaySelectionError ExactCellSet
overlayClosedUnion selectLeft selectRight = selectClosedCells ClosedUnionSelection
  (\support -> any (selectLeft . fst) support || any (selectRight . snd) support)
  (\(left, right) -> selectLeft left || selectRight right)

-- | Intersect marginal closures, not merely full-dimensional label pairs:
-- edge and vertex contacts survive even when no incident face satisfies both.
overlayClosedIntersection
  :: (left -> Bool) -> (right -> Bool) -> OverlayResult (left, right)
  -> Either OverlaySelectionError ExactCellSet
overlayClosedIntersection selectLeft selectRight = selectClosedCells ClosedIntersectionSelection
  (\support -> any (selectLeft . fst) support && any (selectRight . snd) support)
  (\(left, right) -> selectLeft left && selectRight right)

overlayRegularizedDifference
  :: (left -> Bool) -> (right -> Bool) -> OverlayResult (left, right)
  -> Either OverlaySelectionError ExactCellSet
overlayRegularizedDifference selectLeft selectRight result = do
  let selected (left, right) = selectLeft left && not (selectRight right)
  outside <- first OverlaySelectionProvenance (faceLabels result (FaceId 0))
  if selected outside
    then Left (OverlaySelectionContainsUnboundedCell RegularizedDifferenceSelection)
    else closeSelectedCells [] [] selected result

selectClosedCells
  :: OverlaySelectionKind -> (NonEmpty labels -> Bool) -> (labels -> Bool)
  -> OverlayResult labels -> Either OverlaySelectionError ExactCellSet
selectClosedCells selectionKind selectSupport selectFace result = do
  outside <- first OverlaySelectionProvenance (faceLabels result (FaceId 0))
  if selectFace outside
    then Left (OverlaySelectionContainsUnboundedCell selectionKind)
    else do
      selectedVertices <- filterM (fmap selectSupport . first OverlaySelectionProvenance . vertexSupport)
        (fmap fst (overlayArrangementVertices result))
      selectedEdges <- filterM (fmap selectSupport . first OverlaySelectionProvenance . edgeSupport)
        (fmap fst (overlayArrangementEdges result))
      closeSelectedCells selectedVertices selectedEdges selectFace result
 where
  incidence = overlayResultIncidence result
  isolatedFaces = IntMap.fromList
    [ (vertexIdIndex vertex, face)
    | index <- [0 .. incidenceFaceCount incidence - 1]
    , let face = FaceId (fromIntegral index)
    , vertex <- faceIsolatedVertices incidence face
    ]
  vertexSupport vertex = case NonEmpty.nonEmpty (incidenceVertexOutgoingEdges incidence vertex) of
    Just outgoing -> traverse (faceLabels result . incidenceIncidentFace incidence) outgoing
    Nothing -> do
      face <- maybe (Left (OverlayVertexMissing vertex)) Right (IntMap.lookup (vertexIdIndex vertex) isolatedFaces)
      (:| []) <$> faceLabels result face
  edgeSupport edge =
    let forward = normalizedDirected edge
     in traverse (faceLabels result . incidenceIncidentFace incidence) (forward :| [reverseEdge forward])

closeSelectedCells
  :: [VertexId] -> [UndirectedEdgeId] -> (labels -> Bool) -> OverlayResult labels
  -> Either OverlaySelectionError ExactCellSet
closeSelectedCells selectedVertices selectedEdges selectFace result =
  first OverlaySelectionInvalid
    (closeExactCellSetWith
      (exactPointAt (\vertex -> CellVertexOutOfRange vertex (V.length (overlayResultVertices result))) result)
      (overlayResultIncidence result) selectedVertices selectedEdges
      (fmap (FaceId . fromIntegral) (IntSet.toAscList (selectedFaceIndices selectFace result))))

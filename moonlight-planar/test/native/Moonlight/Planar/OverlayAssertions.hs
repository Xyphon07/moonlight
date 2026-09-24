-- | Shared correspondence laws for exact overlay incidence, receipts, and provenance.
module Moonlight.Planar.OverlayAssertions
  ( assertOverlayIntegrity
  , unboundedLoopCount
  ) where

import Control.Monad ( unless )
import Moonlight.Planar.Exact ( SegmentRelation(SegmentsShareEndpoint), exactSegment,
  exactSegmentEndpoints, ExactSegment )
import Moonlight.Planar.Internal.ExactSegmentEvents ( exactSegmentEventPlan,
  exactSegmentRelationMap, ExactSweepSegmentId(..) )
import Moonlight.Planar.Internal.HandleDefs ( FaceId(..) )
import Moonlight.Planar.Internal.Incidence ( faceBoundaryComponents, incidenceEdgeCount,
  incidenceFaceCount, incidenceUndirectedEndpoints, incidenceVertexCount )
import Moonlight.Planar.Internal.Overlay.Types ( overlayResultIncidence )
import Moonlight.Planar.Overlay ( OverlayEdgeOrigin(overlayEdgeSources),
  OverlayReceipt(overlayProvenanceIncidences, overlayAtomicEdges, overlayOutputVertices,
  overlayArrangementCells, overlayBoundaryOrbits), OverlayResult, OverlayVertex(overlayVertexOrigin,
  overlayExactPoint), OverlayVertexOrigin(..), overlayArrangementEdges, overlayArrangementVertices,
  overlayCells, overlayReceipt )
import Support ( assertEqual, requireRight )
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Vector as V

assertOverlayIntegrity
  :: String
  -> OverlayResult labels
  -> IO ()
assertOverlayIntegrity label result = do
  let incidence = overlayResultIncidence result
      boundaryEdges = overlayArrangementEdges result
      cellIds = Set.fromList (map fst (overlayCells result))
      exactVertices = overlayArrangementVertices result
      pointTable = Map.fromList (fmap (fmap overlayExactPoint) exactVertices)
      receipt = overlayReceipt result
  assertEqual
    (label <> " atomic incidence correspondence")
    (overlayAtomicEdges receipt)
    (length boundaryEdges)
  assertEqual (label <> " incidence cardinalities match exact payload planes")
    (length exactVertices, length boundaryEdges, length (overlayCells result))
    (incidenceVertexCount incidence, incidenceEdgeCount incidence, incidenceFaceCount incidence)
  assertEqual (label <> " all faces including the single outside face are labelled")
    (Set.fromList [FaceId (fromIntegral index) | index <- [0 .. incidenceFaceCount incidence - 1]]) cellIds
  assertEqual (label <> " exact receipt face and vertex counts")
    (length exactVertices, length (overlayCells result))
    (overlayOutputVertices receipt, overlayArrangementCells receipt)
  assertEqual (label <> " receipt counts raw boundary orbits, not simple polygon loops")
    (sum (fmap (length . faceBoundaryComponents incidence . fst) (overlayCells result)))
    (overlayBoundaryOrbits receipt)
  unless
    ( all
        (vertexOriginIsNonEmpty . overlayVertexOrigin . snd)
        (overlayArrangementVertices result)
    )
    (fail (label <> ": exact vertex lost all typed origins"))
  let originCount =
        sum (fmap (NonEmpty.length . overlayEdgeSources . snd) boundaryEdges)
          + sum (fmap (\(_, vertex) -> let origin = overlayVertexOrigin vertex
                                       in length (overlayOriginVertices origin) + length (overlayOriginEdges origin)) exactVertices)
  assertEqual (label <> " all retained source incidences are charged") originCount (overlayProvenanceIncidences receipt)
  atomicSegments <-
    traverse
      (\(edge, _) -> do
         let (fromVertex, toVertex) = incidenceUndirectedEndpoints incidence edge
         fromPoint <- maybe (fail (label <> " missing atomic source")) pure (Map.lookup fromVertex pointTable)
         toPoint <- maybe (fail (label <> " missing atomic destination")) pure (Map.lookup toVertex pointTable)
         requireRight
               (label <> " atomic segment")
               (exactSegment fromPoint toPoint))
      boundaryEdges
  atomicPlan <-
    requireRight
      (label <> " atomic endpoint-incidence proof")
      (exactSegmentEventPlan (V.fromList atomicSegments))
  assertEqual
    (label <> " atomics have only endpoint-incidence relations")
    (endpointIncidenceRelations atomicSegments)
    (exactSegmentRelationMap atomicPlan)

endpointIncidenceRelations
  :: [ExactSegment]
  -> Map.Map (ExactSweepSegmentId, ExactSweepSegmentId) SegmentRelation
endpointIncidenceRelations segments =
  Map.fromList
    [ ( (ExactSweepSegmentId leftIndex, ExactSweepSegmentId rightIndex)
      , SegmentsShareEndpoint
      )
    | (leftIndex, left) <- zip [0 :: Int ..] segments
    , (rightIndex, right) <- zip [leftIndex + 1 ..] (drop (leftIndex + 1) segments)
    , let (leftFrom, leftTo) = exactSegmentEndpoints left
          (rightFrom, rightTo) = exactSegmentEndpoints right
    , not
        ( Set.null
            ( Set.intersection
                (Set.fromList [leftFrom, leftTo])
                (Set.fromList [rightFrom, rightTo])
            )
        )
    ]

vertexOriginIsNonEmpty :: OverlayVertexOrigin -> Bool
vertexOriginIsNonEmpty origin =
  not
    (null (overlayOriginVertices origin) && null (overlayOriginEdges origin))

unboundedLoopCount :: OverlayResult labels -> Maybe Int
unboundedLoopCount result =
  case
    [ length (faceBoundaryComponents (overlayResultIncidence result) face)
    | (face, _) <- overlayCells result
    , face == FaceId 0
    ] of
    [count] -> Just count
    _ -> Nothing

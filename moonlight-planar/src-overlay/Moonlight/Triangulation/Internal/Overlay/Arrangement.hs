{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Exact source normalization and binary64 embedding certification. This
-- module ends at the arrangement/resident seam: no DCEL face state crosses it.
module Moonlight.Triangulation.Internal.Overlay.Arrangement
  ( ExactEdgeKey
  , AtomicEdge (..)
  , atomicEdgeFrom
  , atomicEdgeTo
  , OverlayVertexSeed (..)
  , ArrangementMetrics (..)
  , CertifiedArrangement (..)
  , certifyArrangement
  , canonicalEdgeKey
  , atomicKey
  , compareAround
  ) where

import Data.Bifunctor (first)
import Data.List (sortBy)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Moonlight.Triangulation.Exact
  ( ExactGeometryError
  , ExactPoint
  , ExactSegment
  , SegmentRelation (SegmentsShareEndpoint)
  , compareExactVectorAngle
  , exactPointCoordinates
  , exactPointFromQueryPoint
  , exactSegment
  , exactSegmentEndpoints
  , exactVectorFromPoints
  )
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( consecutivePairs
  , orderedPair
  , unorderedPairs
  )
import Moonlight.Triangulation.Internal.ExactRational
  ( ExactRational
  , exactDivide
  , exactRationalIsZero
  )
import Moonlight.Triangulation.Internal.ExactSegmentEvents
  ( ExactSegmentEvent (..)
  , ExactSegmentEventPlan
  , ExactSweepSegmentId (..)
  , exactSegmentEventPlan
  , exactSegmentEvents
  , exactSegmentPairChecks
  , exactSegmentRelationMap
  , exactSegmentSplitPoints
  , exactSegmentSweepMaximumHeight
  )
import Moonlight.Triangulation.Embedding
  ( DraftId (..)
  , DraftIncidence (..)
  , DraftNeighborhood (..)
  , DraftReference (..)
  , DraftVertexId
  , ExactArrangementDraft (..)
  , LocalEmbeddingCertificate
  , certificateRoundedVertices
  , OverlayEmbeddingObstruction (..)
  , certifyLocalEmbedding
  )
import Moonlight.Triangulation.Internal.Overlay.Types
import Moonlight.Triangulation.Internal.Types
  ( HasPosition (..)
  , Point
  , queryPointValue
  )
import Moonlight.Triangulation.Region
  ( ExactLoop
  , PlanarLayer
  , PolygonComponent
  , exactLoopPoints
  , planarLayerOutsideLabel
  , planarLayerRegions
  , planarRegionComponents
  , polygonHoleLoops
  , polygonOuterLoop
  )

type ExactEdgeKey = (ExactPoint, ExactPoint)

data SourceBoundary leftLabel rightLabel
  = LeftSourceBoundary
      !ExactSegment
      !leftLabel
      !(BoundaryVertexRef 'LeftOverlayOperand)
      !(BoundaryVertexRef 'LeftOverlayOperand)
      !(BoundaryEdgeRef 'LeftOverlayOperand)
  | RightSourceBoundary
      !ExactSegment
      !rightLabel
      !(BoundaryVertexRef 'RightOverlayOperand)
      !(BoundaryVertexRef 'RightOverlayOperand)
      !(BoundaryEdgeRef 'RightOverlayOperand)

type SourceBoundaryConstructor operand label leftLabel rightLabel =
  ExactSegment
  -> label
  -> BoundaryVertexRef operand
  -> BoundaryVertexRef operand
  -> BoundaryEdgeRef operand
  -> SourceBoundary leftLabel rightLabel

data AtomicContribution leftLabel rightLabel =
  AtomicContribution !(SourceBoundary leftLabel rightLabel) !Bool

data AtomicEdge leftLabel rightLabel = AtomicEdge
  { atomicEdgeSegment :: !ExactSegment
  , atomicEdgeOrigin :: !OverlayEdgeOrigin
  , atomicEdgeLeftTransition :: !(Maybe (leftLabel, leftLabel))
  , atomicEdgeRightTransition :: !(Maybe (rightLabel, rightLabel))
  }

data OriginAccumulation = OriginAccumulation
  { accumulatedLeftVertices :: !(Set (BoundaryVertexRef 'LeftOverlayOperand))
  , accumulatedRightVertices :: !(Set (BoundaryVertexRef 'RightOverlayOperand))
  , accumulatedLeftEdges :: !(Set (BoundaryEdgeRef 'LeftOverlayOperand))
  , accumulatedRightEdges :: !(Set (BoundaryEdgeRef 'RightOverlayOperand))
  }

data OverlayVertexSeed = OverlayVertexSeed
  { seedExactPoint :: !ExactPoint
  , seedEmbeddedPoint :: !Point
  , seedOrigin :: !OverlayVertexOrigin
  }
  deriving stock (Eq, Ord, Show, Generic)

instance HasPosition OverlayVertexSeed where
  position = seedEmbeddedPoint

data ArrangementMetrics = ArrangementMetrics
  { arrangementInputSegments :: !Int
  , arrangementRelationEvents :: !Int
  , arrangementExactCrossings :: !Int
  , arrangementOverlapIntervals :: !Int
  , arrangementEmbeddingCandidates :: !Int
  , arrangementTotalRelationChecks :: !Int
  , arrangementSweepMaximumHeight :: !Int
  }

data CertifiedArrangement leftLabel rightLabel = CertifiedArrangement
  { certifiedAtomicEdges :: !(V.Vector (AtomicEdge leftLabel rightLabel))
  , certifiedVertexSeeds :: !(V.Vector OverlayVertexSeed)
  , certifiedConstraints :: !(V.Vector (Int, Int))
  , certifiedInexactEmbeddingPoints :: !(Set ExactPoint)
  , certifiedMetrics :: !ArrangementMetrics
  }

certifyArrangement
  :: (Ord leftLabel, Ord rightLabel)
  => PlanarLayer leftLabel
  -> PlanarLayer rightLabel
  -> Either
      (OverlayError leftLabel rightLabel)
      (CertifiedArrangement leftLabel rightLabel)
certifyArrangement leftLayer rightLayer = do
  sources <- flattenLayers leftLayer rightLayer
  sourcePlan <-
    first OverlaySegmentEventsInvalid
      (exactSegmentEventPlan (V.map sourceExactSegment sources))
  atomicEdges <- normalizeAtomicEdges leftLayer rightLayer sources sourcePlan
  let atomicVector = V.fromList atomicEdges
      pointIds = exactPointIds atomicVector
      origins = vertexOrigins sources atomicVector
      atomicRelations = atomicEndpointRelations atomicVector
  draft <- exactArrangementDraft sources sourcePlan atomicVector pointIds atomicRelations
  localCertificate <- first OverlayEmbeddingRefused (certifyLocalEmbedding draft)
  let projectedPoints = projectedExactPoints localCertificate
      inexactEmbeddingPoints =
        Set.fromList
          [ exactPoint
          | (exactPoint, vertexId) <- Map.toAscList pointIds
          , Map.lookup vertexId projectedPoints /= Just exactPoint
          ]
  projectedSegments <- projectedAtomicSegments atomicVector pointIds projectedPoints
  projectedPlan <-
    first OverlaySegmentEventsInvalid
      (exactSegmentEventPlan projectedSegments)
  dischargeGlobalRelations atomicRelations projectedPlan
  seeds <- overlayVertexSeeds pointIds origins localCertificate
  constraints <- atomicConstraints pointIds atomicVector
  let events = exactSegmentEvents sourcePlan
  pure
    CertifiedArrangement
      { certifiedAtomicEdges = atomicVector
      , certifiedVertexSeeds = seeds
      , certifiedConstraints = constraints
      , certifiedInexactEmbeddingPoints = inexactEmbeddingPoints
      , certifiedMetrics =
          ArrangementMetrics
            { arrangementInputSegments = V.length sources
            , arrangementRelationEvents = length events
            , arrangementExactCrossings =
                length [() | ExactProperCrossing {} <- events]
            , arrangementOverlapIntervals =
                length [() | ExactCollinearOverlap {} <- events]
            , arrangementEmbeddingCandidates = Map.size pointIds
            , arrangementTotalRelationChecks =
                exactSegmentPairChecks sourcePlan
                  + exactSegmentPairChecks projectedPlan
            , arrangementSweepMaximumHeight =
                max
                  (exactSegmentSweepMaximumHeight sourcePlan)
                  (exactSegmentSweepMaximumHeight projectedPlan)
            }
      }
flattenLayers
  :: PlanarLayer leftLabel
  -> PlanarLayer rightLabel
  -> Either (OverlayError leftLabel rightLabel) (V.Vector (SourceBoundary leftLabel rightLabel))
flattenLayers leftLayer rightLayer = do
  leftSources <- flattenLayer LeftSourceBoundary leftLayer
  rightSources <- flattenLayer RightSourceBoundary rightLayer
  pure (V.fromList (leftSources <> rightSources))

flattenLayer
  :: SourceBoundaryConstructor operand label leftLabel rightLabel
  -> PlanarLayer label
  -> Either (OverlayError leftLabel rightLabel) [SourceBoundary leftLabel rightLabel]
flattenLayer makeBoundary layer =
  fmap concat
    ( traverse
        (\(componentIndex, (label, component)) ->
           flattenComponent makeBoundary componentIndex label component)
        (zip [0 ..] (labelledComponents layer))
    )
{-# INLINE flattenLayer #-}

labelledComponents :: PlanarLayer label -> [(label, PolygonComponent)]
labelledComponents layer =
  [ (label, component)
  | (label, region) <- Map.toAscList (planarLayerRegions layer)
  , component <- planarRegionComponents region
  ]

flattenComponent
  :: SourceBoundaryConstructor operand label leftLabel rightLabel
  -> Int
  -> label
  -> PolygonComponent
  -> Either (OverlayError leftLabel rightLabel) [SourceBoundary leftLabel rightLabel]
flattenComponent makeBoundary componentIndex label component = do
  outer <-
    flattenLoop
      makeBoundary
      componentIndex
      BoundaryOuterLoop
      label
      (polygonOuterLoop component)
  holes <-
    fmap concat
      ( traverse
          (\(holeIndex, loop) ->
             flattenLoop
               makeBoundary
               componentIndex
               (BoundaryHoleLoop holeIndex)
               label
               loop)
          (zip [0 ..] (polygonHoleLoops component))
      )
  pure (outer <> holes)
{-# INLINE flattenComponent #-}

flattenLoop
  :: SourceBoundaryConstructor operand label leftLabel rightLabel
  -> Int
  -> BoundaryLoopRef
  -> label
  -> ExactLoop
  -> Either (OverlayError leftLabel rightLabel) [SourceBoundary leftLabel rightLabel]
flattenLoop makeBoundary componentIndex loopRef label loop =
  traverse constructSource (indexedCycle (exactLoopPoints loop))
 where
  constructSource (edgeIndex, fromIndex, from, toIndex, to) = do
    segment <- first (sourceGeometryError from) (exactSegment from to)
    pure
      ( makeBoundary
          segment
          label
          (BoundaryRef componentIndex loopRef fromIndex)
          (BoundaryRef componentIndex loopRef toIndex)
          (BoundaryRef componentIndex loopRef edgeIndex)
      )
{-# INLINE flattenLoop #-}

sourceGeometryError
  :: ExactPoint
  -> ExactGeometryError
  -> OverlayError leftLabel rightLabel
sourceGeometryError point _ = OverlayArrangementInvalid (OverlayRotationDegenerate point)

indexedCycle :: NonEmpty value -> [(Int, Int, value, Int, value)]
indexedCycle (firstValue :| remaining) =
  let values = firstValue : remaining
      count = length values
   in [ (index, index, from, (index + 1) `mod` count, to)
      | (index, (from, to)) <- zip [0 ..] (zip values (remaining <> [firstValue]))
      ]

sourceExactSegment :: SourceBoundary leftLabel rightLabel -> ExactSegment
sourceExactSegment (LeftSourceBoundary segment _ _ _ _) = segment
sourceExactSegment (RightSourceBoundary segment _ _ _ _) = segment

normalizeAtomicEdges
  :: (Ord leftLabel, Ord rightLabel)
  => PlanarLayer leftLabel
  -> PlanarLayer rightLabel
  -> V.Vector (SourceBoundary leftLabel rightLabel)
  -> ExactSegmentEventPlan
  -> Either (OverlayError leftLabel rightLabel) [AtomicEdge leftLabel rightLabel]
normalizeAtomicEdges leftLayer rightLayer sources plan =
  fmap concat
    ( traverse
        (resolveAtomicContributions leftOutside rightOutside)
        (Map.toAscList grouped)
    )
 where
  leftOutside = planarLayerOutsideLabel leftLayer
  rightOutside = planarLayerOutsideLabel rightLayer
  grouped =
    V.ifoldl'
      (\groups sourceIndex source ->
         List.foldl'
           (insertAtomic source)
           groups
           (consecutivePairs (exactSegmentSplitPoints plan (ExactSweepSegmentId sourceIndex))))
      Map.empty
      sources
  insertAtomic
    :: SourceBoundary leftLabel' rightLabel'
    -> Map ExactEdgeKey [AtomicContribution leftLabel' rightLabel']
    -> (ExactPoint, ExactPoint)
    -> Map ExactEdgeKey [AtomicContribution leftLabel' rightLabel']
  insertAtomic source groups (from, to)
    | from == to = groups
    | otherwise =
        let key@(canonicalFrom, _) = canonicalEdgeKey from to
            contribution = AtomicContribution source (from == canonicalFrom)
         in Map.insertWith (<>) key [contribution] groups

resolveAtomicContributions
  :: (Ord leftLabel, Ord rightLabel)
  => leftLabel
  -> rightLabel
  -> (ExactEdgeKey, [AtomicContribution leftLabel rightLabel])
  -> Either (OverlayError leftLabel rightLabel) [AtomicEdge leftLabel rightLabel]
resolveAtomicContributions leftOutside rightOutside ((from, to), contributions) = do
  let
      ( leftLabelsOnLeft
        , leftLabelsOnRight
        , rightLabelsOnLeft
        , rightLabelsOnRight
        , leftSources
        , rightSources
        ) =
          List.foldl'
            collectContribution
            (Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty)
            contributions
      origin =
        OverlayEdgeOrigin
          (Set.toAscList leftSources)
          (Set.toAscList rightSources)
  leftTransition <-
    resolveTransition
      OverlayLeftSourceSideConflict
      origin
      leftOutside
      leftLabelsOnLeft
      leftLabelsOnRight
  rightTransition <-
    resolveTransition
      OverlayRightSourceSideConflict
      origin
      rightOutside
      rightLabelsOnLeft
      rightLabelsOnRight
  if transitionIsIdentity leftTransition && transitionIsIdentity rightTransition
    then Right []
    else do
      segment <- first (sourceGeometryError from) (exactSegment from to)
      Right
        [ AtomicEdge
            { atomicEdgeSegment = segment
            , atomicEdgeOrigin = origin
            , atomicEdgeLeftTransition = leftTransition
            , atomicEdgeRightTransition = rightTransition
            }
        ]
 where
  collectContribution
    :: (Ord leftLabel', Ord rightLabel')
    => ( Set leftLabel'
       , Set leftLabel'
       , Set rightLabel'
       , Set rightLabel'
       , Set (BoundaryEdgeRef 'LeftOverlayOperand)
       , Set (BoundaryEdgeRef 'RightOverlayOperand)
       )
    -> AtomicContribution leftLabel' rightLabel'
    -> ( Set leftLabel'
       , Set leftLabel'
       , Set rightLabel'
       , Set rightLabel'
       , Set (BoundaryEdgeRef 'LeftOverlayOperand)
       , Set (BoundaryEdgeRef 'RightOverlayOperand)
       )
  collectContribution
    ( !leftLeft
      , !leftRight
      , !rightLeft
      , !rightRight
      , !leftSources
      , !rightSources
      )
    contribution =
    case contribution of
      AtomicContribution (LeftSourceBoundary _ label _ _ reference) follows ->
        ( if follows then Set.insert label leftLeft else leftLeft
        , if follows then leftRight else Set.insert label leftRight
        , rightLeft
        , rightRight
        , Set.insert reference leftSources
        , rightSources
        )
      AtomicContribution (RightSourceBoundary _ label _ _ reference) follows ->
        ( leftLeft
        , leftRight
        , if follows then Set.insert label rightLeft else rightLeft
        , if follows then rightRight else Set.insert label rightRight
        , leftSources
        , Set.insert reference rightSources
        )

resolveTransition
  :: (OverlayEdgeOrigin -> NonEmpty label -> OverlayArrangementObstruction leftLabel rightLabel)
  -> OverlayEdgeOrigin
  -> label
  -> Set label
  -> Set label
  -> Either (OverlayError leftLabel rightLabel) (Maybe (label, label))
resolveTransition sideConflict origin outside labelsOnLeft labelsOnRight
  | Set.null labelsOnLeft && Set.null labelsOnRight = Right Nothing
  | otherwise =
      Just
        <$> ((,)
               <$> resolveSide sideConflict origin outside labelsOnLeft
               <*> resolveSide sideConflict origin outside labelsOnRight)
{-# INLINE resolveTransition #-}

transitionIsIdentity :: Eq label => Maybe (label, label) -> Bool
transitionIsIdentity Nothing = True
transitionIsIdentity (Just (leftLabel, rightLabel)) = leftLabel == rightLabel

resolveSide
  :: (OverlayEdgeOrigin -> NonEmpty label -> OverlayArrangementObstruction leftLabel rightLabel)
  -> OverlayEdgeOrigin
  -> label
  -> Set label
  -> Either (OverlayError leftLabel rightLabel) label
resolveSide sideConflict origin outside labels =
  case Set.toAscList labels of
    [] -> Right outside
    [label] -> Right label
    firstLabel : remaining ->
      Left
        ( OverlayArrangementInvalid
            (sideConflict origin (firstLabel :| remaining))
        )
{-# INLINE resolveSide #-}

exactPointIds
  :: V.Vector (AtomicEdge leftLabel rightLabel)
  -> Map ExactPoint DraftVertexId
exactPointIds edges =
  Map.fromAscList
    ( zip
        (Set.toAscList (V.foldl' collect Set.empty edges))
      (map DraftId [0 ..])
    )
 where
  collect
    :: Set ExactPoint
    -> AtomicEdge leftLabel rightLabel
    -> Set ExactPoint
  collect points edge = Set.insert (atomicEdgeFrom edge) (Set.insert (atomicEdgeTo edge) points)

vertexOrigins
  :: V.Vector (SourceBoundary leftLabel rightLabel)
  -> V.Vector (AtomicEdge leftLabel rightLabel)
  -> Map ExactPoint OverlayVertexOrigin
vertexOrigins sources atomicEdges =
  Map.map finalizeOrigin
    ( V.foldl'
        addAtomicOrigin
        (V.foldl' addSourceEndpoints Map.empty sources)
        atomicEdges
    )

emptyOrigin :: OriginAccumulation
emptyOrigin = OriginAccumulation Set.empty Set.empty Set.empty Set.empty

mergeOrigin :: OriginAccumulation -> OriginAccumulation -> OriginAccumulation
mergeOrigin left right =
  OriginAccumulation
    { accumulatedLeftVertices = accumulatedLeftVertices left <> accumulatedLeftVertices right
    , accumulatedRightVertices = accumulatedRightVertices left <> accumulatedRightVertices right
    , accumulatedLeftEdges = accumulatedLeftEdges left <> accumulatedLeftEdges right
    , accumulatedRightEdges = accumulatedRightEdges left <> accumulatedRightEdges right
    }

addSourceEndpoints
  :: Map ExactPoint OriginAccumulation
  -> SourceBoundary leftLabel rightLabel
  -> Map ExactPoint OriginAccumulation
addSourceEndpoints origins source =
  case source of
    LeftSourceBoundary segment _ fromReference toReference _ ->
      let (from, to) = exactSegmentEndpoints segment
       in insertOrigin to (emptyOrigin{accumulatedLeftVertices = Set.singleton toReference})
            (insertOrigin from (emptyOrigin{accumulatedLeftVertices = Set.singleton fromReference}) origins)
    RightSourceBoundary segment _ fromReference toReference _ ->
      let (from, to) = exactSegmentEndpoints segment
       in insertOrigin to (emptyOrigin{accumulatedRightVertices = Set.singleton toReference})
            (insertOrigin from (emptyOrigin{accumulatedRightVertices = Set.singleton fromReference}) origins)

addAtomicOrigin
  :: Map ExactPoint OriginAccumulation
  -> AtomicEdge leftLabel rightLabel
  -> Map ExactPoint OriginAccumulation
addAtomicOrigin origins edge =
  let origin = atomicEdgeOrigin edge
      accumulation =
        emptyOrigin
          { accumulatedLeftEdges = Set.fromList (overlayEdgeLeftSources origin)
          , accumulatedRightEdges = Set.fromList (overlayEdgeRightSources origin)
          }
   in insertOrigin (atomicEdgeTo edge) accumulation
        (insertOrigin (atomicEdgeFrom edge) accumulation origins)

insertOrigin
  :: ExactPoint
  -> OriginAccumulation
  -> Map ExactPoint OriginAccumulation
  -> Map ExactPoint OriginAccumulation
insertOrigin = Map.insertWith mergeOrigin

finalizeOrigin :: OriginAccumulation -> OverlayVertexOrigin
finalizeOrigin accumulated =
  OverlayVertexOrigin
    { overlayOriginLeftVertices = Set.toAscList (accumulatedLeftVertices accumulated)
    , overlayOriginRightVertices = Set.toAscList (accumulatedRightVertices accumulated)
    , overlayOriginLeftEdges = Set.toAscList (accumulatedLeftEdges accumulated)
    , overlayOriginRightEdges = Set.toAscList (accumulatedRightEdges accumulated)
    }

exactArrangementDraft
  :: V.Vector (SourceBoundary leftLabel rightLabel)
  -> ExactSegmentEventPlan
  -> V.Vector (AtomicEdge leftLabel rightLabel)
  -> Map ExactPoint DraftVertexId
  -> Map (ExactSweepSegmentId, ExactSweepSegmentId) SegmentRelation
  -> Either (OverlayError leftLabel rightLabel) ExactArrangementDraft
exactArrangementDraft sources sourcePlan atomicEdges pointIds atomicRelations = do
  draftSegmentsMap <-
    Map.fromList
      <$> traverse
        (\(segmentIndex, edge) -> do
           from <- requireDraftVertex pointIds (atomicEdgeFrom edge)
           to <- requireDraftVertex pointIds (atomicEdgeTo edge)
           pure (DraftId segmentIndex, (from, to)))
        (V.toList (V.indexed atomicEdges))
  memberships <-
    Map.fromList
      <$> traverse
        (\(sourceIndex, source) -> do
           values <-
             traverse
               (\point -> do
                  parameter <- exactSourceParameter source point
                  vertex <- requireDraftVertex pointIds point
                  pure (parameter, vertex))
               (exactSegmentSplitPoints sourcePlan (ExactSweepSegmentId sourceIndex))
           pure (DraftId sourceIndex, values))
        (V.toList (V.indexed sources))
  neighborhoods <- buildDraftNeighborhoods atomicEdges pointIds
  let incidences =
        [ DraftIncidence
            (DraftId leftIndex)
            (DraftId rightIndex)
            relation
        | ((ExactSweepSegmentId leftIndex, ExactSweepSegmentId rightIndex), relation) <-
            Map.toAscList atomicRelations
        ]
  pure
    ExactArrangementDraft
      { draftVertices = Map.fromList [(vertexId, point) | (point, vertexId) <- Map.toAscList pointIds]
      , draftSegments = draftSegmentsMap
      , draftSourceMemberships = memberships
      , draftIncidences = incidences
      , draftNeighborhoods = neighborhoods
      }

requireDraftVertex
  :: Map ExactPoint DraftVertexId
  -> ExactPoint
  -> Either (OverlayError leftLabel rightLabel) DraftVertexId
requireDraftVertex pointIds point =
  case Map.lookup point pointIds of
    Just vertex -> Right vertex
    Nothing -> Left (OverlayProvenanceIncomplete (OverlayExactVertexMissing point))

exactSourceParameter
  :: SourceBoundary leftLabel rightLabel
  -> ExactPoint
  -> Either (OverlayError leftLabel rightLabel) ExactRational
exactSourceParameter source point =
  let (from, to) = exactSegmentEndpoints (sourceExactSegment source)
      (fromX, fromY) = exactPointCoordinates from
      (toX, toY) = exactPointCoordinates to
      (pointX, pointY) = exactPointCoordinates point
      (numerator, denominator) =
        if exactRationalIsZero (toX - fromX)
          then (pointY - fromY, toY - fromY)
          else (pointX - fromX, toX - fromX)
   in first OverlayExactArithmetic (exactDivide numerator denominator)

buildDraftNeighborhoods
  :: V.Vector (AtomicEdge leftLabel rightLabel)
  -> Map ExactPoint DraftVertexId
  -> Either (OverlayError leftLabel rightLabel) [DraftNeighborhood]
buildDraftNeighborhoods edges pointIds =
  traverse neighborhood (Map.toAscList pointIds)
 where
  neighborhood (centerPoint, centerId) = do
    let neighbors = Map.findWithDefault Set.empty centerPoint adjacency
    orderedPoints <-
      case NonEmpty.nonEmpty (sortBy (compareAround centerPoint) (Set.toList neighbors)) of
        Just points -> Right points
        Nothing -> Left (OverlayArrangementInvalid (OverlayRotationDegenerate centerPoint))
    orderedIds <- traverse (requireDraftVertex pointIds) orderedPoints
    pure (DraftNeighborhood centerId orderedIds)
  adjacency =
    V.foldl'
      (\graph edge ->
         Map.insertWith Set.union (atomicEdgeTo edge) (Set.singleton (atomicEdgeFrom edge))
           (Map.insertWith Set.union (atomicEdgeFrom edge) (Set.singleton (atomicEdgeTo edge)) graph))
      Map.empty
      edges

compareAround :: ExactPoint -> ExactPoint -> ExactPoint -> Ordering
compareAround center left right =
  case
    compareExactVectorAngle
      (exactVectorFromPoints center left)
      (exactVectorFromPoints center right) of
    EQ -> compare left right
    ordering -> ordering

-- | Normalization splits every source at every exact event and coalesces
-- duplicate intervals. Distinct atomics can therefore meet only at a stored
-- endpoint; their entire relation section is the endpoint-incidence index.
atomicEndpointRelations
  :: V.Vector (AtomicEdge leftLabel rightLabel)
  -> Map (ExactSweepSegmentId, ExactSweepSegmentId) SegmentRelation
atomicEndpointRelations edges =
  Map.fromList
    [ ( orderedPair
          (ExactSweepSegmentId leftIndex)
          (ExactSweepSegmentId rightIndex)
      , SegmentsShareEndpoint
      )
    | incident <- Map.elems incidenceByPoint
    , (leftIndex, rightIndex) <- unorderedPairs (Set.toAscList incident)
    ]
 where
  incidenceByPoint =
    V.ifoldl'
      (\incidence index edge ->
         Map.insertWith Set.union (atomicEdgeTo edge) (Set.singleton index)
           ( Map.insertWith Set.union
               (atomicEdgeFrom edge)
               (Set.singleton index)
               incidence
           ))
      Map.empty
      edges
projectedExactPoints
  :: LocalEmbeddingCertificate
  -> Map DraftVertexId ExactPoint
projectedExactPoints =
  Map.map exactPointFromQueryPoint . certificateRoundedVertices

projectedAtomicSegments
  :: V.Vector (AtomicEdge leftLabel rightLabel)
  -> Map ExactPoint DraftVertexId
  -> Map DraftVertexId ExactPoint
  -> Either (OverlayError leftLabel rightLabel) (V.Vector ExactSegment)
projectedAtomicSegments edges pointIds projected =
  V.imapM project edges
 where
  project segmentIndex edge = do
    fromId <- requireDraftVertex pointIds (atomicEdgeFrom edge)
    toId <- requireDraftVertex pointIds (atomicEdgeTo edge)
    from <- requireProjected fromId
    to <- requireProjected toId
    first
      (\_ ->
         OverlayEmbeddingRefused
           ( ProjectedSegmentCollapsed
               (DraftId segmentIndex)
               fromId
               toId
               :| []
           ))
      (exactSegment from to)
  requireProjected vertex =
    case Map.lookup vertex projected of
      Just point -> Right point
      Nothing ->
        Left
          ( OverlayEmbeddingRefused
              (DraftReferenceMissing (DraftVertexReference vertex) :| [])
          )

dischargeGlobalRelations
  :: Map (ExactSweepSegmentId, ExactSweepSegmentId) SegmentRelation
  -> ExactSegmentEventPlan
  -> Either (OverlayError leftLabel rightLabel) ()
dischargeGlobalRelations exactRelations projectedPlan =
  case NonEmpty.nonEmpty obstructions of
    Nothing -> Right ()
    Just failures -> Left (OverlayEmbeddingRefused failures)
 where
  projectedRelations = exactSegmentRelationMap projectedPlan
  keys = Set.toAscList (Map.keysSet exactRelations <> Map.keysSet projectedRelations)
  obstructions = concatMap compareRelation keys
  compareRelation key@(ExactSweepSegmentId leftId, ExactSweepSegmentId rightId) =
    case (Map.lookup key exactRelations, Map.lookup key projectedRelations) of
      (Nothing, Just projected) ->
        [GlobalRelationAdded (DraftId leftId) (DraftId rightId) projected]
      (Just exact, Nothing) ->
        [GlobalRelationRemoved (DraftId leftId) (DraftId rightId) exact]
      (Just exact, Just projected)
        | exact /= projected ->
            [GlobalRelationChanged (DraftId leftId) (DraftId rightId) exact projected]
      _ -> []

overlayVertexSeeds
  :: Map ExactPoint DraftVertexId
  -> Map ExactPoint OverlayVertexOrigin
  -> LocalEmbeddingCertificate
  -> Either (OverlayError leftLabel rightLabel) (V.Vector OverlayVertexSeed)
overlayVertexSeeds pointIds origins certificate =
  V.fromList
    <$> traverse
      (\(point, vertexId) -> do
         embedded <-
           case Map.lookup vertexId (certificateRoundedVertices certificate) of
             Just value -> Right value
             Nothing ->
               Left
                 ( OverlayEmbeddingRefused
                     (DraftReferenceMissing (DraftVertexReference vertexId) :| [])
                 )
         origin <-
           case Map.lookup point origins of
             Just value -> Right value
             Nothing -> Left (OverlayProvenanceIncomplete (OverlayExactVertexMissing point))
         pure (OverlayVertexSeed point (queryPointValue embedded) origin))
      (Map.toAscList pointIds)

atomicConstraints
  :: Map ExactPoint DraftVertexId
  -> V.Vector (AtomicEdge leftLabel rightLabel)
  -> Either (OverlayError leftLabel rightLabel) (V.Vector (Int, Int))
atomicConstraints pointIds =
  V.mapM
    (\edge -> do
       DraftId from <- requireDraftVertex pointIds (atomicEdgeFrom edge)
       DraftId to <- requireDraftVertex pointIds (atomicEdgeTo edge)
       pure (from, to))


canonicalEdgeKey :: ExactPoint -> ExactPoint -> ExactEdgeKey
canonicalEdgeKey = orderedPair

atomicEdgeFrom :: AtomicEdge leftLabel rightLabel -> ExactPoint
atomicEdgeFrom = fst . exactSegmentEndpoints . atomicEdgeSegment

atomicEdgeTo :: AtomicEdge leftLabel rightLabel -> ExactPoint
atomicEdgeTo = snd . exactSegmentEndpoints . atomicEdgeSegment

atomicKey :: AtomicEdge leftLabel rightLabel -> ExactEdgeKey
atomicKey edge = (atomicEdgeFrom edge, atomicEdgeTo edge)

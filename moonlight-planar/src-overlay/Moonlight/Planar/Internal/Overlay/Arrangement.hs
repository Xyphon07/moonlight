{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

-- | One exact source-family arrangement. The event owner splits and merges
-- source supports; exact rotations descend to boundary orbits, containment
-- glues those orbits into faces, and sparse source transitions label the
-- resulting incidence. No binary64 realization participates in construction.
module Moonlight.Planar.Internal.Overlay.Arrangement
  ( certifyArrangement
  , overlayFaceInteriorPoint
  , canonicalEdgeKey
  , compareAround
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)
import Moonlight.Planar.Exact
  ( ExactBounds
  , exactPointsBounds
  , pointInBounds
  , ExactPoint
  , ExactSegment
  , ExactVector (..)
  , compareExactVectorAngle
  , exactCross
  , exactOnClosedSegment
  , exactOrient2d
  , exactPoint
  , exactPointCoordinates
  , exactPointCross
  , exactSegment
  , exactSegmentEndpoints
  , exactVectorFromPoints
  , translateExactPoint
  )
import Moonlight.Planar.Internal.BoundaryCycle
  ( consecutivePairs
  , cyclePairs
  , orderedPair
  )
import Moonlight.Planar.Internal.ExactRational
  ( ExactRational
  , exactDivide
  , exactRationalFromDyadic
  )
import Moonlight.Planar.Internal.ExactSegmentEvents
  ( ExactSegmentEvent (..)
  , ExactSegmentEventPlan
  , ExactSweepSegmentId (..)
  , exactSegmentEventPlan
  , exactSegmentEvents
  , exactSegmentPairChecks
  , exactSegmentSplitPoints
  , exactSegmentSweepMaximumHeight
  )
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId (..)
  , FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , directedPair
  , faceIdIndex
  , reverseEdge
  , vertexIdIndex
  )
import Moonlight.Planar.Internal.Incidence
  ( PlanarIncidence
  , admitPlanarIncidence
  , faceBoundaryComponents
  , incidenceDestination
  , incidenceEdgeCount
  , incidenceFaceCount
  , incidenceOrigin
  , incidenceUndirectedEndpoints
  , incidenceVertexOutEdge
  )
import Moonlight.Planar.Internal.Overlay.Types
import Moonlight.Planar.Internal.PackedIndex (indexLimit, noIndex)
import Moonlight.Planar.Internal.Paged (fromVector)
import Moonlight.Planar.Region
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

data SourceBoundary = SourceBoundary
  { sourceSegment :: !ExactSegment
  , sourceLabel :: !OverlayLabelId
  , sourceFromRef :: !BoundaryVertexRef
  , sourceToRef :: !BoundaryVertexRef
  , sourceEdgeRef :: !BoundaryEdgeRef
  }

data AtomicContribution = AtomicContribution !SourceBoundary !Bool

data AtomicEdge = AtomicEdge
  { atomicSegment :: !ExactSegment
  , atomicOrigin :: !OverlayEdgeOrigin
  , atomicTransitions :: !(IntMap (OverlayLabelId, OverlayLabelId))
  }

data SourceSides = SourceSides !(Set OverlayLabelId) !(Set OverlayLabelId)

instance Semigroup SourceSides where
  SourceSides leftBefore rightBefore <> SourceSides leftAfter rightAfter =
    SourceSides (leftBefore <> leftAfter) (rightBefore <> rightAfter)

data OriginAccumulation = OriginAccumulation !(Set BoundaryVertexRef) !(Set BoundaryEdgeRef)

instance Semigroup OriginAccumulation where
  OriginAccumulation beforeVertices beforeEdges <> OriginAccumulation afterVertices afterEdges =
    OriginAccumulation (beforeVertices <> afterVertices) (beforeEdges <> afterEdges)

data ArrangementDart = ArrangementDart
  { dartHandle :: !DirectedEdgeId
  , dartOrigin :: !VertexId
  , dartFrom :: !ExactPoint
  , dartTo :: !ExactPoint
  }

-- | A rotation-permutation orbit, not an admitted polygon loop. A bridge is
-- present twice with opposite orientation; a cut vertex may occur repeatedly.
data BoundaryOrbit = BoundaryOrbit
  { orbitDarts :: !(NonEmpty ArrangementDart)
  , orbitDoubleArea :: !ExactRational
  , orbitBounds :: !ExactBounds
  }

data FaceTransition = FaceTransition
  { transitionEdge :: !UndirectedEdgeId
  , transitionTarget :: !FaceId
  , transitionSources :: !(IntMap (OverlayLabelId, OverlayLabelId))
  }

data FaceLabelling = FaceLabelling
  { assignedFaceLabels :: !(Map FaceId (IntMap OverlayLabelId))
  , pendingLabelFaces :: !(Seq.Seq FaceId)
  }

outsideLabel :: OverlayLabelId
outsideLabel = OverlayLabelId 0

outerFace :: FaceId
outerFace = FaceId 0

oneHalf :: ExactRational
oneHalf = exactRationalFromDyadic 1 (-1)

-- | Application labels have already been registered into source-local IDs.
-- One shared event plan handles binary and n-ary requests alike; the labels
-- returned here are sparse source states with outside entries omitted.
certifyArrangement
  :: NonEmpty (PlanarLayer OverlayLabelId)
  -> Either OverlayError (OverlayResult (IntMap OverlayLabelId))
certifyArrangement layers = do
  sources <- flattenLayers layers
  sourcePlan <-
    first OverlaySegmentEventsInvalid
      (exactSegmentEventPlan (V.map sourceSegment sources))
  atomics <- normalizeAtomicEdges sources sourcePlan
  let origins = vertexOrigins sources sourcePlan
      pointIds =
        Map.fromAscList
          (zip (Map.keys origins) (map (VertexId . fromIntegral) [0 :: Int ..]))
      vertices = V.fromList (map publishVertex (Map.toAscList origins))
  admitCapacity (Map.size pointIds) (V.length atomics) 1
  darts <- arrangementDarts pointIds atomics
  let outgoing = outgoingDarts darts
      successors = rotationSuccessors outgoing
      predecessors = Map.fromList [(next, previous) | (previous, next) <- Map.toAscList successors]
      isolatedPoints =
        [(point, vertex) | (point, vertex) <- Map.toAscList pointIds, Map.notMember vertex outgoing]
  orbits <- boundaryOrbits darts successors
  let positiveOrbits = filter ((> 0) . orbitDoubleArea) orbits
      anchors = zip (map (FaceId . fromIntegral) [1 :: Int ..]) positiveOrbits
      faceCount = length anchors + 1
      anchorByRoot = Map.fromList [(orbitRoot orbit, face) | (face, orbit) <- anchors]
  admitCapacity (Map.size pointIds) (V.length atomics) faceCount
  let assignedOrbits = map (assignOrbit anchors anchorByRoot) orbits
      dartFaces =
        Map.fromList
          [ (dartHandle dart, face)
          | (face, orbit) <- assignedOrbits
          , dart <- NonEmpty.toList (orbitDarts orbit)
          ]
      groupedOrbits =
        Map.fromListWith (<>)
          [(face, [orbit]) | (face, orbit) <- assignedOrbits]
      isolated =
        IntMap.fromListWith IntSet.union
          [ (faceIdIndex (containingFace anchors point), IntSet.singleton (vertexIdIndex vertex))
          | (point, vertex) <- isolatedPoints
          ]
  incidence <-
    assembleIncidence
      pointIds darts outgoing successors predecessors dartFaces
      anchors groupedOrbits isolated
  adjacency <- faceTransitions atomics dartFaces
  labels <- labelFaces faceCount adjacency
  let events = exactSegmentEvents sourcePlan
      edgeOrigins = V.map atomicOrigin atomics
      receipt =
        OverlayReceipt
          { overlayInputSources = NonEmpty.length layers
          , overlayInputSegments = V.length sources
          , overlayRelationEvents = length events
          , overlayExactCrossings = length [() | ExactProperCrossing {} <- events]
          , overlayOverlapIntervals = length [() | ExactCollinearOverlap {} <- events]
          , overlayAtomicEdges = V.length atomics
          , overlayOutputVertices = V.length vertices
          , overlayArrangementCells = faceCount
          , overlayBoundaryOrbits = length orbits
          , overlayProvenanceIncidences =
              V.foldl' (\count origin -> count + NonEmpty.length (overlayEdgeSources origin)) 0 edgeOrigins
                + V.foldl' vertexProvenanceCount 0 vertices
          , overlayTotalRelationChecks = exactSegmentPairChecks sourcePlan
          , overlaySweepMaximumHeight = exactSegmentSweepMaximumHeight sourcePlan
          }
  pure
    OverlayResult
      { overlayResultIncidence = incidence
      , overlayResultVertices = vertices
      , overlayResultEdges = edgeOrigins
      , overlayResultLabels = labels
      , overlayResultReceipt = receipt
      }
 where
  publishVertex (point, OriginAccumulation vertexRefs edgeRefs) =
    OverlayVertex point (OverlayVertexOrigin (Set.toAscList vertexRefs) (Set.toAscList edgeRefs))
  vertexProvenanceCount count vertex =
    let origin = overlayVertexOrigin vertex
     in count + length (overlayOriginVertices origin) + length (overlayOriginEdges origin)

admitCapacity :: Int -> Int -> Int -> Either OverlayError ()
admitCapacity vertexCount edgeCount faceCount
  | toInteger vertexCount <= limit
      && 2 * toInteger edgeCount <= limit
      && toInteger faceCount <= limit = Right ()
  | otherwise =
      Left
        (OverlayArrangementInvalid (OverlayPackedCapacityExceeded vertexCount edgeCount faceCount))
 where
  limit = toInteger indexLimit

flattenLayers
  :: NonEmpty (PlanarLayer OverlayLabelId)
  -> Either OverlayError (V.Vector SourceBoundary)
flattenLayers layers =
  V.fromList . concat
    <$> traverse flattenSource (zip [0 ..] (NonEmpty.toList layers))
 where
  flattenSource (sourceIndex, layer) = do
    let source = OverlaySourceId sourceIndex
        outside = planarLayerOutsideLabel layer
    if outside == outsideLabel
      then Right ()
      else Left (OverlayArrangementInvalid (OverlaySourceOutsideLabelInvalid source outside))
    concat
      <$> traverse
        (\(componentIndex, (label, component)) -> flattenComponent source componentIndex label component)
        (zip [0 ..] (labelledComponents layer))

labelledComponents :: PlanarLayer label -> [(label, PolygonComponent)]
labelledComponents layer =
  [ (label, component)
  | (label, region) <- Map.toAscList (planarLayerRegions layer)
  , component <- planarRegionComponents region
  ]

flattenComponent
  :: OverlaySourceId
  -> Int
  -> OverlayLabelId
  -> PolygonComponent
  -> Either OverlayError [SourceBoundary]
flattenComponent source componentIndex label component =
  concat
    <$> traverse
      (uncurry (flattenLoop source componentIndex label))
      ( (BoundaryOuterLoop, polygonOuterLoop component)
          : zipWith (\index loop -> (BoundaryHoleLoop index, loop)) [0 ..] (polygonHoleLoops component)
      )

flattenLoop
  :: OverlaySourceId
  -> Int
  -> OverlayLabelId
  -> BoundaryLoopRef
  -> ExactLoop
  -> Either OverlayError [SourceBoundary]
flattenLoop source componentIndex label loopRef loop =
  traverse constructSource (cyclePairs indexedPoints)
 where
  indexedPoints = NonEmpty.zip (0 :| [1 ..]) (exactLoopPoints loop)
  reference :: Int -> BoundaryRef feature
  reference = BoundaryRef source componentIndex loopRef
  constructSource ((fromIndex, from), (toIndex, to)) = do
    segment <- first OverlayExactGeometryInvalid (exactSegment from to)
    pure
      SourceBoundary
        { sourceSegment = segment
        , sourceLabel = label
        , sourceFromRef = reference fromIndex
        , sourceToRef = reference toIndex
        , sourceEdgeRef = reference fromIndex
        }

normalizeAtomicEdges
  :: V.Vector SourceBoundary
  -> ExactSegmentEventPlan
  -> Either OverlayError (V.Vector AtomicEdge)
normalizeAtomicEdges sources plan =
  V.fromList . catMaybes
    <$> traverse resolveAtomicContributions (Map.toAscList grouped)
 where
  grouped =
    V.ifoldl'
      (\groups sourceIndex source ->
         List.foldl' (insertAtomic source) groups
           (consecutivePairs (exactSegmentSplitPoints plan (ExactSweepSegmentId sourceIndex))))
      Map.empty
      sources
  insertAtomic source groups (from, to)
    | from == to = groups
    | otherwise =
        let key@(canonicalFrom, _) = canonicalEdgeKey from to
         in Map.insertWith (<>) key
              (AtomicContribution source (from == canonicalFrom) :| []) groups

resolveAtomicContributions
  :: (ExactEdgeKey, NonEmpty AtomicContribution)
  -> Either OverlayError (Maybe AtomicEdge)
resolveAtomicContributions ((from, to), contributions) = do
  let origin = OverlayEdgeOrigin (NonEmpty.sort (fmap contributionReference contributions))
      sourceSides =
        IntMap.fromListWith (<>)
          [ (overlaySourceIndex (boundaryRefSource (sourceEdgeRef source)), sides source follows)
          | AtomicContribution source follows <- NonEmpty.toList contributions
          ]
  transitions <-
    IntMap.fromAscList . catMaybes
      <$> traverse (resolveSourceTransition origin) (IntMap.toAscList sourceSides)
  if IntMap.null transitions
    then Right Nothing
    else do
      segment <- first OverlayExactGeometryInvalid (exactSegment from to)
      pure (Just (AtomicEdge segment origin transitions))
 where
  contributionReference (AtomicContribution source _) = sourceEdgeRef source
  sides source True = SourceSides (Set.singleton (sourceLabel source)) Set.empty
  sides source False = SourceSides Set.empty (Set.singleton (sourceLabel source))

resolveSourceTransition
  :: OverlayEdgeOrigin
  -> (Int, SourceSides)
  -> Either OverlayError (Maybe (Int, (OverlayLabelId, OverlayLabelId)))
resolveSourceTransition origin (sourceIndex, SourceSides leftLabels rightLabels) = do
  leftLabel <- resolveSide leftLabels
  rightLabel <- resolveSide rightLabels
  pure
    (if leftLabel == rightLabel then Nothing else Just (sourceIndex, (leftLabel, rightLabel)))
 where
  resolveSide labels =
    case Set.toAscList labels of
      [] -> Right outsideLabel
      [label] -> Right label
      label : remaining ->
        Left
          ( OverlayArrangementInvalid
              (OverlaySourceSideConflict (OverlaySourceId sourceIndex) origin (label :| remaining))
          )

-- | Keep all exact source events, including a source vertex that becomes
-- isolated when every incident identity-transition seam disappears.
vertexOrigins
  :: V.Vector SourceBoundary
  -> ExactSegmentEventPlan
  -> Map ExactPoint OriginAccumulation
vertexOrigins sources plan = V.ifoldl' collectSource Map.empty sources
 where
  collectSource origins sourceIndex source =
    let (from, to) = exactSegmentEndpoints (sourceSegment source)
        withVertices =
          insertOrigin to (OriginAccumulation (Set.singleton (sourceToRef source)) Set.empty)
            (insertOrigin from (OriginAccumulation (Set.singleton (sourceFromRef source)) Set.empty) origins)
        edgeOrigin = OriginAccumulation Set.empty (Set.singleton (sourceEdgeRef source))
     in List.foldl'
          (\current point -> insertOrigin point edgeOrigin current)
          withVertices
          (exactSegmentSplitPoints plan (ExactSweepSegmentId sourceIndex))
  insertOrigin = Map.insertWith (<>)

arrangementDarts
  :: Map ExactPoint VertexId
  -> V.Vector AtomicEdge
  -> Either OverlayError (Map DirectedEdgeId ArrangementDart)
arrangementDarts pointIds atomics =
  Map.fromList . concat
    <$> traverse makeDarts (V.toList (V.indexed atomics))
 where
  makeDarts :: (Int, AtomicEdge) -> Either OverlayError [(DirectedEdgeId, ArrangementDart)]
  makeDarts (index, atomic) = do
    let (from, to) = exactSegmentEndpoints (atomicSegment atomic)
        (forward, backward) = directedPair (UndirectedEdgeId (fromIntegral index))
    fromId <- requireExactVertex pointIds from
    toId <- requireExactVertex pointIds to
    pure
      [ (forward, ArrangementDart forward fromId from to)
      , (backward, ArrangementDart backward toId to from)
      ]

requireExactVertex :: Map ExactPoint VertexId -> ExactPoint -> Either OverlayError VertexId
requireExactVertex pointIds point =
  maybe
    (Left (OverlayProvenanceIncomplete (OverlayExactVertexMissing point)))
    Right
    (Map.lookup point pointIds)

outgoingDarts :: Map DirectedEdgeId ArrangementDart -> Map VertexId (NonEmpty ArrangementDart)
outgoingDarts darts =
  Map.map orderOutgoing
    (Map.fromListWith (<>) [(dartOrigin dart, dart :| []) | dart <- Map.elems darts])
 where
  orderOutgoing outgoing@(initial :| _) =
    NonEmpty.sortBy
      (\left right -> compareAround (dartFrom initial) (dartTo left) (dartTo right))
      outgoing

-- | At the destination, the clockwise predecessor of the reverse dart keeps
-- the face on the left. Construct the permutation directly from cyclic pairs;
-- there is no angular index search or numerical angle conversion.
rotationSuccessors
  :: Map VertexId (NonEmpty ArrangementDart)
  -> Map DirectedEdgeId DirectedEdgeId
rotationSuccessors outgoing =
  Map.fromList
    [ (reverseEdge (dartHandle current), dartHandle previous)
    | aroundVertex <- Map.elems outgoing
    , (previous, current) <- cyclePairs aroundVertex
    ]

boundaryOrbits
  :: Map DirectedEdgeId ArrangementDart
  -> Map DirectedEdgeId DirectedEdgeId
  -> Either OverlayError [BoundaryOrbit]
boundaryOrbits darts successors = descend (Map.keysSet darts)
 where
  descend remaining =
    case Set.lookupMin remaining of
      Nothing -> Right []
      Just seed -> do
        (untraced, handles) <- traceOrbit seed remaining seed []
        orbit <- makeOrbit <$> traverse requireDart handles
        (orbit :) <$> descend untraced

  -- This is descent of a finite permutation: every nonclosing step removes
  -- exactly one dart. Returning to another consumed dart is a typed failure,
  -- never a walk-budget success or a vertex-simple cycle substitution.
  traceOrbit seed remaining current reversedHandles
    | current == seed, previous : rest <- reversedHandles =
        Right (remaining, NonEmpty.reverse (previous :| rest))
    | not (Set.member current remaining) =
        Left (OverlayArrangementInvalid (OverlayBoundaryOrbitNotClosed seed current))
    | otherwise = do
        next <- requireSuccessor successors current
        traceOrbit seed (Set.delete current remaining) next (current : reversedHandles)

  requireDart handle =
    maybe
      (Left (OverlayArrangementInvalid (OverlayBoundaryOrbitMissing handle)))
      Right
      (Map.lookup handle darts)

makeOrbit :: NonEmpty ArrangementDart -> BoundaryOrbit
makeOrbit darts =
  BoundaryOrbit
    { orbitDarts = darts
    , orbitDoubleArea =
        List.foldl' (\area dart -> area + exactPointCross (dartFrom dart) (dartTo dart))
          0 (NonEmpty.toList darts)
    , orbitBounds = exactPointsBounds (fmap dartFrom darts)
    }

orbitRoot :: BoundaryOrbit -> DirectedEdgeId
orbitRoot (BoundaryOrbit (initial :| _) _ _) = dartHandle initial

assignOrbit
  :: [(FaceId, BoundaryOrbit)]
  -> Map DirectedEdgeId FaceId
  -> BoundaryOrbit
  -> (FaceId, BoundaryOrbit)
assignOrbit anchors anchorByRoot orbit =
  case Map.lookup (orbitRoot orbit) anchorByRoot of
    Just face -> (face, orbit)
    Nothing -> (containingFace anchors (leastOrbitPoint orbit), orbit)

-- | A nonpositive orbit is its connected support component's exterior
-- boundary. Exact event normalization has already glued every touching
-- component, so this component lies in one face of every other component.
-- Its least exact vertex represents that containing face without constructing
-- an interior probe against every atomic segment. Strict containment below
-- excludes the component's own positive faces at this exposed vertex.
leastOrbitPoint :: BoundaryOrbit -> ExactPoint
leastOrbitPoint orbit =
  let initial :| remaining = orbitDarts orbit
   in List.foldl' (\point dart -> min point (dartFrom dart)) (dartFrom initial) remaining

-- | Positive orbits of one connected graph have disjoint interiors; across
-- disconnected components their interiors are disjoint or nested. The least
-- positive area strictly containing a point is its nearest enclosing face.
-- Bounds reject remote candidates before exact boundary or winding work;
-- raw-walk winding handles repeated vertices and cancels bridge darts.
containingFace :: [(FaceId, BoundaryOrbit)] -> ExactPoint -> FaceId
containingFace anchors point =
  maybe outerFace fst (List.foldl' choose Nothing anchors)
 where
  choose
    :: Maybe (FaceId, BoundaryOrbit)
    -> (FaceId, BoundaryOrbit)
    -> Maybe (FaceId, BoundaryOrbit)
  choose selected candidate@(_, orbit)
    | not (pointInBounds point (orbitBounds orbit)) = selected
    | any (\dart -> exactOnClosedSegment (dartFrom dart) (dartTo dart) point) (orbitDarts orbit) = selected
    | orbitWinding point orbit == 0 = selected
    | otherwise =
        case selected of
          Nothing -> Just candidate
          Just (_, previous)
            | orbitDoubleArea orbit < orbitDoubleArea previous -> Just candidate
            | otherwise -> selected

orbitWinding :: ExactPoint -> BoundaryOrbit -> Int
orbitWinding query orbit = List.foldl' crossing 0 (NonEmpty.toList (orbitDarts orbit))
 where
  (_, queryY) = exactPointCoordinates query
  crossing :: Int -> ArrangementDart -> Int
  crossing count dart =
    let from = dartFrom dart
        to = dartTo dart
        (_, fromY) = exactPointCoordinates from
        (_, toY) = exactPointCoordinates to
        orientation = exactOrient2d from to query
     in if fromY <= queryY && queryY < toY && orientation == GT
          then count + 1
          else
            if toY <= queryY && queryY < fromY && orientation == LT
              then count - 1
              else count

-- | A rational clearance witness, not an epsilon. Starting at an atomic
-- midpoint, the open interval before the first exact left-normal ray hit
-- lies wholly in the dart's left face. Halving that parameter gives an exact
-- interior probe even when distinct vertices collide under binary64.
certifiedLeftProbe
  :: [ExactSegment]
  -> [ExactPoint]
  -> ExactPoint
  -> ExactPoint
  -> Either OverlayError ExactPoint
certifiedLeftProbe segments isolatedPoints from to = do
  let (fromX, fromY) = exactPointCoordinates from
      (toX, toY) = exactPointCoordinates to
      midpoint = exactPoint (oneHalf * (fromX + toX)) (oneHalf * (fromY + toY))
      normal = ExactVector (fromY - toY) (toX - fromX)
  intersections <- traverse (raySegmentParameter midpoint normal) segments
  isolatedIntersections <- traverse (rayIsolatedParameter midpoint normal) isolatedPoints
  let parameter = maybe 1 (oneHalf *) (leastParameter (intersections <> isolatedIntersections))
      ExactVector normalX normalY = normal
  pure (translateExactPoint midpoint (ExactVector (parameter * normalX) (parameter * normalY)))

-- | Derive a genuine interior witness when an observation, such as erosion,
-- requires one. Face assembly needs only strict component containment and
-- does not pay for this all-support clearance query.
overlayFaceInteriorPoint
  :: OverlayResult labels
  -> FaceId
  -> Either OverlayError ExactPoint
overlayFaceInteriorPoint result face
  | faceIdIndex face >= incidenceFaceCount incidence =
      Left (OverlayProvenanceIncomplete (OverlayFaceMissing face))
  | face == outerFace = unavailable
  | otherwise =
      case faceBoundaryComponents incidence face of
        (root : _) : _ -> do
          from <- pointAt (incidenceOrigin incidence root)
          to <- pointAt (incidenceDestination incidence root)
          segments <-
            traverse
              (\index -> do
                 let (fromVertex, toVertex) =
                       incidenceUndirectedEndpoints incidence (UndirectedEdgeId (fromIntegral index))
                 firstPoint <- pointAt fromVertex
                 secondPoint <- pointAt toVertex
                 first OverlayExactGeometryInvalid (exactSegment firstPoint secondPoint))
              [0 .. incidenceEdgeCount incidence - 1]
          let isolatedPoints =
                [ overlayExactPoint vertex
                | (index, vertex) <- V.toList (V.indexed (overlayResultVertices result))
                , incidenceVertexOutEdge incidence (VertexId (fromIntegral index)) == Nothing
                ]
          certifiedLeftProbe segments isolatedPoints from to
        _ -> unavailable
 where
  incidence = overlayResultIncidence result
  unavailable :: Either OverlayError ExactPoint
  unavailable = Left (OverlayProvenanceIncomplete (OverlayFaceInteriorUnavailable face))
  pointAt vertex =
    maybe
      (Left (OverlayProvenanceIncomplete (OverlayVertexMissing vertex)))
      (Right . overlayExactPoint)
      (overlayResultVertices result V.!? vertexIdIndex vertex)

rayIsolatedParameter
  :: ExactPoint
  -> ExactVector
  -> ExactPoint
  -> Either OverlayError (Maybe ExactRational)
rayIsolatedParameter origin direction point
  | exactCross (exactVectorFromPoints origin point) direction /= 0 = Right Nothing
  | otherwise = do
      parameter <- rayPointParameter origin direction point
      pure (if parameter > 0 then Just parameter else Nothing)

leastParameter :: [Maybe ExactRational] -> Maybe ExactRational
leastParameter = List.foldl' choose Nothing
 where
  choose :: Maybe ExactRational -> Maybe ExactRational -> Maybe ExactRational
  choose Nothing candidate = candidate
  choose selected Nothing = selected
  choose (Just previous) (Just candidate) = Just (min previous candidate)

raySegmentParameter
  :: ExactPoint
  -> ExactVector
  -> ExactSegment
  -> Either OverlayError (Maybe ExactRational)
raySegmentParameter origin direction segment =
  let (from, to) = exactSegmentEndpoints segment
      segmentDirection = exactVectorFromPoints from to
      displacement = exactVectorFromPoints origin from
      determinant = exactCross direction segmentDirection
   in if determinant == 0
        then
          if exactCross displacement direction /= 0
            then Right Nothing
            else do
              fromParameter <- rayPointParameter origin direction from
              toParameter <- rayPointParameter origin direction to
              pure (leastParameter [positiveParameter fromParameter, positiveParameter toParameter])
        else do
          parameter <- divide (exactCross displacement segmentDirection) determinant
          segmentParameter <- divide (exactCross displacement direction) determinant
          pure
            (if parameter > 0 && 0 <= segmentParameter && segmentParameter <= 1 then Just parameter else Nothing)
 where
  divide numerator denominator = first OverlayExactArithmetic (exactDivide numerator denominator)
  positiveParameter :: ExactRational -> Maybe ExactRational
  positiveParameter parameter = if parameter > 0 then Just parameter else Nothing

rayPointParameter :: ExactPoint -> ExactVector -> ExactPoint -> Either OverlayError ExactRational
rayPointParameter origin (ExactVector directionX directionY) point =
  let (originX, originY) = exactPointCoordinates origin
      (pointX, pointY) = exactPointCoordinates point
      (numerator, denominator) =
        if directionX /= 0 then (pointX - originX, directionX) else (pointY - originY, directionY)
   in first OverlayExactArithmetic (exactDivide numerator denominator)

requireSuccessor
  :: Map DirectedEdgeId DirectedEdgeId
  -> DirectedEdgeId
  -> Either OverlayError DirectedEdgeId
requireSuccessor successors dart =
  maybe
    (Left (OverlayArrangementInvalid (OverlayBoundaryOrbitMissing dart)))
    Right
    (Map.lookup dart successors)

requireDartFace :: Map DirectedEdgeId FaceId -> DirectedEdgeId -> Either OverlayError FaceId
requireDartFace faces dart =
  maybe
    (Left (OverlayArrangementInvalid (OverlayBoundaryOrbitMissing dart)))
    Right
    (Map.lookup dart faces)

assembleIncidence
  :: Map ExactPoint VertexId
  -> Map DirectedEdgeId ArrangementDart
  -> Map VertexId (NonEmpty ArrangementDart)
  -> Map DirectedEdgeId DirectedEdgeId
  -> Map DirectedEdgeId DirectedEdgeId
  -> Map DirectedEdgeId FaceId
  -> [(FaceId, BoundaryOrbit)]
  -> Map FaceId [BoundaryOrbit]
  -> IntMap IntSet.IntSet
  -> Either OverlayError PlanarIncidence
assembleIncidence pointIds darts outgoing successors predecessors dartFaces anchors groupedOrbits isolated = do
  packedDarts <- concat <$> traverse packDart (Map.elems darts)
  let outerOrbits = Map.findWithDefault [] outerFace groupedOrbits
      outerRoot = case outerOrbits of
        [] -> Nothing
        orbit : _ -> Just (orbitRoot orbit)
      primaryRoots = outerRoot : map (Just . orbitRoot . snd) anchors
      faceHandles = outerFace : map fst anchors
      extraRoots =
        IntMap.fromList
          [ (faceIdIndex face, roots)
          | (face, primary) <- zip faceHandles primaryRoots
          , let roots =
                  [ root
                  | orbit <- Map.findWithDefault [] face groupedOrbits
                  , let root = orbitRoot orbit
                  , Just root /= primary
                  ]
          , not (null roots)
          ]
      vertexOutgoing =
        [ case Map.lookup vertex outgoing of
            Nothing -> noIndex
            Just (initial :| _) -> unDirectedEdgeId (dartHandle initial)
        | vertex <- Map.elems pointIds
        ]
  first OverlayIncidenceInvalid
    ( admitPlanarIncidence
        (fromVector noIndex (U.fromList vertexOutgoing))
        (fromVector noIndex (U.fromList packedDarts))
        (fromVector noIndex (U.fromList (map (maybe noIndex unDirectedEdgeId) primaryRoots)))
        extraRoots
        isolated
    )
 where
  packDart :: ArrangementDart -> Either OverlayError [Word32]
  packDart dart = do
    next <- requireSuccessor successors (dartHandle dart)
    previous <- requireSuccessor predecessors (dartHandle dart)
    face <- requireDartFace dartFaces (dartHandle dart)
    pure
      [ unVertexId (dartOrigin dart)
      , unDirectedEdgeId next
      , unDirectedEdgeId previous
      , unFaceId face
      ]

faceTransitions
  :: V.Vector AtomicEdge
  -> Map DirectedEdgeId FaceId
  -> Either OverlayError (Map FaceId [FaceTransition])
faceTransitions atomics dartFaces =
  Map.fromListWith (<>) . concat
    <$> traverse edgeTransitions (V.toList (V.indexed atomics))
 where
  edgeTransitions :: (Int, AtomicEdge) -> Either OverlayError [(FaceId, [FaceTransition])]
  edgeTransitions (index, atomic) = do
    let edge = UndirectedEdgeId (fromIntegral index)
        (forward, backward) = directedPair edge
        transitions = atomicTransitions atomic
    leftFace <- requireDartFace dartFaces forward
    rightFace <- requireDartFace dartFaces backward
    pure
      [ (leftFace, [FaceTransition edge rightFace transitions])
      , (rightFace, [FaceTransition edge leftFace (fmap (\(left, right) -> (right, left)) transitions)])
      ]

labelFaces
  :: Int
  -> Map FaceId [FaceTransition]
  -> Either OverlayError (V.Vector (IntMap OverlayLabelId))
labelFaces faceCount adjacency = do
  labels <- descend (FaceLabelling (Map.singleton outerFace IntMap.empty) (Seq.singleton outerFace))
  V.fromList
    <$> traverse
      (\index -> requireFaceLabels labels (FaceId (fromIntegral index)))
      [0 .. faceCount - 1]
 where
  descend labelling =
    case Seq.viewl (pendingLabelFaces labelling) of
      Seq.EmptyL -> Right (assignedFaceLabels labelling)
      face Seq.:< remaining -> do
        current <- requireFaceLabels (assignedFaceLabels labelling) face
        next <-
          foldM
            (descendTransition face current)
            (labelling{pendingLabelFaces = remaining})
            (Map.findWithDefault [] face adjacency)
        descend next

  descendTransition face current labelling transition = do
    derived <-
      foldM (applySourceTransition face (transitionEdge transition)) current
        (IntMap.toAscList (transitionSources transition))
    let target = transitionTarget transition
    case Map.lookup target (assignedFaceLabels labelling) of
      Nothing ->
        Right
          FaceLabelling
            { assignedFaceLabels = Map.insert target derived (assignedFaceLabels labelling)
            , pendingLabelFaces = pendingLabelFaces labelling Seq.|> target
            }
      Just existing
        | existing == derived -> Right labelling
        | otherwise ->
            Left
              ( OverlayArrangementInvalid
                  (OverlayFaceLabelConflict target (transitionEdge transition) existing derived)
              )

requireFaceLabels
  :: Map FaceId (IntMap OverlayLabelId)
  -> FaceId
  -> Either OverlayError (IntMap OverlayLabelId)
requireFaceLabels labels face =
  maybe
    (Left (OverlayProvenanceIncomplete (OverlayFaceMissing face)))
    Right
    (Map.lookup face labels)

applySourceTransition
  :: FaceId
  -> UndirectedEdgeId
  -> IntMap OverlayLabelId
  -> (Int, (OverlayLabelId, OverlayLabelId))
  -> Either OverlayError (IntMap OverlayLabelId)
applySourceTransition face edge current (sourceIndex, (expected, destination))
  | actual /= expected =
      Left
        ( OverlayArrangementInvalid
            (OverlayTransitionSourceMismatch face edge (OverlaySourceId sourceIndex) actual expected)
        )
  | destination == outsideLabel = Right (IntMap.delete sourceIndex current)
  | otherwise = Right (IntMap.insert sourceIndex destination current)
 where
  actual = IntMap.findWithDefault outsideLabel sourceIndex current

canonicalEdgeKey :: ExactPoint -> ExactPoint -> ExactEdgeKey
canonicalEdgeKey = orderedPair

compareAround :: ExactPoint -> ExactPoint -> ExactPoint -> Ordering
compareAround center left right =
  case compareExactVectorAngle (exactVectorFromPoints center left) (exactVectorFromPoints center right) of
    EQ -> compare left right
    ordering -> ordering

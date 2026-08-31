{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Resident DCEL descent and exact-cell gluing. The certified arrangement is
-- authoritative at this seam; diagonal schedules may change only its resident
-- triangulation, never its exact cell descriptors.
module Moonlight.Triangulation.Internal.Overlay.Resident
  ( OverlayDiagonalSchedule (..)
  , residentOverlay
  , faceLabels
  , faceCarriesExactArea
  , regionFaceLabels
  , vertexSupport
  , edgeSupport
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List (partition, sort, sortBy)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import qualified Moonlight.Triangulation.Dcel as Dcel
import Moonlight.Triangulation.Dcel
  ( faceData
  , faceDirectedEdges
  , faceVertices
  , imapUndirectedEdges
  , incidentFace
  , isConstraintEdge
  , numInnerFaces
  , numVertices
  , outerFace
  , undirectedEndpoints
  , vertexData
  , vertexOutgoingEdges
  )
import Moonlight.Triangulation.Exact (ExactPoint, exactOrient2d)
import Moonlight.Triangulation.FloodFillIterator
  ( FaceComponent
  , componentBoundary
  , faceComponentFaces
  , faceComponentsBy
  )
import Moonlight.Triangulation.Internal.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  , asUndirected
  , directedPair
  , reverseEdge
  )
import Moonlight.Triangulation.Handles.Iterators.FixedIterators
  ( allFaces
  , innerFaces
  , undirectedEdges
  )
import Moonlight.Triangulation.Internal.BoxedPaged (boxedFromVector)
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( consecutivePairs
  , traceOrientedBoundaryCircuits
  )
import Moonlight.Triangulation.Internal.Canonical (canonicalize)
import Moonlight.Triangulation.Internal.Cdt.Build (constrainedDelaunay)
import Moonlight.Triangulation.Internal.Cdt.Types (CdtError (..))
import Moonlight.Triangulation.Internal.Overlay.Arrangement
  ( ArrangementMetrics (..)
  , AtomicEdge (..)
  , atomicEdgeFrom
  , atomicEdgeTo
  , CertifiedArrangement (..)
  , ExactEdgeKey
  , OverlayVertexSeed (..)
  , atomicKey
  , canonicalEdgeKey
  , compareAround
  )
import Moonlight.Triangulation.Internal.Overlay.Types
import Moonlight.Triangulation.Internal.Region.Publication
  ( polygonComponentFromBoundaryCoordinates
  )
import Moonlight.Triangulation.Internal.Region.Types
  ( ExactLoop
  , PolygonComponent
  , RegionPublicationError (..)
  )
import Moonlight.Triangulation.Internal.Representation
  ( BuildResult (..)
  , Triangulation (..)
  )
import Moonlight.Triangulation.Internal.Types
  ( ConstraintMode (Constrained)
  , ElementDefaults (..)
  )
import Moonlight.Triangulation.Region (exactLoop)

data OverlayDiagonalSchedule
  = CanonicalOverlayDiagonals
  | FlipFirstAdmissibleDiagonal
  deriving stock (Eq, Ord, Show)

data ComponentDraft leftLabel rightLabel = ComponentDraft
  { componentDraftLabels :: !(leftLabel, rightLabel)
  , componentDraftFaces :: !FaceComponent
  , componentDraftPolygon :: !PolygonComponent
  , componentDraftTouchesOuter :: !Bool
  }

residentOverlay
  :: (Ord leftLabel, Ord rightLabel)
  => OverlayDiagonalSchedule
  -> (leftLabel, rightLabel)
  -> CertifiedArrangement leftLabel rightLabel
  -> Either
      (OverlayError leftLabel rightLabel)
      (OverlayResult leftLabel rightLabel)
residentOverlay diagonalSchedule outsidePair certified = do
  let atomicVector = certifiedAtomicEdges certified
      seeds = certifiedVertexSeeds certified
      constraints = certifiedConstraints certified
      metrics = certifiedMetrics certified
      defaults = ElementDefaults () () ()
  built <- first OverlayBuildFailed (constrainedDelaunay defaults seeds constraints)
  let resident = buildTriangulation built
  if numVertices resident == V.length seeds
    then Right ()
    else
      Left
        ( OverlayProvenanceIncomplete
            (OverlayEmbeddedVertexCountMismatch (V.length seeds) (numVertices resident))
        )
  canonicalResident <-
    first (OverlayBuildFailed . CdtBuildError) (canonicalize resident)
  let atomicByKey =
        Map.fromList
          [ (atomicKey edge, edge)
          | edge <- V.toList atomicVector
          ]
  validateAtomicConstraints canonicalResident atomicByKey
  scheduledResident <-
    applyDiagonalSchedule
      diagonalSchedule
      defaults
      seeds
      constraints
      canonicalResident
  labelledFaces <- labelResidentFaces outsidePair scheduledResident atomicByKey
  (fullDimensionalFaces, collapsedFaces) <-
    if Set.null (certifiedInexactEmbeddingPoints certified)
      then
        Right
          ( Set.fromList (innerFaces scheduledResident)
          , Set.empty
          )
      else
        exactFaceDimensions
          (certifiedInexactEmbeddingPoints certified)
          scheduledResident
  componentDrafts <-
    residentComponentDrafts
      scheduledResident
      labelledFaces
      fullDimensionalFaces
  let (unboundedDrafts, boundedDrafts) =
        partitionDrafts outsidePair componentDrafts
  numberedBounded <- numberBoundedComponents boundedDrafts
  let fullCellIdByFace =
        Map.fromList
          ( [ (face, OverlayCellId 0)
            | draftComponent <- unboundedDrafts
            , face <- faceComponentFaces (componentDraftFaces draftComponent)
            ]
              <> [ (face, cellId)
                 | (cellId, draftComponent) <- numberedBounded
                 , face <- faceComponentFaces (componentDraftFaces draftComponent)
                 ]
          )
  collapsedCellIds <-
    descendCollapsedFaces
      scheduledResident
      atomicByKey
      fullCellIdByFace
      collapsedFaces
  let cellIdByFace = fullCellIdByFace <> collapsedCellIds
  unboundedLoops <-
    unboundedCellBoundaryLoops scheduledResident atomicByKey cellIdByFace
  let cells =
        V.fromList
          ( OverlayCell
              { overlayCellLeft = fst outsidePair
              , overlayCellRight = snd outsidePair
              , overlayCellGeometry = UnboundedOverlayCell unboundedLoops
              }
              : [ OverlayCell
                    { overlayCellLeft = fst (componentDraftLabels draftComponent)
                    , overlayCellRight = snd (componentDraftLabels draftComponent)
                    , overlayCellGeometry =
                        BoundedOverlayCell (componentDraftPolygon draftComponent)
                    }
                | (_, draftComponent) <- numberedBounded
                ]
          )
  withFaces <- attachFaceCells collapsedFaces cellIdByFace scheduledResident
  let withVertices = attachOverlayVertices withFaces
      withEdges =
        imapUndirectedEdges
          OverlayDiagonal
          (\edge _ ->
             case Map.lookup (residentEdgeKey withVertices edge) atomicByKey of
               Nothing -> OverlayDiagonal
               Just atomic -> OverlayBoundary (atomicEdgeOrigin atomic))
          withVertices
      receipt =
        OverlayReceipt
          { overlayInputSegments = arrangementInputSegments metrics
          , overlayRelationEvents = arrangementRelationEvents metrics
          , overlayExactCrossings = arrangementExactCrossings metrics
          , overlayOverlapIntervals = arrangementOverlapIntervals metrics
          , overlayAtomicEdges = V.length atomicVector
          , overlayOutputVertices = numVertices withEdges
          , overlayArrangementCells = V.length cells
          , overlayResidentFaces = numInnerFaces withEdges
          , overlayEmbeddingCandidates = arrangementEmbeddingCandidates metrics
          , overlayTotalRelationChecks = arrangementTotalRelationChecks metrics
          , overlaySweepMaximumHeight = arrangementSweepMaximumHeight metrics
          }
  pure
    OverlayResult
      { overlayResultTriangulation = withEdges
      , overlayResultCells = cells
      , overlayResultOutsideLabels = outsidePair
      , overlayResultReceipt = receipt
      }

applyDiagonalSchedule
  :: OverlayDiagonalSchedule
  -> ElementDefaults () () ()
  -> V.Vector OverlayVertexSeed
  -> V.Vector (Int, Int)
  -> Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Either
      (OverlayError leftLabel rightLabel)
      (Triangulation 'Constrained OverlayVertexSeed () () ())
applyDiagonalSchedule CanonicalOverlayDiagonals _ _ _ triangulation =
  Right triangulation
applyDiagonalSchedule
  FlipFirstAdmissibleDiagonal
  defaults
  seeds
  constraints
  triangulation =
    case firstAlternativeDiagonal triangulation of
      Nothing -> Right triangulation
      Just (from, to) -> do
        let indexByPoint =
              Map.fromList
                [ (seedExactPoint seed, index)
                | (index, seed) <- V.toList (V.indexed seeds)
                ]
        fromIndex <- requireSeedIndex indexByPoint from
        toIndex <- requireSeedIndex indexByPoint to
        rebuilt <-
          first OverlayBuildFailed
            (constrainedDelaunay defaults seeds (V.snoc constraints (fromIndex, toIndex)))
        first (OverlayBuildFailed . CdtBuildError)
          (canonicalize (buildTriangulation rebuilt))

requireSeedIndex
  :: Map ExactPoint Int
  -> ExactPoint
  -> Either (OverlayError leftLabel rightLabel) Int
requireSeedIndex indexByPoint point =
  case Map.lookup point indexByPoint of
    Just index -> Right index
    Nothing -> Left (OverlayProvenanceIncomplete (OverlayExactVertexMissing point))

firstAlternativeDiagonal
  :: Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Maybe ExactEdgeKey
firstAlternativeDiagonal triangulation =
  listToMaybe
    (mapMaybe (alternativeDiagonal triangulation) (undirectedEdges triangulation))

alternativeDiagonal
  :: Triangulation 'Constrained OverlayVertexSeed () () ()
  -> UndirectedEdgeId
  -> Maybe ExactEdgeKey
alternativeDiagonal triangulation edge
  | isConstraintEdge triangulation edge = Nothing
  | leftFace == outerFace || rightFace == outerFace = Nothing
  | exactOrient2d c d b == GT && exactOrient2d d c a == GT =
      Just (canonicalEdgeKey c d)
  | otherwise = Nothing
 where
  (forward, backward) = directedPair edge
  leftFace = incidentFace triangulation forward
  rightFace = incidentFace triangulation backward
  a = seedExactPoint (vertexData triangulation (Dcel.origin triangulation forward))
  b = seedExactPoint (vertexData triangulation (Dcel.origin triangulation backward))
  c =
    seedExactPoint
      (vertexData triangulation (Dcel.origin triangulation (Dcel.previous triangulation forward)))
  d =
    seedExactPoint
      (vertexData triangulation (Dcel.origin triangulation (Dcel.previous triangulation backward)))
labelResidentFaces
  :: (Ord leftLabel, Ord rightLabel)
  => (leftLabel, rightLabel)
  -> Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Map ExactEdgeKey (AtomicEdge leftLabel rightLabel)
  -> Either
      (OverlayError leftLabel rightLabel)
      (Map FaceId (leftLabel, rightLabel))
labelResidentFaces outsideLabels triangulation atomicByKey = do
  labelled <-
    descendFaceTransitions
      triangulation
      atomicByKey
      (Map.singleton outerFace outsideLabels)
      (Seq.singleton outerFace)
  case
    [ face
    | face <- innerFaces triangulation
    , Map.notMember face labelled
    ] of
    missing : _ ->
      Left (OverlayProvenanceIncomplete (OverlayResidentFaceUnassigned missing))
    [] -> Right (Map.delete outerFace labelled)

seedDirectedEndpoints
  :: Triangulation mode OverlayVertexSeed directed undirected face
  -> DirectedEdgeId
  -> ExactEdgeKey
seedDirectedEndpoints triangulation edge =
  ( seedExactPoint (vertexData triangulation (Dcel.origin triangulation edge))
  , seedExactPoint (vertexData triangulation (Dcel.destination triangulation edge))
  )

swapTransition :: (label, label) -> (label, label)
swapTransition (fromLabel, toLabel) = (toLabel, fromLabel)

descendFaceTransitions
  :: (Eq leftLabel, Eq rightLabel)
  => Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Map ExactEdgeKey (AtomicEdge leftLabel rightLabel)
  -> Map FaceId (leftLabel, rightLabel)
  -> Seq.Seq FaceId
  -> Either
      (OverlayError leftLabel rightLabel)
      (Map FaceId (leftLabel, rightLabel))
descendFaceTransitions triangulation atomicByKey labelled queued =
  case Seq.viewl queued of
    Seq.EmptyL -> Right labelled
    face Seq.:< remaining ->
      case Map.lookup face labelled of
        Nothing ->
          Left (OverlayProvenanceIncomplete (OverlayResidentFaceUnassigned face))
        Just current -> do
          (nextLabels, nextQueue) <-
            foldM
              (descendFaceTransition triangulation atomicByKey face current)
              (labelled, remaining)
              (faceDirectedEdges triangulation face)
          descendFaceTransitions triangulation atomicByKey nextLabels nextQueue

descendFaceTransition
  :: (Eq leftLabel, Eq rightLabel)
  => Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Map ExactEdgeKey (AtomicEdge leftLabel rightLabel)
  -> FaceId
  -> (leftLabel, rightLabel)
  -> (Map FaceId (leftLabel, rightLabel), Seq.Seq FaceId)
  -> DirectedEdgeId
  -> Either
      (OverlayError leftLabel rightLabel)
      (Map FaceId (leftLabel, rightLabel), Seq.Seq FaceId)
descendFaceTransition triangulation atomicByKey source current (labelled, queued) directed = do
  let edge = asUndirected directed
      destinationFace = incidentFace triangulation (reverseEdge directed)
  (leftTransition, rightTransition) <-
    case Map.lookup (residentSeedEdgeKey triangulation edge) atomicByKey of
      Nothing -> Right (Nothing, Nothing)
      Just atomic -> orientedTransitions edge directed atomic
  let
      expected =
        ( maybe (fst current) fst leftTransition
        , maybe (snd current) fst rightTransition
        )
      derived =
        ( maybe (fst current) snd leftTransition
        , maybe (snd current) snd rightTransition
        )
  if current == expected
    then
      case Map.lookup destinationFace labelled of
        Nothing ->
          Right
            ( Map.insert destinationFace derived labelled
            , queued Seq.|> destinationFace
            )
        Just existing
          | existing == derived -> Right (labelled, queued)
          | otherwise ->
              Left
                ( OverlayArrangementInvalid
                    ( OverlayResidentFaceLabelConflict
                        destinationFace
                        edge
                        existing
                        derived
                    )
                )
    else
      Left
        ( OverlayArrangementInvalid
            (OverlayTransitionSourceMismatch source edge current expected)
        )
 where
  orientedTransitions edge candidateDirection atomic
    | directedEndpoints == (atomicEdgeFrom atomic, atomicEdgeTo atomic) =
        Right
          ( atomicEdgeLeftTransition atomic
          , atomicEdgeRightTransition atomic
          )
    | directedEndpoints == (atomicEdgeTo atomic, atomicEdgeFrom atomic) =
        Right
          ( swapTransition <$> atomicEdgeLeftTransition atomic
          , swapTransition <$> atomicEdgeRightTransition atomic
          )
    | otherwise =
        Left
          ( OverlayProvenanceIncomplete
              ( OverlayAtomicConstraintOrientationMismatch
                  edge
                  (atomicEdgeFrom atomic)
                  (atomicEdgeTo atomic)
              )
          )
   where
    directedEndpoints = seedDirectedEndpoints triangulation candidateDirection

exactFaceDimensions
  :: Set ExactPoint
  -> Triangulation 'Constrained OverlayVertexSeed directed undirected face
  -> Either
      (OverlayError leftLabel rightLabel)
      (Set FaceId, Set FaceId)
exactFaceDimensions inexactEmbeddingPoints triangulation = do
  orientations <-
    traverse
      ( \face ->
          (face,)
            <$> exactFaceOrientation
              inexactEmbeddingPoints
              triangulation
              face
      )
      (innerFaces triangulation)
  case [face | (face, LT) <- orientations] of
    reversed : _ ->
      Left
        ( OverlayArrangementInvalid
            (OverlayResidentFaceOrientationReversed reversed)
        )
    [] ->
      Right
        ( Set.fromList [face | (face, GT) <- orientations]
        , Set.fromList [face | (face, EQ) <- orientations]
        )

exactFaceOrientation
  :: Set ExactPoint
  -> Triangulation mode OverlayVertexSeed directed undirected face
  -> FaceId
  -> Either (OverlayError leftLabel rightLabel) Ordering
exactFaceOrientation inexactEmbeddingPoints triangulation face =
  case
    map
      (seedExactPoint . vertexData triangulation)
      (faceVertices triangulation face) of
    points@[firstPoint, secondPoint, thirdPoint]
      | any (`Set.member` inexactEmbeddingPoints) points ->
          Right (exactOrient2d firstPoint secondPoint thirdPoint)
      | otherwise -> Right GT
    vertices ->
      Left
        ( OverlayArrangementInvalid
            (OverlayResidentFaceArity face (length vertices))
        )

residentComponentDrafts
  :: forall leftLabel rightLabel.
     (Ord leftLabel, Ord rightLabel)
  => Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Map FaceId (leftLabel, rightLabel)
  -> Set FaceId
  -> Either
      (OverlayError leftLabel rightLabel)
      [ComponentDraft leftLabel rightLabel]
residentComponentDrafts triangulation labels fullDimensionalFaces =
  traverse
    convert
    (faceComponentsBy triangulation labelFullDimensionalFace (const True))
 where
  exactPointAt :: VertexId -> Either RegionPublicationError ExactPoint
  exactPointAt vertex =
    Right (seedExactPoint (vertexData triangulation vertex))
  labelFullDimensionalFace :: FaceId -> Maybe (leftLabel, rightLabel)
  labelFullDimensionalFace face
    | Set.member face fullDimensionalFaces = Map.lookup face labels
    | otherwise = Nothing
  convert
    :: ((leftLabel, rightLabel), FaceComponent)
    -> Either
        (OverlayError leftLabel rightLabel)
        (ComponentDraft leftLabel rightLabel)
  convert (componentLabels, component) = do
    boundary <-
      first
        (OverlayRegionPublicationFailed . RegionBoundaryObstruction)
        (componentBoundary triangulation component)
    polygon <-
      first OverlayRegionPublicationFailed
        (polygonComponentFromBoundaryCoordinates exactPointAt boundary)
    pure
      ComponentDraft
        { componentDraftLabels = componentLabels
        , componentDraftFaces = component
        , componentDraftPolygon = polygon
        , componentDraftTouchesOuter = touchesOuter triangulation component
        }

touchesOuter
  :: Triangulation mode vertex directed undirected face
  -> FaceComponent
  -> Bool
touchesOuter triangulation component =
  any
    ( any ((== outerFace) . incidentFace triangulation . reverseEdge)
        . faceDirectedEdges triangulation
    )
    (faceComponentFaces component)

partitionDrafts
  :: (Eq leftLabel, Eq rightLabel)
  => (leftLabel, rightLabel)
  -> [ComponentDraft leftLabel rightLabel]
  -> ([ComponentDraft leftLabel rightLabel], [ComponentDraft leftLabel rightLabel])
partitionDrafts outsidePair =
  partition
    (\draftComponent ->
       componentDraftTouchesOuter draftComponent
         && componentDraftLabels draftComponent == outsidePair)

sortComponentDrafts
  :: (Ord leftLabel, Ord rightLabel)
  => [ComponentDraft leftLabel rightLabel]
  -> [ComponentDraft leftLabel rightLabel]
sortComponentDrafts =
  sortBy
    (\left right ->
       compare
         (componentDraftPolygon left, componentDraftLabels left)
         (componentDraftPolygon right, componentDraftLabels right))

numberBoundedComponents
  :: (Ord leftLabel, Ord rightLabel)
  => [ComponentDraft leftLabel rightLabel]
  -> Either
      (OverlayError leftLabel rightLabel)
      [(OverlayCellId, ComponentDraft leftLabel rightLabel)]
numberBoundedComponents drafts =
  case
    [ componentDraftPolygon left
    | (left, right) <- consecutivePairs ordered
    , componentSignature left == componentSignature right
    ] of
    duplicate : _ ->
      Left
        ( OverlayArrangementInvalid
            (OverlayDuplicateCellSignature duplicate)
        )
    [] ->
      Right
        ( zipWith
            (\index component -> (OverlayCellId index, component))
            [1 ..]
            ordered
        )
 where
  ordered = sortComponentDrafts drafts
  componentSignature
    :: ComponentDraft leftLabel' rightLabel'
    -> (PolygonComponent, (leftLabel', rightLabel'))
  componentSignature component =
    (componentDraftPolygon component, componentDraftLabels component)

-- | A binary64 triangle whose authoritative vertices are collinear is not an
-- exact two-cell. It may remain in the resident triangulation only when its
-- representation diagonals descend uniquely to one full-dimensional exact
-- cell; this prevents the artifact from gluing exact components through a
-- zero-area wedge.
descendCollapsedFaces
  :: forall leftLabel rightLabel.
     Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Map ExactEdgeKey (AtomicEdge leftLabel rightLabel)
  -> Map FaceId OverlayCellId
  -> Set FaceId
  -> Either
      (OverlayError leftLabel rightLabel)
      (Map FaceId OverlayCellId)
descendCollapsedFaces triangulation atomicByKey fullCellIds collapsedFaces =
  Map.fromList . concat <$> traverse resolveComponent collapsedComponents
 where
  exactCellIds = Map.insert outerFace (OverlayCellId 0) fullCellIds
  collapsedComponents =
    fmap snd
      ( faceComponentsBy
          triangulation
          labelCollapsedFace
          isRepresentationDiagonal
      )

  labelCollapsedFace face
    | Set.member face collapsedFaces = Just ()
    | otherwise = Nothing

  isRepresentationDiagonal edge =
    Map.notMember (residentSeedEdgeKey triangulation edge) atomicByKey

  resolveComponent component =
    case faceComponentFaces component of
      [] ->
        Left
          ( OverlayArrangementInvalid
              OverlayFaceComponentEmpty
          )
      componentFaces@(firstFace : remainingFaces) -> do
        let componentWitness = firstFace NonEmpty.:| remainingFaces
            adjacentCellIds =
              Set.fromList
                [ cellId
                | face <- componentFaces
                , directed <- faceDirectedEdges triangulation face
                , isRepresentationDiagonal (asUndirected directed)
                , Just cellId <-
                    [Map.lookup (incidentFace triangulation (reverseEdge directed)) exactCellIds]
                ]
        cellId <-
          case Set.toAscList adjacentCellIds of
            [singleCellId] -> Right singleCellId
            [] ->
              Left
                ( OverlayArrangementInvalid
                    (OverlayCollapsedFacesUnowned componentWitness)
                )
            firstCellId : remainingCellIds ->
              Left
                ( OverlayArrangementInvalid
                    ( OverlayCollapsedFacesAmbiguous
                        componentWitness
                        (firstCellId NonEmpty.:| remainingCellIds)
                    )
                )
        pure (fmap (,cellId) componentFaces)

-- | Trace the finite boundary cycles of the unbounded exact cell from atomic
-- edges only. Resident hull edges and Delaunay diagonals are absent by
-- construction; the DCEL contributes only the already-proved incident-cell
-- gluing needed to orient each atomic edge with cell zero on its left.
unboundedCellBoundaryLoops
  :: Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Map ExactEdgeKey (AtomicEdge leftLabel rightLabel)
  -> Map FaceId OverlayCellId
  -> Either (OverlayError leftLabel rightLabel) [ExactLoop]
unboundedCellBoundaryLoops triangulation atomicByKey cellIdByFace = do
  orientedEdges <-
    Set.fromList . concat
      <$> traverse orientAtomicEdge (undirectedEdges triangulation)
  traceBoundaryCycles orientedEdges
 where
  orientAtomicEdge edge
    | Map.notMember (residentSeedEdgeKey triangulation edge) atomicByKey = Right []
    | otherwise = do
        let (forward, backward) = directedPair edge
        forwardCell <- faceCellId (incidentFace triangulation forward)
        backwardCell <- faceCellId (incidentFace triangulation backward)
        pure
          ( case (forwardCell == OverlayCellId 0, backwardCell == OverlayCellId 0) of
              (True, False) -> [seedDirectedEndpoints triangulation forward]
              (False, True) -> [seedDirectedEndpoints triangulation backward]
              _ -> []
          )
  faceCellId face
    | face == outerFace = Right (OverlayCellId 0)
    | otherwise =
        case Map.lookup face cellIdByFace of
          Just cellId -> Right cellId
          Nothing -> Left (OverlayProvenanceIncomplete (OverlayResidentFaceUnassigned face))

traceBoundaryCycles
  :: Set ExactEdgeKey
  -> Either (OverlayError leftLabel rightLabel) [ExactLoop]
traceBoundaryCycles orientedEdges = do
  pointCycles <-
    traceOrientedBoundaryCircuits
      fst
      snd
      (\seed _ ->
         OverlayArrangementInvalid
           (uncurry OverlayCellCycleDidNotClose seed))
      outgoing
      orientedEdges
  sort
    <$> traverse
      ( first
          (OverlayRegionPublicationFailed . RegionValidationObstruction)
          . exactLoop
      )
      pointCycles
 where
  outgoing =
    Map.mapWithKey orderAroundOrigin
      ( Set.foldl'
          (\byOrigin edge@(from, _) -> Map.insertWith (<>) from [edge] byOrigin)
          Map.empty
          orientedEdges
      )
  orderAroundOrigin :: ExactPoint -> [ExactEdgeKey] -> [ExactEdgeKey]
  orderAroundOrigin origin =
    sortBy (\(_, left) (_, right) -> compareAround origin left right)

attachFaceCells
  :: forall leftLabel rightLabel.
     Set FaceId
  -> Map FaceId OverlayCellId
  -> Triangulation 'Constrained OverlayVertexSeed () () ()
  -> Either
      (OverlayError leftLabel rightLabel)
      (Triangulation 'Constrained OverlayVertexSeed () () OverlayFace)
attachFaceCells collapsedFaces cellIdByFace triangulation = do
  payloads <- V.fromList <$> traverse facePayload (allFaces triangulation)
  let outerPayload = OverlayCellFace (OverlayCellId 0)
      defaults = triElementDefaults triangulation
  pure
    triangulation
      { triFaceData = boxedFromVector (Just outerPayload) payloads
      , triElementDefaults = defaults{defaultFaceData = outerPayload}
      }
 where
  facePayload
    :: FaceId
    -> Either (OverlayError leftLabel rightLabel) OverlayFace
  facePayload face
    | face == outerFace = Right (OverlayCellFace (OverlayCellId 0))
    | otherwise =
        case Map.lookup face cellIdByFace of
          Just cellId
            | Set.member face collapsedFaces ->
                Right (OverlayCollapsedFace cellId)
            | otherwise -> Right (OverlayCellFace cellId)
          Nothing ->
            Left
              ( OverlayProvenanceIncomplete
                  (OverlayResidentFaceUnassigned face)
              )

attachOverlayVertices
  :: Triangulation 'Constrained OverlayVertexSeed () () face
  -> Triangulation 'Constrained OverlayVertex () () face
attachOverlayVertices =
  Dcel.mapVertices
    (\seed -> OverlayVertex (seedExactPoint seed) (seedOrigin seed))

cellPayload
  :: V.Vector (OverlayCell leftLabel rightLabel)
  -> OverlayCellId
  -> Either OverlayCellWitness (OverlayCell leftLabel rightLabel)
cellPayload cells cellId@(OverlayCellId index) =
  case cells V.!? index of
    Just cell -> Right cell
    Nothing -> Left (OverlayCellPayloadMissing cellId)

faceLabels
  :: OverlayResult leftLabel rightLabel
  -> FaceId
  -> Either OverlayCellWitness (leftLabel, rightLabel)
faceLabels result face = do
  cell <-
    cellPayload
      (overlayResultCells result)
      (overlayFaceCellId (faceData (overlayResultTriangulation result) face))
  pure (overlayCellLeft cell, overlayCellRight cell)

-- | Whether a resident triangle has nonzero area under the authoritative
-- exact coordinate section. Binary64-only wedges are representation charts,
-- never exact two-cells.
faceCarriesExactArea
  :: OverlayResult leftLabel rightLabel
  -> FaceId
  -> Bool
faceCarriesExactArea result face =
  case faceData (overlayResultTriangulation result) face of
    OverlayCellFace {} -> True
    OverlayCollapsedFace {} -> False

regionFaceLabels
  :: OverlayResult leftLabel rightLabel
  -> FaceId
  -> Either RegionPublicationError (leftLabel, rightLabel)
regionFaceLabels result face =
  case faceLabels result face of
    Right labels -> Right labels
    Left _ -> Left (RegionFaceLabelMissing face)

vertexSupport
  :: (Ord leftLabel, Ord rightLabel)
  => OverlayResult leftLabel rightLabel
  -> VertexId
  -> Either OverlayCellWitness (OverlayCellSupport leftLabel rightLabel)
vertexSupport result vertex = do
  labels <-
    traverse
      (faceLabels result . incidentFace triangulation)
      (vertexOutgoingEdges triangulation vertex)
  case supportFromPairs labels of
    Just support -> Right support
    Nothing -> Left (OverlayVertexSupportMissing vertex)
 where
  triangulation = overlayResultTriangulation result

edgeSupport
  :: (Ord leftLabel, Ord rightLabel)
  => OverlayResult leftLabel rightLabel
  -> UndirectedEdgeId
  -> Either OverlayCellWitness (OverlayCellSupport leftLabel rightLabel)
edgeSupport result edge = do
  pairs <- traverse (faceLabels result . incidentFace triangulation) [forward, backward]
  case supportFromPairs pairs of
    Just support -> Right support
    Nothing -> Left (OverlayEdgeSupportMissing edge)
 where
  triangulation = overlayResultTriangulation result
  (forward, backward) = directedPair edge

supportFromPairs
  :: (Ord leftLabel, Ord rightLabel)
  => [(leftLabel, rightLabel)]
  -> Maybe (OverlayCellSupport leftLabel rightLabel)
supportFromPairs pairs = do
  let (leftLabels, rightLabels) =
        List.foldl'
          (\(left, right) (leftLabel, rightLabel) ->
             (Set.insert leftLabel left, Set.insert rightLabel right))
          (Set.empty, Set.empty)
          pairs
  leftSupport <- nonEmptySupport leftLabels
  rightSupport <- nonEmptySupport rightLabels
  pure (OverlayCellSupport leftSupport rightSupport)

nonEmptySupport :: Set label -> Maybe (OverlaySupport label)
nonEmptySupport labels = OverlaySupport <$> NonEmpty.nonEmpty (Set.toAscList labels)

validateAtomicConstraints
  :: Triangulation
      'Constrained
      OverlayVertexSeed
      ()
      ()
      ()
  -> Map ExactEdgeKey (AtomicEdge leftLabel rightLabel)
  -> Either (OverlayError leftLabel rightLabel) ()
validateAtomicConstraints triangulation atomicByKey = do
  traverse_ requireAtomic (Map.keys atomicByKey)
  traverse_ requireExpectedConstraint (undirectedEdges triangulation)
 where
  residentByKey =
    Map.fromList
      [ (residentSeedEdgeKey triangulation edge, edge)
      | edge <- undirectedEdges triangulation
      ]
  requireAtomic key@(from, to) =
    case Map.lookup key residentByKey of
      Nothing -> Left (OverlayProvenanceIncomplete (OverlayAtomicConstraintMissing from to))
      Just edge
        | isConstraintEdge triangulation edge -> Right ()
        | otherwise -> Left (OverlayProvenanceIncomplete (OverlayBoundaryEdgeNotConstrained edge))
  requireExpectedConstraint edge
    | isConstraintEdge triangulation edge
        && Map.notMember (residentSeedEdgeKey triangulation edge) atomicByKey =
        Left (OverlayProvenanceIncomplete (OverlayUnexpectedConstraint edge))
    | otherwise = Right ()

residentSeedEdgeKey
  :: Triangulation mode OverlayVertexSeed directed undirected face
  -> UndirectedEdgeId
  -> ExactEdgeKey
residentSeedEdgeKey = residentEdgeKeyBy seedExactPoint

residentEdgeKey
  :: Triangulation mode OverlayVertex directed undirected face
  -> UndirectedEdgeId
  -> ExactEdgeKey
residentEdgeKey = residentEdgeKeyBy overlayExactPoint

residentEdgeKeyBy
  :: (vertex -> ExactPoint)
  -> Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> ExactEdgeKey
residentEdgeKeyBy exactPointAt triangulation edge =
  let (from, to) = undirectedEndpoints triangulation edge
   in canonicalEdgeKey
        (exactPointAt (vertexData triangulation from))
        (exactPointAt (vertexData triangulation to))

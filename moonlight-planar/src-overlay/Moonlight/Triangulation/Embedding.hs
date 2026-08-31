{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Bounded certification that projection of a declared exact arrangement to
-- binary64 preserves its local incidence and rotation data.
module Moonlight.Triangulation.Embedding
  ( DraftId (..)
  , DraftVertexId
  , DraftSegmentId
  , DraftSourceId
  , DraftIncidence (..)
  , DraftNeighborhood (..)
  , ExactArrangementDraft (..)
  , DraftReference (..)
  , OverlayEmbeddingObstruction (..)
  , EmbeddingObligation (..)
  , EmbeddingResidual
  , residualUndischargedObligations
  , milestoneOneResidual
  , LocalEmbeddingCertificate
  , certificateRoundedVertexDistinctnessCount
  , certificateSplitOrderPreservationCount
  , certificateIncidenceRelationPreservationCount
  , certificateNeighborhoodRotationPreservationCount
  , certificateRoundedVertices
  , certificateResidual
  , certifyLocalEmbedding
  ) where

import Control.DeepSeq (NFData)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Moonlight.Triangulation.Exact
  ( ExactPoint
  , SegmentRelation
  , exactPointToEmbeddingCandidate
  , exactSegmentRelation
  )
import Moonlight.Triangulation.Internal.BoundaryCycle
  ( cyclePairs
  , unorderedPairs
  )
import Moonlight.Triangulation.Internal.ExactRational (ExactRational)
import qualified Moonlight.Triangulation.Internal.Predicates as Admitted
  ( orient2d
  , segmentRelation
  )
import Moonlight.Triangulation.Types
  ( Point (..)
  , PointValidationError
  , QueryPoint
  , queryPointValue
  )

-- | Phantom draft kind; three incompatible identifiers share one scalar owner.
data DraftEntity = DraftVertexEntity | DraftSegmentEntity | DraftSourceEntity

newtype DraftId (entity :: DraftEntity) = DraftId Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

type DraftVertexId = DraftId 'DraftVertexEntity
type DraftSegmentId = DraftId 'DraftSegmentEntity
type DraftSourceId = DraftId 'DraftSourceEntity

-- | A declared relation between two atomic draft segments.
data DraftIncidence = DraftIncidence
  { -- | First declared atomic segment.
    draftIncidenceFirstSegment :: !DraftSegmentId
  , -- | Second declared atomic segment.
    draftIncidenceSecondSegment :: !DraftSegmentId
  , -- | Relation declared to hold in both exact and rounded geometry.
    draftIncidenceRelation :: !SegmentRelation
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | One exact vertex and the cyclic neighbor order declared around it.
data DraftNeighborhood = DraftNeighborhood
  { -- | Center of the declared local rotation.
    draftNeighborhoodCenter :: !DraftVertexId
  , -- | Neighbors in cyclic rotation order.
    draftNeighborhoodNeighbors :: !(NonEmpty DraftVertexId)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A hand-built exact arrangement draft. It contains only declared local
-- structure; no arrangement or global crossing search is derived here.
data ExactArrangementDraft = ExactArrangementDraft
  { -- | Exact coordinates keyed by draft-local vertex identity.
    draftVertices :: !(Map DraftVertexId ExactPoint)
  , -- | Atomic segment endpoint identities.
    draftSegments :: !(Map DraftSegmentId (DraftVertexId, DraftVertexId))
  , -- | Exact split parameters and vertices in source-segment order.
    draftSourceMemberships :: !(Map DraftSourceId [(ExactRational, DraftVertexId)])
  , -- | Segment relations declared to remain invariant under projection.
    draftIncidences :: ![DraftIncidence]
  , -- | Cyclic local rotations declared to retain their neighbor order.
    draftNeighborhoods :: ![DraftNeighborhood]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The typed identity of a missing draft reference.
data DraftReference
  = DraftVertexReference !DraftVertexId
  | DraftSegmentReference !DraftSegmentId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A witness that prevents local embedding certification.
data OverlayEmbeddingObstruction
  = DraftReferenceMissing !DraftReference
  | VertexProjectionRefused !DraftVertexId !PointValidationError
  | RoundedVerticesCollide !DraftVertexId !DraftVertexId !Point
  | SplitOrderNotPreserved !DraftSourceId !DraftVertexId !DraftVertexId
  | IncidenceRelationChanged
      !DraftSegmentId
      !DraftSegmentId
      !SegmentRelation
      !SegmentRelation
      !SegmentRelation
  | NeighborhoodRotationChanged
      !DraftVertexId
      !(NonEmpty DraftVertexId)
      !(NonEmpty DraftVertexId)
  | GlobalRelationAdded
      !DraftSegmentId
      !DraftSegmentId
      !SegmentRelation
  | GlobalRelationRemoved
      !DraftSegmentId
      !DraftSegmentId
      !SegmentRelation
  | GlobalRelationChanged
      !DraftSegmentId
      !DraftSegmentId
      !SegmentRelation
      !SegmentRelation
  | ProjectedSegmentCollapsed
      !DraftSegmentId
      !DraftVertexId
      !DraftVertexId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | An embedding obligation not discharged by the bounded local certifier.
data EmbeddingObligation
  = GlobalNoNewCrossing
  deriving stock (Bounded, Enum, Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A nonempty collection of obligations deferred to a later owner.
newtype EmbeddingResidual = EmbeddingResidual (NonEmpty EmbeddingObligation)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Read the obligations that remain explicitly undischarged.
residualUndischargedObligations
  :: EmbeddingResidual
  -> NonEmpty EmbeddingObligation
residualUndischargedObligations (EmbeddingResidual obligations) = obligations

-- | The Milestone 1 residual: the arrangement sweep has not yet proved global
-- absence of new crossings.
milestoneOneResidual :: EmbeddingResidual
milestoneOneResidual = EmbeddingResidual (GlobalNoNewCrossing :| [])

-- | A certificate for exactly four local obligations on a declared draft:
-- vertex distinctness, source split order, declared incidences, and local
-- neighborhood rotation. Global absence of new crossings is unproved until
-- the Milestone 2 arrangement sweep supplies the complete obligation set.
data LocalEmbeddingCertificate = LocalEmbeddingCertificate
  { -- | Number of rounded vertex-pair distinctness checks discharged.
    certificateRoundedVertexDistinctnessCount :: !Int
  , -- | Number of adjacent source split-order checks discharged.
    certificateSplitOrderPreservationCount :: !Int
  , -- | Number of declared incidence checks discharged.
    certificateIncidenceRelationPreservationCount :: !Int
  , -- | Number of neighborhood rotation entries certified.
    certificateNeighborhoodRotationPreservationCount :: !Int
  , -- | The admitted candidate projection certified by the four local obligations.
    certificateRoundedVertices :: !(Map DraftVertexId QueryPoint)
  , -- | The necessarily nonempty global obligation residual.
    certificateResidual :: !EmbeddingResidual
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Certify the four bounded local embedding obligations, collecting every
-- witness within each obligation and preserving obligation order.
certifyLocalEmbedding
  :: ExactArrangementDraft
  -> Either (NonEmpty OverlayEmbeddingObstruction) LocalEmbeddingCertificate
certifyLocalEmbedding draft =
  case NonEmpty.nonEmpty (structuralObstructions draft) of
    Just obstructions -> Left obstructions
    Nothing ->
      case projectVertices (draftVertices draft) of
        Invalid obstructions -> Left obstructions
        Valid projectedVertices ->
          certifyProjectedDraft draft projectedVertices

data Validation value
  = Invalid !(NonEmpty OverlayEmbeddingObstruction)
  | Valid value

instance Functor Validation where
  fmap _ (Invalid obstructions) = Invalid obstructions
  fmap transform (Valid value) = Valid (transform value)

instance Applicative Validation where
  pure = Valid
  Invalid left <*> Invalid right = Invalid (left <> right)
  Invalid obstructions <*> Valid _ = Invalid obstructions
  Valid _ <*> Invalid obstructions = Invalid obstructions
  Valid transform <*> Valid value = Valid (transform value)

invalid :: OverlayEmbeddingObstruction -> Validation value
invalid obstruction = Invalid (obstruction :| [])

data ProjectedVertex = ProjectedVertex
  { projectedExactPoint :: !ExactPoint
  , projectedRoundedPoint :: !QueryPoint
  }

data ResolvedSegment = ResolvedSegment
  { resolvedSegmentId :: !DraftSegmentId
  , resolvedSegmentFrom :: !ResolvedVertex
  , resolvedSegmentTo :: !ResolvedVertex
  }

data ResolvedVertex = ResolvedVertex
  { resolvedVertexId :: !DraftVertexId
  , resolvedProjectedVertex :: !ProjectedVertex
  }

data ResolvedMembership = ResolvedMembership
  { resolvedMembershipParameter :: !ExactRational
  , resolvedMembershipVertex :: !ResolvedVertex
  }

structuralObstructions
  :: ExactArrangementDraft
  -> [OverlayEmbeddingObstruction]
structuralObstructions draft =
  segmentEndpointObstructions
    <> sourceMembershipObstructions
    <> incidenceObstructions
    <> neighborhoodObstructions
 where
  vertices = draftVertices draft
  segments = draftSegments draft
  missingVertex vertexId =
    [ DraftReferenceMissing (DraftVertexReference vertexId)
    | Map.notMember vertexId vertices
    ]
  missingSegment segmentId =
    [ DraftReferenceMissing (DraftSegmentReference segmentId)
    | Map.notMember segmentId segments
    ]
  segmentEndpointObstructions =
    concatMap
      (\(_, (from, to)) -> missingVertex from <> missingVertex to)
      (Map.toAscList segments)
  sourceMembershipObstructions =
    concatMap
      (concatMap (missingVertex . snd) . snd)
      (Map.toAscList (draftSourceMemberships draft))
  incidenceObstructions =
    concatMap
      ( \incidence ->
          missingSegment (draftIncidenceFirstSegment incidence)
            <> missingSegment (draftIncidenceSecondSegment incidence)
      )
      (draftIncidences draft)
  neighborhoodObstructions =
    concatMap
      ( \neighborhood ->
          missingVertex (draftNeighborhoodCenter neighborhood)
            <> concatMap missingVertex (draftNeighborhoodNeighbors neighborhood)
      )
      (draftNeighborhoods draft)

projectVertices
  :: Map DraftVertexId ExactPoint
  -> Validation (Map DraftVertexId ProjectedVertex)
projectVertices =
  Map.traverseWithKey
    ( \vertexId point ->
        case exactPointToEmbeddingCandidate point of
          Left projectionError ->
            invalid (VertexProjectionRefused vertexId projectionError)
          Right roundedPoint -> Valid (ProjectedVertex point roundedPoint)
    )

certifyProjectedDraft
  :: ExactArrangementDraft
  -> Map DraftVertexId ProjectedVertex
  -> Either (NonEmpty OverlayEmbeddingObstruction) LocalEmbeddingCertificate
certifyProjectedDraft draft projectedVertices =
  case resolvedFailures of
    Left obstruction -> Left (obstruction :| [])
    Right localFailures ->
      case NonEmpty.nonEmpty (collisionFailures <> localFailures) of
        Just obstructions -> Left obstructions
        Nothing ->
          Right
            LocalEmbeddingCertificate
              { certificateRoundedVertexDistinctnessCount = distinctnessCount
              , certificateSplitOrderPreservationCount = splitOrderCount
              , certificateIncidenceRelationPreservationCount = incidenceCount
              , certificateNeighborhoodRotationPreservationCount = neighborhoodCount
              , certificateRoundedVertices = Map.map projectedRoundedPoint projectedVertices
              , certificateResidual = milestoneOneResidual
              }
 where
  collisionFailures = roundedVertexCollisionObstructions projectedVertices
  resolvedFailures = do
    splitFailures <- traverse (uncurry resolveSource) (Map.toAscList (draftSourceMemberships draft))
    incidenceFailures <- traverse resolveIncidence (draftIncidences draft)
    neighborhoodFailures <- traverse resolveNeighborhood (draftNeighborhoods draft)
    pure (concat splitFailures <> concat incidenceFailures <> concat neighborhoodFailures)
  resolveVertex vertexId =
    case Map.lookup vertexId projectedVertices of
      Nothing -> Left (DraftReferenceMissing (DraftVertexReference vertexId))
      Just projectedVertex -> Right (ResolvedVertex vertexId projectedVertex)
  resolveSegment segmentId =
    case Map.lookup segmentId (draftSegments draft) of
      Nothing -> Left (DraftReferenceMissing (DraftSegmentReference segmentId))
      Just (from, to) ->
        ResolvedSegment segmentId
          <$> resolveVertex from
          <*> resolveVertex to
  resolveSource sourceId memberships =
    splitOrderObstructions sourceId
      <$> traverse
        ( \(parameter, vertexId) ->
            ResolvedMembership parameter <$> resolveVertex vertexId
        )
        memberships
  resolveIncidence incidence =
    incidenceRelationObstructions
      (draftIncidenceRelation incidence)
      <$> resolveSegment (draftIncidenceFirstSegment incidence)
      <*> resolveSegment (draftIncidenceSecondSegment incidence)
  resolveNeighborhood neighborhood =
    neighborhoodRotationObstructions
      <$> resolveVertex (draftNeighborhoodCenter neighborhood)
      <*> traverse resolveVertex (draftNeighborhoodNeighbors neighborhood)
  vertexCount = Map.size projectedVertices
  distinctnessCount = vertexCount * (vertexCount - 1) `quot` 2
  splitOrderCount =
    sum
      ( map
          (max 0 . subtract 1 . length)
          (Map.elems (draftSourceMemberships draft))
      )
  incidenceCount = length (draftIncidences draft)
  neighborhoodCount =
    sum
      ( map
          (length . draftNeighborhoodNeighbors)
          (draftNeighborhoods draft)
      )

roundedVertexCollisionObstructions
  :: Map DraftVertexId ProjectedVertex
  -> [OverlayEmbeddingObstruction]
roundedVertexCollisionObstructions projectedVertices =
  [ RoundedVerticesCollide leftId rightId (queryPointValue roundedPoint)
  | (roundedPoint, vertexIds) <- Map.toAscList verticesByRoundedPoint
  , (leftId, rightId) <- unorderedPairs (List.sort vertexIds)
  ]
 where
  verticesByRoundedPoint =
    Map.fromListWith (<>)
      [ (projectedRoundedPoint projectedVertex, [vertexId])
      | (vertexId, projectedVertex) <- Map.toAscList projectedVertices
      ]

splitOrderObstructions
  :: DraftSourceId
  -> [ResolvedMembership]
  -> [OverlayEmbeddingObstruction]
splitOrderObstructions sourceId memberships =
  case memberships of
    firstMembership : secondMembership : remainingMemberships ->
      let finalMembership =
            List.foldl' (\_ current -> current) secondMembership remainingMemberships
          sourceFrom = roundedMembershipPoint firstMembership
          sourceTo = roundedMembershipPoint finalMembership
       in [ SplitOrderNotPreserved
              sourceId
              (resolvedVertexId (resolvedMembershipVertex leftMembership))
              (resolvedVertexId (resolvedMembershipVertex rightMembership))
          | (leftMembership, rightMembership) <-
              zip
                memberships
                (drop 1 memberships)
          , compare
              (resolvedMembershipParameter leftMembership)
              (resolvedMembershipParameter rightMembership)
              /= roundedOrderAlong
                sourceFrom
                sourceTo
                (roundedMembershipPoint leftMembership)
                (roundedMembershipPoint rightMembership)
          ]
    _ -> []

roundedMembershipPoint :: ResolvedMembership -> Point
roundedMembershipPoint =
  queryPointValue
    . projectedRoundedPoint
    . resolvedProjectedVertex
    . resolvedMembershipVertex

roundedOrderAlong :: Point -> Point -> Point -> Point -> Ordering
roundedOrderAlong
  (Point sourceFromX sourceFromY)
  (Point sourceToX sourceToY)
  (Point leftX leftY)
  (Point rightX rightY) =
    let directionX = sourceToX - sourceFromX
        directionY = sourceToY - sourceFromY
     in if abs directionX >= abs directionY
          then
            if directionX >= 0
              then compare leftX rightX
              else compare rightX leftX
          else
            if directionY >= 0
              then compare leftY rightY
              else compare rightY leftY

incidenceRelationObstructions
  :: SegmentRelation
  -> ResolvedSegment
  -> ResolvedSegment
  -> [OverlayEmbeddingObstruction]
incidenceRelationObstructions declaredRelation firstSegment secondSegment =
  [ IncidenceRelationChanged
      (resolvedSegmentId firstSegment)
      (resolvedSegmentId secondSegment)
      declaredRelation
      exactRelation
      roundedRelation
 | exactRelation /= declaredRelation || roundedRelation /= declaredRelation
  ]
 where
  exactRelation =
    relationFor exactSegmentRelation projectedExactPoint firstSegment secondSegment
  roundedRelation =
    relationFor Admitted.segmentRelation roundedProjectedPoint firstSegment secondSegment

relationFor
  :: (point -> point -> point -> point -> SegmentRelation)
  -> (ProjectedVertex -> point)
  -> ResolvedSegment
  -> ResolvedSegment
  -> SegmentRelation
relationFor relation project firstSegment secondSegment =
  relation
    (project (resolvedProjectedVertex (resolvedSegmentFrom firstSegment)))
    (project (resolvedProjectedVertex (resolvedSegmentTo firstSegment)))
    (project (resolvedProjectedVertex (resolvedSegmentFrom secondSegment)))
    (project (resolvedProjectedVertex (resolvedSegmentTo secondSegment)))

neighborhoodRotationObstructions
  :: ResolvedVertex
  -> NonEmpty ResolvedVertex
  -> [OverlayEmbeddingObstruction]
neighborhoodRotationObstructions center exactRotation =
  [ NeighborhoodRotationChanged
      (resolvedVertexId center)
      exactVertexRotation
      roundedVertexRotation
  | roundedRotationDescents > 1
  ]
 where
  exactVertexRotation = fmap resolvedVertexId exactRotation
  roundedRotationDescents =
    List.foldl'
      (\descentCount (left, right) ->
         descentCount
           + if compareRoundedAround center left right == GT
               then 1
               else 0)
      (0 :: Int)
      (cyclePairs exactRotation)
  roundedVertexRotation =
    fmap resolvedVertexId
      (NonEmpty.sortBy (compareRoundedAround center) exactRotation)

compareRoundedAround
  :: ResolvedVertex
  -> ResolvedVertex
  -> ResolvedVertex
  -> Ordering
compareRoundedAround center left right =
  compareRoundedAroundWith
    ( Admitted.orient2d
        (roundedVertexPoint center)
        (roundedVertexPoint left)
        (roundedVertexPoint right)
    )
    center
    left
    right

compareRoundedAroundWith
  :: Ordering
  -> ResolvedVertex
  -> ResolvedVertex
  -> ResolvedVertex
  -> Ordering
compareRoundedAroundWith roundedOrientation center left right =
  case compare (roundedVectorHalf center left) (roundedVectorHalf center right) of
    EQ ->
      case roundedOrientation of
        GT -> LT
        LT -> GT
        EQ -> compare (roundedVertexPoint left) (roundedVertexPoint right)
    ordering -> ordering

roundedVectorHalf :: ResolvedVertex -> ResolvedVertex -> Bool
roundedVectorHalf center neighbor =
  let Point centerX centerY = roundedVertexPoint center
      Point neighborX neighborY = roundedVertexPoint neighbor
      deltaX = neighborX - centerX
      deltaY = neighborY - centerY
   in deltaY < 0 || (deltaY == 0 && deltaX < 0)

roundedVertexPoint :: ResolvedVertex -> Point
roundedVertexPoint = roundedProjectedPoint . resolvedProjectedVertex

roundedProjectedPoint :: ProjectedVertex -> Point
roundedProjectedPoint = queryPointValue . projectedRoundedPoint

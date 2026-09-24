{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Canonical constraint segments and the atomic partial union of two
-- constrained meshes, retaining or combining their site annotations.
module Moonlight.Planar.Internal.Cdt.Union
  ( canonicalSegment
  , constraintSegments
  , unionConstrainedWith
  , unionConstrained
  , joinSeparatedConstrained
  , extendConstrainedWith
  , firstRejected
  , completeConstraintConflicts
  , orderedConflict
  , crossingIsRepresented
  ) where

import Control.Monad.ST (ST)
import qualified Data.Bifunctor as Bifunctor
import Data.Either (isRight)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Moonlight.Planar.Dcel as Dcel
import Moonlight.Planar.Internal.HandleDefs
import Moonlight.Planar.Internal.Canonical (canonicalize)
import Moonlight.Planar.Insertion (insertAdmittedPointCombining)
import Moonlight.Planar.Refinement (validateRefinementParameters)
import Moonlight.Planar.Internal.Cdt.Batch
  ( finalizeConstraintBatch
  , interpretConstraintRequests
  , recoverConstraints
  )
import Moonlight.Planar.Internal.Cdt.Build (fromDelaunay)
import Moonlight.Planar.Internal.Cdt.Combinators (foldWhileM)
import Moonlight.Planar.Internal.Cdt.Query (constraintEdges)
import Moonlight.Planar.IntersectionIterator (foldCorridorBetweenPoints)
import Moonlight.Planar.Internal.Cdt.Types
import Moonlight.Planar.Internal.Join.Seam
  ( executeConstrainedSeam
  , planSeam
  , seamExecutionBuildStats
  , seamExecutionPublicationStats
  , seamExecutionCachedFrontierPointReads
  , seamExecutionLeftFaceCount
  , seamExecutionJoinFaces
  , seamExecutionRightFaceEvidence
  , seamExecutionTriangulation
  )
import Moonlight.Planar.Internal.Join.SiteSet
  ( SiteSet
  , delaunayFromSiteSet
  , siteSetAssocs
  , siteSetFromTriangulation
  , siteSetPoints
  , siteSetSize
  , siteSetUnionWith
  )
import Moonlight.Planar.Internal.BoxedPaged (BoxedFill (..), boxedFill)
import Moonlight.Planar.Internal.Paged (TransactionShape (DenseTransaction, LocalTransaction))
import Moonlight.Planar.Internal.Predicates
  ( onClosedSegment
  , segmentsProperlyCross
  )
import Moonlight.Planar.Internal.Representation
import Moonlight.Planar.Internal.Mutable (MutableDcel)
import Moonlight.Planar.Internal.OperationState (OperationState)
import Moonlight.Planar.Internal.Transaction (runTransaction)
import Moonlight.Planar.Internal.Types
import Moonlight.Planar.Point (Point (..), queryPointValue)
import Moonlight.Planar.Internal.Point (QueryPoint (..))

canonicalSegment :: Point -> Point -> CanonicalSegment
canonicalSegment from to
  | from <= to = CanonicalSegment from to
  | otherwise = CanonicalSegment to from
{-# INLINE canonicalSegment #-}

-- | Geometry of the constraint section, deduplicated and canonically ordered.
constraintSegments
  :: Triangulation 'Constrained vertex directed undirected face
  -> V.Vector (CanonicalSegment)
constraintSegments triangulation =
  V.fromList
    ( Set.toAscList
        ( Set.fromList
            [ canonicalSegment
                (Dcel.vertexPoint triangulation from)
                (Dcel.vertexPoint triangulation to)
            | edge <- constraintEdges triangulation
            , let (from, to) = Dcel.undirectedEndpoints triangulation edge
            ]
        )
    )

-- | Atomic partial union of constrained meshes. Coincident sites combine
-- their annotations before construction. Complete canonical conflict
-- witnesses descend first; a successful branch then reaches the existing
-- batch corridor interpreter exactly once.
unionConstrainedWith
  :: (annotation -> annotation -> annotation)
  -> Triangulation 'Constrained annotation () () ()
  -> Triangulation 'Constrained annotation () () ()
  -> Either
      (ConstrainedUnionError)
      (Triangulation 'Constrained annotation () () ())
unionConstrainedWith combine left right =
  case NonEmpty.nonEmpty (Set.toAscList conflicts) of
    Just witnesses -> Left (ConstraintUnionConflicts witnesses)
    Nothing -> do
      unconstrained <-
        Bifunctor.first
          (ConstraintUnionConstructionFailed . CdtBuildError)
          (delaunayFromSiteSet unionSites)
      requests <- traverse (segmentRequestFromHandles (vertexHandles unconstrained)) (V.toList segments)
      recovered <-
        Bifunctor.first ConstraintUnionConstructionFailed
          (recoverConstraints (fromDelaunay unconstrained) (V.fromList requests))
      case firstRejected (constraintBatchOutcomes recovered) of
        Just blocking ->
          Left
            ( ConstraintUnionConstructionFailed
                (ConstraintIntersection blocking)
            )
        Nothing ->
          Bifunctor.first
            (ConstraintUnionConstructionFailed . CdtBuildError)
            (canonicalize (constraintBatchTriangulation recovered))
 where
  leftSites = siteSetFromTriangulation left
  rightSites = siteSetFromTriangulation right
  unionSites = siteSetUnionWith combine leftSites rightSites
  segments =
    V.fromList
      ( Set.toAscList
          ( Set.union
              (Set.fromList (V.toList (constraintSegments left)))
              (Set.fromList (V.toList (constraintSegments right)))
          )
      )
  conflicts = completeConstraintConflicts unionSites segments
{-# INLINE unionConstrainedWith #-}

-- | Atomic partial union specialized to geometry-only constrained meshes.
unionConstrained
  :: Triangulation 'Constrained () () () ()
  -> Triangulation 'Constrained () () () ()
  -> Either
      (ConstrainedUnionError)
      (Triangulation 'Constrained () () () ())
unionConstrained = unionConstrainedWith (\_ _ -> ())
{-# INLINE unionConstrained #-}

-- | Join two strictly separated constrained triangulations with the caller's
-- left mesh resident. The right source is appended and only their common
-- tangent corridor and its selected-face-free exterior cavity are legalized.
-- The selector sees source-local handles on an explicit resident/incoming
-- side; selected face handles and constraint contours are immutable barriers.
-- Passing a selector that is always true retains the preserve-all law and its
-- typed incompatibility when the seam requires a source-face rewrite. The
-- result carries the exact final J component plus transport evidence only for
-- selected incoming faces.
-- The resident operand must have gone through 'geometryOnlyPublication'; a
-- dense boxed payload section would make local freeze enumerate the resident
-- world and is refused as a typed obstruction.
--
-- The published constraint section is exactly the source constraints union
-- the synthetic lower and upper seam bridge paths. Those paths are certified
-- perimeter constraints for the joined world and are subdivided locally when
-- an edge-length law requests it; the operation does not perform a global
-- constraint scan or canonicalization.
--
-- Strict separation in one admitted chart (x, or the orientation-preserving
-- y chart used by the seam planner) proves that the two site sets have no
-- coincident point, so annotations are copied from their source mesh and
-- never combined.
joinSeparatedConstrained
  :: (ConstrainedSeamSide -> FaceId -> Bool)
  -> RefinementParameters
  -> Triangulation 'Constrained annotation () () ()
  -> Triangulation 'Constrained annotation () () ()
  -> Either
      (ConstrainedUnionError)
      (ConstrainedSeamResult annotation)
joinSeparatedConstrained sourceFacePreserved parameters left right = do
  Bifunctor.first
    (ConstraintUnionConstructionFailed . CdtBuildError)
    (validateRefinementParameters parameters)
  seamPlan <- maybe (Left ConstraintUnionNotSeparated) Right (planSeam left right)
  vertexDefault <- ensureDefaultedPayloads left right
  seamExecution <-
    Bifunctor.first
      (ConstraintUnionConstructionFailed . CdtBuildError)
      ( executeConstrainedSeam
          sourceFacePreserved
          parameters
          vertexDefault
          seamPlan
          left
          right
      )
  let published = seamExecutionTriangulation seamExecution
  pure
    ConstrainedSeamResult
      { constrainedSeamResultTriangulation = published
      , constrainedSeamLeftFaceCount = seamExecutionLeftFaceCount seamExecution
      , constrainedSeamRightFaceEvidence = seamExecutionRightFaceEvidence seamExecution
      , constrainedSeamJoinFaces = seamExecutionJoinFaces seamExecution
      , constrainedSeamLeftConstraintCount = Dcel.numConstraints left
      , constrainedSeamConstraintStats = ConstraintBatchStats 0 0 0 0 0 0
      , constrainedSeamBuildStats = seamExecutionBuildStats seamExecution
      , constrainedSeamPublicationStats = seamExecutionPublicationStats seamExecution
      , constrainedSeamCachedFrontierPointReads = seamExecutionCachedFrontierPointReads seamExecution
      }

ensureDefaultedPayloads
  :: Triangulation 'Constrained annotation () () ()
  -> Triangulation 'Constrained annotation () () ()
  -> Either ConstrainedUnionError annotation
ensureDefaultedPayloads left right =
  case (boxedFill (triVertexData left), boxedFill (triVertexData right)) of
    (Fill vertexDefault, Fill _) -> Right vertexDefault
    _ -> Left ConstraintUnionRequiresGeometryOnlyPublication

-- | Extend one already-resident constrained triangulation with one new
-- constrained section. This is intentionally asymmetric: the base mesh is
-- thawed once, extension sites are inserted into it, and only the extension's
-- constraint section is replayed. Unlike 'unionConstrainedWith', it neither
-- rebuilds a canonical site set nor replays base constraints, because both
-- would erase the physical distinction between solved base and new work.
--
-- Incoming constraint recovery is itself the spatial conflict authority. It
-- walks only the incoming corridors against the resident base and returns a
-- typed intersection obstruction. Re-running the canonical all-pairs union
-- preflight here would make a tiny extension quadratic in the base. Any
-- structural or recovery obstruction abandons the transaction before a
-- partially extended mesh can be published.
extendConstrainedWith
  :: (annotation -> annotation -> annotation)
  -> Triangulation 'Constrained annotation () () ()
  -> Triangulation 'Constrained annotation () () ()
  -> Either
      (ConstrainedUnionError)
      (ConstrainedExtensionResult annotation () () ())
extendConstrainedWith combine base extension = do
    (completed, extended, buildStats) <-
      runTransaction
        (ConstraintUnionConstructionFailed . CdtBuildError)
        transactionShape
        base
        (siteSetSize extensionSites)
        (insertAndRecoverExtension combine extensionSites extensionSegments)
    pure
      ConstrainedExtensionResult
        { constrainedExtensionConstraintBatch = finalizeConstraintBatch extended completed
        , constrainedExtensionBuildStats = buildStats
        }
 where
  extensionSites = siteSetFromTriangulation extension
  extensionSegments = constraintSegments extension
  transactionShape =
    case V.uncons extensionSegments of
      Just (segment, remaining)
        | Dcel.numVertices base >= 200000
        , siteSetSize extensionSites <= 128
        , V.null remaining
        , residentCorridorIsEmpty segment -> LocalTransaction
      _ -> DenseTransaction
  residentCorridorIsEmpty segment =
    foldCorridorBetweenPoints
      base
      (QueryPoint (segmentStart segment))
      (QueryPoint (segmentEnd segment))
      (\_ _ -> Left ())
      ()
      == Just (Right ())
{-# INLINE extendConstrainedWith #-}

insertAndRecoverExtension
  :: (annotation -> annotation -> annotation)
  -> SiteSet annotation
  -> V.Vector (CanonicalSegment)
  -> MutableDcel s annotation () () ()
  -> OperationState s
  -> ST s (Either (ConstrainedUnionError) ConstraintBatchAccumulator)
insertAndRecoverExtension combine extensionSites extensionSegments mutable operation = do
  placed <- insertExtensionSites combine extensionSites mutable operation
  case placed of
    Left obstruction -> pure (Left obstruction)
    Right handles ->
      case traverse (segmentRequestFromHandles handles) (V.toList extensionSegments) of
        Left obstruction -> pure (Left obstruction)
        Right requests -> do
          interpreted <-
            fmap
              (Bifunctor.first ConstraintUnionConstructionFailed)
              (interpretConstraintRequests (V.fromList requests) mutable operation)
          case interpreted of
            Left obstruction -> pure (Left obstruction)
            Right completed ->
              case firstRejected (accumulatorOutcomes completed) of
                Just blocking ->
                  pure
                    ( Left
                        ( ConstraintUnionConstructionFailed
                            (ConstraintIntersection blocking)
                        )
                    )
                Nothing -> pure (Right completed)

insertExtensionSites
  :: forall s annotation
   . (annotation -> annotation -> annotation)
  -> SiteSet annotation
  -> MutableDcel s annotation () () ()
  -> OperationState s
  -> ST s (Either (ConstrainedUnionError) (Map.Map (Point) VertexId))
insertExtensionSites combine extensionSites mutable operation =
  fmap
    (fmap (Map.fromDistinctAscList . reverse))
    ( foldWhileM
        isRight
        insertOne
        (Right [])
        (siteSetAssocs extensionSites)
    )
 where
  insertOne
    :: Either (ConstrainedUnionError) [(Point, VertexId)]
    -> (QueryPoint, annotation)
    -> ST s (Either (ConstrainedUnionError) [(Point, VertexId)])
  insertOne rejected@(Left _) _ = pure rejected
  insertOne (Right accumulated) (queryPoint, annotation) =
    fmap
      ( Bifunctor.first (ConstraintUnionConstructionFailed . CdtBuildError)
          . fmap
            ( \(vertex, _) ->
                ( queryPointValue queryPoint
                , VertexId (fromIntegral vertex)
                ) : accumulated
            )
      )
      (insertAdmittedPointCombining combine Nothing mutable operation queryPoint annotation)

accumulatorOutcomes :: ConstraintBatchAccumulator -> V.Vector ConstraintOutcome
accumulatorOutcomes = V.fromList . reverse . accumulatedConstraintOutcomes
{-# INLINE accumulatorOutcomes #-}

vertexHandles :: Triangulation mode annotation () () () -> Map.Map Point VertexId
vertexHandles triangulation =
  Map.fromList
    [ ( Dcel.vertexPoint triangulation vertex
      , vertex
      )
    | raw <- [0 .. Dcel.numVertices triangulation - 1]
    , let vertex = VertexId (fromIntegral raw)
    ]

segmentRequestFromHandles
  :: Map.Map (Point) VertexId
  -> CanonicalSegment
  -> Either (ConstrainedUnionError) (VertexId, VertexId)
segmentRequestFromHandles handles segment =
  case
      ( Map.lookup (segmentStart segment) handles
      , Map.lookup (segmentEnd segment) handles
      ) of
    (Just from, Just to) -> Right (from, to)
    (Nothing, _) -> Left (ConstraintUnionSiteMissing (segmentStart segment))
    (_, Nothing) -> Left (ConstraintUnionSiteMissing (segmentEnd segment))

firstRejected :: V.Vector ConstraintOutcome -> Maybe UndirectedEdgeId
firstRejected =
  V.foldr
    (\outcome later ->
       case outcome of
         ConstraintAccepted _ _ -> later
         ConstraintRejected blocking -> Just blocking
    )
    Nothing

completeConstraintConflicts
  :: SiteSet annotation
  -> V.Vector (CanonicalSegment)
  -> Set.Set (ConstraintConflict)
completeConstraintConflicts unionSites segments =
  Set.fromList
    [ orderedConflict leftSegment rightSegment
    | leftIndex <- [0 .. V.length segments - 1]
    , rightIndex <- [leftIndex + 1 .. V.length segments - 1]
    , let leftSegment = segments V.! leftIndex
    , let rightSegment = segments V.! rightIndex
    , segmentsProperlyCross
        (segmentStart leftSegment)
        (segmentEnd leftSegment)
        (segmentStart rightSegment)
        (segmentEnd rightSegment)
    , not (crossingIsRepresented unionSites leftSegment rightSegment)
    ]

orderedConflict
  :: CanonicalSegment
  -> CanonicalSegment
  -> ConstraintConflict
orderedConflict left right
  | left <= right = ConstraintConflict left right
  | otherwise = ConstraintConflict right left

crossingIsRepresented
  :: SiteSet annotation
  -> CanonicalSegment
  -> CanonicalSegment
  -> Bool
crossingIsRepresented sites first second =
  V.any
    (\point ->
       onClosedSegment (segmentStart first) (segmentEnd first) point
         && onClosedSegment (segmentStart second) (segmentEnd second) point
    )
    (siteSetPoints sites)

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The vocabulary: the types the surface names, none of which mentions the
-- stored representation.
module Moonlight.Planar.Internal.Types
  ( SiteRelation (..)
  , ElementDefaults (..)
  , unitElementDefaults
  , ConstraintMode (..)
  , KnownConstraintMode (..)
  , InsertionDisposition (..)
  , ClosureStats (..)
  , validatePoint
  , BuildError (..)
  , Location (..)
  , LocationHint (..)
  , LocationStats (..)
  , emptyLocationStats
  , NearestStats (..)
  , RefinementParameters (..)
  , defaultRefinementParameters
  , InvariantViolation (..)
  , PlanarIncidenceError (..)
  ) where

import Control.DeepSeq (NFData (..))
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId
  , FaceId
  , UndirectedEdgeId
  , VertexId
  )
import Data.Word (Word8)
import qualified Data.IntSet as IntSet
import GHC.Generics (Generic)
import Moonlight.Planar.Internal.BoxedPaged (BoxedStorageError)
import Moonlight.Planar.Point (Point (..), QueryPoint, PointValidationError (..), mkQueryPoint)
import Moonlight.Planar.Scalar (CoordinateError, NonFiniteValue)

-- | Type-level witness for whether constraint flags may be present.
data ConstraintMode = Unconstrained | Constrained
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Recover a type-level constraint mode as a value.
class KnownConstraintMode (mode :: ConstraintMode) where
  constraintModeValue :: proxy mode -> ConstraintMode

instance KnownConstraintMode 'Unconstrained where
  constraintModeValue _ = Unconstrained

instance KnownConstraintMode 'Constrained where
  constraintModeValue _ = Constrained

-- | Sizes of the closure a checked local domain spans: its admitted faces
-- with the collar across the interface, the directed edges those faces and
-- interface pairs carry, the vertices those edges join, the interface pairs,
-- and the undirected pairs beneath the selected edges (the last field's name
-- predates that meaning). The counts are taken from the closure membership
-- the domain already knows; nothing is validated to produce them. The names
-- keep the older validation vocabulary until the coordinated rename.
data ClosureStats = ClosureStats
  { closureFaces :: {-# UNPACK #-} !Int
  , closureDirectedEdges :: {-# UNPACK #-} !Int
  , closureVertices :: {-# UNPACK #-} !Int
  , closureInterfacePairs :: {-# UNPACK #-} !Int
  , closureConstraintPairs :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Exact geometric relation between two finite coordinate supports. The
-- overlap count is strictly positive in 'PartialOverlap'; equality, subset and
-- disjointness have already been excluded before that constructor is chosen.
data SiteRelation
  = -- | Both supports contain exactly the same coordinates.
    EqualSites
  | -- | Every left coordinate occurs on the right, which has at least one more.
    LeftProperSubset
  | -- | Every right coordinate occurs on the left, which has at least one more.
    RightProperSubset
  | -- | The supports share no coordinate.
    DisjointSites
  | -- | Neither support contains the other; the field is the positive number
    -- of coordinates they share.
    PartialOverlap {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Payloads inherited by topology elements created after initial loading.
data ElementDefaults directed undirected face = ElementDefaults
  { defaultDirectedEdgeData :: !directed
  , defaultUndirectedEdgeData :: !undirected
  , defaultFaceData :: !face
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Payload defaults for geometry-only triangulations.
unitElementDefaults :: ElementDefaults () () ()
unitElementDefaults = ElementDefaults () () ()

-- | Whether an insertion published a new site or selected an existing one.
data InsertionDisposition = Inserted | AlreadyPresent
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Validate a construction point while retaining its optional input slot.
validatePoint :: Maybe Int -> Point -> Either BuildError (QueryPoint)
validatePoint slot point@(Point x y) =
  case mkQueryPoint point of
    Left (InvalidPointX reason) -> Left (InvalidCoordinate slot x reason)
    Left (InvalidPointY reason) -> Left (InvalidCoordinate slot y reason)
    Right queryPoint -> Right queryPoint

-- | Total construction and rewrite obstruction surface.
data BuildError
  = InvalidCoordinate !(Maybe Int) {-# UNPACK #-} !Double !CoordinateError
  | PointLocationFailed !(Point)
  | LocationWalkExhausted !(Point) {-# UNPACK #-} !Int
  | RefinementInputTopologyInvalid !InvariantViolation
  | SeamFrontierUnavailable
  | SeamSourceEdgeRequiresFlip !UndirectedEdgeId
  | FreshInsertionMatchedExistingVertex !VertexId !VertexId
  | DegenerateLineEndpointMissingOutgoing !VertexId
  | DegenerateLineEndpointTurnMissing {-# UNPACK #-} !Int
  | DegenerateLineConnectedVertexMissing {-# UNPACK #-} !Int
  | HullStartNotVisible !DirectedEdgeId
  | OuterRangeDidNotTerminate !DirectedEdgeId !DirectedEdgeId {-# UNPACK #-} !Int
  | OuterRangeContainsInnerEdge !DirectedEdgeId !FaceId
  | ConstrainedEdgeFlipRefused !UndirectedEdgeId
  | RemovalVertexOutOfRange !VertexId {-# UNPACK #-} !Int
  | RemovalEdgeOutOfRange !UndirectedEdgeId {-# UNPACK #-} !Int
  | RemovalFaceOutOfRange !FaceId {-# UNPACK #-} !Int
  | RemovalFaceCycleDidNotTerminate
      !FaceId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  | RemovalEmptyTriangulation !VertexId
  | RemovalTwoPointDegreeMismatch !VertexId {-# UNPACK #-} !Int
  | RemovalCollinearDegreeMismatch !VertexId {-# UNPACK #-} !Int
  | RemovalBorderTooShort {-# UNPACK #-} !Int
  | RemovalBorderArityMismatch {-# UNPACK #-} !Int
  | RemovalOutgoingCycleDidNotTerminate
      !VertexId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  | CircleSweepRequiresDenseStorage
  | CircleSweepHullEmpty
  | OuterCycleDidNotTerminate
      !DirectedEdgeId
      !DirectedEdgeId
      {-# UNPACK #-} !Int
  | HierarchyLevelPopulationMismatch
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | HierarchyInsertionHandleMismatch !VertexId !VertexId
  | PointIndexCapacityExhausted {-# UNPACK #-} !Int
  | RefinementMinimumAngleNotFinite !NonFiniteValue
  | RefinementMinimumAngleOutOfRange {-# UNPACK #-} !Double
  | RefinementMinimumAngleDerivedRatioNotFinite !NonFiniteValue
  | RefinementMaximumAdditionalVerticesNegative {-# UNPACK #-} !Int
  | RefinementMinimumAreaNotFinite !NonFiniteValue
  | RefinementMinimumAreaNegative {-# UNPACK #-} !Double
  | RefinementMaximumAreaNotFinite !NonFiniteValue
  | RefinementMaximumAreaNotPositive {-# UNPACK #-} !Double
  | RefinementMaximumRadiusEdgeRatioNotFinite !NonFiniteValue
  | RefinementMaximumRadiusEdgeRatioNotPositive {-# UNPACK #-} !Double
  | RefinementMaximumEdgeLengthNotFinite !NonFiniteValue
  | RefinementMaximumEdgeLengthNotPositive {-# UNPACK #-} !Double
  | RefinementMinimumAreaExceedsMaximum
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
  | RefinementSeedFaceNotActive !FaceId {-# UNPACK #-} !Int
  | RefinementDomainTopologyChanged
  | RefinementDomainRequiresConvexHullPreservation
  | RefinementDomainRequiresConstraintPreservation
  | RefinementDomainForbidsOuterFaceExclusion
  | RefinementDomainRequiresFiniteVertexBudget
  | RefinementSeamBridgeBudgetExceeded
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  | RefinementSeamBridgeMidpointCollapsed !UndirectedEdgeId
  | BoundarySplitRequiresBoundaryEdge !UndirectedEdgeId
  | RefinementDomainWouldCrossInterface !UndirectedEdgeId !FaceId
  | RefinementDomainWouldRewriteProtectedFace !FaceId
  | RefinementDomainProtectedFaceChanged !FaceId
  | RefinementDomainInterfaceOppositeFaceNotPermitted !UndirectedEdgeId !FaceId
  | RefinementOversizedEdge
      !FaceId
      !UndirectedEdgeId
      {-# UNPACK #-} !Double
      {-# UNPACK #-} !Double
  | CapacityExceeded {-# UNPACK #-} !Int
  | HalfEdgeCapacityExceeded {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | FaceCapacityExceeded {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | PayloadStorageFailure !BoxedStorageError
  | CoordinatePayloadCountMismatch
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Exact position of a query relative to the finite triangulation.
data Location
  = EmptyTriangulation
  | OnVertex !VertexId
  | OnEdge !DirectedEdgeId
  | InFace !FaceId
  | OutsideConvexHull !(Maybe DirectedEdgeId)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Optional starting cell for point-location descent.
data LocationHint
  = VertexHint !VertexId
  | FaceHint !FaceId
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Work performed by point-location descent.
data LocationStats = LocationStats
  { locationWalkSteps :: {-# UNPACK #-} !Int
  , locationUsedFallback :: !Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Location telemetry for a query that required no descent.
emptyLocationStats :: LocationStats
emptyLocationStats = LocationStats 0 False

-- | Work performed by a nearest-neighbor query.
data NearestStats = NearestStats
  { nearestWalkSteps :: {-# UNPACK #-} !Int
  , nearestDistanceTests :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Independent quality bounds and a finite Steiner-vertex budget.
data RefinementParameters = RefinementParameters
  { refineMaxAdditionalVertices :: !(Maybe Int)
  , refineMinArea :: !(Maybe Double)
  , refineMaxArea :: !(Maybe Double)
  , refineMaxRadiusEdgeRatio :: !(Maybe Double)
  , refineMaxEdgeLength :: !(Maybe Double)
  , refinePreserveConvexHull :: !Bool
  , refineKeepConstraintEdges :: !Bool
  , refineExcludeOuterFaces :: !Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Conservative refinement defaults with no explicit area or edge bounds.
defaultRefinementParameters :: RefinementParameters
defaultRefinementParameters =
  RefinementParameters
    { refineMaxAdditionalVertices = Nothing
    , refineMinArea = Nothing
    , refineMaxArea = Nothing
    , refineMaxRadiusEdgeRatio = Just 1
    , refineMaxEdgeLength = Nothing
    , refinePreserveConvexHull = True
    , refineKeepConstraintEdges = False
    , refineExcludeOuterFaces = False
    }

-- | Geometry-free finite incidence refusals, shared by native meshes and
-- exact arrangements. The representation does not own a second vocabulary.
data PlanarIncidenceError
  = IncidencePackedLayoutInvalid !Int !Int !Int
  | IncidenceVertexOriginInvalid !DirectedEdgeId !VertexId
  | IncidenceEdgeLinksInvalid !DirectedEdgeId !DirectedEdgeId !DirectedEdgeId
  | IncidenceEdgeFaceInvalid !DirectedEdgeId !FaceId
  | IncidenceEdgeLinkMismatch !DirectedEdgeId
  | IncidenceEdgeEndpointMismatch !DirectedEdgeId
  | IncidenceVertexRootInvalid !VertexId !(Maybe DirectedEdgeId)
  | IncidenceFaceRootInvalid !FaceId !DirectedEdgeId
  | IncidenceSparseFaceInvalid !Int
  | IncidenceBoundedFaceWithoutBoundary !FaceId
  | IncidenceBoundaryCoverageInvalid !IntSet.IntSet !IntSet.IntSet
  | IncidenceVertexStarCoverageInvalid !IntSet.IntSet !IntSet.IntSet
  | IncidenceIsolatedOwnershipInvalid !IntSet.IntSet !IntSet.IntSet
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | A concrete witness that an immutable DCEL law does not hold.
data InvariantViolation
  = CoordinatePlaneLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | VertexOutgoingLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | VertexPayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | TopologyArenaLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | DirectedPayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | UndirectedPayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | DirectedEdgeCountOdd {-# UNPACK #-} !Int
  | ConstraintLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | NonCanonicalConstraintFlag !UndirectedEdgeId !Word8
  | CachedConstraintCountMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | CachedConstraintIndexMismatch
  | MissingOuterFace
  | FacePayloadLengthMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | EdgeOriginOutOfRange !DirectedEdgeId !VertexId {-# UNPACK #-} !Int
  | EdgeNextOutOfRange !DirectedEdgeId !DirectedEdgeId {-# UNPACK #-} !Int
  | EdgePreviousOutOfRange !DirectedEdgeId !DirectedEdgeId {-# UNPACK #-} !Int
  | EdgeFaceOutOfRange !DirectedEdgeId !FaceId {-# UNPACK #-} !Int
  | VertexOutgoingOutOfRange !VertexId !DirectedEdgeId {-# UNPACK #-} !Int
  | FaceAdjacentOutOfRange !FaceId !DirectedEdgeId {-# UNPACK #-} !Int
  | EdgeSelfLinkedNext !DirectedEdgeId
  | EdgeSelfLinkedPrevious !DirectedEdgeId
  | InnerFaceNotTriangularAtEdge !DirectedEdgeId
  | FaceMissingAdjacentEdge !FaceId
  | FaceRepresentativeMismatch !FaceId !DirectedEdgeId !FaceId
  | InnerFaceVertexCardinalityMismatch !FaceId {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | ConnectedVertexMissingOutgoing !VertexId
  | VertexOutgoingOriginMismatch !VertexId !DirectedEdgeId !VertexId
  | CollinearEdgeCountMismatch {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | EulerCharacteristicMismatch {-# UNPACK #-} !Int
  | InnerFaceNotCounterClockwise !FaceId
  | LocallyIllegalDelaunayEdge !UndirectedEdgeId
  | DelaunayIncidentFaceNotTriangular !UndirectedEdgeId
  | IncidenceViolation !PlanarIncidenceError
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

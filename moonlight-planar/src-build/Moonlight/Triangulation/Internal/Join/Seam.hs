{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linear seam construction for separated Delaunay triangulations. Admission
-- returns an opaque proof carrying the exact source order and tangents; the
-- executor therefore has no untyped precondition and owns no fallback.
module Moonlight.Triangulation.Internal.Join.Seam
  ( SeamPlan
  , planSeam
  , executeSeam
  , ConstrainedSeamExecution
  , seamExecutionTriangulation
  , seamExecutionBuildStats
  , seamExecutionPublicationStats
  , seamExecutionCachedFrontierPointReads
  , seamExecutionLeftFaceCount
  , seamExecutionRightFaceEvidence
  , seamExecutionJoinFaces
  , executeConstrainedSeam
  ) where

import Control.Applicative ((<|>))
import Control.Monad.ST (ST, runST)
import Data.Bits (finiteBitSize, xor)
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Moonlight.Triangulation.Dcel
  ( adjacentEdge
  , faceDirectedEdges
  , faceVertices
  , incidentFace
  , isConstraintEdge
  , numDirectedEdges
  , numFaces
  , numInnerFaces
  , numVertices
  , origin
  , previous
  , vertexOutEdge
  , vertexData
  )
import Moonlight.Triangulation.Internal.FloodFillIterator (floodFillFacesWithRejectedEdges)
import Moonlight.Triangulation.Internal.HandleDefs
  ( DirectedEdgeId (..)
  , FaceId (..)
  , UndirectedEdgeId (..)
  , VertexId (..)
  , directedPair
  )
import Moonlight.Triangulation.Internal.DcelOperations.Hull (closeOuterTurn)
import Moonlight.Triangulation.Internal.DcelOperations.Legalize
  ( legalizeEdges
  , legalizeEdgesPinned
  )
import Moonlight.Triangulation.Internal.DcelOperations.Subdivide
  ( splitBoundaryEdgeWithLegalization
  )
import Moonlight.Triangulation.Internal.DcelOperations.FlipRule (illegalDiagonal)
import Moonlight.Triangulation.Internal.Cdt.Combinators (bindMutable)
import Moonlight.Triangulation.Internal.Cdt.Query (constraintEdges)
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (CounterSteinerPoints)
  , OperationState
  , addCounter
  , freezeBuildStats
  , newOperationState
  )
import Moonlight.Triangulation.Internal.Paged
  ( PublicationStats
  , TransactionShape (LocalTransaction)
  , pagedUnsafeIndex
  )
import Moonlight.Triangulation.Internal.Representation
  ( SeamFrontierIndex (..)
  , Triangulation (..)
  , seamQuarterTurn
  , summarizeSeamChart
  )
import Moonlight.Triangulation.Internal.Cdt.Types
  ( ConstrainedSeamFaceEvidence (..)
  , ConstrainedSeamSide (..)
  )
import Moonlight.Triangulation.Internal.Types
  ( BuildError (..)
  , BuildStats
  , ConstraintMode (Constrained, Unconstrained)
  , InvariantViolation (..)
  , Point (..)
  , QueryPoint (queryPointValue)
  , RefinementParameters (..)
  , unitElementDefaults
  )
import Moonlight.Triangulation.Internal.Predicates
  ( inCircleCoordinates
  , orient2dCoordinates
  )
import Moonlight.Triangulation.Math (distance, midpoint, validatePoint)
import Moonlight.Triangulation.Internal.Transaction (runTransactionWithPublication)

-- | Proof that a particular pair can be copied and stitched by the seam
-- kernel. Constructors stay private so an unseparated pair cannot be handed to
-- the executor by accident.
data SeamPlan
  = -- | The operands are already in geometric left/right order.  The two
    -- retained frontiers are therefore the exact ones consumed by the zipper.
    SeamLeftBeforeRight !SeamTangents !SeamFrontierIndex !SeamFrontierIndex
  | -- | The input operands are reversed, but the retained frontiers remain in
    -- geometric left/right order rather than input-operand order.
    SeamRightBeforeLeft !SeamTangents !SeamFrontierIndex !SeamFrontierIndex

-- | Coordinate chart used by the separated seam.  The second chart is the
-- orientation-preserving quarter turn @(u,v) = (y,-x)@; exact predicates and
-- the zipper therefore remain unchanged while north/south admission reuses
-- the same tangent interpreter.
data SeamAxis
  = SeamAxisX
  | SeamAxisY
  deriving stock (Eq, Show)

planSeam
  :: Triangulation mode vertex () () ()
  -> Triangulation mode' vertex () () ()
  -> Maybe SeamPlan
planSeam left right = do
  leftFrontier <- triSeamFrontier left
  rightFrontier <- triSeamFrontier right
  planAxis SeamAxisX leftFrontier rightFrontier
    <|> planAxis SeamAxisY leftFrontier rightFrontier
 where
  planAxis axis leftFrontier rightFrontier =
    case separatedOrder axis left right leftFrontier rightFrontier of
      Just LeftBeforeRight ->
        (\tangents -> SeamLeftBeforeRight tangents leftFrontier rightFrontier)
          <$> seamTangents axis left leftFrontier right rightFrontier
      Just RightBeforeLeft ->
        (\tangents -> SeamRightBeforeLeft tangents rightFrontier leftFrontier)
          <$> seamTangents axis right rightFrontier left leftFrontier
      Nothing -> Nothing

-- | Execute a proved seam schedule. Numbering follows the schedule;
-- 'Moonlight.Triangulation.Dcel.canonicalize' remains the explicit
-- construction-independent observation.
executeSeam
  :: forall vertex
   . SeamPlan
  -> Triangulation 'Unconstrained vertex () () ()
  -> Triangulation 'Unconstrained vertex () () ()
  -> Either BuildError (Triangulation 'Unconstrained vertex () () ())
executeSeam plan left right =
  case plan of
    SeamLeftBeforeRight tangents _ _ ->
      fmap fst (mergeSeparated left right tangents)
    SeamRightBeforeLeft tangents _ _ ->
      fmap fst (mergeSeparated right left tangents)

-- | Result of the constrained seam kernel. Constraint flags are copied before
-- legalization, so source contour edges are immutable barriers while the
-- zipper constructs only the missing corridor.
data ConstrainedSeamExecution vertex = ConstrainedSeamExecution
  { seamExecutionTriangulation
      :: !(Triangulation 'Constrained vertex () () ())
  , seamExecutionBuildStats :: !BuildStats
  , seamExecutionPublicationStats :: !PublicationStats
  , seamExecutionCachedFrontierPointReads :: {-# UNPACK #-} !Int
  , seamExecutionLeftFaceCount :: {-# UNPACK #-} !Int
  , seamExecutionRightFaceEvidence :: !(V.Vector ConstrainedSeamFaceEvidence)
  , seamExecutionJoinFaces :: !(V.Vector FaceId)
  }

-- | Execute a proved seam while transporting both source constraint planes.
-- This is distinct from promoting the unconstrained result afterward: source
-- hull constraints must already be visible to seam legalization or the
-- legalization schedule could erase a solved source face before recovery had
-- a chance to mark it.
executeConstrainedSeam
  :: forall vertex
   . (ConstrainedSeamSide -> FaceId -> Bool)
  -> RefinementParameters
  -> vertex
  -> SeamPlan
  -> Triangulation 'Constrained vertex () () ()
  -> Triangulation 'Constrained vertex () () ()
  -> Either BuildError (ConstrainedSeamExecution vertex)
executeConstrainedSeam sourceFacePreserved parameters vertexDefault plan left right = do
  let (order, tangents, leftFrontier, rightFrontier) =
        case plan of
          SeamLeftBeforeRight selectedTangents selectedLeftFrontier selectedRightFrontier ->
            (GeometricBaseLeft, selectedTangents, selectedLeftFrontier, selectedRightFrontier)
          SeamRightBeforeLeft selectedTangents selectedLeftFrontier selectedRightFrontier ->
            (GeometricBaseRight, selectedTangents, selectedLeftFrontier, selectedRightFrontier)
  (triangulation, statistics, publicationStats, joinFaces) <-
    executeResidentConstrained
      targetFaceProtected
      parameters
      vertexDefault
      order
      left
      right
      leftFrontier
      rightFrontier
      tangents
  rightEvidence <-
    seamFaceEvidenceForSource
      right
      incomingFaceOffset
      rightEvidenceFaces
  pure
    ConstrainedSeamExecution
      { seamExecutionTriangulation = triangulation
      , seamExecutionBuildStats = statistics
      , seamExecutionPublicationStats = publicationStats
      , seamExecutionCachedFrontierPointReads =
          seamFrontierPointReads tangents
            + case refineMaxEdgeLength parameters of
              Nothing -> 0
              Just _ -> 4
      , seamExecutionLeftFaceCount = numInnerFaces left
      , seamExecutionRightFaceEvidence = rightEvidence
      , seamExecutionJoinFaces = joinFaces
      }
 where
  !residentFaceLimit = numFaces left
  !incomingFaceOffset = residentFaceLimit - 1
  !incomingFaceLimit = incomingFaceOffset + numFaces right

  targetFaceProtected rawFace
    | rawFace <= 0 = False
    | rawFace < residentFaceLimit =
        sourceFacePreserved SeamResident (FaceId (fromIntegral rawFace))
    | rawFace < incomingFaceLimit =
        sourceFacePreserved
          SeamIncoming
          (FaceId (fromIntegral (rawFace - incomingFaceOffset)))
    | otherwise = False

  rightEvidenceFaces =
    filter
      (sourceFacePreserved SeamIncoming)
      (fmap (FaceId . fromIntegral) [1 .. numFaces right - 1])

-- | Descend exactly the unprotected face component touched by the zipper.
-- This is the authoritative J section: it contains both newly allocated seam
-- faces and any source exterior filler lawfully retriangulated into them.
exactJoinFaces
  :: Triangulation 'Constrained vertex () () ()
  -> (Int -> Bool)
  -> V.Vector FaceId
  -> (V.Vector FaceId, [UndirectedEdgeId])
exactJoinFaces triangulation targetFaceProtected seeds =
  let (faces, rejectedPairs) =
        floodFillFacesWithRejectedEdges
          triangulation
          (V.toList seeds)
          canCross
   in (V.fromList faces, rejectedPairs)
 where
  canCross edge =
    not (isConstraintEdge triangulation edge)
      && not
        ( any
            targetFaceIsProtected
            (incidentInnerFaces triangulation edge)
        )

  targetFaceIsProtected (FaceId raw) =
    targetFaceProtected (fromIntegral raw)

-- | Certify only the J/selected-face overlap after local descent. A selected
-- source face is immutable; therefore an unconstrained overlap edge that still
-- requires a Delaunay flip is a genuine incompatibility, never permission to
-- rewrite the solved section.
certifyJoinBoundary
  :: Triangulation 'Constrained vertex () () ()
  -> [UndirectedEdgeId]
  -> Either BuildError ()
certifyJoinBoundary triangulation = traverse_ certifyPair
 where
  certifyPair edge =
    if isConstraintEdge triangulation edge
          || not (immutableEdgeRequiresFlip triangulation edge)
          then Right ()
          else Left (SeamSourceEdgeRequiresFlip edge)

incidentInnerFaces
  :: Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> [FaceId]
incidentInnerFaces triangulation edge =
  filter (/= FaceId 0)
    [ incidentFace triangulation forward
    , incidentFace triangulation backward
    ]
 where
  (forward, backward) = directedPair edge

immutableEdgeRequiresFlip
  :: Triangulation mode vertex directed undirected face
  -> UndirectedEdgeId
  -> Bool
immutableEdgeRequiresFlip triangulation edge =
  leftFace /= FaceId 0
    && rightFace /= FaceId 0
    && orient2dCoordinates cx cy dx dy bx by == GT
    && orient2dCoordinates dx dy cx cy ax ay == GT
    && illegalDiagonal ax ay bx by cx cy dx dy
 where
  (forward, backward) = directedPair edge
  leftFace = incidentFace triangulation forward
  rightFace = incidentFace triangulation backward
  forwardPrevious = previous triangulation forward
  backwardPrevious = previous triangulation backward
  Point ax ay = vertexPointAt triangulation (origin triangulation forward)
  Point bx by = vertexPointAt triangulation (origin triangulation backward)
  Point cx cy = vertexPointAt triangulation (origin triangulation forwardPrevious)
  Point dx dy = vertexPointAt triangulation (origin triangulation backwardPrevious)

vertexPointAt
  :: Triangulation mode vertex directed undirected face
  -> VertexId
  -> Point
vertexPointAt triangulation (VertexId raw) =
  Point
    (triPointX triangulation `pagedUnsafeIndex` fromIntegral raw)
    (triPointY triangulation `pagedUnsafeIndex` fromIntegral raw)

data GeometricBaseOrder
  = GeometricBaseLeft
  | GeometricBaseRight
  deriving stock (Eq, Show)

-- | Finite local work required to turn the two synthetic perimeter bridges
-- into edge-bounded constrained paths.  The witness is derived while the
-- tangent endpoints still identify J exactly; after publication those edges
-- are ordinary constraints and must not be rediscovered by a global scan.
data SeamBridgeSubdivision = SeamBridgeSubdivision
  { seamLowerBridgeDepth :: {-# UNPACK #-} !Int
  , seamUpperBridgeDepth :: {-# UNPACK #-} !Int
  , seamBridgeAddedVertices :: {-# UNPACK #-} !Int
  }

planSeamBridgeSubdivision
  :: RefinementParameters
  -> Point
  -> Point
  -> Point
  -> Point
  -> Either BuildError SeamBridgeSubdivision
planSeamBridgeSubdivision parameters lowerLeft lowerRight upperLeft upperRight =
  case refineMaxEdgeLength parameters of
    Nothing -> Right (SeamBridgeSubdivision 0 0 0)
    Just maximumLength -> do
      let !lowerDepth = requiredBridgeSubdivisionDepth maximumLength lowerLeft lowerRight
          !upperDepth = requiredBridgeSubdivisionDepth maximumLength upperLeft upperRight
          !required =
            subdivisionVertexCount lowerDepth
              + subdivisionVertexCount upperDepth
      if required > toInteger (maxBound :: Int)
        then Left (CapacityExceeded maxBound)
        else do
          let !requiredVertices = fromInteger required
          available <-
            case (requiredVertices, refineMaxAdditionalVertices parameters) of
              (0, Nothing) -> Right 0
              (_, Nothing) -> Left RefinementDomainRequiresFiniteVertexBudget
              (_, Just budget) -> Right budget
          if requiredVertices <= available
            then
              Right
                SeamBridgeSubdivision
                  { seamLowerBridgeDepth = lowerDepth
                  , seamUpperBridgeDepth = upperDepth
                  , seamBridgeAddedVertices = requiredVertices
                  }
            else
              Left
                ( RefinementSeamBridgeBudgetExceeded
                    requiredVertices
                    available
                )
 where
  subdivisionVertexCount :: Int -> Integer
  subdivisionVertexCount depth = (2 :: Integer) ^ depth - 1

requiredBridgeSubdivisionDepth :: Double -> Point -> Point -> Int
requiredBridgeSubdivisionDepth maximumLength from to =
  descend 0 (distance from to)
 where
  descend !depth !currentLength
    | currentLength <= maximumLength = depth
    | depth >= finiteBitSize (0 :: Int) = depth
    | otherwise = descend (depth + 1) (0.5 * currentLength)

vertexPointForOuterEdge
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> Point
vertexPointForOuterEdge triangulation edge =
  vertexPointAt
    triangulation
    (VertexId (fromIntegral (topologyAt triangulation (4 * edge))))

subdivideSeamBridge
  :: forall s vertex directed undirected face
   . (Int -> Bool)
  -> vertex
  -> Int
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> ST s (Either BuildError (Seq.Seq Int))
subdivideSeamBridge targetFaceProtected vertexDefault = descend
 where
  descend
    :: Int
    -> MutableDcel s vertex directed undirected face
    -> OperationState s
    -> Int
    -> ST s (Either BuildError (Seq.Seq Int))
  descend depth mutable operation outerEdge
    | depth <= 0 = pure (Right (Seq.singleton outerEdge))
    | otherwise = do
        fromVertex <- readOrigin mutable outerEdge
        toVertex <- readOrigin mutable (outerEdge `xor` 1)
        fromPoint <- pointAt mutable fromVertex
        toPoint <- pointAt mutable toVertex
        case validatePoint Nothing (midpoint fromPoint toPoint) of
          Left obstruction -> pure (Left obstruction)
          Right admitted ->
            let !splitPoint = queryPointValue admitted
             in if splitPoint == fromPoint || splitPoint == toPoint
                  then
                    pure
                      ( Left
                          ( RefinementSeamBridgeMidpointCollapsed
                              (UndirectedEdgeId (fromIntegral (outerEdge `quot` 2)))
                          )
                      )
                  else do
                    vertex <- appendVertex mutable splitPoint vertexDefault
                    splitBoundaryEdgeWithLegalization
                      (\target targetOperation _ firstCandidate secondCandidate ->
                         legalizeEdgesPinned
                           targetFaceProtected
                           target
                           targetOperation
                           [firstCandidate, secondCandidate]
                      )
                      mutable
                      operation
                      outerEdge
                      vertex
                      `bindMutable` \(firstOuter, secondOuter) -> do
                        addCounter operation CounterSteinerPoints 1
                        descend (depth - 1) mutable operation firstOuter
                          `bindMutable` \firstChain ->
                            fmap (fmap (firstChain Seq.><))
                              (descend (depth - 1) mutable operation secondOuter)

executeResidentConstrained
  :: forall vertex
   . (Int -> Bool)
  -> RefinementParameters
  -> vertex
  -> GeometricBaseOrder
  -> Triangulation 'Constrained vertex () () ()
  -> Triangulation 'Constrained vertex () () ()
  -> SeamFrontierIndex
  -> SeamFrontierIndex
  -> SeamTangents
  -> Either BuildError
      ( Triangulation 'Constrained vertex () () ()
      , BuildStats
      , PublicationStats
      , V.Vector FaceId
      )
executeResidentConstrained targetFaceProtected parameters vertexDefault order base extension leftFrontier rightFrontier tangents = do
  let !baseDirected = numDirectedEdges base
      !extensionDirected = numDirectedEdges extension
      !baseVertices = numVertices base
      (leftSource, rightSource, leftVertexOffset, rightVertexOffset, leftEdgeOffset, rightEdgeOffset) =
        case order of
          GeometricBaseLeft ->
            (base, extension, 0, baseVertices, 0, baseDirected)
          GeometricBaseRight ->
            (extension, base, baseVertices, 0, baseDirected, 0)
      SeamTangents
        { seamLowerLeft = SeamTangent _ lowerLeft
        , seamLowerRight = SeamTangent _ lowerRight
        , seamUpperLeft = SeamTangent _ upperLeft
        , seamUpperRight = SeamTangent _ upperRight
        } = tangents
  bridgePlan <-
    planSeamBridgeSubdivision
      parameters
      (vertexPointForOuterEdge leftSource lowerLeft)
      (vertexPointForOuterEdge rightSource lowerRight)
      (vertexPointForOuterEdge leftSource upperLeft)
      (vertexPointForOuterEdge rightSource upperRight)
  let !extensionVertices = numVertices extension
      !bridgeVertices = seamBridgeAddedVertices bridgePlan
  transactionAdditional <-
    if bridgeVertices > maxBound - extensionVertices
      then Left (CapacityExceeded maxBound)
      else Right (extensionVertices + bridgeVertices)
  ((newFaces, progress, upperBridgeChain, lowerBridgeChain), triangulation, statistics, publicationStats) <-
    runTransactionWithPublication
      id
      LocalTransaction
      base
      transactionAdditional
      (\mutable operation -> do
         appendSourceVertices mutable extension
         _ <- addEdgeBlock mutable (extensionDirected `quot` 2)
         _ <- addFaceBlock mutable (numFaces extension - 1)
         copySource
           mutable
           extension
           baseVertices
           baseDirected
           (numFaces base - 1)
         copySourceConstraints mutable baseDirected extension
         seamBase <-
           spliceLowerTangent
             mutable
             rightEdgeOffset
             (leftEdgeOffset + lowerLeft)
             lowerRight
         -- The two synthetic hull bridges are the certified perimeter of the
         -- published world.  Once a later extension consumes either bridge it
         -- becomes Γ, so fixing it now preserves the prior solve without a
         -- retrospective source rewrite.
         _ <- setConstraint mutable seamBase
         stitchSeamPinned
           targetFaceProtected
           mutable
           operation
           (Seq.length (seamFrontierEdges leftFrontier))
           (tangentIndex (seamLowerLeft tangents))
           (Seq.length (seamFrontierEdges rightFrontier))
           (tangentIndex (seamLowerRight tangents))
           seamBase
           (leftVertexOffset + topologyAt leftSource (4 * upperLeft))
           (rightVertexOffset + topologyAt rightSource (4 * upperRight))
           []
           []
           `bindMutable` \(upperBridge, newFaces, progress) -> do
             _ <- setConstraint mutable upperBridge
             subdivideSeamBridge
               targetFaceProtected
               vertexDefault
               (seamLowerBridgeDepth bridgePlan)
               mutable
               operation
               (seamBase `xor` 1)
               `bindMutable` \lowerBridgeChain ->
                 subdivideSeamBridge
                   targetFaceProtected
                   vertexDefault
                   (seamUpperBridgeDepth bridgePlan)
                   mutable
                   operation
                   upperBridge
                   `bindMutable` \upperBridgeChain ->
                     case Seq.lookup 0 upperBridgeChain of
                       Nothing -> pure (Left SeamFrontierUnavailable)
                       Just frontierStart -> do
                         writeFaceEdge mutable 0 frontierStart
                         pure
                           ( Right
                               (newFaces, progress, upperBridgeChain, lowerBridgeChain)
                           )
      )
  let newFaceSeeds = V.fromList (fmap (FaceId . fromIntegral) newFaces)
      (joinFaces, rejectedBoundaryPairs) =
        exactJoinFaces triangulation targetFaceProtected newFaceSeeds
  certifyJoinBoundary triangulation rejectedBoundaryPairs
  frontier <-
    combineSeamFrontier
      triangulation
      leftFrontier
      rightFrontier
      leftEdgeOffset
      rightEdgeOffset
      upperBridgeChain
      lowerBridgeChain
      progress
  let published :: Triangulation 'Constrained vertex () () ()
      published = triangulation{triSeamFrontier = Just frontier}
  pure
    ( published
    , statistics
    , publicationStats
    , joinFaces
    )

seamFaceEvidenceForSource
  :: Triangulation 'Constrained vertex () () ()
  -> Int
  -> [FaceId]
  -> Either BuildError (V.Vector ConstrainedSeamFaceEvidence)
seamFaceEvidenceForSource source targetOffset sourceFaces =
  V.fromList <$> traverse evidenceFor sourceFaces
 where
  evidenceFor sourceFace = do
    (first, second, third) <- facePoints source sourceFace
    let targetFace = FaceId (unFaceId sourceFace + fromIntegral targetOffset)
    Right
      ConstrainedSeamFaceEvidence
        { constrainedSeamSourceFace = sourceFace
        , constrainedSeamTargetFace = targetFace
        , constrainedSeamFaceFirstPoint = first
        , constrainedSeamFaceSecondPoint = second
        , constrainedSeamFaceThirdPoint = third
        }

  facePoints
    :: Triangulation mode sourceVertex directed undirected face
    -> FaceId
    -> Either BuildError (Point, Point, Point)
  facePoints triangulation faceHandle =
    case List.sort (fmap (vertexPointAt triangulation) (faceVertices triangulation faceHandle)) of
      [first, second, third] -> Right (first, second, third)
      _ -> Left (faceCardinalityFailure triangulation faceHandle)

  faceCardinalityFailure
    :: Triangulation mode sourceVertex directed undirected face
    -> FaceId
    -> BuildError
  faceCardinalityFailure triangulation faceHandle =
    case faceDirectedEdges triangulation faceHandle of
      edge : _ -> RefinementInputTopologyInvalid (InnerFaceNotTriangularAtEdge edge)
      [] -> RefinementInputTopologyInvalid (FaceMissingAdjacentEdge faceHandle)

data SeparatedOrder
  = LeftBeforeRight
  | RightBeforeLeft

separatedOrder
  :: SeamAxis
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode' vertex' directed' undirected' face'
  -> SeamFrontierIndex
  -> SeamFrontierIndex
  -> Maybe SeparatedOrder
separatedOrder axis left right leftFrontier rightFrontier
  | numInnerFaces left <= 0 || numInnerFaces right <= 0 = Nothing
  | chartMaximum axis leftFrontier < chartMinimum axis rightFrontier =
      Just LeftBeforeRight
  | chartMaximum axis rightFrontier < chartMinimum axis leftFrontier =
      Just RightBeforeLeft
  | otherwise = Nothing

chartMinimum :: SeamAxis -> SeamFrontierIndex -> Double
chartMinimum axis frontier =
  case axis of
    SeamAxisX -> seamFrontierXMinimum frontier
    SeamAxisY -> seamFrontierYMinimum frontier

chartMaximum :: SeamAxis -> SeamFrontierIndex -> Double
chartMaximum axis frontier =
  case axis of
    SeamAxisX -> seamFrontierXMaximum frontier
    SeamAxisY -> seamFrontierYMaximum frontier

-- The opposite-sign branch cannot overflow in its sum. The same-sign branch
-- cannot overflow in its difference.
data SeamTangent = SeamTangent
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int

tangentIndex :: SeamTangent -> Int
tangentIndex (SeamTangent index _) = index
{-# INLINE tangentIndex #-}

data SeamTangents = SeamTangents
  { seamLowerLeft :: !SeamTangent
  , seamLowerRight :: !SeamTangent
  , seamUpperLeft :: !SeamTangent
  , seamUpperRight :: !SeamTangent
  , seamFrontierPointReads :: {-# UNPACK #-} !Int
  }

seamTangents
  :: SeamAxis
  -> Triangulation mode vertex directed undirected face
  -> SeamFrontierIndex
  -> Triangulation mode' vertex' directed' undirected' face'
  -> SeamFrontierIndex
  -> Maybe SeamTangents
seamTangents axis left leftFrontier right rightFrontier = do
  (lowerLeft, lowerRight, lowerReads) <- lowerTangent axis left leftFrontier right rightFrontier
  (upperLeft, upperRight, upperReads) <- upperTangent axis left leftFrontier right rightFrontier
  pure
    SeamTangents
      { seamLowerLeft = lowerLeft
      , seamLowerRight = lowerRight
      , seamUpperLeft = upperLeft
      , seamUpperRight = upperRight
      , seamFrontierPointReads = lowerReads + upperReads
      }

lowerTangent
  :: SeamAxis
  -> Triangulation mode vertex directed undirected face
  -> SeamFrontierIndex
  -> Triangulation mode' vertex' directed' undirected' face'
  -> SeamFrontierIndex
  -> Maybe (SeamTangent, SeamTangent, Int)
-- The walk carries both endpoints' coordinates: a step replaces exactly one
-- endpoint, and the replacement is the neighbour whose coordinates the step's
-- own test already read.
lowerTangent axis left leftFrontier right rightFrontier = do
  (leftStart, leftReads) <- frontierPointAtCount axis left leftFrontier leftStartIndex
  (rightStart, rightReads) <- frontierPointAtCount axis right rightFrontier rightStartIndex
  walk leftStartIndex leftStart rightStartIndex rightStart (leftReads + rightReads)
 where
  leftStartIndex :: Int
  rightStartIndex :: Int
  leftSize :: Int
  rightSize :: Int
  walk
    :: Int
    -> (Double, Double)
    -> Int
    -> (Double, Double)
    -> Int
    -> Maybe (SeamTangent, SeamTangent, Int)
  leftStartIndex = chartLowerRightmost axis leftFrontier
  rightStartIndex = chartLowerLeftmost axis rightFrontier
  leftSize = Seq.length (seamFrontierEdges leftFrontier)
  rightSize = Seq.length (seamFrontierEdges rightFrontier)

  walk !leftIndex (!leftX, !leftY) !rightIndex (!rightX, !rightY) !frontierReads = do
    nextLeftIndex <- pure (nextIndex leftSize leftIndex)
    previousRightIndex <- pure (previousIndex rightSize rightIndex)
    (nextLeftX, nextLeftY) <- frontierPointAt axis left leftFrontier nextLeftIndex
    (previousRightX, previousRightY) <- frontierPointAt axis right rightFrontier previousRightIndex
    let !nextReads = frontierReads + 2
        !leftBelow =
          orient2dCoordinates leftX leftY rightX rightY nextLeftX nextLeftY == LT
        !rightBelow =
          orient2dCoordinates leftX leftY rightX rightY previousRightX previousRightY == LT
    if leftBelow
      then walk nextLeftIndex (nextLeftX, nextLeftY) rightIndex (rightX, rightY) nextReads
      else
        if rightBelow
          then walk leftIndex (leftX, leftY) previousRightIndex (previousRightX, previousRightY) nextReads
          else do
            leftEdge <- frontierEdgeAt leftFrontier leftIndex
            rightEdge <- frontierEdgeAt rightFrontier rightIndex
            pure
              ( SeamTangent leftIndex leftEdge
              , SeamTangent rightIndex rightEdge
              , nextReads
              )

upperTangent
  :: SeamAxis
  -> Triangulation mode vertex directed undirected face
  -> SeamFrontierIndex
  -> Triangulation mode' vertex' directed' undirected' face'
  -> SeamFrontierIndex
  -> Maybe (SeamTangent, SeamTangent, Int)
upperTangent axis left leftFrontier right rightFrontier = do
  (leftStart, leftReads) <- frontierPointAtCount axis left leftFrontier leftStartIndex
  (rightStart, rightReads) <- frontierPointAtCount axis right rightFrontier rightStartIndex
  walk leftStartIndex leftStart rightStartIndex rightStart (leftReads + rightReads)
 where
  leftStartIndex :: Int
  rightStartIndex :: Int
  leftSize :: Int
  rightSize :: Int
  walk
    :: Int
    -> (Double, Double)
    -> Int
    -> (Double, Double)
    -> Int
    -> Maybe (SeamTangent, SeamTangent, Int)
  leftStartIndex = chartUpperRightmost axis leftFrontier
  rightStartIndex = chartUpperLeftmost axis rightFrontier
  leftSize = Seq.length (seamFrontierEdges leftFrontier)
  rightSize = Seq.length (seamFrontierEdges rightFrontier)

  walk !leftIndex (!leftX, !leftY) !rightIndex (!rightX, !rightY) !frontierReads = do
    previousLeftIndex <- pure (previousIndex leftSize leftIndex)
    nextRightIndex <- pure (nextIndex rightSize rightIndex)
    (previousLeftX, previousLeftY) <- frontierPointAt axis left leftFrontier previousLeftIndex
    (nextRightX, nextRightY) <- frontierPointAt axis right rightFrontier nextRightIndex
    let !nextReads = frontierReads + 2
        !leftAbove =
          orient2dCoordinates leftX leftY rightX rightY previousLeftX previousLeftY == GT
        !rightAbove =
          orient2dCoordinates leftX leftY rightX rightY nextRightX nextRightY == GT
    if leftAbove
      then walk previousLeftIndex (previousLeftX, previousLeftY) rightIndex (rightX, rightY) nextReads
      else
        if rightAbove
          then walk leftIndex (leftX, leftY) nextRightIndex (nextRightX, nextRightY) nextReads
          else do
            leftEdge <- frontierEdgeAt leftFrontier leftIndex
            rightEdge <- frontierEdgeAt rightFrontier rightIndex
            pure
              ( SeamTangent leftIndex leftEdge
              , SeamTangent rightIndex rightEdge
              , nextReads
              )

frontierEdgeAt :: SeamFrontierIndex -> Int -> Maybe Int
frontierEdgeAt frontier index = Seq.lookup index (seamFrontierEdges frontier)

chartLowerRightmost :: SeamAxis -> SeamFrontierIndex -> Int
chartLowerRightmost axis frontier =
  case axis of
    SeamAxisX -> seamFrontierLowerRightmost frontier
    SeamAxisY -> seamFrontierYLowerRightmost frontier

chartLowerLeftmost :: SeamAxis -> SeamFrontierIndex -> Int
chartLowerLeftmost axis frontier =
  case axis of
    SeamAxisX -> seamFrontierLowerLeftmost frontier
    SeamAxisY -> seamFrontierYLowerLeftmost frontier

chartUpperRightmost :: SeamAxis -> SeamFrontierIndex -> Int
chartUpperRightmost axis frontier =
  case axis of
    SeamAxisX -> seamFrontierUpperRightmost frontier
    SeamAxisY -> seamFrontierYUpperRightmost frontier

chartUpperLeftmost :: SeamAxis -> SeamFrontierIndex -> Int
chartUpperLeftmost axis frontier =
  case axis of
    SeamAxisX -> seamFrontierUpperLeftmost frontier
    SeamAxisY -> seamFrontierYUpperLeftmost frontier

frontierPointAt
  :: SeamAxis
  -> Triangulation mode vertex directed undirected face
  -> SeamFrontierIndex
  -> Int
  -> Maybe (Double, Double)
frontierPointAt axis triangulation frontier index = do
  edge <- frontierEdgeAt frontier index
  let !vertex = topologyAt triangulation (4 * edge)
  pure (chartPoint axis (physicalPoint triangulation vertex))
{-# INLINE frontierPointAt #-}

chartPoint :: SeamAxis -> (Double, Double) -> (Double, Double)
chartPoint axis (x, y) =
  case axis of
    SeamAxisX -> (x, y)
    SeamAxisY -> seamQuarterTurn (x, y)

physicalPoint
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> (Double, Double)
physicalPoint triangulation vertex =
  ( triPointX triangulation `pagedUnsafeIndex` vertex
  , triPointY triangulation `pagedUnsafeIndex` vertex
  )

frontierPointAtCount
  :: SeamAxis
  -> Triangulation mode vertex directed undirected face
  -> SeamFrontierIndex
  -> Int
  -> Maybe ((Double, Double), Int)
frontierPointAtCount axis triangulation frontier index = do
  point <- frontierPointAt axis triangulation frontier index
  pure (point, 1)

nextIndex :: Int -> Int -> Int
nextIndex size index
  | index + 1 == size = 0
  | otherwise = index + 1
{-# INLINE nextIndex #-}

previousIndex :: Int -> Int -> Int
previousIndex size index
  | index == 0 = size - 1
  | otherwise = index - 1
{-# INLINE previousIndex #-}


mergeSeparated
  :: forall outputMode leftMode rightMode vertex
   . Triangulation leftMode vertex () () ()
  -> Triangulation rightMode vertex () () ()
  -> SeamTangents
   -> Either
      BuildError
      (Triangulation outputMode vertex () () (), BuildStats)
mergeSeparated
  left
  right
  SeamTangents
    { seamLowerLeft = SeamTangent _ lowerLeft
    , seamLowerRight = SeamTangent _ lowerRight
    , seamUpperLeft = SeamTangent _ upperLeft
    , seamUpperRight = SeamTangent _ upperRight
    } = runST $ do
  mutable <- newMutableDcel unitElementDefaults (planarDcelCapacity totalVertices)
  pointCapacityOutcome <- ensurePointCapacity mutable totalVertices
  cellCapacityOutcome <-
    ensureCellCapacity
      mutable
      ((leftDirected + rightDirected) `quot` 2 + 1)
      (leftFaces + rightFaces - 2)
  case (pointCapacityOutcome, cellCapacityOutcome) of
    (Left obstruction, _) -> pure (Left obstruction)
    (_, Left obstruction) -> pure (Left obstruction)
    (Right (), Right ()) -> do
      appendSourceVertices mutable left
      appendSourceVertices mutable right
      _ <- addEdgeBlock mutable ((leftDirected + rightDirected) `quot` 2)
      _ <- addFaceBlock mutable (leftFaces + rightFaces - 2)
      copySource mutable left 0 0 0
      copySource mutable right leftVertices leftDirected (leftFaces - 1)
      base <- spliceLowerTangent mutable leftDirected lowerLeft lowerRight
      operation <- newOperationState (halfEdgeCapacity mutable)
      stitched <-
        stitchSeam
          mutable
          operation
          base
          (topologyAt left (4 * upperLeft))
          (leftVertices + topologyAt right (4 * upperRight))
          []
          []
      case stitched of
        Left obstruction -> pure (Left obstruction)
        Right _ -> do
          statistics <- freezeBuildStats operation
          fmap (\triangulation -> (triangulation, statistics))
            <$> freezeTriangulation mutable
 where
  !leftVertices = numVertices left
  !rightVertices = numVertices right
  !totalVertices = leftVertices + rightVertices
  !leftDirected = numDirectedEdges left
  !rightDirected = numDirectedEdges right
  !leftFaces = numFaces left
  !rightFaces = numFaces right

stitchSeam
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> Int
  -> [Int]
  -> [Int]
  -> ST s (Either BuildError (Int, [Int]))
stitchSeam mutable operation base upperLeft upperRight seeds newFaces =
  fmap (fmap (\(finalBase, faces, _) -> (finalBase, faces)))
    ( stitchSeamWith
        (\target ops edges -> legalizeEdges target ops edges >> pure (Right ()))
        (SeamProgress 0 0 0 0 0 0)
        mutable
        operation
        base
        upperLeft
        upperRight
        seeds
        newFaces
    )

stitchSeamPinned
  :: (Int -> Bool)
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> [Int]
  -> [Int]
  -> ST s (Either BuildError (Int, [Int], SeamProgress))
stitchSeamPinned targetFaceProtected mutable operation leftSize leftStart rightSize rightStart base upperLeft upperRight seeds newFaces =
  stitchSeamWith
    (\target ops edges -> legalizeEdgesPinned targetFaceProtected target ops edges >> pure (Right ()))
    (SeamProgress leftSize leftStart (previousIndex leftSize leftStart) rightSize rightStart rightStart)
    mutable
    operation
    base
    upperLeft
    upperRight
    seeds
    newFaces

data SeamProgress = SeamProgress
  { seamProgressLeftSize :: {-# UNPACK #-} !Int
  , seamProgressLeftStart :: {-# UNPACK #-} !Int
  , seamProgressLeftCursor :: {-# UNPACK #-} !Int
  , seamProgressRightSize :: {-# UNPACK #-} !Int
  , seamProgressRightStart :: {-# UNPACK #-} !Int
  , seamProgressRightCursor :: {-# UNPACK #-} !Int
  }

stitchSeamWith
  :: (MutableDcel s vertex directed undirected face -> OperationState s -> [Int] -> ST s (Either BuildError ()))
  -> SeamProgress
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> Int
  -> Int
  -> Int
  -> [Int]
  -> [Int]
  -> ST s (Either BuildError (Int, [Int], SeamProgress))
stitchSeamWith legalize progress mutable operation base upperLeft upperRight seeds newFaces = do
  leftVertex <- readOrigin mutable base
  rightVertex <- readOrigin mutable (base `xor` 1)
  if leftVertex == upperLeft && rightVertex == upperRight
    then do
      legalized <- legalize mutable operation seeds
      pure (fmap (const (base, newFaces, progress)) legalized)
    else do
      leftEdge <- readPrevious mutable base
      rightEdge <- readNext mutable base
      nextLeft <- readOrigin mutable leftEdge
      nextRight <- readOrigin mutable (rightEdge `xor` 1)
      leftTurn <- vertexOrientation mutable nextLeft leftVertex rightVertex
      rightTurn <- vertexOrientation mutable leftVertex rightVertex nextRight
      chooseRight <-
        if rightVertex == upperRight
          then pure False
          else
            if leftVertex == upperLeft
              then pure True
              else
                case (leftTurn == GT, rightTurn == GT) of
                  (True, True) ->
                    (== GT) <$> vertexInCircle mutable nextLeft leftVertex rightVertex nextRight
                  (False, True) -> pure True
                  _ -> pure False
      if chooseRight
        then do
          closed <- closeOuterTurn mutable base
          case closed of
            Left obstruction -> pure (Left obstruction)
            Right nextBase ->
              do
                face <- readFace mutable (nextBase `xor` 1)
                stitchSeamWith
                  legalize
                  (advanceRight progress)
                  mutable
                  operation
                  nextBase
                  upperLeft
                  upperRight
                  (base : rightEdge : seeds)
                  (face : newFaces)
        else do
          closed <- closeOuterTurn mutable leftEdge
          case closed of
            Left obstruction -> pure (Left obstruction)
            Right nextBase ->
              do
                face <- readFace mutable (nextBase `xor` 1)
                stitchSeamWith
                  legalize
                  (advanceLeft progress)
                  mutable
                  operation
                  nextBase
                  upperLeft
                  upperRight
                  (leftEdge : base : seeds)
                  (face : newFaces)

advanceLeft :: SeamProgress -> SeamProgress
advanceLeft progress@SeamProgress{seamProgressLeftSize, seamProgressLeftCursor} =
  progress
    { seamProgressLeftCursor =
        previousIndex seamProgressLeftSize seamProgressLeftCursor
    }

advanceRight :: SeamProgress -> SeamProgress
advanceRight progress@SeamProgress{seamProgressRightSize, seamProgressRightCursor} =
  progress
    { seamProgressRightCursor =
        nextIndex seamProgressRightSize seamProgressRightCursor
    }

combineSeamFrontier
  :: Triangulation mode vertex directed undirected face
  -> SeamFrontierIndex
  -> SeamFrontierIndex
  -> Int
  -> Int
  -> Seq.Seq Int
  -> Seq.Seq Int
  -> SeamProgress
  -> Either BuildError SeamFrontierIndex
combineSeamFrontier triangulation leftFrontier rightFrontier leftEdgeOffset rightEdgeOffset upperBridge lowerBridge progress
  | seamProgressLeftSize progress /= Seq.length (seamFrontierEdges leftFrontier) = Left SeamFrontierUnavailable
  | seamProgressRightSize progress /= Seq.length (seamFrontierEdges rightFrontier) = Left SeamFrontierUnavailable
  | otherwise = do
  leftEdges <- translatedEdges leftFrontier leftEdgeOffset
  rightEdges <- translatedEdges rightFrontier rightEdgeOffset
  rightResidual <-
    cycleSlice
      rightEdges
      (seamProgressRightSize progress)
      (seamProgressRightCursor progress)
      (previousIndex
         (seamProgressRightSize progress)
         (seamProgressRightStart progress))
  leftResidual <-
    cycleSlice
      leftEdges
      (seamProgressLeftSize progress)
      (seamProgressLeftStart progress)
      (seamProgressLeftCursor progress)
  let !outputEdges =
        upperBridge
          Seq.>< rightResidual
          Seq.>< lowerBridge
          Seq.>< leftResidual
      !rightOffset = Seq.length upperBridge
      !lowerOffset = rightOffset + Seq.length rightResidual
      !leftOffset = lowerOffset + Seq.length lowerBridge
      !rightEnd = previousIndex (seamProgressRightSize progress) progressRightStart
      !leftEnd = seamProgressLeftCursor progress
      !candidatePositions =
        IntSet.toAscList
          ( IntSet.fromList
              ( residualBoundaryPositions 0 upperBridge
                  ++ residualBoundaryPositions rightOffset rightResidual
                  ++ residualBoundaryPositions lowerOffset lowerBridge
                  ++ residualBoundaryPositions leftOffset leftResidual
                  ++ sourceAnchorPositions
                    rightOffset
                    (seamProgressRightCursor progress)
                    rightEnd
                    (seamProgressRightSize progress)
                    rightFrontier
                  ++ sourceAnchorPositions
                    leftOffset
                    progressLeftStart
                    leftEnd
                    (seamProgressLeftSize progress)
                    leftFrontier
              )
          )
  (minimumX, maximumX, lowerRight, lowerLeft, upperRight, upperLeft) <-
    summarizeOutputChart id candidatePositions outputEdges
  (minimumY, maximumY, yLowerRight, yLowerLeft, yUpperRight, yUpperLeft) <-
    summarizeOutputChart seamQuarterTurn candidatePositions outputEdges
  pure
    SeamFrontierIndex
      { seamFrontierEdges = outputEdges
      , seamFrontierXMinimum = minimumX
      , seamFrontierXMaximum = maximumX
      , seamFrontierLowerRightmost = lowerRight
      , seamFrontierLowerLeftmost = lowerLeft
      , seamFrontierUpperRightmost = upperRight
      , seamFrontierUpperLeftmost = upperLeft
      , seamFrontierYMinimum = minimumY
      , seamFrontierYMaximum = maximumY
      , seamFrontierYLowerRightmost = yLowerRight
      , seamFrontierYLowerLeftmost = yLowerLeft
      , seamFrontierYUpperRightmost = yUpperRight
      , seamFrontierYUpperLeftmost = yUpperLeft
      }
 where
  progressRightStart = seamProgressRightStart progress
  progressLeftStart = seamProgressLeftStart progress

  translatedEdges
    :: SeamFrontierIndex
    -> Int
    -> Either BuildError (Seq.Seq Int)
  translatedEdges frontier offset
    | offset == 0 = Right (seamFrontierEdges frontier)
    | otherwise = Right (fmap (+ offset) (seamFrontierEdges frontier))

  cycleSlice
    :: Seq.Seq Int
    -> Int
    -> Int
    -> Int
    -> Either BuildError (Seq.Seq Int)
  cycleSlice edges size start end
    | size <= 0 || start < 0 || end < 0 || start >= size || end >= size = Left SeamFrontierUnavailable
    | start <= end = Right (Seq.take (end - start + 1) (Seq.drop start edges))
    | otherwise =
        Right
          ( Seq.drop start edges
              Seq.>< Seq.take (end + 1) edges
          )

  residualBoundaryPositions :: Int -> Seq.Seq Int -> [Int]
  residualBoundaryPositions offset residual =
    case Seq.length residual of
      0 -> []
      lengthOfResidual -> [offset, offset + lengthOfResidual - 1]

  sourceAnchorPositions
    :: Int
    -> Int
    -> Int
    -> Int
    -> SeamFrontierIndex
    -> [Int]
  sourceAnchorPositions offset start end size frontier =
    [ offset + position
    | anchor <- sourceAnchors frontier
    , Right position <- [intervalPosition size anchor start end]
    ]

  sourceAnchors :: SeamFrontierIndex -> [Int]
  sourceAnchors frontier =
    [ seamFrontierLowerRightmost frontier
    , seamFrontierLowerLeftmost frontier
    , seamFrontierUpperRightmost frontier
    , seamFrontierUpperLeftmost frontier
    , seamFrontierYLowerRightmost frontier
    , seamFrontierYLowerLeftmost frontier
    , seamFrontierYUpperRightmost frontier
    , seamFrontierYUpperLeftmost frontier
    ]

  summarizeOutputChart
    :: ((Double, Double) -> (Double, Double))
    -> [Int]
    -> Seq.Seq Int
    -> Either BuildError (Double, Double, Int, Int, Int, Int)
  summarizeOutputChart transform candidatePositions outputEdges = do
    candidates <-
      traverse
        (\index -> do
           edge <- maybe (Left SeamFrontierUnavailable) Right (Seq.lookup index outputEdges)
           let !vertex = topologyAt triangulation (4 * edge)
           pure (index, physicalPoint triangulation vertex))
        candidatePositions
    maybe (Left SeamFrontierUnavailable) Right
      (summarizeSeamChart transform candidates)

  intervalPosition :: Int -> Int -> Int -> Int -> Either BuildError Int
  intervalPosition size target start end
    | target < 0 || target >= size = Left SeamFrontierUnavailable
    | start <= end =
        if target >= start && target <= end
          then Right (target - start)
          else Left SeamFrontierUnavailable
    | target >= start = Right (target - start)
    | target <= end = Right (size - start + target)
    | otherwise = Left SeamFrontierUnavailable

vertexOrientation
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> Int
  -> ST s Ordering
vertexOrientation mutable a b c = do
  ax <- readPointX mutable a
  ay <- readPointY mutable a
  bx <- readPointX mutable b
  by <- readPointY mutable b
  cx <- readPointX mutable c
  cy <- readPointY mutable c
  pure $! orient2dCoordinates ax ay bx by cx cy
{-# INLINE vertexOrientation #-}

vertexInCircle
  :: MutableDcel s vertex directed undirected face
  -> Int
  -> Int
  -> Int
  -> Int
  -> ST s Ordering
vertexInCircle mutable a b c d = do
  ax <- readPointX mutable a
  ay <- readPointY mutable a
  bx <- readPointX mutable b
  by <- readPointY mutable b
  cx <- readPointX mutable c
  cy <- readPointY mutable c
  dx <- readPointX mutable d
  dy <- readPointY mutable d
  pure $! inCircleCoordinates ax ay bx by cx cy dx dy
{-# INLINE vertexInCircle #-}

appendSourceVertices
  :: MutableDcel s vertex () () ()
  -> Triangulation mode vertex () () ()
  -> ST s ()
appendSourceVertices mutable source =
  forRange 0 (numVertices source) $ \vertex -> do
    _ <-
      appendVertexCoordinates
        mutable
        (triPointX source `pagedUnsafeIndex` vertex)
        (triPointY source `pagedUnsafeIndex` vertex)
        (vertexData source (VertexId (fromIntegral vertex)))
    pure ()

copySource
  :: MutableDcel s vertex () () ()
  -> Triangulation mode vertex () () ()
  -> Int
  -> Int
  -> Int
  -> ST s ()
copySource mutable source vertexOffset edgeOffset faceOffset = do
  forRange 0 (numDirectedEdges source) $ \edge -> do
    let !target = edgeOffset + edge
        !sourceBase = 4 * edge
        !sourceFace = topologyAt source (sourceBase + 3)
        !targetFace = if sourceFace == 0 then 0 else faceOffset + sourceFace
    writeOrigin mutable target (vertexOffset + topologyAt source sourceBase)
    writeNext mutable target (edgeOffset + topologyAt source (sourceBase + 1))
    writePrevious mutable target (edgeOffset + topologyAt source (sourceBase + 2))
    writeFace mutable target targetFace
  forRange 0 (numVertices source) $ \vertex ->
    case vertexOutEdge source (VertexId (fromIntegral vertex)) of
      Nothing -> markConnected mutable (vertexOffset + vertex) (-1)
      Just (DirectedEdgeId edge) ->
        markConnected mutable (vertexOffset + vertex) (edgeOffset + fromIntegral edge)
  forRange 1 (numFaces source) $ \face ->
    case adjacentEdge source (FaceId (fromIntegral face)) of
      Nothing -> writeFaceEdge mutable (faceOffset + face) (-1)
      Just (DirectedEdgeId edge) ->
        writeFaceEdge mutable (faceOffset + face) (edgeOffset + fromIntegral edge)

copySourceConstraints
  :: MutableDcel s vertex () () ()
  -> Int
  -> Triangulation 'Constrained vertex () () ()
  -> ST s ()
copySourceConstraints mutable directedEdgeOffset source =
  traverse_
    (\(UndirectedEdgeId edge) ->
       ()
         <$ setConstraint
           mutable
           (directedEdgeOffset + 2 * fromIntegral edge)
    )
    (constraintEdges source)

spliceLowerTangent
  :: MutableDcel s vertex () () ()
  -> Int
  -> Int
  -> Int
  -> ST s Int
spliceLowerTangent mutable rightEdgeOffset leftOuter rightOuterSource = do
  let !rightOuter = rightEdgeOffset + rightOuterSource
  leftVertex <- readOrigin mutable leftOuter
  rightVertex <- readOrigin mutable rightOuter
  leftPrevious <- readPrevious mutable leftOuter
  rightPrevious <- readPrevious mutable rightOuter
  (forward, backward) <- addEdge mutable leftVertex rightVertex
  writeFace mutable forward 0
  writeFace mutable backward 0
  linkEdges mutable leftPrevious forward
  linkEdges mutable forward rightOuter
  linkEdges mutable rightPrevious backward
  linkEdges mutable backward leftOuter
  writeFaceEdge mutable 0 forward
  writeVertexOut mutable leftVertex forward
  writeVertexOut mutable rightVertex backward
  pure forward
{-# INLINE spliceLowerTangent #-}

topologyAt
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> Int
topologyAt triangulation slot =
  fromIntegral (triHalfTopology triangulation `pagedUnsafeIndex` slot)
{-# INLINE topologyAt #-}


forRange :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
forRange from to action = go from
 where
  go !index
    | index >= to = pure ()
    | otherwise = action index >> go (index + 1)
{-# INLINE forRange #-}

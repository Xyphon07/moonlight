{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ruppert refinement: Steiner insertion composed after a built mesh, never a
-- second kind of mesh. Parameters are reached through checked verbs rather than
-- a raw record, so an unrealizable quality bar is a refusal.
module Moonlight.Triangulation.Refinement
  ( refine
  , refineWithinDomain
  , validateRefinementParameters
  , withMinimumAngle
  , radiusEdgeRatioForAngle
  ) where

import Control.Monad (void)
import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Triangulation.Dcel
  ( faceDirectedEdges
  , faceVertices
  , incidentFace
  , isConstraintEdge
  , numFaces
  , numUndirectedEdges
  , numVertices
  , undirectedEndpoints
  , vertexPoint
  )
import Moonlight.Triangulation.FloodFillIterator (facesAtEvenBarrierDepth)
import Moonlight.Triangulation.Internal.HandleDefs
  ( FaceId (..)
  , UndirectedEdgeId (..)
  , asUndirected
  , directedPair
  , faceIdIndex
  , reverseEdge
  , undirectedEdgeIdIndex
  )
import Moonlight.Triangulation.Internal.Mutable
import Moonlight.Triangulation.Internal.Paged
  ( PublicationStats
  , TransactionShape (DenseTransaction, LocalTransaction)
  )
import Moonlight.Triangulation.Internal.Refinement
import Moonlight.Triangulation.Internal.Representation (Triangulation (..))
import Moonlight.Triangulation.Internal.Transaction (runTransactionWithPublication)
import Moonlight.Triangulation.Internal.Validation
  ( validateTopologyClosureWithStats
  )
import Moonlight.Triangulation.Types
import Moonlight.Triangulation.Validation
  ( validateTopology
  )

data RefinementExecution mode vertex directed undirected face = RefinementExecution
  { refinementExecutionResult :: !(RefinementResult mode vertex directed undirected face)
  , refinementExecutionVisitedFaces :: ![Int]
  , refinementExecutionPermittedFaces :: ![Int]
  , refinementExecutionInterfaceBoundaryReads :: {-# UNPACK #-} !Int
  , refinementExecutionBoundaryCrossingAttempts :: {-# UNPACK #-} !Int
  , refinementExecutionPublicationStats :: !PublicationStats
  }

-- | The radius-edge ratio admitting a minimum angle, in degrees.
radiusEdgeRatioForAngle :: Double -> Either BuildError (Maybe Double)
radiusEdgeRatioForAngle degrees =
  case classifyNonFinite degrees of
    Just nonFinite ->
      Left (RefinementMinimumAngleNotFinite nonFinite)
    Nothing
      | degrees < 0 || degrees > 60 ->
          Left (RefinementMinimumAngleOutOfRange degrees)
      | degrees == 0 -> Right Nothing
      | otherwise ->
          let ratio = 0.5 / sin (degrees * pi / 180)
           in case classifyNonFinite ratio of
                Just nonFinite ->
                  Left (RefinementMinimumAngleDerivedRatioNotFinite nonFinite)
                Nothing -> Right (Just ratio)

-- | Set the minimum angle, in degrees.
withMinimumAngle :: Double -> RefinementParameters -> Either BuildError (RefinementParameters)
withMinimumAngle degrees parameters = do
  ratio <- radiusEdgeRatioForAngle degrees
  pure parameters{refineMaxRadiusEdgeRatio = ratio}

-- | Refine an unconstrained or constrained triangulation in the same finite
-- DCEL. The constructor supplies application payloads for Steiner vertices;
-- payloads annotate the requested geometric point and cannot reauthor it.
refine
  :: (Point -> vertex)
  -> RefinementParameters
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (RefinementResult mode vertex directed undirected face)
refine makeVertex parameters =
  fmap refinementExecutionResult
    . refineWithInitialSeed RefineEveryFace Nothing makeVertex parameters

-- | Derive the exact immutable interface separating a checked active face
-- section from every protected inner face.  A permitted face set determines
-- this cut uniquely, so callers cannot supply a second claim about Gamma.
mkRefinementDomain
  :: Set.Set FaceId
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError RefinementDomain
mkRefinementDomain permittedFaces triangulation = do
  permitted <- validateSeedFaces triangulation permittedFaces
  Right
    RefinementDomain
      { refinementDomainPermittedFaces = permitted
      , refinementDomainInterfacePairs = expectedInterface permitted
      , refinementDomainInputFaceCount = numFaces triangulation
      }
 where
  expectedInterface permitted =
    IntSet.fromList
      [ pair
      | rawFace <- IntSet.toAscList permitted
      , edge <- faceDirectedEdges triangulation (FaceId (fromIntegral rawFace))
      , let pair = undirectedEdgeIdIndex (asUndirected edge)
      , let adjacent = incidentFace triangulation (reverseEdge edge)
      , adjacent /= FaceId 0
      , IntSet.notMember (faceIdIndex adjacent) permitted
      ]

-- | Refine exactly one checked local section. Interface edges are installed as
-- transaction-local legalization barriers and removed before publication;
-- the receipt and collar proof inspect only the admitted local closure.
refineWithinDomain
  :: forall mode vertex directed undirected face.
     (Point -> vertex)
  -> RefinementParameters
  -> Set.Set FaceId
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (RefinementDomainResult mode vertex directed undirected face)
refineWithinDomain makeVertex parameters permittedFaces triangulation = do
  validateDomainParameters parameters
  domain <- mkRefinementDomain permittedFaces triangulation
  execution <-
    refineWithInitialSeed
      (RefineSeededFaces (refinementDomainPermittedFaces domain))
      (Just domain)
      makeVertex
      parameters
      triangulation
  let result = refinementExecutionResult execution
  (validationStats, finalInterfaceIncidence) <- validateDomainClosure
    domain
    triangulation
    (refinedTriangulation result)
    (refinementExecutionPermittedFaces execution)
  let receipt =
        buildRefinementReceipt
          domain
          triangulation
          (refinedTriangulation result)
          (refinementExecutionVisitedFaces execution)
          (refinementExecutionPermittedFaces execution)
          finalInterfaceIncidence
          (refinementExecutionInterfaceBoundaryReads execution)
          (refinementExecutionBoundaryCrossingAttempts execution)
          (refinementExecutionPublicationStats execution)
          validationStats
  case V.toList (refinementVisitedProtectedFaces receipt) of
    protected : _ -> Left (RefinementDomainWouldRewriteProtectedFace protected)
    [] ->
       let restoredResult :: RefinementResult mode vertex directed undirected face
           restoredResult =
            result
              { refinedTriangulation =
                  (refinedTriangulation result)
                    -- The checked local interpreter rejects every hull-pair
                    -- split, so the cached outer cycle remains an exact
                    -- handle witness and can be restored without a scan.
                    { triSeamFrontier = triSeamFrontier triangulation
                    }
              }
       in Right
        RefinementDomainResult
          { refinementDomainResult = restoredResult
          , refinementDomainReceipt = receipt
          }

validateDomainParameters :: RefinementParameters -> Either BuildError ()
validateDomainParameters parameters
  | not (refinePreserveConvexHull parameters) =
      Left RefinementDomainRequiresConvexHullPreservation
  | not (refineKeepConstraintEdges parameters) =
      Left RefinementDomainRequiresConstraintPreservation
  | refineExcludeOuterFaces parameters =
      Left RefinementDomainForbidsOuterFaceExclusion
  | refineMaxAdditionalVertices parameters == Nothing =
      Left RefinementDomainRequiresFiniteVertexBudget
  | otherwise = Right ()

refineWithInitialSeed
  :: RefinementInitialSeed
  -> Maybe RefinementDomain
  -> (Point -> vertex)
  -> RefinementParameters
  -> Triangulation mode vertex directed undirected face
  -> Either BuildError (RefinementExecution mode vertex directed undirected face)
refineWithInitialSeed initialSeed domain makeVertex parameters triangulation = do
  validateRefinementParameters parameters
  case domain of
    Just _ -> pure ()
    Nothing ->
      case validateTopology triangulation of
        violation : _ -> Left (RefinementInputTopologyInvalid violation)
        [] -> pure ()
  let originalCount = numVertices triangulation
      budget =
        case domain of
          Just _ -> max 0 (fromMaybe 0 (refineMaxAdditionalVertices parameters))
          Nothing -> max 0 (fromMaybe (10 * max 1 originalCount) (refineMaxAdditionalVertices parameters))
      initialExcludedFaces =
        if domain == Nothing && refineExcludeOuterFaces parameters
          then
            IntSet.fromList
              [ faceIdIndex face
              | face <-
                  facesAtEvenBarrierDepth
                    triangulation
                    (isConstraintEdge triangulation)
              ]
          else IntSet.empty
  ( outcome
    , frozen
    , stats
    , publicationStats
    ) <-
    runTransactionWithPublication id (transactionShape domain) triangulation budget $ \mutable operation -> do
      installInterfaceBarriers mutable domain
      refinement <-
        refineMutable
          makeVertex
          mutable
          operation
          parameters
          budget
          initialExcludedFaces
          initialSeed
          domain
      removeInterfaceBarriers mutable triangulation domain
      pure refinement
  let (complete, added, excluded, visited, permittedFaces, interfaceBoundaryReads, boundaryCrossingAttempts) = outcome
  pure
    RefinementExecution
      { refinementExecutionResult =
          RefinementResult
            { refinedTriangulation = frozen
            , refinementStats = stats
            , refinementAddedVertices = added
            , refinementComplete = complete
            , refinementExcludedFaces = V.fromList (map (FaceId . fromIntegral) excluded)
            }
      , refinementExecutionVisitedFaces = visited
      , refinementExecutionPermittedFaces = permittedFaces
      , refinementExecutionInterfaceBoundaryReads = interfaceBoundaryReads
      , refinementExecutionBoundaryCrossingAttempts = boundaryCrossingAttempts
      , refinementExecutionPublicationStats = publicationStats
      }

transactionShape :: Maybe RefinementDomain -> TransactionShape
transactionShape = maybe DenseTransaction (const LocalTransaction)

installInterfaceBarriers
  :: MutableDcel s vertex directed undirected face
  -> Maybe RefinementDomain
  -> ST s ()
installInterfaceBarriers mutable =
  traverse_
    (\pair -> void (setConstraint mutable (2 * pair)))
    . maybe [] (IntSet.toAscList . refinementDomainInterfacePairs)

removeInterfaceBarriers
  :: MutableDcel s vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
  -> Maybe RefinementDomain
  -> ST s ()
removeInterfaceBarriers mutable input =
  traverse_
    (\pair ->
       let edge = UndirectedEdgeId (fromIntegral pair)
        in if isConstraintEdge input edge
             then pure ()
             else void (clearConstraint mutable (2 * pair))
    )
    . maybe [] (IntSet.toAscList . refinementDomainInterfacePairs)

buildRefinementReceipt
  :: RefinementDomain
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
  -> [Int]
  -> [Int]
  -> V.Vector (UndirectedEdgeId, FaceId, FaceId)
  -> Int
  -> Int
  -> PublicationStats
  -> ValidationClosureStats
  -> RefinementReceipt
buildRefinementReceipt domain before after visited permittedFaces finalInterfaceIncidence interfaceBoundaryReads boundaryCrossingAttempts publicationStats validationStats =
  RefinementReceipt
    { refinementVisitedJoinFaces = V.fromList (fmap toFace visitedJoin)
    , refinementVisitedProtectedFaces = V.fromList (fmap toFace visitedProtected)
    , refinementCreatedFaces = V.fromList (fmap toFace createdFaces)
    , refinementFinalPermittedFaces = V.fromList (fmap toFace permittedFaces)
    , refinementFinalInterfaceIncidence = finalInterfaceIncidence
    , refinementTouchedEdges = V.fromList (fmap toEdge touchedEdges)
    , refinementRemovedEdges = V.fromList (fmap toEdge removedEdges)
    , refinementInterfaceBoundaryReads = interfaceBoundaryReads
    , refinementAttemptedBoundaryCrossings = boundaryCrossingAttempts
    , refinementPublicationStats = publicationStats
    , refinementValidationClosureStats = validationStats
    }
 where
  permitted = refinementDomainPermittedFaces domain
  (visitedProtected, visitedJoin) =
    foldr
      (\face (protected, join) ->
         if IntSet.member face permitted
           then (protected, face : join)
           else if face >= refinementDomainInputFaceCount domain
             then (protected, face : join)
           else (face : protected, join))
      ([], []) visited
  initialFaces = refinementDomainPermittedFaces domain
  finalFaces = IntSet.fromList permittedFaces
  faceClosure = IntSet.toAscList (IntSet.union initialFaces finalFaces)
  createdFaces =
    [ face
    | face <- faceClosure
    , localFaceSignature before face /= localFaceSignature after face
    ]
  initialEdges = localDomainEdges before initialFaces
  finalEdges = localDomainEdges after finalFaces
  edgeClosure = IntSet.toAscList (IntSet.union initialEdges finalEdges)
  touchedEdges =
    [ edge
    | edge <- edgeClosure
    , localEdgeSignature before edge /= localEdgeSignature after edge
    ]
  removedEdges =
    [ edge
    | edge <- IntSet.toAscList initialEdges
    , localEdgeSignature before edge /= localEdgeSignature after edge
    ]
  toFace = FaceId . fromIntegral
  toEdge = UndirectedEdgeId . fromIntegral

localFaceSignature
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> Maybe [Point]
localFaceSignature triangulation raw
  | raw <= 0 || raw >= numFaces triangulation = Nothing
  | otherwise =
      Just
        ( sort
            (fmap (vertexPoint triangulation) (faceVertices triangulation (FaceId (fromIntegral raw))))
        )

localDomainEdges
  :: Triangulation mode vertex directed undirected face
  -> IntSet.IntSet
  -> IntSet.IntSet
localDomainEdges triangulation faces =
  IntSet.fromList
    [ undirectedEdgeIdIndex (asUndirected edge)
    | rawFace <- IntSet.toAscList faces
    , edge <- faceDirectedEdges triangulation (FaceId (fromIntegral rawFace))
    ]

localEdgeSignature
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> Maybe (Point, Point, [FaceId])
localEdgeSignature triangulation raw
  | raw < 0 || raw >= numUndirectedEdges triangulation = Nothing
  | otherwise =
      let edge = UndirectedEdgeId (fromIntegral raw)
          (fromVertex, toVertex) = undirectedEndpoints triangulation edge
          fromPoint = vertexPoint triangulation fromVertex
          toPoint = vertexPoint triangulation toVertex
          (forward, backward) = directedPair edge
       in Just
            ( min fromPoint toPoint
            , max fromPoint toPoint
            , sort
                [ incidentFace triangulation forward
                , incidentFace triangulation backward
                ]
            )

validateDomainClosure
  :: RefinementDomain
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
  -> [Int]
  -> Either BuildError (ValidationClosureStats, V.Vector (UndirectedEdgeId, FaceId, FaceId))
validateDomainClosure domain before after finalPermittedFaces = do
  let finalPermitted = IntSet.fromList finalPermittedFaces
      (closureStats, closureViolations) =
        validateTopologyClosureWithStats
          ( IntSet.union
              (refinementDomainPermittedFaces domain)
              finalPermitted
          )
          (refinementDomainInterfacePairs domain)
          after
  case closureViolations of
    violation : _ -> Left (RefinementInputTopologyInvalid violation)
    [] -> pure ()
  traverse_ validateProtectedFace (IntSet.toAscList protectedFaces)
  finalInterfaceIncidence <-
    V.fromList
      <$> traverse
        (validateInterfaceEdge finalPermitted)
        (IntSet.toAscList (refinementDomainInterfacePairs domain))
  pure (closureStats, finalInterfaceIncidence)
 where
  permitted = refinementDomainPermittedFaces domain
  protectedFaces =
    IntSet.fromList
      [ rawFace
      | rawPair <- IntSet.toAscList (refinementDomainInterfacePairs domain)
      , let edge = UndirectedEdgeId (fromIntegral rawPair)
      , let (forward, backward) = directedPair edge
      , rawFace <-
          [ faceIdIndex (incidentFace before forward)
          , faceIdIndex (incidentFace before backward)
          ]
      , rawFace > 0
      , IntSet.notMember rawFace permitted
      ]

  validateProtectedFace rawFace =
    if localFaceSignature before rawFace == localFaceSignature after rawFace
      then Right ()
      else Left (RefinementDomainProtectedFaceChanged (FaceId (fromIntegral rawFace)))

  validateInterfaceEdge finalPermitted rawPair =
    let edge = UndirectedEdgeId (fromIntegral rawPair)
     in case (localEdgeSignature before rawPair, localEdgeSignature after rawPair) of
      ( Just (beforeFrom, beforeTo, beforeFaces)
        , Just (afterFrom, afterTo, afterFaces)
        )
          | beforeFrom == afterFrom && beforeTo == afterTo ->
              validateInterfaceFaces finalPermitted edge beforeFaces afterFaces
      _ -> Left RefinementDomainTopologyChanged

  validateInterfaceFaces finalPermitted edge beforeFaces afterFaces =
    case filter protected beforeFaces of
      [protectedFace]
        | protectedFace `elem` afterFaces ->
            case filter (/= protectedFace) afterFaces of
              [oppositeFace]
                | IntSet.member (faceIdIndex oppositeFace) finalPermitted ->
                    Right (edge, protectedFace, oppositeFace)
                | otherwise ->
                    Left
                      ( RefinementDomainInterfaceOppositeFaceNotPermitted
                          edge
                          oppositeFace
                      )
              _ -> Left RefinementDomainTopologyChanged
      _ -> Left RefinementDomainTopologyChanged

  protected face =
    let raw = faceIdIndex face
     in raw > 0 && IntSet.notMember raw permitted

-- | Refuse outer or absent faces rather than silently treating an invalid
-- topology witness as an empty repair. Face zero is the outer face and has no
-- active refinement equation.
validateSeedFaces
  :: Triangulation mode vertex directed undirected face
  -> Set.Set FaceId
  -> Either BuildError IntSet.IntSet
validateSeedFaces triangulation requested =
  IntSet.fromList <$> traverse validateFace (Set.toAscList requested)
 where
  totalFaces = numFaces triangulation

  validateFace :: FaceId -> Either BuildError Int
  validateFace face
    | face == FaceId 0 || toInteger (unFaceId face) >= toInteger totalFaces =
        Left (RefinementSeedFaceNotActive face totalFaces)
    | otherwise = Right (faceIdIndex face)

-- | Refuse parameters no mesh can satisfy.
validateRefinementParameters :: RefinementParameters -> Either BuildError ()
validateRefinementParameters RefinementParameters{refineMaxAdditionalVertices, refineMinArea, refineMaxArea, refineMaxRadiusEdgeRatio, refineMaxEdgeLength} = do
  case refineMaxAdditionalVertices of
    Just value
      | value < 0 ->
          Left (RefinementMaximumAdditionalVerticesNegative value)
    _ -> Right ()
  validateOptionalRefinementParameter
    RefinementMinimumAreaNotFinite
    RefinementMinimumAreaNegative
    (>= 0)
    refineMinArea
  validateOptionalRefinementParameter
    RefinementMaximumAreaNotFinite
    RefinementMaximumAreaNotPositive
    (> 0)
    refineMaxArea
  validateOptionalRefinementParameter
    RefinementMaximumRadiusEdgeRatioNotFinite
    RefinementMaximumRadiusEdgeRatioNotPositive
    (> 0)
    refineMaxRadiusEdgeRatio
  validateOptionalRefinementParameter
    RefinementMaximumEdgeLengthNotFinite
    RefinementMaximumEdgeLengthNotPositive
    (> 0)
    refineMaxEdgeLength
  case (refineMinArea, refineMaxArea) of
    (Just minimumArea, Just maximumArea)
      | minimumArea > maximumArea ->
          Left
            ( RefinementMinimumAreaExceedsMaximum
                minimumArea
                maximumArea
            )
    _ -> Right ()

validateOptionalRefinementParameter
  :: (NonFiniteValue -> BuildError)
  -> (Double -> BuildError)
  -> (Double -> Bool)
  -> Maybe Double
  -> Either BuildError ()
validateOptionalRefinementParameter nonFinite outsideRange predicate value =
  case value of
    Nothing -> Right ()
    Just number ->
      case classifyNonFinite number of
        Just nonFiniteValue ->
          Left (nonFinite nonFiniteValue)
        Nothing
          | predicate number -> Right ()
          | otherwise ->
              Left (outsideRange number)

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ruppert refinement: Steiner insertion composed after a built mesh, never a
-- second kind of mesh. Parameters are reached through checked verbs rather than
-- a raw record, so an unrealizable quality bar is a refusal.
module Moonlight.Planar.Refinement
  ( refine
  , refineWithinDomain
  , validateRefinementParameters
  , withMinimumAngle
  , radiusEdgeRatioForAngle
  , withAdditionalVertexBudget
  , withoutAdditionalVertexBudget
  , withMinimumArea
  , withoutMinimumArea
  , withMaximumArea
  , withoutMaximumArea
  , withMaximumRadiusEdgeRatio
  , withoutMaximumRadiusEdgeRatio
  , withMaximumEdgeLength
  , withoutMaximumEdgeLength
  , withConvexHullPreservation
  , withConstraintPreservation
  , withOuterFaceExclusion
  , refinementAdditionalVertexBudget
  , refinementMinimumArea
  , refinementMaximumArea
  , refinementMaximumRadiusEdgeRatio
  , refinementMaximumEdgeLength
  , refinementPreservesConvexHull
  , refinementPreservesConstraints
  , refinementExcludesOuterFaces
  ) where

import Control.Monad (void)
import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Moonlight.Planar.Dcel
  ( faceDirectedEdges
  , faceVertices
  , incidentFace
  , innerFaceDirectedEdges
  , isConstraintEdge
  , numFaces
  , numUndirectedEdges
  , numVertices
  , origin
  , undirectedEndpoints
  , vertexPoint
  )
import Moonlight.Planar.FloodFillIterator (facesAtEvenBarrierDepth)
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId (..)
  , FaceId (..)
  , UndirectedEdgeId (..)
  , asUndirected
  , directedPair
  , faceIdIndex
  , isNormalized
  , reverseEdge
  , undirectedEdgeIdIndex
  )
import Moonlight.Planar.Internal.Mutable
import Moonlight.Planar.Internal.Paged
  ( PublicationStats
  , TransactionShape (DenseTransaction, LocalTransaction)
  )
import Moonlight.Planar.Internal.Refinement
import Moonlight.Planar.Internal.Types (RefinementParameters (..))
import Moonlight.Planar.Internal.Representation (Triangulation (..))
import Moonlight.Planar.Internal.Transaction (runTransactionWithPublication)
import Moonlight.Planar.Internal.Validation
  ( topologyClosureStats
  )
import Moonlight.Planar.Types hiding (RefinementParameters)
import Moonlight.Planar.Point (Point (..))
import Moonlight.Planar.Scalar (NonFiniteValue (..), classifyNonFinite)

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
  checkedRefinementParameters parameters{refineMaxRadiusEdgeRatio = ratio}

withAdditionalVertexBudget :: Int -> RefinementParameters -> Either BuildError RefinementParameters
withAdditionalVertexBudget budget parameters =
  checkedRefinementParameters parameters{refineMaxAdditionalVertices = Just budget}

withoutAdditionalVertexBudget :: RefinementParameters -> RefinementParameters
withoutAdditionalVertexBudget parameters =
  parameters{refineMaxAdditionalVertices = Nothing}

withMinimumArea :: Double -> RefinementParameters -> Either BuildError RefinementParameters
withMinimumArea area parameters =
  checkedRefinementParameters parameters{refineMinArea = Just area}

withoutMinimumArea :: RefinementParameters -> RefinementParameters
withoutMinimumArea parameters =
  parameters{refineMinArea = Nothing}

withMaximumArea :: Double -> RefinementParameters -> Either BuildError RefinementParameters
withMaximumArea area parameters =
  checkedRefinementParameters parameters{refineMaxArea = Just area}

withoutMaximumArea :: RefinementParameters -> RefinementParameters
withoutMaximumArea parameters =
  parameters{refineMaxArea = Nothing}

withMaximumRadiusEdgeRatio :: Double -> RefinementParameters -> Either BuildError RefinementParameters
withMaximumRadiusEdgeRatio ratio parameters =
  checkedRefinementParameters parameters{refineMaxRadiusEdgeRatio = Just ratio}

withoutMaximumRadiusEdgeRatio :: RefinementParameters -> RefinementParameters
withoutMaximumRadiusEdgeRatio parameters =
  parameters{refineMaxRadiusEdgeRatio = Nothing}

withMaximumEdgeLength :: Double -> RefinementParameters -> Either BuildError RefinementParameters
withMaximumEdgeLength lengthValue parameters =
  checkedRefinementParameters parameters{refineMaxEdgeLength = Just lengthValue}

withoutMaximumEdgeLength :: RefinementParameters -> RefinementParameters
withoutMaximumEdgeLength parameters =
  parameters{refineMaxEdgeLength = Nothing}

withConvexHullPreservation :: Bool -> RefinementParameters -> RefinementParameters
withConvexHullPreservation preserve parameters =
  parameters{refinePreserveConvexHull = preserve}

withConstraintPreservation :: Bool -> RefinementParameters -> RefinementParameters
withConstraintPreservation preserve parameters =
  parameters{refineKeepConstraintEdges = preserve}

withOuterFaceExclusion :: Bool -> RefinementParameters -> RefinementParameters
withOuterFaceExclusion exclude parameters =
  parameters{refineExcludeOuterFaces = exclude}

refinementAdditionalVertexBudget :: RefinementParameters -> Maybe Int
refinementAdditionalVertexBudget = refineMaxAdditionalVertices

refinementMinimumArea :: RefinementParameters -> Maybe Double
refinementMinimumArea = refineMinArea

refinementMaximumArea :: RefinementParameters -> Maybe Double
refinementMaximumArea = refineMaxArea

refinementMaximumRadiusEdgeRatio :: RefinementParameters -> Maybe Double
refinementMaximumRadiusEdgeRatio = refineMaxRadiusEdgeRatio

refinementMaximumEdgeLength :: RefinementParameters -> Maybe Double
refinementMaximumEdgeLength = refineMaxEdgeLength

refinementPreservesConvexHull :: RefinementParameters -> Bool
refinementPreservesConvexHull = refinePreserveConvexHull

refinementPreservesConstraints :: RefinementParameters -> Bool
refinementPreservesConstraints = refineKeepConstraintEdges

refinementExcludesOuterFaces :: RefinementParameters -> Bool
refinementExcludesOuterFaces = refineExcludeOuterFaces

checkedRefinementParameters
  :: RefinementParameters
  -> Either BuildError RefinementParameters
checkedRefinementParameters parameters = do
  validateRefinementParameters parameters
  pure parameters

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
-- transaction-local legalization barriers and removed before publication.
-- The collar proof checks the seam (protected faces and interface edges);
-- the receipt reads only the closure the domain spans, and counts it rather
-- than re-validating the interpreter's output.
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
      finalPermittedList = refinementExecutionPermittedFaces execution
      -- The interpreter publishes its permitted faces as marked indices in
      -- ascending order, so the set is built once here without sorting and
      -- shared by the seam check and the receipt.
      finalPermitted = IntSet.fromDistinctAscList finalPermittedList
  (closureStats, finalInterfaceIncidence) <- validateDomainClosure
    domain
    triangulation
    (refinedTriangulation result)
    finalPermitted
  let receipt =
        buildRefinementReceipt
          domain
          triangulation
          (refinedTriangulation result)
          (refinementExecutionVisitedFaces execution)
          finalPermittedList
          finalPermitted
          finalInterfaceIncidence
          (refinementExecutionInterfaceBoundaryReads execution)
          (refinementExecutionBoundaryCrossingAttempts execution)
          (refinementExecutionPublicationStats execution)
          closureStats
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
  -> IntSet.IntSet
  -> V.Vector (UndirectedEdgeId, FaceId, FaceId)
  -> Int
  -> Int
  -> PublicationStats
  -> ClosureStats
  -> RefinementReceipt
buildRefinementReceipt domain before after visited permittedFaces finalFaces finalInterfaceIncidence interfaceBoundaryReads boundaryCrossingAttempts publicationStats closureStats =
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
    , refinementClosureStats = closureStats
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
  -- The face closure is the initial faces merged with the final faces new to
  -- them, both ascending, so the union is never materialized.
  faceClosure =
    mergeAscending
      (IntSet.toAscList initialFaces)
      (filter (`IntSet.notMember` initialFaces) permittedFaces)
  createdFaces =
    [ face
    | face <- faceClosure
    , localFaceSignature before face /= localFaceSignature after face
    ]
  -- The edge closure is emitted once per pair from the cycles of its faces:
  -- an initial face at `before` yields the pairs of its cycle whose other
  -- side is not a lower initial face, a final face at `after` likewise over
  -- the final faces, minus every pair an initial cycle already carried.
  touchedEdges =
    sort
      [ pair
      | pair <-
          sectionPairs before initialFaces (IntSet.toAscList initialFaces)
            ++ filter (not . inInitialSection)
                 (sectionPairs after finalFaces permittedFaces)
      , localEdgeSignature before pair /= localEdgeSignature after pair
      ]
  -- A removed edge is a touched edge of the initial section: the touched list
  -- restricted to the initial edges, not a second pass over their signatures.
  removedEdges = filter inInitialSection touchedEdges
  initialPairCount = numUndirectedEdges before
  inInitialSection pair =
    pair < initialPairCount
      && any (carriedByInitialFace . fromIntegral) [2 * pair, 2 * pair + 1]
  -- A recycled slot may still point at an initial face from before, so the
  -- face's cycle is consulted rather than the slot's face pointer alone.
  carriedByInitialFace edge =
    let face = incidentFace before (DirectedEdgeId edge)
     in IntSet.member (faceIdIndex face) initialFaces
          && DirectedEdgeId edge `elem` faceDirectedEdges before face
  toFace = FaceId . fromIntegral
  toEdge = UndirectedEdgeId . fromIntegral

-- | A face's geometric identity: its cycle's points in ascending order. A
-- triangle is held in three fields rather than a sorted list; any other cycle
-- keeps the list so the identity stays total over what the arena may hold.
data FaceSignature
  = FaceAbsent
  | FaceTriangle !Point !Point !Point
  | FaceCycle [Point]
  deriving stock (Eq)

localFaceSignature
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> FaceSignature
localFaceSignature triangulation raw
  | raw <= 0 || raw >= numFaces triangulation = FaceAbsent
  | otherwise =
      let face = FaceId (fromIntegral raw)
       in case innerFaceDirectedEdges triangulation face of
            Just (e0, e1, e2)
              | e0 /= e1 ->
                  sortedTriangle
                    (vertexPoint triangulation (origin triangulation e0))
                    (vertexPoint triangulation (origin triangulation e1))
                    (vertexPoint triangulation (origin triangulation e2))
            _ -> FaceCycle (sort (fmap (vertexPoint triangulation) (faceVertices triangulation face)))

sortedTriangle :: Point -> Point -> Point -> FaceSignature
sortedTriangle a b c
  | a <= b =
      if b <= c
        then FaceTriangle a b c
        else if a <= c then FaceTriangle a c b else FaceTriangle c a b
  | otherwise =
      if a <= c
        then FaceTriangle b a c
        else if b <= c then FaceTriangle b c a else FaceTriangle c b a

-- | The undirected edges carried by a section's faces, each once: a cycle
-- edge belongs to the lowest section face on either side of it, and to the
-- normalized direction when one face carries both directions.
sectionPairs
  :: Triangulation mode vertex directed undirected face
  -> IntSet.IntSet
  -> [Int]
  -> [Int]
sectionPairs triangulation faces faceList =
  [ undirectedEdgeIdIndex (asUndirected edge)
  | rawFace <- faceList
  , edge <- faceDirectedEdges triangulation (FaceId (fromIntegral rawFace))
  , let across = faceIdIndex (incidentFace triangulation (reverseEdge edge))
  , IntSet.notMember across faces
      || rawFace < across
      || (rawFace == across && isNormalized edge)
  ]

mergeAscending :: [Int] -> [Int] -> [Int]
mergeAscending [] ys = ys
mergeAscending xs [] = xs
mergeAscending (x : xs) (y : ys)
  | x < y = x : mergeAscending xs (y : ys)
  | otherwise = y : mergeAscending (x : xs) ys

-- | An edge's identity: its endpoints and its two incident faces, each pair
-- in ascending order so the directed orientation does not count.
data EdgeSignature
  = EdgeAbsent
  | EdgePresent !Point !Point !FaceId !FaceId
  deriving stock (Eq)

localEdgeSignature
  :: Triangulation mode vertex directed undirected face
  -> Int
  -> EdgeSignature
localEdgeSignature triangulation raw
  | raw < 0 || raw >= numUndirectedEdges triangulation = EdgeAbsent
  | otherwise =
      let edge = UndirectedEdgeId (fromIntegral raw)
          (fromVertex, toVertex) = undirectedEndpoints triangulation edge
          !fromPoint = vertexPoint triangulation fromVertex
          !toPoint = vertexPoint triangulation toVertex
          (forward, backward) = directedPair edge
          !forwardFace = incidentFace triangulation forward
          !backwardFace = incidentFace triangulation backward
       in EdgePresent
            (min fromPoint toPoint)
            (max fromPoint toPoint)
            (min forwardFace backwardFace)
            (max forwardFace backwardFace)

validateDomainClosure
  :: RefinementDomain
  -> Triangulation mode vertex directed undirected face
  -> Triangulation mode vertex directed undirected face
  -> IntSet.IntSet
  -> Either BuildError (ClosureStats, V.Vector (UndirectedEdgeId, FaceId, FaceId))
validateDomainClosure domain before after finalPermitted = do
  -- The published value is the interpreter's own output, admitted by its
  -- type as every other publication is; the closure it spans is counted,
  -- not walked. What is checked here is the seam: the protected collar and
  -- the interface edges the domain promised to leave alone.
  let closureStats =
        topologyClosureStats
          (refinementDomainPermittedFaces domain)
          finalPermitted
          (refinementDomainInterfacePairs domain)
          after
  traverse_ validateProtectedFace (IntSet.toAscList protectedFaces)
  finalInterfaceIncidence <-
    V.fromList
      <$> traverse
        validateInterfaceEdge
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

  validateInterfaceEdge rawPair =
    let edge = UndirectedEdgeId (fromIntegral rawPair)
     in case (localEdgeSignature before rawPair, localEdgeSignature after rawPair) of
      ( EdgePresent beforeFrom beforeTo beforeLeft beforeRight
        , EdgePresent afterFrom afterTo afterLeft afterRight
        )
          | beforeFrom == afterFrom && beforeTo == afterTo ->
              validateInterfaceFaces edge [beforeLeft, beforeRight] [afterLeft, afterRight]
      _ -> Left RefinementDomainTopologyChanged

  validateInterfaceFaces edge beforeFaces afterFaces =
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

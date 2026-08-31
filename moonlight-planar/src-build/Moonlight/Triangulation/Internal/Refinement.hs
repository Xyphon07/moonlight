{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Ruppert refinement over three local worklists: forced segment splits,
-- fixed-edge encroachment candidates, and skinny faces. A face candidate is
-- transactional: preflight locates the circumcenter and follows the
-- prospective legalization cavity without mutating topology; a fixed edge the
-- cavity meets is tested against the point's diametral disk and aborts the
-- plan into a segment split; otherwise the located site commits directly.
module Moonlight.Triangulation.Internal.Refinement
  ( RefinementInitialSeed (..)
  , RefinementDomain (..)
  , refineMutable
  ) where

import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.ST (ST)
import Data.Foldable (traverse_)
import qualified Data.IntSet as IntSet
import Data.List (find)
import qualified Data.List as List
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes, fromMaybe)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)
import Moonlight.Triangulation.Internal.HandleDefs (FaceId (..), UndirectedEdgeId (..))
import Moonlight.Triangulation.Internal.DcelOperations.Legalize (legalizeEdges)
import Moonlight.Triangulation.Internal.DcelOperations.Subdivide (insertOnEdge)
import Moonlight.Triangulation.Internal.DcelOperations.Twin (reverseIndex)
import Moonlight.Triangulation.Internal.Dyadic (exactDiametralDot)
import Moonlight.Triangulation.Internal.Growable
  ( GrowableWord32
  , clearGrowable
  , growableLength
  , newGrowableWord32
  , popGrowableOr
  , pushGrowable
  , readGrowable
  )
import Moonlight.Triangulation.Internal.Location
  ( MutableLocation (..)
  , locateMutable
  , locateMutableWithin
  )
import Moonlight.Triangulation.Internal.FaceQueue
  ( FaceQueue
  , newFaceQueue
  , popFace
  , pushFace
  )
import Moonlight.Triangulation.Internal.Mutable
  ( MutableDcel (..)
  , addEdgeBlock
  , addFaceBlock
  , appendVertex
  , directedEdgeCount
  , edgeOriginPoint
  , ensureCellCapacity
  , faceCount
  , faceEdges
  , markConnected
  , pointAt
  , pointCapacity
  , readConstraint
  , readFace
  , readNext
  , readOrigin
  , readPrevious
  , readVertexOut
  , resetEdgeData
  , resetFaceData
  , setCycle3
  , writeOrigin
  , writeVertexOut
  )
import Moonlight.Triangulation.Internal.OperationState
  ( Counter (..)
  , OperationState
  , addCounter
  , maxCounter
  , readScratch
  , writeScratch
  )
import Moonlight.Triangulation.Internal.PackedIndex (noIndex, packIndex, unpackIndex)
import Moonlight.Triangulation.Internal.Predicates
  ( inCircle
  , inDiametralCircle
  , orient2d
  )
import Moonlight.Triangulation.Internal.Probe (Probe (..))
import Moonlight.Triangulation.Math
  ( canonicalPoint
  , circumcenter
  , isFinite
  , midpoint
  , squaredDistance
  , squaredDistanceWide
  , triangleArea
  , triangleRadiusEdgeRatioSquaredWithArea
  , triangleRadiusEdgeRatioSquaredWithAreaAndSides
  , validateCoordinate
  )
import Moonlight.Triangulation.Insertion (insertExistingVertexAtLocation)
import Moonlight.Triangulation.Types
  ( BuildError (..)
  , Point (..)
  , RefinementParameters (..)
  )

-- | The worklists refinement owns for the duration of one transaction. Cavity
-- and segment queues are local to the current rewrite; the face-capacity mark
-- planes remain a bounded publication workspace until the compact arena is
-- replaced by a domain-indexed one.
data BoolMarks s
  = DenseBoolMarks !(MUV.MVector s Bool)
  | SparseBoolMarks !(STRef s IntSet.IntSet)

data EpochMarks s
  = DenseEpochMarks !(MUV.MVector s Word32)
  | SparseEpochMarks !(STRef s (IntMap.IntMap Word32))

data PairSlots s
  = DensePairSlots !(MUV.MVector s Word32)
  | SparsePairSlots !(STRef s (IntMap.IntMap Word32))

data SegmentOrigins s
  = DenseSegmentOrigins !(MUV.MVector s Word32)
  | SparseSegmentOrigins !(STRef s (IntMap.IntMap (Word32, Word32)))

newBoolMarks :: Maybe Int -> ST s (BoolMarks s)
newBoolMarks capacity =
  case capacity of
    Just size -> DenseBoolMarks <$> MUV.replicate (max 1 size) False
    Nothing -> SparseBoolMarks <$> newSTRef IntSet.empty

readBoolMark :: BoolMarks s -> Int -> ST s Bool
readBoolMark marks index =
  case marks of
    DenseBoolMarks values -> MUV.unsafeRead values index
    SparseBoolMarks values -> IntSet.member index <$> readSTRef values

writeBoolMark :: BoolMarks s -> Int -> Bool -> ST s ()
writeBoolMark marks index enabled =
  case marks of
    DenseBoolMarks values -> MUV.unsafeWrite values index enabled
    SparseBoolMarks values ->
      modifySTRef' values
        (if enabled then IntSet.insert index else IntSet.delete index)

newEpochMarks :: Maybe Int -> ST s (EpochMarks s)
newEpochMarks capacity =
  case capacity of
    Just size -> DenseEpochMarks <$> MUV.replicate (max 1 size) 0
    Nothing -> SparseEpochMarks <$> newSTRef IntMap.empty

readEpochMark :: EpochMarks s -> Int -> ST s Word32
readEpochMark marks index =
  case marks of
    DenseEpochMarks values -> MUV.unsafeRead values index
    SparseEpochMarks values -> fromMaybe 0 . IntMap.lookup index <$> readSTRef values

writeEpochMark :: EpochMarks s -> Int -> Word32 -> ST s ()
writeEpochMark marks index epoch =
  case marks of
    DenseEpochMarks values -> MUV.unsafeWrite values index epoch
    SparseEpochMarks values -> modifySTRef' values (IntMap.insert index epoch)

clearEpochMarks :: EpochMarks s -> ST s ()
clearEpochMarks marks =
  case marks of
    DenseEpochMarks values -> MUV.set values 0
    SparseEpochMarks values -> writeSTRef values IntMap.empty

newPairSlots :: Maybe Int -> ST s (PairSlots s)
newPairSlots capacity =
  case capacity of
    Just size -> DensePairSlots <$> MUV.replicate (max 1 size) noIndex
    Nothing -> SparsePairSlots <$> newSTRef IntMap.empty

readPairSlot :: PairSlots s -> Int -> ST s Word32
readPairSlot slots pair =
  case slots of
    DensePairSlots values -> MUV.unsafeRead values pair
    SparsePairSlots values -> fromMaybe noIndex . IntMap.lookup pair <$> readSTRef values

writePairSlot :: PairSlots s -> Int -> Word32 -> ST s ()
writePairSlot slots pair value =
  case slots of
    DensePairSlots values -> MUV.unsafeWrite values pair value
    SparsePairSlots values ->
      modifySTRef' values
        (if value == noIndex then IntMap.delete pair else IntMap.insert pair value)

newSegmentOrigins :: Maybe Int -> ST s (SegmentOrigins s)
newSegmentOrigins capacity =
  case capacity of
    Just size -> DenseSegmentOrigins <$> MUV.replicate (2 * max 1 size) noIndex
    Nothing -> SparseSegmentOrigins <$> newSTRef IntMap.empty

readSegmentOrigin :: SegmentOrigins s -> Int -> ST s (Word32, Word32)
readSegmentOrigin origins vertex = do
  case origins of
    DenseSegmentOrigins values ->
      (,)
        <$> MUV.unsafeRead values (2 * vertex)
        <*> MUV.unsafeRead values (2 * vertex + 1)
    SparseSegmentOrigins values -> do
      stored <- readSTRef values
      pure (fromMaybe (noIndex, noIndex) (IntMap.lookup vertex stored))

writeSegmentOrigin :: SegmentOrigins s -> Int -> Word32 -> Word32 -> ST s ()
writeSegmentOrigin origins vertex first second =
  case origins of
    DenseSegmentOrigins values -> do
      MUV.unsafeWrite values (2 * vertex) first
      MUV.unsafeWrite values (2 * vertex + 1) second
    SparseSegmentOrigins values ->
      modifySTRef' values (IntMap.insert vertex (first, second))

data Workspace s = Workspace
  { wsQueue :: !(FaceQueue s)
  , wsExcluded :: !(BoolMarks s)
  , wsPermitted :: !(Maybe (BoolMarks s))
  , wsVisited :: !(Maybe (BoolMarks s))
  , wsInterfaceBoundaryReads :: !(STRef s Int)
  , wsBoundaryCrossingAttempts :: !(STRef s Int)
  , wsForcedSplits :: !(GrowableWord32 s)
  , wsEncroachment :: !(GrowableWord32 s)
  -- | Where each pair's live entry sits in 'wsEncroachment', 'noIndex' when
  -- the pair is not queued. A re-push relocates the pair to the top and leaves
  -- the older entry behind as a slot that no longer names it, so a pair is
  -- tested once at its most recent offer instead of once per offer.
  , wsEncroachmentSlot :: !(PairSlots s)
  -- | Steiner vertices on constraints carry the original segment's endpoint
  -- pair, two words per vertex, 'noIndex' when the vertex is not on one.
  , wsSegmentOrigin :: !(SegmentOrigins s)
  -- | Cavity discovery state: an epoch stamp per face, edge pair and boundary
  -- vertex, plus the three records a successful commit consumes. A cavity is
  -- explored without mutating topology; the commit either fans the recorded
  -- boundary or abandons the records entirely.
  , wsCavityEpoch :: !(STRef s Word32)
  , wsCavityFaceMarks :: !(EpochMarks s)
  , wsCavityPairMarks :: !(EpochMarks s)
  , wsCavityVertexMarks :: !(EpochMarks s)
  , wsCavityFaces :: !(GrowableWord32 s)
  , wsCavityInternal :: !(GrowableWord32 s)
  , wsCavityBoundary :: !(GrowableWord32 s)
  , wsCavityCocircular :: !(GrowableWord32 s)
  -- | The recorded boundary in cycle order, written by the chaining walk and
  -- read by the fan.
  , wsCavityChain :: !(GrowableWord32 s)
  -- | The flood and hull-simulation stack. The old mesh-global work stack
  -- served both; the transaction owns it now, cleared before either use.
  , wsCavityWork :: !(GrowableWord32 s)
  }

data FaceHint
  = FaceAcceptable
  | FaceMustSplitLongestEdge
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Double
  | FaceMustRefine
  | FaceShouldRefine

data EdgeSplitLaw
  = SplitFixedSegment
  | SplitQualityEdge

-- | The initial support a refinement run is allowed to inspect. Subsequent
-- cavity, star, and fixed-edge propagation remains exactly the ordinary
-- refinement law; only the first offers differ. The constructor is internal
-- so public callers cannot smuggle a mutable worklist across the transaction
-- boundary.
data RefinementInitialSeed
  = RefineEveryFace
  | RefineSeededFaces !IntSet.IntSet

-- | A checked local section. Face and edge integers are admitted by the
-- immutable constructor in "Moonlight.Triangulation.Refinement"; the mutable
-- interpreter sees only the already-descended dense membership witnesses.
data RefinementDomain = RefinementDomain
  { refinementDomainPermittedFaces :: !IntSet.IntSet
  , refinementDomainInterfacePairs :: !IntSet.IntSet
  , refinementDomainInputFaceCount :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

-- | Refine the existing finite DCEL. Fixed edges (constraints and the convex
-- hull) are legal barriers: a candidate whose prospective cavity meets one
-- inside its diametral disk is abandoned and the segment is split instead.
-- The result is @(worklists-drained, Steiner-count, excluded-outer-faces)@.
refineMutable
  :: forall s vertex directed undirected face. (Point -> vertex)
  -> MutableDcel s vertex directed undirected face
  -> OperationState s
  -> RefinementParameters
  -> Int
  -> IntSet.IntSet
  -> RefinementInitialSeed
  -> Maybe RefinementDomain
  -> ST s (Either BuildError (Bool, Int, [Int], [Int], [Int], Int, Int))
refineMutable makeVertex mutable operation parameters budget initialExcludedFaces initialSeed domain = do
  let !faceBound =
        case domain of
          Nothing -> Just (3 * pointCapacity mutable + 8)
          Just _ -> Nothing
      !pairBound =
        case domain of
          Nothing -> Just (mdHalfCapacity mutable `quot` 2)
          Just _ -> Nothing
      !vertexBound =
        case domain of
          Nothing -> Just (pointCapacity mutable)
          Just _ -> Nothing
  wsQueue <- newFaceQueue faceBound
  wsExcluded <- newBoolMarks faceBound
  wsPermitted <- traverse (const (newBoolMarks Nothing)) domain
  wsVisited <- traverse (const (newBoolMarks Nothing)) domain
  wsInterfaceBoundaryReads <- newSTRef 0
  wsBoundaryCrossingAttempts <- newSTRef 0
  traverse_
    (\membership ->
       traverse_
         (\face -> writeBoolMark membership face True)
         (maybe [] (IntSet.toAscList . refinementDomainPermittedFaces) domain)
    )
    wsPermitted
  when (refineExcludeOuterFaces parameters) $
    traverse_ (\face -> writeBoolMark wsExcluded face True)
      (IntSet.toAscList initialExcludedFaces)
  wsForcedSplits <- newGrowableWord32 16
  wsEncroachment <- newGrowableWord32 64
  wsEncroachmentSlot <- newPairSlots pairBound
  wsSegmentOrigin <- newSegmentOrigins vertexBound
  wsCavityEpoch <- newSTRef 0
  wsCavityFaceMarks <- newEpochMarks faceBound
  wsCavityPairMarks <- newEpochMarks pairBound
  wsCavityVertexMarks <- newEpochMarks vertexBound
  wsCavityFaces <- newGrowableWord32 16
  wsCavityInternal <- newGrowableWord32 16
  wsCavityBoundary <- newGrowableWord32 16
  wsCavityCocircular <- newGrowableWord32 4
  wsCavityChain <- newGrowableWord32 16
  wsCavityWork <- newGrowableWord32 16
  let workspace = Workspace{wsQueue, wsExcluded, wsPermitted, wsVisited, wsInterfaceBoundaryReads, wsBoundaryCrossingAttempts, wsForcedSplits, wsEncroachment, wsEncroachmentSlot, wsSegmentOrigin, wsCavityEpoch, wsCavityFaceMarks, wsCavityPairMarks, wsCavityVertexMarks, wsCavityFaces, wsCavityInternal, wsCavityBoundary, wsCavityCocircular, wsCavityChain, wsCavityWork}
  seedInitialWork workspace initialSeed
  loop workspace IntMap.empty 0
 where
  !limit = max 0 budget

  -- The three bounds every queued face is measured against. They are fixed for
  -- the whole run, and the ratio bound is squared once here rather than once
  -- per face the ratio test reaches.
  !maximumArea = refineMaxArea parameters
  !minimumArea = refineMinArea parameters
  !maximumSquaredEdgeLength = fmap (\bound -> bound * bound) (refineMaxEdgeLength parameters)
  !squaredRatioBound = case refineMaxRadiusEdgeRatio parameters of
    Nothing -> Nothing
    Just bound -> Just $! squareBound bound

  -- One charged stack push: the old mesh-global 'pushWork' wrapper fed the
  -- legalization depth counter on every push, and the hull-simulation sites
  -- used that wrapper deliberately. The cavity flood's bypass stays raw.
  pushChargedWork :: GrowableWord32 s -> Int -> ST s ()
  pushChargedWork work value = do
    pushGrowable work (packIndex value)
    size <- growableLength work
    maxCounter operation CounterLegalizationMaxStack size

  -- Offer a fixed edge pair for the encroachment question. An offer always
  -- wins the pair's slot, so the pair is answered at its most recent offer and
  -- the entries it left behind are recognised as superseded when reached.
  pushEncroachment :: Workspace s -> Int -> ST s ()
  pushEncroachment Workspace{wsEncroachment, wsEncroachmentSlot} pair = do
    index <- growableLength wsEncroachment
    pushGrowable wsEncroachment (packIndex pair)
    writePairSlot wsEncroachmentSlot pair (packIndex index)

  -- 'noIndex' when the offers are drained.
  popEncroachment :: Workspace s -> ST s Word32
  popEncroachment Workspace{wsEncroachment, wsEncroachmentSlot} = go
   where
    go = do
      size <- growableLength wsEncroachment
      if size <= 0
        then pure noIndex
        else do
          let !index = size - 1
          packed <- popGrowableOr noIndex wsEncroachment
          let !pair = fromIntegral packed
          slot <- readPairSlot wsEncroachmentSlot pair
          if slot == packIndex index
            then do
              writePairSlot wsEncroachmentSlot pair noIndex
              pure packed
            else go

  -- The cavity epoch stamps three planes and is compared against them, so the
  -- wrap has to retire every stamp a reused value would answer for.
  nextCavityEpoch :: Workspace s -> ST s Word32
  nextCavityEpoch Workspace{wsCavityEpoch, wsCavityFaceMarks, wsCavityPairMarks, wsCavityVertexMarks} = do
    current <- readSTRef wsCavityEpoch
    let !next = current + 1
    if next == 0
      then do
        clearEpochMarks wsCavityFaceMarks
        clearEpochMarks wsCavityPairMarks
        clearEpochMarks wsCavityVertexMarks
        writeSTRef wsCavityEpoch 1
        pure 1
      else do
        writeSTRef wsCavityEpoch next
        pure next

  loop
    :: Workspace s
    -> IntMap.IntMap Double
    -> Int
    -> ST s (Either BuildError (Bool, Int, [Int], [Int], [Int], Int, Int))
  loop workspace@Workspace{wsQueue, wsForcedSplits} pendingOversizedEdges !added
    | added >= limit = finish workspace pendingOversizedEdges False added
    | otherwise = do
        forced <- popGrowableOr noIndex wsForcedSplits
        if forced /= noIndex
          then do
            splitOutcome <- resolveSplit SplitFixedSegment workspace (fromIntegral forced)
            case splitOutcome of
              Left obstruction -> pure (Left obstruction)
              Right split ->
                if split
                  then loop workspace pendingOversizedEdges (added + 1)
                  else do
                    -- The split refused (degenerate position or a kept
                    -- constraint). Retrying the face that forced it would loop,
                    -- so one queued face is sacrificed, exactly the face whose
                    -- requeue sits on top.
                    _ <- popFace wsQueue
                    loop workspace pendingOversizedEdges added
          else do
            candidate <- popEncroachment workspace
            if candidate /= noIndex
              then do
                splitOutcome <- checkEncroachment workspace (fromIntegral candidate)
                case splitOutcome of
                  Left obstruction -> pure (Left obstruction)
                  Right split ->
                    loop
                      workspace
                      pendingOversizedEdges
                      (if split then added + 1 else added)
              else do
                next <- popFace wsQueue
                case next of
                  Nothing -> finish workspace pendingOversizedEdges True added
                  Just face -> do
                    outcome <- handleFace workspace face
                    case outcome of
                      Left failure -> pure (Left failure)
                      Right (gained, pendingEdge) ->
                        loop
                          workspace
                          ( maybe
                              pendingOversizedEdges
                              (\(pair, actualSquaredLength) ->
                                 IntMap.insertWith
                                   max
                                   pair
                                   actualSquaredLength
                                   pendingOversizedEdges
                              )
                              pendingEdge
                          )
                          (added + gained)

  finish
    :: Workspace s
    -> IntMap.IntMap Double
    -> Bool
    -> Int
    -> ST s (Either BuildError (Bool, Int, [Int], [Int], [Int], Int, Int))
  finish workspace@Workspace{wsExcluded, wsPermitted, wsVisited, wsInterfaceBoundaryReads, wsBoundaryCrossingAttempts} pendingOversizedEdges complete added = do
    pendingObstruction <-
      if complete
        then completedMaximumEdgeObstruction workspace pendingOversizedEdges
        else pure (Right Nothing)
    case pendingObstruction of
      Left failure -> pure (Left failure)
      Right (Just failure) -> pure (Left failure)
      Right Nothing -> do
        excludedFaces <- markedIndices wsExcluded
        visitedFaces <- maybe (pure []) markedIndices wsVisited
        permittedFaces <- maybe (pure []) markedIndices wsPermitted
        interfaceBoundaryReads <- readSTRef wsInterfaceBoundaryReads
        boundaryCrossingAttempts <- readSTRef wsBoundaryCrossingAttempts
        pure (Right (complete, added, excludedFaces, visitedFaces, permittedFaces, interfaceBoundaryReads, boundaryCrossingAttempts))

  -- A quality edge that cannot be split is remembered at the point where the
  -- worklist discovers it.  Only a drained run discharges the pending section;
  -- a budget-limited run remains an ordinary resumable publication.  The
  -- final face is read from the constant-degree live edge neighbourhood so the
  -- reported obstruction retains the former face/edge ordering without an
  -- all-face audit.
  completedMaximumEdgeObstruction
    :: Workspace s
    -> IntMap.IntMap Double
    -> ST s (Either BuildError (Maybe BuildError))
  completedMaximumEdgeObstruction workspace pendingOversizedEdges =
    case refineMaxEdgeLength parameters of
      Nothing -> pure (Right Nothing)
      Just maximumLength -> do
        candidateResults <-
          traverse
            (liveOversizedEdgeCandidate workspace)
            (IntMap.toAscList pendingOversizedEdges)
        pure $ do
          candidates <- catMaybes <$> sequence candidateResults
          Right
            ( case List.sortOn (\(face, pair, _) -> (face, pair)) candidates of
                [] -> Nothing
                (face, pair, actualSquaredLength) : _ ->
                  Just
                    ( RefinementOversizedEdge
                        (FaceId (fromIntegral face))
                        (UndirectedEdgeId (fromIntegral pair))
                        (sqrt actualSquaredLength)
                        maximumLength
                    )
            )

  liveOversizedEdgeCandidate
    :: Workspace s
    -> (Int, Double)
    -> ST s (Either BuildError (Maybe (Int, Int, Double)))
  liveOversizedEdgeCandidate workspace (pair, actualSquaredLength) = do
    directedTotal <- directedEdgeCount mutable
    let directed = 2 * pair
    if directed < 0 || directed + 1 >= directedTotal
      then pure (Left RefinementDomainTopologyChanged)
      else do
        forwardFace <- readFace mutable directed
        backwardFace <- readFace mutable (reverseIndex directed)
        eligibleFaces <-
          filterM
            (maximumEdgeFaceEligible workspace)
            [forwardFace, backwardFace]
        pure
          ( Right
              ( case eligibleFaces of
                  [] -> Nothing
                  _ -> Just (minimum eligibleFaces, pair, actualSquaredLength)
              )
          )

  maximumEdgeFaceEligible :: Workspace s -> Int -> ST s Bool
  maximumEdgeFaceEligible Workspace{wsExcluded, wsPermitted} face
    | face <= 0 = pure False
    | otherwise = do
        excluded <- readBoolMark wsExcluded face
        permitted <-
          maybe
            (pure True)
            (\membership -> readBoolMark membership face)
            wsPermitted
        pure (not excluded && permitted)

  markedIndices :: BoolMarks s -> ST s [Int]
  markedIndices marks =
    case marks of
      DenseBoolMarks values -> do
        let indices = [0 .. MUV.length values - 1]
        filterM (MUV.unsafeRead values) indices
      SparseBoolMarks values -> IntSet.toAscList <$> readSTRef values


  offerAll :: FaceQueue s -> ST s ()
  offerAll queue = do
    faces <- faceCount mutable
    forM_ [1 .. faces - 1] (pushFace queue)

  -- A seeded run is not a weaker refinement interpreter. It begins at the
  -- supplied faces and asks only the fixed edges those faces can immediately
  -- encroach; every later cavity and star contributes its own local closure
  -- through the same queue and encroachment machinery as the global entry.
  seedInitialWork :: Workspace s -> RefinementInitialSeed -> ST s ()
  seedInitialWork workspace seed =
    case seed of
      RefineEveryFace -> seedFixedEdges workspace >> offerAll (wsQueue workspace)
      RefineSeededFaces faces ->
        traverse_ (seedFace workspace) (IntSet.toAscList faces)

  seedFace :: Workspace s -> Int -> ST s ()
  seedFace workspace face = do
    pushFace (wsQueue workspace) face
    (e0, e1, e2) <- faceEdges mutable face
    traverse_ (offerFixedEdge workspace) [e0, e1, e2]

  -- Every fixed edge present at entry, queued once for the existing-vertex
  -- encroachment question. Later candidates arrive from the star walks of the
  -- insertions that could have created a new encroachment.
  seedFixedEdges :: Workspace s -> ST s ()
  seedFixedEdges workspace = do
    halfEdges <- directedEdgeCount mutable
    traverse_
      (offerFixedEdge workspace . (2 *))
      [0 .. halfEdges `quot` 2 - 1]

  offerFixedEdge :: Workspace s -> Int -> ST s ()
  offerFixedEdge workspace directed = do
    fixed <- isFixedEdge directed
    let !pair = directed `quot` 2
    -- An interface is a fixed boundary condition, not a quality obligation
    -- owned by this section. It is still encountered by candidate cavities,
    -- where any demand to split or cross it is a typed obstruction.
    when (fixed && not (isInterfacePair pair)) $
      pushEncroachment workspace pair

  isFixedEdge :: Int -> ST s Bool
  isFixedEdge directed = do
    if isInterfacePair (directed `quot` 2)
      then pure True
      else do
        protected <- readConstraint mutable directed
        if protected
          then pure True
          else do
            own <- readFace mutable directed
            if own == 0
              then pure True
              else (== 0) <$> readFace mutable (reverseIndex directed)

  isInterfacePair :: Int -> Bool
  isInterfacePair pair =
    maybe False (IntSet.member pair . refinementDomainInterfacePairs) domain
  {-# INLINE isInterfacePair #-}

  isHullPair :: Int -> ST s Bool
  isHullPair pair = do
    forwardFace <- readFace mutable (2 * pair)
    backwardFace <- readFace mutable (2 * pair + 1)
    pure (forwardFace == 0 || backwardFace == 0)

  interfaceCrossing :: Workspace s -> Int -> ST s BuildError
  interfaceCrossing Workspace{wsPermitted, wsBoundaryCrossingAttempts} pair = do
    modifySTRef' wsBoundaryCrossingAttempts (+ 1)
    forwardFace <- readFace mutable (2 * pair)
    backwardFace <- readFace mutable (2 * pair + 1)
    joinFace <-
      case wsPermitted of
        Nothing -> pure Nothing
        Just permitted -> do
          forwardPermitted <- dynamicPermitted permitted forwardFace
          backwardPermitted <- dynamicPermitted permitted backwardFace
          pure
            ( if forwardPermitted
                then Just forwardFace
                else if backwardPermitted then Just backwardFace else Nothing
            )
    let result =
          case joinFace of
            Just face ->
              RefinementDomainWouldCrossInterface
                (UndirectedEdgeId (fromIntegral pair))
                (FaceId (fromIntegral face))
            Nothing -> RefinementDomainTopologyChanged
    pure result
   where
    dynamicPermitted :: BoolMarks s -> Int -> ST s Bool
    dynamicPermitted membership face =
      if face > 0 then readBoolMark membership face else pure False

  -- The edge bound is a hard geometric law and therefore precedes the minimum
  -- area short-circuit: a long, thin face must get a direct longest-edge split
  -- even when its area is below the configured minimum. The area bound then
  -- condemns outright ('FaceMustRefine'); the ratio bound only invites
  -- refinement, which the input-angle guard may still decline.
  faceHint :: Int -> Int -> Int -> Point -> Point -> Point -> FaceHint
  faceHint e0 e1 e2 p0 p1 p2 =
    case maximumSquaredEdgeLength of
      Just maximumSquaredLength ->
        let (!longestEdge, !longestSquaredLength, !side01, !side12, !side20) =
              faceEdgeMetrics e0 e1 e2 p0 p1 p2
         in if exceeds longestSquaredLength maximumSquaredLength
              then
                FaceMustSplitLongestEdge
                  (longestEdge `quot` 2)
                  longestSquaredLength
              else
                nonEdgeHint
                  ( ratioBad
                      ( triangleRadiusEdgeRatioSquaredWithAreaAndSides
                          area
                          side01
                          side12
                          side20
                      )
                  )
      Nothing ->
        nonEdgeHint
          (ratioBad (triangleRadiusEdgeRatioSquaredWithArea area p0 p1 p2))
   where
    !area = triangleArea p0 p1 p2
    areaBad = maybe False (exceeds area) maximumArea
    belowMinimum = maybe False (area <) minimumArea
    ratioBad :: Double -> Bool
    ratioBad ratio = case squaredRatioBound of
      Nothing -> False
      Just bound -> exceeds ratio bound
    nonEdgeHint angleBad
      | areaBad = FaceMustRefine
      | belowMinimum = FaceAcceptable
      | angleBad = FaceShouldRefine
      | otherwise = FaceAcceptable

  handleFace
    :: Workspace s
    -> Int
    -> ST s (Either BuildError (Int, Maybe (Int, Double)))
  handleFace workspace@Workspace{wsExcluded, wsPermitted, wsVisited} face = do
    addCounter operation CounterRefinementQueuePops 1
    permitted <-
      maybe
        (pure True)
        (\membership -> readBoolMark membership face)
        wsPermitted
    if not permitted
      then pure (Left (RefinementDomainWouldRewriteProtectedFace (FaceId (fromIntegral face))))
      else do
        traverse_ (\visited -> writeBoolMark visited face True) wsVisited
        skip <- readBoolMark wsExcluded face
        if skip
          then pure (Right (0, Nothing))
          else do
            addCounter operation CounterRefinementFaceChecks 1
            (e0, e1, e2) <- faceEdges mutable face
            p0 <- edgeOriginPoint mutable e0
            p1 <- edgeOriginPoint mutable e1
            p2 <- edgeOriginPoint mutable e2
            case faceHint e0 e1 e2 p0 p1 p2 of
              FaceAcceptable -> pure (Right (0, Nothing))
              FaceMustSplitLongestEdge pair actualSquaredLength ->
                qualitySplit workspace pair actualSquaredLength
              FaceMustRefine ->
                fmap
                  (fmap (\gained -> (gained, Nothing)))
                  (attemptCandidate workspace face p0 p1 p2)
              FaceShouldRefine -> do
                blocked <- inputAngleBlocks workspace e0 e1 e2 p0 p1 p2
                if blocked
                  then pure (Right (0, Nothing))
                  else
                    fmap
                      (fmap (\gained -> (gained, Nothing)))
                      (attemptCandidate workspace face p0 p1 p2)

  faceEdgeMetrics
    :: Int
    -> Int
    -> Int
    -> Point
    -> Point
    -> Point
    -> (Int, Double, Double, Double, Double)
  faceEdgeMetrics e0 e1 e2 p0 p1 p2
    | side01 >= side12 && side01 >= side20 = result e0 side01
    | side12 >= side20 = result e1 side12
    | otherwise = result e2 side20
   where
    !side01 = squaredDistanceWide p0 p1
    !side12 = squaredDistanceWide p1 p2
    !side20 = squaredDistanceWide p2 p0
    result :: Int -> Double -> (Int, Double, Double, Double, Double)
    result edge longest = (edge, longest, side01, side12, side20)

  -- An unsplittable quality edge is a pending law obstruction, not permission
  -- to cross a frozen interface and not immediate failure of a resumable run.
  qualitySplit
    :: Workspace s
    -> Int
    -> Double
    -> ST s (Either BuildError (Int, Maybe (Int, Double)))
  qualitySplit workspace pair actualSquaredLength = do
    outcome <- resolveSplit SplitQualityEdge workspace pair
    case outcome of
      Left (RefinementDomainWouldCrossInterface _ _) ->
        pure (Right (0, Just (pair, actualSquaredLength)))
      Left failure -> pure (Left failure)
      Right True -> pure (Right (1, Nothing))
      Right False -> pure (Right (0, Just (pair, actualSquaredLength)))

  -- Two subsegments of one original segment meeting at a small input angle
  -- cannot be refined apart: the angle is fixed by the input geometry. When
  -- the shortest edge of a ratio-bound face joins two Steiner vertices whose
  -- lineages share an original endpoint, the face is as good as it gets.
  inputAngleBlocks :: Workspace s -> Int -> Int -> Int -> Point -> Point -> Point -> ST s Bool
  inputAngleBlocks Workspace{wsSegmentOrigin} e0 e1 e2 p0 p1 p2 = do
    let !shortest = shortestFaceEdge e0 e1 e2 p0 p1 p2
    a <- readOrigin mutable shortest
    b <- readOrigin mutable (reverseIndex shortest)
    shared <- sharedSegmentOrigin wsSegmentOrigin a b
    if not shared
      then pure False
      else not <$> isFixedEdge shortest

  shortestFaceEdge :: Int -> Int -> Int -> Point -> Point -> Point -> Int
  shortestFaceEdge e0 e1 e2 p0 p1 p2
    | side01 <= side12 && side01 <= side20 = e0
    | side12 <= side20 = e1
    | otherwise = e2
   where
    !side01 = squaredDistanceWide p0 p1
    !side12 = squaredDistanceWide p1 p2
    !side20 = squaredDistanceWide p2 p0

  sharedSegmentOrigin :: SegmentOrigins s -> Int -> Int -> ST s Bool
  sharedSegmentOrigin lineage a b = do
    (a0, a1) <- readSegmentOrigin lineage a
    (b0, b1) <- readSegmentOrigin lineage b
    pure (a0 /= noIndex && b0 /= noIndex && (a0 == b0 || a0 == b1 || a1 == b0 || a1 == b1))

  validCandidate :: Point -> Maybe (Point)
  validCandidate point@(Point x y)
    | validateCoordinate x == Nothing && validateCoordinate y == Nothing = Just point
    | otherwise = Nothing

  attemptCandidate :: Workspace s -> Int -> Point -> Point -> Point -> ST s (Either BuildError Int)
  attemptCandidate workspace@Workspace{wsExcluded, wsForcedSplits} face p0 p1 p2 =
    case circumcenter p0 p1 p2 >>= validCandidate of
      Nothing -> pure (Right 0)
      Just point -> do
        located <-
          case domain of
            Nothing -> locateMutable mutable operation (Just face) point
            Just refinementDomain ->
              locateMutableWithin
                (refinementDomainInterfacePairs refinementDomain)
                mutable
                operation
                (Just face)
                point
        case located of
          -- The local locator has proved that the circumcenter lies beyond
          -- Γ without crossing it.  This is the same boundary-limited fixed
          -- point as a circumcenter outside a preserved hull: no admissible
          -- rewrite exists inside this section, so the face is discharged.
          -- Actual cavity demands involving Γ remain typed obstructions in
          -- 'exploreThenCommit'.
          Left (RefinementDomainWouldCrossInterface _ _) -> do
            modifySTRef' (wsInterfaceBoundaryReads workspace) (+ 1)
            pure (Right 0)
          Left failure -> pure (Left failure)
          Right site -> case site of
            MutableOnVertex _ -> pure (Right 0)
            MutableEmpty -> pure (Right 0)
            MutableOutsideHull edge
              | refinePreserveConvexHull parameters -> pure (Right 0)
              | otherwise -> attemptOutside workspace face point edge
            MutableOnEdge edge -> do
              hull <- case domain of
                Nothing -> pure False
                Just _ -> isHullPair (edge `quot` 2)
              if hull
                then pure (Right 0)
                else if isInterfacePair (edge `quot` 2)
                then Left <$> interfaceCrossing workspace (edge `quot` 2)
                else do
                  protected <- readConstraint mutable edge
                  if protected
                    then do
                      -- A circumcenter on a constraint is a request to split that
                      -- constraint, unless constraints are kept whole.
                      unless (refineKeepConstraintEdges parameters) $
                        pushGrowable wsForcedSplits (packIndex (edge `quot` 2))
                      pure (Right 0)
                    else exploreThenCommit workspace face point site
            MutableInFace under -> do
              permittedSite <-
                maybe
                  (pure True)
                  (\membership -> readBoolMark membership under)
                  (wsPermitted workspace)
              if not permittedSite
                then pure (Left (RefinementDomainWouldRewriteProtectedFace (FaceId (fromIntegral under))))
                else do
                  excludedSite <- readBoolMark wsExcluded under
                  if excludedSite
                    then pure (Right 0)
                    else exploreThenCommit workspace face point site

  -- Follow the prospective legalization cavity of the candidate without
  -- mutating topology, recording what the commit will consume: the cavity
  -- faces, the internal edge pairs, and the ordered boundary. Fixed edges are
  -- never crossed: the ones the cavity meets become boundary and are tested
  -- against the candidate's diametral disk. Every encroached fixed edge met
  -- is queued for splitting and the source face is revisited afterwards; a
  -- clean cavity is committed directly.
  exploreThenCommit :: Workspace s -> Int -> Point -> MutableLocation -> ST s (Either BuildError Int)
  exploreThenCommit workspace@Workspace{wsQueue, wsForcedSplits} face point site = do
    (visible, encroached) <- exploreCavity workspace point site
    case find isInterfacePair encroached of
      Just pair
        | all isInterfacePair encroached -> pure (Right 0)
        | otherwise -> Left <$> interfaceCrossing workspace pair
      Nothing
        | null encroached -> commitCavity workspace point site visible
        | otherwise ->
            do
              pushable <- filterM splittablePair encroached
              traverse_ (\pair -> pushGrowable wsForcedSplits (packIndex pair)) pushable
              unless (null pushable) $ pushFace wsQueue face
              pure (Right 0)

  splittablePair :: Int -> ST s Bool
  splittablePair pair = do
    hull <- case domain of
      Nothing -> pure False
      Just _ -> isHullPair pair
    if hull || isInterfacePair pair
      then pure False
      else if refineKeepConstraintEdges parameters
        then not <$> readConstraint mutable (2 * pair)
        else pure True

  admitStar :: Workspace s -> Int -> ST s ()
  admitStar Workspace{wsPermitted} vertex =
    case wsPermitted of
      Just membership ->
        forEachStarFace vertex $ \face -> rememberPermittedFace membership face
      Nothing -> pure ()

  rememberPermittedFace :: BoolMarks s -> Int -> ST s ()
  rememberPermittedFace membership face =
    if face <= 0
      then pure ()
      else do
        already <- readBoolMark membership face
        unless already $ do
          writeBoolMark membership face True

  retirePermittedFace :: Workspace s -> Int -> ST s ()
  retirePermittedFace Workspace{wsPermitted} face = do
    when (face >= 0) $ do
      traverse_
        (\membership -> writeBoolMark membership face False)
        wsPermitted

  retirePermittedFaces :: Workspace s -> GrowableWord32 s -> ST s ()
  retirePermittedFaces workspace faces = do
    count <- growableLength faces
    traverse_
      (\index -> do
         face <- unpackIndex <$> readGrowable faces index
         retirePermittedFace workspace face
      )
      [0 .. count - 1]

  captureCavityFaces :: GrowableWord32 s -> ST s [(Int, Maybe [Point])]
  captureCavityFaces faces = do
    count <- growableLength faces
    traverse
      (\index -> do
         face <- unpackIndex <$> readGrowable faces index
         signature <- mutableFaceSignature face
         pure (face, signature)
      )
      [0 .. count - 1]

  reconcileCavityFaces :: Workspace s -> [(Int, Maybe [Point])] -> ST s ()
  reconcileCavityFaces workspace signatures =
    traverse_
      (\(face, before) -> do
         after <- mutableFaceSignature face
         when (after /= before) (retirePermittedFace workspace face)
      )
      signatures

  mutableFaceSignature :: Int -> ST s (Maybe [Point])
  mutableFaceSignature face = do
    faces <- faceCount mutable
    if face <= 0 || face >= faces
      then pure Nothing
      else do
        (e0, e1, e2) <- faceEdges mutable face
        points <- traverse (edgeOriginPoint mutable) [e0, e1, e2]
        pure (Just (List.sort points))

  captureIncidentFaceSignatures :: [Int] -> ST s [(Int, Maybe [Point])]
  captureIncidentFaceSignatures vertices = do
    faces <- newSTRef IntSet.empty
    traverse_
      (\vertex -> forEachStarFace vertex (modifySTRef' faces . IntSet.insert))
      vertices
    captured <- IntSet.toAscList <$> readSTRef faces
    traverse
      (\face -> do
         signature <- mutableFaceSignature face
         pure (face, signature)
      )
      captured

  -- The flood never mutates, so a popped edge can be retested idempotently:
  -- an edge whose far face conflicts crosses, marks the face, and pushes that
  -- face's other two edges; each directed edge has at most its two
  -- face-neighbours as pushers, and a revisited crossing is deduplicated by
  -- the face and pair stamps. Boundary visibility for the fan is accumulated
  -- in place: every boundary edge is asked here, where its endpoints are
  -- already loaded, whether the point sees it from the cavity side.
  exploreCavity :: Workspace s -> Point -> MutableLocation -> ST s (Bool, [Int])
  exploreCavity workspace@Workspace{wsVisited, wsCavityFaceMarks, wsCavityPairMarks, wsCavityFaces, wsCavityInternal, wsCavityBoundary, wsCavityCocircular, wsCavityWork} point site = do
    next <- nextCavityEpoch workspace
    clearGrowable wsCavityFaces
    clearGrowable wsCavityInternal
    clearGrowable wsCavityBoundary
    clearGrowable wsCavityCocircular
    clearGrowable wsCavityWork
    seedVisible <- case site of
      MutableInFace under -> do
        markFace next under
        (e0, e1, e2) <- faceEdges mutable under
        pushCavityWork e0
        pushCavityWork e1
        pushCavityWork e2
        pure True
      MutableOnEdge edge -> do
        seedAcross next edge
        seedAcross next (reverseIndex edge)
        -- The located edge is never encroachment-tested: a point on it lies
        -- trivially inside its own diametral circle, and the question would
        -- force the same split forever. A fixed edge is boundary; any other
        -- is the cavity's first internal pair. Its visibility still counts.
        fixed <- isFixedEdge edge
        if fixed
          then do
            pushGrowable wsCavityBoundary (packIndex edge)
            from <- edgeOriginPoint mutable edge
            to <- edgeOriginPoint mutable (reverseIndex edge)
            pure (orient2d from to point == GT)
          else do
            recordInternal next (edge `quot` 2)
            pure True
      _ -> pure True
    drain next [] seedVisible
   where
    markFace :: Word32 -> Int -> ST s ()
    markFace epoch face = do
      writeEpochMark wsCavityFaceMarks face epoch
      traverse_ (\visited -> writeBoolMark visited face True) wsVisited
      pushGrowable wsCavityFaces (packIndex face)

    isMarked :: Word32 -> Int -> ST s Bool
    isMarked epoch face = (== epoch) <$> readEpochMark wsCavityFaceMarks face

    recordInternal :: Word32 -> Int -> ST s ()
    recordInternal epoch pair = do
      seen <- readEpochMark wsCavityPairMarks pair
      when (seen /= epoch) $ do
        writeEpochMark wsCavityPairMarks pair epoch
        pushGrowable wsCavityInternal (packIndex pair)

    -- The flood's stack traffic is transaction-local and owns no diagnostics;
    -- the charged wrapper would report every push as legalization depth.
    pushCavityWork :: Int -> ST s ()
    pushCavityWork value = pushGrowable wsCavityWork (packIndex value)

    seedAcross :: Word32 -> Int -> ST s ()
    seedAcross epoch directed = do
      adjacent <- readFace mutable directed
      when (adjacent /= 0) $ do
        markFace epoch adjacent
        next <- readNext mutable directed
        previous <- readPrevious mutable directed
        pushCavityWork next
        pushCavityWork previous

    -- Visibility is read by the caller only when nothing was encroached, so
    -- the first encroached edge retires the question: every later boundary
    -- edge skips the orientation that would only be conjoined into a value
    -- about to be discarded.
    drain :: Word32 -> [Int] -> Bool -> ST s (Bool, [Int])
    drain epoch = go
     where
      go !acc !visible = do
        packed <- popGrowableOr noIndex wsCavityWork
        if packed == noIndex
          then pure (visible, acc)
          else do
            let !directed = fromIntegral packed
            fixed <- isFixedEdge directed
            if fixed
              then do
                pushGrowable wsCavityBoundary (packIndex directed)
                from <- edgeOriginPoint mutable directed
                to <- edgeOriginPoint mutable (reverseIndex directed)
                let pair = directed `quot` 2
                hull <- case domain of
                  Nothing -> pure False
                  Just _ -> isHullPair pair
                let encroached =
                      if isInterfacePair pair || hull
                        then strictlyInDiametralCircle from to point
                        else inDiametralCircle from to point
                when (isInterfacePair pair) $
                  modifySTRef' (wsInterfaceBoundaryReads workspace) (+ 1)
                if encroached
                  then go (pair : acc) False
                  else go acc (visible && orient2d from to point == GT)
              else do
                across <- readFace mutable (reverseIndex directed)
                marked <- isMarked epoch across
                if marked
                  then do
                    recordInternal epoch (directed `quot` 2)
                    go acc visible
                  else do
                    let !twin = reverseIndex directed
                    acrossPrevious <- readPrevious mutable twin
                    opposite <- readOrigin mutable acrossPrevious
                    from <- edgeOriginPoint mutable directed
                    to <- pointAt mutable =<< readOrigin mutable twin
                    acrossPoint <- pointAt mutable opposite
                    let !verdict = inCircle to from acrossPoint point
                    if verdict == GT
                      then do
                        markFace epoch across
                        recordInternal epoch (directed `quot` 2)
                        acrossNext <- readNext mutable twin
                        pushCavityWork acrossNext
                        pushCavityWork acrossPrevious
                        go acc visible
                      else do
                        pushGrowable wsCavityBoundary (packIndex directed)
                        when (verdict == EQ) $
                          pushGrowable wsCavityCocircular (packIndex directed)
                        go acc (visible && orient2d from to point == GT)

  strictlyInDiametralCircle :: Point -> Point -> Point -> Bool
  strictlyInDiametralCircle (Point ax ay) (Point bx by) (Point px py) =
    exactDiametralDot ax ay bx by px py < 0

  -- The flood never mutates, so a popped edge can be retested idempotently:
  -- an edge whose far face conflicts pushes that face's other two edges, and
  -- each directed edge has at most its two face-neighbours as pushers.
  drainSimulation :: GrowableWord32 s -> Point -> [Int] -> ST s [Int]
  drainSimulation work point = go
   where
    go :: [Int] -> ST s [Int]
    go !acc = do
      packed <- popGrowableOr noIndex work
      if packed == noIndex
        then pure acc
        else do
          let !directed = fromIntegral packed
          fixed <- isFixedEdge directed
          if fixed
            then do
              from <- edgeOriginPoint mutable directed
              to <- edgeOriginPoint mutable (reverseIndex directed)
              if inDiametralCircle from to point
                then go (directed `quot` 2 : acc)
                else go acc
            else do
              let !twin = reverseIndex directed
              acrossPrevious <- readPrevious mutable twin
              opposite <- readOrigin mutable acrossPrevious
              from <- edgeOriginPoint mutable directed
              to <- pointAt mutable =<< readOrigin mutable twin
              across <- pointAt mutable opposite
              if inCircle to from across point == GT
                then do
                  acrossNext <- readNext mutable twin
                  pushChargedWork work acrossNext
                  pushChargedWork work acrossPrevious
                  go acc
                else go acc

  -- A cavity the fan commit cannot take: a degenerate or barrier-bent
  -- boundary, a pinched boundary walk, or a duplicate site. The located site
  -- commits through the ordinary split-and-legalize path instead; the
  -- recorded cavity is simply abandoned.
  commitFlipCandidate :: Workspace s -> Point -> MutableLocation -> ST s (Either BuildError Int)
  commitFlipCandidate workspace@Workspace{wsQueue, wsExcluded, wsCavityFaces} point site = do
    let !canonical = canonicalPoint point
    cavitySignatures <- captureCavityFaces wsCavityFaces
    vertex <- appendVertex mutable canonical (makeVertex canonical)
    splitSides <- case site of
      MutableOnEdge edge
        | refineExcludeOuterFaces parameters -> do
            own <- readFace mutable edge
            let !inner = if own == 0 then reverseIndex edge else edge
            leftFace <- readFace mutable inner
            rightFace <- readFace mutable (reverseIndex inner)
            leftExcluded <- readBoolMark wsExcluded leftFace
            rightExcluded <- sideExcluded wsExcluded (reverseIndex inner)
            faceBase <- faceCount mutable
            pure (Just (leftFace, leftExcluded, rightFace, rightExcluded, faceBase))
      _ -> pure Nothing
    inserted <- insertExistingVertexAtLocation @'ProbeOff mutable operation vertex site
    case inserted of
      Left failure -> pure (Left failure)
      Right () -> do
        addCounter operation CounterSteinerPoints 1
        reconcileCavityFaces workspace cavitySignatures
        admitStar workspace vertex
        case splitSides of
          Just (leftFace, leftExcluded, rightFace, rightExcluded, faceBase) ->
            inheritSplitExclusion wsExcluded leftFace leftExcluded rightFace rightExcluded faceBase
          Nothing -> pure ()
        forEachStarFace vertex $ \starFace -> do
          skip <- readBoolMark wsExcluded starFace
          unless skip $ pushFace wsQueue starFace
        pure (Right 1)

  -- Commit a clean cavity by fanning its boundary to the new vertex. The
  -- recorded cavity faces and internal pairs are recycled into fan faces and
  -- spokes; exactly two faces and three edge pairs are appended, whatever the
  -- cavity's size. Any boundary that is not a simple, strictly-visible cycle
  -- falls back to the split-and-legalize commit: that is the degenerate and
  -- the barrier-bent case, never an error.
  commitCavity :: Workspace s -> Point -> MutableLocation -> Bool -> ST s (Either BuildError Int)
  commitCavity workspace@Workspace{wsCavityFaces, wsCavityInternal, wsCavityBoundary} point site visible = do
    boundaryCount <- growableLength wsCavityBoundary
    cavityFaceCount <- growableLength wsCavityFaces
    internalCount <- growableLength wsCavityInternal
    chained <-
      if visible && boundaryCount == cavityFaceCount + 2 && boundaryCount == internalCount + 3
        then chainBoundary workspace boundaryCount
        else pure False
    if chained
      then fanCommit workspace point boundaryCount cavityFaceCount internalCount
      else commitFlipCandidate workspace point site

  -- Order the recorded boundary edges into the cycle they form, starting each
  -- successor by rotating around the current edge's destination. The records
  -- agree with a simple cycle only when the walk closes after exactly the
  -- recorded count and meets no boundary vertex twice; anything else is the
  -- pinched case the fan cannot take.
  chainBoundary :: Workspace s -> Int -> ST s Bool
  chainBoundary Workspace{wsCavityEpoch, wsCavityFaceMarks, wsCavityVertexMarks, wsCavityBoundary, wsCavityChain} count = do
    epoch <- readSTRef wsCavityEpoch
    walkBudget <- directedEdgeCount mutable
    start <- unpackIndex <$> readGrowable wsCavityBoundary 0
    clearGrowable wsCavityChain
    pushGrowable wsCavityChain (packIndex start)
    walk epoch (walkBudget + 2) start start (count - 1)
   where
    walk :: Word32 -> Int -> Int -> Int -> Int -> ST s Bool
    walk epoch walkBudget start current remaining = do
      destination <- readOrigin mutable (reverseIndex current)
      seen <- readEpochMark wsCavityVertexMarks destination
      if seen == epoch
        then pure False
        else do
          writeEpochMark wsCavityVertexMarks destination epoch
          successor <- nextBoundaryEdge epoch walkBudget current
          if remaining == 0
            then pure (successor == start)
            else
              if successor == start
                then pure False
                else do
                  pushGrowable wsCavityChain (packIndex successor)
                  walk epoch walkBudget start successor (remaining - 1)

    nextBoundaryEdge :: Word32 -> Int -> Int -> ST s Int
    nextBoundaryEdge epoch walkBudget edge = do
      first <- readNext mutable edge
      rotate first walkBudget
     where
      rotate :: Int -> Int -> ST s Int
      rotate candidate !remaining
        | remaining <= 0 = pure candidate
        | otherwise = do
            adjacent <- readFace mutable candidate
            markedHere <- (== epoch) <$> readEpochMark wsCavityFaceMarks adjacent
            across <- readFace mutable (reverseIndex candidate)
            markedAcross <- (== epoch) <$> readEpochMark wsCavityFaceMarks across
            if markedHere && not markedAcross
              then pure candidate
              else do
                following <- readNext mutable (reverseIndex candidate)
                rotate following (remaining - 1)

  fanCommit :: Workspace s -> Point -> Int -> Int -> Int -> ST s (Either BuildError Int)
  fanCommit workspace@Workspace{wsQueue, wsExcluded, wsCavityFaces, wsCavityInternal, wsCavityCocircular, wsCavityChain} point boundaryCount cavityFaceCount internalCount = do
    capacity <- ensureCellCapacity mutable (boundaryCount - internalCount) (boundaryCount - cavityFaceCount)
    case capacity of
      Left obstruction -> pure (Left obstruction)
      Right () -> fanCommitWithCapacity
   where
    fanCommitWithCapacity :: ST s (Either BuildError Int)
    fanCommitWithCapacity = do
      let !canonical = canonicalPoint point
      retirePermittedFaces workspace wsCavityFaces
      vertex <- appendVertex mutable canonical (makeVertex canonical)
      faceBase <- addFaceBlock mutable (boundaryCount - cavityFaceCount)
      edgeBase <- addEdgeBlock mutable (boundaryCount - internalCount)
      firstPair <- fanPair edgeBase 0
      let spoke :: Int -> Int -> ST s ()
          spoke !index !pair
            | index >= boundaryCount = pure ()
            | otherwise = do
                boundaryEdge <- unpackIndex <$> readGrowable wsCavityChain index
                face <- fanFace faceBase index
                nextPair <-
                  if index + 1 >= boundaryCount
                    then pure firstPair
                    else fanPair edgeBase (index + 1)
                origin <- readOrigin mutable boundaryEdge
                writeOrigin mutable (2 * pair) origin
                writeOrigin mutable (2 * pair + 1) vertex
                setCycle3 mutable face boundaryEdge (2 * nextPair) (2 * pair + 1)
                writeVertexOut mutable origin boundaryEdge
                -- 'fanFace' and 'fanPair' hand back the cavity's own faces and
                -- interior edges before they hand out new ones. A recycled slot
                -- is given a spoke to the Steiner vertex and a triangle that did
                -- not exist, so whatever the cavity element it displaced was
                -- labelled with does not survive.
                resetEdgeData mutable pair
                resetFaceData mutable face
                spoke (index + 1) nextPair
      spoke 0 firstPair
      markConnected mutable vertex (2 * firstPair + 1)
      addCounter operation CounterSteinerPoints 1
      admitStar workspace vertex
      -- Exactly-cocircular boundary quads are legal ties for the flip
      -- commit but not for a static boundary. Only those edges are drained:
      -- the flood already decided every other boundary edge's quad, so a
      -- full shouldFlip pass would ask again what it already answered.
      cocircular <- growableLength wsCavityCocircular
      when (cocircular > 0) $ do
        ties <- traverse (\index -> unpackIndex <$> readGrowable wsCavityCocircular index) [0 .. cocircular - 1]
        legalizeEdges mutable operation ties
      admitStar workspace vertex
      let offer :: Int -> ST s ()
          offer !index
            | index >= boundaryCount = pure ()
            | otherwise = do
                face <- fanFace faceBase index
                skip <- readBoolMark wsExcluded face
                unless skip $ pushFace wsQueue face
                offer (index + 1)
      offer 0
      pure (Right 1)

    fanFace :: Int -> Int -> ST s Int
    fanFace faceBase index
      | index < cavityFaceCount = unpackIndex <$> readGrowable wsCavityFaces index
      | otherwise = pure (faceBase + (index - cavityFaceCount))

    fanPair :: Int -> Int -> ST s Int
    fanPair edgeBase index
      | index < internalCount = unpackIndex <$> readGrowable wsCavityInternal index
      | otherwise = pure ((edgeBase + 2 * (index - internalCount)) `quot` 2)

  -- A candidate outside the hull (only reachable when the hull is not
  -- preserved) preflights against the whole visible hull chain: the
  -- prospective cavity reaches the hull, so the chain edges are exactly the
  -- fixed edges the insertion would meet.
  attemptOutside :: Workspace s -> Int -> Point -> Int -> ST s (Either BuildError Int)
  attemptOutside workspace@Workspace{wsQueue, wsExcluded, wsForcedSplits, wsCavityWork} face point edge = do
    chain <- visibleChain edge point
    clearGrowable wsCavityWork
    forM_ chain $ \hull -> do
      pushChargedWork wsCavityWork hull
      let !inner = reverseIndex hull
      innerFace <- readFace mutable inner
      when (innerFace /= 0) $ do
        next <- readNext mutable inner
        previous <- readPrevious mutable inner
        pushChargedWork wsCavityWork next
        pushChargedWork wsCavityWork previous
    encroached <- drainSimulation wsCavityWork point []
    if not (null encroached)
      then do
        pushable <- filterM splittablePair encroached
        forM_ pushable $ \pair -> pushGrowable wsForcedSplits (packIndex pair)
        unless (null pushable) $ pushFace wsQueue face
        pure (Right 0)
      else do
        let !canonical = canonicalPoint point
        vertex <- appendVertex mutable canonical (makeVertex canonical)
        inserted <- insertExistingVertexAtLocation @'ProbeOff mutable operation vertex (MutableOutsideHull edge)
        case inserted of
          Left failure -> pure (Left failure)
          Right () -> do
            addCounter operation CounterSteinerPoints 1
            admitStar workspace vertex
            when (refineExcludeOuterFaces parameters) $ do
              seedCount <- newSTRef 0
              forEachStarFace vertex $ \starFace -> do
                writeBoolMark wsExcluded starFace True
                count <- readSTRef seedCount
                writeScratch operation count starFace
                writeSTRef seedCount (count + 1)
              count <- readSTRef seedCount
              propagateExcluded wsExcluded count
            forEachStarFace vertex $ \starFace -> do
              skip <- readBoolMark wsExcluded starFace
              unless skip $ pushFace wsQueue starFace
            forEachStarFace vertex $ \starFace -> do
              (e0, e1, e2) <- faceEdges mutable starFace
              traverse_ (offerFixedEdge workspace) [e0, e1, e2]
            pure (Right 1)

  -- The maximal run of outer-cycle edges visible from the candidate, centred
  -- on the edge location reported. Bounded by the hull length.
  visibleChain :: Int -> Point -> ST s [Int]
  visibleChain edge point = do
    halfEdges <- directedEdgeCount mutable
    left <- expand (readPrevious mutable) (halfEdges + 1) edge
    right <- expand (readNext mutable) (halfEdges + 1) edge
    walk left right (halfEdges + 1) []
   where
    expand :: (Int -> ST s Int) -> Int -> Int -> ST s Int
    expand step !walkBudget !current
      | walkBudget <= 0 = pure current
      | otherwise = do
          candidate <- step current
          if candidate == edge
            then pure current
            else do
              from <- edgeOriginPoint mutable candidate
              to <- edgeOriginPoint mutable (reverseIndex candidate)
              if orient2d from to point == GT
                then expand step (walkBudget - 1) candidate
                else pure current

    walk :: Int -> Int -> Int -> [Int] -> ST s [Int]
    walk !current !end !walkBudget !acc
      | walkBudget <= 0 = pure acc
      | current == end = pure (current : acc)
      | otherwise = do
          following <- readNext mutable current
          walk following end (walkBudget - 1) (current : acc)

  -- New faces outside the hull sit at barrier depth zero; whatever they now
  -- reach without crossing a constraint joined the outer region with them.
  propagateExcluded :: BoolMarks s -> Int -> ST s ()
  propagateExcluded excluded = drain
   where
    drain :: Int -> ST s ()
    drain !count
      | count <= 0 = pure ()
      | otherwise = do
          face <- readScratch operation (count - 1)
          (e0, e1, e2) <- faceEdges mutable face
          next <- spread (count - 1) e0 >>= (`spread` e1) >>= (`spread` e2)
          drain next

    spread :: Int -> Int -> ST s Int
    spread !count !directed = do
      protected <- readConstraint mutable directed
      if protected
        then pure count
        else do
          adjacent <- readFace mutable (reverseIndex directed)
          if adjacent == 0
            then pure count
            else do
              already <- readBoolMark excluded adjacent
              if already
                then pure count
                else do
                  writeBoolMark excluded adjacent True
                  writeScratch operation count adjacent
                  pure (count + 1)

  -- A queued fixed edge against the vertices currently opposite it, from each
  -- non-excluded side. A stale candidate simply answers for the subsegment
  -- its handle now names, which is the edge the queue cares about.
  checkEncroachment :: Workspace s -> Int -> ST s (Either BuildError Bool)
  checkEncroachment workspace@Workspace{wsExcluded} pair = do
    let !directed = 2 * pair
    first <- sideEncroaches wsExcluded directed
    encroached <-
      if first
        then pure True
        else sideEncroaches wsExcluded (reverseIndex directed)
    if encroached
      then
        if isInterfacePair pair
          then Left <$> interfaceCrossing workspace pair
          else resolveSplit SplitFixedSegment workspace pair
      else pure (Right False)
   where
    sideEncroaches :: BoolMarks s -> Int -> ST s Bool
    sideEncroaches excluded side = do
      adjacent <- readFace mutable side
      if adjacent == 0
        then pure False
        else do
          skip <- readBoolMark excluded adjacent
          if skip
            then pure False
            else do
              opposite <- readOrigin mutable =<< readPrevious mutable side
              query <- pointAt mutable opposite
              from <- edgeOriginPoint mutable side
              to <- edgeOriginPoint mutable (reverseIndex side)
              hull <- case domain of
                Nothing -> pure False
                Just _ -> isHullPair pair
              pure
                ( if isInterfacePair pair || hull
                    then strictlyInDiametralCircle from to query
                    else inDiametralCircle from to query
                )

  -- Split a fixed edge. A first split lands at the midpoint; a subsegment
  -- split rounds its offset to the nearest power of two toward the original
  -- endpoint, so segments meeting at a small input angle stop encroaching
  -- each other instead of subdividing forever.
  resolveSplit :: EdgeSplitLaw -> Workspace s -> Int -> ST s (Either BuildError Bool)
  resolveSplit splitLaw workspace@Workspace{wsExcluded, wsSegmentOrigin} pair = do
    let !directed = 2 * pair
    protected <- readConstraint mutable directed
    hull <- case domain of
      Nothing -> pure False
      Just _ -> isHullPair pair
    if hull
      then pure (Right False)
      else if isInterfacePair pair
      then Left <$> interfaceCrossing workspace pair
      else if refineKeepConstraintEdges parameters && protected
      then pure (Right False)
      else do
        v0 <- readOrigin mutable directed
        v1 <- readOrigin mutable (reverseIndex directed)
        from <- pointAt mutable v0
        to <- pointAt mutable v1
        segmentLineage <-
          case splitLaw of
            SplitQualityEdge -> pure Nothing
            SplitFixedSegment -> do
              (lineage0, second0) <- readSegmentOrigin wsSegmentOrigin v0
              (lineage1, second1) <- readSegmentOrigin wsSegmentOrigin v1
              pure (Just (lineage0, second0, lineage1, second1))
        let (!onSegment0, !onSegment1) =
              case segmentLineage of
                Nothing -> (False, False)
                Just (lineage0, _, lineage1, _) ->
                  (lineage0 /= noIndex, lineage1 /= noIndex)
            !splitPoint = splitPosition onSegment0 onSegment1 from to
        incidentSignatures <- captureIncidentFaceSignatures [v0, v1]
        valid <- validateSplitPosition directed splitPoint
        if not valid
          then pure (Right False)
          else do
            -- Inheritance is read as structural face identities, never as
            -- post-split handle membership: the split's own legalization can
            -- flip seeded edges and relocate handles before any read, but it
            -- cannot rename a face. The kept side faces and the append base
            -- name the sides exactly.
            own <- readFace mutable directed
            let !inner = if own == 0 then reverseIndex directed else directed
            leftFace <- readFace mutable inner
            rightFace <- readFace mutable (reverseIndex inner)
            leftExcluded <- readBoolMark wsExcluded leftFace
            rightExcluded <- sideExcluded wsExcluded (reverseIndex inner)
            faceBase <- faceCount mutable
            let !canonical = canonicalPoint splitPoint
            retirePermittedFace workspace leftFace
            retirePermittedFace workspace rightFace
            vertex <- appendVertex mutable canonical (makeVertex canonical)
            inserted <- insertOnEdge @'ProbeOff mutable operation directed vertex
            case inserted of
              Left obstruction -> pure (Left obstruction)
              Right () -> do
                reconcileCavityFaces workspace incidentSignatures
                admitStar workspace vertex
                case segmentLineage of
                  Nothing -> pure ()
                  Just (lineage0, second0, lineage1, second1) ->
                    if onSegment0
                      then writeSegmentOrigin wsSegmentOrigin vertex lineage0 second0
                      else
                        if onSegment1
                          then writeSegmentOrigin wsSegmentOrigin vertex lineage1 second1
                          else writeSegmentOrigin wsSegmentOrigin vertex (packIndex v0) (packIndex v1)
                when (refineExcludeOuterFaces parameters) $
                  inheritSplitExclusion wsExcluded leftFace leftExcluded rightFace rightExcluded faceBase
                addCounter operation CounterSteinerPoints 1
                pushStarAfterSplit workspace vertex
                pure (Right True)

  splitPosition :: Bool -> Bool -> Point -> Point -> Point
  splitPosition onSegment0 onSegment1 from to
    | not onSegment0 && not onSegment1 = midpoint from to
    | otherwise =
        let !halfLength = sqrt (squaredDistance from to) * 0.5
         in if not (isFinite halfLength) || halfLength <= 0
              then midpoint from to
              else
                let !nearest = 2 ** fromIntegral (round (logBase 2 halfLength) :: Int)
                    !otherWeight = 0.5 * nearest / halfLength
                    !originalWeight = 1 - otherWeight
                    (!weight0, !weight1) =
                      if onSegment0
                        then (otherWeight, originalWeight)
                        else (originalWeight, otherWeight)
                    Point fromX fromY = from
                    Point toX toY = to
                 in Point (fromX * weight0 + toX * weight1) (fromY * weight0 + toY * weight1)

  -- The split is refused when the constructed vertex would leave one of the
  -- four new faces degenerate or clockwise — for an in-segment point that is
  -- exactly the coincidence-with-an-endpoint-or-opposite case.
  validateSplitPosition :: Int -> Point -> ST s Bool
  validateSplitPosition directed splitPoint@(Point splitX splitY)
    | validateCoordinate splitX /= Nothing = pure False
    | validateCoordinate splitY /= Nothing = pure False
    | otherwise = do
        from <- edgeOriginPoint mutable directed
        to <- edgeOriginPoint mutable (reverseIndex directed)
        first <- sideKeepsOrientation directed from to
        if not first
          then pure False
          else sideKeepsOrientation (reverseIndex directed) to from
   where
    sideKeepsOrientation :: Int -> Point -> Point -> ST s Bool
    sideKeepsOrientation side sideFrom sideTo = do
      adjacent <- readFace mutable side
      if adjacent == 0
        then pure True
        else do
          opposite <- readOrigin mutable =<< readPrevious mutable side
          oppositePoint <- pointAt mutable opposite
          pure
            ( orient2d sideFrom oppositePoint splitPoint == LT
                && orient2d oppositePoint sideTo splitPoint == LT
            )

  sideExcluded :: BoolMarks s -> Int -> ST s Bool
  sideExcluded excluded side = do
    adjacent <- readFace mutable side
    if adjacent == 0 then pure True else readBoolMark excluded adjacent

  -- A split keeps each side's classification. The faces a side holds
  -- afterwards are the side face it kept and the face appended over it:
  -- interior splits append two, boundary splits one, and both ids are
  -- immune to the legalization that runs between the geometry and here.
  inheritSplitExclusion :: BoolMarks s -> Int -> Bool -> Int -> Bool -> Int -> ST s ()
  inheritSplitExclusion excluded leftFace leftExcluded rightFace rightExcluded faceBase = do
    writeSide leftExcluded leftFace
    writeSide leftExcluded faceBase
    when (rightFace /= 0) $ do
      writeSide rightExcluded rightFace
      writeSide rightExcluded (faceBase + 1)
   where
    writeSide :: Bool -> Int -> ST s ()
    writeSide status side = do
      when (side /= 0) $ writeBoolMark excluded side status

  pushStarAfterSplit :: Workspace s -> Int -> ST s ()
  pushStarAfterSplit workspace@Workspace{wsQueue} vertex =
    forEachStarFace vertex $ \starFace -> do
      pushFace wsQueue starFace
      (e0, e1, e2) <- faceEdges mutable starFace
      traverse_ (offerFixedEdge workspace) [e0, e1, e2]

  -- Every interior face incident to a vertex, once. One rotation direction
  -- covers a full interior star; a hull star additionally needs the other
  -- direction from the starting edge.
  forEachStarFace :: Int -> (Int -> ST s ()) -> ST s ()
  forEachStarFace vertex visit = do
    start <- readVertexOut mutable vertex
    when (start >= 0) $ do
      wrapped <- rotate start start
      unless wrapped (rotateBack start)
   where
    rotate :: Int -> Int -> ST s Bool
    rotate !start !edge = do
      adjacent <- readFace mutable edge
      if adjacent == 0
        then pure False
        else do
          visit adjacent
          next <- reverseIndex <$> readPrevious mutable edge
          if next == start then pure True else rotate start next

    rotateBack :: Int -> ST s ()
    rotateBack edge = do
      let !twin = reverseIndex edge
      adjacent <- readFace mutable twin
      when (adjacent /= 0) $ do
        visit adjacent
        next <- readNext mutable twin
        rotateBack next

-- | Whether a measurement is over its bound. A non-positive bound admits
-- nothing, which is what dividing by it used to say by returning an infinity
-- that then compared greater than one.
exceeds :: Double -> Double -> Bool
exceeds value bound = bound <= 0 || value > bound
{-# INLINE exceeds #-}

-- | The bound the squared radius-edge ratio is compared against. Squaring is
-- monotone on the non-negative reals, so this asks the same question of the
-- squares that the unsquared bound asked of the lengths.
squareBound :: Double -> Double
squareBound bound
  | bound <= 0 = bound
  | otherwise = bound * bound
{-# INLINE squareBound #-}

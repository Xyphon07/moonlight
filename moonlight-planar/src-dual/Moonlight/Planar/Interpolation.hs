{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Nearest, barycentric, and natural-neighbor interpolation over one
-- authoritative triangulation.
module Moonlight.Planar.Interpolation
  ( BarycentricWeights (..)
  , InterpolationStats (..)
  , NaturalNeighborResult (..)
  , NaturalNeighborWorkspace
  , newNaturalNeighborWorkspace
  , workspaceBytes
  , nearestNeighbor
  , barycentricWeights
  , naturalNeighborWeights
  , foldNaturalNeighborWeights
  , interpolateNearest
  , interpolateBarycentric
  , interpolateNaturalNeighbor
  , estimateGradient
  , estimateGradients
  , interpolateNaturalNeighborGradient
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.ST (ST)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)
import Moonlight.Planar.Dcel
  ( destination
  , counterClockwise
  , incidentFace
  , innerFaceDirectedEdges
  , innerFaceVertices
  , isBoundaryEdge
  , next
  , numConstraints
  , numDirectedEdges
  , numVertices
  , origin
  , outerFace
  , vertexOutEdge
  , vertexOutgoingEdges
  , vertexPoint
  )
import Moonlight.Planar.Handles.Iterators.FixedIterators (foldVertices')
import Moonlight.Planar.Internal.HandleDefs
  ( DirectedEdgeId (..)
  , asUndirected
  , FaceId (..)
  , faceIdIndex
  , VertexId (..)
  , reverseEdge
  , vertexIdIndex
  )
import Moonlight.Planar.Internal.InterpolationWorkspace
import Moonlight.Planar.Internal.Predicates (compareSquaredDistances, inCircle)
import Moonlight.Planar.Math (barycentricCoordinates, circumcenter, projectionFactor)
import Moonlight.Planar.Scalar (isFinite)
import Moonlight.Planar.PointLocation (locatePointWithHint)
import Moonlight.Planar.Scalar (scalarEpsilon)
import Moonlight.Planar.Types (Location (..), LocationHint, LocationStats, NearestStats (..), Triangulation)
import Moonlight.Planar.Point (Point (..), QueryPoint, queryPointValue)
import GHC.Generics (Generic)

-- | Barycentric weights use the mesh's binary64 coordinate domain.
data BarycentricWeights
  = NoWeights
  | OneWeight !VertexId
  | TwoWeights !VertexId !Double !VertexId !Double
  | ThreeWeights !VertexId !Double !VertexId !Double !VertexId !Double
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Work performed by one natural-neighbor query.
data InterpolationStats = InterpolationStats
  { interpolationCavityFaces :: {-# UNPACK #-} !Int
    -- ^ Faces in the query insertion cavity.
  , interpolationNaturalNeighbors :: {-# UNPACK #-} !Int
    -- ^ Sites contributing nonzero Sibson weight.
  , interpolationFaceTests :: {-# UNPACK #-} !Int
    -- ^ Faces tested while discovering the cavity.
    -- | 'True' exactly when the Sibson pipeline declined and the returned
    -- weights are the barycentric coordinates of the located face instead.
    -- A silent degradation from Sibson to barycentric is a defect that hides;
    -- this bit is the announcement.
  , interpolationUsedFallback :: !Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | Weights and receipts produced by one natural-neighbor query.
data NaturalNeighborResult = NaturalNeighborResult
  { naturalNeighborValues :: !(V.Vector (VertexId, Double))
    -- ^ Nonzero weights keyed by source vertex.
  , naturalNeighborLocationStats :: !LocationStats
    -- ^ Work performed while locating the query.
  , naturalNeighborStats :: !InterpolationStats
    -- ^ Work performed while constructing the Sibson coordinates.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Minimum resident vertex by exact squared distance, then by resident handle,
-- using an optional admitted vertex as the descent seed. Empty triangulations
-- have no nearest vertex. Handle ties are deterministic within this mesh, not
-- an identity invariant across differently constructed meshes.
nearestNeighbor
  :: Triangulation mode vertex directed undirected face
  -> Maybe VertexId
  -> QueryPoint
  -> Maybe (VertexId, NearestStats)
nearestNeighbor triangulation hint queryPoint
  | numVertices triangulation == 0 = Nothing
  | numConstraints triangulation == 0 = Just (descend start 0 0)
  | otherwise = Just (completeArgmin start 0 0)
 where
  !query = queryPointValue queryPoint
  !start = validateHint hint

  validateHint (Just vertex)
    | vertexIdIndex vertex < numVertices triangulation = vertex
  validateHint _ = VertexId 0

  compareDistance firstSite secondSite =
    compareSquaredDistances
      query
      (vertexPoint triangulation firstSite)
      (vertexPoint triangulation secondSite)

  -- A Delaunay Voronoi cell is the intersection of its neighbour half-planes.
  -- Outside it, some neighbour is strictly closer; the first such witness is
  -- sufficient. Exact strict improvement is well-founded on the finite site
  -- inventory, so no arbitrary walk budget can publish an unproved incumbent.
  descend !current !steps !tests =
    foldr
      (inspectStar current steps)
      (settle current steps)
      (vertexOutgoingEdges triangulation current)
      False
      tests

  inspectStar current steps edge remaining tied !tests =
    let !candidate = destination triangulation edge
        !nextTests = tests + 1
     in case compareDistance candidate current of
          LT -> descend candidate (steps + 1) nextTests
          EQ -> remaining True nextTests
          GT -> remaining tied nextTests

  -- A local lower-handle tie is not the global minimum: a cocircular plateau
  -- can hide it behind larger handles. Resolve ties by the complete inventory,
  -- which is also the required schedule when constraints break the Voronoi
  -- neighbour argument.
  settle current steps tied tests
    | tied = completeArgmin current steps tests
    | otherwise = (current, NearestStats steps tests)

  completeArgmin seed steps tests =
    ( foldVertices' triangulation (chooseNearest seed) seed
    , NearestStats steps (tests + numVertices triangulation - 1)
    )

  chooseNearest seed !best candidate
    | candidate == seed = best
    | otherwise =
        case compareDistance candidate best of
          LT -> candidate
          EQ -> min candidate best
          GT -> best

-- | Barycentric weights at a validated query, plus point-location work.
barycentricWeights
  :: Triangulation mode vertex directed undirected face
  -> Maybe LocationHint
  -> QueryPoint
  -> (BarycentricWeights, LocationStats)
barycentricWeights triangulation hint queryPoint =
  let !query = queryPointValue queryPoint
      (!location, !stats) = locatePointWithHint triangulation hint queryPoint
   in (weightsFor query location, stats)
 where
  weightsFor _ (OnVertex vertex) = OneWeight vertex
  weightsFor query (OnEdge edge) =
    let !factor = edgeDestinationWeight triangulation edge query
     in TwoWeights (origin triangulation edge) (1 - factor) (destination triangulation edge) factor
  weightsFor query (InFace face) =
    case innerFaceVertices triangulation face of
      Nothing -> NoWeights
      Just (a, b, c) ->
        case barycentricCoordinates
          (vertexPoint triangulation a)
          (vertexPoint triangulation b)
          (vertexPoint triangulation c)
          query of
          Nothing -> NoWeights
          Just (wa, wb, wc) -> ThreeWeights a wa b wb c wc
  weightsFor _ EmptyTriangulation = NoWeights
  weightsFor _ (OutsideConvexHull _) = NoWeights

-- | Calculate Sibson coordinates using fixed-capacity reusable scratch storage.
-- The only per-query heap object is the returned boxed vector.
naturalNeighborWeights
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Maybe LocationHint
  -> QueryPoint
  -> ST s (NaturalNeighborResult)
naturalNeighborWeights workspace hint queryPoint = do
  (!count, !locationStats, !stats) <- queryNaturalNeighborWorkspace workspace hint queryPoint
  values <- V.generateM count $ \index -> do
    rawVertex <- MUV.unsafeRead (nnWeightVertex workspace) index
    weight <- MUV.unsafeRead (nnWeightValue workspace) index
    pure (VertexId rawVertex, weight)
  pure NaturalNeighborResult
    { naturalNeighborValues = values
    , naturalNeighborLocationStats = locationStats
    , naturalNeighborStats = stats
    }

-- | Strictly fold the Sibson coordinates held in a reusable workspace. Unlike
-- 'naturalNeighborWeights', this does not allocate a result vector. It is the
-- canonical path for repeated interpolation and other reductions.
foldNaturalNeighborWeights
  :: (accumulator -> VertexId -> Double -> accumulator)
  -> accumulator
  -> NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Maybe LocationHint
  -> QueryPoint
  -> ST s (accumulator, LocationStats, InterpolationStats)
foldNaturalNeighborWeights combine initial workspace hint query = do
  (!count, !locationStats, !stats) <- queryNaturalNeighborWorkspace workspace hint query
  value <- go 0 initial count
  pure (value, locationStats, stats)
 where
  go !index !accumulator !count
    | index >= count = pure accumulator
    | otherwise = do
        rawVertex <- MUV.unsafeRead (nnWeightVertex workspace) index
        weight <- MUV.unsafeRead (nnWeightValue workspace) index
        let !nextAccumulator = combine accumulator (VertexId rawVertex) weight
        go (index + 1) nextAccumulator count

queryNaturalNeighborWorkspace
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Maybe LocationHint
  -> QueryPoint
  -> ST s (Int, LocationStats, InterpolationStats)
queryNaturalNeighborWorkspace workspace hint queryPoint = do
  let !triangulation = nnTriangulation workspace
      !query = queryPointValue queryPoint
      (!location, !locationStats) = locatePointWithHint triangulation hint queryPoint
  (!count, !stats) <- case location of
    OnVertex vertex -> do
      writeWeight workspace 0 vertex 1
      pure (1, InterpolationStats 0 1 0 False)
    OnEdge edge
      | isBoundaryEdge triangulation (asUndirected edge) -> do
          let !factor = edgeDestinationWeight triangulation edge query
          writeWeight workspace 0 (origin triangulation edge) (1 - factor)
          writeWeight workspace 1 (destination triangulation edge) factor
          pure (2, InterpolationStats 0 2 0 False)
      | otherwise ->
          let !left = incidentFace triangulation edge
              !right = incidentFace triangulation (reverseEdge edge)
              !start = if left /= outerFace then left else right
           in sibsonQuery workspace query start
    InFace face -> sibsonQuery workspace query face
    EmptyTriangulation -> pure (0, InterpolationStats 0 0 0 False)
    OutsideConvexHull _ -> pure (0, InterpolationStats 0 0 0 False)
  pure (count, locationStats, stats)

sibsonQuery
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Point
  -> FaceId
  -> ST s (Int, InterpolationStats)
sibsonQuery workspace query startFace = do
  generation <- nextFaceGeneration workspace
  (cavityCount, faceTests) <- discoverCavity workspace generation query startFace
  boundaryCount <- collectBoundary workspace generation cavityCount
  orderedCount <- orderBoundary workspace boundaryCount
  if orderedCount < 3
    then fallbackBarycentric workspace query startFace faceTests cavityCount
    else do
      cellOkay <- buildInsertionCell workspace query orderedCount
      if not cellOkay
        then fallbackBarycentric workspace query startFace faceTests cavityCount
        else do
          weightCount <- buildStolenAreas workspace generation query orderedCount
          normalized <- normalizeWeights workspace weightCount
          if normalized
            then pure (weightCount, InterpolationStats cavityCount weightCount faceTests False)
            else fallbackBarycentric workspace query startFace faceTests cavityCount

-- The query cavity is exactly the set of faces whose circumcircles contain the
-- inserted point. Generation marks make clearing O(1).
discoverCavity
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Word32
  -> Point
  -> FaceId
  -> ST s (Int, Int)
discoverCavity workspace generation query startFace@(FaceId rawStart) = do
  MUV.unsafeWrite (nnFaceSeenMarks workspace) (faceIdIndex startFace) generation
  MUV.unsafeWrite (nnFaceQueue workspace) 0 rawStart
  go 1 0 0
 where
  !triangulation = nnTriangulation workspace
  go !queueSize !cavitySize !tests
    | queueSize <= 0 = pure (cavitySize, tests)
    | otherwise = do
        let !slot = queueSize - 1
        rawFace <- MUV.unsafeRead (nnFaceQueue workspace) slot
        let !face = FaceId rawFace
            !inside = containsQuery triangulation query face
            !tests' = tests + 1
        if not inside
          then go slot cavitySize tests'
          else do
            MUV.unsafeWrite (nnFaceMarks workspace) (faceIdIndex face) generation
            MUV.unsafeWrite (nnCavityFaces workspace) cavitySize rawFace
            nextQueue <- pushNeighbors triangulation workspace generation slot face
            go nextQueue (cavitySize + 1) tests'

containsQuery
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> FaceId
  -> Bool
containsQuery triangulation query face =
  case innerFaceVertices triangulation face of
    Nothing -> False
    Just (a, b, c) ->
      -- Strictly inside, matching spade's contained_in_circumference: an
      -- exactly cocircular face is NOT part of the cavity, so its opposite
      -- vertex is not a natural neighbour. A '/= LT' reading would include
      -- such a vertex with a zero stolen area and diverge from spade's
      -- neighbour set on cocircular queries.
      inCircle
        (vertexPoint triangulation a)
        (vertexPoint triangulation b)
        (vertexPoint triangulation c)
        query
        == GT

pushNeighbors
  :: Triangulation mode vertex directed undirected face
  -> NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Word32
  -> Int
  -> FaceId
  -> ST s Int
pushNeighbors triangulation workspace generation start face =
  case innerFaceDirectedEdges triangulation face of
    Nothing -> pure start
    Just (e0, e1, e2) -> do
      size1 <- pushOne start e0
      size2 <- pushOne size1 e1
      pushOne size2 e2
 where
  pushOne !size edge =
    let !adjacent@(FaceId raw) = incidentFace triangulation (reverseEdge edge)
        !index = faceIdIndex adjacent
     in if adjacent == outerFace
          then pure size
          else do
            seen <- MUV.unsafeRead (nnFaceSeenMarks workspace) index
            if seen == generation
              then pure size
              else do
                MUV.unsafeWrite (nnFaceSeenMarks workspace) index generation
                MUV.unsafeWrite (nnFaceQueue workspace) size raw
                pure (size + 1)

collectBoundary
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Word32
  -> Int
  -> ST s Int
collectBoundary workspace generation cavityCount = goFaces 0 0
 where
  !triangulation = nnTriangulation workspace
  goFaces !index !boundarySize
    | index >= cavityCount = pure boundarySize
    | otherwise = do
        rawFace <- MUV.unsafeRead (nnCavityFaces workspace) index
        nextSize <- case innerFaceDirectedEdges triangulation (FaceId rawFace) of
          Nothing -> pure boundarySize
          Just (e0, e1, e2) -> do
            size1 <- appendIfBoundary boundarySize e0
            size2 <- appendIfBoundary size1 e1
            appendIfBoundary size2 e2
        goFaces (index + 1) nextSize
  appendIfBoundary !size edge = do
    let adjacent = incidentFace triangulation (reverseEdge edge)
    outside <- if adjacent == outerFace
      then pure True
      else (/= generation) <$> MUV.unsafeRead (nnFaceMarks workspace) (faceIdIndex adjacent)
    if outside
      then case edge of
        DirectedEdgeId raw -> MUV.unsafeWrite (nnBoundaryEdges workspace) size raw >> pure (size + 1)
      else pure size

orderBoundary
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Int
  -> ST s Int
orderBoundary _ 0 = pure 0
orderBoundary workspace count = do
  generation <- nextOriginGeneration workspace
  install generation 0
  firstRaw <- MUV.unsafeRead (nnBoundaryEdges workspace) 0
  follow generation firstRaw firstRaw 0
 where
  !triangulation = nnTriangulation workspace
  install !generation !index
    | index >= count = pure ()
    | otherwise = do
        rawEdge <- MUV.unsafeRead (nnBoundaryEdges workspace) index
        let !from = vertexIdIndex (origin triangulation (DirectedEdgeId rawEdge))
        MUV.unsafeWrite (nnOriginMarks workspace) from generation
        MUV.unsafeWrite (nnOriginEdge workspace) from rawEdge
        install generation (index + 1)
  follow !generation !firstRaw !currentRaw !index
    | index >= count =
        if currentRaw == firstRaw then pure count else pure 0
    | otherwise = do
        MUV.unsafeWrite (nnOrderedEdges workspace) index currentRaw
        let !to = vertexIdIndex (destination triangulation (DirectedEdgeId currentRaw))
        marked <- MUV.unsafeRead (nnOriginMarks workspace) to
        if marked /= generation
          then pure 0
          else do
            nextRaw <- MUV.unsafeRead (nnOriginEdge workspace) to
            if nextRaw == firstRaw && index + 1 == count
              then pure count
              else if nextRaw == firstRaw
                then pure 0
                else follow generation firstRaw nextRaw (index + 1)

buildInsertionCell
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Point
  -> Int
  -> ST s Bool
buildInsertionCell workspace query count = go 0
 where
  !triangulation = nnTriangulation workspace
  go !index
    | index >= count = pure True
    | otherwise = do
        rawEdge <- MUV.unsafeRead (nnOrderedEdges workspace) index
        let !edge = DirectedEdgeId rawEdge
            !from = vertexPoint triangulation (origin triangulation edge)
            !to = vertexPoint triangulation (destination triangulation edge)
        case circumcenter (subtractPoint to query) (subtractPoint from query) (Point 0 0) of
          Nothing -> pure False
          Just (Point x y)
            | isFinite x && isFinite y -> do
                MUV.unsafeWrite (nnInsertionX workspace) index x
                MUV.unsafeWrite (nnInsertionY workspace) index y
                go (index + 1)
            | otherwise -> pure False

buildStolenAreas
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Word32
  -> Point
  -> Int
  -> ST s Int
buildStolenAreas workspace generation query count = do
  lastRaw <- MUV.unsafeRead (nnOrderedEdges workspace) (count - 1)
  lastX <- MUV.unsafeRead (nnInsertionX workspace) (count - 1)
  lastY <- MUV.unsafeRead (nnInsertionY workspace) (count - 1)
  go 0 (DirectedEdgeId lastRaw) (Point lastX lastY)
 where
  !triangulation = nnTriangulation workspace
  go !index !lastEdge !lastPoint
    | index >= count = pure count
    | otherwise = do
        rawStop <- MUV.unsafeRead (nnOrderedEdges workspace) index
        firstX <- MUV.unsafeRead (nnInsertionX workspace) index
        firstY <- MUV.unsafeRead (nnInsertionY workspace) index
        let !stopEdge = DirectedEdgeId rawStop
            !first = Point firstX firstY
        area <- stolenArea workspace generation query stopEdge first lastEdge lastPoint
        case area of
          Nothing -> pure 0
          Just polygonArea -> do
            writeWeight workspace index (origin triangulation stopEdge) polygonArea
            go (index + 1) stopEdge first

stolenArea
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Word32
  -> Point
  -> DirectedEdgeId
  -> Point
  -> DirectedEdgeId
  -> Point
  -> ST s (Maybe Double)
stolenArea workspace generation query stopEdge first initialEdge initialPoint =
  walk initialEdge initialPoint initialPositive initialNegative 0
 where
  !triangulation = nnTriangulation workspace
  -- The boundary loop runs counterclockwise around the cavity, so the fan
  -- walk around each boundary vertex necessarily runs clockwise: the
  -- shoelace sequence first, lastPoint, circumcenters... is the clockwise
  -- traversal of the stolen polygon, and positive - negative is the
  -- NEGATED twice-area. The result is negated here so the returned value is
  -- the positive twice-area 'normalizeWeights' requires. (spade's identical
  -- walk leaves the sum negative and cancels the sign in its total; its
  -- "ordered ccw" comment is wrong.)
  !initialPositive = pointX first * pointY initialPoint
  !initialNegative = pointY first * pointX initialPoint
  !target = reverseEdge stopEdge
  !limit = numDirectedEdges triangulation + 1
  walk !lastEdge !lastPoint !positive !negative !steps
    | steps >= limit = pure Nothing
    | face == outerFace = pure Nothing
    | otherwise = do
        center <- cachedFaceCircumcenter workspace generation query face
        case center of
          Nothing -> pure Nothing
          Just current ->
            let !positive' = positive + pointX lastPoint * pointY current
                !negative' = negative + pointY lastPoint * pointX current
                !nextEdge = reverseEdge (next triangulation lastEdge)
             in if nextEdge == target
                  then
                    let !closedPositive = positive' + pointX current * pointY first
                        !closedNegative = negative' + pointY current * pointX first
                     in pure (Just (closedNegative - closedPositive))
                  else walk nextEdge current positive' negative' (steps + 1)
   where
    face = incidentFace triangulation lastEdge

-- Each fan turns around the vertex two consecutive boundary edges share, so a
-- cavity face is walked once per vertex it has on the boundary loop: twice for
-- two, three times for a face whose whole triangle is on the loop, which is
-- every face of a single-face cavity. Without this plane each of those visits
-- pays two divisions and a dozen multiply-adds for a value already in hand.
--
-- The cached value is the one 'faceCircumcenterRelative' returned, stored as
-- its own coordinates and handed back unchanged; nothing is recomputed from a
-- rearrangement, so a hit and a miss are the same bits. Soundness needs only
-- that the query point cannot change while an entry is readable, which the
-- generation supplies: it is minted once per Sibson query and every stamp from
-- an earlier query is strictly smaller.
--
-- A 'Nothing' is not recorded. It cannot repeat: the first one aborts this
-- fan, and 'buildStolenAreas' abandons the query on the spot.
cachedFaceCircumcenter
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Word32
  -> Point
  -> FaceId
  -> ST s (Maybe (Point))
cachedFaceCircumcenter workspace generation query face = do
  stamp <- MUV.unsafeRead (nnCircumcenterMarks workspace) slot
  if stamp == generation
    then do
      x <- MUV.unsafeRead (nnCircumcenterX workspace) slot
      y <- MUV.unsafeRead (nnCircumcenterY workspace) slot
      pure (Just (Point x y))
    else case faceCircumcenterRelative (nnTriangulation workspace) query face of
      Nothing -> pure Nothing
      Just center@(Point x y) -> do
        MUV.unsafeWrite (nnCircumcenterX workspace) slot x
        MUV.unsafeWrite (nnCircumcenterY workspace) slot y
        MUV.unsafeWrite (nnCircumcenterMarks workspace) slot generation
        pure (Just center)
 where
  !slot = faceIdIndex face

faceCircumcenterRelative
  :: Triangulation mode vertex directed undirected face
  -> Point
  -> FaceId
  -> Maybe (Point)
faceCircumcenterRelative triangulation query face = do
  (a, b, c) <- innerFaceVertices triangulation face
  center <- circumcenter
    (subtractPoint (vertexPoint triangulation a) query)
    (subtractPoint (vertexPoint triangulation b) query)
    (subtractPoint (vertexPoint triangulation c) query)
  if finitePoint center then Just center else Nothing

normalizeWeights
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Int
  -> ST s Bool
normalizeWeights _ 0 = pure False
normalizeWeights workspace count = do
  (!total, !minimumWeight, !finite) <- firstPass 0 0 0 True
  let !tolerance = max 1.0e-12 (128 * scalarEpsilon)
  if not finite || total == 0 || minimumWeight < negate tolerance
    then pure False
    else do
      clampedTotal <- clampPass 0 0
      if clampedTotal <= 0 || not (isFinite clampedTotal)
        then pure False
        else normalizePass 0 clampedTotal >> pure True
 where
  firstPass !index !total !minimumWeight !finite
    | index >= count = pure (total, minimumWeight, finite)
    | otherwise = do
        weight <- MUV.unsafeRead (nnWeightValue workspace) index
        let !minimumWeight' = if index == 0 then weight else min minimumWeight weight
        firstPass (index + 1) (total + weight) minimumWeight' (finite && isFinite weight)
  clampPass !index !total
    | index >= count = pure total
    | otherwise = do
        weight <- MUV.unsafeRead (nnWeightValue workspace) index
        let !clamped = max 0 weight
        MUV.unsafeWrite (nnWeightValue workspace) index clamped
        clampPass (index + 1) (total + clamped)
  normalizePass !index !total
    | index >= count = pure ()
    | otherwise = do
        weight <- MUV.unsafeRead (nnWeightValue workspace) index
        MUV.unsafeWrite (nnWeightValue workspace) index (weight / total)
        normalizePass (index + 1) total

fallbackBarycentric
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Point
  -> FaceId
  -> Int
  -> Int
  -> ST s (Int, InterpolationStats)
fallbackBarycentric workspace query face faceTests cavityCount =
  case innerFaceVertices triangulation face of
    Nothing -> pure (0, InterpolationStats cavityCount 0 faceTests True)
    Just (a, b, c) ->
      case barycentricCoordinates
        (vertexPoint triangulation a)
        (vertexPoint triangulation b)
        (vertexPoint triangulation c)
        query of
        Nothing -> pure (0, InterpolationStats cavityCount 0 faceTests True)
        Just (wa, wb, wc) -> do
          writeWeight workspace 0 a wa
          writeWeight workspace 1 b wb
          writeWeight workspace 2 c wc
          pure (3, InterpolationStats cavityCount 3 faceTests True)
 where
  triangulation = nnTriangulation workspace

writeWeight
  :: NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Int
  -> VertexId
  -> Double
  -> ST s ()
writeWeight workspace index (VertexId vertex) weight = do
  MUV.unsafeWrite (nnWeightVertex workspace) index vertex
  MUV.unsafeWrite (nnWeightValue workspace) index weight

-- | Sample the nearest vertex, or return 'Nothing' for an empty mesh.
interpolateNearest
  :: (VertexId -> value)
  -> Triangulation mode vertex directed undirected face
  -> Maybe VertexId
  -> QueryPoint
  -> Maybe value
interpolateNearest sample triangulation hint query = sample . fst <$> nearestNeighbor triangulation hint query

-- | Interpolate scalar vertex samples in the cell containing the query.
interpolateBarycentric
  :: (VertexId -> Double)
  -> Triangulation mode vertex directed undirected face
  -> Maybe LocationHint
  -> QueryPoint
  -> Maybe Double
interpolateBarycentric sample triangulation hint query =
  case fst (barycentricWeights triangulation hint query) of
    NoWeights -> Nothing
    OneWeight vertex -> Just (sample vertex)
    TwoWeights a wa b wb -> Just (wa * sample a + wb * sample b)
    ThreeWeights a wa b wb c wc -> Just (wa * sample a + wb * sample b + wc * sample c)

-- | Interpolate from reusable Sibson weights.
interpolateNaturalNeighbor
  :: (VertexId -> Double)
  -> NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Maybe LocationHint
  -> QueryPoint
  -> ST s (Maybe Double, InterpolationStats)
interpolateNaturalNeighbor sample workspace hint query = do
  (total, _, stats) <-
    foldNaturalNeighborWeights
      (\accumulator vertex weight -> accumulator + weight * sample vertex)
      0
      workspace
      hint
      query
  pure (if interpolationNaturalNeighbors stats == 0 then Nothing else Just total, stats)

-- | Estimate one vertex gradient from its Delaunay neighbors.
estimateGradient
  :: (VertexId -> Double)
  -> Triangulation mode vertex directed undirected face
  -> VertexId
  -> (Double, Double)
estimateGradient sample triangulation vertex =
  case vertexOutEdge triangulation vertex of
    Nothing -> (0, 0)
    Just start ->
      let !nextEdge = counterClockwise triangulation start
       in if nextEdge == start
            then (0, 0)
            else
              let (!nx, !ny, !nz) = walk start start 0 0 0 0
               in if nz /= 0 && isFinite nz then (-nx / nz, -ny / nz) else (0, 0)
 where
  Point vx vy = vertexPoint triangulation vertex
  !vz = sample vertex
  !bound = numDirectedEdges triangulation + 1

  walk !start !current !steps !sumX !sumY !sumZ
    | steps >= bound = (sumX, sumY, sumZ)
    | otherwise =
        let !nextEdge = counterClockwise triangulation current
            !leftVertex = destination triangulation current
            !rightVertex = destination triangulation nextEdge
            (!nextX, !nextY, !nextZ) = accumulate sumX sumY sumZ leftVertex rightVertex
         in if nextEdge == start
              then (nextX, nextY, nextZ)
              else walk start nextEdge (steps + 1) nextX nextY nextZ

  accumulate !sumX !sumY !sumZ leftVertex rightVertex =
    let Point lx ly = vertexPoint triangulation leftVertex
        Point rx ry = vertexPoint triangulation rightVertex
        !lz = sample leftVertex
        !rz = sample rightVertex
        !d0x = lx - vx
        !d0y = ly - vy
        !d0z = lz - vz
        !d1x = rx - vx
        !d1y = ry - vy
        !d1z = rz - vz
        !normalX = d0y * d1z - d0z * d1y
        !normalY = d0z * d1x - d0x * d1z
        !normalZ = d0x * d1y - d0y * d1x
     in if normalZ > 0
          then (sumX + normalX, sumY + normalY, sumZ + normalZ)
          else (sumX, sumY, sumZ)

-- | Estimate gradients for every vertex in handle order.
estimateGradients
  :: (VertexId -> Double)
  -> Triangulation mode vertex directed undirected face
  -> V.Vector (Double, Double)
estimateGradients sample triangulation =
  V.generate (numVertices triangulation) $ \index ->
    estimateGradient sample triangulation (VertexId (fromIntegral index))

-- | Natural-neighbor interpolation with nodal-gradient correction.
interpolateNaturalNeighborGradient
  :: (VertexId -> Double)
  -> (VertexId -> (Double, Double))
  -> Double
  -> NaturalNeighborWorkspace s mode vertex directed undirected face
  -> Maybe LocationHint
  -> QueryPoint
  -> ST s (Maybe Double, InterpolationStats)
interpolateNaturalNeighborGradient sample gradient flatness workspace hint queryPoint
  | flatness < 0 || not (isFinite flatness) = pure (Nothing, InterpolationStats 0 0 0 False)
  | otherwise = do
      (!count, _, !stats) <- queryNaturalNeighborWorkspace workspace hint queryPoint
      if count == 0
        then pure (Nothing, stats)
        else do
          accumulation <- accumulate 0 count Nothing (0, 0, 0, 0, 0)
          pure (finish accumulation, stats)
 where
  !query = queryPointValue queryPoint
  !triangulation = nnTriangulation workspace

  -- 'flatness' is 0.5 or 1 in practice and @**@ is an exp/log pair per natural
  -- neighbour per query, so both are answered directly. The two shortcuts do
  -- not stand on the same ground.
  --
  -- The identity is exact. @x@ raised to 1 is @x@, which is already a float,
  -- and rounding an exactly representable result admits only that result, so
  -- any implementation faithful to within an ulp returns it.
  --
  -- The square root is not exact and does not need to be. IEEE-754 mandates a
  -- correctly rounded 'sqrt'; @pow@ is a recommended operation carrying no such
  -- requirement. Where the two disagree it is therefore in the last ulp, and it
  -- is 'sqrt' that holds the correctly rounded answer — this substitution can
  -- only move the result toward it. Against this platform's libm they in fact
  -- agree on every non-negative binary32 and on every non-negative binary64
  -- sampled, which is a measurement and not a proof.
  --
  -- They part company at negative zero, where @pow@ answers @+0@ and 'sqrt'
  -- answers @-0@. A sum of two squares is never negative zero, and were it one
  -- the sole consumer below tests @== 0@, which both zeroes satisfy alike.
  raiseToFlatness squared
    | flatness == 0.5 = sqrt squared
    | flatness == 1 = squared
    | otherwise = squared ** flatness

  accumulate !index !count !exact !totals
    | index >= count = pure (exact, totals)
    | otherwise = do
        rawVertex <- MUV.unsafeRead (nnWeightVertex workspace) index
        weight <- MUV.unsafeRead (nnWeightValue workspace) index
        let !vertex = VertexId rawVertex
            !point = vertexPoint triangulation vertex
            !exact' =
              case exact of
                Just _ -> exact
                Nothing
                  | query == point -> Just (sample vertex)
                  | otherwise -> Nothing
            !totals' = contribution totals vertex weight
        accumulate (index + 1) count exact' totals'

  finish
    :: (Maybe Double, (Double, Double, Double, Double, Double))
    -> Maybe Double
  finish (Just value, _) = Just value
  finish (Nothing, (!sumC0, !sumC1, !sumC1Weights, !alphaNumerator, !beta))
    | sumC1Weights == 0 = Just sumC0
    | otherwise =
        let !alpha = alphaNumerator / sumC1Weights
            !c1 = sumC1 / sumC1Weights
            !denominator = alpha + beta
         in if denominator == 0 || not (isFinite denominator)
              then Just sumC0
              else Just ((alpha * sumC0 + beta * c1) / denominator)

  contribution (!sumC0, !sumC1, !sumC1Weights, !alpha, !beta) vertex weight =
    let Point qx qy = query
        Point vx vy = vertexPoint triangulation vertex
        !dx = qx - vx
        !dy = qy - vy
        !radiusSquared = dx * dx + dy * dy
        !radiusPower = raiseToFlatness radiusSquared
        !c1Weight = if radiusPower == 0 then 0 else weight / radiusPower
        (!gx, !gy) = gradient vertex
        !height = sample vertex
        !zeta = height + dx * gx + dy * gy
     in ( sumC0 + height * weight
        , sumC1 + zeta * c1Weight
        , sumC1Weights + c1Weight
        , alpha + c1Weight * radiusSquared
        , beta + weight * radiusSquared
        )

subtractPoint :: Point -> Point -> Point
subtractPoint (Point ax ay) (Point bx by) = Point (ax - bx) (ay - by)
{-# INLINE subtractPoint #-}

finitePoint :: Point -> Bool
finitePoint (Point x y) = isFinite x && isFinite y
{-# INLINE finitePoint #-}

-- | Weight of a directed edge's destination for a query on that edge: the
-- projection factor clamped into the segment, so the origin carries the rest.
edgeDestinationWeight
  :: Triangulation mode vertex directed undirected face
  -> DirectedEdgeId
  -> Point
  -> Double
edgeDestinationWeight triangulation edge query =
  clamp
    0
    1
    ( projectionFactor
        (vertexPoint triangulation (origin triangulation edge))
        (vertexPoint triangulation (destination triangulation edge))
        query
    )

clamp :: Ord value => value -> value -> value -> value
clamp low high = max low . min high
{-# INLINE clamp #-}

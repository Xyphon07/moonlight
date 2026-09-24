{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O3 -fllvm -optlo-O3 -optlc-O3 #-}

module Moonlight.Planar.Internal.CircleSweep
  ( RadiallyOrderedArena
  , radiallyOrderArena
  , radiallyOrderGeometry
  , circleSweepInsert
  ) where

import Moonlight.Planar.BuildStats (BuildMetric (HullInsertions, EdgeFlips, LegalizationMaxStack, SweepFastPoints, SweepSkippedPoints))

import Control.Monad (forM_, when)
import Control.Monad.ST (ST)
import Data.Bits ((.&.), shiftL, shiftR, xor)
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Algorithms.Radix as Radix
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64)
import Moonlight.Planar.Internal.HandleDefs (DirectedEdgeId (..))
import Moonlight.Planar.Insertion (insertExistingVertex)
import Moonlight.Planar.Internal.DcelOperations.Hull
  ( ReservedSweepCells
  , SweepCellCursor
  , SweepInsertion (..)
  , closeOuterTurnReserved
  , commitReservedSweepConnections
  , fixHullConvexity
  , initialSweepCellCursor
  , insertOutsideHullAtEdge
  , reserveSweepCells
  )
import Moonlight.Planar.Internal.DcelOperations.CandidateArena
  ( seedGenericPairInArena
  )
import Moonlight.Planar.Internal.DcelOperations.Chain
  ( connectAscendingLineArena
  , prepareAscendingLineArena
  )
import Moonlight.Planar.Internal.DcelOperations.Normalize
  ( LegalizationDrain (..)
  , drainDenseUnconstrainedGenericLegalization
  )
import Moonlight.Planar.Internal.Mutable
  ( DenseMutableDcel
  , MutableDcel
  , connectedCount
  , denseMutableDcel
  , denseMutableOwner
  , denseFaceEdges
  , denseReadFaceEdge
  , denseReadNext
  , denseReadOrigin
  , denseReadPointX
  , denseReadPointY
  , denseReadPrevious
  , directedEdgeCount
  , faceCount
  , halfEdgeCapacity
  , pointCapacity
  )
import Moonlight.Planar.Internal.OperationState
  ( Counter (..)
  , LegalizationArena
  , OperationState
  , addCounter
  , legalizationArena
  , maxCounter
  , readScratch
  , storeLegalizationArena
  , writeScratch
  )
import Moonlight.Planar.Internal.PackedIndex (noIndex, unpackIndex)
import Moonlight.Planar.Internal.Probe (Probe (..))
import Moonlight.Planar.Internal.Predicates (orient2dCoordinates)
import Moonlight.Planar.Types (BuildError (..))

-- | The hull is an angular index over the DCEL outer-face cycle. The cycle
-- itself already owns hull adjacency, so left and right walking is
-- 'readPrevious'/'readNext' on the live mesh and this record only caches what
-- topology cannot state: the pseudo-angle of each outer edge's origin and the
-- bucket anchors accelerating the predecessor search. An outer edge's key is
-- its origin's @(angle, x, y)@ with the edge id as the final tie-break; the
-- angle lives in 'hullAngleByEdge' and the coordinates are re-read from the
-- immutable origin only when two cached angles compare exactly equal. Slots
-- of edges that have left the outer cycle are never read again, so the cache
-- needs no invalidation, and there is no second ring beside the authoritative
-- one.
data Hull s = Hull
  { hullCenterX :: {-# UNPACK #-} !Double
  , hullCenterY :: {-# UNPACK #-} !Double
  , hullBucketCapacity :: {-# UNPACK #-} !Int
  , hullAngleByEdge :: !(MUV.MVector s Double)
  }

-- | The strict state of the derived angular section. The bucket vector is
-- mutated only to transport the section across one local hull rewrite; its
-- extent and active cardinality are threaded as values, so the sweep does not
-- bounce through singleton mutable cells for facts already known at descent.
data HullIndex s = HullIndex
  !(MUV.MVector s Word32)
  {-# UNPACK #-} !Int

data DeferredInsertion s
  = DeferredInsertionFailure !BuildError
  | DeferredInsertionSuccess
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !SweepCellCursor
      !(LegalizationArena s)
      {-# UNPACK #-} !(HullIndex s)

data ClosedHullSection s = ClosedHullSection
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  !(LegalizationArena s)
  {-# UNPACK #-} !SweepCellCursor

-- | Vertex ids after their radial keys have descended to one total order. The
-- constructor is private: the circle sweep consumes the proof and therefore
-- never pays to establish the same ordering twice. Coordinates remain owned
-- by the DCEL rather than being duplicated in the sweep input.
newtype RadiallyOrderedArena s = RadiallyOrderedArena
  (MUV.MVector s Word32)

radiallyOrderArena
  :: MUV.MVector s (Double, Double, Double, Word32)
  -> ST s (RadiallyOrderedArena s)
radiallyOrderArena arena = do
  Intro.sort arena
  let (_, _, _, vertices) = MUV.unzip4 arena
  pure (RadiallyOrderedArena vertices)
{-# INLINE radiallyOrderArena #-}

-- | Order the geometry-only ingress by moving only its existing vertex-id
-- plane. Stable radix descent first orders the high 36 bits of each binary64
-- distance. Every equal-prefix fibre is then normalized by the complete
-- historical @(distance, x, y, vertex)@ order, recovering the discarded low
-- 28 bits before the fibres glue into the authoritative radial order. The key
-- coordinates remain in their immutable planes, so the sole linear scratch
-- plane is four bytes per site rather than a second copy of every radial
-- record.
--
-- Coordinate admission bounds every squared distance to a finite,
-- non-negative binary64 value, whose unsigned word order is numeric order.
radiallyOrderGeometry
  :: U.Vector Double
  -> U.Vector Double
  -> U.Vector Double
  -> MUV.MVector s Word32
  -> ST s (RadiallyOrderedArena s)
radiallyOrderGeometry distances pointXs pointYs vertices = do
  Radix.sortBy
    radialPassCount
    radialBucketCount
    (radialDistanceBucket distances)
    vertices
  when (MUV.length vertices > 1) $ do
    first <- MUV.unsafeRead vertices 0
    let !firstPrefix = geometryDistancePrefix distances first
    (!finalFibreStart, _) <-
      MUV.ifoldM'
        (\(!fibreStart, !distancePrefix) relativeIndex vertex ->
           let !nextPrefix = geometryDistancePrefix distances vertex
               !index = relativeIndex + 1
            in if nextPrefix == distancePrefix
                 then pure (fibreStart, distancePrefix)
                 else do
                   sortGeometryPrefixFibre
                     distances
                     pointXs
                     pointYs
                     vertices
                     fibreStart
                     index
                   pure (index, nextPrefix)
        )
        (0, firstPrefix)
        (MUV.unsafeSlice 1 (MUV.length vertices - 1) vertices)
    sortGeometryPrefixFibre
      distances
      pointXs
      pointYs
      vertices
      finalFibreStart
      (MUV.length vertices)
  pure (RadiallyOrderedArena vertices)
{-# INLINE radiallyOrderGeometry #-}

radialPassCount :: Int
radialPassCount = 3

radialBucketCount :: Int
radialBucketCount = 1 `shiftL` radialPassWidth

radialPassWidth :: Int
radialPassWidth = 12

radialPrefixDiscardedBits :: Int
radialPrefixDiscardedBits = 64 - radialPassCount * radialPassWidth

radialBucketMask :: Word64
radialBucketMask = fromIntegral (radialBucketCount - 1)

radialDistanceBucket :: U.Vector Double -> Int -> Word32 -> Int
radialDistanceBucket distances pass vertex =
  fromIntegral
    ( (castDoubleToWord64 (geometryDistance distances vertex)
         `shiftR` (radialPrefixDiscardedBits + radialPassWidth * pass)
      )
        .&. radialBucketMask
    )
{-# INLINE radialDistanceBucket #-}

geometryDistancePrefix :: U.Vector Double -> Word32 -> Word64
geometryDistancePrefix distances vertex =
  castDoubleToWord64 (geometryDistance distances vertex)
    `shiftR` radialPrefixDiscardedBits
{-# INLINE geometryDistancePrefix #-}

geometryDistance :: U.Vector Double -> Word32 -> Double
geometryDistance distances vertex = U.unsafeIndex distances (fromIntegral vertex)
{-# INLINE geometryDistance #-}

compareGeometryVertex
  :: U.Vector Double
  -> U.Vector Double
  -> U.Vector Double
  -> Word32
  -> Word32
  -> Ordering
compareGeometryVertex distances pointXs pointYs left right =
  case compare (geometryDistance distances left) (geometryDistance distances right) of
    EQ ->
      case compare (coordinateAt pointXs left) (coordinateAt pointXs right) of
        EQ ->
          case compare (coordinateAt pointYs left) (coordinateAt pointYs right) of
            EQ -> compare left right
            yOrder -> yOrder
        xOrder -> xOrder
    distanceOrder -> distanceOrder
{-# INLINE compareGeometryVertex #-}

coordinateAt :: U.Vector Double -> Word32 -> Double
coordinateAt coordinates vertex = U.unsafeIndex coordinates (fromIntegral vertex)
{-# INLINE coordinateAt #-}

sortGeometryPrefixFibre
  :: U.Vector Double
  -> U.Vector Double
  -> U.Vector Double
  -> MUV.MVector s Word32
  -> Int
  -> Int
  -> ST s ()
sortGeometryPrefixFibre distances pointXs pointYs vertices start end =
  when (end - start > 1) $
    Intro.sortBy
      (compareGeometryVertex distances pointXs pointYs)
      (MUV.unsafeSlice start (end - start) vertices)
{-# INLINE sortGeometryPrefixFibre #-}

-- | Circle sweep over one mutable DCEL, consuming one radial vertex order.
-- Coordinates are read from the authoritative dense DCEL. Insertions
-- initially close only acute hull turns. One terminal Graham pass restores
-- full convexity, so construction does not repeatedly pay for global
-- convexity that no intermediate observer can see.
circleSweepInsert
  :: MutableDcel s vertex directed undirected face
  -> OperationState s
  -> RadiallyOrderedArena s
  -> ST s (Either BuildError Int)
circleSweepInsert mutable operation (RadiallyOrderedArena arena) =
  case denseMutableDcel mutable of
    Nothing -> pure (Left CircleSweepRequiresDenseStorage)
    Just dense -> circleSweepInsertDense dense operation arena
{-# INLINE circleSweepInsert #-}

circleSweepInsertDense
  :: forall s vertex directed undirected face
   . DenseMutableDcel s vertex directed undirected face
  -> OperationState s
  -> MUV.MVector s Word32
  -> ST s (Either BuildError Int)
circleSweepInsertDense dense operation arena
  | MUV.length arena == 0 = pure (Right 0)
  | otherwise = do
      residentCount <- connectedCount mutable
      ascendingLine <-
        if residentCount == 0
          then prepareAscendingLineArena dense arena
          else pure Nothing
      case ascendingLine of
        Just line -> connectAscendingLineArena dense operation line
        Nothing -> circleSweepInsertArea dense operation arena
 where
  !mutable = denseMutableOwner dense
{-# INLINE circleSweepInsertDense #-}

circleSweepInsertArea
  :: forall s vertex directed undirected face
   . DenseMutableDcel s vertex directed undirected face
  -> OperationState s
  -> MUV.MVector s Word32
  -> ST s (Either BuildError Int)
circleSweepInsertArea dense operation arena = do
      let !ordered = MUV.length arena
      seed <- insertSeed 0
      case seed of
        Left failure -> pure (Left failure)
        Right seedCount -> do
          faces <- faceCount mutable
          if faces <= 1 || seedCount >= ordered
            then pure (Right seedCount)
            else do
              (centerX, centerY) <- seedCentre dense
              reservedOutcome <- reserveSweepCells dense (ordered - seedCount)
              case reservedOutcome of
                Left failure -> pure (Left failure)
                Right reserved -> do
                  builtHull <- buildHull dense operation centerX centerY
                  case builtHull of
                    Left failure -> pure (Left failure)
                    Right (!hull, !initialHullIndex) -> do
                      initialLegalizationArena <- legalizationArena operation
                      let !initialCursor = initialSweepCellCursor reserved
                      inserted <-
                        insertRemaining
                          reserved
                          initialCursor
                          initialLegalizationArena
                          hull
                          initialHullIndex
                          ordered
                          seedCount
                          (0 :: Int)
                          (0 :: Int)
                          (0 :: Int)
                          (0 :: Int)
                      case inserted of
                        Left failure -> pure (Left failure)
                        Right (!skippedCount, !fastCount, !flips, !maxDepth, !sweepCursor, !sweepArena) -> do
                          repaired <- fixHullConvexity reserved sweepCursor operation sweepArena
                          case repaired of
                            Left failure -> pure (Left failure)
                            Right (!_closures, !terminalFlips, !terminalMaxDepth, !finalArena, !finalCursor) -> do
                              commitReservedSweepConnections reserved finalCursor fastCount
                              storeLegalizationArena operation finalArena
                              -- The sweep counts its own hull insertions; its drains
                              -- hand their flip and depth tallies up once.
                              addCounter operation (Reported HullInsertions) fastCount
                          -- The angular candidate leaves a point to the fallback
                          -- whenever it lands right of, or on, the hull edge its
                          -- own angle selected — spade's
                          -- `is_on_right_side_or_on_line` branch, which spade's own
                          -- source calls "very slow". Both counts ride out in
                          -- BuildStats so the split is comparable across the two
                          -- implementations and satisfies
                          -- seed + fast + skipped = unique.
                          --
                          -- (Reported SweepFastPoints) is NOT (Reported HullInsertions)
                          -- renamed: the latter is also charged by
                          -- insertOutsideHull, so it counts hull-adjacent
                          -- insertions from either path and dominates this one
                          -- whenever a skipped point lands outside the hull. The
                          -- two coincide only while skipped is zero, and charging
                          -- them independently is what makes the identity a check
                          -- rather than a restatement.
                              addCounter operation (Reported SweepFastPoints) fastCount
                              addCounter operation (Reported SweepSkippedPoints) skippedCount
                              addCounter operation (Reported EdgeFlips) flips
                              maxCounter operation (Reported LegalizationMaxStack) maxDepth
                              addCounter operation (Reported EdgeFlips) terminalFlips
                              maxCounter operation (Reported LegalizationMaxStack) terminalMaxDepth
                              insertedSkipped <- insertSkipped skippedCount 0
                              pure (seedCount <$ insertedSkipped)
 where
  !mutable = denseMutableOwner dense

  insertSeed !index
    | index >= MUV.length arena = pure (Right index)
    | otherwise = do
        raw <- MUV.unsafeRead arena index
        result <- insertExistingVertex @'ProbeOff mutable operation (unpackIndex raw)
        case result of
          Left failure -> pure (Left failure)
          Right () -> do
            faces <- faceCount mutable
            if faces > 1
              then pure (Right (index + 1))
              else insertSeed (index + 1)

  insertRemaining
    :: ReservedSweepCells s vertex directed undirected face
    -> SweepCellCursor
    -> LegalizationArena s
    -> Hull s
    -> HullIndex s
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> ST s (Either BuildError (Int, Int, Int, Int, SweepCellCursor, LegalizationArena s))
  insertRemaining !reserved !cursor !candidateArena !hull !hullIndex !ordered !index !skippedCount !fastCount !flips !maxDepth
    | index >= ordered = pure (Right (skippedCount, fastCount, flips, maxDepth, cursor, candidateArena))
    | otherwise = do
        raw <- MUV.unsafeRead arena index
        let !vertex = unpackIndex raw
        queryXWide <- denseReadPointX dense vertex
        queryYWide <- denseReadPointY dense vertex
        let !queryAngle =
              pseudoAngle (hullCenterX hull) (hullCenterY hull) queryXWide queryYWide
        edge <- hullCandidate dense hull hullIndex queryAngle queryXWide queryYWide
        fromVertex <- denseReadOrigin dense edge
        toVertex <- denseReadOrigin dense (edge `xor` 1)
        fromX <- denseReadPointX dense fromVertex
        fromY <- denseReadPointY dense fromVertex
        toX <- denseReadPointX dense toVertex
        toY <- denseReadPointY dense toVertex
        if orient2dCoordinates fromX fromY toX toY queryXWide queryYWide == GT
          then do
            deferred <-
              insertDeferred
                reserved
                cursor
                dense
                hull
                hullIndex
                candidateArena
                edge
                fromVertex
                toVertex
                vertex
                queryXWide
                queryYWide
                queryAngle
            case deferred of
              DeferredInsertionFailure failure -> pure (Left failure)
              DeferredInsertionSuccess !newFlips !newMaxDepth !nextCursor !nextArena !nextHullIndex ->
                insertRemaining
                  reserved
                  nextCursor
                  nextArena
                  hull
                  nextHullIndex
                  ordered
                  (index + 1)
                  skippedCount
                  (fastCount + 1)
                  (flips + newFlips)
                  (max maxDepth newMaxDepth)
          else do
            -- The radial order is consumed monotonically, so its processed
            -- prefix is dead. Compact deferred vertices into that prefix
            -- instead of allocating a second linear arena.
            MUV.unsafeWrite arena skippedCount raw
            insertRemaining reserved cursor candidateArena hull hullIndex ordered (index + 1) (skippedCount + 1) fastCount flips maxDepth

  insertSkipped !count = go
   where
    go !index
      | index >= count = pure (Right ())
      | otherwise = do
          vertex <- fromIntegral <$> MUV.unsafeRead arena index
          inserted <- insertExistingVertex @'ProbeOff mutable operation vertex
          case inserted of
            Left failure -> pure (Left failure)
            Right () -> go (index + 1)

-- | The hull centre: the centroid of the first inner face, in the widened
-- comparison format, computed exactly as @centroid@ states it.
seedCentre
  :: DenseMutableDcel s vertex directed undirected face
  -> ST s (Double, Double)
seedCentre dense = do
  (e0, e1, e2) <- denseFaceEdges dense 1
  o0 <- denseReadOrigin dense e0
  o1 <- denseReadOrigin dense e1
  o2 <- denseReadOrigin dense e2
  x0 <- denseReadPointX dense o0
  y0 <- denseReadPointY dense o0
  x1 <- denseReadPointX dense o1
  y1 <- denseReadPointY dense o1
  x2 <- denseReadPointX dense o2
  y2 <- denseReadPointY dense o2
  pure (x0 + (x1 - x0) / 3 + (x2 - x0) / 3, y0 + (y1 - y0) / 3 + (y2 - y0) / 3)

-- | Index the authoritative outer cycle. The seed fan is star-shaped around
-- the first face's centroid — a collinear chain closed by its apex — so the
-- cycle is already the angular order the predecessor search assumes; what
-- remains is caching each edge's angle and anchoring the buckets.
buildHull
  :: DenseMutableDcel s vertex directed undirected face
  -> OperationState s
  -> Double
  -> Double
  -> ST s (Either BuildError (Hull s, HullIndex s))
buildHull dense operation centerX centerY = do
  let !mutable = denseMutableOwner dense
  countResult <- collectOuterEdges dense operation
  case countResult of
    Left failure -> pure (Left failure)
    Right count
      | count <= 0 -> pure (Left CircleSweepHullEmpty)
      | otherwise -> do
          let !capacity = max (count + 4) (pointCapacity mutable + 8)
          hullAngleByEdge <- MUV.new (halfEdgeCapacity mutable)
          let hull =
                Hull
                  { hullCenterX = centerX
                  , hullCenterY = centerY
                  , hullBucketCapacity = capacity
                  , hullAngleByEdge
                  }
          forM_ [0 .. count - 1] $ \index -> do
            edge <- readScratch operation index
            origin <- denseReadOrigin dense edge
            x <- denseReadPointX dense origin
            y <- denseReadPointY dense origin
            MUV.unsafeWrite hullAngleByEdge edge (pseudoAngle centerX centerY x y)
          hullIndex <- rebuildBuckets dense hull count (initialBucketCount count capacity)
          pure (Right (hull, hullIndex))

collectOuterEdges
  :: forall s vertex directed undirected face.
     DenseMutableDcel s vertex directed undirected face
  -> OperationState s
  -> ST s (Either BuildError Int)
collectOuterEdges dense operation = do
  let !mutable = denseMutableOwner dense
  start <- denseReadFaceEdge dense 0
  if start < 0
    then pure (Right 0)
    else do
      bound <- directedEdgeCount mutable
      go (bound + 1) start start False 0
 where
  go
    :: Int
    -> Int
    -> Int
    -> Bool
    -> Int
    -> ST s (Either BuildError Int)
  go !remaining !start !edge !seen !count
    | remaining <= 0 =
        pure
          ( Left
              ( OuterCycleDidNotTerminate
                  (DirectedEdgeId (fromIntegral start))
                  (DirectedEdgeId (fromIntegral edge))
                  count
              )
          )
    | seen && edge == start = pure (Right count)
    | otherwise = do
        writeScratch operation count edge
        following <- denseReadNext dense edge
        go (remaining - 1) start following True (count + 1)

initialBucketCount :: Int -> Int -> Int
initialBucketCount active capacity =
  min capacity (nextPowerOfTwo (max 8 ((active + 1) `quot` 2)))

nextPowerOfTwo :: Int -> Int
nextPowerOfTwo requested = go 1
 where
  target = max 1 requested
  go !value
    | value >= target = value
    | value > maxBound `quot` 2 = maxBound
    | otherwise = go (value * 2)

readAngle :: Hull s -> Int -> ST s Double
readAngle hull edge = MUV.unsafeRead (hullAngleByEdge hull) edge
{-# INLINE readAngle #-}

rebuildBuckets
  :: forall s vertex directed undirected face.
     DenseMutableDcel s vertex directed undirected face
  -> Hull s
  -> Int
  -> Int
  -> ST s (HullIndex s)
rebuildBuckets dense hull active requested = do
  let !count = max 1 (min (hullBucketCapacity hull) requested)
  buckets <- MUV.replicate count noIndex
  start <- denseReadFaceEdge dense 0
  let go
        :: Int
        -> Int
        -> ST s ()
      go !remaining !edge
        | remaining <= 0 = pure ()
        | otherwise = do
            following <- denseReadNext dense edge
            angle <- readAngle hull edge
            followingAngle <- readAngle hull following
            writeBucketSegment buckets angle followingAngle edge
            go (remaining - 1) following
  when (active > 0 && start >= 0) (go active start)
  pure (HullIndex buckets active)

maybeGrowBuckets :: DenseMutableDcel s vertex directed undirected face -> Hull s -> HullIndex s -> ST s (HullIndex s)
maybeGrowBuckets dense hull hullIndex@(HullIndex buckets active) = do
  let !current = MUV.length buckets
  if active > 2 * current && current < hullBucketCapacity hull
    then rebuildBuckets dense hull active (min (hullBucketCapacity hull) (2 * current))
    else pure hullIndex

bucketFor :: Int -> Double -> Int
bucketFor count angle =
  min (count - 1) (max 0 (floor (angle * fromIntegral count * 0.25)))
{-# INLINE bucketFor #-}

ceilingBucketFor :: Int -> Double -> Int
ceilingBucketFor count angle =
  ceiling (angle * fromIntegral count * 0.25) `rem` count
{-# INLINE ceilingBucketFor #-}

-- | Install the authoritative outer edge whose angular segment contains each
-- bucket boundary in the half-open clockwise arc from @fromAngle@ to
-- @toAngle@. Adjacency remains solely in the DCEL; this is the derived section
-- needed to land a lookup near that ring.
writeBucketSegment
  :: MUV.MVector s Word32
  -> Double
  -> Double
  -> Int
  -> ST s ()
writeBucketSegment buckets fromAngle toAngle edge = do
  let !count = MUV.length buckets
      !fromBucket = ceilingBucketFor count fromAngle
      !toBucket = ceilingBucketFor count toAngle
      !packed = fromIntegral edge
  case compare fromBucket toBucket of
    LT -> MUV.set (MUV.unsafeSlice fromBucket (toBucket - fromBucket) buckets) packed
    GT -> do
      MUV.set (MUV.unsafeSlice fromBucket (count - fromBucket) buckets) packed
      MUV.set (MUV.unsafeSlice 0 toBucket buckets) packed
    EQ -> pure ()
{-# INLINE writeBucketSegment #-}

-- | Whether an outer edge's key orders at or before the stated query key,
-- settled field by field without materializing either key.
edgeAtMostAtAngle
  :: DenseMutableDcel s vertex directed undirected face
  -> Int
  -> Double
  -> Double
  -> Double
  -> Double
  -> Int
  -> ST s Bool
edgeAtMostAtAngle dense edge angle queryAngle queryX queryY tie =
  case compare angle queryAngle of
    LT -> pure True
    GT -> pure False
    EQ -> do
      origin <- denseReadOrigin dense edge
      x <- denseReadPointX dense origin
      case compare x queryX of
        LT -> pure True
        GT -> pure False
        EQ -> do
          y <- denseReadPointY dense origin
          case compare y queryY of
            LT -> pure True
            GT -> pure False
            EQ -> pure (edge <= tie)
{-# INLINE edgeAtMostAtAngle #-}

-- | Reconcile the derived angular index once after the local topology section
-- has glued. Replacing one outer edge by two adds one active edge; every closed
-- turn removes one. The final two edges cover the entire rewritten angular
-- arc, so their segments descend to the bucket view in one gluing step.
finishHullRewrite
  :: DenseMutableDcel s vertex directed undirected face
  -> Hull s
  -> HullIndex s
  -> Int
  -> Int
  -> Int
  -> ST s (HullIndex s)
finishHullRewrite dense hull (HullIndex buckets active) leftEdge rightEdge activeDelta = do
  leftAngle <- readAngle hull leftEdge
  middleAngle <- readAngle hull rightEdge
  afterRight <- denseReadNext dense rightEdge
  rightAngle <- readAngle hull afterRight
  writeBucketSegment buckets leftAngle middleAngle leftEdge
  writeBucketSegment buckets middleAngle rightAngle rightEdge
  maybeGrowBuckets dense hull (HullIndex buckets (active + activeDelta))
{-# INLINE finishHullRewrite #-}

-- | The outer edge whose key is the greatest key at or below the query: the
-- visible candidate the sweep inserts against. Bucket anchors land the walk
-- near the answer and the live outer cycle carries it the rest of the way.
hullCandidate
  :: forall s vertex directed undirected face.
     DenseMutableDcel s vertex directed undirected face
  -> Hull s
  -> HullIndex s
  -> Double
  -> Double
  -> Double
  -> ST s Int
hullCandidate dense hull (HullIndex buckets active) queryAngle queryX queryY = do
  let !count = MUV.length buckets
      !bucket = bucketFor count queryAngle
  raw <- MUV.unsafeRead buckets bucket
  if raw == noIndex
    then denseReadFaceEdge dense 0
    else adjustFromBoundary bucket (unpackIndex raw)
 where
  adjustFromBoundary
    :: Int
    -> Int
    -> ST s Int
  adjustFromBoundary !bucket !initial = do
    initialAngle <- readAngle hull initial
    initialAtMost <- cyclicAtMost bucket initial initialAngle
    if initialAtMost
      then advance active initial initialAngle
      else retreat active initial
   where
    cyclicAtMost
      :: Int
      -> Int
      -> Double
      -> ST s Bool
    cyclicAtMost boundaryBucket edge angle =
      if boundaryBucket == 0 && angle > queryAngle
        then pure True
        else edgeAtMostAtAngle dense edge angle queryAngle queryX queryY maxBound

    advance
      :: Int
      -> Int
      -> Double
      -> ST s Int
    advance !remaining !edge !edgeAngle
      | remaining <= 0 = pure initial
      | otherwise = do
          following <- denseReadNext dense edge
          followingAngle <- readAngle hull following
          -- Only the bucket-zero anchor may precede the query by crossing the
          -- angular seam. Once the walk leaves that anchor, ordinary key order
          -- is authoritative; treating every high-angle edge as below a
          -- bucket-zero query walks straight past the answer and around the
          -- entire ring.
          let crossesSeam = followingAngle < edgeAngle
              seamPermitted = bucket == 0 && edgeAngle > queryAngle
          followingAtMost <- edgeAtMostAtAngle dense following followingAngle queryAngle queryX queryY maxBound
          if (not crossesSeam || seamPermitted) && followingAtMost
            then advance (remaining - 1) following followingAngle
            else pure edge

    retreat
      :: Int
      -> Int
      -> ST s Int
    retreat !remaining !edge
      | remaining <= 0 = pure initial
      | otherwise = do
          previous <- denseReadPrevious dense edge
          previousAngle <- readAngle hull previous
          previousAtMost <- cyclicAtMost bucket previous previousAngle
          if previousAtMost
            then advance remaining previous previousAngle
            else retreat (remaining - 1) previous

insertDeferred
  :: ReservedSweepCells s vertex directed undirected face
  -> SweepCellCursor
  -> DenseMutableDcel s vertex directed undirected face
  -> Hull s
  -> HullIndex s
  -> LegalizationArena s
  -> Int
  -> Int
  -> Int
  -> Int
  -> Double
  -> Double
  -> Double
  -> ST s (DeferredInsertion s)
insertDeferred reserved cursor dense hull hullIndex arena replacedEdge fromVertex toVertex vertex insertedX insertedY insertedAngle = do
  inserted <-
    insertOutsideHullAtEdge
      reserved
      cursor
      arena
      replacedEdge
      fromVertex
      toVertex
      vertex
  case inserted of
    SweepInsertionFailure failure -> pure (DeferredInsertionFailure failure)
    SweepInsertion firstOuterEdge lastOuterEdge initialFlips initialMaxDepth arenaAfterInsertion cursorAfterInsertion ->
      insertDeferredBetween
        firstOuterEdge
        lastOuterEdge
        initialFlips
        initialMaxDepth
        arenaAfterInsertion
        cursorAfterInsertion
 where
  insertDeferredBetween firstEdge lastEdge initialFlips initialMaxDepth arenaAfterInsertion cursorAfterInsertion = do
    -- The patch proves the new keys algebraically, so no key is rediscovered
    -- from the mesh: replacing outer edge a->b with a->v, v->b keeps the
    -- replaced edge's origin for the first spoke (its cached angle carries
    -- over) and starts the last spoke at the inserted vertex, whose angle is the
    -- one the candidate search was already given. The tie-break is each fresh
    -- edge id itself.
    replacedAngle <- readAngle hull replacedEdge
    -- Both spokes join the outer cycle before either is indexed, and a bucket
    -- rebuild during activation walks the live cycle: both angle slots must be
    -- initialized before the first activation can read them.
    MUV.unsafeWrite (hullAngleByEdge hull) firstEdge replacedAngle
    MUV.unsafeWrite (hullAngleByEdge hull) lastEdge insertedAngle
    -- Every turn this insertion closes legalizes together in one epoch. Closure
    -- never deletes an edge and never touches the outer cycle, so which turns
    -- close does not depend on when the interior is repaired; the fan above
    -- keeps its own drain because its candidates are oriented against the
    -- inserted vertex, and that star must still be intact when they are tested.
    ClosedHullSection left closuresLeft topLeft arenaAfterLeft cursorAfterLeft <- closeLeft firstEdge 0 0 arenaAfterInsertion cursorAfterInsertion
    ClosedHullSection right closuresRight topAll arenaAfterRight cursorAfterRight <- closeRight lastEdge 0 topLeft arenaAfterLeft cursorAfterLeft
    let !closures = closuresLeft + closuresRight
    nextHullIndex <- finishHullRewrite dense hull hullIndex left right (1 - closures)
    (!flips, !maxDepth, !finalArena) <-
      if topAll == 0
        then pure (0, 0, arenaAfterRight)
        else do
          LegalizationDrain drainedFlips drainedMaxDepth () drainedArena <-
            drainDenseUnconstrainedGenericLegalization dense arenaAfterRight topAll
          pure (drainedFlips, drainedMaxDepth, drainedArena)
    pure
      ( DeferredInsertionSuccess
          (initialFlips + flips)
          (max initialMaxDepth maxDepth)
          cursorAfterRight
          finalArena
          nextHullIndex
      )

  closeLeft !current !closures !top !sectionArena !sectionCursor = do
    left <- denseReadPrevious dense current
    close <- shouldCloseLeftTurn dense hull insertedAngle insertedX insertedY left current
    if not close
      then pure (ClosedHullSection current closures top sectionArena sectionCursor)
      else do
        leftAngle <- readAngle hull left
        (replacement, nextCursor) <- closeOuterTurnReserved reserved sectionCursor left
        -- Closing consecutive a->b, b->c into a->c keeps the first edge's
        -- origin, so the replacement inherits its angle; the tie is the fresh
        -- edge id. Both retired edges answer to the same left neighbour, the
        -- edge now preceding the replacement on the outer cycle. Seeding
        -- happens here, after the replacement's links exist.
        MUV.unsafeWrite (hullAngleByEdge hull) replacement leftAngle
        (nextArena, nextTop) <- seedGenericPairInArena sectionArena top left current
        closeLeft replacement (closures + 1) nextTop nextArena nextCursor

  closeRight !current !closures !top !sectionArena !sectionCursor = do
    right <- denseReadNext dense current
    close <- shouldCloseRightTurn dense hull insertedAngle insertedX insertedY current right
    if not close
      then pure (ClosedHullSection current closures top sectionArena sectionCursor)
      else do
        currentAngle <- readAngle hull current
        (replacement, nextCursor) <- closeOuterTurnReserved reserved sectionCursor current
        MUV.unsafeWrite (hullAngleByEdge hull) replacement currentAngle
        (nextArena, nextTop) <- seedGenericPairInArena sectionArena top current right
        closeRight replacement (closures + 1) nextTop nextArena nextCursor
{-# INLINE insertDeferred #-}

-- | Test the left-hand turn where the inserted point is the target of the
-- second edge. Adjacency is not rediscovered: 'closeLeft' obtained @first@
-- from @previous second@ in the same local section.
shouldCloseLeftTurn
  :: DenseMutableDcel s vertex directed undirected face
  -> Hull s
  -> Double
  -> Double
  -> Double
  -> Int
  -> Int
  -> ST s Bool
shouldCloseLeftTurn dense hull insertedAngle insertedX insertedY first second = do
  fromVertex <- denseReadOrigin dense first
  middleVertex <- denseReadOrigin dense (first `xor` 1)
  fromX <- denseReadPointX dense fromVertex
  fromY <- denseReadPointY dense fromVertex
  middleX <- denseReadPointX dense middleVertex
  middleY <- denseReadPointY dense middleVertex
  middleAngle <- readAngle hull second
  -- Same-ray/acute compatibility rejects most local sections.  Settle that
  -- cheap obstruction before paying for the exact orientation predicate; the
  -- conjunction is unchanged, only its evaluation order is less profligate.
  if
    middleAngle /= insertedAngle
      && not (acuteAtMiddle fromX fromY middleX middleY insertedX insertedY)
    then pure False
    else pure (orient2dCoordinates fromX fromY middleX middleY insertedX insertedY == GT)
{-# INLINE shouldCloseLeftTurn #-}

-- | The symmetric right-hand test, where the inserted point is the first
-- edge's source. 'closeRight' obtained @second@ from @next first@, so this
-- section likewise consumes that adjacency proof instead of reading it again.
shouldCloseRightTurn
  :: DenseMutableDcel s vertex directed undirected face
  -> Hull s
  -> Double
  -> Double
  -> Double
  -> Int
  -> Int
  -> ST s Bool
shouldCloseRightTurn dense hull insertedAngle insertedX insertedY first second = do
  middleVertex <- denseReadOrigin dense (first `xor` 1)
  targetVertex <- denseReadOrigin dense (second `xor` 1)
  middleX <- denseReadPointX dense middleVertex
  middleY <- denseReadPointY dense middleVertex
  targetX <- denseReadPointX dense targetVertex
  targetY <- denseReadPointY dense targetVertex
  middleAngle <- readAngle hull second
  -- Symmetric to the left-hand descent above: compatibility first, exact
  -- orientation only for sections that can actually glue.
  if
    middleAngle /= insertedAngle
      && not (acuteAtMiddle insertedX insertedY middleX middleY targetX targetY)
    then pure False
    else pure (orient2dCoordinates insertedX insertedY middleX middleY targetX targetY == GT)
{-# INLINE shouldCloseRightTurn #-}

-- Spade's deferred-convexity rule is local: close the turn when the angle at
-- the shared hull vertex is strictly below 90 degrees. Requiring the entire
-- triangle to be acute leaves avoidable star-hull work for the terminal pass.
acuteAtMiddle :: Double -> Double -> Double -> Double -> Double -> Double -> Bool
acuteAtMiddle ax ay bx by cx cy =
  let !ux = ax - bx
      !uy = ay - by
      !vx = cx - bx
      !vy = cy - by
      !dot = ux * vx + uy * vy
      !scale = max 1 (ux * ux + uy * uy + vx * vx + vy * vy)
   in dot > 64 * encodeFloat 1 (-52) * scale
{-# INLINE acuteAtMiddle #-}

-- Clockwise pseudo-angle in [0,4), matching the orientation of the outer-face
-- cycle.
pseudoAngle :: Double -> Double -> Double -> Double -> Double
pseudoAngle centerX centerY x y
  | norm == 0 = 0
  | raw >= 4 = 0
  | otherwise = raw
 where
  !dx = x - centerX
  !dy = y - centerY
  !norm = abs dx + abs dy
  !projection = dx / norm
  !raw = if dy > 0 then 1 + projection else 3 - projection
{-# INLINE pseudoAngle #-}

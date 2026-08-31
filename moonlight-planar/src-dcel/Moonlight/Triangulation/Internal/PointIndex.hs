{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The one derived identity index from canonical position hashes to the
-- vertices holding them. Geometry remains solely in the coordinate arenas:
-- both forms store handles only, and every hit is confirmed against those
-- authoritative coordinates. The persistent form follows a published mesh;
-- the open-addressed form is the denser ingress representation used while a
-- bulk load is still claiming its vertices.
module Moonlight.Triangulation.Internal.PointIndex
  ( PointIndex
  , emptyPointIndex
  , buildPointIndex
  , pointIndexCandidates
  , lookupPointIndex
  , insertPointIndex
  , removePointIndex
  , relocatePointIndex
  , MutablePointIndex
  , MutablePointIndexUpdate (..)
  , newMutablePointIndex
  , seedMutablePointIndex
  , lookupMutablePoint
  , removeMutablePoint
  , relocateMutablePoint
  , resolveMutablePoint
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad.ST (ST)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64)
import Moonlight.Triangulation.Internal.Types (BuildError (PointIndexCapacityExhausted), Point (..))
import Moonlight.Triangulation.Internal.Paged
  ( Paged
  , pagedUnsafeIndex
  , toVector
  )
import Moonlight.Triangulation.Scalar (canonicalScalarZero)
import qualified Data.Vector.Unboxed as U

-- | Hash buckets containing only vertex handles. Callers confirm candidates
-- against the authoritative coordinate planes.
newtype PointIndex = PointIndex (IntMap.IntMap [Int])
  deriving stock (Eq, Show)

-- A point index is a memoized derived view. Forcing a triangulation forces the
-- geometry and topology that determine it, not a cache no read has demanded.
-- This is the same semantic boundary as omitting the cache from serialization.
instance NFData PointIndex where
  rnf _ = ()

emptyPointIndex :: PointIndex
emptyPointIndex = PointIndex IntMap.empty

-- | Derive an index from the authoritative structure-of-arrays geometry.
buildPointIndex :: Paged Double -> Paged Double -> PointIndex
buildPointIndex pointXs pointYs =
  U.ifoldl'
    (\index vertex x ->
       insertPointIndex x (pagedUnsafeIndex pointYs vertex) vertex index
    )
    emptyPointIndex
    (toVector pointXs)

-- | Candidate vertices sharing a position hash. Exact coordinate comparison
-- belongs to the caller that owns the coordinate reader.
pointIndexCandidates :: Double -> Double -> PointIndex -> [Int]
pointIndexCandidates x y (PointIndex buckets) =
  IntMap.findWithDefault [] (pointHash x y) buckets
{-# INLINE pointIndexCandidates #-}

-- | Resolve one exact position against the authoritative coordinate planes.
-- Hashes reject; coordinates prove. Keeping this here makes the persistent
-- removal and constraint schedules consume the same identity law as a mutable
-- session instead of paying a topological point-location walk for a key lookup.
lookupPointIndex
  :: Paged Double
  -> Paged Double
  -> PointIndex
  -> Point
  -> Maybe Int
lookupPointIndex pointXs pointYs pointIndex (Point x y) =
  foldr confirm Nothing (pointIndexCandidates x y pointIndex)
 where
  confirm candidate resolved
    | pagedUnsafeIndex pointXs candidate == x
        && pagedUnsafeIndex pointYs candidate == y = Just candidate
    | otherwise = resolved

insertPointIndex :: Double -> Double -> Int -> PointIndex -> PointIndex
insertPointIndex x y vertex (PointIndex buckets) =
  PointIndex (IntMap.insertWith (++) (pointHash x y) [vertex] buckets)
{-# INLINE insertPointIndex #-}

-- | Forget one proved handle without touching geometry. Empty collision
-- buckets disappear, so the index remains a derived finite view rather than a
-- history of retired sites.
removePointIndex :: Double -> Double -> Int -> PointIndex -> PointIndex
removePointIndex x y vertex (PointIndex buckets) =
  PointIndex (IntMap.update retainOthers (pointHash x y) buckets)
 where
  retainOthers candidates = case candidates of
    [candidate]
      | candidate == vertex -> Nothing
    _ ->
      case removeCandidate candidates of
        [] -> Nothing
        remaining -> Just remaining

  removeCandidate [] = []
  removeCandidate (candidate : remaining)
    | candidate == vertex = remaining
    | otherwise = candidate : removeCandidate remaining
{-# INLINE removePointIndex #-}

-- | Transport the tail vertex into the slot vacated by swap compaction.
-- Coordinate storage remains authoritative; this updates handles only.
relocatePointIndex
  :: Double
  -> Double
  -> Int
  -> Int
  -> PointIndex
  -> PointIndex
relocatePointIndex x y previousVertex currentVertex (PointIndex buckets) =
  PointIndex
    (IntMap.alter (Just . relocateCandidate . maybe [] id) (pointHash x y) buckets)
 where
  relocateCandidate [] = [currentVertex]
  relocateCandidate (candidate : remaining)
    | candidate == previousVertex = currentVertex : remaining
    | otherwise = candidate : relocateCandidate remaining
{-# INLINE relocatePointIndex #-}

-- The arena already holds every key, so a slot is the vertex that owns the
-- position and nothing else. Every lookup rechecks coordinates; a failed local
-- transport invalidates the table rather than letting it outlive that proof.
data MutablePointIndex s = MutablePointIndex
  { tableSlots :: !(MUV.MVector s Word32)
  , tableMask :: {-# UNPACK #-} !Int
  }

-- | A local cache transport either preserves the proof that every slot agrees
-- with the coordinate arenas, or explicitly gives that proof up. Callers must
-- fall back to the lazy immutable derivation after 'MutablePointIndexInvalidated';
-- they never publish a table whose handle correspondence was not established.
data MutablePointIndexUpdate
  = MutablePointIndexUpdated
  | MutablePointIndexInvalidated

vacant :: Word32
vacant = maxBound

-- | Size a table for a stated number of distinct positions.
newMutablePointIndex :: Int -> ST s (MutablePointIndex s)
newMutablePointIndex expected = do
  tableSlots <- MUV.replicate capacity vacant
  pure MutablePointIndex{tableSlots, tableMask = capacity - 1}
 where
  -- Two slots per key, rounded up to a power of two: linear probing stays in a
  -- short run and the mask stands in for a division.
  !capacity = grow 16
  !wanted = 2 * max 1 expected
  grow !size
    | size >= wanted = size
    | otherwise = grow (size * 2)

-- | Seed the open-addressed section from authoritative coordinates. This is
-- the shared ingress/batch operation: no point payload or parallel identity
-- store crosses the boundary.
seedMutablePointIndex
  :: MutablePointIndex s
  -> Int
  -> (Int -> ST s Double)
  -> (Int -> ST s Double)
  -> ST s (Either BuildError ())
seedMutablePointIndex table count readX readY = seed 0
 where
  seed !vertex
    | vertex >= count = pure (Right ())
    | otherwise = do
        x <- readX vertex
        y <- readY vertex
        claimed <- resolveMutablePoint table readX readY x y vertex
        case claimed of
          Left failure -> pure (Left failure)
          Right _ -> seed (vertex + 1)

-- | Look up a canonical position in the mutable section. A vacant slot proves
-- absence; all occupied candidates are confirmed against coordinate authority.
lookupMutablePoint
  :: MutablePointIndex s
  -> (Int -> ST s Double)
  -> (Int -> ST s Double)
  -> Double
  -> Double
  -> ST s (Maybe Int)
lookupMutablePoint MutablePointIndex{tableSlots, tableMask} readX readY x y =
  probe (fromIntegral (mixCoordinates x y) .&. tableMask) (tableMask + 1)
 where
  probe !slot !budget
    | budget <= 0 = pure Nothing
    | otherwise = do
        occupant <- MUV.unsafeRead tableSlots slot
        if occupant == vacant
          then pure Nothing
          else do
            heldX <- readX (fromIntegral occupant)
            heldY <- readY (fromIntegral occupant)
            if heldX == x && heldY == y
              then pure (Just (fromIntegral occupant))
              else probe ((slot + 1) .&. tableMask) (budget - 1)
{-# INLINE lookupMutablePoint #-}

-- | Forget a retired handle and repair its linear-probe cluster by backward
-- shifting only entries whose home run crosses the resulting hole. The reader
-- is needed for the surviving entries' homes; the retiring entry itself is
-- identified by handle because swap compaction may already have overwritten
-- its coordinate slot. An exhausted or absent proof invalidates the derived
-- cache instead of manufacturing a lookup result.
removeMutablePoint
  :: MutablePointIndex s
  -> (Int -> ST s Double)
  -> (Int -> ST s Double)
  -> Double
  -> Double
  -> Int
  -> ST s MutablePointIndexUpdate
removeMutablePoint MutablePointIndex{tableSlots, tableMask} readX readY x y retiredVertex =
  findRetired (fromIntegral (mixCoordinates x y) .&. tableMask) (tableMask + 1)
 where
  findRetired !slot !budget
    | budget <= 0 = pure MutablePointIndexInvalidated
    | otherwise = do
        occupant <- MUV.unsafeRead tableSlots slot
        if occupant == vacant
          then pure MutablePointIndexInvalidated
          else
            if fromIntegral occupant == retiredVertex
              then do
                MUV.unsafeWrite tableSlots slot vacant
                closeProbeHole slot ((slot + 1) .&. tableMask) (budget - 1)
              else findRetired ((slot + 1) .&. tableMask) (budget - 1)

  closeProbeHole !hole !slot !budget
    | budget <= 0 =
        MutablePointIndexInvalidated <$ MUV.unsafeWrite tableSlots hole vacant
    | otherwise = do
        occupant <- MUV.unsafeRead tableSlots slot
        if occupant == vacant
          then MutablePointIndexUpdated <$ MUV.unsafeWrite tableSlots hole vacant
          else do
            heldX <- readX (fromIntegral occupant)
            heldY <- readY (fromIntegral occupant)
            let !home = fromIntegral (mixCoordinates heldX heldY) .&. tableMask
                !distanceToHole = (hole - home) .&. tableMask
                !distanceToSlot = (slot - home) .&. tableMask
            if distanceToHole < distanceToSlot
              then do
                MUV.unsafeWrite tableSlots hole occupant
                closeProbeHole slot ((slot + 1) .&. tableMask) (budget - 1)
              else closeProbeHole hole ((slot + 1) .&. tableMask) (budget - 1)
{-# INLINE removeMutablePoint #-}

-- | Rename the tail handle after DCEL swap compaction. Failure remains a
-- typed cache obstruction, so its caller drops the table and later identity
-- questions descend from geometry rather than trusting an unproved cache.
relocateMutablePoint
  :: MutablePointIndex s
  -> Double
  -> Double
  -> Int
  -> Int
  -> ST s MutablePointIndexUpdate
relocateMutablePoint MutablePointIndex{tableSlots, tableMask} x y previousVertex currentVertex =
  findPrevious (fromIntegral (mixCoordinates x y) .&. tableMask) (tableMask + 1)
 where
  findPrevious !slot !budget
    | budget <= 0 = pure MutablePointIndexInvalidated
    | otherwise = do
        occupant <- MUV.unsafeRead tableSlots slot
        if occupant == vacant
          then pure MutablePointIndexInvalidated
          else
            if fromIntegral occupant == previousVertex
              then
                MutablePointIndexUpdated
                  <$ MUV.unsafeWrite tableSlots slot (fromIntegral currentVertex)
              else findPrevious ((slot + 1) .&. tableMask) (budget - 1)
{-# INLINE relocateMutablePoint #-}

-- | Answer the vertex already holding a canonical position, or claim the
-- position for @candidate@ and answer 'Nothing'. The position travels as raw
-- coordinates — the v'Point' constructor is the cold boundary's packaging and
-- has no business on the ingress path. The caller supplies the arena's
-- coordinate reader, so a claim is only sound if @candidate@ is the very next
-- vertex the arena will append.
resolveMutablePoint
  :: MutablePointIndex s
  -> (Int -> ST s Double)
  -> (Int -> ST s Double)
  -> Double
  -> Double
  -> Int
  -> ST s (Either BuildError (Maybe Int))
resolveMutablePoint MutablePointIndex{tableSlots, tableMask} readX readY x y candidate =
  probe (fromIntegral (mixCoordinates x y) .&. tableMask) (tableMask + 1)
 where
  probe !slot !budget
    | budget <= 0 = pure (Left (PointIndexCapacityExhausted (MUV.length tableSlots)))
    | otherwise = do
        occupant <- MUV.unsafeRead tableSlots slot
        if occupant == vacant
          then Right Nothing <$ MUV.unsafeWrite tableSlots slot (fromIntegral candidate)
          else do
            heldX <- readX (fromIntegral occupant)
            heldY <- readY (fromIntegral occupant)
            if heldX == x && heldY == y
              then pure (Right (Just (fromIntegral occupant)))
              else probe ((slot + 1) .&. tableMask) (budget - 1)

-- Inlined rather than merely specialized: the coordinate reader arrives as an
-- argument, so until the probe loop lands at its call site every collision pays
-- an unknown call and a boxed pair for a read the caller could have made
-- directly.
{-# INLINE resolveMutablePoint #-}

mixCoordinates :: Double -> Double -> Word64
mixCoordinates x y =
  mix
    ( castDoubleToWord64 (canonicalScalarZero x)
        `xor` mix (castDoubleToWord64 (canonicalScalarZero y))
    )
{-# INLINE mixCoordinates #-}

mix :: Word64 -> Word64
mix raw =
  let !z0 = raw + 0x9e3779b97f4a7c15
      !z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
      !z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
   in z2 `xor` (z2 `shiftR` 31)
{-# INLINE mix #-}

pointHash :: Double -> Double -> Int
pointHash x y = fromIntegral (mixCoordinates x y)
{-# INLINE pointHash #-}

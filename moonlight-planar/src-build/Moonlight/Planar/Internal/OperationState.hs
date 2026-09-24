{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The state one operation owns while it works: the legalization arena, the
-- shared scratch arena, and the instrumentation cells. None of it is
-- topology, so none of it lives on @MutableDcel@ — a transaction allocates
-- this record when it thaws, hands it down to the operations it runs, and
-- reads the counters back once when it freezes. The hot loops thread their
-- stack top, maximum depth and flip count as strict loop variables and charge
-- these cells once per drain; cold events (one per insertion, one per walk
-- probe) charge them where they happen.
module Moonlight.Planar.Internal.OperationState
  ( Counter (..)
  , DiagnosticMetric (..)
  , LegalizationArena (..)
  , OperationState
  , newOperationState
  , legalizationArena
  , legalizationArenaLength
  , storeLegalizationArena
  , writeScratch
  , readScratch
  , addCounter
  , setCounter
  , maxCounter
  , readCounter
  , finalizeBuildStats
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word64)
import Moonlight.Planar.Internal.Growable
  ( GrowableWord32
  , newGrowableWord32
  , readGrowable
  , writeGrowable
  )
import Moonlight.Planar.Internal.PackedIndex (packIndex, unpackIndex)
import Moonlight.Planar.Internal.BuildStats (BuildMetric, BuildStats (..), buildMetricCount)

-- | The typed candidate section owned by one operation. The newtype prevents
-- unrelated scratch vectors from being handed to the normalizer while
-- erasing to the same contiguous Word32 arena in the hot path.
newtype LegalizationArena s = LegalizationArena
  (MUV.MVector s Word32)

-- | Reported metrics use their canonical schema; diagnostics occupy a
-- separate finite suffix. Cardinality is not an inhabitable counter.
data Counter
  = Reported !BuildMetric
  | Diagnostic !DiagnosticMetric
  deriving stock (Eq, Ord, Show)

data DiagnosticMetric
  = LegalizationCandidates
  | HullBucketProbeSteps
  | HullKeyRebuilds
  deriving stock (Eq, Ord, Enum, Bounded, Show)

counterIndex :: Counter -> Int
counterIndex (Reported metric) = fromEnum metric
counterIndex (Diagnostic metric) = buildMetricCount + fromEnum metric
{-# INLINE counterIndex #-}

-- | One operation's working state. Scratch is fixed and raw: writes always
-- precede reads within an epoch. The legalization vector is held behind one
-- reference solely so an adversarial generic drain can grow it without
-- reintroducing mesh-global work state. A drain reads that reference once,
-- carries the vector and all stack metrics strictly, and stores it once when
-- finished; there is no per-candidate reference traffic.
data OperationState s = OperationState
  { osLegalizationArena :: !(STRef s (LegalizationArena s))
  , osScratchArena :: !(GrowableWord32 s)
  , osCounters :: !(MUV.MVector s Word64)
  }

-- | Allocate transaction-sized working state. The legalization reservation is a
-- starting size, not a semantic limit: a generic flip pops one candidate and may
-- push four, no linear worst-case depth follows from the input size, and
-- overflow grows the operation-owned vector by doubling. So the reservation is
-- capped. A transaction-sized one charges every singleton verb a fresh block
-- group whose tail no drain reaches, and the doublings that reach a real peak
-- copy less in total than reserving that tail costs.
--
-- Scratch retains four disjoint half-edge sections as its semantic limit: the
-- removal kernel locally glues border, retired-edge, retired-face, and new-fan
-- sections there. Its physical storage still grows only with the star or strip
-- actually observed. Reserving the full mesh bound made every singleton
-- persistent edit allocate an arena whose untouched tail was orders of
-- magnitude larger than the edit.
newOperationState :: Int -> ST s (OperationState s)
newOperationState halfEdgeCapacity = do
  initialArena <- LegalizationArena <$> MUV.new (min (2 * halfEdgeCapacity + 64) initialLegalizationReservation)
  arena <- newSTRef initialArena
  scratch <- newGrowableWord32 (min 64 (halfEdgeCapacity + 8))
  counters <- MUV.replicate (buildMetricCount + fromEnum (maxBound :: DiagnosticMetric) + 1) 0
  pure
    OperationState
      { osLegalizationArena = arena
      , osScratchArena = scratch
      , osCounters = counters
      }

-- | Ordinary cavities are tiny; exceptional stars and recovered strips grow
-- geometrically behind the sealed arena rather than taxing every singleton
-- edit for a pathological frontier it never visits.
initialLegalizationReservation :: Int
initialLegalizationReservation = 64

legalizationArena :: OperationState s -> ST s (LegalizationArena s)
legalizationArena = readSTRef . osLegalizationArena
{-# INLINE legalizationArena #-}

legalizationArenaLength :: LegalizationArena s -> Int
legalizationArenaLength (LegalizationArena values) = MUV.length values
{-# INLINE legalizationArenaLength #-}

storeLegalizationArena :: OperationState s -> LegalizationArena s -> ST s ()
storeLegalizationArena = writeSTRef . osLegalizationArena
{-# INLINE storeLegalizationArena #-}

-- | Write a scratch cell. Collection walks carry their topology-derived
-- termination budgets; the growable arena is physical storage, not a second
-- semantic bound capable of disagreeing with those typed obstructions.
writeScratch :: OperationState s -> Int -> Int -> ST s ()
writeScratch OperationState{osScratchArena} index value =
  writeGrowable osScratchArena index (packIndex value)
{-# INLINE writeScratch #-}

readScratch :: OperationState s -> Int -> ST s Int
readScratch OperationState{osScratchArena} index = unpackIndex <$> readGrowable osScratchArena index
{-# INLINE readScratch #-}

addCounter :: OperationState s -> Counter -> Int -> ST s ()
addCounter OperationState{osCounters} counter amount = do
  let !index = counterIndex counter
  current <- MUV.unsafeRead osCounters index
  MUV.unsafeWrite osCounters index (current + fromIntegral amount)
{-# INLINE addCounter #-}

setCounter :: OperationState s -> Counter -> Int -> ST s ()
setCounter OperationState{osCounters} counter value =
  MUV.unsafeWrite osCounters (counterIndex counter) (fromIntegral value)
{-# INLINE setCounter #-}

maxCounter :: OperationState s -> Counter -> Int -> ST s ()
maxCounter OperationState{osCounters} counter value = do
  let !index = counterIndex counter
  current <- MUV.unsafeRead osCounters index
  when (fromIntegral value > current) (MUV.unsafeWrite osCounters index (fromIntegral value))
{-# INLINE maxCounter #-}

readCounter :: OperationState s -> Counter -> ST s Int
readCounter OperationState{osCounters} counter =
  fromIntegral <$> MUV.unsafeRead osCounters (counterIndex counter)
{-# INLINE readCounter #-}

-- | Transfer the reported prefix at the completed operation's publication
-- boundary. Every caller has finished charging counters and never uses the
-- operation again. The immutable result escapes ST; its mutable owner cannot.
finalizeBuildStats :: OperationState s -> ST s BuildStats
finalizeBuildStats OperationState{osCounters} =
  BuildStats <$> U.unsafeFreeze (MUV.slice 0 buildMetricCount osCounters)
{-# INLINE finalizeBuildStats #-}

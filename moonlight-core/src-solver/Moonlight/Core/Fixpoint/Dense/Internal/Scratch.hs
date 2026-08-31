-- | The graph-agnostic mutable workspace for adaptive reachability: the
-- double-buffered sparse/dense frontier scratch arena, generation-stamped visit
-- marks, frontier folds/membership, and the strict monadic fold primitive. This
-- is the rawest ST stratum — it names neither 'Csr' nor 'GraphSnapshot'.
module Moonlight.Core.Fixpoint.Dense.Internal.Scratch
  ( ScratchSide (..),
    Frontier (..),
    ReachabilityScratch,
    newReachabilityScratch,
    nextReachabilityGeneration,
    markVectorFresh,
    appendFreshTarget,
    appendSparseBuffer,
    writeDenseFrontierM,
    flipScratchSide,
    markVisited,
    isMarked,
    visitedMarksToIntSet,
    strictFoldM,
    frontierEmpty,
    frontierSize,
    frontierMemberM,
    frontierContainsAnyM,
    frontierFoldM',
  )
where

import Control.Monad.ST (ST)
import Data.Bits (setBit, testBit)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed.Mutable qualified as UM
import Data.Word (Word32, Word64)
import Moonlight.Core.Fixpoint.Dense.Internal.AdaptiveIntSet (wordBits, wordCountForSize)
import Prelude

data ScratchSide
  = ScratchA
  | ScratchB
  deriving stock (Eq, Show)

data Frontier
  = SparseFrontier !ScratchSide !Int
  | DenseFrontier !ScratchSide !Int
  deriving stock (Eq, Show)

type ReachabilityScratch :: Type -> Type
data ReachabilityScratch state = ReachabilityScratch
  { visitedGeneration :: !(UM.MVector state Word32),
    currentGeneration :: !(STRef state Word32),
    sparseA :: !(UM.MVector state Int),
    sparseB :: !(UM.MVector state Int),
    denseA :: !(UM.MVector state Word64),
    denseB :: !(UM.MVector state Word64)
  }

newReachabilityScratch :: Int -> ST state (ReachabilityScratch state)
newReachabilityScratch vertexCount = do
  visited <- UM.replicate (max 0 vertexCount) 0
  firstSparse <- UM.replicate (max 0 vertexCount) 0
  secondSparse <- UM.replicate (max 0 vertexCount) 0
  firstDense <- UM.replicate (wordCountForSize vertexCount) 0
  secondDense <- UM.replicate (wordCountForSize vertexCount) 0
  generation <- newSTRef 0
  pure
    ReachabilityScratch
      { visitedGeneration = visited,
        currentGeneration = generation,
        sparseA = firstSparse,
        sparseB = secondSparse,
        denseA = firstDense,
        denseB = secondDense
      }
{-# INLINE newReachabilityScratch #-}

nextReachabilityGeneration :: ReachabilityScratch state -> ST state Word32
nextReachabilityGeneration scratch = do
  previous <- readSTRef (currentGeneration scratch)
  let next = previous + 1
  if next == 0
    then do
      UM.set (visitedGeneration scratch) 0
      writeSTRef (currentGeneration scratch) 1
      pure 1
    else do
      writeSTRef (currentGeneration scratch) next
      pure next
{-# INLINE nextReachabilityGeneration #-}

frontierEmpty :: Frontier -> Bool
frontierEmpty frontier =
  case frontier of
    SparseFrontier _ frontierLength -> frontierLength <= 0
    DenseFrontier _ frontierLength -> frontierLength <= 0
{-# INLINE frontierEmpty #-}

frontierSize :: Frontier -> Int
frontierSize frontier =
  case frontier of
    SparseFrontier _ frontierLength -> frontierLength
    DenseFrontier _ frontierLength -> frontierLength
{-# INLINE frontierSize #-}

frontierMemberM :: ReachabilityScratch state -> Frontier -> Int -> ST state Bool
frontierMemberM scratch frontier key =
  case frontier of
    SparseFrontier side frontierLength ->
      sparseFrontierMemberM (sparseBufferFor side scratch) frontierLength key
    DenseFrontier side _ ->
      denseFrontierMemberM (denseBufferFor side scratch) key
{-# INLINE frontierMemberM #-}

frontierContainsAnyM :: ReachabilityScratch state -> Frontier -> Vector Int -> ST state Bool
frontierContainsAnyM scratch frontier =
  U.foldr
    ( \key remaining -> do
        found <- frontierMemberM scratch frontier key
        if found then pure True else remaining
    )
    (pure False)
{-# INLINE frontierContainsAnyM #-}

frontierFoldM' :: ReachabilityScratch state -> (result -> Int -> ST state result) -> result -> Frontier -> ST state result
frontierFoldM' scratch step initial frontier =
  case frontier of
    SparseFrontier side frontierLength ->
      sparseFrontierFoldM' (sparseBufferFor side scratch) frontierLength step initial
    DenseFrontier side _ ->
      denseFrontierFoldM' (denseBufferFor side scratch) step initial
{-# INLINE frontierFoldM' #-}

markVectorFresh :: ReachabilityScratch state -> Word32 -> ScratchSide -> Vector Int -> ST state Int
markVectorFresh scratch generation targetSide =
  U.foldM' mark 0
  where
    mark freshLength key = do
      marked <- markFresh scratch generation key
      if marked
        then appendSparseBuffer scratch targetSide freshLength key
        else pure freshLength
{-# INLINE markVectorFresh #-}

appendFreshTarget :: ReachabilityScratch state -> Word32 -> ScratchSide -> Int -> Int -> ST state Int
appendFreshTarget scratch generation targetSide freshLength target = do
  marked <- markFresh scratch generation target
  if marked
    then appendSparseBuffer scratch targetSide freshLength target
    else pure freshLength
{-# INLINE appendFreshTarget #-}

appendSparseBuffer :: ReachabilityScratch state -> ScratchSide -> Int -> Int -> ST state Int
appendSparseBuffer scratch targetSide index value
  | index >= UM.length target = pure index
  | otherwise = do
      UM.write target index value
      pure (index + 1)
  where
    target =
      sparseBufferFor targetSide scratch
{-# INLINE appendSparseBuffer #-}

writeDenseFrontierM :: ReachabilityScratch state -> ScratchSide -> ScratchSide -> Int -> ST state ()
writeDenseFrontierM scratch sourceSide targetSide frontierLength = do
  UM.set target 0
  strictFoldM
    ( \() index -> do
        value <- UM.read source index
        setDenseBit target value
    )
    ()
    [0 .. frontierLength - 1]
  where
    source =
      sparseBufferFor sourceSide scratch
    target =
      denseBufferFor targetSide scratch
{-# INLINE writeDenseFrontierM #-}

sparseFrontierFoldM' :: UM.MVector state Int -> Int -> (result -> Int -> ST state result) -> result -> ST state result
sparseFrontierFoldM' sparse frontierLength step initial =
  strictFoldM
    ( \acc index -> do
        value <- UM.read sparse index
        step acc value
    )
    initial
    [0 .. frontierLength - 1]
{-# INLINE sparseFrontierFoldM' #-}

denseFrontierFoldM' :: UM.MVector state Word64 -> (result -> Int -> ST state result) -> result -> ST state result
denseFrontierFoldM' dense step initial =
  strictFoldM foldWord initial [0 .. UM.length dense - 1]
  where
    foldWord acc wordIndex = do
      word <- UM.read dense wordIndex
      strictFoldM
        ( \current bitIndex ->
            if testBit word bitIndex
              then step current (wordIndex * wordBits + bitIndex)
              else pure current
        )
        acc
        [0 .. wordBits - 1]
{-# INLINE denseFrontierFoldM' #-}

sparseFrontierMemberM :: UM.MVector state Int -> Int -> Int -> ST state Bool
sparseFrontierMemberM sparse frontierLength key =
  foldr
    ( \index remaining -> do
        found <- (== key) <$> UM.read sparse index
        if found then pure True else remaining
    )
    (pure False)
    [0 .. frontierLength - 1]
{-# INLINE sparseFrontierMemberM #-}

denseFrontierMemberM :: UM.MVector state Word64 -> Int -> ST state Bool
denseFrontierMemberM dense key
  | key < 0 = pure False
  | wordIndex >= UM.length dense = pure False
  | otherwise = do
      word <- UM.read dense wordIndex
      pure (testBit word bitIndex)
  where
    wordIndex =
      key `quot` wordBits
    bitIndex =
      key `rem` wordBits
{-# INLINE denseFrontierMemberM #-}

sparseBufferFor :: ScratchSide -> ReachabilityScratch state -> UM.MVector state Int
sparseBufferFor side scratch =
  case side of
    ScratchA -> sparseA scratch
    ScratchB -> sparseB scratch
{-# INLINE sparseBufferFor #-}

denseBufferFor :: ScratchSide -> ReachabilityScratch state -> UM.MVector state Word64
denseBufferFor side scratch =
  case side of
    ScratchA -> denseA scratch
    ScratchB -> denseB scratch
{-# INLINE denseBufferFor #-}

flipScratchSide :: ScratchSide -> ScratchSide
flipScratchSide side =
  case side of
    ScratchA -> ScratchB
    ScratchB -> ScratchA
{-# INLINE flipScratchSide #-}

markFresh :: ReachabilityScratch state -> Word32 -> Int -> ST state Bool
markFresh scratch generation key
  | key < 0 = pure False
  | key >= UM.length (visitedGeneration scratch) = pure False
  | otherwise = do
      seen <- isMarked scratch generation key
      if seen
        then pure False
        else markVisited scratch generation key *> pure True
{-# INLINE markFresh #-}

markVisited :: ReachabilityScratch state -> Word32 -> Int -> ST state ()
markVisited scratch generation key =
  UM.write (visitedGeneration scratch) key generation
{-# INLINE markVisited #-}

isMarked :: ReachabilityScratch state -> Word32 -> Int -> ST state Bool
isMarked scratch generation key =
  (== generation) <$> UM.read (visitedGeneration scratch) key
{-# INLINE isMarked #-}

visitedMarksToIntSet :: ReachabilityScratch state -> Word32 -> ST state IntSet
visitedMarksToIntSet scratch generation =
  IntSet.fromDistinctAscList . reverse
    <$> strictFoldM step [] [0 .. UM.length (visitedGeneration scratch) - 1]
  where
    step keys key = do
      seen <- isMarked scratch generation key
      pure (if seen then key : keys else keys)
{-# INLINE visitedMarksToIntSet #-}

setDenseBit :: UM.MVector state Word64 -> Int -> ST state ()
setDenseBit bitmap key
  | key < 0 = pure ()
  | wordIndex >= UM.length bitmap = pure ()
  | otherwise = do
      word <- UM.read bitmap wordIndex
      UM.write bitmap wordIndex (setBit word bitIndex)
  where
    wordIndex =
      key `quot` wordBits
    bitIndex =
      key `rem` wordBits
{-# INLINE setDenseBit #-}

strictFoldM :: (Monad m) => (accumulator -> item -> m accumulator) -> accumulator -> [item] -> m accumulator
strictFoldM step initial items =
  initial `seq` go initial items
  where
    go accumulator remaining =
      case remaining of
        [] ->
          pure accumulator
        item : rest -> do
          next <- step accumulator item
          next `seq` go next rest
{-# INLINE strictFoldM #-}

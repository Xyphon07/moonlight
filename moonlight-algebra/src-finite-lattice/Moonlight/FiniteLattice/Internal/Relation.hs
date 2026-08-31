{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Relation
  ( ContextRows,
    ContextRowIndex,
    relationRowsGenerate,
    relationRowsFromKeyPairs,
    transitiveClosureRows,
    lowerRowsFromUpperRows,
    contextRowIndexFromRows,
    contextRowIndexLookup,
    rowJoinKeyMaybe,
    rowMeetKeyMaybe,
    rowJoinCandidateKeys,
    rowMeetCandidateKeys,
    contextKeyRelated,
    rowForKey,
    rowForRawKey,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits
  ( (.&.),
    (.|.),
    bit,
    countTrailingZeros,
  )
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as UVector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Data.Word (Word64)
import Moonlight.FiniteLattice.Internal.Invariant
  ( unboxedIndexInvariant,
  )
import Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet (..),
    contextKeySetChunkCount,
    contextKeySetFilter,
    contextKeySetFoldr,
    contextKeySetIntersection,
    contextKeySetIntersectsExcept,
    contextKeySetMember,
    contextKeySetToAscList,
  )
import Moonlight.FiniteLattice.Internal.Topological
  ( topologicalOrder,
  )

data ContextRows = ContextRows
  { crSize :: !Int,
    crChunkCount :: !Int,
    crChunks :: !(UVector.Vector Word64)
  }
  deriving stock (Eq, Show)

data ContextRowSignature
  = ContextRowSignature0
  | ContextRowSignature1 !Word64
  | ContextRowSignature2 !Word64 !Word64
  | ContextRowSignatureN ![Word64]
  deriving stock (Eq, Ord, Show)

newtype ContextRowIndex = ContextRowIndex
  (Map ContextRowSignature ContextKey)

-- | Build a relation matrix while evaluating the predicate exactly once for
-- each ordered pair of keys.
relationRowsGenerate :: Int -> (Int -> Int -> Bool) -> ContextRows
relationRowsGenerate size related =
  ContextRows
    { crSize = size,
      crChunkCount = chunkCount,
      crChunks =
        UVector.generate relationWordCount $ \flatIndex ->
          let (sourceOrdinal, chunkIndex) = flatIndex `quotRem` chunkCount
              targetBase = chunkIndex * contextKeyBitsPerChunk
           in generateChunk sourceOrdinal targetBase 0 0
    }
  where
    chunkCount = contextKeySetChunkCount size
    relationWordCount = size * chunkCount

    generateChunk !sourceOrdinal !targetBase !bitIndex !chunkValue
      | bitIndex >= contextKeyBitsPerChunk = chunkValue
      | targetOrdinal >= size = chunkValue
      | otherwise =
          generateChunk
            sourceOrdinal
            targetBase
            (bitIndex + 1)
            ( if related sourceOrdinal targetOrdinal
                then chunkValue .|. bit bitIndex
                else chunkValue
            )
      where
        targetOrdinal = targetBase + bitIndex

relationRowsFromKeyPairs :: Int -> [(ContextKey, ContextKey)] -> ContextRows
relationRowsFromKeyPairs size keyPairs =
  ContextRows
    { crSize = size,
      crChunkCount = chunkCount,
      crChunks =
        UVector.accum
          (.|.)
          (UVector.replicate (size * chunkCount) 0)
          rowBits
    }
  where
    chunkCount = contextKeySetChunkCount size

    rowBits =
      [ ( contextRowsChunkOffset chunkCount sourceOrdinal targetChunk,
          contextKeyBitMask targetOrdinal
        )
      | (ContextKey sourceOrdinal, ContextKey targetOrdinal) <- keyPairs,
        sourceOrdinal >= 0,
        sourceOrdinal < size,
        targetOrdinal >= 0,
        targetOrdinal < size,
        let targetChunk = contextKeyChunkIndex targetOrdinal
      ]

transitiveClosureRows :: ContextRows -> ContextRows
transitiveClosureRows initialRows =
  case topologicalKeyOrder initialRows of
    Just order -> dagTransitiveClosureRows initialRows order
    Nothing -> warshallTransitiveClosureRows initialRows

warshallTransitiveClosureRows :: ContextRows -> ContextRows
warshallTransitiveClosureRows rows =
  rows
    { crChunks =
        runST $ do
          mutableChunks <- UVector.thaw (crChunks rows)
          closeThrough mutableChunks 0
          UVector.unsafeFreeze mutableChunks
    }
  where
    size = crSize rows
    chunkCount = crChunkCount rows

    closeThrough :: MVector.MVector s Word64 -> Int -> ST s ()
    closeThrough mutableChunks !throughOrdinal
      | throughOrdinal >= size =
          pure ()
      | otherwise = do
          closeSource mutableChunks throughOrdinal 0
          closeThrough mutableChunks (throughOrdinal + 1)

    closeSource :: MVector.MVector s Word64 -> Int -> Int -> ST s ()
    closeSource mutableChunks !throughOrdinal !sourceOrdinal
      | sourceOrdinal >= size =
          pure ()
      | otherwise = do
          let throughChunkIndex =
                contextKeyChunkIndex throughOrdinal
              throughMask =
                contextKeyBitMask throughOrdinal
              reachabilityOffset =
                contextRowsChunkOffset
                  chunkCount
                  sourceOrdinal
                  throughChunkIndex

          sourceReachesThrough <-
            MVector.unsafeRead mutableChunks reachabilityOffset

          when
            (sourceReachesThrough .&. throughMask /= 0)
            (orMutableRow mutableChunks sourceOrdinal throughOrdinal)

          closeSource
            mutableChunks
            throughOrdinal
            (sourceOrdinal + 1)

    orMutableRow :: MVector.MVector s Word64 -> Int -> Int -> ST s ()
    orMutableRow mutableChunks !destinationOrdinal !sourceOrdinal =
      sequence_
        [ do
            let destinationOffset =
                  contextRowsChunkOffset
                    chunkCount
                    destinationOrdinal
                    chunkIndex
                sourceOffset =
                  contextRowsChunkOffset
                    chunkCount
                    sourceOrdinal
                    chunkIndex
            destinationChunk <-
              MVector.unsafeRead mutableChunks destinationOffset
            sourceChunk <-
              MVector.unsafeRead mutableChunks sourceOffset
            MVector.unsafeWrite
              mutableChunks
              destinationOffset
              (destinationChunk .|. sourceChunk)
        | chunkIndex <- [0 .. chunkCount - 1]
        ]

-- | For a DAG, reverse topological propagation computes every principal upper
-- set by joining already-computed successor rows.
dagTransitiveClosureRows :: ContextRows -> [Int] -> ContextRows
dagTransitiveClosureRows rows keyOrder =
  rows
    { crChunks =
        runST $ do
          mutableChunks <- UVector.thaw (crChunks rows)
          traverse_ (closeSource mutableChunks) (reverse keyOrder)
          UVector.unsafeFreeze mutableChunks
    }
  where
    chunkCount = crChunkCount rows

    closeSource :: MVector.MVector s Word64 -> Int -> ST s ()
    closeSource mutableChunks sourceOrdinal =
      forEachRelatedTarget rows sourceOrdinal $ \targetOrdinal ->
        when
          (targetOrdinal /= sourceOrdinal)
          (orMutableRow mutableChunks sourceOrdinal targetOrdinal)

    orMutableRow :: MVector.MVector s Word64 -> Int -> Int -> ST s ()
    orMutableRow mutableChunks !destinationOrdinal !sourceOrdinal =
      sequence_
        [ do
            let destinationOffset =
                  contextRowsChunkOffset
                    chunkCount
                    destinationOrdinal
                    chunkIndex
                sourceOffset =
                  contextRowsChunkOffset
                    chunkCount
                    sourceOrdinal
                    chunkIndex
            destinationChunk <-
              MVector.unsafeRead mutableChunks destinationOffset
            sourceChunk <-
              MVector.unsafeRead mutableChunks sourceOffset
            MVector.unsafeWrite
              mutableChunks
              destinationOffset
              (destinationChunk .|. sourceChunk)
        | chunkIndex <- [0 .. chunkCount - 1]
        ]

forEachRelatedTarget ::
  Monad m =>
  ContextRows ->
  Int ->
  (Int -> m ()) ->
  m ()
forEachRelatedTarget rows sourceOrdinal action =
  traverse_ visitChunk [0 .. crChunkCount rows - 1]
  where
    visitChunk chunkIndex =
      visitWord
        (chunkIndex * contextKeyBitsPerChunk)
        (contextRowsChunkAt rows sourceOrdinal chunkIndex)

    visitWord !baseOrdinal !remainingBits
      | remainingBits == 0 =
          pure ()
      | otherwise = do
          let bitIndex = countTrailingZeros remainingBits
              targetOrdinal = baseOrdinal + bitIndex
              nextBits = remainingBits .&. (remainingBits - 1)
          action targetOrdinal
          visitWord baseOrdinal nextBits
{-# INLINE forEachRelatedTarget #-}

lowerRowsFromUpperRows :: ContextRows -> ContextRows
lowerRowsFromUpperRows upperRows =
  ContextRows
    { crSize = crSize upperRows,
      crChunkCount = crChunkCount upperRows,
      crChunks =
        UVector.generate
          (crSize upperRows * crChunkCount upperRows)
          lowerChunk
    }
  where
    lowerChunk flatIndex =
      let (targetOrdinal, chunkIndex) = flatIndex `quotRem` crChunkCount upperRows
          sourceBase = chunkIndex * contextKeyBitsPerChunk
       in generateChunk targetOrdinal sourceBase 0 0

    generateChunk !targetOrdinal !sourceBase !bitIndex !chunkValue
      | bitIndex >= contextKeyBitsPerChunk = chunkValue
      | sourceOrdinal >= crSize upperRows = chunkValue
      | contextKeyRelated upperRows (ContextKey sourceOrdinal) (ContextKey targetOrdinal) =
          generateChunk targetOrdinal sourceBase (bitIndex + 1) (chunkValue .|. bit bitIndex)
      | otherwise =
          generateChunk targetOrdinal sourceBase (bitIndex + 1) chunkValue
      where
        sourceOrdinal = sourceBase + bitIndex

contextRowIndexFromRows :: ContextRows -> ContextRowIndex
contextRowIndexFromRows rows =
  ContextRowIndex
    ( Map.fromList
        [ (contextRowSignature (rowForRawKey rows keyOrdinal), ContextKey keyOrdinal)
        | keyOrdinal <- [0 .. crSize rows - 1]
        ]
    )

contextRowIndexLookup :: ContextKeySet -> ContextRowIndex -> Maybe ContextKey
contextRowIndexLookup keySet (ContextRowIndex rowIndex) =
  Map.lookup (contextRowSignature keySet) rowIndex

contextKeyRelated :: ContextRows -> ContextKey -> ContextKey -> Bool
contextKeyRelated rows leftKey (ContextKey rightOrdinal) =
  contextKeySetMember rightOrdinal (rowForKey rows leftKey)
{-# INLINE contextKeyRelated #-}

rowJoinKeyMaybe ::
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextKey ->
  ContextKey ->
  Maybe ContextKey
rowJoinKeyMaybe upperRows lowerRows upperRowIndex leftKey rightKey =
  case contextRowIndexLookup upperBounds upperRowIndex of
    Just joinKey -> Just joinKey
    Nothing -> uniqueKey (minimalRowKeys lowerRows upperBounds)
  where
    upperBounds =
      contextKeySetIntersection
        (rowForKey upperRows leftKey)
        (rowForKey upperRows rightKey)
{-# INLINE rowJoinKeyMaybe #-}

rowMeetKeyMaybe ::
  ContextRows ->
  ContextRows ->
  ContextRowIndex ->
  ContextKey ->
  ContextKey ->
  Maybe ContextKey
rowMeetKeyMaybe upperRows lowerRows lowerRowIndex leftKey rightKey =
  case contextRowIndexLookup lowerBounds lowerRowIndex of
    Just meetKey -> Just meetKey
    Nothing -> uniqueKey (maximalRowKeys upperRows lowerBounds)
  where
    lowerBounds =
      contextKeySetIntersection
        (rowForKey lowerRows leftKey)
        (rowForKey lowerRows rightKey)
{-# INLINE rowMeetKeyMaybe #-}

rowJoinCandidateKeys ::
  ContextRows ->
  ContextRows ->
  ContextKey ->
  ContextKey ->
  ContextKeySet
rowJoinCandidateKeys upperRows lowerRows leftKey rightKey =
  minimalRowKeys lowerRows upperBounds
  where
    upperBounds =
      contextKeySetIntersection
        (rowForKey upperRows leftKey)
        (rowForKey upperRows rightKey)

rowMeetCandidateKeys ::
  ContextRows ->
  ContextRows ->
  ContextKey ->
  ContextKey ->
  ContextKeySet
rowMeetCandidateKeys upperRows lowerRows leftKey rightKey =
  maximalRowKeys upperRows lowerBounds
  where
    lowerBounds =
      contextKeySetIntersection
        (rowForKey lowerRows leftKey)
        (rowForKey lowerRows rightKey)

minimalRowKeys :: ContextRows -> ContextKeySet -> ContextKeySet
minimalRowKeys lowerRows candidates =
  contextKeySetFilter
    (\candidateOrdinal ->
       not
         ( contextKeySetIntersectsExcept
             candidateOrdinal
             candidates
             (rowForRawKey lowerRows candidateOrdinal)
         )
    )
    candidates

maximalRowKeys :: ContextRows -> ContextKeySet -> ContextKeySet
maximalRowKeys upperRows candidates =
  contextKeySetFilter
    (\candidateOrdinal ->
       not
         ( contextKeySetIntersectsExcept
             candidateOrdinal
             candidates
             (rowForRawKey upperRows candidateOrdinal)
         )
    )
    candidates

uniqueKey :: ContextKeySet -> Maybe ContextKey
uniqueKey candidates =
  case contextKeySetToAscList candidates of
    [keyOrdinal] -> Just (ContextKey keyOrdinal)
    _ -> Nothing

-- | Total for keys produced by the same compiled relation.
rowForKey :: ContextRows -> ContextKey -> ContextKeySet
rowForKey rows (ContextKey keyOrdinal) =
  rowForRawKey rows keyOrdinal
{-# INLINE rowForKey #-}

-- | Total for ordinals in @[0, crSize)@. Every caller is an internal bounded
-- loop or starts from an abstract key.
rowForRawKey :: ContextRows -> Int -> ContextKeySet
rowForRawKey rows keyOrdinal =
  ContextKeySet
    ( UVector.slice
        (contextRowsChunkOffset (crChunkCount rows) keyOrdinal 0)
        (crChunkCount rows)
        (crChunks rows)
    )
{-# INLINE rowForRawKey #-}

topologicalKeyOrder :: ContextRows -> Maybe [Int]
topologicalKeyOrder rows =
  topologicalOrder (crSize rows) $ \sourceOrdinal step initial ->
    contextKeySetFoldr
      (\targetOrdinal rest -> step targetOrdinal rest)
      initial
      (rowForRawKey rows sourceOrdinal)

contextRowSignature :: ContextKeySet -> ContextRowSignature
contextRowSignature (ContextKeySet chunks) =
  case UVector.length chunks of
    0 -> ContextRowSignature0
    1 -> ContextRowSignature1 (unboxedIndexInvariant chunks 0)
    2 ->
      ContextRowSignature2
        (unboxedIndexInvariant chunks 0)
        (unboxedIndexInvariant chunks 1)
    _ -> ContextRowSignatureN (UVector.toList chunks)

contextRowsChunkAt :: ContextRows -> Int -> Int -> Word64
contextRowsChunkAt rows rowOrdinal chunkIndex =
  unboxedIndexInvariant (crChunks rows) (contextRowsChunkOffset (crChunkCount rows) rowOrdinal chunkIndex)

contextRowsChunkOffset :: Int -> Int -> Int -> Int
contextRowsChunkOffset chunkCount rowOrdinal chunkIndex =
  rowOrdinal * chunkCount + chunkIndex
{-# INLINE contextRowsChunkOffset #-}

contextKeyChunkIndex :: Int -> Int
contextKeyChunkIndex keyOrdinal =
  keyOrdinal `quot` contextKeyBitsPerChunk
{-# INLINE contextKeyChunkIndex #-}

contextKeyBitMask :: Int -> Word64
contextKeyBitMask keyOrdinal =
  bit (keyOrdinal .&. (contextKeyBitsPerChunk - 1))
{-# INLINE contextKeyBitMask #-}

contextKeyBitsPerChunk :: Int
contextKeyBitsPerChunk = 64

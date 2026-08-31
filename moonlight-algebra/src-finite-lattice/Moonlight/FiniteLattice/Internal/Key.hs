{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GHC2024 #-}

module Moonlight.FiniteLattice.Internal.Key
  ( ContextKey (..),
    ContextKeySet (..),
    contextKeySetEmpty,
    contextKeySetAll,
    contextKeySetSingleton,
    contextKeySetFromKeys,
    contextKeySetUnion,
    contextKeySetIntersection,
    contextKeySetIntersects,
    contextKeySetUnionImages,
    contextKeySetImage2,
    contextKeySetDifference,
    contextKeySetDelete,
    contextKeySetMember,
    contextKeySetNull,
    contextKeySetCardinality,
    contextKeySetFilter,
    contextKeySetFind,
    contextKeySetFindDifference,
    contextKeySetIntersectsExcept,
    contextKeySetFoldr,
    contextKeySetToAscList,
    contextKeySetChunkCount,
    ContextKeyTable,
    contextKeyTableGenerateM,
    contextKeyTableLookup,
    pairKeyOffset,
  )
where

import Data.Bits
  ( (.&.),
    (.|.),
    bit,
    complement,
    countTrailingZeros,
    popCount,
  )
import Data.Kind (Type)
import Data.Vector.Unboxed qualified as UVector
import Data.Word (Word64)
import Moonlight.FiniteLattice.Internal.Invariant
  ( unboxedIndexInvariant,
  )

type ContextKey :: Type
newtype ContextKey = ContextKey
  { contextKeyOrdinal :: Int
  }
  deriving stock (Eq, Ord, Show)

type ContextKeySet :: Type
newtype ContextKeySet = ContextKeySet
  { contextKeySetChunks :: UVector.Vector Word64
  }
  deriving stock (Eq, Show)

contextKeySetEmpty :: Int -> ContextKeySet
contextKeySetEmpty chunkCount =
  ContextKeySet (UVector.replicate chunkCount 0)

contextKeySetAll :: Int -> ContextKeySet
contextKeySetAll size
  | size <= 0 =
      ContextKeySet UVector.empty
  | otherwise =
      ContextKeySet
        ( UVector.generate chunkCount $ \chunkIndex ->
            if chunkIndex == chunkCount - 1
              then finalChunk
              else maxBound
        )
  where
    chunkCount = contextKeySetChunkCount size
    finalBitCount = size .&. (contextKeyBitsPerChunk - 1)
    finalChunk
      | finalBitCount == 0 = maxBound
      | otherwise = bit finalBitCount - 1

contextKeySetSingleton :: Int -> ContextKey -> ContextKeySet
contextKeySetSingleton chunkCount (ContextKey keyOrdinal) =
  contextKeySetFromKeys chunkCount [keyOrdinal]

contextKeySetFromKeys :: Int -> [Int] -> ContextKeySet
contextKeySetFromKeys chunkCount keyOrdinals =
  ContextKeySet
    ( UVector.accum
        (.|.)
        (UVector.replicate chunkCount 0)
        [ (contextKeyChunkIndex keyOrdinal, contextKeyBitMask keyOrdinal)
        | keyOrdinal <- keyOrdinals,
          keyOrdinal >= 0,
          contextKeyChunkIndex keyOrdinal < chunkCount
        ]
    )

contextKeySetUnion :: ContextKeySet -> ContextKeySet -> ContextKeySet
contextKeySetUnion =
  contextKeySetZipWith (.|.)
{-# INLINE contextKeySetUnion #-}

contextKeySetIntersection :: ContextKeySet -> ContextKeySet -> ContextKeySet
contextKeySetIntersection =
  contextKeySetZipWith (.&.)
{-# INLINE contextKeySetIntersection #-}

contextKeySetIntersects :: ContextKeySet -> ContextKeySet -> Bool
contextKeySetIntersects left right =
  not (contextKeySetNull (contextKeySetIntersection left right))
{-# INLINE contextKeySetIntersects #-}

contextKeySetUnionImages ::
  Int ->
  (Int -> ContextKeySet) ->
  ContextKeySet ->
  ContextKeySet
contextKeySetUnionImages chunkCount image =
  contextKeySetFoldr
    (contextKeySetUnion . image)
    (contextKeySetEmpty chunkCount)

contextKeySetImage2 ::
  Int ->
  (Int -> Int -> Int) ->
  ContextKeySet ->
  ContextKeySet ->
  ContextKeySet
contextKeySetImage2 chunkCount combine leftKeys rightKeys =
  contextKeySetFromKeys chunkCount
    [ combine leftOrdinal rightOrdinal
    | leftOrdinal <- contextKeySetToAscList leftKeys,
      rightOrdinal <- contextKeySetToAscList rightKeys
    ]

contextKeySetDifference :: ContextKeySet -> ContextKeySet -> ContextKeySet
contextKeySetDifference =
  contextKeySetZipWith (\leftChunk rightChunk -> leftChunk .&. complement rightChunk)
{-# INLINE contextKeySetDifference #-}

contextKeySetDelete :: Int -> ContextKeySet -> ContextKeySet
contextKeySetDelete keyOrdinal keySet@(ContextKeySet chunks)
  | keyOrdinal < 0 || chunkIndex >= UVector.length chunks = keySet
  | otherwise =
      ContextKeySet
        ( UVector.imap
            ( \currentChunkIndex chunk ->
                if currentChunkIndex == chunkIndex
                  then chunk .&. complement mask
                  else chunk
            )
            chunks
        )
  where
    chunkIndex = contextKeyChunkIndex keyOrdinal
    mask = contextKeyBitMask keyOrdinal

contextKeySetMember :: Int -> ContextKeySet -> Bool
contextKeySetMember keyOrdinal (ContextKeySet chunks)
  | keyOrdinal < 0 = False
  | chunkIndex >= UVector.length chunks = False
  | otherwise =
      UVector.unsafeIndex chunks chunkIndex .&. contextKeyBitMask keyOrdinal /= 0
  where
    chunkIndex = contextKeyChunkIndex keyOrdinal
{-# INLINE contextKeySetMember #-}

contextKeySetNull :: ContextKeySet -> Bool
contextKeySetNull (ContextKeySet chunks) =
  UVector.all (== 0) chunks

contextKeySetCardinality :: ContextKeySet -> Int
contextKeySetCardinality (ContextKeySet chunks) =
  UVector.foldl' (\count chunkValue -> count + popCount chunkValue) 0 chunks

contextKeySetFilter :: (Int -> Bool) -> ContextKeySet -> ContextKeySet
contextKeySetFilter predicate (ContextKeySet chunks) =
  ContextKeySet (UVector.imap filterChunk chunks)
  where
    filterChunk chunkIndex =
      retainBits (chunkIndex * contextKeyBitsPerChunk) 0

    retainBits !baseOrdinal !retainedBits !remainingBits
      | remainingBits == 0 = retainedBits
      | otherwise =
          let bitIndex = countTrailingZeros remainingBits
              bitMask = bit bitIndex
              nextBits = remainingBits .&. (remainingBits - 1)
              nextRetainedBits =
                if predicate (baseOrdinal + bitIndex)
                  then retainedBits .|. bitMask
                  else retainedBits
           in retainBits baseOrdinal nextRetainedBits nextBits

contextKeySetFind :: (Int -> Bool) -> ContextKeySet -> Maybe Int
contextKeySetFind predicate (ContextKeySet chunks) =
  findChunk 0
  where
    chunkCount = UVector.length chunks

    findChunk !chunkIndex
      | chunkIndex >= chunkCount = Nothing
      | otherwise =
          case findWord
            (chunkIndex * contextKeyBitsPerChunk)
            (unboxedIndexInvariant chunks chunkIndex) of
            Nothing -> findChunk (chunkIndex + 1)
            found -> found

    findWord !baseOrdinal !remainingBits
      | remainingBits == 0 = Nothing
      | otherwise =
          let bitIndex = countTrailingZeros remainingBits
              keyOrdinal = baseOrdinal + bitIndex
              nextBits = remainingBits .&. (remainingBits - 1)
           in if predicate keyOrdinal
                then Just keyOrdinal
                else findWord baseOrdinal nextBits

-- | Find the least key in @left \\ right@. Sets must have the same internal
-- shape; every call in the compiled representation satisfies that invariant.
contextKeySetFindDifference :: ContextKeySet -> ContextKeySet -> Maybe Int
contextKeySetFindDifference (ContextKeySet leftChunks) (ContextKeySet rightChunks) =
  findChunk 0
  where
    chunkCount = UVector.length leftChunks

    findChunk !chunkIndex
      | chunkIndex >= chunkCount = Nothing
      | otherwise =
          let leftChunk = unboxedIndexInvariant leftChunks chunkIndex
              rightChunk = unboxedIndexInvariant rightChunks chunkIndex
              difference = leftChunk .&. complement rightChunk
           in if difference == 0
                then findChunk (chunkIndex + 1)
                else
                  Just
                    ( chunkIndex * contextKeyBitsPerChunk
                        + countTrailingZeros difference
                    )

-- | Whether the intersection contains a key other than the excluded key.
-- This avoids allocating an intersection for every Hasse-edge candidate.
contextKeySetIntersectsExcept ::
  Int ->
  ContextKeySet ->
  ContextKeySet ->
  Bool
contextKeySetIntersectsExcept excludedOrdinal (ContextKeySet leftChunks) (ContextKeySet rightChunks) =
  go 0
  where
    chunkCount = UVector.length leftChunks
    excludedChunk = contextKeyChunkIndex excludedOrdinal
    excludedMask = contextKeyBitMask excludedOrdinal

    go !chunkIndex
      | chunkIndex >= chunkCount = False
      | otherwise =
          let common =
                unboxedIndexInvariant leftChunks chunkIndex
                  .&. unboxedIndexInvariant rightChunks chunkIndex
              relevant =
                if chunkIndex == excludedChunk
                  then common .&. complement excludedMask
                  else common
           in relevant /= 0 || go (chunkIndex + 1)

contextKeySetFoldr :: (Int -> result -> result) -> result -> ContextKeySet -> result
contextKeySetFoldr step initial (ContextKeySet chunks) =
  UVector.ifoldr foldChunk initial chunks
  where
    foldChunk chunkIndex =
      foldWord (chunkIndex * contextKeyBitsPerChunk)

    foldWord !baseOrdinal !remainingBits rest
      | remainingBits == 0 = rest
      | otherwise =
          let bitIndex = countTrailingZeros remainingBits
              nextBits = remainingBits .&. (remainingBits - 1)
           in step
                (baseOrdinal + bitIndex)
                (foldWord baseOrdinal nextBits rest)

contextKeySetToAscList :: ContextKeySet -> [Int]
contextKeySetToAscList =
  contextKeySetFoldr (:) []

contextKeySetChunkCount :: Int -> Int
contextKeySetChunkCount size
  | size <= 0 = 0
  | otherwise = 1 + (size - 1) `quot` contextKeyBitsPerChunk
{-# INLINE contextKeySetChunkCount #-}

contextKeySetZipWith ::
  (Word64 -> Word64 -> Word64) ->
  ContextKeySet ->
  ContextKeySet ->
  ContextKeySet
contextKeySetZipWith combine (ContextKeySet leftChunks) (ContextKeySet rightChunks) =
  ContextKeySet
    ( UVector.generate
        (UVector.length leftChunks)
        ( \chunkIndex ->
            combine
              (unboxedIndexInvariant leftChunks chunkIndex)
              (unboxedIndexInvariant rightChunks chunkIndex)
        )
    )
{-# INLINE contextKeySetZipWith #-}

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

type ContextKeyTable :: Type
data ContextKeyTable = ContextKeyTable
  { contextKeyTableSize :: !Int,
    contextKeyTableEntries :: !(UVector.Vector Int)
  }

contextKeyTableGenerateM ::
  Monad m =>
  Int ->
  Int ->
  (Int -> m ContextKey) ->
  m ContextKeyTable
contextKeyTableGenerateM size entryCount generateEntry =
  ContextKeyTable size
    <$> UVector.generateM
      entryCount
      (fmap contextKeyOrdinal . generateEntry)

-- | Total under the representation invariant: both keys are in @[0,n)@ and
-- the table contains exactly @n*n@ entries. Constructors never cross the
-- public module boundary.
contextKeyTableLookup :: ContextKeyTable -> ContextKey -> ContextKey -> ContextKey
contextKeyTableLookup table leftKey rightKey =
  ContextKey
    ( unboxedIndexInvariant
        (contextKeyTableEntries table)
        (pairKeyOffset (contextKeyTableSize table) leftKey rightKey)
    )
{-# INLINE contextKeyTableLookup #-}

pairKeyOffset :: Int -> ContextKey -> ContextKey -> Int
pairKeyOffset size (ContextKey leftOrdinal) (ContextKey rightOrdinal) =
  leftOrdinal * size + rightOrdinal
{-# INLINE pairKeyOffset #-}


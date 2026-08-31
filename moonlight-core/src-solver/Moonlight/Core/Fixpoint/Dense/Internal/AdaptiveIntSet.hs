-- | An adaptive immutable @Int@-set representation — tiny sorted vector,
-- chunked bitmap, or full bitmap — chosen by density, with its codec and the
-- shared @Word64@ bit arithmetic. A pure leaf.
module Moonlight.Core.Fixpoint.Dense.Internal.AdaptiveIntSet
  ( AdaptiveIntSet (..),
    ChunkedBitmap,
    BitmapChunk (..),
    adaptiveIntSetFromIntSet,
    adaptiveIntSetToIntSet,
    wordBits,
    wordCountForSize,
  )
where

import Data.Bits (shiftL, testBit, (.|.))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List qualified as List
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64)
import Prelude

type AdaptiveIntSet :: Type
data AdaptiveIntSet
  = TinySorted !(Vector Int)
  | ChunkedBitmap !ChunkedBitmap
  | FullBitmap !(Vector Word64)
  deriving stock (Eq, Show)

type ChunkedBitmap :: Type
data ChunkedBitmap = ChunkedBitmapData
  { chunkedBitmapChunkBits :: !Int,
    chunkedBitmapChunks :: !(IntMap.IntMap BitmapChunk)
  }
  deriving stock (Eq, Show)

type BitmapChunk :: Type
data BitmapChunk
  = SortedChunk !(Vector Int)
  | BitmapChunkWords !(Vector Word64)
  deriving stock (Eq, Show)

adaptiveIntSetTinyLimit :: Int
adaptiveIntSetTinyLimit =
  64
{-# INLINE adaptiveIntSetTinyLimit #-}

adaptiveIntSetChunkBits :: Int
adaptiveIntSetChunkBits =
  12
{-# INLINE adaptiveIntSetChunkBits #-}

adaptiveIntSetChunkSize :: Int
adaptiveIntSetChunkSize =
  chunkSizeForBits adaptiveIntSetChunkBits
{-# INLINE adaptiveIntSetChunkSize #-}

chunkSizeForBits :: Int -> Int
chunkSizeForBits chunkBits =
  1 `shiftL` max 0 chunkBits
{-# INLINE chunkSizeForBits #-}

adaptiveIntSetChunkBitmapLimit :: Int
adaptiveIntSetChunkBitmapLimit =
  wordCountForSize adaptiveIntSetChunkSize
{-# INLINE adaptiveIntSetChunkBitmapLimit #-}

adaptiveIntSetFullBitmapDensity :: Int
adaptiveIntSetFullBitmapDensity =
  8
{-# INLINE adaptiveIntSetFullBitmapDensity #-}

adaptiveIntSetFromIntSet :: IntSet -> AdaptiveIntSet
adaptiveIntSetFromIntSet values
  | IntSet.size values <= adaptiveIntSetTinyLimit =
      TinySorted (U.fromList (IntSet.toAscList values))
  | fullBitmapPreferred values =
      FullBitmap (bitmapFromIntSet values)
  | otherwise =
      ChunkedBitmap (chunkedBitmapFromIntSet values)
{-# INLINE adaptiveIntSetFromIntSet #-}

adaptiveIntSetToIntSet :: AdaptiveIntSet -> IntSet
adaptiveIntSetToIntSet adaptive =
  case adaptive of
    TinySorted values ->
      IntSet.fromDistinctAscList (U.toList values)
    ChunkedBitmap chunks ->
      chunkedBitmapToIntSet chunks
    FullBitmap bitmapWords ->
      bitmapToIntSet bitmapWords
{-# INLINE adaptiveIntSetToIntSet #-}

fullBitmapPreferred :: IntSet -> Bool
fullBitmapPreferred values =
  case (IntSet.lookupMin values, IntSet.lookupMax values) of
    (Just minimumKey, Just maximumKey)
      | minimumKey >= 0 ->
          IntSet.size values * adaptiveIntSetFullBitmapDensity >= maximumKey + 1
    _ ->
      False
{-# INLINE fullBitmapPreferred #-}

chunkedBitmapFromIntSet :: IntSet -> ChunkedBitmap
chunkedBitmapFromIntSet values =
  ChunkedBitmapData
    { chunkedBitmapChunkBits = adaptiveIntSetChunkBits,
      chunkedBitmapChunks =
        fmap bitmapChunkFromOffsets $
          IntMap.fromListWith
            (<>)
            [ (chunkIndex, [chunkOffset])
              | key <- IntSet.toAscList values,
                let (chunkIndex, chunkOffset) = key `divMod` adaptiveIntSetChunkSize
            ]
    }
{-# INLINE chunkedBitmapFromIntSet #-}

bitmapChunkFromOffsets :: [Int] -> BitmapChunk
bitmapChunkFromOffsets offsets
  | length sortedOffsets <= adaptiveIntSetChunkBitmapLimit =
      SortedChunk (U.fromList sortedOffsets)
  | otherwise =
      BitmapChunkWords (bitmapFromOffsets sortedOffsets)
  where
    sortedOffsets =
      List.sort offsets
{-# INLINE bitmapChunkFromOffsets #-}

bitmapFromOffsets :: [Int] -> Vector Word64
bitmapFromOffsets offsets =
  U.generate
    (wordCountForSize adaptiveIntSetChunkSize)
    (\wordIndex -> IntMap.findWithDefault 0 wordIndex wordsByIndex)
  where
    wordsByIndex =
      foldl' insertOffset IntMap.empty offsets
    insertOffset bitmapWords offset =
      IntMap.insertWith (.|.) (offset `quot` wordBits) (bitMask (offset `rem` wordBits)) bitmapWords
{-# INLINE bitmapFromOffsets #-}

chunkedBitmapToIntSet :: ChunkedBitmap -> IntSet
chunkedBitmapToIntSet (ChunkedBitmapData chunkBits chunks) =
  IntSet.fromDistinctAscList $
    concatMap chunkEntries (IntMap.toAscList chunks)
  where
    chunkSize =
      chunkSizeForBits chunkBits
    chunkEntries (chunkIndex, chunk) =
      fmap (+ (chunkIndex * chunkSize)) (bitmapChunkOffsets chunk)
{-# INLINE chunkedBitmapToIntSet #-}

bitmapChunkOffsets :: BitmapChunk -> [Int]
bitmapChunkOffsets chunk =
  case chunk of
    SortedChunk offsets ->
      U.toList offsets
    BitmapChunkWords bitmapWords ->
      bitmapOffsets bitmapWords
{-# INLINE bitmapChunkOffsets #-}

bitmapOffsets :: Vector Word64 -> [Int]
bitmapOffsets bitmapWords =
  [ wordIndex * wordBits + bitIndex
    | (wordIndex, word) <- zip [0 ..] (U.toList bitmapWords),
      bitIndex <- [0 .. wordBits - 1],
      testBit word bitIndex
  ]
{-# INLINE bitmapOffsets #-}

bitmapFromIntSet :: IntSet -> Vector Word64
bitmapFromIntSet values =
  U.generate wordCount (\wordIndex -> IntMap.findWithDefault 0 wordIndex wordsByIndex)
  where
    wordCount =
      maybe 0 ((+ 1) . (`quot` wordBits)) (IntSet.lookupMax values)
    wordsByIndex =
      IntSet.foldl' insertBit IntMap.empty values
    insertBit bitmapWords key =
      IntMap.insertWith (.|.) (key `quot` wordBits) (bitMask (key `rem` wordBits)) bitmapWords
{-# INLINE bitmapFromIntSet #-}

bitmapToIntSet :: Vector Word64 -> IntSet
bitmapToIntSet bitmapWords =
  IntSet.fromDistinctAscList
    [ wordIndex * wordBits + bitIndex
      | (wordIndex, word) <- zip [0 ..] (U.toList bitmapWords),
        bitIndex <- [0 .. wordBits - 1],
        testBit word bitIndex
    ]
{-# INLINE bitmapToIntSet #-}

wordBits :: Int
wordBits =
  64
{-# INLINE wordBits #-}

wordCountForSize :: Int -> Int
wordCountForSize size =
  (max 0 size + wordBits - 1) `quot` wordBits
{-# INLINE wordCountForSize #-}

bitMask :: Int -> Word64
bitMask bitIndex =
  (1 :: Word64) `shiftL` bitIndex
{-# INLINE bitMask #-}

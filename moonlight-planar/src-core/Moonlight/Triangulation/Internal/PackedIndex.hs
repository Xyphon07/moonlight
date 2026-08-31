module Moonlight.Triangulation.Internal.PackedIndex
  ( noIndex
  , indexLimit
  , packIndex
  , unpackIndex
  , unpackOptionalIndex
  ) where

import Data.Word (Word32)

noIndex :: Word32
noIndex = maxBound

indexLimit :: Int
indexLimit = fromIntegral (maxBound - 1 :: Word32)

-- | Pack an admitted arena handle. The build boundary limits vertex capacity
-- so the mutable topology reservation, @8n + 16@, stays below 'indexLimit';
-- every caller supplies a non-negative handle or worklist position derived
-- from those arenas. Optional @-1@ handles are represented separately by
-- 'noIndex' before this function is reached.
packIndex :: Int -> Word32
packIndex = fromIntegral
{-# INLINE packIndex #-}

unpackIndex :: Word32 -> Int
unpackIndex = fromIntegral
{-# INLINE unpackIndex #-}

unpackOptionalIndex :: Word32 -> Maybe Int
unpackOptionalIndex value
  | value == noIndex = Nothing
  | otherwise = Just (unpackIndex value)
{-# INLINE unpackOptionalIndex #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Triangulation.Internal.Growable
  ( GrowableWord32
  , newGrowableWord32
  , clearGrowable
  , growableLength
  , pushGrowable
  , popGrowableOr
  , readGrowable
  , writeGrowable
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Word (Word32)

-- | Transaction-local vector. It starts small and grows only with the local
-- frontier; no operation allocates scratch memory proportional to mesh size.
-- The size lives in an unboxed cell: the stack is popped and pushed inside
-- every legalization drain, and a boxed counter would allocate on each step.
data GrowableWord32 s = GrowableWord32
  { growableVector :: !(STRef s (MUV.MVector s Word32))
  , growableSize :: !(MUV.MVector s Int)
  }

newGrowableWord32 :: Int -> ST s (GrowableWord32 s)
newGrowableWord32 requested = do
  vector <- MUV.new (max 8 requested)
  growableVector <- newSTRef vector
  growableSize <- MUV.replicate 1 0
  pure GrowableWord32{growableVector, growableSize}

clearGrowable :: GrowableWord32 s -> ST s ()
clearGrowable GrowableWord32{growableSize} = MUV.unsafeWrite growableSize 0 0
{-# INLINE clearGrowable #-}

growableLength :: GrowableWord32 s -> ST s Int
growableLength GrowableWord32{growableSize} = MUV.unsafeRead growableSize 0
{-# INLINE growableLength #-}

pushGrowable :: GrowableWord32 s -> Word32 -> ST s ()
pushGrowable growable@GrowableWord32{growableSize} value = do
  index <- MUV.unsafeRead growableSize 0
  vector <- ensureCapacity growable (index + 1)
  MUV.unsafeWrite vector index value
  MUV.unsafeWrite growableSize 0 (index + 1)
{-# INLINE pushGrowable #-}

-- | Pop, answering the caller's sentinel on emptiness instead of allocating a
-- 'Maybe' per step. Sound only against a sentinel no push can store.
popGrowableOr :: Word32 -> GrowableWord32 s -> ST s Word32
popGrowableOr sentinel GrowableWord32{growableVector, growableSize} = do
  size <- MUV.unsafeRead growableSize 0
  if size <= 0
    then pure sentinel
    else do
      let !index = size - 1
      vector <- readSTRef growableVector
      value <- MUV.unsafeRead vector index
      MUV.unsafeWrite growableSize 0 index
      pure value
{-# INLINE popGrowableOr #-}

readGrowable :: GrowableWord32 s -> Int -> ST s Word32
readGrowable GrowableWord32{growableVector} index = do
  vector <- readSTRef growableVector
  MUV.unsafeRead vector index
{-# INLINE readGrowable #-}

writeGrowable :: GrowableWord32 s -> Int -> Word32 -> ST s ()
writeGrowable growable@GrowableWord32{growableSize} index value = do
  vector <- ensureCapacity growable (index + 1)
  MUV.unsafeWrite vector index value
  size <- MUV.unsafeRead growableSize 0
  when (index >= size) (MUV.unsafeWrite growableSize 0 (index + 1))
{-# INLINE writeGrowable #-}

-- Answering with the vector is what keeps a push to one cell read: the caller
-- would otherwise re-read the reference this just proved current.
ensureCapacity :: GrowableWord32 s -> Int -> ST s (MUV.MVector s Word32)
ensureCapacity GrowableWord32{growableVector} required = do
  vector <- readSTRef growableVector
  let !current = MUV.length vector
  if required <= current
    then pure vector
    else do
      let !next = until (>= required) (* 2) current
      grown <- MUV.grow vector (next - current)
      writeSTRef growableVector grown
      pure grown

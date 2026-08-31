{-# LANGUAGE BangPatterns #-}

-- | Bounded traversal of a cyclic successor relation.
module Moonlight.Triangulation.Handles.Iterators.CircularIterator
  ( circularList
  , foldCircular'
  ) where

import GHC.Exts (build)

-- | The cycle reached from a start by repeated advance, in visit order.
--
-- Emitted forwards, for the reason 'Moonlight.Triangulation.Dcel.circularWalk'
-- is: accumulating in reverse and reversing at the end builds the ring twice
-- and hands back a list no consumer can fuse with.
circularList :: Eq a => Int -> (a -> a) -> a -> [a]
circularList limit advance start =
  build
    ( \link stop ->
        let go !remaining !current !visited
              | remaining <= 0 = stop
              | visited && current == start = stop
              | otherwise = link current (go (remaining - 1) (advance current) True)
         in go limit start False
    )
{-# INLINE circularList #-}

-- | Strictly fold a bounded cycle in visit order.
foldCircular' :: Eq a => Int -> (a -> a) -> a -> (b -> a -> b) -> b -> b
foldCircular' limit advance start step = go limit start False
 where
  go !remaining !current !visited !accumulator
    | remaining <= 0 = accumulator
    | visited && current == start = accumulator
    | otherwise = go (remaining - 1) (advance current) True (step accumulator current)

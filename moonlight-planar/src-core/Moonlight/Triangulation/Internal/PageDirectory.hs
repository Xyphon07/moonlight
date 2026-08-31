{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The map from page number to page, for the paged stores.
module Moonlight.Triangulation.Internal.PageDirectory
  ( PageDirectory
  , emptyDirectory
  , lookupDirectory
  , insertDirectory
  , directoryFromAscList
  , directoryToAscList
  , directorySize
  , directoryLookupMax
  , directoryRestrict
  ) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.List as List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Generics (Generic)

-- Page numbers are small, dense and non-negative, and the read path resolves
-- one on every field access that misses its owner's current page. An ordered
-- map answers that with a chain of prefix comparisons whose length grows with
-- the mesh; a 32-way radix answers it with two indexed loads at every size the
-- meshes reach, and the arithmetic is shifts and masks rather than branches.
--
-- Absence is representable because the boxed payload store leaves a page out
-- entirely when every slot in it still holds the fill.
data Slot a
  = Absent
  | Page !a
  | Fanout !(V.Vector (Slot a))
  deriving stock (Generic, Functor)
  deriving anyclass (NFData)

-- | @directoryHeight@ counts the fanout levels below the root: at height 0 the
-- root's children are pages, at height 1 they are fanouts of pages, and so on.
data PageDirectory a = PageDirectory
  { directoryHeight :: {-# UNPACK #-} !Int
  , directoryRoot :: !(Slot a)
  , directorySize :: {-# UNPACK #-} !Int
  }
  deriving stock (Generic, Functor)
  deriving anyclass (NFData)

fanout :: Int
fanout = 32
{-# INLINE fanout #-}

slotBits :: Int
slotBits = 5
{-# INLINE slotBits #-}

-- Path copying replaces one slot per level, and the bulk-update operator would
-- route that through a list of one pair. This is the copy the level needs and
-- nothing besides.
updateSlot :: V.Vector (Slot a) -> Int -> Slot a -> V.Vector (Slot a)
updateSlot children index child =
  V.modify (\node -> MV.unsafeWrite node index child) children
{-# INLINE updateSlot #-}

-- | The first page number a directory of this height cannot address.
capacity :: Int -> Int
capacity height = 1 `shiftL` (slotBits * (height + 1))
{-# INLINE capacity #-}

emptyDirectory :: PageDirectory a
emptyDirectory = PageDirectory 0 Absent 0

lookupDirectory :: Int -> PageDirectory a -> Maybe a
lookupDirectory key PageDirectory{directoryHeight, directoryRoot}
  | key < 0 || key >= capacity directoryHeight = Nothing
  | otherwise = go directoryHeight directoryRoot
 where
  go !height slot = case slot of
    Absent -> Nothing
    Page value -> Just value
    Fanout children ->
      go (height - 1) (V.unsafeIndex children ((key `shiftR` (slotBits * height)) .&. (fanout - 1)))
{-# INLINE lookupDirectory #-}

insertDirectory :: Int -> a -> PageDirectory a -> PageDirectory a
insertDirectory key value directory
  | key >= capacity (directoryHeight directory) = insertDirectory key value (grow directory)
  | otherwise =
      let (!root, !added) = write (directoryHeight directory) (directoryRoot directory)
       in directory
            { directoryRoot = root
            , directorySize = directorySize directory + if added then 1 else 0
            }
 where
  write !height slot
    | height < 0 = (Page value, case slot of Page _ -> False; _ -> True)
    | otherwise =
        let !index = (key `shiftR` (slotBits * height)) .&. (fanout - 1)
            !children = case slot of
              Fanout existing -> existing
              _ -> V.replicate fanout Absent
            (!child, !added) = write (height - 1) (V.unsafeIndex children index)
         in (Fanout (updateSlot children index child), added)

-- A directory that has run out of addressable pages gains a level, and its
-- whole former extent becomes child zero of the new root.
grow :: PageDirectory a -> PageDirectory a
grow directory@PageDirectory{directoryHeight, directoryRoot} =
  directory
    { directoryHeight = directoryHeight + 1
    , directoryRoot = case directoryRoot of
        Absent -> Absent
        occupied -> Fanout (updateSlot (V.replicate fanout Absent) 0 occupied)
    }

-- | Build from ascending, distinct page numbers in one descent.
--
-- Every dense transaction ends by publishing a whole directory, so this is on
-- the freeze path of bulk load, constraint recovery and refinement alike.
-- Folding 'insertDirectory' over the list would copy each root path once per
-- page; laying the levels down directly touches each node once.
directoryFromAscList :: [(Int, a)] -> PageDirectory a
directoryFromAscList [] = emptyDirectory
directoryFromAscList entries@((firstKey, _) : remainingEntries) =
  PageDirectory
    { directoryHeight = height
    , directoryRoot = fst (build height 0 entries)
    , directorySize = length entries
    }
 where
  !largest = List.foldl' (\largestKey (key, _) -> max largestKey key) firstKey remainingEntries
  !height = heightFor 0
  heightFor !candidate
    | largest < capacity candidate = candidate
    | otherwise = heightFor (candidate + 1)

  build :: Int -> Int -> [(Int, b)] -> (Slot b, [(Int, b)])
  build !level !base remaining = case remaining of
    [] -> (Absent, [])
    ((key, value) : rest)
      | key >= base + capacity level -> (Absent, remaining)
      | level < 0 -> (Page value, rest)
      | otherwise ->
          let !childSpan = capacity (level - 1)
              step (children, unconsumed) index =
                let (!child, !beyond) = build (level - 1) (base + index * childSpan) unconsumed
                 in (child : children, beyond)
              (!reversed, !left) = List.foldl' step ([], remaining) [0 .. fanout - 1]
           in (Fanout (V.fromListN fanout (reverse reversed)), left)

directoryToAscList :: PageDirectory a -> [(Int, a)]
directoryToAscList PageDirectory{directoryHeight, directoryRoot} = go directoryHeight 0 directoryRoot []
 where
  go :: Int -> Int -> Slot b -> [(Int, b)] -> [(Int, b)]
  go !height !prefix slot rest = case slot of
    Absent -> rest
    Page value -> (prefix, value) : rest
    Fanout children ->
      foldr
        (\index accumulated ->
          go
            (height - 1)
            (prefix + (index `shiftL` (slotBits * height)))
            (V.unsafeIndex children index)
            accumulated
        )
        rest
        [0 .. fanout - 1]

directoryLookupMax :: PageDirectory a -> Maybe (Int, a)
directoryLookupMax PageDirectory{directoryHeight, directoryRoot} = go directoryHeight 0 directoryRoot
 where
  go :: Int -> Int -> Slot b -> Maybe (Int, b)
  go !height !prefix slot = case slot of
    Absent -> Nothing
    Page value -> Just (prefix, value)
    Fanout children -> descend (fanout - 1)
     where
      descend !index
        | index < 0 = Nothing
        | otherwise =
            case go (height - 1) (prefix + (index `shiftL` (slotBits * height))) (V.unsafeIndex children index) of
              Nothing -> descend (index - 1)
              found -> found

-- | Keep only the pages a directory of the given page count can hold. The
-- common case is that every page is already within the count, and that is
-- settled by one descent rather than a rebuild.
directoryRestrict :: Int -> PageDirectory a -> PageDirectory a
directoryRestrict pageCount directory
  | pageCount <= 0 = emptyDirectory
  | otherwise = case directoryLookupMax directory of
      Nothing -> directory
      Just (largest, _)
        | largest < pageCount -> directory
        | otherwise ->
            directoryFromAscList (filter ((< pageCount) . fst) (directoryToAscList directory))

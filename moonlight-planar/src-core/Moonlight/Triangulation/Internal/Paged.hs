{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Moonlight.Triangulation.Internal.Paged
  ( Paged
  , MutablePaged
  , FlatMutablePaged
  , emptyPaged
  , fromVector
  , fromLocalVector
  , toVector
  , pagedLength
  , pagedPageCount
  , pagedOverlayPageCount
  , PublicationStats (..)
  , emptyPublicationStats
  , addPublicationStats
  , pagedUnsafeIndex
  , pagedFoldl'
  , newMutablePaged
  , newLocalMutablePaged
  , TransactionShape (..)
  , thawPaged
  , thawPagedDense
  , thawPagedShaped
  , flatMutableSection
  , readFlatMutable
  , writeFlatMutable
  , readPaged
  , writePaged
  , freezePaged
  , readMutablePagedPublicationStats
  ) where

import Control.DeepSeq (NFData (..))
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.IntMap.Strict as IntMap
import Data.List (unfoldr)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MUV
import Moonlight.Triangulation.Internal.PageDirectory
  ( PageDirectory
  , directoryRestrict
  , directorySize
  , emptyDirectory
  , insertDirectory
  , lookupDirectory
  )

-- | One flat root plus one cumulative page overlay. An extension never wraps
-- a published value in another 'Paged' node: dirty pages are merged into the
-- existing directory, so an untouched read remains one overlay lookup plus
-- one root lookup regardless of extension count.
data Paged a
  = FlatPaged {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(U.Vector a)
  | SharedPaged
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      !(U.Vector a)
      !(PageDirectory (U.Vector a))

-- | Exact storage work observed by one persistent publication.  The dense
-- page counters describe a whole-base traversal; dirty-page and copied-cell
-- counters describe only pages opened by the local write set.  A zero dense
-- counter is therefore a storage fact from the transaction owner, never a
-- benchmark inference; page work is independent of the resident prefix except
-- for directory-path depth.
data PublicationStats = PublicationStats
  { publicationUnboxedBasePageEnumerations :: {-# UNPACK #-} !Int
  , publicationUnboxedBasePageOpens :: {-# UNPACK #-} !Int
  , publicationUnboxedBasePageFreezes :: {-# UNPACK #-} !Int
  , publicationUnboxedDirtyBasePages :: {-# UNPACK #-} !Int
  , publicationUnboxedDirtyAppendedPages :: {-# UNPACK #-} !Int
  , publicationUnboxedCopiedCells :: {-# UNPACK #-} !Int
  , publicationBoxedBasePageEnumerations :: {-# UNPACK #-} !Int
  , publicationBoxedBasePageOpens :: {-# UNPACK #-} !Int
  , publicationBoxedBasePageFreezes :: {-# UNPACK #-} !Int
  , publicationBoxedDirtyBasePages :: {-# UNPACK #-} !Int
  , publicationBoxedDirtyAppendedPages :: {-# UNPACK #-} !Int
  , publicationBoxedCopiedCells :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

instance NFData PublicationStats where
  rnf stats = stats `seq` ()

emptyPublicationStats :: PublicationStats
emptyPublicationStats =
  PublicationStats
    { publicationUnboxedBasePageEnumerations = 0
    , publicationUnboxedBasePageOpens = 0
    , publicationUnboxedBasePageFreezes = 0
    , publicationUnboxedDirtyBasePages = 0
    , publicationUnboxedDirtyAppendedPages = 0
    , publicationUnboxedCopiedCells = 0
    , publicationBoxedBasePageEnumerations = 0
    , publicationBoxedBasePageOpens = 0
    , publicationBoxedBasePageFreezes = 0
    , publicationBoxedDirtyBasePages = 0
    , publicationBoxedDirtyAppendedPages = 0
    , publicationBoxedCopiedCells = 0
    }

addPublicationStats :: PublicationStats -> PublicationStats -> PublicationStats
addPublicationStats left right =
  PublicationStats
    { publicationUnboxedBasePageEnumerations = publicationUnboxedBasePageEnumerations left + publicationUnboxedBasePageEnumerations right
    , publicationUnboxedBasePageOpens = publicationUnboxedBasePageOpens left + publicationUnboxedBasePageOpens right
    , publicationUnboxedBasePageFreezes = publicationUnboxedBasePageFreezes left + publicationUnboxedBasePageFreezes right
    , publicationUnboxedDirtyBasePages = publicationUnboxedDirtyBasePages left + publicationUnboxedDirtyBasePages right
    , publicationUnboxedDirtyAppendedPages = publicationUnboxedDirtyAppendedPages left + publicationUnboxedDirtyAppendedPages right
    , publicationUnboxedCopiedCells = publicationUnboxedCopiedCells left + publicationUnboxedCopiedCells right
    , publicationBoxedBasePageEnumerations = publicationBoxedBasePageEnumerations left + publicationBoxedBasePageEnumerations right
    , publicationBoxedBasePageOpens = publicationBoxedBasePageOpens left + publicationBoxedBasePageOpens right
    , publicationBoxedBasePageFreezes = publicationBoxedBasePageFreezes left + publicationBoxedBasePageFreezes right
    , publicationBoxedDirtyBasePages = publicationBoxedDirtyBasePages left + publicationBoxedDirtyBasePages right
    , publicationBoxedDirtyAppendedPages = publicationBoxedDirtyAppendedPages left + publicationBoxedDirtyAppendedPages right
    , publicationBoxedCopiedCells = publicationBoxedCopiedCells left + publicationBoxedCopiedCells right
    }

-- | A fresh build owns a contiguous arena. A local transaction owns only its
-- dirty pages; clean values remain reachable through the immutable base.
data MutablePaged s a
  = MutableFlatPaged {-# UNPACK #-} !Int !(MUV.MVector s a)
  | MutableSharedPaged
      {-# UNPACK #-} !Int
      !(Paged a)
      !(STRef s (IntMap.IntMap (MUV.MVector s a)))
      !(STRef s PublicationStats)

-- | A proof that one mutable sequence is in its contiguous physical section.
data FlatMutablePaged s a = FlatMutablePaged !(MUV.MVector s a)

localPageBits :: Int
localPageBits = 8
{-# INLINE localPageBits #-}

traversalPageBits :: Int
traversalPageBits = 10
{-# INLINE traversalPageBits #-}

pageSize :: Int -> Int
pageSize bits = 1 `shiftL` bits
{-# INLINE pageSize #-}

pageOf :: Int -> Int -> Int
pageOf bits index = index `shiftR` bits
{-# INLINE pageOf #-}

offsetOf :: Int -> Int -> Int
offsetOf bits index = index .&. (pageSize bits - 1)
{-# INLINE offsetOf #-}

pagesFor :: Int -> Int -> Int
pagesFor bits count
  | count <= 0 = 0
  | otherwise = pageOf bits (count - 1) + 1
{-# INLINE pagesFor #-}

pagedLength :: Paged a -> Int
pagedLength paged = case paged of
  FlatPaged count _ _ -> count
  SharedPaged count _ _ _ -> count
{-# INLINE pagedLength #-}

pagedPageCount :: Paged a -> Int
pagedPageCount paged = pagesFor (pagedBits paged) (pagedLength paged)
{-# INLINE pagedPageCount #-}

-- | Number of pages in the persistent overlay.  A flat root has no copied
-- pages; a shared value records only pages dirtied by its transaction.  This
-- is an observation for publication receipts, not a read-path traversal.
pagedOverlayPageCount :: Paged a -> Int
pagedOverlayPageCount paged = case paged of
  FlatPaged{} -> 0
  SharedPaged _ _ _ directory -> directorySize directory
{-# INLINE pagedOverlayPageCount #-}

pagedBits :: Paged a -> Int
pagedBits paged = case paged of
  FlatPaged _ bits _ -> bits
  SharedPaged _ bits _ _ -> bits
{-# INLINE pagedBits #-}

instance (U.Unbox a, Eq a) => Eq (Paged a) where
  left == right =
    pagedLength left == pagedLength right
      && case (left, right) of
        (FlatPaged _ _ leftValues, FlatPaged _ _ rightValues) -> leftValues == rightValues
        _ -> pagedChunksEqual left right

pagedChunksEqual :: forall a. (U.Unbox a, Eq a) => Paged a -> Paged a -> Bool
pagedChunksEqual left right =
  foldl'
    (\equal (index, width) ->
       equal
         && U.unsafeSlice 0 width (pageSlice left index)
              == U.unsafeSlice 0 width (pageSlice right index))
    True
    chunks
 where
  !count = pagedLength left
  !leftBits = pagedBits left
  !rightBits = pagedBits right
  chunks = unfoldr nextChunk 0

  nextChunk !index
    | index >= count = Nothing
    | otherwise = Just ((index, width), index + width)
    where
      !leftPageEnd = (pageOf leftBits index + 1) * pageSize leftBits
      !rightPageEnd = (pageOf rightBits index + 1) * pageSize rightBits
      !width = min (count - index) (min (leftPageEnd - index) (rightPageEnd - index))
  pageSlice :: Paged a -> Int -> U.Vector a
  pageSlice paged index =
    let !bits = pagedBits paged
        !page = pageOf bits index
        !offset = offsetOf bits index
     in case paged of
          FlatPaged _ _ values -> U.unsafeSlice index (U.length values - index) values
          SharedPaged _ _ root directory ->
            case lookupDirectory page directory of
              Just values -> U.unsafeSlice offset (U.length values - offset) values
              Nothing -> U.unsafeSlice index (U.length root - index) root

instance (U.Unbox a, Show a) => Show (Paged a) where
  showsPrec precedence = showsPrec precedence . U.toList . toVector

instance NFData (Paged a) where
  rnf paged = case paged of
    FlatPaged count bits values -> count `seq` bits `seq` values `seq` ()
    SharedPaged count bits root directory ->
      count `seq` bits `seq` root `seq` directory `seq` ()

emptyPaged :: U.Unbox a => Paged a
emptyPaged = FlatPaged 0 traversalPageBits U.empty

fromVector :: U.Unbox a => a -> U.Vector a -> Paged a
fromVector _padding values = FlatPaged (U.length values) traversalPageBits values

fromLocalVector :: U.Unbox a => a -> U.Vector a -> Paged a
fromLocalVector _padding values = FlatPaged (U.length values) localPageBits values

toVector :: U.Unbox a => Paged a -> U.Vector a
toVector paged = case paged of
  FlatPaged count _ values -> U.unsafeSlice 0 count values
  SharedPaged count _ _ _ -> U.generate count (pagedUnsafeIndex paged)

pagedUnsafeIndex :: U.Unbox a => Paged a -> Int -> a
pagedUnsafeIndex paged index =
  case paged of
    FlatPaged _ _ values -> U.unsafeIndex values index
    SharedPaged _ bits root directory ->
      case lookupDirectory (pageOf bits index) directory of
        Just values -> U.unsafeIndex values (offsetOf bits index)
        Nothing -> U.unsafeIndex root index
{-# INLINE pagedUnsafeIndex #-}

-- | Fold directly over the physical sections. Unlike the old implementation,
-- this does not allocate a complete vector merely to consume it.
pagedFoldl' :: U.Unbox a => (b -> a -> b) -> b -> Paged a -> b
pagedFoldl' step initial paged =
  case paged of
    FlatPaged count _ values -> U.foldl' step initial (U.take count values)
    SharedPaged count bits root directory ->
      foldl' foldPage initial [0 .. pagesFor bits count - 1]
     where
      foldPage !accumulated page =
        let !start = page * pageSize bits
            !width = min (pageSize bits) (count - start)
            !values =
              case lookupDirectory page directory of
                Just pageValues -> U.unsafeSlice 0 width pageValues
                Nothing -> U.unsafeSlice start width root
         in U.foldl' step accumulated values
{-# INLINE pagedFoldl' #-}

newMutablePaged :: U.Unbox a => Int -> ST s (MutablePaged s a)
newMutablePaged capacity = do
  values <- MUV.new (max 0 capacity)
  pure (MutableFlatPaged traversalPageBits values)

newLocalMutablePaged :: U.Unbox a => Int -> ST s (MutablePaged s a)
newLocalMutablePaged capacity = do
  values <- MUV.new (max 0 capacity)
  pure (MutableFlatPaged localPageBits values)

-- | Open a persistent transaction without constructing a page table over the
-- resident mesh. The mutable dictionary starts empty and acquires storage only
-- when a write first reaches a page.
thawPaged :: Int -> Paged a -> ST s (MutablePaged s a)
thawPaged _requestedCapacity paged = do
  dirty <- newSTRef IntMap.empty
  stats <- newSTRef emptyPublicationStats
  pure (MutableSharedPaged (pagedBits paged) paged dirty stats)

data TransactionShape = DenseTransaction | LocalTransaction

thawPagedShaped :: U.Unbox a => TransactionShape -> Int -> Paged a -> ST s (MutablePaged s a)
thawPagedShaped shape = case shape of
  DenseTransaction -> thawPagedDense
  LocalTransaction -> thawPaged
{-# INLINE thawPagedShaped #-}

thawPagedDense :: U.Unbox a => Int -> Paged a -> ST s (MutablePaged s a)
thawPagedDense requestedCapacity paged = do
  let !count = pagedLength paged
      !capacity = max requestedCapacity count
  values <- MUV.new capacity
  U.copy (MUV.unsafeSlice 0 count values) (toVector paged)
  pure (MutableFlatPaged (pagedBits paged) values)

flatMutableSection :: MutablePaged s a -> Maybe (FlatMutablePaged s a)
flatMutableSection mutable = case mutable of
  MutableFlatPaged _ values -> Just (FlatMutablePaged values)
  MutableSharedPaged{} -> Nothing
{-# INLINE flatMutableSection #-}

readFlatMutable :: U.Unbox a => FlatMutablePaged s a -> Int -> ST s a
readFlatMutable (FlatMutablePaged values) = MUV.unsafeRead values
{-# INLINE readFlatMutable #-}

writeFlatMutable :: U.Unbox a => FlatMutablePaged s a -> Int -> a -> ST s ()
writeFlatMutable (FlatMutablePaged values) = MUV.unsafeWrite values
{-# INLINE writeFlatMutable #-}

readPaged :: U.Unbox a => MutablePaged s a -> Int -> ST s a
readPaged mutable index = case mutable of
  MutableFlatPaged _ values -> MUV.unsafeRead values index
  MutableSharedPaged bits base dirtyRef _ -> do
    dirty <- readSTRef dirtyRef
    case IntMap.lookup (pageOf bits index) dirty of
      Just values -> MUV.unsafeRead values (offsetOf bits index)
      Nothing -> pure (pagedUnsafeIndex base index)
{-# INLINE readPaged #-}

writePaged :: U.Unbox a => MutablePaged s a -> Int -> a -> ST s ()
writePaged mutable index value = case mutable of
  MutableFlatPaged _ values -> MUV.unsafeWrite values index value
  MutableSharedPaged bits base dirtyRef statsRef -> do
    let !page = pageOf bits index
        !start = page * pageSize bits
        !existingWidth = min (pageSize bits) (max 0 (pagedLength base - start))
    dirty <- readSTRef dirtyRef
    values <- case IntMap.lookup page dirty of
      Just existing -> pure existing
      Nothing -> do
        pageValues <- MUV.new (pageSize bits)
        when (existingWidth > 0) $
            U.copy
              (MUV.unsafeSlice 0 existingWidth pageValues)
              (U.generate existingWidth (\offset -> pagedUnsafeIndex base (start + offset)))
        writeSTRef dirtyRef $! IntMap.insert page pageValues dirty
        modifySTRef'
          statsRef
          (addPublicationStats
             emptyPublicationStats
               { publicationUnboxedBasePageOpens = if existingWidth > 0 then 1 else 0
               , publicationUnboxedDirtyBasePages = if existingWidth > 0 then 1 else 0
               , publicationUnboxedDirtyAppendedPages = if existingWidth > 0 then 0 else 1
               , publicationUnboxedCopiedCells = existingWidth
               })
        pure pageValues
    MUV.unsafeWrite values (offsetOf bits index) value
{-# INLINE writePaged #-}

-- | Publish only dirty pages. A local publication retains one immutable root
-- and merges the changed page paths into its cumulative directory.
freezePaged :: forall s a. U.Unbox a => Int -> MutablePaged s a -> ST s (Paged a)
freezePaged count mutable
  | count <= 0 = pure emptyPaged
  | otherwise = case mutable of
      MutableFlatPaged bits values
        | MUV.length values <= count + (count `quot` 8) ->
            FlatPaged count bits . U.take count <$> U.unsafeFreeze values
        | otherwise ->
            FlatPaged count bits <$> U.freeze (MUV.unsafeSlice 0 count values)
      MutableSharedPaged bits base dirtyRef _statsRef -> do
        dirty <- readSTRef dirtyRef
        if IntMap.null dirty && count == pagedLength base
          then pure base
          else do
            frozen <- traverse freezePage (IntMap.toAscList dirty)
            let !priorDirectory = sharedDirectory base
                !directory =
                  foldl'
                    (\accumulated (page, values) -> insertDirectory page values accumulated)
                    priorDirectory
                    frozen
                !root = sharedRoot base
            pure (SharedPaged count bits root (directoryRestrict (pagesFor bits count) directory))
 where
  freezePage :: (Int, MUV.MVector s a) -> ST s (Int, U.Vector a)
  freezePage (page, values) = do
    frozen <- U.unsafeFreeze values
    pure (page, frozen)

sharedRoot :: Paged a -> U.Vector a
sharedRoot paged = case paged of
  FlatPaged _ _ values -> values
  SharedPaged _ _ root _ -> root
{-# INLINE sharedRoot #-}

sharedDirectory :: Paged a -> PageDirectory (U.Vector a)
sharedDirectory paged = case paged of
  FlatPaged _ _ _ -> emptyDirectory
  SharedPaged _ _ _ directory -> directory
{-# INLINE sharedDirectory #-}

readMutablePagedPublicationStats :: MutablePaged s a -> ST s PublicationStats
readMutablePagedPublicationStats mutable = case mutable of
  MutableFlatPaged _ _ -> pure emptyPublicationStats
  MutableSharedPaged _ _ _ statsRef -> readSTRef statsRef

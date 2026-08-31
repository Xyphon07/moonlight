{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Moonlight.Triangulation.Internal.BoxedPaged
  ( BoxedPaged
  , MutableBoxedPaged
  , BoxedStorageError (..)
  , emptyBoxedPaged
  , boxedFill
  , boxedDefaulted
  , boxedMaterializedPageCount
  , boxedPagedLength
  , boxedUnsafeIndex
  , boxedFromVector
  , boxedToVector
  , thawBoxedPaged
  , readMutableBoxedPublicationStats
  , readBoxedPaged
  , writeBoxedPaged
  , resetBoxedRange
  , boxedThawPristine
  , freezeBoxedPaged
  , boxedUpdate
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR, (.&.))
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Moonlight.Triangulation.Internal.PageDirectory
import Moonlight.Triangulation.Internal.Paged
  ( PublicationStats (..)
  , TransactionShape (..)
  , addPublicationStats
  , emptyPublicationStats
  )

import GHC.Generics (Generic)

-- | A paged payload store in which an absent page means every slot in it holds
-- the fill. The fill belongs to the store rather than to a thaw of it: a
-- payload component either has a default that new elements inherit — the three
-- element payloads do — or it has none and every slot must be written before it
-- is read, which is the vertex component, whose payloads arrive with it.
--
-- The near-universal instantiation is @() () ()@, and under an unconditional
-- representation that costs one pointer per directed edge, undirected edge and
-- face, all aimed at the same closure. Here it costs nothing: an element
-- created at its default writes no slot, so no page is ever materialized.
data BoxedStorage a
  = DenseBoxedPages !(V.Vector (V.Vector a))
  | DefaultedBoxedPages !a !(PageDirectory (V.Vector a))
  deriving stock (Generic, Functor)
  deriving anyclass (NFData)

data BoxedPaged a = BoxedPaged
  { boxedLength :: {-# UNPACK #-} !Int
  , boxedStorage :: !(BoxedStorage a)
  }
  deriving stock (Generic, Functor)
  deriving anyclass (NFData)

data BoxedStorageError
  = BoxedFreezeLengthNegative {-# UNPACK #-} !Int
  | BoxedFreezeDensePageMissing {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

boxedFill :: BoxedPaged a -> Maybe a
boxedFill BoxedPaged{boxedStorage = DenseBoxedPages _} = Nothing
boxedFill BoxedPaged{boxedStorage = DefaultedBoxedPages fill _} = Just fill

-- | Number of materialized payload pages.  Defaulted stores report only their
-- sparse directory; an absent page is the fill and was never copied.
boxedMaterializedPageCount :: BoxedPaged a -> Int
boxedMaterializedPageCount BoxedPaged{boxedStorage} = case boxedStorage of
  DenseBoxedPages pages -> V.length pages
  DefaultedBoxedPages _ pages -> directorySize pages
{-# INLINE boxedMaterializedPageCount #-}

-- | A length-bearing payload section whose every slot is its default.  The
-- section deliberately owns no materialized pages; a geometry-only
-- publication can therefore carry an arbitrarily large unit payload plane
-- without forcing a dense boxed vector at its next freeze.
boxedDefaulted :: a -> Int -> BoxedPaged a
boxedDefaulted fill length' =
  BoxedPaged
    { boxedLength = max 0 length'
    , boxedStorage = DefaultedBoxedPages fill emptyDirectory
    }

-- Structural equality would distinguish a store that materialized a page of
-- defaults from one that did not, and the two are the same store. Equality is
-- therefore what can be observed: the length, the fill every unwritten slot
-- will report, and the sequence itself.
instance Eq a => Eq (BoxedPaged a) where
  left == right =
    boxedLength left == boxedLength right
      && boxedFill left == boxedFill right
      && boxedToVector left == boxedToVector right

instance Show a => Show (BoxedPaged a) where
  showsPrec precedence paged =
    showParen (precedence > 10) $
      showString "BoxedPaged "
        . showsPrec 11 (boxedFill paged)
        . showString " "
        . showsPrec 11 (boxedToVector paged)

data MutableBoxedPaged s a = MutableBoxedPaged
  { mutableBoxedBase :: !(BoxedPaged a)
  , mutableBoxedDirty :: !(STRef s (IntMap.IntMap (MV.MVector s a)))
  , mutableBoxedStats :: !(Maybe (STRef s PublicationStats))
  , -- | The thaw found no materialized page. Together with an empty dirty set
    -- this says every slot in the store reports the fill, which is the state a
    -- payload component nobody writes stays in for the whole transaction —
    -- the near-universal @() () ()@ case. Read through 'boxedThawPristine'.
    mutableBoxedPristineBase :: !Bool
  }

pageBits :: Int
pageBits = 8
{-# INLINE pageBits #-}

pageSize :: Int
pageSize = 1 `shiftL` pageBits
{-# INLINE pageSize #-}

-- Every index this store decomposes is an element identifier or a walk over
-- one, so the signed division a 'quotRem' would emit corrects for a sign that
-- cannot occur.
pageOf :: Int -> Int
pageOf index = index `shiftR` pageBits
{-# INLINE pageOf #-}

offsetOf :: Int -> Int
offsetOf index = index .&. (pageSize - 1)
{-# INLINE offsetOf #-}

emptyBoxedPaged :: Maybe a -> BoxedPaged a
emptyBoxedPaged fill =
  BoxedPaged
    0
    (case fill of
       Nothing -> DenseBoxedPages V.empty
       Just value -> DefaultedBoxedPages value emptyDirectory)

boxedPagedLength :: BoxedPaged a -> Int
boxedPagedLength = boxedLength
{-# INLINE boxedPagedLength #-}

-- | Index with a handle already admitted by the owning triangulation. Dense
-- stores carry every live page explicitly; defaulted stores can answer an
-- absent or short page from their stored default. The handle domain is owned
-- by the DCEL, so this raw kernel performs no second, disagreeing bounds check.
boxedUnsafeIndex :: BoxedPaged a -> Int -> a
boxedUnsafeIndex BoxedPaged{boxedStorage} index =
  let !page = pageOf index
      !offset = offsetOf index
   in case boxedStorage of
        DenseBoxedPages pages ->
          V.unsafeIndex (V.unsafeIndex pages page) offset
        DefaultedBoxedPages fill pages ->
          case lookupDirectory page pages of
            Just values | offset < V.length values -> V.unsafeIndex values offset
            _ -> fill
{-# INLINE boxedUnsafeIndex #-}

boxedFromVector :: Maybe a -> V.Vector a -> BoxedPaged a
boxedFromVector fill values =
  let !chunks =
        List.unfoldr
          (\offset ->
             if offset >= V.length values
               then Nothing
               else
                 let !page = pageOf offset
                     !count = min pageSize (V.length values - offset)
                     !chunk = V.slice offset count values
                  in Just ((page, chunk), offset + count))
          0
   in
  BoxedPaged
    { boxedLength = V.length values
    , boxedStorage =
        case fill of
          Nothing -> DenseBoxedPages (V.fromList (map snd chunks))
          Just value -> DefaultedBoxedPages value (directoryFromAscList chunks)
    }

-- Resolved a page at a time. Asking the directory per element paid a descent
-- for every slot of a run the descent had already found.
boxedToVector :: forall a. BoxedPaged a -> V.Vector a
boxedToVector BoxedPaged{boxedLength, boxedStorage}
  | boxedLength <= 0 = V.empty
  | otherwise = case boxedStorage of
      DenseBoxedPages pages -> V.take boxedLength (V.concat (V.toList pages))
      DefaultedBoxedPages fill pages ->
        V.concat (map (pageRun fill pages) [0 .. pageOf (boxedLength - 1)])
 where
  pageRun :: a -> PageDirectory (V.Vector a) -> Int -> V.Vector a
  pageRun fill pages page =
    let !base = page `shiftL` pageBits
        !width = min pageSize (boxedLength - base)
     in case lookupDirectory page pages of
          Just values
            | V.length values >= width -> V.slice 0 width values
            | otherwise -> values V.++ V.replicate (width - V.length values) fill
          Nothing -> V.replicate width fill

-- The fill comes from the store, not from the caller. A thaw that took it as an
-- argument obliged every call site to name the right one, and 'boxedUpdate' —
-- which passed the value being written — named the wrong one. That was harmless
-- only while pages were never absent.
thawBoxedPaged :: TransactionShape -> BoxedPaged a -> ST s (MutableBoxedPaged s a)
thawBoxedPaged shape base = do
  mutableBoxedDirty <- newSTRef IntMap.empty
  mutableBoxedStats <-
    case shape of
      DenseTransaction -> pure Nothing
      LocalTransaction -> Just <$> newSTRef emptyPublicationStats
  pure
    MutableBoxedPaged
      { mutableBoxedBase = base
      , mutableBoxedDirty
      , mutableBoxedStats
      , mutableBoxedPristineBase = case boxedStorage base of
          DenseBoxedPages pages -> V.null pages
          DefaultedBoxedPages _ pages -> directorySize pages == 0
      }

readBoxedPaged :: MutableBoxedPaged s a -> Int -> ST s a
readBoxedPaged MutableBoxedPaged{mutableBoxedBase, mutableBoxedDirty} index = do
  dirty <- readSTRef mutableBoxedDirty
  let !page = pageOf index
      !offset = offsetOf index
  case IntMap.lookup page dirty of
    Just values -> MV.unsafeRead values offset
    Nothing -> case boxedStorage mutableBoxedBase of
      DenseBoxedPages pages ->
        pure (V.unsafeIndex (V.unsafeIndex pages page) offset)
      DefaultedBoxedPages fill pages ->
        case lookupDirectory page pages of
          Just values | offset < V.length values -> pure (V.unsafeIndex values offset)
          _ -> pure fill
{-# INLINE readBoxedPaged #-}

writeBoxedPaged :: MutableBoxedPaged s a -> Int -> a -> ST s ()
writeBoxedPaged paged index value = do
  let !page = pageOf index
      !offset = offsetOf index
  values <- ensureMutableBoxedPage paged page
  MV.unsafeWrite values offset value
{-# INLINE writeBoxedPaged #-}

-- | Whether the thaw found no materialized page. With no writer into the
-- element payload planes inside a transaction — there is none; they are
-- written only through the persistent setters, outside one — this answers for
-- the whole transaction that every slot reports the fill, so a rewrite has
-- nothing to return and a relocation has nothing to move. It is why the
-- near-universal @() () ()@ mesh pays a predictable branch and no page.
boxedThawPristine :: MutableBoxedPaged s a -> Bool
boxedThawPristine = mutableBoxedPristineBase
{-# INLINE boxedThawPristine #-}

-- | Return a contiguous run of slots to the store's fill. A slot no write has
-- reached already reports the fill, so a page absent from both the base and the
-- dirty set is left absent: recycling an element at its default keeps the store
-- sparse. Only a page holding written values is materialized and overwritten.
resetBoxedRange :: MutableBoxedPaged s a -> a -> Int -> Int -> ST s ()
resetBoxedRange paged@MutableBoxedPaged{mutableBoxedBase, mutableBoxedDirty} fill start count
  | count <= 0 = pure ()
  | otherwise = traverse_ resetChunk chunkStarts
 where
  !end = start + count
  chunkStarts =
    List.unfoldr
      (\index ->
         if index >= end
           then Nothing
           else
             let !width = min (pageSize - offsetOf index) (end - index)
              in Just ((index, width), index + width))
      start

  resetChunk (index, width) = do
    dirty <- readSTRef mutableBoxedDirty
    let !page = pageOf index
        !offset = offsetOf index
        !written =
          IntMap.member page dirty
            || case boxedStorage mutableBoxedBase of
              DenseBoxedPages pages ->
                page < V.length pages
                  && offset < V.length (V.unsafeIndex pages page)
              DefaultedBoxedPages _ pages ->
                case lookupDirectory page pages of
                  Just values -> offset < V.length values
                  Nothing -> False
    when written $ do
      values <- ensureMutableBoxedPage paged page
      MV.set (MV.slice offset width values) fill

ensureMutableBoxedPage :: forall s a. MutableBoxedPaged s a -> Int -> ST s (MV.MVector s a)
ensureMutableBoxedPage MutableBoxedPaged{mutableBoxedBase, mutableBoxedDirty, mutableBoxedStats} page = do
  dirty <- readSTRef mutableBoxedDirty
  case IntMap.lookup page dirty of
    Just values -> pure values
    Nothing -> do
      -- Copying a page slot by slot writes every element twice — once to
      -- establish the fill and once to overwrite it — and pays a write barrier
      -- per store. A full page is one thaw; a short tail page fills only the
      -- slots the base does not reach. This is the same shape the unboxed
      -- store already uses.
      (values, copiedCells) <- case boxedStorage mutableBoxedBase of
        DenseBoxedPages pages
          | page < V.length pages -> copyExisting (V.unsafeIndex pages page)
          | otherwise -> do
              vector <- MV.new pageSize
              pure (vector, 0)
        DefaultedBoxedPages fill pages ->
          case lookupDirectory page pages of
            Just old -> copyExisting old
            Nothing -> do
              vector <- MV.replicate pageSize fill
              pure (vector, 0)
      writeSTRef mutableBoxedDirty $! IntMap.insert page values dirty
      let !basePageCount = boxedPageCount mutableBoxedBase
          !basePage = page < basePageCount
      recordPublicationStats
        mutableBoxedStats
        (emptyPublicationStats
             { publicationBoxedBasePageOpens = if basePage then 1 else 0
             , publicationBoxedDirtyBasePages = if basePage then 1 else 0
             , publicationBoxedDirtyAppendedPages = if basePage then 0 else 1
             , publicationBoxedCopiedCells = copiedCells
             })
      pure values
 where
  boxedPageCount :: BoxedPaged a -> Int
  boxedPageCount base
    | boxedLength base <= 0 = 0
    | otherwise = pageOf (boxedLength base - 1) + 1

  copyExisting :: V.Vector a -> ST s (MV.MVector s a, Int)
  copyExisting old
    | V.length old == pageSize = (, V.length old) <$> V.thaw old
    | otherwise = do
        vector <- case boxedStorage mutableBoxedBase of
          DenseBoxedPages _ -> MV.new pageSize
          DefaultedBoxedPages fill _ -> MV.replicate pageSize fill
        V.copy (MV.slice 0 (V.length old) vector) old
        pure (vector, V.length old)

recordPublicationStats :: Maybe (STRef s PublicationStats) -> PublicationStats -> ST s ()
recordPublicationStats Nothing _ = pure ()
recordPublicationStats (Just statsRef) delta = modifySTRef' statsRef (addPublicationStats delta)
{-# INLINE recordPublicationStats #-}

freezeBoxedPaged :: forall s a. Int -> MutableBoxedPaged s a -> ST s (Either BoxedStorageError (BoxedPaged a))
freezeBoxedPaged length' MutableBoxedPaged{mutableBoxedBase, mutableBoxedDirty, mutableBoxedStats}
  | length' < 0 = pure (Left (BoxedFreezeLengthNegative length'))
  | otherwise = do
      dirty <- readSTRef mutableBoxedDirty
      frozenDirty <- traverse V.unsafeFreeze dirty
      case boxedStorage mutableBoxedBase of
        DenseBoxedPages _ ->
          recordPublicationStats
            mutableBoxedStats
            (emptyPublicationStats
                 { publicationBoxedBasePageEnumerations = pageCount
                 , publicationBoxedBasePageFreezes = pageCount
                 })
        DefaultedBoxedPages _ _ -> pure ()
      pure $ case boxedStorage mutableBoxedBase of
        DenseBoxedPages basePages -> do
          pages <-
            traverse
              (densePage frozenDirty basePages)
              [0 .. pageCount - 1]
          pure
            BoxedPaged
              { boxedLength = length'
              , boxedStorage = DenseBoxedPages (V.fromList pages)
              }
        DefaultedBoxedPages fill basePages ->
          let !merged =
                List.foldl'
                  (\pages (page, values) -> insertDirectory page values pages)
                  basePages
                  (IntMap.toAscList frozenDirty)
              !kept = directoryRestrict pageCount merged
              !trimmed = case directoryLookupMax kept of
                Nothing -> kept
                Just (lastPage, values) ->
                  let !lastLength = length' - lastPage * pageSize
                   in if lastLength < V.length values
                        then insertDirectory lastPage (V.take lastLength values) kept
                        else kept
           in Right
                BoxedPaged
                  { boxedLength = length'
                  , boxedStorage = DefaultedBoxedPages fill trimmed
                }
 where
  !pageCount = if length' <= 0 then 0 else pageOf (length' - 1) + 1

  densePage
    :: IntMap.IntMap (V.Vector a)
    -> V.Vector (V.Vector a)
    -> Int
    -> Either BoxedStorageError (V.Vector a)
  densePage frozenDirty basePages page =
    let !width = min pageSize (length' - page * pageSize)
     in case IntMap.lookup page frozenDirty of
          Just values -> Right (V.take width values)
          Nothing -> case basePages V.!? page of
            Just values -> Right (V.take width values)
            Nothing -> Left (BoxedFreezeDensePageMissing page pageCount)

readMutableBoxedPublicationStats :: MutableBoxedPaged s a -> ST s PublicationStats
readMutableBoxedPublicationStats MutableBoxedPaged{mutableBoxedStats} =
  maybe (pure emptyPublicationStats) readSTRef mutableBoxedStats

boxedUpdate :: Int -> a -> BoxedPaged a -> BoxedPaged a
boxedUpdate index value source@BoxedPaged{boxedStorage} =
  source{boxedStorage = updateStorage boxedStorage}
 where
  !page = pageOf index
  !offset = offsetOf index

  updateStorage (DenseBoxedPages pages) =
    let !values = V.unsafeIndex pages page
        !updated = V.modify (\mutable -> MV.unsafeWrite mutable offset value) values
     in DenseBoxedPages (V.modify (\mutable -> MV.unsafeWrite mutable page updated) pages)
  updateStorage (DefaultedBoxedPages fill pages) =
    let !values = case lookupDirectory page pages of
          Just existing
            | V.length existing == pageSize -> existing
            | otherwise -> existing V.++ V.replicate (pageSize - V.length existing) fill
          Nothing -> V.replicate pageSize fill
        !updated = V.modify (\mutable -> MV.unsafeWrite mutable offset value) values
     in DefaultedBoxedPages fill (insertDirectory page updated pages)

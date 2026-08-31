{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Dense bit-packed reachability closure over finite relations: Tarjan SCC with
-- per-component reachability, in single-'Word64' and packed-row variants.
module Moonlight.Category.Pure.Finite.DenseReachability
  ( DenseClosure (..),
    denseReachabilityWithCycles,
    denseReachabilityRows,
    relationUniverse,
    relationBitRows,
    transposeBitRows,
    objectIndexOf,
    objectComponentsFromIndices,
    objectSetFromBits,
    bitsDifference,
    intListBits,
    bitsToAscList,
  )
where

import Control.Monad (foldM, when)
import Control.Monad.ST (ST, runST)
import Data.Bits (bit, popCount, testBit, (.&.), (.|.))
import qualified Data.Bits as Bits
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Kind (Type)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector
import Data.Word (Word64)

type DenseClosure :: Type
data DenseClosure = DenseClosure
  { denseClosureReachabilityRows :: !(Vector Integer),
    denseClosureCycleComponents :: ![NonEmpty Int],
    denseClosureComponentCount :: !Int
  }
  deriving stock (Eq, Show)


data PackedRows = PackedRows !Int !(UVector.Vector Word64)

data MutablePackedRows s = MutablePackedRows !Int !(UMVector.MVector s Word64)

data RowCursor
  = EmptyRowCursor
  | RowCursor !Int !Int !Word64

denseReachabilityWithCycles :: Vector Integer -> DenseClosure
denseReachabilityWithCycles inputRows =
  runST (denseReachabilityWithCyclesST inputRows)
{-# INLINABLE denseReachabilityWithCycles #-}

denseReachabilityWithCyclesST :: forall s. Vector Integer -> ST s DenseClosure
denseReachabilityWithCyclesST inputRows
  | Vector.length inputRows <= wordBitCount =
      denseReachabilityWithCyclesWord64ST inputRows
  | otherwise =
      denseReachabilityWithCyclesPackedST inputRows
{-# INLINABLE denseReachabilityWithCyclesST #-}

denseReachabilityWithCyclesWord64ST :: forall s. Vector Integer -> ST s DenseClosure
denseReachabilityWithCyclesWord64ST inputRows = do
  let vertexCount = Vector.length inputRows
      rows =
        UVector.generate vertexCount $
          integerRowChunkWord vertexCount 0 . (inputRows Vector.!)

  discoveryOf <- UMVector.replicate vertexCount (-1 :: Int)
  lowlinkOf <- UMVector.replicate vertexCount (0 :: Int)
  onStackOf <- UMVector.replicate vertexCount False
  componentOf <- UMVector.replicate vertexCount (-1 :: Int)
  nextDiscovery <- newSTRef (0 :: Int)
  nextComponent <- newSTRef (0 :: Int)
  tarjanStack <- newSTRef ([] :: [Int])

  closure <- UMVector.replicate vertexCount (0 :: Word64)
  componentReach <- UMVector.replicate vertexCount (0 :: Word64)
  cyclicAccumulator <- newSTRef ([] :: [NonEmpty Int])

  let discover :: Int -> ST s ()
      discover vertex = do
        stamp <- readSTRef nextDiscovery
        writeSTRef nextDiscovery (stamp + 1)
        UMVector.write discoveryOf vertex stamp
        UMVector.write lowlinkOf vertex stamp
        UMVector.write onStackOf vertex True
        modifySTRef' tarjanStack (vertex :)

      popComponentMembers :: Int -> ST s (NonEmpty Int)
      popComponentMembers rootVertex = do
        stacked <- readSTRef tarjanStack
        let (above, rest) = List.break (== rootVertex) stacked
        case rest of
          _root : below -> do
            writeSTRef tarjanStack below
            pure (rootVertex :| above)
          [] -> do
            writeSTRef tarjanStack []
            pure (rootVertex :| above)

      distinctSuccessorComponents :: Word64 -> ST s IntSet
      distinctSuccessorComponents =
        collect IntSet.empty
        where
          collect :: IntSet -> Word64 -> ST s IntSet
          collect !acc successorBits
            | successorBits == 0 =
                pure acc
            | otherwise = do
                let successor = Bits.countTrailingZeros successorBits
                componentId <- UMVector.read componentOf successor
                let nextAcc =
                      if componentId >= 0
                        then IntSet.insert componentId acc
                        else acc
                collect nextAcc (clearLowestSetBitWord successorBits)

      emitComponent :: Int -> ST s ()
      emitComponent rootVertex = do
        members <- popComponentMembers rootVertex
        componentId <- readSTRef nextComponent
        writeSTRef nextComponent (componentId + 1)
        traverse_
          ( \member -> do
              UMVector.write componentOf member componentId
              UMVector.write onStackOf member False
          )
          members
        let memberBits = List.foldl' (\bits member -> bits .|. bit member) 0 members
            outBits = List.foldl' (\bits member -> bits .|. wordRowAt rows member) 0 members
            successorBits = outBits `withoutWordBits` memberBits
            cyclic =
              case members of
                single :| [] -> testBit (wordRowAt rows single) single
                _ -> True
            selfBits =
              if cyclic
                then memberBits
                else 0
        successorComponents <- distinctSuccessorComponents successorBits
        downstreamBits <-
          foldM
            ( \ !acc successorComponentId ->
                (acc .|.) <$> UMVector.read componentReach successorComponentId
            )
            0
            (IntSet.toList successorComponents)
        let reachabilityBits = selfBits .|. successorBits .|. downstreamBits
        UMVector.write componentReach componentId reachabilityBits
        traverse_
          (\member -> UMVector.write closure member reachabilityBits)
          members
        when cyclic $
          modifySTRef' cyclicAccumulator (NonEmpty.sort members :)

      walk :: [(Int, Word64)] -> ST s ()
      walk [] =
        pure ()
      walk ((vertex, remaining) : parents)
        | remaining /= 0 = do
            let successor = Bits.countTrailingZeros remaining
                remainingTail = clearLowestSetBitWord remaining
            successorDiscovery <- UMVector.read discoveryOf successor
            if successorDiscovery < 0
              then do
                discover successor
                walk
                  ( (successor, wordRowAt rows successor) :
                    (vertex, remainingTail) :
                    parents
                  )
              else do
                stacked <- UMVector.read onStackOf successor
                when stacked $ do
                  lowlink <- UMVector.read lowlinkOf vertex
                  UMVector.write lowlinkOf vertex (min lowlink successorDiscovery)
                walk ((vertex, remainingTail) : parents)
        | otherwise = do
            lowlink <- UMVector.read lowlinkOf vertex
            discovery <- UMVector.read discoveryOf vertex
            when (lowlink == discovery) (emitComponent vertex)
            case parents of
              (parent, _) : _ -> do
                parentLowlink <- UMVector.read lowlinkOf parent
                UMVector.write lowlinkOf parent (min parentLowlink lowlink)
              [] ->
                pure ()
            walk parents

  traverse_
    ( \vertex -> do
        discovery <- UMVector.read discoveryOf vertex
        when (discovery < 0) $ do
          discover vertex
          walk [(vertex, wordRowAt rows vertex)]
    )
    [0 .. vertexCount - 1]

  frozenClosure <- Vector.generateM vertexCount (fmap toInteger . UMVector.read closure)
  cycleComponents <- readSTRef cyclicAccumulator
  totalComponents <- readSTRef nextComponent
  pure
    DenseClosure
      { denseClosureReachabilityRows = frozenClosure,
        denseClosureCycleComponents = List.sortOn NonEmpty.head cycleComponents,
        denseClosureComponentCount = totalComponents
      }
{-# INLINABLE denseReachabilityWithCyclesWord64ST #-}

denseReachabilityWithCyclesPackedST :: forall s. Vector Integer -> ST s DenseClosure
denseReachabilityWithCyclesPackedST inputRows = do
  let vertexCount = Vector.length inputRows
      rows@(PackedRows chunkCount _) = packedRowsFromIntegerRows vertexCount inputRows

  discoveryOf <- UMVector.replicate vertexCount (-1 :: Int)
  lowlinkOf <- UMVector.replicate vertexCount (0 :: Int)
  onStackOf <- UMVector.replicate vertexCount False
  componentOf <- UMVector.replicate vertexCount (-1 :: Int)
  nextDiscovery <- newSTRef (0 :: Int)
  nextComponent <- newSTRef (0 :: Int)
  tarjanStack <- newSTRef ([] :: [Int])

  closure <- newMutablePackedRows vertexCount chunkCount
  componentReach <- newMutablePackedRows vertexCount chunkCount
  scratchRow <- UMVector.replicate chunkCount (0 :: Word64)
  cyclicAccumulator <- newSTRef ([] :: [NonEmpty Int])

  let discover :: Int -> ST s ()
      discover vertex = do
        stamp <- readSTRef nextDiscovery
        writeSTRef nextDiscovery (stamp + 1)
        UMVector.write discoveryOf vertex stamp
        UMVector.write lowlinkOf vertex stamp
        UMVector.write onStackOf vertex True
        modifySTRef' tarjanStack (vertex :)

      popComponentMembers :: Int -> ST s (NonEmpty Int)
      popComponentMembers rootVertex = do
        stacked <- readSTRef tarjanStack
        let (above, rest) = List.break (== rootVertex) stacked
        case rest of
          _root : below -> do
            writeSTRef tarjanStack below
            pure (rootVertex :| above)
          [] -> do
            writeSTRef tarjanStack []
            pure (rootVertex :| above)

      distinctSuccessorComponents :: ST s IntSet
      distinctSuccessorComponents =
        foldChunkIndicesM chunkCount IntSet.empty $ \ !acc chunkIndex -> do
          successorWord <- UMVector.read scratchRow chunkIndex
          collectChunkSuccessorComponents (chunkIndex * wordBitCount) acc successorWord
        where
          collectChunkSuccessorComponents :: Int -> IntSet -> Word64 -> ST s IntSet
          collectChunkSuccessorComponents !chunkBase !acc successorWord
            | successorWord == 0 =
                pure acc
            | otherwise = do
                let successor = chunkBase + Bits.countTrailingZeros successorWord
                componentId <- UMVector.read componentOf successor
                let nextAcc =
                      if componentId >= 0
                        then IntSet.insert componentId acc
                        else acc
                collectChunkSuccessorComponents chunkBase nextAcc (clearLowestSetBitWord successorWord)

      emitComponent :: Int -> ST s ()
      emitComponent rootVertex = do
        members <- popComponentMembers rootVertex
        componentId <- readSTRef nextComponent
        writeSTRef nextComponent (componentId + 1)
        traverse_ (\member -> do
          UMVector.write componentOf member componentId
          UMVector.write onStackOf member False) members
        clearScratchRow scratchRow chunkCount
        traverse_ (orPackedRowIntoScratch scratchRow rows) members
        traverse_ (clearScratchBit scratchRow) members
        let cyclic =
              case members of
                single :| [] -> testPackedRowBit rows single single
                _ -> True
        successorComponents <- distinctSuccessorComponents
        traverse_
          (orMutablePackedRowIntoScratch scratchRow componentReach)
          (IntSet.toList successorComponents)
        when cyclic $
          traverse_ (setScratchBit scratchRow) members
        writeScratchRowToMutablePackedRow scratchRow componentReach componentId
        traverse_
          (writeScratchRowToMutablePackedRow scratchRow closure)
          members
        when cyclic $
          modifySTRef' cyclicAccumulator (NonEmpty.sort members :)

      walk :: [(Int, RowCursor)] -> ST s ()
      walk [] =
        pure ()
      walk ((vertex, remaining) : parents) =
        case nextRowCursorSuccessor rows remaining of
          Just (successor, remainingTail) -> do
            successorDiscovery <- UMVector.read discoveryOf successor
            if successorDiscovery < 0
              then do
                discover successor
                walk
                  ( (successor, initialRowCursor rows successor) :
                    (vertex, remainingTail) :
                    parents
                  )
              else do
                stacked <- UMVector.read onStackOf successor
                when stacked $ do
                  lowlink <- UMVector.read lowlinkOf vertex
                  UMVector.write lowlinkOf vertex (min lowlink successorDiscovery)
                walk ((vertex, remainingTail) : parents)
          Nothing -> do
            lowlink <- UMVector.read lowlinkOf vertex
            discovery <- UMVector.read discoveryOf vertex
            when (lowlink == discovery) (emitComponent vertex)
            case parents of
              (parent, _) : _ -> do
                parentLowlink <- UMVector.read lowlinkOf parent
                UMVector.write lowlinkOf parent (min parentLowlink lowlink)
              [] ->
                pure ()
            walk parents

  traverse_ (\vertex -> do
    discovery <- UMVector.read discoveryOf vertex
    when (discovery < 0) $ do
      discover vertex
      walk [(vertex, initialRowCursor rows vertex)]) [0 .. vertexCount - 1]

  frozenClosure <- integerRowsFromMutablePackedRows vertexCount closure
  cycleComponents <- readSTRef cyclicAccumulator
  totalComponents <- readSTRef nextComponent
  pure
    DenseClosure
      { denseClosureReachabilityRows = frozenClosure,
        denseClosureCycleComponents = List.sortOn NonEmpty.head cycleComponents,
        denseClosureComponentCount = totalComponents
      }
{-# INLINABLE denseReachabilityWithCyclesPackedST #-}

denseReachabilityRows :: Vector Integer -> Vector Integer
denseReachabilityRows =
  denseClosureReachabilityRows . denseReachabilityWithCycles
{-# INLINABLE denseReachabilityRows #-}

lowestSetBitIndex :: Integer -> Int
lowestSetBitIndex bits =
  popCount (lowestBit - 1)
  where
    lowestBit = bits .&. negate bits
{-# INLINE lowestSetBitIndex #-}

clearLowestSetBit :: Integer -> Integer
clearLowestSetBit bits =
  bits .&. (bits - 1)
{-# INLINE clearLowestSetBit #-}

withoutBits :: Integer -> Integer -> Integer
withoutBits leftBits rightBits =
  leftBits .&. Bits.complement rightBits
{-# INLINE withoutBits #-}

withoutWordBits :: Word64 -> Word64 -> Word64
withoutWordBits leftBits rightBits =
  leftBits .&. Bits.complement rightBits
{-# INLINE withoutWordBits #-}

wordBitCount :: Int
wordBitCount =
  64
{-# INLINE wordBitCount #-}

chunksForBitCount :: Int -> Int
chunksForBitCount bitCount =
  if bitCount <= 0
    then 0
    else (bitCount + wordBitCount - 1) `quot` wordBitCount
{-# INLINE chunksForBitCount #-}

rowChunkOffset :: Int -> Int -> Int -> Int
rowChunkOffset chunkCount rowIndex chunkIndex =
  rowIndex * chunkCount + chunkIndex
{-# INLINE rowChunkOffset #-}

wordMaskForChunk :: Int -> Int -> Word64
wordMaskForChunk bitCount chunkIndex
  | remainingBits >= wordBitCount = maxBound
  | remainingBits <= 0 = 0
  | otherwise = bit remainingBits - 1
  where
    remainingBits = bitCount - chunkIndex * wordBitCount
{-# INLINE wordMaskForChunk #-}

integerRowChunkWord :: Int -> Int -> Integer -> Word64
integerRowChunkWord bitCount chunkIndex bits =
  if chunkIndex == 0
    then fromInteger bits .&. wordMaskForChunk bitCount chunkIndex
    else fromInteger ((bits `Bits.shiftR` (chunkIndex * wordBitCount)) .&. toInteger (wordMaskForChunk bitCount chunkIndex))
{-# INLINE integerRowChunkWord #-}

wordRowAt :: UVector.Vector Word64 -> Int -> Word64
wordRowAt = (UVector.!)
{-# INLINE wordRowAt #-}

packedRowsFromIntegerRows :: Int -> Vector Integer -> PackedRows
packedRowsFromIntegerRows bitCount rows =
  PackedRows chunkCount $
    UVector.generate (bitCount * chunkCount) $ \flatIndex ->
      let (rowIndex, chunkIndex) = flatIndex `quotRem` chunkCount
       in integerRowChunkWord bitCount chunkIndex (rows Vector.! rowIndex)
  where
    chunkCount = chunksForBitCount bitCount
{-# INLINE packedRowsFromIntegerRows #-}

newMutablePackedRows :: Int -> Int -> ST s (MutablePackedRows s)
newMutablePackedRows rowCount chunkCount =
  MutablePackedRows chunkCount <$> UMVector.replicate (rowCount * chunkCount) 0
{-# INLINE newMutablePackedRows #-}

packedRowChunkAt :: PackedRows -> Int -> Int -> Word64
packedRowChunkAt (PackedRows chunkCount chunks) rowIndex chunkIndex =
  chunks UVector.! rowChunkOffset chunkCount rowIndex chunkIndex
{-# INLINE packedRowChunkAt #-}

readMutablePackedRowChunk :: MutablePackedRows s -> Int -> Int -> ST s Word64
readMutablePackedRowChunk (MutablePackedRows chunkCount chunks) rowIndex chunkIndex =
  UMVector.read chunks (rowChunkOffset chunkCount rowIndex chunkIndex)
{-# INLINE readMutablePackedRowChunk #-}

writeMutablePackedRowChunk :: MutablePackedRows s -> Int -> Int -> Word64 -> ST s ()
writeMutablePackedRowChunk (MutablePackedRows chunkCount chunks) rowIndex chunkIndex =
  UMVector.write chunks (rowChunkOffset chunkCount rowIndex chunkIndex)
{-# INLINE writeMutablePackedRowChunk #-}

foldChunkIndicesM :: Monad m => Int -> a -> (a -> Int -> m a) -> m a
foldChunkIndicesM chunkCount initial step =
  ascend 0 initial
  where
    ascend !chunkIndex !acc
      | chunkIndex >= chunkCount = pure acc
      | otherwise = do
          nextAcc <- step acc chunkIndex
          ascend (chunkIndex + 1) nextAcc
{-# INLINE foldChunkIndicesM #-}

traverseChunkIndices_ :: Monad m => Int -> (Int -> m ()) -> m ()
traverseChunkIndices_ chunkCount action =
  foldChunkIndicesM chunkCount () (\() chunkIndex -> action chunkIndex)
{-# INLINE traverseChunkIndices_ #-}

clearScratchRow :: UMVector.MVector s Word64 -> Int -> ST s ()
clearScratchRow scratchRow chunkCount =
  traverseChunkIndices_ chunkCount $ \chunkIndex ->
    UMVector.write scratchRow chunkIndex 0
{-# INLINE clearScratchRow #-}

orPackedRowIntoScratch :: UMVector.MVector s Word64 -> PackedRows -> Int -> ST s ()
orPackedRowIntoScratch scratchRow rows@(PackedRows chunkCount _) rowIndex =
  traverseChunkIndices_ chunkCount $ \chunkIndex -> do
    scratchWord <- UMVector.read scratchRow chunkIndex
    let rowWord = packedRowChunkAt rows rowIndex chunkIndex
    UMVector.write scratchRow chunkIndex (scratchWord .|. rowWord)
{-# INLINE orPackedRowIntoScratch #-}

orMutablePackedRowIntoScratch :: UMVector.MVector s Word64 -> MutablePackedRows s -> Int -> ST s ()
orMutablePackedRowIntoScratch scratchRow murows@(MutablePackedRows chunkCount _) rowIndex =
  traverseChunkIndices_ chunkCount $ \chunkIndex -> do
    scratchWord <- UMVector.read scratchRow chunkIndex
    rowWord <- readMutablePackedRowChunk murows rowIndex chunkIndex
    UMVector.write scratchRow chunkIndex (scratchWord .|. rowWord)
{-# INLINE orMutablePackedRowIntoScratch #-}

writeScratchRowToMutablePackedRow :: UMVector.MVector s Word64 -> MutablePackedRows s -> Int -> ST s ()
writeScratchRowToMutablePackedRow scratchRow murows@(MutablePackedRows chunkCount _) rowIndex =
  traverseChunkIndices_ chunkCount $ \chunkIndex -> do
    scratchWord <- UMVector.read scratchRow chunkIndex
    writeMutablePackedRowChunk murows rowIndex chunkIndex scratchWord
{-# INLINE writeScratchRowToMutablePackedRow #-}

scratchBitAddress :: Int -> (Int, Int)
scratchBitAddress bitIndex =
  bitIndex `quotRem` wordBitCount
{-# INLINE scratchBitAddress #-}

clearScratchBit :: UMVector.MVector s Word64 -> Int -> ST s ()
clearScratchBit scratchRow bitIndex = do
  let (chunkIndex, wordBitIndex) = scratchBitAddress bitIndex
  scratchWord <- UMVector.read scratchRow chunkIndex
  UMVector.write scratchRow chunkIndex (scratchWord .&. Bits.complement (bit wordBitIndex))
{-# INLINE clearScratchBit #-}

setScratchBit :: UMVector.MVector s Word64 -> Int -> ST s ()
setScratchBit scratchRow bitIndex = do
  let (chunkIndex, wordBitIndex) = scratchBitAddress bitIndex
  scratchWord <- UMVector.read scratchRow chunkIndex
  UMVector.write scratchRow chunkIndex (scratchWord .|. bit wordBitIndex)
{-# INLINE setScratchBit #-}

clearLowestSetBitWord :: Word64 -> Word64
clearLowestSetBitWord word =
  word .&. (word - 1)
{-# INLINE clearLowestSetBitWord #-}

testPackedRowBit :: PackedRows -> Int -> Int -> Bool
testPackedRowBit rows rowIndex bitIndex =
  testBit (packedRowChunkAt rows rowIndex chunkIndex) wordBitIndex
  where
    (chunkIndex, wordBitIndex) = scratchBitAddress bitIndex
{-# INLINE testPackedRowBit #-}

initialRowCursor :: PackedRows -> Int -> RowCursor
initialRowCursor rows rowIndex =
  rowCursorFromChunk rows rowIndex 0
{-# INLINE initialRowCursor #-}

rowCursorFromChunk :: PackedRows -> Int -> Int -> RowCursor
rowCursorFromChunk rows@(PackedRows chunkCount _) rowIndex chunkIndex
  | chunkIndex >= chunkCount = EmptyRowCursor
  | chunkWord == 0 = rowCursorFromChunk rows rowIndex (chunkIndex + 1)
  | otherwise = RowCursor rowIndex chunkIndex chunkWord
  where
    chunkWord = packedRowChunkAt rows rowIndex chunkIndex
{-# INLINE rowCursorFromChunk #-}

rowCursorTail :: PackedRows -> Int -> Int -> Word64 -> RowCursor
rowCursorTail rows rowIndex chunkIndex chunkWord =
  if tailWord == 0
    then rowCursorFromChunk rows rowIndex (chunkIndex + 1)
    else RowCursor rowIndex chunkIndex tailWord
  where
    tailWord = clearLowestSetBitWord chunkWord
{-# INLINE rowCursorTail #-}

nextRowCursorSuccessor :: PackedRows -> RowCursor -> Maybe (Int, RowCursor)
nextRowCursorSuccessor _ EmptyRowCursor =
  Nothing
nextRowCursorSuccessor rows (RowCursor rowIndex chunkIndex chunkWord) =
  Just
    ( chunkIndex * wordBitCount + Bits.countTrailingZeros chunkWord,
      rowCursorTail rows rowIndex chunkIndex chunkWord
    )
{-# INLINE nextRowCursorSuccessor #-}

integerRowsFromMutablePackedRows :: Int -> MutablePackedRows s -> ST s (Vector Integer)
integerRowsFromMutablePackedRows rowCount murows =
  Vector.generateM rowCount (integerFromMutablePackedRow murows)
{-# INLINE integerRowsFromMutablePackedRows #-}

integerFromMutablePackedRow :: MutablePackedRows s -> Int -> ST s Integer
integerFromMutablePackedRow murows@(MutablePackedRows chunkCount _) rowIndex =
  descend (chunkCount - 1) 0
  where
    descend !chunkIndex !acc
      | chunkIndex < 0 = pure acc
      | otherwise = do
          chunkWord <- readMutablePackedRowChunk murows rowIndex chunkIndex
          descend (chunkIndex - 1) ((acc `Bits.shiftL` wordBitCount) .|. toInteger chunkWord)
{-# INLINE integerFromMutablePackedRow #-}


relationUniverse :: Ord obj => Map obj (Set obj) -> Set obj
relationUniverse =
  Map.foldlWithKey' (\accumulated objectValue members -> Set.insert objectValue (Set.union members accumulated)) Set.empty

relationBitRows :: Ord obj => Map obj Int -> Vector obj -> Map obj (Set obj) -> Vector Integer
relationBitRows objectIndex objectVector relation =
  Vector.map
    ( \objectValue ->
        Map.findWithDefault Set.empty objectValue relation
          & Set.toAscList
          & mapMaybe (`Map.lookup` objectIndex)
          & intListBits
    )
    objectVector

transposeBitRows :: Int -> Vector Integer -> Vector Integer
transposeBitRows objectCount rows =
  Vector.generate
    objectCount
    ( \targetIndex ->
        [0 .. objectCount - 1]
          & foldr
            ( \sourceIndex predecessorBits ->
                if maybe False (`testBit` targetIndex) (rows Vector.!? sourceIndex)
                  then predecessorBits .|. bit sourceIndex
                  else predecessorBits
            )
            0
    )

objectIndexOf :: Ord obj => Vector obj -> Map obj Int
objectIndexOf =
  Vector.ifoldl'
    (\objectIndex objectPosition objectValue -> Map.insert objectValue objectPosition objectIndex)
    Map.empty

objectComponentsFromIndices :: Ord obj => Vector obj -> [NonEmpty Int] -> [NonEmpty obj]
objectComponentsFromIndices objectVector =
  List.sortOn NonEmpty.head . mapMaybe (objectComponentFromIndices objectVector)

objectComponentFromIndices :: Ord obj => Vector obj -> NonEmpty Int -> Maybe (NonEmpty obj)
objectComponentFromIndices objectVector component =
  component
    & NonEmpty.toList
    & mapMaybe (objectVector Vector.!?)
    & List.sort
    & NonEmpty.nonEmpty

objectSetFromBits :: Ord obj => Vector obj -> Integer -> Set obj
objectSetFromBits objectVector bits =
  bitsToAscList (Vector.length objectVector) bits
    & mapMaybe (objectVector Vector.!?)
    & Set.fromList

bitsDifference :: Integer -> Integer -> Integer
bitsDifference leftBits rightBits =
  leftBits `withoutBits` rightBits

intListBits :: [Int] -> Integer
intListBits =
  foldr (\objectIndex bits -> bits .|. bit objectIndex) 0
{-# INLINE intListBits #-}

bitsToAscList :: Int -> Integer -> [Int]
bitsToAscList objectCount bits
  | objectCount <= 0 = []
  | bits < 0 = [0 .. objectCount - 1]
  | objectCount <= wordBitCount =
      [0 .. objectCount - 1] & filter (testBit bits)
  | otherwise =
      collect [] bits
  where
    collect !acc remainingBits
      | remainingBits == 0 = List.reverse acc
      | objectIndex >= objectCount = List.reverse acc
      | otherwise =
          collect (objectIndex : acc) (clearLowestSetBit remainingBits)
      where
        objectIndex = lowestSetBitIndex remainingBits
{-# INLINE bitsToAscList #-}

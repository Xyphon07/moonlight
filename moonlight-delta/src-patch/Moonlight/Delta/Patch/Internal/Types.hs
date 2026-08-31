{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Moonlight.Delta.Patch.Internal.Types
  ( CellPatch (..),
    Cell (..),
    Endpoint (..),
    PatchKey,
    PatchValue,
    buildKeyColumn,
    KeyColumn (..),
    ValueRun (..),
    ValueColumn (..),
    EndpointColumn (..),
    Page (..),
    Patch (..),
    ApplyError (..),
    ComposeError (..),
    ReplayError (..),
    BoundaryResult (..),
    CodecStats (..),
    pageCapacity,
    smallFormThreshold,
    entryCount,
    pagesOf,
    smallCellsToAscList,
    cellsForInstance,
    normalize,
    null,
    size,
    netSizeDelta,
    support,
    keyColumnAt,
    keyColumnCount,
    valueColumnAt,
    valueColumnFromArray,
    debugCodecStats,
    pageKeyAt,
  )
where

import Data.Bits
  ( Bits ((.&.), popCount, shiftL, testBit),
    FiniteBits (finiteBitSize),
  )
import Data.Kind (Constraint, Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Primitive.SmallArray
  ( SmallArray,
    emptySmallArray,
    indexSmallArray,
    smallArrayFromList,
    sizeofSmallArray,
  )
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Moonlight.Delta.Normalize
  ( DeltaNormalize (..),
  )
import Moonlight.Delta.Support
  ( DeltaSupport (..),
  )
import Numeric.Natural (Natural)
import Prelude hiding (null)

pageCapacity :: Int
pageCapacity =
  finiteBitSize (0 :: Word64)
{-# INLINE pageCapacity #-}

type CellPatch :: Type -> Type
data CellPatch value
  = AssertAbsent
  | Insert !value
  | Delete !value
  | Replace !value !value
  deriving stock (Eq, Ord, Show)

type Cell :: Type -> Type -> Type
data Cell key value = Cell !key !(CellPatch value)
  deriving stock (Eq, Ord, Show)

type Endpoint :: Type -> Type
data Endpoint value
  = EndpointAbsent
  | EndpointPresent !value
  deriving stock (Eq, Ord, Show)

type KeyColumn :: Type -> Type
data KeyColumn key where
  ExplicitKeys :: !(SmallArray key) -> KeyColumn key
  IntRangeKeys :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> KeyColumn Int
  IntAffineKeys :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> KeyColumn Int

instance Eq key => Eq (KeyColumn key) where
  left == right =
    keyColumnToList left == keyColumnToList right
  {-# INLINE (==) #-}

instance Ord key => Ord (KeyColumn key) where
  compare left right =
    compare (keyColumnToList left) (keyColumnToList right)
  {-# INLINE compare #-}

instance Show key => Show (KeyColumn key) where
  showsPrec precedence column =
    showParen
      (precedence > 10)
      ( showString "KeyColumn "
          . shows (keyColumnToList column)
      )

type PatchRepresentation :: Type
data PatchRepresentation
  = GenericPatchRepresentation
  | IntPatchRepresentation

type PatchRepresentationOf :: Type -> PatchRepresentation
type family PatchRepresentationOf value where
  PatchRepresentationOf Int = 'IntPatchRepresentation
  PatchRepresentationOf _value = 'GenericPatchRepresentation

type PatchKeyColumnBuilder :: PatchRepresentation -> Type -> Constraint
class Ord key => PatchKeyColumnBuilder representation key where
  buildSelectedKeyColumn :: Proxy representation -> SmallArray key -> KeyColumn key

instance Ord key => PatchKeyColumnBuilder 'GenericPatchRepresentation key where
  buildSelectedKeyColumn _representation =
    ExplicitKeys
  {-# INLINE buildSelectedKeyColumn #-}

instance PatchKeyColumnBuilder 'IntPatchRepresentation Int where
  buildSelectedKeyColumn _representation keys =
    case sizeofSmallArray keys of
      0 ->
        ExplicitKeys keys
      1 ->
        IntRangeKeys (indexSmallArray keys 0) 1
      count ->
        let !start = indexSmallArray keys 0
            !step = indexSmallArray keys 1 - start
         in if intKeysAreAffine keys start step count 2
              then
                if step == 1
                  then IntRangeKeys start count
                  else IntAffineKeys start step count
              else ExplicitKeys keys
  {-# INLINE buildSelectedKeyColumn #-}

type PatchKey :: Type -> Constraint
type PatchKey key = PatchKeyColumnBuilder (PatchRepresentationOf key) key

buildKeyColumn :: forall key. PatchKey key => SmallArray key -> KeyColumn key
buildKeyColumn =
  buildSelectedKeyColumn (Proxy @(PatchRepresentationOf key))
{-# INLINE buildKeyColumn #-}

type ValueRun :: Type -> Type
data ValueRun value = ValueRun {-# UNPACK #-} !Int !value
  deriving stock (Eq, Ord, Show)

type ValueColumn :: Type -> Type
data ValueColumn value where
  ConstantValues :: {-# UNPACK #-} !Int -> !value -> ValueColumn value
  RunValues :: {-# UNPACK #-} !Int -> !(SmallArray (ValueRun value)) -> ValueColumn value
  DenseValues :: !(SmallArray value) -> ValueColumn value
  IntAffineValues :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> ValueColumn Int

instance Eq value => Eq (ValueColumn value) where
  left == right =
    valueColumnToList left == valueColumnToList right
  {-# INLINE (==) #-}

instance Ord value => Ord (ValueColumn value) where
  compare left right =
    compare (valueColumnToList left) (valueColumnToList right)
  {-# INLINE compare #-}

instance Show value => Show (ValueColumn value) where
  showsPrec precedence column =
    showParen
      (precedence > 10)
      ( showString "ValueColumn "
          . shows (valueColumnToList column)
      )

type PatchValueColumnBuilder :: PatchRepresentation -> Type -> Constraint
class Eq value => PatchValueColumnBuilder representation value where
  buildSelectedValueColumn :: Proxy representation -> SmallArray value -> ValueColumn value

instance Eq value => PatchValueColumnBuilder 'GenericPatchRepresentation value where
  buildSelectedValueColumn _representation =
    buildGenericValueColumn
  {-# INLINE buildSelectedValueColumn #-}

instance PatchValueColumnBuilder 'IntPatchRepresentation Int where
  buildSelectedValueColumn _representation =
    buildIntValueColumn
  {-# INLINE buildSelectedValueColumn #-}

type PatchValue :: Type -> Constraint
type PatchValue value = PatchValueColumnBuilder (PatchRepresentationOf value) value

buildValueColumn :: forall value. PatchValue value => SmallArray value -> ValueColumn value
buildValueColumn =
  buildSelectedValueColumn (Proxy @(PatchRepresentationOf value))
{-# INLINE buildValueColumn #-}

type EndpointColumn :: Type -> Type
data EndpointColumn value
  = AllPresent !(ValueColumn value)
  | Presence {-# UNPACK #-} !Word64 !(ValueColumn value)
  deriving stock (Eq, Ord, Show)

type Page :: Type -> Type -> Type
data Page key value = Page
  { pageCount :: {-# UNPACK #-} !Int,
    pagePrefixKeys :: !(KeyColumn key),
    pageBeforeColumn :: !(EndpointColumn value),
    pageAfterColumn :: !(EndpointColumn value)
  }
  deriving stock (Eq, Ord, Show)

type Patch :: Type -> Type -> Type
data Patch key value
  = SmallPatch !(SmallArray (Cell key value))
  | PagedPatch {-# UNPACK #-} !Int !(Map key (Page key value))

smallFormThreshold :: Int
smallFormThreshold =
  16
{-# INLINE smallFormThreshold #-}

entryCount :: Patch key value -> Int
entryCount patch =
  case patch of
    SmallPatch cells ->
      sizeofSmallArray cells
    PagedPatch count _pages ->
      count
{-# INLINE entryCount #-}

pagesOf :: Patch key value -> Map key (Page key value)
pagesOf patch =
  case patch of
    PagedPatch _count pages ->
      pages
    SmallPatch _cells ->
      Map.empty
{-# INLINE pagesOf #-}

instance (Eq key, Eq value) => Eq (Patch key value) where
  left == right =
    cellsForInstance left == cellsForInstance right
  {-# INLINE (==) #-}

instance (Ord key, Ord value) => Ord (Patch key value) where
  compare left right =
    compare (cellsForInstance left) (cellsForInstance right)
  {-# INLINE compare #-}

instance (Show key, Show value) => Show (Patch key value) where
  showsPrec precedence patch =
    showParen
      (precedence > 10)
      ( showString "Patch "
          . shows (cellsForInstance patch)
      )


type ApplyError :: Type -> Type -> Type
data ApplyError key value = ApplyBeforeMismatch
  { mismatchKey :: !key,
    expectedBefore :: !(Maybe value),
    actualBefore :: !(Maybe value)
  }
  deriving stock (Eq, Ord, Show)

type ComposeError :: Type -> Type -> Type
data ComposeError key value = ComposeBoundaryMismatch
  { boundaryKey :: !key,
    olderAfter :: !(Maybe value),
    newerBefore :: !(Maybe value)
  }
  deriving stock (Eq, Ord, Show)

type ReplayError :: Type -> Type -> Type
data ReplayError key value = ReplayApplyError
  { replayIndex :: !Natural,
    replayApply :: !(ApplyError key value)
  }
  deriving stock (Eq, Ord, Show)

type BoundaryResult :: Type -> Type
data BoundaryResult error
  = PageBoundaryMatched
  | PageBoundaryDiverged
  | PageBoundaryRejected !error

data CodecStats = CodecStats
  { codecExplicitKeyColumns :: {-# UNPACK #-} !Int,
    codecRangeKeyColumns :: {-# UNPACK #-} !Int,
    codecAffineKeyColumns :: {-# UNPACK #-} !Int,
    codecAbsentColumns :: {-# UNPACK #-} !Int,
    codecConstantColumns :: {-# UNPACK #-} !Int,
    codecRunColumns :: {-# UNPACK #-} !Int,
    codecAffineValueColumns :: {-# UNPACK #-} !Int,
    codecDenseColumns :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Show)

normalize :: Patch key value -> Patch key value
normalize patchValue =
  case patchValue of
    SmallPatch _cells ->
      patchValue
    PagedPatch count _pages
      | count < smallFormThreshold ->
          SmallPatch
            ( smallArrayFromList
                [ Cell key cell
                  | (key, cell) <- cellsForInstance patchValue
                ]
            )
      | otherwise ->
          patchValue
{-# INLINE normalize #-}

null :: Patch key value -> Bool
null patch =
  entryCount patch == 0
{-# INLINE null #-}

size :: Patch key value -> Int
size =
  entryCount
{-# INLINE size #-}

netSizeDelta :: Patch key value -> Int
netSizeDelta patch =
  case patch of
    SmallPatch cells ->
      smallCellsNetSizeDelta cells 0 0
    PagedPatch _count pages ->
      Map.foldlWithKey' addPage 0 pages
  where
    addPage :: Int -> key -> Page key value -> Int
    addPage !total _maximumKey page =
      total + pageNetSizeDelta page
{-# INLINE netSizeDelta #-}

smallCellsNetSizeDelta :: SmallArray (Cell key value) -> Int -> Int -> Int
smallCellsNetSizeDelta cells !index !total
  | index == sizeofSmallArray cells =
      total
  | otherwise =
      let Cell _key cell = indexSmallArray cells index
       in smallCellsNetSizeDelta cells (index + 1) (total + cellNetSizeDelta cell)

cellNetSizeDelta :: CellPatch value -> Int
cellNetSizeDelta cell =
  case cell of
    AssertAbsent ->
      0
    Insert _new ->
      1
    Delete _old ->
      -1
    Replace _old _new ->
      0
{-# INLINE cellNetSizeDelta #-}

smallCellsToAscList :: SmallArray (Cell key value) -> [(key, CellPatch value)]
smallCellsToAscList cells =
  go (sizeofSmallArray cells - 1) []
  where
    go index rest
      | index < 0 =
          rest
      | otherwise =
          let Cell key cell = indexSmallArray cells index
           in go (index - 1) ((key, cell) : rest)

pageNetSizeDelta :: Page key value -> Int
pageNetSizeDelta page =
  endpointColumnPresentCount count (pageAfterColumn page)
    - endpointColumnPresentCount count (pageBeforeColumn page)
  where
    !count = pageCount page
{-# INLINE pageNetSizeDelta #-}

endpointColumnPresentCount :: Int -> EndpointColumn value -> Int
endpointColumnPresentCount count =
  popCount . columnPresenceMaskLocal count
{-# INLINE endpointColumnPresentCount #-}

debugCodecStats :: Patch key value -> CodecStats
debugCodecStats patch =
  case patch of
    SmallPatch _cells ->
      emptyCodecStats
    PagedPatch _count pages ->
      Map.foldl' countPage emptyCodecStats pages
  where
    countPage :: CodecStats -> Page key value -> CodecStats
    countPage stats page =
      countEndpointColumn
        (pageAfterColumn page)
        (countEndpointColumn (pageBeforeColumn page) (countKeyColumn (pagePrefixKeys page) stats))
{-# INLINABLE debugCodecStats #-}

emptyCodecStats :: CodecStats
emptyCodecStats =
  CodecStats
    { codecExplicitKeyColumns = 0,
      codecRangeKeyColumns = 0,
      codecAffineKeyColumns = 0,
      codecAbsentColumns = 0,
      codecConstantColumns = 0,
      codecRunColumns = 0,
      codecAffineValueColumns = 0,
      codecDenseColumns = 0
    }
{-# INLINE emptyCodecStats #-}

countKeyColumn :: KeyColumn key -> CodecStats -> CodecStats
countKeyColumn column stats =
  case column of
    ExplicitKeys _keys ->
      stats {codecExplicitKeyColumns = codecExplicitKeyColumns stats + 1}
    IntRangeKeys {} ->
      stats {codecRangeKeyColumns = codecRangeKeyColumns stats + 1}
    IntAffineKeys {} ->
      stats {codecAffineKeyColumns = codecAffineKeyColumns stats + 1}
{-# INLINE countKeyColumn #-}

countEndpointColumn :: EndpointColumn value -> CodecStats -> CodecStats
countEndpointColumn column stats =
  case column of
    AllPresent values ->
      countValueColumn values stats
    Presence mask values
      | mask == 0 ->
          stats {codecAbsentColumns = codecAbsentColumns stats + 1}
      | otherwise ->
          countValueColumn values stats
{-# INLINE countEndpointColumn #-}

countValueColumn :: ValueColumn value -> CodecStats -> CodecStats
countValueColumn values stats =
  case values of
    ConstantValues {} ->
      stats {codecConstantColumns = codecConstantColumns stats + 1}
    RunValues {} ->
      stats {codecRunColumns = codecRunColumns stats + 1}
    IntAffineValues {} ->
      stats {codecAffineValueColumns = codecAffineValueColumns stats + 1}
    DenseValues {} ->
      stats {codecDenseColumns = codecDenseColumns stats + 1}
{-# INLINE countValueColumn #-}

support :: forall key value. Patch key value -> Set key
support patch =
  case patch of
    SmallPatch cells ->
      Set.fromDistinctAscList (fmap fst (smallCellsToAscList cells))
    PagedPatch _count pages ->
      Set.fromDistinctAscList (Map.foldrWithKey prependPage [] pages)
  where
    prependPage :: key -> Page key value -> [key] -> [key]
    prependPage maximumKey page rest =
      prependPrefix (pageCount page - 2) (maximumKey : rest)
      where
        prependPrefix index accumulated
          | index < 0 =
              accumulated
          | otherwise =
              prependPrefix
                (index - 1)
                (keyColumnAt (pagePrefixKeys page) index : accumulated)
{-# INLINE support #-}

pageKeyAt :: key -> Page key value -> Int -> key
pageKeyAt maximumKey page index =
  if index == pageCount page - 1
    then maximumKey
    else keyColumnAt (pagePrefixKeys page) index
{-# INLINE pageKeyAt #-}

keyColumnAt :: KeyColumn key -> Int -> key
keyColumnAt column index =
  case column of
    ExplicitKeys keys ->
      indexSmallArray keys index
    IntRangeKeys start _count ->
      start + index
    IntAffineKeys start step _count ->
      start + index * step
{-# INLINE keyColumnAt #-}

keyColumnCount :: KeyColumn key -> Int
keyColumnCount column =
  case column of
    ExplicitKeys keys ->
      sizeofSmallArray keys
    IntRangeKeys _start count ->
      count
    IntAffineKeys _start _step count ->
      count
{-# INLINE keyColumnCount #-}

keyColumnToList :: KeyColumn key -> [key]
keyColumnToList column =
  collect 0
  where
    !count = keyColumnCount column

    collect !index
      | index == count =
          []
      | otherwise =
          keyColumnAt column index : collect (index + 1)
{-# INLINE keyColumnToList #-}

intKeysAreAffine :: SmallArray Int -> Int -> Int -> Int -> Int -> Bool
intKeysAreAffine keys !start !step !count !index
  | index == count =
      True
  | indexSmallArray keys index == start + index * step =
      intKeysAreAffine keys start step count (index + 1)
  | otherwise =
      False
{-# INLINE intKeysAreAffine #-}

valueColumnAt :: ValueColumn value -> Int -> value
valueColumnAt column index =
  case column of
    ConstantValues _count value ->
      value
    RunValues _count runs ->
      runValueAt index 0 runs
    DenseValues values ->
      indexSmallArray values index
    IntAffineValues start step _count ->
      start + index * step
{-# INLINE valueColumnAt #-}

valueColumnCount :: ValueColumn value -> Int
valueColumnCount column =
  case column of
    ConstantValues count _value ->
      count
    RunValues count _runs ->
      count
    DenseValues values ->
      sizeofSmallArray values
    IntAffineValues _start _step count ->
      count
{-# INLINE valueColumnCount #-}

valueColumnFromArray :: PatchValue value => SmallArray value -> ValueColumn value
valueColumnFromArray =
  buildValueColumn
{-# INLINE valueColumnFromArray #-}

valueColumnToList :: ValueColumn value -> [value]
valueColumnToList column =
  collect 0
  where
    !count = valueColumnCount column

    collect !index
      | index == count =
          []
      | otherwise =
          valueColumnAt column index : collect (index + 1)
{-# INLINE valueColumnToList #-}

buildGenericValueColumn :: Eq value => SmallArray value -> ValueColumn value
buildGenericValueColumn values =
  case sizeofSmallArray values of
    0 ->
      DenseValues emptySmallArray
    count ->
      let !firstValue = indexSmallArray values 0
          !runs = valueRunsFromArray values count firstValue 1 1 []
       in case runs of
            [ValueRun _runLength value] ->
              ConstantValues count value
            _ ->
              if runEncodingIsSmaller count (List.length runs)
                then RunValues count (smallArrayFromList runs)
                else DenseValues values
{-# INLINABLE buildGenericValueColumn #-}

buildIntValueColumn :: SmallArray Int -> ValueColumn Int
buildIntValueColumn values =
  case sizeofSmallArray values of
    0 ->
      DenseValues emptySmallArray
    1 ->
      ConstantValues 1 (indexSmallArray values 0)
    count ->
      let !start = indexSmallArray values 0
          !step = indexSmallArray values 1 - start
       in if intValuesAreAffine values start step count 2
            then
              if step == 0
                then ConstantValues count start
                else IntAffineValues start step count
            else buildGenericValueColumn values
{-# INLINABLE buildIntValueColumn #-}

intValuesAreAffine :: SmallArray Int -> Int -> Int -> Int -> Int -> Bool
intValuesAreAffine values !start !step !count !index
  | index == count =
      True
  | indexSmallArray values index == start + index * step =
      intValuesAreAffine values start step count (index + 1)
  | otherwise =
      False
{-# INLINE intValuesAreAffine #-}

valueRunsFromArray :: Eq value => SmallArray value -> Int -> value -> Int -> Int -> [ValueRun value] -> [ValueRun value]
valueRunsFromArray values !count current !currentLength !index reversedRuns
  | index == count =
      List.reverse (ValueRun currentLength current : reversedRuns)
  | otherwise =
      let !next = indexSmallArray values index
       in if current == next
            then valueRunsFromArray values count current (currentLength + 1) (index + 1) reversedRuns
            else valueRunsFromArray values count next 1 (index + 1) (ValueRun currentLength current : reversedRuns)
{-# INLINABLE valueRunsFromArray #-}

runEncodingIsSmaller :: Int -> Int -> Bool
runEncodingIsSmaller denseCount runCount =
  runCount * runEncodingCellWeight < denseCount
{-# INLINE runEncodingIsSmaller #-}

runEncodingCellWeight :: Int
runEncodingCellWeight =
  2
{-# INLINE runEncodingCellWeight #-}

runValueAt :: Int -> Int -> SmallArray (ValueRun value) -> value
runValueAt index runIndex runs =
  case indexSmallArray runs runIndex of
    ValueRun runLength value
      | index < runLength ->
          value
      | otherwise ->
          runValueAt (index - runLength) (runIndex + 1) runs
{-# INLINE runValueAt #-}

cellsForInstance :: forall key value. Patch key value -> [(key, CellPatch value)]
cellsForInstance patch =
  case patch of
    SmallPatch cells ->
      smallCellsToAscList cells
    PagedPatch _count pages ->
      Map.foldrWithKey collectPage [] pages
  where
    collectPage :: key -> Page key value -> [(key, CellPatch value)] -> [(key, CellPatch value)]
    collectPage maximumKey page rest =
      let !count = pageCount page
          !beforeMask = columnPresenceMaskLocal count (pageBeforeColumn page)
          !beforeValues = columnValuesLocal (pageBeforeColumn page)
          !afterMask = columnPresenceMaskLocal count (pageAfterColumn page)
          !afterValues = columnValuesLocal (pageAfterColumn page)
       in collectPageCells maximumKey page beforeMask beforeValues afterMask afterValues 0 0 0 rest
{-# INLINE cellsForInstance #-}

collectPageCells ::
  key ->
  Page key value ->
  Word64 ->
  ValueColumn value ->
  Word64 ->
  ValueColumn value ->
  Int ->
  Int ->
  Int ->
  [(key, CellPatch value)] ->
  [(key, CellPatch value)]
collectPageCells maximumKey page beforeMask beforeValues afterMask afterValues index beforePacked afterPacked rest
  | index == pageCount page =
      rest
  | otherwise =
      let !key = pageKeyAt maximumKey page index
          !beforePresent = testBit beforeMask index
          !afterPresent = testBit afterMask index
          !before =
            if beforePresent
              then EndpointPresent (valueColumnAt beforeValues beforePacked)
              else EndpointAbsent
          !after =
            if afterPresent
              then EndpointPresent (valueColumnAt afterValues afterPacked)
              else EndpointAbsent
          !nextBeforePacked =
            if beforePresent
              then beforePacked + 1
              else beforePacked
          !nextAfterPacked =
            if afterPresent
              then afterPacked + 1
              else afterPacked
       in (key, endpointsToCell before after)
            : collectPageCells maximumKey page beforeMask beforeValues afterMask afterValues (index + 1) nextBeforePacked nextAfterPacked rest
{-# INLINE collectPageCells #-}

columnPresenceMaskLocal :: Int -> EndpointColumn value -> Word64
columnPresenceMaskLocal count column =
  case column of
    AllPresent _values ->
      lowerBitsMaskLocal count
    Presence presenceMask _values ->
      presenceMask .&. lowerBitsMaskLocal count
{-# INLINE columnPresenceMaskLocal #-}

columnValuesLocal :: EndpointColumn value -> ValueColumn value
columnValuesLocal column =
  case column of
    AllPresent values ->
      values
    Presence _presenceMask values ->
      values
{-# INLINE columnValuesLocal #-}

lowerBitsMaskLocal :: Int -> Word64
lowerBitsMaskLocal count
  | count <= 0 =
      0
  | count >= pageCapacity =
      maxBound
  | otherwise =
      (1 `shiftL` count) - 1
{-# INLINE lowerBitsMaskLocal #-}

endpointsToCell :: Endpoint value -> Endpoint value -> CellPatch value
endpointsToCell before after =
  case (before, after) of
    (EndpointAbsent, EndpointAbsent) ->
      AssertAbsent
    (EndpointAbsent, EndpointPresent afterValue) ->
      Insert afterValue
    (EndpointPresent beforeValue, EndpointAbsent) ->
      Delete beforeValue
    (EndpointPresent beforeValue, EndpointPresent afterValue) ->
      Replace beforeValue afterValue
{-# INLINE endpointsToCell #-}

instance DeltaNormalize (Patch key value) where
  normalizeDelta =
    normalize

  deltaNull =
    null

instance DeltaSupport (Patch key value) where
  type DeltaSupportSet (Patch key value) = Set key

  emptySupport =
    Set.empty

  deltaSupport =
    support

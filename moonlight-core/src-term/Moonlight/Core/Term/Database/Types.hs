{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RoleAnnotations #-}

module Moonlight.Core.Term.Database.Types where

import Data.Foldable (toList, traverse_)
import Data.Functor (void)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray (SmallArray)
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Sequence (Seq)
import Data.STRef (STRef)
import Data.Vector.Unboxed qualified as U
import Moonlight.Core.Identifier.EGraph (PatternVar)
import Numeric.Natural (Natural)
import Prelude

type Operator :: (Type -> Type) -> Type
newtype Operator f = Operator {unOperator :: f ()}
type role Operator representational

instance (forall a. Ord a => Ord (f a)) => Eq (Operator f) where
  Operator left == Operator right = left == right

instance (forall a. Ord a => Ord (f a)) => Ord (Operator f) where
  compare (Operator left) (Operator right) = compare left right

instance (forall a. Show a => Show (f a)) => Show (Operator f) where
  showsPrec precedence (Operator value) =
    showParen (precedence > 10) $
      showString "Operator " . showsPrec 11 value

extractOperator :: Traversable f => f key -> Operator f
extractOperator =
  Operator . void

type RowId :: Type
newtype RowId = RowId {unRowId :: Int}
  deriving stock (Eq, Ord, Show)

type RowIdSet :: Type
newtype RowIdSet = RowIdSet
  { unRowIdSet :: IntSet
  }
  deriving stock (Eq, Show)

emptyRowIdSet :: RowIdSet
emptyRowIdSet =
  RowIdSet IntSet.empty

rowIdSetUnion :: RowIdSet -> RowIdSet -> RowIdSet
rowIdSetUnion (RowIdSet left) (RowIdSet right) =
  RowIdSet (IntSet.union left right)

rowIdSetIntersection :: RowIdSet -> RowIdSet -> RowIdSet
rowIdSetIntersection (RowIdSet left) (RowIdSet right) =
  RowIdSet (IntSet.intersection left right)

rowIdSetNull :: RowIdSet -> Bool
rowIdSetNull (RowIdSet rowKeys) =
  IntSet.null rowKeys

rowIdSetSize :: RowIdSet -> Int
rowIdSetSize (RowIdSet rowKeys) =
  IntSet.size rowKeys

rowIdSetFoldl' :: (result -> Int -> result) -> result -> RowIdSet -> result
rowIdSetFoldl' step result (RowIdSet rowKeys) =
  IntSet.foldl' step result rowKeys

rowIdSetToAscList :: RowIdSet -> [Int]
rowIdSetToAscList (RowIdSet rowKeys) =
  IntSet.toAscList rowKeys

type DatabaseRow :: Type
data DatabaseRow = DatabaseRow
  { rowResult :: !Int,
    rowChildrenArray :: !(PrimArray Int)
  }
  deriving stock (Eq, Ord)

instance Show DatabaseRow where
  showsPrec precedence row =
    showParen (precedence > 10) $
      showString "DatabaseRow {rowResult = "
        . shows (rowResult row)
        . showString ", rowChildren = "
        . shows (rowChildren row)
        . showString "}"

rowChildren :: DatabaseRow -> [Int]
rowChildren =
  PrimArray.primArrayToList . rowChildrenArray
{-# INLINE rowChildren #-}

type DatabaseRowDelta :: (Type -> Type) -> Type
-- | Committed non-monotone row edits grouped by operator. Insertions include
-- the row ids assigned by the committed database after duplicate/canonical
-- rows collapse; deletions distinguish this broader change set from the
-- monotone frontier published by 'TermCommitResult'.
data DatabaseRowDelta f = DatabaseRowDelta
  { rowsDeleted :: !(Map (Operator f) [(RowId, DatabaseRow)]),
    rowsInserted :: !(Map (Operator f) [(RowId, DatabaseRow)])
  }

type TermCommitResult :: (Type -> Type) -> Type -> Type
-- | The observable result of one command commit.
--
-- 'insertedRows' is the exact monotone frontier accepted by this commit,
-- grouped by operator. Within each operator its row ids are unique and
-- ascending, every pair is present in 'committedDatabase', and rows already
-- present (or collapsed as duplicates within the commit) are absent. Row ids
-- are snapshot coordinates: 'compact' may rekey them, so this frontier does
-- not promise identifier stability across compaction.
data TermCommitResult f key = TermCommitResult
  { residualCommands :: ![TermCommand f key],
    insertedRows :: !(Map (Operator f) [(RowId, DatabaseRow)]),
    committedDatabase :: !(Database f key)
  }

type TermCommand :: (Type -> Type) -> Type -> Type
data TermCommand f key
  = DeleteRow !(Operator f) !RowId
  | InsertTerm !key !(f key)
  | UnionResults !key !key

type Column :: Type
data Column
  = ResultColumn
  | ChildColumn !Int
  deriving stock (Eq, Ord, Show)

type ArrangementKey :: Type
data ArrangementKey = ArrangementKey !Int !(SmallArray Column)

type ArrangementValidationError :: Type
data ArrangementValidationError
  = NegativeArrangementChildColumn !Int
  | ArrangementChildColumnOutOfBounds !Int !Int
  | ArrangementOperatorArityMismatch !Int !Int
  | ArrangementPrefixTooDeep !Int !Int
  deriving stock (Eq, Show)

instance Eq ArrangementKey where
  ArrangementKey leftArity leftColumns == ArrangementKey rightArity rightColumns =
    leftArity == rightArity
      && smallArrayToList leftColumns == smallArrayToList rightColumns

instance Ord ArrangementKey where
  compare left right =
    compare
      (arrangementKeyArity left, arrangementKeyColumns left)
      (arrangementKeyArity right, arrangementKeyColumns right)

instance Show ArrangementKey where
  showsPrec precedence key =
    showParen (precedence > 10) $
      showString "ArrangementKey "
        . showsPrec 11 (arrangementKeyArity key, arrangementKeyColumns key)

arrangementKeyForOperator :: Foldable f => Operator f -> [Column] -> Either ArrangementValidationError ArrangementKey
arrangementKeyForOperator (Operator shape) columns =
  traverse_ (validateArrangementColumn arity) columns
    *> Right (ArrangementKey arity (SmallArray.smallArrayFromList columns))
  where
    arity =
      length (toList shape)

validateArrangementColumn :: Int -> Column -> Either ArrangementValidationError ()
validateArrangementColumn arity column =
  case column of
    ResultColumn ->
      Right ()
    ChildColumn childIndex
      | childIndex < 0 ->
          Left (NegativeArrangementChildColumn childIndex)
      | childIndex >= arity ->
          Left (ArrangementChildColumnOutOfBounds childIndex arity)
      | otherwise ->
          Right ()

arrangementKeyArity :: ArrangementKey -> Int
arrangementKeyArity (ArrangementKey arity _columns) =
  arity

arrangementKeyColumns :: ArrangementKey -> [Column]
arrangementKeyColumns (ArrangementKey _arity columns) =
  smallArrayToList columns
{-# INLINE arrangementKeyColumns #-}

type ArrangementPrefix :: Type
newtype ArrangementPrefix = ArrangementPrefix
  { unArrangementPrefix :: [Int]
  }
  deriving stock (Eq, Ord, Show)

arrangementPrefixForKey :: ArrangementKey -> [Int] -> Either ArrangementValidationError ArrangementPrefix
arrangementPrefixForKey key prefixValues
  | prefixDepth > keyDepth =
      Left (ArrangementPrefixTooDeep prefixDepth keyDepth)
  | otherwise =
      Right (ArrangementPrefix prefixValues)
  where
    prefixDepth =
      length prefixValues
    keyDepth =
      length (arrangementKeyColumns key)

validateArrangementKeyForOperator :: Foldable f => Operator f -> ArrangementKey -> Either ArrangementValidationError ()
validateArrangementKeyForOperator (Operator shape) key
  | operatorArity == arrangementKeyArity key =
      Right ()
  | otherwise =
      Left (ArrangementOperatorArityMismatch (arrangementKeyArity key) operatorArity)
  where
    operatorArity =
      length (toList shape)

validateArrangementPrefixForKey :: ArrangementKey -> ArrangementPrefix -> Either ArrangementValidationError ()
validateArrangementPrefixForKey key (ArrangementPrefix prefixValues) =
  () <$ arrangementPrefixForKey key prefixValues

type ArrangementNode :: Type
data ArrangementNode
  = OffsetLeaf !RowIdSet
  | PrefixBranch !RowIdSet !(Map Int ArrangementNode)
  deriving stock (Eq, Show)

type Arrangement :: Type
data Arrangement = Arrangement
  { arrangementOrder :: !ArrangementKey,
    arrangementRoot :: !ArrangementNode
  }
  deriving stock (Eq, Show)

type ArrangementCache :: Type
type ArrangementCache = IntMap (Map ArrangementKey Arrangement)

type RelationStats :: Type
data RelationStats = RelationStats
  { rowCount :: !Int,
    liveRowCount :: !Int,
    distinctPerColumn :: !(U.Vector Int),
    distinctPerPrefix :: !(Map ArrangementKey Int),
    maximumBucketSize :: !Int
  }
  deriving stock (Eq, Show)

type QueryVar :: Type
data QueryVar
  = ExplicitQueryVar !Int
  | AuthoredPatternVar !PatternVar
  | GeneratedPatternNodeVar !Natural
  deriving stock (Eq, Ord, Show)

type QueryTerm :: Type -> Type
data QueryTerm key
  = QueryBound !key
  | QueryVariable !QueryVar
  deriving stock (Eq, Ord, Show)

type QueryAtom :: (Type -> Type) -> Type -> Type
data QueryAtom f key = QueryAtom
  { atomOperator :: !(Operator f),
    atomResult :: !(QueryTerm key),
    atomChildren :: ![QueryTerm key]
  }

type FreeJoinPlan :: (Type -> Type) -> Type -> Type
newtype FreeJoinPlan f key = FreeJoinPlan
  { freeJoinAtoms :: [QueryAtom f key]
  }

type FreeJoinStrategy :: Type
data FreeJoinStrategy
  = FreeJoinEmptyConjunction
  | FreeJoinExactAtomProbe
  | FreeJoinGenericIntersection
  deriving stock (Eq, Ord, Show)

type QueryBinding :: Type -> Type
newtype QueryBinding key = QueryBinding
  { queryBindingAssignments :: Map QueryVar key
  }
  deriving stock (Eq, Ord, Show)

type PatternFreeJoinPlan :: (Type -> Type) -> Type -> Type
data PatternFreeJoinPlan f key = PatternFreeJoinPlan
  { patternFreeJoinPlan :: !(FreeJoinPlan f key),
    patternFreeJoinRoots :: !(NonEmpty (QueryTerm key)),
    patternFreeJoinVariables :: !(Map PatternVar QueryVar)
  }

type PatternCompileState :: (Type -> Type) -> Type -> Type
data PatternCompileState f key = PatternCompileState
  { nextGeneratedPatternNodeVar :: !Natural,
    compileAtoms :: ![QueryAtom f key]
  }

type OperatorTableRowEdit :: Type
data OperatorTableRowEdit
  = InsertOperatorTableRow !DatabaseRow
  | DeleteOperatorTableRow !Int !DatabaseRow

type ChildUserIndex :: Type
type ChildUserIndex = IntMap IntSet

type ChildTupleIndex :: Type
data ChildTupleIndex
  = NullaryChildTupleIndex !IntSet
  | UnaryChildTupleIndex !(IntMap IntSet)
  | BinaryChildTupleIndex !(IntMap (IntMap IntSet))
  | NaryChildTupleIndex !(Map ChildTupleKey IntSet)

type ChildTupleKey :: Type
data ChildTupleKey
  = StoredChildTupleKey !(PrimArray Int)
  | ProbeChildTupleKey ![Int]

instance Eq ChildTupleKey where
  left == right =
    compare left right == EQ

instance Ord ChildTupleKey where
  compare left right =
    case (left, right) of
      (StoredChildTupleKey leftChildren, StoredChildTupleKey rightChildren) ->
        compare leftChildren rightChildren
      (ProbeChildTupleKey leftChildren, ProbeChildTupleKey rightChildren) ->
        compare leftChildren rightChildren
      (ProbeChildTupleKey probeChildren, StoredChildTupleKey storedChildren) ->
        compareChildTupleProbe probeChildren storedChildren
      (StoredChildTupleKey storedChildren, ProbeChildTupleKey probeChildren) ->
        reverseOrdering (compareChildTupleProbe probeChildren storedChildren)

compareChildTupleProbe :: [Int] -> PrimArray Int -> Ordering
compareChildTupleProbe probeChildren storedChildren =
  compareAt 0 probeChildren
  where
    storedChildCount =
      PrimArray.sizeofPrimArray storedChildren
    compareAt !childIndex remainingProbeChildren =
      case remainingProbeChildren of
        []
          | childIndex == storedChildCount -> EQ
          | otherwise -> LT
        probeChild : restProbeChildren
          | childIndex == storedChildCount -> GT
          | otherwise ->
              case compare probeChild (PrimArray.indexPrimArray storedChildren childIndex) of
                EQ -> compareAt (childIndex + 1) restProbeChildren
                ordering -> ordering
{-# INLINE compareChildTupleProbe #-}

reverseOrdering :: Ordering -> Ordering
reverseOrdering ordering =
  case ordering of
    LT -> GT
    EQ -> EQ
    GT -> LT
{-# INLINE reverseOrdering #-}

type ExactIndex :: Type
type ExactIndex = ChildTupleIndex

type ExactResultIndex :: Type
type ExactResultIndex = ChildTupleIndex

type ResultIndex :: Type
type ResultIndex = IntMap IntSet

type ChildColumnValueIndex :: Type
data ChildColumnValueIndex
  = NullaryChildColumnValueIndex
  | UnaryChildColumnValueIndex !(IntMap IntSet)
  | BinaryChildColumnValueIndex !(IntMap IntSet) !(IntMap IntSet)
  | NaryChildColumnValueIndex !(SmallArray (IntMap IntSet))

-- | One sealed, fixed-arity column section. All arrays have the same row
-- count; the child-column count is the owning operator's arity.
type OperatorRowChunk :: Type
data OperatorRowChunk = OperatorRowChunk
  { chunkResults :: !(PrimArray Int),
    chunkChildren :: !(SmallArray (PrimArray Int))
  }

-- | The sole physical row authority for one operator. Sealed chunks own the
-- fixed-capacity prefix, the bounded pending sequence owns the suffix, and
-- tombstones remove ids from their union without shifting snapshot rows.
type OperatorRowStore :: Type
data OperatorRowStore = OperatorRowStore
  { sealedRowChunks :: !(Seq OperatorRowChunk),
    pendingStoredRows :: !(Seq DatabaseRow),
    tombstonedRowIds :: !IntSet
  }

type OperatorTable :: (Type -> Type) -> Type
data OperatorTable f = OperatorTable
  { opShape :: !(f ()),
    opArity :: !Int,
    rowStore :: !OperatorRowStore,
    nextRowId :: !Int,
    -- Rows below this watermark are reflected in the derived indices; rows at
    -- or above it live only in rowStore and exactResultIx. This is private
    -- index-refresh bookkeeping, not the public commit frontier.
    derivedIndexWatermark :: !Int,
    resultIx :: !ResultIndex,
    childColumnIx :: !ChildColumnValueIndex,
    exactIx :: !ExactIndex,
    exactResultIx :: !ExactResultIndex,
    childUserIx :: !ChildUserIndex
  }

type Database :: (Type -> Type) -> Type -> Type
data Database f key = Database
  { operatorIds :: !(Map (Operator f) Int),
    operatorShapes :: !(IntMap (Operator f)),
    nextOperatorId :: !Int,
    operatorTables :: !(IntMap (OperatorTable f)),
    arrangements :: !ArrangementCache
  }
type role Database nominal nominal

type DatabaseEditor :: Type -> (Type -> Type) -> Type -> Type
data DatabaseEditor s f key = DatabaseEditor
  { workingRef :: !(STRef s (Database f key)),
    residualCommandsRef :: !(STRef s [TermCommand f key]),
    controlRef :: !(STRef s DatabaseTransactionControl)
  }

type DatabaseTransactionControl :: Type
data DatabaseTransactionControl
  = CommitDatabaseTransaction
  | AbortDatabaseTransaction
  deriving stock (Eq, Show)

type TupleLookup :: Type -> Type
data TupleLookup key
  = TupleMissing
  | TupleUnique !key
  | TupleAmbiguous !(NonEmpty key)
  deriving stock (Eq, Show)

emptyDatabase :: Database f key
emptyDatabase =
  Database
    { operatorIds = Map.empty,
      operatorShapes = IntMap.empty,
      nextOperatorId = 0,
      operatorTables = IntMap.empty,
      arrangements = IntMap.empty
    }

operatorIdFor :: (forall a. Ord a => Ord (f a)) => Operator f -> Database f key -> Maybe Int
operatorIdFor operator =
  Map.lookup operator . operatorIds
{-# INLINE operatorIdFor #-}

mapRowKeys :: (Int -> Int) -> DatabaseRow -> DatabaseRow
mapRowKeys canonicalizeKey row =
  DatabaseRow
    { rowResult = canonicalizeKey (rowResult row),
      rowChildrenArray = PrimArray.mapPrimArray canonicalizeKey (rowChildrenArray row)
    }
smallArrayToList :: SmallArray value -> [value]
smallArrayToList values =
  fmap (SmallArray.indexSmallArray values) [0 .. SmallArray.sizeofSmallArray values - 1]
{-# INLINE smallArrayToList #-}

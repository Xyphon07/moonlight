{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Lookup where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Traversable (mapAccumL)
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Language (Language)
import Moonlight.Core.Term.Database.Encode
import Moonlight.Core.Term.Database.OperatorTable
import Moonlight.Core.Term.Database.Projection
import Moonlight.Core.Term.Database.Types
import Prelude

rowEntry ::
  (DenseKey key, Traversable f) =>
  Operator f ->
  DatabaseRow ->
  Maybe (key, f key)
rowEntry (Operator template) row =
  fmap
    (\filledTemplate -> (decodeDenseKey (rowResult row), filledTemplate))
    (rehydrateChildren template (rowChildren row))

databaseEntries ::
  (DenseKey key, Traversable f) =>
  Database f key ->
  [(key, f key)]
databaseEntries db =
  foldOperatorTables
    (\operator -> entriesForOperatorRows operator . operatorTableRows)
    db

entriesForResultKey ::
  (DenseKey key, Language f) =>
  key ->
  Database f key ->
  [(key, f key)]
entriesForResultKey resultValue db =
  foldOperatorTables (entriesForOperatorResultKey resultValue) db

foldOperatorTables ::
  Monoid result =>
  (Operator f -> OperatorTable f -> result) ->
  Database f key ->
  result
foldOperatorTables collectOperatorTable database =
  Map.foldMapWithKey collectInternedOperator (operatorIds database)
  where
    collectInternedOperator operator operatorId =
      maybe
        mempty
        (collectOperatorTable operator)
        (IntMap.lookup operatorId (operatorTables database))
{-# INLINE foldOperatorTables #-}

entriesForOperatorRows ::
  (DenseKey key, Traversable f) =>
  Operator f ->
  [(rowId, DatabaseRow)] ->
  [(key, f key)]
entriesForOperatorRows operator =
  mapMaybe (rowEntry operator . snd)

entriesForOperatorResultKey ::
  (DenseKey key, Language f) =>
  key ->
  Operator f ->
  OperatorTable f ->
  [(key, f key)]
entriesForOperatorResultKey resultValue (Operator template) table =
  fmap
    (\filledTemplate -> (resultValue, filledTemplate))
    (mapMaybe (rehydrateChildren template . rowChildren . snd) rows)
  where
    rows =
      operatorTableRowsForResultKey (encodeDenseKey resultValue) table

rehydrateTuple ::
  (DenseKey key, Traversable f) =>
  Operator f ->
  [Int] ->
  Maybe (key, f key)
rehydrateTuple (Operator template) (resultKey : childKeys) =
  fmap
    (\children -> (decodeDenseKey resultKey, children))
    (rehydrateChildren template childKeys)
rehydrateTuple _ [] =
  Nothing

rehydrateChildren ::
  (DenseKey key, Traversable f) =>
  f () ->
  [Int] ->
  Maybe (f key)
rehydrateChildren template childKeys =
  case mapAccumL step (Right childKeys) template of
    (Right [], filled) -> sequenceA filled
    _ -> Nothing
  where
    step ::
      DenseKey key =>
      Either () [Int] ->
      () ->
      (Either () [Int], Maybe key)
    step (Left ()) () =
      (Left (), Nothing)
    step (Right []) () =
      (Left (), Nothing)
    step (Right (keyValue : restKeys)) () =
      (Right restKeys, Just (decodeDenseKey keyValue))

lookupTupleAll ::
  (DenseKey key, Language f) =>
  f key ->
  Database f key ->
  TupleLookup key
lookupTupleAll tupleValue database =
  maybe
    TupleMissing
    (tupleLookupFromResultKeys . lookupChildResultKeys childKeys)
    (operatorTableFor (extractOperator tupleValue) database)
  where
    childKeys = encodedChildren tupleValue

lookupTupleUnique ::
  (DenseKey key, Language f) =>
  f key ->
  Database f key ->
  Either (NonEmpty key) (Maybe key)
lookupTupleUnique tupleValue database =
  case lookupTupleAll tupleValue database of
    TupleMissing -> Right Nothing
    TupleUnique key -> Right (Just key)
    TupleAmbiguous keys -> Left keys

lookupLeastTuple ::
  (DenseKey key, Language f) =>
  f key ->
  Database f key ->
  Maybe key
lookupLeastTuple tupleValue database =
  maybe
    Nothing
    (tupleLeastFromResultKeys . lookupChildResultKeys childKeys)
    (operatorTableFor (extractOperator tupleValue) database)
  where
    childKeys = encodedChildren tupleValue

tupleLookupFromResultKeys :: DenseKey key => IntSet -> TupleLookup key
tupleLookupFromResultKeys encodedResultKeys =
  case fmap decodeDenseKey (IntSet.toAscList encodedResultKeys) of
    [] -> TupleMissing
    [key] -> TupleUnique key
    key : otherKeys -> TupleAmbiguous (key :| otherKeys)

tupleLeastFromResultKeys :: DenseKey key => IntSet -> Maybe key
tupleLeastFromResultKeys =
  fmap decodeDenseKey . IntSet.lookupMin
{-# INLINE tupleLeastFromResultKeys #-}

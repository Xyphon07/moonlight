{-# LANGUAGE RoleAnnotations #-}

-- | 'TotalRegistry', a total mapping from a finite key type to values.
-- Totality is discharged at construction: 'mkTotalRegistry' validates that
-- every key of the 'FiniteUniverse' is covered and rejects incomplete
-- coverage, so 'lookupTotal' is total for every lawful 'FiniteUniverse'
-- instance (the completeness law stated on that class). For a key outside the
-- finite universe, the result is deliberately unspecified but non-crashing.
module Moonlight.Core.TotalRegistry
  ( TotalRegistry,
    mkTotalRegistry,
    lookupTotal,
  )
where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Moonlight.Core.Finite (FiniteUniverse (..))
import Moonlight.Core.Validation (Validation (..), validationToEither)
import Prelude

type TotalRegistry :: Type -> Type -> Type
data TotalRegistry key value = TotalRegistry !(Map key value) !value
type role TotalRegistry nominal representational

mkTotalRegistry :: (FiniteUniverse key, Ord key) => Map key value -> Either [key] (TotalRegistry key value)
mkTotalRegistry entries =
  case validationToEither (traverse (lookupFiniteEntry entries) finiteUniverse) of
    Left missingKeys ->
      Left missingKeys
    Right validatedEntries@((_firstKey, firstValue) :| _rest) ->
      Right (TotalRegistry (Map.fromList (NonEmpty.toList validatedEntries)) firstValue)

lookupFiniteEntry :: Ord key => Map key value -> key -> Validation [key] (key, value)
lookupFiniteEntry entries key =
  case Map.lookup key entries of
    Nothing ->
      Invalid [key]
    Just value ->
      Valid (key, value)

lookupTotal :: Ord key => TotalRegistry key value -> key -> value
lookupTotal (TotalRegistry entries fallbackValue) key =
  Map.findWithDefault fallbackValue key entries

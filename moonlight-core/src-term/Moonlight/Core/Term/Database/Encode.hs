{-# LANGUAGE QuantifiedConstraints #-}

module Moonlight.Core.Term.Database.Encode where

import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as PrimArray
import Moonlight.Core.DenseKey (DenseKey (..))
import Moonlight.Core.Term.Database.Types
import Prelude

encodedRow ::
  (DenseKey key, Foldable f) =>
  key ->
  f key ->
  DatabaseRow
encodedRow resultValue tupleValue =
  DatabaseRow
    { rowResult = encodeDenseKey resultValue,
      rowChildrenArray = encodedChildren tupleValue
    }
{-# INLINE encodedRow #-}

encodedChildren ::
  (DenseKey key, Foldable f) =>
  f key ->
  PrimArray Int
encodedChildren tupleValue =
  PrimArray.primArrayFromListN
    (length tupleValue)
    (foldr ((:) . encodeDenseKey) [] tupleValue)
{-# INLINE encodedChildren #-}

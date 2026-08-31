-- | The 'DenseKey' class: a stable round-tripping encoding between a key type
-- and an 'Int' key. Callers that require compact non-negative ranges must state
-- that stronger storage invariant separately.
module Moonlight.Core.DenseKey
  ( DenseKey (..),
  )
where

import Data.Kind (Constraint, Type)
import Prelude (Eq, Int, Ord, id)

type DenseKey :: Type -> Constraint
class (Eq key, Ord key) => DenseKey key where
  encodeDenseKey :: key -> Int
  decodeDenseKey :: Int -> key

instance DenseKey Int where
  encodeDenseKey = id
  decodeDenseKey = id

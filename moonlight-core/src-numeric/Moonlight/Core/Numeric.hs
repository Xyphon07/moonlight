{-# LANGUAGE TypeFamilies #-}

-- | Aggregate aliases of the scalar tower: 'OrderedRing', 'OrderedField', and 'ContinuousField' (an ordered field whose magnitude is its own carrier).
module Moonlight.Core.Numeric
  ( OrderedRing,
    OrderedField,
    ContinuousField,
  )
where

import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (~))
import Moonlight.Core.Scalar (Field, Magnitude, Metric, Ring)
import Prelude (Double, Float, Int, Integer, Ord)

type OrderedRing :: Type -> Constraint
class (Ring a, Ord a) => OrderedRing a

type OrderedField :: Type -> Constraint
class (OrderedRing a, Field a) => OrderedField a

type ContinuousField :: Type -> Constraint
class (OrderedField a, Metric a, Magnitude a ~ a) => ContinuousField a

instance OrderedRing Int

instance OrderedRing Integer

instance OrderedRing Double

instance OrderedField Double

instance ContinuousField Double

instance OrderedRing Float

instance OrderedField Float

instance ContinuousField Float

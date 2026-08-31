-- | Real-valued magnitudes of obstructions ('HasMagnitude'), plus a total
-- summing the magnitudes carried by a validation.
module Moonlight.Algebra.Pure.Magnitude
  ( HasMagnitude (..),
    totalObstructionMagnitude,
  )
where

import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Moonlight.Core (Validation (..))

type HasMagnitude :: Type -> Constraint
class HasMagnitude obstruction where
  obstructionMagnitude :: obstruction -> Double

totalObstructionMagnitude :: HasMagnitude obstruction => Validation (NonEmpty obstruction) value -> Double
totalObstructionMagnitude validationValue =
  case validationValue of
    Valid _ -> 0.0
    Invalid obstructions -> sum (fmap obstructionMagnitude obstructions)

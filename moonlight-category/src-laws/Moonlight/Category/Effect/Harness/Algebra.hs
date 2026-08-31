{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Executable checks for Galois-connection laws.
module Moonlight.Category.Effect.Harness.Algebra
  ( galoisAdjoint,
    galoisDeflation,
    galoisInflation,
    galoisRetraction,
    ordinalGaloisMonotone,
  )
where

import Moonlight.Category.Pure.Galois (GaloisConnection (..), OrdinalGalois (..))

galoisAdjoint :: forall a b. GaloisConnection a b => a -> b -> Bool
galoisAdjoint left right = (left <= gamma right) == (alpha left <= right)

galoisDeflation :: forall a b. GaloisConnection a b => b -> Bool
galoisDeflation right = alpha (gamma right) <= right

galoisInflation :: forall a b. GaloisConnection a b => a -> Bool
galoisInflation left = left <= gamma (alpha left)

galoisRetraction :: forall a b. GaloisConnection a b => a -> Bool
galoisRetraction left = alpha (gamma (alpha left)) == alpha left

ordinalGaloisMonotone :: forall a b. OrdinalGalois a b => Bool
ordinalGaloisMonotone =
  let adjacentThresholds = zip (thresholds @a @b) (drop 1 (thresholds @a @b))
   in all (\((leftA, leftB), (rightA, rightB)) -> leftA <= rightA && leftB <= rightB) adjacentThresholds

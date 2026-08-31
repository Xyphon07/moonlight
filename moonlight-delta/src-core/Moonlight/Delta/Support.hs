{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Moonlight.Delta.Support
  ( DeltaSupport (..),
  )
where

import Data.Kind (Type)

-- | Support projection for deltas.
--
-- Instances must satisfy:
--
-- * @deltaSupport (normalizeDelta delta) == deltaSupport delta@ when the
--   delta type also has a 'Moonlight.Delta.Normalize.DeltaNormalize'
--   instance.
-- * null deltas should report 'emptySupport'.
--
-- The support set is the observable footprint of a delta, not an internal
-- storage summary.
class DeltaSupport delta where
  type DeltaSupportSet delta :: Type

  emptySupport :: DeltaSupportSet delta
  deltaSupport :: delta -> DeltaSupportSet delta

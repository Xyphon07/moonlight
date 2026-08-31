module Moonlight.Delta.Normalize
  ( DeltaNormalize (..),
  )
where

import Data.Bool (Bool)

-- | Delta canonicalization.
--
-- Instances must satisfy:
--
-- * @normalizeDelta (normalizeDelta delta) == normalizeDelta delta@.
-- * @deltaNull (normalizeDelta delta) == deltaNull delta@.
--
-- A null delta may still carry representation-specific data before
-- normalization, but canonical nulls should be stable under
-- 'normalizeDelta'.
class DeltaNormalize delta where
  normalizeDelta :: delta -> delta
  deltaNull :: delta -> Bool
